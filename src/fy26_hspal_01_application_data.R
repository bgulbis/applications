library(tidyverse)

f <- mbohelpr::set_data_path("applications", "2026")

raw_apps <- read_csv(paste0(f, "raw/hspal_application_scores_fy26.csv"))

# make tidy data sets

data_demog <- raw_apps |>
    select(
        cas_id:last_name, 
        school = starts_with("custom_questions"), 
        current_city,
        current_state,
        starts_with("assignment")
    ) 

data_app_scores <- raw_apps |>
    select(cas_id, starts_with("assignment")) |>
    mutate(across(starts_with("assignment"), as.character)) |>
    pivot_longer(starts_with("assignment")) |>
    mutate(
        across(name, \(x) str_replace_all(x, pattern = "assignment.{0,1}_application_scoring_", replacement = "")),
        across(name, \(x) str_replace_all(x, pattern = "question_interview_1_yes_0_no_", replacement = "interview"))
    ) |> 
    separate(name, c("name", "n"), sep = "_") |>
    pivot_wider() |>
    mutate(
        across(remark, \(x) str_extract(x, pattern = "[0-9]")),
        across(c(remark, score, n), as.numeric)
    ) |>
    filter(!is.na(score)) 

# create adjusted scores to normalize for different reviewers

all_app_median <- data_app_scores |>
    summarize(across(c(remark, score), \(x) median(x, na.rm = TRUE)))

reviewer_adjust <- data_app_scores |>
    group_by(reviewer) |>
    summarize(across(c(remark, score), \(x) median(x, na.rm = TRUE))) |>
    mutate(reviewer_adjustment = (all_app_median$score - score) * 0.5) |>
    select(reviewer, reviewer_adjustment)

app_median <- data_app_scores |>
    left_join(reviewer_adjust, by = "reviewer") |>
    mutate(score_adj = score + reviewer_adjustment) |>
    group_by(cas_id) |>
    summarize(across(c(remark, score, score_adj), \(x) median(x, na.rm = TRUE)))

df_app_score <- data_app_scores |>
    left_join(reviewer_adjust, by = "reviewer") |>
    mutate(score_adj = score + reviewer_adjustment) |>
    select(cas_id, n, score_adj) |>
    pivot_wider(names_from = n, names_prefix = "score_", values_from = score_adj)

df_app_remark <- data_app_scores |>
    select(cas_id, n, remark) |>
    pivot_wider(names_from = n, names_prefix = "fit_", values_from = remark) 

df_low_fit <- data_app_scores |>
    select(cas_id, n, remark) |>
    mutate(low_fit = remark <= 2) |>
    group_by(cas_id) |>
    summarize(across(low_fit, \(x) sum(x, na.rm = TRUE)))

total_score <- data_demog |>
    left_join(app_median, by = "cas_id") |>
    # left_join(df_app_score, by = "cas_id") |>
    # left_join(df_app_remark, by = "cas_id") |>
    left_join(df_low_fit, by = "cas_id") |>
    # left_join(df_video, by = "cas_id") |>
    # group_by(cas_id) |>
    # mutate(app_name = str_c(last_name, first_name, sep = ", ")) |>
    rename(fit_median = remark, score_median = score, score_adj_median = score_adj) |>
    arrange(desc(score_adj_median)) |>
    select(cas_id, first_name, last_name, score_adj_median, score_median, fit_median, low_fit, school)

openxlsx::write.xlsx(
    total_score,
    paste0(f, "final/2026_hspal_applications.xlsx")
)

# write_csv(total_score, "data/final/2021_applications.csv")
# write_csv(data_app_scores, "data/final/2021_application_scores.csv")
# write_csv(data_interests, "data/final/2021_applicant_interests.csv")

