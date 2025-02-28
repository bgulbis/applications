library(tidyverse)

f <- mbohelpr::set_data_path("applications", "2025")

raw_apps <- read_csv(paste0(f, "raw/pgy1_interview_scores_fy25.csv")) |> 
    filter(!is.na(interview_leadership_score_0))

# make tidy data sets

data_demog <- raw_apps |>
    select(
        cas_id:last_name, 
        school = contains("pharmacy_school"),
        gpa = contains("gpa")
    )

data_fit <- raw_apps |>
    select(cas_id, contains("remarks_")) |>
    mutate(
        across(
            contains("remarks_"), 
            \(x) str_remove_all(
                x,
                pattern = "( ){0,1}- (Great|Poor) Fit"
            ) 
        ),
        across(
            contains("remarks_"), 
            \(x) str_remove_all(
                x,
                pattern = "(Preceptors|Leadership|Admin|Residents|Case - COPD|Case - UTI): "
            )
        )
        # across(contains("remarks_"), as.numeric)
    ) |>
    pivot_longer(-cas_id) |>
    filter(!is.na(value)) |> 
    mutate(
        across(value, as.numeric),
        across(name, \(x) str_remove_all(x, pattern = "interview_|remarks_|1-2_|_-_uti|_-_copd")),
        low_fit = value <= 2    
    ) |>
    separate(name, c("group", "interviewer")) 

data_fit_summary <- data_fit |>
    group_by(cas_id) |>
    summarize(
        # num_fits = n(),
        fit_median = median(value, na.rm = TRUE),
        num_low_fit = sum(low_fit, na.rm = TRUE)
    )

data_application <- raw_apps |>
    select(cas_id, contains("application_scoring_score")) |>
    rename_all(str_replace_all, pattern = "assignments_|scoring_score_", replacement = "") |>
    pivot_longer(-cas_id) |>
    group_by(cas_id) |>
    summarize(application_median = median(value, na.rm = TRUE))

data_interviews <- raw_apps |>
    select(cas_id, starts_with("interview_")) |>
    select(cas_id, contains("score")) |>
    rename_all(str_remove_all, pattern = "interview_|1-2_|score_|_-_") |>
    pivot_longer(-cas_id) |>
    filter(!is.na(value)) |>
    mutate(
        across(name, \(x) str_remove_all(x, "uti|copd")),
        across(name, \(x) str_replace_all(x, "ice_", "ice")),
        across(name, \(x) str_replace_all(x, "soap_", "soap")),
        across(value, \(x) if_else(str_detect(name, "case|soap"), x * 1.15, x))
    ) |>
    separate(name, c("group", "interviewer")) 

data_interviews_summary <- data_interviews |>
    group_by(cas_id) |>
    summarize(
        interview_median = median(value, na.rm = TRUE),
        interview_total = sum(value, na.rm = TRUE),
        num_interview_scores = n()
    )

# data_interviews_individ <- data_interviews |>
#     group_by(cas_id, group) |>
#     summarize(med_score = median(value, na.rm = TRUE)) |>
#     pivot_wider(names_from = group, values_from = med_score)

data_export <- data_demog |>
    left_join(data_interviews_summary, by = "cas_id") |>
    left_join(data_fit_summary, by = "cas_id") |>
    # left_join(data_interviews_individ, by = "cas_id") |>
    left_join(data_application, by = "cas_id") |>
    arrange(desc(interview_median), desc(fit_median))

openxlsx::write.xlsx(
    data_export,
    paste0(f, "final/2025_interviews.xlsx")
)

# write_csv(total_score, "data/final/2021_applications.csv")
# write_csv(data_app_scores, "data/final/2021_application_scores.csv")
# write_csv(data_interests, "data/final/2021_applicant_interests.csv")

