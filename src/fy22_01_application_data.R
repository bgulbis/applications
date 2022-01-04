library(tidyverse)

f <- mbohelpr::set_data_path("applications", "2022")

raw_apps <- read_csv(paste0(f, "raw/pgy1_application_scores_fy22.csv"))

# make tidy data sets

data_demog <- raw_apps %>%
    select(
        cas_id:last_name, 
        school = pharmacy_school_name_0, 
        gpa = pharmacy_school_gpa_0,
        interest_1 = custom_field_interest_1,
        interest_2 = custom_field_interest_2,
        interest_3 = custom_field_interest_3,
        school_score = custom_field_school_score,
        tmc_lor = `custom_field_mh-tmc_rec`
    ) %>%
    mutate(
        across(tmc_lor, list(~. == "Y")),
        across(starts_with("interest"), str_replace_all, pattern = "Not Specified", replacement = ""),
        school_abbrev = school,
        across(school_abbrev, str_replace_all, pattern = "UNIVERSITY", replacement = "UNIV"),
        across(school_abbrev, str_replace_all, pattern = "HEALTH SCIENCE(S){0,1} CENTER", replacement = "HSC"),
        across(school_abbrev, str_replace_all, pattern = "COLLEGE OF PHARMACY", replacement = "COP"),
        across(school_abbrev, str_replace_all, pattern = "AND HEALTH SCIENCE", replacement = "AND HS"),
        across(school_abbrev, str_replace_all, pattern = "AGRICULTURAL AND MECHANICAL", replacement = "A&M"),
        across(school_abbrev, str_replace_all, pattern = " UNIVERSITIES COLLEGE OF MEDICINE", replacement = " UNIV COM"),
        across(school_abbrev, str_replace_all, pattern = " - DOWNERS GROVE", replacement = " - CHI"),
        across(
            school_abbrev, 
            str_replace_all, 
            pattern = paste(
                " - STATE UNIV OF NEW JERSEY - NEW BRUNSWICK",
                " - FORT WORTH",
                " - UNIV PARK",
                " - LAKE ERIE COLLEGE OF OSTEOPATHIC MEDICINE AND SCHOOL OF PHARMACY",
                " OF MEDICINE AND SCIENCE",
                " - DENVER AND HSC",
                " - BOTHELL CAMPUS/SEATTLE CAMPUS/TACOMA CAMPUS",
                " FOR MEDICAL SCIENCES",
                " - CHAPEL HILL",
                " - WEST LAFAYETTE",
                " - KINGSTON",
                " - MAIN CAMPUS",
                " \\(FOREIGN\\) INSTITUTION",
                sep = "|"
            ),
            replacement = ""
        )
    )

data_app_scores <- raw_apps %>%
    select(cas_id, starts_with("assignment")) %>%
    mutate(across(starts_with("assignment"), as.character)) %>%
    pivot_longer(starts_with("assignment")) %>%
    mutate(
        across(name, str_replace_all, pattern = "assignment.{0,1}_application_scoring_", replacement = ""),
        across(
            name, 
            str_replace_all, 
            pattern = c(
                "question_no_score" = "no",
                "question_yes_score" = "yes",
                "question_video_interview_first_score" = "video",
                "question_no_comments" = "commentsNo",
                "question_yes_comments" = "commentsYes",
                "question_video_interview_first_comments" = "commentsVideo"
            )
        )
    ) %>%
    separate(name, c("name", "n"), sep = "_") %>%
    pivot_wider() %>%
    mutate(
        across(remark, str_extract, pattern = "[0-9]"),
        across(c(remark, score, n), as.numeric)
    ) %>%
    filter(!is.na(score)) 

data_ref <- raw_apps %>%
    select(cas_id, starts_with("reference")) %>% 
    pivot_longer(starts_with("reference")) %>%
    mutate(across(name, str_replace_all, pattern = "reference_overall_", replacement = "")) %>%
    separate(name, c("name", "n"), sep = "_") %>%
    pivot_wider() %>%
    mutate(across(n, as.numeric)) %>%
    filter(!is.na(rating))

# create adjusted scores to normalize for different reviewers

all_app_median <- data_app_scores %>%
    summarize(across(c(remark, score), median, na.rm = TRUE))

reviewer_adjust <- data_app_scores %>%
    group_by(reviewer) %>%
    summarize(across(c(remark, score), median, na.rm = TRUE)) %>%
    mutate(reviewer_adjustment = (all_app_median$score - score) * 0.5) %>%
    select(reviewer, reviewer_adjustment)

app_median <- data_app_scores %>%
    left_join(reviewer_adjust, by = "reviewer") %>%
    mutate(score_adj = score + reviewer_adjustment) %>%
    group_by(cas_id) %>%
    summarize(across(c(remark, score, score_adj), median, na.rm = TRUE))

df_app_score <- data_app_scores %>%
    left_join(reviewer_adjust, by = "reviewer") %>%
    mutate(score_adj = score + reviewer_adjustment) %>%
    select(cas_id, n, score_adj) %>%
    pivot_wider(names_from = n, names_prefix = "score_", values_from = score_adj)

df_app_remark <- data_app_scores %>%
    select(cas_id, n, remark) %>%
    pivot_wider(names_from = n, names_prefix = "fit_", values_from = remark) 

df_low_fit <- data_app_scores %>%
    select(cas_id, n, remark) %>%
    mutate(low_fit = remark <= 2) %>%
    group_by(cas_id) %>%
    summarize(across(low_fit, sum, na.rm = TRUE))

df_video <- data_app_scores %>%
    select(cas_id, video_score = n, no:video) %>%
    mutate(
        across(c(no, yes, video), as.numeric),
        across(c(no, yes, video), as.logical)
    ) %>%
    group_by(cas_id) %>%
    summarize(across(c(no, yes, video), sum, na.rm = TRUE)) %>%
    rename(onsite_no = no, onsite_yes = yes, video_first = video)

df_vid_comments <- data_app_scores %>%
    select(cas_id, n, starts_with("comments")) 
    # pivot_wider(names_from = n, name_prefix = "onsite_comments", values_from)


total_score <- data_demog %>%
    left_join(app_median, by = "cas_id") %>%
    # left_join(df_app_score, by = "cas_id") %>%
    # left_join(df_app_remark, by = "cas_id") %>%
    left_join(df_low_fit, by = "cas_id") %>%
    left_join(df_video, by = "cas_id") %>%
    group_by(cas_id) %>%
    # mutate(app_name = str_c(last_name, first_name, sep = ", ")) %>%
    rename(fit_median = remark, score_median = score, score_adj_median = score_adj) %>%
    arrange(desc(score_adj_median)) %>%
    select(cas_id, first_name, last_name, score_adj_median, score_median, fit_median, low_fit, onsite_yes, onsite_no, video_first, school, gpa, starts_with("interest"), tmc_lor)

# total_median <- total_score %>%
#     ungroup() %>%
#     summarize_at(
#         "score", 
#         funs(
#             median, 
#             p25 = quantile(., 0.25), 
#             p75 = quantile(., 0.75)
#         )
#     )

data_interests <- total_score %>%
    select(cas_id, interest_1:interest_3) %>%
    pivot_longer(interest_1:interest_3, values_to = "interest") %>%
    filter(
        !is.na(interest),
        interest != ""
    ) %>%
    separate(name, c(NA, "tmp_pref"), sep = "_") %>%
    mutate(
        preference = case_when(
            tmp_pref == 1 ~ 3,
            tmp_pref == 2 ~ 2,
            tmp_pref == 3 ~ 1
        )
    ) %>%
    select(-tmp_pref)

openxlsx::write.xlsx(
    total_score,
    paste0(f, "final/2022_applications.xlsx")
)

# write_csv(total_score, "data/final/2021_applications.csv")
# write_csv(data_app_scores, "data/final/2021_application_scores.csv")
# write_csv(data_interests, "data/final/2021_applicant_interests.csv")

