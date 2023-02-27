library(tidyverse)

f <- mbohelpr::set_data_path("applications", "2023")

raw_apps <- read_csv(paste0(f, "raw/pgy1_interview_scores_fy23.csv"))

# make tidy data sets

data_demog <- raw_apps %>%
    select(
        cas_id:last_name, 
        school = pharmacy_school_name, 
        gpa = pharmacy_school_gpa,
        interest_1 = custom_field_interest_1,
        interest_2 = custom_field_interest_2,
        interest_3 = custom_field_interest_3,
        # school_score = custom_field_school_score,
        tmc_lor = `custom_field_mh-tmc_rec`
    ) %>%
    mutate(
        # across(tmc_lor, list(~. == "Y")),
        across(tmc_lor, list(~. == "Y"), .names = "{.col}"),
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

data_fit <- raw_apps %>%
    select(cas_id, contains("remarks_")) %>%
    mutate(
        across(
            contains("remarks_"), 
            str_replace_all, 
            pattern = "( ){0,1}- (Great|Poor) Fit", 
            replacement = ""
        ),
        across(
            contains("remarks_"), 
            str_replace_all, 
            pattern = "(Preceptor|Leadership|Admin|Residents|MMI 1-2): ", 
            replacement = ""
        ),
        across(contains("remarks_"), as.numeric)
    ) %>%
    pivot_longer(-cas_id) %>%
    mutate(
        across(name, str_replace_all, pattern = "interview_|remarks_|1-2_", replacement = ""),
        low_fit = value <= 2    
    ) %>%
    separate(name, c("group", "interviewer")) %>%
    filter(!is.na(value))

data_fit_summary <- data_fit %>%
    group_by(cas_id) %>%
    summarize(
        # num_fits = n(),
        fit_median = median(value, na.rm = TRUE),
        num_low_fit = sum(low_fit, na.rm = TRUE)
    )

data_application <- raw_apps %>%
    select(cas_id, contains("application_scoring_score")) %>%
    rename_all(str_replace_all, pattern = "assignments_|scoring_score_", replacement = "") %>%
    pivot_longer(-cas_id) %>%
    group_by(cas_id) %>%
    summarize(application_median = median(value, na.rm = TRUE))

data_interviews <- raw_apps %>%
    select(cas_id, starts_with("interview_")) %>%
    select(cas_id, contains("score")) %>%
    rename_all(str_replace_all, pattern = "interview_|1-2_|score_", replacement = "") %>%
    pivot_longer(-cas_id) %>%
    filter(!is.na(value)) %>%
    separate(name, c("group", "interviewer")) 

data_interviews_summary <- data_interviews %>%
    group_by(cas_id) %>%
    summarize(
        interview_median = median(value, na.rm = TRUE),
        interview_total = sum(value, na.rm = TRUE),
        num_interview_scores = n()
    )

data_interviews_individ <- data_interviews %>%
    group_by(cas_id, group) %>%
    summarize(med_score = median(value, na.rm = TRUE)) %>%
    pivot_wider(names_from = group, values_from = med_score)

data_export <- data_demog %>%
    left_join(data_interviews_summary, by = "cas_id") %>%
    left_join(data_fit_summary, by = "cas_id") %>%
    left_join(data_interviews_individ, by = "cas_id") %>%
    left_join(data_application, by = "cas_id") %>%
    arrange(desc(interview_median), desc(fit_median))

openxlsx::write.xlsx(
    data_export,
    paste0(f, "final/2023_interviews.xlsx")
)

# write_csv(total_score, "data/final/2021_applications.csv")
# write_csv(data_app_scores, "data/final/2021_application_scores.csv")
# write_csv(data_interests, "data/final/2021_applicant_interests.csv")

