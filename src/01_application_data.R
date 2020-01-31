library(tidyverse)

raw_apps <- read_csv("data/raw/2020/pgy1_application_scores.csv")
    # filter(!(last_name %in% c("Ju", "Kessinger", "Rakouki", "Zidaru")))

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
    mutate_at("tmc_lor", funs(. == "Y")) %>%
    mutate_at(
        c("interest_1", "interest_2", "interest_3"), 
        str_replace_all, 
        pattern = "Not Specified", 
        replacement = ""
    ) %>%
    mutate(school_abbrev = school) %>%
    mutate_at(
        "school_abbrev", 
        str_replace_all, 
        pattern = "UNIVERSITY",
        replacement = "UNIV"
    ) %>%
    mutate_at(
        "school_abbrev", 
        str_replace_all, 
        pattern = "HEALTH SCIENCE(S){0,1} CENTER", 
        replacement = "HSC"
    ) %>%
    mutate_at(
        "school_abbrev", 
        str_replace_all, 
        pattern = "COLLEGE OF PHARMACY", 
        replacement = "COP"
    ) %>%
    mutate_at(
        "school_abbrev", 
        str_replace_all, 
        pattern = "AND HEALTH SCIENCE",
        replacement = "AND HS"
    ) %>%
    mutate_at(
        "school_abbrev", 
        str_replace_all, 
        pattern = "AGRICULTURAL AND MECHANICAL", 
        replacement = "A&M"
    ) %>%
    mutate_at(
        "school_abbrev", 
        str_replace_all, 
        pattern = " UNIVERSITIES COLLEGE OF MEDICINE",
        replacement = " UNIV COM"
    ) %>%
    mutate_at(
        "school_abbrev", 
        str_replace_all, 
        pattern = " - DOWNERS GROVE", 
        replacement = " - CHI"
    ) %>%
    mutate_at(
        "school_abbrev", 
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

data_app_scores <- raw_apps %>%
    select(cas_id, starts_with("assignment")) %>%
    gather(key = key, value = value, -cas_id) %>%
    mutate_at(
        "key",
        str_replace_all, 
        pattern = "assignment.{0,1}_application_scoring_",
        replacement = ""
    ) %>%
    separate(key, c("key", "n"), sep = "_") %>%
    spread(key, value) %>%
    mutate_at("remark", str_extract, pattern = "[0-9]") %>%
    mutate_at(c("remark", "score"), as.numeric) %>%
    filter(!is.na(score)) 

data_vidyo_scores <- raw_apps %>%
    select(
        cas_id, 
        starts_with("interview"),
        -interview_preceptor_interviewer
    ) %>%
    gather(key = key, value = value, -cas_id) %>%
    mutate_at(
        "key", 
        str_replace_all, 
        pattern = "interview_vidyo_interview_", 
        replacement = ""
    ) %>%
    separate(key, c("key", "n"), sep = "_") %>%
    spread(key, value) %>%
    mutate_at("remarks", str_extract, pattern = "[0-9]") %>%
    mutate_at(c("remarks", "score"), as.numeric) %>%
    filter(!is.na(score)) 

data_ref <- raw_apps %>%
    select(cas_id, starts_with("reference")) %>%
    gather(key = key, value = value, -cas_id) %>%
    mutate_at(
        "key",
        str_replace_all, 
        pattern = "reference_overall_", 
        replacement = ""
    ) %>%
    separate(key, c("key", "n"), sep = "_") %>%
    spread(key, value) %>%
    filter(!is.na(rating))

# create adjusted scores to normalize for different reviewers

all_app_median <- data_app_scores %>%
    summarize_at(
        c("remark", "score"), 
        funs(
            median, 
            p25 = quantile(., 0.25), 
            p75 = quantile(., 0.75)
        )
    )

app_reviewer_med <- data_app_scores %>%
    group_by(reviewer) %>%
    summarize_at(
        c("remark", "score"), 
        funs(
            app_rev_median = median, 
            app_rev_p25 = quantile(., 0.25), 
            app_rev_p75 = quantile(., 0.75)
        )
    ) %>%
    mutate(
        app_score_adj = (all_app_median$score_median - score_app_rev_median) * 0.5
    )

app_median <- data_app_scores %>%
    left_join(
        app_reviewer_med[c("reviewer", "app_score_adj")], 
        by = "reviewer"
    ) %>%
    mutate(score_adj = score + app_score_adj) %>%
    group_by(cas_id) %>%
    summarize_at(
        c("remark", "score", "score_adj"), 
        funs(
            app_median = median, 
            app_p25 = quantile(., 0.25), 
            app_p75 = quantile(., 0.75)
        )
    )

all_vidyo_median <- data_vidyo_scores %>%
    summarize_at(
        c("remarks", "score"), 
        funs(
            median, 
            p25 = quantile(., 0.25), 
            p75 = quantile(., 0.75)
        )
    )

vid_reviewer_med <- data_vidyo_scores %>%
    group_by(interviewer) %>%
    summarize_at(
        c("remarks", "score"), 
        funs(
            vid_rev_median = median, 
            vid_rev_p25 = quantile(., 0.25), 
            vid_rev_p75 = quantile(., 0.75)
        )
    ) %>%
    mutate(
        vid_score_adj = (all_vidyo_median$score_median - score_vid_rev_median) * 0.5
    )

vidyo_median <- data_vidyo_scores %>%
    left_join(
        vid_reviewer_med[c("interviewer", "vid_score_adj")],
        by = "interviewer"
    ) %>%
    mutate(score_adj = score + vid_score_adj) %>%
    group_by(cas_id) %>%
    summarize_at(
        c("remarks", "score", "score_adj"), 
        funs(
            vid_median = median, 
            vid_p25 = quantile(., 0.25),
            vid_p75 = quantile(., 0.75)
        )
    )

df_app_score <- data_app_scores %>%
    left_join(
        app_reviewer_med[c("reviewer", "app_score_adj")],
        by = "reviewer"
    ) %>%
    mutate(score_adj = score + app_score_adj) %>%
    select(cas_id, n, score_adj) %>%
    rename(app_score = n) %>%
    spread(app_score, score_adj, sep = "_")

df_app_remark <- data_app_scores %>%
    select(-reviewer, -score) %>%
    rename(app_remark = n) %>%
    spread(app_remark, remark, sep = "_") 

df_vidyo_score <- data_vidyo_scores %>%
    left_join(
        vid_reviewer_med[c("interviewer", "vid_score_adj")], 
        by = "interviewer"
    ) %>%
    mutate(score_adj = score + vid_score_adj) %>%
    select(cas_id, n, score_adj) %>%
    rename(vid_score = n) %>%
    spread(vid_score, score_adj, sep = "_") 

df_vidyo_remark <- data_vidyo_scores %>%
    select(-interviewer, -score) %>%
    rename(vid_remark = n) %>%
    spread(vid_remark, remarks, sep = "_") 

df_remark <- data_vidyo_scores %>%
    select(cas_id, remark = remarks) %>%
    bind_rows(data_app_scores[c("cas_id", "remark")]) %>%
    group_by(cas_id) %>%
    summarize_at(
        "remark", 
        funs(
            comb_remark_all = str_c(., collapse = ";"), 
            low_remarks = sum(. <= 2)
        )
    ) 

total_score <- data_demog %>%
    left_join(app_median, by = "cas_id") %>%
    left_join(vidyo_median, by = "cas_id") %>%
    left_join(df_app_score, by = "cas_id") %>%
    left_join(df_app_remark, by = "cas_id") %>%
    left_join(df_vidyo_score, by = "cas_id") %>%
    left_join(df_vidyo_remark, by = "cas_id") %>%
    left_join(df_remark, by = "cas_id") %>%
    group_by(cas_id) %>%
    mutate(
        app_name = str_c(last_name, first_name, sep = ", "),
        score = sum(
            score_vid_median, 
            score_app_median, 
            na.rm = TRUE
        ),
        score_adj = sum(
            score_adj_app_median,
            score_adj_vid_median, 
            na.rm = TRUE
        )
    ) %>%
    arrange(
        desc(score_adj),
        desc(score_adj_app_median), 
        desc(score_adj_vid_median)
    )

total_median <- total_score %>%
    ungroup() %>%
    summarize_at(
        "score", 
        funs(
            median, 
            p25 = quantile(., 0.25), 
            p75 = quantile(., 0.75)
        )
    )

data_interests <- total_score %>%
    select(cas_id, interest_1:interest_3) %>%
    gather(key, interest, interest_1:interest_3) %>%
    filter(
        !is.na(interest),
        interest != ""
    ) %>%
    separate(key, c(NA, "tmp_pref"), sep = "_") %>%
    mutate(
        preference = case_when(
            tmp_pref == 1 ~ 3,
            tmp_pref == 2 ~ 2,
            tmp_pref == 3 ~ 1
        )
    ) %>%
    select(-tmp_pref)

write_csv(total_score, "data/external/2020_applications.csv")

write_csv(data_app_scores, "data/external/2020_application_scores.csv")
write_csv(data_vidyo_scores, "data/external/2020_vidyo_scores.csv")
write_csv(data_interests, "data/external/2020_applicant_interests.csv")

openxlsx::write.xlsx(
    total_score,
    "data/external/2020_applications.xlsx"
)
