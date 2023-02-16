# ------------------- SETTINGS -----------------------
authors_sheet <- "1V-j8oQXI3Y8etUnaRETvUvq9Hr8eAbAoBc_0XoLgb48"
article_title <- "Benefits and risks associated with children’s and adolescents’ interactions with electronic screens An umbrella review" # nolint

# ------------------- FUNCTIONS ----------------------

## Bibliography ----
# Create packages bibliography
create_packages_bib <- function(packages, bibpath) {
  knitr::write_bib(packages, bibpath)
  return(bibpath)
}

# Combine bibliography files
combine_bibs <- function(packages_bib, references_bib, reviews_raw, outpath) {
  # Fix UTF-8 encoding
  refs <- readLines(references_bib, encoding = "UTF-8")
  refs <- str_replace_all(refs, "[^[:graph:]]", " ")
  refs <- iconv(refs, from = "UTF-8", to = "ASCII//TRANSLIT")
  refs_tmp <- tempfile(fileext = ".bib")
  writeLines(refs, con = refs_tmp)

  paths <- c(packages_bib, refs_tmp)
  bibs <- lapply(paths, bib2df)
  full_bib <- bind_rows(bibs)

  full_bib <-
    full_bib %>%
    mutate(ANNOTE = if_else(BIBTEXKEY %in% reviews_raw$bibtex_key,
      "*",
      ""
    ))

  df2bib(full_bib, file = outpath)

  rm(refs_tmp)
  return(outpath)
}

# Upload to GDrive
upload_files <- function(target_file, type = "manuscript") {
  gdrive_path <- switch(type,
    "manuscript" =
      "https://drive.google.com/drive/folders/1WQiAUmDanOL2GPPBoYxUoNkHmZUjpbPj", # nolint
    "supp" =
      "https://drive.google.com/drive/folders/1gW5k2CIRJf1w2FP0dbUHCXwpAxtV1Gxp" # nolint
  )

  drive_val <- drive_put(
    target_file[1],
    path = as_id(gdrive_path)
  )

  return(drive_val)
}

# Make info required for manuscript
create_manuscript_info <- function(effects_clean, prisma, tables_df,
                                   combined_effects, studies_converted) {
  manuscript_info <- list()

  # Results --------------------------------------------------

  ## Number of effects
  manuscript_info$effects_clean_n <- nrow(effects_clean)

  ## Number of unique effects
  manuscript_info$unique_effects <- nrow(effects_clean %>% filter(use_effect))

  ## List of frequent exposures
  manuscript_info$freq_exposures <- effects_clean %>%
    group_by(plain_language_exposure) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    mutate(
      plain_language_exposure = rename_effects(plain_language_exposure),
      plain_language_exposure = case_when(
        plain_language_exposure ==
          "lifestyle risk behaviour (at school) intervention" ~
          "screen-based lifestyle risk behaviour interventions (at school)",
        plain_language_exposure == "general tv programs and movies" ~
          "general TV programs and movies",
        TRUE ~ plain_language_exposure
      ),
      out = paste0(plain_language_exposure, " (*n* = ", n, ")")
    ) %>%
    head(4) %>%
    pull(out) %>%
    knitr::combine_words()

  ## List of frequent outcomes
  manuscript_info$freq_outcomes <- effects_clean %>%
    group_by(plain_language_outcome) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    mutate(
      plain_language_outcome = rename_effects(plain_language_outcome),
      plain_language_outcome = case_when(
        plain_language_outcome == "duration sleep" ~ "sleep duration",
        TRUE ~ plain_language_outcome
      ),
      out = paste0(plain_language_outcome, " (*n* = ", n, ")")
    ) %>%
    head(5) %>%
    pull(out) %>%
    knitr::combine_words()

  ## Frequent combinations
  freq_combos <- effects_clean %>%
    group_by(plain_language_exposure, plain_language_outcome) %>%
    summarise(n = n()) %>%
    arrange(desc(n))

  manuscript_info$freq_combos$n1 <- sum(freq_combos$n == 1)
  manuscript_info$freq_combos$n2 <- sum(freq_combos$n == 2)
  manuscript_info$freq_combos$ngt2 <- sum(freq_combos$n > 2)

  ## Number of studies and samples
  manuscript_info$effects_sum <-
    effects_clean %>%
    filter(use_effect) %>%
    group_by(covidence_review_id, moderator_age) %>%
    slice_max(n, with_ties = TRUE) %>%
    ungroup() %>%
    summarise(
      sum_k = sum(k, na.rm = TRUE),
      sum_n = sum(n)
    ) %>%
    as.list()

  # PRISMA --------------------------------------------------
  manuscript_info$prisma$search <- prisma$data$value[1]
  manuscript_info$prisma$duplicates <- prisma$data$value[2]
  manuscript_info$prisma$full_text <- prisma$data$value[5]
  manuscript_info$prisma$included <- prisma$data$value[8]
  manuscript_info$prisma$unique_reviews <- prisma$data$value[9]

  # Quality Data ---------------------------------------------
  qual_data <-
    tables_df %>%
    filter(table == "main") %>%
    distinct(covidence_id, .keep_all = TRUE) %>%
    select(eligibility_criteria_predefined_and_specified:heterogeneity_assessed)

  reviews_count <-
    qual_data %>%
    mutate(
      across(
        eligibility_criteria_predefined_and_specified:heterogeneity_assessed,
        as.factor
      )
    ) %>%
    rowwise() %>%
    mutate(
      count_low = sum(
        c_across(
          eligibility_criteria_predefined_and_specified:heterogeneity_assessed
        ) == "low"
      ),
      count_high = sum(
        c_across(
          eligibility_criteria_predefined_and_specified:heterogeneity_assessed
        ) == "high"
      ),
      count_high_unclear = sum(
        c_across(
          eligibility_criteria_predefined_and_specified:heterogeneity_assessed
        ) %in% c("high", "unclear")
      )
    )

  qual_data_n <- nrow(qual_data)

  manuscript_info$qual$all_low <- reviews_count %>%
    filter(count_low == 7) %>%
    nrow()

  manuscript_info$qual$all_high <- reviews_count %>%
    filter(count_high == 7) %>%
    nrow()

  manuscript_info$qual$all_high_unclear <- reviews_count %>%
    filter(count_high_unclear == 7) %>%
    nrow()

  reviews_summary <-
    reviews_count %>%
    select(
      eligibility_criteria_predefined_and_specified:heterogeneity_assessed
    ) %>%
    gather(name, value) %>%
    count(name, value) %>%
    arrange(desc(n))


  qual_count <- function(df, name, value) {
    df[df$name == name &
      df$value == value, "n"][[1]]
  }

  qual_str <- function(n, totaln = qual_data_n,
                       ntype = "", prefix = "(*n*", suffix = ")") {
    paste0(
      prefix, ntype, "= ", n, "/",
      totaln, ", ", scales::percent(n / totaln, accuracy = 1), suffix
    )
  }

  manuscript_info$qual$low_hetero <- qual_str(
    qual_count(reviews_summary, "heterogeneity_assessed", "low"),
    qual_data_n,
    " low risk ",
    suffix = " of meta-analyses)"
  )

  manuscript_info$qual$low_char <- qual_str(
    qual_count(
      reviews_summary,
      "included_studies_listed_with_important_characteristics_and_results_of_each", # nolint
      "low"
    ),
    qual_data_n,
    " low risk "
  )

  manuscript_info$qual$low_search <- qual_str(
    qual_count(
      reviews_summary,
      "literature_search_strategy_comprehensive_and_systematic", "low"
    ),
    qual_data_n,
    " low risk "
  )

  manuscript_info$qual$unclear_elig <- qual_str(
    qual_count(
      reviews_summary, "eligibility_criteria_predefined_and_specified",
      "unclear"
    ),
    qual_data_n,
    " unclear "
  )

  manuscript_info$qual$high_dualscreen <- qual_str(
    qual_count(reviews_summary, "dual_independent_screening_review", "high"),
    qual_data_n,
    " high risk "
  )

  manuscript_info$qual$unclear_dualscreen <- qual_str(
    qual_count(reviews_summary, "dual_independent_screening_review", "unclear"),
    qual_data_n,
    " unclear "
  )

  manuscript_info$qual$dualqualstr <- paste0(
    qual_str(
      qual_count(
        reviews_summary, "dual_independent_quality_assessment",
        "high"
      ),
      qual_data_n,
      " high risk ",
      suffix = "; "
    ),
    qual_str(
      qual_count(
        reviews_summary, "dual_independent_quality_assessment",
        "unclear"
      ),
      qual_data_n,
      " high risk ",
      prefix = "n",
      suffix = ")"
    )
  )

  # Education Results ---------------------------------------------
  edu_effect <-
    combined_effects %>%
    filter(outcome_category == "education" & use_effect)

  edu_certain <-
    combined_effects %>%
    filter(outcome_category == "education" & certainty == "meets criteria")

  manuscript_info$edu$n_effect <- edu_effect %>% nrow()
  manuscript_info$edu$n_certain <- edu_certain %>% nrow()

  manuscript_info$edu$n_noindiv <-
    edu_effect %>%
    filter(source == "reported") %>%
    nrow()

  manuscript_info$edu$n_samplesize <-
    edu_effect %>%
    filter(source == "reanalysis" & n < 1000) %>%
    nrow()

  manuscript_info$edu$n_faileggers <-
    edu_effect %>%
    filter(source == "reanalysis" & n > 1000 &
      (eggers_p < 0.05 | is.na(eggers_p))) %>%
    nrow()

  manuscript_info$edu$n_failtest <-
    edu_effect %>%
    filter(
      source == "reanalysis" & n > 1000 & eggers_p > 0.05 & tes_p < 0.05
    ) %>%
    nrow()

  manuscript_info$edu$n_unique <- n_distinct(edu_certain$covidence_review_id)
  manuscript_info$edu$summary <-
    studies_converted %>%
    filter(effect_size_id %in% edu_certain$effect_size_id) %>%
    group_by(study_author, study_year, study_first_page_number) %>%
    slice_max(study_n, with_ties = FALSE) %>%
    ungroup() %>%
    summarise(
      studies_n = n(),
      total_n = sum(study_n)
    ) %>%
    as.list()

  manuscript_info$edu$n_p999 <- edu_certain %>%
    filter(reanalysis_p.value < 0.001) %>%
    nrow()
  manuscript_info$edu$n_p95 <- edu_certain %>%
    filter(reanalysis_p.value < 0.05) %>%
    nrow() - manuscript_info$edu$n_p999

  manuscript_info$edu$n_i2 <- edu_certain %>%
    filter(i2 > 50) %>%
    nrow()

  # Health Results ---------------------------------------------
  health_effect <-
    combined_effects %>%
    filter(outcome_category != "education" & use_effect)

  health_certain <-
    combined_effects %>%
    filter(outcome_category != "education" & certainty == "meets criteria")

  manuscript_info$health$n_effect <- health_effect %>% nrow()
  manuscript_info$health$n_certain <- health_certain %>% nrow()

  manuscript_info$health$n_noindiv <-
    health_effect %>%
    filter(source == "reported") %>%
    nrow()

  manuscript_info$health$n_samplesize <-
    health_effect %>%
    filter(source == "reanalysis" & n < 1000) %>%
    nrow()

  manuscript_info$health$n_faileggers <-
    health_effect %>%
    filter(source == "reanalysis" & n > 1000 &
      (eggers_p < 0.05 | is.na(eggers_p))) %>%
    nrow()

  manuscript_info$health$n_failtest <-
    health_effect %>%
    filter(source == "reanalysis" & n > 1000 &
      eggers_p > 0.05 & tes_p < 0.05) %>%
    nrow()

  manuscript_info$health$n_unique <-
    n_distinct(health_certain$covidence_review_id)

  manuscript_info$health$summary <-
    studies_converted %>%
    filter(effect_size_id %in% health_certain$effect_size_id) %>%
    group_by(study_author, study_year, study_first_page_number) %>%
    slice_max(study_n, with_ties = FALSE) %>%
    ungroup() %>%
    summarise(
      studies_n = n(),
      total_n = sum(study_n)
    ) %>%
    as.list()

  manuscript_info$health$n_p999 <- health_certain %>%
    filter(reanalysis_p.value < 0.001) %>%
    nrow()

  manuscript_info$health$n_p95 <- health_certain %>%
    filter(reanalysis_p.value < 0.05) %>%
    nrow() - manuscript_info$health$n_p999

  manuscript_info$health$n_i2 <- health_certain %>%
    filter(i2 > 50) %>%
    nrow()
  manuscript_info$health$n_r2 <- health_certain %>%
    filter(abs(r) < 0.2) %>%
    nrow()

  return(manuscript_info)
}

rename_effects <- function(cur_name) {
  if (str_detect(cur_name, ":")) {
    cur_name_split <- str_split(cur_name, ": ")
    return(str_to_lower(paste(
      cur_name_split[[1]][2],
      cur_name_split[[1]][1]
    )))
  } else {
    return(str_to_lower(cur_name))
  }
}
rename_effects <- Vectorize(rename_effects)
