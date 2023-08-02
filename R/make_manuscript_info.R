#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param effects_clean
#' @param prisma
#' @param tables_df
#' @param combined_effects
#' @param studies_clean
#' @return
#' @author Taren Sanders
#' @export
make_manuscript_info <- function(effects_clean, prisma, tables_df,
                                 combined_effects, studies_clean) {
  manuscript_info <- list()

  # Results --------------------------------------------------

  ## List of frequent exposures
  manuscript_info$freq_exposures <-
    effects_clean %>%
    select(review_id, plain_language_outcome, plain_language_exposure) %>%
    # Each review can only have one combo, so as to not overcount moderators
    distinct() %>%
    group_by(plain_language_exposure) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    mutate(
      plain_language_exposure = rename_effects(plain_language_exposure),
      plain_language_exposure = case_when(
        plain_language_exposure ==
          "to promote health intervention" ~
          "screen-based interventions to promote health",
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
  manuscript_info$freq_outcomes <-
    effects_clean %>%
    select(review_id, plain_language_outcome, plain_language_exposure) %>%
    # Each review can only have one combo, so as to not overcount moderators
    distinct() %>%
    group_by(plain_language_outcome) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    mutate(
      plain_language_outcome = rename_effects(plain_language_outcome),
      plain_language_outcome = case_when(
        plain_language_outcome == "depression psychological health" ~
          "depression",
        TRUE ~ plain_language_outcome
      ),
      out = paste0(plain_language_outcome, " (*n* = ", n, ")")
    ) %>%
    head(4) %>%
    pull(out) %>%
    knitr::combine_words()

  ## Unique combos
  manuscript_info$unique_effects <- effects_clean %>%
    filter(use_effect) %>%
    nrow()


  ## Frequent combinations
  freq_combos <-
    effects_clean %>%
    select(review_id, plain_language_outcome, plain_language_exposure) %>%
    # Each review can only have one combo, so as to not overcount moderators
    distinct() %>%
    group_by(plain_language_exposure, plain_language_outcome) %>%
    summarise(n = n(), .groups = "keep") %>%
    arrange(desc(n))

  manuscript_info$unique_combos <- nrow(freq_combos)

  manuscript_info$freq_combos$n1 <- sum(freq_combos$n == 1)
  manuscript_info$freq_combos$n2 <- sum(freq_combos$n == 2)
  manuscript_info$freq_combos$ngt2 <- sum(freq_combos$n > 2)

  ## Number of studies and samples
  manuscript_info$effects_sum <-
    effects_clean %>%
    filter(use_effect) %>%
    group_by(review_id, age_group) %>%
    slice_max(original_n, with_ties = TRUE) %>%
    ungroup() %>%
    summarise(
      sum_k = format(sum(original_k, na.rm = TRUE), big.mark = ","),
      sum_n = format(sum(original_n), big.mark = ",")
    ) %>%
    as.list()

  ## Number meeting statistical certainty
  manuscript_info$effects_certain <- combined_effects %>%
    filter(use_effect & certainty == "meets criteria") %>%
    nrow()
  manuscript_info$reviews_certain <- combined_effects %>%
    filter(use_effect & certainty == "meets criteria") %>%
    distinct(review_id) %>%
    nrow()

  # PRISMA --------------------------------------------------
  manuscript_info$prisma$search <- prisma$data$value[1]
  manuscript_info$prisma$duplicates <- prisma$data$value[2]
  manuscript_info$prisma$full_text <- prisma$data$value[5]
  manuscript_info$prisma$included <- prisma$data$value[8]
  manuscript_info$prisma$unique_reviews <- prisma$data$value[11]

  # Quality Data ---------------------------------------------
  qual_data <-
    tables_df %>%
    filter(table == "main") %>%
    distinct(review_id, .keep_all = TRUE) %>%
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

  manuscript_info$qual$not_low <-
    manuscript_info$prisma$unique_reviews - manuscript_info$qual$all_low

  reviews_summary <-
    reviews_count %>%
    select(
      eligibility_criteria_predefined_and_specified:heterogeneity_assessed
    ) %>%
    mutate(
      across(
        eligibility_criteria_predefined_and_specified:heterogeneity_assessed,
        as.character
      )
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
    filter(outcome_category == "education" &
      certainty == "meets criteria" &
      use_effect)

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
      (reanalysis_eggers_p < 0.05 | is.na(reanalysis_eggers_p))) %>%
    nrow()

  manuscript_info$edu$n_failtest <-
    edu_effect %>%
    filter(
      source == "reanalysis" & n > 1000 & reanalysis_eggers_p > 0.05 &
        reanalysis_tes_p < 0.05
    ) %>%
    nrow()

  manuscript_info$edu$n_unique <- n_distinct(edu_certain$review_id)
  manuscript_info$edu$summary <-
    studies_clean %>%
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
    filter(outcome_category != "education" &
      certainty == "meets criteria" & use_effect)

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
      (reanalysis_eggers_p < 0.05 | is.na(reanalysis_eggers_p))) %>%
    nrow()

  manuscript_info$health$n_failtest <-
    health_effect %>%
    filter(source == "reanalysis" & n > 1000 &
      reanalysis_eggers_p > 0.05 & reanalysis_tes_p < 0.05) %>%
    nrow()

  manuscript_info$health$n_unique <-
    n_distinct(health_certain$review_id)

  manuscript_info$health$summary <-
    studies_clean %>%
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

  # Abstract Results ---------------------------------------------
  manuscript_info$abstract$social_media <-
    report_effect(combined_effects, "53160_001", "brackets")

  manuscript_info$abstract$edu <-
    report_effect(combined_effects, "61452_004", "none")

  manuscript_info$abstract$lit_gen <-
    report_effect(combined_effects, "47783_001", "brackets", first = TRUE)

  manuscript_info$abstract$tv_body <-
    report_effect(combined_effects, "8556_119", "brackets")

  manuscript_info$abstract$tv_edu <-
    report_effect(combined_effects, "47569_002", "brackets")

  manuscript_info$abstract$coview <-
    report_effect(combined_effects, "47783_022", "brackets")

  range_df <- combined_effects %>%
    filter(certainty == "meets criteria" & use_effect) %>%
    summarise(min = round(min(r), 2), max = round(max(r), 2))

  manuscript_info$abstract$range <- glue::glue(
    "(range: *r* = {range_df['min']}-{range_df['max']})"
  )

  manuscript_info$abstract$rob <-
    glue::glue("{manuscript_info$qual$not_low}/{manuscript_info$prisma$unique_reviews}") # nolint

  return(manuscript_info)
}

rename_effects <- Vectorize(function(cur_name) {
  if (str_detect(cur_name, ":")) {
    cur_name_split <- str_split(cur_name, ": ")
    return(str_to_lower(paste(
      cur_name_split[[1]][2],
      cur_name_split[[1]][1]
    )))
  } else {
    return(str_to_lower(cur_name))
  }
})
