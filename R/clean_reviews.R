#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param reviews_raw
#' @param effects_raw
#' @param age_codes
#' @param demo_codes
#' @param sample_codes
#' @param design_codes
#' @return
#' @author Taren Sanders
#' @export
clean_reviews <- function(
    reviews_raw, effects_raw, age_codes,
    demo_codes, sample_codes, design_codes) {
  zero_effects_ids <-
    effects_raw %>%
    group_by(review_id) %>%
    summarise(n_valid_effects = sum(
      !is.na(value) & !is.na(combined_n) & use_moderator
    )) %>%
    filter(n_valid_effects == 0) %>%
    pull(review_id)

  reviews_clean <-
    reviews_raw %>%
    mutate(
      demographics_coded = case_when(
        demographics %in% age_codes$mixed ~ "Mixed",
        demographics %in% age_codes$adolescents ~ "Adolescents",
        demographics %in% age_codes$children ~ "Children",
        demographics %in% age_codes$young_children ~ "Young children",
        TRUE ~ demographics
      ),
      # Tidy up the sample age variables
      sample_age_lowest_study_mean =
        as.numeric(case_when(
          grepl("months", sample_age_lowest_study_mean) ~
            as.character(as.numeric(
              str_remove(sample_age_lowest_study_mean, " months")
            ) / 12),
          TRUE ~ sample_age_lowest_study_mean
        )),
      sample_age_highest_study_mean =
        as.numeric(case_when(
          grepl("months", sample_age_highest_study_mean) ~
            as.character(as.numeric(
              str_remove(sample_age_highest_study_mean, " months")
            ) / 12),
          TRUE ~ sample_age_highest_study_mean
        )),
      # Double check the coding based on the age variables
      demographics_coded = case_when(
        sample_age_lowest_study_mean < 8 & sample_age_highest_study_mean > 16 ~
          "Mixed",
        sample_age_lowest_study_mean < 1 & sample_age_highest_study_mean > 10 ~
          "Mixed",
        sample_age_lowest_study_mean >= 12 ~ "Adolescents",
        sample_age_highest_study_mean < 7 ~ "Young children",
        sample_age_lowest_study_mean >= 4 & sample_age_highest_study_mean < 13 ~
          "Children",
        TRUE ~ demographics_coded
      )
    ) %>%
    # Fix the demographics variable for the tables
    mutate(
      demographics = case_when( # nolint start
        demographics %in% demo_codes$demo_all ~ "All",
        demographics %in% demo_codes$demo_adol ~ "Adolescents",
        demographics %in% demo_codes$demo_child ~ "Children",
        demographics %in% demo_codes$demo_child_adol ~ "Children; Adolescents",
        demographics %in% demo_codes$demo_early ~ "Early childhood; Pre-school",
        demographics %in% demo_codes$demo_early_child ~ "Early childhood; Pre-school; School-age Children (Early Primary, Elementary)",
        demographics %in% demo_codes$demo_sch_all ~ "School-age Children",
        demographics %in% demo_codes$demo_sch_elm ~ "School-age Children (Primary, Elementary)",
        demographics %in% demo_codes$demo_sch_early_elm ~ "School-age Children (Early Primary, Elementary)",
        demographics %in% demo_codes$demo_sch_elm_mid ~ "School-age Children (Primary, Elementary, Middle School)",
        demographics %in% demo_codes$demo_sch_elm_mid ~ "School-age Children (High School)",
        demographics %in% demo_codes$demo_sch_mid ~ "School-age Children (Middle School)",
        demographics %in% demo_codes$demo_sch_mid_high ~ "School-age Children (Middle, High School)",
        TRUE ~ demographics # nolint end
      )
    ) %>%
    # Add incl/excl criteria
    mutate(across(
      c(health_condition, health_behaviour, education, psychological),
      ~ if_else(.x %in% na_codes, NA, .x)
    )) %>%
    mutate(
      sample_incl_health_cond =
        str_to_sentence(str_extract(health_condition, "(?<=Include: )(.*)")),
      sample_incl_health_beh =
        str_to_sentence(str_extract(health_behaviour, "(?<=Include: )(.*)")),
      sample_incl_edu =
        str_to_sentence(str_extract(education, "(?<=Include: )(.*)")),
      sample_incl_psych =
        str_to_sentence(str_extract(psychological, "(?<=Include: )(.*)"))
    ) %>%
    unite(
      sample_incl, sample_incl_health_cond:sample_incl_psych,
      sep = "\n", na.rm = TRUE
    ) %>%
    mutate(
      sample_excl_health_cond =
        str_to_sentence(str_extract(health_condition, "(?<=Exclude: )(.*)")),
      sample_excl_health_beh =
        str_to_sentence(str_extract(health_behaviour, "(?<=Exclude: )(.*)")),
      sample_excl_edu =
        str_to_sentence(str_extract(education, "(?<=Exclude: )(.*)")),
      sample_excl_psych =
        str_to_sentence(str_extract(psychological, "(?<=Exclude: )(.*)"))
    ) %>%
    unite(
      sample_excl, sample_excl_health_cond:sample_excl_psych,
      sep = "\n", na.rm = TRUE
    ) %>%
    mutate(across(c(sample_incl, sample_excl), ~ if_else(. == "", NA, .))) %>%
    mutate(sample_type = if_else(
      sample_incl %in% sample_codes$clin_sample_incl |
        sample_incl %in% sample_codes$edu_sample_incl,
      sample_incl, "General"
    )) %>%
    # Add study design
    mutate(
      study_design = case_when( # nolint start
        study_design_restrictions %in% design_codes$design_unspec ~ "Mixed or unspecified",
        study_design_restrictions %in% design_codes$design_obs_mixed ~ "Observational - mixed",
        study_design_restrictions %in% design_codes$design_obs_cross ~ "Cross-sectional only",
        study_design_restrictions %in% design_codes$design_obs_long ~ "Longitudinal only",
        study_design_restrictions %in% design_codes$design_exp ~ "Experimental",
        TRUE ~ study_design_restrictions # nolint end
      )
    ) %>%
    # Add some other helpful columns
    mutate(
      author_year = paste(first_author, ", ", year_of_publication, sep = ""),
      no_valid_effects = if_else(review_id %in% zero_effects_ids, TRUE, FALSE)
    )

  return(reviews_clean)
}
