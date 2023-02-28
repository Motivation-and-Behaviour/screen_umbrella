#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param effects_raw
#' @param reviews_clean
#' @return
#' @author Taren Sanders
#' @export
clean_effects <- function(effects_raw, reviews_clean) {
  effects_clean <-
    effects_raw %>%
    # Remove any unusable effects (must have value and N)
    filter(!is.na(value) & !is.na(combined_n)) %>%
    # Translate the statistical tests to common abbreviations
    mutate(stat_test_clean = translate_tests(statistical_test)) %>%
    # Can only use some of the metric types
    filter(stat_test_clean %in% c("b", "d", "r", "or", "z")) %>%
    convert_effects() %>%
    rename(
      outcome_category = outcome_level_1,
      outcome = outcome_level_2,
      moderator_level = moderator_level_recoded,
      moderator_category = moderator_category_recoded,
      k = k_number_of_effects_informing_this_test,
      n = combined_n,
      i2 = i2_calculated,
      plain_language_outcome = outcome_plain_language_descriptor,
      raw_value = value,
      raw_cilb = value_ci_lower_bound,
      raw_ciub = value_ci_upper_bound
    ) %>%
    filter(use_moderator) %>%
    # Add the demographics in from the review-level data
    left_join(
      select(reviews_clean, review_id, author_year, demographics_coded),
      by = "review_id"
    ) %>%
    rename(moderator_age = demographics_coded)

  # Check for ages within the normal moderators and change moderator_age
  age_moderator_categories <- c(
    "age", "Age", "Educational level", "Grade Level",
    "School level", "Level of education"
  )
  effects_clean <- effects_clean %>%
    mutate(moderator_age = case_when(
      moderator_category %in% age_moderator_categories &
        moderator_level %in% mixed_codes ~ "mixed",
      moderator_category %in% age_moderator_categories &
        moderator_level %in% adolescents_codes ~ "adolescents",
      moderator_category %in% age_moderator_categories &
        moderator_level %in% children_codes ~ "children",
      moderator_category %in% age_moderator_categories &
        moderator_level %in% young_children_codes ~ "young_children",
      TRUE ~ moderator_age
    ))

  # Pick the largest review by N for each outcome/exposure pair and age group
  effects_use <- effects_clean %>%
    group_by(
      plain_language_outcome,
      plain_language_exposure,
      moderator_age
    ) %>%
    slice_max(n, with_ties = TRUE) %>%
    mutate(
      use_effect = TRUE,
      main_effect = TRUE
    ) %>%
    ungroup() %>%
    select(effect_size_id, use_effect, main_effect)

  effects_clean_use <-
    left_join(effects_clean, effects_use, by = "effect_size_id")

  effects_clean_renamed <-
    effects_clean_use %>%
    rename(
      age_group = moderator_age,
      # Raw data
      original_effect_size = raw_value,
      original_effect_size_type = statistical_test,
      original_cilb = raw_cilb,
      original_ciub = raw_ciub,
      original_k = k,
      original_n = n,
      original_i2 = i2,
      original_p_value = p_value,
      original_se = value_raw_se,
      original_sd = value_raw_sd,
      # Converted data
      converted_r = r,
      converted_cilb = cilb,
      converted_ciub = ciub,
      converted_z = z,
      converted_cilb_z = cilb_z,
      converted_ciub_z = ciub_z
    )

  return(effects_clean_renamed)
}
