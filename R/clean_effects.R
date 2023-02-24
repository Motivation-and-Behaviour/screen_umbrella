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

  # TODO: Check for ages within the normal moderators and change moderator_age

  age_moderator_categories <- c(
    "age", "Age", "Educational level", "Learning domain", "Grade Level",
    "School level"
  )

  # These are the distinct age based moderators
  effects_clean %>%
    select(
      review_id, effect_size_id, moderator_age, moderator_level,
      moderator_category
    ) %>%
    filter(moderator_category %in% c(
      "age", "Age", "Educational level", "Learning domain", "Grade Level",
      "School level"
    )) %>%
    distinct(moderator_level) %>%
    dput()

  new_codes <- c(
    "13-18", "7-12", "0-6", "children",
    "older than 8", "younger than 8", "12-17 years", "pre-school",
    "6-11 years", "grades 4-7", "adolescence", "childhood", "Infants",
    "Toddlers", "Preschoolers", "at least 13 years", "younger than 13 years",
    "12 or younger", "7-17 years", "18 or younger", "Middle School",
    "<18", "Children", "Adolescents", "Pre/elementary", "Secondary",
    "<36 months", ">=36 months", "Imitation", "Language learning",
    "Object retrieval", "Other", "Elementary", "Primary", "Preschool",
    "K-3", "Kindergarten", "High School", "Elementary & Kindergarten",
    "Secondary School", "Preparatory education", "<=14", ">14", "Elementary school",
    "Middle/High school"
  )

  new_check <- new_codes[!new_codes %in% c(mixed_codes, adolescents_codes, children_codes, young_children_codes)] %>% dput()

  effects_clean %>%
    select(
      review_id, effect_size_id, moderator_age, moderator_level,
      moderator_category
    ) %>%
    filter(moderator_level %in% c(new_check, mixed_codes, adolescents_codes, children_codes, young_children_codes) & moderator_category %in% age_moderator_categories) %>%
    distinct()



  # if one effect from this review, keep or select "overall"
  # group by study_id and exposure and outcome, pick max n
  q_use <- q %>%
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
    ungroup()

  q <- left_join(q, select(
    q_use,
    effect_size_id_1,
    use_effect, main_effect
  ), by = "effect_size_id_1") %>%
    select(
      author_year, covidence_review_id,
      outcome_category, effect_size_id_1,
      plain_language_outcome,
      plain_language_exposure,
      moderator_level,
      moderator_category,
      risk,
      k, n, r, cilb, ciub, i2, sig, use_effect, main_effect, moderator_age,
      starts_with("raw_"), starts_with("converted_"), std_eff_name
    )

  return(q)
}
