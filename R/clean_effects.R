#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param effects
#' @param reviews
#' @return
#' @author Taren Sanders
#' @export
clean_effects <- function(effects, reviews) {
  effects_clean <-
    effects %>%
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
    filter(use_moderator)


  # Clean the names of the datafile, rename to something more meaningful,
  # remove empty stuff, then cut small studies or rubbish

  # Create the age moderation check
  revs_dems <- revs %>%
    select(covidence_review_id_auto, demographics_consensus)

  q <- left_join(q, revs_dems, by = c(
    "covidence_review_id" =
      "covidence_review_id_auto"
  )) %>%
    mutate(moderator_age = case_when(
      demographics_consensus %in% c(
        "All", "Children; Adolescents", "School-age Children"
      ) ~ "All",
      demographics_consensus %in% c(
        "Adolescents",
        "School-age Children (Middle/High School)",
        "School-age_High School", "school-age_high school",
        "School-age Children (Middle School)"
      ) ~ "Adolescents",
      demographics_consensus %in% c("Early childhood/pre-school") ~
        "Young children",
      TRUE ~ "Children"
    )) %>%
    mutate(moderator_age = case_when(
      moderator_level %in% c(
        "13-18", "12-17 years",
        "adolescence", "at least 13 years"
      ) ~ "Adolescents",
      moderator_level %in% c(
        "7-12", "children", "older than 8", "6-11 years",
        "grades 4-7", "childhood", "12 or younger"
      ) ~ "Children",
      moderator_level %in% c(
        "0-6", "younger than 8", "Infants", "pre-school",
        "Toddlers", "Preschoolers"
      ) ~ "Young children",
      TRUE ~ moderator_age
    ))

  # Add significance and labels
  q$sig <- ((q$cilb * q$ciub) > 0)
  q$sig <- q$sig * .7 + .3
  q$author_year <- paste(q$first_author, ", ", q$year_of_publication, sep = "")

  # bold the rows that are classified as 'risks'
  q$risk <- ifelse(q$benefit_or_risk == "Risk", "bold", "plain")

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
