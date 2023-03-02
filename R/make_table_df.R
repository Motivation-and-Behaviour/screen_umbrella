#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param rob_raw
#' @param effects_clean
#' @param reviews_clean
#' @return
#' @author Taren Sanders
#' @export
make_table_df <- function(rob_raw, effects_clean, reviews_clean) {
  rob <- select(rob_raw, -reviewer)

  effects <- select(
    effects_clean,
    review_id, plain_language_exposure, plain_language_outcome, use_effect
  )

  reviews <- reviews_clean %>%
    filter(!no_valid_effects) %>%
    transmute(
      review_id = review_id,
      first_author = first_author,
      year = year_of_publication,
      design_restrictions = study_design_restrictions,
      regions_restrictions = regions,
      demographics_restrictions = demographics,
      earliest_study_year = earliest_study_publication_year,
      latest_study_year = latest_study_publication_year,
      sample_age_mean_low = sample_age_lowest_study_mean,
      sample_age_mean_high = sample_age_highest_study_mean
    )

  use_effects_ids <-
    effects %>%
    filter(use_effect) %>%
    distinct(review_id) %>%
    pull(review_id)

  joined_df <-
    reviews %>%
    # Merge the datasets
    left_join(rob, by = "review_id") %>%
    left_join(effects, by = "review_id", multiple = "all") %>%
    mutate(table = if_else(review_id %in% use_effects_ids, "main", "supp"))

  return(joined_df)
}
