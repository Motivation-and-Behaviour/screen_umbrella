#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param reviews_raw
#' @param effects_raw
#' @return
#' @author Taren Sanders
#' @export
clean_reviews <- function(reviews_raw, effects_raw) {
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
        demographics %in% mixed_codes ~ "Mixed",
        demographics %in% adolescents_codes ~ "Adolescents",
        demographics %in% children_codes ~ "Children",
        demographics %in% young_children_codes ~ "Young children",
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
    # Add some other helpful columns
    mutate(
      author_year = paste(first_author, ", ", year_of_publication, sep = ""),
      no_valid_effects = if_else(review_id %in% zero_effects_ids, TRUE, FALSE)
    )

  return(reviews_clean)
}
