#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param combined_effects
#' @return
#' @author Taren Sanders
#' @export
make_table_effects <- function(combined_effects) {
  combined_effects %>%
    mutate(
      plain_language_exposure = str_replace(
        plain_language_exposure,
        "^Intervention:",
        "Screen-based intervention:"
      ),
      outcome_lvl_1 = factor(gsub(":.*", "", plain_language_outcome)),
      exposure_lvl_1 = factor(gsub(":.*", "", plain_language_exposure)),
      review_year = factor((gsub(".*, ", "", author_year))),
      outcome_category = str_to_title(outcome_category),
      certainty = str_to_title(certainty),
      use_effect = factor(if_else(use_effect, "Used", "Not Used"))
    ) %>%
    select(
      review_year, outcome_category, outcome_lvl_1, exposure_lvl_1, k, n,
      age_group, sample_type, study_design, study_level_available, certainty,
      use_effect
    ) %>%
    tbl_summary(
      by = use_effect,
      label = list(
        review_year ~ "Review Year",
        outcome_category ~ "Outcome Category",
        outcome_lvl_1 ~ "Broad Outcome",
        exposure_lvl_1 ~ "Broad Exposure",
        k ~ "Number of Contributing Studies",
        n ~ "Pooled Sample Size",
        age_group ~ "Age Group",
        sample_type ~ "Sample Type",
        study_design ~ "Study Design",
        study_level_available ~ "Study-level Data Available",
        certainty ~ "Meets Statistical Certainty Criteria"
      ),
      missing_text = "(missing)"
    ) %>%
    modify_header(
      label ~ "**Variable**"
    ) %>%
    modify_spanning_header(c("stat_1", "stat_2") ~ "**Effect Size Used**") %>%
    bold_labels() %>%
    as_gt() %>%
    tab_header(
      title = html("<strong>Effect Size Characteristics</strong>"),
      subtitle = "Characteristics of included and excluded effect sizes"
    )
}
