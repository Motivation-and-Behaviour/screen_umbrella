#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param studies_raw
#' @return
#' @author Taren Sanders
#' @export
clean_studies <- function(studies_raw) {
  studies_clean <-
    studies_raw %>%
    mutate(study_name = paste(study_author, study_year)) %>%
    # Impute missing N. TODO: Test something other than mean imputation
    group_by(effect_size_id) %>%
    mutate(
      study_n = replace_na(study_n, round(mean(study_n, na.rm = TRUE)))
    ) %>%
    ungroup()

  data <- studies_clean %>%
    mutate(converted_metric = translate_tests(metric)) %>%
    filter(converted_metric %in% c("b", "d", "r", "or", "z", "md")) %>%
    convert_studies() %>%
    drop_na(r_estimate, study_n) %>%
    group_by(effect_size_id) %>%
    tar_group()
}
