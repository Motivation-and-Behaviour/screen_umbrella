#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data
#' @return
#' @author Taren Sanders
#' @export
convert_effects <- function(data) {
  data_converted <- data %>%
    mutate(
      converted_ci_lower_bound = if_else(
        is.na(value_ci_lower_bound) & !is.na(value_raw_se),
        value - 1.96 * value_raw_se,
        value_ci_lower_bound
      ),
      converted_ci_upper_bound = if_else(
        is.na(value_ci_lower_bound) & !is.na(value_raw_se),
        value - 1.96 * value_raw_se,
        value_ci_lower_bound,
      ),
      converted_value = case_when(
        is.na(value) ~ NA_real_,
        stat_test_clean == "r" ~ value,
        stat_test_clean == "b" ~ b2r(value),
        stat_test_clean == "d" ~ effectsize::d_to_r(value),
        stat_test_clean == "or" ~ effectsize::oddsratio_to_r(value),
        stat_test_clean == "z" ~ correlation::z_fisher(z = value)
      ),
      converted_ci_lower_bound = case_when(
        is.na(converted_ci_lower_bound) ~ NA_real_,
        stat_test_clean == "r" ~ converted_ci_lower_bound,
        stat_test_clean == "b" ~ b2r(converted_ci_lower_bound),
        stat_test_clean == "d" ~ effectsize::d_to_r(converted_ci_lower_bound),
        stat_test_clean == "or" ~
          effectsize::oddsratio_to_r(converted_ci_lower_bound),
        stat_test_clean == "z" ~
          correlation::z_fisher(z = converted_ci_lower_bound)
      ),
      converted_ci_upper_bound = case_when(
        is.na(converted_ci_upper_bound) ~ NA_real_,
        stat_test_clean == "r" ~ converted_ci_upper_bound,
        stat_test_clean == "b" ~ b2r(converted_ci_upper_bound),
        stat_test_clean == "d" ~ effectsize::d_to_r(converted_ci_upper_bound),
        stat_test_clean == "or" ~
          effectsize::oddsratio_to_r(converted_ci_upper_bound),
        stat_test_clean == "z" ~
          correlation::z_fisher(z = converted_ci_upper_bound)
      ),
      converted_value_z = correlation::z_fisher(r = converted_value),
      converted_ci_lower_bound_z =
        correlation::z_fisher(r = converted_ci_lower_bound),
      converted_ci_upper_bound_z =
        correlation::z_fisher(r = converted_ci_upper_bound)
    )

  return(data_converted)
}
