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
      cilb = if_else(
        is.na(value_ci_lower_bound) & !is.na(value_raw_se),
        value - 1.96 * value_raw_se,
        value_ci_lower_bound
      ),
      ciub = if_else(
        is.na(value_ci_lower_bound) & !is.na(value_raw_se),
        value + 1.96 * value_raw_se,
        value_ci_lower_bound,
      ),
      r = case_when(
        is.na(value) ~ NA_real_,
        stat_test_clean == "r" ~ value,
        stat_test_clean == "b" ~ b2r(value),
        stat_test_clean == "d" ~ effectsize::d_to_r(value),
        stat_test_clean == "or" ~
          suppressWarnings(effectsize::oddsratio_to_r(value)),
        stat_test_clean == "z" ~ correlation::z_fisher(z = value)
      ),
      cilb = case_when(
        is.na(cilb) ~ NA_real_,
        stat_test_clean == "r" ~ cilb,
        stat_test_clean == "b" ~ b2r(cilb),
        stat_test_clean == "d" ~ effectsize::d_to_r(cilb),
        stat_test_clean == "or" ~
          suppressWarnings(effectsize::oddsratio_to_r(cilb)),
        stat_test_clean == "z" ~ correlation::z_fisher(z = cilb)
      ),
      ciub = case_when(
        is.na(ciub) ~ NA_real_,
        stat_test_clean == "r" ~ ciub,
        stat_test_clean == "b" ~ b2r(ciub),
        stat_test_clean == "d" ~ effectsize::d_to_r(ciub),
        stat_test_clean == "or" ~
          suppressWarnings(effectsize::oddsratio_to_r(ciub)),
        stat_test_clean == "z" ~ correlation::z_fisher(z = ciub)
      ),
      z = correlation::z_fisher(r = r),
      cilb_z = suppressWarnings(correlation::z_fisher(r = cilb)),
      ciub_z = suppressWarnings(correlation::z_fisher(r = ciub))
    )

  # The suppressed warnings are related to NaNs. Check that there were no new
  # NaNs introduced by the conversion
  stopifnot(
    length(is.na(data_converted$value)) == length(is.na(data_converted$r)),
    length(is.na(data_converted$value_ci_lower_bound)) <=
      length(is.na(data_converted$cilb)),
    length(is.na(data_converted$value_ci_upper_bound)) <=
      length(is.na(data_converted$ciub)),
    length(is.na(data_converted$cilb)) == length(is.na(data_converted$cilb_z)),
    length(is.na(data_converted$ciub)) == length(is.na(data_converted$ciub_z))
  )

  return(data_converted)
}
