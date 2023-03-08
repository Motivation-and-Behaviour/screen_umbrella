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
        is.na(raw_cilb) & !is.na(value_raw_se),
        raw_value - 1.96 * value_raw_se,
        raw_cilb
      ),
      ciub = if_else(
        is.na(raw_cilb) & !is.na(value_raw_se),
        raw_value + 1.96 * value_raw_se,
        raw_cilb,
      ),
      r = case_when(
        is.na(raw_value) ~ NA_real_,
        stat_test_clean == "r" ~ raw_value,
        stat_test_clean == "b" ~ b2r(raw_value),
        stat_test_clean == "d" ~ effectsize::d_to_r(raw_value),
        stat_test_clean == "z" ~ correlation::z_fisher(z = raw_value)
      ),
      cilb = case_when(
        is.na(cilb) ~ NA_real_,
        stat_test_clean == "r" ~ cilb,
        stat_test_clean == "b" ~ b2r(cilb),
        stat_test_clean == "d" ~ effectsize::d_to_r(cilb),
        stat_test_clean == "z" ~ correlation::z_fisher(z = cilb)
      ),
      ciub = case_when(
        is.na(ciub) ~ NA_real_,
        stat_test_clean == "r" ~ ciub,
        stat_test_clean == "b" ~ b2r(ciub),
        stat_test_clean == "d" ~ effectsize::d_to_r(ciub),
        stat_test_clean == "z" ~ correlation::z_fisher(z = ciub)
      ),
      z = correlation::z_fisher(r = r),
      cilb_z = suppressWarnings(correlation::z_fisher(r = cilb)),
      ciub_z = suppressWarnings(correlation::z_fisher(r = ciub))
    )

  # The suppressed warnings are related to NaNs. Check that there were no new
  # NaNs introduced by the conversion
  stopifnot(
    length(is.na(data_converted$raw_value)) == length(is.na(data_converted$r)),
    length(is.na(data_converted$raw_cilb)) <=
      length(is.na(data_converted$cilb)),
    length(is.na(data_converted$raw_cilb)) <=
      length(is.na(data_converted$ciub)),
    length(is.na(data_converted$cilb)) == length(is.na(data_converted$cilb_z)),
    length(is.na(data_converted$ciub)) == length(is.na(data_converted$ciub_z))
  )

  return(data_converted)
}
