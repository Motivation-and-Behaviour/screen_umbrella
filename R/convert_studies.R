#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data
#' @return
#' @author Taren Sanders
#' @export
convert_studies <- function(data) {
  data_converted <- data %>%
    mutate(
      r_ci_lower = if_else(
        is.na(lower_ci) & !is.na(standard_error),
        estimate - 1.96 * standard_error,
        lower_ci
      ),
      r_ci_upper = if_else(
        is.na(upper_ci) & !is.na(standard_error),
        estimate + 1.96 * standard_error,
        upper_ci
      ),
      r_estimate = case_when(
        converted_metric == "r" ~ estimate,
        converted_metric == "b" ~ b2r(estimate),
        converted_metric == "d" ~ effectsize::d_to_r(estimate),
        converted_metric == "z" ~ correlation::z_fisher(z = estimate),
        # Calculate d from mean difference where possible
        converted_metric == "md" & !is.na(estimate) &
          !is.na(standard_deviation) ~
          effectsize::d_to_r(estimate / standard_deviation),
        TRUE ~ NA_real_
      ),
      r_ci_lower = case_when(
        converted_metric == "r" ~ lower_ci,
        converted_metric == "b" ~ b2r(lower_ci),
        converted_metric == "d" ~ effectsize::d_to_r(lower_ci),
        converted_metric == "z" ~ correlation::z_fisher(z = lower_ci),
        # Calculate d from mean difference where possible
        converted_metric == "md" & !is.na(lower_ci) &
          !is.na(standard_deviation) ~
          effectsize::d_to_r(lower_ci / standard_deviation),
        TRUE ~ NA_real_
      ),
      r_ci_upper = case_when(
        converted_metric == "r" ~ upper_ci,
        converted_metric == "b" ~ b2r(upper_ci),
        converted_metric == "d" ~ effectsize::d_to_r(upper_ci),
        converted_metric == "z" ~ correlation::z_fisher(z = upper_ci),
        # Calculate d from mean difference where possible
        converted_metric == "md" & !is.na(upper_ci) &
          !is.na(standard_deviation) ~
          effectsize::d_to_r(upper_ci / standard_deviation),
        TRUE ~ NA_real_
      ),
      z_estimate = correlation::z_fisher(r = r_estimate),
      z_ci_lower = suppressWarnings(correlation::z_fisher(r = r_ci_lower)),
      z_ci_upper = suppressWarnings(correlation::z_fisher(r = r_ci_upper))
    )

  # The suppressed warnings are related to NaNs. Check that there were no new
  # NaNs introduced by the conversion
  stopifnot(
    length(is.na(data_converted$r_estimate)) ==
      length(is.na(data_converted$estimate)),
    length(is.na(data_converted$r_ci_lower)) <=
      length(is.na(data_converted$lower_ci)),
    length(is.na(data_converted$r_ci_upper)) <=
      length(is.na(data_converted$upper_ci))
  )

  return(data_converted)
}
