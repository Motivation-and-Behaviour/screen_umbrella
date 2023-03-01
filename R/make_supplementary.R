#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param combined_effects
#' @return
#' @author Taren Sanders
#' @export
make_supp_exposures <- function(combined_effects) {
  out <-
    combined_effects %>%
    transmute(plain_language_exposure = str_replace(
      plain_language_exposure,
      "^Intervention:",
      "Screen-based intervention:"
    )) %>%
    arrange(plain_language_exposure) %>%
    distinct()

  out_path <- here::here(
    "supplementary_files",
    "Supplementary File 4 - List of Exposures.csv"
  )
  readr::write_csv(out, file = out_path, col_names = FALSE)

  return(out_path)
}

#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param combined_effects
#' @return
#' @author Taren Sanders
#' @export
make_supp_effects <- function(combined_effects) {
  out_effects <-
    combined_effects %>%
    select(
      # Description
      author_year, outcome_category, plain_language_outcome,
      plain_language_exposure, age_group,
      # Raw data
      original_effect_size, original_effect_size_type, original_cilb,
      original_ciub, original_k, original_n, original_i2,
      # Converted data
      converted_r, converted_cilb, converted_ciub, converted_z,
      converted_cilb_z, converted_ciub_z,
      # Reanalysed data
      reanalysis_estimate, reanalysis_conf.low, reanalysis_conf.high,
      reanalysis_conf.low_999, reanalysis_conf.high_999, reanalysis_i2,
      reanalysis_n, reanalysis_k,
      # Eggers
      reanalysis_eggers_p, reanalysis_eggers_ci_l, reanalysis_eggers_ci_u,
      # Excess Significance
      reanalysis_tes_obser, reanalysis_tes_expect, reanalysis_tes_ratio,
      reanalysis_tes_p, reanalysis_tes_power, reanalysis_tes_theta
    )

  out_path <- here::here(
    "supplementary_files",
    "Supplementary File 2 - Complete Effects Data.csv"
  )

  readr::write_csv(out_effects, out_path)

  return(out_path)
}
