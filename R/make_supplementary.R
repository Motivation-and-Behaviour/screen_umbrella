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
    "Supplementary File 1 - List of Exposures.csv"
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
    rename(
      reanalysis_cilb = reanalysis_conf.low,
      reanalysis_ciub = reanalysis_conf.high,
      reanalysis_cilb999 = reanalysis_conf.low_999,
      reanalysis_ciub999 = reanalysis_conf.high_999,
      reanalysis_eggers_cilb = reanalysis_eggers_ci_l,
      reanalysis_eggers_ciub = reanalysis_eggers_ci_u,
      original_effect_size_metric = stat_test_clean
    ) %>%
    select(
      # Description
      author_year, outcome_category, plain_language_outcome,
      plain_language_exposure, age_group,
      # Raw data
      original_effect_size_metric, original_effect_size, original_cilb,
      original_ciub, original_k, original_n, original_i2,
      # Converted data
      converted_r, converted_cilb, converted_ciub,
      # Reanalysed data
      reanalysis_estimate, reanalysis_cilb, reanalysis_ciub, reanalysis_cilb999,
      reanalysis_ciub999, reanalysis_k, reanalysis_n, reanalysis_i2,
      # Eggers
      reanalysis_eggers_p, reanalysis_eggers_cilb, reanalysis_eggers_ciub,
      # Excess Significance
      reanalysis_tes_obser, reanalysis_tes_expect, reanalysis_tes_ratio,
      reanalysis_tes_p, reanalysis_tes_power, reanalysis_tes_theta
    ) %>%
    labelled::set_variable_labels( # nolint start
      author_year = "First author and publication year of meta-analysis.",
      outcome_category = "Category the outcome belongs to.",
      plain_language_outcome = "Specific outcome for the effect.",
      plain_language_exposure = "Specific exposure for the effect.",
      age_group = "Broad age group of the participants, if specified.",
      original_effect_size_metric = "Type of effect size `original_effect_size` refers to.",
      original_effect_size = "Effect size reported in the original meta-analysis.",
      original_cilb = "Lower bound for the 95% confidence interval of the reported effect size.",
      original_ciub = "Upper bound for the 95% confidence interval of the reported effect size.",
      original_k = "Number of studies reported as contributing to the reported effect size.",
      original_n = "Number of participants reported as contributing to the reported effect size.",
      original_i2 = "Reported heterogeneity (as I-Squared) for the reported effect size.",
      converted_r = "Effect size as converted to Pearson's r (where possible).",
      converted_cilb = "Lower bound for the 95% confidence interval of the converted effect size.",
      converted_ciub = "Upper bound for the 95% confidence interval of the converted effect size.",
      reanalysis_estimate = "Effect size from the reanalysis of the study-level data (where possible).",
      reanalysis_cilb = "Lower bound for the 95% confidence interval of the reanalysed effect size.",
      reanalysis_ciub = "Upper bound for the 95% confidence interval of the reanalysed effect size.",
      reanalysis_cilb999 = "Lower bound for the 99.9% confidence interval of the reanalysed effect size.",
      reanalysis_ciub999 = "Upper bound for the 99.9% confidence interval of the reanalysed effect size.",
      reanalysis_k = "Number of studies contributing to the reanalysed effect size.",
      reanalysis_n = "Number of participants contributing to the reanalysed effect size.",
      reanalysis_i2 = "Heterogeneity (as I-Squared) for the reanalysed effect size.",
      reanalysis_eggers_p = "P-value for the Egger's test for publication bias.",
      reanalysis_eggers_cilb = "Lower bound for the 95% confidence interval for the Egger's test for publication bias.",
      reanalysis_eggers_ciub = "Upper bound for the 95% confidence interval for the Egger's test for publication bias.",
      reanalysis_tes_obser = "Number of observed significant tests (from Test of Excess Significance).",
      reanalysis_tes_expect = "Number of expected significant tests (from Test of Excess Significance).",
      reanalysis_tes_ratio = "Ratio of observed to expected significant tests (from Test of Excess Significance).",
      reanalysis_tes_p = "P-value for the Test of Excess Significance.",
      reanalysis_tes_power = "Power for each of the tests (from the Test of Excess Significance).",
      reanalysis_tes_theta = "Value of theta used to compute the tests (from the Test of Excess Significance)."
    ) # nolint end

  out_path_csv <- here::here(
    "supplementary_files",
    "Supplementary File 6 - Complete Effects Data.csv"
  )

  readr::write_csv(out_effects, out_path_csv)

  codebook_filepath <-
    "supplementary_files/Supplementary File 10 - Effect Size Codebook.pdf"

  dataReporter::makeCodebook(
    out_effects,
    output = "pdf",
    mode = "summarize",
    file = "supplementary_files/codebook.Rmd",
    replace = TRUE,
    openResult = FALSE,
    codebook = TRUE,
    addSummaryTable = FALSE,
    reportTitle =
      "Codebook for the Complete Effects Data",
  )

  unlink("supplementary_files/codebook.Rmd")
  file.rename("supplementary_files/codebook.pdf", codebook_filepath)

  return(c(out_path_csv, codebook_filepath))
}

#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param other_supps_files
#' @param join_supp_py_script
#' @param table_effects_saved
#' @param supp_exposures
#' @param supp_effects
#' @param references
#' @param table_desc_saved
#' @return
#' @author Taren Sanders
#' @export
join_supps <- function(
    other_supps_files, join_supp_py_script, table_effects_saved,
    supp_exposures, supp_effects, references, table_desc_saved) {
  reticulate::source_python(here::here("python", "combine_pdfs.py"))

  return("supplementary_files/Combined Supplementary Files.pdf")
}
