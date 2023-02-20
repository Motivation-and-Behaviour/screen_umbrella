#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param stat
#' @return
#' @author Taren Sanders
#' @export
convert_tests <- function(data) {
  data <- data %>% mutate(
    stat_test_clean = case_match(
      str_trim(str_to_lower(statistical_test)),
      c(
        "beta", "beta coefficient", "standardised regression coefficient"
      ) ~ "b",
      c(
        "adjusted smd", "average effect size", "cohen d", "cohen's d",
        "effect size (type unclear)", "g", "g+", "hedge's g", "hedges g",
        "hedges' d", "hedges' g", "median effect size",
        "pooled mean effect size", "smd", "standard mean difference",
        "standardised difference in the means", "standardized mean difference",
        "standarised mean difference", "std mean difference",
        "std. mean difference", "standardised mean difference"
      ) ~ "d",
      c("log odds ratio") ~ "lor",
      c(
        "mean", "mean difference", "pooled mean difference",
        "pre-post difference mean", "unstandardized mean difference",
        "weighted mean", "weighted mean difference"
      ) ~ "md",
      c(
        "odd ratio", "odds ratio", "odds ratio iv",
        "pooled fixed effect -odd ratio", "pooled odds ratio"
      ) ~ "or",
      c(
        "attenuated correlation (uncorrected correlation)",
        "corrected correlation", "correlation", "correlation coefficient",
        "pearson's r", "weighted mean correlation coefficient",
        "r = uncorrected sample-weighted mean effect size"
      ) ~ "r",
      c("rate ratio", "relative risk", "risk ratio") ~ "rr",
      c("fisher z", "fisher's z", "z fischer", "z fisher") ~ "z",
      c("mean tau-u", "tau-u") ~ NA,
      .default = str_trim(str_to_lower(statistical_test))
    )
  )

  stopifnot(all(data$stat_test_clean %in% c(
    "b", "d", "lor", "md", "or", "r", "rr", "z", NA
  )))

  return(data)
}
