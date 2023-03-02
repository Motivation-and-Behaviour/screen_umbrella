#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param studies_clean
#' @param type
#' @return
#' @author Taren Sanders
#' @export
run_metaanalysis <- function(studies_clean, type = "r") {
  stopifnot(type %in% c("r", "z"))

  if (type == "r") {
    meta_out <- rma(
      data = studies_clean,
      measure = "COR",
      ri = r_estimate,
      ni = study_n,
      slab = study_name
    )
  }

  if (type == "z") {
    meta_out <- rma(
      data = studies_clean,
      measure = "ZCOR",
      ri = r_estimate,
      ni = study_n,
      slab = study_name
    )
  }

  # Add the ID for later matching
  meta_out$effect_size_id <- studies_clean$effect_size_id[1]

  return(meta_out)
}
