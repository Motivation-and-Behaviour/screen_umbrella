#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param meta_results
#' @return
#' @author Taren Sanders
#' @export
run_eggers <- function(meta_results) {
  # NOTE - limited to studies with k of 10 or more, per cochrane
  if (meta_results$k >= 10) {
    eggers <- regtest(meta_results)
    eggers_df <- tibble(
      effect_size_id = meta_results$effect_size_id,
      eggers_z = eggers$zval,
      eggers_p = eggers$pval,
      eggers_b = eggers$est,
      eggers_ci_l = eggers$ci.lb,
      eggers_ci_u = eggers$ci.ub
    )
  } else {
    eggers_df <- tibble(
      effect_size_id = meta_results$effect_size_id,
      eggers_z = NA_real_,
      eggers_p = NA_real_,
      eggers_b = NA_real_,
      eggers_ci_l = NA_real_,
      eggers_ci_u = NA_real_
    )
  }

  return(eggers_df)
}
