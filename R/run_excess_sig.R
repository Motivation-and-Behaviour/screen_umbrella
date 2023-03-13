#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param meta_results
#' @return
#' @author Taren Sanders
#' @export
run_excess_sig <- function(meta_results) {
  exc_sig <- tes(meta_results)
  tes_df <- tibble(
    effect_size_id = meta_results$effect_size_id,
    tes_obser = exc_sig$O,
    tes_expect = exc_sig$E,
    tes_ratio = exc_sig$OEratio,
    tes_power = paste0(round(exc_sig$power, 3), collapse = "; "),
    tes_p = exc_sig$pval,
    tes_theta = exc_sig$theta,
    tes_thetalim = exc_sig$theta.lim
  )
  return(tes_df)
}
