#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param meta_aggregated_r
#' @param eggers_results_r
#' @param excess_sig_results_r
#' @return
#' @author Taren Sanders
#' @export
combine_study_results <- function(meta_aggregated_r, eggers_results_r,
                                  excess_sig_results_r) {
  main_df <-
    full_join(meta_aggregated_r, eggers_results_r, by = "effect_size_id") %>%
    full_join(excess_sig_results_r, by = "effect_size_id") %>%
    select(-type, -term) %>%
    relocate(effect_size_id) %>%
    rename_with(
      .cols = estimate:tes_thetalim,
      .fn = ~ paste0("reanalysis_", .x)
    )

  return(main_df)
}
