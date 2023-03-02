#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param meta_aggregated_r
#' @param eggers_results_r
#' @param excess_sig_results_r
#' @param meta_aggregated_z
#' @param eggers_results_z
#' @param excess_sig_results_z
#' @return
#' @author Taren Sanders
#' @export
combine_study_results <- function(meta_aggregated_r, eggers_results_r,
                                  excess_sig_results_r, meta_aggregated_z,
                                  eggers_results_z, excess_sig_results_z) {
  main_df <-
    full_join(meta_aggregated_r, eggers_results_r, by = "effect_size_id") %>%
    full_join(excess_sig_results_r, by = "effect_size_id") %>%
    select(-type, -term) %>%
    relocate(effect_size_id) %>%
    rename_with(
      .cols = estimate:tes_thetalim,
      .fn = ~ paste0("reanalysis_", .x)
    )

  z_df <-
    full_join(meta_aggregated_z, eggers_results_z, by = "effect_size_id") %>%
    full_join(excess_sig_results_z, by = "effect_size_id") %>%
    select(-type, -term, -k, , n, -n_max) %>%
    relocate(effect_size_id) %>%
    rename_with(
      .cols = estimate:tes_thetalim,
      .fn = ~ paste0("reanalysis_", .x, "_z")
    )

  joined_df <- full_join(main_df, z_df, by = "effect_size_id")

  return(joined_df)
}
