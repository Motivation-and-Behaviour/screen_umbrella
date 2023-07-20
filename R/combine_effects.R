#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param effects_clean
#' @param studies_results
#' @return
#' @author Taren Sanders
#' @export
combine_effects <- function(effects_clean, studies_results) {
  joined_df <-
    left_join(effects_clean, studies_results, by = "effect_size_id") %>%
    mutate(
      source = if_else(!is.na(reanalysis_estimate),
        "reanalysis",
        "reported"
      ),
      r = if_else(!is.na(reanalysis_estimate),
        reanalysis_estimate,
        converted_r
      ),
      n = if_else(!is.na(reanalysis_estimate),
        reanalysis_n,
        original_n
      ),
      k = if_else(
        !is.na(reanalysis_estimate),
        reanalysis_k,
        as.integer(original_k)
      ),
      i2 = if_else(!is.na(reanalysis_estimate),
        reanalysis_i2,
        original_i2
      ),
      std.err = if_else(
        !is.na(reanalysis_estimate),
        reanalysis_std.error,
        (converted_ciub - converted_cilb) / 3.92
      ),
      cilb95 = if_else(
        !is.na(reanalysis_estimate),
        reanalysis_conf.low,
        converted_cilb
      ),
      ciub95 = if_else(
        !is.na(reanalysis_estimate),
        reanalysis_conf.high,
        converted_ciub
      ),
      cilb999 = if_else(
        !is.na(reanalysis_estimate),
        reanalysis_conf.low_999,
        NA_real_
      ),
      ciub999 = if_else(
        !is.na(reanalysis_estimate),
        reanalysis_conf.high_999,
        NA_real_
      ),
      pval = reanalysis_p.value, # Only report p for reanalysis
      # Statistical certainty indicator
      certainty = case_when(
        # Use reported means unclear
        source == "reported" ~ "unclear",
        # Sample size >1000 and non-sig eggers and excess sig meet criteria
        (reanalysis_n >= 1000 &
          if_else(
            !is.na(reanalysis_eggers_p), reanalysis_eggers_p > 0.05, FALSE
          ) &
          reanalysis_tes_p > 0.05) ~ "meets criteria",
        TRUE ~ "unclear"
      )
    )

  return(joined_df)
}
