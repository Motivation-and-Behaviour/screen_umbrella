## Eggers test ----

run_eggers <- function(meta_results) {
  # Run Egger's test
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

## Excess significance test ----

run_excess_sig <- function(meta_results) {
  exc_sig <- tes(meta_results)
  tes_df <- tibble(
    effect_size_id = meta_results$effect_size_id,
    tes_obser = exc_sig$O,
    tes_expect = exc_sig$E,
    tes_ratio = exc_sig$OEratio,
    tes_power = list(exc_sig$power),
    tes_p = exc_sig$pval,
    tes_theta = exc_sig$theta,
    tes_thetalim = exc_sig$theta.lim
  )
  return(tes_df)
}

combine_study_results <- function(meta_aggregated,
                                  eggers_results,
                                  excess_sig_results) {
  full_join(meta_aggregated, eggers_results, by = "effect_size_id") %>%
    full_join(excess_sig_results, by = "effect_size_id") %>%
    rename_with(
      .cols = estimate:conf.high_999,
      .fn = ~ paste0("reanalysis_", .x)
    ) %>%
    relocate(effect_size_id)
}

join_analyses <- function(effects_clean, studies_results) {
  # Join the results
  joined_df <- left_join(
    effects_clean %>%
      rename("effect_size_id" = "effect_size_id_1") %>%
      rename_with(.cols = k:sig, .fn = ~ paste0("reported_", .x)),
    studies_results,
    by = "effect_size_id"
  ) %>%
    # Create the columns that will be plotted
    mutate(
      source = if_else(!is.na(reanalysis_estimate),
        "reanalysis",
        "reported"
      ),
      r = if_else(!is.na(reanalysis_estimate),
        reanalysis_estimate,
        reported_r
      ),
      n = if_else(!is.na(reanalysis_estimate),
        reanalysis_n,
        reported_n
      ),
      k = if_else(
        !is.na(reanalysis_estimate),
        reanalysis_k,
        as.integer(reported_k)
      ),
      i2 = if_else(!is.na(reanalysis_estimate),
        reanalysis_i2,
        reported_i2
      ),
      std.err = if_else(
        !is.na(reanalysis_estimate),
        reanalysis_std.error,
        (reported_ciub - reported_cilb) / 3.92
      ),
      cilb95 = if_else(
        !is.na(reanalysis_estimate),
        reanalysis_conf.low,
        reported_cilb
      ),
      ciub95 = if_else(
        !is.na(reanalysis_estimate),
        reanalysis_conf.high,
        reported_ciub
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
      )
    ) %>%
    # Create the trustworthiness indicator
    mutate(certainty = case_when(
      source == "reported" ~ "unclear",
      (reanalysis_n >= 1000 &
        if_else(!is.na(eggers_p), eggers_p > 0.05, FALSE) &
        tes_p > 0.05) ~ "meets criteria",
      TRUE ~ "unclear"
    ))

  return(joined_df)
}
