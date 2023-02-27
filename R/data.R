## Study level data ----

convert_studies <- function(raw) {
  # Clean data
  cleaned <- raw %>%
    mutate(
      # Fix the nested imports
      across(
        c(effect_size_id, study_first_page_number),
        ~ sapply(.x, as.character)
      ),
      across(
        c(
          study_n,
          estimate,
          lower_ci,
          upper_ci,
          standard_error,
          standard_deviation,
          variance,
          p_value,
          n_intervention,
          n_control,
          number_of_events_in_exposure_for_or_rr,
          number_of_events_in_comparison_for_or_rr
        ),
        ~ sapply(.x, as.numeric)
      ),
      # Add a study name
      study_name = paste(study_author, study_year)
    ) %>%
    # Impute mising N
    # TODO - Use something other than mean imputation
    group_by(effect_size_id) %>%
    mutate(
      study_n = replace_na(study_n, round(mean(study_n, na.rm = TRUE)))
    ) %>%
    ungroup()

  # Convert effect sizes
  converted <- cleaned %>%
    # Treat Hedge's g as Cohen's d
    mutate(converted_metric = if_else(converted_metric == "g",
      "d", converted_metric
    )) %>%
    # Limit to those with a method to convert
    filter(converted_metric %in% c("r", "d", "or", "z")) %>%
    # Conversions
    mutate(
      # Convert the estimates
      r_estimate = case_when(
        is.na(estimate) ~ NA_real_,
        converted_metric == "r" ~ estimate,
        converted_metric == "d" ~ esc::pearsons_r(d = estimate),
        converted_metric == "or" ~ esc::pearsons_r(or = estimate),
        converted_metric == "z" ~ esc::convert_z2r(estimate)
        # TODO - risk ratios estimates
      ),
      # Convert the CIs
      r_ci_lower = case_when(
        is.na(lower_ci) ~ NA_real_,
        converted_metric == "r" ~ lower_ci,
        converted_metric == "d" ~ esc::pearsons_r(d = lower_ci),
        converted_metric == "or" ~ esc::pearsons_r(or = lower_ci),
        converted_metric == "z" ~ esc::convert_z2r(lower_ci)
        # TODO - risk ratios lower ci
      ),
      r_ci_upper = case_when(
        is.na(upper_ci) ~ NA_real_,
        converted_metric == "r" ~ upper_ci,
        converted_metric == "d" ~ esc::pearsons_r(d = upper_ci),
        converted_metric == "or" ~ esc::pearsons_r(or = upper_ci),
        converted_metric == "z" ~ esc::convert_z2r(upper_ci)
        # TODO - risk ratios upper ci
      ),
      # Reverse coding as needed
      across(
        c(r_estimate, r_ci_lower, r_ci_upper),
        ~ if_else(reverse_code, . * -1, .)
      )
    ) %>%
    drop_na(r_estimate, study_n) %>%
    group_by(effect_size_id) %>%
    tar_group()

  return(converted)
}

## Run meta-analysis ----

run_metaanalysis <- function(converted) {
  # Get the meta analysis object
  meta_out <- rma(
    data = converted,
    measure = "COR",
    ri = r_estimate,
    ni = study_n,
    slab = study_name
  )

  # Repeat for 99.9% CIs since bug in Broom
  meta_out_999 <- rma(
    data = converted,
    measure = "COR",
    ri = r_estimate,
    ni = study_n,
    slab = study_name,
    level = 99.9
  )

  meta_out$conf.low_999 <- meta_out_999$ci.lb
  meta_out$conf.high_999 <- meta_out_999$ci.ub

  # Add the ID for later matching
  meta_out$effect_size_id <- converted$effect_size_id[1]

  return(meta_out)
}

tidy_meta <- function(meta_results) {
  df <- broom::tidy(meta_results, conf.int = TRUE)
  # Add extras not provided by broom
  df$k <- meta_results$k
  df$i2 <- meta_results$I2
  df$n <- sum(meta_results$ni)
  df$n_max <- max(meta_results$ni)
  df$conf.low_999 <- meta_results$conf.low_999
  df$conf.high_999 <- meta_results$conf.high_999
  df$effect_size_id <- meta_results$effect_size_id

  return(df)
}

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
