# ------------------- SETTINGS -----------------------

# ------------------- TARGETS ------------------------

make_supps <-
  list(
    tar_target(
      supp_exposures,
      make_supp_exposures(combined_effects),
      format = "file"
    ),
    tar_target(
      supp_effects,
      make_supp_effects(combined_effects),
      format = "file"
    )
  )


# ------------------- FUNCTIONS ----------------------

make_supp_exposures <-
  function(combined_effects) {
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
      "Supplementary File 3 - List of Exposures.csv"
    )
    write_csv(out, file = out_path, col_names = FALSE)

    # Upload to GDrive
    drive_put(out_path,
      path = as_id("https://drive.google.com/drive/folders/1gW5k2CIRJf1w2FP0dbUHCXwpAxtV1Gxp")
    )


    return(out_path)
  }


make_supp_effects <- function(combined_effects) {
  out_effects <-
    combined_effects %>%
    transmute(
      # Description
      author_year = author_year,
      outcome_category = outcome_category,
      plain_language_outcome = plain_language_outcome,
      plain_language_exposure = plain_language_exposure,
      age_group = moderator_age,
      # Raw data
      original_effect_size = raw_value,
      original_effect_size_type = std_eff_name,
      original_cilb = raw_cilb,
      original_ciub = raw_ciub,
      original_k = reported_k,
      original_n = reported_n,
      original_i2 = reported_i2,
      # Converted data
      converted_r = reported_r,
      converted_cilb = reported_cilb,
      converted_ciub = reported_ciub,
      # Reanalysed data
      reanalysis_r = reanalysis_estimate,
      reanalysis_cilb_95 = reanalysis_conf.low,
      reanalysis_ciub_95 = reanalysis_conf.high,
      reanalysis_cilb_999 = reanalysis_conf.low_999,
      reanalysis_ciub_999 = reanalysis_conf.high_999,
      reanalysis_i2 = reanalysis_i2,
      reanalysis_n = reanalysis_n,
      reanalysis_k = reanalysis_k,
      # Eggers
      eggers_est = eggers_b,
      eggers_cilb = eggers_ci_l,
      eggers_ciub = eggers_ci_u,
      # Excess Significance
      excess_sig_observed = tes_obser,
      excess_sig_expected = tes_expect,
      excess_sig_ratio = tes_ratio,
      excess_sig_pval = tes_p,
      excess_sig_power = tes_power,
      excess_sig_theta = tes_theta
    )

  out_path <- here::here(
    "supplementary_files",
    "Supplementary File 1 - Complete Effects Data.csv"
  )

  write_csv(out_effects, out_path)

  # Upload to GDrive
  drive_put(out_path,
    path = as_id(
      "https://drive.google.com/drive/folders/1gW5k2CIRJf1w2FP0dbUHCXwpAxtV1Gxp"
    )
  )

  return(out_path)
}