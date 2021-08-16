# Targets functions ####

get_mod_date <- function() {
  x <- googledrive::drive_get(id = googledrive::as_id("1z_NZwDomPfrOJg2Rn8-E8cc9yoOjXzqH_Di23vWERu4"))
  return(x$drive_resource[[1]]$modifiedTime)
}

# Convert effects data ####

convert_effects <- function(data) {
  # Conversion functions

  b2r <- function(beta) {
    # conversion formula from 10.1007/s11162-011-9232-5
    r <- NA
    if (!is.na(beta)) {
      r <- beta
    }
    r
  }

  b2r <- Vectorize(b2r)

  d2r <- function(d, a = 4) {
    # assumes equal groups
    # https://www.meta-analysis.com/downloads/Meta-analysis%20Converting%20among%20effect%20sizes.pdf
    # 10.1002/jrsm.1218
    if (is.na(d)) {
      NA
    } else {
      d / (sqrt(d^2 + a))
    }
  }

  d2r <- Vectorize(d2r)

  z2r <- function(z) {
    # test z <- 3.4
    if (is.na(z)) {
      NA
    } else {
      tanh(z)
    }
  }

  z2r <- Vectorize(z2r)

  od2r <- function(or, method = c("pearson", "digby")) {
    # DOI:10.1037/0003-066X.62.3.254
    if (is.na(or)) {
      NA
    } else {
      switch(method,
        pearson = cos(pi / (1 + or^.5)),
        digby = (or^(3 / 4) - 1) / (or^(3 / 4) + 1)
      )
    }
  }

  od2r <- Vectorize(od2r)

  # Process data

  d <- data %>%
    filter(es %in% c("b", "d", "r", "or", "z")) %>%
    rename(std_eff_name = es) %>%
    mutate(
      value_ci_lower_bound_consensus =
        case_when(
          !is.na(value_raw_se) ~ value_consensus - 2 * value_raw_se,
          TRUE ~ value_ci_lower_bound_consensus
        ),
      value_ci_upper_bound_consensus =
        case_when(
          !is.na(value_raw_se) ~ value_consensus + 2 * value_raw_se,
          TRUE ~ value_ci_upper_bound_consensus
        )
    ) %>%
    mutate(
      value_consensus = case_when(
        is.na(value_consensus) ~ NA_real_,
        std_eff_name == "r" ~ value_consensus,
        std_eff_name == "b" ~ b2r(value_consensus),
        std_eff_name == "d" ~ d2r(value_consensus),
        std_eff_name == "or" ~ od2r(value_consensus, method = "digby"),
        std_eff_name == "z" ~ z2r(value_consensus)
      ),
      value_ci_lower_bound_consensus = case_when(
        is.na(value_ci_lower_bound_consensus) ~ NA_real_,
        std_eff_name == "r" ~ value_ci_lower_bound_consensus,
        std_eff_name == "b" ~ b2r(value_ci_lower_bound_consensus),
        std_eff_name == "d" ~ d2r(value_ci_lower_bound_consensus),
        std_eff_name == "or" ~ od2r(value_ci_lower_bound_consensus, method = "digby"),
        std_eff_name == "z" ~ z2r(value_ci_lower_bound_consensus)
      ),
      value_ci_upper_bound_consensus = case_when(
        is.na(value_ci_upper_bound_consensus) ~ NA_real_,
        std_eff_name == "r" ~ value_ci_upper_bound_consensus,
        std_eff_name == "b" ~ b2r(value_ci_upper_bound_consensus),
        std_eff_name == "d" ~ d2r(value_ci_upper_bound_consensus),
        std_eff_name == "or" ~ od2r(value_ci_upper_bound_consensus, method = "digby"),
        std_eff_name == "z" ~ z2r(value_ci_upper_bound_consensus)
      )
    )

  return(d)
}

# General Functions ####
read_sheet <- function(mod_date, sheet) {
  d <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1z_NZwDomPfrOJg2Rn8-E8cc9yoOjXzqH_Di23vWERu4/edit#gid=1427279106",
    sheet = sheet,
    na = c("-999", "", "#N/A")
  ) %>%
    janitor::clean_names()
  return(d)
}


# Effects data ####

process_effects <- function(raw) {
  raw <- raw %>%
    mutate(es = str_to_lower(statistical_test_consensus)) %>%
    select(-ends_with("_r"))
  d <- convert_effects(raw)

  # Clean the names of the datafile, rename to something more meaningful,
  # remove empty stuff, then cut small studies or rubbish
  q <- clean_names(d) %>%
    dplyr::rename(
      r = value_consensus,
      outcome_category = outcome_level_1,
      outcome = outcome_level_2,
      moderator_level = moderator_level_recoded,
      moderator_category = moderator_category_recoded,
      k = k_number_of_effects_informing_this_test_consensus,
      n = combined_n,
      cilb = value_ci_lower_bound_consensus,
      ciub = value_ci_upper_bound_consensus,
      i2 = i2_calculated,
      plain_language_outcome = outcome_plain_language_descriptor
    ) %>%
    remove_empty(which = c("rows", "cols")) %>%
    mutate(n = as.numeric(n)) %>%
    filter(
      r < .99,
      moderator_level != "fixed effects",
      n > 1,
      use_moderator == TRUE
    )

  q$i2 <- as.numeric(sapply(q$i2, as.numeric))
  q$effect_size_id_1 <- as.character(sapply(q$effect_size_id_1, as.character))

  # Add significance and labels
  q$sig <- ((q$cilb * q$ciub) > 0)
  q$sig <- q$sig * .7 + .3
  q$author_year <- paste(q$first_author, ", ", q$year_of_publication, sep = "")

  # bold the rows that are classified as 'risks'
  q$risk <- ifelse(q$benefit_or_risk == "Risk", "bold", "plain")

  # if one effect from this review, keep or select "overall"
  # group by study_id and exposure and outcome, pick max n
  q_use <- q %>%
    group_by(
      plain_language_outcome,
      plain_language_exposure
    ) %>%
    slice_max(n, with_ties = TRUE) %>%
    mutate(use_effect = TRUE) %>%
    ungroup()

  q <- left_join(q, select(q_use, effect_size_id_1, use_effect)) %>%
    select(
      author_year, covidence_review_id,
      outcome_category, effect_size_id_1,
      plain_language_outcome,
      plain_language_exposure,
      risk,
      k, n, r, cilb, ciub, i2, sig, use_effect
    ) %>%
    distinct()

  return(q)
}

# Study level data ####

convert_studies <- function(raw) {
  # Custom conversion functions
  b2r <- function(beta) {
    # conversion formula from 10.1007/s11162-011-9232-5
    r <- case_when(
      beta < -0.5 ~ -0.5,
      beta > 0.5 ~ 0.5,
      TRUE ~ beta
    )
    return(r)
  }

  b2r <- Vectorize(b2r)

  # Clean data
  cleaned <- raw %>%
    filter(ok_to_import) %>%
    select(-ok_to_import, -starts_with("mismatches_")) %>%
    mutate( # Fix the nested imports
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
    mutate(study_n = replace_na(study_n, round(mean(study_n, na.rm = TRUE)))) %>% 
    ungroup()

  # Convert effect sizes
  converted <- cleaned %>%
    # Treat Hedge's g as Cohen's d
    mutate(converted_metric = if_else(converted_metric == "g",
      "d", converted_metric
    )) %>%
    # Limit to those with a method to convert
    filter(converted_metric %in% c("r", "d", "or", "z"))%>%
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
      across(c(r_estimate,r_ci_lower,r_ci_upper),
               ~if_else(reverse_code, . * -1, .))
    ) %>%
    drop_na(r_estimate, study_n) %>% 
    group_by(effect_size_id) %>%
    tar_group()

  return(converted)
}

run_metaanalysis <- function(converted) {
  # Get the meta analysis object
  meta_out <- rma(
    data = converted,
    measure = "COR",
    ri = r_estimate,
    ni = study_n,
    slab = study_name
  )

  # Add the ID for later matching
  meta_out$effect_size_id <- converted$effect_size_id[1]

  return(meta_out)
}

tidy_meta <- function(meta_results) {
  df <- broom::tidy(meta_results, conf.int = T)
  # Add extras not provided by broom
  df$k <- meta_results$k
  df$i2 <- meta_results$I2
  df$effect_size_id <- meta_results$effect_size_id

  return(df)
}

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

run_excess_sig <- function(meta_results){
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

combine_study_results <- function(meta_aggregated, eggers_results, excess_sig_results) {
  full_join(meta_aggregated, eggers_results, by = "effect_size_id") %>% 
    full_join(excess_sig_results, by = "effect_size_id")
}

# Data Vis ####
# NOTE - temporary only
make_comp_plots <- function(effects_clean, studies_results) {
  effects_match <- effects_clean %>%
    filter(effect_size_id_1 %in% studies_results$effect_size_id) %>%
    mutate(
      std.error = (ciub - cilb) / 3.92,
      source = "them",
      outcome_lvl1 = factor(gsub(":.*", "", plain_language_outcome)),
      plain_language_outcome = gsub(".*: ", "", plain_language_outcome),
      desc = paste(
        effect_size_id_1,
        plain_language_outcome,
        plain_language_exposure,
        sep = " \n "
      )
    ) %>%
    rename(estimate = r, effect_size_id = effect_size_id_1)
  
  joined_data <-
    bind_rows(
      studies_results %>% mutate(source = "us") %>%
        left_join(
          effects_match %>%
            select(effect_size_id,
                   desc,
                   outcome_category,
                   outcome_lvl1)
        ),
      effects_match
    )
  
  comp_plots <-
    vector(mode = "list", length = length(unique(joined_data$outcome_category)))
  names(comp_plots) <- unique(joined_data$outcome_category)
  
  for (out_cat in unique(joined_data$outcome_category)) {
    out_plot <-
      forestplot(
        df = joined_data %>% filter(outcome_category == out_cat),
        name = desc,
        estimate = estimate,
        se = std.error,
        colour = source
      ) + ggforce::facet_col(facets = ~ outcome_lvl1,
                             scales = "free_y",
                             space = "free")
    comp_plots[[out_cat]] <- out_plot
  }
  return(comp_plots)
}