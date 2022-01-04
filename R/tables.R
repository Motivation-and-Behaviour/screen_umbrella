# ------------------- SETTINGS -----------------------

# ------------------- TARGETS ------------------------

make_tables <- 
  list(
    tar_target(
      tables_df,
      make_table_data(rob_raw, effects_clean, reviews_raw)),
    tar_target(
      reviews_tables,
      make_desc_tables(tables_df)),
    tar_target(
      export_tables,
      save_tables(reviews_tables),
      format = "file",
      pattern = map(reviews_tables)))


# ------------------- FUNCTIONS ----------------------

make_table_data <- function(rob_df, effects_df, reviews_df) {
  # Clean RoB
  rob <- rob_df %>%
    filter(reviewer == "Consensus") %>%
    select(-starts_with("x"), -study_id, -reviewer)

  # Clean effects
  effects <- effects_df %>%
    select(
      covidence_review_id, plain_language_outcome,
      plain_language_exposure, use_effect
    ) %>%
    rename(covidence_id = covidence_review_id)

  reviews <- reviews_df %>%
    transmute(
      covidence_id = covidence_review_id_auto,
      first_author = first_author_consensus,
      year = year_of_publication_consensus,
      design_restrictions = study_design_restrictions_extracted_consensus,
      regions_restrictions = regions_consensus,
      demographics_restrictions = demographics_consensus,
      earliest_study_year = earliest_study_publication_year_consensus,
      latest_study_year = latest_study_publication_year_consensus,
      sample_age_mean_low = as.numeric(sapply(sample_age_lowest_study_mean_consensus, 
                                   toString)),
      sample_age_mean_high = as.numeric(sapply(sample_age_highest_study_mean_concensus, 
                                    toString)) 
    )

  use_effects_ids <- 
    effects %>% 
    filter(use_effect) %>% 
    distinct(covidence_id) %>% 
    pull(covidence_id)
  
  # Join datasets
  joined_df <- 
    reviews %>%
    # Merge the datasets
    left_join(rob) %>%
    left_join(effects) %>% 
    mutate(table = if_else(covidence_id %in% use_effects_ids, "main","supp"))
  
  return(joined_df)
}
  
make_desc_tables <- function(tables_df) {
  
  bullet_list <- function(column) {
    paste0("<ul style='padding-left:10px'><li>",
           paste(column %>% unique() %>% sort(),
                 collapse = "</li><li>"
           ), "</li></ul>",
           collapse = ""
    )
  }
  
  unique_list <- function(column) {
    paste(column %>% unique() %>% sort(),
          collapse = "<br><br>")
  }
  
  rob_cols <- c("#b7e1cd", "#fce8b2", "#f4c7c3")

  # Main table
  base_table <- tables_df %>%
    filter(table == "main") %>%
    group_by(covidence_id) %>%
    mutate(
      outcomes_assessed = map(bullet_list(plain_language_outcome), gt::html),
      exposures_assessed = map(bullet_list(plain_language_exposure), gt::html),
    ) %>%
    ungroup() %>%
    select(-plain_language_outcome,
           -plain_language_exposure,
           -use_effect,
           -table) %>%
    distinct() %>%
    mutate(
      sample_ages = if_else(
        (is.na(sample_age_mean_low)) | (is.na(sample_age_mean_high)),
        demographics_restrictions,
        paste0(
          demographics_restrictions,
          "<br/><small>(",
          trimws(format(round(
            sample_age_mean_low, 1
          ), nsmall = 1)),
          "-",
          trimws(format(round(
            sample_age_mean_high, 1
          ), nsmall = 1)),
          ")</small>"
        )
      ),
      sample_ages = map(sample_ages, gt::html)
    ) %>%
    mutate_at(
      vars(
        "eligibility_criteria_predefined_and_specified":"heterogeneity_assessed"
      ),
      str_to_title
    )  %>%
    # Reorder the columns
    select(-starts_with("sample_age_mean"),
           -demographics_restrictions,
           -covidence_id) %>%
    relocate(any_of(c(
      "sample_ages", "outcomes_assessed", "exposures_assessed"
    )),
    .after = latest_study_year) 
  
  gt_table_main <- 
    base_table %>% 
    gt(caption = "Review characteristics and quality assessment for studies providing unique effects") %>% 
    # Add color to ROB
    data_color(
      columns = 
        eligibility_criteria_predefined_and_specified:heterogeneity_assessed,
      colors = scales::col_factor(palette = rob_cols,
                                  levels = c("Low", "Unclear", "High"))) %>% 
    cols_align(columns = 
                 eligibility_criteria_predefined_and_specified:heterogeneity_assessed,
               align = "center") %>% 
    tab_header(html("<strong>Study Characteristics</strong>"), 
               subtitle = "Review characteristics and quality assessment for studies providing unique effects") %>% 
    tab_spanner(label = "Review Characteristics",
                columns = first_author:exposures_assessed) %>% 
    tab_spanner(label = "Quality Assessment",
                columns = eligibility_criteria_predefined_and_specified:heterogeneity_assessed) %>%
    cols_merge_range(col_begin = earliest_study_year, col_end = latest_study_year) %>% 
    cols_label(
      first_author = "First Author",
      year = "Year",
      design_restrictions = "Design Restrictions",
      regions_restrictions = "Regions Restrictions",
      earliest_study_year = html("Study Range<br><small>Earliest - Latest</small>"),
      sample_ages = html("Sample Age Restrictions<br><small>(Age Range)</small>"),
      outcomes_assessed = "Outcomes Assessed",
      exposures_assessed = "Exposures Assessed",
      eligibility_criteria_predefined_and_specified = html("Elig. <br>Crit."),
      literature_search_strategy_comprehensive_and_systematic = "Lit. Search",
      dual_independent_screening_review = "Dual Screening",
      dual_independent_quality_assessment = "Dual Quality",
      included_studies_listed_with_important_characteristics_and_results_of_each = "Studies Listed",
      publication_bias_assessed = "Pub. Bias Assessed",
      heterogeneity_assessed = "Heterog. Assessed"
    ) %>% 
    cols_align(columns = 
                 first_author:exposures_assessed,
               align = "left") %>% 

    tab_footnote(footnote = "Where provided",
                 locations = cells_column_labels(sample_ages)) %>% 
    # ROB footnotes
    tab_footnote(footnote = "Eligibility criteria predefined and specified",
                 locations = cells_column_labels(eligibility_criteria_predefined_and_specified)) %>% 
    tab_footnote(footnote = "Literature search strategy comprehensive and systematic",
                 locations = cells_column_labels(literature_search_strategy_comprehensive_and_systematic)) %>% 
    tab_footnote(footnote = "Dual independent screening & review",
                 locations = cells_column_labels(dual_independent_screening_review)) %>% 
    tab_footnote(footnote = "Dual independent quality assessment",
                 locations = cells_column_labels(dual_independent_quality_assessment)) %>% 
    tab_footnote(footnote = "Included studies listed with important characteristics and results of each",
                 locations = cells_column_labels(included_studies_listed_with_important_characteristics_and_results_of_each)) %>% 
    tab_footnote(footnote = "Publication bias assessed",
                 locations = cells_column_labels(publication_bias_assessed)) %>% 
    tab_footnote(footnote = "Heterogeneity assessed",
                 locations = cells_column_labels(heterogeneity_assessed)) %>% 
    tab_footnote(footnote = "Items are from the National Health, Lung and Blood Institute’s Quality Assessment of Systematic Reviews and Meta-Analyses tool. Note that we excluded the first item of the tool.",
                 locations = cells_column_spanners("Quality Assessment")) %>% 
    # Styling
    cols_width(
      first_author ~ pct(6),
      year ~ pct(3),
      design_restrictions ~ pct(10),
      regions_restrictions ~ pct(10),
      earliest_study_year ~ pct(8),
      sample_ages ~ pct(8),
      outcomes_assessed ~ pct(17),
      exposures_assessed ~ pct(17),
      c(eligibility_criteria_predefined_and_specified:heterogeneity_assessed) ~ pct(3)
    ) %>%
    tab_options(heading.align = "left",
                column_labels.border.top.color = "black",
                column_labels.border.top.width = px(3),
                column_labels.border.bottom.color = "black",
                table_body.hlines.color = "white",
                table.border.bottom.color = "black",
                table.border.top.color = "black",
                table.border.bottom.width = px(3),
               table.border.top.width = px(3),
               footnotes.padding = px(0),
               column_labels.padding = px(3),
               data_row.padding = px(1),
               table.font.size = pct(80)
               ) %>% 
    tab_style(style = list(cell_text(weight = "bold",align = "left")),
      locations = cells_column_labels(everything())
    ) %>% 
    tab_style(style = list(cell_text(weight = "bold")),
      locations = cells_column_spanners(everything())
    ) 
  
  reviews_tables <- list(length=1)
  
  reviews_tables[[1]]$table <- gt_table_main
  reviews_tables[[1]]$filename <- "Descriptive table.pdf"
  
  return(reviews_tables)
}


save_tables <- function(tables) {
  file_name <- here::here("figure", tables[[1]]$filename)
  
  gtsave(
    data = tables[[1]]$table,
    filename = tables[[1]]$filename,
    path = here::here("figure")
  )
  
  # Upload to GDrive
  drive_put(file_name, 
            path = as_id("https://drive.google.com/drive/folders/1xvn1B4bGH7hr6yBvDGUfEF7F_eO3Qeml"))
  
  return(file_name)
}