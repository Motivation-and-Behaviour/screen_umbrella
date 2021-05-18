library(DT)
library(kableExtra)

make_tables <- function(rob_df, effects_df, reviews_df) {


  # Clean RoB
  rob <- rob_df %>%
    filter(reviewer == "Consensus") %>%
    select(-starts_with("x"), -study_id, -reviewer)

  # Clean effects
  effects <- effects_df %>%
    select(
      covidence_review_id, plain_language_outcome,
      plain_language_exposure
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
      sample_age_mean_low = as.numeric(sapply(
        sample_age_lowest_study_mean_consensus,
        toString
      )),
      sample_age_mean_high = as.numeric(sapply(
        sample_age_highest_study_mean_concensus,
        toString
      ))
    )

  bullet_list <- function(column) {
    paste0("<ul><li>",
      paste(column %>% unique() %>% sort(),
        collapse = "</li><li>"
      ), "</li></ul>",
      collapse = ""
    )
  }

  # Join datasets
  table_df <- reviews %>%
    # Merge the datasets
    left_join(rob) %>%
    left_join(effects) %>%
    # Make the outcomes/exposures in one cell
    group_by(covidence_id) %>%
    mutate(
      outcomes_assessed = bullet_list(plain_language_outcome),
      exposures_assessed = bullet_list(plain_language_exposure)
    ) %>%
    ungroup() %>%
    select(-plain_language_outcome, -plain_language_exposure) %>%
    distinct() %>%
    # Create the age column
    mutate(sample_ages = if_else(
      (is.na(sample_age_mean_low)) | (is.na(sample_age_mean_high)),
      demographics_restrictions,
      paste0(
        demographics_restrictions,
        "<br/>(",
        trimws(format(round(sample_age_mean_low, 1), nsmall = 1)),
        "—",
        trimws(format(round(sample_age_mean_high, 1), nsmall = 1)),
        ")"
      )
    )) %>%
    # Clean the data up
    mutate_at(
      vars("eligibility_criteria_predefined_and_specified":"heterogeneity_assessed"),
      str_to_title
    ) %>%
    # Reorder the columns
    select(-starts_with("sample_age_mean"), -demographics_restrictions, -covidence_id) %>%
    relocate(any_of(c("sample_ages", "outcomes_assessed", "exposures_assessed")),
      .after = latest_study_year
    ) %>%
    # Clean the column names up
    rename_with(~ gsub("_", " ", .x, fixed = TRUE)) %>%
    rename_with(str_to_title)


  # Make table ####

  ## Settings ####

  rob_cols <- c("#b7e1cd", "#fce8b2", "#f4c7c3")


  ## Use DT for table ####

  sketch <- htmltools::withTags(table(
    class = "display",
    thead(
      tr(
        th(colspan = 9, style = "border-right: solid 1px;", "Review Characteristics"),
        th(colspan = 7, "Quality Assessment")
      ),
      tr(
        lapply(names(table_df)[1:8], th),
        th(style = "border-right: solid 1px;", names(table_df)[9]),
        lapply(names(table_df)[-(1:9)], th)
      )
    )
  ))

  review_table_DT <- table_df %>%
    datatable(
      escape = FALSE,
      rownames = FALSE,
      container = sketch,
      width=1200,
      fillContainer = FALSE, 
      options = list(
        pageLength = 30, 
        autoWidth = TRUE,
        columnDefs = list(
          list(className = "dt-center", targets = 9:15),
          list(width = "300px", targets = 8:9)
        )
      )
    ) %>%
    formatStyle(names(table_df)[-(1:9)],
      backgroundColor = styleEqual(c("Low", "Unclear", "High"), rob_cols)
    )


  ## Use Kable for printing ####
  review_table_kable <- table_df %>%
    mutate_at(
      vars("Eligibility Criteria Predefined And Specified":"Heterogeneity Assessed"),
      ~ case_when(
        .x == "Low" ~ cell_spec(.x,
          background_as_tile = TRUE,
          background = rob_cols[1]
        ),
        .x == "Unclear" ~ cell_spec(.x,
          background_as_tile = TRUE,
          background = rob_cols[2]
        ),
        .x == "High" ~ cell_spec(.x,
          background_as_tile = TRUE,
          background = rob_cols[3]
        )
      )
    ) %>%
    kable(escape = FALSE) %>%
    kable_paper(full_width = TRUE, bootstrap_options = "striped") %>%
    add_header_above(c("Review Characteristics" = 9, "Quality Assessment" = 7)) %>% 
    scroll_box(width="1200px")
  
  reviews_tables <- list(DT = review_table_DT, kable = review_table_kable)
  
  return(reviews_tables)
}