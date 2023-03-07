#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param tables_df
#' @return
#' @author Taren Sanders
#' @export
make_table_desc_gt <- function(tables_df) {
  rob_cols <- c("#b7e1cd", "#fce8b2", "#f4c7c3")

  desc_df <- tables_df %>%
    filter(table == "main")

  base_table <-
    desc_df %>%
    group_by(review_id) %>%
    mutate(
      outcomes_assessed = map(bullet_list(plain_language_outcome), gt::html),
      exposures_assessed = map(bullet_list(plain_language_exposure), gt::html)
    ) %>%
    ungroup() %>%
    select(
      -plain_language_outcome,
      -plain_language_exposure,
      -use_effect,
      -table
    ) %>%
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
    mutate(
      sample_restrictions = case_when(
        !is.na(sample_incl) & !is.na(sample_excl) ~
          paste0("Include: ", sample_incl, "\n", "Exclude: ", sample_excl),
        !is.na(sample_incl) & is.na(sample_excl) ~
          paste0("Include: ", sample_incl),
        is.na(sample_incl) & !is.na(sample_excl) ~
          paste0("Exclude: ", sample_excl),
        TRUE ~ "None specified"
      ),
      sample_restrictions = map(sample_restrictions, gt::html)
    ) %>%
    relocate(sample_restrictions, .after = design_restrictions) %>%
    mutate_at(
      vars(
        "eligibility_criteria_predefined_and_specified":"heterogeneity_assessed"
      ),
      ~ case_match(
        .x,
        "low" ~ "L",
        "unclear" ~ "U",
        "high" ~ "H"
      )
    ) %>%
    # Reorder the columns
    select(
      -starts_with("sample_age_mean"),
      -demographics_restrictions,
      -review_id,
      -sample_incl,
      -sample_excl
    ) %>%
    relocate(
      any_of(c(
        "sample_ages", "outcomes_assessed", "exposures_assessed"
      )),
      .after = latest_study_year
    ) %>%
    arrange(str_to_lower(first_author), year)

  # Make the table
  gt_table <-
    base_table %>%
    gt(
      caption = "Review characteristics for studies providing unique effects"
    ) %>%
    tab_header(html("<strong>Review Characteristics</strong>"),
      subtitle = "Review characteristics and quality assessment for meta-analyses providing unique effects" # nolint
    ) %>%
    data_color(
      columns =
        eligibility_criteria_predefined_and_specified:heterogeneity_assessed,
      colors = scales::col_factor(
        palette = rob_cols,
        levels = c("L", "U", "H")
      )
    ) %>%
    cols_merge_range(
      col_begin = earliest_study_year, col_end = latest_study_year
    ) %>%
    tab_spanner(
      label = "Review Characteristics",
      columns =
        first_author:exposures_assessed
    ) %>%
    tab_spanner(
      label = "Quality Assessment",
      columns =
        eligibility_criteria_predefined_and_specified:heterogeneity_assessed
    ) %>%
    cols_label(
      first_author = "First Author",
      year = "Year",
      design_restrictions = "Design Restrictions",
      sample_restrictions = "Sample Restrictions",
      earliest_study_year = html(
        "Year Range<br><small>Earliest - Latest</small>"
      ),
      sample_ages = html(
        "Sample Age Restrictions<br><small>(Age Range)</small>"
      ),
      outcomes_assessed = "Outcomes Assessed",
      exposures_assessed = "Exposures Assessed",
      eligibility_criteria_predefined_and_specified = html("Elig. <br>Crit."),
      literature_search_strategy_comprehensive_and_systematic = "Lit. Search",
      dual_independent_screening_review = "Dual Screen",
      dual_independent_quality_assessment = "Dual Qual.",
      included_studies_listed_with_important_characteristics_and_results_of_each = "Studies Listed", # nolint
      publication_bias_assessed = "Pub. Bias",
      heterogeneity_assessed = "Hetero."
    ) %>%
    cols_align(
      columns =
        first_author:exposures_assessed,
      align = "left"
    ) %>%
    cols_align(
      columns =
        eligibility_criteria_predefined_and_specified:heterogeneity_assessed,
      align = "center"
    ) %>%
    tab_footnote(
      footnote = "Where provided",
      locations = cells_column_labels(sample_ages)
    ) %>%
    # ROB footnotes
    tab_footnote(
      footnote = "Eligibility criteria predefined and specified",
      locations =
        cells_column_labels(eligibility_criteria_predefined_and_specified)
    ) %>%
    tab_footnote(
      footnote = "Literature search strategy comprehensive and systematic",
      locations =
        cells_column_labels(
          literature_search_strategy_comprehensive_and_systematic
        )
    ) %>%
    tab_footnote(
      footnote = "Dual independent screening & review",
      locations = cells_column_labels(dual_independent_screening_review)
    ) %>%
    tab_footnote(
      footnote = "Dual independent quality assessment",
      locations = cells_column_labels(dual_independent_quality_assessment)
    ) %>%
    tab_footnote(
      footnote = "Included studies listed with important characteristics and results of each", # nolint
      locations =
        cells_column_labels(included_studies_listed_with_important_characteristics_and_results_of_each) # nolint
    ) %>%
    tab_footnote(
      footnote = "Publication bias assessed",
      locations = cells_column_labels(publication_bias_assessed)
    ) %>%
    tab_footnote(
      footnote = "Heterogeneity assessed",
      locations = cells_column_labels(heterogeneity_assessed)
    ) %>%
    tab_footnote(
      footnote = "Items are from the National Health, Lung and Blood Institute’s Quality Assessment of Systematic Reviews and Meta-Analyses tool. Note that we excluded the first item of the tool.", # nolint
      locations = cells_column_spanners("Quality Assessment")
    ) %>%
    # Styling
    cols_width(
      first_author ~ pct(6),
      year ~ pct(3),
      design_restrictions ~ pct(9),
      sample_restrictions ~ pct(9),
      earliest_study_year ~ pct(6),
      sample_ages ~ pct(7),
      outcomes_assessed ~ pct(13),
      exposures_assessed ~ pct(13),
      c(eligibility_criteria_predefined_and_specified:heterogeneity_assessed) ~
        pct(4)
    ) %>%
    tab_options(
      heading.align = "left",
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
      table.font.size = pct(70)
    ) %>%
    tab_style(
      style = list(cell_text(weight = "bold", align = "left")),
      locations = cells_column_labels(everything())
    ) %>%
    tab_style(
      style = list(cell_text(weight = "bold")),
      locations = cells_column_spanners(everything())
    )

  return(gt_table)
}


#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param tables_df
#' @return
#' @author Taren Sanders
#' @export
make_table_desc_latex <- function(tables_df) {
  desc_df <- tables_df %>%
    filter(table == "main") %>%
    select(
      -c(eligibility_criteria_predefined_and_specified:heterogeneity_assessed)
    )

  latex_table <-
    desc_df %>%
    group_by(review_id) %>%
    mutate(
      outcomes_assessed = unique_list(plain_language_outcome),
      exposures_assessed = unique_list(plain_language_exposure)
    ) %>%
    ungroup() %>%
    select(
      -plain_language_outcome,
      -plain_language_exposure,
      -use_effect,
      -table
    ) %>%
    distinct() %>%
    mutate(
      sample_ages = demographics_restrictions
    ) %>%
    tidyr::unite("study_range",
      earliest_study_year:latest_study_year,
      sep = " - "
    ) %>%
    select(
      -starts_with("sample_age_mean"),
      -demographics_restrictions,
      -review_id
    ) %>%
    relocate(
      any_of(c(
        "sample_ages", "outcomes_assessed", "exposures_assessed"
      )),
      .after = study_range
    ) %>%
    arrange(first_author, year) %>%
    mutate_all(kableExtra::linebreak, align = "l")
}

# Helper functions
bullet_list <- function(column) {
  paste0(
    "<ul style='padding-left:10px'><li>",
    paste(column %>% unique() %>% sort(),
      collapse = "</li><li>"
    ),
    "</li></ul>",
    collapse = ""
  )
}

bullet_list_markdown <- function(column) {
  paste0(
    "* ",
    paste(column %>% unique() %>% sort(),
      collapse = "<br>* "
    ),
    "",
    collapse = ""
  )
}

bullet_list_latex <- function(column) {
  paste0(
    "\\begin{itemize} \n  \\item ",
    paste(column %>% unique() %>% sort(),
      collapse = "\n  \\item "
    ),
    " \n  \\end{itemize}",
    collapse = ""
  )
}

unique_list <- function(column) {
  paste(column %>% unique() %>% sort(),
    collapse = "\n"
  )
}
