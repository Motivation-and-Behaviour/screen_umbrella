#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param tables_df
#' @return
#' @author Taren Sanders
#' @export
make_table_rob <- function(tables_df) {
  rob_cols <- c("#b7e1cd", "#fce8b2", "#f4c7c3")

  base_table <-
    tables_df %>%
    filter(table == "main") %>%
    select(
      first_author, year,
      eligibility_criteria_predefined_and_specified:heterogeneity_assessed
    ) %>%
    distinct() %>%
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
    arrange(str_to_lower(first_author), year)

  gt_table <-
    base_table %>%
    gt(
      caption = "Quality assessment for studies providing unique effects"
    ) %>%
    tab_header(md("**Quality Assessment**"),
      subtitle = "Quality assessment for meta-analyses providing unique effects" # nolint
    ) %>%
    data_color(
      columns =
        eligibility_criteria_predefined_and_specified:heterogeneity_assessed,
      fn = scales::col_factor(
        palette = rob_cols,
        levels = c("L", "U", "H")
      )
    ) %>%
    tab_spanner(
      label = "Quality Assessment",
      columns =
        eligibility_criteria_predefined_and_specified:heterogeneity_assessed
    ) %>%
    cols_label(
      first_author = "First Author",
      year = "Year",
      eligibility_criteria_predefined_and_specified = html("Elig. Crit."),
      literature_search_strategy_comprehensive_and_systematic = "Lit. Search",
      dual_independent_screening_review = "Dual Screen",
      dual_independent_quality_assessment = "Dual Qual.",
      included_studies_listed_with_important_characteristics_and_results_of_each = "Studies Listed", # nolint
      publication_bias_assessed = "Pub. Bias",
      heterogeneity_assessed = "Hetero."
    ) %>%
    cols_align(
      columns =
        first_author:year,
      align = "left"
    ) %>%
    cols_align(
      columns =
        eligibility_criteria_predefined_and_specified:heterogeneity_assessed,
      align = "center"
    ) %>% # ROB footnotes
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
      footnote = "Dual independent screening and review",
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
      footnote = "Items are from the National Health, Lung and Blood Institute's Quality Assessment of Systematic Reviews and Meta-Analyses tool. Note that we excluded the first item of the tool. U = Unclear; L = Low; H = High", # nolint
      locations = cells_column_spanners("Quality Assessment")
    ) %>%
    # Styling
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
      table.font.size = pct(100),
      footnotes.multiline = FALSE
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
