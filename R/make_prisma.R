#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param prisma_path
#' @param effects_clean
#' @param reviews_clean
#' @return
#' @author Taren Sanders
#' @export
make_prisma <- function(prisma_path, effects_clean, reviews_clean) {
  prisma_data <- readr::read_file(prisma_path)

  prisma_df_raw <-
    # Convert each to a separate line
    tibble(text = unlist(str_split(prisma_data, pattern = "\\n"))) %>%
    filter(
      !str_detect(text, "studies ongoing"),
      !str_detect(text, "studies awaiting classification")
    ) %>%
    # Determine row type
    mutate(
      type = case_when(
        startsWith(text, "\t\t") ~ "exclude_reason",
        startsWith(text, "\t") ~ "exclude",
        TRUE ~ "main"
      ),
      text = str_replace_all(
        text, "studies included",
        "met inclusion criteria"
      ),
      step = 0,
      text = str_replace_all(text, "\t", ""),
      value = str_extract(text, "^[0-9]+") %>% as.numeric(),
      text = str_remove(text, "^[0-9]+") %>% str_trim(),
      # Fix first line strangeness
      value = if_else(
        str_detect(text, "as [0-9]+ studies"),
        str_extract(text, "[0-9]+") %>% as.numeric(),
        value
      ),
      text = if_else(
        str_detect(text, "as [0-9]+ studies"),
        str_remove(text, "as [0-9]+ studies") %>% str_trim(),
        text
      )
    )

  step <- 0
  for (i in seq_len(nrow(prisma_df_raw))) {
    if (prisma_df_raw$type[i] == "main") step <- step + 1
    prisma_df_raw$step[i] <- step
  }

  prisma_df <-
    prisma_df_raw %>%
    exclusion_collapse(
      c("Not a systematic review", "Not a review"),
      "Not a systematic review"
    ) %>%
    exclusion_collapse(
      c(
        "Systematic review without meta-analysis",
        "Sys review of qual studies", "Review of reviews"
      ),
      "Systematic review without meta-analysis"
    ) %>%
    exclusion_collapse(
      c("Duplicated reference", "DUPLICATE TO MERGE LATER"),
      "Duplicated reference"
    ) %>%
    exclusion_collapse(
      c(
        "Meta analysis that meets inclusion criteria, but youth results not presented and authors did not/could not provide individual study data when contacted.", # nolint
        "Individual level data only",
        "Analysis not moderated by age",
        "Only one eligible sample.",
        "The sample includes adolescent groups, however the analysis are not moderated by age. Individual level data provided.", # nolint
        "Wrong population"
      ),
      "Wrong population"
    ) %>%
    exclusion_collapse(
      c(
        "Clinical outcome",
        "Wrong exposure - clinical health intervention",
        "Wrong exposure - other (explain)"
      ),
      "Wrong exposure"
    ) %>%
    arrange(step, desc(value))

  # Reasons studies were removed
  contributing_reviews <-
    effects_clean %>%
    filter(use_effect) %>%
    distinct(review_id) %>%
    pull(review_id)
  missing_key <-
    reviews_clean %>%
    filter(no_valid_effects) %>%
    pull(review_id)
  larger_avail <- reviews_clean %>%
    filter(!review_id %in% c(contributing_reviews, missing_key)) %>%
    pull(review_id)

  prisma_df_cleaned <-
    prisma_df %>%
    add_row(
      text = "studies removed",
      type = "exclude",
      step = prisma_df$step[nrow(prisma_df)],
      value = prisma_df[prisma_df$step == 4 &
        prisma_df$type == "main", ]$value - length(contributing_reviews)
    ) %>%
    add_row(
      text = "All effects missing key information (estimate, N)",
      type = "exclude_reason",
      step = prisma_df$step[nrow(prisma_df)],
      value = length(missing_key)
    ) %>%
    add_row(
      text = "Larger study available",
      type = "exclude_reason",
      step = prisma_df$step[nrow(prisma_df)],
      value = length(larger_avail)
    ) %>%
    add_row(
      text = "studies contributed unique effects",
      type = "main",
      step = prisma_df$step[nrow(prisma_df)] + 1,
      value = length(contributing_reviews)
    ) %>%
    mutate(text = paste(value, text, sep = " ")) %>%
    group_by(step, type) %>%
    mutate(text = if_else(type == "exclude_reason",
      paste("&nbsp;&nbsp;&nbsp;&nbsp;&#8226; ",
        text,
        collapse = "<br ALIGN = 'LEFT'/> \n"
      ),
      text
    )) %>%
    distinct(step, type, .keep_all = TRUE)

  # Generate PRISMA Diagram

  labels <- prisma_df_cleaned$text

  prisma <- paste("digraph flowchart {
      # node definitions with substituted label text
      node [shape='box', fontsize = 10, width=3.5];
      graph [splines=ortho, nodesep=1, dpi = 72];
      tab1 [label = '", labels[1], "']
      tab2 [label = '", labels[2], "']
      tab3 [label = '", labels[3], "']
      tab4 [label = '", labels[4], "']
      tab5 [label = '", labels[5], "']
      tab6 [label = <", labels[6],
    "<br ALIGN = 'LEFT'/>", labels[7], "<br ALIGN = 'LEFT'/>>]
      tab7 [label = '", labels[8], "']
      tab8 [label = <", labels[9],
    "<br ALIGN = 'LEFT'/>", labels[10], "<br ALIGN = 'LEFT'/>>]
      tab9 [label = '", labels[11], "']

      # edge definitions with the node IDs
      tab1 -> tab3 -> tab5 -> tab7 -> tab9;
      tab1 -> tab2;
      {rank=same; tab1; tab2}
      tab3 -> tab4;
      {rank=same; tab3; tab4}
      tab5 -> tab6;
      {rank=same; tab5; tab6}
      tab7 -> tab8;
      {rank=same; tab7; tab8}
      }
",
    collapse = ""
  )

  prisma_diag <- DiagrammeR::grViz(prisma, width = 1000, height = 700)
  prisma_data <- list(data = prisma_df_cleaned, diag = prisma_diag)

  return(prisma_data)
}

save_prisma <- function(prisma_data) {
  prisma_data$diag %>%
    DiagrammeRsvg::export_svg() %>%
    charToRaw() %>%
    rsvg::rsvg_pdf(here::here("figures", "Figure 1 - PRISMA Diagram.pdf"))

  return(here::here("figures", "Figure 1 - PRISMA Diagram.pdf"))
}

exclusion_collapse <- function(data, old_rows, new_row) {
  bind_rows(
    data %>% filter(!text %in% old_rows),
    data %>%
      filter(text %in% old_rows) %>%
      summarise(value = sum(value), step = min(step)) %>%
      mutate(text = new_row, type = "exclude_reason")
  )
}
