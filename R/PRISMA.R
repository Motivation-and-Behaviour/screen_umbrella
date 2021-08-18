make_prisma <- function(covidence_export, effects_clean) {
  
  # Get Covidence PRISMA data
  covidence_export <- read_file(covidence_export)
  
  dat <-
    # Convert each to a separate line
    tibble(text = unlist(str_split(covidence_export, pattern = "\\n"))) %>%
    # remove unneeded info
    filter(
      !str_detect(text, "studies ongoing"),
      !str_detect(text, "studies awaiting classification")
    ) %>%
    # Determine row type
    mutate(
      # type = if_else(startsWith(text, "\t"), "exclude", "main"),
      type = case_when(
        startsWith(text, "\t\t") ~ "exclude_reason",
        startsWith(text, "\t") ~ "exclude",
        TRUE ~ "main"
      ),
      step = 0,
      text = str_replace_all(text, "\t", ""),
      value = str_extract(text, "^[0-9]+") %>% as.numeric(),
      text = str_remove(text, "^[0-9]+") %>% str_trim()
    )
  # Generate steps
  step <- 0
  for (i in seq_len(nrow(dat))) {
    if (dat$type[i] == "main") step <- step + 1
    dat$step[i] <- step
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
  
  
  # Modify dataframe (add rows as needed)
  dat <- dat %>%
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
        "Meta analysis that meets inclusion criteria, but youth results not presented and authors did not/could not provide individual study data when contacted.",
        "Individual level data only"
      ),
      "All meta-analyses results include non-target samples (e.g., adults)"
    ) %>%
    exclusion_collapse(
      c(
        "Clinical outcome",
        "Wrong exposure - clinical health intervention"
      ),
      "Wrong exposure - clinical health intervention"
    ) %>%
    arrange(step, desc(value))
  
  dat <- dat %>% add_row(
    text = "studies contributed unique effects",
    type = "main",
    step = dat$step[nrow(dat)] + 1,
    value = n_distinct(effects_clean$covidence_review_id)
  )
  
  # Generate PRISMA Diagram ####
  
  # Convert excl_reason to new column
  dat <- dat %>%
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
  
  labels <- dat$text
  
  prisma <- paste("digraph flowchart {
      # node definitions with substituted label text
      node [shape='box', fontsize = 10, width=3.5];
      graph [splines=ortho, nodesep=1, dpi = 72];
      tab1 [label = '", labels[1], "']
      tab2 [label = '", labels[2], "']
      tab3 [label = '", labels[3], "']
      tab4 [label = '", labels[4], "']
      tab5 [label = '", labels[5], "']
      tab6 [label = <", labels[6], "<br ALIGN = 'LEFT'/>", labels[7], "<br ALIGN = 'LEFT'/>>]
      tab7 [label = '", labels[8], "']
      tab8 [label = '", labels[9], "']

      # edge definitions with the node IDs
      tab1 -> tab3 -> tab5 -> tab7 -> tab8;
      tab1 -> tab2;
      {rank=same; tab1; tab2}
      tab3 -> tab4;
      {rank=same; tab3; tab4}
      tab5 -> tab6;
      {rank=same; tab5; tab6}
      }
", collapse = "")
  
  prisma_diag <- DiagrammeR::grViz(prisma)
  prisma_data <- list(data = dat, diag = prisma_diag)
  
  return(prisma_data)
  
}

save_prisma <- function(prisma_data){
  
  prisma_data$diag %>%
    DiagrammeRsvg::export_svg() %>%
    charToRaw() %>%
    rsvg::rsvg_pdf(here::here("figure", "flow.pdf"))
  
  return(here::here("figure", "flow.pdf"))
}
