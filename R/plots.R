# ------------------- SETTINGS -----------------------

# ------------------- TARGETS ------------------------

make_prisma_figure <- list(
  tar_target(prisma_data,
             here::here("covidence_prisma.txt"),
             format = "file"),
  tar_target(prisma,
             make_prisma(prisma_data, effects_clean)),
  tar_target(export_prisma,
             save_prisma(prisma),
             format = "file",)
)

make_forest_plots <- list(
  tar_target(plots,
             make_plots(combined_effects)),
  
  tar_target(
    export_plots,
    save_plots(plots),
    format = "file",
    pattern = map(plots)
  )
)


# ------------------- FUNCTIONS ----------------------

## PRISMA ----

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
  
  # Reasons studies were removed
  dat <- dat %>%
    add_row(
      text = "studies removed",
      type = "exclude",
      step = dat$step[nrow(dat)],
      value = dat[dat$step == 4 &
                    dat$type == "main", ]$value -  effects_clean %>% 
        filter(use_effect) %>% 
        distinct(covidence_review_id) %>% 
        nrow()
    ) %>% 
    add_row(
      text = "All effects missing key information (estimate, N)",
      type = "exclude_reason",
      step = dat$step[nrow(dat)],
      value = 40 # TODO unhardcode this
    ) 
  
  dat <- dat %>% 
    add_row(
      text = "Larger study available",
      type = "exclude_reason",
      step = dat$step[nrow(dat)],
      value = dat[dat$step == 4 &
                    dat$type == "exclude", ]$value - 40 # TODO unhardcode this
    )
    
  dat <- dat %>% add_row(
    text = "studies contributed unique effects",
    type = "main",
    step = dat$step[nrow(dat)] + 1,
    value = effects_clean %>% filter(use_effect) %>% distinct(covidence_review_id) %>% nrow()
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
      tab8 [label = <", labels[9], "<br ALIGN = 'LEFT'/>", labels[10], "<br ALIGN = 'LEFT'/>>]
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
", collapse = "")
  
  prisma_diag <- DiagrammeR::grViz(prisma)
  prisma_data <- list(data = dat, diag = prisma_diag)
  
  return(prisma_data)
  
}

save_prisma <- function(prisma_data){
  
  prisma_data$diag %>%
    DiagrammeRsvg::export_svg() %>%
    charToRaw() %>%
    rsvg::rsvg_pdf(here::here("figure", "PRISMA Diagram.pdf"))
  
  # Upload to GDrive
  drive_put(here::here("figure", "PRISMA Diagram.pdf"), 
            path = as_id("https://drive.google.com/drive/folders/1xvn1B4bGH7hr6yBvDGUfEF7F_eO3Qeml"))
  
  return(here::here("figure", "PRISMA Diagram.pdf"))
}


## Forest Plots ----


make_plots <- function(combined_effects) {

  # Deal with long labels
  trimmer <- Vectorize(function(string, max_len) {
    if (str_length(string) > max_len) {
      # If there's a colon, always break there to keep it neat
      if (str_detect(string, ":")) {
        string <- gsub(":", ":<br/>", string)
      } else {
        string <- str_replace_all(str_wrap(string, width = str_length(string) / 1.5), "\n", "<br/>")
      }
    }

    return(string)
  })

  # Modify the dataset
  combined_effects <-
    combined_effects %>%
    filter(use_effect) %>%
    mutate(
      # Truncate large effects
      cilb999 = if_else(cilb999 < -1, -1, cilb999),
      ciub999 = if_else(ciub999 > 1, 1, ciub999),
      # Take overall outcome into a variable and remove that from the sub-variable
      outcome_lvl_1 = factor(gsub(":.*", "", plain_language_outcome)),
      plain_language_outcome = gsub(".*: ", "", plain_language_outcome),
      outcome_category = factor(str_to_title(outcome_category)),
      plain_language_exposure = str_replace(
        plain_language_exposure,
        "^Intervention:",
        "Screen-based intervention:"
      ),
      # Fix long labels
      plain_language_outcome = trimmer(plain_language_outcome, 40),
      plain_language_exposure = trimmer(plain_language_exposure, 40),
      i2 = scales::percent(i2, 2, scale = 1),
      row_num = as.factor(row_number()),
      n = scales::label_comma(accuracy = 1)(n),
      k = as.character(k),
      rci = paste(format(round(r, 2), nsmall = 2),
        " [", format(round(cilb95, 2), nsmall = 2), ", ",
        format(round(ciub95, 2), nsmall = 2), "]",
        sep = ""
      ),
      indiv_data = fontawesome(if_else(source == "reanalysis", "fa-check", "fa-times")),
      eggers = case_when(
        source == "reported" ~ fontawesome("fa-minus"),
        eggers_p > 0.05 ~ fontawesome("fa-check"),
        TRUE ~ fontawesome("fa-times")
      ),
      esig = case_when(
        source == "reported" ~ fontawesome("fa-minus"),
        tes_p > 0.05 ~ fontawesome("fa-check"),
        TRUE ~ fontawesome("fa-times")
      ),
      font_fam = "fontawesome-webfont",
      moderator_age = factor(moderator_age, levels=c("All","Young children", "Children", "Adolescents"))
    ) %>%
    arrange(
      outcome_lvl_1,
      plain_language_outcome,
      plain_language_exposure,
      moderator_age
    ) %>%
    add_row(
      outcome_lvl_1 = "**Outcome**",
      plain_language_outcome = "**Specific Outcome**",
      plain_language_exposure = "**Exposure**",
      n = "**N**",
      k = "**K**",
      i2 = "**I^2**",
      rci = "**<i>r</i> with 95% CI**",
      author_year = "**Lead Author, Date**",
      row_num = "NA",
      indiv_data = "**Indiv.<br/>Data**",
      eggers = "**Eggers**",
      esig = "**Excess<br/>Signif.**",
      outcome_category = "**Outcome Category**",
      font_fam = "Times",
      moderator_age = "**Age Group**"
    ) %>%
    mutate(
      outcome_lvl_1 = fct_expand(outcome_lvl_1, "**Outcome**") %>%
        fct_relevel("**Outcome**"),
      plain_language_outcome = fct_expand(
        plain_language_outcome,
        "**Specific Outcome**"
      ) %>%
        fct_relevel("**Specific Outcome**"),
      plain_language_exposure = fct_expand(
        plain_language_exposure,
        "**Exposure**"
      ) %>%
        fct_relevel("**Exposure**"),
      outcome_category = fct_expand(outcome_category, "**Outcome Category**") %>%
        fct_relevel("**Outcome Category**"),
      moderator_age = fct_expand(moderator_age, "**Age Group**") %>% 
        fct_relevel("**Age Group**")
    )


  gen_plot <- function(categories, certain, title, positions,
                       caption=FALSE, debug = FALSE) {
    if (debug) labsize <- 1 else labsize <- NA


    plot_effects <- combined_effects %>%
      filter(outcome_category %in% c(categories, "**Outcome Category**"))

    if (certain) {
      plot_effects <- plot_effects %>%
        filter(certainty == "meets criteria" |
          is.na(certainty))
    } else {
      plot_effects <- plot_effects %>%
        filter(certainty == "unclear" |
          is.na(certainty))
    }

    base_plot <- ggplot(
      plot_effects,
      aes(
        x = row_num,
        y = r,
        label = plain_language_exposure
      )
    ) +
      geom_linerange(aes(
        ymin = cilb999,
        ymax = ciub999
      ),
      size = 2,
      colour = "#bdbdbd"
      ) +
      geom_linerange(aes(
        ymin = cilb95,
        ymax = ciub95
      ),
      size = 2,
      colour = "#636363"
      ) +
      geom_hline(aes(yintercept = 0),
        lty = 1,
        size = 0.5
      ) +
      geom_point(
        size = 2, shape = 21,
        fill = "#f0f0f0"
      ) +
      geom_richtext(aes(label = n),
        y = positions$n,
        vjust = 0.5, hjust = 0.5,
        stat = "identity",
        size = 2.5,
        label.size = labsize
      ) +
      geom_richtext(aes(label = k),
        y = positions$k,
        vjust = 0.5, hjust = 0.5,
        stat = "identity",
        size = 2.5,
        label.size = labsize
      ) +
      geom_richtext(aes(label = i2),
        y = positions$i2,
        vjust = 0.5, hjust = 0.5,
        stat = "identity",
        size = 2.5,
        label.size = labsize
      ) +
      geom_richtext(aes(label = rci),
        y = positions$rci,
        vjust = 0.5, hjust = 0.5,
        stat = "identity",
        size = 2.5,
        label.size = labsize
      ) +
      geom_richtext(aes(label = author_year),
        y = positions$author_year,
        vjust = 0.5, hjust = 0,
        stat = "identity",
        size = 2.5,
        label.size = labsize
      ) +
      geom_richtext(aes(label = plain_language_exposure),
        y = positions$expo,
        vjust = 0.5, hjust = 0,
        stat = "identity",
        size = 2.5,
        label.size = labsize
      ) +
      geom_richtext(aes(label = plain_language_outcome),
        y = positions$outcome,
        vjust = 0.5, hjust = 0,
        stat = "identity",
        size = 2.5,
        label.size = labsize
      ) +
      geom_richtext(aes(label = moderator_age),
                    y = positions$mod,
                    vjust = 0.5, hjust = 0,
                    stat = "identity",
                    size = 2.5,
                    label.size = labsize
      )
    
    if (!certain) {
      base_plot <-
        base_plot +
        geom_richtext(aes(
          label = esig,
          family = font_fam
        ),
        y = positions$esig,
        vjust = 0.5, hjust = 0.5,
        stat = "identity",
        size = 2.5,
        label.size = labsize
        ) +
        geom_richtext(aes(
          label = eggers,
          family = font_fam
        ),
        y = positions$eggers,
        vjust = 0.5, hjust = 0.5,
        stat = "identity",
        size = 2.5,
        label.size = labsize
        ) +
        geom_richtext(aes(
          label = indiv_data,
          family = font_fam
        ),
        y = positions$indiv_data,
        vjust = 0.5, hjust = 0.5,
        stat = "identity",
        size = 2.5,
        label.size = labsize
        )
    }
    
    facet_style <-
      facet_grid(
        rows = vars(outcome_lvl_1),
        scales = "free",
        space = "free",
        drop = T,
        switch = "both"
      )

    base_plot <-
      base_plot +
      geom_linerange(
        x = "NA",
        ymin = head(positions$breaks, 1) - 0.01,
        ymax = tail(positions$breaks, 1) + 0.01,
        size = 20,
        colour = "white"
      ) +
      labs(
        x = NULL,
        y = NULL,
        caption = "<b>r</b> with <b style='color:#636363'>95%</b> and <b style='color:#bdbdbd'>99.9%</b> CIs",
        title = title
      ) +
      facet_style +
      coord_flip(
        clip = "off",
        ylim = positions$lims
      ) +
      scale_y_continuous(breaks = positions$breaks) +
      scale_x_discrete(limits = rev, drop=TRUE) +
      tidyMB::theme_mb() %+replace% theme(
        strip.text.y.left = element_markdown(
          angle = 0,
          hjust = 1
        ),
        axis.title.x = element_text(
          hjust = 1,
          vjust = 0,
          face = "bold"
        ),
        axis.text.y = element_blank(),
        plot.caption = element_markdown(hjust = 0.95, size = 10),
        plot.caption.position = "plot",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.placement = "outside",
        strip.background = element_rect(linetype = "solid")
      )
    
    if (caption) {
      cap <- 
        "**Note:**<br>
      **Indiv. Data:** Individual study data available for reanalysis.<br>
      **Eggers:** *P* > 0.05 for Egger's test of asymmetry, or too few studies to analyse (K < 10).<br>
      **Excess Signif.:** *P* > 0.05 for test for excess significance."
      
      base_plot <-
        base_plot +
        labs(tag = cap) + theme(
          plot.tag.position = positions$tag,
          plot.tag = element_textbox(size = 7,
                                     lineheight = 1)
        )
    }

    return(base_plot)
  }

  edu_positions <-
    list(
      # certain = list(
      #   lims = c(-4, 0.5),
      #   breaks = c(-0.4, -.2, 0, .2, 0.4),
      #   esig = NULL,
      #   eggers = NULL,
      #   indiv_data = NULL,
      #   n = -0.6,
      #   k = -0.85,
      #   i2 = -1.05,
      #   rci = -1.45,
      #   author_year = -2.4,
      #   expo = -3.5,
      #   outcome = -4.2,
      #   tag = NA
      # ),
      # uncertain = list(
      #   lims = c(-4.3, 0.6),
      #   breaks = c(-0.4, -.2, 0, .2, 0.4, 0.6),
      #   esig = -0.55,
      #   eggers = -0.75,
      #   indiv_data = -.95,
      #   n = -1.15,
      #   k = -1.35,
      #   i2 = -1.5,
      #   rci = -1.85,
      #   author_year = -2.7,
      #   expo = -3.7,
      #   outcome = -4.55,
      #   tag = c(0.2,0.01)
      # ),
      certain = list(
        lims = c(-4.3, 0.5),
        breaks = c(-0.4, -.2, 0, .2, 0.4),
        esig = NULL,
        eggers = NULL,
        indiv_data = NULL,
        n = -0.6,
        k = -0.85,
        i2 = -1.05,
        rci = -1.45,
        author_year = -2.4,
        mod = -2.75,
        expo = -3.8,
        outcome = -4.5,
        tag = NA
      ),
      uncertain = list(
        lims = c(-4.3, 0.6),
        breaks = c(-0.4, -.2, 0, .2, 0.4, 0.6),
        esig = -0.55,
        eggers = -0.75,
        indiv_data = -.95,
        n = -1.15,
        k = -1.35,
        i2 = -1.5,
        rci = -1.85,
        author_year = -2.65,
        mod = -2.95,
        expo = -3.85,
        outcome = -4.55,
        tag = c(0.2,0.01)
      )
    )

  nonedu_positions <-
    list(
      certain = list(
        lims = c(-4.55, 0.5),
        breaks = c(-0.4, -.2, 0, .2, 0.4),
        esig = NULL,
        eggers = NULL,
        indiv_data = NULL,
        n = -0.6,
        k = -0.85,
        i2 = -1.05,
        rci = -1.4,
        author_year = -2.25,
        mod = -2.7,
        expo = -3.8,
        outcome = -4.8,
        tag = NA
      ),
      uncertain = list(
        lims = c(-5.5, 0.9),
        breaks = c(-1, -0.8, -0.6, -0.4, -.2, 0, .2, 0.4, 0.6, 0.8),
        esig = -1.15,
        eggers = -1.40,
        indiv_data = -1.65,
        n = -1.85,
        k = -2.05,
        i2 = -2.2,
        rci = -2.5,
        author_year = -3.35,
        mod = -3.75,
        expo = -4.9,
        outcome = -5.8,
        tag = c(0.2,0)
      )
    )

  plot_params <- list(
    list(
      filename = "Forest plot for Education.pdf",
      title = "Associations Between Exposures and Education Outcomes",
      categories = "Education",
      certain = TRUE,
      pos = edu_positions$certain,
      dims = c(10, 6),
      moderators = FALSE,
      caption = FALSE
    ),
    list(
      filename = "Forest plot for Health-related Outcomes.pdf",
      title = "Associations Between Exposures and Health-related Outcomes",
      categories = c("Psychology", "Health Behaviour", "Physical Health"),
      certain = TRUE,
      pos = nonedu_positions$certain,
      dims = c(10, 6),
      moderators = FALSE,
      caption = FALSE
    ),
    list(
      filename = "Supplemental Forest plot for Education.pdf",
      title = "Associations Between Exposures and Education Outcomes",
      categories = "Education",
      certain = FALSE,
      pos = edu_positions$uncertain,
      dims = c(14, 12),
      moderators = FALSE,
      caption = TRUE
    ),
    list(
      filename = "Supplemental Forest plot for Health-related Outcomes.pdf",
      title = "Associations Between Exposures and Health-related Outcomes",
      categories = c("Psychology", "Health Behaviour", "Physical Health"),
      certain = FALSE,
      pos = nonedu_positions$uncertain,
      dims = c(16, 32),
      moderators = FALSE,
      caption = TRUE
    )
    # list(
    #   filename = "Forest plot for Education With Moderators.pdf",
    #   title = "Associations Between Exposures and Education Outcomes",
    #   categories = "Education",
    #   certain = TRUE,
    #   pos = edu_positions$certain_mods,
    #   dims = c(10, 6),
    #   moderators = TRUE,
    #   caption = FALSE
    # ),
    # list(
    #   filename = "Supplemental Forest plot for Education with Moderators.pdf",
    #   title = "Associations Between Exposures and Education Outcomes",
    #   categories = "Education",
    #   certain = FALSE,
    #   pos = edu_positions$uncertain_mods,
    #   dims = c(12, 10),
    #   moderators = TRUE,
    #   caption = TRUE
    # )
  )

  plots <- vector(mode = "list", length = 4)

  for (i in 1:length(plot_params)) {
    plots[[i]]$plot <- gen_plot(
      plot_params[[i]]$categories,
      plot_params[[i]]$certain,
      plot_params[[i]]$title,
      plot_params[[i]]$pos,
      plot_params[[i]]$caption,
      debug = FALSE
    )
    plots[[i]]$dims <- plot_params[[i]]$dims
    plots[[i]]$filename <- plot_params[[i]]$filename
    plots[[i]]$categories <- plot_params[[i]]$categories
    plots[[i]]$certain <- plot_params[[i]]$certain
  }

  return(plots)
}

save_plots <- function(plots) {
  file_name <- here::here("figure", plots[[1]]$filename)
  

  ggsave(
    filename = file_name,
    plot = plots[[1]]$plot,
    width = plots[[1]]$dims[[1]],
    height = plots[[1]]$dims[[2]]
  )
  
  # Upload to GDrive
  drive_put(here::here("figure", plots[[1]]$filename),
            path = as_id("https://drive.google.com/drive/folders/1xvn1B4bGH7hr6yBvDGUfEF7F_eO3Qeml"))
  
  return(file_name)
}
