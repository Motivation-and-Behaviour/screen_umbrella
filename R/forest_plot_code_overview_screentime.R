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
  combined_effects <- combined_effects %>%
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
      font_fam = "fontawesome-webfont"
    ) %>%
    arrange(
      outcome_lvl_1,
      plain_language_outcome,
      plain_language_exposure
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
      font_fam = "Times"
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
    )


  gen_plot <- function(categories, certain, title, positions, debug = FALSE) {
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


    if (length(categories) > 1) {
      facet_style <-
        ggh4x::facet_nested(
          rows = vars(outcome_category, outcome_lvl_1),
          scales = "free",
          space = "free",
          drop = T,
          switch = "both"
        )
    } else {
      facet_style <-
        facet_grid(
          rows = vars(outcome_lvl_1),
          scales = "free",
          space = "free",
          drop = T,
          switch = "both"
        )
    }



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
      scale_x_discrete(limits = rev) +
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

    return(base_plot)
  }

  edu_positions <-
    list(
      certain = list(
        lims = c(-4, 0.5),
        breaks = c(-0.4, -.2, 0, .2, 0.4),
        esig = NULL,
        eggers = NULL,
        indiv_data = NULL,
        n = -0.6,
        k = -0.85,
        i2 = -1.05,
        rci = -1.45,
        author_year = -2.4,
        expo = -3.5,
        outcome = -4.2
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
        author_year = -2.7,
        expo = -3.7,
        outcome = -4.55
      )
    )

  nonedu_positions <-
    list(
      certain = list(
        lims = c(-4.0, 0.5),
        breaks = c(-0.4, -.2, 0, .2, 0.4),
        esig = NULL,
        eggers = NULL,
        indiv_data = NULL,
        n = -0.6,
        k = -0.85,
        i2 = -1.05,
        rci = -1.4,
        author_year = -2.35,
        expo = -3.35,
        outcome = -4.25
      ),
      uncertain = list(
        lims = c(-5.7, 0.9),
        breaks = c(-1, -0.8, -0.6, -0.4, -.2, 0, .2, 0.4, 0.6, 0.8),
        esig = -1.15,
        eggers = -1.40,
        indiv_data = -1.65,
        n = -1.85,
        k = -2.05,
        i2 = -2.2,
        rci = -2.6,
        author_year = -3.6,
        expo = -4.85,
        outcome = -6.05
      )
    )

  plot_params <- list(
    list(
      filename = "Forest plot for Education.pdf",
      title = "Effect of Exposures on Education Outcomes",
      categories = "Education",
      certain = TRUE,
      pos = edu_positions$certain,
      dims = c(10, 6)
    ),
    list(
      filename = "Forest plot for Health-related Outcomes.pdf",
      title = "Effect of Exposures on Health-related Outcomes",
      categories = c("Psychology", "Health Behaviour", "Physical Health"),
      certain = TRUE,
      pos = nonedu_positions$certain,
      dims = c(10, 6)
    ),
    list(
      filename = "Supplemental Forest plot for Education.pdf",
      title = "Effect of Exposures on Education Outcomes",
      categories = "Education",
      certain = FALSE,
      pos = edu_positions$uncertain,
      dims = c(12, 8)
    ),
    list(
      filename = "Supplemental Forest plot for Health-related Outcomes.pdf",
      title = "Effect of Exposures on Health-related Outcomes",
      categories = c("Psychology", "Health Behaviour", "Physical Health"),
      certain = FALSE,
      pos = nonedu_positions$uncertain,
      dims = c(16, 25)
    )
  )

  plots <- vector(mode = "list", length = 4)

  for (i in 1:length(plot_params)) {
    plots[[i]]$plot <- gen_plot(
      plot_params[[i]]$categories,
      plot_params[[i]]$certain,
      plot_params[[i]]$title,
      plot_params[[i]]$pos
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

  return(file_name)
}