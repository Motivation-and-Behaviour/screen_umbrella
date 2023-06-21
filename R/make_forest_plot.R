t #' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param combined_effects
#' @param plot_params
#' @param debug
#' @return
#' @author Taren Sanders
#' @export
make_forest_plot <- function(combined_effects, plot_params, debug = TRUE) {
  if (debug) labsize <- 0.1 else labsize <- NA

  plot_effects <-
    combined_effects %>%
    filter(use_effect) %>%
    mutate(
      # Truncate large effects
      cilb999 = if_else(cilb999 < -1, -1, cilb999),
      ciub999 = if_else(ciub999 > 1, 1, ciub999),
      # Overall outcome into a variable and remove that from the sub-variable
      outcome_lvl_1 = factor(gsub(":.*", "", plain_language_outcome)),
      plain_language_outcome = gsub(".*: ", "", plain_language_outcome),
      outcome_category = factor(str_to_title(outcome_category)),
      plain_language_exposure = str_replace(
        plain_language_exposure,
        "^Intervention:",
        "Screen-based intervention:"
      ),
      # Fix long labels
      plain_language_outcome = trimmer(plain_language_outcome, 20),
      plain_language_exposure = trimmer(plain_language_exposure, 26),
      author_year = trimmer(author_year, 14),
      study_design = trimmer(study_design, 8),
      sample_type = trimmer(sample_type, 7),
      i2 = scales::percent(i2, 2, scale = 1),
      n = scales::label_comma(accuracy = 1)(n),
      k = as.character(k),
      rci = paste(format(round(r, 2), nsmall = 2),
        "<br/> [", format(round(cilb95, 2), nsmall = 2), ", ",
        format(round(ciub95, 2), nsmall = 2), "]",
        sep = ""
      ),
      indiv_data = fontawesome(if_else(source == "reanalysis",
        "fa-check", "fa-times"
      )),
      eggers = case_when(
        source == "reported" ~ fontawesome("fa-minus"),
        reanalysis_eggers_p > 0.05 ~ fontawesome("fa-check"),
        TRUE ~ fontawesome("fa-times")
      ),
      esig = case_when(
        source == "reported" ~ fontawesome("fa-minus"),
        reanalysis_tes_p > 0.05 ~ fontawesome("fa-check"),
        TRUE ~ fontawesome("fa-times")
      ),
      font_fam = "fontawesome-webfont",
      age_group = factor(age_group,
        levels = c("Mixed", "Young", "Children", "Adolescents")
      )
    ) %>%
    arrange(
      outcome_lvl_1,
      plain_language_outcome,
      plain_language_exposure,
      age_group,
      study_design
    ) %>%
    mutate(row_num = as.factor(row_number())) %>%
    add_row(
      outcome_lvl_1 = "**Outcome**",
      plain_language_outcome = "**Specific <br/>Outcome**",
      plain_language_exposure = "**Exposure**",
      n = "**N**",
      k = "**K**",
      i2 = "**I<sup>2</sup>**",
      rci = "**<i>r</i> with <br/>95% CI**",
      author_year = "**Lead Author, <br/>Date**",
      row_num = "NA",
      indiv_data = "**Indiv.<br/>Data**",
      eggers = "**Eggers**",
      esig = "**Excess<br/>Signif.**",
      outcome_category = "**Outcome Category**",
      font_fam = "sans",
      age_group = "**Age <br/>Group**",
      sample_type = "**Pop.**",
      study_design = "**Study <br/>Design**"
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
      outcome_category = fct_expand(
        outcome_category,
        "**Outcome Category**"
      ) %>%
        fct_relevel("**Outcome Category**"),
      age_group = fct_expand(age_group, "**Age Group**") %>%
        fct_relevel("**Age Group**")
    ) %>%
    filter(
      outcome_category %in% c(plot_params$categories, "**Outcome Category**")
    )

  if (plot_params$certain) {
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
    geom_linerange(
      aes(
        ymin = cilb999,
        ymax = ciub999
      ),
      linewidth = 2,
      colour = "#bdbdbd"
    ) +
    geom_linerange(
      aes(
        ymin = cilb95,
        ymax = ciub95
      ),
      linewidth = 2,
      colour = "#636363"
    ) +
    geom_hline(aes(yintercept = 0),
      lty = 1,
      linewidth = 0.5
    ) +
    geom_point(
      size = 2, shape = 21,
      fill = "#f0f0f0"
    ) +
    geom_richtext(aes(label = n),
      y = plot_params$pos$n,
      vjust = 0.5, hjust = 0.5,
      stat = "identity",
      size = 2.5,
      label.size = labsize
    ) +
    geom_richtext(aes(label = k),
      y = plot_params$pos$k,
      vjust = 0.5, hjust = 0.5,
      stat = "identity",
      size = 2.5,
      label.size = labsize
    ) +
    geom_richtext(aes(label = i2),
      y = plot_params$pos$i2,
      vjust = 0.5, hjust = 0.5,
      stat = "identity",
      size = 2.5,
      label.size = labsize
    ) +
    geom_richtext(aes(label = rci),
      y = plot_params$pos$rci,
      vjust = 0.5, hjust = 0.5,
      stat = "identity",
      size = 2.5,
      label.size = labsize
    ) +
    geom_richtext(aes(label = author_year),
      y = plot_params$pos$author_year,
      vjust = 0.5, hjust = 0,
      stat = "identity",
      size = 2.5,
      label.size = labsize
    ) +
    geom_richtext(aes(label = plain_language_exposure),
      y = plot_params$pos$expo,
      vjust = 0.5, hjust = 0,
      stat = "identity",
      size = 2.5,
      label.size = labsize
    ) +
    geom_richtext(aes(label = plain_language_outcome),
      y = plot_params$pos$outcome,
      vjust = 0.5, hjust = 0,
      stat = "identity",
      size = 2.5,
      label.size = labsize
    ) +
    geom_richtext(aes(label = age_group),
      y = plot_params$pos$mod,
      vjust = 0.5, hjust = 0,
      stat = "identity",
      size = 2.5,
      label.size = labsize
    ) +
    geom_richtext(aes(label = study_design),
      y = plot_params$pos$design,
      vjust = 0.5, hjust = 0,
      stat = "identity",
      size = 2.5,
      label.size = labsize
    ) +
    geom_richtext(aes(label = sample_type),
      y = plot_params$pos$pop,
      vjust = 0.5, hjust = 0,
      stat = "identity",
      size = 2.5,
      label.size = labsize
    )

  if (!plot_params$certain) {
    base_plot <-
      base_plot +
      geom_richtext(
        aes(
          label = esig,
          family = font_fam
        ),
        y = plot_params$pos$esig,
        vjust = 0.5, hjust = 0.5,
        stat = "identity",
        size = 2.5,
        label.size = labsize
      ) +
      geom_richtext(
        aes(
          label = eggers,
          family = font_fam
        ),
        y = plot_params$pos$eggers,
        vjust = 0.5, hjust = 0.5,
        stat = "identity",
        size = 2.5,
        label.size = labsize
      ) +
      geom_richtext(
        aes(
          label = indiv_data,
          family = font_fam
        ),
        y = plot_params$pos$indiv_data,
        vjust = 0.5, hjust = 0.5,
        stat = "identity",
        size = 2.5,
        label.size = labsize
      )
  }

  base_plot <-
    base_plot +
    geom_linerange(
      x = "NA",
      ymin = head(plot_params$pos$breaks, 1) - 0.01,
      ymax = tail(plot_params$pos$breaks, 1) + 0.01,
      linewidth = 20,
      colour = "white"
    ) +
    labs(
      x = NULL,
      y = NULL,
      caption = "<b>r</b> with <b style='color:#636363'>95%</b> and <b style='color:#bdbdbd'>99.9%</b> CIs", # nolint
      title = plot_params$title
    ) +
    facet_grid(
      rows = vars(outcome_lvl_1),
      scales = "free",
      space = "free",
      drop = TRUE,
      switch = "both"
    ) +
    coord_flip(
      clip = "off",
      ylim = plot_params$pos$lims
    ) +
    scale_y_continuous(breaks = plot_params$pos$breaks) +
    scale_x_discrete(limits = rev, drop = TRUE) +
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
      strip.background = element_rect(linetype = "solid"),
      plot.margin = unit(c(1, -5, 1, 1), "mm")
    )

  if (plot_params$caption) {
    cap <-
      "**Note:**<br>
      **Indiv. Data:** Individual study data available for reanalysis.<br>
      **Eggers:** *P* > 0.05 for Egger's test of asymmetry,
      or too few studies to analyse (K < 10).<br>
      **Excess Signif.:** *P* > 0.05 for test for excess significance."

    base_plot <-
      base_plot +
      labs(tag = cap) + theme(
        plot.tag.position = plot_params$pos$tag,
        plot.tag = element_textbox(
          size = 7,
          lineheight = 1
        )
      )
  }

  return(base_plot)
}

trimmer <- Vectorize(function(string, max_len) {
  # Deal with long labels
  if (str_length(string) > max_len) {
    # If there's a colon, always break there to keep it neat
    # if (str_detect(string, ":")) {
    #   string <- gsub(":", ":<br/>", string)
    # } else {
    string <- str_replace_all(str_wrap(string,
      # width = str_length(string) / 1.5,
      width = max_len,
      whitespace_only = FALSE, exdent = 2
    ), "\n", "<br/>")
    # }
  }

  return(string)
})
