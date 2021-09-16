make_plots <- function(combined_effects) {
  q <- combined_effects %>%
    filter(use_effect == TRUE) %>%
    select(-use_effect)

  # fixing the exposures that someone forgot to capitalise
  q$plain_language_exposure <- paste(toupper(substr(q$plain_language_exposure, 1, 1)),
    substr(q$plain_language_exposure, 2, nchar(q$plain_language_exposure)),
    sep = ""
  )

  q$plain_language_exposure <- gsub(
    "^Intervention:",
    "Screen-based intervention:",
    q$plain_language_exposure
  )

  # Take overall outcome into a variable and remove that from the sub-variable
  q$outcome_lvl_1 <- factor(gsub(":.*", "", q$plain_language_outcome))
  q$plain_language_outcome <- gsub(".*: ", "", q$plain_language_outcome)
  q$outcome_category <- factor(str_to_title(q$outcome_category))


  # Deal with long labels
  trimmer <- Vectorize(function(string, max_len) {
    if (str_length(string) > max_len) {
      # If there's a colon, always break there to keep it neat
      if (str_detect(string, ":")) {
        string <- gsub(":", ":<br/>", string)
      }
      else {
        string <- str_replace_all(str_wrap(string, width = str_length(string) / 1.5), "\n", "<br/>")
      }
    }

    return(string)
  })


  q$plain_language_outcome <- factor(trimmer(q$plain_language_outcome, 40))
  q$plain_language_exposure <- factor(trimmer(q$plain_language_exposure, 55))


  # Cleaning data before plotting
  q$i2[is.null(q$i2)] <- NA
  q$i2 <- as.character(paste(round(as.numeric(q$i2), 0), "%", sep = ""))
  q$i2[grepl("NA", q$i2)] <- NA
  
  # This is no longer used but keeping for backup
  q$rci <- with(q, paste(format(round(r, 2), nsmall = 2),
                         " [", format(round(cilb95, 2), nsmall = 2), ", ",
                         format(round(ciub95, 2), nsmall = 2), "]",
                         sep = ""
  ))
  
  # Adding both 95 and 99.9% conf intervals
  # q$r_s <- as.character(format(round(q$r, 2), nsmall = 2))
  # q$cilb95 <- as.character(format(round(q$cilb95, 2), nsmall = 2))
  # q$cilb999 <- as.character(format(round(q$cilb999, 2), nsmall = 2))
  # q$ciub95 <- as.character(format(round(q$ciub95, 2), nsmall = 2))
  # q$ciub999 <- as.character(format(round(q$ciub999, 2), nsmall = 2))

  # q$n <- format(q$n, big.mark = ",")
  q$n[grepl("NA", q$n)] <- "—"
  q <- ungroup(q)

  q$certainty <- str_to_title(q$certainty)
  
  
  
  # Make forest plots ####

  # Make output lists
  plots <- vector(mode = "list", length = length(unique(q$outcome_category))*2)
  # names(plots) <- unique(q$outcome_category)

  for (i in 1:length(unique(q$outcome_category))) {
     # i <- 2
    edu <- filter(q, as.numeric(outcome_category) == i)

    plot_title <- paste("Effect on ",
      levels(q$outcome_category)[i],
      " Outcomes (r with 95% & 99.9% CI)",
      sep = ""
    )
    # Create a row for labeling the plot
    edu <- add_row(edu)
    last <- nrow(edu)
    # levels(edu$recoded_exposure)
    edu$plain_language_exposure <- fct_expand(edu$plain_language_exposure, "**Exposure**") %>%
      fct_relevel("**Exposure**", "Screen use: General")
    edu$plain_language_exposure[last] <- "**Exposure**"

    edu$author_year[last] <- "**Lead Author, Date**"

    edu$n[last] <- "**N**"
    edu$k[last] <- "**K**"
    edu$i2[last] <- "**I^2**"
    edu$risk[last] <- "plain"
    edu$certainty[last] <- "**Credible**"

    edu$plain_language_outcome <- fct_expand(edu$plain_language_outcome, "**Specific Outcome**") %>%
      fct_relevel("**Specific Outcome**")
    edu$plain_language_outcome[last] <- "**Specific Outcome**"

    edu$outcome_lvl_1 <- fct_expand(edu$outcome_lvl_1, "Outcome") %>%
      fct_relevel("Outcome")
    edu$outcome_lvl_1[last] <- "Outcome"
    
    # edu$cilb999[last] <- "**Lower 99.9% CI**"
    # edu$cilb95[last] <- "**Lower 95% CI**"
    # edu$r[last] <- "**<i>r</i>**"
    # edu$ciub95[last] <- "**Upper 95% CI**"
    # edu$ciub999[last] <- "**Upper 99.9% CI**"


    edu$rci[last] <- "**<i>r</i> with 95% CI**"

    edu <- arrange(
      edu,
      outcome_lvl_1,
      plain_language_outcome,
      plain_language_exposure,
      k
    )
    edu$row_num <- as.factor(1:nrow(edu))

    edu$shape <- 18
    
    edu$cilb999[edu$cilb999 < -.4] <- -.4
    edu$cilb95[edu$cilb95 < -.4] <- -.4
    edu$ciub999[edu$ciub999 < -.4] <- -.4
    edu$ciub95[edu$ciub95 < -.4] <- -.4
    edu$r[edu$r < -.4] <- -.39
    edu$shape[edu$r == -.39] <- 60

    edu$cilb999[edu$cilb999 > .4] <- .4
    edu$cilb95[edu$cilb95 > .4] <- .4
    edu$ciub999[edu$ciub999 > .4] <- .4
    edu$ciub95[edu$ciub95 > .4] <- .4
    edu$r[edu$r > .4] <- .39
    edu$shape[edu$r == .39] <- 62
    
    
    edu_tmp <- edu %>% filter(certainty!="Unclear")
    
    p1 <- 
      ggplot(
      edu_tmp,
      aes(
        x = row_num,
        y = r,
        # ymin = cilb,
        # ymax = ciub,
        label = author_year,
        family = "Times",
        shape = shape,
        #fontface = risk
      )
    ) +
        geom_point(size=2) +
        geom_linerange(
            #99.9% CIs
            aes(ymin=cilb999,
                ymax=ciub999),
            size=0.5,
            position = position_identity()
        )+
        geom_linerange(
            #95% CIs
            aes(ymin=cilb95,
                ymax=ciub95),
            size=1,
            position = position_identity()
        ) +
      # geom_pointrange(
      #   #99.9% CIs
      #   aes(ymin=cilb999,
      #       ymax=ciub999),
      #   size=0.5
      #   ) +
      # geom_pointrange(
      #   #95% CIs
      #   aes(ymin=cilb95,
      #       ymax=ciub95),
      #   size=1.5
      # ) +
      geom_richtext(
        y = -.65, label = edu_tmp$certainty,
        vjust = 0.5, hjust = 0.5,
        stat = "identity",
        size = 2.5,
        label.size = NA
      ) +
      geom_richtext(
        y = -.95, label = edu_tmp$n,
        vjust = 0.5, hjust = 0.5,
        stat = "identity",
        size = 2.5,
        label.size = NA
      ) +
      geom_richtext(
        y = -1.05, label = edu_tmp$k,
        vjust = 0.5, hjust = 0.5,
        stat = "identity",
        size = 2.5,
        label.size = NA
      ) +
      geom_richtext(
        y = -1.2, label = edu_tmp$i2,
        vjust = 0.5, hjust = 0.5,
        stat = "identity",
        size = 2.5,
        label.size = NA
      ) +
      geom_richtext(
        y = -1.5, label = edu_tmp$rci,
        vjust = 0.5, hjust = 0.5,
        stat = "identity",
        size = 2.5,
        label.size = NA
      ) +
      geom_richtext(
        y = -2.2,
        vjust = 0.5, hjust = 0,
        stat = "identity",
        size = 2.5,
        label.size = NA
      ) +
      geom_richtext(
        y = -3.4, label = edu_tmp$plain_language_exposure,
        vjust = 0.5, hjust = 0,
        stat = "identity",
        size = 2.5,
        label.size = NA
      ) +
      geom_richtext(
        y = -4.3, label = edu_tmp$plain_language_outcome,
        vjust = 0.5, hjust = 0,
        stat = "identity",
        size = 2.5,
        label.size = NA
      ) +
      geom_hline(yintercept = 0) +
      labs(
        x = NULL,
        y = plot_title
      ) +
      facet_grid(
        rows = vars(outcome_lvl_1),
        scales = "free",
        space = "free",
        drop = T,
        switch = "both"
      ) +
      coord_flip(ylim = c(-4.2, .4)) +
      scale_shape_identity() +
      scale_x_discrete(limits = rev) +
      scale_y_continuous(breaks = c(-.4, -.2, 0, .2, .4)) +
        scale_fill_discrete(c("black","white")) + 
      tidyMB::theme_mb() %+replace% theme(
        strip.text.y.left = element_text(
          angle = 0,
          hjust = 1
        ),
        axis.title.x = element_text(
          hjust = 1,
          vjust = 0,
          face = "bold"
        ),
        axis.text.y = element_blank(
          # colour = "white"
        ),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.placement = "outside",
        strip.background = element_rect(linetype = "solid"),
        text = element_text(family = "Times")
      )
    
    p1

    plots[[i]]$plot <- p1
    plots[[i]]$name <- levels(q$outcome_category)[i]
    
    # Plots that don't meet criteria
    
    edu_tmp <- edu %>% filter(certainty!="Meets Criteria")
    p2 <- 
      ggplot(
        edu_tmp,
        aes(
          x = row_num,
          y = r,
          # ymin = cilb,
          # ymax = ciub,
          label = author_year,
          family = "Times",
          shape = shape,
          #fontface = risk
        )
      ) +
      geom_point(size=2) +
      geom_linerange(
        #99.9% CIs
        aes(ymin=cilb999,
            ymax=ciub999),
        size=0.5,
        position = position_identity()
      )+
      geom_linerange(
        #95% CIs
        aes(ymin=cilb95,
            ymax=ciub95),
        size=1,
        position = position_identity()
      ) +
      # geom_pointrange(
      #   #99.9% CIs
      #   aes(ymin=cilb999,
      #       ymax=ciub999),
      #   size=0.5
      #   ) +
      # geom_pointrange(
      #   #95% CIs
      #   aes(ymin=cilb95,
      #       ymax=ciub95),
      #   size=1.5
    # ) +
    geom_richtext(
      y = -.65, label = edu_tmp$certainty,
      vjust = 0.5, hjust = 0.5,
      stat = "identity",
      size = 2.5,
      label.size = NA
    ) +
      geom_richtext(
        y = -.95, label = edu_tmp$n,
        vjust = 0.5, hjust = 0.5,
        stat = "identity",
        size = 2.5,
        label.size = NA
      ) +
      geom_richtext(
        y = -1.05, label = edu_tmp$k,
        vjust = 0.5, hjust = 0.5,
        stat = "identity",
        size = 2.5,
        label.size = NA
      ) +
      geom_richtext(
        y = -1.2, label = edu_tmp$i2,
        vjust = 0.5, hjust = 0.5,
        stat = "identity",
        size = 2.5,
        label.size = NA
      ) +
      geom_richtext(
        y = -1.5, label = edu_tmp$rci,
        vjust = 0.5, hjust = 0.5,
        stat = "identity",
        size = 2.5,
        label.size = NA
      ) +
      geom_richtext(
        y = -2.2,
        vjust = 0.5, hjust = 0,
        stat = "identity",
        size = 2.5,
        label.size = NA
      ) +
      geom_richtext(
        y = -3.4, label = edu_tmp$plain_language_exposure,
        vjust = 0.5, hjust = 0,
        stat = "identity",
        size = 2.5,
        label.size = NA
      ) +
      geom_richtext(
        y = -4.3, label = edu_tmp$plain_language_outcome,
        vjust = 0.5, hjust = 0,
        stat = "identity",
        size = 2.5,
        label.size = NA
      ) +
      geom_hline(yintercept = 0) +
      labs(
        x = NULL,
        y = plot_title
      ) +
      facet_grid(
        rows = vars(outcome_lvl_1),
        scales = "free",
        space = "free",
        drop = T,
        switch = "both"
      ) +
      coord_flip(ylim = c(-4.2, .4)) +
      scale_shape_identity() +
      scale_x_discrete(limits = rev) +
      scale_y_continuous(breaks = c(-.4, -.2, 0, .2, .4)) +
      scale_fill_discrete(c("black","white")) + 
      tidyMB::theme_mb() %+replace% theme(
        strip.text.y.left = element_text(
          angle = 0,
          hjust = 1
        ),
        axis.title.x = element_text(
          hjust = 1,
          vjust = 0,
          face = "bold"
        ),
        axis.text.y = element_blank(
          # colour = "white"
        ),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.placement = "outside",
        strip.background = element_rect(linetype = "solid"),
        text = element_text(family = "Times")
      )
    
    p2
    
    plots[[i]]$suppplot <- p2
  }
  return(plots)
}

save_plots <- function(plots){
  
  file_name <- here::here("figure", paste("Forest plot for ", plots[[1]]$name, ".pdf", sep = ""))
  
  #plots[[1]]$plot
  ggsave(
    filename = file_name,
    plot = plots[[1]]$plot,
    width = 14,
    height = 13
  )

  return(file_name)
}