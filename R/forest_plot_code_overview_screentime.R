library(googlesheets4)
library(tidyverse)
library(janitor)
library(ggtext)
library(tidyMB)

# remove existing environment
#rm(list = ls())

raw <- read_sheet() 
simple <- simplify_effects(raw)
d <- convert_data(simple)

q <- clean_names(d)
q <- remove_empty(q, which = c("rows", "cols"))

# Relabelling stuff
q <- filter(q, r < .95)
q$sig <- ((q$value_ci_lower_bound_consensus_r * q$value_ci_upper_bound_consensus_r) > 0)
q$sig <- q$sig * .7 + .3
q$author_year <- paste(q$review_lead_author, ", ", q$review_year, sep = "")

q$outcome_category <- as.factor(q$outcome_category)

q$recoded_exposure <- gsub("_", " ", q$recoded_exposure)
q$recoded_exposure <- str_to_title(q$recoded_exposure)
q$recoded_exposure <- fix_acronyms(q$recoded_exposure)
q$recoded_exposure <- gsub("\nDevice: Computer", "", q$recoded_exposure)
q$recoded_exposure <- gsub("Content: ", "", q$recoded_exposure)

q$recoded_exposure <- as.factor(q$recoded_exposure)

q$outcome <- gsub("_", " ", q$outcome)
q$outcome <- gsub("^learning ", "", q$outcome)
q$outcome <- gsub("literacy ", "literacy: ", q$outcome)
q$outcome <- str_to_title(q$outcome)
q$outcome <- fix_acronyms(q$outcome)

fix_acronyms <- function(chr_vector){
  chr_vector <- gsub("Tv", "TV", chr_vector)
  chr_vector <- gsub("Bmi", "BMI", chr_vector)
  chr_vector <- gsub("Of", "of", chr_vector)
}




q$moderator_level <- gsub("_", " ", q$moderator_level)
q$moderator_level <- str_to_title(q$moderator_level)
q$moderator_level <- fix_acronyms(q$moderator_level)

q$effect_label <- paste(q$outcome, " (", q$moderator_level, ")", sep = "")
q$effect_label <- gsub(" \\(Overall\\)", "", q$effect_label)
q$effect_label <- gsub(" \\(Random Effects\\)", "", q$effect_label)
q <- q[!grepl("Fixed Effects", q$effect_label), ]

alternating <- rep(c("white", "light grey"), 100)

unique(q$moderator_level)

q$value_ci_lower_bound_consensus_r[q$value_ci_lower_bound_consensus_r < -.4] <- -.4


#### Forest plot for education####

for (i in 1:length(unique(q$outcome_category))) {
  # i <- 1
  edu <- subset(q, as.numeric(q$outcome_category) == i)
  edu$value_ci_lower_bound_consensus_r[edu$value_ci_lower_bound_consensus_r < -.4] <- -.4
  plot_title <- paste("Effect on ",
    levels(q$outcome_category)[i],
    " Outcomes (r with 95%CI)",
    sep = ""
  )
  # Create a row for labeling the plot
  edu <- rbind(edu, NA)
  last <- nrow(edu)

  edu$recoded_exposure <- forcats::fct_expand(edu$recoded_exposure, "Exposure") %>%
    forcats::fct_relevel("Exposure", "Overall Screentime")
  edu$recoded_exposure[last] <- "Exposure"
  edu$author_year[last] <- "**Study Label**"

  edu$combined_n <- format(edu$combined_n, big.mark = ",")
  edu$combined_n[grepl("NA", edu$combined_n)] <- NA
  edu$combined_n[last] <- "**N**"
  edu$k[last] <- "**K**"

  edu$i2_calculated <- as.character(paste(round(as.numeric(edu$i2_calculated), 0), "%", sep = ""))
  edu$i2_calculated[grepl("NA", edu$i2_calculated)] <- NA
  edu$i2_calculated[last] <- "**I^2**"

  edu$effect_label <- factor(edu$effect_label,
    levels = unique(edu$effect_label[order(edu$effect_label)])
  )
  
  edu$outcome[last] <- "**Outcome**"
  edu$moderator_level[last] <- "**Moderator Level**"
  
  edu$outcome[edu$moderator_level=="Overall"&
                !is.na(edu$moderator_level)] <- paste("**", edu$outcome[edu$moderator_level=="Overall" &
                                                                           !is.na(edu$moderator_level)], "**", sep = "")
  edu$moderator_level[edu$moderator_level=="Overall"&
                !is.na(edu$moderator_level)] <- paste("**", edu$moderator_level[edu$moderator_level=="Overall" &
                                                                          !is.na(edu$moderator_level)], "**", sep = "")
  
  edu$effect_label <- forcats::fct_expand(edu$effect_label, "Outcome (With Moderators)")
  edu$effect_label[last] <- "Outcome (With Moderators)"
  levels_in_order <- edu$effect_label[with(edu, order(outcome, moderator_level))]
  levels_in_order <- c(as.character(edu$effect_label[edu$moderator_level=="**Overall**"]),
                       as.character(levels_in_order))
  edu$effect_label <- factor(edu$effect_label, levels = unique(levels_in_order))
  edu$effect_label <- forcats::fct_relevel(edu$effect_label, as.character(unique(edu$effect_label[edu$moderator_level=="**Overall**" & !is.na(edu$moderator_level)])))
  edu$facet <- as.character(as.numeric(edu$effect_label))
  p1 <- ggplot(
    edu,
    aes(
      x = facet,
      y = value_consensus_r,
      ymin = value_ci_lower_bound_consensus_r,
      ymax = value_ci_upper_bound_consensus_r,
      label = author_year,
      family = "Times"
    )
  ) +
    geom_pointrange(
      shape = 18,
      alpha = edu$sig
    ) +
    geom_richtext(
      y = -.45, label = edu$combined_n,
      vjust = 0.5, hjust = 1,
      stat = "identity",
      size = 2.5,
      label.size = NA
    ) +
    geom_richtext(
      y = -.6, label = edu$k,
      vjust = 0.5, hjust = 1,
      stat = "identity",
      size = 2.5,
      label.size = NA
    ) +
    geom_richtext(
      y = -.7, label = edu$i2_calculated,
      vjust = 0.5, hjust = 1,
      stat = "identity",
      size = 2.5,
      label.size = NA
    ) +
    geom_richtext(
      y = -1,
      vjust = 0.5, hjust = 0,
      stat = "identity",
      size = 2.5,
      label.size = NA
    ) +
    geom_richtext(
      y = -3, label = edu$outcome,
      vjust = 0.5, hjust = 0,
      stat = "identity",
      size = 2.5,
      label.size = NA
    ) +
    geom_richtext(
      y = -2, label = edu$moderator_level,
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
      rows = vars(recoded_exposure),
      scales = "free",
      space = "free",
      drop = T,
      switch = "both"
    ) +
    coord_flip(ylim = c(-3, .4)) +
    scale_x_discrete(limits=rev) + 
    scale_y_continuous(breaks = c(-.4, -.2, 0, .2, .4)) +
    tidyMB::theme_mb() %+replace% theme(
      strip.text.y.left = element_text(
        angle = 0,
        hjust = 1
      ),
      axis.title.x = element_text(
        hjust = .97,
        vjust = 0,
        face = "bold"
      ),
      axis.text.y = element_text(
        colour = "white"
      ),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      strip.placement = "outside",
      strip.background = element_rect(linetype = "solid"),
      text = element_text(family = "Times")
    )
  p1
  ggsave(paste("Forest plot for ", levels(q$outcome_category)[i], ".pdf", sep = ""), plot = p1, width = 14, height = 12)
}
