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

<<<<<<< HEAD
q <- clean_names(d) %>%
  dplyr::rename(r = value_consensus,
                outcome_category = outcome_level_1,
                outcome = outcome_level_2,
                recoded_exposure = recoded_content,
                moderator_level = moderator_level_recoded,
                moderator_category = moderator_category_recoded,
                k = k_number_of_studies_for_this_effect_consensus,
                n = n_combined_sample_across_studies_examining_this_effect_consensus
                ) %>%
  remove_empty(which = c("rows", "cols")) %>%
  filter(usable_effect_size == TRUE,
         r < .99,
         n >= 1000,
         outcome_category != "N/A")
  
# Relabelling stuff
q$sig <- ((q$value_ci_lower_bound_consensus * q$value_ci_upper_bound_consensus) > 0)
q$sig <- q$sig * .7 + .3
q$author_year <- paste(q$first_author, ", ", q$year_of_publication, sep = "")

fix_acronyms <- function(chr_vector){
  chr_vector <- gsub("Tv", "TV", chr_vector)
  chr_vector <- gsub("Bmi", "BMI", chr_vector)
  chr_vector <- gsub("Of", "of", chr_vector)
  chr_vector <- gsub("<", "&lt;", chr_vector)
  chr_vector <- gsub(">", "&gt;", chr_vector)
}

q$outcome_category <- str_to_title(q$outcome_category)
q$outcome_category <- as.factor(q$outcome_category)


=======
q <- clean_names(d)
q <- remove_empty(q, which = c("rows", "cols"))

# Relabelling stuff
q <- filter(q, r < .95)
q$sig <- ((q$value_ci_lower_bound_consensus_r * q$value_ci_upper_bound_consensus_r) > 0)
q$sig <- q$sig * .7 + .3
q$author_year <- paste(q$review_lead_author, ", ", q$review_year, sep = "")

q$outcome_category <- as.factor(q$outcome_category)

>>>>>>> 85619bf515f5f6fc764efe43847433401e8e53f7
q$recoded_exposure <- gsub("_", " ", q$recoded_exposure)
q$recoded_exposure <- str_to_title(q$recoded_exposure)
q$recoded_exposure <- fix_acronyms(q$recoded_exposure)
q$recoded_exposure <- gsub("\nDevice: Computer", "", q$recoded_exposure)
q$recoded_exposure <- gsub("Content: ", "", q$recoded_exposure)
<<<<<<< HEAD
q$recoded_exposure <- gsub("N/A", "Screen Time Exposure", q$recoded_exposure)
=======

>>>>>>> 85619bf515f5f6fc764efe43847433401e8e53f7
q$recoded_exposure <- as.factor(q$recoded_exposure)

q$outcome <- gsub("_", " ", q$outcome)
q$outcome <- gsub("^learning ", "", q$outcome)
q$outcome <- gsub("literacy ", "literacy: ", q$outcome)
q$outcome <- str_to_title(q$outcome)
<<<<<<< HEAD
q$outcome <- gsub("^Health Behaviour ", "", q$outcome)
q$outcome <- gsub("^Education ", "", q$outcome)
q$outcome <- gsub("^Psychology ", "", q$outcome)
q$outcome <- gsub("^Physical Health ", "", q$outcome)
q$outcome <- gsub("^Cardiometabolic Health ", "", q$outcome)
q$outcome <- fix_acronyms(q$outcome)

q$moderator_level <- gsub("_", " ", q$moderator_level)
q$moderator_level <- str_to_title(q$moderator_level)
q$moderator_level <- gsub("^Health Behaviour ", "", q$moderator_level)
q$moderator_level <- gsub("^Physical Activity ", "", q$moderator_level)
q$moderator_level <- fix_acronyms(q$moderator_level)

q$moderator_category <- str_to_title(q$moderator_category)

=======
q$outcome <- fix_acronyms(q$outcome)

fix_acronyms <- function(chr_vector){
  chr_vector <- gsub("Tv", "TV", chr_vector)
  chr_vector <- gsub("Bmi", "BMI", chr_vector)
  chr_vector <- gsub("Of", "of", chr_vector)
}




q$moderator_level <- gsub("_", " ", q$moderator_level)
q$moderator_level <- str_to_title(q$moderator_level)
q$moderator_level <- fix_acronyms(q$moderator_level)

>>>>>>> 85619bf515f5f6fc764efe43847433401e8e53f7
q$effect_label <- paste(q$outcome, " (", q$moderator_level, ")", sep = "")
q$effect_label <- gsub(" \\(Overall\\)", "", q$effect_label)
q$effect_label <- gsub(" \\(Random Effects\\)", "", q$effect_label)
q <- q[!grepl("Fixed Effects", q$effect_label), ]

alternating <- rep(c("white", "light grey"), 100)

<<<<<<< HEAD
#q$moderator_level[q$n==4526]
#### Forest plot for education####

for (i in 1:length(unique(q$outcome_category))) {
  # i <- 2
  edu <- filter(q, as.numeric(outcome_category) == i)
  edu$value_ci_lower_bound_consensus[edu$value_ci_lower_bound_consensus < -.4] <- -.4
=======
unique(q$moderator_level)

q$value_ci_lower_bound_consensus_r[q$value_ci_lower_bound_consensus_r < -.4] <- -.4


#### Forest plot for education####

for (i in 1:length(unique(q$outcome_category))) {
  # i <- 1
  edu <- subset(q, as.numeric(q$outcome_category) == i)
  edu$value_ci_lower_bound_consensus_r[edu$value_ci_lower_bound_consensus_r < -.4] <- -.4
>>>>>>> 85619bf515f5f6fc764efe43847433401e8e53f7
  plot_title <- paste("Effect on ",
    levels(q$outcome_category)[i],
    " Outcomes (r with 95%CI)",
    sep = ""
  )
  # Create a row for labeling the plot
  edu <- rbind(edu, NA)
  last <- nrow(edu)
<<<<<<< HEAD
  #levels(edu$recoded_exposure)
  edu$recoded_exposure <- forcats::fct_expand(edu$recoded_exposure, "Exposure") %>%
    forcats::fct_relevel("Exposure", "Screen Time Exposure")
=======

  edu$recoded_exposure <- forcats::fct_expand(edu$recoded_exposure, "Exposure") %>%
    forcats::fct_relevel("Exposure", "Overall Screentime")
>>>>>>> 85619bf515f5f6fc764efe43847433401e8e53f7
  edu$recoded_exposure[last] <- "Exposure"
  edu$author_year[last] <- "**Study Label**"

  edu$combined_n <- format(edu$combined_n, big.mark = ",")
  edu$combined_n[grepl("NA", edu$combined_n)] <- NA
  edu$combined_n[last] <- "**N**"
  edu$k[last] <- "**K**"
<<<<<<< HEAD
  edu$i2_calculated[sapply(edu$i2_calculated, is.null)] <- NA
=======

>>>>>>> 85619bf515f5f6fc764efe43847433401e8e53f7
  edu$i2_calculated <- as.character(paste(round(as.numeric(edu$i2_calculated), 0), "%", sep = ""))
  edu$i2_calculated[grepl("NA", edu$i2_calculated)] <- NA
  edu$i2_calculated[last] <- "**I^2**"

  edu$effect_label <- factor(edu$effect_label,
    levels = unique(edu$effect_label[order(edu$effect_label)])
  )
  
  edu$outcome[last] <- "**Outcome**"
  edu$moderator_level[last] <- "**Moderator Level**"
<<<<<<< HEAD
  edu$moderator_category[last] <- "**Moderator Category**"
  edu$rci <- with(edu, paste(round(r,2), " [", round(value_ci_lower_bound_consensus, 2), ", ",
                             round(value_ci_upper_bound_consensus, 2), "]", sep = ""))
  edu$rci[last] <- "**<i>r</i> with 95% CI**"
=======
>>>>>>> 85619bf515f5f6fc764efe43847433401e8e53f7
  
  edu$outcome[edu$moderator_level=="Overall"&
                !is.na(edu$moderator_level)] <- paste("**", edu$outcome[edu$moderator_level=="Overall" &
                                                                           !is.na(edu$moderator_level)], "**", sep = "")
<<<<<<< HEAD
  edu$moderator_category[edu$moderator_level=="Overall"&
                           !is.na(edu$moderator_level)] <- paste("**", edu$moderator_category[edu$moderator_level=="Overall" &
                                                                                                !is.na(edu$moderator_level)], "**", sep = "")
=======
>>>>>>> 85619bf515f5f6fc764efe43847433401e8e53f7
  edu$moderator_level[edu$moderator_level=="Overall"&
                !is.na(edu$moderator_level)] <- paste("**", edu$moderator_level[edu$moderator_level=="Overall" &
                                                                          !is.na(edu$moderator_level)], "**", sep = "")
  
<<<<<<< HEAD

  
  edu$effect_label <- forcats::fct_expand(edu$effect_label, "Outcome (With Moderators)")
  edu$effect_label[last] <- "Outcome (With Moderators)"

  edu$effect_label <- factor(edu$effect_label, levels = unique(levels_in_order))
  # edu$effect_label <- forcats::fct_relevel(edu$effect_label,
  #                                          as.character(unique(edu$effect_label[edu$moderator_level=="**Overall**" & !is.na(edu$moderator_level)])))
  edu <- edu[(with(edu, order(recoded_exposure,
                                      outcome,
                                      moderator_category,
                                      moderator_level))), ]
  edu$row_num <- as.character(1:nrow(edu))


  edu$value_ci_lower_bound_consensus[edu$value_ci_lower_bound_consensus < -.4] <- -.4
  edu$value_ci_upper_bound_consensus[edu$value_ci_upper_bound_consensus < -.4] <- -.4
  edu$r[edu$r < -.4] <- -.4
  
  p1 <- ggplot(
    edu,
    aes(
      x = row_num,
      y = r,
      ymin = value_ci_lower_bound_consensus,
      ymax = value_ci_upper_bound_consensus,
=======
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
      y = -.5, label = edu$combined_n,
      vjust = 0.5, hjust = 0.5,
      stat = "identity",
      size = 2.5,
      label.size = NA
    ) +
    geom_richtext(
      y = -.65, label = edu$k,
      vjust = 0.5, hjust = 0.5,
      stat = "identity",
      size = 2.5,
      label.size = NA
    ) +
    geom_richtext(
      y = -.8, label = edu$i2_calculated,
      vjust = 0.5, hjust = 0.5,
      stat = "identity",
      size = 2.5,
      label.size = NA
    ) + 
    geom_richtext(
      y = -1, label = edu$rci,
      vjust = 0.5, hjust = 0.5,
      stat = "identity",
      size = 2.5,
      label.size = NA
    ) +
    geom_richtext(
      y = -1.5,
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
    geom_richtext(
      y = -2.5, label = edu$moderator_category,
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
        hjust = 1,
        vjust = 0,
        face = "bold"
      ),
      axis.text.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      strip.placement = "outside",
      strip.background = element_rect(linetype = "solid"),
      text = element_text(family = "Times")
    )
  p1
  ggsave(paste("Forest plot for ", levels(q$outcome_category)[i], ".pdf", sep = ""),
         plot = p1,
         width = 14,
         height = 14)
}
