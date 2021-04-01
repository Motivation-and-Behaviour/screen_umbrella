library(tidyverse)
library(janitor)
library(ggtext)
library(tidyMB)

# remove existing environment
#rm(list = ls())

raw <- read_sheet() 
simple <- simplify_effects(raw)
d <- convert_data(simple)

plain_language_outcomes <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1P54V0aXlF5neoVIwViK-zAqFijYHLFFtXIBAK_2xkII/edit#gid=464363801",
                                sheet = "Outcomes",
                                na = "-999") %>% janitor::clean_names()

plain_language_exposures <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1P54V0aXlF5neoVIwViK-zAqFijYHLFFtXIBAK_2xkII/edit#gid=464363801",
                                                     sheet = "Exposures",
                                                     na = "-999") %>% janitor::clean_names()  

# Clean the names of the datafile, rename to something more meaningful, 
# remove empty stuff, then cut small studies or  rubbish
q <- clean_names(d) %>%
  dplyr::rename(r = value_consensus,
                outcome_category = outcome_level_1,
                outcome = outcome_level_2,
                recoded_exposure = recoded_content,
                moderator_level = moderator_level_recoded,
                moderator_category = moderator_category_recoded,
                k = k_number_of_studies_for_this_effect_consensus,
                n = combined_n,
                cilb = value_ci_lower_bound_consensus,
                ciub = value_ci_upper_bound_consensus,
                i2 = i2_calculated
                ) %>%
  remove_empty(which = c("rows", "cols")) %>%
  filter(usable_effect_size == TRUE,
         r < .99,
         n >= 1000) 

# Merge in the plain language exposures and outcomes

q <- left_join(q,
               select(plain_language_outcomes,
                      outcome_consensus,
                      plain_language_descriptor_for_level_3)) %>%
     rename(plain_language_outcome = plain_language_descriptor_for_level_3) %>%
     select(-plain_language_exposure)
q <- left_join(q, select(plain_language_exposures,
                      recoded_content_6,
                      plain_language_exposure) %>%
                 rename(recoded_exposure = recoded_content_6))

# Add significance and labels
q$sig <- ((q$cilb * q$ciub) > 0)
q$sig <- q$sig * .7 + .3
q$author_year <- paste(q$first_author, ", ", q$year_of_publication, sep = "")

#if one effect from this review, keep or select "overall"
# group by study_id and exposure and outcome, pick max K
q <- group_by(q,
              covidence_review_id,
              plain_language_outcome,
              plain_language_exposure) %>% slice_max(k,
                                                     with_ties = TRUE) %>%
    select(author_year, covidence_review_id,
           outcome_category,
           plain_language_outcome,
           plain_language_exposure,
           k, n, r, cilb, ciub, i2, sig) %>%
    distinct()
  
# Take overall outcome into a variable and remove that from the sub-variable
q$outcome_lvl_1 <- factor(gsub(":.*", "", q$plain_language_outcome))
q$plain_language_outcome <- factor(gsub(".*: ", "", q$plain_language_outcome))
q$outcome_category <- factor(str_to_title(q$outcome_category))
q$plain_language_exposure <- factor(q$plain_language_exposure)

#convert funny lists to numeric vectors
q$n <- as.numeric(q$n)
q$k <- as.numeric(q$k)
q$i2 <- as.numeric(sapply(q$i2, as.numeric))

# Cleaning data before plotting
q$i2[is.null(q$i2)] <- NA
q$i2 <- as.character(paste(round(as.numeric(q$i2), 0), "%", sep = ""))
q$i2[grepl("NA", q$i2)] <- NA

q$rci <- with(q, paste(format(round(r,2), nsmall = 2),
                       " [", format(round(cilb, 2), nsmall = 2), ", ",
                       format(round(ciub, 2), nsmall = 2), "]", sep = ""))

q$n <- format(q$n, big.mark = ",")
q$n[grepl("NA", q$n)] <- NA
q <- ungroup(q)
write.csv(q, "data_to_plot.csv")
#### Forest plot for education####

for (i in 1:length(unique(q$outcome_category))) {
  # i <- 2
  edu <- filter(q, as.numeric(outcome_category) == i)
  
  plot_title <- paste("Effect on ",
    levels(q$outcome_category)[i],
    " Outcomes (r with 95%CI)",
    sep = ""
  )
  # Create a row for labeling the plot
  edu <- add_row(edu)
  last <- nrow(edu)
  #levels(edu$recoded_exposure)
  edu$plain_language_exposure <- forcats::fct_expand(edu$plain_language_exposure, "**Exposure**") %>%
    forcats::fct_relevel("**Exposure**", "Screen use: General")
  edu$plain_language_exposure[last] <- "**Exposure**"
  
  edu$author_year[last] <- "**Study Label**"

  edu$n[last] <- "**N**"
  edu$k[last] <- "**K**"
  edu$i2[last] <- "**I^2**"

  edu$plain_language_outcome <- forcats::fct_expand(edu$plain_language_outcome, "**Specific Outcome**") %>%
    forcats::fct_relevel("**Specific Outcome**")
  edu$plain_language_outcome[last] <- "**Specific Outcome**"
  
  edu$outcome_lvl_1 <- forcats::fct_expand(edu$outcome_lvl_1, "Outcome") %>%
    forcats::fct_relevel("Outcome")
  edu$outcome_lvl_1[last] <- "Outcome"
  
  
  edu$rci[last] <- "**<i>r</i> with 95% CI**"
  
  edu <- arrange(edu,
                 outcome_lvl_1, 
                 plain_language_outcome,
                 plain_language_exposure,
                 k)
  edu$row_num <- as.factor(1:nrow(edu))

  edu$shape <- 18

  edu$cilb[edu$cilb < -.4] <- -.4
  edu$ciub[edu$ciub < -.4] <- -.4
  edu$r[edu$r < -.4] <- -.39
  edu$shape[edu$r == -.39] <- 60
  
  edu$cilb[edu$cilb > .4] <- .4
  edu$ciub[edu$ciub > .4] <- .4
  edu$r[edu$r > .4] <- .39
  edu$shape[edu$r == .39] <- 62
  
  p1 <- ggplot(
    edu,
    aes(
      x = row_num,
      y = r,
      ymin = cilb,
      ymax = ciub,
      label = author_year,
      family = "Times",
      shape = shape,
    )
  ) +
    geom_pointrange(
      alpha = edu$sig
    ) +
    geom_richtext(
      y = -.5, label = edu$n,
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
      y = -.8, label = edu$i2,
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
      y = -2.1, label = edu$plain_language_exposure,
      vjust = 0.5, hjust = 0,
      stat = "identity",
      size = 2.5,
      label.size = NA
    ) +
    geom_richtext(
      y = -3, label = edu$plain_language_outcome,
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
    coord_flip(ylim = c(-3, .4)) +
    scale_shape_identity() +
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
      axis.text.y = element_blank(
        #colour = "white"
      ),
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
