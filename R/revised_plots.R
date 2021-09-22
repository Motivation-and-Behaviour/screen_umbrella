
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

# Modify the dataset
combined_effects <- combined_effects %>%
  filter(use_effect) %>%
  mutate(
    # Take overall outcome into a variable and remove that from the sub-variable
    outcome_lvl_1 = factor(gsub(":.*", "", plain_language_outcome)),
    plain_language_outcome = gsub(".*: ", "", plain_language_outcome),
    outcome_category = factor(str_to_title(outcome_category)),
    plain_language_exposure = str_replace(plain_language_exposure,
                                          "^Intervention:",
                                          "Screen-based intervention:"),
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
                sep = ""),
    indiv_data = if_else(source=="reanalysis", fontawesome("fa-check"), fontawesome("fa-times")),
    font_fam = "fontawesome-webfont"
  ) %>%
  arrange(outcome_lvl_1,
          plain_language_outcome,
          plain_language_exposure) %>%
  add_row(
    outcome_lvl_1 = "Outcome",
    plain_language_outcome = "**Specific Outcome**",
    plain_language_exposure = "**Exposure**", 
    n = "**N**",
    k = "**K**",
    i2 = "**I^2**",
    rci = "**<i>r</i> with 95% CI**",
    author_year = "**Lead Author, Date**",
    row_num="NA",
    indiv_data = "**Individual Data**",
    font_fam = "Times" # Fix this
    ) %>% 
  mutate(
    outcome_lvl_1 = fct_expand(outcome_lvl_1, "Outcome") %>%
      fct_relevel("Outcome"),
    plain_language_outcome = fct_expand(plain_language_outcome,
                                        "**Specific Outcome**") %>%
      fct_relevel("**Specific Outcome**"),
    plain_language_exposure = fct_expand(plain_language_exposure,
                                         "**Exposure**") %>%
      fct_relevel("**Exposure**")
  )

make_plot <- function(categories, certain, title){

plot_effects <- combined_effects %>% 
  filter(outcome_category %in% c(categories, NA))

if (certain) {
  plot_effects <- plot_effects %>% 
    filter(certainty=="meets criteria" | 
             is.na(certainty))
} else {
  plot_effects <- plot_effects %>% 
    filter(certainty=="unclear"| 
             is.na(certainty))
}

base_plot <- ggplot(plot_effects, 
       aes(x = row_num,
           y = r,
           label=plain_language_exposure)) +
  geom_linerange(aes(ymin=cilb999,
                     ymax=ciub999),
                 size = 2, 
                 colour="#bdbdbd")  +
  geom_linerange(aes(ymin=cilb95,
                     ymax=ciub95),
                 size = 2, 
                 colour="#636363")  +
  geom_hline(aes(yintercept=0),
             lty=1, 
             size=0.5) +
  geom_point(size=2, shape=21, 
             fill="#f0f0f0") 

if (certain) {
  updated_plot <- base_plot +
    geom_richtext(aes(label=n),
                  y=-0.6,
                  vjust = 0.5, hjust = 0.5,
                  stat = "identity",
                  size = 2.5,
                  label.size = NA) +
    geom_richtext(aes(label=k),
                  y=-0.85,
                  vjust = 0.5, hjust = 0.5,
                  stat = "identity",
                  size = 2.5,
                  label.size = NA) +
    geom_richtext(aes(label=i2),
                  y=-1.05,
                  vjust = 0.5, hjust = 0.5,
                  stat = "identity",
                  size = 2.5,
                  label.size = NA) +
    geom_richtext(aes(label=rci),
                  y=-1.45,
                  vjust = 0.5, hjust = 0.5,
                  stat = "identity",
                  size = 2.5,
                  label.size = NA) +
    geom_richtext(aes(label=author_year),
                  y=-2.4,
                  vjust = 0.5, hjust = 0,
                  stat = "identity",
                  size = 2.5,
                  label.size = NA) +
    geom_richtext(aes(label=plain_language_exposure),
                  y=-3.5,
                  vjust = 0.5, hjust = 0,
                  stat = "identity",
                  size = 2.5,
                  label.size = NA) +
    geom_richtext(aes(label=plain_language_outcome),
                  y=-4.3,
                  vjust = 0.5, hjust = 0,
                  stat = "identity",
                  size = 2.5,
                  label.size = NA) +
    geom_linerange(x= "NA",
                   ymin=-.41,
                   ymax=.41,
                   size = 20, 
                   colour="white")
  

} else {  
  updated_plot <- 
    base_plot +
    geom_richtext(aes(label=indiv_data,
                      family=font_fam),
                  y=-0.6,
                  vjust = 0.5, hjust = 0.5,
                  stat = "identity",
                  size = 2.5,
                  label.size = NA) 
  
  # +
  #   geom_richtext(aes(label=k),
  #                 y=-0.85,
  #                 vjust = 0.5, hjust = 0.5,
  #                 stat = "identity",
  #                 size = 2.5,
  #                 label.size = NA) +
  #   geom_richtext(aes(label=i2),
  #                 y=-1.05,
  #                 vjust = 0.5, hjust = 0.5,
  #                 stat = "identity",
  #                 size = 2.5,
  #                 label.size = NA) +
  #   geom_richtext(aes(label=rci),
  #                 y=-1.45,
  #                 vjust = 0.5, hjust = 0.5,
  #                 stat = "identity",
  #                 size = 2.5,
  #                 label.size = NA) +
  #   geom_richtext(aes(label=author_year),
  #                 y=-2.4,
  #                 vjust = 0.5, hjust = 0,
  #                 stat = "identity",
  #                 size = 2.5,
  #                 label.size = NA) +
  #   geom_richtext(aes(label=plain_language_exposure),
  #                 y=-3.8,
  #                 vjust = 0.5, hjust = 0,
  #                 stat = "identity",
  #                 size = 2.5,
  #                 label.size = NA) +
  #   geom_richtext(aes(label=plain_language_outcome),
  #                 y=-4.8,
  #                 vjust = 0.5, hjust = 0,
  #                 stat = "identity",
  #                 size = 2.5,
  #                 label.size = NA) +
  #   geom_linerange(x= "NA",
  #                  ymin=-.41,
  #                  ymax=.41,
  #                  size = 20, 
  #                  colour="white")
  }


# updated_plot <- 
  updated_plot + 
  labs(x = NULL,
       y=NULL,
       caption="<b>r</b> with <b style='color:#636363'>95%</b> and <b style='color:#bdbdbd'>99.9%</b> CIs",
       title = title) + 
  facet_grid(
    rows = vars(outcome_lvl_1),
    scales = "free",
    space = "free",
    drop = T,
    switch = "both"
  ) +
  coord_flip(clip = 'off',
             ylim=c(-4.1,0.5)) + 
  scale_y_continuous(breaks = c(-.4, -.2, 0, .2, .4)) +
  scale_x_discrete(limits = rev) +
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
    plot.caption = element_markdown(hjust = 0.95,size = 10),
    plot.caption.position = "plot",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.placement = "outside",
    strip.background = element_rect(linetype = "solid")
  )

}

ggsave(here::here("figure","Education.pdf"),width = 10,height=6)
