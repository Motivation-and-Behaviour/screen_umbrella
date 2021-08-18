#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(tidyverse)
library(janitor)
library(ggtext)
library(tidyMB)
library(shiny)
library(stringr)


data_file <- "clean_converted_data.Rdata"
d <- load(data_file)



clean_before_plotting <- function(d) {
  q <- clean_names(d) %>%
    dplyr::rename(
      r = value_consensus,
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
    mutate(n = as.numeric(sapply(n, as.numeric))) %>%
    filter(
      usable_effect_size == TRUE,
      r < .99,
      n >= 1000,
      moderator_level != "fixed effects"
    )

  q$i2 <- as.numeric(sapply(q$i2, as.numeric))
  q$effect_size_id_1 <- as.character(sapply(q$effect_size_id_1, as.character))
  # Merge in the plain language exposures and outcomes

  # dim(q)
  q <- filter(q, is.na(moderator_type))


  # Add significance and labels
  q$sig <- ((q$cilb * q$ciub) > 0)
  q$sig <- q$sig * .7 + .3
  q$author_year <- paste(q$first_author, ", ", q$year_of_publication, sep = "")

  # bold the rows that are classified as 'risks'
  q$benefit_or_risk[is.na(q$benefit_or_risk)] <- "Risk"
  q$risk <- ifelse(q$benefit_or_risk == "Risk", "bold", "plain")
  names(q)
  # if one effect from this review, keep or select "overall"
  # group by study_id and exposure and outcome, pick max K
  q <- rename(q,
    plain_language_outcome = outcome_plain_language_descriptor
  ) %>%
    group_by(
      covidence_review_id,
      plain_language_outcome,
      plain_language_exposure
    ) %>%
    slice_max(k,
      with_ties = TRUE
    ) %>%
    select(
      author_year, covidence_review_id,
      outcome_category, effect_size_id_1,
      plain_language_outcome,
      plain_language_exposure,
      risk,
      k, n, r, cilb, ciub, i2, sig
    ) %>%
    distinct()

  # Tidy up a few really long labels
  q$plain_language_outcome <- gsub("Executive Functioning ", "Executive Functioning<br>", q$plain_language_outcome)
  q$plain_language_outcome <- gsub("reasoning and ", "reasoning and<br>", q$plain_language_outcome)
  # q$plain_language_exposure <- gsub(" \\(", "<br>(", q$plain_language_exposure)



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
  q$plain_language_outcome <- factor(gsub(".*: ", "", q$plain_language_outcome))
  q$outcome_category <- factor(str_to_title(q$outcome_category))
  q$plain_language_exposure <- factor(q$plain_language_exposure)

  # Cleaning data before plotting
  q$i2[is.null(q$i2)] <- NA
  q$i2 <- as.character(paste(round(as.numeric(q$i2), 0), "%", sep = ""))
  q$i2[grepl("NA", q$i2)] <- NA

  q$rci <- with(q, paste(format(round(r, 2), nsmall = 2),
    " [", format(round(cilb, 2), nsmall = 2), ", ",
    format(round(ciub, 2), nsmall = 2), "]",
    sep = ""
  ))

  q$n <- format(q$n, big.mark = ",")
  q$n[grepl("NA", q$n)] <- "—"
  q <- ungroup(q)

  # Rard removing some pesky effects
  q <- filter(
    q, effect_size_id_1 != "34306_020",
    effect_size_id_1 != "34306_023",
    effect_size_id_1 != "34306_015",
    effect_size_id_1 != "34306_016"
  )
  q
}

#### Forest plot function
q <- clean_before_plotting(d)


outcomes <- unique(q$outcome_category)

function_for_plotting <- function(q, outcome) {
  part_q <- filter(q, outcome_category == outcome)

  plot_title <- paste("Effect on ",
    outcome,
    " Outcomes (r with 95%CI)",
    sep = ""
  )
  # Create a row for labeling the plot
  part_q <- add_row(part_q)
  last <- nrow(part_q)
  # levels(part_q$recoded_exposure)
  part_q$plain_language_exposure <- fct_expand(part_q$plain_language_exposure, "**Exposure**") %>%
    fct_relevel("**Exposure**", "Screen use: General")
  part_q$plain_language_exposure[last] <- "**Exposure**"

  part_q$author_year[last] <- "**Lead Author, Date**"

  part_q$n[last] <- "**N**"
  part_q$k[last] <- "**K**"
  part_q$i2[last] <- "**I^2**"
  part_q$risk[last] <- "plain"

  part_q$plain_language_outcome <- fct_expand(part_q$plain_language_outcome, "**Specific Outcome (risks** & benefits**)**") %>%
    fct_relevel("**Specific Outcome (risks** & benefits**)**")
  part_q$plain_language_outcome[last] <- "**Specific Outcome (risks** & benefits**)**"

  part_q$outcome_lvl_1 <- fct_expand(part_q$outcome_lvl_1, "Outcome") %>%
    fct_relevel("Outcome")
  part_q$outcome_lvl_1[last] <- "Outcome"


  part_q$rci[last] <- "**<i>r</i> with 95% CI**"

  part_q <- arrange(
    part_q,
    outcome_lvl_1,
    plain_language_outcome,
    plain_language_exposure,
    k
  )
  part_q$row_num <- as.factor(1:nrow(part_q))

  part_q$shape <- 18

  part_q$cilb[part_q$cilb < -.4] <- -.4
  part_q$ciub[part_q$ciub < -.4] <- -.4
  part_q$r[part_q$r < -.4] <- -.39
  part_q$shape[part_q$r == -.39] <- 60

  part_q$cilb[part_q$cilb > .4] <- .4
  part_q$ciub[part_q$ciub > .4] <- .4
  part_q$r[part_q$r > .4] <- .39
  part_q$shape[part_q$r == .39] <- 62

  p1 <- ggplot(
    part_q,
    aes(
      x = row_num,
      y = r,
      ymin = cilb,
      ymax = ciub,
      label = author_year,
      family = "Times",
      shape = shape,
      fontface = risk,
    )
  ) +
    geom_pointrange(
      # alpha = part_q$sig
    ) +
    geom_richtext(
      y = -.5, label = part_q$n,
      vjust = 0.5, hjust = 0.5,
      stat = "identity",
      size = 2.5,
      label.size = NA
    ) +
    geom_richtext(
      y = -.65, label = part_q$k,
      vjust = 0.5, hjust = 0.5,
      stat = "identity",
      size = 2.5,
      label.size = NA
    ) +
    geom_richtext(
      y = -.8, label = part_q$i2,
      vjust = 0.5, hjust = 0.5,
      stat = "identity",
      size = 2.5,
      label.size = NA
    ) +
    geom_richtext(
      y = -1.1, label = part_q$rci,
      vjust = 0.5, hjust = 0.5,
      stat = "identity",
      size = 2.5,
      label.size = NA
    ) +
    geom_richtext(
      y = -1.8,
      vjust = 0.5, hjust = 0,
      stat = "identity",
      size = 2.5,
      label.size = NA
    ) +
    geom_richtext(
      y = -3, label = part_q$plain_language_exposure,
      vjust = 0.5, hjust = 0,
      stat = "identity",
      size = 2.5,
      label.size = NA
    ) +
    # geom_richtext(
    #   y = 0.41, label = part_q$effect_size_id_1,
    #   vjust = 0.5, hjust = 0,
    #   stat = "identity",
    #   size = 2.5,
    #   label.size = NA
    # ) +
    geom_richtext(
      y = -3.67, label = part_q$plain_language_outcome,
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
    coord_flip(ylim = c(-3.5, .4)) +
    scale_shape_identity() +
    scale_x_discrete(limits = rev) +
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
        # colour = "white"
      ),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      strip.placement = "outside",
      strip.background = element_rect(linetype = "solid"),
      text = element_text(family = "Times")
    )
  p1
}


# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Effects of Sceen Time"),
  setBackgroundColor("#F0F0F0"),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("outcome",
        label = "...?",
        choices = c("Psychology", "Health Behaviour", "Physical Health", "Education")
      ),
      helpText(
        "For dashboard comments or requests",
        a(" email Mike",
          href = "mailto:michael.noetel@acu.edu.au"
        ),
        a(" or log it here",
          href = "https://github.com/Motivation-and-Behaviour/screen_umbrella/issues", target = "_blank"
        )
      ),
      width = 3
    ),

    # Show the requested plot
    mainPanel(
      plotOutput("distPlot"),
      width = 9
    ),
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$distPlot <- renderPlot(
    {
      outcome <- input$outcome
      # outcome <- "Education"
      function_for_plotting(q, outcome)
    },
    height = 800
  )
}

# Run the application
shinyApp(ui = ui, server = server)
