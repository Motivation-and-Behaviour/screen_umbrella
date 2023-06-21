# Settings for each of the outcome plots

edu_certain <- list(
  filename = "figures/Figure 2 - Forest plot for Education.pdf",
  title = "Associations Between Exposures and Education Outcomes",
  categories = "Education",
  certain = TRUE,
  dims = c(185, 210),
  moderators = FALSE,
  caption = FALSE,
  pos =
    list(
      lims = c(-5, 0.5), breaks = c(-0.2, 0, 0.2, 0.4), esig = NULL,
      eggers = NULL, indiv_data = NULL, n = -0.5, k = -0.75, i2 = -0.95,
      rci = -1.35, author_year = -2.2, design = -2.8, pop = -3.15,
      mod = -3.5, expo = -4.71, outcome = -5.38, tag = NA
    )
)

edu_uncertain <- list(
  filename =
    "supplementary_files/Supplementary File 4 - Education Outcomes.pdf",
  title = "Associations Between Exposures and Education Outcomes",
  categories = "Education",
  certain = FALSE,
  dims = c(416.50, 453.25),
  moderators = FALSE,
  caption = TRUE,
  pos =
    list(
      lims = c(-5.3, 0.7),
      breaks = c(-0.8, -0.6, -0.4, -.2, 0, .2, 0.4, 0.6, 0.8, 1),
      esig = -0.9,
      eggers = -1.1,
      indiv_data = -1.3,
      n = -1.5,
      k = -1.7,
      i2 = -1.85,
      rci = -2.15,
      author_year = -2.9,
      design = -3.35,
      pop = -3.655,
      mod = -4.0,
      expo = -4.9,
      outcome = -5.6,
      tag = c(0.15, 0.00)
    )
)

nonedu_certain <- list(
  filename = "figures/Figure 3 - Forest plot for Health-related Outcomes.pdf",
  title = "Associations Between Exposures and Health-related Outcomes",
  categories = c("Psychology", "Health Behaviour", "Physical Health"),
  certain = TRUE,
  dims = c(185, 140),
  moderators = FALSE,
  caption = FALSE,
  pos = list(
    lims = c(-5.7, 0.2),
    breaks = c(-0.6, -0.4, -.2, 0, .2, 0.4),
    esig = NULL,
    eggers = NULL,
    indiv_data = NULL,
    n = -0.75,
    k = -0.95,
    i2 = -1.1,
    rci = -1.45,
    author_year = -2.55,
    design = -3.2,
    pop = -3.55,
    mod = -4.0,
    expo = -5.05,
    outcome = -6,
    tag = NA
  )
)

nonedu_uncertain <-
  list(
    filename =
      "supplementary_files/Supplementary File 5 - Health-related Outcomes.pdf",
    title = "Associations Between Exposures and Health-related Outcomes",
    categories = c("Psychology", "Health Behaviour", "Physical Health"),
    certain = FALSE,
    dims = c(416.50, 1212.75),
    moderators = FALSE,
    caption = TRUE,
    pos = list(
      lims = c(-6.45, 0.7),
      breaks = c(-1, -0.8, -0.6, -0.4, -.2, 0, .2, 0.4, 0.6, 0.8),
      esig = -1.15,
      eggers = -1.35,
      indiv_data = -1.55,
      n = -1.75,
      k = -1.95,
      i2 = -2.1,
      rci = -2.4,
      author_year = -3.25,
      design = -3.8,
      pop = -4.35,
      mod = -4.75,
      expo = -6.0,
      outcome = -6.8,
      tag = c(0.2, 0)
    )
  )
