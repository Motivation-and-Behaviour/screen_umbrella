# Settings for each of the outcome plots

edu_certain <- list(
  filename = "Forest plot for Education.pdf",
  title = "Associations Between Exposures and Education Outcomes",
  categories = "Education",
  certain = TRUE,
  dims = c(10, 6),
  moderators = FALSE,
  caption = FALSE,
  pos =
    list(
      lims = c(-4.3, 0.5),
      breaks = c(-0.4, -.2, 0, .2, 0.4),
      esig = NULL,
      eggers = NULL,
      indiv_data = NULL,
      n = -0.6,
      k = -0.85,
      i2 = -1.05,
      rci = -1.45,
      author_year = -2.45,
      mod = -2.8,
      expo = -3.9,
      outcome = -4.5,
      tag = NA
    )
)

edu_uncertain <- list(
  filename = "Supplementary File 5 - Education Outcomes.pdf",
  title = "Associations Between Exposures and Education Outcomes",
  categories = "Education",
  certain = FALSE,
  dims = c(14, 12),
  moderators = FALSE,
  caption = TRUE,
  pos =
    list(
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
      tag = c(0.2, 0.01)
    )
)

nonedu_certain <- list(
  filename = "Forest plot for Health-related Outcomes.pdf",
  title = "Associations Between Exposures and Health-related Outcomes",
  categories = c("Psychology", "Health Behaviour", "Physical Health"),
  certain = TRUE,
  dims = c(10, 6),
  moderators = FALSE,
  caption = FALSE,
  pos = list(
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
  )
)

nonedu_uncertain <-
  list(
    filename = "Supplementary File 6 - Health-related Outcomes.pdf",
    title = "Associations Between Exposures and Health-related Outcomes",
    categories = c("Psychology", "Health Behaviour", "Physical Health"),
    certain = FALSE,
    dims = c(16, 32),
    moderators = FALSE,
    caption = TRUE,
    pos = list(
      lims = c(-5.6, 0.9),
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
      outcome = -5.9,
      tag = c(0.2, 0)
    )
  )
