# Settings for each of the outcome plots

edu_certain <- list(
  filename = "Forest plot for Education.pdf",
  title = "Associations Between Exposures and Education Outcomes",
  categories = "Education",
  certain = TRUE,
  dims = c(11, 7.5),
  moderators = FALSE,
  caption = FALSE,
  pos =
    list(
      lims = c(-4.0, 0.5),
      breaks = c(-0.4, -.2, 0, .2, 0.4),
      esig = NULL,
      eggers = NULL,
      indiv_data = NULL,
      n = -0.6,
      k = -0.85,
      i2 = -1.05,
      rci = -1.45,
      author_year = -2.3,
      mod = -2.7,
      expo = -3.65,
      outcome = -4.25,
      tag = NA
    )
)

edu_uncertain <- list(
  filename = "Supplementary File 5 - Education Outcomes.pdf",
  title = "Associations Between Exposures and Education Outcomes",
  categories = "Education",
  certain = FALSE,
  dims = c(16, 18),
  moderators = FALSE,
  caption = TRUE,
  pos =
    list(
      lims = c(-4.5, 0.8),
      breaks = c(-0.8, -0.6, -0.4, -.2, 0, .2, 0.4, 0.6, 0.8, 1),
      esig = -0.9,
      eggers = -1.1,
      indiv_data = -1.3,
      n = -1.5,
      k = -1.7,
      i2 = -1.85,
      rci = -2.15,
      author_year = -2.8,
      mod = -3.15,
      expo = -4.05,
      outcome = -4.75,
      tag = c(0.2, 0.01)
    )
)

nonedu_certain <- list(
  filename = "Forest plot for Health-related Outcomes.pdf",
  title = "Associations Between Exposures and Health-related Outcomes",
  categories = c("Psychology", "Health Behaviour", "Physical Health"),
  certain = TRUE,
  dims = c(11, 8),
  moderators = FALSE,
  caption = FALSE,
  pos = list(
    lims = c(-4.75, 0.2),
    breaks = c(-0.6, -0.4, -.2, 0, .2, 0.4),
    esig = NULL,
    eggers = NULL,
    indiv_data = NULL,
    n = -0.8,
    k = -1.05,
    i2 = -1.25,
    rci = -1.6,
    author_year = -2.6,
    mod = -3.1,
    expo = -4.15,
    outcome = -5.0,
    tag = NA
  )
)

nonedu_uncertain <-
  list(
    filename = "Supplementary File 6 - Health-related Outcomes.pdf",
    title = "Associations Between Exposures and Health-related Outcomes",
    categories = c("Psychology", "Health Behaviour", "Physical Health"),
    certain = FALSE,
    dims = c(16, 49),
    moderators = FALSE,
    caption = TRUE,
    pos = list(
      lims = c(-5.6, 0.7),
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
