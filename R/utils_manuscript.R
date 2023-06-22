report_effect <- function(
    combined_effects, effect_id, style = "brackets", first = FALSE) {
  stopifnot(
    style %in% c("brackets", "in-text", "none", "short-brackets", "short-none")
  )
  row <- combined_effects[combined_effects$effect_size_id == effect_id, ]

  r <- format(round(row$r, 2), nsmall = 2)
  ci_l <- format(round(row$cilb95, 2), nsmall = 2)
  ci_u <- format(round(row$ciub95, 2), nsmall = 2)
  k <- as.character(row$k)
  n <- scales::label_comma(accuracy = 1)(row$n)
  ci_text <- if (first) "95% confidence interval [CI]" else "95% CI"

  if (style == "brackets") {
    outstring <-
      glue::glue(
        "(*r* = {r}, {ci_text} {ci_l} to {ci_u},",
        " *k* = {k}, *N* = {n})"
      )
  }

  if (style == "in-text") {
    outstring <-
      glue::glue(
        "*r* = {r} ({ci_text} {ci_l} to {ci_u},",
        " *k* = {k}, *N* = {n})"
      )
  }

  if (style == "none") {
    outstring <-
      glue::glue(
        "*r* = {r}, {ci_text} {ci_l} to {ci_u},",
        " *k* = {k}, *N* = {n}"
      )
  }

  if (style == "short-brackets") {
    outstring <-
      glue::glue(
        "(*r* = {r}, 95% confidence interval {ci_l} to {ci_u})"
      )
  }

  if (style == "short-none") {
    outstring <-
      glue::glue(
        "*r* = {r}, 95% confidence interval {ci_l} to {ci_u}"
      )
  }

  return(outstring)
}
