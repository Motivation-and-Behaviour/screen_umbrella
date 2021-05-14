library(targets)
library(tarchetypes)
source(here::here("R", "functions.R"))
options(tidyverse.quiet = TRUE)
tar_option_set(packages = c(
  "tidyverse", "janitor", "here", "glue", "fuzzyjoin",
  "googlesheets4", "refinr", "tidyr"
))

list(
  # Data sources
  tar_target(
    modified_date,
    get_mod_date(),
    # Force run if outdated or doesn't exist
    cue = tar_cue_force(condition = ifelse(tar_exist_objects("modified_date"),
                        get_mod_date() != tar_read(modified_date),
                        TRUE))
  ),
  tar_target(
    effects_raw,
    read_sheet(modified_date, "EffectSizesValidation")
  ),
  tar_target(
    reviews_raw,
    read_sheet(modified_date, "ReviewLevelValidation")
  ),
  tar_target(
    rob_raw,
    read_sheet(modified_date, "QualityAssessment")
  ),
  # Data cleaning
  tar_target(
    effects_clean,
    process_effects(effects_raw),
  )
  # tar_target(
  #   simple_effects,
  #   simplify_effects(read_data)
  # ),
  # tar_target(
  #   converted_data,
  #   convert_data(simple_effects)
  # ),
  # tarchetypes::tar_knit(
  #   report,
  #   "report.Rmd"
  # )
)