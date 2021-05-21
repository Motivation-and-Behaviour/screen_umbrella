library(targets)
library(tarchetypes)
library(here)

source(here("R", "functions.R"))
source(here("R", "reviews_table.R"))
source(here("R","PRISMA.R"))
source(here("R", "forest_plot_code_overview_screentime.R"))

options(tidyverse.quiet = TRUE, clustermq.scheduler="multiprocess")

tar_option_set(packages = c(
  "DT",
  "ggplot2",
  "ggtext",
  "janitor",
  "kableExtra",
  "tidyMB",
  "tidyverse"
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
  tar_target(
    studies_raw,
    read_sheet(modified_date, "StudyLevel")
  ),
  tar_target(
    prisma_data,
    here("covidence_prisma.txt"),
    format = "file"
  ),
  # Data cleaning
  tar_target(
    effects_clean,
    process_effects(effects_raw),
  ),
  # Data Vis
  tar_target(
    reviews_table,
    make_tables(rob_raw, effects_clean, reviews_raw)
  ),
  tar_target(
    prisma,
    make_prisma(prisma_data, effects_clean)
  ),
  tar_target(
    export_prisma,
    save_prisma(prisma),
    format = "file",
  ),
  tar_target(
    plots,
    make_plots(effects_clean),
  ),
  tar_target(
    export_plots,
    save_plots(plots),
    format = "file"
  ),
  tarchetypes::tar_render(
    report,
    "index.Rmd",
    
  )
)