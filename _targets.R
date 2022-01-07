library(targets)
library(tarchetypes)
library(here)

options(tidyverse.quiet = TRUE, clustermq.scheduler = "multiprocess")

packages <- c("base","broom", "bib2df", "DiagrammeR", "distill", "DT", "esc", "emojifont", 
              "ggh4x", "googledrive", "ggplot2", "ggtext", "gt", "janitor", 
              "knitr", "kableExtra", "magrittr", "metafor", "papaja", "targets", 
              "tarchetypes", "tidyMB", "tidyverse", "scales", "xfun")

tar_option_set(packages = packages, debug = "yml_base", workspace_on_error = TRUE)

googledrive::drive_auth()
googlesheets4::gs4_auth()
source(here("R", "utils.R"))
source(here("R", "data.R"))
source(here("R", "tables.R"))
source(here("R", "plots.R"))
source(here("R", "supplementary.R"))
source(here("R", "reports.R"))

list(
  # Fetch and analyse (data.R)
  fetch_data,
  clean_and_convert,
  reanalyse,
  # Make tables (tables.R)
  make_tables,
  # Make plots (plots.R)
  make_prisma_figure,
  make_forest_plots,
  # Make sundry supplementary material (supplementary.R)
  make_supps,
  # Create reports (reports.R)
  generate_bibliography,
  create_reports
)
