library(targets)

source(here::here("R","functions.R"))
options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("tidyverse", "janitor","here", "glue", "fuzzyjoin",
                            "googlesheets4", "refinr", "tidyr")
)
tar_pipeline(
  tar_target(
    read_data,
    read_sheet()
  ),
  tar_target(
    simple_effects,
    simplify_effects(read_data)
  ),
  tar_target(
    converted_data,
    convert_data(simple_effects)
  ),
  tar_target(write_out,
    googlesheets4::write_sheet(converted_data, ss= "1yEdtTxP0RIc3Td7lswTf7ySJJ_Jr6Qu9LWaIYV8_00k")
  ),
  tarchetypes::tar_knit(
    report,
    "report.Rmd"
  )
)