read_sheet <- function(file) {
  readr::read_csv(file = file, na = c("-999", "", "#N/A")) %>%
    janitor::clean_names()
}

load_packages <- function() {
  invisible(source("./packages.R"))
}
