# ------------------- SETTINGS -----------------------

# ------------------- TARGETS ------------------------




# ------------------- FUNCTIONS ----------------------

## Reading Data ----
read_sheet <- function(file) {
  readr::read_csv(file = file, na = c("-999", "", "#N/A")) %>%
    janitor::clean_names()
}

## For debugging
load_all_packages <- function(packages) {
  lapply(packages, library, character.only = TRUE)
}
