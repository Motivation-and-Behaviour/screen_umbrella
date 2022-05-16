# ------------------- SETTINGS -----------------------

# ------------------- TARGETS ------------------------




# ------------------- FUNCTIONS ----------------------

## Checking Dates ----
get_mod_date <- function(id) {
  x <- googledrive::drive_get(id = googledrive::as_id(id))
  return(x$drive_resource[[1]]$modifiedTime)
}


## Reading Data ----
read_sheet <- function(file, sheet, mod_date) {
  d <- googlesheets4::read_sheet(
    ss = file,
    sheet = sheet,
    na = c("-999", "", "#N/A")
  ) %>%
    janitor::clean_names()
  return(d)
}

## For debugging
load_all_packages <- function(packages) {
  lapply(packages, library, character.only = TRUE)
}