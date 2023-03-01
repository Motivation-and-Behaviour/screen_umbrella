read_sheet <- function(file) {
  readr::read_csv(
    file = file, na = c("-999", "", "#N/A"), show_col_types = FALSE
  ) %>%
    janitor::clean_names()
}

load_packages <- function() {
  invisible(source("./packages.R"))
}

upload_files <- function(target_file, type = "manuscript") {
  gdrive_path <- switch(type,
    "manuscript" =
      "https://drive.google.com/drive/folders/1WQiAUmDanOL2GPPBoYxUoNkHmZUjpbPj", # nolint
    "supp" =
      "https://drive.google.com/drive/folders/1gW5k2CIRJf1w2FP0dbUHCXwpAxtV1Gxp" # nolint
  )

  drive_val <- drive_put(
    target_file[1],
    path = as_id(gdrive_path)
  )

  return(drive_val)
}
