read_sheet <- function(file) {
  readr::read_csv(
    file = file, na = c("-999", "", "#N/A"), show_col_types = FALSE
  ) %>%
    janitor::clean_names()
}

load_packages <- function() {
  invisible(source("./packages.R"))
}

upload_file <- function(target_file, type = "main") {
  gdrive_base_path <- "https://drive.google.com/drive/folders/"
  gdrive_path <- switch(type,
    "main" = "1rcKaSfnMOq_CRAN_-YBRjx6G1oI63iS6",
    "supp" = "1KA4r2qfyAJX5r07NwrlkLMsSpmbuG-7K",
    "figs" = "1PPGIwqk-Vi13RGdn9iU1k4BSlBGcu_hE",
    "tabs" = "1_2IK4QV6Uy5_CaCJgjStSQRcd_KLBFui"
  )

  for (i in seq_along(target_file)) {
    drive_val <- googledrive::drive_put(
      target_file[i],
      path = googledrive::as_id(paste(gdrive_base_path, gdrive_path, sep = ""))
    )
  }
}

upload_all_files <- function(files = c(
                               "manuscript", "revision_letter", "prisma",
                               "main_plots", "supp_files", "tables"
                             )) {
  file_details <- list(
    "manuscript" = list(
      path = c("reports/manuscript.pdf", "reports/manuscript.docx"),
      type = "main"
    ),
    "revision_letter" = list(
      path = c("reports/revision_letter.pdf", "reports/revision_letter.docx"),
      type = "main"
    ),
    "prisma" = list(
      path = "figures/Figure 1 - PRISMA Diagram.pdf",
      type = "figs"
    ),
    "main_plots" = list(
      path = c(
        "figures/Figure 2 - Forest plot for Education.pdf",
        "figures/Figure 3 - Forest plot for Health-related Outcomes.pdf"
      ),
      type = "figs"
    ),
    "supp_files" = list(
      path = c( # nolint start
        "supplementary_files/Supplementary File 1 - Search Strategy.pdf",
        "supplementary_files/Supplementary File 2 - Complete Effects Data.csv",
        "supplementary_files/Supplementary File 3 - Effect Size Codebook.pdf",
        "supplementary_files/Supplementary File 4 - List of reviews without meta-analyses.csv",
        "supplementary_files/Supplementary File 5 - List of Exposures.csv",
        "supplementary_files/Supplementary File 6 - Education Outcomes.pdf",
        "supplementary_files/Supplementary File 7 - Health-related Outcomes.pdf",
        "supplementary_files/Supplementary File 8 - Included Studies.pdf"
      ), # nolint end
      type = "supp"
    ),
    "tables" = list(
      path = "tables/Table 1 - Review characteristics.pdf",
      type = "tabs"
    )
  )

  file_details <- file_details[names(file_details) %in% files]

  for (file in file_details) {
    upload_file(file$path, file$type)
  }
}
