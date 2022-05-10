# ------------------- SETTINGS -----------------------
authors_sheet <- "1V-j8oQXI3Y8etUnaRETvUvq9Hr8eAbAoBc_0XoLgb48"
article_title <- "Benefits and risks associated with children’s and adolescents’ interactions with electronic screens An umbrella review" # nolint

# ------------------- TARGETS ------------------------

## Bibliographies ----
generate_bibliography <-
  list(
    tar_target(
      packages_bib,
      create_packages_bib(
        packages,
        here::here("reports", "packages.bib")
      ),
      format = "file"
    ),
    tar_target(
      references_bib,
      here::here("reports", "references.bib"),
      format = "file"
    ),
    tar_target(
      combined_bib,
      combine_bibs(
        packages_bib,
        references_bib,
        reviews_raw,
        here::here("reports", "combined.bib")
      ),
      format = "file"
    )
  )

## Create reports ----
create_reports <- list(
  tar_render(
    manuscript,
    path = here::here("reports", "manuscript.Rmd"),
    params = list(
      nocite_list =
        paste0(
          "@",
          paste(reviews_raw$bibtex_key, collapse = ", @")
        )
    )
  ),
  tar_target(
    uploaded_manuscripts,
    upload_manuscript(manuscript)
  )
)

# ------------------- FUNCTIONS ----------------------

## Bibliography ----
# Create packages bibliography
create_packages_bib <- function(packages, bibpath) {
  knitr::write_bib(packages, bibpath)
  return(bibpath)
}

# Combine bibliography files
combine_bibs <- function(packages_bib, references_bib, reviews_raw, outpath) {
  # Fix UTF-8 encoding
  refs <- readLines(references_bib, encoding = "UTF-8")
  refs <- str_replace_all(refs, "[^[:graph:]]", " ")
  refs <- iconv(refs, from = "UTF-8", to = "ASCII//TRANSLIT")
  refs_tmp <- tempfile(fileext = ".bib")
  writeLines(refs, con = refs_tmp)

  paths <- c(packages_bib, refs_tmp)
  bibs <- lapply(paths, bib2df)
  full_bib <- bind_rows(bibs)

  full_bib <-
    full_bib %>%
    mutate(ANNOTE = if_else(BIBTEXKEY %in% reviews_raw$bibtex_key,
      "*",
      ""
    ))

  df2bib(full_bib, file = outpath)

  rm(refs_tmp)
  return(outpath)
}

# Upload to GDrive
upload_manuscript <- function(word_report) {
  drive_val <- drive_put(
    word_report[1],
    path = as_id("https://drive.google.com/drive/folders/1WQiAUmDanOL2GPPBoYxUoNkHmZUjpbPj")
  )

  return(drive_val)
}