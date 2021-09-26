# ------------------- SETTINGS -----------------------
authors_sheet <- "1V-j8oQXI3Y8etUnaRETvUvq9Hr8eAbAoBc_0XoLgb48"

# ------------------- TARGETS ------------------------

## Bibliographies ----
generate_bibliography <-
  list(
    tar_target(
      packages_bib,
      create_packages_bib(packages,
                          here::here("reports", "packages.bib")),
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
        here::here("reports", "combined.bib")
      ),
      format = "file"
    )
  )

## Metadata ----

## Create reports ----

# ------------------- FUNCTIONS ----------------------

## Bibliography ----
# Create packages bibliography
create_packages_bib <- function(packages, bibpath){
  knitr::write_bib(packages, bibpath)
  return(bibpath)
}

# Combine bibliography files
combine_bibs <- function(packages_bib, references_bib, outpath){
  fullbib <- lapply(c(packages_bib, references_bib), readLines)
  write(unlist(fullbib), file=outpath)
  return(outpath)
}