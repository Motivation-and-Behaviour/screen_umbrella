#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param packages
#' @param outpath
#' @return
#' @author Taren Sanders
#' @export
make_packages_bib <- function(packages, outpath) {
  knitr::write_bib(packages, outpath)
  return(outpath)
}

#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param packages_bib
#' @param references_bib
#' @param reviews_raw
#' @param outpath
#' @return
#' @author Taren Sanders
#' @export
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
