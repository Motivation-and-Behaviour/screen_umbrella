# ------------------- SETTINGS -----------------------
authors_sheet <- "1V-j8oQXI3Y8etUnaRETvUvq9Hr8eAbAoBc_0XoLgb48"
article_title <- "Benefits and risks associated with children’s and adolescents’ interactions with electronic screens An umbrella review"

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
        reviews_raw,
        here::here("reports", "combined.bib")
      ),
      format = "file"
    )
  )

## Create reports ----
create_reports <- list(
  tar_render(
    distill_report,
    path = here::here("reports", "manuscript_distill.Rmd"),
    output_file = here::here("index.html"),
    output_yaml = here::here("reports", "distill.yaml"),
    params = list(render_img = TRUE, inc_tabs = TRUE,
                  nocite_list = 
                    paste0("@",
                           paste(reviews_raw$bibtex_key, collapse = ", @")))
  ),
  tar_render(
    word_report,
    path = here::here("reports", "manuscript_papaja.Rmd"),
    output_file = here::here("reports", "manuscript.docx"),
    output_yaml = here::here("reports", "docx.yaml"),
    params = list(render_img = TRUE, inc_tabs = TRUE,
                  nocite_list = 
                    paste0("@",
                           paste(reviews_raw$bibtex_key, collapse = ", @")))
  )
  # tar_render(
  #   pdf_report,
  #   path = here::here("reports", "manuscript_papaja.Rmd"),
  #   output_file = here::here("reports", "manuscript.pdf"),
  #   #output_yaml = here::here("reports", "pdf.yaml"),
  #   params = list(render_img = TRUE, inc_tabs = FALSE,
  #                 nocite_list =
  #                   paste0("@",
  #                          paste(reviews_raw$bibtex_key, collapse = ", @")))
  # )
)

# ------------------- FUNCTIONS ----------------------

## Bibliography ----
# Create packages bibliography
create_packages_bib <- function(packages, bibpath){
  knitr::write_bib(packages, bibpath)
  return(bibpath)
}

# Combine bibliography files
combine_bibs <- function(packages_bib, references_bib, reviews_raw, outpath){
  # Fix UTF-8 encoding
  refs <- readLines(references_bib, encoding="UTF-8") 
  refs <- str_replace_all(refs, "[^[:graph:]]", " ") 
  refs <- iconv(refs, from = 'UTF-8', to = 'ASCII//TRANSLIT') 
  refs_tmp <- tempfile(fileext = ".bib")
  writeLines(refs, con=refs_tmp)
  
  paths <- c(packages_bib, refs_tmp)
  bibs <- lapply(paths, bib2df)
  full_bib <- bind_rows(bibs)
  
  full_bib <-
    full_bib %>%
    mutate(ANNOTE = if_else(BIBTEXKEY %in% reviews_raw$bibtex_key,
                            "*",
                            ""))
  
  df2bib(full_bib, file = outpath)
  
  rm(refs_tmp)
  return(outpath)
}