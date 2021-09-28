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
        here::here("reports", "combined.bib")
      ),
      format = "file"
    )
  )

## Metadata ----
create_metadata <-
  list(
    tar_target(
      authors_modified_date,
      get_mod_date(authors_sheet),
      # Force run if outdated or doesn't exist
      cue = tar_cue_force(
        condition = ifelse(
          tar_exist_objects("authors_modified_date"),
          get_mod_date(authors_sheet) != tar_read(authors_modified_date),
          TRUE
        )
      )
    ),
    tar_target(
      authors_details,
      read_sheet(authors_sheet, "Authors", authors_modified_date)
    ),
    tar_target(front_yml,
               make_yaml(authors_details),
               format = "file")
  )

## Create reports ----
create_reports <- list(
  tar_render(
    distill_report,
    path = here::here("reports", "manuscript.Rmd"),
    output_file = here::here("index.html"),
    output_yaml = here::here("reports", "distill.yaml"),
    params = list(render_img = TRUE, inc_ref = FALSE)
  ),
  tar_render(
    markdown_report,
    path = here::here("reports", "manuscript.Rmd"),
    output_file = here::here("reports", "manuscript.md"),
    output_yaml = here::here("reports", "md.yaml"),
    params = list(render_img = FALSE, inc_ref = TRUE)
  )
  
)

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

## Metadata ----

make_yaml <- function(authors_details){
  filepath <- here::here("reports","header.yaml")
  
  # Remove existing
  unlink(filepath)
  
  yml() %>%
    yml_title(article_title) %>%    
    yml_distill_opts(
      repository_url = 
        "https://github.com/Motivation-and-Behaviour/screen_umbrella") %>% 
    yml_distill_author(name = authors_details$name,
                       affiliation = authors_details$affiliation,
                       affiliation_url = authors_details$affiliation_url,
                       orcid_id = authors_details$orcid,
                       url = NULL) %>% 
    yml_citations(bibliography = "combined.bib",
                  csl = "vancouver-brackets.csl") %>% 
    yml_params(render_img= TRUE,
               inc_ref= FALSE) %>% 
    use_yml_file(filepath, quiet = TRUE)
  return(filepath)
  
}