library(ymlthis)

list(  
  tar_target(
  authors_date,
  get_mod_date("1V-j8oQXI3Y8etUnaRETvUvq9Hr8eAbAoBc_0XoLgb48"),
  # Force run if outdated or doesn't exist
  cue = tar_cue_force(
    condition = ifelse(
      tar_exist_objects("authors_date"),
      get_mod_date("1V-j8oQXI3Y8etUnaRETvUvq9Hr8eAbAoBc_0XoLgb48") != tar_read(authors_date),
      TRUE
    )
  ),

  tar_target(
    yml_base,
    yml() %>%
      yml_title(
        "Benefits and risks associated with children’s and adolescents’ interactions with electronic screens: An umbrella review"
      ) %>%
      yml_citations(bibliography = "combined.bib",
                    csl = "nature.csl") 
  tar_target(
    distill_header,
    yml_base %>% 
      
  )
  
  )
  
  
  
  
  
  %>%
    yml_distill_opts(repository_url = "https://github.com/Motivation-and-Behaviour/screen_umbrella")),
  