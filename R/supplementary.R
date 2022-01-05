# ------------------- SETTINGS -----------------------

# ------------------- TARGETS ------------------------

make_supps <- 
  list(
    tar_target(
      supp_exposures,
      make_supp_exposures(combined_effects),
    format = "file"))


# ------------------- FUNCTIONS ----------------------

make_supp_exposures <- 
  function(combined_effects){
    out <- 
      combined_effects %>% 
        transmute(plain_language_exposure = str_replace(
          plain_language_exposure,
          "^Intervention:",
          "Screen-based intervention:"
        )) %>% 
        arrange(plain_language_exposure) %>%
        distinct()
    
    out_path <- here::here("supplementary_files","List of Exposures.csv")
    write_csv(out, file = out_path, col_names = FALSE)
    
    # Upload to GDrive
    drive_put(out_path, 
              path = as_id("https://drive.google.com/drive/folders/1gW5k2CIRJf1w2FP0dbUHCXwpAxtV1Gxp"))
    
    
    return(out_path)
  }