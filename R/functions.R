library(tidyverse)
library(janitor)

# Conversion functions ####

#conversion formula from 10.1007/s11162-011-9232-5
b2r <- function(beta){
  #test beta: beta = -.02
  r <- NA
  if(!is.na(beta)){
    r <- beta 
  }
  r
}

b2r <- Vectorize(b2r)

d2r <- function(d, a = 4){
  #assumes equal groups
  #https://www.meta-analysis.com/downloads/Meta-analysis%20Converting%20among%20effect%20sizes.pdf
  #10.1002/jrsm.1218
  if(is.na(d)){NA}else{d/(sqrt(d^2+a))}
}

d2r <- Vectorize(d2r)

z2r <- function(z){
  #test z <- 3.4
  if(is.na(z)){NA}else{tanh(z)}
}

z2r <- Vectorize(z2r)

od2r <- function(or, method=c("pearson","digby")){
  #DOI:10.1037/0003-066X.62.3.254
  if(is.na(or)){NA
    }else{switch(method,
         pearson = cos(pi/(1+or^.5)),
         digby = (or^(3/4)-1)/(or^(3/4)+1)
  )
    }
}

od2r <- Vectorize(od2r)

# Read data ####
read_sheet <- function(){
  d = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1z_NZwDomPfrOJg2Rn8-E8cc9yoOjXzqH_Di23vWERu4/edit#gid=1427279106",
                                sheet = "EffectSizesValidation",
                                na = c("-999", "", "#N/A")) %>%
    janitor::clean_names() %>%
    #set_names(str_remove(names(.), "_[0-9]+")) %>%
    mutate(es = str_to_lower(statistical_test_consensus)) %>%
    select(-ends_with("_r"))
    
  return(d)
}

# Convert Data ----
simplify_effects <- function(data){
  d = data %>%
    dplyr::filter(es %in% c("b", "d", "r", "or", "z")) %>%
    dplyr::rename(std_eff_name = es)
  return(d)
}

convert_data <- function(data){
  d <- data %>%  
    mutate(value_ci_lower_bound_consensus =
             case_when(
               !is.na(value_raw_se) ~ value_consensus-2*value_raw_se,
               TRUE ~ value_ci_lower_bound_consensus
             ),
           value_ci_upper_bound_consensus =
             case_when(
               !is.na(value_raw_se) ~ value_consensus+2*value_raw_se,
               TRUE ~ value_ci_upper_bound_consensus
             )) %>%
    mutate(value_consensus = case_when(
      is.na(value_consensus) ~ NA_real_,
      std_eff_name == 'r' ~ value_consensus,
      std_eff_name == 'b' ~ b2r(value_consensus),
      std_eff_name == 'd' ~ d2r(value_consensus),
      std_eff_name == 'or' ~ od2r(value_consensus, method = 'digby'),
      std_eff_name == 'z' ~ z2r(value_consensus)
    ),
    value_ci_lower_bound_consensus = case_when(
      is.na(value_ci_lower_bound_consensus) ~ NA_real_,
      std_eff_name == 'r' ~ value_ci_lower_bound_consensus,
      std_eff_name == 'b' ~ b2r(value_ci_lower_bound_consensus),
      std_eff_name == 'd' ~ d2r(value_ci_lower_bound_consensus),
      std_eff_name == 'or' ~ od2r(value_ci_lower_bound_consensus, method = 'digby'),
      std_eff_name == 'z' ~ z2r(value_ci_lower_bound_consensus)
    ),
    value_ci_upper_bound_consensus = case_when(
      is.na(value_ci_upper_bound_consensus) ~ NA_real_,
      std_eff_name == 'r' ~ value_ci_upper_bound_consensus,
      std_eff_name == 'b' ~ b2r(value_ci_upper_bound_consensus),
      std_eff_name == 'd' ~ d2r(value_ci_upper_bound_consensus),
      std_eff_name == 'or' ~ od2r(value_ci_upper_bound_consensus, method = 'digby'),
      std_eff_name == 'z' ~ z2r(value_ci_upper_bound_consensus)
    ))
  
  return(d)
}

# Clean effects data ####

get_effects <- function() {
  
  raw <- read_sheet() 
  simple <- simplify_effects(raw)
  d <- convert_data(simple)
  
  # Clean the names of the datafile, rename to something more meaningful, 
  # remove empty stuff, then cut small studies or rubbish
  q <- clean_names(d) %>%
    dplyr::rename(r = value_consensus,
                  outcome_category = outcome_level_1,
                  outcome = outcome_level_2,
                  moderator_level = moderator_level_recoded,
                  moderator_category = moderator_category_recoded,
                  k = k_number_of_effects_informing_this_test_consensus,
                  n = combined_n,
                  cilb = value_ci_lower_bound_consensus,
                  ciub = value_ci_upper_bound_consensus,
                  i2 = i2_calculated
    ) %>%
    remove_empty(which = c("rows", "cols")) %>%
    mutate(n = as.numeric(n)) %>%
    filter(r < .99,
           moderator_level != "fixed effects",
           k>1,
           use_moderator==TRUE) 
  
  q$i2 <- as.numeric(sapply(q$i2, as.numeric))
  q$effect_size_id_1 <- as.character(sapply(q$effect_size_id_1, as.character))

  # Add significance and labels
  q$sig <- ((q$cilb * q$ciub) > 0)
  q$sig <- q$sig * .7 + .3
  q$author_year <- paste(q$first_author, ", ", q$year_of_publication, sep = "")
  
  #bold the rows that are classified as 'risks'
  q$risk <- ifelse(q$benefit_or_risk=="Risk", "bold", "plain")
  
  #if one effect from this review, keep or select "overall"
  # group by study_id and exposure and outcome, pick max n
  q <- rename(q, 
              plain_language_outcome = outcome_plain_language_descriptor) %>%
    group_by(plain_language_outcome,
             plain_language_exposure) %>% slice_max(n,
                                                    with_ties = TRUE) %>%
    select(author_year, covidence_review_id,
           outcome_category, effect_size_id_1,
           plain_language_outcome,
           plain_language_exposure, 
           risk,
           k, n, r, cilb, ciub, i2, sig) %>%
    distinct()
  
  return(q)
}
