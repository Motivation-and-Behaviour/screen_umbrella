library(tidyverse)
# conversion functions ####

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

read_sheet <- function(){
  d = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1z_NZwDomPfrOJg2Rn8-E8cc9yoOjXzqH_Di23vWERu4/edit#gid=1427279106",
                                sheet = "EffectSizes",
                                na = "-999") %>%
    janitor::clean_names() %>%
    #set_names(str_remove(names(.), "_[0-9]+")) %>%
    mutate(es = str_to_lower(statistical_test_consensus)) %>%
    select(-ends_with("_r"))
    
  return(d)
}

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

