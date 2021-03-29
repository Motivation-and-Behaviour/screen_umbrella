
# General functions ####
# We should assume that Hedges g == Cohen's d and RR == OR
regex_data = tidyr::tribble(
  ~std_eff_name, ~regex,
  "b", "beta",
  "d", "(cohen d|cohen's d)",
  "r", "(pearson's r|correlation|^r$)",
  "d", "(hedges' g|g\\+)",
  "d", "(smd|mean difference|Mean|mean)",
  "or","relative risk",
  "or", "(odds ratio|or|odd ratio)",
  "z", "(Z|z)"
)

# conversion functions ####

#conversion formula from 10.1037/0021-9010.90.1.175
b2r <- function(beta){
  #test beta: beta = -.02
  if(!is.na(beta)){
    if(beta >= 0){
      r <- beta * .98 + .05
    } else {
      r <- beta * .98
    }
    r
  }
}

b2r <- Vectorize(b2r)

d2r <- function(d, a = 4){
  #assumes equal groups
  #https://www.meta-analysis.com/downloads/Meta-analysis%20Converting%20among%20effect%20sizes.pdf
  if(is.na(d)){NA}else{d/(sqrt(d^2+a))}
}

d2r <- Vectorize(d2r)

z2r <- function(z){
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
                                sheet = "Effects_Sortable",
                                na = "-999") %>%
    janitor::clean_names() %>%
    set_names(str_remove(names(.), "_[0-9]+")) %>%
    mutate(es = refinr::key_collision_merge(statistical_test_consensus) %>% str_to_lower()) %>%
    select(-ends_with("_r"))
    
  
  return(d)
}

simplify_effects <- function(data, regex = regex_data){
  d = data %>%
    dplyr::mutate(es = replace(es, es == 'r = uncorrected sample-weighted mean effect size', 'r')) %>%
    fuzzyjoin::regex_inner_join(regex, by = c(es = "regex"))
  
  return(d)
}

convert_data <- function(data){
  d = data %>%  
    mutate(value_ci_lower_bound_consensus = 
             case_when(
               !is.na(value_raw_se) ~ value_consensus_various_raw+2*value_ci_lower_bound_consensus_various_raw,
               TRUE ~ value_ci_lower_bound_consensus_various_raw
             ),
           value_ci_upper_bound_consensus = 
             case_when(
               !is.na(value_raw_se) ~ value_consensus_various_raw+2*value_ci_upper_bound_consensus_various_raw,
               TRUE ~ value_ci_upper_bound_consensus_various_raw
             )) %>%
    mutate(value_consensus_r = case_when(
    is.na(value_consensus_various_raw) ~ NA_real_,
    std_eff_name == 'r' ~ value_consensus_various_raw,
    std_eff_name == 'b' ~ b2r(value_consensus_various_raw),
    std_eff_name == 'd' ~ d2r(value_consensus_various_raw),
    std_eff_name == 'or' ~ od2r(value_consensus_various_raw, method = 'digby'),
    std_eff_name == 'z' ~ z2r(value_consensus_various_raw)
  ),
  value_ci_lower_bound_consensus_r = case_when(
    is.na(value_ci_lower_bound_consensus_various_raw) ~ NA_real_,
    std_eff_name == 'r' ~ value_ci_lower_bound_consensus_various_raw,
    std_eff_name == 'b' ~ b2r(value_ci_lower_bound_consensus_various_raw),
    std_eff_name == 'd' ~ d2r(value_ci_lower_bound_consensus_various_raw),
    std_eff_name == 'or' ~ od2r(value_ci_lower_bound_consensus_various_raw, method = 'digby'),
    std_eff_name == 'z' ~ z2r(value_ci_lower_bound_consensus_various_raw)
  ),
  value_ci_upper_bound_consensus_r = case_when(
    is.na(value_ci_upper_bound_consensus_various_raw) ~ NA_real_,
    std_eff_name == 'r' ~ value_ci_upper_bound_consensus_various_raw,
    std_eff_name == 'b' ~ b2r(value_ci_upper_bound_consensus_various_raw),
    std_eff_name == 'd' ~ d2r(value_ci_upper_bound_consensus_various_raw),
    std_eff_name == 'or' ~ od2r(value_ci_upper_bound_consensus_various_raw, method = 'digby'),
    std_eff_name == 'z' ~ z2r(value_ci_upper_bound_consensus_various_raw)
  ))
  
  return(d)
}

 
  
