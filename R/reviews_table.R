source("R/functions.R")

reviews_df <- read_sheet("ReviewLevelValidation")
rob_df <- read_sheet("QualityAssessment")
effects_df <- get_effects()

# Clean RoB
rob <- rob_df %>% 
  filter(reviewer=="Consensus") %>% 
  select(-starts_with("x"), -study_id, -reviewer)

# Clean effects
effects <- effects_df %>%
  select(covidence_review_id, plain_language_outcome, 
         plain_language_exposure) %>% 
  rename(covidence_id = covidence_review_id)

reviews <- reviews_df %>% 
  transmute(covidence_id = covidence_review_id_auto,
            first_author = first_author_consensus,
            year = year_of_publication_consensus,
            design_restrictions = study_design_restrictions_extracted_consensus,
            regions_restrictions = regions_consensus,
            demographics_restrictions = demographics_consensus,
            earliest_study_year = earliest_study_publication_year_consensus,
            latest_study_year = latest_study_publication_year_consensus,
            sample_age_mean_low = as.numeric(sapply(sample_age_lowest_study_mean_consensus, toString)),
            samples_age_mean_high = as.numeric(sapply(sample_age_highest_study_mean_concensus, toString)))
  

unique_string <- function(column) {
  paste(column %>% unique() %>% sort(), collapse = "<br/>")
}

# Join datasets
reviews %>% 
  # Merge the datasets
  left_join(rob) %>% 
  left_join(effects) %>% 
  group_by(covidence_id) %>% 
  mutate(outcomes_assessed = unique_string(plain_language_outcome),
         exposures_assessed = unique_string(plain_language_exposure)) %>% 
  ungroup() %>% 
  select(-plain_language_outcome, -plain_language_exposure) %>% 
  distinct() %>% 
  select(covidence_id, outcomes_assessed, exposures_assessed)
