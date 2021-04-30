library(DiagrammeR)
library(datapasta)
library(tidyverse)

covidence_export <- "41928 references imported for screening as 41921 studies
	24023 duplicates removed
17898 studies screened against title and abstract
	15934 studies excluded
1959 studies assessed for full-text eligibility
	1789 studies excluded
		526  Not a systematic review
		508  Systematic review without meta-analysis
		187  Wrong exposure - clinical health intervention 
		138  Not a full-text
		117  Not published in English
		89  Doesn&#39;t examine exposure-outcome relationship
		73  Wrong exposure - other (explain)
		65  Wrong population
		30  Not a review
		22  Duplicated reference
		14  Meta analysis that meets inclusion criteria, but youth results not presented and authors did not/could not provide individual study data when contacted.
		11  Individual level data only
		3  Full-text article could not be retrieved
		2  DUPLICATE TO MERGE LATER
		2  Review of reviews
		1  Clinical outcome
		1  Sys review of qual studies
	0 studies ongoing
	0 studies awaiting classification
170 studies included"


dat <- 
  # Convert each to a seperate line
tibble(text = unlist(str_split(covidence_export, pattern = "\\n"))) %>%
  # remove unneeded info
  filter(!str_detect(text, "studies ongoing"), !str_detect(text, "studies awaiting classification")) %>%
  # Determine row type
  mutate(
    type = case_when(
      startsWith(text, "\t\t") ~ "excl_reason",
      startsWith(text, "\t") ~ "substep",
      TRUE ~ "step"
    ),
    step = 0
  )
# Generate steps
step = 0
for (i in 1:nrow(dat)){
  if (dat$type[i]=="step") step <- step+1
  dat$step[i] <- step
}

# Modify dataframe before running below

# Convert excl_reason to new column
dat <- dat %>% mutate(excl_reason = if_else(type=="excl_reason", text, "")) %>% 
  group_by(step, type) %>% 
  mutate(excl_reason = paste(excl_reason, collapse =  "\\\\n")) %>% 
  distinct(step, type, .keep_all = TRUE)

grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle]        
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      tab4 [label = '@@4']
      tab5 [label = '@@5']
      tab6 [label = '@@6']

      # edge definitions with the node IDs
      {rank=same; tab2; tab6}
      tab1 -> tab2 -> tab3 -> tab4 -> tab5;
      tab2 -> tab6;
      {rank=same; tab4; tab5}
      
      }

      [1]: 'Questionnaire sent to n=1000 participants'
      [2]: 'Participants responded to questionnaire n=850'
      [3]: 'Participants came to clinic for evaluation n=700'
      [4]: 'Participants eligible for the study n=600'
      [5]: 'Study sample n=600'
      [6]: 'A new tab\\nwith a new line.'
      ")
