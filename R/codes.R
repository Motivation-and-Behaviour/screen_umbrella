# These are the recoding rules for the variables in the dataset that are used in
# more than one place.

# nolint start

# Ages ----------------------------------------------------------------------
age_codes_list <- list(
  mixed = c(
    "<=18 years",
    "<18 years",
    "<18",
    "<20 years",
    "0-21 years old",
    "1 - 18 years",
    "1-18 years",
    "10 - 19 years",
    "10-19 years",
    "10-20 years old",
    "10-24 years",
    "11-20",
    "2-19 years",
    "3-18 years",
    "5-18 years",
    "5-19.9",
    "6-18 years",
    "6-18 years (or grade 1-12)",
    "6-19",
    "6-21 years",
    "All",
    "Children; Adolescents",
    "Children/adolescents",
    "Include: children and adolescents",
    "K-12",
    "mixed",
    "None specified",
    "PreK-12th grade",
    "preschool-college",
    "Preschool, K-12, undergraduate (graduate), and mixed-stage learners",
    "Primary or secondary school",
    "School-age Children",
    "Under 18"
  ),
  adolescents_codes = c(
    "<=14", # lower bound is 11 in this study
    "11-18 years",
    "12-17 years",
    ">14",
    "12-30 years",
    "13-18",
    "15-30 years old",
    "adolescence",
    "adolescents",
    "Adolescents",
    "at least 13 years",
    "School-age Children (High School)",
    "School-age Children (Middle School)",
    "School-age Children (Middle/High School)",
    "school-age_high school",
    "School-age_High School",
    "High School",
    "Lower secondary education",
    "Middle School",
    "Middle/High school",
    "Secondary",
    "Secondary School",
    "Upper secondary education"
  ),
  children_codes = c(
    "Kindergarten",
    "12 or under",
    "12 or younger",
    "K-3",
    "3-12 years",
    "6-11 years",
    "7-12",
    "8-14 years",
    "Elementary",
    "Elementary & Kindergarten",
    "Elementary school",
    "Average 6 years, all must be less than 8 years",
    "childhood",
    "children",
    "Children",
    "Early childhood; School-age Children (Primary/Elementary)",
    "Early childhood/pre-school; School-age Children (Early Primary/Elementary)",
    "Elementary school students",
    "grades 4-7",
    "K - Grade 8",
    "Kindergarten-Grade 6",
    "older than 8",
    "Pre K to Grade 3",
    "Preschool to Grade 3 (3-9 years old)",
    "School-age Children (Early Primary/Elementary)",
    "School-age Children (Primary/Elementary)",
    "School-age Children (Primary/Elementary/Middle School)",
    "Pre/elementary",
    "Preparatory education",
    "Primary",
    "Primary education"
  ),
  young_children_codes = c(
    "0-5",
    "0-6",
    "1 month - 4.99 years old",
    "Preschool",
    "1-6 years & prior to school entry",
    "1-8 years old",
    "Early childhood/pre-school",
    "Infants",
    "Infants, toddlers, and preschoolers (0-7 years)",
    "pre-school",
    "Preschoolers",
    "Toddlers",
    "younger than 8",
    "young_children"
  )
)


# Demographics restrictions -------------------------------------------------
demo_codes_list <- list(
  demo_all = c(
    "<18 years",
    "<20 years",
    "<=18 years",
    "None specified",
    "Under 18"
  ),
  demo_adol = c(
    "10 - 19 years",
    "10-19 years",
    "10-20 years old",
    "10-24 years",
    "11-18 years",
    "11-20",
    "15-30 years old",
    "12-30 years"
  ),
  demo_child = c(
    "12 or under",
    "3-12 years"
  ),
  demo_child_adol = c(
    "0-21 years old",
    "1 - 18 years",
    "1-18 years",
    "2-19 years",
    "3-18 years",
    "5-18 years",
    "5-19.9",
    "6-18 years",
    "6-18 years (or grade 1-12)",
    "6-19",
    "6-21 years",
    "8-14 years",
    "Children/adolescents",
    "Include: children and adolescents"
  ),
  demo_early = c(
    "0-5",
    "1 month - 4.99 years old",
    "1-6 years and prior to school entry",
    "Infants, toddlers, and preschoolers (0-7 years)",
    "Early childhood/pre-school"
  ),
  demo_early_child = c(
    "1-8 years old",
    "Average 6 years, all must be less than 8 years",
    "Early childhood/pre-school; School-age Children (Early Primary/Elementary)"
  ),
  demo_sch_all = c(
    "K-12",
    "PreK-12th grade",
    "Preschool, K-12, undergraduate (graduate), and mixed-stage learners",
    "Primary or secondary school",
    "preschool-college"
  ),
  demo_sch_elm = c(
    "Elementary school students",
    "Kindergarten-Grade 6",
    "School-age Children (Primary/Elementary)"
  ),
  demo_sch_early_elm = c(
    "Pre K to Grade 3",
    "Preschool to Grade 3 (3-9 years old)",
    "School-age Children (Early Primary/Elementary)",
    "Early childhood/pre-school; School-age Children (Primary/Elementary)"
  ),
  demo_sch_elm_mid = c(
    "K - Grade 8",
    "School-age Children (Primary/Elementary/Middle School)"
  ),
  demo_sch_high = c(
    "School-age Children (High School)"
  ),
  demo_sch_mid = c(
    "School-age Children (Middle School)"
  ),
  demo_sch_mid_high = c(
    "School-age Children (Middle, High School)"
  )
)

# Inclusion/exclusion criteria ----------------------------------------------
na_codes <- c(
  "None",
  "none specified",
  "None specified",
  "None Specified"
)

# Atypical Sample -----------------------------------------------------------
sample_codes_list <- list(
  clin_sample_incl = c(
    "Atypically developing",
    "Autism",
    "Chronic disease",
    "Overweight and obese",
    "Disabilities"
  ),
  edu_sample_incl = c(
    "Learning disabilities or difficulties",
    "Math difficulties",
    "Poor readers",
    "Dyscalculia"
  )
)

# Study Design --------------------------------------------------------------
design_codes_list <- list(
  design_unspec = c(
    "Include: All quantitative designs",
    "Include: All quantitative designs; Experimental",
    "Include: Cohort; Case-control; Cross-sectional; Intervention trials. \nExclude: Case reports; Retrospective studies.",
    "Include: Empirical",
    "Include: Experimental, correlational, or longitudinal",
    "Include: Experimental; Correlational",
    "Include: Experimental; Cross-sectional; Longitudial",
    "Include: Experimental; Observational",
    "Include: Experimental; Quasi-experiment; Cross-sectional; Longitudial",
    "Include: Observational or experimental designs",
    "Include: Quantitative designs",
    "Include: Randomised controlled trials; Quasi-experimental; Observational",
    "Include: experimental or observational",
    "Include: longitudinal, cohort, case-control, cross-sectional, or controlled trials",
    "Include: observational and intervention studies",
    "Include: quantitative designs",
    "None",
    "None specified"
  ),
  design_obs_mixed = c(
    "Include: Cohort; Case-control; Cross-sectional",
    "Include: Cohort; Case-control; Cross-sectional\nExclude: Interventions",
    "Include: Correlational studies",
    "Include: Cross-sectional or longitudinal",
    "Include: Cross-sectional; Case-control; Longitudinal",
    "Include: Cross-sectional; Longitudinal; Case-control",
    "Include: Observational",
    "Include: Observational\nExclude: Experimental",
    "Include: Observational\nExclude: Qualitative",
    "Include: Observational designs",
    "Include: cross-sectional, case-control, and cohort studies",
    "Include: observational cross-sectional, case-control, or longitudinal designs",
    "Include: observational designs"
  ),
  design_obs_cross = c(
    "Include: Cross-sectional",
    "Include: Cross-sectional only",
    "Include: Cross-sectional studies",
    "Include: cross-sectional",
    "Include: cross-sectional only",
    "cross-sectional",
    "Cross-sectional"
  ),
  design_obs_long = c(
    "Include: Longitudinal",
    "Include: Longitudinal designs only",
    "Include: Longitudinal; Retrospective",
    "Include: Prospective design",
    "Include: longitudinal or cohort designs",
    "longitudinal",
    "Prospective",
    "Longitudinal"
  ),
  design_exp = c(
    "Include: Cross-over or parallel randomized controlled trials",
    "Include: Experimental",
    "Include: Experimental\nExclude: Designs with no control group",
    "Include: Experimental designs",
    "Include: Experimental or quasi-experimental designs",
    "Include: Experimental with a control group or reference norm",
    "Include: Experimental with control group",
    "Include: Experimental; Quasi-experimental",
    "Include: Experimental; Quasi-experimental (with control group)",
    "Include: Experimental; Quasi-experimental; Pre-test post-test",
    "Include: Experimental; Single case",
    "Include: Experimental; quasi-experimental",
    "Include: Group-control experimental design",
    "Include: Intervention",
    "Include: Interventions",
    "Include: Interventions (pre-post or controlled). \nExclude: Cross-sectional",
    "Include: Interventions (with comparison)",
    "Include: Pretest–posttest with control group",
    "Include: Randomised controlled trials",
    "Include: Randomised controlled trials\nExclude: Uncontrolled, cross-sectional and animal studies",
    "Include: Randomised controlled trials and quasi-RCTs.",
    "Include: Randomised controlled trials and quasi-RCTs. \nExclude: One group pretest–posttest; One group posttest only design",
    "Include: Randomised controlled trials; Quasi-experimental\n\nExclude: Cohort",
    "Include: Randomized controlled trials",
    "Include: Randomized controlled trials; Quasi-experimental studies; Nonrandomized trials. Exclude: Small pilot; Feasibility trials without follow-up trial",
    "Include: Within subject design; between subject design",
    "Include: desgins with a control group",
    "Include: designs with control groups",
    "Include: experiemental designs with control group",
    "Include: experimental and quasi-experimental designs",
    "Include: experimental designs",
    "Include: experimental or quasi-experimental",
    "Include: experimental or quasi-experimental with control group",
    "Include: experimental with control group",
    "Include: experimental, quasi-experimental, or pre-post test",
    "Include: pre-post designs with or without control group",
    "Include: random assignment or quasi-experimental",
    "Include: randomised experimental designs",
    "Include: randomized and non-randomized controlled with control group with no intervention or traditional exercise intervention",
    "Include: randomized controlled trials",
    "Include: randomized controlled trials, quasi-experimental studies, and single-case control studies",
    "Include: studies with control group",
    "include: randomized and non-randomized controlled trials (control group with no intervention or traditional exercise intervention)",
    "RCTs",
    "Quasi-experimental",
    "Parallel RCT", "True experiments"
  )
)

# nolint end
