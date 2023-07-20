# Set options
options(tidyverse.quiet = TRUE, clustermq.scheduler = "multiprocess")

# Load required functions and packages
lapply(list.files("./R", full.names = TRUE), source)
load_packages()

list(
  # Read data ---------------------------------------------------------------
  tar_file_read(
    effects_raw,
    "data/Effects.csv",
    read_sheet(file = !!.x)
  ),
  tar_file_read(
    reviews_raw,
    "data/Reviews.csv",
    read_sheet(file = !!.x)
  ),
  tar_file_read(
    studies_raw,
    "data/Studies.csv",
    read_sheet(file = !!.x)
  ),
  tar_file_read(
    rob_raw,
    "data/QualityAssessment.csv",
    read_sheet(file = !!.x)
  ),
  tar_target(prisma_path,
    here::here("data/covidence_prisma.txt"),
    format = "file"
  ),
  # Set params --------------------------------------------------------------
  tar_target(age_codes, age_codes_list),
  tar_target(demo_codes, demo_codes_list),
  tar_target(sample_codes, sample_codes_list),
  tar_target(design_codes, design_codes_list),
  # Clean data --------------------------------------------------------------
  tar_target(
    reviews_clean,
    clean_reviews(
      reviews_raw, effects_raw, age_codes, demo_codes, sample_codes,
      design_codes
    )
  ),
  tar_target(
    effects_clean,
    clean_effects(
      effects_raw, reviews_clean, age_codes, sample_codes, design_codes
    )
  ),
  tar_target(
    studies_clean,
    clean_studies(studies_raw),
    iteration = "group"
  ),
  # Re-run analyses ---------------------------------------------------------
  tar_target(
    meta_results_r,
    run_metaanalysis(studies_clean, type = "r"),
    pattern = map(studies_clean),
    iteration = "list"
  ),
  tar_target(
    meta_aggregated_r,
    tidy_meta(meta_results_r),
    pattern = map(meta_results_r)
  ),
  tar_target(
    eggers_results_r,
    run_eggers(meta_results_r),
    pattern = map(meta_results_r)
  ),
  tar_target(
    excess_sig_results_r,
    run_excess_sig(meta_results_r),
    pattern = map(meta_results_r)
  ),
  tar_target(
    studies_results,
    combine_study_results(
      meta_aggregated_r, eggers_results_r, excess_sig_results_r
    )
  ),
  tar_target(
    combined_effects,
    combine_effects(effects_clean, studies_results)
  ),
  # Make tables -------------------------------------------------------------
  tar_target(
    tables_df,
    make_table_df(rob_raw, effects_clean, reviews_clean)
  ),
  tar_target(
    table_desc_gt,
    make_table_desc_gt(tables_df)
  ),
  tar_target(
    table_rob_gt,
    make_table_rob(tables_df)
  ),
  tar_target(
    table_desc_saved,
    save_table(
      table_desc_gt,
      "supplementary_files/Supplementary File 2 - Review characteristics.pdf",
      method = "webshot"
    ),
    format = "file",
  ),
  tar_target(
    table_rob_saved,
    save_table(table_rob_gt, "tables/Table 1 - Quality assessment.pdf",
      method = "webshot"
    ),
    format = "file",
  ),
  tar_target(
    table_rob_saved_docx,
    save_table(table_rob_gt, "tables/Table 1 - Quality assessment.docx",
      method = "webshot2"
    ),
    format = "file",
  ),
  tar_target(
    table_effects,
    make_table_effects(combined_effects)
  ),
  tar_target(
    table_effects_saved,
    save_table(
      table_effects,
      "supplementary_files/Supplementary File 3 - Effect Characteristics.pdf"
    ),
    format = "file",
  ),
  # Make plots --------------------------------------------------------------
  tar_target(
    prisma,
    make_prisma(prisma_path, effects_clean, reviews_clean, combined_effects)
  ),
  tar_target(
    prisma_saved,
    save_prisma(prisma),
    format = "file",
  ),
  tar_target(
    edu_certain_params,
    edu_certain,
  ),
  tar_target(
    edu_uncertain_params,
    edu_uncertain,
  ),
  tar_target(
    nonedu_certain_params,
    nonedu_certain,
  ),
  tar_target(
    nonedu_uncertain_params,
    nonedu_uncertain,
  ),
  tar_target(
    researchbriefing_params,
    researchbriefing,
  ),
  tar_target(
    plot_edu_certain,
    make_forest_plot(combined_effects, edu_certain_params)
  ),
  tar_target(
    plot_edu_uncertain,
    make_forest_plot(combined_effects, edu_uncertain_params)
  ),
  tar_target(
    plot_nonedu_certain,
    make_forest_plot(combined_effects, nonedu_certain_params)
  ),
  tar_target(
    plot_nonedu_uncertain,
    make_forest_plot(combined_effects, nonedu_uncertain_params)
  ),
  tar_target(
    plot_researchbriefing,
    make_forest_plot(
      combined_effects %>%
        filter(gsub(":.*", "", plain_language_outcome) == "Learning"),
      researchbriefing_params
    )
  ),
  tar_target(
    plot_edu_certain_saved,
    save_plot(plot_edu_certain, edu_certain_params),
    format = "file",
  ),
  tar_target(
    plot_edu_uncertain_saved,
    save_plot(plot_edu_uncertain, edu_uncertain_params),
    format = "file",
  ),
  tar_target(
    plot_nonedu_certain_saved,
    save_plot(plot_nonedu_certain, nonedu_certain_params),
    format = "file",
  ),
  tar_target(
    plot_nonedu_uncertain_saved,
    save_plot(plot_nonedu_uncertain, nonedu_uncertain_params),
    format = "file",
  ),
  tar_target(
    plot_researchbriefing_saved,
    save_plot(plot_researchbriefing, researchbriefing_params),
    format = "file",
  ),
  # Create supplementary materials plots ------------------------------------
  tar_target(
    supp_exposures,
    make_supp_exposures(combined_effects),
    format = "file"
  ),
  tar_target(
    supp_effects,
    make_supp_effects(combined_effects),
    format = "file"
  ),
  # Create manuscript  ------------------------------------------------------
  tar_target(
    packages_bib,
    make_packages_bib(
      .packages(),
      here::here("reports", "packages.bib")
    ),
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
  ),
  tar_target(
    manuscript_info,
    make_manuscript_info(
      effects_clean, prisma, tables_df, combined_effects, studies_clean
    )
  ),
  tar_target(other_supps_files, c(
    "supplementary_files/Supplementary File 7 - Search Strategy.pdf",
    "supplementary_files/Supplementary File 11 - PRISMA 2020 checklist.pdf",
    "supplementary_files/Supplementary File 12 - PRISMA 2020 abstract checklist.pdf" # nolint
  ), format = "file"),
  tar_target(join_supp_py_script, "python/combine_pdfs.py", format = "file"),
  tar_target(joined_supps,
    join_supps(
      join_supp_py_script, other_supps_files, table_effects_saved,
      supp_exposures, supp_effects, references, table_desc_saved
    ),
    format = "file"
  ),
  tar_render(
    manuscript,
    path = here::here("reports", "manuscript.Rmd")
  ),
  tar_render(
    manuscript_docx,
    path = here::here("reports", "manuscript.Rmd"),
    output_format = papaja::apa6_docx()
  ),
  tar_render(
    references,
    path = here::here("reports", "references.Rmd"),
    output_file = here::here(
      "supplementary_files",
      "Supplementary File 9 - Included Studies.pdf"
    ),
    params = list(
      nocite_list =
        paste0(
          "@",
          paste(reviews_raw$bibtex_key, collapse = ", @")
        )
    )
  )
)
