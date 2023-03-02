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
  tar_target(
    age_codes,
    list(
      mixed = mixed_codes, adolescents = adolescents_codes,
      children = children_codes, young_children = young_children_codes
    )
  ),
  # Clean data --------------------------------------------------------------
  tar_target(
    reviews_clean,
    clean_reviews(reviews_raw, effects_raw, age_codes)
  ),
  tar_target(
    effects_clean,
    clean_effects(effects_raw, reviews_clean, age_codes)
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
    meta_results_z,
    run_metaanalysis(studies_clean, type = "z"),
    pattern = map(studies_clean),
    iteration = "list"
  ),
  tar_target(
    meta_aggregated_z,
    tidy_meta(meta_results_z),
    pattern = map(meta_results_z)
  ),
  tar_target(
    eggers_results_z,
    run_eggers(meta_results_z),
    pattern = map(meta_results_z)
  ),
  tar_target(
    excess_sig_results_z,
    run_excess_sig(meta_results_z),
    pattern = map(meta_results_z)
  ),
  tar_target(
    studies_results,
    combine_study_results(
      meta_aggregated_r, eggers_results_r, excess_sig_results_r,
      meta_aggregated_z, eggers_results_z, excess_sig_results_z
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
    table_desc_latex,
    make_table_desc_latex(tables_df)
  ),
  tar_target(
    table_rob,
    make_table_rob(tables_df)
  ),
  tar_target(
    table_desc_saved,
    save_table(table_desc_gt, "Review characteristics.pdf"),
    format = "file",
  ),
  tar_target(
    table_rob_saved,
    save_table(table_rob, "Quality assessment table.pdf"),
    format = "file",
  ),
  # Make plots --------------------------------------------------------------
  tar_target(
    prisma,
    make_prisma(prisma_path, effects_clean, reviews_clean)
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
  tar_render(
    manuscript,
    path = here::here("reports", "manuscript.Rmd")
  ),
  tar_render(
    references,
    path = here::here("reports", "references.Rmd"),
    output_file = here::here(
      "supplementary_files",
      "Supplementary File 7 - Included Studies.pdf"
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
