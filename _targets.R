# Load required functions and packages
lapply(list.files("./R", full.names = TRUE), source)
load_packages()

options(tidyverse.quiet = TRUE, clustermq.scheduler = "multiprocess")

list(
  # Fetch and analyse (data.R)
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
  tar_target(
    reviews_clean,
    clean_reviews(reviews_raw)
  ),
  tar_target(
    effects_clean,
    clean_effects(effects_raw, reviews_clean)
  ),
  tar_target(
    studies_clean,
    clean_studies(studies_raw),
    iteration = "group"
  ),
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
      meta_aggregated_r, eggers_results_r, excess_sig_results_r
    )
  ),
  tar_target(
    combined_effects,
    join_analyses(effects_clean, studies_results)
  ),
  # Make tables (tables.R)
  tar_target(
    tables_df,
    make_table_data(rob_raw, effects_clean, reviews_raw)
  ),
  tar_target(
    reviews_tables,
    make_desc_tables(tables_df)
  ),
  tar_target(
    export_tables,
    save_tables(reviews_tables),
    format = "file",
    pattern = map(reviews_tables)
  ),
  # Make plots (plots.R)
  tar_target(prisma_data,
    here::here("data/covidence_prisma.txt"),
    format = "file"
  ),
  tar_target(
    prisma,
    make_prisma(prisma_data, effects_clean)
  ),
  tar_target(export_prisma,
    save_prisma(prisma),
    format = "file",
  ),
  tar_target(
    plots,
    make_plots(combined_effects)
  ),
  tar_target(
    export_plots,
    save_plots(plots),
    format = "file",
    pattern = map(plots)
  ),
  # Make sundry supplementary material (supplementary.R)
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
  # Create reports (reports.R)
  tar_target(
    packages_bib,
    create_packages_bib(
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
    create_manuscript_info(
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
