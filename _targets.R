library(targets)
library(tarchetypes)
library(here)

options(tidyverse.quiet = TRUE, clustermq.scheduler = "multiprocess")

# Load required functions and packages
lapply(list.files("./R", full.names = TRUE), source)
load_packages()

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
    effects_clean,
    process_effects(effects_raw, reviews_raw)
  ),
  tar_target(studies_converted,
    convert_studies(studies_raw),
    iteration = "group"
  ),
  tar_target(
    meta_results,
    run_metaanalysis(studies_converted),
    pattern = map(studies_converted),
    iteration = "list"
  ),
  tar_target(
    meta_aggregated,
    tidy_meta(meta_results),
    pattern = map(meta_results)
  ),
  tar_target(
    eggers_results,
    run_eggers(meta_results),
    pattern = map(meta_results)
  ),
  tar_target(
    excess_sig_results,
    run_excess_sig(meta_results),
    pattern = map(meta_results)
  ),
  tar_target(
    studies_results,
    combine_study_results(meta_aggregated, eggers_results, excess_sig_results)
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
    here::here("covidence_prisma.txt"),
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
      effects_clean, prisma, tables_df, combined_effects, studies_converted
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
  ),
  tar_target(
    uploaded_manuscripts,
    upload_files(manuscript)
  ),
  tar_target(
    uploaded_references,
    upload_files(references)
  )
)
