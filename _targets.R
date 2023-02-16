library(targets)
library(tarchetypes)
library(here)

options(tidyverse.quiet = TRUE, clustermq.scheduler = "multiprocess")

packages <- c(
  "base", "broom", "bib2df", "DiagrammeR", "distill", "DT", "esc",
  "emojifont", "ggh4x", "googledrive", "ggplot2", "ggtext", "gt", "janitor",
  "knitr", "kableExtra", "magick", "magrittr", "metafor", "papaja", "targets",
  "tarchetypes", "tidyMB", "tidyverse", "scales", "xfun"
)

tar_option_set(
  packages = packages,
  debug = "yml_base",
  workspace_on_error = TRUE
)

googledrive::drive_auth()
googlesheets4::gs4_auth()
source(here::here("R", "utils.R"))
source(here::here("R", "data.R"))
source(here::here("R", "tables.R"))
source(here::here("R", "plots.R"))
source(here::here("R", "supplementary.R"))
source(here::here("R", "reports.R"))

list(
  # Fetch and analyse (data.R)
  tar_target(
    modified_date,
    get_mod_date(data_sheet),
    # Force run if outdated or doesn't exist
    cue = tar_cue_force(
      condition = ifelse(
        tar_exist_objects("modified_date"),
        get_mod_date(data_sheet) != tar_read(modified_date),
        TRUE
      )
    )
  ),
  tar_target(
    effects_raw,
    read_sheet(data_sheet, "EffectSizesValidation", modified_date)
  ),
  tar_target(
    reviews_raw,
    read_sheet(data_sheet, "ReviewLevelValidation", modified_date)
  ),
  tar_target(
    rob_raw,
    read_sheet(data_sheet, "QualityAssessment", modified_date)
  ),
  tar_target(
    studies_raw,
    read_sheet(data_sheet, "StudyLevel", modified_date)
  ),
  tar_target(
    effects_clean,
    process_effects(effects_raw, reviews_raw)
  ),
  tar_target(
    update_sheet,
    update_gsheet(effects_clean, data_sheet, "StudyLevelHelper")
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
      packages,
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
