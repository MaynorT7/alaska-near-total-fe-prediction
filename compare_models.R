# ==============================================================================
# Paired V1-V4 model comparison
# ==============================================================================
# Comparison proceeds only after confirming identical sample IDs and shared
# spatial/random fold assignments in every model's OOF predictions.
# ==============================================================================

#-------------------------------------------------------------------------------
# Part 1. Setup and validation --------

config_path <- file.path(getwd(), "R", "model_config.R")
if (!file.exists(config_path)) {
  stop("Run compare_models.R from the project root.", call. = FALSE)
}
source(config_path)
source_project_modules()
cfg <- initialise_project("shared", resolution_m = 1000L)
paths <- processed_output_paths(cfg)

versions <- paste0("V", 1:4)
metric_paths <- file.path(
  cfg$root, paste0("Model_V", 1:4), "outputs", "model_metrics.csv"
)
oof_paths <- file.path(
  cfg$root, paste0("Model_V", 1:4), "outputs", "spatial_oof_predictions.csv"
)
assert_files_exist(c(paths$comparison_ids, metric_paths, oof_paths))
assert_same_comparison_ids(oof_paths)

comparison_ids <- sort(as.character(readRDS(paths$comparison_ids)))
for (path in oof_paths) {
  oof_ids <- sort(as.character(
    readr::read_csv(
      path,
      col_types = readr::cols(sample_id = readr::col_character()),
      show_col_types = FALSE
    )$sample_id
  ))
  if (!identical(oof_ids, comparison_ids)) {
    stop("An OOF file differs from the common comparison cohort.", call. = FALSE)
  }
}

#-------------------------------------------------------------------------------
# Part 2. Comparison table --------

comparison <- dplyr::bind_rows(
  lapply(metric_paths, readr::read_csv, show_col_types = FALSE)
) |>
  dplyr::mutate(model = factor(model, levels = versions)) |>
  dplyr::arrange(model)
readr::write_csv(
  comparison,
  file.path(cfg$root, "model_comparison.csv")
)

print(comparison)
cat("\nValidated common comparison cohort:",
    length(comparison_ids), "samples.\n")
