# ==============================================================================
# Model V1 — parent-material geochemistry + coarse terrain
# ==============================================================================

#-------------------------------------------------------------------------------
# Part 1. Setup --------

# 1) Locate shared configuration and modules

config_candidates <- c(
  file.path(getwd(), "R", "model_config.R"),
  file.path(getwd(), "..", "R", "model_config.R")
)
config_path <- config_candidates[file.exists(config_candidates)][1]
if (is.na(config_path)) stop("Run from project root or Model_V1/.", call. = FALSE)
source(config_path)
source_project_modules()

# 2) Initialise model paths and reproducibility settings

cfg <- initialise_project("Model_V1", resolution_m = 1000L)

#-------------------------------------------------------------------------------
# Part 2. Shared inputs --------

# 3) Load the common cohort, fixed folds and V1 predictor stack

inputs <- load_version_inputs("V1", cfg)

#-------------------------------------------------------------------------------
# Part 3. Primary V1 model --------

# 4) Fit, validate and map the fixed V1 specification

result <- run_fixed_version_model(
  version = "V1",
  data = inputs$data,
  features = inputs$features,
  prediction_stack = inputs$stack,
  domain_mask = inputs$domain_mask,
  cfg = cfg,
  output_dir = cfg$output_dir
)

cat("\nV1 complete. Spatial log-scale R^2:",
    readr::read_csv(result$metrics_path, show_col_types = FALSE)$spatial_log_R2, "\n")
