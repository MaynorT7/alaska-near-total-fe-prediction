# ==============================================================================
# Model V3 — V2 + multi-hot lithology + rock/sediment ratio + coarse TPI
# ==============================================================================

#-------------------------------------------------------------------------------
# Part 1. Setup --------

# 1) Locate shared configuration and modules

config_candidates <- c(
  file.path(getwd(), "R", "model_config.R"),
  file.path(getwd(), "..", "R", "model_config.R")
)
config_path <- config_candidates[file.exists(config_candidates)][1]
if (is.na(config_path)) stop("Run from project root or Model_V3/.", call. = FALSE)
source(config_path)
source_project_modules()

# 2) Initialise model paths and reproducibility settings

cfg <- initialise_project("Model_V3", resolution_m = 1000L)

#-------------------------------------------------------------------------------
# Part 2. Shared inputs --------

# 3) Load the common cohort, fixed folds and V3 predictor stack

inputs <- load_version_inputs("V3", cfg)

#-------------------------------------------------------------------------------
# Part 3. Primary V3 model --------

# 4) Fit, validate and map the fixed V3 specification

result <- run_fixed_version_model(
  version = "V3",
  data = inputs$data,
  features = inputs$features,
  prediction_stack = inputs$stack,
  domain_mask = inputs$domain_mask,
  cfg = cfg,
  output_dir = cfg$output_dir
)

#-------------------------------------------------------------------------------
# Part 4. Completion --------

# Selected-model sensitivity analyses are deliberately excluded from the
# candidate-model scripts. They are run only after the V1-V4 spatial-CV
# comparison has identified the selected specification, in
# supplementary_analysis_and_figures.R.

cat("\nV3 complete.\n")
