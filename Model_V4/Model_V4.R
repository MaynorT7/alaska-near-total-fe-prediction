# ==============================================================================
# Model V4 — V3 + concise 3DEP terrain and 3DHP drainage covariates
# ==============================================================================
# V4 adds mean elevation, mean slope, 1-km TPI, local elevation heterogeneity
# and distance to mapped drainage. Redundant and flow-conditioned derivatives
# are intentionally excluded.
# ==============================================================================

#-------------------------------------------------------------------------------
# Part 1. Setup --------

# 1) Locate shared configuration and modules

config_candidates <- c(
  file.path(getwd(), "R", "model_config.R"),
  file.path(getwd(), "..", "R", "model_config.R")
)
config_path <- config_candidates[file.exists(config_candidates)][1]
if (is.na(config_path)) stop("Run from project root or Model_V4/.", call. = FALSE)
source(config_path)
source_project_modules()

# 2) Initialise model paths and reproducibility settings

cfg <- initialise_project("Model_V4", resolution_m = 1000L)

#-------------------------------------------------------------------------------
# Part 2. Shared inputs --------

# 3) Load the common cohort, fixed folds and V4 predictor stack

inputs <- load_version_inputs("V4", cfg)

#-------------------------------------------------------------------------------
# Part 3. Primary V4 model --------

# 4) Fit, validate and map the fixed V4 specification

result <- run_fixed_version_model(
  version = "V4",
  data = inputs$data,
  features = inputs$features,
  prediction_stack = inputs$stack,
  domain_mask = inputs$domain_mask,
  cfg = cfg,
  output_dir = cfg$output_dir
)

cat("\nV4 complete.\n")
