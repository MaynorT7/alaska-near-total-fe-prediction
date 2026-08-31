# ==============================================================================
# Staged model-input preparation entry point
# ==============================================================================
# The shared V1-V4 core is built once; V2-V4 increments are then cached
# separately. The common comparison cohort and folds are created only after all
# four predictor specifications are available.
# ==============================================================================

#-------------------------------------------------------------------------------
# Part 1. Setup --------

# 1) Locate and source the shared project configuration

config_candidates <- c(
  file.path(getwd(), "R", "model_config.R"),
  file.path(getwd(), "..", "R", "model_config.R")
)
config_path <- config_candidates[file.exists(config_candidates)][1]
if (is.na(config_path)) {
  stop("Run preprocess_all.R from the project root or a model directory.", call. = FALSE)
}
source(config_path)
source_project_modules()

# 2) Initialise the shared 1-km preprocessing configuration

cfg <- initialise_project("shared", resolution_m = 1000L)

# Route disk-backed terra temporary files to a configurable directory.
# The R-session temporary directory is used by default. Set ALASKA_TERRA_TEMP
# externally only when a persistent or larger-capacity location is required.
terra_temp <- Sys.getenv(
  "ALASKA_TERRA_TEMP",
  unset = file.path(tempdir(), "terra")
)
dir.create(terra_temp, recursive = TRUE, showWarnings = FALSE)
terra::terraOptions(
  tempdir = terra_temp,
  memfrac = 0.3,
  memmax = 2,
  todisk = TRUE,
  progress = 1
)
Sys.setenv(CPL_TMPDIR = terra_temp)

#-------------------------------------------------------------------------------
# Part 2. Rebuild policy --------

# 3) Resolve optional force controls
# Default: reuse the required outputs when they exist and are non-empty. Force
# a rebuild after changing raw data or preprocessing code.

force_from_command <- "--force" %in% commandArgs(trailingOnly = TRUE)
force_from_option <- isTRUE(getOption("alaska.force_preprocess", FALSE))
force_from_environment <- tolower(Sys.getenv("FORCE_PREPROCESS", "false")) %in%
  c("1", "true", "yes")
force <- force_from_command || force_from_option || force_from_environment

#-------------------------------------------------------------------------------
# Part 3. Execute staged input preparation --------

# 4) Reuse or rebuild staged predictors, then lock the common cohort and folds

paths <- prepare_all_model_inputs(cfg, force = force)

cat("\nStaged V1-V4 model inputs and shared folds are ready.\n")
cat("Processed directory:", cfg$processed_dir, "\n")
