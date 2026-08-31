# ==============================================================================
# Model V2 — V1 + 1981–2010 climate normals + permafrost probability
# ==============================================================================

#-------------------------------------------------------------------------------
# Part 1. Setup --------

# 1) Locate shared configuration and modules

config_candidates <- c(
  file.path(getwd(), "R", "model_config.R"),
  file.path(getwd(), "..", "R", "model_config.R")
)
config_path <- config_candidates[file.exists(config_candidates)][1]
if (is.na(config_path)) stop("Run from project root or Model_V2/.", call. = FALSE)
source(config_path)
source_project_modules()

# 2) Initialise model paths and reproducibility settings

cfg <- initialise_project("Model_V2", resolution_m = 1000L)

#-------------------------------------------------------------------------------
# Part 2. Shared inputs --------

# 3) Load the common cohort, fixed folds and V2 predictor stack

inputs <- load_version_inputs("V2", cfg)

#-------------------------------------------------------------------------------
# Part 3. Primary V2 model --------

# 4) Fit, validate and map the fixed V2 specification

result <- run_fixed_version_model(
  version = "V2",
  data = inputs$data,
  features = inputs$features,
  prediction_stack = inputs$stack,
  domain_mask = inputs$domain_mask,
  cfg = cfg,
  output_dir = cfg$output_dir
)

#-------------------------------------------------------------------------------
# Part 4. Cohort context --------

# 5) Summarise near-surface permafrost probability at retained soil locations

pf_values <- inputs$data$PF_prob
pf_values <- pf_values[is.finite(pf_values)]
if (length(pf_values) == 0L) {
  stop("No valid permafrost probabilities are available in the V2 cohort.", call. = FALSE)
}
pf_pct <- if (max(pf_values) <= 1.5) pf_values * 100 else pf_values
permafrost_context <- data.frame(
  retained_observations = length(pf_pct),
  median_permafrost_probability_pct = stats::median(pf_pct),
  observations_above_50_pct = mean(pf_pct > 50) * 100
)
readr::write_csv(
  permafrost_context,
  file.path(cfg$output_dir, "permafrost_context_summary.csv")
)
permafrost_statement <- sprintf(
  paste0(
    "At retained soil locations, the median near-surface permafrost probability ",
    "was %.1f%% and %.1f%% of observations exceeded 50%%. The study area is ",
    "therefore described as a permafrost-affected landscape; the sampled ",
    "materials are not collectively described as permafrost soils."
  ),
  permafrost_context$median_permafrost_probability_pct,
  permafrost_context$observations_above_50_pct
)
writeLines(permafrost_statement, file.path(cfg$output_dir, "permafrost_context_statement.txt"))
cat("\n", permafrost_statement, "\n", sep = "")

#-------------------------------------------------------------------------------
# Sensitivity calls are deliberately excluded from candidate-model scripts.
# Selected-model sensitivity analyses are generated only after the V1-V4
# comparison, in supplementary_analysis_and_figures.R.

cat("\nV2 complete.\n")
