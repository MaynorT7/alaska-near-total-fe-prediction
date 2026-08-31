# ==============================================================================
# Shared configuration — Alaska near-total Fe spatial prediction
# ==============================================================================
# One source of truth for packages, paths, study domain, reproducibility,
# cross-validation and fixed model features.
# ==============================================================================

#-------------------------------------------------------------------------------
# Part 1. Package setup --------

# 1) Required packages

required_packages <- c(
  "readr", "dplyr", "tidyr", "sf", "ggplot2", "terra", "ranger",
  "blockCV", "RANN"
)

# 2) Optional package installation

install_project_packages <- function(packages = required_packages,
                                     repos = getOption("repos")) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) == 0L) {
    message("All required R packages are already installed.")
    return(invisible(character()))
  }
  if (identical(repos, c(CRAN = "@CRAN@")) || any(repos == "@CRAN@")) {
    repos <- c(CRAN = "https://cloud.r-project.org")
  }
  install.packages(missing, repos = repos, dependencies = TRUE)
  invisible(missing)
}

# 3) Package loading and validation

load_project_packages <- function(packages = required_packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0L) {
    stop(
      "Missing required R package(s): ", paste(missing, collapse = ", "),
      "\nRun install_project_packages() once, then rerun the pipeline.",
      call. = FALSE
    )
  }
  invisible(lapply(packages, function(package) {
    suppressPackageStartupMessages(library(package, character.only = TRUE))
  }))
}

#-------------------------------------------------------------------------------
# Part 2. Project discovery and module loading --------

# 4) Locate the project root

find_project_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(current, "R", "model_config.R")) &&
        all(dir.exists(file.path(current, paste0("Model_V", 1:4))))) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }
  stop("Project root not found. Expected R/ and Model_V1/ ... Model_V4/.",
       call. = FALSE)
}

# 5) Resolve the root from RStudio or Rscript

bootstrap_project_root <- function() {
  script_arg <- grep("^--file=", commandArgs(), value = TRUE)
  script_path <- if (length(script_arg) > 0L) sub("^--file=", "", script_arg[[1]]) else NULL
  start <- if (!is.null(script_path)) {
    dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE))
  } else {
    getwd()
  }
  find_project_root(start)
}

# 6) Source shared modules once

source_project_modules <- function(root = bootstrap_project_root(),
                                   envir = parent.frame()) {
  module_files <- c("data_preprocessing.R", "create_spatial_folds.R", "model_functions.R")
  for (module_file in module_files) {
    module_path <- file.path(root, "R", module_file)
    if (!file.exists(module_path)) stop("Shared R module not found: ", module_path, call. = FALSE)
    source(module_path, local = envir)
  }
  invisible(root)
}

#-------------------------------------------------------------------------------
# Part 3. Input paths and fixed model specifications --------

# 7) Exact project input paths
# Paths are defined once and reused by validation and preprocessing.

project_input_paths <- function(root) {
  data_dir <- file.path(root, "data")
  dem_dir <- file.path(data_dir, "terrain", "dem", "usgs_3dep")
  drainage_dir <- file.path(data_dir, "hydrology", "drainage")

  list(
    v4 = list(
      dem = file.path(dem_dir, "usgs_3dep_dem_60m_epsg3338_buffer25km.tif"),
      channels = file.path(
        drainage_dir,
        "usgs_3dhp_channel_lines_epsg3338_buffer25km.gpkg"
      )
    )
  )
}

# 8) Fixed predictor sets for V1-V4

feature_sets <- local({
  lithology_multihot <- c(
    "Ign_highFe", "Ign_lowFe", "Sed_fine", "Sed_other",
    "Meta_sed", "Meta_ign", "Unconsolidated", "Other_unknown"
  )

  v1 <- c(
    "rock_Fe_final", "sed_Fe_final",
    "rock_support_missing", "sed_support_missing",
    "rock_n_samples", "sed_n_samples",
    "nn_rock_Fe", "nn_sed_Fe",
    "nn_rock_dist_km", "nn_sed_dist_km",
    "elev", "slope", "eastness", "northness"
  )
  v2 <- c(v1, "PF_prob", "MAAT", "MAP")
  v3 <- c(v2, lithology_multihot, "rock_sed_ratio", "TPI")

  # V4 tests one concise increment: fine-resolution terrain structure and
  # proximity to mapped drainage. Redundant aspect, extra-scale TPI, TWI and
  # HAND derivatives are deliberately omitted.
  v4_added <- c(
    "elev_60m_mean", "slope_60m_mean", "TPI_1000m",
    "elev_sd_1km", "dist_drainage_km"
  )

  list(
    lithology_multihot = lithology_multihot,
    V1 = unique(v1),
    V2 = unique(v2),
    V3 = unique(v3),
    V4 = unique(c(v3, v4_added))
  )
})

#-------------------------------------------------------------------------------
# Part 4. Shared project configuration --------

# 9) Initialise paths, seeds and analytical policies

initialise_project <- function(model_version = "shared",
                               seed = 40L,
                               resolution_m = 1000L) {
  load_project_packages()
  set.seed(seed)
  ggplot2::theme_set(
    ggplot2::theme_bw(base_size = 11) +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(face = "bold")
      )
  )

  root <- bootstrap_project_root()
  processed_dir <- file.path(root, "data", "processed")
  dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

  model_dir <- if (identical(model_version, "shared")) root else file.path(root, model_version)
  output_dir <- if (identical(model_version, "shared")) processed_dir else file.path(model_dir, "outputs")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  list(
    root = root,
    model_version = model_version,
    model_dir = model_dir,
    output_dir = output_dir,
    data_dir = file.path(root, "data"),
    processed_dir = processed_dir,
    inputs = project_input_paths(root),
    seed = as.integer(seed),
    resolution_m = as.integer(resolution_m),
    crs = list(geographic = 4326, projected = 3338),
    bbox = list(lon_min = -165, lon_max = -156, lat_min = 60, lat_max = 63),
    daymet = list(start_year = 1981L, end_year = 2010L),
    analysis = list(
      # Soil, sediment and rock use the same accepted near-total/total Fe methods.
      allowed_fe_methods = c("AES_HF", "WDX_FUSE"),
      primary_qaqc_exclusions = c(
        "DUPLICATE", "SITE DUPLICATE", "LAB REPLICATE", "DUPLICATE(?)"
      ),
      coordinate_tolerance_deg = 1e-4,
      # Explicit waste materials are excluded; contextual mentions are audited.
      exclude_anthropogenic_waste = TRUE,
      anthropogenic_waste_pattern = paste(
        c("tailings?", "treated tailings?", "mine dump", "mine waste",
          "waste rock", "ore dump", "mill waste", "slag"),
        collapse = "|"
      ),
      # Eight soil records were manually confirmed as sampled tailings after
      # reviewing the contextual AGDB4 fields. The two identifiers form
      # record-level pairs and must never be matched independently.
      confirmed_waste_ids = list(
        soil = data.frame(
          AGDB_ID = as.character(c(38521:38525, 400759:400761)),
          OBJECTID = as.character(c(1800:1804, 7840:7842)),
          stringsAsFactors = FALSE
        ),
        sediment = data.frame(
          AGDB_ID = character(),
          OBJECTID = character(),
          stringsAsFactors = FALSE
        ),
        rock = data.frame(
          AGDB_ID = character(),
          OBJECTID = character(),
          stringsAsFactors = FALSE
        )
      ),
      # Structural absence of local auxiliary support is encoded, not dropped.
      structural_support_fill_value = 0,
      # Pastick values outside 0-100 are mask codes, not probabilities.
      permafrost_valid_range = c(0, 100),
      # Operational depth classes are used for audit and sensitivity only.
      depth_breaks_cm = c(shallow_upper = 25, intermediate_upper = 75)
    ),
    cv = list(k = 5L, block_size_m = 50000, iterations = 100L),
    hydrology = list(context_buffer_m = 25000L),
    ranger = list(
      num.trees = 500L,
      min.node.size = 5L,
      importance = "permutation",
      respect.unordered.factors = "partition",
      num.threads = 1L
    ),
    features = feature_sets
  )
}

#-------------------------------------------------------------------------------
# Part 5. File and processed-output validation --------

# 10) Required-file checks

assert_files_exist <- function(paths, label = "Required input") {
  paths <- as.character(paths)
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0L) {
    stop(label, " file(s) not found:\n  ", paste(missing, collapse = "\n  "), call. = FALSE)
  }
  invisible(paths)
}

# 11) Exact filename lookup below data/

find_data_file <- function(data_dir, filename, required = TRUE) {
  if (!dir.exists(data_dir)) stop("Data directory does not exist: ", data_dir, call. = FALSE)
  all_files <- list.files(data_dir, recursive = TRUE, full.names = TRUE, include.dirs = FALSE)
  matches <- all_files[basename(all_files) == filename]
  if (length(matches) == 0L) {
    if (required) stop("Cannot find exact input filename below data/: ", filename, call. = FALSE)
    return(NA_character_)
  }
  if (length(matches) > 1L) {
    stop("Multiple files have exact filename '", filename, "':\n  ",
         paste(matches, collapse = "\n  "), call. = FALSE)
  }
  normalizePath(matches[[1]], winslash = "/", mustWork = TRUE)
}

# 12) Deterministic paths for staged processed outputs

processed_output_paths <- function(cfg) {
  resolution_suffix <- paste0(cfg$resolution_m, "m")
  list(
    # Internal recovery point: cleaned sf layers and their completed audits.
    clean_layers = file.path(cfg$processed_dir, "clean_fe_layers.rds"),
    clean_marker = file.path(cfg$processed_dir, "clean_fe_layers_complete.txt"),

    # Shared V1-V4 core: response records, grid, domain and V1 predictors.
    shared_core_tables = file.path(cfg$processed_dir, "shared_core_tables.rds"),
    unit_code = file.path(
      cfg$processed_dir, paste0("geology_NSACLASS_code_", resolution_suffix, ".tif")
    ),
    v1_stack = file.path(
      cfg$processed_dir, paste0("predictors_v1_", resolution_suffix, ".tif")
    ),
    shared_core_marker = file.path(cfg$processed_dir, "shared_core_complete.txt"),

    # Version-specific additions are cached independently of the shared core.
    v2_increment = file.path(
      cfg$processed_dir, paste0("predictors_v2_increment_", resolution_suffix, ".tif")
    ),
    v2_marker = file.path(cfg$processed_dir, "v2_increment_complete.txt"),
    v3_increment = file.path(
      cfg$processed_dir, paste0("predictors_v3_increment_", resolution_suffix, ".tif")
    ),
    v3_marker = file.path(cfg$processed_dir, "v3_increment_complete.txt"),
    v4_increment = file.path(
      cfg$processed_dir, paste0("predictors_v4_increment_", resolution_suffix, ".tif")
    ),
    v4_marker = file.path(cfg$processed_dir, "v4_increment_complete.txt"),

    # Final paired-comparison inputs. Existing names remain stable for V1-V4.
    master_table = file.path(cfg$processed_dir, "master_table.rds"),
    comparison_ids = file.path(cfg$processed_dir, "comparison_sample_ids.rds"),
    folds = file.path(cfg$processed_dir, "spatial_folds.rds"),
    fold_metadata = file.path(cfg$processed_dir, "spatial_folds_metadata.rds"),
    completion_marker = file.path(
      cfg$processed_dir,
      "preprocess_complete.txt"
    ),
    domain_mask = file.path(
      cfg$processed_dir,
      paste0("domain_mask_", resolution_suffix, ".tif")
    ),
    predictor_stack = file.path(
      cfg$processed_dir,
      paste0("predictor_stack_", resolution_suffix, ".tif")
    )
  )
}

# A marker is trusted only when all artefacts in its stage are non-empty.
stage_output_paths <- function(paths, stage) {
  switch(
    stage,
    clean = c(paths$clean_layers, paths$clean_marker),
    shared_core = c(
      paths$shared_core_tables, paths$unit_code, paths$domain_mask,
      paths$v1_stack, paths$shared_core_marker
    ),
    v2 = c(paths$v2_increment, paths$v2_marker),
    v3 = c(paths$v3_increment, paths$v3_marker),
    v4 = c(paths$v4_increment, paths$v4_marker),
    final = c(
      paths$master_table, paths$comparison_ids, paths$folds,
      paths$fold_metadata, paths$completion_marker, paths$domain_mask,
      paths$predictor_stack
    ),
    stop("Unknown processed-output stage: ", stage, call. = FALSE)
  )
}

stage_outputs_exist <- function(cfg, stage) {
  paths <- processed_output_paths(cfg)
  required <- stage_output_paths(paths, stage)
  all(file.exists(required)) &&
    all(is.finite(file.info(required)$size)) &&
    all(file.info(required)$size > 0)
}

# Backward-compatible final-output check used by existing callers.
processed_outputs_exist <- function(cfg) stage_outputs_exist(cfg, "final")

#-------------------------------------------------------------------------------
# Part 6. Reproducibility records --------

# 13) Package and R versions

record_package_versions <- function(path, extra = list()) {
  versions <- data.frame(
    package = required_packages,
    version = vapply(required_packages, function(x) {
      if (requireNamespace(x, quietly = TRUE)) as.character(utils::packageVersion(x)) else NA_character_
    }, character(1)),
    stringsAsFactors = FALSE
  )
  if (length(extra) > 0L) {
    versions <- dplyr::bind_rows(
      versions,
      data.frame(package = names(extra), version = unlist(extra), stringsAsFactors = FALSE)
    )
  }
  readr::write_csv(versions, path)
  invisible(versions)
}
