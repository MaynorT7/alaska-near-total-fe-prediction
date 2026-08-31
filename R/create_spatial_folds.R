# ==============================================================================
# Shared cross-validation folds
# ==============================================================================
# Spatial, grid-grouped random and observation-level random folds are generated
# once for the common comparison cohort, saved by sample_id, and reused by V1-V4.
# ==============================================================================

shared_fold_columns <- c(
  "spatial_fold", "random_fold", "observation_random_fold"
)

#-------------------------------------------------------------------------------
# Part 1. Fold-input validation --------

# 1) Require unique record identifiers

validate_unique_sample_ids <- function(data) {
  assert_required_columns(data, "sample_id", "fold data")
  ids <- trimws(as.character(data$sample_id))
  if (anyNA(ids) || any(!nzchar(ids))) {
    stop("sample_id contains missing or blank values.", call. = FALSE)
  }
  if (anyDuplicated(ids)) {
    stop("sample_id must be unique before fold assignment.", call. = FALSE)
  }
  invisible(TRUE)
}

validate_fold_indices <- function(folds, n) {
  for (index in seq_along(folds)) {
    train <- folds[[index]][[1]]
    test <- folds[[index]][[2]]
    if (
      length(train) == 0L ||
      length(test) == 0L ||
      length(intersect(train, test)) > 0L ||
      any(c(train, test) < 1L) ||
      any(c(train, test) > n)
    ) {
      stop("Invalid train/test indices in fold ", index, ".", call. = FALSE)
    }
  }
  invisible(TRUE)
}

fold_assignment <- function(folds, n) {
  assignment <- rep(NA_integer_, n)
  for (index in seq_along(folds)) {
    assignment[folds[[index]][[2]]] <- index
  }
  if (anyNA(assignment)) stop("Fold assignment is incomplete.", call. = FALSE)
  assignment
}

#-------------------------------------------------------------------------------
# Part 2. Grid-grouped random folds --------

# 2) Balance complete 1-km prediction cells across grid-grouped random folds

create_grouped_random_fold_ids <- function(group_id, k = 5L, seed = 40L) {
  group_id <- trimws(as.character(group_id))
  if (anyNA(group_id) || any(!nzchar(group_id))) {
    stop("Random-CV grid IDs contain missing or blank values.", call. = FALSE)
  }
  groups <- as.data.frame(table(group_id), stringsAsFactors = FALSE)
  names(groups) <- c("grid_id", "n")
  if (nrow(groups) < k) stop("Fewer grid cells than folds.", call. = FALSE)

  # Assign larger groups first and use seeded random values only to break ties.
  set.seed(seed)
  groups$tie <- stats::runif(nrow(groups))
  groups <- groups[order(-groups$n, groups$tie), ]
  fold_load <- integer(k)
  groups$fold <- NA_integer_
  for (index in seq_len(nrow(groups))) {
    candidates <- which(fold_load == min(fold_load))
    selected <- candidates[[sample.int(length(candidates), 1L)]]
    groups$fold[index] <- selected
    fold_load[selected] <- fold_load[selected] + groups$n[index]
  }
  as.integer(groups$fold[match(group_id, groups$grid_id)])
}

# 3) Balance individual observations across conventional random folds

create_observation_random_fold_ids <- function(sample_id, k = 5L, seed = 40L) {
  ids <- trimws(as.character(sample_id))
  k <- as.integer(k)
  if (length(k) != 1L || is.na(k) || k < 2L) {
    stop("k must be a single integer of at least 2.", call. = FALSE)
  }
  if (anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) {
    stop("Observation-level random CV requires unique, non-blank sample IDs.",
         call. = FALSE)
  }
  n <- length(ids)
  if (n < k) stop("Fewer observations than folds.", call. = FALSE)

  # Sort by ID before seeded allocation so row ordering cannot change the folds.
  stable_order <- order(ids, method = "radix")
  balanced_folds <- rep(seq_len(k), length.out = n)
  set.seed(as.integer(seed))
  assigned_in_stable_order <- sample(
    balanced_folds, size = n, replace = FALSE
  )
  assignment <- integer(n)
  assignment[stable_order] <- assigned_in_stable_order
  assignment
}

#-------------------------------------------------------------------------------
# Part 3. Spatially blocked folds --------

# 4) Assign occupied 1-km cell centres to 50-km hexagonal blocks

create_spatial_fold_ids <- function(data,
                                    k = 5L,
                                    block_size_m = 50000,
                                    iterations = 100L,
                                    seed = 40L,
                                    projected_crs = 3338) {
  assert_required_columns(
    data,
    c("sample_id", "grid_id", "grid_x", "grid_y"),
    "spatial CV data"
  )
  validate_unique_sample_ids(data)
  grid_units <- data |>
    dplyr::select(grid_id, grid_x, grid_y) |>
    dplyr::distinct()
  if (anyDuplicated(grid_units$grid_id)) {
    stop("A grid_id has more than one raster-cell centre.", call. = FALSE)
  }
  if (nrow(grid_units) < k) stop("Fewer occupied cells than folds.", call. = FALSE)

  points <- sf::st_as_sf(
    grid_units,
    coords = c("grid_x", "grid_y"),
    crs = projected_crs,
    remove = FALSE
  )
  # column=NULL is required because Fe is a continuous response.
  arguments <- list(
    x = points,
    column = NULL,
    k = as.integer(k),
    size = block_size_m,
    hexagon = TRUE,
    selection = "random",
    iteration = as.integer(iterations),
    balance = TRUE,
    presence_bg = FALSE,
    seed = as.integer(seed),
    plot = FALSE,
    report = FALSE,
    progress = FALSE
  )
  supported <- names(formals(blockCV::cv_spatial))
  required <- c("x", "column", "k", "size", "hexagon")
  if (length(setdiff(required, supported)) > 0L) {
    stop("Installed blockCV is incompatible with this fold design.", call. = FALSE)
  }
  arguments <- arguments[names(arguments) %in% supported]

  set.seed(seed)
  result <- do.call(blockCV::cv_spatial, arguments)
  folds <- result$folds_list
  if (is.null(folds) || length(folds) != k) {
    stop("blockCV did not return the requested folds.", call. = FALSE)
  }
  validate_fold_indices(folds, nrow(grid_units))
  grid_fold <- fold_assignment(folds, nrow(grid_units))
  as.integer(grid_fold[match(data$grid_id, grid_units$grid_id)])
}

# 5) Generate and save all three shared designs

create_and_save_shared_folds <- function(data,
                                         output_path,
                                         metadata_path = sub("\\.rds$", "_metadata.rds", output_path),
                                         k = 5L,
                                         block_size_m = 50000,
                                         iterations = 100L,
                                         seed = 40L,
                                         projected_crs = 3338,
                                         resolution_m = 1000L) {
  validate_unique_sample_ids(data)
  spatial_fold <- create_spatial_fold_ids(
    data, k, block_size_m, iterations, seed, projected_crs
  )
  assignment <- data.frame(
    sample_id = as.character(data$sample_id),
    spatial_fold = spatial_fold,
    random_fold = create_grouped_random_fold_ids(data$grid_id, k, seed),
    observation_random_fold = create_observation_random_fold_ids(
      data$sample_id, k, seed
    ),
    stringsAsFactors = FALSE
  )
  for (column in shared_fold_columns) {
    if (
      anyNA(assignment[[column]]) ||
      !setequal(unique(assignment[[column]]), seq_len(k))
    ) {
      stop(column, " is incomplete.", call. = FALSE)
    }
  }

  # Cells remain indivisible in the spatial and grid-grouped designs; 
  # observation-level random CV permits same-cell observations 
  # to enter different folds and serves as a conventional, potentially optimistic benchmark.
  check <- data.frame(
    grid_id = data$grid_id,
    spatial_fold = assignment$spatial_fold,
    random_fold = assignment$random_fold
  ) |>
    dplyr::group_by(grid_id) |>
    dplyr::summarise(
      n_spatial = dplyr::n_distinct(spatial_fold),
      n_random = dplyr::n_distinct(random_fold),
      .groups = "drop"
    )
  if (any(check$n_spatial != 1L) || any(check$n_random != 1L)) {
    stop("A 1-km prediction cell was split across folds.", call. = FALSE)
  }

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(assignment, output_path)
  readr::write_csv(assignment, sub("\\.rds$", ".csv", output_path))
  metadata <- list(
    sample_ids = sort(as.character(data$sample_id)),
    k = as.integer(k),
    block_size_m = as.numeric(block_size_m),
    iterations = as.integer(iterations),
    seed = as.integer(seed),
    projected_crs = as.integer(projected_crs),
    resolution_m = as.integer(resolution_m),
    fold_columns = shared_fold_columns
  )
  saveRDS(metadata, metadata_path)
  invisible(assignment)
}

# 6) Validate the cohort and settings associated with a saved fold design

validate_saved_fold_metadata <- function(data,
                                         metadata_path,
                                         k,
                                         block_size_m,
                                         iterations,
                                         seed,
                                         projected_crs,
                                         resolution_m) {
  assert_files_exist(metadata_path, "Shared fold metadata")
  validate_unique_sample_ids(data)
  metadata <- readRDS(metadata_path)
  required <- c(
    "sample_ids", "k", "block_size_m", "iterations", "seed",
    "projected_crs", "resolution_m", "fold_columns"
  )
  if (!is.list(metadata) || !all(required %in% names(metadata))) {
    stop("Shared fold metadata are incomplete.", call. = FALSE)
  }
  expected_ids <- sort(as.character(data$sample_id))
  if (!identical(sort(as.character(metadata$sample_ids)), expected_ids)) {
    stop("Saved folds do not match the final comparison cohort.", call. = FALSE)
  }
  expected <- list(
    k = as.integer(k),
    block_size_m = as.numeric(block_size_m),
    iterations = as.integer(iterations),
    seed = as.integer(seed),
    projected_crs = as.integer(projected_crs),
    resolution_m = as.integer(resolution_m),
    fold_columns = shared_fold_columns
  )
  for (field in names(expected)) {
    if (!identical(metadata[[field]], expected[[field]])) {
      stop("Saved fold setting differs for ", field, ".", call. = FALSE)
    }
  }
  invisible(TRUE)
}

# 7) Attach the saved assignments

attach_shared_folds <- function(data, fold_path, k = NULL) {
  assert_files_exist(fold_path, "Shared fold assignment")
  validate_unique_sample_ids(data)
  assignment <- readRDS(fold_path)
  assert_required_columns(
    assignment,
    c("sample_id", shared_fold_columns),
    "fold assignment"
  )
  validate_unique_sample_ids(assignment)
  if (nrow(assignment) != nrow(data) ||
      !setequal(as.character(assignment$sample_id), as.character(data$sample_id))) {
    stop("Fold assignment and model cohort contain different sample IDs.",
         call. = FALSE)
  }
  if (!is.null(k)) {
    expected_folds <- seq_len(as.integer(k))
    for (column in shared_fold_columns) {
      values <- suppressWarnings(as.numeric(assignment[[column]]))
      if (any(!is.finite(values)) || any(values != as.integer(values)) ||
          !setequal(as.integer(values), expected_folds)) {
        stop(column, " does not contain the required fold IDs.", call. = FALSE)
      }
    }
  }
  joined <- dplyr::left_join(data, assignment, by = "sample_id")
  if (any(vapply(joined[shared_fold_columns], anyNA, logical(1)))) {
    stop("One or more samples lack shared fold assignments.", call. = FALSE)
  }
  if ("grid_id" %in% names(joined)) {
    grid_check <- joined |>
      dplyr::group_by(grid_id) |>
      dplyr::summarise(
        n_spatial = dplyr::n_distinct(spatial_fold),
        n_random = dplyr::n_distinct(random_fold),
        .groups = "drop"
      )
    if (any(grid_check$n_spatial != 1L) || any(grid_check$n_random != 1L)) {
      stop("A saved fold assignment splits a 1-km prediction cell.",
           call. = FALSE)
    }
  }
  joined
}

#-------------------------------------------------------------------------------
# Part 4. Fold conversion and cross-model checks --------

# 8) Convert a saved fold column to ranger train/test indices

folds_from_column <- function(data, fold_column) {
  assert_required_columns(data, fold_column, "folded data")
  if (anyNA(data[[fold_column]])) stop(fold_column, " contains NA.", call. = FALSE)
  folds <- lapply(sort(unique(data[[fold_column]])), function(fold) {
    list(
      which(data[[fold_column]] != fold),
      which(data[[fold_column]] == fold)
    )
  })
  validate_fold_indices(folds, nrow(data))
  folds
}

# 9) Confirm identical cohorts and folds across model outputs

assert_same_comparison_ids <- function(oof_paths) {
  tables <- lapply(oof_paths, function(path) {
    readr::read_csv(
      path,
      col_types = readr::cols(sample_id = readr::col_character()),
      show_col_types = FALSE
    )
  })
  maps <- lapply(tables, function(x) {
    map <- x |>
      dplyr::select(sample_id, dplyr::all_of(shared_fold_columns)) |>
      dplyr::arrange(sample_id)
    map <- as.data.frame(map, stringsAsFactors = FALSE)
    rownames(map) <- NULL
    map
  })
  if (!all(vapply(maps[-1L], identical, logical(1), maps[[1L]]))) {
    stop("V1-V4 do not use identical sample IDs and folds.", call. = FALSE)
  }
  invisible(TRUE)
}
