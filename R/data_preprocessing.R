# ==============================================================================
# Staged preprocessing and spatial covariate construction
# ==============================================================================
# Shared V1-V4 inputs are built once; V2-V4 additions are cached 
# by version. The final common cohort and folds are assembled
# only after every candidate version is available.
# ==============================================================================

#-------------------------------------------------------------------------------
# Part 1. General validation and I/O helpers --------

# 1) Generic helpers

# Fail early when an input table lacks columns required by downstream operations.
assert_required_columns <- function(data, required, object_name = "data") {
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop(object_name, " is missing required column(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

# Validate, selectively repair and standardise an sf polygon layer.
prepare_polygon_sf <- function(x, object_name = "polygon layer") {
  if (!inherits(x, "sf")) stop(object_name, " is not an sf object.", call. = FALSE)
  if (is.na(sf::st_crs(x))) stop(object_name, " has no CRS.", call. = FALSE)
  if (nrow(x) == 0L) stop(object_name, " contains no features.", call. = FALSE)

  x <- x[!sf::st_is_empty(x), , drop = FALSE]
  if (nrow(x) == 0L) {
    stop(object_name, " contains no non-empty geometries.", call. = FALSE)
  }

  # Preserve valid source geometry and repair only features that require it.
  valid <- suppressWarnings(sf::st_is_valid(x, NA_on_exception = TRUE))
  repair <- is.na(valid) | !valid
  if (any(repair)) {
    message("Repairing ", sum(repair), " invalid ", object_name, " geometries.")
    geometry <- sf::st_geometry(x)
    geometry[repair] <- suppressWarnings(sf::st_make_valid(geometry[repair]))
    sf::st_geometry(x) <- geometry
  }

  geometry_types <- as.character(sf::st_geometry_type(x, by_geometry = TRUE))
  if (any(geometry_types == "GEOMETRYCOLLECTION")) {
    x <- suppressWarnings(sf::st_collection_extract(x, "POLYGON"))
    x <- x[!sf::st_is_empty(x), , drop = FALSE]
    geometry_types <- as.character(sf::st_geometry_type(x, by_geometry = TRUE))
  }
  if (nrow(x) == 0L) stop(object_name, " has no valid polygons.", call. = FALSE)
  invalid_types <- setdiff(unique(geometry_types), c("POLYGON", "MULTIPOLYGON"))
  if (length(invalid_types) > 0L) {
    stop(
      object_name, " contains unsupported geometry type(s): ",
      paste(invalid_types, collapse = ", "),
      call. = FALSE
    )
  }
  x
}

# Locate exactly one named subdirectory below data/ and return its normalised path.
find_data_directory <- function(data_dir, directory_name) {
  candidates <- list.dirs(data_dir, recursive = TRUE, full.names = TRUE)
  matches <- candidates[basename(candidates) == directory_name]
  if (length(matches) == 0L) stop("Cannot find directory below data/: ", directory_name, call. = FALSE)
  if (length(matches) > 1L) {
    stop("Multiple matching directories found for ", directory_name, ":\n  ",
         paste(matches, collapse = "\n  "), call. = FALSE)
  }
  normalizePath(matches[[1]], winslash = "/", mustWork = TRUE)
}

# Apply log10 only when all observed values lie within its mathematical domain.
safe_log10 <- function(x, variable_name = "value") {
  if (any(!is.na(x) & x <= 0)) {
    stop(variable_name, " contains non-positive values; log10 is undefined.", call. = FALSE)
  }
  log10(x)
}

# Standardise categorical code fields for reproducible matching and filtering.
normalise_code_text <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  toupper(gsub("[[:space:]]+", " ", x))
}

# Parse supported collection-date formats without replacing successful matches.
parse_collection_date <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  formats <- c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%Y/%m/%d", "%m-%d-%Y", "%d-%b-%Y")
  out <- as.Date(rep(NA_character_, length(x)))
  for (format in formats) {
    unresolved <- is.na(out) & !is.na(x)
    if (!any(unresolved)) break
    parsed <- as.Date(x[unresolved], format = format)
    out[which(unresolved)[!is.na(parsed)]] <- parsed[!is.na(parsed)]
  }
  out
}

# Write a raster with overwrite enabled and an optional explicit storage datatype.
safe_write_raster <- function(x, path, datatype = NULL) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  arguments <- list(x = x, filename = path, overwrite = TRUE)
  if (!is.null(datatype)) arguments$datatype <- datatype
  do.call(terra::writeRaster, arguments)
  invisible(path)
}

# Reopen and validate a cached raster before it is reused. Completion markers
# prevent partial writes from being mistaken for valid stage outputs.
read_valid_raster <- function(path, expected_names, template = NULL) {
  if (!file.exists(path) || !is.finite(file.info(path)$size) ||
      file.info(path)$size <= 0) {
    return(NULL)
  }
  raster <- tryCatch(terra::rast(path), error = function(e) NULL)
  if (is.null(raster) || terra::nlyr(raster) != length(expected_names)) {
    return(NULL)
  }
  # GDAL rasterize does not persist an R layer name for a single output band.
  # The deterministic file path and geometry validate the artefact; restore its
  # declared analytical name before downstream lookup operations.
  if (terra::nlyr(raster) == 1L) names(raster) <- expected_names
  if (!identical(names(raster), expected_names)) return(NULL)
  if (!is.null(template) && !isTRUE(terra::compareGeom(
    raster, template, stopOnError = FALSE
  ))) {
    return(NULL)
  }
  raster
}

write_stage_marker <- function(path, stage) {
  writeLines(
    c(
      paste("Stage:", stage),
      paste("Completed:", format(Sys.time(), tz = "UTC", usetz = TRUE))
    ),
    path
  )
  invisible(path)
}

# Preserve the expected CSV schema when an audit or review table contains no rows.
write_empty_safe_csv <- function(data, path, columns) {
  if (nrow(data) == 0L) {
    data <- as.data.frame(setNames(replicate(length(columns), character(), simplify = FALSE), columns))
  }
  readr::write_csv(data, path)
  invisible(path)
}

#-------------------------------------------------------------------------------
# Part 2. AGDB4 schema, metadata and record integrity --------

# 2) Identifier and audit-field definitions

# Record-level unique keys — one row per key is expected.
source_identifier_columns <- c("OBJECTID", "AGDB_ID")

# Sample and laboratory identifiers legitimately repeat when one physical
# sample has multiple analyses (different method or laboratory).
# Retained for audit, but not used as uniqueness keys.
source_context_columns <- c("SPL_ID", "LAB_ID")

fe_audit_columns <- c(
  "OBJECTID", "AGDB_ID", "SPL_ID", "FIELD_ID", "LAB_ID", "JOB_ID",
  "PROJECT_NAME", "PUBL_ID", "AGENCY", "x", "y", "LATITUDE", "LONGITUDE",
  "QUAD", "DEPTH", "HORIZON", "SAMPLE_ZONE", "DATE_COLLECT", "SAMPLE_SOURCE",
  "METHOD_COLLECTED", "PRIMARY_CLASS", "SECONDARY_CLASS", "SPECIFIC_NAME",
  "Fe_pct", "Fe_AM", "QAQC_TYPE", "PREP", "MINE_NAME", "DEPOSIT_NAME",
  "SAMPLE_COMMENT", "SAMPLE_COMMENT_LONG", "ADDL_ATTR"
)

optional_character <- function(data, column) {
  if (column %in% names(data)) trimws(as.character(data[[column]])) else rep(NA_character_, nrow(data))
}

resolve_source_record_duplicates <- function(raw, layer_name, audit_dir, prefix) {
  # Validate only rows that actually share a source identifier. This avoids
  # constructing full-table character signatures, which can exhaust RAM for
  # large, wide AGDB4 sediment and rock files.
  comparison_columns <- setdiff(names(raw), "source_row_id")
  rows_to_remove <- rep(FALSE, nrow(raw))
  removal_entries <- list()

  for (id_column in source_identifier_columns) {
    ids <- trimws(as.character(raw[[id_column]]))
    ids[ids == ""] <- NA_character_
    repeated <- !is.na(ids) & (duplicated(ids) | duplicated(ids, fromLast = TRUE))
    if (!any(repeated)) next

    groups <- split(which(repeated), ids[repeated], drop = TRUE)
    conflicts <- character()
    for (current_id in names(groups)) {
      group_index <- groups[[current_id]]
      # A repeated source identifier is removable only when every retained
      # source field agrees. Any disagreement requires manual resolution.
      if (nrow(unique(raw[group_index, comparison_columns, drop = FALSE])) > 1L) {
        conflicts <- c(conflicts, current_id)
        next
      }
      ordered_index <- group_index[order(raw$source_row_id[group_index])]
      remove_index <- ordered_index[-1L]
      remove_index <- remove_index[!rows_to_remove[remove_index]]
      if (length(remove_index) > 0L) {
        removal_entries[[length(removal_entries) + 1L]] <- dplyr::mutate(
          raw[remove_index, , drop = FALSE],
          duplicate_id_column = id_column,
          duplicate_id_value = current_id,
          kept_source_row_id = raw$source_row_id[ordered_index[[1L]]]
        )
        rows_to_remove[remove_index] <- TRUE
      }
    }
    if (length(conflicts) > 0L) {
      readr::write_csv(
        data.frame(
        id_column = id_column,
          id_value = conflicts,
          stringsAsFactors = FALSE
        ),
        file.path(audit_dir, paste0(prefix, id_column, "_conflicts.csv"))
      )
      stop(
        layer_name, " contains conflicting records for ", id_column,
        "; resolve the saved audit before modelling.", call. = FALSE
      )
    }
  }

  removal_log <- if (length(removal_entries) > 0L) {
    dplyr::bind_rows(removal_entries)
  } else {
    raw[FALSE, , drop = FALSE]
  }
  write_empty_safe_csv(
    removal_log,
    file.path(audit_dir, paste0(prefix, "duplicate_removal_log.csv")),
    c(names(raw), "duplicate_id_column", "duplicate_id_value", "kept_source_row_id")
  )

  raw[!rows_to_remove, , drop = FALSE]
}

# Project-derived coordinate key, not an AGDB4 field-site identifier.
make_site_id <- function(lon, lat) {
  ifelse(
    is.na(lon) | is.na(lat),
    NA_character_,
    paste0(
      "SITE_", formatC(lat, format = "f", digits = 6), "_",
      formatC(lon, format = "f", digits = 6)
    )
  )
}

make_support_id <- function(site_id, depth, horizon) {
  depth_key <- ifelse(is.na(depth) | !nzchar(depth), "DEPTH_UNSPECIFIED", depth)
  horizon_key <- ifelse(is.na(horizon) | !nzchar(horizon), "HORIZON_UNSPECIFIED", horizon)
  ifelse(is.na(site_id), NA_character_, paste(site_id, depth_key, horizon_key, sep = "__"))
}

# 3) Parse heterogeneous free-text sampling depths for audit and sensitivity

parse_depth_support <- function(x,
                                shallow_upper_cm = 25,
                                intermediate_upper_cm = 75) {
  parse_one <- function(value) {
    raw <- trimws(as.character(value))
    if (is.na(raw) || !nzchar(raw)) {
      return(data.frame(
        depth_parse_status = "missing",
        depth_unit = NA_character_,
        depth_is_interval = NA,
        depth_lower_cm = NA_real_,
        depth_upper_cm = NA_real_,
        depth_midpoint_cm = NA_real_,
        depth_class = NA_character_,
        stringsAsFactors = FALSE
      ))
    }

    text <- tolower(raw)
    text <- gsub("[\u2013\u2014\u2212]", "-", text, perl = TRUE)
    unit <- if (grepl("centimet|\\bcm\\b", text, perl = TRUE)) {
      "cm"
    } else if (grepl("\\b(feet|foot|ft)\\b|'", text, perl = TRUE)) {
      "ft"
    } else if (grepl("\\b(inches|inch|in)\\b|\"", text, perl = TRUE)) {
      "in"
    } else {
      NA_character_
    }

    number_match <- gregexpr("[0-9]+(?:\\.[0-9]+)?", text, perl = TRUE)[[1]]
    values <- if (identical(number_match[[1]], -1L)) {
      numeric()
    } else {
      as.numeric(regmatches(text, list(number_match))[[1]])
    }

    status <- "parsed"
    lower <- upper <- midpoint <- NA_real_
    is_interval <- NA

    if (length(values) == 0L) {
      status <- "no_numeric_value"
    } else if (is.na(unit)) {
      status <- "unit_not_recognised"
    } else if (length(values) > 2L) {
      status <- "too_many_numeric_values"
    } else if (length(values) == 2L && values[[1]] > values[[2]]) {
      status <- "reversed_interval"
      is_interval <- TRUE
    } else {
      factor_to_cm <- switch(unit, cm = 1, `in` = 2.54, ft = 30.48)
      is_interval <- length(values) == 2L
      lower <- values[[1]] * factor_to_cm
      upper <- values[[length(values)]] * factor_to_cm
      midpoint <- if (is_interval) mean(c(lower, upper)) else lower
      status <- if (is_interval) "parsed_interval" else "parsed_single"
    }

    depth_class <- if (!is.finite(midpoint)) {
      NA_character_
    } else if (midpoint < shallow_upper_cm) {
      "shallow"
    } else if (midpoint <= intermediate_upper_cm) {
      "intermediate"
    } else {
      "deep"
    }

    data.frame(
      depth_parse_status = status,
      depth_unit = unit,
      depth_is_interval = is_interval,
      depth_lower_cm = lower,
      depth_upper_cm = upper,
      depth_midpoint_cm = midpoint,
      depth_class = depth_class,
      stringsAsFactors = FALSE
    )
  }

  dplyr::bind_rows(lapply(x, parse_one))
}

anthropogenic_waste_flags <- function(raw, pattern) {
  combine_fields <- function(fields) {
    fields <- intersect(fields, names(raw))
    if (length(fields) == 0L) return(rep("", nrow(raw)))
    apply(
      as.data.frame(lapply(raw[, fields, drop = FALSE], function(x) {
        value <- tolower(trimws(as.character(x)))
        value[is.na(value)] <- ""
        value
      })),
      1L,
      paste,
      collapse = " | "
    )
  }

  # Structured material classes support automatic exclusion. Free-text mentions
  # are review flags only, because they may describe nearby—not sampled—waste.
  structured_text <- combine_fields(c(
    "SAMPLE_SOURCE", "PRIMARY_CLASS", "SECONDARY_CLASS", "SPECIFIC_NAME"
  ))
  note_text <- combine_fields(c(
    "SAMPLE_COMMENT", "SAMPLE_COMMENT_LONG", "ADDL_ATTR",
    "MINE_NAME", "DEPOSIT_NAME"
  ))
  explicit <- grepl(pattern, structured_text, ignore.case = TRUE, perl = TRUE)
  review <- !explicit & grepl(pattern, note_text, ignore.case = TRUE, perl = TRUE)
  list(
    explicit = explicit,
    review = review,
    structured_text = structured_text,
    review_text = note_text
  )
}

confirmed_waste_id_flag <- function(raw, confirmed_ids = NULL) {
  flag <- rep(FALSE, nrow(raw))
  if (is.null(confirmed_ids)) return(flag)

  pairs <- as.data.frame(confirmed_ids, stringsAsFactors = FALSE)
  assert_required_columns(pairs, c("AGDB_ID", "OBJECTID"), "confirmed waste pairs")
  if (nrow(pairs) == 0L) return(flag)
  assert_required_columns(raw, c("AGDB_ID", "OBJECTID"), "AGDB4 source records")

  pairs$AGDB_ID <- trimws(as.character(pairs$AGDB_ID))
  pairs$OBJECTID <- trimws(as.character(pairs$OBJECTID))
  if (
    anyNA(pairs$AGDB_ID) || anyNA(pairs$OBJECTID) ||
    any(!nzchar(pairs$AGDB_ID)) || any(!nzchar(pairs$OBJECTID))
  ) {
    stop("Confirmed waste pairs contain missing or blank identifiers.", call. = FALSE)
  }
  if (anyDuplicated(pairs[c("AGDB_ID", "OBJECTID")])) {
    stop("Confirmed waste pairs contain a duplicated AGDB_ID–OBJECTID pair.",
         call. = FALSE)
  }

  raw_agdb_id <- trimws(as.character(raw$AGDB_ID))
  raw_objectid <- trimws(as.character(raw$OBJECTID))
  raw_agdb_id[is.na(raw_agdb_id)] <- ""
  raw_objectid[is.na(raw_objectid)] <- ""

  # Match the reviewed identifiers jointly. Each configured pair must identify
  # exactly one post-deduplication source record; partial-ID matches are invalid.
  match_matrix <- vapply(
    seq_len(nrow(pairs)),
    function(index) {
      raw_agdb_id == pairs$AGDB_ID[[index]] &
        raw_objectid == pairs$OBJECTID[[index]]
    },
    logical(nrow(raw))
  )
  match_matrix <- matrix(
    match_matrix, nrow = nrow(raw), ncol = nrow(pairs)
  )
  pair_counts <- colSums(match_matrix)
  if (any(pair_counts != 1L)) {
    affected <- paste0(
      pairs$AGDB_ID[pair_counts != 1L], "+",
      pairs$OBJECTID[pair_counts != 1L], " (n=", pair_counts[pair_counts != 1L], ")"
    )
    stop(
      "Each confirmed waste pair must match exactly one source record: ",
      paste(affected, collapse = "; "), call. = FALSE
    )
  }

  flag <- rowSums(match_matrix) == 1L
  if (sum(flag) != nrow(pairs) || any(rowSums(match_matrix) > 1L)) {
    stop("Confirmed waste pair matching produced missing or additional records.",
         call. = FALSE)
  }
  flag
}

#-------------------------------------------------------------------------------
# Part 3. Unified AGDB4 Fe cleaning --------

# 3) Clean one sample medium using the same ordered validation pipeline

clean_fe_layer <- function(csv_path,
                           layer_name,
                           bbox,
                           audit_dir,
                           allowed_methods = NULL,
                           primary_qaqc_exclusions = c(
                             "DUPLICATE", "SITE DUPLICATE", "LAB REPLICATE", "DUPLICATE(?)"
                           ),
                           coordinate_tolerance_deg = 1e-4,
                           depth_breaks_cm = c(shallow_upper = 25, intermediate_upper = 75),
                           exclude_anthropogenic_waste = FALSE,
                           anthropogenic_waste_pattern = NULL,
                           confirmed_waste_ids = NULL,
                           fe_min = 0,
                           fe_max = 70) {
  assert_files_exist(csv_path, paste(layer_name, "input"))
  dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
  prefix <- paste0(tolower(layer_name), "_")

  cat("\n------------", toupper(layer_name), "------------\n")

  # 3.1 Import only fields required for modelling, traceability and audit.
  core_columns <- c(
    source_identifier_columns,
    source_context_columns,
    "x", "y", "LONGITUDE", "LATITUDE",
    "Fe_pct", "Fe_AM", "QAQC_TYPE",
    "FIELD_ID", "PROJECT_NAME", "SAMPLE_SOURCE",
    "DEPTH", "HORIZON", "SAMPLE_ZONE", "DATE_COLLECT"
  )
  requested_columns <- unique(c(core_columns, fe_audit_columns))

  source_header <- names(readr::read_csv(
    csv_path,
    n_max = 0L,
    show_col_types = FALSE,
    progress = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  ))
  selected_columns <- intersect(requested_columns, source_header)

  cat("Reading", length(selected_columns), "selected columns from", layer_name, "...\n")
  raw <- readr::read_csv(
    csv_path,
    col_select = dplyr::all_of(selected_columns),
    show_col_types = FALSE,
    progress = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )
  parse_problems <- readr::problems(raw)
  if (nrow(parse_problems) > 0L) {
    readr::write_csv(
      parse_problems,
      file.path(audit_dir, paste0(prefix, "readr_problems.csv"))
    )
    stop(layer_name, " CSV has parsing problems; see the saved audit.", call. = FALSE)
  }

  assert_required_columns(
    raw,
    c(source_identifier_columns, "x", "y", "Fe_pct", "Fe_AM", "QAQC_TYPE"),
    layer_name
  )
  raw$source_row_id <- seq_len(nrow(raw))
  n_imported <- nrow(raw)

  # 3.2 Resolve record-level duplicates before value, method or spatial filters.
  raw <- resolve_source_record_duplicates(raw, layer_name, audit_dir, prefix)
  n_after_source_dedup <- nrow(raw)

  waste_flags <- if (!is.null(anthropogenic_waste_pattern)) {
    anthropogenic_waste_flags(raw, anthropogenic_waste_pattern)
  } else {
    list(
      explicit = rep(FALSE, nrow(raw)),
      review = rep(FALSE, nrow(raw)),
      structured_text = rep("", nrow(raw)),
      review_text = rep("", nrow(raw))
    )
  }
  confirmed_waste <- confirmed_waste_id_flag(raw, confirmed_waste_ids)

  # 3.3 Standardise identifiers, coordinates, Fe values and optional metadata.
  data <- raw |>
    dplyr::transmute(
      source_row_id,
      source_objectid = trimws(as.character(OBJECTID)),
      source_agdb_id = trimws(as.character(AGDB_ID)),
      source_spl_id = optional_character(raw, "SPL_ID"),
      source_lab_id = optional_character(raw, "LAB_ID"),
      sample_id = trimws(as.character(AGDB_ID)),
      lon = suppressWarnings(as.numeric(x)),
      lat = suppressWarnings(as.numeric(y)),
      alt_lon = suppressWarnings(as.numeric(optional_character(raw, "LONGITUDE"))),
      alt_lat = suppressWarnings(as.numeric(optional_character(raw, "LATITUDE"))),
      Fe_pct = suppressWarnings(as.numeric(Fe_pct)),
      Fe_AM = normalise_code_text(Fe_AM),
      qaqc = normalise_code_text(QAQC_TYPE),
      field_id = optional_character(raw, "FIELD_ID"),
      project_name = optional_character(raw, "PROJECT_NAME"),
      media = optional_character(raw, "SAMPLE_SOURCE"),
      depth = optional_character(raw, "DEPTH"),
      horizon = optional_character(raw, "HORIZON"),
      sample_zone = optional_character(raw, "SAMPLE_ZONE"),
      date_collect_raw = optional_character(raw, "DATE_COLLECT"),
      date_collect = parse_collection_date(optional_character(raw, "DATE_COLLECT")),
      anthropogenic_waste_flag = waste_flags$explicit | confirmed_waste,
      anthropogenic_waste_review_flag = waste_flags$review & !confirmed_waste,
      anthropogenic_waste_manual_confirmed = confirmed_waste,
      anthropogenic_structured_text = waste_flags$structured_text,
      anthropogenic_review_text = waste_flags$review_text
    ) |>
    dplyr::mutate(
      dplyr::across(
        c(source_objectid, source_agdb_id, source_spl_id, source_lab_id,
          sample_id, field_id, project_name, media, depth, horizon, sample_zone),
        ~ dplyr::na_if(.x, "")
      ),
      qaqc = dplyr::na_if(qaqc, ""),
      Fe_AM = dplyr::na_if(Fe_AM, ""),
      site_id = make_site_id(lon, lat),
      support_id = make_support_id(site_id, depth, horizon)
    )

  depth_parsed <- parse_depth_support(
    data$depth,
    shallow_upper_cm = unname(depth_breaks_cm[["shallow_upper"]]),
    intermediate_upper_cm = unname(depth_breaks_cm[["intermediate_upper"]])
  )
  data <- dplyr::bind_cols(data, depth_parsed)

  # 3.4 Compare modelling and alternative coordinates without auto-correction.
  coordinate_pair_available <-
    is.finite(data$lon) & is.finite(data$lat) &
    is.finite(data$alt_lon) & is.finite(data$alt_lat)
  coordinate_mismatch <- coordinate_pair_available & (
    abs(data$lon - data$alt_lon) > coordinate_tolerance_deg |
      abs(data$lat - data$alt_lat) > coordinate_tolerance_deg
  )
  coordinate_audit <- data |>
    dplyr::mutate(
      alternative_pair_available = coordinate_pair_available,
      lon_difference_deg = lon - alt_lon,
      lat_difference_deg = lat - alt_lat,
      exceeds_tolerance = coordinate_mismatch
    ) |>
    dplyr::filter(exceeds_tolerance | !alternative_pair_available)
  write_empty_safe_csv(
    coordinate_audit,
    file.path(audit_dir, paste0(prefix, "coordinate_crosscheck.csv")),
    c(names(data), "alternative_pair_available", "lon_difference_deg",
      "lat_difference_deg", "exceeds_tolerance")
  )
  cat(
    "Coordinate cross-check:", sum(coordinate_mismatch), "record(s) exceed",
    coordinate_tolerance_deg, "deg tolerance;",
    sum(!coordinate_pair_available), "lack an alternative pair.\n"
  )

  # 3.5 Apply required-field, Fe-range and study-domain exclusions.
  exclusion_log <- list()
  exclusion_log$value_bbox <- data |>
    dplyr::mutate(
      exclusion_reason = dplyr::case_when(
        is.na(sample_id) | !nzchar(sample_id) ~ "missing_sample_id",
        !is.finite(lon) | !is.finite(lat) ~ "missing_or_non_finite_coordinate",
        !is.finite(Fe_pct) ~ "missing_or_non_finite_Fe_pct",
        Fe_pct <= fe_min ~ paste0("Fe_pct_at_or_below_", fe_min),
        Fe_pct >= fe_max ~ paste0("Fe_pct_at_or_above_", fe_max),
        lon < bbox$lon_min | lon > bbox$lon_max |
          lat < bbox$lat_min | lat > bbox$lat_max ~ "outside_study_bbox",
        TRUE ~ NA_character_
      ),
      exclusion_stage = "required_fields_Fe_range_and_bbox"
    ) |>
    dplyr::filter(!is.na(exclusion_reason))
  data <- data |>
    dplyr::filter(
      !is.na(sample_id),
      nzchar(sample_id),
      is.finite(lon),
      is.finite(lat),
      is.finite(Fe_pct),
      Fe_pct > fe_min,
      Fe_pct < fe_max,
      lon >= bbox$lon_min,
      lon <= bbox$lon_max,
      lat >= bbox$lat_min,
      lat <= bbox$lat_max
    )
  n_after_required_value_bbox <- nrow(data)

  # 3.6 Apply the prespecified analytical-method whitelist.
  if (!is.null(allowed_methods)) {
    allowed_methods <- normalise_code_text(allowed_methods)
    exclusion_log$method <- data |>
      dplyr::filter(is.na(Fe_AM) | !(Fe_AM %in% allowed_methods)) |>
      dplyr::mutate(
        exclusion_reason = "analytical_method_not_allowed",
        exclusion_stage = "analytical_method_filter"
      )
    data <- data |> dplyr::filter(Fe_AM %in% allowed_methods)
  }
  n_after_method <- nrow(data)

  # 3.7 Exclude explicit waste materials; retain contextual matches for review.
  exclusion_log$waste <- data |>
    dplyr::filter(anthropogenic_waste_flag) |>
    dplyr::mutate(
      exclusion_reason = dplyr::if_else(
        anthropogenic_waste_manual_confirmed,
        "manually_confirmed_sampled_tailings",
        "structured_material_identified_as_waste"
      ),
      exclusion_stage = "anthropogenic_waste_filter"
    )
  waste_review <- data |>
    dplyr::filter(anthropogenic_waste_review_flag) |>
    dplyr::mutate(review_reason = "waste_term_in_comment_or_mine_context")
  write_empty_safe_csv(
    waste_review,
    file.path(audit_dir, paste0(prefix, "anthropogenic_waste_manual_review.csv")),
    c(names(data), "review_reason")
  )

  if (exclude_anthropogenic_waste) {
    data <- data |> dplyr::filter(!anthropogenic_waste_flag)
  }
  n_after_waste <- nrow(data)

  # 3.8 Apply the same AGDB4 QA/QC duplicate policy to every medium.
  primary_qaqc_exclusions <- normalise_code_text(primary_qaqc_exclusions)
  exclusion_log$qaqc <- data |>
    dplyr::filter(!is.na(qaqc), qaqc %in% primary_qaqc_exclusions) |>
    dplyr::mutate(
      exclusion_reason = "primary_qaqc_exclusion",
      exclusion_stage = "primary_QAQC_filter"
    )
  write_empty_safe_csv(
    dplyr::bind_rows(exclusion_log),
    file.path(audit_dir, paste0(prefix, "exclusion_log.csv")),
    c(names(data), "exclusion_reason", "exclusion_stage")
  )
  data <- data |>
    dplyr::filter(is.na(qaqc) | !(qaqc %in% primary_qaqc_exclusions))
  n_after_qaqc <- nrow(data)
  if (n_after_qaqc == 0L) {
    stop(layer_name, " has no records after the prespecified cleaning pipeline.",
         call. = FALSE)
  }

  # Co-location alone is not evidence of duplication.
  if (anyDuplicated(data$sample_id)) {
    stop("Internal error: source-ID validation left duplicate sample_id values.", call. = FALSE)
  }

  # 3.9 Save the soil depth audit and each medium's sample-flow summary.
  # The soil method summary is written once by write_soil_eda(); sediment and
  # rock method-specific summaries are not model inputs.
  if (identical(tolower(layer_name), "soil")) {
    readr::write_csv(
      data |>
        dplyr::count(depth_parse_status, depth_unit, depth_class, name = "n") |>
        dplyr::arrange(depth_parse_status, depth_unit, depth_class),
      file.path(audit_dir, "soil_retained_depth_summary.csv")
    )
  }

  retained_counts <- c(
    n_imported,
    n_after_source_dedup,
    n_after_required_value_bbox,
    n_after_method,
    n_after_waste,
    n_after_qaqc
  )
  sample_flow <- data.frame(
    stage = c(
      "imported",
      "source_record_deduplication",
      "required_fields_Fe_range_and_bbox",
      "analytical_method_filter",
      "confirmed_or_structured_anthropogenic_waste_filter",
      "primary_QAQC_filter"
    ),
    retained_n = retained_counts,
    excluded_since_previous = c(NA_integer_, head(retained_counts, -1L) - tail(retained_counts, -1L)),
    stringsAsFactors = FALSE
  )
  readr::write_csv(
    sample_flow,
    file.path(audit_dir, paste0(prefix, "sample_flow.csv"))
  )

  sf::st_as_sf(data, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
}

# 4) Apply the unified pipeline to soil, sediment and rock

prepare_fe_layers <- function(cfg, output_dir) {
  audit_dir <- file.path(output_dir, "audit")
  common_arguments <- list(
    bbox = cfg$bbox,
    audit_dir = audit_dir,
    allowed_methods = cfg$analysis$allowed_fe_methods,
    primary_qaqc_exclusions = cfg$analysis$primary_qaqc_exclusions,
    coordinate_tolerance_deg = cfg$analysis$coordinate_tolerance_deg,
    depth_breaks_cm = cfg$analysis$depth_breaks_cm,
    exclude_anthropogenic_waste = cfg$analysis$exclude_anthropogenic_waste,
    anthropogenic_waste_pattern = cfg$analysis$anthropogenic_waste_pattern
  )

  soil <- do.call(clean_fe_layer, c(
    list(
      csv_path = find_data_file(cfg$data_dir, "Fe_soil_sample_all_methods.csv"),
      layer_name = "soil",
      confirmed_waste_ids = cfg$analysis$confirmed_waste_ids$soil
    ),
    common_arguments
  ))
  sediment <- do.call(clean_fe_layer, c(
    list(
      csv_path = find_data_file(cfg$data_dir, "Fe_sediment_sample_all_methods.csv"),
      layer_name = "sediment",
      confirmed_waste_ids = cfg$analysis$confirmed_waste_ids$sediment
    ),
    common_arguments
  ))
  rock <- do.call(clean_fe_layer, c(
    list(
      csv_path = find_data_file(cfg$data_dir, "Fe_rock_sample_all_methods.csv"),
      layer_name = "rock",
      confirmed_waste_ids = cfg$analysis$confirmed_waste_ids$rock
    ),
    common_arguments
  ))

  # Consolidate stage counts without replacing the medium-specific audit files.
  media_names <- c("soil", "sediment", "rock")
  combined_flow <- dplyr::bind_rows(lapply(media_names, function(media_name) {
    readr::read_csv(
      file.path(audit_dir, paste0(media_name, "_sample_flow.csv")),
      show_col_types = FALSE
    ) |>
      dplyr::mutate(medium = media_name, .before = 1)
  }))
  readr::write_csv(
    combined_flow,
    file.path(audit_dir, "agdb4_sample_flow_all_media.csv")
  )

  list(soil = soil, sediment = sediment, rock = rock)
}

#-------------------------------------------------------------------------------
# Part 4. Soil EDA and common prediction grid --------

# 5) Response-scale EDA

write_soil_eda <- function(soil_sf, output_dir) {
  soil <- sf::st_drop_geometry(soil_sf)
  soil$Fe_log10 <- safe_log10(soil$Fe_pct, "soil$Fe_pct")
  method_summary <- soil |>
    dplyr::group_by(Fe_AM) |>
    dplyr::summarise(
      n = dplyr::n(), median = stats::median(Fe_pct),
      mean = mean(Fe_pct), sd = stats::sd(Fe_pct), .groups = "drop"
    )
  readr::write_csv(method_summary, file.path(output_dir, "soil_Fe_method_summary.csv"))
  histogram <- ggplot2::ggplot(soil, ggplot2::aes(x = Fe_log10)) +
    ggplot2::geom_histogram(ggplot2::aes(y = after_stat(density)), bins = 15) +
    ggplot2::stat_function(
      fun = stats::dnorm,
      args = list(mean = mean(soil$Fe_log10), sd = stats::sd(soil$Fe_log10))
    ) +
    ggplot2::labs(x = expression(log[10] * "(Fe %)"), y = "Density")
  ggplot2::ggsave(file.path(output_dir, "soil_Fe_log10_histogram.png"),
                  histogram, width = 7.5, height = 5, dpi = 300)
  invisible(method_summary)
}

# 6) Common 1-km EPSG:3338 template and exact geographic study boundary

create_study_area_polygon <- function(cfg, points_per_edge = 1001L) {
  points_per_edge <- max(3L, as.integer(points_per_edge))
  lon_forward <- seq(cfg$bbox$lon_min, cfg$bbox$lon_max, length.out = points_per_edge)
  lon_reverse <- rev(lon_forward)
  lat_forward <- seq(cfg$bbox$lat_min, cfg$bbox$lat_max, length.out = points_per_edge)
  lat_reverse <- rev(lat_forward)

  # Densifying in longitude/latitude before projection preserves the four
  # stated meridians/parallels rather than replacing them with four chords.
  coordinates <- rbind(
    cbind(lon_forward, cfg$bbox$lat_min),
    cbind(cfg$bbox$lon_max, lat_forward[-1L]),
    cbind(lon_reverse[-1L], cfg$bbox$lat_max),
    cbind(cfg$bbox$lon_min, lat_reverse[-c(1L, points_per_edge)]),
    c(cfg$bbox$lon_min, cfg$bbox$lat_min)
  )
  sf::st_sfc(
    sf::st_polygon(list(coordinates)),
    crs = cfg$crs$geographic
  ) |>
    sf::st_transform(cfg$crs$projected)
}

create_project_template <- function(cfg) {
  bbox_polygon <- create_study_area_polygon(cfg)
  template <- terra::rast(
    terra::ext(terra::vect(bbox_polygon)),
    resolution = cfg$resolution_m,
    crs = paste0("EPSG:", cfg$crs$projected)
  )
  list(bbox_polygon = bbox_polygon, template = template)
}

#-------------------------------------------------------------------------------
# Part 5. Coarse terrain and mapped geology --------

# 7) Coarse terrain covariates

prepare_coarse_terrain <- function(cfg, template) {
  dem_raw <- terra::rast(find_data_file(cfg$data_dir, "gmted_mea300.tif"))
  bbox_wgs84 <- terra::ext(
    cfg$bbox$lon_min, cfg$bbox$lon_max,
    cfg$bbox$lat_min, cfg$bbox$lat_max
  )
  dem_crop <- terra::crop(dem_raw, bbox_wgs84)
  elevation <- terra::project(dem_crop, template, method = "bilinear")
  names(elevation) <- "elev"
  slope <- terra::terrain(elevation, v = "slope", unit = "degrees")
  aspect <- terra::terrain(elevation, v = "aspect", unit = "radians")
  eastness <- sin(aspect)
  northness <- cos(aspect)
  names(slope) <- "slope"
  names(eastness) <- "eastness"
  names(northness) <- "northness"
  c(elevation, slope, eastness, northness)
}

# 8) SIM 3340 FileGDB tables and polygon layers

read_filegdb_table <- function(geodatabase, layer, required_columns) {
  temporary_directory <- tempfile("filegdb_table_")
  dir.create(temporary_directory, recursive = TRUE)
  on.exit(unlink(temporary_directory, recursive = TRUE, force = TRUE), add = TRUE)
  output_csv <- file.path(temporary_directory, paste0(layer, ".csv"))
  quote_identifier <- function(x) paste0('"', gsub('"', '""', x), '"')
  sql <- sprintf(
    "SELECT %s FROM %s",
    paste(quote_identifier(required_columns), collapse = ", "),
    quote_identifier(layer)
  )
  sf::gdal_utils(
    util = "vectortranslate",
    source = geodatabase,
    destination = output_csv,
    options = c(
      "-f", "CSV",
      "-sql", sql,
      "-dialect", "OGRSQL"
    ),
    quiet = TRUE
  )
  if (!file.exists(output_csv)) {
    stop("GDAL did not create the table export: ", output_csv, call. = FALSE)
  }
  result <- readr::read_csv(
    output_csv,
    show_col_types = FALSE,
    progress = FALSE
  )
  assert_required_columns(result, required_columns, layer)
  result
}

load_geology_layers <- function(cfg, bbox_polygon) {
  geodatabase <- find_data_directory(cfg$data_dir, "AKStategeol.gdb")
  polygon_layer <- "AKStategeol_poly"

  # OGR SQL retains geometry implicitly; wkt_filter fails on complex
  # multi-part polygons in this geodatabase, returning a plain data.frame.
  polygons <- sf::st_read(
    dsn = geodatabase,
    query = paste0("SELECT NSACLASS, STATE_LABEL FROM ", polygon_layer),
    quiet = TRUE
  )
  if (is.na(sf::st_crs(polygons))) {
    stop("The geology polygon layer has no defined CRS.", call. = FALSE)
  }
  assert_required_columns(polygons, "NSACLASS", "geology polygons")
  
  # Crop before validation: repairing all statewide polygons exhausts memory.
  polygons <- sf::st_transform(polygons, cfg$crs$projected)
  bbox_projected <- sf::st_transform(bbox_polygon, cfg$crs$projected)
  polygons <- sf::st_crop(polygons, sf::st_bbox(bbox_projected))
  message("Geology polygons within study area: ", nrow(polygons))
  
  polygons <- prepare_polygon_sf(polygons, "geology polygons")

  nsalith <- read_filegdb_table(
    geodatabase = geodatabase,
    layer = "nsalith",
    required_columns = c("NSACLASS", "LITH1", "LITH3", "RANK")
  )

  list(polygons = polygons, nsalith = nsalith)
}

# 9) Geology-to-lithology lookup and point assignment

make_nsalith_primary_lookup <- function(nsalith) {
  ranked <- nsalith |>
    dplyr::filter(!is.na(NSACLASS)) |>
    dplyr::mutate(
      rank_priority = dplyr::case_when(
        RANK == "Major" ~ 1L,
        RANK == "Indeterminate, major" ~ 2L,
        TRUE ~ 3L
      ),
      LITH1_clean = trimws(as.character(LITH1)),
      LITH1_normalised = normalise_lithology_text(LITH1)
    ) |>
    dplyr::group_by(NSACLASS) |>
    dplyr::filter(rank_priority == min(rank_priority, na.rm = TRUE)) |>
    dplyr::ungroup()

  ranked |>
    dplyr::group_by(NSACLASS) |>
    dplyr::summarise(
      LITH1 = {
        values <- sort(unique(LITH1_clean[!is.na(LITH1_clean) & nzchar(LITH1_clean)]))
        if (length(values) == 0L) NA_character_ else paste(values, collapse = " + ")
      },
      water_or_ice_only = {
        values <- unique(LITH1_normalised[nzchar(LITH1_normalised)])
        length(values) > 0L && all(values %in% c("water", "ice"))
      },
      .groups = "drop"
    )
}

assign_points_to_geology <- function(points, polygons, buffer_m = 5) {
  point_candidates <- sf::st_intersects(points, polygons)
  chosen <- rep(NA_integer_, nrow(points))

  for (i in seq_len(nrow(points))) {
    candidates <- point_candidates[[i]]
    if (length(candidates) == 1L) {
      chosen[i] <- candidates
    } else if (length(candidates) > 1L) {
      point_buffer <- sf::st_buffer(points[i, ], dist = buffer_m)
      # Evaluate candidates separately because st_intersection may drop empty members.
      areas <- vapply(candidates, function(j) {
        intersection <- suppressWarnings(sf::st_intersection(
          sf::st_geometry(polygons[j, ]), sf::st_geometry(point_buffer)
        ))
        if (length(intersection) == 0L) 0 else sum(as.numeric(sf::st_area(intersection)))
      }, numeric(1))
      best <- which(areas == max(areas, na.rm = TRUE))
      if (length(best) > 1L) {
        class_values <- as.character(polygons$NSACLASS[candidates[best]])
        best <- best[order(class_values, na.last = TRUE)][1L]
      }
      chosen[i] <- candidates[best[1L]]
    }
  }

  attributes <- sf::st_drop_geometry(polygons)[chosen, , drop = FALSE]
  rownames(attributes) <- NULL
  dplyr::bind_cols(points, attributes)
}

# Rasterise complex mapped geology once with GDAL rather than terra's polygon
# rasteriser. Omitting -at applies the cell-centre rule used throughout the
# project; all geology-derived layers are subsequently obtained by lookup.
rasterize_geology_unit_codes <- function(geology_units,
                                         template,
                                         output_path) {
  if (!inherits(geology_units, "sf")) {
    stop("geology_units is not an sf object.", call. = FALSE)
  }
  assert_required_columns(
    geology_units, "NSACLASS_code", "geology unit polygons"
  )
  if (!inherits(template, "SpatRaster") || terra::nlyr(template) != 1L) {
    stop("template must be a single-layer SpatRaster.", call. = FALSE)
  }

  geology_units <- geology_units[
    !sf::st_is_empty(geology_units) & !is.na(geology_units$NSACLASS_code),
    "NSACLASS_code",
    drop = FALSE
  ]
  if (nrow(geology_units) == 0L) {
    stop("No geology polygons are available for unit-code rasterisation.",
         call. = FALSE)
  }
  geology_units$NSACLASS_code <- as.integer(geology_units$NSACLASS_code)
  geology_units <- geology_units[order(geology_units$NSACLASS_code), , drop = FALSE]

  # Keep both the temporary vector and output raster beside processed data so
  # the operation never depends on limited system-drive temporary space.
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  work_dir <- tempfile("geology_gdal_", tmpdir = dirname(output_path))
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)

  vector_path <- file.path(work_dir, "geology_unit_codes.gpkg")
  sf::st_write(
    geology_units,
    dsn = vector_path,
    layer = "geology_units",
    append = FALSE,
    quiet = TRUE
  )

  if (file.exists(output_path) && !file.remove(output_path)) {
    stop("Cannot replace the existing geology unit-code raster: ", output_path,
         call. = FALSE)
  }

  raster_extent <- terra::ext(template)
  format_number <- function(x) {
    format(x, scientific = FALSE, trim = TRUE, digits = 15)
  }
  options <- c(
    "-l", "geology_units",
    "-a", "NSACLASS_code",
    "-init", "0",
    "-a_nodata", "0",
    "-ot", "Int32",
    "-of", "GTiff",
    "-te",
    format_number(terra::xmin(raster_extent)),
    format_number(terra::ymin(raster_extent)),
    format_number(terra::xmax(raster_extent)),
    format_number(terra::ymax(raster_extent)),
    "-ts", as.character(terra::ncol(template)), as.character(terra::nrow(template)),
    "-optim", "VECTOR",
    "-co", "TILED=YES",
    "-co", "COMPRESS=DEFLATE",
    "-co", "BIGTIFF=IF_SAFER"
  )

  message("Rasterising one deterministic NSACLASS code layer with GDAL ...")
  success <- sf::gdal_utils(
    util = "rasterize",
    source = vector_path,
    destination = output_path,
    options = options,
    quiet = TRUE,
    config_options = c(
      GDAL_CACHEMAX = "256",
      GDAL_NUM_THREADS = "1"
    )
  )
  if (!isTRUE(success) || !file.exists(output_path)) {
    stop("GDAL failed to create the geology unit-code raster.", call. = FALSE)
  }

  unit_code <- terra::rast(output_path)
  terra::NAflag(unit_code) <- 0
  names(unit_code) <- "NSACLASS_code"
  if (!isTRUE(terra::compareGeom(unit_code, template, stopOnError = FALSE))) {
    stop("The geology unit-code raster is not aligned with the project template.",
         call. = FALSE)
  }
  message("Geology unit-code raster complete.")
  unit_code
}

#-------------------------------------------------------------------------------
# Part 6. Auxiliary geochemistry and structural missingness --------

# 10) Aggregate retained auxiliary records sharing the same EPSG:3338
# coordinate after rounding to 0.001 m.

aggregate_colocated_points <- function(points, media_label) {
  coordinates <- sf::st_coordinates(points)
  table <- sf::st_drop_geometry(points) |>
    dplyr::mutate(
      .x_round = round(coordinates[, 1], 3),
      .y_round = round(coordinates[, 2], 3)
    ) |>
    dplyr::group_by(.x_round, .y_round) |>
    dplyr::summarise(
      sample_id = paste(sort(unique(sample_id)), collapse = ";"),
      Fe_pct = stats::median(Fe_pct, na.rm = TRUE),
      Fe_min = min(Fe_pct, na.rm = TRUE),
      Fe_max = max(Fe_pct, na.rm = TRUE),
      Fe_range = Fe_max - Fe_min,
      n_colocated = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(media = media_label)
  sf::st_as_sf(table, coords = c(".x_round", ".y_round"), crs = sf::st_crs(points), remove = FALSE)
}

# Assign each 1-km cell centre to its nearest point, 
# inducing a rasterised Voronoi-like partition without constructing continuous polygons.
nearest_point_rasters <- function(points,
                                  template,
                                  value_column,
                                  value_name,
                                  distance_name) {
  if (!inherits(points, "sf") || nrow(points) == 0L) {
    stop("Nearest-point input must be a non-empty sf layer.", call. = FALSE)
  }
  assert_required_columns(points, value_column, "nearest-point input")
  values <- as.numeric(points[[value_column]])
  if (any(!is.finite(values))) {
    stop("Nearest-point values contain non-finite entries.", call. = FALSE)
  }

  source_xy <- sf::st_coordinates(points)[, 1:2, drop = FALSE]
  query_xy <- terra::xyFromCell(template, seq_len(terra::ncell(template)))
  nearest <- RANN::nn2(source_xy, query = query_xy, k = 1L)
  nearest_index <- nearest$nn.idx[, 1L]

  value_raster <- terra::setValues(
    terra::rast(template), values[nearest_index]
  )
  distance_raster <- terra::setValues(
    terra::rast(template), nearest$nn.dists[, 1L] / 1000
  )
  names(value_raster) <- value_name
  names(distance_raster) <- distance_name
  c(value_raster, distance_raster)
}

# 11) Build geology, nearest-neighbour and support covariates

build_geology_covariates <- function(fe_layers, geology, cfg, template, output_dir) {
  audit_dir <- file.path(output_dir, "audit")
  dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
  soil_3338 <- sf::st_transform(fe_layers$soil, cfg$crs$projected)
  rock_3338 <- aggregate_colocated_points(
    sf::st_transform(fe_layers$rock, cfg$crs$projected), "rock"
  )
  sediment_3338 <- aggregate_colocated_points(
    sf::st_transform(fe_layers$sediment, cfg$crs$projected), "sediment"
  )

  # The layer has already been spatially filtered and validated.
  polygons <- geology$polygons

  soil_geology <- assign_points_to_geology(soil_3338, polygons)
  rock_geology <- assign_points_to_geology(rock_3338, polygons)
  sediment_geology <- assign_points_to_geology(sediment_3338, polygons)

  primary_lithology <- make_nsalith_primary_lookup(geology$nsalith)
  unit_rock <- rock_geology |>
    sf::st_drop_geometry() |>
    dplyr::filter(!is.na(NSACLASS)) |>
    dplyr::group_by(NSACLASS) |>
    dplyr::summarise(
      rock_Fe_mean = mean(Fe_pct),
      rock_n_samples = sum(n_colocated), .groups = "drop"
    )
  unit_sediment <- sediment_geology |>
    sf::st_drop_geometry() |>
    dplyr::filter(!is.na(NSACLASS)) |>
    dplyr::group_by(NSACLASS) |>
    dplyr::summarise(
      sed_Fe_mean = mean(Fe_pct),
      sed_n_samples = sum(n_colocated), .groups = "drop"
    )

  rock_lithology_mean <- rock_geology |>
    sf::st_drop_geometry() |>
    dplyr::left_join(primary_lithology, by = "NSACLASS") |>
    dplyr::filter(!is.na(LITH1)) |>
    dplyr::group_by(LITH1) |>
    dplyr::summarise(rock_Fe_lith1_mean = mean(Fe_pct), .groups = "drop")
  sediment_lithology_mean <- sediment_geology |>
    sf::st_drop_geometry() |>
    dplyr::left_join(primary_lithology, by = "NSACLASS") |>
    dplyr::filter(!is.na(LITH1)) |>
    dplyr::group_by(LITH1) |>
    dplyr::summarise(sed_Fe_lith1_mean = mean(Fe_pct), .groups = "drop")

  # Apply the same hierarchy to observations and prediction polygons:
  # geological-unit mean, broad-lithology fallback, then structural zero.
  add_support_covariates <- function(x) {
    x |>
    dplyr::left_join(primary_lithology, by = "NSACLASS") |>
    dplyr::left_join(unit_rock, by = "NSACLASS") |>
    dplyr::left_join(unit_sediment, by = "NSACLASS") |>
    dplyr::left_join(rock_lithology_mean, by = "LITH1") |>
    dplyr::left_join(sediment_lithology_mean, by = "LITH1") |>
    dplyr::mutate(
      rock_Fe_final_raw = dplyr::coalesce(rock_Fe_mean, rock_Fe_lith1_mean),
      sed_Fe_final_raw = dplyr::coalesce(sed_Fe_mean, sed_Fe_lith1_mean),
      rock_support_missing = as.integer(is.na(rock_Fe_final_raw)),
      sed_support_missing = as.integer(is.na(sed_Fe_final_raw)),
      rock_source = dplyr::case_when(
        !is.na(rock_Fe_mean) ~ "unit_mean",
        !is.na(rock_Fe_final_raw) ~ "lith1_fallback",
        TRUE ~ "missing"
      ),
      sed_source = dplyr::case_when(
        !is.na(sed_Fe_mean) ~ "unit_mean",
        !is.na(sed_Fe_final_raw) ~ "lith1_fallback",
        TRUE ~ "missing"
      ),
      rock_n_samples = dplyr::coalesce(rock_n_samples, 0L),
      sed_n_samples = dplyr::coalesce(sed_n_samples, 0L),
      rock_Fe_final = dplyr::coalesce(
        rock_Fe_final_raw,
        cfg$analysis$structural_support_fill_value
      ),
      sed_Fe_final = dplyr::coalesce(
        sed_Fe_final_raw,
        cfg$analysis$structural_support_fill_value
      )
    )
  }
  soil_model <- add_support_covariates(soil_geology)

  # Structural absence is encoded by zero counts plus explicit source flags.
  support_audit <- soil_model |>
    sf::st_drop_geometry() |>
    dplyr::summarise(
      n_soil = dplyr::n(),
      n_missing_rock_support = sum(rock_support_missing == 1L),
      pct_missing_rock_support = 100 * mean(rock_support_missing == 1L),
      n_missing_sediment_support = sum(sed_support_missing == 1L),
      pct_missing_sediment_support = 100 * mean(sed_support_missing == 1L),
      n_missing_both = sum(
        rock_support_missing == 1L & sed_support_missing == 1L
      )
    )
  readr::write_csv(
    support_audit,
    file.path(audit_dir, "soil_auxiliary_support_summary.csv")
  )

  polygon_prediction <- add_support_covariates(polygons) |>
    dplyr::select(
      NSACLASS, LITH1, water_or_ice_only,
      rock_Fe_final, sed_Fe_final,
      rock_support_missing, sed_support_missing,
      rock_n_samples, sed_n_samples
    )
  polygon_prediction <- polygon_prediction[
    !sf::st_is_empty(polygon_prediction),
    ,
    drop = FALSE
  ]

  # Build one deterministic class-code lookup. The expensive complex polygon
  # geometry is rasterised once; all numeric geology layers reuse this code.
  support_fields <- c(
    "rock_Fe_final", "sed_Fe_final",
    "rock_support_missing", "sed_support_missing",
    "rock_n_samples", "sed_n_samples"
  )
  unit_lookup <- polygon_prediction |>
    sf::st_drop_geometry() |>
    dplyr::mutate(NSACLASS = trimws(as.character(NSACLASS))) |>
    dplyr::filter(!is.na(NSACLASS), nzchar(NSACLASS)) |>
    dplyr::select(
      NSACLASS, LITH1, water_or_ice_only,
      dplyr::all_of(support_fields)
    ) |>
    dplyr::distinct()
  inconsistent_units <- unit_lookup |>
    dplyr::count(NSACLASS, name = "n_definitions") |>
    dplyr::filter(n_definitions != 1L)
  if (nrow(inconsistent_units) > 0L) {
    stop(
      "One or more NSACLASS values have inconsistent predictor definitions: ",
      paste(inconsistent_units$NSACLASS, collapse = ", "),
      call. = FALSE
    )
  }
  unit_lookup <- unit_lookup |>
    dplyr::arrange(NSACLASS) |>
    dplyr::mutate(
      NSACLASS_code = dplyr::row_number(),
      domain_valid = as.integer(
        !dplyr::coalesce(water_or_ice_only, FALSE)
      )
    )

  geology_units <- polygon_prediction |>
    dplyr::mutate(NSACLASS = trimws(as.character(NSACLASS))) |>
    dplyr::left_join(
      unit_lookup |>
        dplyr::select(NSACLASS, NSACLASS_code, domain_valid),
      by = "NSACLASS"
    )
  if (any(
    !is.na(geology_units$NSACLASS) & nzchar(geology_units$NSACLASS) &
      is.na(geology_units$NSACLASS_code)
  )) {
    stop("One or more geology polygons lack an NSACLASS code.", call. = FALSE)
  }

  unit_code <- rasterize_geology_unit_codes(
    geology_units = geology_units,
    template = template,
    output_path = file.path(
      output_dir,
      paste0("geology_NSACLASS_code_", cfg$resolution_m, "m.tif")
    )
  )

  lookup_values <- as.matrix(unit_lookup[, c(support_fields, "domain_valid")])
  storage.mode(lookup_values) <- "double"
  geology_values <- terra::subst(
    unit_code,
    from = unit_lookup$NSACLASS_code,
    to = lookup_values
  )
  names(geology_values) <- c(support_fields, "domain_valid")
  geology_stack <- geology_values[[support_fields]]
  land_mask <- geology_values[["domain_valid"]]

  rock_nearest <- nearest_point_rasters(
    rock_3338, template, "Fe_pct", "nn_rock_Fe", "nn_rock_dist_km"
  )
  sediment_nearest <- nearest_point_rasters(
    sediment_3338, template, "Fe_pct", "nn_sed_Fe", "nn_sed_dist_km"
  )
  geology_stack <- c(
    geology_stack,
    rock_nearest[["nn_rock_Fe"]],
    sediment_nearest[["nn_sed_Fe"]],
    rock_nearest[["nn_rock_dist_km"]],
    sediment_nearest[["nn_sed_dist_km"]]
  )

  study_area <- sf::st_sf(
    study_domain = 1L,
    geometry = create_study_area_polygon(cfg)
  )
  study_mask <- terra::rasterize(
    terra::vect(study_area),
    template,
    field = "study_domain",
    background = NA,
    touches = FALSE
  )
  # A valid prediction cell must have its centre inside the stated geographic
  # domain and intersect mapped non-water/non-ice geology.
  domain_mask <- terra::ifel(land_mask == 1 & study_mask == 1, 1, NA)
  names(domain_mask) <- "domain_mask"

  safe_write_raster(
    domain_mask,
    file.path(output_dir, paste0("domain_mask_", cfg$resolution_m, "m.tif")),
    datatype = "INT1U"
  )

  list(
    soil_model = soil_model,
    geology_stack = geology_stack,
    geology_units = geology_units,
    unit_code = unit_code,
    unit_lookup = unit_lookup,
    domain_mask = domain_mask
  )
}

#-------------------------------------------------------------------------------
# Part 7. Point extraction and V2 environmental covariates --------

# 12) Extract model data from the exact prediction rasters

extract_model_table <- function(points_sf, covariate_stack, features, target = "Fe_log10") {
  point_data <- sf::st_drop_geometry(points_sf)
  assert_required_columns(point_data, c("sample_id", "lon", "lat", "Fe_pct"), "soil points")

  # Grid membership is computed from the exact 1 km prediction template. Random
  # CV later keeps every record in the same grid cell within one fold.
  cells <- terra::cellFromXY(covariate_stack[[1]], sf::st_coordinates(points_sf))
  if (anyNA(cells)) stop("One or more soil points fall outside the prediction grid.", call. = FALSE)
  point_data$grid_id <- paste0("GRID_", as.integer(cells))
  grid_centres <- terra::xyFromCell(covariate_stack[[1]], cells)
  point_data$grid_x <- grid_centres[, 1]
  point_data$grid_y <- grid_centres[, 2]

  # Training predictors come from the final raster stack used for mapping.
  point_data <- point_data |> dplyr::select(-dplyr::any_of(features))
  extracted <- terra::extract(covariate_stack[[features]], terra::vect(points_sf), ID = FALSE)
  data <- dplyr::bind_cols(point_data, extracted)
  keep_columns <- intersect(
    c(
      "source_row_id", "source_objectid", "source_agdb_id", "source_spl_id",
      "source_lab_id", "sample_id", "site_id", "support_id", "grid_id",
      "grid_x", "grid_y", "lon", "lat", "Fe_pct", "Fe_AM", "qaqc", "media",
      "depth", "horizon", "sample_zone", "depth_parse_status", "depth_unit",
      "depth_is_interval", "depth_lower_cm", "depth_upper_cm", "depth_midpoint_cm",
      "depth_class", "date_collect", "field_id", "project_name",
      "anthropogenic_waste_flag",
      "anthropogenic_waste_review_flag",
      "NSACLASS", "LITH1", "rock_source", "sed_source"
    ),
    names(data)
  )
  data <- data |> dplyr::select(dplyr::all_of(keep_columns), dplyr::all_of(features))
  data[[target]] <- safe_log10(data$Fe_pct, "Fe_pct")
  data
}

# 13) Permafrost and climate alignment for V2

align_probability_raster <- function(input,
                                     template,
                                     name,
                                     valid_range = NULL) {
  source_raster <- if (inherits(input, "SpatRaster")) input else terra::rast(input)
  raster <- source_raster
  if (!is.null(valid_range)) {
    if (length(valid_range) != 2L || any(!is.finite(valid_range))) {
      stop("valid_range must contain two finite bounds.", call. = FALSE)
    }
    raster <- terra::ifel(
      source_raster >= valid_range[[1]] & source_raster <= valid_range[[2]],
      source_raster,
      NA
    )
  }

  # Area-weighted numerator/denominator aggregation preserves the mean of valid
  # source pixels while explicitly tracking partial source coverage per 1 km cell.
  valid <- terra::ifel(is.na(raster), 0, 1)
  filled <- terra::ifel(is.na(raster), 0, raster)
  numerator <- terra::project(filled, template, method = "average")
  valid_fraction <- terra::project(valid, template, method = "average")
  aligned <- terra::ifel(valid_fraction > 0, numerator / valid_fraction, NA)
  names(aligned) <- name
  aligned
}

inventory_daymet_files <- function(cfg, output_dir) {
  all_files <- list.files(cfg$data_dir, recursive = TRUE, full.names = TRUE, include.dirs = FALSE)
  # Earthdata may prepend the collection name to the canonical granule name;
  # match the stable Daymet token at the end of either filename form.
  pattern <- "daymet_v4_(tmin_annavg|tmax_annavg|prcp_annttl)_na_([0-9]{4})\\.nc$"
  capture_pattern <- paste0(".*", pattern)
  matched <- all_files[grepl(pattern, basename(all_files), ignore.case = TRUE)]
  if (length(matched) == 0L) {
    stop("No Daymet annual tmin/tmax/prcp NetCDF were found.", call. = FALSE)
  }
  names_only <- tolower(basename(matched))
  type <- sub(capture_pattern, "\\1", names_only, ignore.case = TRUE)
  year <- as.integer(sub(capture_pattern, "\\2", names_only, ignore.case = TRUE))
  inventory <- data.frame(path = matched, variable = type, year = year, stringsAsFactors = FALSE) |>
    dplyr::filter(year >= cfg$daymet$start_year, year <= cfg$daymet$end_year) |>
    dplyr::arrange(variable, year)

  expected <- tidyr::expand_grid(
    variable = c("tmin_annavg", "tmax_annavg", "prcp_annttl"),
    year = seq.int(cfg$daymet$start_year, cfg$daymet$end_year)
  )
  counts <- inventory |> dplyr::count(variable, year, name = "n")
  check <- expected |> dplyr::left_join(counts, by = c("variable", "year")) |>
    dplyr::mutate(n = dplyr::coalesce(n, 0L))
  readr::write_csv(inventory, file.path(output_dir, "daymet_1981_2010_inventory.csv"))
  readr::write_csv(check, file.path(output_dir, "daymet_1981_2010_completeness.csv"))
  invalid <- check |> dplyr::filter(n != 1L)
  if (nrow(invalid) > 0L || nrow(inventory) != 90L) {
    stop(
      "Daymet inventory must contain exactly one tmin, tmax and prcp file for each year 1981-2010 ",
      "(90 files total). See completeness audit.", call. = FALSE
    )
  }
  inventory
}

mean_raster_collection <- function(paths, template) {
  if (length(paths) == 0L) stop("Raster collection is empty.", call. = FALSE)
  # Project each annual layer directly to the final template. This limits I/O to
  # the study region instead of stacking full North America rasters in memory.
  aligned <- lapply(paths, function(path) {
    terra::project(terra::rast(path), template, method = "bilinear")
  })
  terra::app(terra::rast(aligned), mean, na.rm = TRUE)
}

prepare_v2_environmental_covariates <- function(cfg, template, output_dir) {
  inventory <- inventory_daymet_files(cfg, output_dir)
  permafrost <- align_probability_raster(
    find_data_file(cfg$data_dir, "AK_ProbofPF_1m_clipped.img"),
    template,
    "PF_prob",
    valid_range = cfg$analysis$permafrost_valid_range
  )
  tmin_mean <- mean_raster_collection(
    inventory$path[inventory$variable == "tmin_annavg"], template
  )
  tmax_mean <- mean_raster_collection(
    inventory$path[inventory$variable == "tmax_annavg"], template
  )
  maat <- (tmin_mean + tmax_mean) / 2
  names(maat) <- "MAAT"
  map <- mean_raster_collection(
    inventory$path[inventory$variable == "prcp_annttl"], template
  )
  names(map) <- "MAP"
  c(permafrost, maat, map)
}

#-------------------------------------------------------------------------------
# Part 8. Lithology representation for V3-V4 --------

# 14) Lithology classification and encoding

normalise_lithology_text <- function(x) {
  x <- tolower(trimws(ifelse(is.na(x), "", as.character(x))))
  gsub("[[:space:]_]+", "-", x)
}

classify_lithology_rows <- function(lith1, lith3) {
  lith1 <- normalise_lithology_text(lith1)
  lith3 <- normalise_lithology_text(lith3)
  dplyr::case_when(
    lith1 %in% c("ice", "water") ~ NA_character_,
    lith1 == "igneous" & grepl(
      "mafic|gabbroic|ultramafic|melilitic|lamprophyre|dioritic|alkalic-volcanic", lith3
    ) ~ "Ign_highFe",
    lith1 == "igneous" & grepl(
      "felsic|granitic|syenitic|foidal-syenitic|anorthosite", lith3
    ) ~ "Ign_lowFe",
    lith1 == "sedimentary" & grepl("mudstone|siltstone", lith3) ~ "Sed_fine",
    lith1 == "sedimentary" ~ "Sed_other",
    lith1 == "metamorphic" & grepl(
      "metaclastic|metacarbonate|paragneiss|calc-silicate|mica-schist|chlorite-schist|amphibole-schist|quartz-feldspar-schist|hornfels",
      lith3
    ) ~ "Meta_sed",
    lith1 == "metamorphic" & grepl(
      "metavolcanic|metaintrusive|orthogneiss|hornblende-gneiss|biotite-gneiss|serpentinite|greenstone",
      lith3
    ) ~ "Meta_ign",
    lith1 == "unconsolidated" ~ "Unconsolidated",
    TRUE ~ "Other_unknown"
  )
}

build_lithology_multihot <- function(nsalith) {
  classes <- feature_sets$lithology_multihot
  rows <- nsalith |>
    dplyr::filter(RANK %in% c("Major", "Indeterminate, major")) |>
    dplyr::mutate(lith_class = classify_lithology_rows(LITH1, LITH3)) |>
    dplyr::filter(!is.na(lith_class)) |>
    dplyr::select(NSACLASS, lith_class) |>
    dplyr::distinct()
  multihot <- rows |>
    dplyr::mutate(present = 1L) |>
    tidyr::pivot_wider(id_cols = NSACLASS, names_from = lith_class,
                       values_from = present, values_fill = 0L)
  for (class_name in classes) if (!class_name %in% names(multihot)) multihot[[class_name]] <- 0L
  list(table = multihot |> dplyr::select(NSACLASS, dplyr::all_of(classes)), classes = classes)
}

rasterize_lithology_multihot <- function(unit_code,
                                         unit_lookup,
                                         lithology_table,
                                         classes,
                                         lookup_path) {
  if (!inherits(unit_code, "SpatRaster") || terra::nlyr(unit_code) != 1L) {
    stop("unit_code must be a single-layer SpatRaster.", call. = FALSE)
  }
  unit_lookup <- as.data.frame(unit_lookup, stringsAsFactors = FALSE)
  assert_required_columns(
    unit_lookup, c("NSACLASS_code", "NSACLASS"), "geology unit lookup"
  )
  if (anyDuplicated(unit_lookup$NSACLASS_code) ||
      anyDuplicated(unit_lookup$NSACLASS)) {
    stop("Geology unit lookup contains duplicated codes or NSACLASS values.",
         call. = FALSE)
  }

  lithology_table <- as.data.frame(lithology_table, stringsAsFactors = FALSE)
  assert_required_columns(
    lithology_table, c("NSACLASS", classes), "lithology multi-hot lookup"
  )
  lithology_table$NSACLASS <- trimws(as.character(lithology_table$NSACLASS))
  if (anyDuplicated(lithology_table$NSACLASS)) {
    stop("Lithology lookup contains duplicated NSACLASS values.", call. = FALSE)
  }

  # Reuse the exact cell-centre geological-unit assignment already used by all
  # numeric geology predictors; no polygon rasterisation is repeated here.
  unit_lookup <- unit_lookup |>
    dplyr::mutate(NSACLASS = trimws(as.character(NSACLASS))) |>
    dplyr::select(NSACLASS_code, NSACLASS) |>
    dplyr::left_join(lithology_table, by = "NSACLASS")
  if (nrow(unit_lookup) == 0L) {
    stop("No valid NSACLASS values are available for lithology rasterisation.",
         call. = FALSE)
  }
  for (class_name in classes) {
    unit_lookup[[class_name]] <- dplyr::coalesce(
      as.integer(unit_lookup[[class_name]]), 0L
    )
  }
  class_matrix <- as.data.frame(unit_lookup[, classes, drop = FALSE])
  all_zero <- rowSums(class_matrix, na.rm = TRUE) == 0L
  unit_lookup$Other_unknown[all_zero] <- 1L

  class_values <- as.matrix(unit_lookup[, classes, drop = FALSE])
  storage.mode(class_values) <- "integer"
  stack <- terra::subst(
    unit_code,
    from = unit_lookup$NSACLASS_code,
    to = class_values
  )
  names(stack) <- classes

  readr::write_csv(
    unit_lookup |> dplyr::select(NSACLASS_code, NSACLASS, dplyr::all_of(classes)),
    lookup_path
  )
  stack
}

#-------------------------------------------------------------------------------
# Part 9. Fine terrain and hydrology for V4 --------

# 15) Fine-resolution input helpers

read_first_line_layer <- function(gpkg_path, target_crs) {
  assert_files_exist(gpkg_path, "3DHP channel")
  for (layer_name in sf::st_layers(gpkg_path)$name) {
    candidate <- tryCatch(sf::st_read(gpkg_path, layer = layer_name, quiet = TRUE),
                          error = function(e) NULL)
    if (is.null(candidate) || !inherits(candidate, "sf")) next
    if (any(grepl("LINESTRING", as.character(sf::st_geometry_type(candidate))))) {
      return(sf::st_transform(candidate, target_crs))
    }
  }
  stop("No LINESTRING/MULTILINESTRING layer found in: ", gpkg_path, call. = FALSE)
}

odd_window_cells <- function(scale_m, cell_size_m) {
  cells <- max(3L, as.integer(round(scale_m / cell_size_m)))
  if (cells %% 2L == 0L) cells <- cells + 1L
  cells
}

align_fine_feature <- function(raster, template, name) {
  # Area-weighted means preserve the interpretation of continuous 60-m
  # derivatives when they are transferred to the common 1-km support.
  aligned <- terra::resample(raster, template, method = "average")
  names(aligned) <- name
  aligned
}

# 16) Derive and aggregate V4 terrain/hydrology covariates

prepare_v4_fine_terrain <- function(cfg, template, inputs = cfg$inputs$v4) {
  assert_files_exist(unname(unlist(inputs)), "V4 input")

  dem <- terra::rast(inputs$dem)
  expected_crs <- paste0("EPSG:", cfg$crs$projected)
  if (!isTRUE(terra::same.crs(dem, expected_crs))) {
    stop("3DEP DEM must use EPSG:", cfg$crs$projected, ".", call. = FALSE)
  }
  context_extent <- terra::ext(
    terra::xmin(template) - cfg$hydrology$context_buffer_m,
    terra::xmax(template) + cfg$hydrology$context_buffer_m,
    terra::ymin(template) - cfg$hydrology$context_buffer_m,
    terra::ymax(template) + cfg$hydrology$context_buffer_m
  )
  dem_buffered <- terra::crop(dem, context_extent, snap = "out")
  names(dem_buffered) <- "elevation_60m"
  dem_extent <- terra::ext(dem_buffered)
  cell_size <- mean(terra::res(dem_buffered))
  tolerance <- max(terra::res(dem_buffered)) * 1.5
  contains_context <- terra::xmin(dem_extent) <= terra::xmin(context_extent) + tolerance &&
    terra::xmax(dem_extent) >= terra::xmax(context_extent) - tolerance &&
    terra::ymin(dem_extent) <= terra::ymin(context_extent) + tolerance &&
    terra::ymax(dem_extent) >= terra::ymax(context_extent) - tolerance
  if (!contains_context) {
    stop(
      "3DEP DEM does not cover the required ", cfg$hydrology$context_buffer_m / 1000,
      " km context around the final template.", call. = FALSE
    )
  }

  channels <- read_first_line_layer(inputs$channels, cfg$crs$projected)
  channels <- suppressWarnings(sf::st_crop(channels, sf::st_bbox(c(
    xmin = terra::xmin(dem_buffered), xmax = terra::xmax(dem_buffered),
    ymin = terra::ymin(dem_buffered), ymax = terra::ymax(dem_buffered)
  ), crs = cfg$crs$projected)))
  if (nrow(channels) == 0L) stop("No 3DHP channels intersect buffered DEM.", call. = FALSE)
  channels$stream_value <- 1L
  stream_raster <- terra::rasterize(
    terra::vect(channels[, "stream_value"]), dem_buffered,
    field = "stream_value", background = NA, touches = TRUE
  )

  # The retained V4 variables use only the supplied DEM and mapped channels.
  # No flow conditioning, channel burning, TWI or HAND derivation is required.
  slope_radians <- terra::terrain(dem_buffered, v = "slope", unit = "radians", neighbors = 8)
  slope_degrees <- slope_radians * 180 / pi
  window_1000m <- odd_window_cells(1000, cell_size)
  tpi_weights <- matrix(1, window_1000m, window_1000m)
  tpi_weights[ceiling(window_1000m / 2), ceiling(window_1000m / 2)] <- NA
  mean_1000m <- terra::focal(
    dem_buffered, w = tpi_weights, fun = "mean", na.rm = TRUE
  )
  tpi_1000m <- dem_buffered - mean_1000m
  # This is a 1-km moving-neighbourhood elevation SD, subsequently averaged
  # within each 1-km prediction cell; it is not a direct within-cell SD.
  elevation_sd_1km <- terra::focal(
    dem_buffered, w = matrix(1, window_1000m, window_1000m),
    fun = "sd", na.rm = TRUE
  )
  stream_presence <- terra::ifel(stream_raster > 0, 1, NA)
  distance_to_drainage <- terra::distance(stream_presence) / 1000

  c(
    align_fine_feature(dem_buffered, template, "elev_60m_mean"),
    align_fine_feature(slope_degrees, template, "slope_60m_mean"),
    align_fine_feature(tpi_1000m, template, "TPI_1000m"),
    align_fine_feature(elevation_sd_1km, template, "elev_sd_1km"),
    align_fine_feature(distance_to_drainage, template, "dist_drainage_km")
  )
}

#-------------------------------------------------------------------------------
# Part 10. Staged model-input preparation and loading --------

# 17) Clean and audit AGDB4 once; retain a small internal recovery point.

prepare_clean_layers_stage <- function(cfg, paths, force = FALSE) {
  if (!force && stage_outputs_exist(cfg, "clean")) {
    cached <- tryCatch(readRDS(paths$clean_layers), error = function(e) NULL)
    valid <- is.list(cached) && identical(names(cached), c("soil", "sediment", "rock")) &&
      all(vapply(cached, inherits, logical(1), what = "sf")) &&
      all(vapply(cached, nrow, integer(1)) > 0L)
    if (isTRUE(valid)) {
      message("Reusing cleaned AGDB4 layers.")
      return(list(data = cached, rebuilt = FALSE))
    }
  }

  message("Building cleaned AGDB4 checkpoint ...")
  if (file.exists(paths$clean_marker)) unlink(paths$clean_marker)
  cleaned <- prepare_fe_layers(cfg, cfg$processed_dir)
  write_soil_eda(cleaned$soil, cfg$processed_dir)
  saveRDS(cleaned, paths$clean_layers)
  check <- tryCatch(readRDS(paths$clean_layers), error = function(e) NULL)
  if (!is.list(check) || !identical(names(check), c("soil", "sediment", "rock"))) {
    stop("Clean-layer checkpoint could not be validated.", call. = FALSE)
  }
  write_stage_marker(paths$clean_marker, "clean_AGDB4")
  list(data = cleaned, rebuilt = TRUE)
}

# 18) Build the shared V1-V4 core: domain, geology support and V1 stack.

load_shared_core_cache <- function(cfg, paths, template) {
  if (!stage_outputs_exist(cfg, "shared_core")) return(NULL)
  tables <- tryCatch(readRDS(paths$shared_core_tables), error = function(e) NULL)
  if (!is.list(tables) ||
      !all(c("soil_model", "unit_lookup", "nsalith") %in% names(tables)) ||
      !inherits(tables$soil_model, "sf") || nrow(tables$soil_model) == 0L ||
      !all(c("sample_id", "Fe_pct", "rock_source", "sed_source") %in%
           names(tables$soil_model)) ||
      !is.data.frame(tables$unit_lookup) ||
      !all(c("NSACLASS", "NSACLASS_code") %in% names(tables$unit_lookup)) ||
      !is.data.frame(tables$nsalith) ||
      !all(c("NSACLASS", "LITH1", "LITH3", "RANK") %in%
           names(tables$nsalith))) {
    return(NULL)
  }
  unit_code <- read_valid_raster(paths$unit_code, "NSACLASS_code", template)
  domain_mask <- read_valid_raster(paths$domain_mask, "domain_mask", template)
  v1_stack <- read_valid_raster(paths$v1_stack, feature_sets$V1, template)
  if (is.null(unit_code) || is.null(domain_mask) || is.null(v1_stack)) return(NULL)
  list(
    soil_model = tables$soil_model,
    unit_lookup = tables$unit_lookup,
    nsalith = tables$nsalith,
    unit_code = unit_code,
    domain_mask = domain_mask,
    v1_stack = v1_stack,
    rebuilt = FALSE
  )
}

prepare_shared_core <- function(cfg, paths, force = FALSE) {
  framework <- create_project_template(cfg)
  template <- framework$template
  if (!force) {
    cached <- load_shared_core_cache(cfg, paths, template)
    if (!is.null(cached)) {
      message("Reusing true V1-V4 shared core.")
      return(cached)
    }
  }

  message("Building true V1-V4 shared core and V1 stack ...")
  if (file.exists(paths$shared_core_marker)) unlink(paths$shared_core_marker)
  clean_stage <- prepare_clean_layers_stage(cfg, paths, force = force)
  terrain <- prepare_coarse_terrain(cfg, template)
  geology <- load_geology_layers(cfg, framework$bbox_polygon)
  geology_covariates <- build_geology_covariates(
    clean_stage$data, geology, cfg, template, cfg$processed_dir
  )
  v1_stack <- c(
    geology_covariates$geology_stack,
    terrain[[c("elev", "slope", "eastness", "northness")]]
  )
  if (!setequal(names(v1_stack), feature_sets$V1)) {
    stop("V1 covariates do not match the fixed V1 feature set.", call. = FALSE)
  }
  v1_stack <- v1_stack[[feature_sets$V1]]
  safe_write_raster(v1_stack, paths$v1_stack)
  saveRDS(
    list(
      soil_model = geology_covariates$soil_model,
      unit_lookup = geology_covariates$unit_lookup,
      nsalith = geology$nsalith
    ),
    paths$shared_core_tables
  )
  readr::write_csv(
    as.data.frame(geology_covariates$unit_lookup),
    file.path(cfg$processed_dir, "geology_NSACLASS_support_lookup.csv")
  )

  if (is.null(read_valid_raster(paths$unit_code, "NSACLASS_code", template)) ||
      is.null(read_valid_raster(paths$domain_mask, "domain_mask", template)) ||
      is.null(read_valid_raster(paths$v1_stack, feature_sets$V1, template))) {
    stop("Shared-core raster validation failed.", call. = FALSE)
  }
  write_stage_marker(paths$shared_core_marker, "shared_core_V1")
  core <- load_shared_core_cache(cfg, paths, template)
  if (is.null(core)) stop("Shared-core checkpoint could not be reopened.", call. = FALSE)
  core$rebuilt <- TRUE
  core
}

# 19) Cache each version-specific predictor increment independently.

prepare_v2_increment <- function(cfg, paths, template, force = FALSE) {
  expected <- setdiff(feature_sets$V2, feature_sets$V1)
  if (!force && stage_outputs_exist(cfg, "v2")) {
    cached <- read_valid_raster(paths$v2_increment, expected, template)
    if (!is.null(cached)) {
      message("Reusing V2 climate/permafrost increment.")
      return(list(stack = cached, rebuilt = FALSE))
    }
  }
  message("Building V2 climate/permafrost increment ...")
  if (file.exists(paths$v2_marker)) unlink(paths$v2_marker)
  stack <- prepare_v2_environmental_covariates(cfg, template, cfg$processed_dir)
  if (!setequal(names(stack), expected)) {
    stop("V2 increment does not match the fixed V2 feature addition.", call. = FALSE)
  }
  stack <- stack[[expected]]
  safe_write_raster(stack, paths$v2_increment)
  if (is.null(read_valid_raster(paths$v2_increment, expected, template))) {
    stop("V2 increment validation failed.", call. = FALSE)
  }
  write_stage_marker(paths$v2_marker, "V2_increment")
  list(stack = terra::rast(paths$v2_increment), rebuilt = TRUE)
}

prepare_v3_increment <- function(cfg, paths, template, core, force = FALSE) {
  expected <- setdiff(feature_sets$V3, feature_sets$V2)
  if (!force && stage_outputs_exist(cfg, "v3")) {
    cached <- read_valid_raster(paths$v3_increment, expected, template)
    if (!is.null(cached)) {
      message("Reusing V3 lithology/ratio/TPI increment.")
      return(list(stack = cached, rebuilt = FALSE))
    }
  }
  message("Building V3 lithology/ratio/TPI increment ...")
  if (file.exists(paths$v3_marker)) unlink(paths$v3_marker)
  lithology <- build_lithology_multihot(core$nsalith)
  lithology_stack <- rasterize_lithology_multihot(
    core$unit_code,
    core$unit_lookup,
    lithology$table,
    lithology$classes,
    file.path(cfg$processed_dir, "lithology_NSACLASS_lookup.csv")
  )
  # Positive retained Fe values permit an unadjusted ratio. Structural absence
  # remains zero and is explicitly represented by the two support flags.
  ratio <- terra::ifel(
    core$v1_stack[["rock_support_missing"]] == 1 |
      core$v1_stack[["sed_support_missing"]] == 1,
    0,
    core$v1_stack[["rock_Fe_final"]] /
      core$v1_stack[["sed_Fe_final"]]
  )
  names(ratio) <- "rock_sed_ratio"
  coarse_tpi <- terra::terrain(core$v1_stack[["elev"]], v = "TPI")
  names(coarse_tpi) <- "TPI"
  stack <- c(lithology_stack, ratio, coarse_tpi)
  if (!setequal(names(stack), expected)) {
    stop("V3 increment does not match the fixed V3 feature addition.", call. = FALSE)
  }
  stack <- stack[[expected]]
  safe_write_raster(stack, paths$v3_increment)
  if (is.null(read_valid_raster(paths$v3_increment, expected, template))) {
    stop("V3 increment validation failed.", call. = FALSE)
  }
  write_stage_marker(paths$v3_marker, "V3_increment")
  list(stack = terra::rast(paths$v3_increment), rebuilt = TRUE)
}

prepare_v4_increment <- function(cfg, paths, template, force = FALSE) {
  expected <- setdiff(feature_sets$V4, feature_sets$V3)
  if (!force && stage_outputs_exist(cfg, "v4")) {
    cached <- read_valid_raster(paths$v4_increment, expected, template)
    if (!is.null(cached)) {
      message("Reusing V4 fine-terrain/drainage increment.")
      return(list(stack = cached, rebuilt = FALSE))
    }
  }
  message("Building V4 fine-terrain/drainage increment ...")
  if (file.exists(paths$v4_marker)) unlink(paths$v4_marker)
  stack <- prepare_v4_fine_terrain(cfg, template, cfg$inputs$v4)
  if (!setequal(names(stack), expected)) {
    stop("V4 increment does not match the fixed V4 feature addition.", call. = FALSE)
  }
  stack <- stack[[expected]]
  safe_write_raster(stack, paths$v4_increment)
  if (is.null(read_valid_raster(paths$v4_increment, expected, template))) {
    stop("V4 increment validation failed.", call. = FALSE)
  }
  write_stage_marker(paths$v4_marker, "V4_increment")
  list(stack = terra::rast(paths$v4_increment), rebuilt = TRUE)
}

# 20) Assemble the final common cohort, then create one shared fold assignment.

final_inputs_are_valid <- function(cfg, paths, template) {
  if (!processed_outputs_exist(cfg)) return(FALSE)
  stack <- read_valid_raster(paths$predictor_stack, feature_sets$V4, template)
  master <- tryCatch(readRDS(paths$master_table), error = function(e) NULL)
  ids <- tryCatch(as.character(readRDS(paths$comparison_ids)), error = function(e) NULL)
  if (is.null(stack) || !is.data.frame(master) || is.null(ids) ||
      length(ids) == 0L || anyNA(ids) || anyDuplicated(ids)) {
    return(FALSE)
  }
  selected <- master[master$sample_id %in% ids, , drop = FALSE]
  if (!setequal(as.character(selected$sample_id), ids)) return(FALSE)
  isTRUE(tryCatch({
    assert_required_columns(
      selected,
      c(
        "sample_id", "Fe_pct", "Fe_log10", "grid_id", "grid_x", "grid_y",
        feature_sets$V4
      ),
      "cached final comparison table"
    )
    if (nrow(selected) != length(ids) ||
        !all(stats::complete.cases(
          selected[, c("Fe_log10", feature_sets$V4), drop = FALSE]
        ))) {
      stop("Cached final comparison table is incomplete.", call. = FALSE)
    }
    validate_saved_fold_metadata(
      selected,
      paths$fold_metadata,
      k = cfg$cv$k,
      block_size_m = cfg$cv$block_size_m,
      iterations = cfg$cv$iterations,
      seed = cfg$seed,
      projected_crs = cfg$crs$projected,
      resolution_m = cfg$resolution_m
    )
    folded <- attach_shared_folds(selected, paths$folds, k = cfg$cv$k)
    setequal(as.character(folded$sample_id), ids)
  }, error = function(e) FALSE))
}

assemble_final_comparison_inputs <- function(cfg,
                                             paths,
                                             template,
                                             core,
                                             v2,
                                             v3,
                                             v4,
                                             force = FALSE) {
  if (!force && final_inputs_are_valid(cfg, paths, template)) {
    message("Reusing final V1-V4 comparison inputs and folds.")
    return(invisible(paths))
  }
  message("Assembling the final common cohort and one shared fold design ...")
  if (file.exists(paths$completion_marker)) unlink(paths$completion_marker)

  predictor_stack <- c(core$v1_stack, v2$stack, v3$stack, v4$stack)
  if (anyDuplicated(names(predictor_stack)) ||
      !setequal(names(predictor_stack), feature_sets$V4)) {
    stop("The final predictor union does not match the V4 feature set.", call. = FALSE)
  }
  predictor_stack <- predictor_stack[[feature_sets$V4]]
  safe_write_raster(predictor_stack, paths$predictor_stack)
  if (is.null(read_valid_raster(
    paths$predictor_stack, feature_sets$V4, template
  ))) {
    stop("Final predictor-stack validation failed.", call. = FALSE)
  }

  master <- extract_model_table(
    core$soil_model, predictor_stack, feature_sets$V4, "Fe_log10"
  )
  master$domain_mask <- terra::extract(
    core$domain_mask, terra::vect(core$soil_model), ID = FALSE
  )[["domain_mask"]]
  master <- master |> dplyr::arrange(sample_id)

  required_v4 <- c("Fe_log10", feature_sets$V4)
  cohort_audit <- master |>
    dplyr::mutate(
      valid_land_domain = dplyr::coalesce(domain_mask == 1, FALSE),
      complete_required_v4 = stats::complete.cases(dplyr::across(
        dplyr::all_of(required_v4)
      )),
      comparison_included = valid_land_domain & complete_required_v4
    )
  readr::write_csv(
    cohort_audit |>
      dplyr::select(
        sample_id, lon, lat, valid_land_domain, complete_required_v4,
        comparison_included, rock_support_missing, sed_support_missing,
        rock_source, sed_source
      ),
    file.path(cfg$processed_dir, "comparison_cohort_audit.csv")
  )
  readr::write_csv(
    cohort_audit |>
      dplyr::summarise(
        n_primary_soil = dplyr::n(),
        n_valid_land_domain = sum(valid_land_domain),
        n_complete_required_v4 = sum(complete_required_v4),
        n_comparison = sum(comparison_included),
        n_structural_rock_rescued = sum(rock_support_missing == 1L, na.rm = TRUE),
        n_structural_sediment_rescued = sum(sed_support_missing == 1L, na.rm = TRUE)
      ),
    file.path(cfg$processed_dir, "comparison_cohort_summary.csv")
  )

  comparison_metadata <- cohort_audit |>
    dplyr::filter(comparison_included)
  readr::write_csv(
    comparison_metadata |>
      dplyr::summarise(
        n = dplyr::n(),
        n_collection_date = sum(!is.na(date_collect)),
        collection_date_pct = 100 * mean(!is.na(date_collect)),
        earliest_collection = if (all(is.na(date_collect))) {
          as.Date(NA)
        } else {
          min(date_collect, na.rm = TRUE)
        },
        latest_collection = if (all(is.na(date_collect))) {
          as.Date(NA)
        } else {
          max(date_collect, na.rm = TRUE)
        },
        n_depth_raw = sum(!is.na(depth) & nzchar(depth)),
        n_depth_usable = sum(is.finite(depth_midpoint_cm)),
        n_horizon = sum(!is.na(horizon) & nzchar(horizon)),
        n_sample_zone = sum(!is.na(sample_zone) & nzchar(sample_zone))
      ),
    file.path(cfg$processed_dir, "comparison_cohort_metadata_summary.csv")
  )
  readr::write_csv(
    comparison_metadata |>
      dplyr::count(depth_class, depth_parse_status, name = "n") |>
      dplyr::arrange(depth_class, depth_parse_status),
    file.path(cfg$processed_dir, "comparison_cohort_depth_summary.csv")
  )

  comparison_ids <- comparison_metadata |>
    dplyr::pull(sample_id) |>
    as.character() |>
    unique() |>
    sort()
  if (length(comparison_ids) < 100L) {
    stop("Fewer than 100 samples remain in the common V1-V4 comparison cohort.",
         call. = FALSE)
  }
  master$analysis_id <- as.integer(match(master$sample_id, comparison_ids))
  saveRDS(master, paths$master_table)
  saveRDS(comparison_ids, paths$comparison_ids)
  readr::write_csv(
    data.frame(sample_id = comparison_ids),
    file.path(cfg$processed_dir, "comparison_sample_ids.csv")
  )

  comparison_table <- master |>
    dplyr::filter(sample_id %in% comparison_ids) |>
    dplyr::arrange(match(sample_id, comparison_ids))
  create_and_save_shared_folds(
    comparison_table,
    paths$folds,
    metadata_path = paths$fold_metadata,
    k = cfg$cv$k,
    block_size_m = cfg$cv$block_size_m,
    iterations = cfg$cv$iterations,
    seed = cfg$seed,
    projected_crs = cfg$crs$projected,
    resolution_m = cfg$resolution_m
  )
  validate_saved_fold_metadata(
    comparison_table,
    paths$fold_metadata,
    k = cfg$cv$k,
    block_size_m = cfg$cv$block_size_m,
    iterations = cfg$cv$iterations,
    seed = cfg$seed,
    projected_crs = cfg$crs$projected,
    resolution_m = cfg$resolution_m
  )
  record_package_versions(
    file.path(cfg$processed_dir, "package_versions.csv"),
    extra = list(R = R.version.string)
  )
  writeLines(
    c(
      paste("Stage: final_V1-V4_comparison_inputs"),
      paste("Completed:", format(Sys.time(), tz = "UTC", usetz = TRUE)),
      "Rebuild with: Rscript preprocess_all.R --force"
    ),
    paths$completion_marker
  )
  invisible(paths)
}

# 21) Orchestrate all cached stages without mixing their scientific roles.

prepare_all_model_inputs <- function(cfg, force = FALSE) {
  paths <- processed_output_paths(cfg)
  dir.create(cfg$processed_dir, recursive = TRUE, showWarnings = FALSE)
  core <- prepare_shared_core(cfg, paths, force = force)
  template <- create_project_template(cfg)$template

  # V2 and V4 depend on the fixed template, whereas V3 also depends on the
  # geology core. A repaired core therefore invalidates V3 and the final table,
  # but not otherwise valid climate or fine-terrain caches.
  v2 <- prepare_v2_increment(cfg, paths, template, force = force)
  v3 <- prepare_v3_increment(
    cfg, paths, template, core,
    force = force || isTRUE(core$rebuilt)
  )
  v4 <- prepare_v4_increment(cfg, paths, template, force = force)
  final_force <- force || isTRUE(core$rebuilt) || isTRUE(v2$rebuilt) ||
    isTRUE(v3$rebuilt) || isTRUE(v4$rebuilt)
  assemble_final_comparison_inputs(
    cfg, paths, template, core, v2, v3, v4, force = final_force
  )
}

# 22) Load the common comparison cohort and one fixed predictor subset

load_version_inputs <- function(version, cfg) {
  if (!(version %in% paste0("V", 1:4))) {
    stop("Unknown model version: ", version, call. = FALSE)
  }
  paths <- processed_output_paths(cfg)
  assert_files_exist(
    c(
      paths$master_table,
      paths$comparison_ids,
      paths$folds,
      paths$fold_metadata,
      paths$completion_marker,
      paths$domain_mask,
      paths$predictor_stack
    ),
    paste(version, "processed input")
  )
  master <- readRDS(paths$master_table)
  comparison_ids <- as.character(readRDS(paths$comparison_ids))
  data <- master |>
    dplyr::filter(sample_id %in% comparison_ids) |>
    dplyr::arrange(match(sample_id, comparison_ids))
  validate_saved_fold_metadata(
    data,
    paths$fold_metadata,
    k = cfg$cv$k,
    block_size_m = cfg$cv$block_size_m,
    iterations = cfg$cv$iterations,
    seed = cfg$seed,
    projected_crs = cfg$crs$projected,
    resolution_m = cfg$resolution_m
  )
  data <- attach_shared_folds(data, paths$folds, k = cfg$cv$k)
  features <- feature_sets[[version]]
  assert_required_columns(data, c("sample_id", "Fe_pct", "Fe_log10", features),
                          paste(version, "processed model table"))
  if (!all(stats::complete.cases(data[, c("Fe_log10", features), drop = FALSE]))) {
    stop(version, " common comparison cohort contains missing values.", call. = FALSE)
  }
  stack_path <- paths$predictor_stack
  stack <- terra::rast(stack_path)
  missing_stack_features <- setdiff(features, names(stack))
  if (length(missing_stack_features) > 0L) {
    stop(
      version, " predictor layer(s) missing from the shared stack: ",
      paste(missing_stack_features, collapse = ", "),
      call. = FALSE
    )
  }
  list(
    data = data,
    features = features,
    stack = stack[[features]],
    stack_path = stack_path,
    domain_mask = terra::rast(paths$domain_mask),
    paths = paths
  )
}
