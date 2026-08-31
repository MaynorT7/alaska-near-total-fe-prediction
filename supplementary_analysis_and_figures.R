# ==============================================================================
# Combined dissertation audit: Methodology figures, Results and Discussion
# ==============================================================================
# Renders to HTML with knitr::spin():
#   knitr::spin("supplementary_analysis_and_figures.R", knit = FALSE)
#   rmarkdown::render("supplementary_analysis_and_figures.Rmd")
#
# Sections: (1) Methodology figures 3.1-3.3, (2) Chapter 4 Results audit with
# publication-quality Figures 4.1-4.5, (3) Discussion data verification and
# (4) Appendix B supplementary model diagnostics.
#
# The project root is resolved from an explicit option/environment override,
# the active script, the working directory, or a nearby sibling directory.
# Outputs are written to supplementary/Methodology/, supplementary/Results/
# supplementary/Discussion/ and supplementary/Appendix_B/.
# ==============================================================================
#
#' # Combined dissertation audit
#' Methodology figures, Chapter 4 Results audit and Discussion data verification.

# ==============================================================================
# Shared project setup
# ==============================================================================
#' ## Project setup

# This script may be stored outside the repository for dissertation drafting.
# No drive letter or user-specific absolute path is embedded here.

is_project_root <- function(path) {
  is.character(path) && length(path) == 1L && dir.exists(path) &&
    file.exists(file.path(path, "R", "model_config.R")) &&
    all(dir.exists(file.path(path, paste0("Model_V", 1:4))))
}

normalise_existing_path <- function(path) {
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

active_script_path <- function() {
  script_arg <- grep(
    "^--file=",
    commandArgs(trailingOnly = FALSE),
    value = TRUE
  )
  if (length(script_arg) > 0L) {
    return(normalise_existing_path(sub("^--file=", "", script_arg[[1]])))
  }
  
  frame_paths <- vapply(
    sys.frames(),
    function(frame) {
      value <- frame$ofile
      if (is.null(value) || length(value) == 0L) NA_character_ else as.character(value[[1]])
    },
    character(1)
  )
  frame_paths <- frame_paths[!is.na(frame_paths) & file.exists(frame_paths)]
  if (length(frame_paths) > 0L) {
    return(normalise_existing_path(frame_paths[[length(frame_paths)]]))
  }
  
  if (
    interactive() &&
    requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()
  ) {
    editor_path <- tryCatch(
      rstudioapi::getSourceEditorContext()$path,
      error = function(error) ""
    )
    if (nzchar(editor_path) && file.exists(editor_path)) {
      return(normalise_existing_path(editor_path))
    }
  }
  
  NA_character_
}

ancestor_paths <- function(start, max_levels = 4L) {
  current <- normalise_existing_path(start)
  output <- current
  for (index in seq_len(max_levels)) {
    parent <- dirname(current)
    if (identical(parent, current)) break
    output <- c(output, parent)
    current <- parent
  }
  unique(output)
}

find_direct_project_root <- function(start) {
  if (
    !is.character(start) || length(start) != 1L ||
    is.na(start) || !nzchar(start) || !file.exists(start)
  ) {
    return(character())
  }
  
  current <- normalise_existing_path(start)
  if (!dir.exists(current)) current <- dirname(current)
  
  repeat {
    if (is_project_root(current)) return(current)
    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }
  
  character()
}

resolve_project_root <- function() {
  override <- getOption(
    "alaska_fe.project_root",
    Sys.getenv("ALASKA_FE_PROJECT_ROOT", unset = "")
  )
  if (
    is.character(override) && length(override) == 1L &&
    !is.na(override) && nzchar(override)
  ) {
    override <- normalise_existing_path(override)
    if (!is_project_root(override)) {
      stop(
        "Configured Alaska Fe project root is invalid:\n  ", override,
        "\nExpected R/model_config.R and Model_V1/ ... Model_V4/.",
        call. = FALSE
      )
    }
    return(override)
  }
  
  script_path <- active_script_path()
  anchors <- unique(c(
    getwd(),
    if (!is.na(script_path)) dirname(script_path) else character()
  ))
  
  # Prefer a project root that directly contains the active script or working
  # directory. This makes the repository self-locating even when other valid
  # project copies exist nearby, without depending on the repository name.
  direct_matches <- unique(unlist(
    lapply(anchors, find_direct_project_root),
    use.names = FALSE
  ))
  if (length(direct_matches) == 1L) return(direct_matches[[1]])
  if (length(direct_matches) > 1L) {
    stop(
      "The active script and working directory resolve to different Alaska ",
      "Fe project roots:\n  ",
      paste(direct_matches, collapse = "\n  "),
      "\nSet options(alaska_fe.project_root = '...') for this R session ",
      "to select one.",
      call. = FALSE
    )
  }
  
  # Search nearby directories only when neither anchor belongs directly to a
  # valid project. Ambiguous sibling projects are never selected implicitly.
  ancestors <- unique(unlist(
    lapply(anchors, ancestor_paths),
    use.names = FALSE
  ))
  nearby_directories <- unique(unlist(
    lapply(
      ancestors,
      function(path) {
        tryCatch(
          list.dirs(path, full.names = TRUE, recursive = FALSE),
          error = function(error) character()
        )
      }
    ),
    use.names = FALSE
  ))
  candidates <- unique(c(ancestors, nearby_directories))
  matches <- candidates[vapply(candidates, is_project_root, logical(1))]
  matches <- unique(vapply(matches, normalise_existing_path, character(1)))
  
  if (length(matches) == 1L) return(matches[[1]])
  if (length(matches) > 1L) {
    stop(
      "Multiple Alaska Fe project roots were found:\n  ",
      paste(matches, collapse = "\n  "),
      "\nSet options(alaska_fe.project_root = '...') for this R session ",
      "to select one.",
      call. = FALSE
    )
  }
  stop(
    "Cannot locate the Alaska Fe project root from the active script, ",
    "working directory, or nearby parent/sibling directories.\n",
    "Set options(alaska_fe.project_root = 'path/to/Alaska_Fe_Prediction') ",
    "once, then source this script again.",
    call. = FALSE
  )
}

project_root <- resolve_project_root()
config_path <- file.path(project_root, "R", "model_config.R")
source(config_path)
source_project_modules(root = project_root)

# initialise_project() uses the working directory for root discovery. Change it
# only inside this helper and restore it even if initialisation fails.
initialise_at_project_root <- function() {
  previous_wd <- setwd(project_root)
  on.exit(setwd(previous_wd), add = TRUE)
  initialise_project("shared", resolution_m = 1000L)
}
cfg <- initialise_at_project_root()
rm(initialise_at_project_root)

project_paths <- processed_output_paths(cfg)

# Shared output directories under supplementary/
methodology_output_dir <- file.path(cfg$root, "supplementary", "Methodology")
results_output_dir     <- file.path(cfg$root, "supplementary", "Results")
discussion_output_dir  <- file.path(cfg$root, "supplementary", "Discussion")
appendix_b_output_dir  <- file.path(cfg$root, "supplementary", "Appendix_B")
dir.create(methodology_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(results_output_dir,     recursive = TRUE, showWarnings = FALSE)
dir.create(discussion_output_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(appendix_b_output_dir,  recursive = TRUE, showWarnings = FALSE)

# Derived paths reused by both Methodology and Results sections
ROOT <- cfg$root
PROC <- cfg$processed_dir
v3_output_dir <- file.path(cfg$root, "Model_V3", "outputs")

cat("Project root:", ROOT, "\n")
cat("Outputs: Methodology ->", methodology_output_dir, "\n")
cat("         Results     ->", results_output_dir, "\n")
cat("         Discussion  ->", discussion_output_dir, "\n")
cat("         Appendix B  ->", appendix_b_output_dir, "\n")


# ==============================================================================
# SECTION 1: METHODOLOGY FIGURES (Figures 3.1-3.3)
# ==============================================================================
#' # Methodology figures (Figures 3.1-3.3)
#' Figures 3.1 (analytical-sequence block diagram), 3.2 (spatial configuration)
#' and 3.3 (shared resampling framework) from Chapter 3.



pkgs <- c("ggplot2", "dplyr", "sf", "terra", "scales", "cowplot", "systemfonts")
miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss))
  stop("Install: ", paste(miss, collapse = ", "), call. = FALSE)

methodology_paths <- list(
  master    = file.path(PROC, "master_table.rds"),
  ids       = file.path(PROC, "comparison_sample_ids.rds"),
  folds     = file.path(PROC, "spatial_folds.rds"),
  mask      = file.path(PROC, "domain_mask_1000m.tif"),
  stack     = file.path(PROC, "predictor_stack_1000m.tif")
)
bad <- unlist(methodology_paths)[
  !file.exists(unlist(methodology_paths))
]
if (length(bad))
  stop("Run preprocess_all.R first. Missing:\n  ",
       paste(bad, collapse = "\n  "), call. = FALSE)

OUT_DIR <- methodology_output_dir
REF_DIR <- file.path(ROOT, "data", "reference", "figure_3_2")
dir.create(REF_DIR, recursive = TRUE, showWarnings = FALSE)


# ---- 2. Shared publication design -------------------------------------------

# Require the requested typeface; do not silently substitute a generic serif.
FONT <- "Times New Roman"
if (!FONT %in% systemfonts::system_fonts()$family)
  stop("Install Times New Roman before exporting the diagrams.", call. = FALSE)
if (.Platform$OS.type == "windows")
  grDevices::windowsFonts("Times New Roman" = grDevices::windowsFont(FONT))

CRS_GEO <- 4326L
CRS_PRJ <- 3338L
BBOX    <- c(lon_min = -165, lon_max = -156, lat_min = 60, lat_max = 63)

INK <- "gray10"

save_figure <- function(plot, stem, width_mm, height_mm) {
  ggplot2::ggsave(
    file.path(OUT_DIR, paste0(stem, ".pdf")),
    plot, device = grDevices::cairo_pdf,
    width = width_mm, height = height_mm, units = "mm", bg = "white"
  )
  ggplot2::ggsave(
    file.path(OUT_DIR, paste0(stem, ".tiff")),
    plot, device = "tiff", dpi = 600, compression = "lzw",
    width = width_mm, height = height_mm, units = "mm", bg = "white"
  )
}


# ---- 3. Load and validate the fixed Methodology data ------------------------

comp_ids <- sort(as.character(readRDS(methodology_paths$ids)))
n_obs    <- length(comp_ids)
sv_folds <- readRDS(methodology_paths$folds)
need <- c("sample_id", "spatial_fold", "random_fold",
          "observation_random_fold")
if (!all(need %in% names(sv_folds)) ||
    !setequal(as.character(sv_folds$sample_id), comp_ids))
  stop("spatial_folds.rds does not match the comparison cohort.", call. = FALSE)


# ---- 4. Figure 3.1: analytical-sequence block diagram -----------------------

# At the exported size: 12-pt bold headings, 11-pt body text, 13.2-pt leading,
# 5-pt heading/body gap and at least 12-pt internal padding.
# Abbreviations belong in the manuscript caption: RF, OOF, OOB, PF, MAAT,
# MAP and TPI. This script draws the analytical sequence; it does not refit it.

save_grid_figure <- function(draw, stem, width_mm, height_mm) {
  render <- function(extension) {
    path <- file.path(OUT_DIR, paste0(stem, ".", extension))
    if (extension == "pdf") {
      grDevices::cairo_pdf(
        path, width = width_mm / 25.4, height = height_mm / 25.4,
        family = FONT, bg = "white"
      )
    } else {
      grDevices::tiff(
        path, width = width_mm, height = height_mm, units = "mm",
        res = 600, compression = "lzw", bg = "white"
      )
    }
    on.exit(grDevices::dev.off(), add = TRUE)
    grid::grid.newpage()
    draw()
  }
  render("pdf")
  render("tiff")
}

draw_figure_3_1 <- function() {
  width <- 180 * 72 / 25.4
  height <- 185 * 72 / 25.4
  margin <- 12
  padding <- 12
  heading_size <- 12
  body_size <- 11
  heading_gap <- 5
  leading <- 13.2
  shaft <- 14
  annotation_band <- 36
  usable <- width - 2 * margin
  centre <- width / 2
  gp_line <- grid::gpar(col = INK, fill = INK, lwd = 0.8, lineend = "butt")
  
  # Measure text on the active export device, using the requested font.
  node <- function(id, title, body, w) {
    title_grob <- grid::textGrob(
      title, gp = grid::gpar(
        fontfamily = FONT, fontsize = heading_size, fontface = "bold", col = INK
      )
    )
    body_grobs <- lapply(body, function(label) grid::textGrob(
      label, gp = grid::gpar(fontfamily = FONT, fontsize = body_size, col = INK)
    ))
    title_h <- grid::convertHeight(
      grid::grobHeight(title_grob), "bigpts", valueOnly = TRUE
    )
    body_h <- vapply(body_grobs, function(g) grid::convertHeight(
      grid::grobHeight(g), "bigpts", valueOnly = TRUE
    ), numeric(1))
    text_w <- vapply(c(list(title_grob), body_grobs), function(g) {
      grid::convertWidth(grid::grobWidth(g), "bigpts", valueOnly = TRUE)
    }, numeric(1))
    if (max(text_w) > w - 2 * padding)
      stop("Figure 3.1 text exceeds its box: ", id, call. = FALSE)
    list(
      id = id, w = w, h = 2 * padding + title_h + heading_gap +
        (length(body) - 1) * leading + max(body_h),
      title = title_grob, body = body_grobs, title_h = title_h
    )
  }
  
  p <- c(V1 = 14L, V2 = 17L, V3 = 27L, V4 = 32L)
  mt <- floor(sqrt(p))
  model_w <- (usable - 3 * 12) / 4
  model_titles <- c("V1: baseline", "V2: environment", "V3: geology",
                    "V4: fine terrain")
  model_bodies <- list(
    c("Rock/sediment Fe", "+ coarse terrain"),
    c("V1 + PF", "+ MAAT + MAP"),
    c("V2 + lithology", "+ Fe ratio + TPI"),
    c("V3 + fine terrain", "+ drainage")
  )
  models <- lapply(seq_along(p), function(i) node(
    names(p)[i], model_titles[i],
    c(as.list(model_bodies[[i]]), list(
      bquote(list(p == .(p[i]), m[plain(try)] == .(mt[i])))
    )), model_w
  ))
  inputs <- list(
    node("response", "Soil response", list(
      "Near-total Fe (wt%)",
      quote(paste(
        "QC response: ", log[10], "(", plain(Fe)[plain(wt) * "%"], ")"
      ))
    ), (usable - 24) / 2),
    node("predictors", "Predictor inputs", list(
      "Rock/sediment Fe; terrain",
      "Climate, PF, lithology, drainage",
      "Aligned 1-km predictors"
    ), (usable - 24) / 2)
  )
  locked <- node("locked", "Shared comparison", list(
    paste0("V4-complete cohort (n = ", n_obs, ")"),
    "3 saved 5-fold CV partitions"
  ), 330)
  evaluation <- node("evaluation", "Paired model evaluation", list(
    "Same held-out observations across V1-V4",
    "Spatial CV: primary; random CVs + OOB: diagnostics"
  ), usable - 24)
  selection <- node("selection", "Pre-specified selection", list(
    quote(paste("Maximum pooled spatial OOF ", R^2)),
    quote(paste(
      "on the ", log[10], "(", plain(Fe)[plain(wt) * "%"], ") scale"
    ))
  ), 196)
  final <- node("final", "Selected-model map", list(
    "Refit cohort; Duan mean correction",
    "Predict within valid 1-km domain",
    "Mask unseen joint support states"
  ), usable - selection$w - shaft)
  
  row_h <- c(
    max(vapply(inputs, function(b) b$h, numeric(1))),
    locked$h, max(vapply(models, function(b) b$h, numeric(1))),
    evaluation$h, max(selection$h, final$h)
  )
  gaps <- c(2 * shaft, annotation_band + shaft, 2 * shaft, 2 * shaft)
  total_h <- sum(row_h) + sum(gaps)
  stopifnot(total_h <= height - 2 * margin)
  row_top <- (height + total_h) / 2 -
    c(0, cumsum(head(row_h, -1) + gaps))
  row_bottom <- row_top - row_h
  
  place <- function(b, x, row) {
    b$x <- x
    b$top <- row_top[row]
    b$bottom <- row_bottom[row]
    b
  }
  input_x <- c(margin + inputs[[1]]$w / 2,
               width - margin - inputs[[2]]$w / 2)
  model_x <- margin + model_w / 2 + (0:3) * (model_w + 12)
  inputs <- lapply(1:2, function(i) place(inputs[[i]], input_x[i], 1))
  locked <- place(locked, centre, 2)
  models <- lapply(1:4, function(i) place(models[[i]], model_x[i], 3))
  evaluation <- place(evaluation, centre, 4)
  selection <- place(selection, margin + selection$w / 2, 5)
  final <- place(final, width - margin - final$w / 2, 5)
  
  segment <- function(x0, y0, x1, y1, arrow = FALSE) {
    grid::grid.segments(
      x0, y0, x1, y1, default.units = "bigpts", gp = gp_line,
      arrow = if (arrow) grid::arrow(
        type = "closed", angle = 22, length = grid::unit(3.5, "bigpts")
      ) else NULL
    )
  }
  join_input <- row_bottom[1] - shaft
  join_above <- row_top[3] + shaft
  join_below <- row_bottom[3] - shaft
  join_select <- row_top[5] + shaft
  
  segment(input_x, row_bottom[1], input_x, join_input)
  segment(input_x[1], join_input, input_x[2], join_input)
  segment(centre, join_input, centre, row_top[2], TRUE)
  segment(centre, row_bottom[2], centre, join_above)
  segment(model_x[1], join_above, model_x[4], join_above)
  segment(model_x, join_above, model_x, row_top[3], TRUE)
  segment(model_x, row_bottom[3], model_x, join_below)
  segment(model_x[1], join_below, model_x[4], join_below)
  segment(centre, join_below, centre, row_top[4], TRUE)
  segment(centre, row_bottom[4], centre, join_select)
  segment(centre, join_select, selection$x, join_select)
  segment(selection$x, join_select, selection$x, row_top[5], TRUE)
  segment(
    selection$x + selection$w / 2, mean(c(selection$top, selection$bottom)),
    final$x - final$w / 2, mean(c(final$top, final$bottom)), TRUE
  )
  
  draw_node <- function(b) {
    text_top <- if (b$id == "selection") {
      (b$top + b$bottom + b$h) / 2
    } else b$top
    grid::grid.roundrect(
      b$x, mean(c(b$top, b$bottom)), b$w, b$top - b$bottom,
      r = grid::unit(4, "bigpts"), default.units = "bigpts",
      gp = grid::gpar(fill = "white", col = INK, lwd = 0.8)
    )
    grid::grid.draw(grid::editGrob(
      b$title, x = grid::unit(b$x, "bigpts"),
      y = grid::unit(text_top - padding, "bigpts"), just = c("centre", "top")
    ))
    body_top <- text_top - padding - b$title_h - heading_gap
    for (i in seq_along(b$body)) grid::grid.draw(grid::editGrob(
      b$body[[i]], x = grid::unit(b$x, "bigpts"),
      y = grid::unit(body_top - (i - 1) * leading, "bigpts"),
      just = c("centre", "top")
    ))
  }
  invisible(lapply(
    c(inputs, list(locked), models, list(evaluation, selection, final)),
    draw_node
  ))
  
  # Each annotation is centred in its half-band, clear of the central line.
  annotation_y <- mean(c(row_bottom[2], join_above))
  notes <- list(
    "4 cumulative, pre-specified versions",
    quote(paste(
      "RF: 500 trees; min. node = 5; ",
      m[plain(try)], " = ", group(lfloor, sqrt(p), rfloor)
    ))
  )
  note_x <- c((margin + centre - 8) / 2,
              (centre + 8 + width - margin) / 2)
  for (i in 1:2) {
    g <- grid::textGrob(
      notes[[i]], x = note_x[i], y = annotation_y,
      default.units = "bigpts",
      gp = grid::gpar(fontfamily = FONT, fontsize = body_size, col = INK)
    )
    stopifnot(grid::convertWidth(
      grid::grobWidth(g), "bigpts", valueOnly = TRUE
    ) <= usable / 2 - 8)
    grid::grid.draw(g)
  }
}

save_grid_figure(
  draw_figure_3_1, "Figure_3_1_analytical_sequence",
  width_mm = 180, height_mm = 185
)


# ---- 5. Figure 3.2: spatial configuration (dual-panel, WGS 84) --------------

# Domain polygon in WGS 84

edge_lon <- seq(BBOX[["lon_min"]], BBOX[["lon_max"]], length.out = 100)
edge_lat <- seq(BBOX[["lat_min"]], BBOX[["lat_max"]], length.out = 50)
domain_xy <- rbind(
  cbind(edge_lon, BBOX[["lat_min"]]),
  cbind(BBOX[["lon_max"]], edge_lat[-1]),
  cbind(rev(edge_lon[-length(edge_lon)]), BBOX[["lat_max"]]),
  cbind(BBOX[["lon_min"]], rev(edge_lat[-c(1, length(edge_lat))])),
  c(BBOX[["lon_min"]], BBOX[["lat_min"]])
)
domain_geo <- sf::st_sf(
  d = 1L,
  geometry = sf::st_sfc(sf::st_polygon(list(domain_xy)), crs = CRS_GEO)
)
domain_prj <- sf::st_transform(domain_geo, CRS_PRJ)


# Reference layers

read_ref <- function(fn, url) {
  cache <- file.path(REF_DIR, fn)
  if (file.exists(cache)) return(sf::st_read(cache, quiet = TRUE))
  layer <- sf::st_make_valid(sf::st_read(url, quiet = TRUE))
  sf::st_write(layer, cache, append = FALSE, quiet = TRUE)
  layer
}

ms_url <- paste0(
  "https://services.arcgis.com/QVENGdaPbd4LUkLV/ArcGIS/rest/services/",
  "FWS_R7_Realty_USGS_Topo_Maps_feature_layer/FeatureServer/0/query?",
  "where=1%3D1&outFields=USGSNAME%2CQUADCODE%2CS_LAT_DD%2CE_LON_DD%2C",
  "N_LAT_DD%2CW_LON_DD&returnGeometry=true&outSR=4326&",
  "geometry=-165%2C60%2C-156%2C63&geometryType=esriGeometryEnvelope&",
  "inSR=4326&spatialRel=esriSpatialRelIntersects&f=geojson"
)
ad_url <- paste0(
  "https://tigerweb.geo.census.gov/arcgis/rest/services/TIGERweb/",
  "State_County/MapServer/1/query?where=STATE%3D%2702%27&outFields=NAME&",
  "returnGeometry=true&outSR=4326&f=geojson"
)

map_sheets   <- read_ref("usgs_250k_map_sheets.gpkg", ms_url)
alaska_admin <- read_ref("census_2025_alaska_county_equivalents.gpkg", ad_url)

quad_fields <- c("USGSNAME", "QUADCODE", "S_LAT_DD", "N_LAT_DD",
                 "W_LON_DD", "E_LON_DD")
stopifnot(all(quad_fields %in% names(map_sheets)),
          "NAME" %in% names(alaska_admin))

# Select positive overlap using the official nominal geographic bounds.
# This excludes boundary-touching neighbours without an arbitrary sliver cutoff.
# Keep the service geometries; coord_sf clips their display to the map frame.
map_sheets <- map_sheets[
  map_sheets$S_LAT_DD < BBOX[["lat_max"]] &
    map_sheets$N_LAT_DD > BBOX[["lat_min"]] &
    map_sheets$W_LON_DD < BBOX[["lon_max"]] &
    map_sheets$E_LON_DD > BBOX[["lon_min"]], ]
n_sheets <- nrow(map_sheets)
stopifnot(
  n_sheets == 9L,
  setequal(trimws(map_sheets$QUADCODE),
           c("KWI", "XHC", "IDT", "MAR", "RUS", "SLT", "XBI", "BTH", "TAY")),
  all(map_sheets$N_LAT_DD - map_sheets$S_LAT_DD == 1),
  all(map_sheets$E_LON_DD - map_sheets$W_LON_DD == 3)
)
sheet_info <- sf::st_drop_geometry(map_sheets)[, quad_fields]
sheet_info <- sheet_info[
  order(-sheet_info$S_LAT_DD, sheet_info$W_LON_DD), ]
message(n_sheets, " USGS 1:250,000 map sheets intersect the domain.")

ms_labels <- sf::st_as_sf(
  transform(
    sheet_info, lon = (W_LON_DD + E_LON_DD) / 2,
    lat = (S_LAT_DD + N_LAT_DD) / 2
  ), coords = c("lon", "lat"), crs = CRS_GEO
)
ms_labels$label <- vapply(
  tools::toTitleCase(tolower(trimws(ms_labels$USGSNAME))),
  function(x) paste(strwrap(x, width = 14), collapse = "\n"),
  character(1)
)

# Keep eastern map-sheet labels clear of the north arrow and scale bar.
label_xy <- sf::st_coordinates(ms_labels)
sheet_names <- toupper(trimws(ms_labels$USGSNAME))
label_xy[sheet_names %in% c("IDITAROD", "TAYLOR MOUNTAINS"), "X"] <- -157.65
label_xy[sheet_names == "TAYLOR MOUNTAINS", "Y"] <- 60.72
sf::st_geometry(ms_labels) <- sf::st_sfc(
  lapply(seq_len(nrow(label_xy)), function(i) sf::st_point(label_xy[i, 1:2])),
  crs = CRS_GEO
)

# Admin clipped to domain
alaska_admin <- alaska_admin |>
  dplyr::mutate(NAME = trimws(as.character(NAME))) |>
  sf::st_make_valid()
admin_clip <- suppressWarnings(sf::st_intersection(alaska_admin, domain_geo))
admin_clip <- admin_clip[as.numeric(sf::st_area(admin_clip)) > 1e6, ]

# Coordinate-derived sites and count classes

master <- readRDS(methodology_paths$master)
stopifnot(all(c("sample_id", "site_id", "lon", "lat") %in% names(master)),
          !anyDuplicated(master$sample_id))
samp_rows <- match(comp_ids, as.character(master$sample_id))
stopifnot(!anyNA(samp_rows))

# Use the saved site_id (lon/lat formatted to 6 decimal places), not a distance
# radius or a 1-km cell identifier. Aggregation is for symbol display only.
cohort_sites <- master[samp_rows, c("sample_id", "site_id", "lon", "lat")]
stopifnot(!anyNA(cohort_sites$site_id), all(nzchar(cohort_sites$site_id)))
site_levels <- c("1", "2\u20134", "\u22655")
site_counts <- cohort_sites |>
  dplyr::group_by(site_id) |>
  dplyr::summarise(
    n_obs = dplyr::n(), lon = dplyr::first(lon), lat = dplyr::first(lat),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    count_class = cut(n_obs, breaks = c(0, 1, 4, Inf),
                      labels = site_levels, right = TRUE)
  ) |>
  dplyr::arrange(n_obs, site_id)
stopifnot(sum(site_counts$n_obs) == n_obs, !anyNA(site_counts$count_class))
message(
  n_obs, " observations; ", nrow(site_counts), " coordinate-derived sites; ",
  "maximum observations per site = ", max(site_counts$n_obs), "."
)
sample_sites <- sf::st_as_sf(
  site_counts, coords = c("lon", "lat"), crs = CRS_GEO, remove = FALSE
)

# Terrain in WGS 84 (multi-directional hillshade)

predictors  <- terra::rast(methodology_paths$stack)
domain_mask <- terra::rast(methodology_paths$mask)
stopifnot("elev" %in% names(predictors))

elev_prj <- terra::mask(predictors[["elev"]], domain_mask)
names(elev_prj) <- "elevation_m"

# Compute terrain derivatives in the projected CRS where distances are valid
slp <- terra::terrain(elev_prj, v = "slope",  unit = "radians", neighbors = 8)
asp <- terra::terrain(elev_prj, v = "aspect", unit = "radians", neighbors = 8)

# Multi-directional hillshade (4 azimuths, 2 solar elevations)
hs <- (
  terra::shade(slp, asp, angle = 45, direction = 315) +
    terra::shade(slp, asp, angle = 45, direction = 270) +
    terra::shade(slp, asp, angle = 30, direction = 225) +
    terra::shade(slp, asp, angle = 55, direction = 0)
) / 4
names(hs) <- "illumination"

# Project elevation and hillshade to a regular WGS 84 grid (~1 km)
wgs_template <- terra::rast(
  xmin = BBOX[["lon_min"]], xmax = BBOX[["lon_max"]],
  ymin = BBOX[["lat_min"]], ymax = BBOX[["lat_max"]],
  res = 0.01, crs = paste0("EPSG:", CRS_GEO)
)
elev_wgs <- terra::project(elev_prj, wgs_template, method = "bilinear")
hs_wgs   <- terra::project(hs,       wgs_template, method = "bilinear")

relief <- terra::as.data.frame(c(elev_wgs, hs_wgs), xy = TRUE, na.rm = TRUE)
# Shadow alpha: stronger contrast for ArcGIS-Pro-like relief
relief$shadow <- 0.48 * (1 - scales::rescale(
  relief$illumination, to = c(0.25, 1)
))

# Domain outline in WGS 84 for overlay
domain_land <- terra::as.polygons(domain_mask, dissolve = TRUE,
                                  na.rm = TRUE) |>
  sf::st_as_sf() |> sf::st_make_valid() |>
  sf::st_transform(CRS_GEO)

# Generalised land context for panel (a) only, not the analytical domain.
# Omit interior holes and detached components smaller than 25 km2; simplify
# the retained outline with a 1-km tolerance in EPSG:3338.
# The original domain_mask, domain_land and terrain layers remain unchanged.
land_parts <- sf::st_cast(
  sf::st_transform(sf::st_geometry(domain_land), CRS_PRJ), "POLYGON"
)
land_context <- sf::st_sfc(
  lapply(land_parts, function(part) sf::st_polygon(list(part[[1]]))),
  crs = CRS_PRJ
)
land_context <- land_context[as.numeric(sf::st_area(land_context)) >= 25e6] |>
  sf::st_union() |>
  sf::st_simplify(dTolerance = 1000, preserveTopology = TRUE) |>
  sf::st_transform(CRS_GEO) |>
  sf::st_as_sf()


# Shared map design (WGS 84)

lon_label <- function(x) {
  paste0(abs(x), "\u00b0", ifelse(x < 0, "W", ifelse(x > 0, "E", "")))
}
lat_label <- function(x) {
  paste0(abs(x), "\u00b0", ifelse(x < 0, "S", ifelse(x > 0, "N", "")))
}
lon_breaks <- seq(ceiling(BBOX[["lon_min"]]),
                  floor(BBOX[["lon_max"]]), by = 2)
lat_breaks <- seq(ceiling(BBOX[["lat_min"]]),
                  floor(BBOX[["lat_max"]]), by = 1)

map_x_span <- BBOX[["lon_max"]] - BBOX[["lon_min"]]
map_y_span <- BBOX[["lat_max"]] - BBOX[["lat_min"]]
MAP_FONT <- "sans"
MAP_INK  <- "darkslategray"

# A locally exact 100-km scale bar, matching the Figure 4.5 map design.
scale_bar_km <- 100
scale_bar_lat <- BBOX[["lat_min"]] + 0.085 * map_y_span
earth_radius_m <- 6371008.8
central_angle <- scale_bar_km * 1000 / earth_radius_m
latitude_rad <- scale_bar_lat * pi / 180
scale_bar_delta_lon <- acos(
  (cos(central_angle) - sin(latitude_rad)^2) / cos(latitude_rad)^2
) * 180 / pi
scale_bar_x2 <- BBOX[["lon_max"]] - 0.045 * map_x_span
scale_bar_x1 <- scale_bar_x2 - scale_bar_delta_lon
scale_tick_half_height <- 0.018 * map_y_span

# Square north-arrow marker. Longitude is converted to latitude span so that
# the frame and the arrow bounding box remain square on the WGS 84 display.
north_x <- BBOX[["lon_max"]] - 0.060 * map_x_span
north_y1 <- BBOX[["lat_max"]] - 0.075 * map_y_span
north_box_w <- 0.075 * map_x_span
north_box_h <- north_box_w *
  cos(mean(c(BBOX[["lat_min"]], BBOX[["lat_max"]])) * pi / 180)

map_annotations <- list(
  ggplot2::annotate(
    "rect",
    xmin = scale_bar_x1 - 0.025 * map_x_span,
    xmax = scale_bar_x2 + 0.025 * map_x_span,
    ymin = scale_bar_lat - 0.055 * map_y_span,
    ymax = scale_bar_lat + 0.070 * map_y_span,
    fill = scales::alpha("white", 0.82), colour = NA
  ),
  ggplot2::annotate(
    "segment",
    x = scale_bar_x1, xend = scale_bar_x2,
    y = scale_bar_lat, yend = scale_bar_lat,
    colour = MAP_INK, linewidth = 0.72, lineend = "butt"
  ),
  ggplot2::annotate(
    "segment",
    x = c(scale_bar_x1, scale_bar_x2),
    xend = c(scale_bar_x1, scale_bar_x2),
    y = scale_bar_lat - scale_tick_half_height,
    yend = scale_bar_lat + scale_tick_half_height,
    colour = MAP_INK, linewidth = 0.62
  ),
  ggplot2::annotate(
    "text",
    x = mean(c(scale_bar_x1, scale_bar_x2)),
    y = scale_bar_lat + 0.033 * map_y_span,
    label = paste0(scale_bar_km, " km"),
    family = MAP_FONT, fontface = "bold", size = 2.20, colour = MAP_INK
  ),
  ggplot2::annotate(
    "text",
    x = mean(c(scale_bar_x1, scale_bar_x2)),
    y = scale_bar_lat - 0.034 * map_y_span,
    label = sprintf("Scale true at %.1f\u00b0N", scale_bar_lat),
    family = MAP_FONT, size = 1.78, colour = "lightskyblue4"
  ),
  ggplot2::annotate(
    "rect",
    xmin = north_x - 0.50 * north_box_w,
    xmax = north_x + 0.50 * north_box_w,
    ymin = north_y1 - 0.66 * north_box_h,
    ymax = north_y1 + 0.34 * north_box_h,
    fill = scales::alpha("white", 0.82), colour = NA
  ),
  ggplot2::annotate(
    "polygon",
    x = north_x + c(0, -0.26, 0, 0.26) * north_box_w,
    y = north_y1 + c(0, -0.52, -0.34, -0.52) * north_box_h,
    fill = MAP_INK, colour = NA
  ),
  ggplot2::annotate(
    "text",
    x = north_x, y = north_y1 + 0.17 * north_box_h,
    label = "N", family = MAP_FONT, fontface = "bold",
    size = 2.60, colour = MAP_INK
  )
)

map_theme <- ggplot2::theme_minimal(base_family = MAP_FONT, base_size = 7.2) +
  ggplot2::theme(
    panel.background  = ggplot2::element_rect(fill = "mintcream", colour = NA),
    panel.grid.major  = ggplot2::element_line(colour = "azure3",
                                              linewidth = 0.22),
    panel.grid.minor  = ggplot2::element_blank(),
    panel.border      = ggplot2::element_rect(fill = NA, colour = MAP_INK,
                                              linewidth = 0.38),
    axis.title        = ggplot2::element_blank(),
    axis.text         = ggplot2::element_text(colour = MAP_INK, size = 6.4),
    axis.ticks        = ggplot2::element_line(colour = MAP_INK, linewidth = 0.3),
    axis.ticks.length = grid::unit(1.3, "mm"),
    plot.title        = ggplot2::element_blank(),
    legend.title      = ggplot2::element_text(face = "bold", size = 6.8),
    legend.text       = ggplot2::element_text(size = 6.2),
    plot.margin       = ggplot2::margin(1, 1, 1, 1, unit = "mm")
  )

map_coord <- ggplot2::coord_sf(
  crs    = sf::st_crs(CRS_GEO),
  datum  = sf::st_crs(CRS_GEO),
  xlim   = c(BBOX[["lon_min"]], BBOX[["lon_max"]]),
  ylim   = c(BBOX[["lat_min"]], BBOX[["lat_max"]]),
  expand = FALSE,
  ndiscr = 200
)

map_scales <- list(
  ggplot2::scale_x_continuous(breaks = lon_breaks, labels = lon_label),
  ggplot2::scale_y_continuous(breaks = lat_breaks, labels = lat_label)
)
site_fill <- "skyblue"
site_scale <- ggplot2::scale_size_manual(
  name = "Observations per site",
  values = stats::setNames(c(1.1, 2.0, 3.0), site_levels),
  limits = site_levels, breaks = site_levels, drop = FALSE
)


# Panel (a): administrative and map-sheet context

panel_a <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = land_context, fill = "azure2", colour = MAP_INK,
    linewidth = 0.30
  ) +
  ggplot2::geom_sf(
    data = admin_clip, fill = NA, colour = "lightslategray",
    linewidth = 0.30
  ) +
  ggplot2::geom_sf(
    data = map_sheets, fill = NA, colour = "deepskyblue4",
    linewidth = 0.42, linetype = 2
  ) +
  ggplot2::geom_sf_label(
    data = ms_labels, ggplot2::aes(label = label),
    family = MAP_FONT, fontface = "bold", colour = "deepskyblue4",
    fill = scales::alpha("white", 0.82), label.size = 0,
    label.padding = grid::unit(0.55, "mm"),
    size = 1.70, lineheight = 0.88
  ) +
  map_annotations + map_scales + map_coord + map_theme +
  ggplot2::theme(legend.position = "none")


# Panel (b): topographic context with multi-directional hillshade

terrain_pal <- grDevices::hcl.colors(6, palette = "BluYl")

panel_b <- ggplot2::ggplot() +
  # Elevation colour ramp
  ggplot2::geom_raster(
    data = relief,
    ggplot2::aes(x = x, y = y, fill = elevation_m)
  ) +
  # Multi-directional hillshade overlay
  ggplot2::geom_raster(
    data = relief,
    ggplot2::aes(x = x, y = y, alpha = shadow),
    fill = "black"
  ) +
  ggplot2::scale_alpha_identity() +
  ggplot2::scale_fill_gradientn(
    colours = terrain_pal, name = "Elevation (m)", na.value = NA
  ) +
  ggplot2::geom_sf(
    data = map_sheets, fill = NA,
    colour = scales::alpha("deepskyblue4", 0.38),
    linewidth = 0.26, linetype = 2
  ) +
  ggplot2::geom_sf(
    data = domain_land, fill = NA, colour = MAP_INK, linewidth = 0.44
  ) +
  ggplot2::geom_sf(
    data = sample_sites, ggplot2::aes(size = count_class),
    shape = 21, show.legend = "point",
    fill = site_fill, colour = "white",
    stroke = 0.20, alpha = 1
  ) +
  site_scale + map_annotations + map_scales +
  ggplot2::guides(
    fill = ggplot2::guide_colourbar(
      order = 1, title.position = "top", title.hjust = 0.5,
      barwidth = grid::unit(32, "mm"), barheight = grid::unit(2.5, "mm"),
      ticks = TRUE, frame.colour = MAP_INK
    ),
    size = ggplot2::guide_legend(
      order = 2, nrow = 1, title.position = "top", title.hjust = 0.5,
      override.aes = list(alpha = 1)
    )
  ) +
  map_coord + map_theme +
  ggplot2::theme(
    legend.position = "bottom",
    legend.box      = "horizontal",
    legend.box.just = "centre",
    legend.margin   = ggplot2::margin(0, 0, 0, 0)
  )


# Alaska locator inset

alaska_outline <- sf::st_sf(
  geometry = sf::st_union(sf::st_geometry(alaska_admin))
)
alaska_outline_prj <- sf::st_transform(alaska_outline, CRS_PRJ)
locator_bbox <- sf::st_bbox(alaska_outline_prj)

locator <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = alaska_outline_prj, fill = "azure2",
    colour = "lightskyblue4", linewidth = 0.28
  ) +
  ggplot2::geom_sf(
    data = domain_geo,
    fill = scales::alpha("cadetblue", 0.30),
    colour = "deepskyblue4", linewidth = 0.55
  ) +
  ggplot2::coord_sf(
    crs = sf::st_crs(CRS_PRJ), datum = NA,
    xlim = c(locator_bbox[["xmin"]], locator_bbox[["xmax"]]),
    ylim = c(locator_bbox[["ymin"]], locator_bbox[["ymax"]]),
    expand = FALSE
  ) +
  ggplot2::theme_void(base_family = MAP_FONT) +
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "white", colour = NA),
    panel.border     = ggplot2::element_rect(fill = NA, colour = MAP_INK,
                                             linewidth = 0.32),
    plot.margin      = ggplot2::margin(0, 0, 0, 0, unit = "mm")
  )


# Assemble the dual-panel figure

shared_legend <- cowplot::get_legend(panel_b + ggplot2::guides(size = "none"))
# Build all 3 site-count keys explicitly, independently of the terrain layers.
point_legend <- cowplot::get_legend(
  ggplot2::ggplot(
    data.frame(count_class = factor(site_levels, levels = site_levels)),
    ggplot2::aes(x = count_class, y = 1, size = count_class)
  ) +
    ggplot2::geom_point(
      shape = 21, fill = site_fill, colour = "white",
      stroke = 0.20, alpha = 1, show.legend = TRUE
    ) +
    site_scale +
    ggplot2::guides(size = ggplot2::guide_legend(
      nrow = 1, title.position = "top", title.hjust = 0.5
    )) +
    map_theme +
    ggplot2::theme(
      legend.position = "bottom",
      legend.margin = ggplot2::margin(0, 0, 0, 0)
    )
)
panel_b_nol <- panel_b + ggplot2::theme(legend.position = "none")

# Existing map slots and locator placement are retained.
figure_width_mm <- 180
figure_height_mm <- 110
panel_w <- 0.46
panel_h <- 0.57
panel_y <- 0.38
panel_a_x <- 0.02
panel_b_x <- 0.52

locator_aspect <- as.numeric(
  (locator_bbox[["ymax"]] - locator_bbox[["ymin"]]) /
    (locator_bbox[["xmax"]] - locator_bbox[["xmin"]])
)
inset_w <- 0.22
inset_h <- inset_w * locator_aspect * figure_width_mm / figure_height_mm
inset_x <- panel_a_x + 0.015
inset_gap <- 0.030
inset_y <- panel_y - inset_gap - inset_h

# Projected study-domain bottom vertices in locator-relative coordinates.
domain_bottom_prj <- sf::st_sfc(
  sf::st_point(c(BBOX[["lon_min"]], BBOX[["lat_min"]])),
  sf::st_point(c(BBOX[["lon_max"]], BBOX[["lat_min"]])),
  crs = CRS_GEO
) |>
  sf::st_transform(CRS_PRJ)
domain_bottom_xy <- sf::st_coordinates(domain_bottom_prj)
domain_bottom_xy <- domain_bottom_xy[order(domain_bottom_xy[, "X"]), , drop = FALSE]
domain_rel_x <- (
  domain_bottom_xy[, "X"] - locator_bbox[["xmin"]]
) / (locator_bbox[["xmax"]] - locator_bbox[["xmin"]])
domain_rel_y <- (
  domain_bottom_xy[, "Y"] - locator_bbox[["ymin"]]
) / (locator_bbox[["ymax"]] - locator_bbox[["ymin"]])

# Resolve frame and locator positions at draw time, including axis padding.
# This keeps the anchors aligned in both the plot pane and fixed-size exports.
makeContent.figure32_overlay <- function(x) {
  width_mm <- grid::convertWidth(grid::unit(1, "npc"), "mm", valueOnly = TRUE)
  height_mm <- grid::convertHeight(grid::unit(1, "npc"), "mm", valueOnly = TRUE)
  widths <- grid::convertWidth(x$frame_widths, "mm", valueOnly = TRUE)
  heights <- grid::convertHeight(x$frame_heights, "mm", valueOnly = TRUE)
  map_w <- min(
    x$map_slot[["w"]] * width_mm - sum(widths),
    (x$map_slot[["h"]] * height_mm - sum(heights)) / x$map_aspect
  )
  map_h <- map_w * x$map_aspect
  left <- x$map_slot[["x"]] +
    (x$map_slot[["w"]] - (sum(widths) + map_w) / width_mm) / 2 +
    sum(widths[seq_along(widths) < x$panel_col]) / width_mm
  bottom <- x$map_slot[["y"]] +
    (x$map_slot[["h"]] - (sum(heights) + map_h) / height_mm) / 2 +
    sum(heights[seq_along(heights) > x$panel_row]) / height_mm
  right <- left + map_w / width_mm
  top <- bottom + map_h / height_mm
  
  # d: map-frame to longitude-label centre; intersections are 2d below it.
  d <- grid::convertHeight(
    x$tick_length + x$label_margin + 0.5 * grid::grobHeight(x$axis_label),
    "mm", valueOnly = TRUE
  ) / height_mm
  anchor_y <- bottom - 2 * d
  
  loc_w <- min(x$locator_slot[["w"]] * width_mm,
               x$locator_slot[["h"]] * height_mm / x$locator_aspect)
  loc_h <- loc_w * x$locator_aspect
  loc_left <- x$locator_slot[["x"]] +
    (x$locator_slot[["w"]] - loc_w / width_mm) / 2
  loc_bottom <- x$locator_slot[["y"]] +
    (x$locator_slot[["h"]] - loc_h / height_mm) / 2
  domain_x <- loc_left + loc_w / width_mm * x$domain_rel_x
  domain_y <- loc_bottom + loc_h / height_mm * x$domain_rel_y
  
  grid::setChildren(x, grid::gList(
    grid::segmentsGrob(
      x0 = c(domain_x, left, right),
      y0 = c(domain_y, rep(anchor_y - d / 2, 2)),
      x1 = c(left, right, left, right),
      y1 = c(rep(anchor_y, 2), rep(anchor_y + d / 2, 2)),
      default.units = "npc",
      gp = grid::gpar(col = x$ink, lwd = 0.38 * 72.27 / 25.4, lineend = "butt")
    ),
    grid::textGrob(
      c("(a) USGS 1:250,000 map sheets",
        "(b) Elevation and sampling sites"),
      x = c(left, left + x$panel_shift),
      y = top + 0.035, hjust = 0, vjust = 0.5,
      gp = grid::gpar(col = x$ink, fontfamily = x$font_family,
                      fontface = "bold", fontsize = 8.2)
    )
  ))
}

frame_grob <- ggplot2::ggplotGrob(panel_a)
frame_cell <- frame_grob$layout[frame_grob$layout$name == "panel", ]
frame_grob$widths[frame_cell$l] <- grid::unit(0, "mm")
frame_grob$heights[frame_cell$t] <- grid::unit(0, "mm")
axis_text <- ggplot2::calc_element("axis.text.x.bottom", map_theme)
map_overlay <- grid::gTree(
  cl = "figure32_overlay",
  frame_widths = frame_grob$widths, frame_heights = frame_grob$heights,
  panel_col = frame_cell$l, panel_row = frame_cell$b,
  map_slot = c(x = panel_a_x, y = panel_y, w = panel_w, h = panel_h),
  panel_shift = panel_b_x - panel_a_x,
  map_aspect = map_y_span / (map_x_span * cos(
    mean(c(BBOX[["lat_min"]], BBOX[["lat_max"]])) * pi / 180
  )),
  locator_slot = c(x = inset_x, y = inset_y, w = inset_w, h = inset_h),
  locator_aspect = locator_aspect,
  domain_rel_x = domain_rel_x, domain_rel_y = domain_rel_y,
  tick_length = grid::unit(1.3, "mm"), label_margin = axis_text$margin[1],
  axis_label = grid::textGrob(
    lon_label(lon_breaks),
    gp = grid::gpar(fontfamily = axis_text$family, fontsize = axis_text$size,
                    fontface = axis_text$face, lineheight = axis_text$lineheight)
  ),
  font_family = MAP_FONT, ink = MAP_INK
)

legend_w <- 0.34
legend_h <- 0.12
legend_x <- panel_b_x + (panel_w - legend_w) / 2
legend_y <- 0.13

figure_3_2 <- cowplot::ggdraw() +
  cowplot::draw_plot(
    panel_a, x = panel_a_x, y = panel_y,
    width = panel_w, height = panel_h
  ) +
  cowplot::draw_plot(
    panel_b_nol, x = panel_b_x, y = panel_y,
    width = panel_w, height = panel_h
  ) +
  cowplot::draw_grob(
    shared_legend, x = legend_x, y = legend_y,
    width = legend_w, height = legend_h
  ) +
  cowplot::draw_grob(
    point_legend, x = legend_x, y = legend_y + legend_h + 0.010,
    width = legend_w, height = 0.10
  ) +
  cowplot::draw_plot(
    locator, x = inset_x, y = inset_y,
    width = inset_w, height = inset_h
  ) +
  cowplot::draw_label(
    "Alaska", x = inset_x + inset_w / 2,
    y = inset_y - 0.014,
    hjust = 0.5, vjust = 1,
    fontfamily = MAP_FONT, fontface = "bold",
    size = 6.5, colour = MAP_INK
  ) +
  cowplot::draw_grob(map_overlay, x = 0, y = 0, width = 1, height = 1)

print(figure_3_2)
save_figure(figure_3_2, "Figure_3_2_spatial_configuration",
            width_mm = figure_width_mm, height_mm = figure_height_mm)


# ---- 6. Figure 3.3: shared resampling framework -----------------------------

# The same 28 illustrative observations occupy the same 21 cells in all panels.
# Only training/test membership changes. These are not empirical fold assignments
# and the cell/block size ratio is not the project's 1-km/50-km scale ratio.

make_cv_schematic <- function() {
  cells <- data.frame(
    x = c(0, 2, 5, 7, 1, 3, 4, 6, 0, 2, 4, 7, 1, 3, 5, 6, 0, 2, 7, 1, 6),
    y = c(0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5),
    n = c(1, 1, 1, 1, 3, 1, 1, 1, 1, 2, 1, 2, 1, 1, 3, 1, 2, 1, 1, 1, 1)
  )
  cells$cell_id <- seq_len(nrow(cells))
  
  # A regular hexagonal tessellation; nearest-centre assignment is equivalent
  # to hexagon containment for the cell centres used here (none is on an edge).
  radius <- 1.45
  blocks <- expand.grid(q = -1:4, r = -3:3)
  blocks$cx <- 1 + 1.5 * radius * blocks$q
  blocks$cy <- 1 + sqrt(3) * radius * (blocks$r + blocks$q / 2)
  blocks$block_id <- paste(blocks$q, blocks$r, sep = ":")
  distance2 <- outer(cells$x + 0.5, blocks$cx, "-")^2 +
    outer(cells$y + 0.5, blocks$cy, "-")^2
  nearest <- max.col(-distance2, ties.method = "first")
  cells$block_id <- blocks$block_id[nearest]
  # Arrange illustrative symbols inside their cells, clear of block edges.
  # These same positions are reused in all panels; no empirical points move.
  direction <- cbind(
    blocks$cx[nearest] - (cells$x + 0.5),
    blocks$cy[nearest] - (cells$y + 0.5)
  )
  layout_shift <- 0.20 * direction / sqrt(rowSums(direction^2))
  
  offsets <- list(
    matrix(c(0, 0), ncol = 2),
    matrix(c(-0.17, -0.14, 0.17, 0.14), ncol = 2, byrow = TRUE),
    matrix(c(-0.19, -0.15, 0.19, -0.15, 0, 0.20),
           ncol = 2, byrow = TRUE)
  )
  points <- do.call(rbind, lapply(seq_len(nrow(cells)), function(i) {
    shift <- offsets[[cells$n[i]]]
    data.frame(
      cell_id = cells$cell_id[i], block_id = cells$block_id[i],
      x = cells$x[i] + 0.5 + layout_shift[i, 1] + shift[, 1],
      y = cells$y[i] + 0.5 + layout_shift[i, 2] + shift[, 2]
    )
  }))
  points$point_id <- seq_len(nrow(points))
  
  # Move the single training observation in schematic row 2, column 3 nearer
  # the left cell boundary. Its position remains identical across all panels.
  target_cell <- which(cells$x == 2 & cells$y == 4)
  stopifnot(length(target_cell) == 1L, cells$n[target_cell] == 1L)
  points$x[points$cell_id == cells$cell_id[target_cell]] <-
    cells$x[target_cell] + 0.25
  
  points$observation_test <- points$point_id %in% c(2, 5, 10, 15, 21, 27)
  points$grid_test <- points$cell_id %in% c(5, 6, 17)
  points$spatial_test <- points$block_id %in% c("1:0", "3:0")
  
  same_group <- function(state, group) {
    all(vapply(split(state, group), function(x) length(unique(x)) == 1L,
               logical(1)))
  }
  stopifnot(
    nrow(points) == 28L,
    any(cells$n == 1L), any(cells$n > 1L),
    !same_group(points$observation_test, points$cell_id),
    same_group(points$grid_test, points$cell_id),
    same_group(points$spatial_test, points$cell_id),
    same_group(points$spatial_test, points$block_id),
    all(colSums(points[c("observation_test", "grid_test", "spatial_test")]) == 6L)
  )
  list(cells = cells, points = points, blocks = blocks, radius = radius)
}

cv_schematic <- make_cv_schematic()

draw_figure_3_3 <- function() {
  train_colour <- "steelblue"
  test_colour <- "forestgreen"  # triangles provide a second cue
  cell_colour <- "darkseagreen3"
  block_colour <- "darkslategray"
  points <- cv_schematic$points
  blocks <- cv_schematic$blocks
  panel_titles <- c(
    "(a) Observation-level random",
    "(b) Grid-grouped random",
    "(c) Spatially blocked"
  )
  states <- c("observation_test", "grid_test", "spatial_test")
  panel_w <- 54
  panel_h <- panel_w * 6 / 8
  panel_x <- c(4, 63, 122)
  panel_y <- 27
  
  for (i in 1:3) {
    grid::grid.text(
      panel_titles[i],
      x = grid::unit(panel_x[i], "mm"), y = grid::unit(76, "mm"),
      just = c("left", "top"),
      gp = grid::gpar(fontfamily = FONT, fontsize = 10.5, fontface = "bold",
                      lineheight = 1.05, col = INK)
    )
    grid::pushViewport(grid::viewport(
      x = grid::unit(panel_x[i], "mm"), y = grid::unit(panel_y, "mm"),
      width = grid::unit(panel_w, "mm"), height = grid::unit(panel_h, "mm"),
      just = c("left", "bottom"), xscale = c(0, 8), yscale = c(0, 6),
      clip = "on"
    ))
    grid::grid.segments(
      x0 = c(0:8, rep(0, 7)), y0 = c(rep(0, 9), 0:6),
      x1 = c(0:8, rep(8, 7)), y1 = c(rep(6, 9), 0:6),
      default.units = "native", gp = grid::gpar(col = cell_colour, lwd = 0.5)
    )
    if (i == 3L) {
      angle <- (0:6) * pi / 3
      for (j in seq_len(nrow(blocks))) grid::grid.lines(
        x = blocks$cx[j] + cv_schematic$radius * cos(angle),
        y = blocks$cy[j] + cv_schematic$radius * sin(angle),
        default.units = "native",
        gp = grid::gpar(col = block_colour, lwd = 0.9)
      )
    }
    held_out <- points[[states[i]]]
    for (test in c(FALSE, TRUE)) {
      keep <- held_out == test
      grid::grid.points(
        points$x[keep], points$y[keep], default.units = "native",
        pch = if (test) 24 else 21, size = grid::unit(2.0, "mm"),
        gp = grid::gpar(
          fill = if (test) test_colour else train_colour,
          col = if (test) "darkslategray" else "steelblue4", lwd = 0.45
        )
      )
    }
    if (i == 1L) {
      annotation_x <- 5.00
      annotation_y <- 4.88
      annotation_w <- 4.50
      annotation_h <- 1.20
      
      grid::grid.rect(
        x = annotation_x, y = annotation_y,
        width = annotation_w, height = annotation_h,
        default.units = "native",
        gp = grid::gpar(fill = "lightblue", col = "steelblue4", lwd = 0.45)
      )
      grid::grid.text(
        "Same-cell observations\nshare one predictor vector",
        x = annotation_x, y = annotation_y,
        just = c("centre", "centre"),
        default.units = "native",
        gp = grid::gpar(fontfamily = FONT, fontsize = 8.2,
                        lineheight = 1.05, col = INK)
      )
      # Draw last so the black arrow remains above the annotation box. Its
      # endpoint is unchanged and its shaft is half the previous length.
      grid::grid.segments(
        5.125, 4.28, 5.45, 3.87, default.units = "native",
        gp = grid::gpar(col = "black", lwd = 0.5),
        arrow = grid::arrow(length = grid::unit(1.1, "mm"), type = "closed")
      )
    }
    grid::grid.rect(gp = grid::gpar(fill = NA, col = block_colour, lwd = 0.6))
    grid::popViewport()
  }
  
  # One shared legend, with colour and shape distinctions for observations.
  legend_labels <- c("Training (4 folds)", "Test (1 fold)", "Cell", "Hexagonal block")
  legend_text <- lapply(legend_labels, function(label) grid::textGrob(
    label, gp = grid::gpar(fontfamily = FONT, fontsize = 8.5, col = INK)
  ))
  label_width <- vapply(legend_text, function(g) grid::convertWidth(
    grid::grobWidth(g), "mm", valueOnly = TRUE
  ), numeric(1))
  key_w <- 6
  legend_gap <- 6
  item_w <- key_w + label_width
  legend_x <- (180 - sum(item_w) - 3 * legend_gap) / 2 +
    c(0, cumsum(head(item_w, -1) + legend_gap))
  for (i in 1:4) {
    if (i <= 2) {
      grid::grid.points(
        grid::unit(legend_x[i] + 2, "mm"), grid::unit(15, "mm"),
        pch = c(21, 24)[i], size = grid::unit(2.0, "mm"),
        gp = grid::gpar(fill = c(train_colour, test_colour)[i],
                        col = "darkslategray", lwd = 0.45)
      )
    } else {
      grid::grid.segments(
        grid::unit(legend_x[i], "mm"), grid::unit(15, "mm"),
        grid::unit(legend_x[i] + 4, "mm"), grid::unit(15, "mm"),
        gp = grid::gpar(col = c(cell_colour, block_colour)[i - 2],
                        lwd = c(0.5, 0.9)[i - 2])
      )
    }
    grid::grid.draw(grid::editGrob(
      legend_text[[i]], x = grid::unit(legend_x[i] + key_w, "mm"),
      y = grid::unit(15, "mm"), just = c("left", "centre")
    ))
  }
}

save_grid_figure(
  draw_figure_3_3, "Figure_3_3_shared_resampling",
  width_mm = 180, height_mm = 84
)

message("\nFigures written to:\n  ", OUT_DIR)



# ==============================================================================
# SECTION 2: CHAPTER 4 RESULTS AUDIT
# ==============================================================================
#' # Chapter 4 Results audit
#' Primary outputs are read, checked and reported in manuscript order.
#' Publication-quality Figures 4.1-4.5 replace the diagnostic versions.

# Results output directory (replaces supplementary_results.R line 43-44)
output_dir <- results_output_dir


EXPECTED_FINAL_N <- 425L
EXPECTED_SELECTED_VERSION <- "V3"
TOL <- 1e-10

# 2) Result registry: one manuscript placeholder -> one traceable value

results_values <- data.frame(
  section = character(),
  result_key = character(),
  value = character(),
  source = character(),
  stringsAsFactors = FALSE
)

add_result <- function(section, result_key, value, source) {
  value_text <- if (length(value) == 0L || is.na(value)) {
    NA_character_
  } else if (is.numeric(value)) {
    format(value, digits = 15, scientific = FALSE, trim = TRUE)
  } else {
    as.character(value)
  }
  results_values <<- rbind(
    results_values,
    data.frame(
      section = section,
      result_key = result_key,
      value = value_text,
      source = source,
      stringsAsFactors = FALSE
    )
  )
  invisible(value)
}

assert_close <- function(actual, expected, label, tolerance = TOL) {
  if (
    length(actual) != 1L || length(expected) != 1L ||
    !is.finite(actual) || !is.finite(expected) ||
    abs(actual - expected) > tolerance
  ) {
    stop(
      label, " differs: actual = ", actual,
      ", expected = ", expected, ".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

safe_quantile <- function(x, probability) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) return(NA_real_)
  as.numeric(stats::quantile(x, probs = probability, names = FALSE, type = 7))
}

# Bias-adjusted Fisher-Pearson sample skewness.
adjusted_skewness <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 3L) return(NA_real_)
  centred <- x - mean(x)
  m2 <- mean(centred^2)
  if (!is.finite(m2) || m2 <= 0) return(NA_real_)
  g1 <- mean(centred^3) / (m2^(3 / 2))
  sqrt(n * (n - 1)) / (n - 2) * g1
}

flow_value <- function(flow, stage, column = "retained_n") {
  index <- match(stage, flow$stage)
  if (is.na(index)) stop("Sample-flow stage not found: ", stage, call. = FALSE)
  flow[[column]][[index]]
}

assert_files_exist(
  c(
    project_paths$master_table,
    project_paths$comparison_ids,
    project_paths$folds,
    project_paths$fold_metadata,
    project_paths$domain_mask,
    project_paths$predictor_stack,
    project_paths$clean_layers
  ),
  "Primary processed artefact"
)

master <- readRDS(project_paths$master_table)
comparison_ids <- as.character(readRDS(project_paths$comparison_ids))
if (length(comparison_ids) != EXPECTED_FINAL_N) {
  stop(
    "The saved common cohort no longer contains ", EXPECTED_FINAL_N,
    " observations; reconcile Chapter 4 before reporting results.",
    call. = FALSE
  )
}

v3_inputs <- load_version_inputs("V3", cfg)
final_data <- v3_inputs$data
v3_features <- v3_inputs$features
if (nrow(final_data) != EXPECTED_FINAL_N) {
  stop("Loaded V3 data do not match the common cohort size.", call. = FALSE)
}

cat("Primary artefacts loaded: common cohort n = ", nrow(final_data),
    "; selected-model specification = V3; predictors = ",
    length(v3_features), ".\n", sep = "")

#-------------------------------------------------------------------------------
# Part 2. Results 4.1 - Sample flow and final common comparison cohort --------
#' ## 4.1 Sample flow and final common comparison cohort

cat("\n=== Results 4.1: Sample flow and final common comparison cohort ===\n")

# 3) Reconstruct the sequential soil sample flow used by Table 4.1

soil_flow_path <- file.path(cfg$processed_dir, "audit", "soil_sample_flow.csv")
rock_flow_path <- file.path(cfg$processed_dir, "audit", "rock_sample_flow.csv")
sed_flow_path <- file.path(cfg$processed_dir, "audit", "sediment_sample_flow.csv")
cohort_audit_path <- file.path(cfg$processed_dir, "comparison_cohort_audit.csv")
assert_files_exist(c(soil_flow_path, rock_flow_path, sed_flow_path, cohort_audit_path))

soil_flow <- readr::read_csv(soil_flow_path, show_col_types = FALSE)
rock_flow <- readr::read_csv(rock_flow_path, show_col_types = FALSE)
sed_flow <- readr::read_csv(sed_flow_path, show_col_types = FALSE)
cohort_audit <- readr::read_csv(cohort_audit_path, show_col_types = FALSE)

soil_stage_names <- c(
  imported = "imported",
  source_dedup = "source_record_deduplication",
  value_bbox = "required_fields_Fe_range_and_bbox",
  method = "analytical_method_filter",
  waste = "confirmed_or_structured_anthropogenic_waste_filter",
  qaqc = "primary_QAQC_filter"
)
soil_counts <- vapply(
  soil_stage_names,
  function(stage) flow_value(soil_flow, stage),
  numeric(1)
)

n_land_soil <- sum(cohort_audit$valid_land_domain, na.rm = TRUE)
n_final <- sum(cohort_audit$valid_land_domain & cohort_audit$complete_required_v4,
               na.rm = TRUE)

if (soil_counts[["qaqc"]] != nrow(cohort_audit)) {
  stop("Primary-soil count differs between sample-flow and cohort audit.",
       call. = FALSE)
}
if (n_final != length(comparison_ids)) {
  stop("Sequential land/V4 filtering does not reproduce the common cohort.",
       call. = FALSE)
}

retained_n <- c(
  soil_counts,
  land = n_land_soil,
  final_v4 = n_final
)
excluded_previous <- c(NA_real_, head(retained_n, -1L) - tail(retained_n, -1L))
retained_pct <- 100 * retained_n / retained_n[[1L]]

table_4_1 <- data.frame(
  stage = c(
    "Imported AGDB4 soil records",
    "After source-identifier duplicate resolution",
    "After required-field, Fe-range and bounding-box filters",
    "After analytical-method restriction",
    "After waste and tailings exclusions",
    "After AGDB4 QAQC exclusions",
    "Within the final land-domain mask",
    "Complete for log10(Fe_wt%) and all 32 V4 predictors"
  ),
  retained_n = as.integer(retained_n),
  excluded_since_previous = as.integer(excluded_previous),
  retained_percentage_of_imported = retained_pct,
  stringsAsFactors = FALSE
)
readr::write_csv(table_4_1, file.path(output_dir, "table_4_1_sample_flow.csv"))
cat("4.1 Sequential soil sample flow:\n")
print(table_4_1, row.names = FALSE)

keys_4_1 <- c(
  "N_imported_soil", "N_after_source_dedup", "N_after_value_bbox",
  "N_after_method", "N_after_waste", "N_primary_soil",
  "N_land_soil", "N_final_common_cohort"
)
for (index in seq_along(keys_4_1)) {
  add_result("4.1", keys_4_1[[index]], retained_n[[index]], "table_4_1_sample_flow.csv")
}
percentage_keys_4_1 <- c(
  "pct_imported", "pct_after_source_dedup", "pct_after_value_bbox",
  "pct_after_method", "pct_after_waste", "pct_after_QAQC",
  "pct_within_land", "retention_pct"
)
for (index in seq_along(percentage_keys_4_1)) {
  add_result(
    "4.1", percentage_keys_4_1[[index]], retained_pct[[index]],
    "table_4_1_sample_flow.csv"
  )
}
add_result("4.1", "N_excluded_source_duplicates", excluded_previous[[2]],
           "table_4_1_sample_flow.csv")
add_result("4.1", "N_excluded_value_bbox", excluded_previous[[3]],
           "table_4_1_sample_flow.csv")
add_result("4.1", "N_excluded_method", excluded_previous[[4]],
           "table_4_1_sample_flow.csv")
add_result("4.1", "N_excluded_waste", excluded_previous[[5]],
           "table_4_1_sample_flow.csv")
add_result("4.1", "N_excluded_QAQC", excluded_previous[[6]],
           "table_4_1_sample_flow.csv")
add_result("4.1", "N_excluded_land", excluded_previous[[7]],
           "table_4_1_sample_flow.csv")
add_result("4.1", "N_excluded_incomplete_V4_after_land", excluded_previous[[8]],
           "table_4_1_sample_flow.csv")

# 4) Quantify the V4-driven restriction relative to V1 on the same land domain

assert_required_columns(
  master,
  c("sample_id", "grid_id", "grid_x", "grid_y", "domain_mask", "Fe_log10",
    feature_sets$V4),
  "master table"
)
land <- !is.na(master$domain_mask) & master$domain_mask == 1
v1_complete_land <- land & stats::complete.cases(
  master[, c("Fe_log10", feature_sets$V1), drop = FALSE]
)
v4_complete_land <- land & stats::complete.cases(
  master[, c("Fe_log10", feature_sets$V4), drop = FALSE]
)

if (!setequal(master$sample_id[v4_complete_land], comparison_ids)) {
  stop("V4-complete land-domain rows differ from saved comparison IDs.",
       call. = FALSE)
}

n_v1_complete <- sum(v1_complete_land)
n_v1_cells <- dplyr::n_distinct(master$grid_id[v1_complete_land])
n_v4_cells <- dplyr::n_distinct(master$grid_id[v4_complete_land])
n_v1_minus_v4 <- sum(v1_complete_land & !v4_complete_land)
n_v1_cells_minus_v4 <- length(setdiff(
  unique(master$grid_id[v1_complete_land]),
  unique(master$grid_id[v4_complete_land])
))
v4_loss_pct <- 100 * n_v1_minus_v4 / n_v1_complete
v4_cell_loss_pct <- 100 * n_v1_cells_minus_v4 / n_v1_cells

cat(
  "4.1 V4-driven common-cohort restriction: V1-complete n = ", n_v1_complete,
  " across ", n_v1_cells, " cells; V4 removed ", n_v1_minus_v4,
  " observations (", round(v4_loss_pct, 3), "%) and ",
  n_v1_cells_minus_v4, " occupied cells (", round(v4_cell_loss_pct, 3),
  "%).\n", sep = ""
)

add_result("4.1", "N_V1_complete", n_v1_complete, "master_table.rds")
add_result("4.1", "N_V1_cells", n_v1_cells, "master_table.rds")
add_result("4.1", "N_V1_minus_V4", n_v1_minus_v4, "master_table.rds")
add_result("4.1", "V4_loss_pct", v4_loss_pct, "master_table.rds")
add_result("4.1", "N_V1_cells_minus_V4", n_v1_cells_minus_v4,
           "master_table.rds")
add_result("4.1", "V4_cell_loss_pct", v4_cell_loss_pct, "master_table.rds")

cohort_loss_map_data <- master[v1_complete_land, c(
  "sample_id", "grid_id", "grid_x", "grid_y"
), drop = FALSE]
cohort_loss_map_data$status <- ifelse(
  cohort_loss_map_data$sample_id %in% comparison_ids,
  "Retained by V4 common cohort",
  "Excluded only by V4 completeness"
)
readr::write_csv(
  cohort_loss_map_data,
  file.path(output_dir, "v1_v4_cohort_loss_spatial_data.csv")
)
cat("Cohort-loss spatial data (", nrow(cohort_loss_map_data), " rows).\n", sep = "")

# Quantify the spatial effect of the V4 completeness restriction. The excluded
# cells are described by their pairwise separation and by the change in the
# occupied-cell convex hull; no subjective concentration threshold is imposed.

v1_cells_xy <- master[v1_complete_land, c("grid_id", "grid_x", "grid_y")] |>
  dplyr::distinct()
v4_cells_xy <- master[v4_complete_land, c("grid_id", "grid_x", "grid_y")] |>
  dplyr::distinct()
lost_cells_xy <- dplyr::anti_join(v1_cells_xy, v4_cells_xy, by = "grid_id")

lost_observations <- cohort_loss_map_data |>
  dplyr::filter(status == "Excluded only by V4 completeness")
lost_observations_sf <- sf::st_as_sf(
  lost_observations,
  coords = c("grid_x", "grid_y"),
  crs = cfg$crs$projected,
  remove = FALSE
) |>
  sf::st_transform(cfg$crs$geographic)
lost_observations_ll <- sf::st_coordinates(lost_observations_sf)
lost_observations$longitude <- lost_observations_ll[, 1]
lost_observations$latitude <- lost_observations_ll[, 2]

readr::write_csv(
  lost_observations,
  file.path(output_dir, "v1_v4_excluded_observations_locations.csv")
)
cat("4.1 Observations excluded only by the V4 completeness requirement:\n")
print(lost_observations, row.names = FALSE)

pairwise_distances_km <- function(x) {
  if (nrow(x) < 2L) return(numeric())
  as.numeric(stats::dist(as.matrix(x[, c("grid_x", "grid_y")]), method = "euclidean")) / 1000
}

convex_hull_area_km2 <- function(x, crs) {
  if (nrow(x) < 3L) return(NA_real_)
  geometry <- sf::st_as_sf(
    x,
    coords = c("grid_x", "grid_y"),
    crs = crs,
    remove = FALSE
  )
  as.numeric(
    sf::st_area(sf::st_convex_hull(sf::st_union(geometry)))
  ) / 1e6
}

v1_pair_dist_km <- pairwise_distances_km(v1_cells_xy)
lost_pair_dist_km <- pairwise_distances_km(lost_cells_xy)
lost_pair_median_km <- if (length(lost_pair_dist_km) > 0L) {
  stats::median(lost_pair_dist_km)
} else {
  NA_real_
}
lost_pair_percentile <- if (
  is.finite(lost_pair_median_km) && length(v1_pair_dist_km) > 0L
) {
  100 * mean(v1_pair_dist_km <= lost_pair_median_km)
} else {
  NA_real_
}

v1_hull_km2 <- convex_hull_area_km2(v1_cells_xy, cfg$crs$projected)
v4_hull_km2 <- convex_hull_area_km2(v4_cells_xy, cfg$crs$projected)
hull_area_change_pct <- if (is.finite(v1_hull_km2) && v1_hull_km2 > 0) {
  100 * (v4_hull_km2 - v1_hull_km2) / v1_hull_km2
} else {
  NA_real_
}

spatial_support_summary <- data.frame(
  v1_occupied_cells = nrow(v1_cells_xy),
  v4_occupied_cells = nrow(v4_cells_xy),
  lost_cells = nrow(lost_cells_xy),
  lost_cell_pair_median_distance_km = lost_pair_median_km,
  lost_pair_distance_percentile_among_all_v1_pairs = lost_pair_percentile,
  v1_convex_hull_area_km2 = v1_hull_km2,
  v4_convex_hull_area_km2 = v4_hull_km2,
  convex_hull_area_change_pct = hull_area_change_pct,
  stringsAsFactors = FALSE
)

readr::write_csv(
  spatial_support_summary,
  file.path(output_dir, "v1_v4_spatial_support_summary.csv")
)
cat("4.1 Quantitative spatial-support effect of the V4 completeness restriction:\n")
print(spatial_support_summary, row.names = FALSE)

add_result(
  "4.1", "V4_lost_cell_pair_median_distance_km", lost_pair_median_km,
  "v1_v4_spatial_support_summary.csv"
)
add_result(
  "4.1", "V4_lost_pair_distance_percentile", lost_pair_percentile,
  "v1_v4_spatial_support_summary.csv"
)
add_result(
  "4.1", "V1_V4_convex_hull_area_change_pct", hull_area_change_pct,
  "v1_v4_spatial_support_summary.csv"
)

# 5) Count retained ancillary records and co-location-aggregated sites

n_rock_records <- flow_value(rock_flow, "primary_QAQC_filter")
n_sed_records <- flow_value(sed_flow, "primary_QAQC_filter")
clean_layers <- readRDS(project_paths$clean_layers)
rock_sites <- aggregate_colocated_points(
  sf::st_transform(clean_layers$rock, cfg$crs$projected), "rock"
)
sed_sites <- aggregate_colocated_points(
  sf::st_transform(clean_layers$sediment, cfg$crs$projected), "sediment"
)
n_rock_sites <- nrow(rock_sites)
n_sed_sites <- nrow(sed_sites)

ancillary_counts <- data.frame(
  medium = c("rock", "sediment"),
  retained_records = c(n_rock_records, n_sed_records),
  colocated_aggregated_sites = c(n_rock_sites, n_sed_sites),
  stringsAsFactors = FALSE
)
readr::write_csv(
  ancillary_counts,
  file.path(output_dir, "ancillary_record_site_counts.csv")
)
cat("4.1 Retained ancillary records and co-location-aggregated sites:\n")
print(ancillary_counts, row.names = FALSE)
add_result("4.1", "N_rock_records", n_rock_records,
           "audit/rock_sample_flow.csv")
add_result("4.1", "N_sed_records", n_sed_records,
           "audit/sediment_sample_flow.csv")
add_result("4.1", "N_rock_sites", n_rock_sites,
           "clean_fe_layers.rds + aggregate_colocated_points()")
add_result("4.1", "N_sed_sites", n_sed_sites,
           "clean_fe_layers.rds + aggregate_colocated_points()")

#-------------------------------------------------------------------------------
# Part 3. Results 4.2 - Response distribution and sampling structure --------
#' ## 4.2 Response distribution and sampling structure

cat("\n=== Results 4.2: Response distribution and sampling structure ===\n")

#' ### 4.2.1 Fe distribution, analytical composition and vertical support
# 6) Results 4.2.1: final-cohort Fe distribution, method and depth summaries

cat("\n4.2.1 Fe distribution, analytical composition and vertical support:\n")

fe_raw <- final_data$Fe_pct
fe_log <- final_data$Fe_log10
fe_summary <- data.frame(
  scale = c("Fe_pct", "Fe_log10"),
  n = c(sum(is.finite(fe_raw)), sum(is.finite(fe_log))),
  median = c(stats::median(fe_raw), stats::median(fe_log)),
  q25 = c(safe_quantile(fe_raw, 0.25), safe_quantile(fe_log, 0.25)),
  q75 = c(safe_quantile(fe_raw, 0.75), safe_quantile(fe_log, 0.75)),
  min = c(min(fe_raw), min(fe_log)),
  max = c(max(fe_raw), max(fe_log)),
  skewness_adjusted_fisher_pearson = c(
    adjusted_skewness(fe_raw), adjusted_skewness(fe_log)
  ),
  stringsAsFactors = FALSE
)
readr::write_csv(fe_summary, file.path(output_dir, "final_cohort_Fe_summary.csv"))
cat("4.2.1 Final-cohort Fe distribution (adjusted Fisher-Pearson skewness):\n")
print(fe_summary, row.names = FALSE)

raw_row <- fe_summary[fe_summary$scale == "Fe_pct", ]
log_row <- fe_summary[fe_summary$scale == "Fe_log10", ]
for (item in list(
  c("median_Fe", raw_row$median), c("Q25_Fe", raw_row$q25),
  c("Q75_Fe", raw_row$q75), c("min_Fe", raw_row$min),
  c("max_Fe", raw_row$max), c("skew_raw", raw_row$skewness_adjusted_fisher_pearson),
  c("median_logFe", log_row$median), c("Q25_logFe", log_row$q25),
  c("Q75_logFe", log_row$q75), c("min_logFe", log_row$min),
  c("max_logFe", log_row$max), c("skew_log", log_row$skewness_adjusted_fisher_pearson)
)) {
  add_result("4.2.1", item[[1]], as.numeric(item[[2]]), "final_cohort_Fe_summary.csv")
}

method_summary <- final_data |>
  dplyr::count(Fe_AM, name = "n") |>
  dplyr::mutate(percentage = 100 * n / nrow(final_data)) |>
  dplyr::arrange(Fe_AM)
readr::write_csv(
  method_summary,
  file.path(output_dir, "final_cohort_method_summary.csv")
)
cat("4.2.1 Analytical-method composition:\n")
print(method_summary, row.names = FALSE)
for (method in c("AES_HF", "WDX_FUSE")) {
  row <- method_summary[method_summary$Fe_AM == method, , drop = FALSE]
  if (nrow(row) != 1L) stop("Expected final-cohort method not found: ", method,
                            call. = FALSE)
  add_result("4.2.1", paste0("N_", method), row$n,
             "final_cohort_method_summary.csv")
  add_result("4.2.1", paste0(method, "_pct"), row$percentage,
             "final_cohort_method_summary.csv")
}

n_date <- sum(!is.na(final_data$date_collect))
date_pct <- 100 * n_date / nrow(final_data)
earliest_year <- if (n_date > 0L) {
  as.integer(format(min(final_data$date_collect, na.rm = TRUE), "%Y"))
} else {
  NA_integer_
}
latest_year <- if (n_date > 0L) {
  as.integer(format(max(final_data$date_collect, na.rm = TRUE), "%Y"))
} else {
  NA_integer_
}
cat(
  "4.2.1 Collection dates: ", n_date, "/", nrow(final_data), " (",
  round(date_pct, 3), "%), spanning ", earliest_year, "-", latest_year, ".\n",
  sep = ""
)
add_result("4.2.1", "N_date", n_date, "master_table.rds")
add_result("4.2.1", "date_pct", date_pct, "master_table.rds")
add_result("4.2.1", "earliest_year", earliest_year, "master_table.rds")
add_result("4.2.1", "latest_year", latest_year, "master_table.rds")

parseable_depth <- is.finite(final_data$depth_midpoint_cm)
n_depth_parseable <- sum(parseable_depth)
depth_summary <- final_data[parseable_depth, , drop = FALSE] |>
  dplyr::count(depth_class, name = "n") |>
  dplyr::mutate(percentage_of_parseable = 100 * n / n_depth_parseable) |>
  dplyr::arrange(factor(depth_class, levels = c("shallow", "intermediate", "deep")))
readr::write_csv(
  depth_summary,
  file.path(output_dir, "final_cohort_depth_summary.csv")
)
cat(
  "4.2.1 Parseable depth midpoints: ", n_depth_parseable, "/",
  nrow(final_data), " (",
  round(100 * n_depth_parseable / nrow(final_data), 3), "%).\n", sep = ""
)
cat("4.2.1 Depth classes among parseable observations:\n")
print(depth_summary, row.names = FALSE)
add_result("4.2.1", "N_depth_parseable", n_depth_parseable, "master_table.rds")
add_result("4.2.1", "depth_pct", 100 * n_depth_parseable / nrow(final_data),
           "master_table.rds")
for (depth_class in c("shallow", "intermediate", "deep")) {
  row <- depth_summary[depth_summary$depth_class == depth_class, , drop = FALSE]
  n_value <- if (nrow(row) == 0L) 0L else row$n
  pct_value <- if (nrow(row) == 0L) 0 else row$percentage_of_parseable
  add_result("4.2.1", paste0("N_", depth_class), n_value,
             "final_cohort_depth_summary.csv")
  add_result("4.2.1", paste0(depth_class, "_pct"), pct_value,
             "final_cohort_depth_summary.csv")
}


#-------------------------------------------------------------------------------
# Results publication design (from Results_Figures_4_1_to_4_5.R) --------
#' ### Publication figure design

# ---- 2b. Shared publication design (results figures) ------------------------

# "sans" resolves to Arial on Windows without requiring font registration.
font_family <- "sans"
colours <- list(
  ink = "darkslategray",
  muted = "lightskyblue4",
  grid = "azure3",
  teal_dark = "deepskyblue4",
  teal = "darkcyan",
  teal_light = "powderblue",
  blue = "steelblue",
  nodata = "gray85",
  water = "mintcream"
)
fe_colours <- grDevices::hcl.colors(6, palette = "BluYl")

theme_results <- function(base_size = 8.2) {
  ggplot2::theme_classic(base_family = font_family, base_size = base_size) +
    ggplot2::theme(
      axis.line = ggplot2::element_line(colour = colours$ink, linewidth = 0.34),
      axis.ticks = ggplot2::element_line(colour = colours$ink, linewidth = 0.32),
      axis.ticks.length = grid::unit(1.25, "mm"),
      axis.text = ggplot2::element_text(colour = colours$ink, size = base_size - 0.5),
      axis.title = ggplot2::element_text(colour = colours$ink, size = base_size),
      legend.title = ggplot2::element_text(face = "bold", size = base_size - 0.2),
      legend.text = ggplot2::element_text(size = base_size - 0.5),
      legend.key = ggplot2::element_blank(),
      legend.spacing.x = grid::unit(1.6, "mm"),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        face = "bold", colour = colours$ink, size = base_size
      ),
      plot.title = ggplot2::element_text(
        face = "bold", colour = colours$ink, size = base_size + 1.4,
        margin = ggplot2::margin(b = 1.2, unit = "mm")
      ),
      plot.subtitle = ggplot2::element_text(
        colour = colours$muted, size = base_size - 0.1, lineheight = 1.05,
        margin = ggplot2::margin(b = 2.8, unit = "mm")
      ),
      plot.caption = ggplot2::element_text(
        colour = colours$muted, size = base_size - 0.8,
        hjust = 0, lineheight = 1.05,
        margin = ggplot2::margin(t = 1.5, unit = "mm")
      ),
      plot.title.position = "plot",
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin = ggplot2::margin(2.2, 3.0, 2.2, 2.2, unit = "mm")
    )
}

save_figure <- function(plot, stem, width_mm, height_mm) {
  ggplot2::ggsave(
    file.path(output_dir, paste0(stem, ".pdf")),
    plot = plot, device = grDevices::cairo_pdf,
    width = width_mm, height = height_mm, units = "mm",
    bg = "white", limitsize = FALSE
  )
  ggplot2::ggsave(
    file.path(output_dir, paste0(stem, ".tiff")),
    plot = plot, device = "tiff", dpi = 600, compression = "lzw",
    width = width_mm, height = height_mm, units = "mm",
    bg = "white", limitsize = FALSE
  )
}

read_csv_base <- function(path) {
  utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
}

require_columns <- function(data, columns, label) {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop(label, " is missing column(s): ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
}


# Bridge: variable aliases for publication figures
cohort <- final_data
model_order <- paste0("V", 1:4)
results_fig_dir <- file.path(cfg$root, "figures", "results")
dir.create(results_fig_dir, recursive = TRUE, showWarnings = FALSE)

# Publication Figure 4.1 (replaces diagnostic version)

# ---- 4. Figure 4.1: response distributions ---------------------------------

fe_raw <- cohort$Fe_pct
fe_log <- cohort$Fe_log10
n_bins <- 22L
raw_breaks <- seq(min(fe_raw), max(fe_raw), length.out = n_bins + 1L)
log_breaks <- seq(min(fe_log), max(fe_log), length.out = n_bins + 1L)
max_count <- max(
  graphics::hist(fe_raw, breaks = raw_breaks, plot = FALSE)$counts,
  graphics::hist(fe_log, breaks = log_breaks, plot = FALSE)$counts
)
y_upper <- max_count * 1.06

histogram_data <- function(values, breaks, panel) {
  counts <- graphics::hist(values, breaks = breaks, plot = FALSE)
  data.frame(
    panel = panel,
    xmin = head(counts$breaks, -1L),
    xmax = tail(counts$breaks, -1L),
    count = counts$counts
  )
}

hist_data <- rbind(
  histogram_data(fe_raw, raw_breaks, "original"),
  histogram_data(fe_log, log_breaks, "modelling")
)
hist_data$panel <- factor(
  hist_data$panel,
  levels = c("original", "modelling")
)
hist_panel_labels <- c(
  original = "'(a) Original scale: near-total soil Fe (wt%)'",
  modelling = paste0(
    "bold('(b) Modelling scale: ' * ",
    "log[bold('10')](Fe[wt * '%']))"
  )
)

figure_4_1 <- ggplot2::ggplot(hist_data) +
  ggplot2::geom_rect(
    ggplot2::aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = count),
    fill = colours$teal_light, colour = colours$teal_dark, linewidth = 0.30
  ) +
  ggplot2::facet_wrap(
    ggplot2::vars(panel), nrow = 1, scales = "free_x",
    labeller = ggplot2::as_labeller(
      hist_panel_labels, default = ggplot2::label_parsed
    )
  ) +
  ggplot2::scale_x_continuous(
    breaks = scales::breaks_pretty(n = 6),
    expand = ggplot2::expansion(mult = c(0.01, 0.02))
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, y_upper), breaks = scales::breaks_pretty(n = 5),
    expand = ggplot2::expansion(mult = c(0, 0))
  ) +
  ggplot2::labs(
    title = NULL,
    subtitle = NULL,
    x = NULL,
    y = "Number of observations"
  ) +
  theme_results() +
  ggplot2::theme(panel.spacing.x = grid::unit(8, "mm"))

print(figure_4_1)
save_figure(
  figure_4_1, "Figure_4_1_Fe_distributions",
  width_mm = 180, height_mm = 88
)

#' ### 4.2.2 Spatial clustering and covariate-support context
# 7) Results 4.2.2: cell multiplicity and predictor-support context

cat("\n4.2.2 Spatial clustering and covariate-support context:\n")

cell_counts <- final_data |>
  dplyr::count(grid_id, name = "n_observations") |>
  dplyr::arrange(dplyr::desc(n_observations), grid_id)
n_grid_cells <- nrow(cell_counts)
n_multisample_cells <- sum(cell_counts$n_observations > 1L)
n_max_cell <- max(cell_counts$n_observations)
max_cell_row_weight_pct <- 100 * n_max_cell / nrow(final_data)
readr::write_csv(cell_counts, file.path(output_dir, "final_cohort_cell_counts.csv"))
cat(
  "4.2.2 Sampling cells: ", n_grid_cells, " occupied 1-km cells; ",
  n_multisample_cells, " contain >1 observation; maximum cell count = ",
  n_max_cell, " (", round(max_cell_row_weight_pct, 3),
  "% of row weight).\n", sep = ""
)
add_result("4.2.2", "N_grid_cells", n_grid_cells, "final_cohort_cell_counts.csv")
add_result("4.2.2", "N_multisample_cells", n_multisample_cells,
           "final_cohort_cell_counts.csv")
add_result("4.2.2", "N_max_cell", n_max_cell, "final_cohort_cell_counts.csv")
add_result("4.2.2", "max_cell_row_weight_pct", max_cell_row_weight_pct,
           "final_cohort_cell_counts.csv")

# Every observation in one 1-km cell must share the same raster predictor vector.
cell_predictor_check <- final_data |>
  dplyr::group_by(grid_id) |>
  dplyr::summarise(
    dplyr::across(dplyr::all_of(feature_sets$V4), dplyr::n_distinct),
    .groups = "drop"
  )
if (any(as.matrix(cell_predictor_check[, feature_sets$V4, drop = FALSE]) != 1L)) {
  stop("At least one 1-km cell contains inconsistent raster predictor values.",
       call. = FALSE)
}

# Infer support categories from the raster-derived predictors actually supplied
# to the model. Point-level rock_source/sed_source are retained only as geometry
# audit fields and are not used to classify model support at polygon boundaries.
derive_model_support <- function(support_missing, n_samples, medium) {
  if (any(!is.finite(support_missing)) || any(!is.finite(n_samples))) {
    stop(medium, " support predictors contain non-finite values.", call. = FALSE)
  }
  if (any(!(support_missing %in% c(0, 1)))) {
    stop(medium, "_support_missing must contain only 0/1.", call. = FALSE)
  }
  if (any(n_samples < 0 | abs(n_samples - round(n_samples)) > TOL)) {
    stop(medium, "_n_samples must contain non-negative integer counts.",
         call. = FALSE)
  }
  if (any(support_missing == 1 & n_samples != 0)) {
    stop(medium, " structural-missingness rows must have n_samples = 0.",
         call. = FALSE)
  }
  
  ifelse(
    support_missing == 1,
    "missing",
    ifelse(n_samples > 0, "unit_mean", "lith1_fallback")
  )
}

rock_model_support <- derive_model_support(
  final_data$rock_support_missing, final_data$rock_n_samples, "rock"
)
sed_model_support <- derive_model_support(
  final_data$sed_support_missing, final_data$sed_n_samples, "sediment"
)

support_summary <- dplyr::bind_rows(
  data.frame(source = rock_model_support, stringsAsFactors = FALSE) |>
    dplyr::count(source, name = "n") |>
    dplyr::mutate(medium = "rock"),
  data.frame(source = sed_model_support, stringsAsFactors = FALSE) |>
    dplyr::count(source, name = "n") |>
    dplyr::mutate(medium = "sediment")
) |>
  dplyr::mutate(percentage = 100 * n / nrow(final_data)) |>
  dplyr::select(medium, source, n, percentage) |>
  dplyr::arrange(
    medium,
    factor(source, levels = c("unit_mean", "lith1_fallback", "missing"))
  )

readr::write_csv(
  support_summary,
  file.path(output_dir, "final_cohort_geological_support_summary.csv")
)
cat("4.2.2 Raster-derived geological-support categories used by the model:\n")
print(support_summary, row.names = FALSE)

rock_point_support <- as.character(final_data$rock_source)
sed_point_support <- as.character(final_data$sed_source)
rock_support_mismatch <- sum(
  !is.na(rock_point_support) & rock_point_support != rock_model_support
)
sed_support_mismatch <- sum(
  !is.na(sed_point_support) & sed_point_support != sed_model_support
)
cat(
  "4.2.2 Support-representation audit: point/raster category mismatches = rock ",
  rock_support_mismatch, ", sediment ", sed_support_mismatch,
  ". These are diagnostic only; Results use raster-derived model support.\n",
  sep = ""
)
add_result(
  "4.2.2", "rock_point_raster_support_mismatch_n", rock_support_mismatch,
  "master_table.rds diagnostic"
)
add_result(
  "4.2.2", "sed_point_raster_support_mismatch_n", sed_support_mismatch,
  "master_table.rds diagnostic"
)

source_key <- c(
  unit_mean = "unit",
  lith1_fallback = "fallback",
  missing = "missing"
)
for (medium in c("rock", "sediment")) {
  for (source_value in names(source_key)) {
    row <- support_summary[
      support_summary$medium == medium & support_summary$source == source_value,
      , drop = FALSE
    ]
    n_value <- if (nrow(row) == 0L) 0L else row$n
    pct_value <- if (nrow(row) == 0L) 0 else row$percentage
    prefix <- if (medium == "sediment") "sed" else "rock"
    add_result("4.2.2", paste0("N_", prefix, "_", source_key[[source_value]]),
               n_value, "final_cohort_geological_support_summary.csv")
    add_result("4.2.2", paste0(prefix, "_", source_key[[source_value]], "_pct"),
               pct_value, "final_cohort_geological_support_summary.csv")
  }
}

nn_summary <- data.frame(
  predictor = c("nn_rock_dist_km", "nn_sed_dist_km"),
  median = c(stats::median(final_data$nn_rock_dist_km),
             stats::median(final_data$nn_sed_dist_km)),
  q25 = c(safe_quantile(final_data$nn_rock_dist_km, 0.25),
          safe_quantile(final_data$nn_sed_dist_km, 0.25)),
  q75 = c(safe_quantile(final_data$nn_rock_dist_km, 0.75),
          safe_quantile(final_data$nn_sed_dist_km, 0.75)),
  min = c(min(final_data$nn_rock_dist_km), min(final_data$nn_sed_dist_km)),
  max = c(max(final_data$nn_rock_dist_km), max(final_data$nn_sed_dist_km)),
  stringsAsFactors = FALSE
)
readr::write_csv(
  nn_summary,
  file.path(output_dir, "final_cohort_nearest_neighbour_distance_summary.csv")
)
cat("4.2.2 Nearest aggregated ancillary-site distances (km):\n")
print(nn_summary, row.names = FALSE)
add_result("4.2.2", "median_nn_rock_dist", nn_summary$median[[1]],
           "final_cohort_nearest_neighbour_distance_summary.csv")
add_result("4.2.2", "median_nn_sed_dist", nn_summary$median[[2]],
           "final_cohort_nearest_neighbour_distance_summary.csv")
add_result("4.2.2", "Q25_nn_rock_dist", nn_summary$q25[[1]],
           "final_cohort_nearest_neighbour_distance_summary.csv")
add_result("4.2.2", "Q75_nn_rock_dist", nn_summary$q75[[1]],
           "final_cohort_nearest_neighbour_distance_summary.csv")
add_result("4.2.2", "Q25_nn_sed_dist", nn_summary$q25[[2]],
           "final_cohort_nearest_neighbour_distance_summary.csv")
add_result("4.2.2", "Q75_nn_sed_dist", nn_summary$q75[[2]],
           "final_cohort_nearest_neighbour_distance_summary.csv")

pf <- final_data$PF_prob
pf <- pf[is.finite(pf)]
pf_pct_values <- if (length(pf) > 0L && max(pf) <= 1.5) pf * 100 else pf
pf_median <- stats::median(pf_pct_values)
n_pf_above_50 <- sum(pf_pct_values > 50)
pf_above_50_pct <- 100 * n_pf_above_50 / length(pf_pct_values)
pf_summary <- data.frame(
  n = length(pf_pct_values),
  median_probability_pct = pf_median,
  n_above_50_pct = n_pf_above_50,
  percentage_above_50_pct = pf_above_50_pct
)
readr::write_csv(
  pf_summary,
  file.path(output_dir, "final_cohort_permafrost_summary.csv")
)
cat("4.2.2 Near-surface permafrost probability at sampled locations:\n")
print(pf_summary, row.names = FALSE)
add_result("4.2.2", "PF_median", pf_median, "final_cohort_permafrost_summary.csv")
add_result("4.2.2", "N_PF_above_50", n_pf_above_50,
           "final_cohort_permafrost_summary.csv")
add_result("4.2.2", "PF_above_50_pct", pf_above_50_pct,
           "final_cohort_permafrost_summary.csv")

#-------------------------------------------------------------------------------
# Part 4. Results 4.3 - Paired V1-V4 performance and model selection --------
#' ## 4.3 Paired V1-V4 performance and model selection

cat("\n=== Results 4.3: Paired V1-V4 performance and model selection ===\n")

# 8) Rebuild the comparison table from model-specific authoritative metrics

versions <- paste0("V", 1:4)
metric_paths <- file.path(
  cfg$root, paste0("Model_V", 1:4), "outputs", "model_metrics.csv"
)
oof_paths <- file.path(
  cfg$root, paste0("Model_V", 1:4), "outputs", "spatial_oof_predictions.csv"
)
assert_files_exist(c(metric_paths, oof_paths))
assert_same_comparison_ids(oof_paths)

comparison <- dplyr::bind_rows(
  lapply(metric_paths, readr::read_csv, show_col_types = FALSE)
) |>
  dplyr::mutate(
    model = factor(model, levels = versions),
    m_try = pmax(1L, floor(sqrt(n_features)))
  ) |>
  dplyr::arrange(model)

if (any(comparison$n != EXPECTED_FINAL_N)) {
  stop("One or more model metrics use a different comparison cohort.",
       call. = FALSE)
}

selected_version <- as.character(comparison$model[which.max(comparison$spatial_log_R2)])
add_result("4.3", "selected_version", selected_version,
           "Model_V1-V4/outputs/model_metrics.csv")

# Main-text Table 4.2 columns; full metrics are retained separately.
table_4_2 <- comparison |>
  dplyr::select(
    model, p = n_features, m_try,
    spatial_R2 = spatial_log_R2,
    spatial_RMSE = spatial_log_RMSE,
    spatial_MAE = spatial_log_MAE,
    spatial_bias = spatial_log_bias,
    observation_random_R2 = observation_random_log_R2
  )
readr::write_csv(table_4_2, file.path(output_dir, "table_4_2_model_performance.csv"))
readr::write_csv(comparison, file.path(output_dir, "all_model_metrics.csv"))
cat("4.3 Paired V1-V4 log10-scale performance:\n")
print(table_4_2, row.names = FALSE)
cat(
  "4.3 Selection: ", selected_version,
  " has the maximum pooled spatially blocked R2 = ",
  round(max(comparison$spatial_log_R2), 6), ".\n", sep = ""
)

selection_uncertainty <- data.frame(
  quantity = c(
    "V2_to_V3_spatial_R2_difference",
    "V2_V3_ranking_stability",
    "post_selection_optimism_magnitude"
  ),
  value = c(
    format(
      comparison$spatial_log_R2[comparison$model == "V3"] -
        comparison$spatial_log_R2[comparison$model == "V2"],
      digits = 15, scientific = FALSE, trim = TRUE
    ),
    NA_character_,
    NA_character_
  ),
  status = c(
    "point estimate available",
    "not quantified by the fixed single-partition design",
    "not quantified by the fixed single-partition design"
  ),
  stringsAsFactors = FALSE
)
readr::write_csv(
  selection_uncertainty,
  file.path(output_dir, "v2_v3_selection_uncertainty.csv")
)
cat("4.3 Selection uncertainty supported by the current validation design:\n")
print(selection_uncertainty, row.names = FALSE)

add_result(
  "4.3", "V2_V3_ranking_stability_quantified", "No",
  "v2_v3_selection_uncertainty.csv"
)
add_result(
  "4.3", "post_selection_optimism_magnitude_quantified", "No",
  "v2_v3_selection_uncertainty.csv"
)

# Figure 4.2 compares all three pre-specified CV designs; model selection still
# uses only spatially blocked CV.
# Publication Figure 4.2 (replaces diagnostic version)

# ---- 5. Figure 4.2: V1-V4 R^2 across three CV designs -----------------------

r2_data <- rbind(
  data.frame(
    model = model_order,
    validation = "Spatially blocked",
    R2 = comparison$spatial_log_R2
  ),
  data.frame(
    model = model_order,
    validation = "Grid-grouped random",
    R2 = comparison$random_log_R2
  ),
  data.frame(
    model = model_order,
    validation = "Observation-level random",
    R2 = comparison$observation_random_log_R2
  )
)

r2_data$model <- factor(
  r2_data$model,
  levels = model_order
)

validation_levels <- c(
  "Spatially blocked",
  "Grid-grouped random",
  "Observation-level random"
)

r2_data$validation <- factor(
  r2_data$validation,
  levels = validation_levels
)

r2_data$label <- sprintf(
  "%.3f",
  r2_data$R2
)

r2_data$label_y <- r2_data$R2 + ifelse(
  r2_data$validation == "Observation-level random",
  0.013,
  -0.013
)

validation_colours <- c(
  "Spatially blocked" = "palegreen2",
  "Grid-grouped random" = "purple",
  "Observation-level random" = "steelblue"
)

validation_shapes <- c(
  "Spatially blocked" = 16,
  "Grid-grouped random" = 15,
  "Observation-level random" = 17
)

figure_4_2 <- ggplot2::ggplot(
  r2_data,
  ggplot2::aes(
    x = model,
    y = R2
  )
) +
  ggplot2::geom_hline(
    yintercept = 0,
    colour = colours$ink,
    linewidth = 0.34
  ) +
  ggplot2::geom_line(
    ggplot2::aes(
      group = validation,
      colour = validation
    ),
    linewidth = 0.65,
    show.legend = FALSE
  ) +
  ggplot2::geom_point(
    ggplot2::aes(
      colour = validation,
      shape = validation
    ),
    size = 2.7,
    stroke = 0.35
  ) +
  ggplot2::geom_text(
    ggplot2::aes(
      y = label_y,
      label = label,
      colour = validation
    ),
    family = font_family,
    size = 2.35,
    show.legend = FALSE
  ) +
  ggplot2::scale_colour_manual(
    values = validation_colours,
    breaks = validation_levels,
    name = "Cross-validation design"
  ) +
  ggplot2::scale_shape_manual(
    values = validation_shapes,
    breaks = validation_levels,
    name = "Cross-validation design"
  ) +
  ggplot2::guides(
    colour = ggplot2::guide_legend(
      title.position = "left",
      title.hjust = 0,
      nrow = 1,
      byrow = TRUE,
      order = 1
    ),
    shape = ggplot2::guide_legend(
      title.position = "left",
      title.hjust = 0,
      nrow = 1,
      byrow = TRUE,
      order = 1
    )
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 0.30),
    breaks = seq(
      0,
      0.30,
      by = 0.05
    ),
    labels = scales::label_number(
      accuracy = 0.01
    ),
    expand = ggplot2::expansion(
      mult = c(0, 0)
    )
  ) +
  ggplot2::labs(
    title = NULL,
    subtitle = NULL,
    x = "Pre-specified model version",
    y = expression(
      "Pooled OOF " *
        italic(R)^2 *
        " for " *
        log[10](plain(Fe)[plain(wt) * "%"])
    )
  ) +
  theme_results() +
  ggplot2::theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification = "left",
    legend.box.just = "left",
    
    # Move the complete legend row slightly towards the left plot margin.
    legend.box.margin = ggplot2::margin(
      t = 0,
      r = 0,
      b = 0,
      l = -1.5,
      unit = "mm"
    ),
    
    # Compact the legend without changing the plotted data-label sizes.
    legend.title = ggplot2::element_text(
      family = font_family,
      face = "bold",
      size = 7.4
    ),
    legend.text = ggplot2::element_text(
      family = font_family,
      size = 7.2
    ),
    legend.key.width = grid::unit(
      3.5,
      "mm"
    ),
    legend.key.height = grid::unit(
      3.5,
      "mm"
    ),
    legend.spacing.x = grid::unit(
      0.8,
      "mm"
    ),
    legend.box.spacing = grid::unit(
      1.0,
      "mm"
    ),
    legend.margin = ggplot2::margin(
      t = 0,
      r = 0,
      b = 1.2,
      l = 0,
      unit = "mm"
    ),
    
    panel.grid.major.y = ggplot2::element_line(
      colour = colours$grid,
      linewidth = 0.28
    )
  )

print(figure_4_2)
save_figure(
  figure_4_2,
  "Figure_4_2_V1_V4_R2_by_CV_design",
  width_mm = 170,
  height_mm = 99
)

#-------------------------------------------------------------------------------
# Part 5. Results 4.4 - Validation diagnostics for selected V3 --------
#' ## 4.4 Validation diagnostics for the selected V3 model

cat("\n=== Results 4.4: Validation diagnostics for selected V3 ===\n")

#' ### 4.4.1 Contrast among fold-assignment designs
# 9) Results 4.4.1: validation-design comparison

cat("\n4.4.1 Validation-design comparison:\n")

v3_metrics <- comparison[as.character(comparison$model) == "V3", , drop = FALSE]
if (nrow(v3_metrics) != 1L) stop("V3 metrics row is missing or duplicated.",
                                 call. = FALSE)
table_4_3 <- data.frame(
  design = c(
    "Observation-level random CV",
    "Grid-grouped random CV",
    "Spatially blocked CV",
    "OOB evaluation"
  ),
  assignment_or_exclusion_unit = c(
    "Individual observation",
    "Complete 1-km cell; observation-load balancing",
    "Complete 50-km hexagonal block",
    "Bootstrap exclusion of individual observations"
  ),
  same_1km_cell_may_enter_training_and_testing = c("Yes", "No", "No", "Yes"),
  log10_R2 = c(
    v3_metrics$observation_random_log_R2,
    v3_metrics$random_log_R2,
    v3_metrics$spatial_log_R2,
    v3_metrics$OOB_log_R2
  ),
  stringsAsFactors = FALSE
)
readr::write_csv(
  table_4_3,
  file.path(output_dir, "table_4_3_validation_designs.csv")
)
cat("4.4.1 V3 validation-design comparison:\n")
print(table_4_3, row.names = FALSE)

#' ### 4.4.2 Fold-wise and pooled OOF behaviour
# 10) Results 4.4.2: fold-wise and pooled OOF diagnostics

cat("\n4.4.2 Fold-wise and pooled OOF behaviour:\n")

v3_output_dir <- file.path(cfg$root, "Model_V3", "outputs")
spatial_fold_metrics_path <- file.path(v3_output_dir, "spatial_fold_metrics.csv")
v3_oof_path <- file.path(v3_output_dir, "spatial_oof_predictions.csv")
assert_files_exist(c(spatial_fold_metrics_path, v3_oof_path))
spatial_fold_metrics <- readr::read_csv(
  spatial_fold_metrics_path,
  show_col_types = FALSE
)
v3_oof <- readr::read_csv(
  v3_oof_path,
  col_types = readr::cols(
    .default = readr::col_guess(),
    sample_id = readr::col_character()
  ),
  show_col_types = FALSE
)
v3_oof$sample_id <- trimws(as.character(v3_oof$sample_id))
final_data$sample_id <- trimws(as.character(final_data$sample_id))

if (
  anyNA(v3_oof$sample_id) ||
  any(!nzchar(v3_oof$sample_id)) ||
  anyDuplicated(v3_oof$sample_id)
) {
  stop("V3 spatial-OOF sample_id values must be unique, non-missing character identifiers.",
       call. = FALSE)
}
if (!setequal(v3_oof$sample_id, final_data$sample_id)) {
  stop("V3 spatial-OOF sample IDs do not match the final comparison cohort.",
       call. = FALSE)
}

# Retain the original Results audit calculation. Appendix B is generated once,
# in its own final section, directly from these saved OOF predictions.
fold_structure <- final_data |>
  dplyr::group_by(spatial_fold) |>
  dplyr::summarise(
    n_observations = dplyr::n(),
    n_cells = dplyr::n_distinct(grid_id),
    observed_log_sd = stats::sd(Fe_log10),
    .groups = "drop"
  ) |>
  dplyr::rename(fold = spatial_fold)
fold_diagnostics <- dplyr::left_join(
  spatial_fold_metrics,
  fold_structure,
  by = "fold"
)
if (any(fold_diagnostics$n_test != fold_diagnostics$n_observations)) {
  stop("Fold metrics and final-cohort fold membership disagree.", call. = FALSE)
}
readr::write_csv(
  fold_diagnostics,
  file.path(output_dir, "v3_spatial_fold_diagnostics.csv")
)
cat("4.4.2 V3 spatial-fold diagnostics:\n")
print(fold_diagnostics, row.names = FALSE)

range_values <- list(
  fold_n_min = min(fold_diagnostics$n_test),
  fold_n_max = max(fold_diagnostics$n_test),
  fold_cell_min = min(fold_diagnostics$n_cells),
  fold_cell_max = max(fold_diagnostics$n_cells),
  fold_R2_min = min(fold_diagnostics$log_R2),
  fold_R2_max = max(fold_diagnostics$log_R2),
  fold_RMSE_min = min(fold_diagnostics$log_RMSE),
  fold_RMSE_max = max(fold_diagnostics$log_RMSE),
  fold_bias_min = min(fold_diagnostics$log_bias),
  fold_bias_max = max(fold_diagnostics$log_bias),
  fold_sd_min = min(fold_diagnostics$observed_log_sd),
  fold_sd_max = max(fold_diagnostics$observed_log_sd),
  obs_range_min = min(v3_oof$observed_Fe_log10),
  obs_range_max = max(v3_oof$observed_Fe_log10),
  pred_range_min = min(v3_oof$predicted_Fe_log10),
  pred_range_max = max(v3_oof$predicted_Fe_log10)
)
for (key in names(range_values)) {
  add_result("4.4.2", key, range_values[[key]],
             "v3_spatial_fold_diagnostics.csv / spatial_oof_predictions.csv")
}
cat(
  "4.4.2 Fold ranges: n = ", range_values$fold_n_min, "-", range_values$fold_n_max,
  "; cells = ", range_values$fold_cell_min, "-", range_values$fold_cell_max,
  "; R2 = ", round(range_values$fold_R2_min, 6), "-",
  round(range_values$fold_R2_max, 6),
  "; RMSE = ", round(range_values$fold_RMSE_min, 6), "-",
  round(range_values$fold_RMSE_max, 6),
  "; bias = ", round(range_values$fold_bias_min, 6), "-",
  round(range_values$fold_bias_max, 6),
  "; observed SD = ", round(range_values$fold_sd_min, 6), "-",
  round(range_values$fold_sd_max, 6), ".\n", sep = ""
)
cat(
  "4.4.2 OOF ranges: observed log10(Fe_wt%) = ",
  round(range_values$obs_range_min, 6), "-", round(range_values$obs_range_max, 6),
  "; predicted = ", round(range_values$pred_range_min, 6), "-",
  round(range_values$pred_range_max, 6), ".\n", sep = ""
)

recomputed_v3_oof <- regression_metrics(
  v3_oof$observed_Fe_log10,
  v3_oof$predicted_Fe_log10
)
assert_close(recomputed_v3_oof$R2, v3_metrics$spatial_log_R2,
             "Recomputed V3 pooled spatial R2")
assert_close(recomputed_v3_oof$RMSE, v3_metrics$spatial_log_RMSE,
             "Recomputed V3 pooled spatial RMSE")
assert_close(recomputed_v3_oof$MAE, v3_metrics$spatial_log_MAE,
             "Recomputed V3 pooled spatial MAE")
assert_close(recomputed_v3_oof$bias, v3_metrics$spatial_log_bias,
             "Recomputed V3 pooled spatial bias")

# Publication Figure 4.3 (replaces diagnostic version)

# ---- 6. Figure 4.3: selected V3 spatial OOF predictions ---------------------

v3_metrics <- comparison[comparison$model == "V3", , drop = FALSE]
shared_range <- range(
  c(v3_oof$observed_Fe_log10, v3_oof$predicted_Fe_log10),
  finite = TRUE
)
axis_limits <- c(
  floor(shared_range[[1]] * 10) / 10,
  ceiling(shared_range[[2]] * 10) / 10
)
axis_span <- diff(axis_limits)
metric_r2_label <- sprintf(
  "'Spatial OOF ' * italic(R)^2 * ' = %.3f'",
  v3_metrics$spatial_log_R2
)
metric_rmse_label <- sprintf(
  "'RMSE = %.3f'", v3_metrics$spatial_log_RMSE
)
metric_n_label <- sprintf(
  "italic(n) * ' = %d'", nrow(v3_oof)
)

figure_4_3 <- ggplot2::ggplot(
  v3_oof,
  ggplot2::aes(x = observed_Fe_log10, y = predicted_Fe_log10)
) +
  ggplot2::geom_abline(
    intercept = 0, slope = 1,
    colour = colours$muted, linewidth = 0.62
  ) +
  ggplot2::geom_point(
    shape = 16, size = 1.35, alpha = 0.56,
    colour = colours$teal
  ) +
  ggplot2::annotate(
    "text",
    x = axis_limits[[1]] + 0.045 * axis_span,
    y = axis_limits[[2]] - 0.045 * axis_span,
    label = metric_r2_label,
    hjust = 0, vjust = 1,
    parse = TRUE,
    family = font_family, size = 2.45,
    colour = colours$ink
  ) +
  ggplot2::annotate(
    "text",
    x = axis_limits[[1]] + 0.045 * axis_span,
    y = axis_limits[[2]] - 0.080 * axis_span,
    label = metric_rmse_label,
    hjust = 0, vjust = 1,
    parse = TRUE,
    family = font_family, size = 2.45,
    colour = colours$ink
  ) +
  ggplot2::annotate(
    "text",
    x = axis_limits[[1]] + 0.045 * axis_span,
    y = axis_limits[[2]] - 0.115 * axis_span,
    label = metric_n_label,
    hjust = 0, vjust = 1,
    parse = TRUE,
    family = font_family, size = 2.45,
    colour = colours$ink
  ) +
  ggplot2::scale_x_continuous(
    breaks = scales::breaks_width(0.2),
    expand = ggplot2::expansion(mult = c(0, 0))
  ) +
  ggplot2::scale_y_continuous(
    breaks = scales::breaks_width(0.2),
    expand = ggplot2::expansion(mult = c(0, 0))
  ) +
  ggplot2::coord_equal(xlim = axis_limits, ylim = axis_limits, clip = "off") +
  ggplot2::labs(
    title = NULL,
    subtitle = NULL,
    x = expression(
      "Observed " * log[10](plain(Fe)[plain(wt) * "%"])
    ),
    y = expression(
      "Spatial OOF-predicted " *
        log[10](plain(Fe)[plain(wt) * "%"])
    )
  ) +
  theme_results()

print(figure_4_3)
save_figure(
  figure_4_3, "Figure_4_3_V3_spatial_OOF",
  width_mm = 125, height_mm = 116
)

#' ### 4.4.3 Prespecified sensitivity analyses
# 11) Results 4.4.3: prespecified subset sensitivities

cat("\n4.4.3 Prespecified sensitivity analyses:\n")

sensitivity_rows <- list()

record_subset_sensitivity <- function(result, label, section_prefix) {
  if (!identical(result$status, "completed")) {
    sensitivity_rows[[length(sensitivity_rows) + 1L]] <<- data.frame(
      sensitivity = label,
      status = result$status,
      fit = NA_character_,
      n = result$n,
      R2 = NA_real_,
      RMSE = NA_real_,
      stringsAsFactors = FALSE
    )
    return(invisible(NULL))
  }
  metrics <- result$metrics
  sensitivity_rows[[length(sensitivity_rows) + 1L]] <<- data.frame(
    sensitivity = label,
    status = "completed",
    fit = metrics$fit,
    n = metrics$n,
    R2 = metrics$R2,
    RMSE = metrics$RMSE,
    stringsAsFactors = FALSE
  )
  main <- metrics[metrics$fit == "main_model", , drop = FALSE]
  refit <- metrics[metrics$fit == "subset_refit", , drop = FALSE]
  add_result("4.4.3", paste0(section_prefix, "_main_R2"), main$R2,
             "sensitivity_results.csv")
  add_result("4.4.3", paste0(section_prefix, "_main_RMSE"), main$RMSE,
             "sensitivity_results.csv")
  add_result("4.4.3", paste0(section_prefix, "_refit_R2"), refit$R2,
             "sensitivity_results.csv")
  add_result("4.4.3", paste0(section_prefix, "_refit_RMSE"), refit$RMSE,
             "sensitivity_results.csv")
  invisible(NULL)
}

# AES_HF-only: same observations for restricted-primary and subset-refit metrics.
aes_mask <- final_data$Fe_AM == "AES_HF"
aes_result <- subset_sensitivity_cv(
  data = final_data,
  features = v3_features,
  main_spatial_oof = v3_oof,
  subset_mask = aes_mask,
  cfg = cfg,
  label = "V3 AES_HF-only",
  minimum_n = 100L
)
add_result("4.4.3", "N_AES_sensitivity", sum(aes_mask, na.rm = TRUE),
           "master_table.rds")
record_subset_sensitivity(aes_result, "AES_HF-only", "AES")

# Shallow-only: d < 25 cm among observations with a parseable midpoint.
shallow_mask <- is.finite(final_data$depth_midpoint_cm) &
  final_data$depth_midpoint_cm < cfg$analysis$depth_breaks_cm[["shallow_upper"]]
shallow_result <- subset_sensitivity_cv(
  data = final_data,
  features = v3_features,
  main_spatial_oof = v3_oof,
  subset_mask = shallow_mask,
  cfg = cfg,
  label = "V3 shallow-only",
  minimum_n = 100L
)
add_result("4.4.3", "N_shallow_sensitivity", sum(shallow_mask),
           "master_table.rds")
record_subset_sensitivity(shallow_result, "Shallow-only", "shallow")

# Site aggregation: exact co-located soil observations share one raster-derived
# predictor vector. The mean log_10 response equals log_10 of the geometric mean.
if (anyNA(final_data$site_id) || any(!nzchar(final_data$site_id))) {
  stop("Site aggregation requires non-missing site_id values.", call. = FALSE)
}
site_groups <- split(seq_len(nrow(final_data)), final_data$site_id, drop = TRUE)
site_rows <- lapply(names(site_groups), function(site) {
  index <- site_groups[[site]]
  if (dplyr::n_distinct(final_data$grid_id[index]) != 1L ||
      dplyr::n_distinct(final_data$spatial_fold[index]) != 1L) {
    stop("A co-located site spans multiple grid cells or spatial folds: ", site,
         call. = FALSE)
  }
  for (feature in v3_features) {
    if (dplyr::n_distinct(final_data[[feature]][index]) != 1L) {
      stop("A co-located site has non-constant predictor values: ", site,
           " / ", feature, call. = FALSE)
    }
  }
  row <- final_data[index[[1]], c("spatial_fold", v3_features), drop = FALSE]
  row$sample_id <- site
  row$Fe_log10 <- mean(final_data$Fe_log10[index])
  row$Fe_pct <- 10^row$Fe_log10
  row[, c("sample_id", "Fe_pct", "Fe_log10", "spatial_fold", v3_features),
      drop = FALSE]
})
site_data <- dplyr::bind_rows(site_rows)
if (!setequal(unique(site_data$spatial_fold), seq_len(cfg$cv$k))) {
  stop("Site aggregation removed one or more spatial folds.", call. = FALSE)
}
site_cv <- evaluate_cv(
  data = site_data,
  target = "Fe_log10",
  features = v3_features,
  folds = folds_from_column(site_data, "spatial_fold"),
  label = "V3 SITE-AGGREGATED SPATIAL CV",
  seed = cfg$seed,
  num.trees = cfg$ranger$num.trees,
  min.node.size = cfg$ranger$min.node.size,
  respect.unordered.factors = cfg$ranger$respect.unordered.factors,
  num.threads = cfg$ranger$num.threads
)
site_metrics <- data.frame(
  sensitivity = "Site-aggregated geometric-mean response",
  status = "completed",
  fit = "site_refit",
  n = nrow(site_data),
  R2 = site_cv$log_metrics$R2,
  RMSE = site_cv$log_metrics$RMSE,
  stringsAsFactors = FALSE
)
sensitivity_rows[[length(sensitivity_rows) + 1L]] <- site_metrics
add_result("4.4.3", "N_site_aggregated", nrow(site_data),
           "site-aggregated V3 spatial CV")
add_result("4.4.3", "site_R2", site_cv$log_metrics$R2,
           "site-aggregated V3 spatial CV")
add_result("4.4.3", "site_RMSE", site_cv$log_metrics$RMSE,
           "site-aggregated V3 spatial CV")

# DUPLICATE(?) inclusion: change only this QAQC decision, then reapply the
# frozen V4-driven common-cohort rule. Existing spatial-fold labels are inherited
# by grid_id. If a newly included record occupies a previously unseen grid cell,
# the paired fixed-fold sensitivity is not identifiable from saved assignments
# and is reported as requiring a new spatial partition rather than guessed.
duplicate_audit_dir <- file.path(output_dir, "duplicate_sensitivity_audit")
dir.create(duplicate_audit_dir, recursive = TRUE, showWarnings = FALSE)

soil_duplicate_inclusive <- clean_fe_layer(
  csv_path = find_data_file(cfg$data_dir, "Fe_soil_sample_all_methods.csv"),
  layer_name = "soil_duplicate_sensitivity",
  bbox = cfg$bbox,
  audit_dir = duplicate_audit_dir,
  allowed_methods = cfg$analysis$allowed_fe_methods,
  primary_qaqc_exclusions = setdiff(
    cfg$analysis$primary_qaqc_exclusions,
    "DUPLICATE(?)"
  ),
  coordinate_tolerance_deg = cfg$analysis$coordinate_tolerance_deg,
  depth_breaks_cm = cfg$analysis$depth_breaks_cm,
  exclude_anthropogenic_waste = cfg$analysis$exclude_anthropogenic_waste,
  anthropogenic_waste_pattern = cfg$analysis$anthropogenic_waste_pattern,
  confirmed_waste_ids = cfg$analysis$confirmed_waste_ids$soil
)
soil_duplicate_inclusive <- sf::st_transform(
  soil_duplicate_inclusive,
  cfg$crs$projected
)
all_stack <- terra::rast(project_paths$predictor_stack)
duplicate_master <- extract_model_table(
  soil_duplicate_inclusive,
  all_stack,
  feature_sets$V4,
  target = "Fe_log10"
)
duplicate_master$domain_mask <- terra::extract(
  terra::rast(project_paths$domain_mask),
  terra::vect(soil_duplicate_inclusive),
  ID = FALSE
)[["domain_mask"]]
duplicate_keep <- !is.na(duplicate_master$domain_mask) &
  duplicate_master$domain_mask == 1 &
  stats::complete.cases(
    duplicate_master[, c("Fe_log10", feature_sets$V4), drop = FALSE]
  )
duplicate_data <- duplicate_master[duplicate_keep, , drop = FALSE]

if (!all(comparison_ids %in% duplicate_data$sample_id)) {
  stop(
    "Relaxing DUPLICATE(?) unexpectedly removed one or more primary-cohort IDs.",
    call. = FALSE
  )
}
new_duplicate_ids <- setdiff(duplicate_data$sample_id, comparison_ids)
add_result("4.4.3", "N_duplicate_added", length(new_duplicate_ids),
           "DUPLICATE(?) alternative cohort")
if (length(new_duplicate_ids) > 0L) {
  new_qaqc <- unique(duplicate_data$qaqc[
    duplicate_data$sample_id %in% new_duplicate_ids
  ])
  if (!all(new_qaqc %in% "DUPLICATE(?)")) {
    stop("The alternative cohort added records other than DUPLICATE(?).",
         call. = FALSE)
  }
}

original_grid_fold <- final_data |>
  dplyr::select(grid_id, spatial_fold) |>
  dplyr::distinct()
if (anyDuplicated(original_grid_fold$grid_id)) {
  stop("Primary spatial folds are not unique by grid_id.", call. = FALSE)
}
duplicate_data <- duplicate_data |>
  dplyr::left_join(original_grid_fold, by = "grid_id")
unmapped_new <- duplicate_data |>
  dplyr::filter(sample_id %in% new_duplicate_ids, is.na(spatial_fold))
add_result("4.4.3", "N_duplicate_new_cells_without_saved_fold", nrow(unmapped_new),
           "DUPLICATE(?) fold-inheritance diagnostic")

if (nrow(unmapped_new) == 0L) {
  duplicate_cv <- evaluate_cv(
    data = duplicate_data,
    target = "Fe_log10",
    features = v3_features,
    folds = folds_from_column(duplicate_data, "spatial_fold"),
    label = "V3 DUPLICATE(?) INCLUSION SPATIAL CV",
    seed = cfg$seed,
    num.trees = cfg$ranger$num.trees,
    min.node.size = cfg$ranger$min.node.size,
    respect.unordered.factors = cfg$ranger$respect.unordered.factors,
    num.threads = cfg$ranger$num.threads
  )
  duplicate_status <- "completed_with_inherited_grid_folds"
  duplicate_metrics <- data.frame(
    sensitivity = "DUPLICATE(?) inclusion",
    status = duplicate_status,
    fit = "duplicate_inclusive_refit",
    n = nrow(duplicate_data),
    R2 = duplicate_cv$log_metrics$R2,
    RMSE = duplicate_cv$log_metrics$RMSE,
    stringsAsFactors = FALSE
  )
  sensitivity_rows[[length(sensitivity_rows) + 1L]] <- duplicate_metrics
  add_result("4.4.3", "N_duplicate_sensitivity", nrow(duplicate_data),
             "DUPLICATE(?) alternative cohort")
  add_result("4.4.3", "duplicate_R2", duplicate_cv$log_metrics$R2,
             "DUPLICATE(?) alternative cohort")
  add_result("4.4.3", "duplicate_RMSE", duplicate_cv$log_metrics$RMSE,
             "DUPLICATE(?) alternative cohort")
} else {
  duplicate_status <- "requires_new_spatial_partition"
  duplicate_metrics <- data.frame(
    sensitivity = "DUPLICATE(?) inclusion",
    status = duplicate_status,
    fit = NA_character_,
    n = nrow(duplicate_data),
    R2 = NA_real_,
    RMSE = NA_real_,
    stringsAsFactors = FALSE
  )
  sensitivity_rows[[length(sensitivity_rows) + 1L]] <- duplicate_metrics
  readr::write_csv(
    unmapped_new,
    file.path(output_dir, "duplicate_sensitivity_unmapped_new_cells.csv")
  )
  cat("  Duplicate-sensitivity unmapped new cells: ", nrow(unmapped_new), ".\n", sep = "")
  add_result("4.4.3", "N_duplicate_sensitivity", nrow(duplicate_data),
             "DUPLICATE(?) alternative cohort")
  warning(
    "DUPLICATE(?) sensitivity contains newly occupied grid cells without saved ",
    "spatial-fold labels. No fold assignment was guessed; see diagnostics."
  )
}

sensitivity_results <- dplyr::bind_rows(sensitivity_rows)
readr::write_csv(
  sensitivity_results,
  file.path(output_dir, "sensitivity_results.csv")
)
cat("4.4.3 Sensitivity-analysis summary:\n")
print(sensitivity_results, row.names = FALSE)

#-------------------------------------------------------------------------------
# Part 6. Results 4.5 - Full-cohort OOB permutation importance --------
#' ## 4.5 Full-cohort OOB permutation importance

cat("\n=== Results 4.5: Full-cohort OOB permutation importance ===\n")

# 12) Validate and export the selected-model importance ranking

importance_path <- file.path(v3_output_dir, "v3_importance.csv")
assert_files_exist(importance_path)
importance <- readr::read_csv(importance_path, show_col_types = FALSE) |>
  dplyr::arrange(dplyr::desc(importance))
if (nrow(importance) != length(v3_features) ||
    !setequal(importance$variable, v3_features)) {
  stop("V3 permutation-importance table does not match the 27 V3 predictors.",
       call. = FALSE)
}
importance <- as.data.frame(importance, stringsAsFactors = FALSE)
importance$rank <- seq_len(nrow(importance))
importance <- importance[, c("rank", "variable", "importance")]

readr::write_csv(
  importance,
  file.path(output_dir, "v3_permutation_importance.csv")
)
cat("4.5 Complete V3 permutation-importance ranking (all 27 predictors):\n")
print(importance, row.names = FALSE)

for (rank in seq_len(nrow(importance))) {
  add_result(
    "4.5", paste0("importance_rank_", rank, "_predictor"),
    importance$variable[[rank]], "v3_permutation_importance.csv"
  )
  add_result(
    "4.5", paste0("importance_rank_", rank, "_value"),
    importance$importance[[rank]], "v3_permutation_importance.csv"
  )
}

# Publication Figure 4.4 (replaces diagnostic version)
# Bridge: provide path alias for publication figure
project_paths$v3_importance <- importance_path

# ---- 7. Figure 4.4: full V3 permutation importance --------------------------

predictor_labels <- c(
  rock_Fe_final = "Rock Fe",
  sed_Fe_final = "Sediment Fe",
  rock_support_missing = "Rock support missing",
  sed_support_missing = "Sediment support missing",
  rock_n_samples = "Rock sample count",
  sed_n_samples = "Sediment sample count",
  nn_rock_Fe = "Nearest-rock Fe",
  nn_sed_Fe = "Nearest-sediment Fe",
  nn_rock_dist_km = "Nearest-rock distance",
  nn_sed_dist_km = "Nearest-sediment distance",
  elev = "Elevation",
  slope = "Slope",
  eastness = "Eastness",
  northness = "Northness",
  PF_prob = "Permafrost probability",
  MAAT = "Mean annual air temperature",
  MAP = "Mean annual precipitation",
  Ign_highFe = "High-Fe igneous",
  Ign_lowFe = "Low-Fe igneous",
  Sed_fine = "Fine-grained sedimentary",
  Sed_other = "Other sedimentary",
  Meta_sed = "Metasedimentary",
  Meta_ign = "Meta-igneous",
  Unconsolidated = "Unconsolidated",
  Other_unknown = "Other / unknown",
  rock_sed_ratio = "Rock : sediment Fe ratio",
  TPI = "Topographic position index"
)

if (!setequal(importance$variable, names(predictor_labels))) {
  stop(
    "V3 importance variables do not match the fixed 27-predictor specification.",
    call. = FALSE
  )
}

importance$display_name <- unname(
  predictor_labels[importance$variable]
)
importance$display_name[is.na(importance$display_name)] <-
  importance$variable[is.na(importance$display_name)]

importance <- importance[
  order(importance$importance),
  ,
  drop = FALSE
]

importance$rank <- rank(
  -importance$importance,
  ties.method = "min"
)

importance$display_name <- factor(
  importance$display_name,
  levels = importance$display_name
)

importance$value_label <- sprintf(
  "%.5f",
  importance$importance
)

importance_max <- max(
  importance$importance,
  na.rm = TRUE
)

# Separate plotting strip for the rank column:
# predictor names | rank | zero line | importance values
rank_panel_left <- -0.085 * importance_max
rank_x <- -0.035 * importance_max

top_predictor <- as.character(
  importance$display_name[nrow(importance)]
)

importance_breaks <- seq(
  0,
  ceiling(importance_max * 1000) / 1000,
  by = 0.002
)

figure_4_4 <- ggplot2::ggplot(
  importance,
  ggplot2::aes(
    x = importance,
    y = display_name
  )
) +
  ggplot2::geom_vline(
    xintercept = 0,
    colour = colours$muted,
    linewidth = 0.32
  ) +
  ggplot2::geom_segment(
    ggplot2::aes(
      x = 0,
      xend = importance,
      yend = display_name
    ),
    colour = "lightcyan3",
    linewidth = 0.48
  ) +
  ggplot2::geom_text(
    ggplot2::aes(
      y = display_name,
      label = rank
    ),
    x = rank_x,
    hjust = 0.5,
    vjust = 0.5,
    family = font_family,
    fontface = "bold",
    size = 2.35,
    colour = colours$ink
  ) +
  ggplot2::annotate(
    "text",
    x = rank_x,
    y = top_predictor,
    label = "Rank",
    hjust = 0.5,
    vjust = -1.10,
    family = font_family,
    fontface = "bold",
    size = 2.35,
    colour = colours$ink
  ) +
  ggplot2::geom_point(
    shape = 21,
    size = 2.25,
    stroke = 0.35,
    fill = colours$teal,
    colour = colours$teal_dark
  ) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = value_label
    ),
    hjust = -0.16,
    vjust = 0.5,
    family = font_family,
    size = 2.35,
    colour = colours$ink
  ) +
  ggplot2::scale_x_continuous(
    limits = c(rank_panel_left, NA_real_),
    labels = scales::label_number(accuracy = 0.001),
    breaks = importance_breaks,
    expand = ggplot2::expansion(
      mult = c(0, 0.18)
    )
  ) +
  ggplot2::scale_y_discrete(
    expand = ggplot2::expansion(
      add = c(0.55, 0.95)
    )
  ) +
  ggplot2::labs(
    title = NULL,
    subtitle = NULL,
    x = "Unscaled permutation importance",
    y = NULL
  ) +
  ggplot2::coord_cartesian(
    clip = "off"
  ) +
  theme_results(
    base_size = 7.8
  ) +
  ggplot2::theme(
    axis.text.y = ggplot2::element_text(
      size = 8.4,
      margin = ggplot2::margin(
        r = 1.5,
        unit = "mm"
      )
    ),
    axis.line.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_line(
      colour = colours$grid,
      linewidth = 0.26
    ),
    plot.margin = ggplot2::margin(
      2.2,
      9.0,
      2.2,
      2.2,
      unit = "mm"
    )
  )

print(figure_4_4)
save_figure(
  figure_4_4,
  "Figure_4_4_V3_permutation_importance",
  width_mm = 170,
  height_mm = 150
)

#-------------------------------------------------------------------------------
# Part 7. Results 4.6 - Final predicted surface and map coverage --------
#' ## 4.6 Final predicted surface and map coverage

cat("\n=== Results 4.6: Final predicted surface and map coverage ===\n")

# 13) Count valid land cells, predicted V3 cells and mapped Fe distribution

prediction_path <- file.path(v3_output_dir, "Fe_prediction_v3_1000m.tif")
assert_files_exist(prediction_path)
prediction <- terra::rast(prediction_path)
domain_mask <- terra::rast(project_paths$domain_mask)
if (!terra::compareGeom(prediction, domain_mask, stopOnError = FALSE)) {
  stop("V3 prediction raster and land-domain mask are not geometrically aligned.",
       call. = FALSE)
}
if (!all(c("Fe_log10_pred", "Fe_pct_median", "Fe_pct_mean") %in%
         names(prediction))) {
  stop("V3 prediction raster lacks the expected response-scale layers.",
       call. = FALSE)
}

# Remove stale conditional outputs so a previous run cannot be mistaken for the
# current audit outcome.
conditional_support_outputs <- file.path(
  output_dir,
  c(
    "unsupported_structural_support_cells.tif",
    "Figure_audit_unsupported_structural_support_cells.png",
    "Fe_prediction_v3_1000m_support_restricted.tif"
  )
)
unlink(
  conditional_support_outputs[file.exists(conditional_support_outputs)],
  force = TRUE
)

# Audit four overlapping structural-missingness conditions in the final
# training cohort and the cells that initially receive V3 predictions.
# Joint indicator combinations are evaluated separately below when identifying
# mapping states absent from training. This is a targeted support audit, not a
# general area-of-applicability analysis.
predictor_stack <- terra::rast(project_paths$predictor_stack)
support_layers <- c("rock_support_missing", "sed_support_missing")
if (!all(support_layers %in% names(predictor_stack))) {
  stop("Shared predictor stack lacks structural-support indicator layers.",
       call. = FALSE)
}
support_stack <- predictor_stack[[support_layers]]
if (!terra::compareGeom(support_stack, prediction, stopOnError = FALSE)) {
  stop("Structural-support rasters are not aligned with the V3 prediction raster.",
       call. = FALSE)
}

land_values <- terra::values(domain_mask, mat = FALSE)
original_map_values <- terra::values(
  prediction[["Fe_pct_mean"]],
  mat = FALSE
)
current_prediction_cells <-
  is.finite(land_values) & land_values == 1 & is.finite(original_map_values)

rock_map_missing <- terra::values(
  support_stack[["rock_support_missing"]],
  mat = FALSE
)
sed_map_missing <- terra::values(
  support_stack[["sed_support_missing"]],
  mat = FALSE
)
if (
  any(!is.finite(rock_map_missing[current_prediction_cells])) ||
  any(!is.finite(sed_map_missing[current_prediction_cells])) ||
  any(!(rock_map_missing[current_prediction_cells] %in% c(0, 1))) ||
  any(!(sed_map_missing[current_prediction_cells] %in% c(0, 1)))
) {
  stop("Structural-support indicators are invalid within predicted cells.",
       call. = FALSE)
}

rock_train_missing <- as.integer(final_data$rock_support_missing)
sed_train_missing <- as.integer(final_data$sed_support_missing)
if (
  anyNA(rock_train_missing) ||
  anyNA(sed_train_missing) ||
  any(!(rock_train_missing %in% c(0L, 1L))) ||
  any(!(sed_train_missing %in% c(0L, 1L)))
) {
  stop("Training structural-support indicators are invalid.", call. = FALSE)
}

training_mapping_support <- data.frame(
  audit_condition = c(
    "rock_support_missing",
    "sed_support_missing",
    "either_support_missing",
    "both_support_missing"
  ),
  training_observations_n = c(
    sum(rock_train_missing == 1L),
    sum(sed_train_missing == 1L),
    sum(rock_train_missing == 1L | sed_train_missing == 1L),
    sum(rock_train_missing == 1L & sed_train_missing == 1L)
  ),
  training_observations_total = nrow(final_data),
  mapping_cells_n = c(
    sum(rock_map_missing[current_prediction_cells] == 1),
    sum(sed_map_missing[current_prediction_cells] == 1),
    sum(
      rock_map_missing[current_prediction_cells] == 1 |
        sed_map_missing[current_prediction_cells] == 1
    ),
    sum(
      rock_map_missing[current_prediction_cells] == 1 &
        sed_map_missing[current_prediction_cells] == 1
    )
  ),
  mapping_cells_total = sum(current_prediction_cells),
  stringsAsFactors = FALSE
) |>
  dplyr::mutate(
    training_observations_pct =
      100 * training_observations_n / training_observations_total,
    mapping_cells_pct =
      100 * mapping_cells_n / mapping_cells_total
  ) |>
  dplyr::select(
    audit_condition,
    training_observations_n,
    training_observations_total,
    training_observations_pct,
    mapping_cells_n,
    mapping_cells_total,
    mapping_cells_pct
  )
readr::write_csv(
  training_mapping_support,
  file.path(output_dir, "Table_audit_training_mapping_structural_support.csv")
)
cat("4.6 Training-mapping structural-support comparison:\n")
print(training_mapping_support, row.names = FALSE)
cat(
  "4.6 Denominators: training percentages use observations (n = ",
  nrow(final_data),
  "); mapping percentages use cells with an initial V3 prediction (n = ",
  sum(current_prediction_cells),
  "). These percentages diagnose predictor-state support and are not directly ",
  "comparable as representative prevalence estimates.\n",
  sep = ""
)

# All four overlapping training missingness-audit counts are expected to be
# zero; equivalently, every training observation must have the joint indicator
# state (rock_support_missing, sed_support_missing) = (0, 0).
if (any(rock_train_missing != 0L) || any(sed_train_missing != 0L)) {
  stop(
    "The final training cohort does not have the assumed joint structural-support ",
    "indicator state (0, 0) for every observation; revise the Results interpretation ",
    "before mapping.",
    call. = FALSE
  )
}

training_states <- unique(
  paste(rock_train_missing, sed_train_missing, sep = "_")
)
mapping_states <- paste(
  as.integer(rock_map_missing),
  as.integer(sed_map_missing),
  sep = "_"
)
unsupported_cells <-
  current_prediction_cells & !(mapping_states %in% training_states)
n_unsupported_cells <- sum(unsupported_cells)

# The original Model_V3 raster remains untouched. When unsupported states occur,
# create a conservative support-restricted Results raster and use it for the
# reported map statistics and Figure 4.5.
results_prediction_path <- prediction_path
if (n_unsupported_cells > 0L) {
  unsupported_values <- rep(NA_real_, terra::ncell(domain_mask))
  unsupported_values[unsupported_cells] <- 1
  unsupported_raster <- terra::setValues(
    terra::rast(domain_mask),
    unsupported_values
  )
  names(unsupported_raster) <- "unsupported_structural_support"
  safe_write_raster(
    unsupported_raster,
    file.path(output_dir, "unsupported_structural_support_cells.tif")
  )
  
  support_mask_values <- rep(NA_real_, terra::ncell(domain_mask))
  support_mask_values[current_prediction_cells & !unsupported_cells] <- 1
  support_mask <- terra::setValues(
    terra::rast(domain_mask),
    support_mask_values
  )
  names(support_mask) <- "training_supported_structural_state"
  
  prediction <- terra::mask(prediction, support_mask)
  results_prediction_path <- file.path(
    output_dir,
    "Fe_prediction_v3_1000m_support_restricted.tif"
  )
  safe_write_raster(prediction, results_prediction_path)
  
  cat(
    "4.6 Structural-support audit: ", n_unsupported_cells,
    " predicted cells contain a structural-support indicator combination absent ",
    "from training; these cells are ",
    "NoData in the Results-ready support-restricted map.\n",
    sep = ""
  )
} else {
  cat(
    "4.6 Structural-support audit: no predicted cells contain structural-support ",
    "indicator combinations absent from training; no restricted raster is needed.\n",
    sep = ""
  )
}

n_land_domain_cells <- sum(is.finite(land_values) & land_values == 1)
map_values <- terra::values(
  prediction[["Fe_pct_mean"]],
  mat = FALSE,
  na.rm = TRUE
)
n_predicted_cells <- length(map_values)
n_v3_nodata_cells <- n_land_domain_cells - n_predicted_cells
if (n_v3_nodata_cells < 0L) {
  stop("Predicted-cell count exceeds the valid land-domain count.", call. = FALSE)
}
prediction_coverage_pct <- 100 * n_predicted_cells / n_land_domain_cells

map_summary <- data.frame(
  n_land_domain_cells = n_land_domain_cells,
  n_predicted_cells_before_support_restriction = sum(current_prediction_cells),
  n_unsupported_structural_support_cells = n_unsupported_cells,
  n_predicted_cells = n_predicted_cells,
  n_v3_nodata_cells = n_v3_nodata_cells,
  prediction_coverage_pct = prediction_coverage_pct,
  median = stats::median(map_values),
  q25 = safe_quantile(map_values, 0.25),
  q75 = safe_quantile(map_values, 0.75),
  p05 = safe_quantile(map_values, 0.05),
  p95 = safe_quantile(map_values, 0.95),
  min = min(map_values),
  max = max(map_values),
  stringsAsFactors = FALSE
)
readr::write_csv(map_summary, file.path(output_dir, "v3_map_summary.csv"))
cat("4.6 Final V3 map coverage and predicted mean-scale Fe summary:\n")
print(map_summary, row.names = FALSE)

for (pair in list(
  c("N_land_domain_cells", map_summary$n_land_domain_cells),
  c("N_predicted_cells", map_summary$n_predicted_cells),
  c("N_V3_nodata_cells", map_summary$n_v3_nodata_cells),
  c("prediction_coverage_pct", map_summary$prediction_coverage_pct),
  c("map_median", map_summary$median),
  c("map_Q25", map_summary$q25),
  c("map_Q75", map_summary$q75),
  c("map_P5", map_summary$p05),
  c("map_P95", map_summary$p95)
)) {
  add_result("4.6", pair[[1]], as.numeric(pair[[2]]), "v3_map_summary.csv")
}
add_result("4.6", "final_smearing_factor", v3_metrics$final_smearing_factor,
           "Model_V3/outputs/model_metrics.csv")
add_result(
  "4.6",
  "N_predicted_before_support_restriction",
  sum(current_prediction_cells),
  "Table_audit_training_mapping_structural_support.csv"
)
add_result(
  "4.6",
  "N_unsupported_structural_support_cells",
  n_unsupported_cells,
  "Table_audit_training_mapping_structural_support.csv"
)

# Derive map-interpretation diagnostics from the mapped cells themselves.
# These summaries are descriptive only; no inferential significance is attached
# to raster-cell counts or to the fitted broad-scale spatial trend.

unit_lookup_path <- file.path(
  cfg$processed_dir, "geology_NSACLASS_support_lookup.csv"
)
assert_files_exist(c(project_paths$unit_code, unit_lookup_path))
unit_code <- terra::rast(project_paths$unit_code)
if (!terra::compareGeom(
  unit_code, prediction[["Fe_pct_mean"]], stopOnError = FALSE
)) {
  stop("NSACLASS unit-code raster is not aligned with the V3 prediction raster.",
       call. = FALSE)
}

pred_mean_all <- terra::values(
  prediction[["Fe_pct_mean"]], mat = FALSE
)
pred_log_all <- terra::values(
  prediction[["Fe_log10_pred"]], mat = FALSE
)
unit_all <- terra::values(unit_code, mat = FALSE)
pred_cells <- which(is.finite(pred_mean_all) & is.finite(pred_log_all))
pred_xy <- terra::xyFromCell(prediction, pred_cells)

map_cells <- data.frame(
  cell = pred_cells,
  x = pred_xy[, 1],
  y = pred_xy[, 2],
  Fe_log10_pred = pred_log_all[pred_cells],
  Fe_pct_mean = pred_mean_all[pred_cells],
  NSACLASS_code = as.integer(unit_all[pred_cells]),
  stringsAsFactors = FALSE
)

unit_lookup <- readr::read_csv(
  unit_lookup_path,
  col_types = readr::cols(
    .default = readr::col_guess(),
    NSACLASS_code = readr::col_integer(),
    NSACLASS = readr::col_character(),
    LITH1 = readr::col_character()
  ),
  show_col_types = FALSE
) |>
  dplyr::select(NSACLASS_code, NSACLASS, LITH1) |>
  dplyr::distinct()

if (anyDuplicated(unit_lookup$NSACLASS_code)) {
  stop("NSACLASS lookup is not one-to-one by NSACLASS_code.", call. = FALSE)
}

map_cells <- dplyr::left_join(
  map_cells, unit_lookup, by = "NSACLASS_code"
)

geology_summary <- map_cells |>
  dplyr::filter(!is.na(NSACLASS_code)) |>
  dplyr::group_by(NSACLASS_code, NSACLASS, LITH1) |>
  dplyr::summarise(
    n_cells = dplyr::n(),
    median_Fe_pct = stats::median(Fe_pct_mean),
    mean_Fe_pct = mean(Fe_pct_mean),
    q25_Fe_pct = safe_quantile(Fe_pct_mean, 0.25),
    q75_Fe_pct = safe_quantile(Fe_pct_mean, 0.75),
    p05_Fe_pct = safe_quantile(Fe_pct_mean, 0.05),
    p95_Fe_pct = safe_quantile(Fe_pct_mean, 0.95),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(median_Fe_pct))

readr::write_csv(
  geology_summary,
  file.path(output_dir, "v3_map_NSACLASS_summary.csv")
)
cat("4.6 NSACLASS summaries ranked by mapped median Fe; highest 10:\n")
print(utils::head(as.data.frame(geology_summary), 10L), row.names = FALSE)
cat("4.6 NSACLASS summaries ranked by mapped median Fe; lowest 10:\n")
print(utils::tail(as.data.frame(geology_summary), 10L), row.names = FALSE)

lith1_summary <- map_cells |>
  dplyr::filter(!is.na(LITH1), nzchar(LITH1)) |>
  dplyr::group_by(LITH1) |>
  dplyr::summarise(
    n_cells = dplyr::n(),
    median_Fe_pct = stats::median(Fe_pct_mean),
    mean_Fe_pct = mean(Fe_pct_mean),
    q25_Fe_pct = safe_quantile(Fe_pct_mean, 0.25),
    q75_Fe_pct = safe_quantile(Fe_pct_mean, 0.75),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(median_Fe_pct))

readr::write_csv(
  lith1_summary,
  file.path(output_dir, "v3_map_LITH1_summary.csv")
)
cat("4.6 Broad LITH1 summaries of mapped mean-scale Fe:\n")
print(as.data.frame(lith1_summary), row.names = FALSE)

# Locate the upper and lower 5% prediction zones using cell-centre coordinates.
low_threshold <- safe_quantile(map_cells$Fe_pct_mean, 0.05)
high_threshold <- safe_quantile(map_cells$Fe_pct_mean, 0.95)
extreme_cells <- map_cells |>
  dplyr::filter(
    Fe_pct_mean <= low_threshold | Fe_pct_mean >= high_threshold
  ) |>
  dplyr::mutate(
    zone = ifelse(
      Fe_pct_mean >= high_threshold, "upper_5_percent", "lower_5_percent"
    )
  )

extreme_sf <- sf::st_as_sf(
  extreme_cells,
  coords = c("x", "y"),
  crs = cfg$crs$projected,
  remove = FALSE
) |>
  sf::st_transform(cfg$crs$geographic)
extreme_ll <- sf::st_coordinates(extreme_sf)
extreme_cells$longitude <- extreme_ll[, 1]
extreme_cells$latitude <- extreme_ll[, 2]

map_x_mid <- stats::median(map_cells$x)
map_y_mid <- stats::median(map_cells$y)
cat(
  "4.6 Descriptive quadrant partition defined by the median easting (",
  format(map_x_mid, scientific = FALSE, trim = TRUE),
  " m) and median northing (",
  format(map_y_mid, scientific = FALSE, trim = TRUE),
  " m) of the retained mapped-cell centres.\n",
  sep = ""
)
extreme_cells$quadrant <- paste0(
  ifelse(extreme_cells$y >= map_y_mid, "N", "S"),
  ifelse(extreme_cells$x >= map_x_mid, "E", "W")
)

extreme_zone_summary <- extreme_cells |>
  dplyr::group_by(zone) |>
  dplyr::summarise(
    n_cells = dplyr::n(),
    longitude_min = min(longitude),
    longitude_median = stats::median(longitude),
    longitude_max = max(longitude),
    latitude_min = min(latitude),
    latitude_median = stats::median(latitude),
    latitude_max = max(latitude),
    median_Fe_pct = stats::median(Fe_pct_mean),
    .groups = "drop"
  )

extreme_quadrant_summary <- extreme_cells |>
  dplyr::count(zone, quadrant, name = "n_cells") |>
  dplyr::group_by(zone) |>
  dplyr::mutate(
    percentage_within_zone = 100 * n_cells / sum(n_cells)
  ) |>
  dplyr::ungroup() |>
  dplyr::arrange(zone, dplyr::desc(n_cells))

lower_5pct_sw_pct <- extreme_quadrant_summary |>
  dplyr::filter(zone == "lower_5_percent", quadrant == "SW") |>
  dplyr::pull(percentage_within_zone)
upper_5pct_northern_pct <- extreme_quadrant_summary |>
  dplyr::filter(
    zone == "upper_5_percent",
    quadrant %in% c("NW", "NE")
  ) |>
  dplyr::summarise(value = sum(percentage_within_zone)) |>
  dplyr::pull(value)
if (length(lower_5pct_sw_pct) != 1L || !is.finite(lower_5pct_sw_pct) ||
    length(upper_5pct_northern_pct) != 1L || !is.finite(upper_5pct_northern_pct)) {
  stop("Extreme-zone quadrant summaries are incomplete.", call. = FALSE)
}

extreme_lith1_summary <- extreme_cells |>
  dplyr::mutate(
    LITH1 = dplyr::if_else(
      is.na(LITH1) | !nzchar(LITH1), "Unclassified", LITH1
    )
  ) |>
  dplyr::count(zone, LITH1, name = "n_cells") |>
  dplyr::group_by(zone) |>
  dplyr::mutate(
    percentage_within_zone = 100 * n_cells / sum(n_cells)
  ) |>
  dplyr::ungroup() |>
  dplyr::arrange(zone, dplyr::desc(n_cells))

extreme_nsaclass_summary <- extreme_cells |>
  dplyr::mutate(
    NSACLASS = trimws(as.character(NSACLASS)),
    LITH1 = trimws(as.character(LITH1)),
    NSACLASS = dplyr::if_else(
      is.na(NSACLASS) | !nzchar(NSACLASS),
      "Unclassified",
      NSACLASS
    ),
    LITH1 = dplyr::if_else(
      is.na(LITH1) | !nzchar(LITH1),
      "Unclassified",
      LITH1
    )
  ) |>
  dplyr::count(zone, NSACLASS, LITH1, name = "n_cells") |>
  dplyr::group_by(zone) |>
  dplyr::mutate(
    percentage_within_zone = 100 * n_cells / sum(n_cells)
  ) |>
  dplyr::ungroup() |>
  dplyr::arrange(zone, dplyr::desc(n_cells))

readr::write_csv(
  extreme_zone_summary,
  file.path(output_dir, "v3_map_extreme_zone_geography.csv")
)
readr::write_csv(
  extreme_quadrant_summary,
  file.path(output_dir, "v3_map_extreme_zone_quadrants.csv")
)
readr::write_csv(
  extreme_lith1_summary,
  file.path(output_dir, "v3_map_extreme_zone_LITH1.csv")
)
readr::write_csv(
  extreme_nsaclass_summary,
  file.path(output_dir, "v3_map_extreme_zone_NSACLASS.csv")
)

cat("4.6 Geographic extent of the lower and upper 5% mapped Fe zones:\n")
print(as.data.frame(extreme_zone_summary), row.names = FALSE)
cat(
  "4.6 Distribution of extreme prediction zones under the descriptive quadrant ",
  "partition defined by the median easting and northing of the retained mapped-cell centres:\n",
  sep = ""
)
print(as.data.frame(extreme_quadrant_summary), row.names = FALSE)
cat(
  "4.6 Results-ready quadrant summaries: lower 5% in SW = ",
  sprintf("%.2f", lower_5pct_sw_pct),
  "%; upper 5% in northern quadrants (NW + NE) = ",
  sprintf("%.2f", upper_5pct_northern_pct),
  "%.\n",
  sep = ""
)
cat("4.6 LITH1 composition of the lower and upper 5% mapped Fe zones:\n")
print(as.data.frame(extreme_lith1_summary), row.names = FALSE)
cat("4.6 Leading NSACLASS units in each extreme prediction zone:\n")
extreme_nsaclass_print <- extreme_nsaclass_summary |>
  dplyr::group_by(zone) |>
  dplyr::slice_head(n = 10L) |>
  dplyr::ungroup()
print(as.data.frame(extreme_nsaclass_print), row.names = FALSE)

add_result(
  "4.6", "lower_5pct_SW_percentage", lower_5pct_sw_pct,
  "v3_map_extreme_zone_quadrants.csv"
)
add_result(
  "4.6", "upper_5pct_northern_quadrants_percentage", upper_5pct_northern_pct,
  "v3_map_extreme_zone_quadrants.csv"
)
add_result(
  "4.6", "map_low_5pct_threshold_Fe_pct", low_threshold,
  "v3_map_extreme_zone_geography.csv"
)
add_result(
  "4.6", "map_high_5pct_threshold_Fe_pct", high_threshold,
  "v3_map_extreme_zone_geography.csv"
)

# Fit a descriptive first-order spatial plane on the modelled log_10 scale.
# Coefficients represent mapped change per 100 km eastward/northward.
trend_data <- map_cells |>
  dplyr::transmute(
    Fe_log10_pred,
    east_100km = (x - mean(x)) / 100000,
    north_100km = (y - mean(y)) / 100000
  )
trend_fit <- stats::lm(
  Fe_log10_pred ~ east_100km + north_100km,
  data = trend_data
)
trend_coef <- stats::coef(trend_fit)
trend_r2 <- summary(trend_fit)$r.squared
east_coef <- unname(trend_coef[["east_100km"]])
north_coef <- unname(trend_coef[["north_100km"]])
gradient_azimuth_deg <- (
  atan2(east_coef, north_coef) * 180 / pi
) %% 360

compass_labels <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
compass_index <- (floor((gradient_azimuth_deg + 22.5) / 45) %% 8) + 1L
gradient_direction <- compass_labels[[compass_index]]

spatial_trend_summary <- data.frame(
  eastward_change_log10_Fe_per_100km = east_coef,
  northward_change_log10_Fe_per_100km = north_coef,
  gradient_azimuth_degrees_from_north_clockwise = gradient_azimuth_deg,
  direction_of_increase = gradient_direction,
  linear_plane_R2 = trend_r2,
  stringsAsFactors = FALSE
)
readr::write_csv(
  spatial_trend_summary,
  file.path(output_dir, "v3_map_linear_spatial_trend.csv")
)
cat("4.6 Descriptive first-order spatial trend of mapped log10(Fe_wt%):\n")
print(spatial_trend_summary, row.names = FALSE)

add_result(
  "4.6", "map_linear_trend_direction", gradient_direction,
  "v3_map_linear_spatial_trend.csv"
)
add_result(
  "4.6", "map_linear_trend_R2", trend_r2,
  "v3_map_linear_spatial_trend.csv"
)

# Publication Figure 4.5 (replaces diagnostic version)
# Bridge: provide path aliases for publication figure
project_paths$results_prediction <- results_prediction_path
project_paths$domain_mask <- file.path(cfg$processed_dir, "domain_mask_1000m.tif")

# ---- 8. Figure 4.5: Results-ready V3 prediction map -------------------------

prediction <- terra::rast(project_paths$results_prediction)
domain_mask <- terra::rast(project_paths$domain_mask)
if (!terra::compareGeom(prediction, domain_mask, stopOnError = FALSE)) {
  stop("The Results-ready V3 raster and domain mask are not aligned.", call. = FALSE)
}
if (!("Fe_pct_mean" %in% names(prediction))) {
  stop("The Results-ready V3 raster has no Fe_pct_mean layer.", call. = FALSE)
}

prediction_mean <- prediction[["Fe_pct_mean"]]
prediction_ll <- terra::project(
  prediction_mean, "EPSG:4326", method = "bilinear"
)
map_data <- terra::as.data.frame(prediction_ll, xy = TRUE, na.rm = TRUE)

domain_land <- terra::as.polygons(
  domain_mask, dissolve = TRUE, na.rm = TRUE
) |>
  sf::st_as_sf() |>
  sf::st_transform(4326)
map_bbox <- sf::st_bbox(domain_land)

lon_label <- function(x) {
  paste0(abs(x), "\u00b0", ifelse(x < 0, "W", ifelse(x > 0, "E", "")))
}
lat_label <- function(x) {
  paste0(abs(x), "\u00b0", ifelse(x < 0, "S", ifelse(x > 0, "N", "")))
}
lon_breaks <- seq(ceiling(map_bbox[["xmin"]]), floor(map_bbox[["xmax"]]), by = 2)
lat_breaks <- seq(ceiling(map_bbox[["ymin"]]), floor(map_bbox[["ymax"]]), by = 1)

# A WGS84 scale varies across this 3-degree latitude span. Draw a locally exact
# 100-km great-circle bar at its stated reference latitude instead of using an
# automatic whole-panel approximation.
map_x_span <- as.numeric(map_bbox[["xmax"]] - map_bbox[["xmin"]])
map_y_span <- as.numeric(map_bbox[["ymax"]] - map_bbox[["ymin"]])
scale_bar_km <- 100
scale_bar_lat <- as.numeric(map_bbox[["ymin"]] + 0.085 * map_y_span)
earth_radius_m <- 6371008.8
central_angle <- scale_bar_km * 1000 / earth_radius_m
latitude_rad <- scale_bar_lat * pi / 180
scale_bar_delta_lon <- acos(
  (cos(central_angle) - sin(latitude_rad)^2) / cos(latitude_rad)^2
) * 180 / pi
scale_bar_x2 <- as.numeric(map_bbox[["xmax"]] - 0.045 * map_x_span)
scale_bar_x1 <- scale_bar_x2 - scale_bar_delta_lon
scale_tick_half_height <- 0.018 * map_y_span

north_x <- as.numeric(map_bbox[["xmax"]] - 0.060 * map_x_span)
north_y0 <- as.numeric(map_bbox[["ymax"]] - (0.075 + 0.130 * 2 / 3) * map_y_span)
north_y1 <- as.numeric(map_bbox[["ymax"]] - 0.075 * map_y_span)

north_box_w <- 0.050 * map_x_span
north_box_h <- north_box_w *
  cos(mean(as.numeric(map_bbox[c("ymin", "ymax")])) * pi / 180)

map_theme <- ggplot2::theme_minimal(base_family = font_family, base_size = 7.8) +
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = colours$water, colour = NA),
    panel.grid.major = ggplot2::element_line(
      colour = colours$grid, linewidth = 0.25
    ),
    panel.grid.minor = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(
      fill = NA, colour = colours$ink, linewidth = 0.38
    ),
    axis.title = ggplot2::element_text(
      colour = colours$ink, size = 7.4,
      margin = ggplot2::margin(t = 1.2, r = 1.2, unit = "mm")
    ),
    axis.text = ggplot2::element_text(colour = colours$ink, size = 7.1),
    axis.ticks = ggplot2::element_line(colour = colours$ink, linewidth = 0.30),
    axis.ticks.length = grid::unit(1.3, "mm"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.box.just = "centre",
    legend.title = ggplot2::element_text(face = "bold", size = 7.2),
    legend.text = ggplot2::element_text(size = 6.8),
    legend.spacing.x = grid::unit(1.6, "mm"),
    plot.title = ggplot2::element_text(
      face = "bold", colour = colours$ink, size = 9.8,
      margin = ggplot2::margin(b = 1.2, unit = "mm")
    ),
    plot.subtitle = ggplot2::element_text(
      colour = colours$muted, size = 7.7, lineheight = 1.05,
      margin = ggplot2::margin(b = 2.6, unit = "mm")
    ),
    plot.caption = ggplot2::element_text(
      colour = colours$muted, size = 6.7, hjust = 0, lineheight = 1.05,
      margin = ggplot2::margin(t = 1.4, unit = "mm")
    ),
    plot.title.position = "plot",
    plot.background = ggplot2::element_rect(fill = "white", colour = NA),
    plot.margin = ggplot2::margin(2.0, 2.5, 1.0, 2.0, unit = "mm")
  )

figure_4_5 <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = domain_land, fill = colours$nodata, colour = NA
  ) +
  ggplot2::geom_raster(
    data = map_data,
    ggplot2::aes(x = x, y = y, fill = Fe_pct_mean),
    interpolate = FALSE
  ) +
  ggplot2::geom_sf(
    data = domain_land, fill = NA, colour = colours$ink, linewidth = 0.38
  ) +
  ggplot2::annotate(
    "rect",
    xmin = scale_bar_x1 - 0.025 * map_x_span,
    xmax = scale_bar_x2 + 0.025 * map_x_span,
    ymin = scale_bar_lat - 0.055 * map_y_span,
    ymax = scale_bar_lat + 0.070 * map_y_span,
    fill = scales::alpha("white", 0.82), colour = NA
  ) +
  ggplot2::annotate(
    "segment",
    x = scale_bar_x1, xend = scale_bar_x2,
    y = scale_bar_lat, yend = scale_bar_lat,
    colour = colours$ink, linewidth = 0.85, lineend = "butt"
  ) +
  ggplot2::annotate(
    "segment",
    x = c(scale_bar_x1, scale_bar_x2),
    xend = c(scale_bar_x1, scale_bar_x2),
    y = scale_bar_lat - scale_tick_half_height,
    yend = scale_bar_lat + scale_tick_half_height,
    colour = colours$ink, linewidth = 0.70
  ) +
  ggplot2::annotate(
    "text",
    x = mean(c(scale_bar_x1, scale_bar_x2)),
    y = scale_bar_lat + 0.033 * map_y_span,
    label = paste0(scale_bar_km, " km"),
    family = font_family, fontface = "bold", size = 2.45,
    colour = colours$ink
  ) +
  ggplot2::annotate(
    "text",
    x = mean(c(scale_bar_x1, scale_bar_x2)),
    y = scale_bar_lat - 0.034 * map_y_span,
    label = sprintf("Scale true at %.1f\u00b0N", scale_bar_lat),
    family = font_family, size = 1.95,
    colour = colours$muted
  ) +
  ggplot2::annotate(
    "rect",
    xmin = north_x - 0.50 * north_box_w,
    xmax = north_x + 0.50 * north_box_w,
    ymin = north_y1 - 0.66 * north_box_h,
    ymax = north_y1 + 0.34 * north_box_h,
    fill = scales::alpha("white", 0.82),
    colour = NA
  ) +
  ggplot2::annotate(
    "polygon",
    x = north_x + c(0, -0.26, 0, 0.26) * north_box_w,
    y = north_y1 + c(0, -0.52, -0.34, -0.52) * north_box_h,
    fill = colours$ink,
    colour = NA
  ) +
  ggplot2::annotate(
    "text",
    x = north_x,
    y = north_y1 + 0.17 * north_box_h,
    label = "N",
    family = font_family,
    fontface = "bold",
    size = 2.8,
    colour = colours$ink
  ) +
  ggplot2::geom_point(
    data = data.frame(
      x = as.numeric(map_bbox[["xmin"]]) - 1,
      y = as.numeric(map_bbox[["ymin"]]) - 1,
      status = "NoData within land domain"
    ),
    ggplot2::aes(x = x, y = y, shape = status),
    inherit.aes = FALSE, colour = colours$nodata, fill = colours$nodata,
    show.legend = TRUE
  ) +
  ggplot2::scale_fill_gradientn(
    colours = fe_colours,
    name = "Predicted near-total soil Fe (wt%)",
    breaks = scales::breaks_pretty(n = 5),
    labels = scales::label_number(accuracy = 0.1),
    na.value = colours$nodata
  ) +
  ggplot2::scale_shape_manual(
    name = "Map status", values = c("NoData within land domain" = 22)
  ) +
  ggplot2::scale_x_continuous(breaks = lon_breaks, labels = lon_label) +
  ggplot2::scale_y_continuous(breaks = lat_breaks, labels = lat_label) +
  ggplot2::coord_sf(
    crs = sf::st_crs(4326), datum = sf::st_crs(4326),
    xlim = c(map_bbox[["xmin"]], map_bbox[["xmax"]]),
    ylim = c(map_bbox[["ymin"]], map_bbox[["ymax"]]),
    expand = FALSE, ndiscr = 200
  ) +
  ggplot2::labs(
    title = NULL,
    subtitle = NULL,
    x = "Longitude",
    y = "Latitude"
  ) +
  ggplot2::guides(
    fill = ggplot2::guide_colourbar(
      order = 1, title.position = "top", title.hjust = 0.5,
      barwidth = grid::unit(46, "mm"), barheight = grid::unit(3.0, "mm"),
      ticks = TRUE, frame.colour = colours$ink
    ),
    shape = ggplot2::guide_legend(
      order = 2,
      override.aes = list(
        shape = 22, size = 3.2, stroke = 0.3,
        fill = colours$nodata, colour = colours$ink
      )
    )
  ) +
  map_theme

print(figure_4_5)
save_figure(
  figure_4_5, "Figure_4_5_V3_prediction_map",
  width_mm = 180, height_mm = 138
)

message(
  "Figures 4.1-4.5 written to:\n  ", output_dir,
  paste0(
    "\nFigure 4.5 uses the support-restricted mean-scale V3 raster in ",
    "WGS84 with a latitude-referenced 100-km scale bar."
  )
)

#-------------------------------------------------------------------------------
# Part 8. Cross-result validation and export --------
#' ## Cross-result validation

cat("\n=== Cross-result validation ===\n")

# 14) Final manuscript-consistency checks

validation_messages <- character()
record_validation <- function(message) {
  validation_messages <<- c(validation_messages, paste0("PASS: ", message))
  cat("PASS: ", message, "\n", sep = "")
}

if (selected_version != EXPECTED_SELECTED_VERSION) {
  stop(
    "Current spatial-CV ranking selects ", selected_version,
    ", not ", EXPECTED_SELECTED_VERSION,
    ". Chapter 4 model-selection text must be reconciled.",
    call. = FALSE
  )
}
record_validation("V3 remains the maximum-spatial-R2 candidate.")

expected_feature_counts <- c(V1 = 14L, V2 = 17L, V3 = 27L, V4 = 32L)
actual_feature_counts <- setNames(comparison$n_features, as.character(comparison$model))
if (!identical(as.integer(actual_feature_counts[names(expected_feature_counts)]),
               as.integer(expected_feature_counts))) {
  stop("V1-V4 feature counts differ from the Chapter 4 specification.",
       call. = FALSE)
}
record_validation("V1-V4 feature counts are 14, 17, 27 and 32.")

for (path in oof_paths) {
  oof <- readr::read_csv(
    path,
    col_types = readr::cols(sample_id = readr::col_character()),
    show_col_types = FALSE
  )
  if (nrow(oof) != EXPECTED_FINAL_N ||
      !setequal(as.character(oof$sample_id), comparison_ids)) {
    stop("An OOF file differs from the 425-observation common cohort.",
         call. = FALSE)
  }
}
record_validation("All V1-V4 spatial OOF files use the identical 425 sample IDs.")
record_validation("Saved cross-model fold assignments passed assert_same_comparison_ids().")

if (sum(fold_diagnostics$n_test) != EXPECTED_FINAL_N) {
  stop("V3 fold-wise test counts do not sum to 425.", call. = FALSE)
}
record_validation("V3 fold-wise test counts sum to 425.")
record_validation("V3 pooled OOF R2, RMSE, MAE and bias were independently recomputed.")
record_validation("V3 importance contains exactly the 27 selected predictors.")
record_validation("V3 predicted-cell count does not exceed the valid land-domain count.")
record_validation(
  "Four overlapping training-mapping structural-missingness audit conditions were evaluated without refitting the model; joint indicator combinations were then compared for mapping support."
)
if (n_unsupported_cells > 0L) {
  validation_messages <- c(
    validation_messages,
    paste0(
      "REVIEW: ", n_unsupported_cells,
      " predicted cells had a structural-support indicator combination absent from training. ",
      "Figure 4.5 and map summaries use the support-restricted supplementary ",
      "raster; Methods 3.8 and Results 4.6 should state this mapping restriction."
    )
  )
} else {
  record_validation(
    "No predicted cells had structural-support indicator combinations absent from training."
  )
}
if (duplicate_status == "requires_new_spatial_partition") {
  validation_messages <- c(
    validation_messages,
    paste0(
      "REVIEW: DUPLICATE(?) inclusion introduced at least one previously unseen ",
      "1-km grid cell. A fixed-fold paired sensitivity was not fabricated; ",
      "a separate fold-design decision is required before reporting its R2/RMSE."
    )
  )
} else {
  record_validation(
    "DUPLICATE(?) sensitivity inherited spatial folds only through existing grid IDs."
  )
}

validation_messages <- c(
  validation_messages,
  paste0(
    "PASS: V1-to-V4 spatial-support loss is quantified by lost-cell separation ",
    "and convex-hull change; qualitative 'concentrated/dispersed' wording is ",
    "unnecessary unless explicitly defined."
  ),
  paste0(
    "REVIEW: Final-map interpretation is now supported by upper/lower 5% zone ",
    "geography, NSACLASS/LITH1 summaries and a descriptive linear spatial trend. ",
    "Named regional claims still ",
    "require an explicit regional boundary source or direct defensible map reading."
  ),
  paste0(
    "PASS: The complete 27-predictor V3 importance ranking is printed and exported."
  ),
  paste0(
    "LIMITATION: V2-V3 ranking stability and the magnitude of post-selection ",
    "optimism are not identifiable from the fixed single spatial-fold assignment; ",
    "no pseudo-confidence interval is generated."
  )
)

readr::write_csv(
  results_values,
  file.path(output_dir, "results_values.csv")
)
writeLines(
  validation_messages,
  file.path(output_dir, "results_validation_summary.txt")
)

cat("\nResults placeholder registry (authoritative values and sources):\n")
print(results_values, row.names = FALSE)

cat("\nValidation and review notes:\n")
cat(paste(validation_messages, collapse = "\n"), "\n")

cat(
  "\nCompleted: Chapter 4 audit; selected model = ", selected_version,
  "; common cohort n = ", EXPECTED_FINAL_N,
  "; DUPLICATE(?) sensitivity = ", duplicate_status,
  "; outputs = ", output_dir, ".\n", sep = ""
)


# ==============================================================================
# SECTION 3: DISCUSSION DATA VERIFICATION
# ==============================================================================
#' # Discussion data verification
#' Values referenced in Chapter 5 that are not individually registered in the
#' Results audit above are extracted here for manuscript consistency checks.

# Discussion outputs
discussion_out <- discussion_output_dir

cat("\n=== Discussion data verification ===\n")

# 15) Concentration-scale Duan-smearing comparison (Section 5.1, Section 5.3)

v3_comp <- comparison[as.character(comparison$model) == "V3", , drop = FALSE]

cat("5.1/5.3 V3 concentration-scale spatial-OOF metrics:\n")
cat(
  "  Direct inverse (median back-transform): R2 = ",
  round(v3_comp$spatial_Fe_median_R2, 4),
  ", RMSE = ", round(v3_comp$spatial_Fe_median_RMSE, 4), " wt%\n",
  sep = ""
)
cat(
  "  Duan-smearing (mean back-transform):     R2 = ",
  round(v3_comp$spatial_Fe_mean_R2, 4),
  ", RMSE = ", round(v3_comp$spatial_Fe_mean_RMSE, 4), " wt%\n",
  sep = ""
)
add_result(
  "5.1", "V3_spatial_Fe_median_R2", v3_comp$spatial_Fe_median_R2,
  "Model_V3/outputs/model_metrics.csv"
)
add_result(
  "5.1", "V3_spatial_Fe_mean_R2", v3_comp$spatial_Fe_mean_R2,
  "Model_V3/outputs/model_metrics.csv"
)
add_result(
  "5.1", "V3_spatial_Fe_median_RMSE", v3_comp$spatial_Fe_median_RMSE,
  "Model_V3/outputs/model_metrics.csv"
)
add_result(
  "5.1", "V3_spatial_Fe_mean_RMSE", v3_comp$spatial_Fe_mean_RMSE,
  "Model_V3/outputs/model_metrics.csv"
)

# 16) Staged specification contrasts (Section 5.2)

delta_r2 <- diff(comparison$spatial_log_R2[order(comparison$model)])
names(delta_r2) <- c("V1_to_V2", "V2_to_V3", "V3_to_V4")
cat("5.2 Staged spatial R2 contrasts:\n")
print(data.frame(
  transition = names(delta_r2),
  delta_R2 = delta_r2,
  stringsAsFactors = FALSE
), row.names = FALSE)
for (nm in names(delta_r2)) {
  add_result("5.2", paste0("delta_spatial_R2_", nm), delta_r2[[nm]],
             "all_model_metrics.csv")
}

# 17) Multi-observation cell statistics (Section 5.3)

n_occupied_cells <- dplyr::n_distinct(final_data$grid_id)
n_multisample_cells_check <- sum(table(final_data$grid_id) > 1L)
max_cell_count_check <- max(table(final_data$grid_id))
max_cell_weight_check <- 100 * max_cell_count_check / nrow(final_data)

cat(
  "5.3 Cell statistics: ", n_occupied_cells, " occupied cells; ",
  n_multisample_cells_check, " with >1 observation; max count = ",
  max_cell_count_check, " (",
  round(max_cell_weight_check, 2), "% row weight).\n",
  sep = ""
)
add_result("5.3", "N_occupied_cells", n_occupied_cells, "final_data")
add_result("5.3", "max_cell_row_weight_pct_check", max_cell_weight_check,
           "final_data")

# 18) Nearest-neighbour maximum distances (Section 5.4.1)

max_nn_rock_km <- max(final_data$nn_rock_dist_km)
max_nn_sed_km <- max(final_data$nn_sed_dist_km)
cat(
  "5.4.1 Maximum nearest ancillary-point distances: rock ",
  round(max_nn_rock_km, 2), " km; sediment ",
  round(max_nn_sed_km, 2), " km.\n", sep = ""
)
add_result("5.4.1", "max_nn_rock_dist_km", max_nn_rock_km, "final_data")
add_result("5.4.1", "max_nn_sed_dist_km", max_nn_sed_km, "final_data")

# 19) Unconsolidated percentage in the lower-5% prediction zone (Section 5.4.1)

if (exists("extreme_lith1_summary")) {
  unconsolidated_lower5 <- extreme_lith1_summary |>
    dplyr::filter(zone == "lower_5_percent", LITH1 == "Unconsolidated")
  if (nrow(unconsolidated_lower5) == 1L) {
    cat(
      "5.4.1 Unconsolidated in lower-5% zone: ",
      round(unconsolidated_lower5$percentage_within_zone, 2), "%.\n", sep = ""
    )
    add_result(
      "5.4.1", "unconsolidated_lower5_pct",
      unconsolidated_lower5$percentage_within_zone,
      "v3_map_extreme_zone_LITH1.csv"
    )
  }
}

# 20) LITH1 mapped medians (Section 5.4.1)

if (exists("lith1_summary")) {
  discussion_lith1 <- lith1_summary |>
    dplyr::filter(LITH1 %in% c(
      "igneous", "sedimentary", "Unconsolidated",
      "Igneous", "Sedimentary"
    )) |>
    dplyr::select(LITH1, median_Fe_pct, n_cells)
  cat("5.4.1 LITH1 mapped medians referenced in Discussion:\n")
  print(as.data.frame(discussion_lith1), row.names = FALSE)
}

# 21) OOF range contraction (Section 5.3)

cat(
  "5.3 OOF range contraction: observed log10(Fe_wt%) = [",
  round(min(v3_oof$observed_Fe_log10), 4), ", ",
  round(max(v3_oof$observed_Fe_log10), 4), "]; predicted = [",
  round(min(v3_oof$predicted_Fe_log10), 4), ", ",
  round(max(v3_oof$predicted_Fe_log10), 4), "].\n", sep = ""
)
add_result(
  "5.3", "OOF_observed_range",
  paste0(
    round(min(v3_oof$observed_Fe_log10), 4), " to ",
    round(max(v3_oof$observed_Fe_log10), 4)
  ),
  "spatial_oof_predictions.csv"
)
add_result(
  "5.3", "OOF_predicted_range",
  paste0(
    round(min(v3_oof$predicted_Fe_log10), 4), " to ",
    round(max(v3_oof$predicted_Fe_log10), 4)
  ),
  "spatial_oof_predictions.csv"
)

cat("\nDiscussion data verification completed.\n")

# Export the final results registry including Discussion values
readr::write_csv(
  results_values,
  file.path(discussion_out, "results_values_with_discussion.csv")
)

cat("\nFull results registry (Results + Discussion values):\n")
print(results_values, row.names = FALSE)


# ==============================================================================
# SECTION 4: APPENDIX B. SUPPLEMENTARY MODEL DIAGNOSTICS
# ==============================================================================
#' # Appendix B. Supplementary model diagnostics
#' This section generates only Table B1 and Figure B1 from the saved V3
#' spatial out-of-fold (OOF) predictions.

cat("\n=== Appendix B: Supplementary model diagnostics ===\n")

# ---- Table B1: fold-wise spatial CV composition and performance -------------
#' **Table B1. Fold-wise spatial CV composition and performance.** Metrics are
#' calculated separately within each saved spatial OOF fold on the
#' \(\log_{10}(\mathrm{Fe}_{\mathrm{wt}\%})\) modelling scale. Signed bias is
#' defined as predicted minus observed, \(\hat{y}-y\).

appendix_b_required_oof_columns <- c(
  "sample_id",
  "spatial_fold",
  "evaluation_fold",
  "observed_Fe_log10",
  "predicted_Fe_log10"
)
if (!all(appendix_b_required_oof_columns %in% names(v3_oof))) {
  stop(
    "V3 spatial OOF predictions lack required Appendix B columns: ",
    paste(
      setdiff(appendix_b_required_oof_columns, names(v3_oof)),
      collapse = ", "
    ),
    call. = FALSE
  )
}

appendix_b_oof <- final_data |>
  dplyr::select(
    sample_id,
    grid_id,
    grid_x,
    grid_y,
    spatial_fold,
    Fe_log10
  ) |>
  dplyr::left_join(
    v3_oof |>
      dplyr::transmute(
        sample_id,
        oof_spatial_fold = spatial_fold,
        evaluation_fold,
        observed_Fe_log10,
        predicted_Fe_log10
      ),
    by = "sample_id",
    relationship = "one-to-one"
  )

appendix_b_join_columns <- c(
  "oof_spatial_fold",
  "evaluation_fold",
  "observed_Fe_log10",
  "predicted_Fe_log10"
)
if (anyNA(appendix_b_oof[, appendix_b_join_columns])) {
  stop("Appendix B OOF join is incomplete.", call. = FALSE)
}
if (
  any(appendix_b_oof$spatial_fold != appendix_b_oof$oof_spatial_fold) ||
  any(appendix_b_oof$spatial_fold != appendix_b_oof$evaluation_fold)
) {
  stop(
    "Saved V3 OOF fold labels disagree with the final comparison cohort.",
    call. = FALSE
  )
}
if (
  max(abs(
    appendix_b_oof$Fe_log10 - appendix_b_oof$observed_Fe_log10
  )) > TOL
) {
  stop(
    "Saved V3 OOF responses disagree with the final comparison cohort.",
    call. = FALSE
  )
}

table_b1_internal <- appendix_b_oof |>
  dplyr::mutate(fold = evaluation_fold) |>
  dplyr::group_by(fold) |>
  dplyr::group_modify(
    function(data, key) {
      metrics <- regression_metrics(
        data$observed_Fe_log10,
        data$predicted_Fe_log10
      )
      data.frame(
        n_observations = nrow(data),
        n_occupied_cells = dplyr::n_distinct(data$grid_id),
        SD_y = stats::sd(data$observed_Fe_log10),
        R2 = metrics$R2,
        RMSE = metrics$RMSE,
        MAE = metrics$MAE,
        signed_bias = mean(
          data$predicted_Fe_log10 - data$observed_Fe_log10
        ),
        stringsAsFactors = FALSE
      )
    }
  ) |>
  dplyr::ungroup() |>
  dplyr::arrange(fold)

if (
  nrow(table_b1_internal) != cfg$cv$k ||
  sum(table_b1_internal$n_observations) != EXPECTED_FINAL_N
) {
  stop(
    "Table B1 must contain five folds totalling 425 observations.",
    call. = FALSE
  )
}

# Verify the direct OOF calculations against, but do not copy values from,
# spatial_fold_metrics.csv.
appendix_b_saved_fold_check <- spatial_fold_metrics |>
  dplyr::transmute(
    fold,
    saved_n = n_test,
    saved_R2 = log_R2,
    saved_RMSE = log_RMSE,
    saved_MAE = log_MAE,
    saved_signed_bias = log_bias
  )
appendix_b_metric_check <- dplyr::left_join(
  table_b1_internal,
  appendix_b_saved_fold_check,
  by = "fold"
)
if (
  anyNA(appendix_b_metric_check) ||
  any(
    appendix_b_metric_check$n_observations !=
    appendix_b_metric_check$saved_n
  ) ||
  any(abs(
    appendix_b_metric_check$R2 - appendix_b_metric_check$saved_R2
  ) > TOL) ||
  any(abs(
    appendix_b_metric_check$RMSE - appendix_b_metric_check$saved_RMSE
  ) > TOL) ||
  any(abs(
    appendix_b_metric_check$MAE - appendix_b_metric_check$saved_MAE
  ) > TOL) ||
  any(abs(
    appendix_b_metric_check$signed_bias -
    appendix_b_metric_check$saved_signed_bias
  ) > TOL)
) {
  stop(
    "Direct Table B1 calculations disagree with spatial_fold_metrics.csv.",
    call. = FALSE
  )
}

table_b1 <- data.frame(
  Fold = table_b1_internal$fold,
  `n obs.` = table_b1_internal$n_observations,
  `n cells` = table_b1_internal$n_occupied_cells,
  `SD(y)` = table_b1_internal$SD_y,
  `R^2` = table_b1_internal$R2,
  RMSE = table_b1_internal$RMSE,
  MAE = table_b1_internal$MAE,
  Bias = table_b1_internal$signed_bias,
  check.names = FALSE
)
readr::write_csv(
  table_b1,
  file.path(appendix_b_output_dir, "Table_B1_fold_wise_spatial_CV.csv")
)
print(table_b1, row.names = FALSE)

# ---- Figure B1: cell-mean signed spatial OOF error --------------------------
#' **Figure B1. Spatial distribution of cell-mean signed OOF error.** Each
#' point represents one occupied 1-km cell. Where a cell contains multiple
#' observations, the plotted value is their mean OOF error, \(\hat{y}-y\), on
#' the \(\log_{10}(\mathrm{Fe}_{\mathrm{wt}\%})\) modelling scale. Positive
#' values indicate overprediction. This is a descriptive diagnostic of the
#' saved spatial OOF predictions, not an independent spatial validation.

appendix_b_cell_fold_check <- appendix_b_oof |>
  dplyr::group_by(grid_id) |>
  dplyr::summarise(
    n_spatial_folds = dplyr::n_distinct(spatial_fold),
    .groups = "drop"
  )
if (any(appendix_b_cell_fold_check$n_spatial_folds != 1L)) {
  stop(
    "At least one occupied 1-km cell has more than one spatial-fold label.",
    call. = FALSE
  )
}

figure_b1_data <- appendix_b_oof |>
  dplyr::mutate(
    signed_oof_error = predicted_Fe_log10 - observed_Fe_log10
  ) |>
  dplyr::group_by(grid_id, grid_x, grid_y) |>
  dplyr::summarise(
    n_observations = dplyr::n(),
    cell_mean_signed_oof_error = mean(signed_oof_error),
    .groups = "drop"
  )
if (
  nrow(figure_b1_data) != dplyr::n_distinct(final_data$grid_id) ||
  anyNA(figure_b1_data$cell_mean_signed_oof_error)
) {
  stop(
    "Figure B1 must contain one complete mean signed OOF error per occupied cell.",
    call. = FALSE
  )
}

readr::write_csv(
  figure_b1_data,
  file.path(
    appendix_b_output_dir,
    "Figure_B1_cell_mean_signed_spatial_OOF_error_data.csv"
  )
)
cat("Figure B1 plotting data (", nrow(figure_b1_data), " cells).\n", sep = "")

appendix_b_domain_mask <- terra::rast(project_paths$domain_mask)
appendix_b_domain_sf <- sf::st_as_sf(
  terra::as.polygons(
    appendix_b_domain_mask,
    aggregate = TRUE,
    values = FALSE,
    na.rm = TRUE
  )
) |>
  sf::st_make_valid() |>
  sf::st_transform(cfg$crs$geographic)

# Signed errors are calculated and aggregated at the analytical 1-km support
# in EPSG:3338 above. Transform only the completed cell-level display layer.
figure_b1_points_sf <- sf::st_as_sf(
  figure_b1_data,
  coords = c("grid_x", "grid_y"),
  crs = cfg$crs$projected,
  remove = FALSE
) |>
  sf::st_transform(cfg$crs$geographic)

appendix_b_bbox <- sf::st_bbox(appendix_b_domain_sf)
appendix_b_lon_breaks <- seq(
  ceiling(appendix_b_bbox[["xmin"]]),
  floor(appendix_b_bbox[["xmax"]]),
  by = 2
)
appendix_b_lat_breaks <- seq(
  ceiling(appendix_b_bbox[["ymin"]]),
  floor(appendix_b_bbox[["ymax"]]),
  by = 1
)

# Match the WGS 84 map furniture and typography used by Figures 3.2 and 4.5.
appendix_b_x_span <- as.numeric(
  appendix_b_bbox[["xmax"]] - appendix_b_bbox[["xmin"]]
)
appendix_b_y_span <- as.numeric(
  appendix_b_bbox[["ymax"]] - appendix_b_bbox[["ymin"]]
)
appendix_b_scale_bar_km <- 100
appendix_b_scale_bar_lat <- as.numeric(
  appendix_b_bbox[["ymin"]] + 0.085 * appendix_b_y_span
)
appendix_b_central_angle <-
  appendix_b_scale_bar_km * 1000 / earth_radius_m
appendix_b_latitude_rad <- appendix_b_scale_bar_lat * pi / 180
appendix_b_scale_bar_delta_lon <- acos(
  (
    cos(appendix_b_central_angle) -
      sin(appendix_b_latitude_rad)^2
  ) / cos(appendix_b_latitude_rad)^2
) * 180 / pi
appendix_b_scale_bar_x2 <- as.numeric(
  appendix_b_bbox[["xmax"]] - 0.045 * appendix_b_x_span
)
appendix_b_scale_bar_x1 <-
  appendix_b_scale_bar_x2 - appendix_b_scale_bar_delta_lon
appendix_b_scale_tick_half_height <- 0.018 * appendix_b_y_span

appendix_b_north_x <- as.numeric(
  appendix_b_bbox[["xmax"]] - 0.060 * appendix_b_x_span
)
appendix_b_north_y1 <- as.numeric(
  appendix_b_bbox[["ymax"]] - 0.075 * appendix_b_y_span
)
appendix_b_north_box_w <- 0.050 * appendix_b_x_span
appendix_b_north_box_h <- appendix_b_north_box_w * cos(
  mean(as.numeric(appendix_b_bbox[c("ymin", "ymax")])) * pi / 180
)

appendix_b_error_limit <- max(
  abs(figure_b1_data$cell_mean_signed_oof_error),
  na.rm = TRUE
)
if (!is.finite(appendix_b_error_limit) || appendix_b_error_limit <= 0) {
  stop("Figure B1 signed OOF errors have no finite non-zero range.", call. = FALSE)
}

figure_b1 <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = appendix_b_domain_sf,
    fill = scales::alpha("darkgreen", 0.42),
    colour = NA
  ) +
  ggplot2::geom_sf(
    data = figure_b1_points_sf,
    ggplot2::aes(
      fill = cell_mean_signed_oof_error
    ),
    shape = 21,
    size = 2.0,
    stroke = 0.25,
    colour = colours$ink,
    alpha = 0.92
  ) +
  ggplot2::geom_sf(
    data = appendix_b_domain_sf,
    fill = NA,
    colour = colours$ink,
    linewidth = 0.38
  ) +
  ggplot2::annotate(
    "rect",
    xmin = appendix_b_scale_bar_x1 - 0.025 * appendix_b_x_span,
    xmax = appendix_b_scale_bar_x2 + 0.025 * appendix_b_x_span,
    ymin = appendix_b_scale_bar_lat - 0.055 * appendix_b_y_span,
    ymax = appendix_b_scale_bar_lat + 0.070 * appendix_b_y_span,
    fill = scales::alpha("white", 0.82),
    colour = NA
  ) +
  ggplot2::annotate(
    "segment",
    x = appendix_b_scale_bar_x1,
    xend = appendix_b_scale_bar_x2,
    y = appendix_b_scale_bar_lat,
    yend = appendix_b_scale_bar_lat,
    colour = colours$ink,
    linewidth = 0.85,
    lineend = "butt"
  ) +
  ggplot2::annotate(
    "segment",
    x = c(appendix_b_scale_bar_x1, appendix_b_scale_bar_x2),
    xend = c(appendix_b_scale_bar_x1, appendix_b_scale_bar_x2),
    y = appendix_b_scale_bar_lat - appendix_b_scale_tick_half_height,
    yend = appendix_b_scale_bar_lat + appendix_b_scale_tick_half_height,
    colour = colours$ink,
    linewidth = 0.70
  ) +
  ggplot2::annotate(
    "text",
    x = mean(c(appendix_b_scale_bar_x1, appendix_b_scale_bar_x2)),
    y = appendix_b_scale_bar_lat + 0.033 * appendix_b_y_span,
    label = paste0(appendix_b_scale_bar_km, " km"),
    family = font_family,
    fontface = "bold",
    size = 2.45,
    colour = colours$ink
  ) +
  ggplot2::annotate(
    "text",
    x = mean(c(appendix_b_scale_bar_x1, appendix_b_scale_bar_x2)),
    y = appendix_b_scale_bar_lat - 0.034 * appendix_b_y_span,
    label = sprintf(
      "Scale true at %.1fdeg N",
      appendix_b_scale_bar_lat
    ),
    family = font_family,
    size = 1.95,
    colour = colours$muted
  ) +
  ggplot2::annotate(
    "rect",
    xmin = appendix_b_north_x - 0.50 * appendix_b_north_box_w,
    xmax = appendix_b_north_x + 0.50 * appendix_b_north_box_w,
    ymin = appendix_b_north_y1 - 0.66 * appendix_b_north_box_h,
    ymax = appendix_b_north_y1 + 0.34 * appendix_b_north_box_h,
    fill = scales::alpha("white", 0.82),
    colour = NA
  ) +
  ggplot2::annotate(
    "polygon",
    x = appendix_b_north_x +
      c(0, -0.26, 0, 0.26) * appendix_b_north_box_w,
    y = appendix_b_north_y1 +
      c(0, -0.52, -0.34, -0.52) * appendix_b_north_box_h,
    fill = colours$ink,
    colour = NA
  ) +
  ggplot2::annotate(
    "text",
    x = appendix_b_north_x,
    y = appendix_b_north_y1 + 0.17 * appendix_b_north_box_h,
    label = "N",
    family = font_family,
    fontface = "bold",
    size = 2.8,
    colour = colours$ink
  ) +
  ggplot2::scale_fill_gradient2(
    low = "blue4",
    mid = "white",
    high = "firebrick3",
    midpoint = 0,
    limits = c(-appendix_b_error_limit, appendix_b_error_limit),
    name = expression(
      atop(
        "Cell-mean signed OOF error",
        (hat(y) - y) ~ "for" ~ log[10](plain(Fe)[plain(wt) * "%"])
      )
    ),
    breaks = scales::breaks_pretty(n = 5),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  ggplot2::scale_x_continuous(
    breaks = appendix_b_lon_breaks,
    labels = lon_label
  ) +
  ggplot2::scale_y_continuous(
    breaks = appendix_b_lat_breaks,
    labels = lat_label
  ) +
  ggplot2::labs(
    title = NULL,
    subtitle = NULL,
    x = "Longitude",
    y = "Latitude"
  ) +
  ggplot2::coord_sf(
    crs = sf::st_crs(cfg$crs$geographic),
    datum = sf::st_crs(cfg$crs$geographic),
    xlim = c(appendix_b_bbox[["xmin"]], appendix_b_bbox[["xmax"]]),
    ylim = c(appendix_b_bbox[["ymin"]], appendix_b_bbox[["ymax"]]),
    expand = FALSE,
    ndiscr = 200
  ) +
  ggplot2::guides(
    fill = ggplot2::guide_colourbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = grid::unit(46, "mm"),
      barheight = grid::unit(3.0, "mm"),
      ticks = TRUE,
      frame.colour = colours$ink
    )
  ) +
  map_theme +
  ggplot2::theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.box.just = "centre"
  )

print(figure_b1)
ggplot2::ggsave(
  file.path(
    appendix_b_output_dir,
    "Figure_B1_cell_mean_signed_spatial_OOF_error.pdf"
  ),
  plot = figure_b1,
  device = grDevices::cairo_pdf,
  width = 180,
  height = 138,
  units = "mm",
  bg = "white",
  limitsize = FALSE
)
ggplot2::ggsave(
  file.path(
    appendix_b_output_dir,
    "Figure_B1_cell_mean_signed_spatial_OOF_error.tiff"
  ),
  plot = figure_b1,
  device = "tiff",
  dpi = 600,
  compression = "lzw",
  width = 180,
  height = 138,
  units = "mm",
  bg = "white",
  limitsize = FALSE
)

cat(
  "Appendix B completed: Table B1 and Figure B1 written to ",
  appendix_b_output_dir,
  ".\n",
  sep = ""
)