# ==============================================================================
# Shared modelling, validation and prediction functions
# ==============================================================================

#-------------------------------------------------------------------------------
# Part 1. Model fitting and performance metrics --------

# 1) Formula construction and ranger fitting

make_model_formula <- function(target, features) {
  stats::reformulate(features, response = target)
}

fit_ranger_model <- function(data,
                             target,
                             features,
                             seed = 40L,
                             num.trees = 500L,
                             mtry = NULL,
                             min.node.size = 5L,
                             importance = "permutation",
                             respect.unordered.factors = "partition",
                             num.threads = 1L) {
  assert_required_columns(data, c(target, features), "model data")
  if (length(features) == 0L) {
    stop("At least one model feature is required.", call. = FALSE)
  }
  if (!all(stats::complete.cases(data[, c(target, features), drop = FALSE]))) {
    stop("Model data contain missing target or predictor values.", call. = FALSE)
  }
  if (is.null(mtry)) mtry <- max(1L, floor(sqrt(length(features))))

  ranger::ranger(
    formula = make_model_formula(target, features),
    data = data[, c(target, features), drop = FALSE],
    num.trees = as.integer(num.trees),
    mtry = as.integer(mtry),
    min.node.size = as.integer(min.node.size),
    importance = importance,
    respect.unordered.factors = respect.unordered.factors,
    seed = as.integer(seed),
    num.threads = as.integer(num.threads)
  )
}

# 2) Metrics and back-transformation support

regression_metrics <- function(observed, predicted) {
  valid <- stats::complete.cases(observed, predicted)
  observed <- observed[valid]
  predicted <- predicted[valid]
  if (length(observed) < 2L) {
    return(data.frame(
      n = length(observed),
      R2 = NA_real_,
      RMSE = NA_real_,
      MAE = NA_real_,
      bias = NA_real_,
      correlation = NA_real_
    ))
  }

  residual <- observed - predicted
  ss_total <- sum((observed - mean(observed))^2)
  data.frame(
    n = length(observed),
    R2 = if (ss_total > 0) 1 - sum(residual^2) / ss_total else NA_real_,
    RMSE = sqrt(mean(residual^2)),
    MAE = mean(abs(residual)),
    bias = mean(predicted - observed),
    correlation = if (
      stats::sd(observed) > 0 && stats::sd(predicted) > 0
    ) stats::cor(observed, predicted) else NA_real_
  )
}

estimate_duan_smearing <- function(observed_log10, predicted_log10) {
  valid <- stats::complete.cases(observed_log10, predicted_log10)
  if (!any(valid)) return(1)
  mean(10^(observed_log10[valid] - predicted_log10[valid]), na.rm = TRUE)
}

final_model_smearing <- function(model, data, target) {
  oob <- model$predictions
  if (is.null(oob) || length(oob) != nrow(data)) {
    oob <- predict(model, data = data, num.threads = 1L)$predictions
  }
  estimate_duan_smearing(data[[target]], oob)
}

#-------------------------------------------------------------------------------
# Part 2. Shared-fold cross-validation --------

# 3) Fixed-fold cross-validation

evaluate_cv <- function(data,
                        target,
                        features,
                        folds,
                        label,
                        seed = 40L,
                        num.trees = 500L,
                        min.node.size = 5L,
                        respect.unordered.factors = "partition",
                        num.threads = 1L) {
  assert_required_columns(
    data,
    c("sample_id", "Fe_pct", target, features),
    "CV data"
  )
  validate_fold_indices(folds, nrow(data))

  out_log <- rep(NA_real_, nrow(data))
  out_median <- rep(NA_real_, nrow(data))
  out_mean <- rep(NA_real_, nrow(data))
  fold_id <- rep(NA_integer_, nrow(data))
  fold_metrics <- vector("list", length(folds))

  cat("\n----", label, "----\n")
  for (fold_index in seq_along(folds)) {
    train_index <- folds[[fold_index]][[1]]
    test_index <- folds[[fold_index]][[2]]
    training_data <- data[train_index, , drop = FALSE]

    model <- fit_ranger_model(
      data = training_data,
      target = target,
      features = features,
      seed = seed + fold_index - 1L,
      num.trees = num.trees,
      min.node.size = min.node.size,
      importance = "none",
      respect.unordered.factors = respect.unordered.factors,
      num.threads = num.threads
    )
    prediction_log <- predict(
      model,
      data = data[test_index, features, drop = FALSE],
      num.threads = num.threads
    )$predictions
    smear <- final_model_smearing(model, training_data, target)

    # Direct back-transformation is reported on the geometric-mean scale;
    # the fold-specific Duan factor provides the corresponding arithmetic-mean-scale estimate.
    prediction_median <- 10^prediction_log
    prediction_mean <- prediction_median * smear
    out_log[test_index] <- prediction_log
    out_median[test_index] <- prediction_median
    out_mean[test_index] <- prediction_mean
    fold_id[test_index] <- fold_index

    log_metrics <- regression_metrics(
      data[[target]][test_index], prediction_log
    )
    raw_median_metrics <- regression_metrics(
      data$Fe_pct[test_index], prediction_median
    )
    raw_mean_metrics <- regression_metrics(
      data$Fe_pct[test_index], prediction_mean
    )
    fold_metrics[[fold_index]] <- data.frame(
      fold = fold_index,
      n_test = length(test_index),
      smearing_factor = smear,
      log_R2 = log_metrics$R2,
      log_RMSE = log_metrics$RMSE,
      log_MAE = log_metrics$MAE,
      log_bias = log_metrics$bias,
      Fe_median_R2 = raw_median_metrics$R2,
      Fe_median_RMSE = raw_median_metrics$RMSE,
      Fe_mean_R2 = raw_mean_metrics$R2,
      Fe_mean_RMSE = raw_mean_metrics$RMSE,
      Fe_mean_MAE = raw_mean_metrics$MAE,
      Fe_mean_bias = raw_mean_metrics$bias
    )
    cat(
      "Fold", fold_index,
      "| n =", length(test_index),
      "| log RMSE =", round(log_metrics$RMSE, 4), "\n"
    )
  }

  if (anyNA(out_log)) stop("OOF predictions are incomplete.", call. = FALSE)
  global_log <- regression_metrics(data[[target]], out_log)
  global_median <- regression_metrics(data$Fe_pct, out_median)
  global_mean <- regression_metrics(data$Fe_pct, out_mean)

  oof <- data.frame(
    analysis_id = if ("analysis_id" %in% names(data)) {
      data$analysis_id
    } else {
      seq_len(nrow(data))
    },
    sample_id = as.character(data$sample_id),
    spatial_fold = if ("spatial_fold" %in% names(data)) {
      data$spatial_fold
    } else {
      NA_integer_
    },
    random_fold = if ("random_fold" %in% names(data)) {
      data$random_fold
    } else {
      NA_integer_
    },
    observation_random_fold = if ("observation_random_fold" %in% names(data)) {
      data$observation_random_fold
    } else {
      NA_integer_
    },
    evaluation_fold = fold_id,
    observed_Fe_log10 = data[[target]],
    predicted_Fe_log10 = out_log,
    observed_Fe_pct = data$Fe_pct,
    predicted_Fe_pct_median = out_median,
    predicted_Fe_pct_mean = out_mean,
    stringsAsFactors = FALSE
  )

  list(
    label = label,
    fold_metrics = dplyr::bind_rows(fold_metrics),
    log_metrics = global_log,
    raw_median_metrics = global_median,
    raw_mean_metrics = global_mean,
    oof = oof
  )
}

#-------------------------------------------------------------------------------
# Part 3. Diagnostics and model summaries --------

# 4) Importance, OOF plots and metrics

importance_table <- function(model) {
  if (is.null(model$variable.importance)) {
    stop("Model does not contain variable importance values.", call. = FALSE)
  }
  data.frame(
    variable = names(model$variable.importance),
    importance = unname(model$variable.importance)
  ) |>
    dplyr::arrange(dplyr::desc(importance))
}

save_model_diagnostics <- function(model,
                                   spatial_cv,
                                   model_label,
                                   output_dir) {
  importance <- importance_table(model)
  readr::write_csv(
    importance,
    file.path(output_dir, paste0(tolower(model_label), "_importance.csv"))
  )
  readr::write_csv(
    spatial_cv$fold_metrics,
    file.path(output_dir, "spatial_fold_metrics.csv")
  )
  readr::write_csv(
    spatial_cv$oof,
    file.path(output_dir, "spatial_oof_predictions.csv")
  )

  importance_plot <- ggplot2::ggplot(
    importance,
    ggplot2::aes(
      x = stats::reorder(variable, importance),
      y = importance
    )
  ) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = paste(model_label, "permutation importance"),
      x = NULL,
      y = "Importance"
    )
  ggplot2::ggsave(
    file.path(
      output_dir,
      paste0(tolower(model_label), "_importance.png")
    ),
    importance_plot,
    width = 8,
    height = 6,
    dpi = 300
  )

  scatter <- ggplot2::ggplot(
    spatial_cv$oof,
    ggplot2::aes(
      x = observed_Fe_log10,
      y = predicted_Fe_log10
    )
  ) +
    ggplot2::geom_point(alpha = 0.55, size = 1.7) +
    ggplot2::geom_abline(
      intercept = 0,
      slope = 1,
      colour = "forestgreen",
      linewidth = 0.8
    ) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title = paste0(
        model_label,
        " spatial OOF predictions (R² = ",
        round(spatial_cv$log_metrics$R2, 3),
        ")"
      ),
      x = "Observed log10(Fe %)",
      y = "Predicted log10(Fe %)"
    )
  ggplot2::ggsave(
    file.path(
      output_dir,
      paste0(tolower(model_label), "_spatial_oof.png")
    ),
    scatter,
    width = 6.5,
    height = 6,
    dpi = 300
  )
  invisible(importance)
}

write_model_metrics <- function(model_version,
                                output_dir,
                                spatial_cv,
                                random_cv,
                                observation_random_cv,
                                final_model,
                                n,
                                features,
                                smearing_factor) {
  metrics <- data.frame(
    model = model_version,
    comparison_cohort = "shared_complete_case_cohort",
    n = n,
    n_features = length(features),
    spatial_log_R2 = spatial_cv$log_metrics$R2,
    spatial_log_RMSE = spatial_cv$log_metrics$RMSE,
    spatial_log_MAE = spatial_cv$log_metrics$MAE,
    spatial_log_bias = spatial_cv$log_metrics$bias,
    spatial_Fe_median_R2 = spatial_cv$raw_median_metrics$R2,
    spatial_Fe_median_RMSE = spatial_cv$raw_median_metrics$RMSE,
    spatial_Fe_mean_R2 = spatial_cv$raw_mean_metrics$R2,
    spatial_Fe_mean_RMSE = spatial_cv$raw_mean_metrics$RMSE,
    spatial_Fe_mean_MAE = spatial_cv$raw_mean_metrics$MAE,
    spatial_Fe_mean_bias = spatial_cv$raw_mean_metrics$bias,
    random_log_R2 = random_cv$log_metrics$R2,
    random_log_RMSE = random_cv$log_metrics$RMSE,
    random_log_MAE = random_cv$log_metrics$MAE,
    random_log_bias = random_cv$log_metrics$bias,
    random_Fe_median_R2 = random_cv$raw_median_metrics$R2,
    random_Fe_median_RMSE = random_cv$raw_median_metrics$RMSE,
    random_Fe_mean_R2 = random_cv$raw_mean_metrics$R2,
    random_Fe_mean_RMSE = random_cv$raw_mean_metrics$RMSE,
    random_Fe_mean_MAE = random_cv$raw_mean_metrics$MAE,
    random_Fe_mean_bias = random_cv$raw_mean_metrics$bias,
    observation_random_log_R2 = observation_random_cv$log_metrics$R2,
    observation_random_log_RMSE = observation_random_cv$log_metrics$RMSE,
    observation_random_log_MAE = observation_random_cv$log_metrics$MAE,
    observation_random_log_bias = observation_random_cv$log_metrics$bias,
    OOB_log_R2 = final_model$r.squared,
    OOB_log_RMSE = sqrt(final_model$prediction.error),
    final_smearing_factor = smearing_factor,
    features = paste(features, collapse = ";"),
    stringsAsFactors = FALSE
  )
  readr::write_csv(
    metrics,
    file.path(output_dir, "model_metrics.csv")
  )
  invisible(metrics)
}

#-------------------------------------------------------------------------------
# Part 4. Disk-backed spatial prediction --------

# 5) Predict the predictor-complete 1-km raster

predict_ranger_raster <- function(model,
                                  covariate_stack,
                                  features,
                                  output_path,
                                  smearing_factor = 1,
                                  domain_mask = NULL,
                                  num.threads = 1L) {
  missing <- setdiff(features, names(covariate_stack))
  if (length(missing) > 0L) {
    stop(
      "Prediction stack is missing layer(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (
    !is.null(domain_mask) &&
    !terra::compareGeom(
      covariate_stack,
      domain_mask,
      stopOnError = FALSE
    )
  ) {
    stop(
      "domain_mask geometry does not match the prediction stack.",
      call. = FALSE
    )
  }

  temporary_log_path <- tempfile(
    pattern = "rf_log_prediction_",
    tmpdir = dirname(output_path),
    fileext = ".tif"
  )
  on.exit(unlink(temporary_log_path), add = TRUE)
  ranger_predict <- function(model, data) {
    predict(
      model,
      data = data,
      num.threads = num.threads
    )$predictions
  }
  prediction_log <- terra::predict(
    covariate_stack[[features]],
    model,
    fun = ranger_predict,
    na.rm = TRUE,
    cores = 1,
    filename = temporary_log_path,
    overwrite = TRUE
  )
  if (!is.null(domain_mask)) {
    prediction_log <- terra::mask(prediction_log, domain_mask)
  }
  names(prediction_log) <- "Fe_log10_pred"
  prediction_median <- 10^prediction_log
  prediction_mean <- prediction_median * smearing_factor
  names(prediction_median) <- "Fe_pct_median"
  names(prediction_mean) <- "Fe_pct_mean"

  output <- c(
    prediction_log,
    prediction_median,
    prediction_mean
  )
  safe_write_raster(output, output_path)
  terra::rast(output_path)
}

# 6) Static prediction-map export

save_prediction_map <- function(prediction_path, output_path, model_label) {
  prediction <- terra::rast(prediction_path)
  grDevices::png(
    output_path,
    width = 1800,
    height = 1200,
    res = 180
  )
  on.exit(try(grDevices::dev.off(), silent = TRUE), add = TRUE)
  terra::plot(
    prediction[["Fe_pct_mean"]],
    main = paste(
      model_label,
      "predicted near-total Fe (%) — Duan-smearing mean"
    )
  )
  grDevices::dev.off()
  on.exit(NULL, add = FALSE)
  invisible(output_path)
}

#-------------------------------------------------------------------------------
# Part 5. Fixed V1-V4 model orchestration --------

# 7) Fit, validate, predict and save one fixed version

run_fixed_version_model <- function(version,
                                    data,
                                    features,
                                    prediction_stack,
                                    domain_mask,
                                    cfg,
                                    output_dir) {
  target <- "Fe_log10"
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  obsolete <- list.files(
    output_dir,
    pattern = paste0(
      "^(Fe_uncertainty_|applicability_diagnostic_|boruta_|",
      "diagnostic_feature_ablation|v[1-4]_state\\.rds$)"
    ),
    full.names = TRUE
  )
  if (length(obsolete) > 0L) unlink(obsolete)
  assert_required_columns(
    data,
    c(
      "sample_id", "Fe_pct", target, features,
      "spatial_fold", "random_fold", "observation_random_fold"
    ),
    paste(version, "data")
  )

  spatial_cv <- evaluate_cv(
    data = data,
    target = target,
    features = features,
    folds = folds_from_column(data, "spatial_fold"),
    label = paste(version, "SHARED SPATIAL BLOCK CV"),
    seed = cfg$seed,
    num.trees = cfg$ranger$num.trees,
    min.node.size = cfg$ranger$min.node.size,
    respect.unordered.factors = cfg$ranger$respect.unordered.factors,
    num.threads = cfg$ranger$num.threads
  )
  random_cv <- evaluate_cv(
    data = data,
    target = target,
    features = features,
    folds = folds_from_column(data, "random_fold"),
    label = paste(version, "SHARED GRID-GROUPED RANDOM CV"),
    seed = cfg$seed,
    num.trees = cfg$ranger$num.trees,
    min.node.size = cfg$ranger$min.node.size,
    respect.unordered.factors = cfg$ranger$respect.unordered.factors,
    num.threads = cfg$ranger$num.threads
  )
  observation_random_cv <- evaluate_cv(
    data = data,
    target = target,
    features = features,
    folds = folds_from_column(data, "observation_random_fold"),
    label = paste(version, "SHARED OBSERVATION-LEVEL RANDOM CV"),
    seed = cfg$seed,
    num.trees = cfg$ranger$num.trees,
    min.node.size = cfg$ranger$min.node.size,
    respect.unordered.factors = cfg$ranger$respect.unordered.factors,
    num.threads = cfg$ranger$num.threads
  )

  model <- fit_ranger_model(
    data = data,
    target = target,
    features = features,
    seed = cfg$seed,
    num.trees = cfg$ranger$num.trees,
    min.node.size = cfg$ranger$min.node.size,
    importance = cfg$ranger$importance,
    respect.unordered.factors = cfg$ranger$respect.unordered.factors,
    num.threads = cfg$ranger$num.threads
  )
  smearing_factor <- final_model_smearing(model, data, target)
  model_path <- file.path(
    output_dir,
    paste0("rf_", tolower(version), ".rds")
  )
  saveRDS(model, model_path)

  save_model_diagnostics(
    model,
    spatial_cv,
    version,
    output_dir
  )
  readr::write_csv(
    random_cv$fold_metrics,
    file.path(output_dir, "random_fold_metrics.csv")
  )
  readr::write_csv(
    random_cv$oof,
    file.path(output_dir, "random_oof_predictions.csv")
  )
  readr::write_csv(
    observation_random_cv$fold_metrics,
    file.path(output_dir, "observation_random_fold_metrics.csv")
  )
  readr::write_csv(
    observation_random_cv$oof,
    file.path(output_dir, "observation_random_oof_predictions.csv")
  )
  write_model_metrics(
    model_version = version,
    output_dir = output_dir,
    spatial_cv = spatial_cv,
    random_cv = random_cv,
    observation_random_cv = observation_random_cv,
    final_model = model,
    n = nrow(data),
    features = features,
    smearing_factor = smearing_factor
  )
  readr::write_csv(
    data.frame(feature = features),
    file.path(output_dir, "fixed_features.csv")
  )

  prediction_path <- file.path(
    output_dir,
    paste0(
      "Fe_prediction_", tolower(version), "_",
      cfg$resolution_m, "m.tif"
    )
  )
  predict_ranger_raster(
    model,
    prediction_stack,
    features,
    prediction_path,
    smearing_factor = smearing_factor,
    domain_mask = domain_mask,
    num.threads = cfg$ranger$num.threads
  )
  prediction_map_path <- file.path(
    output_dir,
    paste0("Fe_prediction_", tolower(version), "_map.png")
  )
  save_prediction_map(
    prediction_path,
    prediction_map_path,
    version
  )

  list(
    version = version,
    target = target,
    features = features,
    n = nrow(data),
    model_path = model_path,
    prediction_path = prediction_path,
    prediction_map_path = prediction_map_path,
    metrics_path = file.path(output_dir, "model_metrics.csv"),
    oof_path = file.path(output_dir, "spatial_oof_predictions.csv"),
    smearing_factor = smearing_factor,
    spatial_cv = spatial_cv,
    random_cv = random_cv,
    observation_random_cv = observation_random_cv
  )
}

#-------------------------------------------------------------------------------
# Part 6. Optional final-model sensitivity --------

# 8) Paired subset refit for AES_HF-only or shallow-only observations

# Used only for selected-model sensitivity analyses; 
# this function is excluded from the V1-V4 candidate comparison.
subset_sensitivity_cv <- function(data,
                                  features,
                                  main_spatial_oof,
                                  subset_mask,
                                  cfg,
                                  label,
                                  minimum_n = 100L) {
  assert_required_columns(
    data,
    c("sample_id", "Fe_pct", "Fe_log10", "spatial_fold", features),
    "sensitivity data"
  )
  if (length(subset_mask) != nrow(data)) {
    stop("subset_mask length does not match the model cohort.", call. = FALSE)
  }
  subset_mask[is.na(subset_mask)] <- FALSE
  subset <- data[subset_mask, , drop = FALSE]
  if (
    nrow(subset) < minimum_n ||
    !setequal(unique(subset$spatial_fold), seq_len(cfg$cv$k))
  ) {
    return(list(
      status = "skipped",
      reason = "insufficient_observations_or_fold_coverage",
      n = nrow(subset)
    ))
  }

  refit <- evaluate_cv(
    data = subset,
    target = "Fe_log10",
    features = features,
    folds = folds_from_column(subset, "spatial_fold"),
    label = paste(label, "SPATIAL CV"),
    seed = cfg$seed,
    num.trees = cfg$ranger$num.trees,
    min.node.size = cfg$ranger$min.node.size,
    respect.unordered.factors = cfg$ranger$respect.unordered.factors,
    num.threads = cfg$ranger$num.threads
  )
  main <- main_spatial_oof[
    match(subset$sample_id, main_spatial_oof$sample_id),
    ,
    drop = FALSE
  ]
  if (anyNA(main$sample_id)) {
    stop("Main OOF predictions do not match the sensitivity subset.",
         call. = FALSE)
  }
  main_metrics <- regression_metrics(
    subset$Fe_log10,
    main$predicted_Fe_log10
  )
  metrics <- dplyr::bind_rows(
    dplyr::mutate(main_metrics, fit = "main_model"),
    dplyr::mutate(refit$log_metrics, fit = "subset_refit")
  ) |>
    dplyr::relocate(fit)

  list(
    status = "completed",
    metrics = metrics,
    oof = data.frame(
      sample_id = subset$sample_id,
      spatial_fold = subset$spatial_fold,
      observed_Fe_log10 = subset$Fe_log10,
      predicted_main_Fe_log10 = main$predicted_Fe_log10,
      predicted_subset_refit_Fe_log10 =
        refit$oof$predicted_Fe_log10,
      stringsAsFactors = FALSE
    )
  )
}
