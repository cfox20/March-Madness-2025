library("Metrics")
library("readr")
library("xgboost")
library("tidyverse")

source("R/config.R")
source("R/pipeline_options.R")
source("R/xgb_utils.R")
source("read_data.R")

export_model_input_data <- function(train_frame, valid_frame, feature_names, input_files) {
  dir.create(MODEL_INPUT_DIR, recursive = TRUE, showWarnings = FALSE)

  write_csv(train_frame, input_files$train)
  write_csv(valid_frame, input_files$valid)
  write_csv(tibble(feature_name = feature_names), input_files$features)
}

oversample_tournament_rows <- function(data, oversample_factor = 1) {
  if (is.null(oversample_factor)) {
    oversample_factor <- 1L
  }

  oversample_factor <- max(1L, as.integer(oversample_factor))

  if (oversample_factor <= 1L || !"sample_weight" %in% names(data)) {
    return(data)
  }

  tournament_rows <- data |>
    filter(sample_weight > 1)

  if (nrow(tournament_rows) == 0) {
    return(data)
  }

  bind_rows(data, rep(list(tournament_rows), oversample_factor - 1L))
}

resolve_task_model_params <- function(params, task_type = c("classification", "regression")) {
  task_type <- match.arg(task_type)
  resolved <- params

  if (identical(task_type, "classification")) {
    if (is.null(resolved$objective)) {
      resolved$objective <- "binary:logistic"
    }

    if (is.null(resolved$eval_metric)) {
      resolved$eval_metric <- "logloss"
    }
  } else {
    if (is.null(resolved$objective) || identical(resolved$objective, "binary:logistic")) {
      resolved$objective <- "reg:squarederror"
    }

    if (is.null(resolved$eval_metric) || identical(resolved$eval_metric, "logloss")) {
      resolved$eval_metric <- "rmse"
    }
  }

  resolved
}

build_training_labels <- function(frame, label_name, task_type = c("classification", "regression")) {
  task_type <- match.arg(task_type)

  if (identical(task_type, "classification")) {
    as.integer(frame[[label_name]] == "a")
  } else {
    as.numeric(frame[[label_name]])
  }
}

train_division_model <- function(train_data_path, feature_file, model_file, prep_fn, input_files, options, division, label_name = "w_team", task_type = c("classification", "regression"), calibrate = TRUE) {
  task_type <- match.arg(task_type)
  model_data <- read_rds(train_data_path)

  train_frame_raw <- prep_fn(model_data$train_data)
  valid_frame_raw <- prep_fn(model_data$valid_data)

  train_frame_raw <- limit_rows(train_frame_raw, options$model$max_train_rows, options$seed)
  valid_frame_raw <- limit_rows(valid_frame_raw, options$model$max_valid_rows, options$seed)
  train_frame_raw <- oversample_tournament_rows(train_frame_raw, options$model$tournament_oversample_factor)

  train_weights <- if ("sample_weight" %in% names(train_frame_raw)) train_frame_raw$sample_weight else rep(1, nrow(train_frame_raw))
  valid_weights <- if ("sample_weight" %in% names(valid_frame_raw)) valid_frame_raw$sample_weight else rep(1, nrow(valid_frame_raw))

  train_frame <- train_frame_raw |>
    apply_model_feature_selection(options, division, label_name = label_name)
  valid_frame <- valid_frame_raw |>
    apply_model_feature_selection(options, division, label_name = label_name)

  train_prepared <- prepare_matrix(train_frame, excluded_columns = label_name)
  valid_prepared <- prepare_matrix(
    valid_frame,
    excluded_columns = label_name,
    reference_feature_names = train_prepared$feature_names
  )

  export_model_input_data(
    train_frame = train_frame,
    valid_frame = valid_frame,
    feature_names = train_prepared$feature_names,
    input_files = input_files
  )

  train_input <- list(
    matrix = train_prepared$matrix,
    label = build_training_labels(train_frame, label_name, task_type = task_type),
    weight = train_weights
  )

  valid_input <- list(
    matrix = valid_prepared$matrix,
    label = build_training_labels(valid_frame, label_name, task_type = task_type),
    weight = valid_weights
  )

  model_params <- resolve_task_model_params(options$model$params, task_type = task_type)

  if (isTRUE(options$model$tune_regularization) && length(valid_input$label) > 0) {
    tuning_results <- tune_xgb_regularization(
      train_data = train_input,
      valid_data = valid_input,
      params = model_params,
      regularization_grid = options$model$regularization_grid,
      nrounds = options$model$nrounds,
      early_stopping_rounds = options$model$early_stopping_rounds,
      task_type = task_type
    )

    model_params <- tuning_results$best_params
    print(tuning_results$results)
    print(
      tibble(
        selected_lambda = model_params$lambda,
        selected_alpha = model_params$alpha,
        selected_gamma = model_params$gamma
      )
    )
  }

  results <- train_xgb_model(
    train_data = train_input,
    valid_data = valid_input,
    params = model_params,
    nrounds = options$model$nrounds,
    early_stopping_rounds = options$model$early_stopping_rounds,
    task_type = task_type
  )

  xgb.save(results$model, model_file)
  write_rds(train_prepared$feature_names, feature_file)

  calibration_method <- options$model$calibration$method %fallback% "none"
  calibration_file <- CALIBRATION_FILES[[division]]

  if (isTRUE(calibrate) && identical(task_type, "classification") && identical(calibration_method, "platt")) {
    calibration_model <- fit_platt_scaler(valid_input$label, results$predictions)
    write_rds(calibration_model, calibration_file)

    if (!is.null(calibration_model)) {
      calibrated_predictions <- apply_probability_calibration(results$predictions, calibration_model)
      print(tibble(
        calibration_method = calibration_method,
        calibrated_logloss = Metrics::logLoss(valid_input$label, calibrated_predictions),
        calibrated_mse = Metrics::mse(valid_input$label, calibrated_predictions),
        calibrated_accuracy = mean((calibrated_predictions >= 0.5) == valid_input$label)
      ))
    }
  } else if (isTRUE(calibrate) && file.exists(calibration_file)) {
    file.remove(calibration_file)
  }

  print(results$metrics)

  invisible(results)
}

run_train_xgboost <- function(options = default_pipeline_options()) {
  options <- resolve_pipeline_options(options)
  results <- list()

  if (division_enabled(options, "mens")) {
    results$mens <- train_division_model(
      train_data_path = MODEL_DATA_FILES$mens_train,
      feature_file = FEATURE_FILES$mens,
      model_file = MODEL_FILES$mens,
      prep_fn = prepare_mens_training_frame,
      division = "mens",
      input_files = list(
        train = MODEL_INPUT_FILES$mens_train,
        valid = MODEL_INPUT_FILES$mens_valid,
        features = MODEL_INPUT_FILES$mens_features
      ),
      options = options
    )
  }

  if (division_enabled(options, "womens")) {
    results$womens <- train_division_model(
      train_data_path = MODEL_DATA_FILES$womens_train,
      feature_file = FEATURE_FILES$womens,
      model_file = MODEL_FILES$womens,
      prep_fn = prepare_womens_training_frame,
      division = "womens",
      input_files = list(
        train = MODEL_INPUT_FILES$womens_train,
        valid = MODEL_INPUT_FILES$womens_valid,
        features = MODEL_INPUT_FILES$womens_features
      ),
      options = options
    )
  }

  invisible(results)
}
