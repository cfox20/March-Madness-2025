library("dplyr")
library("readr")
library("stringr")
library("tibble")
library("tidyr")
source("R/config.R")
source("R/pipeline_options.R")

`%fallback%` <- function(x, y) {
  if (is.null(x)) y else x
}

clamp_probabilities <- function(x) {
  pmin(pmax(x, 0), 1)
}

safe_qlogis <- function(x, eps = 1e-6) {
  stats::qlogis(clamp_probabilities(x) |> pmin(1 - eps) |> pmax(eps))
}

compute_binary_metrics <- function(actual, predicted) {
  tibble(
    logloss = Metrics::logLoss(actual, predicted),
    mse = Metrics::mse(actual, predicted),
    accuracy = mean((predicted >= 0.5) == actual)
  )
}

sanitize_dummy_level <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("^_+|_+$", "")
}

expand_categorical_features <- function(data) {
  categorical_columns <- names(data)[vapply(data, function(x) is.character(x) || is.factor(x), logical(1))]

  if (length(categorical_columns) == 0) {
    return(data)
  }

  expanded <- data |> select(-any_of(categorical_columns))

  for (column_name in categorical_columns) {
    column_values <- as.character(data[[column_name]])
    column_levels <- sort(unique(column_values[!is.na(column_values)]))

    if (length(column_levels) == 0) {
      next
    }

    dummy_columns <- lapply(column_levels, function(level_value) {
      dummy <- as.integer(column_values == level_value)
      dummy[is.na(column_values)] <- NA_integer_
      dummy
    })

    dummy_names <- paste0(column_name, "__", sanitize_dummy_level(column_levels))
    expanded <- bind_cols(expanded, as_tibble(setNames(dummy_columns, dummy_names)))
  }

  expanded
}

prepare_matrix <- function(data, excluded_columns = character(), reference_feature_names = NULL) {
  raw_team_id_columns <- intersect(c("a_team_id", "b_team_id"), names(data))

  feature_data <- data |>
    select(-any_of(c(excluded_columns, raw_team_id_columns))) |>
    expand_categorical_features() |>
    mutate(
      across(where(is.logical), as.integer)
    )

  if (!is.null(reference_feature_names)) {
    feature_data <- align_feature_frame(feature_data, reference_feature_names)
  }

  list(
    matrix = as.matrix(feature_data),
    feature_names = names(feature_data)
  )
}

align_feature_frame <- function(data, feature_names) {
  missing_features <- setdiff(feature_names, names(data))

  aligned <- data

  for (feature_name in missing_features) {
    aligned[[feature_name]] <- 0
  }

  aligned |>
    select(all_of(feature_names))
}

get_mens_training_features <- function(train_data) {
  filtered_data <- train_data |>
    relocate(season, .after = b_team_id) |>
    select(-day_num)

  resolve_feature_columns(filtered_data, pipeline_options, "mens", label_name = "w_team")
}

get_womens_training_features <- function(train_data) {
  filtered_data <- train_data |>
    relocate(season, .after = b_team_id) |>
    select(-day_num)

  resolve_feature_columns(filtered_data, pipeline_options, "womens", label_name = "w_team")
}

prepare_mens_training_frame <- function(data) {
  data |>
    relocate(season, .after = b_team_id) |>
    mutate(season = as.character(season)) |>
    select(-day_num)
}

prepare_womens_training_frame <- function(data) {
  data |>
    relocate(season, .after = b_team_id) |>
    mutate(season = as.character(season)) |>
    select(-day_num)
}

apply_model_feature_selection <- function(data, options, division, label_name = "w_team") {
  selected_columns <- resolve_feature_columns(data, options, division, label_name = label_name)
  data |>
    select(any_of(selected_columns))
}

train_xgb_model <- function(train_data, valid_data, params, nrounds = 2000, early_stopping_rounds = 100) {
  dtrain <- xgboost::xgb.DMatrix(
    data = train_data$matrix,
    label = train_data$label,
    weight = train_data$weight %fallback% rep(1, length(train_data$label))
  )
  dvalid <- xgboost::xgb.DMatrix(
    data = valid_data$matrix,
    label = valid_data$label,
    weight = valid_data$weight %fallback% rep(1, length(valid_data$label))
  )

  model <- xgboost::xgb.train(
    params = params,
    data = dtrain,
    nrounds = nrounds,
    evals = list(train = dtrain, valid = dvalid),
    early_stopping_rounds = early_stopping_rounds,
    verbose = 1
  )

  predictions <- predict(model, dvalid)

  list(
    model = model,
    metrics = compute_binary_metrics(valid_data$label, predictions),
    predictions = predictions
  )
}

expand_regularization_grid <- function(grid) {
  if (is.null(grid) || length(grid) == 0) {
    return(tibble())
  }

  tidyr::expand_grid(
    lambda = grid$lambda %fallback% 0,
    alpha = grid$alpha %fallback% 0,
    gamma = grid$gamma %fallback% 0
  ) |>
    distinct()
}

tune_xgb_regularization <- function(train_data, valid_data, params, regularization_grid, nrounds = 2000, early_stopping_rounds = 100) {
  grid <- expand_regularization_grid(regularization_grid)

  if (nrow(grid) == 0) {
    return(list(best_params = params, results = tibble()))
  }

  tuning_results <- purrr::pmap_dfr(grid, function(lambda, alpha, gamma) {
    tuned_params <- params
    tuned_params$lambda <- lambda
    tuned_params$alpha <- alpha
    tuned_params$gamma <- gamma

    results <- train_xgb_model(
      train_data = train_data,
      valid_data = valid_data,
      params = tuned_params,
      nrounds = nrounds,
      early_stopping_rounds = early_stopping_rounds
    )

    tibble(
      lambda = lambda,
      alpha = alpha,
      gamma = gamma,
      logloss = results$metrics$logloss[[1]],
      mse = results$metrics$mse[[1]],
      accuracy = results$metrics$accuracy[[1]],
      best_iteration = results$model$best_iteration %fallback% NA_integer_
    )
  })

  best_result <- tuning_results |>
    arrange(logloss, mse, desc(accuracy)) |>
    slice(1)

  best_params <- params
  best_params$lambda <- best_result$lambda[[1]]
  best_params$alpha <- best_result$alpha[[1]]
  best_params$gamma <- best_result$gamma[[1]]

  list(
    best_params = best_params,
    results = tuning_results
  )
}

make_matchup_id <- function(season, a_team_id, b_team_id) {
  glue::glue("{season}_{a_team_id}_{b_team_id}")
}

fit_platt_scaler <- function(actual, predicted) {
  if (length(actual) == 0 || length(predicted) == 0) {
    return(NULL)
  }

  calibration_frame <- tibble(
    actual = actual,
    logit_pred = safe_qlogis(predicted)
  )

  calibration_fit <- tryCatch(
    stats::glm(actual ~ logit_pred, data = calibration_frame, family = stats::binomial()),
    error = function(...) NULL
  )

  if (is.null(calibration_fit)) {
    return(NULL)
  }

  coefficients <- stats::coef(calibration_fit)

  if (any(is.na(coefficients))) {
    return(NULL)
  }

  list(
    method = "platt",
    intercept = unname(coefficients[[1]]),
    slope = unname(coefficients[[2]])
  )
}

apply_probability_calibration <- function(predicted, calibration_model = NULL) {
  if (is.null(calibration_model) || !identical(calibration_model$method, "platt")) {
    return(clamp_probabilities(predicted))
  }

  stats::plogis(calibration_model$intercept + calibration_model$slope * safe_qlogis(predicted)) |>
    clamp_probabilities()
}

build_seed_matchup_calibration_report <- function(valid_frame, predicted_probabilities) {
  required_columns <- c("w_team", "a_seed", "b_seed")

  if (!all(required_columns %in% names(valid_frame)) || length(predicted_probabilities) != nrow(valid_frame)) {
    return(tibble())
  }

  valid_frame |>
    mutate(
      predicted_a = predicted_probabilities,
      predicted_b = 1 - predicted_probabilities,
      a_seed = as.integer(a_seed),
      b_seed = as.integer(b_seed)
    ) |>
    filter(!is.na(a_seed), !is.na(b_seed), a_seed != b_seed) |>
    transmute(
      top_seed = pmin(a_seed, b_seed),
      bottom_seed = pmax(a_seed, b_seed),
      lower_seed_won = case_when(
        w_team == "a" & a_seed > b_seed ~ 1,
        w_team == "b" & b_seed > a_seed ~ 1,
        TRUE ~ 0
      ),
      lower_seed_predicted = case_when(
        a_seed > b_seed ~ predicted_a,
        b_seed > a_seed ~ predicted_b,
        TRUE ~ NA_real_
      )
    ) |>
    group_by(top_seed, bottom_seed) |>
    summarise(
      games = n(),
      actual_lower_seed_win_rate = mean(lower_seed_won),
      avg_model_lower_seed_win_prob = mean(lower_seed_predicted, na.rm = TRUE),
      calibration_gap = avg_model_lower_seed_win_prob - actual_lower_seed_win_rate,
      .groups = "drop"
    ) |>
    arrange(desc(games))
}
