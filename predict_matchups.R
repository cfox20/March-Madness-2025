library("readr")
library("xgboost")
library("tidyverse")

source("R/config.R")
source("R/pipeline_options.R")
source("R/xgb_utils.R")
source("read_data.R")

build_seed_table <- function(seed_data, tournament_season) {
  seed_data |>
    filter(season == tournament_season) |>
    mutate(
      region = str_extract(seed, "[A-Za-z]+"),
      seed = str_extract(seed, "\\d+") |> as.integer()
    ) |>
    select(team_id, seed, region)
}

build_matchup_grid <- function(seed_table, tournament_season) {
  a_seed_table <- seed_table |>
    rename(a_team_id = team_id, a_seed = seed, a_region = region)

  b_seed_table <- seed_table |>
    rename(b_team_id = team_id, b_seed = seed, b_region = region)

  expand_grid(a_team_id = seed_table$team_id, b_team_id = seed_table$team_id) |>
    filter(a_team_id < b_team_id) |>
    left_join(a_seed_table, by = "a_team_id") |>
    left_join(b_seed_table, by = "b_team_id") |>
    mutate(ID = make_matchup_id(tournament_season, a_team_id, b_team_id)) |>
    relocate(ID)
}

extract_latest_team_features_from_training <- function(model_data_file, tournament_season) {
  if (!file.exists(model_data_file)) {
    return(tibble(team_id = integer()))
  }

  training_bundle <- read_rds(model_data_file)
  train_data <- training_bundle$train_data

  if (is.null(train_data) || nrow(train_data) == 0) {
    return(tibble(team_id = integer()))
  }

  a_features <- train_data |>
    filter(season == tournament_season) |>
    select(day_num, team_id = a_team_id, starts_with("a_")) |>
    rename_with(~ str_remove(.x, "^a_"), starts_with("a_"))

  b_features <- train_data |>
    filter(season == tournament_season) |>
    select(day_num, team_id = b_team_id, starts_with("b_")) |>
    rename_with(~ str_remove(.x, "^b_"), starts_with("b_"))

  bind_rows(a_features, b_features) |>
    arrange(team_id, desc(day_num)) |>
    distinct(team_id, .keep_all = TRUE) |>
    select(-day_num)
}

build_seed_history_report <- function(scored_matchups, seed_data, tournament_results, tournament_season) {
  historical_rates <- build_historical_seed_rates(
    seed_data = seed_data,
    tournament_results = tournament_results,
    tournament_season = tournament_season
  )

  current_prediction_rates <- scored_matchups |>
    filter(!is.na(a_seed), !is.na(b_seed), a_seed != b_seed) |>
    transmute(
      top_seed = pmin(a_seed, b_seed),
      bottom_seed = pmax(a_seed, b_seed),
      lower_seed_predicted_win_prob = if_else(a_seed > b_seed, xgb_predicted, 1 - xgb_predicted)
    ) |>
    group_by(top_seed, bottom_seed) |>
    summarise(
      current_matchups = n(),
      mean_2026_lower_seed_win_prob = mean(lower_seed_predicted_win_prob),
      .groups = "drop"
    )

  historical_rates |>
    inner_join(current_prediction_rates, by = c("top_seed", "bottom_seed")) |>
    mutate(
      probability_gap = mean_2026_lower_seed_win_prob - historical_lower_seed_win_rate
    ) |>
    arrange(top_seed, bottom_seed)
}

build_historical_seed_rates <- function(seed_data, tournament_results, tournament_season) {
  numeric_seeds <- seed_data |>
    mutate(seed = str_extract(seed, "\\d+") |> as.integer())

  tournament_results |>
    transmute(
      season,
      w_team_id,
      l_team_id
    ) |>
    left_join(
      numeric_seeds |>
        select(season, team_id, w_seed = seed),
      by = c("season", "w_team_id" = "team_id")
    ) |>
    left_join(
      numeric_seeds |>
        select(season, team_id, l_seed = seed),
      by = c("season", "l_team_id" = "team_id")
    ) |>
    filter(
      season < tournament_season,
      !is.na(w_seed),
      !is.na(l_seed),
      w_seed != l_seed
    ) |>
    transmute(
      top_seed = pmin(w_seed, l_seed),
      bottom_seed = pmax(w_seed, l_seed),
      historical_lower_seed_win_rate = as.integer(w_seed > l_seed)
    ) |>
    group_by(top_seed, bottom_seed) |>
    summarise(
      historical_games = n(),
      historical_lower_seed_win_rate = mean(historical_lower_seed_win_rate),
      .groups = "drop"
    )
}

normalize_seed_matchup_labels <- function(matchups) {
  if (is.null(matchups) || length(matchups) == 0) {
    return(character())
  }

  matchups |>
    as.character() |>
    stringr::str_trim() |>
    stringr::str_replace_all("\\s+", "") |>
    purrr::map_chr(function(matchup) {
      parts <- stringr::str_split(matchup, "[-:/]", simplify = TRUE)
      if (length(parts) < 2) {
        return(NA_character_)
      }

      seeds <- suppressWarnings(as.integer(parts[1:2]))
      if (any(is.na(seeds))) {
        return(NA_character_)
      }

      paste(sort(seeds), collapse = "-")
    }) |>
    stats::na.omit() |>
    unique()
}

apply_seed_matchup_shrinkage <- function(scored_matchups, historical_rates, shrinkage_options = NULL) {
  if (is.null(shrinkage_options)) {
    return(scored_matchups)
  }

  matchups <- normalize_seed_matchup_labels(shrinkage_options$matchups)
  strength <- shrinkage_options$strength

  if (is.null(strength)) {
    strength <- 0
  }

  if (length(matchups) == 0 || is.null(strength) || !is.finite(strength) || strength <= 0) {
    return(scored_matchups)
  }

  strength <- max(0, min(1, as.numeric(strength)))

  scored_matchups |>
    mutate(
      top_seed = pmin(a_seed, b_seed),
      bottom_seed = pmax(a_seed, b_seed),
      matchup_label = paste(top_seed, bottom_seed, sep = "-")
    ) |>
    left_join(
      historical_rates |>
        select(top_seed, bottom_seed, historical_lower_seed_win_rate),
      by = c("top_seed", "bottom_seed")
    ) |>
    mutate(
      lower_seed_predicted_win_prob = if_else(a_seed > b_seed, xgb_predicted, 1 - xgb_predicted),
      lower_seed_predicted_win_prob = if_else(
        matchup_label %in% matchups &
          !is.na(historical_lower_seed_win_rate) &
          lower_seed_predicted_win_prob < historical_lower_seed_win_rate,
        ((1 - strength) * lower_seed_predicted_win_prob) + (strength * historical_lower_seed_win_rate),
        lower_seed_predicted_win_prob
      ),
      xgb_predicted = if_else(
        a_seed > b_seed,
        lower_seed_predicted_win_prob,
        1 - lower_seed_predicted_win_prob
      ) |>
        clamp_probabilities()
    ) |>
    select(-top_seed, -bottom_seed, -matchup_label, -historical_lower_seed_win_rate, -lower_seed_predicted_win_prob)
}

apply_injury_adjustments <- function(scored_matchups, injury_options = NULL) {
  if (is.null(injury_options)) {
    return(scored_matchups)
  }

  team_penalties <- injury_options$teams

  if (is.null(team_penalties) || length(team_penalties) == 0) {
    return(scored_matchups)
  }

  if (!is.list(team_penalties)) {
    team_penalties <- as.list(team_penalties)
  }

  team_penalties <- purrr::imap(team_penalties, function(value, team_name) {
    penalty <- suppressWarnings(as.numeric(value))
    team_name <- stringr::str_trim(as.character(team_name))

    if (!nzchar(team_name) || is.na(penalty) || !is.finite(penalty) || penalty <= 0) {
      return(NULL)
    }

    max(0, min(1, penalty))
  }) |>
    purrr::compact()

  if (length(team_penalties) == 0) {
    return(scored_matchups)
  }

  scored_matchups |>
    mutate(
      a_penalty = purrr::map_dbl(a_team_name, function(team_name) {
        penalty <- team_penalties[[team_name]]
        if (is.null(penalty)) 0 else penalty
      }),
      b_penalty = purrr::map_dbl(b_team_name, function(team_name) {
        penalty <- team_penalties[[team_name]]
        if (is.null(penalty)) 0 else penalty
      }),
      xgb_predicted = case_when(
        a_penalty > 0 & b_penalty <= 0 ~ xgb_predicted - a_penalty,
        a_penalty <= 0 & b_penalty > 0 ~ xgb_predicted + b_penalty,
        a_penalty > 0 & b_penalty > 0 ~ xgb_predicted + b_penalty - a_penalty,
        TRUE ~ xgb_predicted
      ) |>
        clamp_probabilities()
    ) |>
    select(-a_penalty, -b_penalty)
}

build_matchup_features <- function(matchups, team_features, feature_names, tournament_season, include_conference = FALSE) {
  a_features <- team_features |> rename(a_team_id = team_id) |> rename_with(~ paste0("a_", .x), -a_team_id)
  b_features <- team_features |> rename(b_team_id = team_id) |> rename_with(~ paste0("b_", .x), -b_team_id)

  feature_frame <- matchups |>
    left_join(a_features, by = "a_team_id") |>
    left_join(b_features, by = "b_team_id")

  if (!"a_seed" %in% names(feature_frame)) {
    feature_frame$a_seed <- NA_integer_
  }

  if (!"b_seed" %in% names(feature_frame)) {
    feature_frame$b_seed <- NA_integer_
  }

  feature_frame <- feature_frame |>
    mutate(
      season = as.character(tournament_season),
      seed_diff = a_seed - b_seed
    )

  if (all(c("a_elo", "b_elo") %in% names(feature_frame))) {
    feature_frame <- feature_frame |> mutate(elo_diff = a_elo - b_elo)
  }

  if (all(c("a_massey", "b_massey") %in% names(feature_frame))) {
    feature_frame <- feature_frame |> mutate(massey_diff = a_massey - b_massey)
  }

  if (all(c("a_colley", "b_colley") %in% names(feature_frame))) {
    feature_frame <- feature_frame |> mutate(colley_diff = a_colley - b_colley)
  }

  if (all(c("a_pagerank", "b_pagerank") %in% names(feature_frame))) {
    feature_frame <- feature_frame |> mutate(pagerank_diff = a_pagerank - b_pagerank)
  }

  if (all(c("a_adj_net_rating", "b_adj_net_rating") %in% names(feature_frame))) {
    feature_frame <- feature_frame |> mutate(adj_net_rating_diff = a_adj_net_rating - b_adj_net_rating)
  }

  prepared <- prepare_matrix(feature_frame, reference_feature_names = feature_names)

  list(raw = feature_frame, matrix = prepared$matrix)
}

score_division <- function(seed_data, team_features, model_file, feature_file, team_lookup, output_file, tournament_season, tournament_results, shrinkage_options = NULL, injury_options = NULL, include_conference = FALSE) {
  feature_names <- read_rds(feature_file)
  model <- xgb.load(model_file)
  calibration_file <- if (identical(output_file, PREDICTION_FILES$mens)) CALIBRATION_FILES$mens else CALIBRATION_FILES$womens
  calibration_model <- if (file.exists(calibration_file)) read_rds(calibration_file) else NULL

  seed_table <- build_seed_table(seed_data, tournament_season)
  matchups <- build_matchup_grid(seed_table, tournament_season)
  matchup_features <- build_matchup_features(
    matchups = matchups,
    team_features = team_features,
    feature_names = feature_names,
    tournament_season = tournament_season,
    include_conference = include_conference
  )

  predictions <- predict(model, xgb.DMatrix(matchup_features$matrix)) |>
    apply_probability_calibration(calibration_model)

  scored_matchups <- matchups |>
    mutate(xgb_predicted = clamp_probabilities(predictions)) |>
    left_join(rename(team_lookup, a_team_name = team_name), by = c("a_team_id" = "team_id")) |>
    left_join(rename(team_lookup, b_team_name = team_name), by = c("b_team_id" = "team_id"))

  scored_matchups <- apply_seed_matchup_shrinkage(
    scored_matchups = scored_matchups,
    historical_rates = build_historical_seed_rates(
      seed_data = seed_data,
      tournament_results = tournament_results,
      tournament_season = tournament_season
    ),
    shrinkage_options = shrinkage_options
  )

  scored_matchups <- apply_injury_adjustments(
    scored_matchups = scored_matchups,
    injury_options = injury_options
  )

  write_rds(scored_matchups, output_file)

  scored_matchups
}

score_sample_submission_division <- function(sample_rows, team_features, model_file, feature_file, team_lookup, tournament_season, injury_options = NULL) {
  if (
    nrow(sample_rows) == 0 ||
      nrow(team_features) == 0 ||
      !file.exists(model_file) ||
      !file.exists(feature_file)
  ) {
    return(tibble(ID = character(), Pred = numeric()))
  }

  feature_names <- read_rds(feature_file)
  model <- xgb.load(model_file)
  calibration_file <- if (identical(model_file, MODEL_FILES$mens)) CALIBRATION_FILES$mens else CALIBRATION_FILES$womens
  calibration_model <- if (file.exists(calibration_file)) read_rds(calibration_file) else NULL

  matchups <- sample_rows |>
    mutate(
      a_seed = NA_integer_,
      b_seed = NA_integer_
    )

  matchup_features <- build_matchup_features(
    matchups = matchups,
    team_features = team_features,
    feature_names = feature_names,
    tournament_season = tournament_season
  )

  predictions <- predict(model, xgb.DMatrix(matchup_features$matrix)) |>
    apply_probability_calibration(calibration_model)

  scored_matchups <- matchups |>
    mutate(xgb_predicted = clamp_probabilities(predictions)) |>
    left_join(rename(team_lookup, a_team_name = team_name), by = c("a_team_id" = "team_id")) |>
    left_join(rename(team_lookup, b_team_name = team_name), by = c("b_team_id" = "team_id")) |>
    apply_injury_adjustments(injury_options = injury_options)

  scored_matchups |>
    transmute(ID, Pred = if_else(xgb_predicted > 0.95, 1, xgb_predicted))
}

build_sample_submission_predictions <- function(options, tournament_season) {
  sample_submission <- read_csv(file.path(INPUT_DIR, "SampleSubmissionStage2.csv"), show_col_types = FALSE) |>
    tidyr::separate(ID, into = c("season", "a_team_id", "b_team_id"), sep = "_", convert = TRUE, remove = FALSE)

  mens_team_features <- extract_latest_team_features_from_training(MODEL_DATA_FILES$mens_train, tournament_season)
  womens_team_features <- extract_latest_team_features_from_training(MODEL_DATA_FILES$womens_train, tournament_season)

  mens_team_ids <- unique(mens_team_features$team_id)
  womens_team_ids <- unique(womens_team_features$team_id)

  mens_rows <- sample_submission |>
    filter(a_team_id %in% mens_team_ids, b_team_id %in% mens_team_ids)

  womens_rows <- sample_submission |>
    filter(a_team_id %in% womens_team_ids, b_team_id %in% womens_team_ids)

  mens_predictions <- if (division_enabled(options, "mens")) {
    score_sample_submission_division(
      sample_rows = mens_rows,
      team_features = mens_team_features,
      model_file = MODEL_FILES$mens,
      feature_file = FEATURE_FILES$mens,
      team_lookup = m_teams |> select(team_id, team_name),
      tournament_season = tournament_season,
      injury_options = options$model$injury_adjustments
    )
  } else {
    tibble(ID = character(), Pred = numeric())
  }

  womens_predictions <- if (division_enabled(options, "womens")) {
    score_sample_submission_division(
      sample_rows = womens_rows,
      team_features = womens_team_features,
      model_file = MODEL_FILES$womens,
      feature_file = FEATURE_FILES$womens,
      team_lookup = w_teams |> distinct(team_id, team_name),
      tournament_season = tournament_season,
      injury_options = NULL
    )
  } else {
    tibble(ID = character(), Pred = numeric())
  }

  submission_predictions <- bind_rows(mens_predictions, womens_predictions)

  sample_submission |>
    select(ID) |>
    left_join(submission_predictions, by = "ID") |>
    mutate(Pred = coalesce(Pred, 0.5))
}

build_submission_review_file <- function(submission, tournament_season) {
  team_lookup <- bind_rows(
    m_teams |>
      transmute(team_id = as.integer(team_id), team_name, division = "mens"),
    w_teams |>
      distinct(team_id, team_name) |>
      transmute(team_id = as.integer(team_id), team_name, division = "womens")
  )

  submission |>
    tidyr::separate(ID, into = c("season", "team_1_id", "team_2_id"), sep = "_", convert = TRUE, remove = FALSE) |>
    filter(season == tournament_season) |>
    left_join(
      team_lookup |>
        select(team_1_id = team_id, team_1_name = team_name, division),
      by = "team_1_id"
    ) |>
    left_join(
      team_lookup |>
        select(team_2_id = team_id, team_2_name = team_name),
      by = "team_2_id"
    ) |>
    relocate(division, .after = ID) |>
    relocate(team_1_id, team_1_name, .after = division) |>
    relocate(team_2_id, team_2_name, .after = team_1_name)
}

run_predict_matchups <- function(options = default_pipeline_options()) {
  options <- resolve_pipeline_options(options)
  tournament_season <- get_tournament_season(options)
  prediction_frames <- list()

  if (division_enabled(options, "mens")) {
    prediction_frames$mens <- score_division(
      seed_data = m_tourney_seeds,
      team_features = read_rds(MODEL_DATA_FILES$mens_tourney),
      model_file = MODEL_FILES$mens,
      feature_file = FEATURE_FILES$mens,
      team_lookup = m_teams |> select(team_id, team_name),
      output_file = PREDICTION_FILES$mens,
      tournament_season = tournament_season,
      tournament_results = m_tourney_compact_results,
      shrinkage_options = options$model$seed_matchup_shrinkage,
      injury_options = options$model$injury_adjustments
    )

    mens_seed_report <- build_seed_history_report(
      scored_matchups = prediction_frames$mens,
      seed_data = m_tourney_seeds,
      tournament_results = m_tourney_compact_results,
      tournament_season = tournament_season
    )

    if (nrow(mens_seed_report) > 0) {
      write_csv(mens_seed_report, PREDICTION_FILES$mens_seed_report)
    }
  }

  if (division_enabled(options, "womens")) {
    womens_team_lookup <- w_teams |>
      distinct(team_id, team_name)

    prediction_frames$womens <- score_division(
      seed_data = w_tourney_seeds,
      team_features = read_rds(MODEL_DATA_FILES$womens_tourney),
      model_file = MODEL_FILES$womens,
      feature_file = FEATURE_FILES$womens,
      team_lookup = womens_team_lookup,
      output_file = PREDICTION_FILES$womens,
      tournament_season = tournament_season,
      tournament_results = w_tourney_compact_results,
      shrinkage_options = options$model$seed_matchup_shrinkage,
      injury_options = NULL
    )

    womens_seed_report <- build_seed_history_report(
      scored_matchups = prediction_frames$womens,
      seed_data = w_tourney_seeds,
      tournament_results = w_tourney_compact_results,
      tournament_season = tournament_season
    )

    if (nrow(womens_seed_report) > 0) {
      write_csv(womens_seed_report, PREDICTION_FILES$womens_seed_report)
    }
  }

  if (length(prediction_frames) == 0) {
    stop("No divisions selected for matchup prediction.")
  }

  sample_submission_season <- get_sample_submission_season()

  if (!is.na(sample_submission_season) && sample_submission_season != tournament_season) {
    message(
      "Skipped Kaggle submission export because SampleSubmissionStage2.csv is for season ",
      sample_submission_season,
      " but pipeline_options$tournament_season is ",
      tournament_season,
      "."
    )

    return(invisible(prediction_frames))
  }

  submission <- build_sample_submission_predictions(options, tournament_season)
  dir.create(SUBMISSIONS_DIR, recursive = TRUE, showWarnings = FALSE)
  write_csv(submission, SUBMISSION_FILES$kaggle)
  write_csv(build_submission_review_file(submission, tournament_season), SUBMISSION_FILES$kaggle_review)

  invisible(prediction_frames)
}
