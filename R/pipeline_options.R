default_feature_group_settings <- function() {
  list(
    common = c("season", "team_ids", "cumulative_core", "rolling_core", "tournament_seeds"),
    mens = character(),
    womens = c("derived_rankings", "ranking_diffs")
  )
}

default_pipeline_options <- function() {
  list(
    divisions = c("mens", "womens"),
    tournament_season = TARGET_SEASON,
    stages = list(
      prepare_data = TRUE,
      train_models = TRUE,
      predict_matchups = TRUE,
      create_brackets = TRUE
    ),
    seed = 123,
    data = list(
      mens_start_season = 2012,
      womens_start_season = 2010,
      train_fraction = 0.9,
      validation_tournament_fraction = 0.25,
      max_rows_per_division = NULL
    ),
    model = list(
      params = list(
        objective = "binary:logistic",
        eval_metric = "logloss",
        eta = 0.03,
        max_depth = 6,
        min_child_weight = 5,
        subsample = 0.8,
        colsample_bytree = 0.8,
        lambda = 0.5,
        alpha = 0,
        gamma = 0
      ),
      tune_regularization = FALSE,
      regularization_grid = list(
        lambda = c(1, 2),
        alpha = c(0, 0.1),
        gamma = c(0, 0.1)
      ),
      nrounds = 3000,
      early_stopping_rounds = 100,
      max_train_rows = NULL,
      max_valid_rows = NULL,
      tournament_game_weight = 2,
      tournament_oversample_factor = 5,
      calibration = list(
        method = "none"
      ),
      seed_matchup_shrinkage = list(
        matchups = character(),
        strength = 0
      ),
      injury_adjustments = list(
        teams = list()
      )
    ),
    brackets = list(
      pool_n_brackets = 100,
      simulation_n_brackets = 25000,
      workers = NULL
    ),
    features = default_feature_group_settings()
  )
}

make_test_pipeline_options <- function(divisions = c("mens", "womens")) {
  options <- default_pipeline_options()
  options$divisions <- divisions
  options$data$mens_start_season <- max(2012, options$tournament_season - 3L)
  options$data$womens_start_season <- max(2010, options$tournament_season - 3L)
  options$data$max_rows_per_division <- 5000
  options$data$validation_tournament_fraction <- 0.5
  options$model$tune_regularization <- FALSE
  options$model$nrounds <- 100
  options$model$early_stopping_rounds <- 20
  options$model$max_train_rows <- 4000
  options$model$max_valid_rows <- 1000
  options$brackets$pool_n_brackets <- 20
  options$brackets$simulation_n_brackets <- 100
  options$brackets$workers <- 1
  options
}

resolve_pipeline_options <- function(options = NULL) {
  defaults <- default_pipeline_options()

  if (is.null(options)) {
    return(defaults)
  }

  utils::modifyList(defaults, options, keep.null = TRUE)
}

load_pipeline_options <- function(config_path = "pipeline_run_config.yml") {
  options <- default_pipeline_options()

  if (!file.exists(config_path)) {
    return(options)
  }

  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required to read pipeline_run_config.yml. Run source('setup_packages.R') first.")
  }

  config_options <- yaml::read_yaml(config_path)

  if (is.null(config_options)) {
    return(options)
  }

  if (!is.list(config_options)) {
    stop("pipeline_run_config.yml must parse to a named list.")
  }

  if (!is.null(config_options$profile) && identical(config_options$profile, "test")) {
    divisions <- config_options$divisions
    if (is.null(divisions)) {
      divisions <- options$divisions
    }
    options <- make_test_pipeline_options(divisions = divisions)
    config_options$profile <- NULL
  }

  resolve_pipeline_options(utils::modifyList(options, config_options, keep.null = TRUE))
}

division_enabled <- function(options, division) {
  division %in% options$divisions
}

limit_rows <- function(data, max_rows, seed) {
  if (is.null(max_rows) || nrow(data) <= max_rows) {
    return(data)
  }

  set.seed(seed)
  dplyr::slice_sample(data, n = max_rows)
}

default_bracket_workers <- function() {
  max(1, future::availableCores() - 2)
}

feature_group_patterns <- function() {
  shared_box_stats <- paste(
    c(
      "win_pct", "score", "opp_score", "fgm", "fga", "fgm3", "fga3", "ftm", "fta",
      "or", "dr", "ast", "to", "stl", "blk", "pf",
      "opp_fgm", "opp_fga", "opp_fgm3", "opp_fga3", "opp_ftm", "opp_fta",
      "opp_or", "opp_dr", "opp_ast", "opp_to", "opp_stl", "opp_blk", "opp_pf",
      "score_margin", "score_margin_abs", "to_margin", "fg_perc", "fg3_perc",
      "efg_perc", "opp_efg_perc", "ast_fgm_ratio",
      "poss", "pts_per_poss", "opp_poss", "opp_pts_per_poss",
      "off_rating", "def_rating", "net_rating",
      "true_shooting_perc", "opp_true_shooting_perc",
      "free_throw_rate", "opp_free_throw_rate",
      "turnover_rate", "forced_turnover_rate",
      "offensive_rebound_rate", "defensive_rebound_rate", "total_rebound_rate",
      "three_point_attempt_rate", "opp_three_point_attempt_rate",
      "close_game"
    ),
    collapse = "|"
  )

  list(
    common = list(
      season = c("^season$"),
      team_ids = c("^a_team_id_", "^b_team_id_"),
      cumulative_core = c(paste0("^([ab]_c_avg_(", shared_box_stats, "))$")),
      rolling_core = c(paste0("^([ab]_r_avg_(", shared_box_stats, "))$")),
      tournament_seeds = c("^seed_diff$")
    ),
    mens = list(
      pom_rank = c("^([ab]_pom)$"),
      conference_indicators = c("^([ab]_conf_)"),
      kenpom = c("^([ab]_(adj_em|adj_o|adj_d|adj_t|luck|sos_adj_em|sos_opp_o|sos_opp_d|ncsos_adj_em))$")
    ),
    womens = list(
      derived_rankings = c("^([ab]_(elo|massey|colley|pagerank|adj_net_rating))$"),
      ranking_diffs = c("^(elo|massey|colley|pagerank|adj_net_rating)_diff$")
    )
  )
}

enabled_feature_groups <- function(options, division) {
  options <- resolve_pipeline_options(options)
  feature_settings <- options$features

  common_groups <- feature_settings$common
  division_groups <- feature_settings[[division]]

  if (is.null(common_groups)) {
    common_groups <- character()
  }

  if (is.null(division_groups)) {
    division_groups <- character()
  }

  if (is.list(common_groups)) {
    common_groups <- unlist(common_groups, use.names = FALSE)
  }

  if (is.list(division_groups)) {
    division_groups <- unlist(division_groups, use.names = FALSE)
  }

  common_groups <- as.character(common_groups)
  division_groups <- as.character(division_groups)

  list(common = common_groups, division = division_groups)
}

resolve_feature_columns <- function(data, options, division, label_name = NULL) {
  pattern_catalog <- feature_group_patterns()
  enabled_groups <- enabled_feature_groups(options, division)

  selected_patterns <- c(
    unlist(pattern_catalog$common[enabled_groups$common], use.names = FALSE),
    unlist(pattern_catalog[[division]][enabled_groups$division], use.names = FALSE)
  )

  selected_columns <- character()

  for (pattern in selected_patterns) {
    selected_columns <- c(selected_columns, grep(pattern, names(data), value = TRUE))
  }

  selected_columns <- unique(selected_columns)

  if (!is.null(label_name) && label_name %in% names(data)) {
    selected_columns <- c(label_name, selected_columns)
  }

  intersect(names(data), selected_columns)
}
