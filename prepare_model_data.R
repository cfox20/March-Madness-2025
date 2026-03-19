
# Load Packages -----------------------------------------------------------

library("tidyverse")
library("here")
library("janitor")
library("slider")

# Load Data ---------------------------------------------------------------

source("R/config.R")
source("R/pipeline_options.R")
source("R/xgb_utils.R")
source("read_data.R")

pipeline_options <- if (exists("pipeline_options", inherits = TRUE)) {
  resolve_pipeline_options(get("pipeline_options", inherits = TRUE))
} else {
  default_pipeline_options()
}

tournament_season <- get_tournament_season(pipeline_options)
mens_start_season <- pipeline_options$data$mens_start_season
womens_start_season <- pipeline_options$data$womens_start_season
train_fraction <- pipeline_options$data$train_fraction
validation_tournament_fraction <- pipeline_options$data$validation_tournament_fraction
max_rows_per_division <- pipeline_options$data$max_rows_per_division
tournament_game_weight <- pipeline_options$model$tournament_game_weight

add_tournament_team_indicators <- function(team_data, tournament_team_ids) {
  tournament_team_ids <- sort(unique(as.integer(tournament_team_ids)))

  if (length(tournament_team_ids) == 0) {
    return(team_data)
  }

  indicator_df <- map_dfc(
    tournament_team_ids,
    ~ tibble(!!paste0("team_id_", .x) := as.integer(team_data$team_id == .x))
  )

  bind_cols(team_data, indicator_df)
}

add_conference_indicators <- function(team_data, conference_col = "conf") {
  conference_col <- rlang::as_name(rlang::ensym(conference_col))

  conference_values <- team_data |>
    pull(all_of(conference_col)) |>
    unique() |>
    discard(is.na) |>
    sort()

  if (length(conference_values) == 0) {
    return(team_data)
  }

  indicator_df <- map_dfc(
    conference_values,
    ~ tibble(!!paste0("conf_", make_clean_names(.x)) := as.integer(team_data[[conference_col]] == .x))
  )

  bind_cols(team_data, indicator_df)
}

make_daily_side_features <- function(team_data, side) {
  team_id_col <- paste0(side, "_team_id")

  team_data |>
    select(-game_id) |>
    rename(!!team_id_col := team_id) |>
    rename_with(~ paste0(side, "_", .x), -all_of(c("season", "day_num", team_id_col)))
}

make_snapshot_side_features <- function(team_data, side) {
  team_id_col <- paste0(side, "_team_id")

  build_team_snapshot(team_data) |>
    select(-day_num, -game_id) |>
    rename(!!team_id_col := team_id) |>
    rename_with(~ paste0(side, "_", .x), -all_of(c("season", team_id_col)))
}

build_team_snapshot <- function(team_data) {
  team_data |>
    group_by(season, team_id) |>
    filter(day_num == max(day_num)) |>
    ungroup()
}

add_advanced_game_metrics <- function(team_games) {
  team_games |>
    mutate(
      score_margin = score - opp_score,
      score_margin_abs = abs(score_margin),
      to_margin = to - opp_to,
      fg_perc = fgm / fga,
      fg3_perc = fgm3 / fga3,
      efg_perc = (fgm + 0.5 * fgm3) / fga,
      opp_efg_perc = (opp_fgm + 0.5 * opp_fgm3) / opp_fga,
      ast_fgm_ratio = ast / fgm,
      poss = fga - or + to + (0.475 * fta),
      opp_poss = opp_fga - opp_or + opp_to + (0.475 * opp_fta),
      pts_per_poss = score / poss,
      opp_pts_per_poss = opp_score / opp_poss,
      off_rating = 100 * pts_per_poss,
      def_rating = 100 * opp_pts_per_poss,
      net_rating = off_rating - def_rating,
      true_shooting_perc = score / (2 * (fga + (0.475 * fta))),
      opp_true_shooting_perc = opp_score / (2 * (opp_fga + (0.475 * opp_fta))),
      free_throw_rate = fta / fga,
      opp_free_throw_rate = opp_fta / opp_fga,
      turnover_rate = to / poss,
      forced_turnover_rate = opp_to / opp_poss,
      offensive_rebound_rate = or / (or + opp_dr),
      defensive_rebound_rate = dr / (dr + opp_or),
      total_rebound_rate = (or + dr) / (or + dr + opp_or + opp_dr),
      three_point_attempt_rate = fga3 / fga,
      opp_three_point_attempt_rate = opp_fga3 / opp_fga,
      close_game = as.integer(score_margin_abs <= 5)
    )
}

compute_colley_ratings <- function(games, teams) {
  team_count <- length(teams)

  if (nrow(games) == 0 || team_count == 0) {
    return(tibble(team_id = teams, colley = 0.5))
  }

  team_index <- setNames(seq_along(teams), teams)
  colley_matrix <- matrix(0, nrow = team_count, ncol = team_count)
  wins <- setNames(numeric(team_count), teams)
  losses <- setNames(numeric(team_count), teams)

  for (i in seq_len(nrow(games))) {
    team_a <- as.character(games$team_a[i])
    team_b <- as.character(games$team_b[i])
    idx_a <- team_index[[team_a]]
    idx_b <- team_index[[team_b]]

    colley_matrix[idx_a, idx_a] <- colley_matrix[idx_a, idx_a] + 1
    colley_matrix[idx_b, idx_b] <- colley_matrix[idx_b, idx_b] + 1
    colley_matrix[idx_a, idx_b] <- colley_matrix[idx_a, idx_b] - 1
    colley_matrix[idx_b, idx_a] <- colley_matrix[idx_b, idx_a] - 1

    if (games$score_a[i] > games$score_b[i]) {
      wins[team_a] <- wins[team_a] + 1
      losses[team_b] <- losses[team_b] + 1
    } else {
      wins[team_b] <- wins[team_b] + 1
      losses[team_a] <- losses[team_a] + 1
    }
  }

  diag(colley_matrix) <- diag(colley_matrix) + 2
  b_vector <- 1 + ((wins - losses) / 2)
  ratings <- tryCatch(
    solve(colley_matrix, b_vector),
    error = function(...) {
      tryCatch(
        qr.solve(colley_matrix, b_vector),
        error = function(...) rep(0.5, team_count)
      )
    }
  )

  tibble(team_id = as.integer(teams), colley = as.numeric(ratings))
}

compute_massey_ratings <- function(games, teams) {
  team_count <- length(teams)

  if (nrow(games) == 0 || team_count == 0) {
    return(tibble(team_id = teams, massey = 0))
  }

  team_index <- setNames(seq_along(teams), teams)
  massey_matrix <- matrix(0, nrow = team_count, ncol = team_count)
  margin_vector <- setNames(numeric(team_count), teams)

  for (i in seq_len(nrow(games))) {
    team_a <- as.character(games$team_a[i])
    team_b <- as.character(games$team_b[i])
    idx_a <- team_index[[team_a]]
    idx_b <- team_index[[team_b]]
    margin <- games$score_a[i] - games$score_b[i]

    massey_matrix[idx_a, idx_a] <- massey_matrix[idx_a, idx_a] + 1
    massey_matrix[idx_b, idx_b] <- massey_matrix[idx_b, idx_b] + 1
    massey_matrix[idx_a, idx_b] <- massey_matrix[idx_a, idx_b] - 1
    massey_matrix[idx_b, idx_a] <- massey_matrix[idx_b, idx_a] - 1
    margin_vector[team_a] <- margin_vector[team_a] + margin
    margin_vector[team_b] <- margin_vector[team_b] - margin
  }

  massey_matrix[team_count, ] <- 1
  margin_vector[team_count] <- 0
  ratings <- tryCatch(
    solve(massey_matrix, margin_vector),
    error = function(...) {
      tryCatch(
        qr.solve(massey_matrix, margin_vector),
        error = function(...) rep(0, team_count)
      )
    }
  )

  tibble(team_id = as.integer(teams), massey = as.numeric(ratings))
}

compute_pagerank_ratings <- function(games, teams, damping = 0.85, iterations = 50) {
  team_count <- length(teams)

  if (nrow(games) == 0 || team_count == 0) {
    return(tibble(team_id = teams, pagerank = rep(ifelse(team_count > 0, 1 / team_count, 0), team_count)))
  }

  team_index <- setNames(seq_along(teams), teams)
  transition_matrix <- matrix(0, nrow = team_count, ncol = team_count)

  for (i in seq_len(nrow(games))) {
    if (games$score_a[i] == games$score_b[i]) {
      next
    }

    winner <- if (games$score_a[i] > games$score_b[i]) as.character(games$team_a[i]) else as.character(games$team_b[i])
    loser <- if (games$score_a[i] > games$score_b[i]) as.character(games$team_b[i]) else as.character(games$team_a[i])
    margin_weight <- abs(games$score_a[i] - games$score_b[i]) + 1
    transition_matrix[team_index[[loser]], team_index[[winner]]] <- transition_matrix[team_index[[loser]], team_index[[winner]]] + margin_weight
  }

  row_sums <- rowSums(transition_matrix)
  stochastic_matrix <- transition_matrix

  for (i in seq_len(team_count)) {
    if (row_sums[i] > 0) {
      stochastic_matrix[i, ] <- stochastic_matrix[i, ] / row_sums[i]
    } else {
      stochastic_matrix[i, ] <- rep(1 / team_count, team_count)
    }
  }

  ranks <- rep(1 / team_count, team_count)

  for (iteration in seq_len(iterations)) {
    ranks <- ((1 - damping) / team_count) + damping * as.numeric(t(stochastic_matrix) %*% ranks)
  }

  tibble(team_id = as.integer(teams), pagerank = ranks)
}

compute_adjusted_net_ratings <- function(games, teams) {
  if (nrow(games) == 0 || length(teams) == 0) {
    return(tibble(team_id = teams, adj_net_rating = 0))
  }

  team_history <- bind_rows(
    games |>
      transmute(team_id = team_a, opponent_id = team_b, net_rating = 100 * ((score_a - score_b) / pmax(1, (fga_a - or_a + to_a + (0.475 * fta_a))))),
    games |>
      transmute(team_id = team_b, opponent_id = team_a, net_rating = 100 * ((score_b - score_a) / pmax(1, (fga_b - or_b + to_b + (0.475 * fta_b)))))
  )

  raw_ratings <- team_history |>
    group_by(team_id) |>
    summarise(raw_net_rating = mean(net_rating, na.rm = TRUE), .groups = "drop")

  opponent_strength <- team_history |>
    left_join(raw_ratings, by = c("opponent_id" = "team_id")) |>
    group_by(team_id) |>
    summarise(avg_opponent_raw_net = mean(raw_net_rating, na.rm = TRUE), .groups = "drop")

  tibble(team_id = as.integer(teams)) |>
    left_join(raw_ratings, by = "team_id") |>
    left_join(opponent_strength, by = "team_id") |>
    mutate(
      raw_net_rating = replace_na(raw_net_rating, 0),
      avg_opponent_raw_net = replace_na(avg_opponent_raw_net, 0),
      adj_net_rating = raw_net_rating - avg_opponent_raw_net
    ) |>
    select(team_id, adj_net_rating)
}

compute_womens_daily_rankings <- function(reg_results, starting_elo = 1500, k_factor = 20) {
  season_games <- reg_results |>
    transmute(
      season,
      game_id,
      day_num,
      team_a = w_team_id,
      team_b = l_team_id,
      score_a = w_score,
      score_b = l_score,
      fga_a = wfga,
      fta_a = wfta,
      or_a = wor,
      to_a = wto,
      fga_b = lfga,
      fta_b = lfta,
      or_b = lor,
      to_b = lto
    ) |>
    arrange(season, day_num, game_id)

  if (nrow(season_games) == 0) {
    return(tibble(season = integer(), day_num = integer(), team_id = integer()))
  }

  map_dfr(sort(unique(season_games$season)), function(season_value) {
    season_schedule <- season_games |>
      filter(season == season_value)

    teams <- sort(unique(c(season_schedule$team_a, season_schedule$team_b)))
    day_values <- sort(unique(season_schedule$day_num))
    team_index <- setNames(seq_along(teams), teams)
    elo_ratings <- setNames(rep(starting_elo, length(teams)), teams)
    daily_snapshots <- vector("list", length(day_values))

    for (day_idx in seq_along(day_values)) {
      day_value <- day_values[day_idx]
      prior_games <- season_schedule |>
        filter(day_num < day_value)

      colley_ratings <- compute_colley_ratings(prior_games, teams)
      massey_ratings <- compute_massey_ratings(prior_games, teams)
      pagerank_ratings <- compute_pagerank_ratings(prior_games, teams)
      adjusted_net_ratings <- compute_adjusted_net_ratings(prior_games, teams)

      daily_snapshots[[day_idx]] <- tibble(
        season = season_value,
        day_num = day_value,
        team_id = as.integer(teams),
        elo = as.numeric(elo_ratings[as.character(teams)])
      ) |>
        left_join(colley_ratings, by = "team_id") |>
        left_join(massey_ratings, by = "team_id") |>
        left_join(pagerank_ratings, by = "team_id") |>
        left_join(adjusted_net_ratings, by = "team_id")

      day_games <- season_schedule |>
        filter(day_num == day_value)

      if (nrow(day_games) == 0) {
        next
      }

      for (game_idx in seq_len(nrow(day_games))) {
        team_a <- as.character(day_games$team_a[game_idx])
        team_b <- as.character(day_games$team_b[game_idx])
        expected_a <- 1 / (1 + 10 ^ ((elo_ratings[team_b] - elo_ratings[team_a]) / 400))
        actual_a <- ifelse(day_games$score_a[game_idx] > day_games$score_b[game_idx], 1, 0)
        elo_delta <- k_factor * (actual_a - expected_a)
        elo_ratings[team_a] <- elo_ratings[team_a] + elo_delta
        elo_ratings[team_b] <- elo_ratings[team_b] - elo_delta
      }
    }

    bind_rows(daily_snapshots)
  })
}

mens_uses_kenpom_features <- function(options) {
  enabled_groups <- enabled_feature_groups(options, "mens")$division
  any(c("pom_rank", "conference_indicators", "kenpom") %in% enabled_groups)
}

split_train_valid <- function(data, tournament_season, validation_tournament_fraction, seed, tournament_game_weight = 1) {
  target_season_rows <- data |>
    filter(season == tournament_season)

  historical_rows <- data |>
    filter(season != tournament_season)

  historical_tourney_seasons <- historical_rows |>
    filter(game_type == "tournament") |>
    distinct(season) |>
    arrange(season) |>
    pull(season)

  if (length(historical_tourney_seasons) == 0) {
    return(list(
      train = bind_rows(target_season_rows, historical_rows) |>
        mutate(sample_weight = if_else(game_type == "tournament", tournament_game_weight, 1)) |>
        select(-id, -game_type),
      valid = historical_rows[0, ] |>
        mutate(sample_weight = numeric()) |>
        select(-id, -game_type)
    ))
  }

  validation_tournament_fraction <- min(max(validation_tournament_fraction, 0), 1)
  valid_season_count <- max(1L, floor(length(historical_tourney_seasons) * validation_tournament_fraction))
  valid_season_count <- min(valid_season_count, length(historical_tourney_seasons))

  set.seed(seed)
  valid_tournament_seasons <- sample(historical_tourney_seasons, size = valid_season_count)

  valid <- historical_rows |>
    filter(game_type == "tournament", season %in% valid_tournament_seasons)

  train <- bind_rows(
    target_season_rows,
    historical_rows |>
      filter(game_type == "regular_season" | !season %in% valid_tournament_seasons)
  )

  list(
    train = train |>
      mutate(sample_weight = if_else(game_type == "tournament", tournament_game_weight, 1)) |>
      select(-id, -game_type),
    valid = valid |>
      mutate(sample_weight = if_else(game_type == "tournament", tournament_game_weight, 1)) |>
      select(-id, -game_type)
  )
}

# Calculate Season Data ---------------------------------------------------

if (division_enabled(pipeline_options, "mens")) {

  m_reg_results <- m_reg_detailed_results |>
    mutate(game_id = row_number()) |>
    relocate(game_id)

  w_team <- m_reg_results |>
    filter(season >= mens_start_season) |>
    select(-num_ot, -l_team_id, -w_loc) |>
    rename_with(~ str_replace(.x, "^w", "")) |>
    rename_with(~ str_replace(.x, "^l", "opp_")) |>
    rename_with(~ str_replace(.x, "^_", "")) |>
    rename_with(~ str_replace(.x, "__", "_")) |>
    mutate(win_pct = 1) |>
    relocate(win_pct, .after = day_num) |>
    relocate(team_id, .after = season)

  l_team <- m_reg_results |>
    filter(season >= mens_start_season) |>
    select(-num_ot, -w_team_id, -w_loc) |>
    rename_with(~ str_replace(.x, "^l", "")) |>
    rename_with(~ str_replace(.x, "^w", "opp_")) |>
    rename_with(~ str_replace(.x, "^_", "")) |>
    rename_with(~ str_replace(.x, "__", "_")) |>
    mutate(win_pct = 0) |>
    relocate(win_pct, .after = day_num) |>
    relocate(team_id, .after = season)

  m_all_team_games <- bind_rows(w_team, l_team)

  m_reg_season_ranks <- m_team_rankings |>
    filter(season >= mens_start_season) |>
    select(season, team_id, ranking_day_num, POM) |>
    rename(day_num = ranking_day_num) |>
    ungroup()

  m_all_ranks <- expand_grid(
    day_num = 1:155,
    distinct(m_reg_season_ranks, team_id),
    distinct(m_reg_season_ranks, season)
  ) |>
    left_join(m_reg_season_ranks, by = c("season", "team_id", "day_num")) |>
    arrange(season, team_id, day_num) |>
    group_by(season, team_id) |>
    filter(day_num > 10) |>
    fill(POM, .direction = "down") |>
    ungroup()

  m_team_cumulative_avg <- m_all_team_games |>
    filter(season >= mens_start_season) |>
    arrange(season, team_id, day_num) |>
    group_by(season, team_id) |>
    add_advanced_game_metrics() |>
    mutate(across(-c(game_id, day_num), ~ cummean(.x), .names = "c_avg_{.col}")) |>
    mutate(across(starts_with("c_avg_"), ~ lag(.x, default = 0))) |>
    ungroup() |>
    arrange(season, team_id, game_id) |>
    select(season, day_num, team_id, game_id, starts_with("c_avg")) |>
    group_by(season, team_id) |>
    mutate(game_num = row_number()) |>
    filter(game_num > 9) |>
    select(-game_num) |>
    ungroup() |>
    left_join(m_all_ranks, by = c("season", "team_id", "day_num")) |>
    rename(pom = POM)

  m_team_rolling_avg <- m_all_team_games |>
    filter(season >= mens_start_season) |>
    arrange(team_id, season, day_num) |>
    group_by(team_id, season) |>
    add_advanced_game_metrics() |>
    mutate(across(-c(game_id, day_num), ~ slide_dbl(.x, ~ mean(.x, na.rm = TRUE), .before = 2, .complete = FALSE))) |>
    rename_with(.fn = ~ paste0("r_avg_", .x), .cols = 5:last_col()) |>
    mutate(across(starts_with("r_avg_"), ~ lag(.x, default = 0))) |>
    mutate(game_num = row_number()) |>
    filter(game_num > 9) |>
    select(-game_num)

  m_team_cumulative_avg <- left_join(
    m_team_cumulative_avg,
    m_team_rolling_avg,
    by = c("game_id", "season", "team_id", "day_num")
  )

  if (mens_uses_kenpom_features(pipeline_options)) {
    ken_pom <- read_rds(file.path(INPUT_DIR, "ken_pom.rds")) |>
      add_conference_indicators(conf) |>
      select(-w_l, -conf)

    m_team_cumulative_avg <- left_join(
      filter(m_team_cumulative_avg, season >= mens_start_season),
      ken_pom,
      by = c("team_id", "season")
    )
  }

  mens_tourney_team_ids <- m_tourney_seeds |>
    filter(season == tournament_season) |>
    pull(team_id) |>
    as.integer()

  m_team_cumulative_avg <- add_tournament_team_indicators(m_team_cumulative_avg, mens_tourney_team_ids)
  m_team_seed_lookup <- m_tourney_seeds |>
    filter(season >= mens_start_season) |>
    transmute(season, team_id, seed = as.integer(seed))
  m_team_cumulative_avg <- m_team_cumulative_avg |>
    left_join(m_team_seed_lookup, by = c("season", "team_id"))

  m_daily_a_features <- make_daily_side_features(m_team_cumulative_avg, "a")
  m_daily_b_features <- make_daily_side_features(m_team_cumulative_avg, "b")
  m_snapshot_a_features <- make_snapshot_side_features(m_team_cumulative_avg, "a")
  m_snapshot_b_features <- make_snapshot_side_features(m_team_cumulative_avg, "b")

  m_reg_matchups <- m_reg_compact_results |>
    filter(season >= mens_start_season) |>
    select(season, day_num, w_team_id, l_team_id)

  reg_season_games <- m_reg_matchups |>
    mutate(w_team = sample(c("a", "b"), n(), replace = TRUE), w_team2 = w_team) |>
    pivot_wider(names_from = w_team2, values_from = w_team_id) |>
    mutate(
      a = ifelse(is.na(a), l_team_id, a),
      b = ifelse(is.na(b), l_team_id, b)
    ) |>
    rename(a_team_id = a, b_team_id = b) |>
    select(-l_team_id) |>
    inner_join(m_daily_a_features, by = c("season", "day_num", "a_team_id"), relationship = "many-to-many") |>
    inner_join(m_daily_b_features, by = c("season", "day_num", "b_team_id"), relationship = "many-to-many") |>
    mutate(game_type = "regular_season")

  m_tourney_matchups <- m_tourney_compact_results |>
    filter(season >= mens_start_season) |>
    select(season, day_num, w_team_id, l_team_id)

  m_tournament_games <- m_tourney_matchups |>
    mutate(w_team = sample(c("a", "b"), n(), replace = TRUE), w_team2 = w_team) |>
    pivot_wider(names_from = w_team2, values_from = w_team_id) |>
    mutate(
      a = ifelse(is.na(a), l_team_id, a),
      b = ifelse(is.na(b), l_team_id, b)
    ) |>
    rename(a_team_id = a, b_team_id = b) |>
    select(-l_team_id) |>
    left_join(m_snapshot_a_features, by = c("season", "a_team_id")) |>
    left_join(m_snapshot_b_features, by = c("season", "b_team_id")) |>
    mutate(game_type = "tournament")

  m_all_games <- bind_rows(reg_season_games, m_tournament_games)
  m_all_games <- m_all_games |>
    mutate(seed_diff = a_seed - b_seed)

  tb <- m_all_games |>
    mutate(id = row_number())
  tb <- limit_rows(tb, max_rows_per_division, pipeline_options$seed)

  split_data <- split_train_valid(tb, tournament_season, validation_tournament_fraction, pipeline_options$seed, tournament_game_weight)
  train <- split_data$train
  valid <- split_data$valid

  write_rds(
    list(train_data = train, valid_data = valid),
    MODEL_DATA_FILES$mens_train
  )

  mens_feature_names <- get_mens_training_features(train)

  m_tourney_team_data <- build_team_snapshot(m_team_cumulative_avg) |>
    filter(season == tournament_season, team_id %in% mens_tourney_team_ids) |>
    select(-day_num) |>
    select(team_id, any_of(str_remove(mens_feature_names[startsWith(mens_feature_names, "a_")], "a_")))

  write_rds(m_tourney_team_data, MODEL_DATA_FILES$mens_tourney)
}

if (division_enabled(pipeline_options, "womens")) {

  w_reg_results <- w_reg_detailed_results |>
    mutate(game_id = row_number()) |>
    relocate(game_id)

  w_w_team <- w_reg_results |>
    filter(season >= womens_start_season) |>
    select(-num_ot, -l_team_id, -w_loc) |>
    rename_with(~ str_replace(.x, "^w", "")) |>
    rename_with(~ str_replace(.x, "^l", "opp_")) |>
    rename_with(~ str_replace(.x, "^_", "")) |>
    rename_with(~ str_replace(.x, "__", "_")) |>
    mutate(win_pct = 1) |>
    relocate(win_pct, .after = day_num) |>
    relocate(team_id, .after = season)

  w_l_team <- w_reg_results |>
    filter(season >= womens_start_season) |>
    select(-num_ot, -w_team_id, -w_loc) |>
    rename_with(~ str_replace(.x, "^l", "")) |>
    rename_with(~ str_replace(.x, "^w", "opp_")) |>
    rename_with(~ str_replace(.x, "^_", "")) |>
    rename_with(~ str_replace(.x, "__", "_")) |>
    mutate(win_pct = 0) |>
    relocate(win_pct, .after = day_num) |>
    relocate(team_id, .after = season)

  w_all_team_games <- bind_rows(w_w_team, w_l_team)
  w_all_team_games <- add_advanced_game_metrics(w_all_team_games)

  w_daily_rankings <- compute_womens_daily_rankings(w_reg_results)

  w_team_cumulative_avg <- w_all_team_games |>
    filter(season >= womens_start_season) |>
    arrange(season, team_id, day_num) |>
    group_by(season, team_id) |>
    mutate(across(-c(game_id, day_num), ~ cummean(.x), .names = "c_avg_{.col}")) |>
    mutate(across(starts_with("c_avg_"), ~ lag(.x, default = 0))) |>
    ungroup() |>
    arrange(season, team_id, game_id) |>
    select(season, day_num, team_id, game_id, starts_with("c_avg")) |>
    group_by(season, team_id) |>
    mutate(game_num = row_number()) |>
    filter(game_num > 5) |>
    select(-game_num) |>
    ungroup()

  w_team_rolling_avg <- w_all_team_games |>
    filter(season >= womens_start_season) |>
    arrange(team_id, season, day_num) |>
    group_by(team_id, season) |>
    mutate(across(-c(game_id, day_num), ~ slide_dbl(.x, ~ mean(.x, na.rm = TRUE), .before = 2, .complete = FALSE))) |>
    rename_with(.fn = ~ paste0("r_avg_", .x), .cols = 5:last_col()) |>
    mutate(across(starts_with("r_avg_"), ~ lag(.x, default = 0))) |>
    mutate(game_num = row_number()) |>
    filter(game_num > 5) |>
    select(-game_num)

  w_team_cumulative_avg <- left_join(
    w_team_cumulative_avg,
    w_team_rolling_avg,
    by = c("game_id", "season", "team_id", "day_num")
  )
  w_team_cumulative_avg <- w_team_cumulative_avg |>
    left_join(w_daily_rankings, by = c("season", "team_id", "day_num"))

  womens_tourney_team_ids <- w_tourney_seeds |>
    filter(season == tournament_season) |>
    pull(team_id) |>
    as.integer()

  w_team_cumulative_avg <- add_tournament_team_indicators(w_team_cumulative_avg, womens_tourney_team_ids)
  w_team_seed_lookup <- w_tourney_seeds |>
    filter(season >= womens_start_season) |>
    transmute(season, team_id, seed = as.integer(seed))
  w_team_cumulative_avg <- w_team_cumulative_avg |>
    left_join(w_team_seed_lookup, by = c("season", "team_id"))

  w_daily_a_features <- make_daily_side_features(w_team_cumulative_avg, "a")
  w_daily_b_features <- make_daily_side_features(w_team_cumulative_avg, "b")
  w_snapshot_a_features <- make_snapshot_side_features(w_team_cumulative_avg, "a")
  w_snapshot_b_features <- make_snapshot_side_features(w_team_cumulative_avg, "b")

  w_reg_matchups <- w_reg_compact_results |>
    filter(season >= womens_start_season) |>
    select(season, day_num, w_team_id, l_team_id)

  w_reg_season_games <- w_reg_matchups |>
    mutate(w_team = sample(c("a", "b"), n(), replace = TRUE), w_team2 = w_team) |>
    pivot_wider(names_from = w_team2, values_from = w_team_id) |>
    mutate(
      a = ifelse(is.na(a), l_team_id, a),
      b = ifelse(is.na(b), l_team_id, b)
    ) |>
    rename(a_team_id = a, b_team_id = b) |>
    select(-l_team_id) |>
    inner_join(w_daily_a_features, by = c("season", "day_num", "a_team_id"), relationship = "many-to-many") |>
    inner_join(w_daily_b_features, by = c("season", "day_num", "b_team_id"), relationship = "many-to-many") |>
    mutate(game_type = "regular_season")

  w_tourney_matchups <- w_tourney_compact_results |>
    filter(season >= womens_start_season) |>
    select(season, day_num, w_team_id, l_team_id)

  w_tournament_games <- w_tourney_matchups |>
    mutate(w_team = sample(c("a", "b"), n(), replace = TRUE), w_team2 = w_team) |>
    pivot_wider(names_from = w_team2, values_from = w_team_id) |>
    mutate(
      a = ifelse(is.na(a), l_team_id, a),
      b = ifelse(is.na(b), l_team_id, b)
    ) |>
    rename(a_team_id = a, b_team_id = b) |>
    select(-l_team_id) |>
    left_join(w_snapshot_a_features, by = c("season", "a_team_id")) |>
    left_join(w_snapshot_b_features, by = c("season", "b_team_id")) |>
    mutate(game_type = "tournament")

  w_all_games <- bind_rows(w_reg_season_games, w_tournament_games)
  w_all_games <- w_all_games |>
    mutate(
      seed_diff = a_seed - b_seed,
      elo_diff = a_elo - b_elo,
      massey_diff = a_massey - b_massey,
      colley_diff = a_colley - b_colley,
      pagerank_diff = a_pagerank - b_pagerank,
      adj_net_rating_diff = a_adj_net_rating - b_adj_net_rating
    )

  tb <- w_all_games |>
    mutate(id = row_number())
  tb <- limit_rows(tb, max_rows_per_division, pipeline_options$seed)

  split_data <- split_train_valid(tb, tournament_season, validation_tournament_fraction, pipeline_options$seed, tournament_game_weight)
  train <- split_data$train
  valid <- split_data$valid

  write_rds(
    list(train_data = train, valid_data = valid),
    MODEL_DATA_FILES$womens_train
  )

  womens_feature_names <- get_womens_training_features(train)

  w_tourney_team_data <- build_team_snapshot(w_team_cumulative_avg) |>
    filter(season == tournament_season, team_id %in% womens_tourney_team_ids) |>
    select(-day_num) |>
    select(team_id, any_of(str_remove(womens_feature_names[startsWith(womens_feature_names, "a_")], "a_")))

  write_rds(w_tourney_team_data, MODEL_DATA_FILES$womens_tourney)
}
