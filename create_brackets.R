library("furrr")
library("readr")
library("tidyverse")

source("R/config.R")
source("R/pipeline_options.R")

load_seed_data <- function(tournament_code) {
  seed_file <- if (tournament_code == "M") {
    file.path(INPUT_DIR, "MNCAATourneySeeds.csv")
  } else {
    file.path(INPUT_DIR, "WNCAATourneySeeds.csv")
  }

  read_csv(seed_file, show_col_types = FALSE) |>
    janitor::clean_names()
}

build_bracket_seed_table <- function(seed_data, tournament_code, tournament_season) {
  seed_data |>
    filter(season == tournament_season) |>
    mutate(
      region = str_extract(seed, "[A-Za-z]+"),
      seed = str_extract(seed, "\\d+") |> as.integer(),
      seed_label = str_c(region, str_pad(seed, width = 2, pad = "0")),
      Tournament = tournament_code
    ) |>
    select(Tournament, team_id, region, seed, seed_label)
}

build_bracket_probability_lookup <- function(predictions) {
  predictions |>
    transmute(
      matchup_id = str_c(pmin(a_team_id, b_team_id), pmax(a_team_id, b_team_id), sep = "_"),
      a_team_id = pmin(a_team_id, b_team_id),
      b_team_id = pmax(a_team_id, b_team_id),
      prob_a_wins = if_else(a_team_id <= b_team_id, xgb_predicted, 1 - xgb_predicted)
    ) |>
    distinct(matchup_id, .keep_all = TRUE)
}

build_round_one_games <- function(seed_table) {
  seed_table |>
    mutate(
      best_seed = if_else(seed <= 8, seed, 17 - seed),
      Slot = str_c("R1", region, best_seed)
    ) |>
    arrange(Slot, seed) |>
    group_by(Slot) |>
    summarise(
      team_1 = first(team_id),
      team_2 = nth(team_id, 2),
      .groups = "drop"
    )
}

advance_bracket_round <- function(round_results, slot_prefix, next_prefix, probability_lookup, seed_table) {
  round_results |>
    mutate(
      region = str_extract(Slot, paste0("(?<=", slot_prefix, ")\\w")),
      best_seed = as.integer(str_extract(Slot, "\\d$")),
      best_seed = case_when(
        best_seed <= 4 & next_prefix == "R2" ~ best_seed,
        best_seed > 4 & next_prefix == "R2" ~ 9 - best_seed,
        best_seed <= 2 & next_prefix == "R3" ~ best_seed,
        best_seed > 2 & next_prefix == "R3" ~ 5 - best_seed,
        best_seed <= 1 & next_prefix == "R4" ~ best_seed,
        best_seed > 1 & next_prefix == "R4" ~ 3 - best_seed,
        TRUE ~ 1L
      ),
      next_slot = str_c(next_prefix, region, best_seed)
    ) |>
    group_by(next_slot) |>
    summarise(team_1 = first(team_id), team_2 = nth(team_id, 2), .groups = "drop") |>
    rename(Slot = next_slot) |>
    play_bracket_round(probability_lookup, seed_table)
}

play_bracket_round <- function(games, probability_lookup, seed_table) {
  games |>
    mutate(
      matchup_id = str_c(pmin(team_1, team_2), pmax(team_1, team_2), sep = "_"),
      low_team_id = pmin(team_1, team_2),
      high_team_id = pmax(team_1, team_2)
    ) |>
    left_join(select(probability_lookup, matchup_id, prob_a_wins), by = "matchup_id") |>
    mutate(
      prob_team_1_wins = if_else(team_1 == low_team_id, prob_a_wins, 1 - prob_a_wins),
      winner = if_else(runif(n()) < prob_team_1_wins, team_1, team_2)
    ) |>
    left_join(select(seed_table, team_id, seed_label), by = c("winner" = "team_id")) |>
    transmute(Slot, Team = seed_label, team_id = winner)
}

play_bracket_final_four <- function(round_results, probability_lookup, seed_table, tournament_code) {
  semifinal_slots <- if (tournament_code == "M") c("R5WX", "R5WX", "R5YZ", "R5YZ") else c("R5WX", "R5WX", "R5YZ", "R5YZ")

  semifinals <- round_results |>
    mutate(Slot = semifinal_slots) |>
    group_by(Slot) |>
    summarise(team_1 = first(team_id), team_2 = nth(team_id, 2), .groups = "drop") |>
    play_bracket_round(probability_lookup, seed_table)

  championship <- semifinals |>
    mutate(Slot = "R6CH") |>
    group_by(Slot) |>
    summarise(team_1 = first(team_id), team_2 = nth(team_id, 2), .groups = "drop") |>
    play_bracket_round(probability_lookup, seed_table)

  bind_rows(semifinals, championship)
}

simulate_bracket <- function(seed_table, probability_lookup, tournament_code, bracket_number) {
  r1 <- build_round_one_games(seed_table) |> play_bracket_round(probability_lookup, seed_table)
  r2 <- advance_bracket_round(r1, "R1", "R2", probability_lookup, seed_table)
  r3 <- advance_bracket_round(r2, "R2", "R3", probability_lookup, seed_table)
  r4 <- advance_bracket_round(r3, "R3", "R4", probability_lookup, seed_table)
  r5_r6 <- play_bracket_final_four(r4, probability_lookup, seed_table, tournament_code)

  bind_rows(r1, r2, r3, r4, r5_r6) |>
    mutate(Tournament = tournament_code, Bracket = bracket_number) |>
    select(Tournament, Bracket, Slot, Team, team_id)
}

summarize_bracket_round_probabilities <- function(bracket_pool, seed_table) {
  round_lookup <- tibble(
    slot_prefix = c("R1", "R2", "R3", "R4", "R5", "R6"),
    round_name = c("Round of 64", "Round of 32", "Sweet 16", "Elite Eight", "Final Four", "Champion"),
    round_order = c(1L, 2L, 3L, 4L, 5L, 6L)
  )

  bracket_pool |>
    mutate(slot_prefix = str_sub(Slot, 1, 2)) |>
    left_join(round_lookup, by = "slot_prefix") |>
    count(Tournament, team_id, Team, round_name, round_order, name = "appearances") |>
    left_join(seed_table |> distinct(Tournament, team_id, region), by = c("Tournament", "team_id")) |>
    group_by(Tournament) |>
    mutate(
      total_brackets = n_distinct(bracket_pool$Bracket[bracket_pool$Tournament == first(Tournament)]),
      round_probability = appearances / total_brackets
    ) |>
    ungroup() |>
    select(Tournament, team_id, Team, region, round_name, round_order, appearances, total_brackets, round_probability)
}

run_bracket_batch <- function(n_brackets, workers, seed_table, probability_lookup, tournament_code) {
  simulate_one <- function(bracket_number) {
    simulate_bracket(seed_table, probability_lookup, tournament_code, bracket_number)
  }

  if (workers <= 1) {
    return(purrr::map_dfr(seq_len(n_brackets), simulate_one))
  }

  plan(multisession, workers = workers)
  on.exit(plan(sequential), add = TRUE)

  tryCatch(
    future_map_dfr(
      seq_len(n_brackets),
      simulate_one,
      .options = furrr_options(seed = TRUE)
    ),
    error = function(err) {
      if (grepl("future.globals.maxSize|getGlobalsAndPackages", conditionMessage(err))) {
        warning(
          "Parallel bracket generation exceeded future globals limits; falling back to sequential execution.",
          call. = FALSE
        )
        return(purrr::map_dfr(seq_len(n_brackets), simulate_one))
      }

      stop(err)
    }
  )
}

run_create_brackets <- function(options = default_pipeline_options()) {
  options <- resolve_pipeline_options(options)
  tournament_season <- get_tournament_season(options)
  pool_n_brackets <- options$brackets$pool_n_brackets
  simulation_n_brackets <- options$brackets$simulation_n_brackets
  workers <- options$brackets$workers
  if (is.null(workers)) {
    workers <- default_bracket_workers()
  }
  bracket_pool_results <- list()
  bracket_simulation_results <- list()

  set.seed(options$seed)

  if (division_enabled(options, "mens")) {
    mens_seeds <- build_bracket_seed_table(load_seed_data("M"), "M", tournament_season)
    mens_probs <- build_bracket_probability_lookup(read_rds(PREDICTION_FILES$mens))

    bracket_pool_results$mens <- run_bracket_batch(
      n_brackets = pool_n_brackets,
      workers = workers,
      seed_table = mens_seeds,
      probability_lookup = mens_probs,
      tournament_code = "M"
    )

    bracket_simulation_results$mens <- if (identical(simulation_n_brackets, pool_n_brackets)) {
      bracket_pool_results$mens
    } else {
      run_bracket_batch(
        n_brackets = simulation_n_brackets,
        workers = workers,
        seed_table = mens_seeds,
        probability_lookup = mens_probs,
        tournament_code = "M"
      )
    }
  }

  if (division_enabled(options, "womens")) {
    womens_seeds <- build_bracket_seed_table(load_seed_data("W"), "W", tournament_season)
    womens_probs <- build_bracket_probability_lookup(read_rds(PREDICTION_FILES$womens))

    bracket_pool_results$womens <- run_bracket_batch(
      n_brackets = pool_n_brackets,
      workers = workers,
      seed_table = womens_seeds,
      probability_lookup = womens_probs,
      tournament_code = "W"
    )

    bracket_simulation_results$womens <- if (identical(simulation_n_brackets, pool_n_brackets)) {
      bracket_pool_results$womens
    } else {
      run_bracket_batch(
        n_brackets = simulation_n_brackets,
        workers = workers,
        seed_table = womens_seeds,
        probability_lookup = womens_probs,
        tournament_code = "W"
      )
    }
  }

  bracket_pool <- bind_rows(bracket_pool_results) |>
    mutate(RowId = row_number() - 1) |>
    relocate(RowId)

  round_summaries <- bind_rows(
    if (division_enabled(options, "mens")) {
      summarize_bracket_round_probabilities(
        bracket_pool = bracket_simulation_results$mens,
        seed_table = build_bracket_seed_table(load_seed_data("M"), "M", tournament_season)
      )
    },
    if (division_enabled(options, "womens")) {
      summarize_bracket_round_probabilities(
        bracket_pool = bracket_simulation_results$womens,
        seed_table = build_bracket_seed_table(load_seed_data("W"), "W", tournament_season)
      )
    }
  )

  write_csv(bracket_pool, get_bracket_output_file(options))
  write_csv(round_summaries, get_bracket_round_summary_file(options))

  invisible(list(pool = bracket_pool_results, simulation = bracket_simulation_results))
}
