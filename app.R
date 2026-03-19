library("dplyr")
library("grid")
library("readr")
library("shiny")
library("stringr")
library("tidyr")

source("R/config.R")

load_predictions <- function(path, division_label) {
  if (!file.exists(path)) {
    return(tibble())
  }

  read_rds(path) |>
    mutate(
      division = division_label,
      season = as.integer(str_extract(ID, "^\\d+"))
    )
}

load_bracket_pool <- function(season) {
  bracket_file <- get_bracket_output_file(list(tournament_season = season))
  if (!file.exists(bracket_file)) {
    return(tibble())
  }

  read_csv(bracket_file, show_col_types = FALSE)
}

load_round_probabilities <- function(season) {
  summary_file <- get_bracket_round_summary_file(list(tournament_season = season))
  if (!file.exists(summary_file)) {
    return(tibble())
  }

  read_csv(summary_file, show_col_types = FALSE)
}

load_seed_data <- function(division_code) {
  seed_file <- if (division_code == "mens") {
    file.path(INPUT_DIR, "MNCAATourneySeeds.csv")
  } else {
    file.path(INPUT_DIR, "WNCAATourneySeeds.csv")
  }

  read_csv(seed_file, show_col_types = FALSE) |>
    transmute(
      season = Season,
      team_id = TeamID,
      seed_raw = Seed,
      region = str_extract(Seed, "[A-Za-z]+"),
      seed_num = as.integer(str_extract(Seed, "\\d+"))
    )
}

load_team_lookup <- function(division_code) {
  team_file <- if (division_code == "mens") {
    file.path(INPUT_DIR, "MTeams.csv")
  } else {
    file.path(INPUT_DIR, "WTeams.csv")
  }

  read_csv(team_file, show_col_types = FALSE) |>
    transmute(team_id = TeamID, team_name = TeamName)
}

load_region_lookup <- function(division_code) {
  season_file <- if (division_code == "mens") {
    file.path(INPUT_DIR, "MSeasons.csv")
  } else {
    file.path(INPUT_DIR, "WSeasons.csv")
  }

  read_csv(season_file, show_col_types = FALSE) |>
    transmute(
      season = Season,
      W = RegionW,
      X = RegionX,
      Y = RegionY,
      Z = RegionZ
    ) |>
    pivot_longer(cols = c(W, X, Y, Z), names_to = "region", values_to = "region_name")
}

load_tournament_slots <- function(division_code) {
  slot_file <- if (division_code == "mens") {
    file.path(INPUT_DIR, "MNCAATourneySlots.csv")
  } else {
    file.path(INPUT_DIR, "WNCAATourneySlots.csv")
  }

  read_csv(slot_file, show_col_types = FALSE) |>
    transmute(
      season = Season,
      Slot,
      StrongSeed,
      WeakSeed
    )
}

load_regular_season_detailed_results <- function(division_code) {
  results_file <- if (division_code == "mens") {
    file.path(INPUT_DIR, "MRegularSeasonDetailedResults.csv")
  } else {
    file.path(INPUT_DIR, "WRegularSeasonDetailedResults.csv")
  }

  read_csv(results_file, show_col_types = FALSE)
}

build_team_game_log <- function(results, team_lookup, season) {
  wins <- results |>
    filter(Season == !!season) |>
    transmute(
      season = Season,
      day_num = DayNum,
      team_id = WTeamID,
      opponent_id = LTeamID,
      location = WLoc,
      outcome = "Win",
      team_score = WScore,
      opponent_score = LScore,
      margin = WScore - LScore,
      fgm = WFGM,
      fga = WFGA,
      fg3m = WFGM3,
      fg3a = WFGA3,
      ftm = WFTM,
      fta = WFTA,
      or = WOR,
      dr = WDR,
      ast = WAst,
      tov = WTO,
      stl = WStl,
      blk = WBlk,
      pf = WPF,
      opp_fgm = LFGM,
      opp_fga = LFGA,
      opp_fg3m = LFGM3,
      opp_fg3a = LFGA3,
      opp_ftm = LFTM,
      opp_fta = LFTA,
      opp_or = LOR,
      opp_dr = LDR,
      opp_ast = LAst,
      opp_tov = LTO,
      opp_stl = LStl,
      opp_blk = LBlk,
      opp_pf = LPF
    )

  losses <- results |>
    filter(Season == !!season) |>
    transmute(
      season = Season,
      day_num = DayNum,
      team_id = LTeamID,
      opponent_id = WTeamID,
      location = case_when(
        WLoc == "H" ~ "A",
        WLoc == "A" ~ "H",
        TRUE ~ "N"
      ),
      outcome = "Loss",
      team_score = LScore,
      opponent_score = WScore,
      margin = LScore - WScore,
      fgm = LFGM,
      fga = LFGA,
      fg3m = LFGM3,
      fg3a = LFGA3,
      ftm = LFTM,
      fta = LFTA,
      or = LOR,
      dr = LDR,
      ast = LAst,
      tov = LTO,
      stl = LStl,
      blk = LBlk,
      pf = LPF,
      opp_fgm = WFGM,
      opp_fga = WFGA,
      opp_fg3m = WFGM3,
      opp_fg3a = WFGA3,
      opp_ftm = WFTM,
      opp_fta = WFTA,
      opp_or = WOR,
      opp_dr = WDR,
      opp_ast = WAst,
      opp_tov = WTO,
      opp_stl = WStl,
      opp_blk = WBlk,
      opp_pf = WPF
    )

  bind_rows(wins, losses) |>
    left_join(team_lookup |> rename(team_name = team_name), by = "team_id") |>
    left_join(team_lookup |> rename(opponent_name = team_name), by = c("opponent_id" = "team_id")) |>
    mutate(
      total_rebounds = or + dr,
      opp_total_rebounds = opp_or + opp_dr,
      fg_pct = if_else(fga > 0, fgm / fga, NA_real_),
      fg3_pct = if_else(fg3a > 0, fg3m / fg3a, NA_real_),
      ft_pct = if_else(fta > 0, ftm / fta, NA_real_),
      efg_pct = if_else(fga > 0, (fgm + 0.5 * fg3m) / fga, NA_real_),
      poss = 0.5 * ((fga + 0.4 * fta - 1.07 * (or / pmax(or + opp_dr, 1)) * (fga - fgm) + tov) +
        (opp_fga + 0.4 * opp_fta - 1.07 * (opp_or / pmax(opp_or + dr, 1)) * (opp_fga - opp_fgm) + opp_tov)),
      off_rating = if_else(poss > 0, 100 * team_score / poss, NA_real_),
      def_rating = if_else(poss > 0, 100 * opponent_score / poss, NA_real_)
    )
}

compute_percentile <- function(x, higher_is_better = TRUE) {
  if (length(x) <= 1) {
    return(rep(1, length(x)))
  }

  pct <- dplyr::percent_rank(x)
  pct[is.na(pct)] <- NA_real_
  if (higher_is_better) pct else 1 - pct
}

build_team_summary_table <- function(game_log, seed_data, region_lookup, season) {
  season_team_lookup <- game_log |>
    filter(season == !!season) |>
    group_by(team_id, team_name) |>
    summarise(
      games = n(),
      wins = sum(outcome == "Win"),
      losses = sum(outcome == "Loss"),
      avg_points = mean(team_score),
      avg_points_allowed = mean(opponent_score),
      avg_margin = mean(margin),
      fg_pct = mean(fg_pct, na.rm = TRUE),
      fg3_pct = mean(fg3_pct, na.rm = TRUE),
      ft_pct = mean(ft_pct, na.rm = TRUE),
      efg_pct = mean(efg_pct, na.rm = TRUE),
      rebounds = mean(total_rebounds),
      assists = mean(ast),
      turnovers = mean(tov),
      steals = mean(stl),
      blocks = mean(blk),
      off_rating = mean(off_rating, na.rm = TRUE),
      def_rating = mean(def_rating, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      win_pct = wins / pmax(games, 1)
    ) |>
    left_join(
      seed_data |>
        filter(season == !!season) |>
        left_join(filter(region_lookup, season == !!season), by = c("season", "region")) |>
        select(team_id, seed_label = seed_raw, region_name),
      by = "team_id"
    ) |>
    mutate(
      win_pct_pct = compute_percentile(win_pct, TRUE),
      avg_points_pct = compute_percentile(avg_points, TRUE),
      avg_points_allowed_pct = compute_percentile(avg_points_allowed, FALSE),
      avg_margin_pct = compute_percentile(avg_margin, TRUE),
      fg_pct_pct = compute_percentile(fg_pct, TRUE),
      fg3_pct_pct = compute_percentile(fg3_pct, TRUE),
      ft_pct_pct = compute_percentile(ft_pct, TRUE),
      efg_pct_pct = compute_percentile(efg_pct, TRUE),
      rebounds_pct = compute_percentile(rebounds, TRUE),
      assists_pct = compute_percentile(assists, TRUE),
      turnovers_pct = compute_percentile(turnovers, FALSE),
      steals_pct = compute_percentile(steals, TRUE),
      blocks_pct = compute_percentile(blocks, TRUE),
      off_rating_pct = compute_percentile(off_rating, TRUE),
      def_rating_pct = compute_percentile(def_rating, FALSE)
    )

  season_team_lookup
}

build_team_summary <- function(game_log, summary_table, team_id, season) {
  team_games <- game_log |>
    filter(team_id == !!team_id, season == !!season) |>
    arrange(day_num)

  if (nrow(team_games) == 0) {
    return(NULL)
  }

  season_team_lookup <- summary_table |> select(team_id, team_name, wins, losses, win_pct)

  summary_stats <- summary_table |>
    filter(team_id == !!team_id) |>
    slice(1)

  key_games <- team_games |>
    left_join(
      season_team_lookup |>
        select(opponent_id = team_id, opponent_win_pct = win_pct),
      by = "opponent_id"
    ) |>
    mutate(
      location_label = case_when(
        location == "H" ~ "Home",
        location == "A" ~ "Away",
        TRUE ~ "Neutral"
      ),
      game_label = paste0(opponent_name, " (", location_label, ")")
    )

  best_wins <- key_games |>
    filter(outcome == "Win") |>
    arrange(desc(opponent_win_pct), desc(margin), desc(opponent_score)) |>
    transmute(
      game = game_label,
      score = paste0(team_score, "-", opponent_score),
      margin = margin,
      opponent_win_pct = opponent_win_pct
    ) |>
    slice_head(n = 5)

  worst_losses <- key_games |>
    filter(outcome == "Loss") |>
    arrange(opponent_win_pct, margin, opponent_score) |>
    transmute(
      game = game_label,
      score = paste0(team_score, "-", opponent_score),
      margin = margin,
      opponent_win_pct = opponent_win_pct
    ) |>
    slice_head(n = 5)

  list(summary = summary_stats, best_wins = best_wins, worst_losses = worst_losses)
}

format_percent <- function(value, digits = 1) {
  if (is.na(value)) {
    return("TBD")
  }

  paste0(round(value * 100, digits), "%")
}

interpolate_heat_color <- function(probability, min_probability = 0, max_probability = 1) {
  if (is.na(probability)) {
    return("#f8f5ec")
  }

  if (is.na(min_probability) || is.na(max_probability) || max_probability <= min_probability) {
    scaled <- 1
  } else {
    scaled <- (probability - min_probability) / (max_probability - min_probability)
  }

  scaled <- min(1, max(0, scaled))

  low_col <- grDevices::col2rgb("#c94b3c")
  mid_col <- grDevices::col2rgb("#f2d58a")
  high_col <- grDevices::col2rgb("#1f7a4c")

  blend <- function(start_col, end_col, t) {
    rgb(
      red = start_col[1] + (end_col[1] - start_col[1]) * t,
      green = start_col[2] + (end_col[2] - start_col[2]) * t,
      blue = start_col[3] + (end_col[3] - start_col[3]) * t,
      maxColorValue = 255
    )
  }

  if (scaled <= 0.5) {
    blend(low_col, mid_col, scaled / 0.5)
  } else {
    blend(mid_col, high_col, (scaled - 0.5) / 0.5)
  }
}

load_historical_seed_rates <- function(division_code) {
  results_file <- if (division_code == "mens") {
    file.path(INPUT_DIR, "MNCAATourneyCompactResults.csv")
  } else {
    file.path(INPUT_DIR, "WNCAATourneyCompactResults.csv")
  }

  seed_data <- load_seed_data(division_code)
  valid_pairs <- tibble(
    top_seed = c(1, 8, 5, 4, 6, 3, 7, 2),
    bottom_seed = c(16, 9, 12, 13, 11, 14, 10, 15)
  )

  read_csv(results_file, show_col_types = FALSE) |>
    transmute(
      season = Season,
      w_team_id = WTeamID,
      l_team_id = LTeamID
    ) |>
    left_join(
      seed_data |>
        select(season, team_id, w_seed = seed_num),
      by = c("season", "w_team_id" = "team_id")
    ) |>
    left_join(
      seed_data |>
        select(season, team_id, l_seed = seed_num),
      by = c("season", "l_team_id" = "team_id")
    ) |>
    filter(!is.na(w_seed), !is.na(l_seed), w_seed != l_seed) |>
    mutate(
      top_seed = pmin(w_seed, l_seed),
      bottom_seed = pmax(w_seed, l_seed),
      lower_seed_won = as.integer(w_seed > l_seed)
    ) |>
    inner_join(valid_pairs, by = c("top_seed", "bottom_seed")) |>
    group_by(top_seed, bottom_seed) |>
    summarise(
      lower_seed_win_rate = mean(lower_seed_won),
      games = n(),
      .groups = "drop"
    )
}

extract_matchup_prediction <- function(predictions, team_a_id, team_b_id) {
  if (length(team_a_id) == 0 || length(team_b_id) == 0 || is.na(team_a_id) || is.na(team_b_id) || team_a_id == team_b_id) {
    return(tibble())
  }

  predictions |>
    filter(
      ((a_team_id == team_a_id & b_team_id == team_b_id) |
         (a_team_id == team_b_id & b_team_id == team_a_id))
    ) |>
    slice(1)
}

matchup_probability <- function(predictions, team_a_id, team_b_id) {
  matchup <- extract_matchup_prediction(predictions, team_a_id, team_b_id)

  if (nrow(matchup) == 0) {
    return(NA_real_)
  }

  if (matchup$a_team_id[[1]] == team_a_id) {
    matchup$xgb_predicted[[1]]
  } else {
    1 - matchup$xgb_predicted[[1]]
  }
}

build_seed_choices <- function(seed_data, team_lookup, region_lookup, season, region_filter = "All") {
  choices <- seed_data |>
    filter(season == !!season) |>
    left_join(team_lookup, by = "team_id") |>
    left_join(filter(region_lookup, season == !!season), by = c("season", "region"))

  if (!identical(region_filter, "All")) {
    choices <- choices |> filter(region_name == region_filter)
  }

  choices |>
    arrange(region_name, seed_num, team_name) |>
    mutate(choice_label = paste0(region_name, " ", str_pad(seed_num, 2, pad = "0"), " - ", team_name)) |>
    select(choice_label, team_id, team_name, region, region_name, seed_num)
}

build_first_round_games <- function(seed_data, team_lookup, region_lookup, historical_seed_rates, predictions, season) {
  round_one_pairs <- tibble(
    game_order = 1:8,
    top_seed = c(1, 8, 5, 4, 6, 3, 7, 2),
    bottom_seed = c(16, 9, 12, 13, 11, 14, 10, 15)
  )

  seeded_teams <- seed_data |>
    filter(season == !!season) |>
    left_join(team_lookup, by = "team_id") |>
    left_join(filter(region_lookup, season == !!season), by = c("season", "region")) |>
    group_by(region, seed_num) |>
    summarise(
      team_ids = list(team_id),
      team_label = paste(team_name, collapse = " / "),
      region_name = first(region_name),
      team_count = n(),
      .groups = "drop"
    )

  regions <- seeded_teams |>
    distinct(region) |>
    arrange(region)

  first_round <- expand_grid(region = regions$region, round_one_pairs) |>
    left_join(
      seeded_teams |>
        rename(
          top_team_ids = team_ids,
          top_team = team_label,
          top_region_name = region_name,
          top_team_count = team_count,
          top_seed_num = seed_num
        ),
      by = c("region", "top_seed" = "top_seed_num")
    ) |>
    left_join(
      seeded_teams |>
        rename(
          bottom_team_ids = team_ids,
          bottom_team = team_label,
          bottom_region_name = region_name,
          bottom_team_count = team_count,
          bottom_seed_num = seed_num
        ),
      by = c("region", "bottom_seed" = "bottom_seed_num")
    ) |>
    left_join(historical_seed_rates, by = c("top_seed", "bottom_seed")) |>
    mutate(
      top_team_id = vapply(
        top_team_ids,
        function(x) if (length(x) == 1) as.integer(x[[1]]) else NA_integer_,
        integer(1)
      ),
      bottom_team_id = vapply(
        bottom_team_ids,
        function(x) if (length(x) == 1) as.integer(x[[1]]) else NA_integer_,
        integer(1)
      ),
      team_a_win_probability = vapply(
        seq_len(n()),
        function(i) matchup_probability(predictions, top_team_id[[i]], bottom_team_id[[i]]),
        numeric(1)
      ),
      team_b_win_probability = ifelse(is.na(team_a_win_probability), NA_real_, 1 - team_a_win_probability),
      lower_seed_win_probability = team_b_win_probability,
      upset_watch = !is.na(lower_seed_win_probability) &
        bottom_seed > top_seed &
        !is.na(lower_seed_win_rate) &
        lower_seed_win_probability > 0.10 &
        lower_seed_win_probability > lower_seed_win_rate,
      matchup_status = case_when(
        is.na(top_team_id) | is.na(bottom_team_id) ~ "Play-in pending",
        upset_watch ~ "Upset watch",
        TRUE ~ "Ready"
      ),
      matchup_label = paste0(top_region_name, " ", top_seed, " vs ", bottom_seed)
    ) |>
    transmute(
      game_order,
      region,
      region_name = top_region_name,
      matchup_label,
      team_a = top_team,
      team_b = bottom_team,
      team_a_seed = top_seed,
      team_b_seed = bottom_seed,
      team_a_win_probability = round(team_a_win_probability, 3),
      team_b_win_probability = round(team_b_win_probability, 3),
      lower_seed_historic_win_rate = round(lower_seed_win_rate, 3),
      upset_watch,
      matchup_status
    ) |>
    arrange(region, game_order) |>
    select(-game_order)

  first_round
}

format_probability <- function(probability) {
  ifelse(is.na(probability), "TBD", paste0(round(probability * 100, 1), "%"))
}

clean_seed_display <- function(seed_value) {
  seed_text <- str_extract(as.character(seed_value), "\\d+")
  ifelse(is.na(seed_text), "", seed_text)
}

percentile_color <- function(percentile) {
  if (is.na(percentile)) {
    return("#f8f4ec")
  }

  percentile <- min(max(percentile, 0), 1)
  low_rgb <- grDevices::col2rgb("#f2d9cc")
  high_rgb <- grDevices::col2rgb("#b8e1d2")
  rgb(
    red = (1 - percentile) * low_rgb[1] + percentile * high_rgb[1],
    green = (1 - percentile) * low_rgb[2] + percentile * high_rgb[2],
    blue = (1 - percentile) * low_rgb[3] + percentile * high_rgb[3],
    maxColorValue = 255
  )
}

summary_stat_card_ui <- function(label, value, percentile = NA_real_) {
  div(
    class = "summary-stat-card",
    style = paste0("background:", percentile_color(percentile), ";"),
    div(class = "summary-stat-label", label),
    div(class = "summary-stat-value", value),
    div(class = "summary-stat-rank", if (is.na(percentile)) "No national percentile" else paste0(round(percentile * 100), "th percentile"))
  )
}

key_game_table_ui <- function(data, empty_text) {
  if (nrow(data) == 0) {
    return(div(class = "section-note", empty_text))
  }

  tagList(
    div(
      class = "table-shell",
      tags$table(
        class = "table team-game-table",
        tags$thead(
          tags$tr(
            tags$th("Game"),
            tags$th("Score"),
            tags$th("Margin"),
            tags$th("Opponent Win %")
          )
        ),
        tags$tbody(
          lapply(seq_len(nrow(data)), function(i) {
            row <- data[i, ]
            tags$tr(
              tags$td(row$game[[1]]),
              tags$td(row$score[[1]]),
              tags$td(ifelse(is.na(row$margin[[1]]), "TBD", sprintf("%+.0f", row$margin[[1]]))),
              tags$td(format_percent(row$opponent_win_pct[[1]]))
            )
          })
        )
      )
    )
  )
}

round_probability_table_ui <- function(data) {
  if (nrow(data) == 0) {
    return(div(class = "section-note", "No simulation probability data is available yet. Run the bracket stage first."))
  }

  rounds <- c("Round of 64", "Round of 32", "Sweet 16", "Elite Eight", "Final Four", "Champion")

  round_ranges <- data |>
    group_by(round_name) |>
    summarise(
      min_probability = min(round_probability, na.rm = TRUE),
      max_probability = max(round_probability, na.rm = TRUE),
      .groups = "drop"
    )

  round_fill_color <- function(round_label, probability) {
    round_range <- round_ranges |>
      filter(round_name == !!round_label)

    interpolate_heat_color(
      probability = probability,
      min_probability = round_range$min_probability[[1]],
      max_probability = round_range$max_probability[[1]]
    )
  }

  wide_data <- data |>
    mutate(
      round_name = factor(round_name, levels = rounds),
      probability_label = format_probability(round_probability)
    ) |>
    select(team_name, seed_label, region_name, round_name, round_probability, probability_label) |>
    tidyr::pivot_wider(
      names_from = round_name,
      values_from = c(round_probability, probability_label),
      names_glue = "{.value}__{round_name}"
    ) |>
    arrange(desc(`round_probability__Champion`), desc(`round_probability__Final Four`), region_name, seed_label, team_name)

  tagList(
    div(
      class = "table-shell",
      tags$table(
        class = "table team-game-table",
        tags$thead(
          tags$tr(
            tags$th("Team"),
            tags$th("Seed"),
            tags$th("Region"),
            lapply(rounds, tags$th)
          )
        ),
        tags$tbody(
          lapply(seq_len(nrow(wide_data)), function(i) {
            row <- wide_data[i, ]
            tags$tr(
              tags$td(row$team_name[[1]]),
              tags$td(row$seed_label[[1]]),
              tags$td(row$region_name[[1]]),
              lapply(rounds, function(round_name) {
                probability_col <- paste0("round_probability__", round_name)
                label_col <- paste0("probability_label__", round_name)
                probability_value <- row[[probability_col]][[1]]
                label_value <- row[[label_col]][[1]]

                tags$td(
                  style = paste0(
                    "background:", round_fill_color(round_name, probability_value), ";",
                    "font-weight:700;"
                  ),
                  ifelse(is.null(label_value) || is.na(label_value), "TBD", label_value)
                )
              })
            )
          })
        )
      )
    )
  )
}

matchup_card_ui <- function(team_a, team_b, prob_a, prob_b, title, subtitle = NULL, upset_watch = FALSE) {
  favored_a <- !is.na(prob_a) && (is.na(prob_b) || prob_a >= prob_b)
  favored_b <- !is.na(prob_b) && !favored_a
  alpha_from_probability <- function(probability) {
    if (is.na(probability) || probability <= 0.5) {
      return(0)
    }

    min(1, (probability - 0.5) / 0.5)
  }

  team_style <- function(probability, favored) {
    if (!favored) {
      return("background:#f3efe4;")
    }

    paste0(
      "background:",
      grDevices::adjustcolor("#1f7a4c", alpha.f = alpha_from_probability(probability)),
      ";"
    )
  }

  border_style <- if (isTRUE(upset_watch)) {
    "border: 2px solid #c0392b; box-shadow: 0 10px 24px rgba(192, 57, 43, 0.18);"
  } else {
    ""
  }

  div(
    class = if (isTRUE(upset_watch)) "matchup-card matchup-card-alert" else "matchup-card",
    style = border_style,
    div(class = "matchup-title", title),
    if (isTRUE(upset_watch)) {
      div(
        class = "upset-alert-banner",
        div(class = "upset-alert-kicker", "Upset Alert"),
        div(class = "upset-alert-text", "Lower seed win probability is above the historical average.")
      )
    },
    if (!is.null(subtitle)) div(class = "matchup-subtitle", subtitle),
    div(
      class = "matchup-grid",
      div(
        class = "matchup-side",
        style = team_style(prob_a, favored_a),
        div(class = "team-name", team_a),
        div(class = "team-prob", format_probability(prob_a))
      ),
      div(
        class = "matchup-side",
        style = team_style(prob_b, favored_b),
        div(class = "team-name", team_b),
        div(class = "team-prob", format_probability(prob_b))
      )
    )
  )
}

export_matchups_jpg <- function(matchups, file, title) {
  if (nrow(matchups) == 0) {
    jpeg(filename = file, width = 1400, height = 700, quality = 100)
    grid.newpage()
    grid.rect(gp = gpar(fill = "#f3efe5", col = NA))
    grid.text(title, x = 0.07, y = 0.9, just = "left", gp = gpar(fontsize = 24, fontface = "bold", col = "#10253c"))
    grid.text("No matchups available to export.", x = 0.07, y = 0.82, just = "left", gp = gpar(fontsize = 14, col = "#5d6a78"))
    dev.off()
    return(invisible(file))
  }

  height_px <- max(980, 210 * nrow(matchups) + 220)
  jpeg(filename = file, width = 1600, height = height_px, quality = 100)
  grid.newpage()
  grid.rect(gp = gpar(fill = "#f3efe5", col = NA))
  grid.circle(x = 0.94, y = 0.94, r = 0.13, gp = gpar(fill = grDevices::adjustcolor("#0d7c6f", alpha.f = 0.08), col = NA))
  grid.circle(x = 0.08, y = 0.08, r = 0.16, gp = gpar(fill = grDevices::adjustcolor("#c99f4b", alpha.f = 0.1), col = NA))
  pushViewport(viewport(layout = grid.layout(nrow(matchups) + 1, 1)))
  pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
  grid.roundrect(
    x = 0.5, y = 0.55, width = 0.96, height = 0.72,
    r = unit(0.03, "snpc"),
    gp = gpar(fill = grDevices::adjustcolor("#ffffff", alpha.f = 0.82), col = "#e1d7c4", lwd = 1.5)
  )
  grid.text(title, x = 0.06, y = 0.62, just = "left", gp = gpar(fontsize = 24, fontface = "bold", col = "#10253c"))
  grid.text("March Madness Models", x = 0.06, y = 0.42, just = "left", gp = gpar(fontsize = 11, fontface = "bold", col = "#0d7c6f"))
  popViewport()

  for (i in seq_len(nrow(matchups))) {
    row <- matchups[i, ]
    y_row <- i + 1
    prob_a <- row$team_a_win_probability[[1]]
    prob_b <- row$team_b_win_probability[[1]]
    favored_a <- !is.na(prob_a) && (is.na(prob_b) || prob_a >= prob_b)
    favored_b <- !is.na(prob_b) && !favored_a
    alpha_a <- if (favored_a && !is.na(prob_a) && prob_a > 0.5) min(1, (prob_a - 0.5) / 0.5) else 0
    alpha_b <- if (favored_b && !is.na(prob_b) && prob_b > 0.5) min(1, (prob_b - 0.5) / 0.5) else 0
    border_col <- if ("upset_watch" %in% names(row) && isTRUE(row$upset_watch[[1]])) "#c0392b" else "#d8d1bf"
    subtitle <- if ("matchup_status" %in% names(row)) row$matchup_status[[1]] else ""

    pushViewport(viewport(layout.pos.row = y_row, layout.pos.col = 1))
    grid.roundrect(
      x = 0.5, y = 0.5, width = 0.96, height = 0.85,
      r = unit(0.03, "snpc"),
      gp = gpar(fill = "#fffdf8", col = border_col, lwd = 1.8)
    )
    grid.roundrect(
      x = 0.5, y = 0.88, width = 0.96, height = 0.06,
      r = unit(0.03, "snpc"),
      gp = gpar(fill = "#13314d", col = NA)
    )
    grid.text(row$matchup_label[[1]], x = 0.06, y = 0.82, just = "left", gp = gpar(fontsize = 16, fontface = "bold", col = "#10253c"))
    grid.text(subtitle, x = 0.06, y = 0.68, just = "left", gp = gpar(fontsize = 11, col = "#6b7280"))
    grid.roundrect(
      x = 0.27, y = 0.34, width = 0.4, height = 0.34, r = unit(0.02, "snpc"),
      gp = gpar(fill = if (favored_a) grDevices::adjustcolor("#0d7c6f", alpha.f = 0.20 + (0.55 * alpha_a)) else "#f5efe3", col = "#dfd4c2", lwd = 1)
    )
    grid.roundrect(
      x = 0.73, y = 0.34, width = 0.4, height = 0.34, r = unit(0.02, "snpc"),
      gp = gpar(fill = if (favored_b) grDevices::adjustcolor("#0d7c6f", alpha.f = 0.20 + (0.55 * alpha_b)) else "#f5efe3", col = "#dfd4c2", lwd = 1)
    )
    grid.text(row$team_a[[1]], x = 0.08, y = 0.41, just = "left", gp = gpar(fontsize = 14, fontface = "bold", col = "#10253c"))
    grid.text(row$team_b[[1]], x = 0.54, y = 0.41, just = "left", gp = gpar(fontsize = 14, fontface = "bold", col = "#10253c"))
    grid.text(format_probability(prob_a), x = 0.39, y = 0.41, just = "right", gp = gpar(fontsize = 18, fontface = "bold", col = "#10253c"))
    grid.text(format_probability(prob_b), x = 0.85, y = 0.41, just = "right", gp = gpar(fontsize = 18, fontface = "bold", col = "#10253c"))
    popViewport()
  }

  dev.off()
  invisible(file)
}

export_team_summary_jpg <- function(team_summary, file, title) {
  jpeg(filename = file, width = 1600, height = 1200, quality = 100)
  grid.newpage()

  if (is.null(team_summary)) {
    grid.text(title, y = 0.92, gp = gpar(fontsize = 24, fontface = "bold"))
    grid.text("No team summary is available for that selection.", y = 0.84, gp = gpar(fontsize = 14))
    dev.off()
    return(invisible(file))
  }

  summary_row <- team_summary$summary
  stat_labels <- c("Record", "Seed", "Points For", "Points Allowed", "Avg Margin", "eFG%", "Off Rating", "Def Rating")
  stat_values <- c(
    paste0(summary_row$wins[[1]], "-", summary_row$losses[[1]], " (", format_percent(summary_row$win_pct[[1]]), ")"),
    ifelse(is.na(summary_row$seed_label[[1]]), "Not seeded", paste(summary_row$seed_label[[1]], summary_row$region_name[[1]])),
    round(summary_row$avg_points[[1]], 1),
    round(summary_row$avg_points_allowed[[1]], 1),
    sprintf("%+.1f", summary_row$avg_margin[[1]]),
    format_percent(summary_row$efg_pct[[1]]),
    round(summary_row$off_rating[[1]], 1),
    round(summary_row$def_rating[[1]], 1)
  )

  grid.text(title, x = 0.05, y = 0.95, just = "left", gp = gpar(fontsize = 24, fontface = "bold"))
  grid.text(summary_row$team_name[[1]], x = 0.05, y = 0.90, just = "left", gp = gpar(fontsize = 30, fontface = "bold"))

  for (i in seq_along(stat_labels)) {
    col_idx <- ((i - 1) %% 4)
    row_idx <- ((i - 1) %/% 4)
    x_left <- 0.05 + col_idx * 0.23
    y_top <- 0.83 - row_idx * 0.14
    grid.roundrect(
      x = x_left + 0.095, y = y_top - 0.045,
      width = 0.19, height = 0.10,
      just = c("left", "top"),
      r = unit(0.02, "snpc"),
      gp = gpar(fill = "#fffdf7", col = "#d8d1bf")
    )
    grid.text(stat_labels[[i]], x = x_left + 0.015, y = y_top - 0.02, just = "left", gp = gpar(fontsize = 11, fontface = "bold", col = "#6b7280"))
    grid.text(as.character(stat_values[[i]]), x = x_left + 0.015, y = y_top - 0.065, just = "left", gp = gpar(fontsize = 18, fontface = "bold"))
  }

  draw_game_section <- function(data, section_title, y_top) {
    grid.text(section_title, x = 0.05, y = y_top, just = "left", gp = gpar(fontsize = 20, fontface = "bold"))
    if (nrow(data) == 0) {
      grid.text("No games to highlight.", x = 0.05, y = y_top - 0.05, just = "left", gp = gpar(fontsize = 13))
      return(invisible(NULL))
    }

    for (i in seq_len(min(nrow(data), 5))) {
      row <- data[i, ]
      y <- y_top - 0.06 - (i - 1) * 0.08
      grid.roundrect(
        x = 0.5, y = y,
        width = 0.90, height = 0.06,
        r = unit(0.015, "snpc"),
        gp = gpar(fill = "#fffdf7", col = "#d8d1bf")
      )
      grid.text(row$game[[1]], x = 0.07, y = y + 0.012, just = "left", gp = gpar(fontsize = 13, fontface = "bold"))
      grid.text(paste0("Score ", row$score[[1]], "   Margin ", sprintf("%+.0f", row$margin[[1]]), "   Opp Win% ", format_percent(row$opponent_win_pct[[1]])), x = 0.07, y = y - 0.012, just = "left", gp = gpar(fontsize = 11, col = "#5f6c7b"))
    }
  }

  draw_game_section(team_summary$best_wins, "Key Wins", 0.52)
  draw_game_section(team_summary$worst_losses, "Key Losses", 0.22)

  dev.off()
  invisible(file)
}

export_round_probabilities_jpg <- function(data, file, title) {
  jpeg(filename = file, width = 1800, height = max(1000, 160 + 42 * nrow(data)), quality = 100)
  grid.newpage()

  if (nrow(data) == 0) {
    grid.text(title, y = 0.92, gp = gpar(fontsize = 24, fontface = "bold"))
    grid.text("No round probability data is available.", y = 0.84, gp = gpar(fontsize = 14))
    dev.off()
    return(invisible(file))
  }

  rounds <- c("Round of 64", "Round of 32", "Sweet 16", "Elite Eight", "Final Four", "Champion")
  wide_data <- data |>
    mutate(probability_label = format_probability(round_probability)) |>
    select(team_name, seed_label, region_name, round_name, probability_label) |>
    mutate(round_name = factor(round_name, levels = rounds)) |>
    tidyr::pivot_wider(names_from = round_name, values_from = probability_label) |>
    arrange(region_name, seed_label, team_name)

  grid.text(title, x = 0.03, y = 0.97, just = "left", gp = gpar(fontsize = 24, fontface = "bold"))

  header_y <- 0.92
  col_x <- c(0.04, 0.28, 0.36, 0.48, 0.58, 0.67, 0.76, 0.85, 0.94)
  headers <- c("Team", "Seed", "Region", rounds)
  for (i in seq_along(headers)) {
    grid.text(headers[[i]], x = col_x[[i]], y = header_y, just = "left", gp = gpar(fontsize = 11, fontface = "bold"))
  }

  for (i in seq_len(nrow(wide_data))) {
    y <- header_y - i * 0.028
    row <- wide_data[i, ]
    values <- c(
      row$team_name[[1]],
      row$seed_label[[1]],
      row$region_name[[1]],
      vapply(rounds, function(round_name) {
        value <- row[[round_name]][[1]]
        ifelse(is.na(value), "TBD", value)
      }, character(1))
    )
    for (j in seq_along(values)) {
      grid.text(values[[j]], x = col_x[[j]], y = y, just = "left", gp = gpar(fontsize = 10))
    }
  }

  dev.off()
  invisible(file)
}

build_bracket_seed_table_app <- function(seed_data, team_lookup, region_lookup, season, tournament_code) {
  seed_data |>
    filter(season == !!season) |>
    left_join(team_lookup, by = "team_id") |>
    left_join(filter(region_lookup, season == !!season), by = c("season", "region")) |>
    mutate(
      seed_label = as.character(seed_num),
      tournament_code = tournament_code
    ) |>
    select(tournament_code, team_id, team_name, region, region_name, seed_num, seed_label)
}

build_bracket_probability_lookup_app <- function(predictions) {
  predictions |>
    transmute(
      matchup_id = paste(pmin(a_team_id, b_team_id), pmax(a_team_id, b_team_id), sep = "_"),
      a_team_id = pmin(a_team_id, b_team_id),
      b_team_id = pmax(a_team_id, b_team_id),
      prob_a_wins = if_else(a_team_id <= b_team_id, xgb_predicted, 1 - xgb_predicted)
    ) |>
    distinct(matchup_id, .keep_all = TRUE)
}

simulate_play_in_team <- function(team_ids, probability_lookup) {
  if (length(team_ids) <= 1) {
    return(team_ids[[1]])
  }

  team_1 <- team_ids[[1]]
  team_2 <- team_ids[[2]]
  matchup_id <- paste(pmin(team_1, team_2), pmax(team_1, team_2), sep = "_")
  matchup <- probability_lookup |>
    filter(matchup_id == !!matchup_id) |>
    slice(1)

  if (nrow(matchup) == 0) {
    return(team_1)
  }

  prob_team_1_wins <- if (team_1 == matchup$a_team_id[[1]]) matchup$prob_a_wins[[1]] else 1 - matchup$prob_a_wins[[1]]
  if (runif(1) < prob_team_1_wins) team_1 else team_2
}

build_round_one_games_app <- function(seed_table, probability_lookup) {
  round_one_pairs <- tibble(
    best_seed = 1:8,
    top_seed = c(1, 8, 5, 4, 6, 3, 7, 2),
    bottom_seed = c(16, 9, 12, 13, 11, 14, 10, 15)
  )

  pairings <- expand_grid(region = sort(unique(seed_table$region)), round_one_pairs) |>
    left_join(
      seed_table |>
        group_by(region, seed_num) |>
        summarise(team_ids = list(team_id), .groups = "drop") |>
        rename(top_seed = seed_num, top_team_ids = team_ids),
      by = c("region", "top_seed")
    ) |>
    left_join(
      seed_table |>
        group_by(region, seed_num) |>
        summarise(team_ids = list(team_id), .groups = "drop") |>
        rename(bottom_seed = seed_num, bottom_team_ids = team_ids),
      by = c("region", "bottom_seed")
    ) |>
    rowwise() |>
    mutate(
      team_1 = top_team_ids[[1]][[1]],
      team_2 = simulate_play_in_team(bottom_team_ids[[1]], probability_lookup),
      Slot = paste0("R1", region, best_seed)
    ) |>
    ungroup() |>
    select(Slot, team_1, team_2)

  pairings
}

play_bracket_round_app <- function(games, probability_lookup, seed_table) {
  games |>
    mutate(
      matchup_id = paste(pmin(team_1, team_2), pmax(team_1, team_2), sep = "_"),
      low_team_id = pmin(team_1, team_2),
      high_team_id = pmax(team_1, team_2)
    ) |>
    left_join(select(probability_lookup, matchup_id, prob_a_wins), by = "matchup_id") |>
    mutate(
      prob_team_1_wins = if_else(team_1 == low_team_id, prob_a_wins, 1 - prob_a_wins),
      winner = if_else(runif(n()) < prob_team_1_wins, team_1, team_2)
    ) |>
    left_join(select(seed_table, team_id, seed_label, team_name, seed_num, region_name), by = c("winner" = "team_id")) |>
    transmute(Slot, Team = seed_label, team_name, winner, seed_num, region_name)
}

advance_bracket_round_app <- function(round_results, slot_prefix, next_prefix, probability_lookup, seed_table) {
  round_results |>
    mutate(
      region = str_extract(Slot, paste0("(?<=", slot_prefix, ")\\w")),
      game_index = as.integer(str_extract(Slot, "\\d$")),
      next_slot = paste0(next_prefix, region, ceiling(game_index / 2))
    ) |>
    group_by(next_slot) |>
    summarise(team_1 = first(winner), team_2 = nth(winner, 2), .groups = "drop") |>
    rename(Slot = next_slot) |>
    play_bracket_round_app(probability_lookup, seed_table)
}

play_bracket_final_four_app <- function(round_results, probability_lookup, seed_table) {
  semifinals <- round_results |>
    mutate(Slot = c("R5WX", "R5WX", "R5YZ", "R5YZ")) |>
    group_by(Slot) |>
    summarise(team_1 = first(winner), team_2 = nth(winner, 2), .groups = "drop") |>
    play_bracket_round_app(probability_lookup, seed_table)

  championship <- semifinals |>
    mutate(Slot = "R6CH") |>
    group_by(Slot) |>
    summarise(team_1 = first(winner), team_2 = nth(winner, 2), .groups = "drop") |>
    play_bracket_round_app(probability_lookup, seed_table)

  bind_rows(semifinals, championship)
}

simulate_bracket_app <- function(seed_table, probability_lookup, bracket_number = 1) {
  r1 <- build_round_one_games_app(seed_table, probability_lookup) |> play_bracket_round_app(probability_lookup, seed_table)
  r2 <- advance_bracket_round_app(r1, "R1", "R2", probability_lookup, seed_table)
  r3 <- advance_bracket_round_app(r2, "R2", "R3", probability_lookup, seed_table)
  r4 <- advance_bracket_round_app(r3, "R3", "R4", probability_lookup, seed_table)
  r5_r6 <- play_bracket_final_four_app(r4, probability_lookup, seed_table)

  bind_rows(r1, r2, r3, r4, r5_r6) |>
    mutate(Bracket = bracket_number)
}

lookup_matchup_probability_app <- function(team_1, team_2, probability_lookup) {
  if (length(team_1) == 0 || length(team_2) == 0 || is.na(team_1) || is.na(team_2) || team_1 == team_2) {
    return(NA_real_)
  }

  matchup_id <- paste(pmin(team_1, team_2), pmax(team_1, team_2), sep = "_")
  matchup <- probability_lookup |>
    filter(matchup_id == !!matchup_id) |>
    slice(1)

  if (nrow(matchup) == 0 || is.na(matchup$prob_a_wins[[1]])) {
    return(NA_real_)
  }

  if (team_1 == matchup$a_team_id[[1]]) matchup$prob_a_wins[[1]] else 1 - matchup$prob_a_wins[[1]]
}

build_interactive_round_games_app <- function(round_games, round_label, round_num, seed_table, probability_lookup, selections) {
  team_labels <- seed_table |>
    distinct(team_id, team_name, seed_label, region_name)
  selection_raw <- unname(selections[round_games$Slot])

  round_games |>
    mutate(
      round_label = round_label,
      round_num = round_num,
      selected_team_id = vapply(seq_len(n()), function(i) {
        available_teams <- c(team_1[[i]], team_2[[i]])
        available_teams <- available_teams[!is.na(available_teams)]
        if (!length(available_teams)) {
          return(NA_integer_)
        }

        selected_value <- suppressWarnings(as.integer(selection_raw[[i]]))
        if (is.na(selected_value) || !any(selected_value == available_teams)) {
          return(NA_integer_)
        }
        selected_value
      }, integer(1)),
      winner = selected_team_id,
      team_1_probability = vapply(seq_len(n()), function(i) {
        lookup_matchup_probability_app(team_1[[i]], team_2[[i]], probability_lookup)
      }, numeric(1)),
      team_2_probability = if_else(is.na(team_1_probability), NA_real_, 1 - team_1_probability)
    ) |>
    left_join(rename(team_labels, team_1 = team_id, team_1_name = team_name, team_1_seed = seed_label, team_1_region_name = region_name), by = "team_1") |>
    left_join(rename(team_labels, team_2 = team_id, team_2_name = team_name, team_2_seed = seed_label, team_2_region_name = region_name), by = "team_2") |>
    left_join(rename(team_labels, selected_team_id = team_id, selected_team_seed = seed_label), by = "selected_team_id") |>
    mutate(
      display_region = coalesce(team_1_region_name, team_2_region_name),
      selected_team_name = case_when(
        selected_team_id == team_1 ~ team_1_name,
        selected_team_id == team_2 ~ team_2_name,
        TRUE ~ NA_character_
      )
    )
}

build_interactive_bracket_games_app <- function(seed_table, probability_lookup, selections = character()) {
  region_names <- seed_table |>
    distinct(region, region_name)

  round_one <- build_round_one_games_app(seed_table, probability_lookup) |>
    mutate(region = str_extract(Slot, "(?<=R1)\\w"))
  round_one <- build_interactive_round_games_app(round_one, "Round of 64", 1, seed_table, probability_lookup, selections)

  build_next_round <- function(previous_round, next_prefix, round_label, round_num) {
    previous_round |>
      mutate(
        region = str_extract(Slot, "(?<=R\\d)\\w"),
        game_index = as.integer(str_extract(Slot, "\\d$")),
        next_slot = paste0(next_prefix, region, ceiling(game_index / 2))
      ) |>
      group_by(next_slot, region) |>
      summarise(team_1 = first(winner), team_2 = nth(winner, 2), .groups = "drop") |>
      rename(Slot = next_slot) |>
      build_interactive_round_games_app(round_label, round_num, seed_table, probability_lookup, selections)
  }

  round_two <- build_next_round(round_one, "R2", "Round of 32", 2)
  round_three <- build_next_round(round_two, "R3", "Sweet 16", 3)
  round_four <- build_next_round(round_three, "R4", "Elite Eight", 4)

  semifinals <- round_four |>
    transmute(
      Slot = c("R5WX", "R5WX", "R5YZ", "R5YZ"),
      region = c("WX", "WX", "YZ", "YZ"),
      winner
    ) |>
    group_by(Slot, region) |>
    summarise(team_1 = first(winner), team_2 = nth(winner, 2), .groups = "drop") |>
    build_interactive_round_games_app("Final Four", 5, seed_table, probability_lookup, selections) |>
    mutate(display_region = case_when(
      region == "WX" ~ paste(region_names$region_name[region_names$region == "W"][[1]], region_names$region_name[region_names$region == "X"][[1]], sep = " / "),
      region == "YZ" ~ paste(region_names$region_name[region_names$region == "Y"][[1]], region_names$region_name[region_names$region == "Z"][[1]], sep = " / "),
      TRUE ~ "Final Four"
    ))

  championship <- semifinals |>
    transmute(Slot = "R6CH", region = "CH", winner) |>
    group_by(Slot, region) |>
    summarise(team_1 = first(winner), team_2 = nth(winner, 2), .groups = "drop") |>
    build_interactive_round_games_app("Championship", 6, seed_table, probability_lookup, selections) |>
    mutate(display_region = "National Championship")

  bind_rows(round_one, round_two, round_three, round_four, semifinals, championship) |>
    mutate(
      matchup_order = case_when(
        round_num == 1 ~ as.integer(str_extract(Slot, "\\d$")),
        round_num == 2 ~ as.integer(str_extract(Slot, "\\d$")),
        round_num == 3 ~ as.integer(str_extract(Slot, "\\d$")),
        round_num == 4 ~ as.integer(str_extract(Slot, "\\d$")),
        round_num == 5 ~ match(Slot, c("R5WX", "R5YZ")),
        TRUE ~ 1L
      )
    ) |>
    arrange(round_num, region, matchup_order)
}

resolve_seed_team_for_display_app <- function(team_ids, selected_team_ids, probability_lookup) {
  team_ids <- unique(stats::na.omit(as.integer(team_ids)))
  if (!length(team_ids)) {
    return(NA_integer_)
  }

  if (length(team_ids) == 1) {
    return(team_ids[[1]])
  }

  selected_team_ids <- unique(stats::na.omit(suppressWarnings(as.integer(selected_team_ids))))
  used_team_ids <- intersect(team_ids, selected_team_ids)
  if (length(used_team_ids) > 0) {
    return(used_team_ids[[1]])
  }

  fallback_probability <- lookup_matchup_probability_app(team_ids[[1]], team_ids[[2]], probability_lookup)
  if (is.na(fallback_probability) || fallback_probability >= 0.5) {
    team_ids[[1]]
  } else {
    team_ids[[2]]
  }
}

build_round_one_games_from_selections_app <- function(seed_table, probability_lookup, selections) {
  round_one_pairs <- tibble(
    best_seed = 1:8,
    top_seed = c(1, 8, 5, 4, 6, 3, 7, 2),
    bottom_seed = c(16, 9, 12, 13, 11, 14, 10, 15)
  )

  expand_grid(region = sort(unique(seed_table$region)), round_one_pairs) |>
    left_join(
      seed_table |>
        group_by(region, seed_num) |>
        summarise(team_ids = list(team_id), .groups = "drop") |>
        rename(top_seed = seed_num, top_team_ids = team_ids),
      by = c("region", "top_seed")
    ) |>
    left_join(
      seed_table |>
        group_by(region, seed_num) |>
        summarise(team_ids = list(team_id), .groups = "drop") |>
        rename(bottom_seed = seed_num, bottom_team_ids = team_ids),
      by = c("region", "bottom_seed")
    ) |>
    transmute(
      Slot = paste0("R1", region, best_seed),
      region = region,
      team_1 = vapply(top_team_ids, resolve_seed_team_for_display_app, integer(1), selected_team_ids = selections, probability_lookup = probability_lookup),
      team_2 = vapply(bottom_team_ids, resolve_seed_team_for_display_app, integer(1), selected_team_ids = selections, probability_lookup = probability_lookup)
    )
}

resolve_seed_reference_app <- function(seed_table, seed_ref, bracket_results) {
  region_code <- str_extract(seed_ref, "^[A-Z]+")
  seed_number <- suppressWarnings(as.integer(str_extract(seed_ref, "\\d+")))

  seed_entries <- seed_table |>
    filter(region == !!region_code, seed_num == !!seed_number) |>
    arrange(team_name)

  if (nrow(seed_entries) == 0) {
    return(list(
      team_id = NA_integer_,
      seed_label = clean_seed_display(seed_ref),
      team_name = "TBD",
      region_name = NA_character_,
      label = "TBD"
    ))
  }

  chosen_team_id <- bracket_results$team_id[bracket_results$team_id %in% seed_entries$team_id]
  chosen_team_id <- if (length(chosen_team_id)) chosen_team_id[[1]] else NA_integer_

  chosen_entry <- if (!is.na(chosen_team_id)) {
    seed_entries |>
      filter(team_id == !!chosen_team_id) |>
      slice(1)
  } else if (nrow(seed_entries) == 1) {
    seed_entries |>
      slice(1)
  } else {
    tibble()
  }

  if (nrow(chosen_entry) == 1) {
    label <- trimws(paste(chosen_entry$seed_label[[1]], chosen_entry$team_name[[1]]))
    return(list(
      team_id = chosen_entry$team_id[[1]],
      seed_label = chosen_entry$seed_label[[1]],
      team_name = chosen_entry$team_name[[1]],
      region_name = chosen_entry$region_name[[1]],
      label = label
    ))
  }

  combined_name <- paste(seed_entries$team_name, collapse = " / ")
  list(
    team_id = NA_integer_,
    seed_label = seed_entries$seed_label[[1]],
    team_name = combined_name,
    region_name = seed_entries$region_name[[1]],
    label = trimws(paste(seed_entries$seed_label[[1]], combined_name))
  )
}

build_saved_bracket_games_app <- function(seed_table, bracket_results, slots_frame, season) {
  if (nrow(bracket_results) == 0) {
    return(tibble())
  }

  winners_lookup <- bracket_results |>
    left_join(
      seed_table |>
        select(team_id, team_name, seed_label, region_name),
      by = "team_id"
    ) |>
    mutate(
      winner_label = if_else(
        is.na(team_name),
        "TBD",
        trimws(paste(seed_label, team_name))
      )
    )

  resolve_slot_entry <- function(slot_ref) {
    slot_entry <- winners_lookup |>
      filter(Slot == !!slot_ref) |>
      slice(1)

    if (nrow(slot_entry) == 0) {
      return(list(
        team_id = NA_integer_,
        seed_label = "",
        team_name = "TBD",
        region_name = NA_character_,
        label = "TBD"
      ))
    }

    list(
      team_id = slot_entry$team_id[[1]],
      seed_label = coalesce(slot_entry$seed_label[[1]], ""),
      team_name = coalesce(slot_entry$team_name[[1]], "TBD"),
      region_name = slot_entry$region_name[[1]],
      label = coalesce(slot_entry$winner_label[[1]], "TBD")
    )
  }

  resolve_reference <- function(ref) {
    if (str_detect(ref, "^R\\d")) {
      return(resolve_slot_entry(ref))
    }

    resolve_seed_reference_app(seed_table, ref, bracket_results)
  }

  region_names <- seed_table |>
    distinct(region, region_name)

  slots_for_season <- slots_frame |>
    filter(season == !!season, Slot %in% bracket_results$Slot) |>
    distinct(Slot, StrongSeed, WeakSeed)

  game_rows <- lapply(seq_len(nrow(slots_for_season)), function(i) {
    slot_row <- slots_for_season[i, ]
    ordered_refs <- order_slot_references_app(slot_row$StrongSeed[[1]], slot_row$WeakSeed[[1]])
    top_entry <- resolve_reference(ordered_refs[[1]])
    bottom_entry <- resolve_reference(ordered_refs[[2]])
    winner_entry <- resolve_slot_entry(slot_row$Slot[[1]])
    round_num <- as.integer(str_extract(slot_row$Slot[[1]], "(?<=R)\\d"))
    region_code <- case_when(
      round_num <= 4 ~ str_extract(slot_row$Slot[[1]], "(?<=R\\d)[A-Z]"),
      round_num == 5 ~ str_extract(slot_row$Slot[[1]], "(?<=R5)[A-Z]{2}"),
      TRUE ~ "CH"
    )

    display_region <- case_when(
      round_num <= 4 ~ region_names$region_name[match(region_code, region_names$region)],
      region_code == "WX" ~ paste(region_names$region_name[match("W", region_names$region)], region_names$region_name[match("X", region_names$region)], sep = " / "),
      region_code == "YZ" ~ paste(region_names$region_name[match("Y", region_names$region)], region_names$region_name[match("Z", region_names$region)], sep = " / "),
      TRUE ~ "National Championship"
    )

    round_label <- case_when(
      round_num == 1 ~ "Round of 64",
      round_num == 2 ~ "Round of 32",
      round_num == 3 ~ "Sweet 16",
      round_num == 4 ~ "Elite Eight",
      round_num == 5 ~ "Final Four",
      TRUE ~ "Championship"
    )

    matchup_order <- case_when(
      round_num <= 4 ~ as.integer(str_extract(slot_row$Slot[[1]], "\\d$")),
      round_num == 5 ~ match(slot_row$Slot[[1]], c("R5WX", "R5YZ")),
      TRUE ~ 1L
    )

    tibble(
      Slot = slot_row$Slot[[1]],
      region = region_code,
      team_1 = top_entry$team_id,
      team_2 = bottom_entry$team_id,
      round_label = round_label,
      round_num = round_num,
      selected_team_id = winner_entry$team_id,
      winner = winner_entry$team_id,
      team_1_probability = NA_real_,
      team_2_probability = NA_real_,
      team_1_name = top_entry$team_name,
      team_1_seed = top_entry$seed_label,
      team_1_region_name = top_entry$region_name,
      team_2_name = bottom_entry$team_name,
      team_2_seed = bottom_entry$seed_label,
      team_2_region_name = bottom_entry$region_name,
      team_name = winner_entry$team_name,
      selected_team_seed = winner_entry$seed_label,
      region_name = winner_entry$region_name,
      display_region = display_region,
      selected_team_name = winner_entry$team_name,
      matchup_order = matchup_order,
      team_1_label = top_entry$label,
      team_2_label = bottom_entry$label,
      winner_label = winner_entry$label
    )
  })

  bind_rows(game_rows) |>
    arrange(round_num, region, matchup_order)
}

build_seed_display_label_app <- function(seed_table, region_code, seed_number, slot_winner_id = NA_integer_) {
  seed_entries <- seed_table |>
    filter(region == !!region_code, seed_num == !!seed_number) |>
    arrange(team_name)

  if (nrow(seed_entries) == 0) {
    return("TBD")
  }

  seed_prefix <- paste0(seed_entries$seed_label[[1]], " ")
  if (nrow(seed_entries) == 1) {
    return(paste0(seed_prefix, seed_entries$team_name[[1]]))
  }

  if (!is.na(slot_winner_id) && any(seed_entries$team_id == slot_winner_id)) {
    chosen_team <- seed_entries |>
      filter(team_id == slot_winner_id) |>
      slice(1)
    return(paste0(seed_prefix, chosen_team$team_name[[1]]))
  }

  paste0(seed_prefix, paste(seed_entries$team_name, collapse = " / "))
}

build_saved_bracket_display_app <- function(seed_table, bracket_results, slots_frame, season) {
  if (nrow(bracket_results) == 0) {
    return(NULL)
  }

  bracket_games <- build_saved_bracket_games_app(
    seed_table = seed_table,
    bracket_results = bracket_results,
    slots_frame = slots_frame,
    season = season
  )

  first_round <- bracket_games |>
    filter(round_num == 1) |>
    transmute(
      region,
      game_index = match(Slot, paste0("R1", region, c(1, 8, 5, 4, 6, 3, 7, 2))),
      top_label = team_1_label,
      bottom_label = team_2_label,
      winner_label = winner_label
    )

  round_two <- bracket_games |>
    filter(round_num == 2) |>
    transmute(
      region,
      game_index = match(Slot, paste0("R2", region, c(1, 4, 3, 2))),
      top_label = team_1_label,
      bottom_label = team_2_label,
      winner_label = winner_label
    )

  round_three <- bracket_games |>
    filter(round_num == 3) |>
    transmute(
      region,
      game_index = match(Slot, paste0("R3", region, c(1, 2))),
      top_label = team_1_label,
      bottom_label = team_2_label,
      winner_label = winner_label
    )

  round_four <- bracket_games |>
    filter(round_num == 4) |>
    transmute(
      region,
      game_index = matchup_order,
      top_label = team_1_label,
      bottom_label = team_2_label,
      winner_label = winner_label
    )

  semifinals <- bracket_games |>
    filter(round_num == 5) |>
    transmute(
      Slot,
      top_label = team_1_label,
      bottom_label = team_2_label,
      winner_label = winner_label
    ) |>
    arrange(Slot)

  championship <- bracket_games |>
    filter(round_num == 6) |>
    transmute(
      Slot,
      top_label = team_1_label,
      bottom_label = team_2_label,
      winner_label = winner_label
    ) |>
    slice(1)

  list(
    first_round = first_round,
    round_two = round_two,
    round_three = round_three,
    round_four = round_four,
    semifinals = semifinals,
    championship = championship
  )
}

build_bracket_display_from_games_app <- function(games) {
  if (nrow(games) == 0) {
    return(NULL)
  }

  label_team <- function(seed_value, team_name) {
    if (is.na(team_name) || !nzchar(team_name)) {
      return("TBD")
    }

    trimws(paste(clean_seed_display(seed_value), team_name))
  }

  winner_team <- function(seed_value, team_name) {
    if (is.na(team_name) || !nzchar(team_name)) {
      return("TBD")
    }

    trimws(paste(clean_seed_display(seed_value), team_name))
  }

  round_one <- games |>
    filter(str_detect(Slot, "^R1")) |>
    mutate(
      region = str_extract(Slot, "(?<=R1)\\w"),
      game_index = as.integer(str_extract(Slot, "\\d$")),
      top_label = mapply(label_team, team_1_seed, team_1_name, USE.NAMES = FALSE),
      bottom_label = mapply(label_team, team_2_seed, team_2_name, USE.NAMES = FALSE),
      winner_label = mapply(winner_team, selected_team_seed, selected_team_name, USE.NAMES = FALSE)
    ) |>
    select(region, game_index, top_label, bottom_label, winner_label) |>
    arrange(region, game_index)

  winner_round <- function(prefix) {
    games |>
      filter(str_detect(Slot, paste0("^", prefix))) |>
      mutate(
        region = case_when(
          prefix == "R5" ~ str_extract(Slot, "(?<=R5)[A-Z]{2}"),
          prefix == "R6" ~ "CH",
          TRUE ~ str_extract(Slot, paste0("(?<=", prefix, ")\\w"))
        ),
        game_index = if (prefix %in% c("R5", "R6")) {
          seq_len(n())
        } else {
          as.integer(str_extract(Slot, "\\d$"))
        },
        winner_label = mapply(winner_team, selected_team_seed, selected_team_name, USE.NAMES = FALSE)
      ) |>
      select(Slot, region, game_index, winner_label) |>
      arrange(region, game_index)
  }

  list(
    first_round = round_one,
    round_two = winner_round("R2"),
    round_three = winner_round("R3"),
    round_four = winner_round("R4"),
    semifinals = winner_round("R5"),
    championship = winner_round("R6") |> slice(1)
  )
}

export_bracket_jpg <- function(bracket_display, region_lookup_frame, season, file, title, bracket_games = NULL) {
  jpeg(filename = file, width = 2400, height = 1500, quality = 100)
  grid.newpage()
  grid.rect(gp = gpar(fill = "#f3efe5", col = NA))
  grid.circle(x = 0.92, y = 0.92, r = 0.16, gp = gpar(fill = grDevices::adjustcolor("#0d7c6f", alpha.f = 0.08), col = NA))
  grid.circle(x = 0.12, y = 0.12, r = 0.18, gp = gpar(fill = grDevices::adjustcolor("#c99f4b", alpha.f = 0.1), col = NA))
  grid.roundrect(
    x = 0.5, y = 0.5, width = 0.96, height = 0.94,
    r = unit(0.03, "snpc"),
    gp = gpar(fill = grDevices::adjustcolor("#ffffff", alpha.f = 0.82), col = "#e1d7c4", lwd = 1.5)
  )

  grid.text(title, x = 0.05, y = 0.965, just = "left", gp = gpar(fontsize = 28, fontface = "bold", col = "#10253c"))
  grid.text("Classic tournament bracket export", x = 0.05, y = 0.938, just = "left", gp = gpar(fontsize = 12, fontface = "bold", col = "#0d7c6f"))

  if (is.null(bracket_display)) {
    grid.text("No bracket is available to export.", x = 0.05, y = 0.88, just = "left", gp = gpar(fontsize = 16, col = "#5d6a78"))
    dev.off()
    return(invisible(file))
  }

  region_names <- region_lookup_frame |>
    filter(season == !!season)

  label_rows <- list(
    team_top = c(2, 6, 10, 14, 18, 22, 26, 30),
    team_bottom = c(4, 8, 12, 16, 20, 24, 28, 32),
    r1 = c(3, 7, 11, 15, 19, 23, 27, 31),
    r2 = c(5, 13, 21, 29),
    r3 = c(9, 25),
    r4 = c(17)
  )

  box_label <- function(label) {
    stringr::str_trunc(ifelse(is.na(label) || !nzchar(label), "TBD", label), width = 24)
  }

  draw_box <- function(x, y, label, width = 0.105, height = 0.021, fill = "#ffffff", border = "#c8d1db", fontface = "plain") {
    grid.roundrect(
      x = x, y = y, width = width, height = height,
      r = unit(0.0025, "snpc"),
      gp = gpar(fill = fill, col = border, lwd = 1.1)
    )
    grid.text(box_label(label), x = x, y = y, gp = gpar(fontsize = 8.5, fontface = fontface, col = "#1d3146"))
  }

  draw_matchup_box <- function(x, y, team_1_label, team_2_label, selected_label = NA_character_, width = 0.13, height = 0.052) {
    winner_top <- !is.na(selected_label) && identical(selected_label, team_1_label)
    winner_bottom <- !is.na(selected_label) && identical(selected_label, team_2_label)

    grid.roundrect(
      x = x, y = y, width = width, height = height,
      r = unit(0.004, "snpc"),
      gp = gpar(fill = "#ffffff", col = "#c8d1db", lwd = 1.1)
    )
    grid.rect(
      x = x, y = y + (height * 0.18), width = width * 0.96, height = height * 0.34,
      gp = gpar(fill = if (winner_top) "#eaf4f1" else "#ffffff", col = NA)
    )
    grid.rect(
      x = x, y = y - (height * 0.18), width = width * 0.96, height = height * 0.34,
      gp = gpar(fill = if (winner_bottom) "#eaf4f1" else "#ffffff", col = NA)
    )
    grid.lines(
      x = unit(c(x - (width * 0.46), x + (width * 0.46)), "npc"),
      y = unit(c(y, y), "npc"),
      gp = gpar(col = "#dde4ec", lwd = 1)
    )
    grid.text(box_label(team_1_label), x = x, y = y + (height * 0.18), gp = gpar(fontsize = 8.2, fontface = if (winner_top) "bold" else "plain", col = "#1d3146"))
    grid.text(box_label(team_2_label), x = x, y = y - (height * 0.18), gp = gpar(fontsize = 8.2, fontface = if (winner_bottom) "bold" else "plain", col = "#1d3146"))
  }

  draw_connector <- function(source_x, target_x, y_top, y_bottom, target_y) {
    mid_x <- (source_x + target_x) / 2
    connector_gp <- gpar(col = "#c4ccd6", lwd = 1.1)
    grid.lines(x = unit(c(source_x, mid_x), "npc"), y = unit(c(y_top, y_top), "npc"), gp = connector_gp)
    grid.lines(x = unit(c(source_x, mid_x), "npc"), y = unit(c(y_bottom, y_bottom), "npc"), gp = connector_gp)
    grid.lines(x = unit(c(mid_x, mid_x), "npc"), y = unit(c(y_top, y_bottom), "npc"), gp = connector_gp)
    grid.lines(x = unit(c(mid_x, target_x), "npc"), y = unit(c(target_y, target_y), "npc"), gp = connector_gp)
  }

  draw_single_connector <- function(source_x, target_x, source_y, target_y) {
    mid_x <- (source_x + target_x) / 2
    connector_gp <- gpar(col = "#c4ccd6", lwd = 1.1)
    grid.lines(x = unit(c(source_x, mid_x), "npc"), y = unit(c(source_y, source_y), "npc"), gp = connector_gp)
    grid.lines(x = unit(c(mid_x, mid_x), "npc"), y = unit(c(source_y, target_y), "npc"), gp = connector_gp)
    grid.lines(x = unit(c(mid_x, target_x), "npc"), y = unit(c(target_y, target_y), "npc"), gp = connector_gp)
  }

  row_y <- function(row_value, y_top, region_height) {
    y_top - ((row_value - 1) / 31) * region_height
  }

  draw_region <- function(region_code, side = "left", y_top = 0.84) {
    region_name <- region_names$region_name[region_names$region == region_code][[1]]
    first_round <- bracket_display$first_round |>
      filter(region == region_code) |>
      arrange(game_index)
    round_two <- bracket_display$round_two |>
      filter(region == region_code) |>
      arrange(game_index)
    round_three <- bracket_display$round_three |>
      filter(region == region_code) |>
      arrange(game_index)
    round_four <- bracket_display$round_four |>
      filter(region == region_code) |>
      arrange(game_index)

    region_height <- 0.30
    if (identical(side, "left")) {
      centers <- c(first = 0.105, r1 = 0.195, r2 = 0.285, r3 = 0.375, r4 = 0.465)
      title_x <- 0.05
    } else {
      centers <- c(first = 0.895, r1 = 0.805, r2 = 0.715, r3 = 0.625, r4 = 0.535)
      title_x <- 0.95
    }

    box_half <- 0.105 / 2
    grid.text(
      region_name,
      x = title_x,
      y = y_top + 0.03,
      just = if (identical(side, "left")) "left" else "right",
      gp = gpar(fontsize = 14, fontface = "bold", col = "#0d7c6f")
    )

    for (i in seq_len(nrow(first_round))) {
      top_y <- row_y(label_rows$team_top[[i]], y_top, region_height)
      bottom_y <- row_y(label_rows$team_bottom[[i]], y_top, region_height)
      winner_y <- row_y(label_rows$r1[[i]], y_top, region_height)

      draw_box(centers[["first"]], top_y, first_round$top_label[[i]])
      draw_box(centers[["first"]], bottom_y, first_round$bottom_label[[i]])
      draw_box(centers[["r1"]], winner_y, first_round$winner_label[[i]], fill = "#f8fbff", fontface = "bold")

      draw_connector(
        if (identical(side, "left")) centers[["first"]] + box_half else centers[["first"]] - box_half,
        if (identical(side, "left")) centers[["r1"]] - box_half else centers[["r1"]] + box_half,
        top_y,
        bottom_y,
        winner_y
      )
    }

    for (i in seq_len(nrow(round_two))) {
      winner_y <- row_y(label_rows$r2[[i]], y_top, region_height)
      draw_box(centers[["r2"]], winner_y, round_two$winner_label[[i]], fill = "#f8fbff", fontface = "bold")
      draw_connector(
        if (identical(side, "left")) centers[["r1"]] + box_half else centers[["r1"]] - box_half,
        if (identical(side, "left")) centers[["r2"]] - box_half else centers[["r2"]] + box_half,
        row_y(label_rows$r1[[2 * i - 1]], y_top, region_height),
        row_y(label_rows$r1[[2 * i]], y_top, region_height),
        winner_y
      )
    }

    for (i in seq_len(nrow(round_three))) {
      winner_y <- row_y(label_rows$r3[[i]], y_top, region_height)
      draw_box(centers[["r3"]], winner_y, round_three$winner_label[[i]], fill = "#f8fbff", fontface = "bold")
      draw_connector(
        if (identical(side, "left")) centers[["r2"]] + box_half else centers[["r2"]] - box_half,
        if (identical(side, "left")) centers[["r3"]] - box_half else centers[["r3"]] + box_half,
        row_y(label_rows$r2[[2 * i - 1]], y_top, region_height),
        row_y(label_rows$r2[[2 * i]], y_top, region_height),
        winner_y
      )
    }

    if (nrow(round_four) == 1) {
      winner_y <- row_y(label_rows$r4[[1]], y_top, region_height)
      draw_box(centers[["r4"]], winner_y, round_four$winner_label[[1]], fill = "#f6f2e7", border = "#c7b183", fontface = "bold")
      draw_connector(
        if (identical(side, "left")) centers[["r3"]] + box_half else centers[["r3"]] - box_half,
        if (identical(side, "left")) centers[["r4"]] - box_half else centers[["r4"]] + box_half,
        row_y(label_rows$r3[[1]], y_top, region_height),
        row_y(label_rows$r3[[2]], y_top, region_height),
        winner_y
      )
    }
  }

  draw_region("W", side = "left", y_top = 0.84)
  draw_region("X", side = "left", y_top = 0.46)
  draw_region("Y", side = "right", y_top = 0.84)
  draw_region("Z", side = "right", y_top = 0.46)

  if (!is.null(bracket_games) && nrow(bracket_games) > 0) {
    semifinals <- bracket_games |>
      filter(Slot %in% c("R5WX", "R5YZ")) |>
      arrange(Slot)
    championship_game <- bracket_games |>
      filter(Slot == "R6CH") |>
      slice(1)

    format_team_line <- function(seed_value, team_name) {
      if (is.na(team_name) || !nzchar(team_name)) {
        return("TBD")
      }

      trimws(paste(clean_seed_display(seed_value), team_name))
    }

    grid.text("Final Four", x = 0.5, y = 0.58, gp = gpar(fontsize = 12, fontface = "bold", col = "#6a7684"))
    grid.text("Championship", x = 0.5, y = 0.455, gp = gpar(fontsize = 10, fontface = "bold", col = "#6a7684"))

    if (nrow(semifinals) >= 1) {
      draw_matchup_box(
        0.44, 0.53,
        format_team_line(semifinals$team_1_seed[[1]], semifinals$team_1_name[[1]]),
        format_team_line(semifinals$team_2_seed[[1]], semifinals$team_2_name[[1]]),
        format_team_line(semifinals$selected_team_seed[[1]], semifinals$selected_team_name[[1]])
      )
    }

    if (nrow(semifinals) >= 2) {
      draw_matchup_box(
        0.56, 0.53,
        format_team_line(semifinals$team_1_seed[[2]], semifinals$team_1_name[[2]]),
        format_team_line(semifinals$team_2_seed[[2]], semifinals$team_2_name[[2]]),
        format_team_line(semifinals$selected_team_seed[[2]], semifinals$selected_team_name[[2]])
      )
    }

    if (nrow(championship_game) == 1) {
      draw_matchup_box(
        0.5, 0.41,
        format_team_line(championship_game$team_1_seed[[1]], championship_game$team_1_name[[1]]),
        format_team_line(championship_game$team_2_seed[[1]], championship_game$team_2_name[[1]]),
        format_team_line(championship_game$selected_team_seed[[1]], championship_game$selected_team_name[[1]]),
        width = 0.15,
        height = 0.058
      )
    }

    draw_single_connector(0.505, 0.425, 0.53, 0.41)
    draw_single_connector(0.495, 0.575, 0.53, 0.41)
  } else {
    semifinals <- bracket_display$semifinals
    championship <- bracket_display$championship

    grid.text("Final Four", x = 0.5, y = 0.58, gp = gpar(fontsize = 12, fontface = "bold", col = "#6a7684"))
    draw_box(0.46, 0.53, if (nrow(semifinals) >= 1) semifinals$winner_label[[1]] else "TBD", width = 0.10, height = 0.026, fill = "#f8fbff", fontface = "bold")
    draw_box(0.54, 0.53, if (nrow(semifinals) >= 2) semifinals$winner_label[[2]] else "TBD", width = 0.10, height = 0.026, fill = "#f8fbff", fontface = "bold")
    draw_box(0.5, 0.42, if (nrow(championship) == 1) championship$winner_label[[1]] else "TBD", width = 0.11, height = 0.03, fill = "#f6f2e7", border = "#c7b183", fontface = "bold")
    grid.text("Champion", x = 0.5, y = 0.455, gp = gpar(fontsize = 10, fontface = "bold", col = "#6a7684"))
    draw_single_connector(0.51, 0.445, 0.53, 0.42)
    draw_single_connector(0.49, 0.555, 0.53, 0.42)
  }

  dev.off()
  invisible(file)
}

normalize_svg_text <- function(text) {
  if (length(text) == 0 || is.null(text)) {
    return("TBD")
  }

  text <- as.character(text[[1]])
  if (is.na(text) || !nzchar(text)) {
    return("TBD")
  }

  text
}

svg_escape <- function(text) {
  text <- normalize_svg_text(text)

  text <- str_replace_all(text, fixed("&"), "&amp;")
  text <- str_replace_all(text, fixed("<"), "&lt;")
  str_replace_all(text, fixed(">"), "&gt;")
}

svg_label <- function(text, width = 24) {
  svg_escape(str_trunc(normalize_svg_text(text), width = width))
}

svg_line <- function(x1, y1, x2, y2, stroke = "#cbd5e1", stroke_width = 1) {
  sprintf(
    "<line x1='%.1f' y1='%.1f' x2='%.1f' y2='%.1f' stroke='%s' stroke-width='%.1f' stroke-linecap='square' />",
    x1, y1, x2, y2, stroke, stroke_width
  )
}

svg_box <- function(x, y, width, height, label, fill = "#ffffff", stroke = "#cbd5e1", font_weight = 600, font_size = 10) {
  sprintf(
    paste0(
      "<g>",
      "<rect x='%.1f' y='%.1f' width='%.1f' height='%.1f' rx='2' ry='2' fill='%s' stroke='%s' />",
      "<text x='%.1f' y='%.1f' fill='#0f172a' font-size='%.1f' font-weight='%d' dominant-baseline='middle'>%s</text>",
      "</g>"
    ),
    x, y, width, height, fill, stroke,
    x + 6, y + (height / 2), font_size, font_weight, svg_label(label)
  )
}

svg_matchup_box <- function(x, y, width, height, top_label, bottom_label, winner_label = NA_character_) {
  winner_label <- ifelse(is.na(winner_label), "", as.character(winner_label))
  top_is_winner <- identical(as.character(top_label), winner_label)
  bottom_is_winner <- identical(as.character(bottom_label), winner_label)
  split_y <- y + (height / 2)

  paste0(
    "<g>",
    sprintf(
      "<rect x='%.1f' y='%.1f' width='%.1f' height='%.1f' rx='2' ry='2' fill='#ffffff' stroke='#cbd5e1' />",
      x, y, width, height
    ),
    sprintf(
      "<rect x='%.1f' y='%.1f' width='%.1f' height='%.1f' fill='%s' stroke='none' />",
      x + 1, y + 1, width - 2, (height / 2) - 1, if (top_is_winner) "#eef4ff" else "#ffffff"
    ),
    sprintf(
      "<rect x='%.1f' y='%.1f' width='%.1f' height='%.1f' fill='%s' stroke='none' />",
      x + 1, split_y, width - 2, (height / 2) - 1, if (bottom_is_winner) "#eef4ff" else "#ffffff"
    ),
    sprintf(
      "<line x1='%.1f' y1='%.1f' x2='%.1f' y2='%.1f' stroke='#e2e8f0' stroke-width='1' />",
      x, split_y, x + width, split_y
    ),
    sprintf(
      "<text x='%.1f' y='%.1f' fill='#0f172a' font-size='10' font-weight='%d' dominant-baseline='middle'>%s</text>",
      x + 6, y + (height * 0.25), if (top_is_winner) 700 else 600, svg_label(top_label)
    ),
    sprintf(
      "<text x='%.1f' y='%.1f' fill='#0f172a' font-size='10' font-weight='%d' dominant-baseline='middle'>%s</text>",
      x + 6, y + (height * 0.75), if (bottom_is_winner) 700 else 600, svg_label(bottom_label)
    ),
    "</g>"
  )
}

svg_bracket_connector <- function(source_x, target_x, top_y, bottom_y, target_y, stroke = "#cbd5e1") {
  mid_x <- (source_x + target_x) / 2
  c(
    svg_line(source_x, top_y, mid_x, top_y, stroke = stroke),
    svg_line(source_x, bottom_y, mid_x, bottom_y, stroke = stroke),
    svg_line(mid_x, top_y, mid_x, bottom_y, stroke = stroke),
    svg_line(mid_x, target_y, target_x, target_y, stroke = stroke)
  )
}

slot_display_order_app <- function(slot_ref) {
  if (length(slot_ref) == 0 || is.null(slot_ref)) {
    return(Inf)
  }

  slot_ref <- as.character(slot_ref[[1]])
  if (is.na(slot_ref) || !nzchar(slot_ref)) {
    return(Inf)
  }

  round_num <- suppressWarnings(as.integer(str_extract(slot_ref, "(?<=R)\\d")))
  if (is.na(round_num)) {
    return(Inf)
  }

  within_order <- case_when(
    round_num == 1 ~ match(as.integer(str_extract(slot_ref, "\\d$")), c(1, 8, 5, 4, 6, 3, 7, 2)),
    round_num == 2 ~ match(as.integer(str_extract(slot_ref, "\\d$")), c(1, 4, 3, 2)),
    round_num == 3 ~ match(as.integer(str_extract(slot_ref, "\\d$")), c(1, 2)),
    round_num == 4 ~ 1L,
    round_num == 5 ~ match(slot_ref, c("R5WX", "R5YZ")),
    round_num == 6 ~ 1L,
    TRUE ~ NA_integer_
  )

  region_key <- case_when(
    round_num <= 4 ~ str_extract(slot_ref, "(?<=R\\d)[A-Z]"),
    round_num == 5 ~ str_extract(slot_ref, "(?<=R5)[A-Z]{2}"),
    TRUE ~ "CH"
  )

  region_order <- match(region_key, c("W", "X", "Y", "Z", "WX", "YZ", "CH"))
  ifelse(is.na(within_order) || is.na(region_order), Inf, (round_num * 100) + (region_order * 10) + within_order)
}

order_slot_references_app <- function(ref_a, ref_b) {
  if (!str_detect(ref_a, "^R\\d") || !str_detect(ref_b, "^R\\d")) {
    return(c(ref_a, ref_b))
  }

  order_a <- slot_display_order_app(ref_a)
  order_b <- slot_display_order_app(ref_b)

  if (is.infinite(order_a) || is.infinite(order_b) || order_a <= order_b) {
    c(ref_a, ref_b)
  } else {
    c(ref_b, ref_a)
  }
}

build_generated_region_svg <- function(bracket_games, region_code, region_name, side = "left", origin_x = 0, origin_y = 0) {
  round_one <- bracket_games |>
    filter(round_num == 1, region == region_code) |>
    mutate(display_order = match(Slot, paste0("R1", region_code, c(1, 8, 5, 4, 6, 3, 7, 2)))) |>
    arrange(display_order)
  round_two <- bracket_games |>
    filter(round_num == 2, region == region_code) |>
    mutate(display_order = match(Slot, paste0("R2", region_code, c(1, 4, 3, 2)))) |>
    arrange(display_order)
  round_three <- bracket_games |>
    filter(round_num == 3, region == region_code) |>
    mutate(display_order = match(Slot, paste0("R3", region_code, c(1, 2)))) |>
    arrange(display_order)
  round_four <- bracket_games |>
    filter(round_num == 4, region == region_code) |>
    arrange(Slot)

  box_w <- 138
  box_h <- 44
  first_round_tops <- seq(0, by = 62, length.out = 8)
  center_y <- function(top_values) top_values + (box_h / 2)
  next_round_tops <- function(top_values) {
    centers <- center_y(top_values)
    vapply(seq(1, length(centers), by = 2), function(idx) {
      mean(centers[idx:(idx + 1)]) - (box_h / 2)
    }, numeric(1))
  }

  round_two_tops <- next_round_tops(first_round_tops)
  round_three_tops <- next_round_tops(round_two_tops)
  round_four_tops <- next_round_tops(round_three_tops)

  xs <- if (identical(side, "left")) {
    c(r1 = 0, r2 = 176, r3 = 352, r4 = 528)
  } else {
    c(r1 = 528, r2 = 352, r3 = 176, r4 = 0)
  }

  text_anchor <- if (identical(side, "left")) "" else " text-anchor='end'"
  title_x <- if (identical(side, "left")) 16 else 1764
  header_text <- c("FIRST ROUND", "SECOND ROUND", "SWEET 16", "ELITE EIGHT")
  header_y <- origin_y - 18
  header_xs <- if (identical(side, "left")) {
    origin_x + xs + (box_w / 2)
  } else {
    origin_x + xs + (box_w / 2)
  }

  elements <- c(
    sprintf(
      "<text x='%.1f' y='%.1f' fill='#0f766e' font-size='13' font-weight='700' letter-spacing='2.0'%s>%s</text>",
      title_x,
      origin_y - 58,
      text_anchor,
      svg_escape(str_to_upper(region_name))
    ),
    vapply(seq_along(header_text), function(i) {
      sprintf(
        "<text x='%.1f' y='%.1f' fill='#64748b' font-size='9' font-weight='700' letter-spacing='1.4' text-anchor='middle'>%s</text>",
        header_xs[[i]],
        header_y,
        header_text[[i]]
      )
    }, character(1))
  )

  add_round <- function(games, tops, x_key, prev_tops = NULL, prev_x_key = NULL) {
    round_elements <- character()

    for (i in seq_len(nrow(games))) {
      round_elements <- c(
        round_elements,
        svg_matchup_box(
          origin_x + xs[[x_key]],
          origin_y + tops[[i]],
          box_w,
          box_h,
          games$team_1_label[[i]],
          games$team_2_label[[i]],
          games$winner_label[[i]]
        )
      )

      if (!is.null(prev_tops) && !is.null(prev_x_key)) {
        round_elements <- c(
          round_elements,
          svg_bracket_connector(
            source_x = if (identical(side, "left")) origin_x + xs[[prev_x_key]] + box_w else origin_x + xs[[prev_x_key]],
            target_x = if (identical(side, "left")) origin_x + xs[[x_key]] else origin_x + xs[[x_key]] + box_w,
            top_y = origin_y + prev_tops[[2 * i - 1]] + (box_h / 2),
            bottom_y = origin_y + prev_tops[[2 * i]] + (box_h / 2),
            target_y = origin_y + tops[[i]] + (box_h / 2)
          )
        )
      }
    }

    round_elements
  }

  elements <- c(
    elements,
    add_round(round_one, first_round_tops, "r1"),
    add_round(round_two, round_two_tops, "r2", first_round_tops, "r1"),
    add_round(round_three, round_three_tops, "r3", round_two_tops, "r2"),
    add_round(round_four, round_four_tops, "r4", round_three_tops, "r3")
  )

  list(
    elements = elements,
    champion_edge_x = if (identical(side, "left")) origin_x + xs[["r4"]] + box_w else origin_x + xs[["r4"]],
    champion_center_y = origin_y + round_four_tops[[1]] + (box_h / 2)
  )
}

build_generated_bracket_page_ui <- function(bracket_games, region_lookup_frame, season) {
  if (nrow(bracket_games) == 0) {
    return(div(class = "section-note", "No bracket generated yet."))
  }

  region_names <- region_lookup_frame |>
    filter(season == !!season)

  region_name_for <- function(region_code) {
    region_value <- region_names$region_name[region_names$region == region_code]
    if (length(region_value) == 0 || is.na(region_value[[1]])) region_code else region_value[[1]]
  }

  left_top <- build_generated_region_svg(bracket_games, "W", region_name_for("W"), side = "left", origin_x = 40, origin_y = 100)
  left_bottom <- build_generated_region_svg(bracket_games, "X", region_name_for("X"), side = "left", origin_x = 40, origin_y = 610)
  right_top <- build_generated_region_svg(bracket_games, "Y", region_name_for("Y"), side = "right", origin_x = 1040, origin_y = 100)
  right_bottom <- build_generated_region_svg(bracket_games, "Z", region_name_for("Z"), side = "right", origin_x = 1040, origin_y = 610)

  semifinals <- bracket_games |>
    filter(round_num == 5) |>
    arrange(match(Slot, c("R5WX", "R5YZ")))
  championship <- bracket_games |>
    filter(round_num == 6) |>
    slice(1)

  semi_left <- list(x = 760, y = 440, width = 120, height = 48)
  semi_right <- list(x = 900, y = 440, width = 120, height = 48)
  title_y <- 410
  championship_box <- list(x = 830, y = 590, width = 120, height = 48)
  champion_box <- list(x = 855, y = 700, width = 110, height = 22)

  center_elements <- c(
    sprintf("<text x='820' y='%.1f' fill='#64748b' font-size='10' font-weight='700' letter-spacing='1.4'>FINAL FOUR</text>", title_y),
    sprintf("<text x='960' y='%.1f' fill='#64748b' font-size='10' font-weight='700' letter-spacing='1.4' text-anchor='end'>FINAL FOUR</text>", title_y),
    svg_matchup_box(
      semi_left$x,
      semi_left$y,
      semi_left$width,
      semi_left$height,
      if (nrow(semifinals) >= 1) semifinals$team_1_label[[1]] else "TBD",
      if (nrow(semifinals) >= 1) semifinals$team_2_label[[1]] else "TBD",
      if (nrow(semifinals) >= 1) semifinals$winner_label[[1]] else "TBD"
    ),
    svg_matchup_box(
      semi_right$x,
      semi_right$y,
      semi_right$width,
      semi_right$height,
      if (nrow(semifinals) >= 2) semifinals$team_1_label[[2]] else "TBD",
      if (nrow(semifinals) >= 2) semifinals$team_2_label[[2]] else "TBD",
      if (nrow(semifinals) >= 2) semifinals$winner_label[[2]] else "TBD"
    ),
    svg_bracket_connector(left_top$champion_edge_x, semi_left$x, left_top$champion_center_y, left_bottom$champion_center_y, semi_left$y + (semi_left$height / 2)),
    svg_bracket_connector(right_top$champion_edge_x, semi_right$x + semi_right$width, right_top$champion_center_y, right_bottom$champion_center_y, semi_right$y + (semi_right$height / 2)),
    sprintf("<text x='890' y='570' fill='#64748b' font-size='10' font-weight='700' letter-spacing='1.4' text-anchor='middle'>CHAMPIONSHIP</text>"),
    svg_matchup_box(
      championship_box$x,
      championship_box$y,
      championship_box$width,
      championship_box$height,
      if (nrow(championship) == 1) championship$team_1_label[[1]] else "TBD",
      if (nrow(championship) == 1) championship$team_2_label[[1]] else "TBD",
      if (nrow(championship) == 1) championship$winner_label[[1]] else "TBD"
    ),
    svg_bracket_connector(
      semi_left$x + semi_left$width,
      championship_box$x,
      semi_left$y + (semi_left$height / 2),
      semi_right$y + (semi_right$height / 2),
      championship_box$y + (championship_box$height / 2)
    ),
    sprintf("<text x='910' y='685' fill='#64748b' font-size='10' font-weight='700' letter-spacing='1.4' text-anchor='middle'>CHAMPION</text>"),
    svg_box(
      champion_box$x,
      champion_box$y,
      champion_box$width,
      champion_box$height,
      if (nrow(championship) == 1) championship$winner_label[[1]] else "TBD",
      stroke = "#94a3b8",
      font_weight = 700
    ),
    svg_line(
      championship_box$x + (championship_box$width / 2),
      championship_box$y + championship_box$height,
      champion_box$x + (champion_box$width / 2),
      champion_box$y,
      stroke = "#cbd5e1"
    )
  )

  svg_markup <- paste0(
    "<svg class='generated-bracket-svg' viewBox='0 0 1780 1080' preserveAspectRatio='xMidYMin meet' role='img' aria-label='Generated NCAA tournament bracket'>",
    "<rect x='1' y='1' width='1778' height='1078' rx='14' ry='14' fill='#ffffff' stroke='#e2e8f0' />",
    paste(c(left_top$elements, left_bottom$elements, right_top$elements, right_bottom$elements, center_elements), collapse = ""),
    "</svg>"
  )

  div(
    class = "generated-bracket-view",
    div(
      class = "generated-bracket-board generated-bracket-board-svg",
      HTML(svg_markup)
    )
  )
}

render_static_region_bracket_ui <- function(bracket_display, region_code, region_name, side = "left") {
  first_round <- bracket_display$first_round |>
    filter(region == region_code) |>
    arrange(game_index)
  round_two <- bracket_display$round_two |>
    filter(region == region_code) |>
    arrange(game_index)
  round_three <- bracket_display$round_three |>
    filter(region == region_code) |>
    arrange(game_index)
  round_four <- bracket_display$round_four |>
    filter(region == region_code) |>
    arrange(game_index)

  round_rows <- list(
    team_top = c(2, 6, 10, 14, 18, 22, 26, 30),
    team_bottom = c(4, 8, 12, 16, 20, 24, 28, 32),
    r1 = c(3, 7, 11, 15, 19, 23, 27, 31),
    r2 = c(5, 13, 21, 29),
    r3 = c(9, 25),
    r4 = c(17)
  )

  if (side == "left") {
    box_cols <- c(first = 1, r1 = 3, r2 = 5, r3 = 7, r4 = 9)
    connector_cols <- c(c01 = 2, c12 = 4, c23 = 6, c34 = 8)
    connector_class <- "generated-connector-left"
    header_cols <- c(first = 1, r1 = 3, r2 = 5, r3 = 7)
  } else {
    box_cols <- c(first = 9, r1 = 7, r2 = 5, r3 = 3, r4 = 1)
    connector_cols <- c(c01 = 8, c12 = 6, c23 = 4, c34 = 2)
    connector_class <- "generated-connector-right"
    header_cols <- c(first = 9, r1 = 7, r2 = 5, r3 = 3)
  }

  box_ui <- function(label, grid_row, grid_col, extra_class = NULL) {
    div(
      class = paste("generated-bracket-box", extra_class),
      style = paste0("grid-column:", grid_col, ";grid-row:", grid_row, ";"),
      label
    )
  }

  connector_ui <- function(grid_col, row_start, row_end) {
    div(
      class = paste("generated-connector", connector_class),
      style = paste0("grid-column:", grid_col, ";grid-row:", row_start, " / ", row_end, ";")
    )
  }

  first_round_nodes <- unlist(
    lapply(seq_len(nrow(first_round)), function(i) {
      list(
        box_ui(first_round$top_label[[i]], round_rows$team_top[[i]], box_cols[["first"]], "generated-seed-box"),
        box_ui(first_round$bottom_label[[i]], round_rows$team_bottom[[i]], box_cols[["first"]], "generated-seed-box"),
        box_ui(first_round$winner_label[[i]], round_rows$r1[[i]], box_cols[["r1"]], "generated-winner-box"),
        connector_ui(connector_cols[["c01"]], round_rows$team_top[[i]], round_rows$team_bottom[[i]])
      )
    }),
    recursive = FALSE
  )

  round_two_nodes <- unlist(
    lapply(seq_len(nrow(round_two)), function(i) {
      list(
        box_ui(round_two$winner_label[[i]], round_rows$r2[[i]], box_cols[["r2"]], "generated-winner-box"),
        connector_ui(connector_cols[["c12"]], round_rows$r1[[2 * i - 1]], round_rows$r1[[2 * i]])
      )
    }),
    recursive = FALSE
  )

  round_three_nodes <- unlist(
    lapply(seq_len(nrow(round_three)), function(i) {
      list(
        box_ui(round_three$winner_label[[i]], round_rows$r3[[i]], box_cols[["r3"]], "generated-winner-box"),
        connector_ui(connector_cols[["c23"]], round_rows$r2[[2 * i - 1]], round_rows$r2[[2 * i]])
      )
    }),
    recursive = FALSE
  )

  round_four_nodes <- if (nrow(round_four) == 1) {
    list(
      box_ui(round_four$winner_label[[1]], round_rows$r4[[1]], box_cols[["r4"]], "generated-winner-box generated-region-champ"),
      connector_ui(connector_cols[["c34"]], round_rows$r3[[1]], round_rows$r3[[2]])
    )
  } else {
    list()
  }

  div(
    class = paste("generated-region", side),
    div(class = "generated-region-title", region_name),
    div(
      class = paste("generated-region-grid", side),
      div(class = "generated-round-head", style = paste0("grid-column:", header_cols[["first"]], ";grid-row:1;"), "First Round"),
      div(class = "generated-round-head", style = paste0("grid-column:", header_cols[["r1"]], ";grid-row:1;"), "Second Round"),
      div(class = "generated-round-head", style = paste0("grid-column:", header_cols[["r2"]], ";grid-row:1;"), "Sweet 16"),
      div(class = "generated-round-head", style = paste0("grid-column:", header_cols[["r3"]], ";grid-row:1;"), "Elite Eight"),
      c(first_round_nodes, round_two_nodes, round_three_nodes, round_four_nodes)
    )
  )
}

render_static_final_four_ui <- function(bracket_display) {
  normalize_label <- function(label) {
    if (length(label) == 0 || is.na(label) || !nzchar(label)) "TBD" else label
  }

  matchup_box_ui <- function(team_1_label, team_2_label, winner_label = NA_character_, extra_class = NULL) {
    team_1_label <- normalize_label(team_1_label)
    team_2_label <- normalize_label(team_2_label)
    winner_label <- normalize_label(winner_label)

    div(
      class = paste("generated-final-matchup", extra_class),
      div(
        class = paste("generated-final-team", if (identical(team_1_label, winner_label)) "is-winner"),
        team_1_label
      ),
      div(
        class = paste("generated-final-team", if (identical(team_2_label, winner_label)) "is-winner"),
        team_2_label
      )
    )
  }

  region_champs <- bracket_display$round_four |>
    arrange(region)
  semifinals <- bracket_display$semifinals |>
    arrange(Slot)
  championship <- bracket_display$championship |>
    slice(1)

  w_champ <- region_champs |>
    filter(region == "W") |>
    pull(winner_label) |>
    first()
  x_champ <- region_champs |>
    filter(region == "X") |>
    pull(winner_label) |>
    first()
  y_champ <- region_champs |>
    filter(region == "Y") |>
    pull(winner_label) |>
    first()
  z_champ <- region_champs |>
    filter(region == "Z") |>
    pull(winner_label) |>
    first()

  semi_left_winner <- semifinals |>
    filter(Slot == "R5WX") |>
    pull(winner_label) |>
    first()
  semi_right_winner <- semifinals |>
    filter(Slot == "R5YZ") |>
    pull(winner_label) |>
    first()
  champion_label <- championship |>
    pull(winner_label) |>
    first() |>
    normalize_label()

  div(
    class = "generated-final-four",
    div(
      class = "generated-final-grid",
      div(class = "generated-round-head generated-final-head generated-final-head-left", "Semifinal"),
      div(class = "generated-round-head generated-final-head generated-final-head-champ", "Championship"),
      div(class = "generated-round-head generated-final-head generated-final-head-right", "Semifinal"),
      div(
        class = "generated-final-slot generated-final-slot-left",
        matchup_box_ui(w_champ, x_champ, semi_left_winner, "generated-final-box-left")
      ),
      div(
        class = "generated-final-slot generated-final-slot-right",
        matchup_box_ui(y_champ, z_champ, semi_right_winner, "generated-final-box-right")
      ),
      div(class = "generated-final-connector generated-final-connector-left"),
      div(class = "generated-final-connector generated-final-connector-right"),
      div(
        class = "generated-final-slot generated-final-slot-championship",
        matchup_box_ui(semi_left_winner, semi_right_winner, champion_label, "generated-final-box-championship")
      ),
      div(class = "generated-final-connector generated-final-connector-down"),
      div(class = "generated-championship-label", "Champion"),
      div(class = "generated-bracket-box generated-champion-box generated-final-champion", champion_label)
    )
  )
}

interactive_matchup_picker_ui <- function(game_row, extra_class = NULL, read_only = FALSE) {
  input_id <- paste0("pick_", game_row$Slot[[1]])
  team_1_available <- !is.na(game_row$team_1[[1]])
  team_2_available <- !is.na(game_row$team_2[[1]])
  matchup_ready <- team_1_available && team_2_available
  partial_matchup <- xor(team_1_available, team_2_available)
  selected_team_id <- suppressWarnings(as.integer(game_row$selected_team_id[[1]]))

  pick_choice_ui <- function(team_id, seed_value, team_name, probability) {
    is_selected <- !is.na(selected_team_id) && identical(as.integer(team_id), selected_team_id)
    seed_display <- if (str_detect(game_row$Slot[[1]], "^R1")) clean_seed_display(seed_value) else ""

    tags$div(
      class = paste("bracket-pick-choice", if (is_selected) "is-selected"),
      tags$span(
        class = "bracket-pick-prob",
        style = paste0("background:", interpolate_heat_color(probability, 0, 1), ";"),
        format_probability(probability)
      ),
      tags$span(
        class = "bracket-pick-team",
        paste(trimws(paste(seed_display, team_name)), collapse = " ")
      )
    )
  }

  div(
    class = paste("bracket-team-box bracket-winner-box bracket-interactive-box", extra_class),
    if (matchup_ready) {
      if (isTRUE(read_only)) {
        div(
          class = "bracket-pick-stack",
          div(
            class = "bracket-pick-options bracket-pick-options-static",
            pick_choice_ui(
              team_id = game_row$team_1[[1]],
              seed_value = game_row$team_1_seed[[1]],
              team_name = game_row$team_1_name[[1]],
              probability = game_row$team_1_probability[[1]]
            ),
            pick_choice_ui(
              team_id = game_row$team_2[[1]],
              seed_value = game_row$team_2_seed[[1]],
              team_name = game_row$team_2_name[[1]],
              probability = game_row$team_2_probability[[1]]
            )
          )
        )
      } else {
        div(
          class = "bracket-pick-stack",
          div(
            class = "bracket-pick-options",
            radioButtons(
              inputId = input_id,
              label = NULL,
              choiceNames = list(
                pick_choice_ui(
                  team_id = game_row$team_1[[1]],
                  seed_value = game_row$team_1_seed[[1]],
                  team_name = game_row$team_1_name[[1]],
                  probability = game_row$team_1_probability[[1]]
                ),
                pick_choice_ui(
                  team_id = game_row$team_2[[1]],
                  seed_value = game_row$team_2_seed[[1]],
                  team_name = game_row$team_2_name[[1]],
                  probability = game_row$team_2_probability[[1]]
                )
              ),
              choiceValues = as.character(c(game_row$team_1[[1]], game_row$team_2[[1]])),
              selected = if (!is.na(selected_team_id)) as.character(selected_team_id) else character(0)
            )
          )
        )
      }
    } else {
      tagList(
        div(class = "bracket-pick-round", game_row$round_label[[1]]),
        div(
          class = "bracket-pending-box",
          if (partial_matchup) {
            tagList(
              div(class = "bracket-pending-team", coalesce(game_row$team_1_name[[1]], game_row$team_2_name[[1]])),
              div(class = "bracket-pending-note", "Waiting on opponent")
            )
          } else {
            "Waiting on prior pick"
          }
        )
      )
    }
  )
}

render_interactive_region_bracket_ui <- function(games, region_code, region_name, side = "left", read_only = FALSE) {
  round_rows <- list(
    r1 = c(1, 3, 5, 7, 9, 11, 13, 15),
    r2 = c(2, 6, 10, 14),
    r3 = c(4, 12),
    r4 = c(8)
  )

  round_data <- list(
    r1 = games |>
      filter(str_detect(Slot, paste0("^R1", region_code))) |>
      arrange(slot_sort_value(Slot)),
    r2 = games |>
      filter(str_detect(Slot, paste0("^R2", region_code))) |>
      arrange(slot_sort_value(Slot)),
    r3 = games |>
      filter(str_detect(Slot, paste0("^R3", region_code))) |>
      arrange(slot_sort_value(Slot)),
    r4 = games |>
      filter(str_detect(Slot, paste0("^R4", region_code))) |>
      arrange(slot_sort_value(Slot))
  )

  if (side == "left") {
    box_cols <- c(r1 = 1, r2 = 3, r3 = 5, r4 = 7)
    connector_cols <- c(c12 = 2, c23 = 4, c34 = 6)
    connector_class <- "connector-left"
  } else {
    box_cols <- c(r1 = 7, r2 = 5, r3 = 3, r4 = 1)
    connector_cols <- c(c12 = 6, c23 = 4, c34 = 2)
    connector_class <- "connector-right"
  }

  picker_box_ui <- function(grid_row, grid_col, row, extra_class = NULL) {
    div(
      style = paste0("grid-column:", grid_col, ";grid-row:", grid_row, ";"),
      interactive_matchup_picker_ui(row, extra_class = extra_class, read_only = read_only)
    )
  }

  connector_ui <- function(grid_col, row_start, row_end) {
    div(
      class = paste("bracket-connector", connector_class),
      style = paste0("grid-column:", grid_col, ";grid-row:", row_start, " / ", row_end, ";")
    )
  }

  box_nodes <- c(
    lapply(seq_len(nrow(round_data$r1)), function(i) picker_box_ui(
      round_rows$r1[[i]],
      box_cols[["r1"]],
      round_data$r1[i, ],
      if (i %% 2 == 1) "first-round-top" else "first-round-bottom"
    )),
    lapply(seq_len(nrow(round_data$r2)), function(i) picker_box_ui(round_rows$r2[[i]], box_cols[["r2"]], round_data$r2[i, ])),
    lapply(seq_len(nrow(round_data$r3)), function(i) picker_box_ui(round_rows$r3[[i]], box_cols[["r3"]], round_data$r3[i, ])),
    lapply(seq_len(nrow(round_data$r4)), function(i) picker_box_ui(round_rows$r4[[i]], box_cols[["r4"]], round_data$r4[i, ]))
  )

  connector_nodes <- c(
    lapply(seq(1, 8, by = 2), function(i) connector_ui(connector_cols[["c12"]], round_rows$r1[[i]], round_rows$r1[[i + 1]])),
    lapply(seq(1, 4, by = 2), function(i) connector_ui(connector_cols[["c23"]], round_rows$r2[[i]], round_rows$r2[[i + 1]])),
    lapply(seq(1, 2, by = 2), function(i) connector_ui(connector_cols[["c34"]], round_rows$r3[[i]], round_rows$r3[[i + 1]]))
  )

  div(
    class = paste("region-bracket", side),
    div(class = "region-title", region_name),
    div(
      class = paste("region-grid", side),
      div(class = "round-title round-head round-head-r1", "First Round"),
      div(class = "round-title round-head round-head-r2", "Second Round"),
      div(class = "round-title round-head round-head-r3", "Sweet 16"),
      div(class = "round-title round-head round-head-r4", "Elite Eight"),
      c(box_nodes, connector_nodes)
    )
  )
}

render_interactive_final_four_ui <- function(games, read_only = FALSE) {
  semis <- games |>
    filter(Slot %in% c("R5WX", "R5YZ")) |>
    arrange(Slot)
  championship <- games |>
    filter(Slot == "R6CH")

  div(
    class = "final-four-bracket",
    div(
      class = "final-four-grid",
      div(class = "round-title final-head final-head-left", "Final Four"),
      div(class = "round-title final-head final-head-champ", "Championship"),
      div(class = "round-title final-head final-head-right", "Final Four"),
      div(
        class = "center-box final-semi final-semi-left",
        if (nrow(semis) >= 1) interactive_matchup_picker_ui(semis[1, ], extra_class = "center-pick-box", read_only = read_only)
      ),
      div(
        class = "center-box final-semi final-semi-right",
        if (nrow(semis) >= 2) interactive_matchup_picker_ui(semis[2, ], extra_class = "center-pick-box", read_only = read_only)
      ),
      div(class = "final-connector final-connector-left"),
      div(class = "final-connector final-connector-right"),
      div(class = "final-connector final-connector-down"),
      div(class = "championship-label", "Champion"),
      div(
        class = "final-champion",
        if (nrow(championship) == 1) interactive_matchup_picker_ui(championship[1, ], extra_class = "center-pick-box", read_only = read_only)
      )
    )
  )
}

interactive_bracket_ui <- function(games, region_lookup_frame, season, read_only = FALSE) {
  region_names <- region_lookup_frame |>
    filter(season == !!season)

  div(
    class = "interactive-bracket-scroll",
    div(
      class = "bracket-shell",
      div(
        class = "bracket-side",
        render_interactive_region_bracket_ui(games, "W", region_names$region_name[region_names$region == "W"][[1]], side = "left", read_only = read_only),
        render_interactive_region_bracket_ui(games, "X", region_names$region_name[region_names$region == "X"][[1]], side = "left", read_only = read_only)
      ),
      div(
        class = "bracket-center",
        render_interactive_final_four_ui(games, read_only = read_only)
      ),
      div(
        class = "bracket-side",
        render_interactive_region_bracket_ui(games, "Y", region_names$region_name[region_names$region == "Y"][[1]], side = "right", read_only = read_only),
        render_interactive_region_bracket_ui(games, "Z", region_names$region_name[region_names$region == "Z"][[1]], side = "right", read_only = read_only)
      )
    )
  )
}

slot_sort_value <- function(slot) {
  round_num <- as.integer(str_extract(slot, "(?<=R)\\d"))
  seed_num <- suppressWarnings(as.integer(str_extract(slot, "\\d$")))
  ifelse(is.na(seed_num), 0L, round_num * 100L + seed_num)
}

render_region_bracket_ui <- function(bracket_data, region_code, region_name, side = "left") {
  round_rows <- list(
    r1 = c(1, 3, 5, 7, 9, 11, 13, 15),
    r2 = c(2, 6, 10, 14),
    r3 = c(4, 12),
    r4 = c(8)
  )

  round_data <- list(
    r1 = bracket_data |>
      filter(str_detect(Slot, paste0("^R1", region_code))) |>
      arrange(slot_sort_value(Slot)),
    r2 = bracket_data |>
      filter(str_detect(Slot, paste0("^R2", region_code))) |>
      arrange(slot_sort_value(Slot)),
    r3 = bracket_data |>
      filter(str_detect(Slot, paste0("^R3", region_code))) |>
      arrange(slot_sort_value(Slot)),
    r4 = bracket_data |>
      filter(str_detect(Slot, paste0("^R4", region_code))) |>
      arrange(slot_sort_value(Slot))
  )

  if (side == "left") {
    box_cols <- c(r1 = 1, r2 = 3, r3 = 5, r4 = 7)
    connector_cols <- c(c12 = 2, c23 = 4, c34 = 6)
    connector_class <- "connector-left"
  } else {
    box_cols <- c(r1 = 7, r2 = 5, r3 = 3, r4 = 1)
    connector_cols <- c(c12 = 6, c23 = 4, c34 = 2)
    connector_class <- "connector-right"
  }

  team_box_ui <- function(grid_row, grid_col, row, extra_class = NULL) {
    seed_display <- if (str_detect(row$Slot[[1]], "^R1")) clean_seed_display(row$Team[[1]]) else ""

    div(
      class = paste("bracket-team-box bracket-winner-box", extra_class),
      style = paste0("grid-column:", grid_col, ";grid-row:", grid_row, ";"),
      if (nzchar(seed_display)) div(class = "bracket-seed", seed_display),
      div(class = "bracket-team", row$team_name[[1]])
    )
  }

  connector_ui <- function(grid_col, row_start, row_end) {
    div(
      class = paste("bracket-connector", connector_class),
      style = paste0("grid-column:", grid_col, ";grid-row:", row_start, " / ", row_end, ";")
    )
  }

  box_nodes <- c(
    lapply(seq_len(nrow(round_data$r1)), function(i) team_box_ui(
      round_rows$r1[[i]],
      box_cols[["r1"]],
      round_data$r1[i, ],
      if (i %% 2 == 1) "first-round-top" else "first-round-bottom"
    )),
    lapply(seq_len(nrow(round_data$r2)), function(i) team_box_ui(round_rows$r2[[i]], box_cols[["r2"]], round_data$r2[i, ])),
    lapply(seq_len(nrow(round_data$r3)), function(i) team_box_ui(round_rows$r3[[i]], box_cols[["r3"]], round_data$r3[i, ])),
    lapply(seq_len(nrow(round_data$r4)), function(i) team_box_ui(round_rows$r4[[i]], box_cols[["r4"]], round_data$r4[i, ]))
  )

  connector_nodes <- c(
    lapply(seq(1, 8, by = 2), function(i) connector_ui(connector_cols[["c12"]], round_rows$r1[[i]], round_rows$r1[[i + 1]])),
    lapply(seq(1, 4, by = 2), function(i) connector_ui(connector_cols[["c23"]], round_rows$r2[[i]], round_rows$r2[[i + 1]])),
    lapply(seq(1, 2, by = 2), function(i) connector_ui(connector_cols[["c34"]], round_rows$r3[[i]], round_rows$r3[[i + 1]]))
  )

  div(
    class = paste("region-bracket", side),
    div(class = "region-title", region_name),
    div(
      class = paste("region-grid", side),
      div(class = "round-title round-head round-head-r1", "First Round"),
      div(class = "round-title round-head round-head-r2", "Second Round"),
      div(class = "round-title round-head round-head-r3", "Sweet 16"),
      div(class = "round-title round-head round-head-r4", "Elite Eight"),
      c(box_nodes, connector_nodes)
    )
  )
}

render_final_four_ui <- function(bracket_data) {
  semis <- bracket_data |>
    filter(Slot %in% c("R5WX", "R5YZ")) |>
    arrange(Slot)
  championship <- bracket_data |>
    filter(Slot == "R6CH")

  div(
    class = "final-four-bracket",
    div(class = "region-title center-title", "Final Four"),
    div(
      class = "final-four-grid",
      div(class = "round-title final-head final-head-left", "National Semifinal"),
      div(class = "round-title final-head final-head-champ", "National Championship"),
      div(class = "round-title final-head final-head-right", "National Semifinal"),
      div(
        class = "bracket-team-box bracket-winner-box center-box final-semi final-semi-left",
        if (nrow(semis) >= 1) {
          tagList(
            div(class = "bracket-team", semis$team_name[[1]])
          )
        }
      ),
      div(
        class = "bracket-team-box bracket-winner-box center-box final-semi final-semi-right",
        if (nrow(semis) >= 2) {
          tagList(
            div(class = "bracket-team", semis$team_name[[2]])
          )
        }
      ),
      div(class = "final-connector final-connector-left"),
      div(class = "final-connector final-connector-right"),
      div(class = "final-connector final-connector-down"),
      div(class = "championship-label", "Champion"),
      div(
        class = "bracket-team-box champion-box bracket-winner-box final-champion",
        if (nrow(championship) == 1) {
          tagList(
            div(class = "bracket-team", championship$team_name[[1]])
          )
        }
      )
    )
  )
}

predictions_all <- bind_rows(
  load_predictions(PREDICTION_FILES$mens, "mens"),
  load_predictions(PREDICTION_FILES$womens, "womens")
)

available_prediction_years <- predictions_all |>
  distinct(season) |>
  arrange(desc(season)) |>
  pull(season)

if (length(available_prediction_years) == 0) {
  available_prediction_years <- TARGET_SEASON
}

default_year <- if (2025 %in% available_prediction_years) 2025 else available_prediction_years[[1]]

tab_stage_ui <- function(kicker, title, description, note_output_id, content, controls = NULL) {
  div(
    class = "tab-stage",
    div(
      class = "tab-stage-toolbar",
      div(class = "tab-stage-note", textOutput(note_output_id)),
      if (!is.null(controls)) div(class = "tab-stage-controls", controls)
    ),
    div(class = "tab-stage-content", content)
  )
}

ui <- fluidPage(
  class = "app-shell",
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    includeCSS("www/theme.css")
  ),
  div(
    class = "container-fluid app-frame",
    div(
      class = "app-drawer",
      tags$input(id = "control_drawer_toggle", type = "checkbox", class = "drawer-toggle"),
      tags$label(
        `for` = "control_drawer_toggle",
        class = "drawer-button",
        tags$span(class = "drawer-button-icon", HTML("&#9776;")),
        tags$span(
          class = "drawer-button-copy",
          tags$span(class = "drawer-button-label", "Menu"),
          tags$span(class = "drawer-button-meta", "Filters and export")
        )
      ),
      tags$label(`for` = "control_drawer_toggle", class = "drawer-backdrop"),
      div(
        class = "drawer-panel",
        div(
          class = "drawer-panel-header",
          div(
            class = "drawer-panel-copy",
            div(class = "drawer-panel-kicker", "March Madness Models"),
            div(class = "drawer-panel-title", "Controls")
          ),
          tags$label(`for` = "control_drawer_toggle", class = "drawer-close", "Close")
        ),
        div(
          class = "sidebar-card drawer-card",
          div(class = "sidebar-kicker", "Control Center"),
          div(class = "sidebar-title", "Tournament filters"),
          tags$p(
            class = "sidebar-copy",
            "Set the division, season, and team context here, then export the current view whenever you want a presentation-ready image."
          ),
          div(class = "control-section-title", "Core filters"),
          selectInput(
            "division",
            "Division",
            choices = c("Mens" = "mens", "Womens" = "womens"),
            selected = "mens"
          ),
          selectInput(
            "season",
            "Tournament season",
            choices = available_prediction_years,
            selected = default_year
          ),
          conditionalPanel(
            condition = "input.main_tab == 'Team Summary'",
            div(
              div(class = "control-section-title", "Team context"),
              selectInput(
                "team_summary_team",
                "Team summary team",
                choices = list()
              )
            )
          ),
          div(class = "control-section-title", "Export"),
          downloadButton("export_jpg", "Export Current Tab JPG", class = "download-cta"),
          helpText("Use 2025 now, then switch the pipeline and app to 2026 once the official bracket is announced.")
        )
      )
    ),
    div(
      class = "app-workspace",
      div(
        class = "content-shell",
        div(
          class = "content-card main-tabset",
          tabsetPanel(
            id = "main_tab",
            tabPanel(
              "First Round",
              tab_stage_ui(
                kicker = "Opening slate",
                title = "First-round matchup probabilities",
                description = "Scan every opening game in a card-driven layout that highlights likely winners, upset pressure points, and historical lower-seed context at a glance.",
                note_output_id = "first_round_note",
                content = uiOutput("first_round_cards")
              )
            ),
            tabPanel(
              "Custom Matchups",
              tab_stage_ui(
                kicker = "Scenario builder",
                title = "Build custom head-to-head comparisons",
                description = "Filter the tournament field by region, compare any two seeded teams, and stack your own what-if board for side-by-side probability reads.",
                note_output_id = "custom_note",
                controls = div(
                  class = "control-box",
                  fluidRow(
                    column(
                      width = 6,
                      selectInput("custom_region_a", "Team A region", choices = "All", selected = "All")
                    ),
                    column(
                      width = 6,
                      selectInput("custom_region_b", "Team B region", choices = "All", selected = "All")
                    )
                  ),
                  fluidRow(
                    column(
                      width = 6,
                      selectInput("custom_team_a", "Team A", choices = list())
                    ),
                    column(
                      width = 6,
                      selectInput("custom_team_b", "Team B", choices = list())
                    )
                  ),
                  fluidRow(
                    column(width = 4, actionButton("add_matchup", "Add Matchup", class = "action-cta")),
                    column(width = 4, actionButton("clear_matchups", "Clear Matchups", class = "secondary-cta"))
                  )
                ),
                content = uiOutput("custom_matchup_cards")
              )
            ),
            tabPanel(
              "Bracket Generator",
              tab_stage_ui(
                kicker = "Simulation draw",
                title = "Generate a saved simulated bracket",
                description = "Pull a complete bracket from the simulation pool and review the field in a cleaner bracket canvas with a more premium presentation.",
                note_output_id = "bracket_note",
                controls = div(
                  class = "control-box interactive-actions",
                  actionButton("generate_bracket", "Generate Bracket", class = "action-cta")
                ),
                content = uiOutput("bracket_view")
              )
            ),
            tabPanel(
              "Fill Out Bracket",
              tab_stage_ui(
                kicker = "Interactive picks",
                title = "Fill out the bracket with cascading selections",
                description = "Choose winners one game at a time, watch each pick advance automatically, and use the refreshed bracket UI to keep the path easy to read.",
                note_output_id = "fill_bracket_note",
                controls = div(
                  class = "control-box interactive-actions",
                  actionButton("clear_filled_bracket", "Clear Picks", class = "secondary-cta")
                ),
                content = uiOutput("fill_bracket_view")
              )
            ),
            tabPanel(
              "Simulation Probabilities",
              tab_stage_ui(
                kicker = "Round advancement",
                title = "Track how often each team survives each round",
                description = "Review the saved simulation outputs in a wider, calmer table treatment designed for fast comparison across the entire tournament field.",
                note_output_id = "simulation_note",
                content = uiOutput("simulation_probabilities_view")
              )
            ),
            tabPanel(
              "Team Summary",
              tab_stage_ui(
                kicker = "Team dossier",
                title = "Deep dive into a single team's season profile",
                description = "Inspect efficiency-style metrics, percentile indicators, and the best and worst results on the schedule in a more editorial team-summary layout.",
                note_output_id = "team_summary_note",
                content = uiOutput("team_summary_view")
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  custom_matchups <- reactiveVal(tibble())
  generated_bracket <- reactiveVal(tibble())

  tournament_code <- reactive({
    if (input$division == "mens") "M" else "W"
  })

  division_predictions <- reactive({
    predictions_all |>
      filter(division == input$division, season == as.integer(input$season))
  })

  bracket_seed_table <- reactive({
    build_bracket_seed_table_app(
      seed_data = seed_data(),
      team_lookup = team_lookup(),
      region_lookup = region_lookup(),
      season = as.integer(input$season),
      tournament_code = tournament_code()
    )
  })

  bracket_probability_lookup <- reactive({
    build_bracket_probability_lookup_app(division_predictions())
  })

  bracket_pool <- reactive({
    load_bracket_pool(as.integer(input$season)) |>
      filter(Tournament == tournament_code())
  })

  round_probabilities <- reactive({
    load_round_probabilities(as.integer(input$season)) |>
      filter(Tournament == tournament_code())
  })

  seed_data <- reactive({
    load_seed_data(input$division)
  })

  team_lookup <- reactive({
    load_team_lookup(input$division)
  })

  region_lookup <- reactive({
    load_region_lookup(input$division)
  })

  tournament_slots <- reactive({
    load_tournament_slots(input$division)
  })

  historical_seed_rates <- reactive({
    load_historical_seed_rates(input$division)
  })

  regular_season_results <- reactive({
    load_regular_season_detailed_results(input$division)
  })

  team_game_log <- reactive({
    build_team_game_log(
      results = regular_season_results(),
      team_lookup = team_lookup(),
      season = as.integer(input$season)
    )
  })

  team_summary_table <- reactive({
    build_team_summary_table(
      game_log = team_game_log(),
      seed_data = seed_data(),
      region_lookup = region_lookup(),
      season = as.integer(input$season)
    )
  })

  available_regions <- reactive({
    filter(region_lookup(), season == as.integer(input$season)) |>
      distinct(region_name) |>
      arrange(region_name) |>
      pull(region_name)
  })

  available_summary_teams <- reactive({
    team_summary_table() |>
      distinct(team_id, team_name) |>
      arrange(team_name)
  })

  team_summary_data <- reactive({
    team_id <- suppressWarnings(as.integer(input$team_summary_team))
    if (is.na(team_id)) {
      return(NULL)
    }

    build_team_summary(
      game_log = team_game_log(),
      summary_table = team_summary_table(),
      team_id = team_id,
      season = as.integer(input$season)
    )
  })

  available_bracket_ids <- reactive({
    bracket_pool() |>
      distinct(Bracket) |>
      arrange(Bracket) |>
      pull(Bracket)
  })

  observe({
    region_choices <- c("All", available_regions())
    updateSelectInput(session, "custom_region_a", choices = region_choices, selected = "All")
    updateSelectInput(session, "custom_region_b", choices = region_choices, selected = "All")
  })

  observe({
    summary_teams <- available_summary_teams()
    team_choices <- setNames(as.list(as.character(summary_teams$team_id)), summary_teams$team_name)
    selected_team <- isolate(input$team_summary_team)
    if (!length(team_choices)) {
      team_choices <- list()
      selected_team <- character(0)
    } else if (is.null(selected_team) || !selected_team %in% unlist(team_choices, use.names = FALSE)) {
      selected_team <- unlist(team_choices, use.names = FALSE)[[1]]
    }

    updateSelectInput(session, "team_summary_team", choices = team_choices, selected = selected_team)
  })

  observe({
    team_choices_a <- build_seed_choices(
      seed_data = seed_data(),
      team_lookup = team_lookup(),
      region_lookup = region_lookup(),
      season = as.integer(input$season),
      region_filter = input$custom_region_a
    )

    choice_list_a <- setNames(as.list(as.character(team_choices_a$team_id)), team_choices_a$choice_label)
    updateSelectInput(session, "custom_team_a", choices = choice_list_a)
  })

  observe({
    team_choices_b <- build_seed_choices(
      seed_data = seed_data(),
      team_lookup = team_lookup(),
      region_lookup = region_lookup(),
      season = as.integer(input$season),
      region_filter = input$custom_region_b
    )

    choice_list_b <- setNames(as.list(as.character(team_choices_b$team_id)), team_choices_b$choice_label)
    updateSelectInput(session, "custom_team_b", choices = choice_list_b)
  })

  observeEvent(list(input$division, input$season), {
    custom_matchups(tibble())
    generated_bracket(tibble())
  }, ignoreInit = TRUE)

  filled_bracket_games <- reactive({
    req(nrow(division_predictions()) > 0)

    pick_slots <- paste0("pick_", c(
      paste0("R1", rep(c("W", "X", "Y", "Z"), each = 8), rep(1:8, times = 4)),
      paste0("R2", rep(c("W", "X", "Y", "Z"), each = 4), rep(1:4, times = 4)),
      paste0("R3", rep(c("W", "X", "Y", "Z"), each = 2), rep(1:2, times = 4)),
      paste0("R4", rep(c("W", "X", "Y", "Z"), each = 1), rep(1, times = 4)),
      c("R5WX", "R5YZ", "R6CH")
    ))
    selections <- lapply(pick_slots, function(slot) input[[slot]])
    names(selections) <- str_remove(pick_slots, "^pick_")

    build_interactive_bracket_games_app(
      seed_table = bracket_seed_table(),
      probability_lookup = bracket_probability_lookup(),
      selections = selections
    )
  })

  generated_bracket_games <- reactive({
    if (nrow(generated_bracket()) == 0) {
      return(tibble())
    }

    build_saved_bracket_games_app(
      seed_table = bracket_seed_table(),
      bracket_results = generated_bracket(),
      slots_frame = tournament_slots(),
      season = as.integer(input$season)
    )
  })

  generated_bracket_display <- reactive({
    if (nrow(generated_bracket()) == 0) {
      return(NULL)
    }

    build_saved_bracket_display_app(
      seed_table = bracket_seed_table(),
      bracket_results = generated_bracket(),
      slots_frame = tournament_slots(),
      season = as.integer(input$season)
    )
  })

  filled_bracket_display <- reactive({
    build_bracket_display_from_games_app(filled_bracket_games())
  })

  observeEvent(input$clear_matchups, {
    custom_matchups(tibble())
  })

  observeEvent(input$clear_filled_bracket, {
    all_slots <- c(
      paste0("R1", rep(c("W", "X", "Y", "Z"), each = 8), rep(1:8, times = 4)),
      paste0("R2", rep(c("W", "X", "Y", "Z"), each = 4), rep(1:4, times = 4)),
      paste0("R3", rep(c("W", "X", "Y", "Z"), each = 2), rep(1:2, times = 4)),
      paste0("R4", c("W1", "X1", "Y1", "Z1")),
      "R5WX",
      "R5YZ",
      "R6CH"
    )

    lapply(all_slots, function(slot) {
      updateRadioButtons(session, paste0("pick_", slot), selected = character(0))
    })
  })

  observeEvent(input$generate_bracket, {
    if (nrow(bracket_pool()) == 0) {
      showNotification("No pre-generated brackets exist yet for that season and division. Run the bracket stage first.", type = "warning")
      return()
    }

    selected_bracket <- sample(available_bracket_ids(), size = 1)

    bracket_frame <- bracket_pool() |>
      filter(Bracket == selected_bracket)

    generated_bracket(bracket_frame)
  })

  observeEvent(input$add_matchup, {
    if (nrow(division_predictions()) == 0) {
      showNotification("No saved predictions exist yet for that season and division.", type = "warning")
      return()
    }

    team_a_id <- suppressWarnings(as.integer(input$custom_team_a))
    team_b_id <- suppressWarnings(as.integer(input$custom_team_b))

    if (is.na(team_a_id) || is.na(team_b_id)) {
      showNotification("Pick both teams before adding a matchup.", type = "warning")
      return()
    }

    if (team_a_id == team_b_id) {
      showNotification("Choose two different teams.", type = "warning")
      return()
    }

    seed_choices_all <- build_seed_choices(
      seed_data(),
      team_lookup(),
      region_lookup(),
      as.integer(input$season),
      region_filter = "All"
    )
    team_name_lookup <- stats::setNames(seed_choices_all$choice_label, seed_choices_all$team_id)
    probability <- matchup_probability(division_predictions(), team_a_id, team_b_id)

    new_row <- tibble(
      matchup_id = paste0(team_a_id, "_", team_b_id),
      team_a = team_name_lookup[[as.character(team_a_id)]],
      team_b = team_name_lookup[[as.character(team_b_id)]],
      team_a_win_probability = round(probability, 3),
      team_b_win_probability = round(ifelse(is.na(probability), NA_real_, 1 - probability), 3)
    )

    custom_matchups(bind_rows(custom_matchups(), new_row) |> distinct(matchup_id, .keep_all = TRUE))
  })

  first_round_games <- reactive({
    req(nrow(division_predictions()) > 0)
    build_first_round_games(
      seed_data = seed_data(),
      team_lookup = team_lookup(),
      region_lookup = region_lookup(),
      historical_seed_rates = historical_seed_rates(),
      predictions = division_predictions(),
      season = as.integer(input$season)
    )
  })

  output$team_summary_note <- renderText({
    if (nrow(available_summary_teams()) == 0) {
      return("No regular-season team data is available for that division and season.")
    }

    "This page summarizes the selected team's regular season, including efficiency-style stats and the wins and losses that stand out most."
  })

  output$team_summary_view <- renderUI({
    summary_data <- team_summary_data()
    if (is.null(summary_data)) {
      return(div(class = "section-note", "Pick a team to view its season summary."))
    }

    summary_row <- summary_data$summary

    div(
      div(
        class = "summary-header",
        div(class = "summary-team-name", summary_row$team_name[[1]]),
        div(
          class = "summary-meta",
          paste(
            paste0("Record ", summary_row$wins[[1]], "-", summary_row$losses[[1]]),
            paste0("Win% ", format_percent(summary_row$win_pct[[1]])),
            if (!is.na(summary_row$seed_label[[1]])) {
              paste0("Tournament seed ", summary_row$seed_label[[1]], " (", summary_row$region_name[[1]], ")")
            } else {
              "Not currently seeded"
            },
            sep = " | "
          )
        )
      ),
      div(
        class = "summary-stat-grid",
        summary_stat_card_ui("Win %", format_percent(summary_row$win_pct[[1]]), summary_row$win_pct_pct[[1]]),
        summary_stat_card_ui("Points For", round(summary_row$avg_points[[1]], 1), summary_row$avg_points_pct[[1]]),
        summary_stat_card_ui("Points Allowed", round(summary_row$avg_points_allowed[[1]], 1), summary_row$avg_points_allowed_pct[[1]]),
        summary_stat_card_ui("Average Margin", sprintf("%+.1f", summary_row$avg_margin[[1]]), summary_row$avg_margin_pct[[1]]),
        summary_stat_card_ui("Field Goal %", format_percent(summary_row$fg_pct[[1]]), summary_row$fg_pct_pct[[1]]),
        summary_stat_card_ui("3PT %", format_percent(summary_row$fg3_pct[[1]]), summary_row$fg3_pct_pct[[1]]),
        summary_stat_card_ui("FT %", format_percent(summary_row$ft_pct[[1]]), summary_row$ft_pct_pct[[1]]),
        summary_stat_card_ui("Effective FG %", format_percent(summary_row$efg_pct[[1]]), summary_row$efg_pct_pct[[1]]),
        summary_stat_card_ui("Rebounds", round(summary_row$rebounds[[1]], 1), summary_row$rebounds_pct[[1]]),
        summary_stat_card_ui("Assists", round(summary_row$assists[[1]], 1), summary_row$assists_pct[[1]]),
        summary_stat_card_ui("Turnovers", round(summary_row$turnovers[[1]], 1), summary_row$turnovers_pct[[1]]),
        summary_stat_card_ui("Steals", round(summary_row$steals[[1]], 1), summary_row$steals_pct[[1]]),
        summary_stat_card_ui("Blocks", round(summary_row$blocks[[1]], 1), summary_row$blocks_pct[[1]]),
        summary_stat_card_ui("Off Rating", round(summary_row$off_rating[[1]], 1), summary_row$off_rating_pct[[1]]),
        summary_stat_card_ui("Def Rating", round(summary_row$def_rating[[1]], 1), summary_row$def_rating_pct[[1]])
      ),
      div(
        class = "summary-section",
        div(class = "summary-section-title", "Key Wins"),
        key_game_table_ui(summary_data$best_wins, "No wins available for this selection.")
      ),
      div(
        class = "summary-section",
        div(class = "summary-section-title", "Key Losses"),
        key_game_table_ui(summary_data$worst_losses, "No losses available for this selection.")
      )
    )
  })

  output$first_round_note <- renderText({
    if (nrow(division_predictions()) == 0) {
      return("No saved prediction file is available for that season and division yet. Run the pipeline for that tournament year first.")
    }

    if (any(first_round_games()$matchup_status == "Play-in pending")) {
      "Play-in slots are marked as pending. Once those teams are set, you can add the final matchup in the custom tab."
    } else if (any(first_round_games()$upset_watch)) {
      "Favored teams are shaded green on a 0.5-to-1 scale. Each card also shows the historical lower-seed win rate for that seed matchup. Upset alerts appear when the lower seed win probability is above that historical average."
    } else {
      "Favored teams are shaded green on a 0.5-to-1 scale. Each card also shows the historical lower-seed win rate for that seed matchup."
    }
  })

  output$first_round_cards <- renderUI({
    req(nrow(division_predictions()) > 0)

    cards <- lapply(seq_len(nrow(first_round_games())), function(i) {
      row <- first_round_games()[i, ]
      matchup_card_ui(
        team_a = row$team_a[[1]],
        team_b = row$team_b[[1]],
        prob_a = row$team_a_win_probability[[1]],
        prob_b = row$team_b_win_probability[[1]],
        title = row$matchup_label[[1]],
        upset_watch = isTRUE(row$upset_watch[[1]]),
        subtitle = if (!is.na(row$lower_seed_historic_win_rate[[1]])) {
          paste0(row$matchup_status[[1]], " - historical lower-seed win rate ", format_probability(row$lower_seed_historic_win_rate[[1]]))
        } else {
          row$matchup_status[[1]]
        }
      )
    })

    div(class = "matchup-card-grid", cards)
  })

  output$custom_note <- renderText({
    if (nrow(division_predictions()) == 0) {
      return("Run the saved predictions for this season first, then you can add custom matchups here.")
    }

    "Filter teams by region, choose a matchup, and add it to the list below. Each card includes win probability."
  })

  output$custom_matchup_cards <- renderUI({
    if (nrow(custom_matchups()) == 0) {
      return(div(class = "section-note", "No custom matchups added yet."))
    }

    cards <- lapply(seq_len(nrow(custom_matchups())), function(i) {
      row <- custom_matchups()[i, ]
      matchup_card_ui(
        team_a = row$team_a[[1]],
        team_b = row$team_b[[1]],
        prob_a = row$team_a_win_probability[[1]],
        prob_b = row$team_b_win_probability[[1]],
        title = paste("Custom Matchup", i)
      )
    })

    div(class = "matchup-card-grid", cards)
  })

  output$bracket_note <- renderText({
    if (nrow(bracket_pool()) == 0) {
      return("Run the bracket simulation stage first, then this page can sample from the saved bracket pool.")
    }

    if (nrow(generated_bracket()) == 0) {
      return("Generate a bracket to randomly draw one pre-simulated bracket from the saved bracket pool.")
    }

    paste0("This bracket was randomly selected from ", length(available_bracket_ids()), " pre-generated simulations for the selected division and season.")
  })

  output$fill_bracket_note <- renderText({
    if (nrow(division_predictions()) == 0) {
      return("No saved prediction file is available for that season and division yet. Run the pipeline for that tournament year first.")
    }

    "Pick winners game by game. Each matchup shows win probability, and your pick advances automatically to the next round."
  })

  output$simulation_note <- renderText({
    if (nrow(round_probabilities()) == 0) {
      return("Run the bracket simulation stage first to populate round-advancement probabilities.")
    }

    "These probabilities come from the saved bracket simulations and show how often each team reached each round."
  })

  output$simulation_probabilities_view <- renderUI({
    if (nrow(round_probabilities()) == 0) {
      return(div(class = "section-note", "No round simulation probabilities are available yet."))
    }

    simulation_table <- round_probabilities() |>
      left_join(team_lookup(), by = "team_id") |>
      left_join(
        seed_data() |>
          filter(season == as.integer(input$season)) |>
          transmute(team_id, seed_label = seed_raw),
        by = "team_id"
      ) |>
      left_join(
        region_lookup() |>
          filter(season == as.integer(input$season)) |>
          select(region, region_name),
        by = "region"
      )

    round_probability_table_ui(simulation_table)
  })

  output$bracket_view <- renderUI({
    build_generated_bracket_page_ui(
      bracket_games = generated_bracket_games(),
      region_lookup_frame = region_lookup(),
      season = as.integer(input$season)
    )
  })

  output$fill_bracket_view <- renderUI({
    if (nrow(division_predictions()) == 0) {
      return(div(class = "section-note", "No saved predictions exist yet for that season and division."))
    }

    interactive_bracket_ui(filled_bracket_games(), region_lookup(), as.integer(input$season))
  })

  output$export_jpg <- downloadHandler(
    filename = function() {
      paste0(
        str_replace_all(tolower(input$main_tab), "\\s+", "_"),
        "_",
        input$division,
        "_",
        input$season,
        ".jpg"
      )
    },
    content = function(file) {
      if (identical(input$main_tab, "Team Summary")) {
        export_team_summary_jpg(
          team_summary = team_summary_data(),
          file = file,
          title = paste("Team Summary -", tools::toTitleCase(input$division), input$season)
        )
      } else if (identical(input$main_tab, "Simulation Probabilities")) {
        simulation_table <- round_probabilities() |>
          left_join(team_lookup(), by = "team_id") |>
          left_join(
            seed_data() |>
              filter(season == as.integer(input$season)) |>
              transmute(team_id, seed_label = seed_raw),
            by = "team_id"
          ) |>
          left_join(
            region_lookup() |>
              filter(season == as.integer(input$season)) |>
              select(region, region_name),
            by = "region"
          )

        export_round_probabilities_jpg(
          data = simulation_table,
          file = file,
          title = paste("Simulation Probabilities -", tools::toTitleCase(input$division), input$season)
        )
      } else if (identical(input$main_tab, "First Round")) {
        export_data <- first_round_games() |>
          transmute(
            matchup_label,
            team_a,
            team_b,
            team_a_win_probability,
            team_b_win_probability,
            matchup_status,
            upset_watch
          )

        export_matchups_jpg(
          export_data,
          file = file,
          title = paste("First Round Matchups -", tools::toTitleCase(input$division), input$season)
        )
      } else if (identical(input$main_tab, "Bracket Generator")) {
        export_bracket_jpg(
          bracket_display = generated_bracket_display(),
          region_lookup_frame = region_lookup(),
          file = file,
          bracket_games = generated_bracket_games(),
          season = as.integer(input$season),
          title = paste("Generated Bracket -", tools::toTitleCase(input$division), input$season)
        )
      } else if (identical(input$main_tab, "Fill Out Bracket")) {
        export_bracket_jpg(
          bracket_display = filled_bracket_display(),
          region_lookup_frame = region_lookup(),
          file = file,
          bracket_games = filled_bracket_games(),
          season = as.integer(input$season),
          title = paste("Fill Out Bracket -", tools::toTitleCase(input$division), input$season)
        )
      } else {
        export_data <- custom_matchups() |>
          mutate(
            matchup_label = paste("Custom Matchup", row_number()),
            matchup_status = "Custom"
          )

        export_matchups_jpg(
          export_data,
          file = file,
          title = paste("Custom Matchups -", tools::toTitleCase(input$division), input$season)
        )
      }
    }
  )
}

shinyApp(ui = ui, server = server)
