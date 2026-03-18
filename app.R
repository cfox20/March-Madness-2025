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

matchup_probability <- function(predictions, team_a_id, team_b_id) {
  if (length(team_a_id) == 0 || length(team_b_id) == 0 || is.na(team_a_id) || is.na(team_b_id) || team_a_id == team_b_id) {
    return(NA_real_)
  }

  matchup <- predictions |>
    filter(
      ((a_team_id == team_a_id & b_team_id == team_b_id) |
         (a_team_id == team_b_id & b_team_id == team_a_id))
    ) |>
    slice(1)

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
    return("#f3efe4")
  }

  percentile <- min(max(percentile, 0), 1)
  rgb(
    red = (1 - percentile) * 235 + percentile * 68,
    green = (1 - percentile) * 99 + percentile * 164,
    blue = (1 - percentile) * 99 + percentile * 83,
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
    grid.text(title, y = 0.9, gp = gpar(fontsize = 22, fontface = "bold"))
    grid.text("No matchups available to export.", y = 0.8, gp = gpar(fontsize = 14))
    dev.off()
    return(invisible(file))
  }

  height_px <- max(900, 170 * nrow(matchups) + 180)
  jpeg(filename = file, width = 1600, height = height_px, quality = 100)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(nrow(matchups) + 1, 1)))
  grid.text(title, vp = viewport(layout.pos.row = 1, layout.pos.col = 1), y = 0.7, gp = gpar(fontsize = 24, fontface = "bold"))

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
      gp = gpar(fill = "#fffdf7", col = border_col, lwd = 2)
    )
    grid.text(row$matchup_label[[1]], x = 0.06, y = 0.82, just = "left", gp = gpar(fontsize = 16, fontface = "bold"))
    grid.text(subtitle, x = 0.06, y = 0.68, just = "left", gp = gpar(fontsize = 11, col = "#6b7280"))
    grid.roundrect(x = 0.27, y = 0.34, width = 0.4, height = 0.34, r = unit(0.02, "snpc"),
                   gp = gpar(fill = if (favored_a) grDevices::adjustcolor("#1f7a4c", alpha.f = alpha_a) else "#f3efe4", col = NA))
    grid.roundrect(x = 0.73, y = 0.34, width = 0.4, height = 0.34, r = unit(0.02, "snpc"),
                   gp = gpar(fill = if (favored_b) grDevices::adjustcolor("#1f7a4c", alpha.f = alpha_b) else "#f3efe4", col = NA))
    grid.text(row$team_a[[1]], x = 0.08, y = 0.41, just = "left", gp = gpar(fontsize = 14, fontface = "bold"))
    grid.text(row$team_b[[1]], x = 0.54, y = 0.41, just = "left", gp = gpar(fontsize = 14, fontface = "bold"))
    grid.text(format_probability(prob_a), x = 0.39, y = 0.41, just = "right", gp = gpar(fontsize = 18, fontface = "bold"))
    grid.text(format_probability(prob_b), x = 0.85, y = 0.41, just = "right", gp = gpar(fontsize = 18, fontface = "bold"))
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

interactive_matchup_picker_ui <- function(game_row, extra_class = NULL) {
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

render_interactive_region_bracket_ui <- function(games, region_code, region_name, side = "left") {
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
      interactive_matchup_picker_ui(row, extra_class = extra_class)
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

render_interactive_final_four_ui <- function(games) {
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
        if (nrow(semis) >= 1) interactive_matchup_picker_ui(semis[1, ], extra_class = "center-pick-box")
      ),
      div(
        class = "center-box final-semi final-semi-right",
        if (nrow(semis) >= 2) interactive_matchup_picker_ui(semis[2, ], extra_class = "center-pick-box")
      ),
      div(class = "final-connector final-connector-left"),
      div(class = "final-connector final-connector-right"),
      div(class = "final-connector final-connector-down"),
      div(class = "championship-label", "Champion"),
      div(
        class = "final-champion",
        if (nrow(championship) == 1) interactive_matchup_picker_ui(championship[1, ], extra_class = "center-pick-box")
      )
    )
  )
}

interactive_bracket_ui <- function(games, region_lookup_frame, season) {
  region_names <- region_lookup_frame |>
    filter(season == !!season)

  div(
    class = "interactive-bracket-scroll",
    div(
      class = "bracket-shell",
      div(
        class = "bracket-side",
        render_interactive_region_bracket_ui(games, "W", region_names$region_name[region_names$region == "W"][[1]], side = "left"),
        render_interactive_region_bracket_ui(games, "X", region_names$region_name[region_names$region == "X"][[1]], side = "left")
      ),
      div(
        class = "bracket-center",
        render_interactive_final_four_ui(games)
      ),
      div(
        class = "bracket-side",
        render_interactive_region_bracket_ui(games, "Y", region_names$region_name[region_names$region == "Y"][[1]], side = "right"),
        render_interactive_region_bracket_ui(games, "Z", region_names$region_name[region_names$region == "Z"][[1]], side = "right")
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

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background: #f6f4ec; color: #1f2933; }
      .tabbable > .nav > li > a { background: #ebe5d6; color: #4a4032; border-radius: 10px 10px 0 0; }
      .tabbable > .nav > li.active > a { background: #1f4d3a; color: #fff; }
      .matchup-card {
        background: #fffdf7;
        border: 1px solid #d8d1bf;
        border-radius: 14px;
        padding: 14px;
        margin-bottom: 14px;
        box-shadow: 0 6px 18px rgba(31, 41, 51, 0.06);
      }
      .matchup-card-alert {
        background: linear-gradient(180deg, #fff7f4 0%, #fffdf7 100%);
      }
      .matchup-title { font-size: 18px; font-weight: 700; margin-bottom: 4px; }
      .matchup-subtitle { font-size: 13px; color: #6b7280; margin-bottom: 10px; }
      .upset-alert-banner {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 12px;
        background: linear-gradient(135deg, #c0392b 0%, #e67e22 100%);
        color: #fff;
        border-radius: 12px;
        padding: 10px 12px;
        margin: 8px 0 12px 0;
      }
      .upset-alert-kicker {
        font-size: 11px;
        font-weight: 800;
        letter-spacing: 0.12em;
        text-transform: uppercase;
        white-space: nowrap;
      }
      .upset-alert-text {
        font-size: 12px;
        font-weight: 600;
        text-align: right;
      }
      .matchup-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 10px; }
      .matchup-side {
        background: #f3efe4;
        border-radius: 12px;
        padding: 12px;
        min-height: 92px;
      }
      .team-name { font-size: 15px; font-weight: 600; margin-bottom: 8px; }
      .team-prob { font-size: 22px; font-weight: 800; }
      .section-note { margin: 12px 0 18px 0; color: #5f6c7b; }
      .control-box {
        background: #fffdf7;
        border: 1px solid #d8d1bf;
        border-radius: 14px;
        padding: 14px;
        margin-bottom: 16px;
      }
      .summary-header {
        background: #fffdf7;
        border: 1px solid #d8d1bf;
        border-radius: 16px;
        padding: 18px;
        margin-bottom: 16px;
      }
      .summary-team-name {
        font-size: 28px;
        font-weight: 800;
        margin-bottom: 6px;
      }
      .summary-meta {
        color: #5f6c7b;
        font-size: 14px;
      }
      .summary-stat-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
        gap: 12px;
        margin-bottom: 18px;
      }
      .summary-stat-card {
        background: #fffdf7;
        border: 1px solid #d8d1bf;
        border-radius: 14px;
        padding: 14px;
      }
      .summary-stat-label {
        font-size: 12px;
        text-transform: uppercase;
        letter-spacing: 0.08em;
        color: #6b7280;
        font-weight: 700;
        margin-bottom: 8px;
      }
      .summary-stat-value {
        font-size: 22px;
        font-weight: 800;
      }
      .summary-stat-rank {
        margin-top: 6px;
        font-size: 12px;
        color: #3f4c5a;
        font-weight: 600;
      }
      .summary-section {
        background: #fffdf7;
        border: 1px solid #d8d1bf;
        border-radius: 16px;
        padding: 16px;
        margin-bottom: 16px;
      }
      .summary-section-title {
        font-size: 18px;
        font-weight: 800;
        margin-bottom: 12px;
      }
      .team-game-table th {
        font-size: 12px;
        text-transform: uppercase;
        letter-spacing: 0.05em;
        color: #6b7280;
      }
      .team-game-table td {
        vertical-align: middle;
      }
      .bracket-shell {
        display: grid;
        grid-template-columns: minmax(0, 1fr) 184px minmax(0, 1fr);
        gap: 18px;
        align-items: center;
      }
      .bracket-side {
        display: flex;
        flex-direction: column;
        gap: 2px;
      }
      .bracket-center {
        display: flex;
        align-items: center;
        justify-content: center;
        min-height: 100%;
      }
      .region-bracket {
        padding: 0;
      }
      .region-title {
        font-size: 11px;
        font-weight: 800;
        margin-bottom: 2px;
        text-transform: uppercase;
        letter-spacing: 0.14em;
        color: #355a80;
      }
      .region-bracket.right .region-title {
        text-align: right;
      }
      .center-title {
        text-align: center;
      }
      .region-grid {
        display: grid;
        grid-template-columns: minmax(88px, 1fr) 18px minmax(88px, 1fr) 18px minmax(88px, 1fr) 18px minmax(88px, 1fr);
        grid-template-rows: 16px repeat(15, minmax(16px, auto));
        column-gap: 0;
        row-gap: 0;
        align-items: center;
      }
      .round-head {
        grid-row: 1;
        text-align: center;
      }
      .round-head-r1 { grid-column: 1; }
      .round-head-r2 { grid-column: 3; }
      .round-head-r3 { grid-column: 5; }
      .round-head-r4 { grid-column: 7; }
      .final-four-bracket {
        padding-top: 12px;
        display: flex;
        flex-direction: column;
        justify-content: center;
        width: 100%;
      }
      .final-four-grid {
        display: grid;
        grid-template-columns: minmax(70px, 1fr) 14px minmax(84px, 1fr) 14px minmax(70px, 1fr);
        grid-template-rows: 14px 34px 34px 10px 38px;
        gap: 0;
        align-items: center;
        min-height: 100%;
      }
      .final-head {
        grid-row: 1;
        text-align: center;
        font-size: 8px;
        line-height: 1.15;
      }
      .final-head-left { grid-column: 1; }
      .final-head-champ { grid-column: 3; }
      .final-head-right { grid-column: 5; }
      .championship-label {
        grid-column: 3;
        grid-row: 4;
        text-align: center;
        font-size: 8px;
        text-transform: uppercase;
        letter-spacing: 0.08em;
        color: #6b7280;
        font-weight: 700;
      }
      .bracket-round {
        display: flex;
        flex-direction: column;
        gap: 10px;
      }
      .round-title {
        font-size: 9px;
        text-transform: uppercase;
        letter-spacing: 0.12em;
        color: #5f6c7a;
        font-weight: 700;
      }
      .bracket-team-box {
        background: #ffffff;
        border: 1px solid #b9c0c8;
        border-radius: 0;
        padding: 1px 4px 2px 4px;
        min-height: 22px;
        display: flex;
        flex-direction: column;
        justify-content: center;
      }
      .bracket-winner-box {
        border: 1px solid #b9c0c8;
        box-shadow: none;
      }
      .first-round-top {
        transform: translateY(5px);
      }
      .first-round-bottom {
        transform: translateY(-5px);
      }
      .bracket-connector {
        position: relative;
        align-self: stretch;
      }
      .bracket-connector::before {
        content: '';
        position: absolute;
        top: 50%;
        width: 50%;
        border-top: 1px solid #adb5bf;
      }
      .bracket-connector::after {
        content: '';
        position: absolute;
        top: 0;
        bottom: 0;
        width: 1px;
        background: #adb5bf;
      }
      .connector-left::before {
        left: 50%;
      }
      .connector-left::after {
        right: 50%;
      }
      .connector-right::before {
        right: 50%;
      }
      .connector-right::after {
        left: 50%;
      }
      .center-box {
        min-height: 30px;
      }
      .final-semi {
        grid-row: 2 / span 2;
      }
      .final-semi-left { grid-column: 1; }
      .final-semi-right { grid-column: 5; }
      .final-champion {
        grid-column: 3;
        grid-row: 5;
        min-height: 38px;
      }
      .final-connector {
        position: relative;
        align-self: stretch;
      }
      .final-connector-left {
        grid-column: 2;
        grid-row: 2 / span 2;
      }
      .final-connector-right {
        grid-column: 4;
        grid-row: 2 / span 2;
      }
      .final-connector-left::before,
      .final-connector-right::before {
        content: '';
        position: absolute;
        top: 50%;
        width: 50%;
        border-top: 1px solid #adb5bf;
      }
      .final-connector-left::before { left: 0; }
      .final-connector-right::before { right: 0; }
      .final-connector-left::after,
      .final-connector-right::after {
        content: '';
        position: absolute;
        top: 50%;
        bottom: 0;
        width: 1px;
        background: #adb5bf;
      }
      .final-connector-left::after { right: 0; }
      .final-connector-right::after { left: 0; }
      .final-connector-down {
        grid-column: 3;
        grid-row: 3 / 5;
        justify-self: center;
        width: 1px;
        background: #adb5bf;
      }
      .champion-box {
        background: #ffffff;
        border: 1px solid #8f98a3;
      }
      .bracket-seed {
        font-size: 7px;
        color: #4d5863;
        font-weight: 700;
        margin-bottom: 0;
      }
      .bracket-team {
        font-size: 8px;
        font-weight: 600;
        line-height: 1.05;
      }
      .bracket-actions {
        margin-bottom: 10px;
      }
      .interactive-actions {
        display: flex;
        gap: 10px;
        align-items: center;
        flex-wrap: wrap;
      }
      .interactive-bracket-scroll {
        overflow-x: auto;
        overflow-y: visible;
        padding-bottom: 6px;
      }
      .bracket-interactive-box {
        padding: 2px 3px;
        min-height: 20px;
        overflow: hidden;
        align-items: stretch;
      }
      .bracket-interactive-box.first-round-top {
        transform: translateY(14px);
      }
      .bracket-interactive-box.first-round-bottom {
        transform: translateY(-14px);
      }
      .bracket-pick-round {
        font-size: 6px;
        font-weight: 800;
        letter-spacing: 0.08em;
        text-transform: uppercase;
        color: #6b7280;
        margin-bottom: 1px;
      }
      .bracket-interactive-box .form-group,
      .bracket-interactive-box .shiny-input-container {
        margin-bottom: 0;
      }
      .bracket-pick-options {
        width: 100%;
        min-width: 0;
        height: 100%;
      }
      .bracket-interactive-box .radio {
        margin-top: 0;
        margin-bottom: 0;
        min-height: 0;
        width: 100%;
      }
      .bracket-interactive-box .shiny-options-group {
        display: flex;
        flex-direction: column;
        gap: 0;
        justify-content: center;
        width: 100%;
        min-width: 0;
        height: 100%;
      }
      .bracket-interactive-box .radio label {
        display: flex;
        align-items: center;
        padding: 0;
        margin: 0;
        font-weight: 600;
        cursor: pointer;
        line-height: 1;
        width: 100%;
        min-width: 0;
        overflow: hidden;
      }
      .bracket-interactive-box .radio:last-child label {
        margin-bottom: 0;
      }
      .bracket-interactive-box input[type='radio'] {
        position: absolute;
        opacity: 0;
        pointer-events: none;
        width: 0;
        height: 0;
        margin: 0;
      }
      .bracket-pick-choice {
        display: flex;
        align-items: center;
        gap: 2px;
        width: 100%;
        flex: 1 1 auto;
        min-width: 0;
        min-height: 8px;
        padding: 0;
        border: 1px solid transparent;
        background: transparent;
        pointer-events: none;
      }
      .bracket-pick-choice.is-selected {
        background: #eef3f8;
        border-color: #7f93aa;
      }
      .bracket-pick-prob {
        min-width: 23px;
        text-align: center;
        border-radius: 999px;
        padding: 1px 2px;
        font-size: 6.5px;
        font-weight: 800;
        color: #1f2933;
        flex: 0 0 auto;
      }
      .bracket-pick-team {
        font-size: 6.5px;
        line-height: 1;
        font-weight: 600;
        color: #111827;
        flex: 1 1 auto;
        min-width: 0;
        white-space: normal;
      }
      .bracket-pending-box {
        font-size: 7px;
        line-height: 1;
        color: #6b7280;
        padding: 2px 3px;
        background: #f7f4eb;
        min-height: 14px;
        display: flex;
        flex-direction: column;
        justify-content: center;
        gap: 1px;
      }
      .bracket-pending-team {
        font-size: 6.5px;
        line-height: 1;
        font-weight: 600;
        color: #111827;
      }
      .bracket-pending-note {
        font-size: 6px;
        line-height: 1;
        color: #6b7280;
      }
      .interactive-game-selection {
        margin-top: 8px;
        font-size: 12px;
        font-weight: 700;
        color: #1f4d3a;
      }
      @media (max-width: 1500px) {
        .bracket-shell {
          grid-template-columns: 1fr;
        }
      }
    "))
  ),
  titlePanel("March Madness Matchup Probabilities"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      class = "control-box",
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
        selectInput(
          "team_summary_team",
          "Team summary team",
          choices = list()
        )
      ),
      downloadButton("export_jpg", "Export Current Tab JPG"),
      helpText("Use 2025 now, then switch the pipeline and app to 2026 once the official bracket is announced.")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tab",
        tabPanel(
          "First Round",
          br(),
          div(class = "section-note", textOutput("first_round_note")),
          uiOutput("first_round_cards")
        ),
        tabPanel(
          "Custom Matchups",
          br(),
          div(
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
              column(width = 4, actionButton("add_matchup", "Add Matchup")),
              column(width = 4, actionButton("clear_matchups", "Clear Matchups"))
            )
          ),
          div(class = "section-note", textOutput("custom_note")),
          uiOutput("custom_matchup_cards")
        ),
        tabPanel(
          "Bracket Generator",
          br(),
          div(
            class = "control-box bracket-actions",
            actionButton("generate_bracket", "Generate Bracket")
          ),
          div(class = "section-note", textOutput("bracket_note")),
          uiOutput("bracket_view")
        ),
        tabPanel(
          "Fill Out Bracket",
          br(),
          div(
            class = "control-box bracket-actions interactive-actions",
            actionButton("clear_filled_bracket", "Clear Picks")
          ),
          div(class = "section-note", textOutput("fill_bracket_note")),
          uiOutput("fill_bracket_view")
        ),
        tabPanel(
          "Simulation Probabilities",
          br(),
          div(class = "section-note", textOutput("simulation_note")),
          uiOutput("simulation_probabilities_view")
        ),
        tabPanel(
          "Team Summary",
          br(),
          div(class = "section-note", textOutput("team_summary_note")),
          uiOutput("team_summary_view")
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

    set.seed(as.integer(input$season) + ifelse(input$division == "mens", 1L, 2L) + input$generate_bracket)
    selected_bracket <- sample(available_bracket_ids(), size = 1)

    bracket_frame <- bracket_pool() |>
      filter(Bracket == selected_bracket) |>
      left_join(team_lookup(), by = "team_id") |>
      left_join(
        seed_data() |>
          filter(season == as.integer(input$season)) |>
          transmute(team_id, seed_num, region, Team = seed_raw),
        by = c("team_id", "Team")
      ) |>
      left_join(
        region_lookup() |>
          filter(season == as.integer(input$season)) |>
          select(region, region_name),
        by = "region"
      )

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
      "Favored teams are shaded green on a 0.5-to-1 scale. Each card shows the historical lower-seed win rate for that seed matchup, and upset alerts appear when the lower seed win probability is above that historical average."
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

    tagList(cards)
  })

  output$custom_note <- renderText({
    if (nrow(division_predictions()) == 0) {
      return("Run the saved predictions for this season first, then you can add custom matchups here.")
    }

    "Filter teams by region, choose a matchup, and add it to the list below. Only the favored team is shaded green."
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

    tagList(cards)
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

    "Pick winners game by game. Each team shows its win probability for the current matchup, and your pick advances automatically to the next round."
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
    if (nrow(generated_bracket()) == 0) {
      return(div(class = "section-note", "No bracket generated yet."))
    }

    region_names <- filter(region_lookup(), season == as.integer(input$season))
    bracket_data <- generated_bracket()

    div(
      class = "bracket-shell",
      div(
        class = "bracket-side",
        render_region_bracket_ui(
          bracket_data = bracket_data,
          region_code = "W",
          region_name = region_names$region_name[region_names$region == "W"][[1]],
          side = "left"
        ),
        render_region_bracket_ui(
          bracket_data = bracket_data,
          region_code = "X",
          region_name = region_names$region_name[region_names$region == "X"][[1]],
          side = "left"
        )
      ),
      div(
        class = "bracket-center",
        render_final_four_ui(bracket_data)
      ),
      div(
        class = "bracket-side",
        render_region_bracket_ui(
          bracket_data = bracket_data,
          region_code = "Y",
          region_name = region_names$region_name[region_names$region == "Y"][[1]],
          side = "right"
        ),
        render_region_bracket_ui(
          bracket_data = bracket_data,
          region_code = "Z",
          region_name = region_names$region_name[region_names$region == "Z"][[1]],
          side = "right"
        )
      )
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
        export_data <- generated_bracket() |>
          transmute(
            matchup_label = Slot,
            team_a = Team,
            team_b = team_name,
            team_a_win_probability = NA_real_,
            team_b_win_probability = NA_real_,
            matchup_status = "Bracket slot",
            upset_watch = FALSE
          )

        export_matchups_jpg(
          export_data,
          file = file,
          title = paste("Generated Bracket -", tools::toTitleCase(input$division), input$season)
        )
      } else if (identical(input$main_tab, "Fill Out Bracket")) {
        export_data <- filled_bracket_games() |>
          transmute(
            matchup_label = paste(round_label, display_region),
            team_a = if_else(is.na(team_1_name), "TBD", paste(team_1_seed, team_1_name)),
            team_b = if_else(is.na(team_2_name), "TBD", paste(team_2_seed, team_2_name)),
            team_a_win_probability = team_1_probability,
            team_b_win_probability = team_2_probability,
            matchup_status = if_else(is.na(selected_team_name), "Awaiting pick", paste("Picked", selected_team_name)),
            upset_watch = FALSE
          )

        export_matchups_jpg(
          export_data,
          file = file,
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
