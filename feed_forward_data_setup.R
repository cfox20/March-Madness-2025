
# Load Packages -----------------------------------------------------------

library("tidyverse")
library("here")
library("janitor")
library("slider")
# library("fuzzyjoin")

# Load Data ---------------------------------------------------------------

source("read_data.R")


# Calculate Season Data ---------------------------------------------------

m_reg_results <- m_reg_detailed_results |>
  mutate(game_id = row_number()) |>
         # w_team = sample(c("A","B"), n(), replace = TRUE)) |>
  relocate(game_id)

# Create separate tibbles to randomly assign the winners to A or B
w_team <- m_reg_results |>
  filter(season >= 2012) |>
  select(-num_ot, -l_team_id, -w_loc) |>
  # mutate(team = "A") |>
  rename_with(~ str_replace(.x, "^w", "")) |>
  rename_with(~ str_replace(.x, "^l", "opp_")) |>
  rename_with(~ str_replace(.x, "^_", "")) |>
  rename_with(~ str_replace(.x, "__", "_")) |>
  mutate(win_pct = 1) |>
  relocate(win_pct, .after = day_num) |>
  relocate(team_id, .after = season)

l_team <- m_reg_results |>
  filter(season >= 2012) |>
  select(-num_ot, -w_team_id, -w_loc) |>
  rename_with(~ str_replace(.x, "^l", "")) |>
  rename_with(~ str_replace(.x, "^w", "opp_")) |>
  rename_with(~ str_replace(.x, "^_", "")) |>
  rename_with(~ str_replace(.x, "__", "_")) |>
  mutate(win_pct = 0) |>
  relocate(win_pct, .after = day_num) |>
  relocate(team_id, .after = season)

m_all_games <- bind_rows(w_team, l_team)


# str(m_all_games)

# Rankings
m_reg_season_ranks <- m_team_rankings |>
  filter(season >= 2012) |>
  select(season, team_id, ranking_day_num, POM) |>
  # mutate(RPI_NET = coalesce(RPI, NET)) |>
  # group_by(season) |>
  # filter(ranking_day_num == max(ranking_day_num)) |>
  # select(-c(NET, RPI)) |>
  rename(day_num = ranking_day_num) |>
  ungroup()

m_all_ranks <- expand_grid(day_num = 1:155, distinct(m_reg_season_ranks, team_id), distinct(m_reg_season_ranks, season)) |>
  left_join(m_reg_season_ranks, by = c("season", "team_id", "day_num")) |>
  arrange(season, team_id, day_num) |>
  group_by(season, team_id) |>
  filter(day_num > 10) |>
  fill(POM, .direction = "down") |>
  ungroup() |>
  drop_na()

# Cumulative Season Averages
m_team_cumulative_avg <- m_all_games |>
  filter(season >= 2012) |>
  arrange(season, team_id, day_num) |>
  group_by(season, team_id) |>
  mutate(score_margin = score - opp_score,
         to_margin = to - opp_to,
         fg_perc = fgm / fga,
         fg3_perc = fgm3 / fga3,
         # ft_perc = ftm / fta,
         ast_fgm_ratio = ast / fgm,
         poss = fga - or + to + (.475*fta),
         pts_per_poss = score / poss,
         opp_poss = opp_fga - opp_or + opp_to + (.475*opp_fta),
         opp_pts_per_poss = opp_score / opp_poss,
         across(-c(game_id, day_num),
                ~cummean(.x),
                .names = "c_avg_{.col}")) |>
  group_by(season, team_id)  |>
  mutate(across(starts_with("c_avg_"),
                ~lag(.x, default = 0))) |>
  ungroup() |>
  arrange(season, team_id, game_id) |>
  select(season, day_num, team_id, game_id, starts_with("c_avg")) |>
  group_by(season, team_id) |>
  mutate(game_num = row_number()) |>
  filter(game_num > 9) |>
  select(-game_num) |>
  ungroup() |>
  left_join(m_all_ranks, by = c("season", "team_id", "day_num")) |>
  rename(c_pom = POM)


m_team_rolling_avg <- m_all_games |>
  filter(season >= 2012) |>
  arrange(team_id, season, day_num) |>
  group_by(team_id, season) |>
  mutate(across(-c(game_id, day_num),
                ~slide_dbl(.x, ~mean(.x, na.rm = TRUE), .before = 2, .complete = FALSE))) |>
  rename_with(.fn = ~paste0("r_avg_", .x), .cols = 5:last_col()) |>
  mutate(across(starts_with("r_avg_"),
                ~lag(.x, default = 0))) |>
  mutate(game_num = row_number()) |>
  filter(game_num > 9) |>
  select(-game_num)

m_team_cumulative_avg <- left_join(m_team_cumulative_avg,
          m_team_rolling_avg,
          by = c("game_id", "season", "team_id", "day_num"))


# Add in kenpom and evamiya data

ken_pom <- read_rds("data//ken_pom.rds") |>
  mutate(power_5 = as.numeric(conf %in% c("B10", "B12", "ACC", "P12", "BE", "SEC"))) |>
  relocate(power_5, .after = conf) |>
  select(-w_l, -conf)

m_team_cumulative_avg <- left_join(filter(m_team_cumulative_avg, season >= 2012), ken_pom,
          by = c("team_id", "season")) |>
  relocate(power_5, .after = team_id) |>
  rename(c_power_5 = power_5,
         c_adj_em = adj_em,
         c_adj_o = adj_o,
         c_adj_d = adj_d,
         c_adj_t = adj_t,
         c_luck = luck,
         c_sos_adj_em = sos_adj_em,
         c_sos_opp_o = sos_opp_o,
         c_sos_opp_d = sos_opp_d,
         c_ncsos_adj_em = ncsos_adj_em,
         )

em_ranks <- read_csv(here(data_path, "em_team_data.csv")) |>
  select(team_id, season, rank, obpr, dbpr, bpr, tempo) |>
  rename(c_em_rank = rank,
         c_obpr = obpr,
         c_dbpr = dbpr,
         c_bpr = bpr,
         c_tempo = tempo)


m_team_cumulative_avg <- left_join(m_team_cumulative_avg,
          em_ranks,
          by = c("team_id", "season"))


# Add indicator variables for tournament teams
# Add this later
#
#
#
warning("update with 2025 seeds")
tourney_teams <- read_csv("data//2024_tourney_seeds.csv") |>
  janitor::clean_names() |>
  filter(tournament == "M") |>
  select(team_id) |>
  unlist() |>
  as.numeric()

pattern <- paste0("team_id2_(", paste(tourney_teams, collapse = "|"), ")")
#
m_team_cumulative_avg <- m_team_cumulative_avg |>
  mutate(team_id2 = factor(team_id)) |>
  relocate(team_id2, .after = team_id) |>
  data.table::as.data.table() |>
  mltools::one_hot(cols = "team_id2", dropCols = TRUE) |>
  as_tibble() |>
  dplyr::select(matches(pattern), names(m_team_cumulative_avg)) |>
  relocate(season, day_num, game_id, team_id, c_power_5) |>
  # mutate(team_id = as.numeric(team_id)) |>
  rename_with(~ str_replace(.x, "team_id2_", "c_team_id_"))


# Combine team cumulative averages with regular season game matchups
m_reg_results <- m_reg_compact_results |>
  filter(season >= 2012) |>
  select(season, day_num, w_team_id, l_team_id)

reg_season_games <- m_reg_results |>
  mutate(w_team = sample(c("a", "b"), n(), replace = TRUE),
         w_team2 = w_team) |>
  pivot_wider(names_from = w_team2, values_from = w_team_id) |>
  mutate(a = ifelse(is.na(a), l_team_id, a),
         b = ifelse(is.na(b), l_team_id, b)) |>
  rename(a_team_id = a,
         b_team_id = b) |>
  select(-l_team_id) |>
  inner_join(m_team_cumulative_avg |> select(-game_id), by = join_by(a_team_id == team_id, season, day_num)) |>
  rename_with(~str_replace(.x, "^c_", "a_c_")) |>
  rename_with(~str_replace(.x, "^r_", "a_r_")) |>
  # select(-game_id) |>
  inner_join(m_team_cumulative_avg |> select(-game_id), by = join_by(b_team_id == team_id, season, day_num)) |>
  rename_with(~str_replace(.x, "^c_", "b_c_")) |>
  rename_with(~str_replace(.x, "^r_", "b_r_"))
  # select(-game_id)



# filter(reg_season_games, if_any(everything(), is.na))



# Combine team cumulative averages with tournament game matchups
m_tourney_results <- m_tourney_compact_results |>
  filter(season >= 2012) |>
  select(season, day_num, w_team_id, l_team_id)

m_tournament_games <- m_tourney_results |>
  mutate(w_team = sample(c("a", "b"), n(), replace = TRUE),
         w_team2 = w_team) |>
  pivot_wider(names_from = w_team2, values_from = w_team_id) |>
  mutate(a = ifelse(is.na(a), l_team_id, a),
         b = ifelse(is.na(b), l_team_id, b)) |>
  rename(a_team_id = a,
         b_team_id = b) |>
  select(-l_team_id) |>
  left_join(group_by(m_team_cumulative_avg, season, team_id) |> filter(day_num == max(day_num)) |> ungroup() |> select(-day_num),
             by = join_by(a_team_id == team_id, season)) |>
  select(-game_id) |>
  rename_with(~str_replace(.x, "^c_", "a_c_")) |>
  rename_with(~str_replace(.x, "^r_", "a_r_")) |>
  left_join(group_by(m_team_cumulative_avg, season, team_id) |> filter(day_num == max(day_num)) |> ungroup() |> select(-day_num),
             by = join_by(b_team_id == team_id, season)) |>
  select(-game_id) |>
  rename_with(~str_replace(.x, "^c_", "b_c_")) |>
  rename_with(~str_replace(.x, "^r_", "b_r_"))


# Combine regular season and tourney games
m_all_games <- bind_rows(reg_season_games, m_tournament_games)



# filter(m_all_games, if_any(everything(), is.na))



#If you do not have an ID per row, use the following code to create an ID
tb <- m_all_games |> mutate(id = row_number())

#Create training set
train <- tb |> sample_frac(.80)

#Create test set
valid  <- anti_join(tb, train, by = 'id') |> select(-id)
train <- select(train, -id)





# Save Data ---------------------------------------------------------------

write_rds(list(train_data = train,
               valid_data = valid),
               # test_data = test),
          "data//ff_mod_data.rds")




# Setup Tourney Team Data -------------------------------------------------
#
tourney_seeds <- read_csv("data//2024_tourney_seeds.csv") |>
  janitor::clean_names() |>
  filter(tournament == "M")

# m_all_games <- bind_rows(w_team, l_team)
#


m_reg_results <- m_reg_detailed_results |>
  mutate(game_id = row_number()) |>
  # w_team = sample(c("A","B"), n(), replace = TRUE)) |>
  relocate(game_id)

# Create separate tibbles to randomly assign the winners to A or B
w_team <- m_reg_results |>
  filter(season == 2024, w_team_id %in% tourney_seeds$team_id) |>
  select(-num_ot, -l_team_id, -w_loc) |>
  # mutate(team = "A") |>
  rename_with(~ str_replace(.x, "^w", "")) |>
  rename_with(~ str_replace(.x, "^l", "opp_")) |>
  rename_with(~ str_replace(.x, "^_", "")) |>
  rename_with(~ str_replace(.x, "__", "_")) |>
  mutate(win_pct = 1) |>
  relocate(win_pct, .after = day_num) |>
  relocate(team_id, .after = season)

l_team <- m_reg_results |>
  filter(season == 2024, l_team_id %in% tourney_seeds$team_id) |>
  select(-num_ot, -w_team_id, -w_loc) |>
  rename_with(~ str_replace(.x, "^l", "")) |>
  rename_with(~ str_replace(.x, "^w", "opp_")) |>
  rename_with(~ str_replace(.x, "^_", "")) |>
  rename_with(~ str_replace(.x, "__", "_")) |>
  mutate(win_pct = 0) |>
  relocate(win_pct, .after = day_num) |>
  relocate(team_id, .after = season)

m_all_games <- bind_rows(w_team, l_team)


# str(m_all_games)

# Rankings
m_2024_ranks <- m_team_rankings |>
  filter(season == 2024, team_id %in% tourney_seeds$team_id, ranking_day_num == 128) |>
  select(team_id, POM)

# Cumulative Season Averages
m_tourney_team_avgs <- m_all_games |>
  arrange(team_id, day_num) |>
  group_by(season, team_id) |>
  mutate(score_margin = score - opp_score,
         to_margin = to - opp_to,
         fg_perc = fgm / fga,
         fg3_perc = fgm3 / fga3,
         # ft_perc = ftm / fta,
         ast_fgm_ratio = ast / fgm,
         poss = fga - or + to + (.475*fta),
         pts_per_poss = score / poss,
         opp_poss = opp_fga - opp_or + opp_to + (.475*opp_fta),
         opp_pts_per_poss = opp_score / opp_poss,
         across(-c(game_id, day_num),
                ~mean(.x),
                .names = "c_avg_{.col}")) |>
  ungroup() |>
  arrange(season, team_id, game_id) |>
  select(team_id, starts_with("c_avg")) |>
  distinct(team_id, .keep_all = TRUE) |>
  left_join(m_2024_ranks, by = "team_id") |>
  rename(c_pom = POM)


m_tourney_team_3game_avg <- m_all_games |>
  arrange(team_id, season, day_num) |>
  group_by(team_id, season) |>
  mutate(across(-c(game_id, day_num),
                ~slide_dbl(.x, ~mean(.x, na.rm = TRUE), .before = 2, .complete = FALSE))) |>
  rename_with(.fn = ~paste0("r_avg_", .x), .cols = 5:last_col()) |>
  mutate(across(starts_with("r_avg_"),
                ~lag(.x, default = 0))) |>
  filter(day_num == max(day_num)) |>
  ungroup() |>
  select(-season, -game_id, -day_num)

m_tourney_team_data <- left_join(m_tourney_team_avgs,
                                   m_tourney_team_3game_avg,
                                   by = c("team_id"))


# Add in kenpom and evamiya data


m_tourney_team_data <- left_join(m_tourney_team_data, filter(ken_pom, season == 2024) |> select(-season),
                                   by = c("team_id")) |>
  relocate(power_5, .after = team_id) |>
  rename(c_power_5 = power_5,
         c_adj_em = adj_em,
         c_adj_o = adj_o,
         c_adj_d = adj_d,
         c_adj_t = adj_t,
         c_luck = luck,
         c_sos_adj_em = sos_adj_em,
         c_sos_opp_o = sos_opp_o,
         c_sos_opp_d = sos_opp_d,
         c_ncsos_adj_em = ncsos_adj_em,
  ) |>
  left_join(filter(em_ranks, season == 2024) |> select(-season), by = c("team_id"))



pattern <- paste0("team_id2_(", paste(tourney_teams, collapse = "|"), ")")

m_tourney_team_data <- m_tourney_team_data |>
  mutate(team_id2 = factor(team_id)) |>
  relocate(team_id2, .after = team_id) |>
  data.table::as.data.table() |>
  mltools::one_hot(cols = "team_id2", dropCols = TRUE) |>
  as_tibble() |>
  # dplyr::select(matches(pattern), names(m_team_cumulative_avg)) |>
  relocate(team_id, c_power_5) |>
  # mutate(team_id = as.numeric(team_id)) |>
  rename_with(~ str_replace(.x, "team_id2_", "c_team_id_"))

write_rds(m_tourney_team_data, "data//m_tourney_data.rds")






































# Setup Womens' Data ------------------------------------------------------


w_reg_results <- w_reg_detailed_results |>
  mutate(game_id = row_number()) |>
  # w_team = sample(c("A","B"), n(), replace = TRUE)) |>
  relocate(game_id)

# Create separate tibbles to randomly assign the winners to A or B
w_w_team <- w_reg_results |>
  # filter(season >= 2012) |>
  select(-num_ot, -l_team_id, -w_loc) |>
  # mutate(team = "A") |>
  rename_with(~ str_replace(.x, "^w", "")) |>
  rename_with(~ str_replace(.x, "^l", "opp_")) |>
  rename_with(~ str_replace(.x, "^_", "")) |>
  rename_with(~ str_replace(.x, "__", "_")) |>
  mutate(win_pct = 1) |>
  relocate(win_pct, .after = day_num) |>
  relocate(team_id, .after = season)

w_l_team <- w_reg_results |>
  # filter(season >= 2012) |>
  select(-num_ot, -w_team_id, -w_loc) |>
  rename_with(~ str_replace(.x, "^l", "")) |>
  rename_with(~ str_replace(.x, "^w", "opp_")) |>
  rename_with(~ str_replace(.x, "^_", "")) |>
  rename_with(~ str_replace(.x, "__", "_")) |>
  mutate(win_pct = 0) |>
  relocate(win_pct, .after = day_num) |>
  relocate(team_id, .after = season)

w_all_games <- bind_rows(w_w_team, w_l_team)


# str(m_all_games)

# Cumulative Season Averages
w_team_cumulative_avg <- w_all_games |>
  arrange(season, team_id, day_num) |>
  group_by(season, team_id) |>
  mutate(score_margin = score - opp_score,
         to_margin = to - opp_to,
         fg_perc = fgm / fga,
         fg3_perc = fgm3 / fga3,
         # ft_perc = ftm / fta,
         ast_fgm_ratio = ast / fgm,
         poss = fga - or + to + (.475*fta),
         pts_per_poss = score / poss,
         opp_poss = opp_fga - opp_or + opp_to + (.475*opp_fta),
         opp_pts_per_poss = opp_score / opp_poss,
         across(-c(game_id, day_num),
                ~cummean(.x),
                .names = "c_avg_{.col}")) |>
  group_by(season, team_id)  |>
  mutate(across(starts_with("c_avg_"),
                ~lag(.x, default = 0))) |>
  ungroup() |>
  arrange(season, team_id, game_id) |>
  select(season, day_num, team_id, game_id, starts_with("c_avg")) |>
  group_by(season, team_id) |>
  mutate(game_num = row_number()) |>
  filter(game_num > 5) |>
  select(-game_num) |>
  ungroup()


w_team_rolling_avg <- w_all_games |>
  arrange(team_id, season, day_num) |>
  group_by(team_id, season) |>
  mutate(across(-c(game_id, day_num),
                ~slide_dbl(.x, ~mean(.x, na.rm = TRUE), .before = 2, .complete = FALSE))) |>
  rename_with(.fn = ~paste0("r_avg_", .x), .cols = 5:last_col()) |>
  mutate(across(starts_with("r_avg_"),
                ~lag(.x, default = 0))) |>
  mutate(game_num = row_number()) |>
  filter(game_num > 5) |>
  select(-game_num)

w_team_cumulative_avg <- left_join(w_team_cumulative_avg,
                                   w_team_rolling_avg,
                                   by = c("game_id", "season", "team_id", "day_num"))



# Add indicator variables for tournament teams
w_tourney_teams <- read_csv("data//2024_tourney_seeds.csv") |>
  janitor::clean_names() |>
  filter(tournament == "W") |>
  select(team_id) |>
  unlist() |>
  as.numeric()

pattern <- paste0("team_id2_(", paste(w_tourney_teams, collapse = "|"), ")")

w_team_cumulative_avg <- w_team_cumulative_avg |>
  mutate(team_id2 = factor(team_id)) |>
  relocate(team_id2, .after = team_id) |>
  data.table::as.data.table() |>
  mltools::one_hot(cols = "team_id2", dropCols = TRUE) |>
  as_tibble() |>
  dplyr::select(matches(pattern), names(w_team_cumulative_avg)) |>
  relocate(season, day_num, game_id, team_id) |>
  # mutate(team_id = as.numeric(team_id)) |>
  rename_with(~ str_replace(.x, "team_id2_", "c_team_id_"))


# Combine team cumulative averages with regular season game matchups
w_reg_results <- w_reg_compact_results |>
  select(season, day_num, w_team_id, l_team_id)

w_reg_season_games <- w_reg_results |>
  mutate(w_team = sample(c("a", "b"), n(), replace = TRUE),
         w_team2 = w_team) |>
  pivot_wider(names_from = w_team2, values_from = w_team_id) |>
  mutate(a = ifelse(is.na(a), l_team_id, a),
         b = ifelse(is.na(b), l_team_id, b)) |>
  rename(a_team_id = a,
         b_team_id = b) |>
  select(-l_team_id) |>
  inner_join(w_team_cumulative_avg |> select(-game_id), by = join_by(a_team_id == team_id, season, day_num)) |>
  rename_with(~str_replace(.x, "^c_", "a_c_")) |>
  rename_with(~str_replace(.x, "^r_", "a_r_")) |>
  # select(-game_id) |>
  inner_join(w_team_cumulative_avg |> select(-game_id), by = join_by(b_team_id == team_id, season, day_num)) |>
  rename_with(~str_replace(.x, "^c_", "b_c_")) |>
  rename_with(~str_replace(.x, "^r_", "b_r_"))
# select(-game_id)



# filter(w_reg_season_games, if_any(everything(), is.na))



# Combine team cumulative averages with tournament game matchups
w_tourney_results <- w_tourney_compact_results |>
  filter(season >= 2010) |>
  select(season, day_num, w_team_id, l_team_id)

w_tournament_games <- w_tourney_results |>
  mutate(w_team = sample(c("a", "b"), n(), replace = TRUE),
         w_team2 = w_team) |>
  pivot_wider(names_from = w_team2, values_from = w_team_id) |>
  mutate(a = ifelse(is.na(a), l_team_id, a),
         b = ifelse(is.na(b), l_team_id, b)) |>
  rename(a_team_id = a,
         b_team_id = b) |>
  select(-l_team_id) |>
  left_join(group_by(w_team_cumulative_avg, season, team_id) |> filter(day_num == max(day_num)) |> ungroup() |> select(-day_num),
            by = join_by(a_team_id == team_id, season)) |>
  select(-game_id) |>
  rename_with(~str_replace(.x, "^c_", "a_c_")) |>
  rename_with(~str_replace(.x, "^r_", "a_r_")) |>
  left_join(group_by(w_team_cumulative_avg, season, team_id) |> filter(day_num == max(day_num)) |> ungroup() |> select(-day_num),
            by = join_by(b_team_id == team_id, season)) |>
  select(-game_id) |>
  rename_with(~str_replace(.x, "^c_", "b_c_")) |>
  rename_with(~str_replace(.x, "^r_", "b_r_"))


# Combine regular season and tourney games
w_all_games <- bind_rows(w_reg_season_games, w_tournament_games)



# filter(m_all_games, if_any(everything(), is.na))



#If you do not have an ID per row, use the following code to create an ID
tb <- w_all_games |> mutate(id = row_number())

#Create training set
train <- tb |> sample_frac(.80)

#Create test set
valid  <- anti_join(tb, train, by = 'id') |> select(-id)
train <- select(train, -id)





# Save Data ---------------------------------------------------------------

write_rds(list(train_data = train,
               valid_data = valid),
          # test_data = test),
          "data//w_ff_mod_data.rds")




# Setup Tourney Team Data -------------------------------------------------
#
w_tourney_seeds <- read_csv("data//2024_tourney_seeds.csv") |>
  janitor::clean_names() |>
  filter(tournament == "W")

# m_all_games <- bind_rows(w_team, l_team)
#


w_reg_results <- w_reg_detailed_results |>
  mutate(game_id = row_number()) |>
  # w_team = sample(c("A","B"), n(), replace = TRUE)) |>
  relocate(game_id)

# Create separate tibbles to randomly assign the winners to A or B
w_w_team <- w_reg_results |>
  filter(season == 2024, w_team_id %in% w_tourney_seeds$team_id) |>
  select(-num_ot, -l_team_id, -w_loc) |>
  # mutate(team = "A") |>
  rename_with(~ str_replace(.x, "^w", "")) |>
  rename_with(~ str_replace(.x, "^l", "opp_")) |>
  rename_with(~ str_replace(.x, "^_", "")) |>
  rename_with(~ str_replace(.x, "__", "_")) |>
  mutate(win_pct = 1) |>
  relocate(win_pct, .after = day_num) |>
  relocate(team_id, .after = season)

w_l_team <- w_reg_results |>
  filter(season == 2024, l_team_id %in% w_tourney_seeds$team_id) |>
  select(-num_ot, -w_team_id, -w_loc) |>
  rename_with(~ str_replace(.x, "^l", "")) |>
  rename_with(~ str_replace(.x, "^w", "opp_")) |>
  rename_with(~ str_replace(.x, "^_", "")) |>
  rename_with(~ str_replace(.x, "__", "_")) |>
  mutate(win_pct = 0) |>
  relocate(win_pct, .after = day_num) |>
  relocate(team_id, .after = season)

w_all_games <- bind_rows(w_w_team, w_l_team)


# Cumulative Season Averages
w_tourney_team_avgs <- w_all_games |>
  arrange(team_id, day_num) |>
  group_by(season, team_id) |>
  mutate(score_margin = score - opp_score,
         to_margin = to - opp_to,
         fg_perc = fgm / fga,
         fg3_perc = fgm3 / fga3,
         # ft_perc = ftm / fta,
         ast_fgm_ratio = ast / fgm,
         poss = fga - or + to + (.475*fta),
         pts_per_poss = score / poss,
         opp_poss = opp_fga - opp_or + opp_to + (.475*opp_fta),
         opp_pts_per_poss = opp_score / opp_poss,
         across(-c(game_id, day_num),
                ~mean(.x),
                .names = "c_avg_{.col}")) |>
  ungroup() |>
  arrange(season, team_id, game_id) |>
  select(team_id, starts_with("c_avg")) |>
  distinct(team_id, .keep_all = TRUE)


w_tourney_team_3game_avg <- w_all_games |>
  arrange(team_id, season, day_num) |>
  group_by(team_id, season) |>
  mutate(across(-c(game_id, day_num),
                ~slide_dbl(.x, ~mean(.x, na.rm = TRUE), .before = 2, .complete = FALSE))) |>
  rename_with(.fn = ~paste0("r_avg_", .x), .cols = 5:last_col()) |>
  mutate(across(starts_with("r_avg_"),
                ~lag(.x, default = 0))) |>
  filter(day_num == max(day_num)) |>
  ungroup() |>
  select(-season, -game_id, -day_num)

w_tourney_team_data <- left_join(w_tourney_team_avgs,
                                 w_tourney_team_3game_avg,
                                 by = c("team_id"))


# Add in kenpom and evamiya data

pattern <- paste0("team_id2_(", paste(w_tourney_teams, collapse = "|"), ")")

w_tourney_team_data <- w_tourney_team_data |>
  mutate(team_id2 = factor(team_id)) |>
  relocate(team_id2, .after = team_id) |>
  data.table::as.data.table() |>
  mltools::one_hot(cols = "team_id2", dropCols = TRUE) |>
  as_tibble() |>
  # dplyr::select(matches(pattern), names(m_team_cumulative_avg)) |>
  relocate(team_id) |>
  # mutate(team_id = as.numeric(team_id)) |>
  rename_with(~ str_replace(.x, "team_id2_", "c_team_id_"))

write_rds(w_tourney_team_data, "data//w_tourney_data.rds")

