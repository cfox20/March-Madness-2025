
# Load Packages -----------------------------------------------------------

library("tidyverse")
library("here")
library("janitor")

# Set Path ----------------------------------------------------------------

data_path <- here(INPUT_DIR)



# Teams -------------------------------------------------------------------

m_teams <- read_csv(here(data_path, "MTeams.csv")) |>
  clean_names()
# summarise(across(everything(), ~sum(is.na(.))))

w_teams <- read_csv(here(data_path, "WTeams.csv")) |>
  clean_names()

# str(m_teams)
# str(w_teams)

# Team files contain the team id and name for reference. The men's teams include first year as D1.


# Seasons -----------------------------------------------------------------

m_seasons <- read_csv(here(data_path, "MSeasons.csv"), col_types = "dcffff") |>
  clean_names() |>
  mutate(day_zero = mdy(day_zero))

w_seasons <- read_csv(here(data_path, "WSeasons.csv"), col_types = "dcffff") |>
  clean_names() |>
  mutate()

# str(m_seasons)
# str(w_seasons)

# Files contain the season, the season day 0 (day 154 = championship), and the region label (WXYZ)


# Tourney Seeds -----------------------------------------------------------

m_tourney_seeds <- read_csv(here(data_path, "MNCAATourneySeeds.csv")) |>
  clean_names() |>
  mutate(
    region = str_extract(seed, "[a-zA-Z]+") |> factor(),
    seed = str_extract(seed, "\\d+") |> as.integer()) |>
  relocate(season, team_id)

w_tourney_seeds <- read_csv(here(data_path, "WNCAATourneySeeds.csv")) |>
  clean_names() |>
  mutate(
    region = str_extract(seed, "[a-zA-Z]+") |> factor(),
    seed = str_extract(seed, "\\d+") |> as.integer()) |>
  relocate(season, team_id)

# str(m_tourney_seeds)
# str(w_tourney_seeds)

# Files contain the year of the season, the team id, their seed, and the region


# Regular Season Compact Results ------------------------------------------

m_reg_compact_results <- read_csv(here(data_path, "MRegularSeasonCompactResults.csv")) |>
  clean_names() |>
  mutate(w_loc = factor(w_loc))
  # summarise(across(everything(), ~sum(is.na(.))))

w_reg_compact_results <- read_csv(here(data_path, "WRegularSeasonCompactResults.csv")) |>
  clean_names() |>
  mutate(w_loc = factor(w_loc))
  # summarise(across(everything(), ~sum(is.na(.))))


# str(m_reg_compact_results)
# str(w_reg_compact_results)


# Tourney Compact Results -------------------------------------------------

m_tourney_compact_results <- read_csv(here(data_path, "MNCAATourneyCompactResults.csv")) |>
  clean_names() |>
  mutate(w_loc = factor(w_loc))
# summarise(across(everything(), ~sum(is.na(.))))

w_tourney_compact_results <- read_csv(here(data_path, "WNCAATourneyCompactResults.csv")) |>
  clean_names() |>
  mutate(w_loc = factor(w_loc))
# summarise(across(everything(), ~sum(is.na(.))))

#
# str(m_reg_compact_results)
# str(w_reg_compact_results)


# Sample Submission -------------------------------------------------------

# sample_sub <- read_csv(here(data_path, "sample_submission.csv"))


# Team Box Scores ---------------------------------------------------------

m_reg_detailed_results <- read_csv(here(data_path, "MRegularSeasonDetailedResults.csv")) |>
  clean_names()
  # summarise(across(everything(), ~sum(is.na(.))))

w_reg_detailed_results <- read_csv(here(data_path, "WRegularSeasonDetailedResults.csv")) |>
  clean_names()
  # summarise(across(everything(), ~sum(is.na(.))))

m_tourney_detailed_results <- read_csv(here(data_path, "MNCAATourneyDetailedResults.csv")) |>
  clean_names()
  # summarise(across(everything(), ~sum(is.na(.))))

w_tourney_detailed_results <- read_csv(here(data_path, "WNCAATourneyDetailedResults.csv")) |>
  clean_names()
  # summarise(across(everything(), ~sum(is.na(.))))


# Team Rankings -----------------------------------------------------------

# Focus on:
# USA
# AP
# EMK
# RPI
# EBP
# ESR
# NET
# POM
m_team_rankings <- read_csv(here(data_path, "MMasseyOrdinals.csv")) |>
  clean_names() |>
  filter(system_name %in% c(
    "USA"
    ,"AP"
    ,"EMK"
    ,"RPI"
    ,"EBP"
    ,"ESR"
    ,"NET"
    ,"POM"
  )) |>
  pivot_wider(names_from = system_name, values_from = ordinal_rank)







