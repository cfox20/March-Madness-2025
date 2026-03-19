
library("tidyverse")
theme_set(theme_minimal())
theme_update(panel.grid.minor = element_blank(),
  plot.title = element_text(hjust = 0.5))
library("rvest")
library("data.table")



scrape_ken_pom <- function(year) {
  url <- glue::glue("https://kenpom.com/index.php?y={year}")

  page <- read_html(url)

  tb <- page |>
    html_node("table") |>
    html_table()

  tb <- tb[,c(2:6, seq(8, 20, by = 2))]


  colnames(tb) <- c("team", "conf", "w_l", "adj_em", "adj_o", "adj_d", "adj_t",
                    "luck", "sos_adj_em", "sos_opp_o", "sos_opp_d", "ncsos_adj_em")

  Sys.sleep(2)

  tb |>
    slice(-1) |>
    mutate(season = year)
}

source("R/config.R")

ken_pom <- map_dfr(2011:TARGET_SEASON, scrape_ken_pom)


# ken_pom |>
  # filter(season == 2011, team == "East Carolina")
# ken_pom_orig |>
#   filter(season == 2011, team == "Montana St.")
# ken_pom_orig |>
#   filter(season == 2011, team == "Western Illinois")
# ken_pom_orig |>
#   filter(season == 2011, team_id == 1.41)

ken_pom <- ken_pom |>
  filter(conf != "Conf", team != "") |>
  mutate(
    team = str_remove(team, "\\s\\d+") |> str_remove_all("\\*"),
    across((.cols = 4:12), .fns = ~as.numeric(.)),
    team = str_replace(team, "^Arkansas Little Rock$", "Ark Little Rock"),
    team = str_replace(team, "^Little Rock$", "Ark Little Rock"),
    team = str_replace(team, "^Charleston$", "Col Charleston"),
    team = str_replace(team, "^College of Charleston$", "Col Charleston"),
    team = str_replace(team, "^College of Charleston$", "Col Charleston"),
    team = str_replace(team, "^IPFW$", "PFW"),
    team = str_replace(team, "^LIU$","LIU Brooklyn"),
    team = str_replace(team, "^Middle Tennessee$","MTSU"),
    team = str_replace(team, "^Nebraska Omaha$","NE Omaha"),
    team = str_replace(team, "^Northern Illinois$","N Illinois"),
    team = str_replace(team, "^Purdue Fort Wayne$","PFW"),
    team = str_replace(team, "^Queens$","Queens NC"),
    team = str_replace(team, "^Sacramento St.$","CS Sacramento"),
    team = str_replace(team, "^Saint Francis$","St Francis PA"),
    team = str_replace(team, "^SIU Edwardsville$","SIUE"),
    team = str_replace(team, "^Southeast Missouri St.$","SE Missouri St"),
    team = str_replace(team, "^Georgia Southern Univ$","GA Southern"),
    team = str_replace(team, "^Southern Indiana$","Southern Indiana"),
    team = str_replace(team, "^Southern Utah$","Southern Utah"),
    team = str_replace(team, "^St. Thomas$","St Thomas MN"),
    team = str_replace(team, "^Tennessee Martin$","TN Martin"),
    team = str_replace(team, "^Texas A&M Commerce$","TX A&M Commerce"),
    team = str_replace(team, "^The Citadel$","Citadel"),
    team = str_replace(team, "^UMass Lowell$","MA Lowell"),
    team = str_replace(team, "^UMKC$","Missouri KC"),
    team = str_replace(team, "^USC Upstate$","SC Upstate"),
    team = str_replace(team, "^UT Rio Grande Valley$","UTRGV"),
    team = str_replace(team, "^Western Carolina$","W Carolina"),
    team = str_replace(team, "^Western Illinois$","W Illinois"),
    team = str_replace(team, "^Southern$","Southern Univ"),
    team = str_replace(team, "^Texas Southern$","TX Southern"),
    team = str_replace(team, "^Western Illinois$","W Illinois")
  )

# ####Clean Team Names so that they can be merged to NCAA data
# # Replacing Southern with Southen Univ forces recorrecting TX Southern & Miss Southern
ken_pom_dt <- ken_pom |> as.data.table()

ken_pom_dt[,team:=gsub("\\.","",team)]
ken_pom_dt[,team:=gsub("Cal St","CS",team)]
ken_pom_dt[,team:=gsub("Albany","SUNY Albany",team)]
ken_pom_dt[,team:=gsub("Abilene Christian","Abilene Chr",team)]
ken_pom_dt[,team:=gsub("American","American Univ",team)]
ken_pom_dt[,team:=gsub("Arkansas Pine Bluff","Ark Pine Bluff",team)]
ken_pom_dt[,team:=gsub("Boston University","Boston Univ",team)]
ken_pom_dt[,team:=gsub("Bethune Cookman","Bethune-Cookman",team)]
ken_pom_dt[,team:=gsub("Birmingham Southern","Birmingham So",team)]
ken_pom_dt[,team:=gsub("Central Michigan","C Michigan",team)]
ken_pom_dt[,team:=gsub("Central Connecticut","Central Conn",team)]
ken_pom_dt[,team:=gsub("Central Arkansas","Cent Arkansas",team)]
ken_pom_dt[,team:=gsub("Coastal Carolina","Coastal Car",team)]
# ken_pom_dt[,team:=gsub("College of Charleston","Col Charleston",team)]
# ken_pom_dt[,team:=gsub("Charleston","Col Charleston",team)]
ken_pom_dt[,team:=gsub("Charleston Southern","Charleston So",team)]
ken_pom_dt[,team:=gsub("Detroit Mercy","Detroit",team)]
ken_pom_dt[,team:=gsub("Eastern Washington","E Washington",team)]
ken_pom_dt[,team:=gsub("Eastern Illinois","E Illinois",team)]
ken_pom_dt[,team:=gsub("Eastern Kentucky","E Kentucky",team)]
ken_pom_dt[,team:=gsub("Eastern Michigan","E Michigan",team)]
ken_pom_dt[,team:=gsub("East Tennessee St","ETSU",team)]
ken_pom_dt[,team:=gsub("Fairleigh Dickinson","F Dickinson",team)]
ken_pom_dt[,team:=gsub("Florida Atlantic","FL Atlantic",team)]
ken_pom_dt[,team:=gsub("FIU","Florida Intl",team)]
ken_pom_dt[,team:=gsub("Fort Wayne","PFW",team)]
ken_pom_dt[,team:=gsub("Purdue Fort Wayne","PFW",team)]
ken_pom_dt[,team:=gsub("Florida Gulf Coast","FL Gulf Coast",team)]
ken_pom_dt[,team:=gsub("George Washington","G Washington",team)]
ken_pom_dt[,team:=gsub("Georgia Southern","Ga Southern",team)]
ken_pom_dt[,team:=gsub("Grambling St","Grambling",team)]
ken_pom_dt[,team:=gsub("Illinois Chicago","IL Chicago",team)]
ken_pom_dt[,team:=gsub("Houston Baptist","Houston Chr",team)]
ken_pom_dt[,team:=gsub("Houston Christian","Houston Chr",team)]
ken_pom_dt[,team:=gsub("Kent St","Kent",team)]
ken_pom_dt[,team:=gsub("Kennesaw St","Kennesaw",team)]
# ken_pom_dt[,team:=gsub("Little Rock","Ark Little Rock",team)]
ken_pom_dt[,team:=gsub("Arkansas Little Rock","Ark Little Rock",team)]
ken_pom_dt[,team:=gsub("Loyola Marymount","Loy Marymount",team)]
ken_pom_dt[,team:=gsub("Louisiana Monroe","ULM",team)]
ken_pom_dt[,team:=gsub("Monmouth","Monmouth NJ",team)]
ken_pom_dt[,team:=gsub("Maryland Eastern Shore","MD E Shore",team)]
ken_pom_dt[,team:=gsub("Mississippi Valley St","MS Valley St",team)]
ken_pom_dt[,team:=gsub("Mount St Mary's","Mt St Mary's",team)]
# ken_pom_dt[,team:=gsub("Montana St","MTSU",team)]
ken_pom_dt[,team:=gsub("Northern Colorado","N Colorado",team)]
ken_pom_dt[,team:=gsub("North Dakota St","N Dakota St",team)]
ken_pom_dt[,team:=gsub("Northern Kentucky","N Kentucky",team)]
ken_pom_dt[,team:=gsub("North Carolina A&T","NC A&T",team)]
ken_pom_dt[,team:=gsub("North Carolina Central","NC Central",team)]
ken_pom_dt[,team:=gsub("North Carolina St","NC State",team)]
ken_pom_dt[,team:=gsub("Northwestern St","Northwestern LA",team)]
ken_pom_dt[,team:=gsub("Prairie View A&M","Prairie View",team)]
ken_pom_dt[,team:=gsub("South Carolina St","S Carolina St",team)]
ken_pom_dt[,team:=gsub("South Dakota St","S Dakota St",team)]
ken_pom_dt[,team:=gsub("Southern Illinois","S Illinois",team)]
ken_pom_dt[,team:=gsub("Southeastern Louisiana","SE Louisiana",team)]
ken_pom_dt[,team:=gsub("Stephen F Austin","SF Austin",team)]
# ken_pom_dt[,team:=gsub("Southern","Southern Univ",team)]
ken_pom_dt[,team:=gsub("Southern Univ Miss","Southern Miss",team)]
ken_pom_dt[,team:=gsub("Saint Joseph's","St Joseph's PA",team)]
ken_pom_dt[,team:=gsub("Saint Louis","St Louis",team)]
ken_pom_dt[,team:=gsub("Saint Mary's","St Mary's CA",team)]
ken_pom_dt[,team:=gsub("Saint Peter's","St Peter's",team)]
ken_pom_dt[,team:=gsub("Texas A&M Corpus Chris","TAM C. Christi",team)]
ken_pom_dt[,team:=gsub("Troy St","Troy",team)]
ken_pom_dt[,team:=gsub("Texas Southern Univ","TX Southern",team)]
ken_pom_dt[,team:=gsub("Louisiana Lafayette","Louisiana",team)]
ken_pom_dt[,team:=gsub("UTSA","UT San Antonio",team)]
ken_pom_dt[,team:=gsub("Western Michigan","W Michigan",team)]
ken_pom_dt[,team:=gsub("Green Bay","WI Green Bay",team)]
ken_pom_dt[,team:=gsub("Milwaukee","WI Milwaukee",team)]
ken_pom_dt[,team:=gsub("Western Kentucky","WKU",team)]
ken_pom_dt[,team:=gsub("College of Charleston","Col Charleston",team)]
ken_pom_dt[,team:=gsub("Loyola Chicago","Loyola-Chicago",team)]


ken_pom_new <- as_tibble(ken_pom_dt) |>
  mutate(team = str_to_lower(team)) |>
  rename(team_name = team)

m_teams <- read_csv(file.path(INPUT_DIR, "MTeams.csv")) |>
  janitor::clean_names() |>
  mutate(team_name = str_to_lower(team_name)) |>
  select(team_id, team_name)

ken_pom <- left_join(ken_pom_new, m_teams, by = "team_name") |>
  drop_na(team_id) |>
  relocate(team_id, season) |>
  select(-team_name)

write_rds(ken_pom, file.path(INPUT_DIR, "ken_pom.rds"))







