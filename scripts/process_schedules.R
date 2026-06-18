library(tidyverse)
library(cfbfastR)

# load cleaned lookup table 
# gets exact team names used in pipeline
unique_oris <- read_csv("data/target_oris_lookup.csv", show_col_types = FALSE)
target_teams <- unique(unique_oris$team_name)

# pull games for target years 
# data repo only goes back to 2001 ...
# might need an API key, I'm getting errors that CollegeFootballData.com now requires API key
# might be easier to just get API key

raw_games <- load_cfb_schedules(seasons = 2000:2005)

# clean and filter the schedules
schedules_clean <- raw_games %>%
  filter(season_type == "regular") %>%
  mutate(date = as.Date(start_date)) %>%
  select(date, home_team, away_team)

# reshape data so every team has its own row per game
# 1 game row has a home_team and away_team
# split into 2 separate rows

home_games <- schedules_clean %>%
  filter(home_team %in% target_teams) %>%
  select(date, team_name = home_team) %>%
  mutate(game_status = "Home")

away_games <- schedules_clean %>%
  filter(away_team %in% target_teams) %>%
  select(date, team_name = away_team) %>%
  mutate(game_status = "Away")

# combine them into one clean master game log
master_game_log <- bind_rows(home_games, away_games) %>%
  distinct(date, team_name, .keep_all = TRUE)

# save schedules to data folder
write_csv(master_game_log, "data/processed_schedules_2000_2005.csv")

print("football scheds processed and saved success")
glimpse(master_game_log)