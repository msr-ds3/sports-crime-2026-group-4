library(tidyverse)
library(cfbfastR)

# api key
my_key <- readLines("data/cfbd_api_key.txt", warn = FALSE) %>% str_trim()
Sys.setenv(CFBD_API_KEY = my_key)

# load cleaned lookup table
# gets exact team names used in pipeline
unique_oris <- read_csv("data/target_oris_lookup.csv", show_col_types = FALSE)
target_teams <- unique(unique_oris$team_name)

# pull games for target years
# data repo only goes back to 2001 ...
# might need an API key, I'm getting errors that CollegeFootballData.com now requires API key
# might be easier to just get API key

# create empty list to store schedules year by year
schedules_list <- list()

# loop through each year individually to fix the vapply length error
# > raw_games <- cfbd_game_info(year = 2000:2005)
# Error in vapply(elements, encode, character(1)) : 
# values must be length 1, but FUN(X[[1]]) result is length 6

for (current_year in 2000:2005) {
  print(paste("Pulling schedule for year:", current_year))
  
  # pull single year from API
  year_games <- cfbd_game_info(year = current_year)
  
  schedules_list[[as.character(current_year)]] <- year_games
}

# combine all years into one df
raw_games <- bind_rows(schedules_list)
View(raw_games)

# clean and filter the schedules
schedules_clean <- raw_games %>%
  filter(season_type == "regular") %>%
  mutate(date = as.Date(start_date)) %>%
  select(date, home_team, away_team)
View(schedules_clean)

# reshape data so every team has its own row per game
# 1 game row has a home_team and away_team
# split into 2 separate rows

home_games <- schedules_clean %>%
  filter(home_team %in% target_teams) %>%
  select(date, team_name = home_team) %>%
  mutate(game_status = "Home")
View(home_games)

away_games <- schedules_clean %>%
  filter(away_team %in% target_teams) %>%
  select(date, team_name = away_team) %>%
  mutate(game_status = "Away")
View(away_games)

# combine them into one clean master game log
master_game_log <- bind_rows(home_games, away_games) %>%
  distinct(date, team_name, .keep_all = TRUE)
View(master_game_log)

# save schedules to data folder
write_csv(master_game_log, "data/processed_schedules_2000_2005.csv")

glimpse(master_game_log)