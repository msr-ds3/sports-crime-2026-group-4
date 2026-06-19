install.packages("cfbfastR")
install.packages('usethis')
library(cfbfastR)
library(tidyverse)
usethis::edit_r_environ()

# Accessing team information 
team_info <- cfbd_team_info()

# Getting the schedules from 2000 to 2005
schedules <- purrr::map_dfr(2000:2005, ~espn_cfb_schedule(year = .x))




# Column Names of games_info
# [1] "game_id"            "season"             "week"              
#  [4] "season_type"        "start_date"         "start_time_tbd"    
#  [7] "completed"          "neutral_site"       "conference_game"   
# [10] "attendance"         "venue_id"           "venue"             
# [13] "home_id"            "home_team"          "home_division"     
# [16] "home_conference"    "home_points"        "home_post_win_prob"
# [19] "home_pregame_elo"   "home_postgame_elo"  "away_id"           
# [22] "away_team"          "away_division"      "away_conference"   
# [25] "away_points"        "away_post_win_prob" "away_pregame_elo"  
# [28] "away_postgame_elo"  "excitement_index"   "highlights"        
# [31] "notes"   

# Colnames of team_info
#  [1] "team_id"          "school"           "mascot"           "abbreviation"    
#  [5] "alt_name1"        "alt_name2"        "alt_name3"        "conference"      
#  [9] "division"         "classification"   "color"            "alt_color"       
# [13] "logo"             "logo_2"           "twitter"          "venue_id"        
# [17] "venue_name"       "city"             "state"            "zip"             
# [21] "country_code"     "timezone"         "latitude"         "longitude"       
# [25] "elevation"        "capacity"         "year_constructed" "grass"           
# [29] "dome"

unique_schedules_id <- sort(unique(schedules$home_id))
unique_team_id <- sort(unique(team_info$team_id))

schedules_with_team_info <- dplyr::left_join(
  schedules,
  team_info,
  by = c("home_id" = "team_id") 
)

ori_map <- read_csv('data/nibrs_batch_header_1991_2024.csv')
ori_mapping <- ori_map %>% 
filter(year >= 2000 & year <= 2005) %>% 
View()
