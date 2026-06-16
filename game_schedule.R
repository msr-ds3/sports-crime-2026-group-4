install.packages("cfbfastR")
install.packages('usethis')
library(cfbfastR)
usethis::edit_r_environ()



# Accessing team information 
team_info <- cfbd_team_info()

# Getting the regular season info from 2000
regular_games_2000_info <- cfbd_game_info(year = 2000, season_type = "regular")
