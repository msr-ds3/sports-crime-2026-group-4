library(tidyverse)

# loaded clean data 
daily_city_crime   <- read_csv("data/processed_daily_crime_2000_2005.csv", show_col_types = FALSE)
target_oris_lookup <- read_csv("data/target_oris_lookup.csv", show_col_types = FALSE)
processed_schedules <- read_csv("data/processed_schedules_2000_2005.csv", show_col_types = FALSE)

# Date-by-ORI calendar spine aka the baseline
# so every agency has a row for the days in 2000-2005 context window
all_dates <- seq(as.Date("2000-01-01"), as.Date("2005-12-31"), by = "day")
View(all_dates)

date_agency_spine <- crossing(
  date = all_dates,
  ori = unique(target_oris_lookup$ori)
) %>%
  # attach agency names, team names, and states to this master timeline
  inner_join(target_oris_lookup, by = "ori")
View(date_agency_spine)

# haha i forget what's in my data ...
# daily_city_crime is already in a wide format so separate assault/vandalism cols
# left_join on date, team, city, and state
# layers historical crime counts directly onto complete calendar tracking spine
spine_with_crime <- date_agency_spine %>% 
  left_join(daily_city_crime, 
            by = c("date", "team_name", "city_name.y", "state_abbr")) %>% 
# if a town had zero reported crimes on a given day,
# left_join outputs NA
# convert NAs to 0 so control days are 
# accurately captured in summary stats and regression baselines
  mutate( 
    Assault = replace_na(total_assaults, 0), 
    Vandalism = replace_na(total_vandalism, 0) 
    ) %>% 
    select(-total_assaults, -total_vandalism) # cleaning up old names ?
    # hope this doesnt become a problem later
    # but drop the raw unstandardized cols for clean dataset
View(spine_with_crime)
nrow(spine_with_crime)

# merge Football Schedules onto the timeline
final_analysis_panel <- spine_with_crime %>%
  left_join(processed_schedules, by = c("date", "team_name")) %>%
  # if a date has no match in your schedule log, it was an implicit "No Game" day
  mutate(game_status = replace_na(game_status, "No Game"))
View(final_analysis_panel)
colnames(final_analysis_panel)
nrow(final_analysis_panel)

# save master model dataset
write_csv(final_analysis_panel, "data/final_analysis_panel.csv")

# look
glimpse(final_analysis_panel)