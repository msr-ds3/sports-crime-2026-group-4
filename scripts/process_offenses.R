library(tidyverse)
library(vroom) # fast file reader for large csv files 

# load lookup table of 32 ORIs generated in team_agency_mapping script 
unique_oris <- read_csv("data/target_oris_lookup.csv")

# create empty list to store clean aggregated data for each year
# aggregated as in massive data and condense to structured high level summary
# like avgs, counts, totals (at least that's the goal)
crime_years_list <- list()

# loop through each year from 2000 to 2005 to process files one by one
# go back and figure out a way to do this without for loop - penny note to self
for (current_year in 2000:2005) {
  
  # construct path to the offense file for the current loop year
  file_path <- str_glue("data/nibrs_offense_segment_{current_year}.csv")
  
  # print for sanity so i know which R is running
  print(paste("Processing offense file for year:", current_year))
  
  # read massive file but filter it to save computer memory
  # col_select load the 3 cols I care about 
  year_data <- vroom(file_path, col_select = c(ori, incident_date, ucr_offense_code)) %>%
    # filter immediately for the 32 matched ORIs
    filter(ori %in% unique_oris$ori) %>%
    mutate(date = as.Date(incident_date)) %>%
    # create dummy cols for tracking target crimes
    # look up nibrs offense codes
    # 13 = Assault
    # 13A = Aggravated
    # 13B = Simple
    # 13C = Intimidation
    # 290 = Vandalism

# (?i) case insenstive matching (make it more robust)
    mutate(
      is_assault = if_else(str_detect(str_to_lower(ucr_offense_code), "(?i)assault"), 1, 0),
      is_vandalism = if_else(str_detect(str_to_lower(ucr_offense_code), "(?i)vandalism"), 1, 0)
    ) %>%
    # join to get clean city and team names mapped to ORIs
    left_join(unique_oris, by = "ori") %>%
    # GROUP BY THE CITY/TEAM AND DATE (not the ORI!)
    # collapses city and police rows into one daily total
    group_by(team_name, city_name.y, state_abbr, date) %>%
    # sum up the crimes for the whole town
    summarize(
      # remove na vals before performing calculations
      # for example: so instead of na it gives us a mean like 3.33
      total_assaults = sum(is_assault, na.rm = TRUE),
      total_vandalism = sum(is_vandalism, na.rm = TRUE),
      .groups = "drop"
    )

  # store this year's clean data into list
  crime_years_list[[as.character(current_year)]] <- year_data
}

# combine all the clean processed years into one master dataset table
df_daily_city_crime <- bind_rows(crime_years_list) %>%
# drop any messy spillover dates from late 1999 or early 2006
  filter(date >= "2000-01-01" & date <= "2005-12-31") %>%

# CRITICAL: Turn implicit missing dates into explicit rows with 0 crime. 
# If a college town had 0 crimes on a given day, NIBRS omits it. 
# force it back in
complete( 
    nesting(team_name, city_name.y, state_abbr), 
    date = seq.Date(as.Date("2000-01-01"), as.Date("2005-12-31"), by = "day"), 
    fill = list(total_assaults = 0, total_vandalism = 0) 
  ) 

# save the clean aggregated crime data to data folder
write_csv(df_daily_city_crime, "data/processed_daily_crime_2000_2005.csv")

# sanity checks
# 1) view blueprint of final dataset
# 2) verify we have 26 distinct college teams 
# 3) use to cross verify matches in paper

# look
glimpse(df_daily_city_crime)

# why is there 1999 ???
# late reports? as in late reported incidents spill over to 2000
# filter(date >= "2000-01-01" & date <= "2005-12-31")
# dont forget to pipe it; just did 

# count unique team names sanity check
# 26
df_daily_city_crime %>% 
  summarize(n_unique_teams = n_distinct(team_name))

# print unique team names 
df_daily_city_crime %>%
  distinct(team_name) %>%
  arrange(team_name) %>%
  View()