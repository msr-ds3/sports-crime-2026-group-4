library(tidyverse)

# load batch header file from project dir
batch_header <- read_csv("data/nibrs_batch_header_1991_2024.csv")
# glimpse(batch_header)

# load agencies_schools.csv file that I manually created and made cols
# This is from the Notes at the end of the paper number 8
# that lists the agencies and schools
college_towns <- read_csv("scripts/agencies_schools.csv")

# clean batch header columns to make them match
# converting everything to lowercase to fix the case mismatch found in the raw data
batch_header_clean <- batch_header %>%
  filter(year >= 2000 & year <= 2005) %>%
  # no white spaces
  # convert to lowercase to handle inconsistencies
  # Ex. "montgomery", "al"
  mutate(
    city_clean = str_to_lower(str_trim(city_name)),
    state_clean = str_to_lower(str_trim(state_abbreviation))
  )

# clean manual reference data to match that lowercase layout 
college_towns_clean <- college_towns %>%
  mutate(
    city_match = str_to_lower(str_trim(city_name)),
    state_match = str_to_lower(str_trim(state_abbr))
  )

# inner join on compound key (City and State) using the clean lowercase keys
matched_agencies <- batch_header_clean %>%
  inner_join(
    college_towns_clean, 
    by = c("city_clean" = "city_match", "state_clean" = "state_match")
  )

# collapse duplicates to get unique ORIs
unique_oris <- matched_agencies %>% 
  distinct(ori, city_name.y, state_abbr, team_name, university_name)

# look 
print(unique_oris)
# Q: why are there 32 rows instead of 26
# Ans: some college towns have multiple police agencies ...

# find specific cities that are duplicating
unique_oris %>%
  count(city_name.y, state_abbr, name = "number_of_agencies") %>%
  filter(number_of_agencies > 1)