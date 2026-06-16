library(tidyverse)

# load batch header file from project dir
batch_header <- read_csv("../sports-crime-2026-group-4/data/nibrs_batch_header_1991_2024.csv") 
glimpse(batch_header)

# load agencies_schools.csv file that I manually created and made cols
# This is from the Notes at the end of the paper number 8
# that lists the agencies and schools
college_towns <- read_csv("agencies_schools.csv")

# clean batch header columns to make them match
batch_header_clean <- batch_header %>%
  filter(year >= 2000 & year <= 2005) %>%
  # no white spaces
  # convert to title case aka first letter capital
  # Ex. Akron
  mutate(
    city_clean = str_to_title(str_trim(city_name)),
    state_clean = str_trim(state_abbreviation)
  )

# inner join on compound key (City and State)
matched_agencies <- batch_header_clean %>%
  inner_join(
    college_towns, 
    by = c("city_clean" = "city_name", "state_clean" = "state_abbr")
  )

# collapse duplicates to get unique ORIs
unique_oris <- matched_agencies %>% 
  distinct(ori, city_clean, state_clean, team_name, university_name)

# look 
print(unique_oris)