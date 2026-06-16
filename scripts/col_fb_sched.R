library(cfbfastR)
library(dplyr)

years <- 2000:2005

load_cfb_schedules()

# peek at data
glimpse(schedules)

College football schedules: One option is the cfbfastR R package that provides college football game schedules via ESPN (should not require an API key).