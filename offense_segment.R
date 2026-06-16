library(tidyverse)


years <- 2000:2005
segments <- c()

for (year in years){
    segments[[as.character(year)]] <- read_csv(paste0('data/nibrs_offense_segment_', year, '.csv'))
}

