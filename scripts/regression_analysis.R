library(tidyverse)

# load final_analysis_panel.csv
panel_data <- read_csv("data/final_analysis_panel.csv", show_col_types = FALSE)
View(panel_data)

# high-level: estimate impact of home/away games on assaults
fit_assault <- lm(Assault ~ game_status + factor(ori) + 
    wday(date) + month(date) + factor(year(date)), 
    data = filter(panel_data, game_status != "No Game" | TRUE))
print(fit_assault)

# high-level: estimate impact of home/away games on vandalism
fit_vandalism <- lm(Vandalism ~ game_status + factor(ori) + 
    wday(date) + month(date) + factor(year(date)), 
    data = filter(panel_data, game_status != "No Game" | TRUE))
print(fit_vandalism)