library(tidyverse)

# load final_analysis_panel.csv
panel_data <- read_csv("data/final_analysis_panel.csv", show_col_types = FALSE)
View(panel_data)

# SATURDAY MEANS
# filter and calculate means + standard errors for Saturdays
saturday_means <- panel_data %>%
  # separate Saturdays only
  mutate(day_of_week = wday(date, label = TRUE, abbr = FALSE)) %>%
  filter(day_of_week == "Saturday") %>% 

  # group by game context
  group_by(game_status) %>%
  
  # calculate summary stats for both crimes
  summarize(
    across(c(Assault, Vandalism), 
           list(
             Mean = ~mean(.),
             SE   = ~sd(.) / sqrt(n()) # Standard Error 
           ),
           .names = "{.col}_{.fn}")
  ) %>%
  
  # reshape wide data to long format so ggplot can facet by crime type
  pivot_longer(
    cols = -game_status, 
    names_to = "Metric", 
    values_to = "Value"
  ) %>%
  separate(Metric, into = c("Offense", "Statistic"), sep = "_") %>%
  pivot_wider(names_from = Statistic, values_from = Value) %>%
  
  # Order game status clean: No Game -> Away -> Home
  mutate(game_status = factor(game_status, levels = c("No Game", "Away", "Home")))

print(saturday_means)

# PLOT SATURDAY MEANS
ggplot(saturday_means, aes(x = game_status, y = Mean, fill = game_status)) +
  # draw average crime bars
  geom_col(color = "black", alpha = 0.85, width = 0.6) +
  
  # adds Standard Error intervals
  geom_errorbar(
    aes(ymin = pmax(0, Mean - SE), ymax = Mean + SE), 
    width = 0.15, 
    linewidth = 0.7,
    color = "black"
  ) +
  
  # split plot into 2 windows for Assault and Vandalism
  facet_wrap(~Offense, scales = "free_y") +
  
  # labels 
  labs(
    title = "Mean Saturday Offenses by Game Status",
    subtitle = "Replication Sample (2000-2005) with Standard Error Bars",
    x = "Game Day Context",
    y = "Average Daily Incidents per Agency"
  ) +
  scale_fill_manual(values = c("No Game" = "red", "Away" = "blue", "Home" = "green")) 

# save vis to plots folder as .png 
# ggsave("plots/saturday_crime_descriptive_fig.png", width = 8, height = 5)