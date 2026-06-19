library(tidyverse)
library(lubridate) # for table 2

# load final_analysis_panel.csv
panel_data <- read_csv("data/final_analysis_panel.csv", show_col_types = FALSE)
View(panel_data)

# APPENDIX TABLE 1
appendix_table_1 <- panel_data %>%
  # separate assault and vandalism across all tracking rows
  summarize(
    across(c(Assault, Vandalism), 
           list(
             Mean = ~mean(.),
             SD = ~sd(.),
             Min = ~min(.),
             Q25 = ~quantile(., 0.25),
             Median = ~median(.),
             Q75 = ~quantile(., 0.75),
             Q90 = ~quantile(., 0.90),
             Max = ~max(.)
           ),
           .names = "{.col}_{.fn}")
  ) %>%
  # reshape to pivot the table so it looks nicer
  pivot_longer(everything(), names_to = "Metric", values_to = "Value") %>%
  separate(Metric, into = c("Offense", "Statistic"), sep = "_") %>%
  pivot_wider(names_from = Statistic, values_from = Value)

print(appendix_table_1)

# PLOT APPENDIX TABLE 1

# data needed for plotting (means and SDs)
plot_data <- appendix_table_1 %>%
  select(Offense, Mean, SD)

# ggplot for errorbar
ggplot(plot_data, aes(x = Offense, y = Mean, fill = Offense)) +
  geom_col(color = "black", alpha = 0.8, width = 0.5) +
  
  # SD error bars to show data spread
  # pmax for lower bound doesn't go negative crime count
  geom_errorbar(
    aes(ymin = pmax(0, Mean - SD), ymax = Mean + SD), 
    width = 0.15, 
    color = "black",
    linewidth = 0.7
  ) +
  labs(
    title = "Appendix Table 1: Descriptive Statistics for Count Variables",
    subtitle = "Replication Sample Baseline (2000-2005)",
    x = "Offense Type",
    y = "Average Daily Incidents per Town"
  ) +
  scale_fill_manual(values = c("Assault" = "blue", "Vandalism" = "orange")) +

# save vis to plots folder as .png
# ggsave("plots/appendix_table_1_summary.png", width = 7, height = 7)

# TABLE 2
# want: day of week, games, observations
panel_data %>% 
# filter to game days
filter(game_status != "No Game") %>% 
mutate(day = wday(date, label = TRUE, abbr = FALSE)) %>% 
select(date, game_status, day) %>% 
head(5)

table_2_complete <- panel_data %>%
  filter(game_status %in% c("Home", "Away")) %>%
  distinct(date, team_name, game_status) %>%
  # extract day of the week
  mutate(day_of_week = wday(date, label = TRUE, abbr = FALSE)) %>%
  # count existing days
  count(day_of_week, name = "Game_Count") %>%
  # include all possible days of the week, fill missing ones with 0
  # monday was missing but in original paper there was 1 game on monday
  complete(day_of_week = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"), 
           fill = list(Game_Count = 0)) %>%
  # calculate percentages 
  mutate(Percentage = (Game_Count / sum(Game_Count)) * 100)

print(table_2_complete)

# PLOT TABLE 2
ggplot(table_2_complete, aes(x = day_of_week, y = Percentage, fill = day_of_week)) +
  # border of bars
  geom_col(color = "black", alpha = 0.85, width = 0.6) +
  
  # Add text labels on top of the bars showing the actual game counts
  geom_text(
    aes(label = sprintf("n = %d", Game_Count)), 
  ) +
  labs(
    title = "Table 2: Distribution of Game Days by Day of the Week",
    subtitle = "Replication Sample (2000-2005)",
    x = "Day of the Week",
    y = "Percentage of Total Games"
  ) 

# save vis to plots folder as .png 
# ggsave("plots/table_2_game_distribution.png", width = 7, height = 5)