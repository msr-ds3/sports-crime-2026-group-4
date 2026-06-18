library(tidyverse)

# load final_analysis_panel.csv
panel_data <- read_csv("data/final_analysis_panel.csv", show_col_types = FALSE)

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

# PLOTTING TIME

# data needed for plotting (means and SDs)
plot_data <- appendix_table_1 %>%
  select(Offense, Mean, SD)

# ggplot
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
ggsave("plots/appendix_table_1_summary.png", width = 7, height = 7)