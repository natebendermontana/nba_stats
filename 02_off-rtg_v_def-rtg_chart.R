# Test creating an offensive eff v defensive efficiency chart
library(ggthemes)
library(tidyverse)
library(ggrepel) 

#-------------------------------------------------------------------------------
# Offensive Efficiency v Defensive Efficiency chart
games_played <- nba_teambox_raw %>%
  group_by(team_name) %>%
  summarise(games_played = n_distinct(game_id)) %>%
  summarise(max_games = max(games_played)) %>%
  pull(max_games)

nba_teambox_raw %>%
  group_by(team_name) %>%
  summarise(
    avg_offensive_eff = mean(offensive_efficiency, na.rm = TRUE),
    avg_defensive_eff = mean(defensive_efficiency, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  ggplot(aes(x = avg_offensive_eff, y = avg_defensive_eff)) +
  geom_point(aes(color = avg_offensive_eff), size = 3, show.legend = FALSE) +
  geom_text_repel(aes(label = team_name), size = 3, max.overlaps = 10, point.padding = unit(1, "lines")) +
  geom_hline(yintercept = mean(nba_teambox_raw$defensive_efficiency, na.rm = TRUE),
             color = "darkgrey", linetype = "dashed", size = 0.6) +
  geom_vline(xintercept = mean(nba_teambox_raw$offensive_efficiency, na.rm = TRUE),
             color = "darkgrey", linetype = "dashed", size = 0.6) +
  annotate("text", x = Inf,
           y = mean(nba_teambox_raw$defensive_efficiency) - .25, 
           label = "Avg Defensive Efficiency",
           hjust = 1.1,
           vjust = 0,
           size = 2.9, color = "black") +
  annotate("text", x = mean(nba_teambox_raw$offensive_efficiency, na.rm = TRUE) - 2.5, 
           y = -Inf, 
           label = "Avg Offensive Efficiency", angle = 0,
           hjust = 0,
           vjust = 1.4,
           size = 2.9, color = "black") +
  scale_y_reverse() +
  labs(
    title = "NBA Efficiency Landscape",
    subtitle = paste("Over the first", games_played, "games of the '24-25 season"),
    x = "\nOffensive Efficiency",
    y = "Defensive Efficiency\n"
  ) +
  theme(legend.position = NULL) +
  theme_solarized()

#-------------------------------------------------------------------------------
# 3PA v 3P% chart
total_player_stats_24_25 %>% 
  arrange(desc(total_three_point_attempts)) %>%
  slice_head(n=20) %>% 
  ggplot(aes(x = total_three_point_attempts, y = three_point_perc)) +
  geom_point(aes(size = avg_points), color = "#3082bc") +  # Map size to avg_points
  geom_text_repel(aes(label = athlete_display_name), size = 3, max.overlaps = 10, point.padding = unit(1.1, "lines")  # Adjust padding based on circle size
) + 
  labs(
    title = "Three-Point Efficiency Landscape",
    subtitle = paste0("Top 20 players by attempts, over ", max(total_player_stats_24_25$games_played), " games"),
    x = "\nTotal Three-Point Attempts",
    y = "Three-Point Percentage\n",
    size = "Points per game"
  ) +
  theme_fivethirtyeight() + 
  theme(
    axis.title = element_text(),  # Add axis titles
    plot.title = element_text(size = 16, face = "bold")  # Center and style title
  )


