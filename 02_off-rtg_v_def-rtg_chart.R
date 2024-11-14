# Test creating an offensive eff v defensive efficiency chart
library(ggthemes)

games_played <- nba_teambox_raw %>%
  group_by(team_name) %>%
  summarise(games_played = n_distinct(game_id)) %>%
  summarise(avg_games = mean(games_played)) %>%
  pull(avg_games) %>%
  round()  # Round to the nearest integer

nba_teambox_raw %>%
  select(game_id, game_date, team_name, team_winner, team_score, possessions, 
         offensive_efficiency, defensive_efficiency) %>%
  group_by(team_name) %>%
  summarise(
    avg_offensive_eff = mean(offensive_efficiency, na.rm = TRUE),
    avg_defensive_eff = mean(defensive_efficiency, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  ggplot(aes(x = avg_offensive_eff, y = avg_defensive_eff)) +
  geom_point(aes(color = avg_offensive_eff), size = 3, show.legend = FALSE) +
  geom_text(aes(label = team_name), vjust = 1.5, hjust = 0.5, size = 3) +
  
  # Add league-wide average lines
  geom_hline(yintercept = mean(nba_teambox_raw$defensive_efficiency, na.rm = TRUE), 
             color = "black", linetype = "solid", size = 0.5) +
  geom_vline(xintercept = mean(nba_teambox_raw$offensive_efficiency, na.rm = TRUE), 
             color = "black", linetype = "solid", size = 0.5) +
  
  # Add labels for the average lines
  annotate("text", x = mean(nba_teambox_raw$offensive_efficiency, na.rm = TRUE) + 6,
           y = mean(nba_teambox_raw$defensive_efficiency), label = "Offensive Efficiency",
           hjust = 0, vjust = -0.5, size = 2.8, color = "black") +
  
  annotate("text", x = mean(nba_teambox_raw$offensive_efficiency, na.rm = TRUE) + 0.2, 
           y = mean(nba_teambox_raw$defensive_efficiency, na.rm = TRUE) + 5, 
           label = "Defensive Efficiency", angle = 90, 
           hjust = 1, size = 2.8, color = "black") +
  
  scale_y_reverse() +
  labs(
    title = "NBA Efficiency Landscape",
    subtitle = paste("Over the first", games_played, "games of the '24-25 season"),
  ) +
  theme(legend.position = NULL) +
  theme_fivethirtyeight()
