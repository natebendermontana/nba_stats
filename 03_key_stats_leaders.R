# Key stats leaders
library(ggthemes)
library(tidyverse)
library(tidytext)
library(wesanderson)

#-------------------------------------------------------------------------------
# True Shooting - all
nba_playerboxscores_raw %>%
  filter(!is.na(true_shooting_pct)) %>%
  group_by(athlete_display_name) %>%
  summarise(
    total_points = sum(points, na.rm = TRUE),      
    games_played = n_distinct(game_id),            
    avg_true_shooting_pct = mean(true_shooting_pct),  
    points_per_game = total_points / games_played  
  ) %>%
  filter(points_per_game >= 725 / 82) %>%
  arrange(desc(avg_true_shooting_pct)) %>%
  slice_head(n = 10)

# True Shooting - high volume 3 pt shooters
nba_playerboxscores_raw %>%
  filter(!is.na(true_shooting_pct)) %>%
  group_by(athlete_display_name) %>%
  summarise(
    total_points = sum(points, na.rm = TRUE),  
    total_3pa = sum(three_point_field_goals_attempted),
    games_played = n_distinct(game_id),    
    avg_true_shooting_pct = mean(true_shooting_pct),
    avg_3pa = total_3pa / games_played,
    points_per_game = total_points / games_played  
  ) %>%
  filter(points_per_game >= 725 / 82,
         avg_3pa >= 8) %>%
  arrange(desc(avg_true_shooting_pct)) %>%
  slice_head(n = 10)

#-------------------------------------------------------------------------------
# Faceted graph of top five key stats leaders with historical comparisons
min_games_played_rate <- 57.4 / 82  # ~0.7
min_trueshooting_rate <- 725 / 82 # points per 82 game rate

general_eligible <- total_player_stats_24_25 %>%
  filter(games_played_rate >= min_games_played_rate) %>%
  select(athlete_display_name, avg_points, avg_rebounds, avg_assists, avg_steals, avg_blocks) %>%
  pivot_longer(
    cols = c(avg_points, avg_rebounds, avg_assists, avg_steals, avg_blocks),
    names_to = "stat",
    values_to = "value"
  ) %>%
  group_by(stat) %>%
  arrange(desc(value)) %>%
  slice_head(n = 5) %>%
  ungroup()

trueshooting_eligible <- total_player_stats_24_25 %>%
  filter(avg_points >= min_trueshooting_rate) %>%
  select(athlete_display_name, avg_true_shooting_pct) %>%
  pivot_longer(
    cols = c(avg_true_shooting_pct),
    names_to = "stat",
    values_to = "value"
  ) %>%
  group_by(stat) %>%
  arrange(desc(value)) %>%
  slice_head(n = 5) %>%
  ungroup()

for_plot <- general_eligible %>% 
  rbind(trueshooting_eligible) %>% 
  mutate(ordered_names = reorder_within(athlete_display_name, value, stat))

cbp1 <- c("#005900", "#000078", "#490d00", "#8a034f", "#005a8a", "#443500", "#585858")

ggplot(for_plot, aes(ordered_names, value, fill = stat)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_hline(data = lastfive_reference %>% 
               pivot_longer(
                 cols = c(points_highestrate, rebounds_highestrate, assists_highestrate, steals_highestrate, blocks_highestrate, ts_highestrate),
                 names_to = "stat",
                 values_to = "highestrate"
               ) %>% 
               mutate(stat = recode(
                 stat,
                 "points_highestrate" = "avg_points",
                 "rebounds_highestrate" = "avg_rebounds",
                 "assists_highestrate" = "avg_assists",
                 "steals_highestrate" = "avg_steals",
                 "blocks_highestrate" = "avg_blocks",
                 "ts_highestrate" = "avg_true_shooting_pct"
               )),
             aes(yintercept = highestrate), linetype = "dashed", color = "black") +
  facet_wrap(~stat, scales = "free", labeller = labeller(stat = c(
    "avg_points" = "Points",
    "avg_rebounds" = "Rebounds",
    "avg_assists" = "Assists",
    "avg_steals" = "Steals",
    "avg_blocks" = "Blocks",
    "avg_true_shooting_pct" = "True Shooting %"
  ))) +
  labs(
    title = "2024-25 Per-Game Leaders",
    subtitle = "Compared to highest average (black dashed line) from the last five years",
    x = "Player",
    y = "Stat Value"
  ) +
  scale_x_reordered() +
  theme_solarized() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12),  # Facet labels
    plot.title = element_text(size = 16, face = "bold"),
    panel.grid.major.x = element_blank()
  )
