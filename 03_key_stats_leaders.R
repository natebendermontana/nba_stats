# Key stats leaders
library(ggthemes)
library(tidyverse)
library(tidytext)
library(gt)

# load data
total_player_stats_24_25 <- read.csv(here("data", "total_player_stats_24_25.csv"))
lastfive_reference <- read.csv(here("data", "lastfive_reference.csv"))

#-------------------------------------------------------------------------------
## ---- tsleaders_3ball
total_player_stats_24_25 %>%
  group_by(athlete_display_name, team_name, athlete_headshot_href) %>%
  filter(!is.na(avg_true_shooting_pct),
                avg_points >= 9,
                avg_3pa >= 8) %>%
  select(athlete_headshot_href, athlete_display_name, team_name, avg_points, avg_true_shooting_pct, avg_3pm) %>% 
  arrange(desc(avg_true_shooting_pct)) %>%
  ungroup() %>% 
  slice_head(n = 10) %>%
  gt() %>% 
  text_transform(
    locations = cells_body(columns = athlete_headshot_href),
    fn = function(x) {
      gt::web_image(url = x, height = 40)
    }
  ) %>%
  # Rename and format the table columns
  cols_label(
    athlete_headshot_href = "",
    athlete_display_name = "Player",
    team_name = "Team",
    avg_points = "PPG  ",
    avg_true_shooting_pct = "TS  ",
    avg_3pm = "Avg 3PM"
  ) %>%
  fmt_number(
    columns = c(avg_points, avg_3pm),
    decimals = 1
  ) %>%
  fmt_percent(
    columns = avg_true_shooting_pct,
    decimals = 1  
  ) %>%
  tab_header(
    title = "TS% Leaders: 3-ball Specialists",
    subtitle = "Minimum 9 points and 8 three point attempts per game."
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", align = "left"),
    locations = cells_column_labels(everything())
  ) %>% 
  tab_style(
    style = cell_text(align = "left"),
    locations = cells_body()  
  ) %>% 
  opt_horizontal_padding(scale = 2)

#-------------------------------------------------------------------------------
## ---- stats_leaders_faceted
# Faceted graph of top five key stats leaders with historical comparisons
min_games_played_rate <- 57.4 / 82  # ~70% of all games
min_trueshooting_rate <- 725 / 82 # points per 82 game rate. Roughly 8.8 ppg.

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
  #rbind(trueshooting_eligible) %>% 
  mutate(ordered_names = reorder_within(athlete_display_name, value, stat))

#cust_palette <- c("#005900", "#000078", "#490d00", "#8a034f", "#005a8a", "#443500", "#585858")

ggplot(for_plot, aes(ordered_names, value, fill = stat)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_hline(data = lastfive_reference %>% 
               pivot_longer(
                 cols = c(points_highestrate, rebounds_highestrate, assists_highestrate, steals_highestrate, blocks_highestrate), #, ts_highestrate),
                 names_to = "stat",
                 values_to = "highestrate"
               ) %>% 
               mutate(stat = recode(
                 stat,
                 "points_highestrate" = "avg_points",
                 "rebounds_highestrate" = "avg_rebounds",
                 "assists_highestrate" = "avg_assists",
                 "steals_highestrate" = "avg_steals",
                 "blocks_highestrate" = "avg_blocks"
                 #"ts_highestrate" = "avg_true_shooting_pct"
               )),
             aes(yintercept = highestrate), linetype = "dashed", color = "black") +
  facet_wrap(~stat, scales = "free", labeller = labeller(stat = c(
    "avg_points" = "Points",
    "avg_rebounds" = "Rebounds",
    "avg_assists" = "Assists",
    "avg_steals" = "Steals",
    "avg_blocks" = "Blocks"
    #"avg_true_shooting_pct" = "True Shooting %"
  ))) +
  labs(
    title = "2024-25 Per-Game Leaders",
    subtitle = "----- Highest average from the last five years.",
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

#-------------------------------------------------------------------------------
## ---- stats_leaders_faceted_ts
# Exploring a good split between Bigs and non-Bigs for showing TS% for both groups
# Grabbing only players with a certain % of threes attempts relative to total attempts
# won't filter out all bigs but it will filter out the paint-only bigs.
non_bigs <- total_player_stats_24_25 %>%
  filter(games_played_rate >= min_games_played_rate,
         avg_3p_pergame >= 3,
         avg_points >= 5) %>% 
  mutate(type = "nonbigs") %>% 
  arrange(desc(avg_true_shooting_pct)) %>%
  slice_head(n = 10)

bigs <- total_player_stats_24_25 %>%
  filter(games_played_rate >= min_games_played_rate,
         avg_3p_pergame <= 3,
         avg_points >= 5) %>% 
  mutate(type = "bigs") %>% 
  arrange(desc(avg_true_shooting_pct)) %>%
  slice_head(n = 10)

for_plot <- bigs %>%
  rbind(non_bigs) %>%
  mutate(ordered_names = fct_reorder(athlete_display_name, avg_true_shooting_pct)) 

ggplot(for_plot, aes(ordered_names, avg_true_shooting_pct, fill = type)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_hline(data = lastfive_reference %>% 
               pivot_longer(
                 cols = c(ts_highestrate),
                 names_to = "stat",
                 values_to = "highestrate"
               ) %>% 
               mutate(stat = recode(
                 stat,
                 "ts_highestrate" = "avg_true_shooting_pct"
               )),
             aes(yintercept = highestrate), linetype = "dashed", color = "black") +
  facet_wrap(~type, scales = "free_x", labeller = labeller(type = c(
    "bigs" = "Interior",
    "nonbigs" = "Perimeter"
  ))) +
  labs(
    title = "2024-25 TS% Leaders",
    subtitle = "----- Highest average from the last five years.",
    x = "Player",
    y = "TS%"
  ) +
  scale_x_reordered() +
  theme_solarized() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12),  # Facet labels
    plot.title = element_text(size = 16, face = "bold"),
    panel.grid.major.x = element_blank()
  )


  
