# Ingest and clean data
library(hoopR)
library(tidyverse)

#-------------------------------------------------------------------------------
# team box scores
tictoc::tic()
progressr::with_progress({
  nba_teambox_raw <- load_nba_team_box(seasons = most_recent_nba_season())
})
tictoc::toc()

nba_teambox_raw %>% 
  head(10)

nba_teambox_raw <- nba_teambox_raw %>%
  group_by(game_id) %>%
  arrange(desc(team_winner)) %>% # so the lead/lag syntax works
  mutate(
    # Select opponent stats dynamically based on team_winner
    opponent_fg_attempts = ifelse(team_winner, lead(field_goals_attempted), lag(field_goals_attempted)),
    opponent_fg_made = ifelse(team_winner, lead(field_goals_made), lag(field_goals_made)),
    opponent_ft_attempts = ifelse(team_winner, lead(free_throws_attempted), lag(free_throws_attempted)),
    opponent_off_rebounds = ifelse(team_winner, lead(offensive_rebounds), lag(offensive_rebounds)),
    opponent_def_rebounds = ifelse(team_winner, lead(defensive_rebounds), lag(defensive_rebounds)),
    opponent_turnovers = ifelse(team_winner, lead(turnovers), lag(turnovers)),
    
    # Calculate possessions for each team using the provided formula
    possessions = 0.5 * (
      (field_goals_attempted + 0.4 * free_throws_attempted -
         1.07 * (offensive_rebounds / (offensive_rebounds + opponent_def_rebounds)) *
         (field_goals_attempted - field_goals_made) + turnovers) +
        (opponent_fg_attempts + 0.4 * opponent_ft_attempts -
           1.07 * (opponent_off_rebounds / (opponent_off_rebounds + defensive_rebounds)) *
           (opponent_fg_attempts - opponent_fg_made) + opponent_turnovers)
    ),
    
    # Offensive Efficiency = Points / Possessions * 100
    offensive_efficiency = (team_score / possessions) * 100
  ) %>%
  
  # Assign defensive efficiency based on opponent's offensive efficiency
  mutate(defensive_efficiency = ifelse(team_winner, 
                                       lead(offensive_efficiency), 
                                       lag(offensive_efficiency))) %>%
  ungroup()


#-------------------------------------------------------------------------------
# play by play
tictoc::tic()
progressr::with_progress({
  nba_pbp_raw <- load_nba_pbp(seasons = most_recent_nba_season())
})
tictoc::toc()

nba_pbp_raw %>% 
  head(10)

#-------------------------------------------------------------------------------
# player box scores - current
nba_playerboxscores_raw <-  load_nba_player_box(seasons = most_recent_nba_season()) %>% 
  mutate(
    # If the player did not play, set TS% to NA
    true_shooting_pct = ifelse(
      did_not_play,
      NA,
      0.5 * (points) / (field_goals_attempted + 0.44 * free_throws_attempted)
    )
  )

# Player stats current season
total_player_stats_24_25 <- nba_playerboxscores_raw %>%
  group_by(athlete_id, athlete_display_name) %>%  # Group by unique player
  summarize(
    total_points = sum(points, na.rm = TRUE),
    games_played = sum(ifelse(did_not_play == FALSE, 1, 0)),  # Only count games player participated in
    team_games = n_distinct(game_id),  # Total unique team games
    games_played_rate = games_played / team_games,
    avg_true_shooting_pct = mean(true_shooting_pct, na.rm = TRUE),
    avg_points = total_points / games_played,
    total_three_point_attempts = sum(three_point_field_goals_attempted, na.rm = TRUE),
    total_three_point_made = sum(three_point_field_goals_made, na.rm = TRUE),
    three_point_perc = total_three_point_made / total_three_point_attempts,
    three_point_pergame = total_three_point_attempts / games_played,
    total_rebounds = sum(rebounds, na.rm = TRUE),
    avg_rebounds = total_rebounds / games_played,
    total_assists = sum(assists, na.rm = TRUE),
    avg_assists = total_assists / games_played,
    total_steals = sum(steals, na.rm = TRUE),
    avg_steals = total_steals / games_played,
    total_blocks = sum(blocks, na.rm = TRUE),
    avg_blocks = total_blocks / games_played) %>% 
  ungroup()

#-------------------------------------------------------------------------------
# player box scores - last five years
current_season <- most_recent_nba_season()  # Get current season (e.g., 2024)
previous_five_seasons <- (current_season - 5):(current_season - 1)  # Last five seasons

nba_playerboxscores_lastfive_raw <- load_nba_player_box(seasons = previous_five_seasons) %>%
  mutate(
    # If the player did not play, set TS% to NA
    true_shooting_pct = ifelse(
      did_not_play,
      NA,
      0.5 * (points) / (field_goals_attempted + 0.44 * free_throws_attempted)
    )
  )

min_games_played_rate <- 57.4 / 82  # ~0.7

lastfive_reference <- nba_playerboxscores_lastfive_raw %>%
  group_by(athlete_id, athlete_display_name) %>%
  summarize(
    games_played = sum(ifelse(did_not_play == FALSE, 1, 0)),  # Only count games player participated in
    team_games = n_distinct(game_id),  # Total unique team games
    games_played_rate = games_played / team_games,
    points_per_game = sum(points, na.rm = TRUE) / games_played,
    avg_true_shooting_pct = mean(true_shooting_pct, na.rm = TRUE),
    rebounds_per_game = sum(rebounds, na.rm = TRUE) / games_played,
    assists_per_game = sum(assists, na.rm = TRUE) / games_played,
    steals_per_game = sum(steals, na.rm = TRUE) / games_played,
    blocks_per_game = sum(blocks, na.rm = TRUE) / games_played,
    .groups = "drop"
  ) %>% 
filter(games_played >= min_games_played_rate * team_games) %>%
  summarize(
    ts_highestrate = max(avg_true_shooting_pct, na.rm = TRUE),
    points_highestrate = max(points_per_game, na.rm = TRUE),
    rebounds_highestrate = max(rebounds_per_game, na.rm = TRUE),
    assists_highestrate = max(assists_per_game, na.rm = TRUE),
    steals_highestrate = max(steals_per_game, na.rm = TRUE),
    blocks_highestrate = max(blocks_per_game, na.rm = TRUE),
  )

