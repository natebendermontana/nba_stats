# Ingest and clean data
library(hoopR)
library(tidyverse)

# team box scores
tictoc::tic()
progressr::with_progress({
  nba_teambox_raw <- load_nba_team_box(seasons = most_recent_nba_season())
})
tictoc::toc()

nba_teambox_raw %>% 
  head(10)

# play by play
tictoc::tic()
progressr::with_progress({
  nba_teambox_raw <- load_nba_team_box(seasons = most_recent_nba_season())
})
tictoc::toc()

# player box scores
test <-  load_nba_player_box(seasons = most_recent_nba_season())
