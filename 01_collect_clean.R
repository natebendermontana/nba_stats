# Ingest and clean data
library(hoopR)
library(tidyverse)

tictoc::tic()
progressr::with_progress({
  nba_php <- load_nba_team_box(seasons = most_recent_nba_season())
})
tictoc::toc()

nba_php %>% 
  head(10)

test <-  load_nba_player_box(seasons = most_recent_nba_season())
