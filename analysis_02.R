source('data_01.R')

# Pulling Games -----------------------

# This season stats
earlySeasonGames <-   
  pbpData |>
  dplyr::filter(season == 2021, week < 8)

recentGames <-   
  pbpData |>
  dplyr::filter(season == 2021, week >= 8)

# Historical data
historicalGames <- 
  pbpData |>
  dplyr::filter(season < 2021)


early_season_down_stats <- getDownStats(earlySeasonGames)
recent_down_stats <- getDownStats(recentGames)

early_season_player_stats <- getPlayerStats(earlySeasonGames)
recent_player_stats <- getPlayerStats(recentGames)

early_season_team_stats <- getTeamStats(earlySeasonGames)
recent_team_stats <- getTeamStats(recentGames)