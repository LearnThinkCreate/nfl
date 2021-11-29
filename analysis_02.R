source('data_01.R')


# Pulling Games -----------------------

getGames <- function(season = 2021, weeks = 1:18) {
  
  # This season stats
  games <-   
    pbp |>
    dplyr::filter(season == {{season}}, week %in% {{weeks}})
  
  return(games)
}

getStats <- function(func) {
  
  early_season_down_stats <- 
    func(getGames(weeks = 1:7)) |>
    dplyr::mutate('label' = 'earlySeason')
  
  recent_down_stats <- 
    func(getGames(weeks = 8:17)) |>
    dplyr::mutate('label' = 'recentGames')
  
  data <- 
    rbind(early_season_down_stats, recent_down_stats)
  
  return(data)
}

downStats <- getStats(getDownStats)
playerStats <- getStats(getPlayerStats)
teamStats <- getStats(getTeamStats)
