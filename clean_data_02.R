source('data_01.R')


# Pulling Data ------------------------

getGames <- function(season = 2021, weeks = 1:18) {
  
  # This season stats
  games <-   
    pbp |>
    dplyr::filter(season == {{season}}, week %in% {{weeks}})
  
  return(games)
}

getStats <- function(func) {
  
  early_season_stats <- 
    func(getGames(weeks = 1:7)) |>
    dplyr::mutate('label' = 'Weeks 1-7')
  
  recent_stats <- 
    func(getGames(weeks = 8:17)) |>
    dplyr::mutate('label' = 'Weeks 7-12')
  
  season_stats <- 
    func(getGames()) |>
    dplyr::mutate('label' = '2021 Season')
  
  
  data <- 
    rbind(early_season_stats, recent_stats, season_stats)
  
  return(data)
}

playerStats <- 
  getStats(getPlayerStats) |>
  dplyr::mutate_if(is.numeric, ~replace(., is.infinite(.), 0)) |>
  data.table::as.data.table()

# Tableau Helper ----------------------

printTeamColors <- function() {
  logos <- 
    nflfastR::teams_colors_logos |>
    dplyr::filter(!(team_abbr %in% c('LA', "SD", "STL", 'OAK')))
  
  for (i in 1:nrow(logos)) {
    message(paste("<color>", logos$team_color[i], '</color>'))
  }
}

# Google Sheets Helpers ---------------
updateSheet <- function(data, sheet_name, autofit = T, 
                        email = "whyson@tampaprep.org") {
  # Setting auth email so function will work in non-interactive environment
  withr::local_options(
    gargle_oauth_email = email
  )
  
  sheet_id <- 
    googlesheets4::gs4_find({{sheet_name}}) |>
    dplyr::pull(id)
  
  googlesheets4::range_clear(sheet_id)
  
  googlesheets4::range_write(
    sheet_id, 
    data
  )
  
  if (autofit) {
    googlesheets4::range_autofit(
      ss = sheet_id
    )
  }
}

# Updating Data -----------------------

updateSheet(playerStats, "Player Stats")
updateSheet(getStats(getTeamStats), "Team Stats")


