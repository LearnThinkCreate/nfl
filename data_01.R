# Getting data ------------------------
# future::plan("multisession")
# 
# pbp <-
#   # Getting the pbp for the last 4 seasons
#   nflfastR::fast_scraper_schedules(c(2018, 2019, 2020, 2021))  |>
#   dplyr::pull(game_id) |>
#   nflfastR::build_nflfastR_pbp()

pbp <- readRDS('.pbp')

pbpData <- 
  pbp |>
  dplyr::filter(posteam %in% c("PHI", "LAC"))

# Helper function ---------------------

viewNames <- function(df_name) {
  
  View(as.data.frame(names({{df_name}})))
}

# Functions to clean data -------------

cleanOffensiveStats <- function(pbp) {
  
  # Raw Offensive data
  cleanOffensiveStats <-
    pbp |>
    dplyr::filter(
      qb_kneel == 0,
      qb_spike == 0
      ) |>
    dplyr::select(
      # Primary key
      game_id, play_id,
      # Play type
      shotgun, no_huddle, pass_attempt, rush_attempt, 
      play_type_nfl,
      # Play stats
      yards_gained, 
      # Penalties
      penalty, penalty_yards, penalty_type, penalty_team,
      # Passing play stats
      qb_dropback, qb_scramble, pass_length, sack,
      pass_location, qb_hit, air_yards,
      yards_after_catch,
      # Running play stats
      run_location,
      # Passing stats
      passing_yards, complete_pass, interception,
      # Rushing stats
      rushing_yards,
      # Down stats
      down, ydstogo, first_down,
      # 1st down stats
      first_down_rush, first_down_pass, 
      # 3rd down stats
      third_down_converted, third_down_failed,
      # 4th down stats
      fourth_down_converted, fourth_down_failed,
      # Drive stats
      drive, drive_time_of_possession, drive_inside20,
      drive_play_count, drive_first_downs, drive_yards_penalized,
      drive_start_yard_line, fixed_drive_result,
      # Score Differential
      score_differential,
      # Metadata
      yardline_100, posteam, defteam, qtr, quarter_seconds_remaining, 
      game_seconds_remaining, posteam_type, wp, week, season,
      div_game, stadium, start_time,
      spread_line, roof, surface, 
      wind, temp, home_team, away_team, result
    ) |>
    dplyr::mutate(dplyr::across(
      .cols = c(
        play_type_nfl,
        penalty, penalty_type,
        pass_location, 
        run_location,
        down, 
        posteam, defteam, qtr, posteam_type,
        div_game, stadium,
        roof, surface, start_time
      ),
      .fns = factor
      )) |>
    dplyr::filter(!is.na(posteam))
  
  return(cleanOffensiveStats)
}


getPlayerStats <- function(pbp) {
  
  # Cleaning dataset and pulling player stats
  playerStats <- 
    pbp |>
    nflfastR::calculate_player_stats() |>
    dplyr::mutate(
      # Yards Per Game
      passingYardsPerGame = (passing_yards / games),
      rushingYardsPerGame = (rushing_yards / games),
      recievingYardsPerGame = (receiving_yards / games),
      
      # Attempts per game
      passAttemptsPerGame = (attempts / games),
      carriesPerGame = (carries / games),
      catchesPerGame = (receptions / games),
      
      # Yards per attempt
      yardsPerAttempt = (passing_yards / attempts),
      yardsPerCarry = (rushing_yards / carries),
      yardsPerCatch = (receiving_yards / receptions),
      
      # First downs per game 
      passingFirstDownsPerGame = (passing_first_downs / games),
      rushingFirstDownsPerGame = (rushing_first_downs / games),
      receivingFirstDownsPerGame = (receiving_first_downs / games),
      
      # Air stats
      avgAirYardsPerAttempt = (passing_air_yards / attempts),
      recievingAirYardsPerCatch = (receiving_air_yards / receptions),
      
      # Other passing stats
      completionsPerGame = (completions / games),
      completionPercentage = (completions / attempts),
      sacksPerGame = (sacks / games),
      
      # Fantasy
      fantasyPointsPerGame = (fantasy_points / games)
    ) |>
    dplyr::select(
      # Bio stats
      player_id, player_name, games, 
      # Yards Per game
      passingYardsPerGame, rushingYardsPerGame, recievingYardsPerGame,
      # Passing stats
      completionPercentage, passing_tds, interceptions,
      yardsPerAttempt, passAttemptsPerGame, avgAirYardsPerAttempt,
      completionsPerGame, sacksPerGame, sack_fumbles, 
      sack_fumbles_lost, passingFirstDownsPerGame,
      # Rushing stats
      rushing_tds, carriesPerGame, yardsPerCarry, 
      rushing_fumbles, rushing_fumbles_lost,
      rushingFirstDownsPerGame,
      # Receiving stats
      receiving_tds, yardsPerCatch, catchesPerGame, 
      recievingAirYardsPerCatch, receivingFirstDownsPerGame,
      # Fantasy
      fantasyPointsPerGame
    ) |>
    dplyr::arrange(desc(dplyr::across(
      c(passingYardsPerGame, rushingYardsPerGame, recievingYardsPerGame)
    )))
  
  roster <- 
    nflfastR::fast_scraper_roster(2021) |>
    dplyr::select(
      team, position, full_name, status, gsis_id
    )
  
  playerStats <- 
    playerStats |>
    dplyr::inner_join(roster, by = c('player_id' = 'gsis_id'))
  
  return(playerStats)
}


getTimeOfPossession <- function(pbp) {
  
  # Filtering data
  timeOfPossession <- 
    cleanOffensiveStats(pbp) |>
    dplyr::select(
      game_id, drive, drive_time_of_possession,
      posteam, defteam
    ) |>
    unique() |>
    dplyr::filter(!(is.na(drive) | is.na(drive_time_of_possession))) |>
    dplyr::mutate(
      "drive_time_of_possession" = lubridate::ms(drive_time_of_possession),
      "drive_time_of_possession" = lubridate::as.duration(drive_time_of_possession)
      ) |>
    dplyr::group_by(game_id, posteam, defteam) |>
    dplyr::summarise(timeOfPossession = sum(drive_time_of_possession) / 60) |>
    dplyr::ungroup() |>
    dplyr::arrange(posteam, game_id)
  
  return(timeOfPossession)
}


getDriveStats <- function(pbp) {
  
  # Cleaning data
  driveStats <- 
    pbp |>
    cleanOffensiveStats() 
  
  # Splitting drive_start_yard_line field
  fieldPosition = stringr::str_split_fixed(driveStats$drive_start_yard_line, " ", 2)
  
  
  # Assigning split values to the data
  driveStats$drive_start_half = fieldPosition[ , 1]
  driveStats$drive_start_yard = as.numeric(fieldPosition[ , 2])
  
  # Cleaning drive start yard line efficently 
  yardline <- 
    driveStats |> 
    dplyr::select(
      drive_start_yard_line, drive_start_half, drive_start_yard, posteam) |>
    unique()
  
  # Adding the starting yard line
  for (i in 1:nrow(yardline)) {
    
    if (yardline$drive_start_half[i] == '50') {
      yardline$drive_start_yard[i] = 50
    } else if (yardline$drive_start_half[i] == yardline$posteam[i]) {
      yardline$drive_start_yard[i] = (100 - yardline$drive_start_yard[i])
    }
  }
  
# Joining starting yard line with the pbp data
  driveStats <- 
    driveStats |>
    dplyr::select(-drive_start_yard, -drive_start_half) |>
    dplyr::left_join(
      yardline,
      by = c('drive_start_yard_line', 'posteam'),
      na_matches = 'never'
    )
  
  
  # Getting drive stats for each game
  driveStats <- 
    driveStats |>
    dplyr::filter(play_type_nfl %in% c('RUSH', "PASS")) |>
    dplyr::select(
      game_id, drive,
      pass_attempt, rush_attempt,
      drive_play_count, drive_first_downs, 
      penalty, drive_yards_penalized, drive_start_yard,
      fixed_drive_result, posteam, yards_gained
    ) |>
    dplyr::group_by(game_id, drive, posteam) |>
    dplyr::mutate(
      passAttempts = sum(pass_attempt),
      rushAttempts = sum(rush_attempt),
      yardsGained = sum(yards_gained),
      td = ifelse(fixed_drive_result == "Touchdown", 1, 0),
      score = ifelse(
          (fixed_drive_result %in% c("Touchdown", "Field goal")),1,0
        )
      ) |>
    dplyr::select(
        -pass_attempt, -rush_attempt, -yards_gained
      ) |>
    unique() |>
    dplyr::arrange(posteam, game_id, drive) |>
    dplyr::group_by(game_id, posteam) |>
    dplyr::summarise(
      numDrives = dplyr::n(),
      numTds = sum(td),
      numScores = sum(score),
      avgFieldPos = mean(drive_start_yard),
      avgFirstDowns = mean(drive_first_downs),
      avgPlayCount = mean(drive_play_count),
      avgRushes = mean(rushAttempts),
      avgPasses = mean(passAttempts)
    )
  
  return(driveStats)
}


getPassingStats <- function(pbp) {
  
  qbStats <- 
    pbp |>
    cleanOffensiveStats() |> 
    dplyr::filter(penalty == 0, pass_attempt == 1) |>
    dplyr::select(
      game_id, posteam, play_id,
      pass_attempt,
      pass_length, pass_location, qb_hit, 
      sack, air_yards, yards_after_catch,
      passing_yards, interception
    ) 
  
  passLength <- 
    qbStats |>
    dplyr::group_by(game_id, posteam) |>
    dplyr::filter(!is.na(pass_length)) |>
    dplyr::count(pass_length) |>
    tidyr::pivot_wider(
      id_cols = c(game_id, posteam),
      names_from = pass_length,
      values_from = n
    ) |>
    dplyr::rename(
      deep_passes = deep,
      short_passes = short
    )
  
  passLocation <- 
    qbStats |>
    dplyr::group_by(game_id, posteam) |>
    dplyr::filter(!is.na(pass_location)) |>
    dplyr::count(pass_location) |>
    tidyr::pivot_wider(
      id_cols = c(game_id, posteam),
      names_from = pass_location,
      names_prefix = "pass_",
      values_from = n
    )
  
  passingTendacies <- passLength |> dplyr::inner_join(passLocation)
  
  gameStats <- 
    qbStats |>
    dplyr::group_by(game_id, posteam) |>
    dplyr::mutate(
      airYards = ifelse(!is.na(passing_yards), air_yards, 0)
    ) |>
    dplyr::summarise(
     qbHits = sum(qb_hit, na.rm = T),
     sacks = sum(sack, na.rm = T),
     recYAC = sum(yards_after_catch, na.rm = T),
     passingYards = sum(passing_yards, na.rm = T),
     interceptions = sum(interception, na.rm = T),
     airYards = sum(airYards, na.rm = T),
     passAttempts = sum(pass_attempt)
    )
  
  passingStats <- 
    gameStats |>
    dplyr::inner_join(passingTendacies) |>
    dplyr::arrange(posteam, game_id)
  
  return(passingStats)
}


getRushingStats <- function(pbp) {
  
  rushBreakdown <- 
    pbp |>
    cleanOffensiveStats() |> 
    dplyr::filter(penalty == 0, rush_attempt == 1) |>
    dplyr::select(
      game_id, posteam, rushing_yards,
      run_location
    ) |>
    dplyr::group_by(game_id, posteam) |>
    dplyr::filter(!is.na(run_location)) |>
    dplyr::count(run_location) |>
    tidyr::pivot_wider(
      id_cols = c(game_id, posteam),
      names_from = run_location,
      names_prefix = "rush_",
      values_from = n
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(rushAttempts = sum(dplyr::c_across(where(is.integer)), na.rm = T)) |>
    dplyr::arrange(posteam, game_id)
  
  rushStats <- 
    pbp |>
    cleanOffensiveStats() |> 
    dplyr::filter(penalty == 0, rush_attempt == 1) |>
    dplyr::select(
      game_id, posteam, play_id,
      rush_attempt,rushing_yards,
    ) |>
    dplyr::group_by(game_id, posteam) |>
    dplyr::summarise(rushingYards = sum(rushing_yards)) |>
    dplyr::arrange(posteam, game_id)
  
  rushStats <- 
    rushStats |>
    dplyr::inner_join(rushBreakdown)
  
  return(rushStats)
}


getRedzoneStats <- function(pbp) {
  
  redzoneStats <- 
    pbp |>
    cleanOffensiveStats() |>
    dplyr::select(
      game_id, posteam, drive_inside20, fixed_drive_result, 
      drive
    ) |>
    unique() |>
    dplyr::mutate(
      td = ifelse(
        (drive_inside20 == 1 & fixed_drive_result == "Touchdown"), 1, 0),
      score = ifelse(
        (drive_inside20 == 1 & fixed_drive_result %in% c("Touchdown", "Field goal")),
        1,
        0
      )
    ) |>
    dplyr::group_by(game_id, posteam) |>
    dplyr::summarise(
      redzoneAttempts = sum(drive_inside20),
      redzoneTds = sum(td),
      redzoneScores = sum(score)
    ) |>
    dplyr::arrange(posteam, game_id)
  
  return(redzoneStats)
}


getPenaltyStats <- function(pbp) {
  
  penaltyStats <- 
    pbp |>
    cleanOffensiveStats() |>
    dplyr::filter(penalty == 1, 
                  penalty_team == posteam
                  ) |>
    dplyr::select(
      game_id, penalty_type, penalty_yards, posteam
    ) |>
    dplyr::group_by(game_id, posteam, penalty_type) |>
    dplyr::summarise(
      penalty_yards = sum(penalty_yards),
      times_called = dplyr::n()
      ) |>
    dplyr::ungroup() |>
    dplyr::group_by(game_id) |>
    dplyr::mutate(penalties = sum(times_called)) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(
      id_cols = c(game_id, posteam, penalties),
      names_from = penalty_type,
      values_from = penalty_yards
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(penaltyYards = sum(dplyr::c_across(where(is.double)), na.rm = T)) |>
    dplyr::arrange(posteam, game_id)
    
  return(penaltyStats)
  
}


getDownStats <- function(pbp, down = 1:4) {
  
  # Pulling downs data from pbp
  downs <- 
    cleanOffensiveStats(pbp) |>
    dplyr::filter(
      penalty == 0,
      down %in% {{down}},
      play_type_nfl %in% c('RUSH', "PASS")
    ) |>
    dplyr::select(
      game_id, down, yards_gained, 
      ydstogo, play_type_nfl, score_differential, 
      qtr, game_seconds_remaining, drive, first_down,
      yardline_100, posteam
    )
  
  
  # Getting stats 
  downStats <- 
    downs |>
    dplyr::group_by(down, posteam) |>
    dplyr::summarise(
      avgYardsGained = mean(yards_gained),
      sdYardsGained = sd(yards_gained),
      avgYardsToGo = mean(ydstogo),
      conversionRate =  mean(first_down),
      avgFieldPosition = mean(yardline_100)
    ) |>
    dplyr::arrange(posteam)
  
  return(downStats)
}


getResults <- function(pbp) {
  
  results <- 
    pbp |>
    cleanOffensiveStats() |>
    dplyr::mutate(
      result = ifelse(home_team == posteam, result, (result * -1))
    ) |>
    dplyr::mutate(
      win = ifelse(result > 0, 1, 0),
      lose = ifelse(result < 0, 1, 0)
    ) |>
    dplyr::select(game_id, posteam, result, win, lose, week, season) |>
    unique()
  
  return(results)
}


# Getting team stats ------------------

getTeamStats <- function(pbp) {
  
  driveStats <- getDriveStats(pbp)
  
  passingStats <- getPassingStats(pbp)
  
  rushingStasts <- getRushingStats(pbp)
  
  redzoneStats <- getRedzoneStats(pbp)
  
  driveStats <- getDriveStats(pbp)
  
  timeOfPossession <- getTimeOfPossession(pbp)
  
  penaltyStats <- 
    getPenaltyStats(pbp) |>
    dplyr::select(game_id, posteam, penalties, penaltyYards)
  
  results <- getResults(pbp)
  
  teamStats <- 
    passingStats |>
    dplyr::left_join(rushingStasts, by = c('game_id', 'posteam')) |>
    dplyr::left_join(redzoneStats, by = c('game_id', 'posteam')) |>
    dplyr::left_join(driveStats, by = c('game_id', 'posteam')) |>
    dplyr::left_join(timeOfPossession, by = c('game_id', 'posteam')) |>
    dplyr::left_join(penaltyStats, by = c('game_id', 'posteam')) |>
    dplyr::left_join(results, by = c('game_id', 'posteam'))
  
  # Columns to get mean for
  cols <- names(teamStats)[-which(names(teamStats) %in% c(
    'game_id', 'posteam', 'defteam', 'result', 'win', 'lose'
  ))]
  
  teamStats <- 
    teamStats |>
    dplyr::group_by(posteam) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(cols), ~ mean(.x, na.rm = T)),
      wins = sum(win),
      loses = sum(lose)
    ) |>
    dplyr::mutate(games = wins + loses)
  
  return(teamStats)
}







