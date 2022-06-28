require(nflfastR)
require(dplyr)

# Getting data ------------------------
future::plan("multisession")

# Loading pbp data
tryCatch(
  # Checking if RData file exist
  {suppressWarnings({pbp <- readRDS('.pbp2021')})},
  # If not loading the 2021 data
  error = function(e) {
    pbp <-
      # Getting the pbp for the last 4 seasons
      nflfastR::fast_scraper_schedules(2021)  |>
      dplyr::pull(game_id) |>
      nflfastR::build_nflfastR_pbp()
  }
)







