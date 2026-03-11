# =============================================================================
# 01b_build_historical_from_nishaa.R - Build M* files from nishaanamin dataset
# =============================================================================
# Converts Tournament Matchups (2008-2025) to NCAA M* format for extended history.
# Run after downloading nishaanamin/march-madness-data.
# Output: Merges into data/raw/ (or writes to data/raw_extended/)
# =============================================================================

library(here)
library(readr)
library(dplyr)

source(here("src", "config.R"))
source(here("src", "utils", "bracket_slots.R"))
OUT_DIR <- RAW_EXTENDED_DIR

#' Parse Tournament Matchups into game results
#' Format: YEAR, BY YEAR NO, TEAM NO, TEAM, SEED, ROUND, CURRENT ROUND, SCORE
#' ROUND = 64,32,16,8,4,2 (bracket size); each game = 2 consecutive rows by ByYearNo
parse_tournament_matchups <- function(path) {
  tm <- read_csv(path, show_col_types = FALSE)
  names(tm) <- c("Year", "ByYearNo", "TeamID", "Team", "Seed", "Round", "CurrentRound", "Score")

  games <- tm %>%
    arrange(Year, desc(ByYearNo)) %>%
    mutate(game_idx = (row_number() - 1) %/% 2)

  paired <- games %>%
    group_by(Year, game_idx) %>%
    filter(n() == 2) %>%
    summarise(
      Season = first(Year),
      WTeamID = TeamID[which.max(Score)],
      LTeamID = TeamID[which.min(Score)],
      WScore = as.integer(max(Score)),
      LScore = as.integer(min(Score)),
      DayNum = as.integer(134 + (7 - as.integer(log2(max(Round))))),
      .groups = "drop"
    ) %>%
    select(Season, DayNum, WTeamID, LTeamID, WScore, LScore)

  paired
}

#' Build MNCAATourneySeeds from Tournament Matchups
#' Each region gets one of each seed 1-16. Assign by (Year, Seed) groups.
build_seeds <- function(tm_path) {
  tm <- read_csv(tm_path, show_col_types = FALSE)
  names(tm) <- c("Year", "ByYearNo", "TeamID", "Team", "Seed", "Round", "CurrentRound", "Score")

  # Get 64 teams per year (all have CurrentRound=64). Seed is 1-16 within region.
  # Four teams per seed; assign each to W,X,Y,Z to get W01..Z16.
  seeds_df <- tm %>%
    filter(CurrentRound == 64) %>%
    distinct(Year, TeamID, Seed, .keep_all = FALSE) %>%
    arrange(Year, Seed, TeamID) %>%
    group_by(Year, Seed) %>%
    mutate(region_ord = row_number()) %>%
    ungroup() %>%
    mutate(
      Region = c("W", "X", "Y", "Z")[region_ord],
      SeedStr = paste0(Region, sprintf("%02d", Seed))
    ) %>%
    select(Season = Year, Seed = SeedStr, TeamID)

  seeds_df
}

#' Build MTeams from unique teams in Tournament Matchups
build_teams <- function(tm_path) {
  tm <- read_csv(tm_path, show_col_types = FALSE)
  names(tm) <- c("Year", "ByYearNo", "TeamID", "Team", "Seed", "Round", "CurrentRound", "Score")

  teams <- tm %>%
    select(TeamID, TeamName = Team) %>%
    distinct(TeamID, .keep_all = TRUE) %>%
    arrange(TeamID)

  teams
}

#' Main: build extended M* files from nishaanamin
main <- function() {
  tm_path <- file.path(NISHAA_DIR, "Tournament Matchups.csv")
  if (!file.exists(tm_path)) {
    stop("Tournament Matchups.csv not found. Download nishaanamin/march-madness-data to data/raw_nishaa/")
  }

  if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

  message("Building MNCAATourneyCompactResults from Tournament Matchups...")
  tourney_results <- parse_tournament_matchups(tm_path)

  message("Building MNCAATourneySeeds...")
  seeds <- build_seeds(tm_path)

  message("Building MTeams...")
  teams <- build_teams(tm_path)

  # MSeasons
  seasons <- tourney_results %>%
    distinct(Season) %>%
    mutate(DayZero = "2020-11-25") %>%
    arrange(Season)

  # MNCAATourneySlots - always use correct NCAA bracket (R2 = 1v8, 2v7, 3v6, 4v5)
  # Never use raw/Kaggle MNCAATourneySlots which has wrong R2 pairings (1v2, 3v4)
  slots <- CORRECT_BRACKET_BASE %>%
    rename(StrongSeed = Strong, WeakSeed = Weak) %>%
    select(Slot, StrongSeed, WeakSeed)

  # Write output
  write_csv(tourney_results, file.path(OUT_DIR, "MNCAATourneyCompactResults.csv"))
  write_csv(seeds, file.path(OUT_DIR, "MNCAATourneySeeds.csv"))
  write_csv(teams, file.path(OUT_DIR, "MTeams.csv"))
  write_csv(seasons, file.path(OUT_DIR, "MSeasons.csv"))
  write_csv(slots, file.path(OUT_DIR, "MNCAATourneySlots.csv"))

  message("Historical tournament data built. Seasons: ", min(seeds$Season), "-", max(seeds$Season))
  message("Output: ", OUT_DIR)
  message("To use: copy/copy these files to data/raw/ or update 02_process_data to read from raw_extended")
  message("Note: run scripts/fix_seeds_from_danvk.R to correct region (W/X/Y/Z) assignments using danvk bracket data")
}

main()
