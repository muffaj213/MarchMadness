# =============================================================================
# 02_process_data.R - Read raw data, join, engineer features, write processed
# =============================================================================

library(here)
library(readr)
library(dplyr)

# Source utilities
source(here("src", "utils", "feature_engineering.R"))
source(here("src", "utils", "kenpom_utils.R"))

RAW_DIR <- here("data", "raw")
RAW_EXTENDED_DIR <- here("data", "raw_extended")
PROC_DIR <- here("data", "processed")

REQUIRED_FILES <- c(
  "MTeams.csv", "MSeasons.csv", "MNCAATourneyCompactResults.csv",
  "MNCAATourneySeeds.csv", "MNCAATourneySlots.csv"
)
REGULAR_FILE <- "MRegularSeasonCompactResults.csv"

#' Choose dir for a file (raw vs raw_extended); raw_extended overrides for extended-years data
choose_dir <- function(f) {
  p_ext <- file.path(RAW_EXTENDED_DIR, f)
  p_raw <- file.path(RAW_DIR, f)
  if (file.exists(p_ext)) p_ext else p_raw
}

#' Read all raw CSVs into a list
read_raw_data <- function() {
  dir_used <- if (dir.exists(RAW_EXTENDED_DIR)) RAW_EXTENDED_DIR else RAW_DIR
  paths <- file.path(dir_used, REQUIRED_FILES)
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0) {
    stop("Missing raw data. Run 01_download_data.R and/or 01b_build_historical_from_nishaa.R.\nMissing: ",
         paste(basename(missing), collapse = ", "))
  }

  # When using raw_extended, only use regular from raw_extended (never raw - different TeamIDs)
  regular_path <- if (dir.exists(RAW_EXTENDED_DIR)) {
    file.path(RAW_EXTENDED_DIR, REGULAR_FILE)
  } else {
    file.path(RAW_DIR, REGULAR_FILE)
  }
  regular_results <- if (file.exists(regular_path)) {
    read_csv(regular_path, show_col_types = FALSE)
  } else {
    tibble(Season = integer(), DayNum = integer(), WTeamID = integer(), LTeamID = integer(),
          WScore = integer(), LScore = integer())
  }
  if (nrow(regular_results) == 0) {
    message("No regular season data; using seed-based features only for those seasons.")
  }

  list(
    teams = read_csv(file.path(dir_used, "MTeams.csv"), show_col_types = FALSE),
    seasons = read_csv(file.path(dir_used, "MSeasons.csv"), show_col_types = FALSE),
    tourney_results = read_csv(file.path(dir_used, "MNCAATourneyCompactResults.csv"), show_col_types = FALSE),
    tourney_seeds = read_csv(file.path(dir_used, "MNCAATourneySeeds.csv"), show_col_types = FALSE),
    tourney_slots = read_csv(file.path(dir_used, "MNCAATourneySlots.csv"), show_col_types = FALSE),
    regular_results = regular_results
  )
}

#' Main processing pipeline
main <- function() {
  if (!dir.exists(PROC_DIR)) dir.create(PROC_DIR, recursive = TRUE)

  message("Reading raw data...")
  raw <- read_raw_data()

  # Normalize column names (some Kaggle versions use different names)
  tourney_results <- raw$tourney_results
  if (!"WTeamID" %in% names(tourney_results)) {
    idx <- grep("W.*Team|Winner", names(tourney_results), ignore.case = TRUE)
    if (length(idx) >= 1) names(tourney_results)[idx[1]] <- "WTeamID"
  }
  if (!"LTeamID" %in% names(tourney_results)) {
    idx <- grep("L.*Team|Loser", names(tourney_results), ignore.case = TRUE)
    if (length(idx) >= 1) names(tourney_results)[idx[1]] <- "LTeamID"
  }

  regular_results <- raw$regular_results
  if (!"WTeamID" %in% names(regular_results)) {
    idx <- grep("W.*Team|Winner", names(regular_results), ignore.case = TRUE)
    if (length(idx) >= 1) names(regular_results)[idx[1]] <- "WTeamID"
  }
  if (!"LTeamID" %in% names(regular_results)) {
    idx <- grep("L.*Team|Loser", names(regular_results), ignore.case = TRUE)
    if (length(idx) >= 1) names(regular_results)[idx[1]] <- "LTeamID"
  }

  message("Computing team statistics...")
  win_pct <- compute_win_pct(regular_results)
  points_stats <- compute_points_stats(regular_results)

  message("Loading KenPom data...")
  kenpom_stats <- load_kenpom_stats(raw$tourney_seeds, raw$teams)
  if (nrow(kenpom_stats) > 0) {
    message("  KenPom: ", nrow(kenpom_stats), " team-season rows from ",
            min(kenpom_stats$Season), "-", max(kenpom_stats$Season))
    # Fill in missing win_pct from KenPom (e.g. for 2025 when no regular-season data)
    if ("win_pct" %in% names(kenpom_stats)) {
      kp_win <- kenpom_stats %>%
        filter(!is.na(win_pct)) %>%
        mutate(
          Wins = if ("Wins" %in% names(.)) Wins else round(win_pct * 32),
          Losses = if ("Losses" %in% names(.)) Losses else round((1 - win_pct) * 32),
          Games = if ("Games" %in% names(.)) Games else Wins + Losses
        ) %>%
        select(Season, TeamID, WinPct = win_pct, Wins, Losses, Games)
      missing <- kp_win %>% anti_join(win_pct, by = c("Season", "TeamID"))
      if (nrow(missing) > 0) {
        win_pct <- bind_rows(win_pct, missing)
        message("  Filled ", nrow(missing), " win_pct rows from KenPom for missing seasons/teams")
      }
    }
  } else {
    message("  No KenPom data found; model will use seed/winpct/pf features only.")
  }

  message("Building matchup training data...")
  matchup_data <- build_matchup_data(
    tourney_results,
    raw$tourney_seeds,
    win_pct,
    points_stats,
    kenpom_stats = kenpom_stats
  )

  message("Saving processed data...")
  write_csv(win_pct, file.path(PROC_DIR, "win_pct.csv"))
  write_csv(points_stats, file.path(PROC_DIR, "points_stats.csv"))
  if (nrow(kenpom_stats) > 0) {
    write_csv(kenpom_stats, file.path(PROC_DIR, "kenpom_stats.csv"))
  }
  write_csv(matchup_data, file.path(PROC_DIR, "matchup_data.csv"))

  # Save seeds and slots for prediction (no processing needed)
  write_csv(raw$tourney_seeds, file.path(PROC_DIR, "tourney_seeds.csv"))
  write_csv(raw$tourney_slots, file.path(PROC_DIR, "tourney_slots.csv"))
  write_csv(raw$teams, file.path(PROC_DIR, "teams.csv"))

  message("Processing complete. Output in ", PROC_DIR)
}

main()
