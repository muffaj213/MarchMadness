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

  # Prefer raw_extended; fill missing seasons from raw if raw_extended lacks them
  reg_ext_path <- file.path(RAW_EXTENDED_DIR, REGULAR_FILE)
  reg_raw_path <- file.path(RAW_DIR, REGULAR_FILE)
  regular_ext <- if (file.exists(reg_ext_path)) {
    read_csv(reg_ext_path, show_col_types = FALSE)
  } else {
    tibble(Season = integer(), DayNum = integer(), WTeamID = integer(), LTeamID = integer(),
          WScore = integer(), LScore = integer())
  }
  regular_raw <- if (file.exists(reg_raw_path) && dir.exists(RAW_DIR)) {
    read_csv(reg_raw_path, show_col_types = FALSE)
  } else {
    tibble(Season = integer(), DayNum = integer(), WTeamID = integer(), LTeamID = integer(),
          WScore = integer(), LScore = integer())
  }
  ext_seasons <- unique(regular_ext$Season)
  raw_only <- regular_raw %>% filter(!Season %in% ext_seasons)
  regular_results <- if (nrow(raw_only) > 0 && nrow(regular_ext) > 0) {
    bind_rows(regular_ext, raw_only)
  } else if (nrow(regular_ext) > 0) {
    regular_ext
  } else {
    regular_raw
  }
  if (nrow(raw_only) > 0) {
    message("  Merged ", nrow(raw_only), " regular-season rows from raw for seasons ",
            paste(range(raw_only$Season), collapse = "-"))
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
  late_win_pct <- compute_late_win_pct(regular_results, day_cutoff = 90)
  recent_win_pct <- compute_recent_win_pct(regular_results, n_games = 10L, tourney_start_day = 134L)
  recent_mov <- compute_recent_mov(regular_results, n_games = 10L, tourney_start_day = 134L)
  if (nrow(late_win_pct) > 0) {
    message("  Late-season win pct: ", nrow(late_win_pct), " team-season rows (DayNum >= 90)")
  }
  if (nrow(recent_win_pct) > 0) {
    message("  Recent win pct (last 10 games): ", nrow(recent_win_pct), " team-season rows")
  }
  if (nrow(recent_mov) > 0) {
    message("  Recent MOV (last 10 games): ", nrow(recent_mov), " team-season rows")
  }

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

  message("Computing head-to-head, SOS, rest...")
  head_to_head <- compute_head_to_head(regular_results)
  sos_stats <- compute_sos(regular_results, win_pct)
  rest_stats <- compute_rest(regular_results, tourney_start_day = 134L)
  if (nrow(head_to_head) > 0) message("  Head-to-head: ", nrow(head_to_head), " team-pair rows")
  if (nrow(sos_stats) > 0) message("  SOS: ", nrow(sos_stats), " team-season rows")
  if (nrow(rest_stats) > 0) message("  Rest: ", nrow(rest_stats), " team-season rows")

  message("Loading home/away win rates and resume stats...")
  lookup <- build_season_team_lookup(raw$tourney_seeds, raw$teams)
  home_away_stats <- load_home_away_win_rates(lookup = lookup)
  resume_stats <- load_resume_stats(lookup = lookup)
  barttorvik_metrics <- load_barttorvik_resume_metrics(lookup = lookup)
  if (nrow(barttorvik_metrics) > 0 && nrow(resume_stats) > 0) {
    resume_stats <- resume_stats %>%
      left_join(barttorvik_metrics, by = c("Season", "TeamID")) %>%
      mutate(barthag = replace_na(barthag, 0.5), elite_sos = replace_na(elite_sos, 0))
    message("  Barttorvik (BARTHAG/ELITE SOS): ", nrow(barttorvik_metrics), " team-season rows merged")
  } else if (nrow(barttorvik_metrics) > 0 && nrow(resume_stats) == 0) {
    resume_stats <- barttorvik_metrics %>%
      mutate(elo = 0, net = 200, wab = 200, barthag = replace_na(barthag, 0.5), elite_sos = replace_na(elite_sos, 0))
    message("  Resume stats from Barttorvik only (BARTHAG/ELITE SOS): ", nrow(resume_stats), " rows")
  } else if (nrow(resume_stats) > 0) {
    resume_stats <- resume_stats %>% mutate(barthag = 0.5, elite_sos = 0)
  }
  if (nrow(home_away_stats) > 0) message("  Home/away: ", nrow(home_away_stats), " team-season rows")
  if (nrow(resume_stats) > 0) message("  Resume (NET/ELO/WAB): ", nrow(resume_stats), " team-season rows")

  message("Building matchup training data...")
  matchup_data <- build_matchup_data(
    tourney_results,
    raw$tourney_seeds,
    win_pct,
    points_stats,
    kenpom_stats = kenpom_stats,
    late_win_pct = late_win_pct,
    head_to_head = head_to_head,
    sos_stats = sos_stats,
    rest_stats = rest_stats,
    home_away_stats = home_away_stats,
    resume_stats = resume_stats,
    recent_win_pct = recent_win_pct,
    recent_mov = recent_mov
  )

  message("Saving processed data...")
  write_csv(win_pct, file.path(PROC_DIR, "win_pct.csv"))
  write_csv(points_stats, file.path(PROC_DIR, "points_stats.csv"))
  if (nrow(late_win_pct) > 0) {
    write_csv(late_win_pct, file.path(PROC_DIR, "late_win_pct.csv"))
  }
  if (nrow(recent_win_pct) > 0) {
    write_csv(recent_win_pct, file.path(PROC_DIR, "recent_win_pct.csv"))
  }
  if (nrow(recent_mov) > 0) {
    write_csv(recent_mov, file.path(PROC_DIR, "recent_mov.csv"))
  }
  if (nrow(kenpom_stats) > 0) {
    write_csv(kenpom_stats, file.path(PROC_DIR, "kenpom_stats.csv"))
  }
  write_csv(matchup_data, file.path(PROC_DIR, "matchup_data.csv"))
  if (nrow(head_to_head) > 0) write_csv(head_to_head, file.path(PROC_DIR, "head_to_head.csv"))
  if (nrow(sos_stats) > 0) write_csv(sos_stats, file.path(PROC_DIR, "sos_stats.csv"))
  if (nrow(rest_stats) > 0) write_csv(rest_stats, file.path(PROC_DIR, "rest_stats.csv"))
  if (nrow(home_away_stats) > 0) write_csv(home_away_stats, file.path(PROC_DIR, "home_away_stats.csv"))
  if (nrow(resume_stats) > 0) write_csv(resume_stats, file.path(PROC_DIR, "resume_stats.csv"))

  # Save seeds and slots for prediction (no processing needed)
  write_csv(raw$tourney_seeds, file.path(PROC_DIR, "tourney_seeds.csv"))
  write_csv(raw$tourney_slots, file.path(PROC_DIR, "tourney_slots.csv"))
  write_csv(raw$teams, file.path(PROC_DIR, "teams.csv"))

  # Build team name master table for ESPN/KenPom -> TeamID resolution
  if (file.exists(here("scripts", "build_team_master.R"))) {
    tryCatch(
      { source(here("scripts", "build_team_master.R"), local = TRUE) },
      error = function(e) message("  Note: Could not rebuild team_name_master.csv: ", conditionMessage(e))
    )
  }

  message("Processing complete. Output in ", PROC_DIR)
}

main()
