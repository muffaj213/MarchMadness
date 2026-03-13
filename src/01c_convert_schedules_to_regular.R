# =============================================================================
# 01c_convert_schedules_to_regular.R - Build MRegularSeasonCompactResults from schedules
# =============================================================================
# Converts raw_schedules/*.csv to M* format using TeamIDs from raw_extended (nishaanamin).
# Maps schedule names to canonical (MTeams) via team_aliases.csv before lookup.
# Run after 01b_build_historical_from_nishaa.R. Output: data/raw_extended/MRegularSeasonCompactResults.csv
# =============================================================================

library(here)
library(readr)
library(dplyr)

source(here("src", "config.R"))
source(here("src", "utils", "bracket_utils.R"))
TEAMS_PATH <- file.path(RAW_EXTENDED_DIR, "MTeams.csv")
SEEDS_PATH <- file.path(RAW_EXTENDED_DIR, "MNCAATourneySeeds.csv")
OUT_PATH <- file.path(RAW_EXTENDED_DIR, "MRegularSeasonCompactResults.csv")

#' Build (Season, TeamName) -> TeamID from seeds + teams (tournament teams use season-specific IDs)
build_season_specific_mapping <- function(seeds_df, teams_df) {
  seeds_df %>%
    left_join(teams_df %>% select(TeamID, TeamName), by = "TeamID") %>%
    filter(!is.na(TeamName)) %>%
    mutate(
      name = trimws(TeamName),
      name_lower = tolower(gsub("\\s+", " ", name))
    ) %>%
    select(Season, TeamID, name, name_lower) %>%
    distinct(Season, name_lower, .keep_all = TRUE)
}

#' Build global name -> TeamID fallback for non-tournament teams
build_global_name_to_id <- function(teams_df) {
  teams <- teams_df %>% mutate(TeamName = trimws(TeamName))
  lookup <- data.frame(
    name = teams$TeamName,
    name_lower = tolower(gsub("\\s+", " ", teams$TeamName)),
    TeamID = teams$TeamID,
    stringsAsFactors = FALSE
  )
  lookup %>% group_by(name_lower) %>% slice(1) %>% ungroup()
}

#' Parse schedule file to extract Season and game rows
parse_schedule <- function(path) {
  season <- NA_integer_
  m <- regmatches(basename(path), regexpr("[0-9]{4}-[0-9]{2}", basename(path)))
  if (length(m) > 0) {
    parts <- strsplit(m[[1]], "-")[[1]]
    season <- as.integer(parts[1]) + 1L  # 2023-24 -> 2024
  }
  df <- read_csv(path, show_col_types = FALSE)
  # Handle different column names
  nm <- tolower(names(df))
  team_col <- names(df)[which(nm %in% c("team", "teamname", "team_name"))[1]]
  opp_col <- names(df)[which(nm %in% c("opp", "opp.", "opponent", "opp_name"))[1]]
  pf_col <- names(df)[which(nm %in% c("points_for", "pf"))[1]]
  pa_col <- names(df)[which(nm %in% c("points_against", "pa"))[1]]
  win_col <- names(df)[which(nm %in% c("win"))[1]]
  if (is.na(team_col)) team_col <- "Team"
  if (is.na(opp_col)) opp_col <- "Opp."
  if (is.na(pf_col)) pf_col <- "Points_For"
  if (is.na(pa_col)) pa_col <- "Points_Against"
  if (is.na(win_col)) win_col <- "Win"
  df %>%
    rename(Team = !!sym(team_col), Opp = !!sym(opp_col),
           Points_For = !!sym(pf_col), Points_Against = !!sym(pa_col),
           Win = !!sym(win_col)) %>%
    mutate(Season = season) %>%
    select(Season, Team, Opp, Points_For, Points_Against, Win)
}

main <- function() {
  if (!file.exists(TEAMS_PATH)) {
    stop("MTeams.csv not found. Run 01b_build_historical_from_nishaa.R first.")
  }
  if (!file.exists(SEEDS_PATH)) {
    stop("MNCAATourneySeeds.csv not found. Run 01b_build_historical_from_nishaa.R first.")
  }
  teams <- read_csv(TEAMS_PATH, show_col_types = FALSE)
  seeds <- read_csv(SEEDS_PATH, show_col_types = FALSE)
  season_map <- build_season_specific_mapping(seeds, teams)
  global_lookup <- build_global_name_to_id(teams)

  schedule_files <- list.files(SCHEDULES_DIR, pattern = "[0-9]{4}-[0-9]{2}_schedule\\.csv$", full.names = TRUE)
  if (length(schedule_files) == 0) {
    message("No schedule files found in ", SCHEDULES_DIR)
    return(invisible(NULL))
  }

  all_games <- list()
  for (f in schedule_files) {
    dat <- parse_schedule(f)
    if (is.na(dat$Season[1]) || nrow(dat) == 0) next
    all_games[[length(all_games) + 1]] <- dat
  }
  if (length(all_games) == 0) stop("No valid schedule data.")
  games <- bind_rows(all_games)

  # Map schedule names to canonical (team_aliases) before lookup
  # Resolve only distinct names (avoids repeated file reads)
  team_names <- unique(c(trimws(as.character(games$Team)), trimws(as.character(games$Opp))))
  canon_lookup <- setNames(
    vapply(team_names, function(x) resolve_to_canonical(x), FUN.VALUE = character(1)),
    team_names
  )
  games <- games %>%
    mutate(
      Team_canonical = canon_lookup[trimws(as.character(Team))],
      Opp_canonical = canon_lookup[trimws(as.character(Opp))],
      Team_name_lower = tolower(gsub("\\s+", " ", trimws(Team_canonical))),
      Opp_name_lower = tolower(gsub("\\s+", " ", trimws(Opp_canonical)))
    )
  # Also use team_name_master for ID lookup (covers teams not in MTeams but in tourney history)
  master_path <- file.path(PROC_DIR, "team_name_master.csv")
  master_lookup <- if (file.exists(master_path)) {
    master <- read_csv(master_path, show_col_types = FALSE)
    name_col <- intersect(names(master), c("Name", "TeamName"))[1]
    if (is.na(name_col)) name_col <- names(master)[1]
    master %>% mutate(name_lower = tolower(gsub("\\s+", " ", trimws(!!sym(name_col))))) %>%
      group_by(name_lower) %>% slice(1) %>% ungroup() %>%
      select(name_lower, TeamID_master = PreferredTeamID)
  } else NULL

  team_ids <- games %>%
    select(Season, Team_name_lower) %>%
    distinct() %>%
    left_join(season_map %>% select(Season, name_lower, TeamID), by = c("Season", "Team_name_lower" = "name_lower")) %>%
    left_join(global_lookup %>% select(name_lower, TeamID) %>% rename(TeamID_global = TeamID), by = c("Team_name_lower" = "name_lower")) %>%
    { if (!is.null(master_lookup)) left_join(., master_lookup, by = c("Team_name_lower" = "name_lower")) else mutate(., TeamID_master = NA_integer_) } %>%
    mutate(TeamID = coalesce(TeamID, TeamID_global, TeamID_master)) %>%
    select(Season, Team_name_lower, TeamID)
  opp_ids <- games %>%
    select(Season, Opp_name_lower) %>%
    distinct() %>%
    left_join(season_map %>% select(Season, name_lower, TeamID), by = c("Season", "Opp_name_lower" = "name_lower")) %>%
    left_join(global_lookup %>% select(name_lower, TeamID) %>% rename(TeamID_global = TeamID), by = c("Opp_name_lower" = "name_lower")) %>%
    { if (!is.null(master_lookup)) left_join(., master_lookup, by = c("Opp_name_lower" = "name_lower")) else mutate(., TeamID_master = NA_integer_) } %>%
    mutate(OppID = coalesce(TeamID, TeamID_global, TeamID_master)) %>%
    select(Season, Opp_name_lower, OppID)
  # Use Win column for winner (PF/PA may be game-total, not team perspective)
  games <- games %>%
    left_join(team_ids, by = c("Season", "Team_name_lower")) %>%
    left_join(opp_ids, by = c("Season", "Opp_name_lower")) %>%
    mutate(
      WTeamID = if_else(as.integer(Win) == 1, TeamID, OppID),
      LTeamID = if_else(as.integer(Win) == 1, OppID, TeamID),
      WScore = pmax(Points_For, Points_Against),
      LScore = pmin(Points_For, Points_Against)
    )

  # Require both teams to resolve; assign DayNum from game order (row order = chronological best guess)
  # DayNum 20-120 approximates regular season; enables late_win_pct (DayNum>=90), recent_*, rest
  results <- games %>%
    filter(!is.na(WTeamID), !is.na(LTeamID), WTeamID != LTeamID) %>%
    mutate(game_key = paste(Season, pmin(WTeamID, LTeamID), pmax(WTeamID, LTeamID), WScore, LScore)) %>%
    group_by(Season) %>%
    mutate(
      game_seq = match(game_key, unique(game_key)),
      n_games_season = n_distinct(game_key),
      DayNum = 20L + as.integer(100 * (game_seq - 1) / max(1, n_games_season - 1))
    ) %>%
    ungroup() %>%
    distinct(Season, WTeamID, LTeamID, WScore, LScore, .keep_all = TRUE) %>%
    select(Season, DayNum, WTeamID, LTeamID, WScore, LScore)

  if (nrow(results) == 0) {
    message("No games could be mapped to TeamIDs. Check name matching.")
    return(invisible(NULL))
  }

  dir.create(dirname(OUT_PATH), recursive = TRUE, showWarnings = FALSE)
  write_csv(results, OUT_PATH)
  message("MRegularSeasonCompactResults written: ", nrow(results), " games. Seasons: ", min(results$Season), "-", max(results$Season))
  message("Output: ", OUT_PATH)
}

main()
