# =============================================================================
# 01c_convert_schedules_to_regular.R - Build MRegularSeasonCompactResults from schedules
# =============================================================================
# Converts raw_schedules/*.csv to M* format using TeamIDs from raw_extended (nishaanamin).
# Run after 01b_build_historical_from_nishaa.R. Output: data/raw_extended/MRegularSeasonCompactResults.csv
# =============================================================================

library(here)
library(readr)
library(dplyr)

SCHEDULES_DIR <- here("data", "raw_schedules")
TEAMS_PATH <- here("data", "raw_extended", "MTeams.csv")
OUT_PATH <- here("data", "raw_extended", "MRegularSeasonCompactResults.csv")

#' Build name -> TeamID map from MTeams; handle common spelling variants
build_name_to_id <- function(teams_df) {
  teams <- teams_df %>% mutate(TeamName = trimws(TeamName))
  exact <- setNames(teams$TeamID, teams$TeamName)

  # Add lowercase normalized keys for matching
  for (i in seq_len(nrow(teams))) {
    nn <- tolower(gsub("\\s+", " ", trimws(teams$TeamName[i])))
    if (!nn %in% names(exact)) exact[nn] <- teams$TeamID[i]
  }
  exact
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
  if (is.na(team_col)) team_col <- "Team"
  if (is.na(opp_col)) opp_col <- "Opp."
  if (is.na(pf_col)) pf_col <- "Points_For"
  if (is.na(pa_col)) pa_col <- "Points_Against"
  df %>%
    rename(Team = !!sym(team_col), Opp = !!sym(opp_col),
           Points_For = !!sym(pf_col), Points_Against = !!sym(pa_col)) %>%
    mutate(Season = season) %>%
    select(Season, Team, Opp, Points_For, Points_Against)
}

main <- function() {
  if (!file.exists(TEAMS_PATH)) {
    stop("MTeams.csv not found. Run 01b_build_historical_from_nishaa.R first.")
  }
  teams <- read_csv(TEAMS_PATH, show_col_types = FALSE)
  name_to_id <- build_name_to_id(teams)

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

  # Resolve team names to IDs via lookup (one ID per name)
  lookup_df <- data.frame(name = names(name_to_id), TeamID = as.integer(name_to_id), stringsAsFactors = FALSE)
  lookup_lower <- lookup_df %>% mutate(name_lower = tolower(gsub("\\s+", " ", trimws(name))))
  # Deduplicate by name_lower, keep first
  lookup_lower <- lookup_lower %>% group_by(name_lower) %>% slice(1) %>% ungroup()

  resolve_ids <- function(names_vec) {
    n <- trimws(as.character(names_vec))
    n_lower <- tolower(gsub("\\s+", " ", n))
    # Match to lookup - ensure 1:1
    idx <- match(n_lower, lookup_lower$name_lower)
    ids <- lookup_lower$TeamID[idx]
    # Fallback: exact match on original
    unresolved <- is.na(ids)
    if (any(unresolved)) {
      midx <- match(n[unresolved], lookup_df$name)
      ids[unresolved] <- lookup_df$TeamID[midx]
    }
    ids
  }

  games <- games %>%
    mutate(
      TeamID = resolve_ids(Team),
      OppID = resolve_ids(Opp),
      WTeamID = if_else(Points_For >= Points_Against, TeamID, OppID),
      LTeamID = if_else(Points_For >= Points_Against, OppID, TeamID),
      WScore = pmax(Points_For, Points_Against),
      LScore = pmin(Points_For, Points_Against)
    )

  # Require both teams to resolve
  results <- games %>%
    filter(!is.na(WTeamID), !is.na(LTeamID), WTeamID != LTeamID) %>%
    mutate(DayNum = 1L) %>%
    select(Season, DayNum, WTeamID, LTeamID, WScore, LScore) %>%
    distinct()

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
