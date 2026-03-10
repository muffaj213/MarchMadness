# =============================================================================
# team_id_consolidation.R - Consolidate duplicate TeamIDs to a single canonical ID
# =============================================================================
# Ensures each team has exactly one record across the pipeline. Uses
# team_id_consolidation.csv (OldTeamID -> PreferredTeamID) when present, else
# builds from team_name_master + teams or from tourney_seeds for duplicates.
# =============================================================================

library(here)
library(readr)
library(dplyr)

source(here("src", "config.R"))

TEAM_ID_CONSOLIDATION_PATH <- file.path(PROC_DIR, "team_id_consolidation.csv")

#' Build consolidation map: OldTeamID -> PreferredTeamID for duplicate team names
#' @param teams Data frame with TeamID and TeamName (or Name)
#' @param tourney_seeds Data frame with Season, TeamID
#' @param team_name_master Optional: Name, PreferredTeamID (authoritative when present)
build_consolidation_map <- function(teams, tourney_seeds, team_name_master = NULL) {
  team_col <- intersect(names(teams), c("TeamName", "Team_Name", "Name"))[1]
  if (is.na(team_col)) team_col <- names(teams)[2]
  teams <- teams %>% rename(TeamName = !!sym(team_col))

  # Find names with multiple TeamIDs
  dups <- teams %>%
    group_by(TeamName) %>%
    filter(n_distinct(TeamID) > 1) %>%
    ungroup() %>%
    distinct(TeamName, TeamID)

  if (nrow(dups) == 0) return(tibble(OldTeamID = integer(), PreferredTeamID = integer()))

  # For each duplicate name, pick PreferredTeamID
  preferred <- if (!is.null(team_name_master) && nrow(team_name_master) > 0) {
    name_col <- intersect(names(team_name_master), c("Name", "TeamName"))[1]
    pref_col <- intersect(names(team_name_master), c("PreferredTeamID", "PreferredTeamId"))[1]
    team_name_master %>%
      rename(TeamName = !!sym(name_col), PreferredTeamID = !!sym(pref_col)) %>%
      select(TeamName, PreferredTeamID)
  } else {
    # Use tourney_seeds: most recent Season per TeamID, pick that ID as preferred
    seeds_latest <- tourney_seeds %>%
      group_by(TeamID) %>%
      summarise(max_season = max(Season, na.rm = TRUE), .groups = "drop")
    dups %>%
      left_join(teams %>% select(TeamName, TeamID), by = "TeamName") %>%
      left_join(seeds_latest, by = "TeamID") %>%
      group_by(TeamName) %>%
      slice_max(max_season, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(TeamName, PreferredTeamID = TeamID)
  }

  # Build OldID -> Preferred map (only for IDs that differ from preferred)
  dups %>%
    left_join(preferred, by = "TeamName") %>%
    filter(TeamID != PreferredTeamID) %>%
    transmute(OldTeamID = TeamID, PreferredTeamID = PreferredTeamID) %>%
    distinct()
}

#' Load consolidation map from CSV if it exists
load_consolidation_map <- function() {
  if (!file.exists(TEAM_ID_CONSOLIDATION_PATH)) return(NULL)
  m <- read_csv(TEAM_ID_CONSOLIDATION_PATH, show_col_types = FALSE)
  nm <- names(m)
  old_col <- intersect(nm, c("OldTeamID", "OldTeamId", "FromTeamID"))[1]
  pref_col <- intersect(nm, c("PreferredTeamID", "PreferredTeamId", "ToTeamID"))[1]
  if (is.na(old_col)) old_col <- nm[1]
  if (is.na(pref_col)) pref_col <- nm[2]
  m %>% transmute(OldTeamID = !!sym(old_col), PreferredTeamID = !!sym(pref_col))
}

#' Apply consolidation map to remap TeamIDs in a data frame
#' @param df Data frame
#' @param id_cols Character vector of column names containing TeamIDs
apply_remap <- function(df, map, id_cols) {
  if (is.null(map) || nrow(map) == 0) return(df)
  old_to_pref <- setNames(map$PreferredTeamID, map$OldTeamID)
  for (col in id_cols) {
    if (col %in% names(df)) {
      df[[col]] <- as.integer(ifelse(
        as.character(df[[col]]) %in% names(old_to_pref),
        old_to_pref[as.character(df[[col]])],
        df[[col]]
      ))
    }
  }
  df
}

#' Build and apply team ID consolidation to raw data
#' Deduplicates teams so each team name has exactly one row (PreferredTeamID).
#' @param raw List from read_raw_data (teams, tourney_seeds, tourney_results, regular_results)
#' @return Updated raw list with consolidated IDs and deduplicated teams
build_and_apply_team_id_consolidation <- function(raw) {
  map <- load_consolidation_map()

  if (is.null(map) || nrow(map) == 0) {
    team_name_master <- NULL
    if (file.exists(file.path(PROC_DIR, "team_name_master.csv"))) {
      team_name_master <- read_csv(file.path(PROC_DIR, "team_name_master.csv"), show_col_types = FALSE)
    }
    map <- build_consolidation_map(raw$teams, raw$tourney_seeds, team_name_master)
    if (nrow(map) > 0) {
      if (!dir.exists(PROC_DIR)) dir.create(PROC_DIR, recursive = TRUE)
      write_csv(map, TEAM_ID_CONSOLIDATION_PATH)
      message("  Wrote ", TEAM_ID_CONSOLIDATION_PATH, " (", nrow(map), " ID remappings)")
    }
  }

  if (nrow(map) == 0) {
    # Still deduplicate teams by name (keep one row per name)
    team_col <- intersect(names(raw$teams), c("TeamName", "Team_Name", "Name"))[1]
    if (is.na(team_col)) team_col <- names(raw$teams)[2]
    dup_names <- raw$teams %>%
      rename(TeamName = !!sym(team_col)) %>%
      group_by(TeamName) %>%
      filter(n() > 1) %>%
      pull(TeamName) %>%
      unique()
    if (length(dup_names) > 0) {
      raw$teams <- raw$teams %>%
        rename(TeamName = !!sym(team_col)) %>%
        group_by(TeamName) %>%
        slice_min(TeamID, n = 1, with_ties = FALSE) %>%
        ungroup()
      if ("TeamName" != team_col) raw$teams <- raw$teams %>% rename(!!sym(team_col) := TeamName)
      message("  Deduplicated ", length(dup_names), " team names in teams.csv")
    }
    return(raw)
  }

  message("  Applying team ID consolidation: ", nrow(map), " remappings")

  raw$regular_results <- apply_remap(raw$regular_results, map, c("WTeamID", "LTeamID"))
  raw$tourney_results <- apply_remap(raw$tourney_results, map, c("WTeamID", "LTeamID"))
  raw$tourney_seeds <- apply_remap(raw$tourney_seeds, map, c("TeamID", "Team"))

  team_col <- intersect(names(raw$teams), c("TeamName", "Team_Name", "Name"))[1]
  if (is.na(team_col)) team_col <- names(raw$teams)[2]
  raw$teams <- apply_remap(raw$teams, map, "TeamID")
  # Keep one row per TeamID (after remap, duplicates may collapse)
  raw$teams <- raw$teams %>%
    rename(TeamName = !!sym(team_col)) %>%
    distinct(TeamID, .keep_all = TRUE)
  if ("TeamName" != team_col) raw$teams <- raw$teams %>% rename(!!sym(team_col) := TeamName)

  raw
}
