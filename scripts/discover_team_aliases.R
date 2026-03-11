# =============================================================================
# discover_team_aliases.R - Find team names that don't resolve to MTeams
# =============================================================================
# Scans schedules, bracket, and KenPom/Barttorvik for team names, attempts
# resolution via team_aliases + MTeams, and reports unresolved names with
# suggested aliases (closest matches from MTeams).
#
# Run: Rscript scripts/discover_team_aliases.R
# Output: prints to console; use output to add rows to data/processed/team_aliases.csv
# =============================================================================

library(here)
library(readr)
library(dplyr)

source(here("src", "config.R"))
source(here("src", "utils", "bracket_utils.R"))

#' Get distinct team names from all sources
gather_team_names <- function() {
  names_list <- list()

  # Schedules: Team and Opp columns
  schedule_files <- list.files(SCHEDULES_DIR, pattern = "[0-9]{4}-[0-9]{2}_schedule\\.csv$", full.names = TRUE)
  for (f in schedule_files) {
    if (!file.exists(f)) next
    df <- tryCatch(read_csv(f, show_col_types = FALSE), error = function(e) NULL)
    if (is.null(df)) next
    team_col <- names(df)[tolower(names(df)) %in% c("team", "teamname", "team_name")][1]
    opp_col <- names(df)[tolower(names(df)) %in% c("opp", "opp.", "opponent", "opp_name")][1]
    if (!is.na(team_col)) names_list[[length(names_list) + 1]] <- trimws(as.character(df[[team_col]]))
    if (!is.na(opp_col)) names_list[[length(names_list) + 1]] <- trimws(as.character(df[[opp_col]]))
  }

  # Bracket manual
  bracket_files <- list.files(BRACKET_DIR, pattern = "\\.csv$", full.names = TRUE)
  for (f in bracket_files) {
    df <- tryCatch(read_csv(f, show_col_types = FALSE), error = function(e) NULL)
    if (is.null(df) || !"Team" %in% names(df)) next
    names_list[[length(names_list) + 1]] <- trimws(as.character(df$Team))
  }

  # KenPom Barttorvik
  bt_path <- file.path(NISHAA_DIR, "KenPom Barttorvik.csv")
  if (file.exists(bt_path)) {
    df <- tryCatch(read_csv(bt_path, show_col_types = FALSE), error = function(e) NULL)
    if (!is.null(df) && "TEAM" %in% names(df)) {
      names_list[[length(names_list) + 1]] <- trimws(as.character(df$TEAM))
    }
  }

  unique(names_list %>% unlist() %>% .[!is.na(.) & nzchar(.)])
}

#' Resolve name to TeamID (simplified: canonical + MTeams match)
try_resolve <- function(nm, teams_df, aliases) {
  canon <- resolve_to_canonical(nm)
  name_col <- intersect(names(teams_df), c("TeamName", "Team_Name", "Name"))[1]
  if (is.na(name_col)) name_col <- names(teams_df)[2]
  canonical_list <- teams_df[[name_col]]
  idx <- which(tolower(trimws(canonical_list)) == tolower(canon))
  if (length(idx) >= 1) return(teams_df$TeamID[idx[1]])
  idx <- which(tolower(trimws(canonical_list)) == tolower(nm))
  if (length(idx) >= 1) return(teams_df$TeamID[idx[1]])
  NA_integer_
}

#' Find closest match in MTeams by string distance
suggest_alias <- function(nm, teams_df, max_dist = 5) {
  name_col <- intersect(names(teams_df), c("TeamName", "Team_Name", "Name"))[1]
  if (is.na(name_col)) name_col <- names(teams_df)[2]
  names_vec <- teams_df[[name_col]]
  dists <- adist(tolower(nm), tolower(names_vec), ignore.case = TRUE)[1, ]
  best_idx <- which.min(dists)
  if (dists[best_idx] <= max_dist && dists[best_idx] < nchar(nm)) {
    return(names_vec[best_idx])
  }
  NA_character_
}

main <- function() {
  message("Discovering team names from schedules, bracket, and KenPom...")
  all_names <- gather_team_names()
  all_names <- unique(all_names)
  message("  Found ", length(all_names), " unique names")

  teams_path <- if (dir.exists(RAW_EXTENDED_DIR)) {
    file.path(RAW_EXTENDED_DIR, "MTeams.csv")
  } else {
    file.path(RAW_DIR, "MTeams.csv")
  }
  if (!file.exists(teams_path)) {
    message("MTeams.csv not found. Skipping.")
    return(invisible(NULL))
  }
  teams <- read_csv(teams_path, show_col_types = FALSE)
  teams <- teams %>% distinct(TeamID, .keep_all = TRUE)

  unresolved <- character()
  for (nm in all_names) {
    tid <- try_resolve(nm, teams, NULL)
    if (is.na(tid)) unresolved <- c(unresolved, nm)
  }

  if (length(unresolved) == 0) {
    message("All team names resolve successfully.")
    return(invisible(NULL))
  }

  message("\n--- Unresolved team names (", length(unresolved), ") ---")
  message("Add these to data/processed/team_aliases.csv as Alias,CanonicalName\n")

  suggestions <- data.frame(
    UnresolvedName = unresolved,
    SuggestedCanonical = character(length(unresolved)),
    stringsAsFactors = FALSE
  )
  for (i in seq_along(unresolved)) {
    sug <- suggest_alias(unresolved[i], teams)
    suggestions$SuggestedCanonical[i] <- if (is.na(sug)) "" else sug
  }

  for (i in seq_len(nrow(suggestions))) {
    msg <- paste0("  ", suggestions$UnresolvedName[i], " -> ")
    if (nzchar(suggestions$SuggestedCanonical[i])) {
      msg <- paste0(msg, suggestions$SuggestedCanonical[i], " (suggested)")
    } else {
      msg <- paste0(msg, "???")
    }
    message(msg)
  }
  message("\nExample CSV rows to add:")
  for (i in seq_len(min(10, nrow(suggestions)))) {
    if (nzchar(suggestions$SuggestedCanonical[i])) {
      message(paste0(suggestions$UnresolvedName[i], ",", suggestions$SuggestedCanonical[i]))
    }
  }
}

main()
