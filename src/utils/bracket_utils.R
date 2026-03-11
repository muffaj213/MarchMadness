# =============================================================================
# bracket_utils.R - Team name resolution for projected bracket
# =============================================================================
# Maps TeamName from ESPN/KenPom to Kaggle TeamID. When season is provided,
# uses team_season_ids.csv for season-aware resolution (fixes ID mismatches).
# Uses team_aliases.csv (e.g. Charleston -> College of Charleston).
# Falls back to team_name_master.csv and teams.csv when no season-specific data.
# =============================================================================

#' Resolve name to canonical (alias lookup). Charleston -> College of Charleston.
#' Charleston Southern is distinct and not aliased.
resolve_to_canonical <- function(nm) {
  nm <- trimws(as.character(nm))
  if (is.na(nm) || nm == "") return(nm)
  alias_path <- here::here("data", "processed", "team_aliases.csv")
  if (file.exists(alias_path)) {
    aliases <- readr::read_csv(alias_path, show_col_types = FALSE)
    idx <- which(tolower(trimws(aliases$Alias)) == tolower(nm))
    if (length(idx) >= 1) return(trimws(aliases$CanonicalName[idx[1]]))
  }
  nm
}

#' Resolve team names to TeamIDs (season-aware when possible)
#'
#' When season is provided, uses team_season_ids.csv for (Season, CanonicalName)
#' -> KaggleTeamID. Applies team_aliases (e.g. Charleston -> College of Charleston).
#' Falls back to team_name_master.csv and teams.csv when no match.
#'
#' @param team_names Character vector of team names from projection
#' @param teams_df Data frame with TeamID and TeamName (fallback when no master)
#' @param season Optional season year for season-aware resolution (recommended)
#' @param master_path Optional path to team_name_master.csv
#' @return Integer vector of TeamIDs; NA where no match
resolve_team_names_to_ids <- function(team_names, teams_df, season = NULL, master_path = NULL) {
  proc_dir <- here::here("data", "processed")
  if (is.null(master_path)) master_path <- file.path(proc_dir, "team_name_master.csv")
  master <- if (file.exists(master_path)) readr::read_csv(master_path, show_col_types = FALSE) else NULL

  # Season-aware: team_season_ids (Season, CanonicalName) -> KaggleTeamID
  season_ids <- NULL
  if (!is.null(season) && length(season) > 0 && !is.na(season[1])) {
    sid_path <- file.path(proc_dir, "team_season_ids.csv")
    if (file.exists(sid_path)) {
      season_ids <- readr::read_csv(sid_path, show_col_types = FALSE) %>%
        filter(Season == as.integer(season[1]))
    }
  }

  result <- rep(NA_integer_, length(team_names))
  for (i in seq_along(team_names)) {
    nm <- trimws(as.character(team_names[i]))
    if (is.na(nm) || nm == "") next

    canonical <- resolve_to_canonical(nm)

    # 1. Season-aware: (Season, CanonicalName) -> KaggleTeamID
    if (!is.null(season_ids) && nrow(season_ids) > 0) {
      idx <- which(tolower(trimws(season_ids$CanonicalName)) == tolower(canonical))
      if (length(idx) >= 1) {
        result[i] <- as.integer(season_ids$KaggleTeamID[idx[1]])
        next
      }
    }

    # 2. Try master table (exact match, case-insensitive)
    if (!is.null(master) && nrow(master) > 0) {
      idx <- which(tolower(trimws(master$Name)) == tolower(nm))
      if (length(idx) == 0) idx <- which(tolower(trimws(master$Name)) == tolower(canonical))
      if (length(idx) >= 1) {
        result[i] <- as.integer(master$PreferredTeamID[idx[1]])
        next
      }
      source(here::here("src", "utils", "kenpom_utils.R"), local = TRUE)
      norm_nm <- normalize_team_name(nm)
      idx <- which(tolower(trimws(master$Name)) == tolower(norm_nm))
      if (length(idx) >= 1) {
        result[i] <- as.integer(master$PreferredTeamID[idx[1]])
        next
      }
    }

    # 3. Fallback: teams.csv with normalize + fuzzy
    source(here::here("src", "utils", "kenpom_utils.R"), local = TRUE)
    team_col <- intersect(names(teams_df), c("TeamName", "Team_Name", "Name"))[1]
    if (is.na(team_col)) team_col <- names(teams_df)[2]
    canonical_list <- teams_df[[team_col]]
    canonical_norm <- normalize_team_name(canonical_list)
    norm_nm <- normalize_team_name(canonical)

    idx <- which(canonical_norm == norm_nm)
    if (length(idx) == 1) {
      result[i] <- teams_df$TeamID[idx[1]]
    } else if (length(idx) > 1 && !is.null(master)) {
      canon_name <- canonical_list[idx[1]]
      midx <- which(tolower(trimws(master$Name)) == tolower(normalize_team_name(canon_name)))
      if (length(midx) >= 1) result[i] <- as.integer(master$PreferredTeamID[midx[1]])
      else result[i] <- teams_df$TeamID[idx[1]]
    } else if (length(idx) > 1) {
      result[i] <- teams_df$TeamID[idx[1]]
    } else {
      dists <- adist(norm_nm, canonical_norm, ignore.case = TRUE)
      best_idx <- which.min(dists[1, ])
      best_dist <- dists[1, best_idx]
      if (best_dist <= 3) {
        message("Fuzzy match for '", nm, "' -> '", canonical_list[best_idx], "' (TeamID ", teams_df$TeamID[best_idx], ")")
        result[i] <- teams_df$TeamID[best_idx]
      } else {
        warning("No match for '", nm, "'. Closest: '", canonical_list[best_idx], "' (edit distance ", best_dist, ")")
      }
    }
  }
  result
}
