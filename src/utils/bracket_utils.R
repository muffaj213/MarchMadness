# =============================================================================
# bracket_utils.R - Team name resolution for projected bracket
# =============================================================================
# Maps TeamName from ESPN/KenPom to Kaggle TeamID. Uses team_name_master.csv
# when available (explicit one-ID-per-team), else falls back to teams.csv.
# =============================================================================

#' Resolve team names to TeamIDs
#'
#' Uses team_name_master.csv if present (exact match on Name). Falls back to
#' teams.csv with normalize_team_name and fuzzy match when master has no match.
#'
#' @param team_names Character vector of team names from projection
#' @param teams_df Data frame with TeamID and TeamName (fallback when no master)
#' @param master_path Optional path to team_name_master.csv
#' @return Integer vector of TeamIDs; NA where no match
resolve_team_names_to_ids <- function(team_names, teams_df, master_path = NULL) {
  if (is.null(master_path)) {
    master_path <- here::here("data", "processed", "team_name_master.csv")
  }
  master <- if (file.exists(master_path)) {
    readr::read_csv(master_path, show_col_types = FALSE)
  } else {
    NULL
  }

  result <- rep(NA_integer_, length(team_names))
  for (i in seq_along(team_names)) {
    nm <- trimws(as.character(team_names[i]))
    if (is.na(nm) || nm == "") next

    # 1. Try master table (exact match, case-insensitive)
    if (!is.null(master) && nrow(master) > 0) {
      idx <- which(tolower(trimws(master$Name)) == tolower(nm))
      if (length(idx) >= 1) {
        result[i] <- as.integer(master$PreferredTeamID[idx[1]])
        next
      }
      # Also try normalized name (alias might be in master)
      source(here::here("src", "utils", "kenpom_utils.R"), local = TRUE)
      norm_nm <- normalize_team_name(nm)
      idx <- which(tolower(trimws(master$Name)) == tolower(norm_nm))
      if (length(idx) >= 1) {
        result[i] <- as.integer(master$PreferredTeamID[idx[1]])
        next
      }
    }

    # 2. Fallback: teams.csv with normalize + fuzzy
    source(here::here("src", "utils", "kenpom_utils.R"), local = TRUE)
    team_col <- intersect(names(teams_df), c("TeamName", "Team_Name", "Name"))[1]
    if (is.na(team_col)) team_col <- names(teams_df)[2]
    canonical <- teams_df[[team_col]]
    canonical_norm <- normalize_team_name(canonical)
    norm_nm <- normalize_team_name(nm)

    idx <- which(canonical_norm == norm_nm)
    if (length(idx) == 1) {
      result[i] <- teams_df$TeamID[idx[1]]
    } else if (length(idx) > 1 && !is.null(master)) {
      # Multiple in teams; prefer master if canonical has entry
      canon_name <- canonical[idx[1]]
      midx <- which(tolower(trimws(master$Name)) == tolower(normalize_team_name(canon_name)))
      if (length(midx) >= 1) {
        result[i] <- as.integer(master$PreferredTeamID[midx[1]])
      } else {
        result[i] <- teams_df$TeamID[idx[1]]
      }
    } else if (length(idx) > 1) {
      result[i] <- teams_df$TeamID[idx[1]]
    } else {
      # Fuzzy match
      dists <- adist(norm_nm, canonical_norm, ignore.case = TRUE)
      best_idx <- which.min(dists[1, ])
      best_dist <- dists[1, best_idx]
      if (best_dist <= 3) {
        message("Fuzzy match for '", nm, "' -> '", canonical[best_idx], "' (TeamID ", teams_df$TeamID[best_idx], ")")
        result[i] <- teams_df$TeamID[best_idx]
      } else {
        warning("No match for '", nm, "'. Closest: '", canonical[best_idx], "' (edit distance ", best_dist, ")")
      }
    }
  }
  result
}
