# =============================================================================
# bracket_utils.R - Team name resolution for projected bracket
# =============================================================================
# Maps TeamName from ESPN/KenPom to Kaggle TeamID using teams.csv.
# Uses normalize_team_name from kenpom_utils.R.
# =============================================================================

#' Resolve team names to TeamIDs
#'
#' Normalizes names, prefers exact match, uses fuzzy match for close variants.
#' Warns on no match or multiple matches; suggests closest matches for unmatched.
#'
#' @param team_names Character vector of team names from projection
#' @param teams_df Data frame with TeamID and TeamName (or similar)
#' @return Integer vector of TeamIDs; NA where no match
resolve_team_names_to_ids <- function(team_names, teams_df) {
  source(here::here("src", "utils", "kenpom_utils.R"), local = TRUE)
  team_col <- intersect(names(teams_df), c("TeamName", "Team_Name", "Name"))[1]
  if (is.na(team_col)) team_col <- names(teams_df)[2]
  canonical <- teams_df[[team_col]]
  canonical_norm <- normalize_team_name(canonical)

  result <- rep(NA_integer_, length(team_names))
  for (i in seq_along(team_names)) {
    nm <- trimws(as.character(team_names[i]))
    if (is.na(nm) || nm == "") next
    norm_nm <- normalize_team_name(nm)
    idx <- which(canonical_norm == norm_nm)
    if (length(idx) == 1) {
      result[i] <- teams_df$TeamID[idx[1]]
    } else if (length(idx) > 1) {
      warning("Multiple matches for '", nm, "': TeamIDs ", paste(teams_df$TeamID[idx], collapse = ", "),
              ". Using first.")
      result[i] <- teams_df$TeamID[idx[1]]
    } else {
      # Fuzzy match: find closest
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
