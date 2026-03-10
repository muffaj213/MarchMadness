# =============================================================================
# build_team_master.R - Generate team_name_master.csv
# =============================================================================
# Builds explicit master table: Name -> PreferredTeamID for consistent
# resolution of ESPN/KenPom names to Kaggle TeamIDs. PreferredTeamID is
# the TeamID from the most recent tournament appearance.
# Run: Rscript scripts/build_team_master.R
# =============================================================================

library(here)
library(readr)
library(dplyr)

source(here("src", "config.R"))

# Known aliases: SourceName (ESPN/KenPom) -> CanonicalName (Kaggle)
# From kenpom_utils.R normalize_team_name substitutions
ALIASES <- list(
  "NC State" = "North Carolina St.",
  "North Carolina State" = "North Carolina St.",
  "UConn" = "Connecticut",
  "Ole Miss" = "Mississippi",
  "UNC Wilmington" = "UNCW",
  "Southern Illinois" = "S Illinois",
  "Louisiana Lafayette" = "Louisiana",
  "Louisiana Monroe" = "UL Monroe",
  "UT Arlington" = "Texas Arlington",
  "Illinois Chicago" = "UIC",
  "Central Connecticut" = "Central Conn",
  "St. Francis NY" = "St Francis NY",
  "St. Francis PA" = "St Francis PA",
  "LIU Brooklyn" = "Long Island",
  "Arkansas Little Rock" = "Little Rock",
  "Southwest Texas St." = "Texas St.",
  "Texas Pan American" = "UTRGV",
  "Texas A&M Corpus Chris" = "Texas A&M-Corpus Christi",
  "Michigan State" = "Michigan St.",
  "Washington State" = "Washington St.",
  "Ohio State" = "Ohio St.",
  "Oregon State" = "Oregon St.",
  "Colorado State" = "Colorado St.",
  "Mississippi State" = "Mississippi St.",
  "Kansas State" = "Kansas St.",
  "Florida State" = "Florida St.",
  "Penn State" = "Penn St.",
  "Iowa State" = "Iowa St.",
  "Oklahoma State" = "Oklahoma St.",
  "Arizona State" = "Arizona St.",
  "San Diego State" = "San Diego St.",
  "Boise State" = "Boise St.",
  "Kent State" = "Kent St.",
  "Utah State" = "Utah St.",
  "North Dakota State" = "North Dakota St.",
  "South Dakota State" = "South Dakota St.",
  "McNeese" = "McNeese St.",
  "Morehead State" = "Morehead St."
)

main <- function() {
  seeds <- read_csv(file.path(PROC_DIR, "tourney_seeds.csv"), show_col_types = FALSE)
  teams <- read_csv(file.path(PROC_DIR, "teams.csv"), show_col_types = FALSE)
  team_col <- intersect(names(teams), c("TeamName", "Team_Name", "Name"))[1]
  if (is.na(team_col)) team_col <- names(teams)[2]

  # PreferredTeamID = TeamID from most recent season in tourney_seeds
  latest <- seeds %>%
    left_join(teams %>% select(TeamID, TeamName = !!sym(team_col)), by = "TeamID") %>%
    group_by(TeamName) %>%
    slice_max(Season, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(CanonicalName = TeamName, PreferredTeamID = TeamID)

  # Build master: one row per Name (canonical or alias) -> PreferredTeamID
  master <- latest %>% mutate(Name = CanonicalName, .before = 1) %>% select(Name, PreferredTeamID)

  # Add alias rows (alias -> same PreferredTeamID as canonical)
  for (alias in names(ALIASES)) {
    canonical <- ALIASES[[alias]]
    pref <- latest %>% filter(CanonicalName == canonical) %>% pull(PreferredTeamID)
    if (length(pref) > 0) {
      master <- bind_rows(master, tibble(Name = alias, PreferredTeamID = pref[1]))
    }
  }

  # Remove duplicates (keep first = canonical)
  master <- master %>% distinct(Name, .keep_all = TRUE) %>% arrange(Name)

  out_path <- file.path(PROC_DIR, "team_name_master.csv")
  write_csv(master, out_path)
  message("Wrote ", out_path, " (", nrow(master), " name -> TeamID mappings)")
}

main()
