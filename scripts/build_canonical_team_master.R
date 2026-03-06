# =============================================================================
# build_canonical_team_master.R - Align all team IDs to raw_historical/Teams.csv
# =============================================================================
# Uses data/raw_historical/Teams.csv as the canonical master. Builds mappings
# from Kaggle TeamID and other sources to canonical Team_Id. Produces:
#   - data/processed/team_id_master.csv (canonical + source mappings)
#   - output/team_id_comparison.csv (inner-joined comparison)
# Run: Rscript scripts/build_canonical_team_master.R
# =============================================================================

library(here)
library(readr)
library(dplyr)

RAW_HIST_DIR <- here("data", "raw_historical")
PROC_DIR <- here("data", "processed")
OUTPUT_DIR <- here("output")

#' Normalize team name for matching (collapsed, comparable form)
normalize_for_match <- function(x) {
  x <- trimws(tolower(as.character(x)))
  x <- gsub("['.'']", "", x)
  x <- gsub("[^a-z0-9&]", " ", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  # Standardize common abbreviations
  x <- gsub("\\bsaint\\b", "st", x)
  x <- gsub("\\bst\\b", "st", x)
  x <- gsub("\\buniversity\\b", "univ", x)
  x <- gsub("\\buniv\\b", "univ", x)
  x <- gsub("\\bcollege\\b", "col", x)
  x <- gsub("\\bcol\\b", "col", x)
  x <- gsub("\\barkansas\\b", "ark", x)
  x <- gsub("\\bcentral\\b", "cent", x)
  x <- gsub("\\bnorthern\\b", "n", x)
  x <- gsub("\\bsouthern\\b", "s", x)
  x <- gsub("\\beastern\\b", "e", x)
  x <- gsub("\\bwestern\\b", "w", x)
  x <- gsub("\\bflorida\\b", "fl", x)
  x <- gsub("\\bstate\\b", "st", x)
  x <- gsub("\\bst\\b", "st", x)
  x <- gsub("\\binternational\\b", "intl", x)
  x <- gsub("\\bintl\\b", "intl", x)
  gsub("\\s+", "", x)
}

#' Explicit raw_historical Team_Name -> Kaggle TeamName mappings for ambiguous cases
HIST_TO_KAGGLE <- c(
  "Abilene Chr" = "Abilene Christian",
  "Alabama St" = "Alabama St.",
  "Ark Little Rock" = "Little Rock",
  "Ark Pine Bluff" = "Arkansas Pine Bluff",
  "American Univ" = "American",
  "Boston Univ" = "Boston University",
  "Bethune-Cookman" = "Bethune-Cookman",
  "Birmingham So" = "Birmingham Southern",
  "C Michigan" = "Central Michigan",
  "Cal Poly SLO" = "Cal Poly",
  "Cent Arkansas" = "Central Arkansas",
  "Charleston So" = "Charleston Southern",
  "Chicago St" = "Chicago St.",
  "Cleveland St" = "Cleveland St.",
  "Coastal Car" = "Coastal Carolina",
  "Col Charleston" = "College of Charleston",
  "CS Bakersfield" = "Cal St. Bakersfield",
  "CS Fullerton" = "Cal St. Fullerton",
  "CS Northridge" = "Cal St. Northridge",
  "CS Sacramento" = "Sacramento St.",
  "Delaware St" = "Delaware St.",
  "E Illinois" = "Eastern Illinois",
  "E Kentucky" = "Eastern Kentucky",
  "E Michigan" = "Eastern Michigan",
  "E Washington" = "Eastern Washington",
  "FL Atlantic" = "Florida Atlantic",
  "FL Gulf Coast" = "Florida Gulf Coast",
  "Florida Intl" = "Florida International",
  "Florida St" = "Florida St.",
  "F Dickinson" = "Fairleigh Dickinson",
  "Fresno St" = "Fresno St.",
  "G Washington" = "George Washington",
  "Ga Southern" = "Georgia Southern",
  "Gardner Webb" = "Gardner-Webb",
  "Georgia St" = "Georgia St.",
  "Georgia Tech" = "Georgia Tech",
  "Houston Bap" = "Houston Baptist",
  "Idaho St" = "Idaho St.",
  "IL Chicago" = "UIC",
  "Illinois St" = "Illinois St.",
  "Indiana St" = "Indiana St.",
  "Iowa St" = "Iowa St.",
  "Jackson St" = "Jackson St.",
  "Jacksonville St" = "Jacksonville St.",
  "Kansas St" = "Kansas St.",
  "Kent" = "Kent St.",
  "Long Beach St" = "Long Beach St.",
  "Loy Marymount" = "Loyola Marymount",
  "Loyola MD" = "Loyola MD",
  "MA Lowell" = "UMass Lowell",
  "McNeese St" = "McNeese St.",
  "MD E Shore" = "Maryland Eastern Shore",
  "Miami OH" = "Miami OH",
  "Michigan St" = "Michigan St.",
  "Mississippi St" = "Mississippi St.",
  "Missouri St" = "Missouri St.",
  "Monmouth NJ" = "Monmouth",
  "Montana St" = "Montana St.",
  "Morehead St" = "Morehead St.",
  "Morgan St" = "Morgan St.",
  "MS Valley St" = "Mississippi Valley St.",
  "Mt St Mary's" = "Mount St. Mary's",
  "MTSU" = "Middle Tennessee",
  "Murray St" = "Murray St.",
  "N Colorado" = "Northern Colorado",
  "N Dakota St" = "North Dakota St.",
  "N Illinois" = "Northern Illinois",
  "N Kentucky" = "Northern Kentucky",
  "NC A&T" = "North Carolina A&T",
  "NC Central" = "North Carolina Central",
  "NC State" = "North Carolina St.",
  "NE Omaha" = "Nebraska Omaha",
  "Nevada" = "Nevada",
  "New Mexico St" = "New Mexico St.",
  "Nicholls St" = "Nicholls St.",
  "N Kentucky" = "Northern Kentucky",
  "N Illinois" = "Northern Illinois",
  "Northern Arizona" = "Northern Arizona",
  "Northern Iowa" = "Northern Iowa",
  "Northwestern LA" = "Northwestern State",
  "Ohio St" = "Ohio St.",
  "Oklahoma St" = "Oklahoma St.",
  "Oregon St" = "Oregon St.",
  "Portland St" = "Portland St.",
  "S Carolina St" = "South Carolina St.",
  "S Dakota St" = "South Dakota St.",
  "S Illinois" = "S Illinois",
  "Sam Houston St" = "Sam Houston St.",
  "San Diego St" = "San Diego St.",
  "SE Missouri St" = "Southeast Missouri St.",
  "SF Austin" = "Stephen F Austin",
  "St Bonaventure" = "St. Bonaventure",
  "St Francis NY" = "St Francis NY",
  "St Francis PA" = "St Francis PA",
  "St John's" = "St. John's",
  "St Joseph's PA" = "Saint Joseph's",
  "St Louis" = "St. Louis",
  "St Mary's CA" = "Saint Mary's",
  "St Peter's" = "Saint Peter's",
  "TN Martin" = "Tennessee Martin",
  "Texas St" = "Texas St.",
  "TX Southern" = "Texas Southern",
  "Utah St" = "Utah St.",
  "VA Commonwealth" = "VCU",
  "W Carolina" = "Western Carolina",
  "W Illinois" = "Western Illinois",
  "WKU" = "Western Kentucky",
  "W Michigan" = "Western Michigan",
  "Washington St" = "Washington St.",
  "WI Green Bay" = "Green Bay",
  "WI Milwaukee" = "Milwaukee",
  "Wichita St" = "Wichita St.",
  "Wright St" = "Wright St.",
  "Youngstown St" = "Youngstown St."
)

main <- function() {
  if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

  # Load canonical (raw_historical)
  hist_path <- file.path(RAW_HIST_DIR, "Teams.csv")
  if (!file.exists(hist_path)) {
    stop("Canonical master not found: ", hist_path)
  }
  hist_to_kaggle_df <- tibble(
    Team_Name = names(HIST_TO_KAGGLE),
    hist_name_expanded = unname(HIST_TO_KAGGLE)
  )
  canonical <- read_csv(hist_path, show_col_types = FALSE) %>%
    left_join(hist_to_kaggle_df, by = "Team_Name") %>%
    mutate(
      hist_name_expanded = coalesce(hist_name_expanded, Team_Name),
      match_key_hist = normalize_for_match(Team_Name),
      match_key_kaggle = normalize_for_match(hist_name_expanded)
    )

  # Load Kaggle MTeams (prefer raw_extended)
  kaggle_path <- if (file.exists(file.path(here("data", "raw_extended"), "MTeams.csv"))) {
    file.path(here("data", "raw_extended"), "MTeams.csv")
  } else {
    file.path(here("data", "raw"), "MTeams.csv")
  }
  if (!file.exists(kaggle_path)) {
    stop("Kaggle MTeams not found. Run 01_download_data.R first.")
  }
  kaggle <- read_csv(kaggle_path, show_col_types = FALSE) %>%
    rename(Kaggle_TeamID = TeamID, Kaggle_TeamName = TeamName) %>%
    mutate(match_key = normalize_for_match(Kaggle_TeamName))

  # Join: try match_key_kaggle (canonical expanded to Kaggle style) first
  # relationship = many-to-many OK: Kaggle has duplicate names (same team, different IDs by season)
  join1 <- canonical %>%
    inner_join(kaggle %>% select(Kaggle_TeamID, Kaggle_TeamName, match_key),
               by = c("match_key_kaggle" = "match_key"),
               relationship = "many-to-many")

  # For canonical rows not matched, try match_key_hist (raw canonical form)
  unmatched_hist <- canonical %>% anti_join(join1 %>% distinct(Team_Id), by = "Team_Id")
  if (nrow(unmatched_hist) > 0) {
    join2 <- unmatched_hist %>%
      inner_join(kaggle %>% select(Kaggle_TeamID, Kaggle_TeamName, match_key),
                 by = c("match_key_hist" = "match_key"),
                 relationship = "many-to-many") %>%
      select(Team_Id, Team_Name, Kaggle_TeamID, Kaggle_TeamName)
    join1 <- bind_rows(
      join1 %>% select(Team_Id, Team_Name, Kaggle_TeamID, Kaggle_TeamName),
      join2
    )
  } else {
    join1 <- join1 %>% select(Team_Id, Team_Name, Kaggle_TeamID, Kaggle_TeamName)
  }

  # One row per canonical Team_Id: prefer Kaggle_TeamID with exact name match, else highest ID
  join1 <- join1 %>%
    mutate(name_match = tolower(Team_Name) == tolower(Kaggle_TeamName)) %>%
    group_by(Team_Id, Team_Name) %>%
    arrange(desc(name_match), desc(Kaggle_TeamID)) %>%
    slice(1) %>%
    ungroup() %>%
    select(Team_Id, Team_Name, Kaggle_TeamID, Kaggle_TeamName)

  # Build team_id_master: canonical Team_Id <-> Kaggle TeamID
  team_id_master <- join1 %>%
    select(Canonical_Team_Id = Team_Id, Canonical_Team_Name = Team_Name,
           Kaggle_TeamID, Kaggle_TeamName) %>%
    arrange(Canonical_Team_Id)

  # Comparison CSV (inner-joined): full side-by-side
  comparison <- join1 %>%
    select(
      Canonical_Team_Id = Team_Id,
      Canonical_Team_Name = Team_Name,
      Kaggle_TeamID,
      Kaggle_TeamName
    ) %>%
    arrange(Canonical_Team_Id)

  # Save
  write_csv(team_id_master, file.path(PROC_DIR, "team_id_master.csv"))
  write_csv(comparison, file.path(OUTPUT_DIR, "team_id_comparison.csv"))

  message("Wrote ", file.path(PROC_DIR, "team_id_master.csv"),
          " (", nrow(team_id_master), " canonical <-> Kaggle mappings)")
  message("Wrote ", file.path(OUTPUT_DIR, "team_id_comparison.csv"),
          " (inner-joined comparison)")
  message("Canonical teams: ", nrow(canonical), " | Matched: ", nrow(comparison),
          " | Unmatched: ", nrow(canonical) - nrow(comparison))
}

main()
