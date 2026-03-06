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

#' Explicit raw_historical Team_Name -> Kaggle TeamName mappings.
#' Includes static mappings + promoted approved mappings from teams_to_review (Option A).
#' These persist for future data ingestions.
HIST_TO_KAGGLE <- c(
  "Abilene Chr" = "Abilene Christian",
  "Air Force" = "Air Force",
  "Alabama A&M" = "Alabama A&M",
  "Alabama St" = "Alabama St.",
  "Albany NY" = "Albany",
  "Alcorn St" = "Alcorn St.",
  "American Univ" = "American",
  "Appalachian St" = "Appalachian St.",
  "Ark Little Rock" = "Little Rock",
  "Ark Pine Bluff" = "Arkansas Pine Bluff",
  "Arkansas St" = "Arkansas St.",
  "Army" = "Army",
  "Ball St" = "Ball St.",
  "Bethune-Cookman" = "Bethune-Cookman",
  "Birmingham So" = "Birmingham Southern",
  "Boston Univ" = "Boston University",
  "Bowling Green" = "Bowling Green",
  "C Michigan" = "Central Michigan",
  "Campbell" = "Campbell",
  "Canisius" = "Canisius",
  "Cent Arkansas" = "Central Arkansas",
  "Centenary" = "Centenary",
  "Central Conn" = "Central Conn.",
  "Charleston So" = "Charleston Southern",
  "Charlotte" = "Charlotte",
  "Chicago St" = "Chicago St.",
  "Citadel" = "The Citadel",
  "Cleveland St" = "Cleveland St.",
  "Coastal Car" = "Coastal Carolina",
  "Col Charleston" = "College of Charleston",
  "Columbia" = "Columbia",
  "Coppin St" = "Coppin St.",
  "CS Bakersfield" = "Cal St. Bakersfield",
  "CS Fullerton" = "Cal St. Fullerton",
  "CS Northridge" = "Cal St. Northridge",
  "CS Sacramento" = "Sacramento St.",
  "Dartmouth" = "Dartmouth",
  "Delaware St" = "Delaware St.",
  "Denver" = "Denver",
  "DePaul" = "DePaul",
  "E Illinois" = "Eastern Illinois",
  "E Kentucky" = "Eastern Kentucky",
  "E Michigan" = "Eastern Michigan",
  "E Washington" = "Eastern Washington",
  "East Carolina" = "East Carolina",
  "Edwardsville" = "SIU Edwardsville",
  "Elon" = "Elon",
  "ETSU" = "East Tennessee St.",
  "Evansville" = "Evansville",
  "Fairfield" = "Fairfield",
  "FL Atlantic" = "Florida Atlantic",
  "FL Gulf Coast" = "Florida Gulf Coast",
  "Florida A&M" = "Florida A&M",
  "Florida Intl" = "Florida International",
  "Florida St" = "Florida St.",
  "Fordham" = "Fordham",
  "F Dickinson" = "Fairleigh Dickinson",
  "Fresno St" = "Fresno St.",
  "G Washington" = "George Washington",
  "Ga Southern" = "Georgia Southern",
  "Gardner Webb" = "Gardner-Webb",
  "Georgia St" = "Georgia St.",
  "Georgia Tech" = "Georgia Tech",
  "Grambling" = "Grambling",
  "Hofstra" = "Hofstra",
  "Houston Bap" = "Houston Baptist",
  "Idaho" = "Idaho",
  "Idaho St" = "Idaho St.",
  "IL Chicago" = "UIC",
  "Illinois St" = "Illinois St.",
  "Incarnate Word" = "Incarnate Word",
  "Indiana St" = "Indiana St.",
  "Iowa St" = "Iowa St.",
  "Jackson St" = "Jackson St.",
  "Jacksonville" = "Jacksonville",
  "Jacksonville St" = "Jacksonville St.",
  "Kansas St" = "Kansas St.",
  "Kennesaw" = "Kennesaw",
  "Kent" = "Kent St.",
  "Lamar" = "Lamar",
  "Long Beach St" = "Long Beach St.",
  "Long Island" = "Long Island",
  "Louisiana Tech" = "Louisiana Tech",
  "Loy Marymount" = "Loyola Marymount",
  "Loyola MD" = "Loyola MD",
  "MA Lowell" = "UMass Lowell",
  "Maine" = "Maine",
  "Marist" = "Marist",
  "McNeese St" = "McNeese St.",
  "MD E Shore" = "Maryland Eastern Shore",
  "Miami OH" = "Miami OH",
  "Michigan St" = "Michigan St.",
  "Mississippi St" = "Mississippi St.",
  "Missouri KC" = "UMKC",
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
  "New Hampshire" = "New Hampshire",
  "New Mexico St" = "New Mexico St.",
  "New Orleans" = "New Orleans",
  "Niagara" = "Niagara",
  "Nicholls St" = "Nicholls St.",
  "NJIT" = "NJIT",
  "North Florida" = "North Florida",
  "Northern Arizona" = "Northern Arizona",
  "Northern Iowa" = "Northern Iowa",
  "Northwestern LA" = "Northwestern State",
  "Navy" = "Navy",
  "Ohio St" = "Ohio St.",
  "Oklahoma St" = "Oklahoma St.",
  "Oregon St" = "Oregon St.",
  "Pepperdine" = "Pepperdine",
  "Portland" = "Portland",
  "Portland St" = "Portland St.",
  "Prairie View" = "Prairie View",
  "Presbyterian" = "Presbyterian",
  "Quinnipiac" = "Quinnipiac",
  "Rice" = "Rice",
  "Rider" = "Rider",
  "S Carolina St" = "South Carolina St.",
  "S Dakota St" = "South Dakota St.",
  "S Illinois" = "S Illinois",
  "Sacred Heart" = "Sacred Heart",
  "Sam Houston St" = "Sam Houston St.",
  "San Diego St" = "San Diego St.",
  "San Jose St" = "San Jose St.",
  "Santa Barbara" = "UC Santa Barbara",
  "Santa Clara" = "Santa Clara",
  "SC Upstate" = "USC Upstate",
  "SE Louisiana" = "Southeastern Louisiana",
  "SE Missouri St" = "Southeast Missouri St.",
  "Seattle" = "Seattle",
  "SF Austin" = "Stephen F Austin",
  "South Dakota" = "South Dakota",
  "Southern Univ" = "Southern",
  "Southern Utah" = "Southern Utah",
  "St Bonaventure" = "St. Bonaventure",
  "St Francis NY" = "St Francis NY",
  "St Francis PA" = "St Francis PA",
  "St John's" = "St. John's",
  "St Joseph's PA" = "Saint Joseph's",
  "St Louis" = "St. Louis",
  "St Mary's CA" = "Saint Mary's",
  "St Peter's" = "Saint Peter's",
  "TAM C. Christi" = "Texas A&M-Corpus Christi",
  "Tennessee St" = "Tennessee St.",
  "Tennessee Tech" = "Tennessee Tech",
  "Texas St" = "Texas St.",
  "TN Martin" = "Tennessee Martin",
  "TX Southern" = "Texas Southern",
  "Toledo" = "Toledo",
  "Towson" = "Towson",
  "Tulane" = "Tulane",
  "UTRGV" = "UTRGV",
  "UC Riverside" = "UC Riverside",
  "ULL" = "Louisiana",
  "ULM" = "UL Monroe",
  "UT San Antonio" = "UT San Antonio",
  "Utah St" = "Utah St.",
  "Utah Valley" = "Utah Valley",
  "VA Commonwealth" = "VCU",
  "VMI" = "VMI",
  "W Carolina" = "Western Carolina",
  "W Illinois" = "Western Illinois",
  "WKU" = "Western Kentucky",
  "W Michigan" = "Western Michigan",
  "Washington St" = "Washington St.",
  "WI Green Bay" = "Green Bay",
  "WI Milwaukee" = "Milwaukee",
  "Wichita St" = "Wichita St.",
  "William & Mary" = "William & Mary",
  "Wright St" = "Wright St.",
  "Youngstown St" = "Youngstown St."
)

#' Suggested Kaggle names for unmatched teams (for teams_to_review.csv)
SUGGESTED_FOR_REVIEW <- c(
  "Air Force" = "Air Force",
  "Alabama A&M" = "Alabama A&M",
  "Albany NY" = "Albany",
  "Alcorn St" = "Alcorn St.",
  "Appalachian St" = "Appalachian St.",
  "Arkansas St" = "Arkansas St.",
  "Ball St" = "Ball St.",
  "Bethune-Cookman" = "Bethune-Cookman",
  "Birmingham So" = "Birmingham Southern",
  "Bowling Green" = "Bowling Green",
  "C Michigan" = "Central Michigan",
  "Campbell" = "Campbell",
  "Canisius" = "Canisius",
  "Cent Arkansas" = "Central Arkansas",
  "Centenary" = "Centenary",
  "Central Conn" = "Central Conn.",
  "Charleston So" = "Charleston Southern",
  "Charlotte" = "Charlotte",
  "Chicago St" = "Chicago St.",
  "Citadel" = "The Citadel",
  "Columbia" = "Columbia",
  "Coppin St" = "Coppin St.",
  "CS Sacramento" = "Sacramento St.",
  "Dartmouth" = "Dartmouth",
  "Delaware St" = "Delaware St.",
  "Denver" = "Denver",
  "DePaul" = "DePaul",
  "E Illinois" = "Eastern Illinois",
  "E Michigan" = "Eastern Michigan",
  "East Carolina" = "East Carolina",
  "Edwardsville" = "SIU Edwardsville",
  "Elon" = "Elon",
  "ETSU" = "East Tennessee St.",
  "Evansville" = "Evansville",
  "Fairfield" = "Fairfield",
  "Florida A&M" = "Florida A&M",
  "Florida Intl" = "Florida International",
  "Fordham" = "Fordham",
  "Ga Southern" = "Georgia Southern",
  "Grambling" = "Grambling",
  "Hofstra" = "Hofstra",
  "Houston Bap" = "Houston Baptist",
  "Idaho" = "Idaho",
  "Idaho St" = "Idaho St.",
  "IL Chicago" = "UIC",
  "Illinois St" = "Illinois St.",
  "Incarnate Word" = "Incarnate Word",
  "Army" = "Army",
  "Jackson St" = "Jackson St.",
  "Jacksonville" = "Jacksonville",
  "Kennesaw" = "Kennesaw",
  "Lamar" = "Lamar",
  "Long Island" = "Long Island",
  "Louisiana Tech" = "Louisiana Tech",
  "Loy Marymount" = "Loyola Marymount",
  "MA Lowell" = "UMass Lowell",
  "Maine" = "Maine",
  "Marist" = "Marist",
  "MD E Shore" = "Maryland Eastern Shore",
  "Miami OH" = "Miami OH",
  "Missouri KC" = "UMKC",
  "Missouri St" = "Missouri St.",
  "Monmouth NJ" = "Monmouth",
  "N Illinois" = "Northern Illinois",
  "Navy" = "Navy",
  "New Hampshire" = "New Hampshire",
  "New Orleans" = "New Orleans",
  "Niagara" = "Niagara",
  "Nicholls St" = "Nicholls St.",
  "NJIT" = "NJIT",
  "North Florida" = "North Florida",
  "Northern Arizona" = "Northern Arizona",
  "Pepperdine" = "Pepperdine",
  "Portland" = "Portland",
  "Prairie View" = "Prairie View",
  "Presbyterian" = "Presbyterian",
  "Quinnipiac" = "Quinnipiac",
  "Rice" = "Rice",
  "Rider" = "Rider",
  "S Carolina St" = "South Carolina St.",
  "S Illinois" = "S Illinois",
  "Sacred Heart" = "Sacred Heart",
  "San Jose St" = "San Jose St.",
  "Santa Barbara" = "UC Santa Barbara",
  "Santa Clara" = "Santa Clara",
  "SC Upstate" = "USC Upstate",
  "SE Louisiana" = "Southeastern Louisiana",
  "SE Missouri St" = "Southeast Missouri St.",
  "Seattle" = "Seattle",
  "South Dakota" = "South Dakota",
  "Southern Univ" = "Southern",
  "Southern Utah" = "Southern Utah",
  "St Francis NY" = "St Francis NY",
  "St Francis PA" = "St Francis PA",
  "TAM C. Christi" = "Texas A&M-Corpus Christi",
  "Tennessee St" = "Tennessee St.",
  "Tennessee Tech" = "Tennessee Tech",
  "Texas St" = "Texas St.",
  "TN Martin" = "Tennessee Martin",
  "Toledo" = "Toledo",
  "Towson" = "Towson",
  "Tulane" = "Tulane",
  "UTRGV" = "UTRGV",
  "UC Riverside" = "UC Riverside",
  "ULL" = "Louisiana",
  "ULM" = "UL Monroe",
  "UT San Antonio" = "UT San Antonio",
  "Utah Valley" = "Utah Valley",
  "VMI" = "VMI",
  "W Carolina" = "Western Carolina",
  "W Illinois" = "Western Illinois",
  "William & Mary" = "William & Mary",
  "Youngstown St" = "Youngstown St."
)

main <- function() {
  if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

  # Load canonical (raw_historical)
  hist_path <- file.path(RAW_HIST_DIR, "Teams.csv")
  if (!file.exists(hist_path)) {
    stop("Canonical master not found: ", hist_path)
  }

  # Build hist_to_kaggle from HIST_TO_KAGGLE (includes promoted approved mappings, Option A)
  hist_to_kaggle_df <- tibble(
    Team_Name = names(HIST_TO_KAGGLE),
    hist_name_expanded = unname(HIST_TO_KAGGLE)
  ) %>%
    filter(!is.na(hist_name_expanded), trimws(hist_name_expanded) != "") %>%
    distinct(Team_Name, .keep_all = TRUE)

  # Overlay any new approved from teams_to_review (for future manual reviews)
  review_path <- file.path(OUTPUT_DIR, "teams_to_review.csv")
  if (file.exists(review_path)) {
    review <- read_csv(review_path, show_col_types = FALSE)
    approved <- review %>%
      filter(!is.na(Approved_Kaggle_Name), trimws(Approved_Kaggle_Name) != "") %>%
      distinct(Team_Id, .keep_all = TRUE) %>%
      distinct(Canonical_Name, .keep_all = TRUE) %>%
      select(Team_Name = Canonical_Name, hist_name_expanded = Approved_Kaggle_Name)
    if (nrow(approved) > 0) {
      hist_to_kaggle_df <- hist_to_kaggle_df %>%
        filter(!Team_Name %in% approved$Team_Name) %>%
        bind_rows(approved)
      message("Loaded ", nrow(approved), " approved mappings from teams_to_review.csv (overlay)")
    }
  }

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

  # Unmatched teams -> teams_to_review.csv for manual review
  matched_ids <- join1$Team_Id
  unmatched <- canonical %>%
    filter(!Team_Id %in% matched_ids) %>%
    select(Team_Id, Canonical_Name = Team_Name)
  suggested_df <- tibble(
    Canonical_Name = names(SUGGESTED_FOR_REVIEW),
    Suggested_Kaggle_Name = unname(SUGGESTED_FOR_REVIEW)
  ) %>%
    distinct(Canonical_Name, .keep_all = TRUE)
  to_review <- unmatched %>%
    left_join(suggested_df, by = "Canonical_Name") %>%
    mutate(
      Approved_Kaggle_Name = NA_character_,
      Notes = NA_character_
    )
  if (file.exists(review_path)) {
    prev <- read_csv(review_path, show_col_types = FALSE)
    if (all(c("Team_Id", "Approved_Kaggle_Name", "Notes") %in% names(prev))) {
      prev <- prev %>% distinct(Team_Id, .keep_all = TRUE)
      to_review <- to_review %>%
        select(-Approved_Kaggle_Name, -Notes) %>%
        left_join(prev %>% select(Team_Id, Approved_Kaggle_Name, Notes), by = "Team_Id") %>%
        mutate(
          Approved_Kaggle_Name = coalesce(Approved_Kaggle_Name, ""),
          Notes = coalesce(Notes, "")
        )
    }
  } else {
    to_review <- to_review %>% mutate(Approved_Kaggle_Name = "", Notes = "")
  }
  to_review <- to_review %>%
    distinct(Team_Id, .keep_all = TRUE) %>%
    select(Team_Id, Canonical_Name, Suggested_Kaggle_Name, Approved_Kaggle_Name, Notes) %>%
    arrange(Team_Id)
  write_csv(to_review, review_path)

  # Save
  write_csv(team_id_master, file.path(PROC_DIR, "team_id_master.csv"))
  write_csv(comparison, file.path(OUTPUT_DIR, "team_id_comparison.csv"))

  message("Wrote ", file.path(PROC_DIR, "team_id_master.csv"),
          " (", nrow(team_id_master), " canonical <-> Kaggle mappings)")
  message("Wrote ", file.path(OUTPUT_DIR, "team_id_comparison.csv"),
          " (inner-joined comparison)")
  message("Wrote ", review_path, " (", nrow(to_review), " teams for manual review)")
  message("Canonical teams: ", nrow(canonical), " | Matched: ", nrow(comparison),
          " | Unmatched: ", nrow(to_review))
}

main()
