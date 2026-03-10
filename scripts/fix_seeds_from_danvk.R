# =============================================================================
# fix_seeds_from_danvk.R - Rebuild MNCAATourneySeeds with correct region assignment
# =============================================================================
# Uses danvk/march-madness-data (region + seed + team) to fix the region bug
# in build_seeds() from nishaa Tournament Matchups (which has no region).
#
# Run: Rscript scripts/fix_seeds_from_danvk.R
# Requires: jsonlite, curl (or built-in url())
# =============================================================================

library(here)
library(readr)
library(dplyr)

source(here("src", "config.R"))

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Install jsonlite: install.packages('jsonlite')")
}

DANVK_BASE <- "https://raw.githubusercontent.com/danvk/march-madness-data/master/data"
# danvk has: 1985-2019, 2021-2024 (no 2020 - cancelled)
DANVK_YEARS <- c(1985:2019, 2021:2024)

# danvk region index -> W,X,Y,Z (consistent assignment)
REGION_LETTERS <- c("W", "X", "Y", "Z")

# Team name normalization: danvk name -> nishaa-style name (for matching)
NAME_NORM <- list(
  "UConn" = "Connecticut",
  "St. Joseph's" = "Saint Joseph's",
  "Saint Joseph's" = "Saint Joseph's",
  "Southern California" = "USC",
  "Cal State Fullerton" = "Cal St. Fullerton",
  "Cal St. Fullerton" = "Cal St. Fullerton",
  "Mississippi State" = "Mississippi St.",
  "Miami (FL)" = "Miami FL",
  "Texas\u2013Arlington" = "UT Arlington",  # en-dash
  "Texas-Arlington" = "UT Arlington",
  "UT Arlington" = "UT Arlington",
  "WKU" = "Western Kentucky",
  "Miss Valley St." = "Mississippi Valley St.",
  "Mississippi Valley St." = "Mississippi Valley St.",
  "Florida Atlantic" = "Florida Atlantic",
  "FAU" = "Florida Atlantic",
  "San Diego State" = "San Diego St.",
  "San Diego St." = "San Diego St.",
  "Morehead State" = "Morehead St.",
  "Morehead St." = "Morehead St.",
  "Washington State" = "Washington St.",
  "Washington St." = "Washington St.",
  "South Dakota State" = "South Dakota St.",
  "South Dakota St." = "South Dakota St.",
  "Michigan State" = "Michigan St.",
  "Michigan St." = "Michigan St.",
  "Grand Canyon" = "Grand Canyon",
  "College of Charleston" = "College of Charleston",
  "Charleston" = "College of Charleston",
  "NC State" = "North Carolina St.",
  "North Carolina St." = "North Carolina St.",
  "Grambling State" = "Grambling St.",
  "Grambling St." = "Grambling St.",
  "Long Beach State" = "Long Beach St.",
  "Long Beach St." = "Long Beach St.",
  "McNeese State" = "McNeese St.",
  "McNeese St." = "McNeese St.",
  "McNeese" = "McNeese St.",
  "Saint Peter's" = "Saint Peter's",
  "St. Peter's" = "Saint Peter's",
  "Kent St." = "Kent St.",
  "Ohio St." = "Ohio St.",
  "Iowa State" = "Iowa St.",
  "Iowa St." = "Iowa St.",
  "Colorado State" = "Colorado St.",
  "Colorado St." = "Colorado St.",
  "Utah State" = "Utah St.",
  "Utah St." = "Utah St.",
  "Texas A&M" = "Texas A&M",
  "Davidson " = "Davidson",  # trailing space in danvk 2008
  "Ole Miss" = "Mississippi",
  "Southern California" = "USC",
  "Cal State Northridge" = "Cal St. Northridge",
  "Cal St. Northridge" = "Cal St. Northridge",
  "Portland State" = "Portland St.",
  "North Dakota State" = "North Dakota St.",
  "Cleveland State" = "Cleveland St.",
  "Oklahoma State" = "Oklahoma St.",
  "Arizona State" = "Arizona St.",
  "Morgan State" = "Morgan St.",
  "Stephen F. Austin" = "Stephen F. Austin",
  "Cal State Bakersfield" = "Cal St. Bakersfield",
  "Cal St. Bakersfield" = "Cal St. Bakersfield",
  "East Tennessee St." = "East Tennessee St.",
  "Wichita State" = "Wichita St.",
  "Fresno State" = "Fresno St.",
  "Oregon State" = "Oregon St.",
  "Loyola Chicago" = "Loyola Chicago",
  "Loyola-Chicago" = "Loyola Chicago",
  "St. Bonaventure" = "St. Bonaventure",
  "North Carolina State" = "North Carolina St.",
  "Texas A&M\u2013Corpus Christi" = "Texas A&M-Corpus Christi",
  "Texas A&M-Corpus Christi" = "Texas A&M-Corpus Christi",
  "Fairleigh Dickinson" = "Fairleigh Dickinson",
  "Virginia Tech" = "Virginia Tech",
  "Boise State" = "Boise St.",
  "Kennesaw State" = "Kennesaw St.",
  "Montana State" = "Montana St.",
  "Penn State" = "Penn St.",
  "Kent State" = "Kent St.",
  "Ohio State" = "Ohio St.",
  "Kansas State" = "Kansas St.",
  "Florida Gulf Coast" = "Florida Gulf Coast",
  "Florida St." = "Florida St.",
  "UNC Asheville" = "UNC Asheville",
  "UNC Wilmington" = "UNC Wilmington",
  "UNC Greensboro" = "UNC Greensboro",
  "Green Bay" = "Green Bay",
  "Little Rock" = "Little Rock",
  "Stony Brook" = "Stony Brook",
  "Weber State" = "Weber St.",
  "Middle Tennessee" = "Middle Tennessee",
  "Eastern Washington" = "Eastern Washington",
  "Abilene Christian" = "Abilene Christian",
  "Northern Kentucky" = "Northern Kentucky",
  "Louisiana" = "Louisiana"
)

#' Normalize team name for matching to nishaa Team names
norm_name <- function(x) {
  x <- trimws(as.character(x))
  if (nchar(x) == 0) return(x)
  # Explicit mapping first
  if (!is.null(NAME_NORM[[x]])) return(NAME_NORM[[x]])
  # Generic: "State" -> "St." (e.g. "Morehead State" -> "Morehead St.")
  x2 <- gsub(" State$", " St.", x)
  if (x2 != x) return(x2)
  x
}

#' Build (Year, TeamName) -> TeamID from nishaa Tournament Matchups
build_nishaa_lookup <- function() {
  tm_path <- file.path(NISHAA_DIR, "Tournament Matchups.csv")
  if (!file.exists(tm_path)) {
    stop("Tournament Matchups.csv not found. Need nishaa data.")
  }
  tm <- read_csv(tm_path, show_col_types = FALSE)
  names(tm) <- c("Year", "ByYearNo", "TeamID", "Team", "Seed", "Round", "CurrentRound", "Score")
  # Use distinct (Year, TeamID, Team) - prefer first occurrence per Year
  lookup <- tm %>%
    filter(CurrentRound == 64) %>%
    distinct(Year, TeamID, .keep_all = TRUE) %>%
    select(Year, TeamID, Team)
  lookup
}

DANVK_CACHE <- file.path(RAW_EXTENDED_DIR, "danvk_cache")
if (!dir.exists(DANVK_CACHE)) dir.create(DANVK_CACHE, recursive = TRUE)

#' Fetch danvk bracket JSON for a year (download to cache, then read)
fetch_danvk_bracket <- function(year) {
  cache_path <- file.path(DANVK_CACHE, sprintf("%d.json", year))
  if (!file.exists(cache_path)) {
    url <- sprintf("%s/%d.json", DANVK_BASE, year)
    tryCatch({
      download.file(url, cache_path, quiet = TRUE, mode = "wb")
    }, error = function(e) {
      message("Failed to fetch ", year, ": ", e$message)
      return(NULL)
    })
  }
  tryCatch({
    jsonlite::read_json(cache_path)
  }, error = function(e) {
    message("Failed to parse ", year, ": ", e$message)
    NULL
  })
}

#' Extract R64 teams (seed, team) per region from danvk JSON
#' Structure: regions[[ri]][[1]] = R64 round = list of 8 games; each game = list of 2 teams
extract_r64_teams <- function(bracket) {
  if (is.null(bracket) || is.null(bracket$regions)) return(NULL)
  out <- list()
  for (ri in seq_along(bracket$regions)) {
    region <- bracket$regions[[ri]]
    if (length(region) < 1) next
    r64 <- region[[1]]
    if (length(r64) < 1) next
    # Flatten: each game has 2 teams; collect all into data.frame
    rows <- lapply(r64, function(game) {
      lapply(game, function(t) data.frame(seed = t$seed, team = t$team, stringsAsFactors = FALSE))
    })
    teams <- bind_rows(lapply(rows, bind_rows))
    teams <- teams %>% distinct(seed, team, .keep_all = TRUE)
    out[[ri]] <- teams[, c("seed", "team")]
  }
  out
}

#' Map danvk team name to nishaa TeamID
lookup_team_id <- function(year, team_name, nishaa_lookup) {
  nm <- norm_name(team_name)
  # Exact match
  m <- nishaa_lookup %>% filter(Year == year, Team == nm)
  if (nrow(m) > 0) return(m$TeamID[1])
  # Try normalized danvk name as Team
  m <- nishaa_lookup %>% filter(Year == year, Team == team_name)
  if (nrow(m) > 0) return(m$TeamID[1])
  # Partial (e.g. "North Carolina" in "North Carolina St.")
  # Be careful: "North Carolina" != "North Carolina St."
  m <- nishaa_lookup %>% filter(Year == year, trimws(Team) == trimws(team_name))
  if (nrow(m) > 0) return(m$TeamID[1])
  NA_integer_
}

main <- function() {
  message("Building nishaa (Year, Team) -> TeamID lookup...")
  nishaa <- build_nishaa_lookup()
  nishaa_years <- unique(nishaa$Year)

  # Restrict to years we have nishaa data for
  years_to_process <- intersect(DANVK_YEARS, nishaa_years)
  message("Processing ", length(years_to_process), " years with both danvk and nishaa data.")

  all_seeds <- list()
  unmapped <- character()

  for (yr in years_to_process) {
    bracket <- fetch_danvk_bracket(yr)
    if (is.null(bracket)) next

    r64 <- extract_r64_teams(bracket)
    if (is.null(r64) || length(r64) == 0) next

    for (ri in seq_along(r64)) {
      letter <- REGION_LETTERS[ri]
      df <- r64[[ri]]
      for (i in seq_len(nrow(df))) {
        seed_num <- as.integer(df$seed[i])
        team_name <- as.character(df$team[i])
        tid <- lookup_team_id(yr, team_name, nishaa)
        if (is.na(tid)) {
          unmapped <- c(unmapped, paste0(yr, ":", team_name))
        }
        seed_str <- sprintf("%s%02d", letter, seed_num)
        all_seeds[[length(all_seeds) + 1]] <- data.frame(
          Season = yr,
          Seed = seed_str,
          TeamID = if (is.na(tid)) 0L else tid
        )
      }
    }
  }

  seeds_df <- bind_rows(all_seeds)

  # For failed lookups (TeamID==0), fill from existing seeds if available
  seeds_valid <- seeds_df %>% filter(TeamID > 0)
  seeds_failed <- seeds_df %>% filter(TeamID == 0)
  existing_path <- file.path(RAW_EXTENDED_DIR, "MNCAATourneySeeds.csv")

  if (nrow(seeds_failed) > 0) {
    message("\nWARNING: ", nrow(seeds_failed), " team(s) could not be mapped to TeamID. Filling from existing seeds.")
    if (file.exists(existing_path)) {
      existing <- read_csv(existing_path, show_col_types = FALSE)
      for (i in seq_len(nrow(seeds_failed))) {
        row <- seeds_failed[i, ]
        fill <- existing %>%
          filter(Season == row$Season, Seed == row$Seed) %>%
          pull(TeamID)
        if (length(fill) > 0) {
          seeds_valid <- bind_rows(seeds_valid, row %>% mutate(TeamID = fill[1]))
        }
      }
    }
  }

  # Merge with existing seeds for years danvk doesn't cover (e.g. 2025)
  if (file.exists(existing_path)) {
    existing <- read_csv(existing_path, show_col_types = FALSE)
    existing_years <- unique(existing$Season)
    danvk_years <- unique(seeds_valid$Season)
    extra_years <- setdiff(existing_years, danvk_years)
    if (length(extra_years) > 0) {
      extra <- existing %>% filter(Season %in% extra_years)
      seeds_valid <- bind_rows(seeds_valid, extra)
      message("Kept ", length(extra_years), " year(s) from existing seeds (not in danvk): ",
              paste(extra_years, collapse = ", "))
    }
  }

  seeds_out <- seeds_valid %>% arrange(Season, Seed)
  out_path <- file.path(RAW_EXTENDED_DIR, "MNCAATourneySeeds.csv")
  if (!dir.exists(RAW_EXTENDED_DIR)) dir.create(RAW_EXTENDED_DIR, recursive = TRUE)
  write_csv(seeds_out, out_path)

  message("\nWrote corrected seeds to ", out_path)
  message("Seasons: ", min(seeds_out$Season), " - ", max(seeds_out$Season))
  message("Total rows: ", nrow(seeds_out))
}

main()
