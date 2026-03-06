# =============================================================================
# kenpom_utils.R - Load and map KenPom historical data to Kaggle TeamIDs
# =============================================================================
# Uses data/raw_kenpom/kenpom.csv (2002-2017 from GitHub) and optionally
# data/raw_nishaa/KenPom Barttorvik.csv for 2024-2025.
# =============================================================================

library(dplyr)
library(readr)

#' Normalize team name for matching (trim, common substitutions)
normalize_team_name <- function(x) {
  x <- trimws(as.character(x))
  # Common KenPom/ESPN -> Kaggle name mappings (only when Kaggle uses different name)
  substitutions <- c(
    "NC State" = "North Carolina St.",
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
    "North Carolina State" = "North Carolina St.",
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
    "South Dakota State" = "South Dakota St."
  )
  for (i in seq_along(substitutions)) {
    x[x == names(substitutions)[i]] <- substitutions[i]
  }
  x
}

#' Build Season+TeamID -> TeamName lookup from seeds and teams
#'
#' @param seeds Tourney seeds (Season, Seed, TeamID)
#' @param teams Teams (TeamID, TeamName)
#' @return Tibble with Season, TeamID, TeamName
build_season_team_lookup <- function(seeds, teams) {
  team_col <- intersect(names(teams), c("TeamName", "Team_Name", "Name"))[1]
  if (is.na(team_col)) team_col <- names(teams)[2]
  seeds %>%
    distinct(Season, TeamID) %>%
    left_join(teams %>% select(TeamID, TeamName = !!sym(team_col)), by = "TeamID")
}

#' Map KenPom (Season, Team) to TeamID via lookup (vectorized)
map_kenpom_to_teamids <- function(kp_df, lookup) {
  lookup_norm <- lookup %>%
    mutate(TeamNameNorm = normalize_team_name(TeamName)) %>%
    distinct(Season, TeamNameNorm, .keep_all = TRUE) %>%
    select(Season, TeamID, TeamNameNorm)
  kp_norm <- kp_df %>%
    mutate(TeamNorm = normalize_team_name(Team), .row = row_number())
  # Join on exact match (one-to-one)
  matched <- kp_norm %>%
    left_join(
      lookup_norm,
      by = c("Season", "TeamNorm" = "TeamNameNorm")
    ) %>%
    arrange(.row)
  matched$TeamID
}

#' Load and process GitHub KenPom data (Year, Team, AdjustO, AdjustD, AdjustT, Pyth)
#'
#' Pyth is efficiency margin (AdjEM-like). AdjustO/AdjustD = offensive/defensive efficiency.
#' Returns tibble with Season, TeamID, adj_em, adj_o, adj_d, adj_t
load_github_kenpom <- function(kenpom_path, lookup) {
  if (!file.exists(kenpom_path)) return(tibble())
  kp <- read_csv(kenpom_path, show_col_types = FALSE)
  if (nrow(kp) == 0) return(tibble())
  # GitHub columns: Year, Team, AdjustO, AdjustD, AdjustT, Pyth
  kp <- kp %>% rename(Season = Year)
  # adj_em ≈ Pyth (efficiency margin); or compute as AdjustO - (200 - AdjustD) conceptually
  # KenPom AdjEM = Offensive Eff - Defensive Eff (relative to avg). Pyth is similar.
  kp <- kp %>%
    mutate(
      adj_o = as.numeric(AdjustO),
      adj_d = as.numeric(AdjustD),
      adj_t = as.numeric(AdjustT),
      adj_em = as.numeric(Pyth)
    )
  # Map Team -> TeamID per season (vectorized)
  kp$TeamID <- map_kenpom_to_teamids(kp, lookup)
  # Win pct from Wins / (Wins + Losses)
  kp <- kp %>%
    mutate(
      Wins_num = suppressWarnings(as.numeric(Wins)),
      Losses_num = suppressWarnings(as.numeric(Losses)),
      Games = Wins_num + Losses_num,
      win_pct = if_else(Games > 0, Wins_num / Games, 0.5)
    )
  kp %>%
    filter(!is.na(TeamID)) %>%
    select(Season, TeamID, adj_em, adj_o, adj_d, adj_t, win_pct, Wins = Wins_num, Losses = Losses_num, Games)
}


#' Load KenPom Barttorvik data (KADJ EM, KADJ O, KADJ D, KADJ T) for 2024+
load_barttorvik_kenpom <- function(bt_path, lookup) {
  if (!file.exists(bt_path)) return(tibble())
  bt <- read_csv(bt_path, show_col_types = FALSE)
  if (nrow(bt) == 0) return(tibble())
  # Barttorvik columns: YEAR, TEAM, KADJ EM, KADJ O, KADJ D, KADJ T
  names(bt) <- gsub(" ", "_", names(bt))
  bt <- bt %>% rename(Season = YEAR, Team = TEAM)
  # Column names after gsub: KADJ_EM, KADJ_O, KADJ_D, KADJ_T (exact)
  em_col <- "KADJ_EM"
  o_col <- "KADJ_O"
  d_col <- "KADJ_D"
  t_col <- "KADJ_T"
  if (!em_col %in% names(bt)) em_col <- names(bt)[grepl("EM", names(bt))][1]
  if (!o_col %in% names(bt)) o_col <- names(bt)[grepl("KADJ_O|KO_RANK", names(bt))][1]
  if (!all(c(em_col, o_col) %in% names(bt))) return(tibble())
  bt <- bt %>%
    mutate(
      adj_em = as.numeric(.data[[em_col]]),
      adj_o = as.numeric(.data[[o_col]]),
      adj_d = as.numeric(.data[[d_col]]),
      adj_t = as.numeric(.data[[t_col]])
    )
  # Win pct from W and L columns (W / (W + L))
  w_col <- if ("W" %in% names(bt)) "W" else names(bt)[grepl("^Wins$", names(bt), ignore.case = TRUE)][1]
  l_col <- if ("L" %in% names(bt)) "L" else names(bt)[grepl("^Losses$", names(bt), ignore.case = TRUE)][1]
  games_col <- if ("GAMES" %in% names(bt)) "GAMES" else NULL
  if (is.na(w_col)) w_col <- "W"
  if (is.na(l_col)) l_col <- "L"
  bt <- bt %>%
    mutate(
      Wins_num = suppressWarnings(as.numeric(if (w_col %in% names(bt)) .data[[w_col]] else 0)),
      Losses_num = suppressWarnings(as.numeric(if (l_col %in% names(bt)) .data[[l_col]] else 0)),
      Games = if (!is.null(games_col) && games_col %in% names(bt)) as.numeric(.data[[games_col]]) else Wins_num + Losses_num,
      win_pct = if_else(Games > 0, Wins_num / Games, 0.5)
    )
  bt$TeamID <- map_kenpom_to_teamids(bt, lookup)
  bt %>%
    filter(!is.na(TeamID)) %>%
    select(Season, TeamID, adj_em, adj_o, adj_d, adj_t, win_pct, Wins = Wins_num, Losses = Losses_num, Games)
}

#' Ensure KenPom CSV exists; download from GitHub if missing
ensure_kenpom_downloaded <- function(kenpom_dir = NULL) {
  if (is.null(kenpom_dir)) kenpom_dir <- here::here("data", "raw_kenpom")
  kp_path <- file.path(kenpom_dir, "kenpom.csv")
  if (!file.exists(kp_path)) {
    dir.create(kenpom_dir, showWarnings = FALSE, recursive = TRUE)
    url <- "https://raw.githubusercontent.com/jfinocchiaro/marchmadness/master/kenpom.csv"
    tryCatch(
      utils::download.file(url, kp_path, quiet = TRUE),
      error = function(e) message("Could not download KenPom. Place kenpom.csv in ", kenpom_dir)
    )
  }
}

#' Load all KenPom data, merge GitHub + Barttorvik, return Season/TeamID stats
#' @param seeds Tourney seeds (Season, Seed, TeamID)
#' @param teams Teams (TeamID, TeamName)
#' @param projected_seeds_dir If provided, also load seeds_projected_*.csv to enable KenPom mapping for future seasons (e.g. 2025)
load_kenpom_stats <- function(seeds, teams, kenpom_dir = NULL, barttorvik_path = NULL, projected_seeds_dir = NULL) {
  seeds_aug <- seeds
  if (is.null(projected_seeds_dir)) projected_seeds_dir <- here::here("data", "bracket")
  proj_files <- list.files(projected_seeds_dir, pattern = "^seeds_projected_[0-9]+\\.csv$", full.names = TRUE)
  for (f in proj_files) {
    if (file.exists(f)) {
      proj <- tryCatch(read_csv(f, show_col_types = FALSE), error = function(e) NULL)
      if (!is.null(proj) && nrow(proj) > 0 && all(c("Season", "TeamID") %in% names(proj))) {
        seeds_aug <- bind_rows(seeds_aug, proj %>% select(Season, Seed, TeamID) %>% distinct(Season, TeamID, .keep_all = TRUE))
      }
    }
  }
  lookup <- build_season_team_lookup(seeds_aug, teams)
  if (is.null(kenpom_dir)) kenpom_dir <- here::here("data", "raw_kenpom")
  ensure_kenpom_downloaded(kenpom_dir)
  kp_github <- load_github_kenpom(file.path(kenpom_dir, "kenpom.csv"), lookup)
  if (is.null(barttorvik_path)) {
    barttorvik_path <- here::here("data", "raw_nishaa", "KenPom Barttorvik.csv")
  }
  kp_bt <- load_barttorvik_kenpom(barttorvik_path, lookup)
  # Combine; Barttorvik overwrites for overlapping seasons
  bind_rows(
    kp_github %>% filter(!(Season %in% unique(kp_bt$Season))),
    kp_bt
  ) %>%
    arrange(Season, TeamID)
}
