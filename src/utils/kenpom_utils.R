# =============================================================================
# kenpom_utils.R - Load and map KenPom historical data to Kaggle TeamIDs
# =============================================================================
# Uses data/raw_kenpom/kenpom.csv (2002-2017 from GitHub) and optionally
# data/raw_nishaa/KenPom Barttorvik.csv for 2024-2025.
# =============================================================================

library(dplyr)
library(readr)

source(here::here("src", "config.R"))

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
      adj_em = as.numeric(Pyth),
      luck = suppressWarnings(as.numeric(if ("Luck" %in% names(.)) Luck else NA_real_))
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
    select(Season, TeamID, adj_em, adj_o, adj_d, adj_t, luck, win_pct, Wins = Wins_num, Losses = Losses_num, Games)
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
    mutate(luck = NA_real_) %>%
    select(Season, TeamID, adj_em, adj_o, adj_d, adj_t, luck, win_pct, Wins = Wins_num, Losses = Losses_num, Games)
}

#' Load KenPom gap file (2018-2023 from toRvik/bart_ratings)
#' Expected columns: Season, Team, adj_em, adj_o, adj_d, adj_t, win_pct, Wins, Losses, Games
load_kenpom_gap <- function(gap_path, lookup) {
  if (!file.exists(gap_path)) return(tibble())
  gap <- read_csv(gap_path, show_col_types = FALSE)
  if (nrow(gap) == 0) return(tibble())
  gap$TeamID <- map_kenpom_to_teamids(gap, lookup)
  gap %>%
    filter(!is.na(TeamID)) %>%
    mutate(luck = NA_real_) %>%
    select(Season, TeamID, adj_em, adj_o, adj_d, adj_t, luck, win_pct, Wins, Losses, Games)
}

#' Ensure KenPom CSV exists; download from GitHub if missing
ensure_kenpom_downloaded <- function(kenpom_dir = NULL) {
  if (is.null(kenpom_dir)) kenpom_dir <- KENPOM_DIR
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
  if (is.null(projected_seeds_dir)) projected_seeds_dir <- BRACKET_DIR
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
  if (is.null(kenpom_dir)) kenpom_dir <- KENPOM_DIR
  ensure_kenpom_downloaded(kenpom_dir)
  kp_github <- load_github_kenpom(file.path(kenpom_dir, "kenpom.csv"), lookup)
  kp_gap <- load_kenpom_gap(file.path(kenpom_dir, "kenpom_gap_2018_2023.csv"), lookup)
  if (is.null(barttorvik_path)) {
    barttorvik_path <- file.path(NISHAA_DIR, "KenPom Barttorvik.csv")
  }
  kp_bt <- load_barttorvik_kenpom(barttorvik_path, lookup)
  # Combine with explicit precedence to avoid duplicate Season/TeamID:
  # GitHub (2002-2017) | gap (2018-2023) | Barttorvik (2024+, preferred for any overlap with gap)
  bt_seasons <- if (nrow(kp_bt) > 0) unique(kp_bt$Season) else integer()
  gap_seasons_excluding_bt <- if (nrow(kp_gap) > 0) unique(kp_gap$Season) else integer()
  gap_seasons_excluding_bt <- setdiff(gap_seasons_excluding_bt, bt_seasons)
  all_other_seasons <- unique(c(bt_seasons, gap_seasons_excluding_bt))
  bind_rows(
    kp_github %>% filter(!(Season %in% all_other_seasons)),
    kp_gap %>% filter(Season %in% gap_seasons_excluding_bt),
    kp_bt
  ) %>%
    distinct(Season, TeamID, .keep_all = TRUE) %>%
    arrange(Season, TeamID)
}

#' Load home/away win rates from College basketball 2012-24.csv (atoziye)
#' Expected columns: Year, Team, Home win rate, Away win rate
#' @return Tibble with Season, TeamID, home_win_rate, away_win_rate
load_home_away_win_rates <- function(path = NULL, lookup) {
  if (is.null(path)) path <- file.path(RAW_ATOZIYE_DIR, "College basketball 2012-24.csv")
  if (!file.exists(path)) return(tibble())
  df <- read_csv(path, show_col_types = FALSE)
  if (nrow(df) == 0) return(tibble())
  if ("Year" %in% names(df)) df <- df %>% rename(Season = Year)
  if ("YEAR" %in% names(df) && !"Season" %in% names(df)) df <- df %>% rename(Season = YEAR)
  if ("TEAM" %in% names(df) && !"Team" %in% names(df)) df <- df %>% rename(Team = TEAM)
  if (!"Team" %in% names(df)) return(tibble())
  hr <- names(df)[grep("Home.*win|home.*win", names(df), ignore.case = TRUE)][1]
  ar <- names(df)[grep("Away.*win|away.*win", names(df), ignore.case = TRUE)][1]
  if (is.na(hr) || is.na(ar)) return(tibble())
  df$TeamID <- map_kenpom_to_teamids(df, lookup)
  df %>%
    filter(!is.na(TeamID)) %>%
    mutate(
      home_win_rate = suppressWarnings(as.numeric(!!sym(hr))),
      away_win_rate = suppressWarnings(as.numeric(!!sym(ar)))
    ) %>%
    filter(!is.na(home_win_rate) | !is.na(away_win_rate)) %>%
    select(Season, TeamID, home_win_rate, away_win_rate)
}

#' Load NET, ELO, WAB from Resumes.csv and Teamsheet Ranks.csv (nishaa)
#' Resumes: YEAR, TEAM, ELO, WAB RANK. Teamsheet: YEAR, TEAM, NET, WAB
#' @return Tibble with Season, TeamID, elo, net, wab
load_resume_stats <- function(resumes_path = NULL, teamsheet_path = NULL, lookup) {
  if (is.null(resumes_path)) resumes_path <- file.path(NISHAA_DIR, "Resumes.csv")
  if (is.null(teamsheet_path)) teamsheet_path <- file.path(NISHAA_DIR, "Teamsheet Ranks.csv")
  out <- tibble(Season = integer(), TeamID = integer(), elo = numeric(), net = numeric(), wab = numeric())
  if (file.exists(resumes_path)) {
    r <- read_csv(resumes_path, show_col_types = FALSE)
    if (nrow(r) > 0 && "TEAM" %in% names(r)) {
      r <- r %>% rename(Season = YEAR, Team = TEAM)
      r$TeamID <- map_kenpom_to_teamids(r, lookup)
      elo_col <- names(r)[grep("^ELO$", names(r), ignore.case = TRUE)][1]
      wab_col <- names(r)[grep("WAB", names(r), ignore.case = TRUE)][1]
      r <- r %>%
        filter(!is.na(TeamID)) %>%
        mutate(
          elo = if (length(elo_col) > 0 && !is.na(elo_col)) suppressWarnings(as.numeric(.data[[elo_col]])) else NA_real_,
          wab_resume = if (length(wab_col) > 0 && !is.na(wab_col)) suppressWarnings(as.numeric(.data[[wab_col]])) else NA_real_
        ) %>%
        select(Season, TeamID, elo, wab_resume)
      out <- r
    }
  }
  if (file.exists(teamsheet_path)) {
    ts <- read_csv(teamsheet_path, show_col_types = FALSE)
    if (nrow(ts) > 0 && "TEAM" %in% names(ts)) {
      ts <- ts %>% rename(Season = YEAR, Team = TEAM)
      ts$TeamID <- map_kenpom_to_teamids(ts, lookup)
      net_col <- names(ts)[grep("^NET$", names(ts), ignore.case = TRUE)][1]
      wab_col <- names(ts)[grep("^WAB$", names(ts), ignore.case = TRUE)][1]
      ts <- ts %>%
        filter(!is.na(TeamID)) %>%
        mutate(
          net = if (length(net_col) > 0 && !is.na(net_col)) suppressWarnings(as.numeric(.data[[net_col]])) else NA_real_,
          wab = if (length(wab_col) > 0 && !is.na(wab_col)) suppressWarnings(as.numeric(.data[[wab_col]])) else NA_real_
        ) %>%
        select(Season, TeamID, net, wab)
      if (nrow(out) > 0) {
        out <- out %>%
          full_join(ts, by = c("Season", "TeamID")) %>%
          mutate(
            wab = coalesce(wab, wab_resume),
            elo = replace_na(elo, 0),
            net = replace_na(net, 200),
            wab = replace_na(wab, 200)
          ) %>%
          select(Season, TeamID, elo, net, wab)
      } else {
        out <- ts %>% mutate(elo = 0, net = replace_na(net, 200), wab = replace_na(wab, 200)) %>%
          select(Season, TeamID, elo, net, wab)
      }
    }
  }
  # When only Resumes: ensure net, wab present
  if (nrow(out) > 0 && !"net" %in% names(out)) {
    out <- out %>% mutate(net = 200, wab = coalesce(wab_resume, 200)) %>% select(Season, TeamID, elo, net, wab)
  } else if (nrow(out) > 0 && "wab_resume" %in% names(out)) {
    out <- out %>% mutate(wab = coalesce(wab, wab_resume, 200)) %>% select(Season, TeamID, elo, net, wab)
  }
  out
}

#' Load team-conference mapping and conference strength (BADJ EM)
#' Uses KenPom Conference and raw_nishaa Conference Stats.csv
#' @return Tibble Season, TeamID, conf_em (conference strength, higher = stronger conf)
load_conference_strength <- function(lookup, conf_stats_path = NULL) {
  if (is.null(conf_stats_path)) conf_stats_path <- file.path(NISHAA_DIR, "Conference Stats.csv")
  if (!file.exists(conf_stats_path)) return(tibble(Season = integer(), TeamID = integer(), conf_em = numeric()))
  conf <- read_csv(conf_stats_path, show_col_types = FALSE)
  if (nrow(conf) == 0) return(tibble(Season = integer(), TeamID = integer(), conf_em = numeric()))
  em_col <- names(conf)[grepl("BADJ.*EM|BADJ_EM", names(conf), ignore.case = TRUE)][1]
  if (is.na(em_col)) em_col <- "BADJ EM"
  conf <- conf %>%
    rename(Season = YEAR) %>%
    mutate(conf_em = suppressWarnings(as.numeric(.data[[em_col]]))) %>%
    select(Season, CONF, conf_em) %>%
    filter(!is.na(conf_em))
  # Team -> Conference from KenPom (github) and Barttorvik
  team_conf <- tibble(Season = integer(), TeamID = integer(), Conference = character())
  kp_path <- file.path(KENPOM_DIR, "kenpom.csv")
  if (file.exists(kp_path) && "Conference" %in% names(read_csv(kp_path, n_max = 1, show_col_types = FALSE))) {
    kp <- read_csv(kp_path, show_col_types = FALSE) %>% rename(Season = Year)
    kp$TeamID <- map_kenpom_to_teamids(kp, lookup)
    team_conf <- kp %>% filter(!is.na(TeamID)) %>%
      select(Season, TeamID, Conference) %>% distinct(Season, TeamID, .keep_all = TRUE)
  }
  bt_path <- file.path(NISHAA_DIR, "KenPom Barttorvik.csv")
  if (file.exists(bt_path) && "CONF" %in% names(read_csv(bt_path, n_max = 1, show_col_types = FALSE))) {
    bt <- read_csv(bt_path, show_col_types = FALSE) %>%
      rename(Season = YEAR, Team = TEAM, Conference = CONF)
    bt$TeamID <- map_kenpom_to_teamids(bt, lookup)
    bt_conf <- bt %>% filter(!is.na(TeamID)) %>%
      select(Season, TeamID, Conference) %>% distinct(Season, TeamID, .keep_all = TRUE)
    team_conf <- bind_rows(
      team_conf %>% filter(!Season %in% bt_conf$Season),
      bt_conf
    )
  }
  if (nrow(team_conf) == 0) return(tibble(Season = integer(), TeamID = integer(), conf_em = numeric()))
  team_conf %>%
    left_join(conf, by = c("Season", "Conference" = "CONF")) %>%
    select(Season, TeamID, conf_em) %>%
    filter(!is.na(conf_em))
}

#' Load quadrant win rates (Q1, Q1+Q2) from Teamsheet Ranks
#' @return Tibble Season, TeamID, quad1_winpct, quad12_winpct
load_quadrant_stats <- function(lookup, path = NULL) {
  if (is.null(path)) path <- file.path(NISHAA_DIR, "Teamsheet Ranks.csv")
  if (!file.exists(path)) return(tibble(Season = integer(), TeamID = integer(), quad1_winpct = numeric(), quad12_winpct = numeric()))
  ts <- read_csv(path, show_col_types = FALSE)
  if (nrow(ts) == 0) return(tibble(Season = integer(), TeamID = integer(), quad1_winpct = numeric(), quad12_winpct = numeric()))
  ts <- ts %>% rename(Season = YEAR, Team = TEAM)
  q1w <- names(ts)[grep("^Q1\\s*W$|^Q1W$", names(ts), ignore.case = TRUE)][1]
  if (is.na(q1w)) q1w <- names(ts)[grep("Q1.*W", names(ts), ignore.case = TRUE)][1]
  q1l <- names(ts)[grep("^Q1\\s*L$|^Q1L$", names(ts), ignore.case = TRUE)][1]
  if (is.na(q1l)) q1l <- names(ts)[grep("Q1.*L", names(ts), ignore.case = TRUE)][1]
  q2w <- names(ts)[grep("^Q2\\s*W$|^Q2W$", names(ts), ignore.case = TRUE)][1]
  if (is.na(q2w)) q2w <- names(ts)[grep("Q2.*W", names(ts), ignore.case = TRUE)][1]
  q2l <- names(ts)[grep("^Q2\\s*L$|^Q2L$", names(ts), ignore.case = TRUE)][1]
  if (is.na(q2l)) q2l <- names(ts)[grep("Q2.*L", names(ts), ignore.case = TRUE)][1]
  if (is.na(q1w) || is.na(q1l)) return(tibble(Season = integer(), TeamID = integer(), quad1_winpct = numeric(), quad12_winpct = numeric()))
  ts$TeamID <- map_kenpom_to_teamids(ts, lookup)
  ts %>%
    filter(!is.na(TeamID)) %>%
    mutate(
      Q1W = suppressWarnings(as.numeric(.data[[q1w]])),
      Q1L = suppressWarnings(as.numeric(.data[[q1l]])),
      Q2W = if (!is.na(q2w) && q2w %in% names(.)) suppressWarnings(as.numeric(.data[[q2w]])) else 0,
      Q2L = if (!is.na(q2l) && q2l %in% names(.)) suppressWarnings(as.numeric(.data[[q2l]])) else 0
    ) %>%
    mutate(
      quad1_winpct = if_else(Q1W + Q1L > 0, Q1W / (Q1W + Q1L), 0.5),
      quad12_winpct = if_else(Q1W + Q1L + Q2W + Q2L > 0, (Q1W + Q2W) / (Q1W + Q1L + Q2W + Q2L), 0.5)
    ) %>%
    select(Season, TeamID, quad1_winpct, quad12_winpct)
}

#' Load BARTHAG and ELITE SOS from Barttorvik CSV (optional resume metrics)
#' @return Tibble with Season, TeamID, barthag, elite_sos (or empty)
load_barttorvik_resume_metrics <- function(bt_path = NULL, lookup) {
  if (is.null(bt_path)) bt_path <- file.path(NISHAA_DIR, "KenPom Barttorvik.csv")
  if (!file.exists(bt_path)) return(tibble())
  bt <- read_csv(bt_path, show_col_types = FALSE)
  if (nrow(bt) == 0) return(tibble())
  bt <- bt %>% rename(Season = YEAR, Team = TEAM)
  bt$TeamID <- map_kenpom_to_teamids(bt, lookup)
  barthag_col <- names(bt)[grep("BARTHAG", names(bt), ignore.case = TRUE)][1]
  elite_col <- names(bt)[grep("ELITE.*SOS|ELITE_SOS", names(bt), ignore.case = TRUE)][1]
  if (is.na(barthag_col)) return(tibble())
  bt %>%
    filter(!is.na(TeamID)) %>%
    mutate(
      barthag = suppressWarnings(as.numeric(.data[[barthag_col]])),
      elite_sos = if (!is.na(elite_col) && elite_col %in% names(.)) suppressWarnings(as.numeric(.data[[elite_col]])) else NA_real_
    ) %>%
    select(Season, TeamID, barthag, elite_sos)
}
