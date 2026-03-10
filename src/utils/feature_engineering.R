# =============================================================================
# feature_engineering.R - Compute matchup features for NCAA tournament games
# =============================================================================

library(dplyr)
library(tidyr)

#' Parse seed string (e.g., "W01", "X16") to extract numeric seed (1-16)
#'
#' @param seed_char Character vector of seeds like "W01", "X02"
#' @return Integer vector of seed numbers
parse_seed_number <- function(seed_char) {
  as.integer(gsub("^[A-Za-z]+0?", "", seed_char))
}

#' Compute regular-season win percentage for each team in each season
#'
#' @param regular_results Data frame with WTeamID, LTeamID, Season
#' @return Data frame with TeamID, Season, WinPct, Wins, Losses, Games
compute_win_pct <- function(regular_results) {
  wins <- regular_results %>%
    count(Season, WTeamID, name = "Wins") %>%
    rename(TeamID = WTeamID)

  losses <- regular_results %>%
    count(Season, LTeamID, name = "Losses") %>%
    rename(TeamID = LTeamID)

  wins %>%
    full_join(losses, by = c("Season", "TeamID")) %>%
    mutate(
      Wins = replace_na(Wins, 0),
      Losses = replace_na(Losses, 0),
      Games = Wins + Losses,
      WinPct = Wins / Games
    ) %>%
    select(Season, TeamID, WinPct, Wins, Losses, Games)
}

#' Compute recent win percentage (last N games before tournament)
#' With optional exponential decay: more recent games weighted higher.
#' @param decay Decay factor for recency; 0 = flat average, >0 = exponential decay (e.g. 0.2)
#' @return Data frame with TeamID, Season, RecentWinPct, RecentWins, RecentLosses, RecentGames
compute_recent_win_pct <- function(regular_results, n_games = 10L, tourney_start_day = 134L, decay = 0.2) {
  if (!"DayNum" %in% names(regular_results) || nrow(regular_results) == 0) {
    return(tibble(Season = integer(), TeamID = integer(), RecentWinPct = numeric(),
                  RecentWins = integer(), RecentLosses = integer(), RecentGames = integer()))
  }
  reg <- regular_results %>% filter(DayNum < tourney_start_day)
  if (nrow(reg) == 0) {
    return(tibble(Season = integer(), TeamID = integer(), RecentWinPct = numeric(),
                  RecentWins = integer(), RecentLosses = integer(), RecentGames = integer()))
  }
  # Build game-level rows for each team (TeamID, Season, DayNum, Won)
  team_games <- reg %>%
    mutate(TeamID = WTeamID, Won = 1L) %>%
    select(Season, TeamID, DayNum, Won) %>%
    bind_rows(
      reg %>% mutate(TeamID = LTeamID, Won = 0L) %>% select(Season, TeamID, DayNum, Won)
    )
  team_games %>%
    arrange(Season, TeamID, desc(DayNum)) %>%
    group_by(Season, TeamID) %>%
    slice_head(n = n_games) %>%
    mutate(
      rank_recent = row_number(),
      weight = if (decay > 0) exp(-decay * (rank_recent - 1)) else 1
    ) %>%
    summarise(
      RecentWins = sum(Won),
      RecentGames = n(),
      RecentLosses = RecentGames - RecentWins,
      RecentWinPct = if_else(RecentGames > 0, sum(Won * weight) / sum(weight), NA_real_),
      .groups = "drop"
    ) %>%
    select(Season, TeamID, RecentWinPct, RecentWins, RecentLosses, RecentGames)
}

#' Compute average margin of victory in last N games (positive = winning margins)
#' With optional exponential decay: more recent games weighted higher.
#' @param decay Decay factor for recency; 0 = flat average, >0 = exponential decay (e.g. 0.2)
#' @return Data frame with TeamID, Season, RecentMOV (weighted avg pts margin per game)
compute_recent_mov <- function(regular_results, n_games = 10L, tourney_start_day = 134L, decay = 0.2) {
  if (!"DayNum" %in% names(regular_results) || !"WScore" %in% names(regular_results) || nrow(regular_results) == 0) {
    return(tibble(Season = integer(), TeamID = integer(), RecentMOV = numeric()))
  }
  reg <- regular_results %>% filter(DayNum < tourney_start_day)
  if (nrow(reg) == 0) return(tibble(Season = integer(), TeamID = integer(), RecentMOV = numeric()))
  team_games <- reg %>%
    mutate(TeamID = WTeamID, Margin = WScore - LScore) %>%
    select(Season, TeamID, DayNum, Margin) %>%
    bind_rows(
      reg %>% mutate(TeamID = LTeamID, Margin = LScore - WScore) %>% select(Season, TeamID, DayNum, Margin)
    )
  team_games %>%
    arrange(Season, TeamID, desc(DayNum)) %>%
    group_by(Season, TeamID) %>%
    slice_head(n = n_games) %>%
    mutate(
      rank_recent = row_number(),  # 1 = most recent
      weight = if (decay > 0) exp(-decay * (rank_recent - 1)) else 1
    ) %>%
    summarise(
      RecentMOV = weighted.mean(Margin, w = weight, na.rm = TRUE),
      .groups = "drop"
    )
}

#' Compute late-season win percentage (last N days or last N games)
#' DayNum 100+ typically indicates late-season games.
#'
#' @param regular_results Data frame with WTeamID, LTeamID, Season, DayNum
#' @param day_cutoff Include games with DayNum >= this (e.g., 90 = roughly last month)
#' @return Data frame with TeamID, Season, LateWinPct, LateWins, LateLosses, LateGames
compute_late_win_pct <- function(regular_results, day_cutoff = 90) {
  if (!"DayNum" %in% names(regular_results)) {
    return(tibble(Season = integer(), TeamID = integer(), LateWinPct = numeric(),
                  LateWins = integer(), LateLosses = integer(), LateGames = integer()))
  }
  late <- regular_results %>% filter(DayNum >= day_cutoff)
  if (nrow(late) == 0) {
    return(tibble(Season = integer(), TeamID = integer(), LateWinPct = numeric(),
                  LateWins = integer(), LateLosses = integer(), LateGames = integer()))
  }
  wins <- late %>%
    count(Season, WTeamID, name = "LateWins") %>%
    rename(TeamID = WTeamID)
  losses <- late %>%
    count(Season, LTeamID, name = "LateLosses") %>%
    rename(TeamID = LTeamID)
  wins %>%
    full_join(losses, by = c("Season", "TeamID")) %>%
    mutate(
      LateWins = replace_na(LateWins, 0),
      LateLosses = replace_na(LateLosses, 0),
      LateGames = LateWins + LateLosses,
      LateWinPct = if_else(LateGames > 0, LateWins / LateGames, NA_real_)
    ) %>%
    select(Season, TeamID, LateWinPct, LateWins, LateLosses, LateGames)
}

#' Map tournament DayNum to round (0=First Four, 1=R64, 2=R32, 3=S16, 4=E8, 5=F4, 6=Champ)
#' DayNum: 134-135 First Four, 136-139 R64, 140-143 R32, 144-147 S16, 148-151 E8, 152+ F4/Champ
#' Matches bracket_logic.R where play-in slots get round=0 for consistency (train vs predict).
daynum_to_round <- function(day_num) {
  d <- as.integer(day_num)
  dplyr::case_when(
    d <= 135 ~ 0L,   # First Four (play-in) - matches prediction round for play-in slots
    d <= 139 ~ 1L,   # R64
    d <= 143 ~ 2L,   # R32
    d <= 147 ~ 3L,   # S16
    d <= 151 ~ 4L,   # E8
    d <= 155 ~ 5L,   # F4
    TRUE ~ 6L        # Championship
  )
}

#' Compute head-to-head record between all team pairs per season
#'
#' @param regular_results Data frame with WTeamID, LTeamID, Season
#' @return Data frame with Season, Team1, Team2, h2h_games, h2h_team1_wins (Team1 < Team2)
compute_head_to_head <- function(regular_results) {
  if (nrow(regular_results) == 0) {
    return(tibble(Season = integer(), Team1 = integer(), Team2 = integer(),
                  h2h_games = integer(), h2h_team1_wins = integer()))
  }
  reg <- regular_results %>%
    mutate(
      Team1 = pmin(WTeamID, LTeamID),
      Team2 = pmax(WTeamID, LTeamID),
      Team1Won = as.integer(WTeamID == Team1)
    )
  reg %>%
    group_by(Season, Team1, Team2) %>%
    summarise(
      h2h_games = n(),
      h2h_team1_wins = sum(Team1Won),
      .groups = "drop"
    )
}

#' Compute strength of schedule (average opponent win pct) per team per season
#'
#' @param regular_results Data frame with WTeamID, LTeamID, Season
#' @param win_pct Win percentage by team/season (Season, TeamID, WinPct)
#' @return Data frame with Season, TeamID, sos (avg opponent WinPct)
compute_sos <- function(regular_results, win_pct) {
  if (nrow(regular_results) == 0 || nrow(win_pct) == 0) {
    return(tibble(Season = integer(), TeamID = integer(), sos = numeric()))
  }
  # Get opponents for each game: when Team A plays, opponent is the other team
  opp_wp <- regular_results %>%
    mutate(TeamID = WTeamID, OppID = LTeamID) %>%
    bind_rows(
      regular_results %>% mutate(TeamID = LTeamID, OppID = WTeamID)
    ) %>%
    left_join(
      win_pct %>% select(Season, OppID = TeamID, OppWinPct = WinPct),
      by = c("Season", "OppID")
    ) %>%
    mutate(OppWinPct = replace_na(OppWinPct, 0.5))
  opp_wp %>%
    group_by(Season, TeamID) %>%
    summarise(sos = mean(OppWinPct, na.rm = TRUE), .groups = "drop")
}

#' Compute teams that played in the First Four (DayNum 134-135)
#' First Four teams have less rest before their R1 game.
#' @param tourney_results Tournament results with DayNum
#' @return Tibble Season, TeamID, played_first_four (1/0)
compute_first_four_teams <- function(tourney_results) {
  if (!"DayNum" %in% names(tourney_results) || nrow(tourney_results) == 0) {
    return(tibble(Season = integer(), TeamID = integer(), played_first_four = integer()))
  }
  ff <- tourney_results %>%
    filter(DayNum >= 134L, DayNum <= 135L) %>%
    select(Season, WTeamID, LTeamID) %>%
    pivot_longer(c(WTeamID, LTeamID), values_to = "TeamID") %>%
    distinct(Season, TeamID) %>%
    mutate(played_first_four = 1L)
  ff
}

#' Compute days of rest before tournament (days since last regular-season game)
#'
#' @param regular_results Data frame with WTeamID, LTeamID, Season, DayNum
#' @param tourney_start_day First DayNum of tournament (typically 134)
#' @return Data frame with Season, TeamID, days_rest
compute_rest <- function(regular_results, tourney_start_day = 134L) {
  if (!"DayNum" %in% names(regular_results) || nrow(regular_results) == 0) {
    return(tibble(Season = integer(), TeamID = integer(), days_rest = integer()))
  }
  last_game <- regular_results %>%
    filter(DayNum < tourney_start_day) %>%
    mutate(TeamID = WTeamID, GameDay = DayNum) %>%
    bind_rows(
      regular_results %>%
        filter(DayNum < tourney_start_day) %>%
        mutate(TeamID = LTeamID, GameDay = DayNum)
    ) %>%
    group_by(Season, TeamID) %>%
    summarise(last_day = max(GameDay, na.rm = TRUE), .groups = "drop")
  last_game %>%
    mutate(days_rest = tourney_start_day - last_day) %>%
    select(Season, TeamID, days_rest)
}

#' Compute points-for and points-against per game for each team
#'
#' @param regular_results Data frame with WTeamID, LTeamID, WScore, LScore, Season
#' @return Data frame with TeamID, Season, PF_per_game, PA_per_game
compute_points_stats <- function(regular_results) {
  # Points when team won
  pf_wins <- regular_results %>%
    group_by(Season, WTeamID) %>%
    summarise(PF = sum(WScore), PA = sum(LScore), Games = n(), .groups = "drop") %>%
    rename(TeamID = WTeamID)

  # Points when team lost
  pf_losses <- regular_results %>%
    group_by(Season, LTeamID) %>%
    summarise(PF = sum(LScore), PA = sum(WScore), Games = n(), .groups = "drop") %>%
    rename(TeamID = LTeamID)

  pf_wins %>%
    full_join(pf_losses, by = c("Season", "TeamID"), suffix = c("_w", "_l")) %>%
    mutate(
      PF = replace_na(PF_w, 0) + replace_na(PF_l, 0),
      PA = replace_na(PA_w, 0) + replace_na(PA_l, 0),
      Games = replace_na(Games_w, 0) + replace_na(Games_l, 0),
      PF_per_game = PF / Games,
      PA_per_game = PA / Games
    ) %>%
    select(Season, TeamID, PF_per_game, PA_per_game)
}

#' Build matchup-level training data from tournament results
#'
#' For each tournament game, creates one row with features and outcome.
#' outcome = 1 if WTeamID (winner) is "team_a" in the row.
#'
#' Delegates feature computation to compute_matchup_features (single source of truth).
#'
#' Note on pf_diff: Uses regular-season points margin. If raw_extended has no regular
#' data for a season (e.g. 2008-2013), both teams get default 70,70 -> pf_diff=0.
#' Ensure MRegularSeasonCompactResults covers all tournament seasons.
#'
#' @param tourney_results Tournament results (WTeamID, LTeamID, Season, DayNum)
#' @param seeds Seeds data (Season, Seed, TeamID)
#' @param win_pct Win percentage by team/season
#' @param points_stats Points stats by team/season
#' @param kenpom_stats Optional KenPom stats (Season, TeamID, adj_em, adj_o, adj_d, adj_t)
#' @param late_win_pct Optional late-season win pct (Season, TeamID, LateWinPct)
#' @param head_to_head Optional H2H (Season, Team1, Team2, h2h_games, h2h_team1_wins)
#' @param sos_stats Optional SOS (Season, TeamID, sos)
#' @param rest_stats Optional rest (Season, TeamID, days_rest)
#' @return Data frame with Season, TeamA, TeamB, seed_diff, winpct_diff, etc., outcome
build_matchup_data <- function(tourney_results, seeds, win_pct, points_stats, kenpom_stats = NULL, late_win_pct = NULL,
                              head_to_head = NULL, sos_stats = NULL, rest_stats = NULL,
                              home_away_stats = NULL, resume_stats = NULL, recent_win_pct = NULL, recent_mov = NULL,
                              conference_stats = NULL, quadrant_stats = NULL, first_four_stats = NULL) {
  seeds <- seeds %>% mutate(SeedNum = parse_seed_number(Seed))

  games_with_seeds <- tourney_results %>%
    left_join(
      seeds %>% select(Season, WTeamID = TeamID, SeedNum),
      by = c("Season", "WTeamID")
    ) %>%
    rename(WSeed = SeedNum) %>%
    left_join(
      seeds %>% select(Season, LTeamID = TeamID, SeedNum),
      by = c("Season", "LTeamID")
    ) %>%
    rename(LSeed = SeedNum)

  games <- games_with_seeds %>%
    mutate(
      TeamA = if_else(WSeed <= LSeed, WTeamID, LTeamID),
      TeamB = if_else(WSeed <= LSeed, LTeamID, WTeamID),
      outcome = as.integer(WTeamID == TeamA),
      round = if ("DayNum" %in% names(.)) daynum_to_round(DayNum) else 1L
    ) %>%
    select(Season, TeamA, TeamB, outcome, round)

  rows <- vector("list", nrow(games))
  for (i in seq_len(nrow(games))) {
    row <- games[i, ]
    feat <- compute_matchup_features(
      team_a = row$TeamA, team_b = row$TeamB, season = row$Season,
      seeds = seeds, win_pct = win_pct, points_stats = points_stats,
      kenpom_stats = kenpom_stats, late_win_pct = late_win_pct,
      head_to_head = head_to_head, sos_stats = sos_stats, rest_stats = rest_stats,
      home_away_stats = home_away_stats, resume_stats = resume_stats,
      recent_win_pct = recent_win_pct, recent_mov = recent_mov,
      conference_stats = conference_stats, quadrant_stats = quadrant_stats,
      first_four_stats = first_four_stats,
      round = row$round
    )
    rows[[i]] <- bind_cols(
      tibble(Season = row$Season, TeamA = row$TeamA, TeamB = row$TeamB, outcome = row$outcome),
      feat
    )
  }
  out <- bind_rows(rows)
  out %>%
    select(Season, TeamA, TeamB, outcome, round, seed_diff, seed_diff_sq, seed_sum, winpct_diff, late_winpct_diff,
           recent_winpct_diff, recent_mov_diff, is_upset_matchup, upset_seed_gap, seed_winpct_interaction, pf_diff, h2h_team_a_winpct, h2h_games,
           sos_diff, rest_diff, conf_em_diff, quad1_winpct_diff, quad12_winpct_diff, first_four_rest_diff,
           home_win_rate_diff, away_win_rate_diff, elo_diff, net_diff, wab_diff, barthag_diff, elite_sos_diff,
           adjem_diff, adj_off_diff, adj_def_diff, tempo_diff, luck_diff, off_vs_def_adv, adjem_seed_interaction, seed_latewinpct_interaction,
           round_seed_interaction, seed_barthag_interaction, seed_recentmov_interaction)
}

#' Compute features for a single matchup (for prediction)
#'
#' @param team_a Team A ID (better seed)
#' @param team_b Team B ID
#' @param season Season year
#' @param seeds Seeds for the season
#' @param win_pct Win percentages
#' @param points_stats Points statistics
#' @param kenpom_stats Optional KenPom stats
#' @param late_win_pct Optional late-season win pct (Season, TeamID, LateWinPct)
#' @param head_to_head Optional H2H (Season, Team1, Team2, h2h_games, h2h_team1_wins)
#' @param sos_stats Optional SOS (Season, TeamID, sos)
#' @param rest_stats Optional rest (Season, TeamID, days_rest)
#' @param round Round of game (1-6); default 1
#' @return Data frame with one row of features
compute_matchup_features <- function(team_a, team_b, season, seeds, win_pct, points_stats, kenpom_stats = NULL, late_win_pct = NULL,
                                     head_to_head = NULL, sos_stats = NULL, rest_stats = NULL,
                                     home_away_stats = NULL, resume_stats = NULL, recent_win_pct = NULL, recent_mov = NULL,
                                     conference_stats = NULL, quadrant_stats = NULL, first_four_stats = NULL, round = 1L) {
  seeds <- seeds %>% mutate(SeedNum = parse_seed_number(Seed))

  seed_a <- seeds %>% filter(Season == season, TeamID == team_a) %>% pull(SeedNum)
  seed_b <- seeds %>% filter(Season == season, TeamID == team_b) %>% pull(SeedNum)
  seed_a <- if (length(seed_a) == 0) NA_integer_ else seed_a[1]
  seed_b <- if (length(seed_b) == 0) NA_integer_ else seed_b[1]

  wp_a <- win_pct %>% filter(Season == season, TeamID == team_a) %>% pull(WinPct)
  wp_b <- win_pct %>% filter(Season == season, TeamID == team_b) %>% pull(WinPct)
  wp_a <- if (length(wp_a) == 0) 0.5 else wp_a[1]
  wp_b <- if (length(wp_b) == 0) 0.5 else wp_b[1]

  pf_a <- points_stats %>% filter(Season == season, TeamID == team_a)
  pf_b <- points_stats %>% filter(Season == season, TeamID == team_b)
  margin_a <- if (nrow(pf_a) > 0) pf_a$PF_per_game[1] - pf_a$PA_per_game[1] else 0
  margin_b <- if (nrow(pf_b) > 0) pf_b$PF_per_game[1] - pf_b$PA_per_game[1] else 0

  seed_diff <- if (is.na(seed_a) || is.na(seed_b)) 0 else (seed_b - seed_a)
  winpct_diff <- wp_a - wp_b
  pf_diff <- margin_a - margin_b
  seed_winpct_interaction <- seed_diff * winpct_diff
  seed_diff_sq <- seed_diff^2
  seed_sum <- if (is.na(seed_a) || is.na(seed_b)) NA_integer_ else (seed_a + seed_b)
  seed_sum <- if (is.na(seed_sum)) 17L else seed_sum  # default mid-range

  # Late-season win pct (needs late_win_pct passed; for now use 0.5 each -> diff 0)
  late_wp_a <- if (!is.null(late_win_pct) && "LateWinPct" %in% names(late_win_pct)) {
    x <- late_win_pct %>% filter(Season == season, TeamID == team_a) %>% pull(LateWinPct)
    if (length(x) > 0 && !is.na(x[1])) x[1] else 0.5
  } else 0.5
  late_wp_b <- if (!is.null(late_win_pct) && "LateWinPct" %in% names(late_win_pct)) {
    x <- late_win_pct %>% filter(Season == season, TeamID == team_b) %>% pull(LateWinPct)
    if (length(x) > 0 && !is.na(x[1])) x[1] else 0.5
  } else 0.5
  late_winpct_diff <- late_wp_a - late_wp_b

  # Recent win pct (last N games)
  recent_wp_a <- if (!is.null(recent_win_pct) && "RecentWinPct" %in% names(recent_win_pct)) {
    x <- recent_win_pct %>% filter(Season == season, TeamID == team_a) %>% pull(RecentWinPct)
    if (length(x) > 0 && !is.na(x[1])) x[1] else 0.5
  } else 0.5
  recent_wp_b <- if (!is.null(recent_win_pct) && "RecentWinPct" %in% names(recent_win_pct)) {
    x <- recent_win_pct %>% filter(Season == season, TeamID == team_b) %>% pull(RecentWinPct)
    if (length(x) > 0 && !is.na(x[1])) x[1] else 0.5
  } else 0.5
  recent_winpct_diff <- recent_wp_a - recent_wp_b

  recent_mov_diff <- 0
  if (!is.null(recent_mov) && nrow(recent_mov) > 0 && "RecentMOV" %in% names(recent_mov)) {
    mov_a <- recent_mov %>% filter(Season == season, TeamID == team_a) %>% pull(RecentMOV)
    mov_b <- recent_mov %>% filter(Season == season, TeamID == team_b) %>% pull(RecentMOV)
    mov_a <- if (length(mov_a) > 0 && !is.na(mov_a[1])) mov_a[1] else 0
    mov_b <- if (length(mov_b) > 0 && !is.na(mov_b[1])) mov_b[1] else 0
    recent_mov_diff <- mov_a - mov_b
  }

  round_num <- as.integer(round)
  is_upset_matchup <- as.integer(seed_diff >= 10)
  upset_seed_gap <- seed_diff * pmax(0, 7L - round_num)

  # H2H, SOS, rest (from head_to_head, sos_stats, rest_stats when provided)
  h2h_winpct <- 0.5
  h2h_games_val <- 0L
  if (!is.null(head_to_head) && nrow(head_to_head) > 0) {
    t1 <- pmin(team_a, team_b)
    t2 <- pmax(team_a, team_b)
    h2h_row <- head_to_head %>% filter(Season == season, Team1 == t1, Team2 == t2)
    if (nrow(h2h_row) > 0) {
      h2h_games_val <- as.integer(h2h_row$h2h_games[1])
      if (h2h_games_val > 0) {
        wins <- if (team_a == t1) h2h_row$h2h_team1_wins[1] else (h2h_row$h2h_games[1] - h2h_row$h2h_team1_wins[1])
        h2h_winpct <- wins / h2h_row$h2h_games[1]
      }
    }
  }
  sos_diff_val <- 0
  if (!is.null(sos_stats) && nrow(sos_stats) > 0) {
    sos_a <- sos_stats %>% filter(Season == season, TeamID == team_a) %>% pull(sos)
    sos_b <- sos_stats %>% filter(Season == season, TeamID == team_b) %>% pull(sos)
    sos_a <- if (length(sos_a) > 0 && !is.na(sos_a[1])) sos_a[1] else 0.5
    sos_b <- if (length(sos_b) > 0 && !is.na(sos_b[1])) sos_b[1] else 0.5
    sos_diff_val <- sos_a - sos_b
  }
  rest_diff_val <- 0L
  if (!is.null(rest_stats) && nrow(rest_stats) > 0) {
    rest_a <- rest_stats %>% filter(Season == season, TeamID == team_a) %>% pull(days_rest)
    rest_b <- rest_stats %>% filter(Season == season, TeamID == team_b) %>% pull(days_rest)
    rest_a <- if (length(rest_a) > 0 && !is.na(rest_a[1])) rest_a[1] else 0L
    rest_b <- if (length(rest_b) > 0 && !is.na(rest_b[1])) rest_b[1] else 0L
    rest_diff_val <- as.integer(rest_a - rest_b)
  }
  conf_em_diff_val <- 0
  quad1_diff_val <- 0
  quad12_diff_val <- 0
  first_four_rest_diff_val <- 0L
  if (!is.null(conference_stats) && nrow(conference_stats) > 0) {
    ca <- conference_stats %>% filter(Season == season, TeamID == team_a) %>% pull(conf_em)
    cb <- conference_stats %>% filter(Season == season, TeamID == team_b) %>% pull(conf_em)
    conf_em_diff_val <- (if (length(ca) > 0 && !is.na(ca[1])) ca[1] else 0) - (if (length(cb) > 0 && !is.na(cb[1])) cb[1] else 0)
  }
  if (!is.null(quadrant_stats) && nrow(quadrant_stats) > 0) {
    qa <- quadrant_stats %>% filter(Season == season, TeamID == team_a)
    qb <- quadrant_stats %>% filter(Season == season, TeamID == team_b)
    quad1_a <- if (nrow(qa) > 0) qa$quad1_winpct[1] else 0.5
    quad1_b <- if (nrow(qb) > 0) qb$quad1_winpct[1] else 0.5
    quad12_a <- if (nrow(qa) > 0) qa$quad12_winpct[1] else 0.5
    quad12_b <- if (nrow(qb) > 0) qb$quad12_winpct[1] else 0.5
    quad1_diff_val <- quad1_a - quad1_b
    quad12_diff_val <- quad12_a - quad12_b
  }
  if (!is.null(first_four_stats) && nrow(first_four_stats) > 0) {
    ff_a <- first_four_stats %>% filter(Season == season, TeamID == team_a) %>% pull(played_first_four)
    ff_b <- first_four_stats %>% filter(Season == season, TeamID == team_b) %>% pull(played_first_four)
    first_four_rest_diff_val <- as.integer((if (length(ff_b) > 0 && !is.na(ff_b[1])) ff_b[1] else 0) - (if (length(ff_a) > 0 && !is.na(ff_a[1])) ff_a[1] else 0))
  }

  out <- tibble(
    round = round_num,
    seed_diff = seed_diff,
    seed_diff_sq = seed_diff_sq,
    seed_sum = seed_sum,
    winpct_diff = winpct_diff,
    late_winpct_diff = late_winpct_diff,
    recent_winpct_diff = recent_winpct_diff,
    recent_mov_diff = recent_mov_diff,
    is_upset_matchup = is_upset_matchup,
    upset_seed_gap = upset_seed_gap,
    seed_winpct_interaction = seed_winpct_interaction,
    pf_diff = pf_diff,
    h2h_team_a_winpct = h2h_winpct,
    h2h_games = h2h_games_val,
    sos_diff = sos_diff_val,
    rest_diff = rest_diff_val,
    conf_em_diff = conf_em_diff_val,
    quad1_winpct_diff = quad1_diff_val,
    quad12_winpct_diff = quad12_diff_val,
    first_four_rest_diff = first_four_rest_diff_val,
    home_win_rate_diff = 0,
    away_win_rate_diff = 0,
    elo_diff = 0,
    net_diff = 0,
    wab_diff = 0,
    barthag_diff = 0,
    elite_sos_diff = 0,
    adjem_diff = 0,
    adj_off_diff = 0,
    adj_def_diff = 0,
    tempo_diff = 0,
    luck_diff = 0,
    off_vs_def_adv = 0,
    adjem_seed_interaction = 0,
    seed_latewinpct_interaction = seed_diff * late_winpct_diff,
    round_seed_interaction = round_num * seed_diff
  )

  # Home/away and resume features
  if (!is.null(home_away_stats) && nrow(home_away_stats) > 0) {
    ha_a <- home_away_stats %>% filter(Season == season, TeamID == team_a)
    ha_b <- home_away_stats %>% filter(Season == season, TeamID == team_b)
    home_a <- if (nrow(ha_a) > 0) ha_a$home_win_rate[1] else 0.5
    home_b <- if (nrow(ha_b) > 0) ha_b$home_win_rate[1] else 0.5
    away_a <- if (nrow(ha_a) > 0) ha_a$away_win_rate[1] else 0.5
    away_b <- if (nrow(ha_b) > 0) ha_b$away_win_rate[1] else 0.5
    out <- out %>% mutate(home_win_rate_diff = home_a - home_b, away_win_rate_diff = away_a - away_b)
  }
  if (!is.null(resume_stats) && nrow(resume_stats) > 0) {
    rs_a <- resume_stats %>% filter(Season == season, TeamID == team_a)
    rs_b <- resume_stats %>% filter(Season == season, TeamID == team_b)
    elo_a <- if (nrow(rs_a) > 0) rs_a$elo[1] else 0
    elo_b <- if (nrow(rs_b) > 0) rs_b$elo[1] else 0
    net_a <- if (nrow(rs_a) > 0) rs_a$net[1] else 200
    net_b <- if (nrow(rs_b) > 0) rs_b$net[1] else 200
    wab_a <- if (nrow(rs_a) > 0) rs_a$wab[1] else 200
    wab_b <- if (nrow(rs_b) > 0) rs_b$wab[1] else 200
    barthag_a <- if (nrow(rs_a) > 0 && "barthag" %in% names(rs_a)) rs_a$barthag[1] else 0.5
    barthag_b <- if (nrow(rs_b) > 0 && "barthag" %in% names(rs_b)) rs_b$barthag[1] else 0.5
    elite_sos_a <- if (nrow(rs_a) > 0 && "elite_sos" %in% names(rs_a)) rs_a$elite_sos[1] else 0
    elite_sos_b <- if (nrow(rs_b) > 0 && "elite_sos" %in% names(rs_b)) rs_b$elite_sos[1] else 0
    out <- out %>% mutate(
      elo_diff = elo_a - elo_b,
      net_diff = net_b - net_a,
      wab_diff = wab_b - wab_a,
      barthag_diff = barthag_a - barthag_b,
      elite_sos_diff = elite_sos_a - elite_sos_b
    )
  }

  # KenPom features
  if (!is.null(kenpom_stats) && nrow(kenpom_stats) > 0) {
    kp_a <- kenpom_stats %>% filter(Season == season, TeamID == team_a)
    kp_b <- kenpom_stats %>% filter(Season == season, TeamID == team_b)
    adj_em_a <- if (nrow(kp_a) > 0) kp_a$adj_em[1] else 0
    adj_em_b <- if (nrow(kp_b) > 0) kp_b$adj_em[1] else 0
    adj_o_a <- if (nrow(kp_a) > 0) kp_a$adj_o[1] else 100
    adj_o_b <- if (nrow(kp_b) > 0) kp_b$adj_o[1] else 100
    adj_d_a <- if (nrow(kp_a) > 0) kp_a$adj_d[1] else 100
    adj_d_b <- if (nrow(kp_b) > 0) kp_b$adj_d[1] else 100
    adj_t_a <- if (nrow(kp_a) > 0) kp_a$adj_t[1] else 68
    adj_t_b <- if (nrow(kp_b) > 0) kp_b$adj_t[1] else 68
    luck_a <- if (nrow(kp_a) > 0 && "luck" %in% names(kp_a)) kp_a$luck[1] else 0
    luck_b <- if (nrow(kp_b) > 0 && "luck" %in% names(kp_b)) kp_b$luck[1] else 0
    off_vs_def_adv <- (adj_o_a - adj_d_b) - (adj_o_b - adj_d_a)
    out <- out %>%
      mutate(
        adjem_diff = adj_em_a - adj_em_b,
        adj_off_diff = adj_o_a - adj_o_b,
        adj_def_diff = adj_d_b - adj_d_a,
        tempo_diff = adj_t_a - adj_t_b,
        luck_diff = luck_a - luck_b,
        off_vs_def_adv = off_vs_def_adv,
        adjem_seed_interaction = (adj_em_a - adj_em_b) * seed_diff
      )
  }
  # New seed x metric interactions (always compute from final values)
  out <- out %>% mutate(
    seed_barthag_interaction = seed_diff * barthag_diff,
    seed_recentmov_interaction = seed_diff * recent_mov_diff
  )
  out
}
