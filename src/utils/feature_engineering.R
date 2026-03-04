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
#' @param tourney_results Tournament results (WTeamID, LTeamID, Season)
#' @param seeds Seeds data (Season, Seed, TeamID)
#' @param win_pct Win percentage by team/season
#' @param points_stats Points stats by team/season
#' @param kenpom_stats Optional KenPom stats (Season, TeamID, adj_em, adj_o, adj_d, adj_t)
#' @return Data frame with Season, TeamA, TeamB, seed_diff, winpct_diff, etc., outcome
build_matchup_data <- function(tourney_results, seeds, win_pct, points_stats, kenpom_stats = NULL) {
  # Add seed numbers
  seeds <- seeds %>%
    mutate(SeedNum = parse_seed_number(Seed))

  # Join seeds to get seed for each team in each game
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

  # Create consistent pair: TeamA = better (lower) seed, TeamB = worse seed
  # outcome = 1 if TeamA won
  games <- games_with_seeds %>%
    mutate(
      TeamA = if_else(WSeed <= LSeed, WTeamID, LTeamID),
      TeamB = if_else(WSeed <= LSeed, LTeamID, WTeamID),
      outcome = as.integer(WTeamID == TeamA),
      SeedA = pmin(WSeed, LSeed),
      SeedB = pmax(WSeed, LSeed)
    ) %>%
    select(Season, TeamA, TeamB, WTeamID, LTeamID, outcome, SeedA, SeedB)

  # Seed diff: positive = TeamA (lower seed number) is better
  games <- games %>%
    mutate(seed_diff = SeedB - SeedA)

  # Add win pct and points stats (use 0.5 / 0 when no regular-season data)
  games <- games %>%
    left_join(
      win_pct %>% select(Season, TeamA = TeamID, WinPctA = WinPct),
      by = c("Season", "TeamA")
    ) %>%
    left_join(
      win_pct %>% select(Season, TeamB = TeamID, WinPctB = WinPct),
      by = c("Season", "TeamB")
    ) %>%
    mutate(
      WinPctA = replace_na(WinPctA, 0.5),
      WinPctB = replace_na(WinPctB, 0.5),
      winpct_diff = WinPctA - WinPctB
    ) %>%
    left_join(
      points_stats %>% select(Season, TeamA = TeamID, PFA = PF_per_game, PAA = PA_per_game),
      by = c("Season", "TeamA")
    ) %>%
    left_join(
      points_stats %>% select(Season, TeamB = TeamID, PFB = PF_per_game, PAB = PA_per_game),
      by = c("Season", "TeamB")
    ) %>%
    mutate(
      PFA = replace_na(PFA, 70), PAA = replace_na(PAA, 70),
      PFB = replace_na(PFB, 70), PAB = replace_na(PAB, 70),
      pf_diff = (PFA - PAA) - (PFB - PAB),
      rating_diff = winpct_diff
    )

  # Add KenPom features if available
  if (!is.null(kenpom_stats) && nrow(kenpom_stats) > 0) {
    games <- games %>%
      left_join(
        kenpom_stats %>% select(Season, TeamA = TeamID, adj_em_A = adj_em, adj_o_A = adj_o, adj_d_A = adj_d, adj_t_A = adj_t),
        by = c("Season", "TeamA")
      ) %>%
      left_join(
        kenpom_stats %>% select(Season, TeamB = TeamID, adj_em_B = adj_em, adj_o_B = adj_o, adj_d_B = adj_d, adj_t_B = adj_t),
        by = c("Season", "TeamB")
      ) %>%
      mutate(
        adj_em_A = replace_na(adj_em_A, 0), adj_em_B = replace_na(adj_em_B, 0),
        adj_o_A = replace_na(adj_o_A, 100), adj_o_B = replace_na(adj_o_B, 100),
        adj_d_A = replace_na(adj_d_A, 100), adj_d_B = replace_na(adj_d_B, 100),
        adj_t_A = replace_na(adj_t_A, 68), adj_t_B = replace_na(adj_t_B, 68),
        adjem_diff = adj_em_A - adj_em_B,
        adj_off_diff = adj_o_A - adj_o_B,
        adj_def_diff = adj_d_B - adj_d_A,  # lower adj_d = better D, so B - A when A better
        tempo_diff = adj_t_A - adj_t_B
      )
    games <- games %>%
      select(Season, TeamA, TeamB, outcome, seed_diff, winpct_diff, rating_diff, pf_diff,
             adjem_diff, adj_off_diff, adj_def_diff, tempo_diff)
  } else {
    games <- games %>%
      select(Season, TeamA, TeamB, outcome, seed_diff, winpct_diff, rating_diff, pf_diff)
  }
  games
}

#' Compute features for a single matchup (for prediction)
#'
#' @param team_a Team A ID
#' @param team_b Team B ID
#' @param season Season year
#' @param seeds Seeds for the season
#' @param win_pct Win percentages
#' @param points_stats Points statistics
#' @param kenpom_stats Optional KenPom stats
#' @return Data frame with one row of features
compute_matchup_features <- function(team_a, team_b, season, seeds, win_pct, points_stats, kenpom_stats = NULL) {
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
  rating_diff <- winpct_diff
  pf_diff <- margin_a - margin_b

  out <- tibble(
    seed_diff = seed_diff,
    winpct_diff = winpct_diff,
    rating_diff = rating_diff,
    pf_diff = pf_diff
  )

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
    out <- out %>%
      mutate(
        adjem_diff = adj_em_a - adj_em_b,
        adj_off_diff = adj_o_a - adj_o_b,
        adj_def_diff = adj_d_b - adj_d_a,
        tempo_diff = adj_t_a - adj_t_b
      )
  }
  out
}
