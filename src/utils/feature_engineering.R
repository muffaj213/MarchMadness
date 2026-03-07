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

#' Map tournament DayNum to round (1=R64, 2=R32, 3=S16, 4=E8, 5=F4, 6=Champ)
#' DayNum: 134-135 First Four, 136-139 R64, 140-143 R32, 144-147 S16, 148-151 E8, 152+ F4/Champ
daynum_to_round <- function(day_num) {
  d <- as.integer(day_num)
  dplyr::case_when(
    d <= 135 ~ 1L,
    d <= 139 ~ 1L,
    d <= 143 ~ 2L,
    d <= 147 ~ 3L,
    d <= 151 ~ 4L,
    d <= 155 ~ 5L,
    TRUE ~ 6L
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
                              head_to_head = NULL, sos_stats = NULL, rest_stats = NULL) {
  # Add seed numbers
  seeds <- seeds %>%
    mutate(SeedNum = parse_seed_number(Seed))

  # Join seeds to get seed for each team in each game (preserve DayNum for round)
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
  # outcome = 1 if TeamA won (preserve DayNum for round)
  games <- games_with_seeds %>%
    mutate(
      TeamA = if_else(WSeed <= LSeed, WTeamID, LTeamID),
      TeamB = if_else(WSeed <= LSeed, LTeamID, WTeamID),
      outcome = as.integer(WTeamID == TeamA),
      SeedA = pmin(WSeed, LSeed),
      SeedB = pmax(WSeed, LSeed)
    ) %>%
    select(Season, TeamA, TeamB, WTeamID, LTeamID, outcome, SeedA, SeedB, any_of("DayNum"))

  # Seed features: diff (positive = TeamA better), squared, sum; round from DayNum
  games <- games %>%
    mutate(
      seed_diff = SeedB - SeedA,
      seed_diff_sq = seed_diff^2,
      seed_sum = SeedA + SeedB,
      round = if ("DayNum" %in% names(.)) daynum_to_round(DayNum) else 1L
    )

  # Add win pct and points stats (use 0.5 / 0 when no regular-season data)
  win_pct_dedup <- win_pct %>% distinct(Season, TeamID, .keep_all = TRUE)
  games <- games %>%
    left_join(
      win_pct_dedup %>% select(Season, TeamA = TeamID, WinPctA = WinPct),
      by = c("Season", "TeamA")
    ) %>%
    left_join(
      win_pct_dedup %>% select(Season, TeamB = TeamID, WinPctB = WinPct),
      by = c("Season", "TeamB")
    ) %>%
    mutate(
      WinPctA = replace_na(WinPctA, 0.5),
      WinPctB = replace_na(WinPctB, 0.5),
      winpct_diff = WinPctA - WinPctB
    )

  # Add late-season win pct if available
  if (!is.null(late_win_pct) && nrow(late_win_pct) > 0 && "LateWinPct" %in% names(late_win_pct)) {
    games <- games %>%
      left_join(
        late_win_pct %>% select(Season, TeamA = TeamID, LateWinPctA = LateWinPct),
        by = c("Season", "TeamA")
      ) %>%
      left_join(
        late_win_pct %>% select(Season, TeamB = TeamID, LateWinPctB = LateWinPct),
        by = c("Season", "TeamB")
      ) %>%
      mutate(
        LateWinPctA = replace_na(LateWinPctA, 0.5),
        LateWinPctB = replace_na(LateWinPctB, 0.5),
        late_winpct_diff = LateWinPctA - LateWinPctB
      ) %>%
      select(-LateWinPctA, -LateWinPctB)
  } else {
    games <- games %>% mutate(late_winpct_diff = 0)
  }

  games <- games %>%
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
      pf_diff = (PFA - PAA) - (PFB - PAB)
    )

  # Add head-to-head, SOS, rest (optional)
  if (!is.null(head_to_head) && nrow(head_to_head) > 0) {
    h2h_lookup <- head_to_head %>%
      mutate(
        Team1 = pmin(Team1, Team2), Team2 = pmax(Team1, Team2),
        Team1Wins = h2h_team1_wins
      ) %>%
      select(Season, Team1, Team2, h2h_games, Team1Wins)
    games <- games %>%
      mutate(T1 = pmin(TeamA, TeamB), T2 = pmax(TeamA, TeamB)) %>%
      left_join(h2h_lookup, by = c("Season", "T1" = "Team1", "T2" = "Team2")) %>%
      mutate(
        h2h_team_a_wins = if_else(TeamA == T1, replace_na(Team1Wins, 0L), replace_na(h2h_games - Team1Wins, 0L)),
        h2h_games = replace_na(h2h_games, 0L),
        h2h_team_a_winpct = if_else(h2h_games > 0, h2h_team_a_wins / h2h_games, 0.5)
      ) %>%
      select(-T1, -T2, -Team1Wins)
  } else {
    games <- games %>% mutate(h2h_team_a_winpct = 0.5)
  }
  if (!is.null(sos_stats) && nrow(sos_stats) > 0) {
    games <- games %>%
      left_join(sos_stats %>% select(Season, TeamA = TeamID, sos_A = sos), by = c("Season", "TeamA")) %>%
      left_join(sos_stats %>% select(Season, TeamB = TeamID, sos_B = sos), by = c("Season", "TeamB")) %>%
      mutate(
        sos_A = replace_na(sos_A, 0.5), sos_B = replace_na(sos_B, 0.5),
        sos_diff = sos_A - sos_B
      ) %>%
      select(-sos_A, -sos_B)
  } else {
    games <- games %>% mutate(sos_diff = 0)
  }
  if (!is.null(rest_stats) && nrow(rest_stats) > 0) {
    games <- games %>%
      left_join(rest_stats %>% select(Season, TeamA = TeamID, rest_A = days_rest), by = c("Season", "TeamA")) %>%
      left_join(rest_stats %>% select(Season, TeamB = TeamID, rest_B = days_rest), by = c("Season", "TeamB")) %>%
      mutate(rest_diff = replace_na(rest_A, 0L) - replace_na(rest_B, 0L)) %>%
      select(-rest_A, -rest_B)
  } else {
    games <- games %>% mutate(rest_diff = 0L)
  }

  # Add KenPom features if available
  if (!is.null(kenpom_stats) && nrow(kenpom_stats) > 0) {
    kenpom_dedup <- kenpom_stats %>% distinct(Season, TeamID, .keep_all = TRUE)
    games <- games %>%
      left_join(
        kenpom_dedup %>% select(Season, TeamA = TeamID, adj_em_A = adj_em, adj_o_A = adj_o, adj_d_A = adj_d, adj_t_A = adj_t),
        by = c("Season", "TeamA")
      ) %>%
      left_join(
        kenpom_dedup %>% select(Season, TeamB = TeamID, adj_em_B = adj_em, adj_o_B = adj_o, adj_d_B = adj_d, adj_t_B = adj_t),
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
      ) %>%
      mutate(
        seed_winpct_interaction = seed_diff * winpct_diff,
        adjem_seed_interaction = adjem_diff * seed_diff,
        seed_latewinpct_interaction = seed_diff * late_winpct_diff
      )
    games <- games %>%
      select(Season, TeamA, TeamB, outcome, round, seed_diff, seed_diff_sq, seed_sum, winpct_diff, late_winpct_diff,
             seed_winpct_interaction, pf_diff, h2h_team_a_winpct, sos_diff, rest_diff,
             adjem_diff, adj_off_diff, adj_def_diff, tempo_diff, adjem_seed_interaction, seed_latewinpct_interaction)
  } else {
    games <- games %>%
      mutate(
        seed_winpct_interaction = seed_diff * winpct_diff,
        adjem_seed_interaction = 0,
        seed_latewinpct_interaction = seed_diff * late_winpct_diff
      ) %>%
      select(Season, TeamA, TeamB, outcome, round, seed_diff, seed_diff_sq, seed_sum, winpct_diff, late_winpct_diff,
             seed_winpct_interaction, pf_diff, h2h_team_a_winpct, sos_diff, rest_diff, adjem_seed_interaction, seed_latewinpct_interaction)
  }
  games
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
                                     head_to_head = NULL, sos_stats = NULL, rest_stats = NULL, round = 1L) {
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

  out <- tibble(
    round = as.integer(round),
    seed_diff = seed_diff,
    seed_diff_sq = seed_diff_sq,
    seed_sum = seed_sum,
    winpct_diff = winpct_diff,
    late_winpct_diff = late_winpct_diff,
    seed_winpct_interaction = seed_winpct_interaction,
    pf_diff = pf_diff,
    h2h_team_a_winpct = 0.5,
    sos_diff = 0,
    rest_diff = 0L,
    adjem_seed_interaction = 0,
    seed_latewinpct_interaction = seed_diff * late_winpct_diff
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
