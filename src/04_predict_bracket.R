# =============================================================================
# 04_predict_bracket.R - Load model and bracket, simulate, output predictions
# =============================================================================
# Run after training to predict the full bracket for a given season.
# Set PREDICT_SEASON to the year you want to predict (e.g., 2024).
# =============================================================================

library(here)
library(readr)
library(dplyr)
library(tidymodels)  # for predict.workflow on saved model

source(here("src", "config.R"))
source(here("src", "utils", "feature_engineering.R"))  # compute_matchup_features for simulate_bracket

# Which season to predict (use seeds from this year)
PREDICT_SEASON <- 2026L

#' Read CSV if file exists, else NULL
read_optional_csv <- function(path) {
  if (file.exists(path)) read_csv(path, show_col_types = FALSE) else NULL
}

#' Build audit of which features used defaults (missing data) per game
#' @return Tibble with slot, round, team_a, team_b, team_a_missing, team_b_missing, prob_a
build_prediction_defaults_audit <- function(game_results, season,
                                            win_pct, points_stats, kenpom_stats,
                                            late_win_pct, recent_win_pct, recent_mov,
                                            sos_stats, rest_stats, conf_tourney_stats, conference_stats,
                                            quadrant_stats, resume_stats, home_away_stats,
                                            first_four_stats, fte_ratings, evanmiya_metrics,
                                            shooting_style_metrics, tourney_location_metrics) {
  has_data <- function(df, sid, tid) {
    if (is.null(df) || nrow(df) == 0) return(FALSE)
    if (!"TeamID" %in% names(df)) return(FALSE)
    nrow(df %>% filter(Season == sid, TeamID == tid)) > 0
  }
  game_results %>%
    rowwise() %>%
    mutate(
      team_a_missing = paste(c(
        if (!has_data(win_pct, season, team_a)) "win_pct",
        if (!has_data(points_stats, season, team_a)) "points_stats",
        if (!has_data(kenpom_stats, season, team_a)) "kenpom",
        if (!has_data(late_win_pct, season, team_a)) "late_win_pct",
        if (!has_data(recent_win_pct, season, team_a)) "recent_win_pct",
        if (!has_data(recent_mov, season, team_a)) "recent_mov",
        if (!has_data(sos_stats, season, team_a)) "sos",
        if (!has_data(rest_stats, season, team_a)) "rest",
        if (!has_data(conf_tourney_stats, season, team_a)) "conf_tourney",
        if (!has_data(conference_stats, season, team_a)) "conference",
        if (!has_data(quadrant_stats, season, team_a)) "quadrant",
        if (!has_data(resume_stats, season, team_a)) "resume",
        if (!has_data(home_away_stats, season, team_a)) "home_away",
        if (!has_data(first_four_stats, season, team_a)) "first_four",
        if (!has_data(fte_ratings, season, team_a)) "538_rating",
        if (!has_data(evanmiya_metrics, season, team_a)) "evanmiya",
        if (!has_data(shooting_style_metrics, season, team_a)) "shooting",
        if (!has_data(tourney_location_metrics, season, team_a)) "location"
      ), collapse = ","),
      team_b_missing = paste(c(
        if (!has_data(win_pct, season, team_b)) "win_pct",
        if (!has_data(points_stats, season, team_b)) "points_stats",
        if (!has_data(kenpom_stats, season, team_b)) "kenpom",
        if (!has_data(late_win_pct, season, team_b)) "late_win_pct",
        if (!has_data(recent_win_pct, season, team_b)) "recent_win_pct",
        if (!has_data(recent_mov, season, team_b)) "recent_mov",
        if (!has_data(sos_stats, season, team_b)) "sos",
        if (!has_data(rest_stats, season, team_b)) "rest",
        if (!has_data(conf_tourney_stats, season, team_b)) "conf_tourney",
        if (!has_data(conference_stats, season, team_b)) "conference",
        if (!has_data(quadrant_stats, season, team_b)) "quadrant",
        if (!has_data(resume_stats, season, team_b)) "resume",
        if (!has_data(home_away_stats, season, team_b)) "home_away",
        if (!has_data(first_four_stats, season, team_b)) "first_four",
        if (!has_data(fte_ratings, season, team_b)) "538_rating",
        if (!has_data(evanmiya_metrics, season, team_b)) "evanmiya",
        if (!has_data(shooting_style_metrics, season, team_b)) "shooting",
        if (!has_data(tourney_location_metrics, season, team_b)) "location"
      ), collapse = ",")
    ) %>%
    ungroup() %>%
    select(slot, round, team_a, team_b, team_a_missing, team_b_missing, prob_a)
}

#' Load model and processed data
#' @param seeds_file Optional path to seeds CSV (Season, Seed, TeamID). If NULL, use tourney_seeds.csv.
load_for_prediction <- function(seeds_file = NULL) {
  model_path <- file.path(MODELS_DIR, "bracket_model.rds")
  if (!file.exists(model_path)) {
    stop("Model not found. Run 03_train_model.R first.")
  }

  model <- readRDS(model_path)
  source(here("src", "utils", "kenpom_utils.R"), local = TRUE)

  win_pct <- read_csv(file.path(PROC_DIR, "win_pct.csv"), show_col_types = FALSE)
  points_stats <- read_csv(file.path(PROC_DIR, "points_stats.csv"), show_col_types = FALSE)
  late_win_pct <- read_optional_csv(file.path(PROC_DIR, "late_win_pct.csv"))
  recent_win_pct <- read_optional_csv(file.path(PROC_DIR, "recent_win_pct.csv"))
  recent_mov <- read_optional_csv(file.path(PROC_DIR, "recent_mov.csv"))
  seeds <- if (is.null(seeds_file)) {
    read_csv(file.path(PROC_DIR, "tourney_seeds.csv"), show_col_types = FALSE)
  } else {
    if (!file.exists(seeds_file)) stop("Seeds file not found: ", seeds_file)
    read_csv(seeds_file, show_col_types = FALSE)
  }
  slots <- read_csv(file.path(PROC_DIR, "tourney_slots.csv"), show_col_types = FALSE)
  teams <- read_csv(file.path(PROC_DIR, "teams.csv"), show_col_types = FALSE)
  kenpom_stats <- read_optional_csv(file.path(PROC_DIR, "kenpom_stats.csv"))

  # Augment KenPom for prediction seasons not in processed (e.g. 2025)
  # Processed data is built from historical seeds; future seasons need raw KenPom/Barttorvik
  seeds_seasons <- unique(seeds$Season)
  kp_seasons <- if (!is.null(kenpom_stats) && nrow(kenpom_stats) > 0) unique(kenpom_stats$Season) else integer()
  missing_kp_seasons <- setdiff(seeds_seasons, kp_seasons)
  if (length(missing_kp_seasons) > 0) {
    source(here("src", "utils", "kenpom_utils.R"), local = TRUE)
    kp_raw <- load_kenpom_stats(seeds, teams)
    if (nrow(kp_raw) > 0) {
      kp_augment <- kp_raw %>% filter(Season %in% missing_kp_seasons)
      if (nrow(kp_augment) > 0) {
        kenpom_stats <- if (is.null(kenpom_stats) || nrow(kenpom_stats) == 0) {
          kp_augment
        } else {
          bind_rows(kenpom_stats, kp_augment) %>% distinct(Season, TeamID, .keep_all = TRUE)
        }
        message("Augmented KenPom for season(s) ", paste(missing_kp_seasons, collapse = ", "), " from raw data")
      }
    }
  }
  home_away_stats <- read_optional_csv(file.path(PROC_DIR, "home_away_stats.csv"))
  resume_stats <- read_optional_csv(file.path(PROC_DIR, "resume_stats.csv"))
  fte_ratings <- read_optional_csv(file.path(PROC_DIR, "fte_ratings.csv"))
  evanmiya_metrics <- read_optional_csv(file.path(PROC_DIR, "evanmiya_metrics.csv"))
  shooting_style_metrics <- read_optional_csv(file.path(PROC_DIR, "shooting_style_metrics.csv"))
  tourney_location_metrics <- read_optional_csv(file.path(PROC_DIR, "tourney_location_metrics.csv"))
  seed_round_priors <- read_optional_csv(file.path(PROC_DIR, "seed_round_priors.csv"))
  head_to_head <- read_optional_csv(file.path(PROC_DIR, "head_to_head.csv"))
  sos_stats <- read_optional_csv(file.path(PROC_DIR, "sos_stats.csv"))
  rest_stats <- read_optional_csv(file.path(PROC_DIR, "rest_stats.csv"))
  conf_tourney_stats <- read_optional_csv(file.path(PROC_DIR, "conf_tourney_stats.csv"))
  conference_stats <- read_optional_csv(file.path(PROC_DIR, "conference_stats.csv"))
  quadrant_stats <- read_optional_csv(file.path(PROC_DIR, "quadrant_stats.csv"))
  first_four_stats <- read_optional_csv(file.path(PROC_DIR, "first_four_stats.csv"))
  tourney_history_stats <- read_optional_csv(file.path(PROC_DIR, "tourney_history_stats.csv"))
  tourney_h2h <- read_optional_csv(file.path(PROC_DIR, "tourney_h2h.csv"))
  upset_history <- read_optional_csv(file.path(PROC_DIR, "upset_history.csv"))

  # For future/manual seed seasons (e.g. 2026), re-load auxiliary team-level features
  # directly from raw sources when processed files do not include those seasons.
  augment_team_feature <- function(existing_df, fresh_df, label) {
    if (is.null(fresh_df) || nrow(fresh_df) == 0) return(existing_df)
    if (!all(c("Season", "TeamID") %in% names(fresh_df))) return(existing_df)
    have <- if (!is.null(existing_df) && nrow(existing_df) > 0 &&
                  all(c("Season", "TeamID") %in% names(existing_df))) {
      unique(existing_df$Season)
    } else integer()
    need <- setdiff(seeds_seasons, have)
    if (length(need) == 0) return(existing_df)
    to_add <- fresh_df %>% filter(Season %in% need)
    if (nrow(to_add) == 0) return(existing_df)
    out <- if (is.null(existing_df) || nrow(existing_df) == 0) {
      to_add
    } else {
      bind_rows(existing_df, to_add) %>% distinct(Season, TeamID, .keep_all = TRUE)
    }
    message("Augmented ", label, " for season(s) ", paste(sort(unique(to_add$Season)), collapse = ", "),
            " (", nrow(to_add), " rows)")
    out
  }

  report_seed_source_coverage <- function(source_df, label) {
    if (is.null(source_df) || nrow(source_df) == 0 || !all(c("Season", "TeamID") %in% names(source_df))) {
      message("TeamID coverage [", label, "]: source empty or missing Season/TeamID columns")
      return(invisible(NULL))
    }
    for (sid in sort(unique(seeds$Season))) {
      seed_keys <- seeds %>% filter(Season == sid) %>% distinct(Season, TeamID)
      source_keys <- source_df %>% filter(Season == sid) %>% distinct(Season, TeamID)
      covered <- nrow(inner_join(seed_keys, source_keys, by = c("Season", "TeamID")))
      missing <- seed_keys %>% anti_join(source_keys, by = c("Season", "TeamID"))
      message("TeamID coverage [", label, "] season ", sid, ": ", covered, "/", nrow(seed_keys),
              " covered, ", nrow(missing), " missing")
      if (nrow(missing) > 0) {
        missing_named <- missing %>%
          left_join(teams %>% select(TeamID, TeamName), by = "TeamID") %>%
          mutate(team_label = if_else(is.na(TeamName), as.character(TeamID), paste0(TeamName, " (", TeamID, ")")))
        preview <- paste(head(missing_named$team_label, 12), collapse = ", ")
        suffix <- if (nrow(missing_named) > 12) paste0(" ... +", nrow(missing_named) - 12, " more") else ""
        message("  Missing TeamIDs [", label, "] season ", sid, ": ", preview, suffix)
      }
    }
  }

  ensure_seed_defaults <- function(df, defaults, label) {
    keys <- seeds %>% distinct(Season, TeamID)
    if (is.null(df) || nrow(df) == 0) {
      df <- keys
      added <- nrow(keys)
    } else {
      if (!all(c("Season", "TeamID") %in% names(df))) return(df)
      missing_keys <- keys %>% anti_join(df %>% select(Season, TeamID) %>% distinct(),
                                         by = c("Season", "TeamID"))
      added <- nrow(missing_keys)
      if (added > 0) df <- bind_rows(df, missing_keys)
    }
    for (nm in names(defaults)) {
      def <- defaults[[nm]]
      if (!nm %in% names(df)) {
        df[[nm]] <- def
      } else {
        df[[nm]] <- ifelse(is.na(df[[nm]]), def, df[[nm]])
      }
    }
    if (added > 0) {
      message("Filled ", label, " defaults for ", added, " seed-team rows")
    }
    df
  }

  lookup <- build_season_team_lookup(seeds, teams)
  home_away_raw <- load_home_away_win_rates(lookup = lookup)
  resume_raw <- load_resume_stats(lookup = lookup)
  bt_resume_raw <- load_barttorvik_resume_metrics(lookup = lookup)
  fte_raw <- load_fte_ratings(lookup = lookup)
  evan_raw <- load_evanmiya_metrics(lookup = lookup)
  shooting_raw <- load_shooting_style_metrics(lookup = lookup)
  location_raw <- load_tourney_location_metrics(lookup = lookup)
  if (nrow(bt_resume_raw) > 0) {
    resume_raw <- if (nrow(resume_raw) > 0) {
      resume_raw %>% full_join(bt_resume_raw, by = c("Season", "TeamID"))
    } else {
      bt_resume_raw
    }
  }
  conference_raw <- load_conference_strength(lookup = lookup)
  quadrant_raw <- load_quadrant_stats(lookup = lookup)

  # Explicit diagnostics so missing TeamID mappings are visible before defaults fill.
  report_seed_source_coverage(kenpom_stats, "kenpom_stats (processed+augmented)")
  report_seed_source_coverage(home_away_raw, "home_away_raw")
  report_seed_source_coverage(resume_raw, "resume_raw")
  report_seed_source_coverage(fte_raw, "fte_raw")
  report_seed_source_coverage(evan_raw, "evanmiya_raw")
  report_seed_source_coverage(shooting_raw, "shooting_raw")
  report_seed_source_coverage(location_raw, "tourney_location_raw")
  report_seed_source_coverage(conference_raw, "conference_raw")
  report_seed_source_coverage(quadrant_raw, "quadrant_raw")

  home_away_stats <- augment_team_feature(home_away_stats, home_away_raw, "home/away stats")
  resume_stats <- augment_team_feature(resume_stats, resume_raw, "resume stats")
  fte_ratings <- augment_team_feature(fte_ratings, fte_raw, "538 ratings")
  evanmiya_metrics <- augment_team_feature(evanmiya_metrics, evan_raw, "EvanMiya metrics")
  shooting_style_metrics <- augment_team_feature(shooting_style_metrics, shooting_raw, "shooting style metrics")
  if (!is.null(location_raw) && nrow(location_raw) > 0) {
    have_loc_seasons <- if (!is.null(tourney_location_metrics) && nrow(tourney_location_metrics) > 0 &&
                             "Season" %in% names(tourney_location_metrics)) unique(tourney_location_metrics$Season) else integer()
    need_loc <- setdiff(seeds_seasons, have_loc_seasons)
    if (length(need_loc) > 0) {
      loc_add <- location_raw %>% filter(Season %in% need_loc)
      if (nrow(loc_add) > 0) {
        tourney_location_metrics <- if (is.null(tourney_location_metrics) || nrow(tourney_location_metrics) == 0) {
          loc_add
        } else {
          bind_rows(tourney_location_metrics, loc_add) %>%
            distinct(Season, TeamID, round, .keep_all = TRUE)
        }
        message("Augmented tournament location metrics for season(s) ", paste(sort(unique(loc_add$Season)), collapse = ", "),
                " (", nrow(loc_add), " rows)")
      }
    }
  }
  conference_stats <- augment_team_feature(conference_stats, conference_raw, "conference stats")
  quadrant_stats <- augment_team_feature(quadrant_stats, quadrant_raw, "quadrant stats")

  # First Four status can be inferred directly from a/b split seeds for prediction seasons.
  have_ff <- if (!is.null(first_four_stats) && nrow(first_four_stats) > 0 &&
                   "Season" %in% names(first_four_stats)) unique(first_four_stats$Season) else integer()
  need_ff <- setdiff(seeds_seasons, have_ff)
  if (length(need_ff) > 0) {
    ff_from_seeds <- seeds %>%
      filter(Season %in% need_ff) %>%
      distinct(Season, TeamID, Seed) %>%
      mutate(played_first_four = as.integer(grepl("[ab]$", Seed))) %>%
      select(Season, TeamID, played_first_four)
    first_four_stats <- if (is.null(first_four_stats) || nrow(first_four_stats) == 0) {
      ff_from_seeds
    } else {
      bind_rows(first_four_stats, ff_from_seeds) %>% distinct(Season, TeamID, .keep_all = TRUE)
    }
    message("Augmented first_four stats from seeds for season(s) ", paste(sort(need_ff), collapse = ", "),
            " (", nrow(ff_from_seeds), " rows)")
  }

  # Ensure every seeded team has explicit rows in team-level feature tables.
  # This does not change model behavior vs current fallback defaults; it prevents
  # silent sparsity and keeps defaults-audit focused on true data gaps.
  win_pct <- ensure_seed_defaults(win_pct, list(WinPct = 0.5, Wins = 0L, Losses = 0L, Games = 0L), "win_pct")
  points_stats <- ensure_seed_defaults(points_stats, list(PF_per_game = 70, PA_per_game = 70), "points_stats")
  kenpom_stats <- ensure_seed_defaults(kenpom_stats, list(
    adj_em = 0, adj_o = 100, adj_d = 100, adj_t = 68, luck = 0, win_pct = 0.5, Wins = 0L, Losses = 0L, Games = 0L
  ), "kenpom")
  late_win_pct <- ensure_seed_defaults(late_win_pct, list(LateWinPct = 0.5, LateWins = 0L, LateLosses = 0L, LateGames = 0L), "late_win_pct")
  recent_win_pct <- ensure_seed_defaults(recent_win_pct, list(RecentWinPct = 0.5, RecentWins = 0L, RecentLosses = 0L, RecentGames = 0L), "recent_win_pct")
  recent_mov <- ensure_seed_defaults(recent_mov, list(RecentMOV = 0), "recent_mov")
  sos_stats <- ensure_seed_defaults(sos_stats, list(sos = 0.5), "sos")
  rest_stats <- ensure_seed_defaults(rest_stats, list(days_rest = 0L), "rest")
  conf_tourney_stats <- ensure_seed_defaults(conf_tourney_stats, list(
    conf_tourney_games = 0L,
    conf_tourney_wins = 0L,
    conf_tourney_depth = 0L
  ), "conf_tourney")
  conference_stats <- ensure_seed_defaults(conference_stats, list(conf_em = 0), "conference")
  quadrant_stats <- ensure_seed_defaults(quadrant_stats, list(quad1_winpct = 0.5, quad12_winpct = 0.5), "quadrant")
  home_away_stats <- ensure_seed_defaults(home_away_stats, list(home_win_rate = 0.5, away_win_rate = 0.5), "home_away")
  resume_stats <- ensure_seed_defaults(resume_stats, list(elo = 0, net = 200, wab = 200, barthag = 0.5, elite_sos = 0), "resume")
  fte_ratings <- ensure_seed_defaults(fte_ratings, list(fte_power_rating = 0), "538_rating")
  evanmiya_metrics <- ensure_seed_defaults(evanmiya_metrics, list(injury_rank = 180, roster_rank = 180, evan_killshots_margin = 0), "evanmiya")
  shooting_style_metrics <- ensure_seed_defaults(shooting_style_metrics, list(
    threes_share = 35, threes_d_share = 35, close_twos_share = 35, close_twos_d_share = 35
  ), "shooting")
  first_four_stats <- ensure_seed_defaults(first_four_stats, list(played_first_four = 0L), "first_four")

  # Compute historical tournament features for prediction seasons not in saved data (e.g. 2026)
  tourney_results_path <- file.path(RAW_EXTENDED_DIR, "MNCAATourneyCompactResults.csv")
  if (!file.exists(tourney_results_path)) tourney_results_path <- file.path(RAW_DIR, "MNCAATourneyCompactResults.csv")
  pred_seasons <- setdiff(seeds_seasons, if (!is.null(tourney_history_stats) && nrow(tourney_history_stats) > 0) unique(tourney_history_stats$Season) else integer())
  if (length(pred_seasons) > 0 && file.exists(tourney_results_path)) {
    tourney_results <- read_csv(tourney_results_path, show_col_types = FALSE)
    if (!"WTeamID" %in% names(tourney_results)) {
      idx <- grep("W.*Team|Winner|Wteam", names(tourney_results), ignore.case = TRUE)
      if (length(idx) >= 1) names(tourney_results)[idx[1]] <- "WTeamID"
    }
    if (!"LTeamID" %in% names(tourney_results)) {
      idx <- grep("L.*Team|Loser|Lteam", names(tourney_results), ignore.case = TRUE)
      if (length(idx) >= 1) names(tourney_results)[idx[1]] <- "LTeamID"
    }
    predict_seeds <- seeds %>% filter(Season %in% pred_seasons) %>% select(Season, TeamID)
    th_pred <- compute_tourney_history_stats(tourney_results, seeds, n_years = 5L, predict_seeds = predict_seeds)
    uh_pred <- compute_upset_history(tourney_results, seeds, n_years = 5L, predict_seeds = predict_seeds)
    tourney_history_stats <- if (is.null(tourney_history_stats) || nrow(tourney_history_stats) == 0) {
      th_pred
    } else {
      bind_rows(tourney_history_stats, th_pred %>% filter(Season %in% pred_seasons)) %>%
        distinct(Season, TeamID, .keep_all = TRUE)
    }
    upset_history <- if (is.null(upset_history) || nrow(upset_history) == 0) {
      uh_pred
    } else {
      bind_rows(upset_history, uh_pred %>% filter(Season %in% pred_seasons)) %>%
        distinct(Season, TeamID, .keep_all = TRUE)
    }
    if (nrow(th_pred) > 0) message("Computed tourney history for prediction season(s) ", paste(pred_seasons, collapse = ", "))
  }

  # Fill missing win_pct from KenPom for seasons not in regular-season data (e.g. 2025)
  # Uses kenpom_stats (already augmented above if prediction season was missing)
  seeds_needed <- seeds %>% distinct(Season, TeamID)
  win_pct_have <- win_pct %>% distinct(Season, TeamID)
  missing <- seeds_needed %>% anti_join(win_pct_have, by = c("Season", "TeamID"))
  if (nrow(missing) > 0 && !is.null(kenpom_stats) && nrow(kenpom_stats) > 0 && "win_pct" %in% names(kenpom_stats)) {
    kp_win <- kenpom_stats %>%
      filter(!is.na(win_pct), (Season %in% missing$Season)) %>%
      mutate(
        Wins = if ("Wins" %in% names(.)) Wins else round(win_pct * 32),
        Losses = if ("Losses" %in% names(.)) Losses else round((1 - win_pct) * 32),
        Games = if ("Games" %in% names(.)) Games else pmax(1L, as.integer(Wins + Losses))
      ) %>%
      select(Season, TeamID, WinPct = win_pct, Wins, Losses, Games)
    to_add <- kp_win %>% inner_join(missing, by = c("Season", "TeamID"))
    if (nrow(to_add) > 0) {
      win_pct <- bind_rows(win_pct, to_add)
      message("Filled ", nrow(to_add), " win_pct rows from KenPom for missing season(s)")
    }
  }

  list(
    model = model,
    win_pct = win_pct,
    points_stats = points_stats,
    late_win_pct = late_win_pct,
    recent_win_pct = recent_win_pct,
    recent_mov = recent_mov,
    seeds = seeds,
    slots = slots,
    teams = teams,
    kenpom_stats = kenpom_stats,
    home_away_stats = home_away_stats,
    resume_stats = resume_stats,
    fte_ratings = fte_ratings,
    evanmiya_metrics = evanmiya_metrics,
    shooting_style_metrics = shooting_style_metrics,
    tourney_location_metrics = tourney_location_metrics,
    seed_round_priors = seed_round_priors,
    head_to_head = head_to_head,
    sos_stats = sos_stats,
    rest_stats = rest_stats,
    conf_tourney_stats = conf_tourney_stats,
    conference_stats = conference_stats,
    quadrant_stats = quadrant_stats,
    first_four_stats = first_four_stats,
    tourney_history_stats = tourney_history_stats,
    tourney_h2h = tourney_h2h,
    upset_history = upset_history
  )
}

#' Run bracket simulation
#' @param season Season year
#' @param seeds_file Optional path to seeds CSV for projected bracket. If NULL, use tourney_seeds.csv.
#' @param use_projected_output If TRUE, write to bracket_prediction_projected_YEAR.csv
#' @param deterministic If TRUE, pick higher-probability team; if FALSE, sample
# Manual seeds override pattern: seeds_68team_<season>.csv
default_manual_seeds_path <- function(season) {
  file.path(BRACKET_DIR, paste0("seeds_68team_", as.integer(season), ".csv"))
}

#' Run Monte Carlo bracket simulation and save slot/champion odds
#' @param n_sims Number of bracket simulations
#' @param seed RNG seed for reproducibility
run_monte_carlo <- function(data, season, seeds_season, slots_season, lookup,
                            n_sims = 1000L, seed = 2026L, use_projected_output = FALSE,
                            use_seed_round_priors = FALSE) {
  source(here("src", "utils", "bracket_logic.R"), local = TRUE)
  format_seconds <- function(x) {
    x <- as.integer(max(0, round(as.numeric(x))))
    h <- x %/% 3600
    m <- (x %% 3600) %/% 60
    s <- x %% 60
    if (h > 0) {
      sprintf("%dh %02dm %02ds", h, m, s)
    } else {
      sprintf("%dm %02ds", m, s)
    }
  }
  n_workers_env <- suppressWarnings(as.integer(Sys.getenv("MONTE_CARLO_WORKERS", unset = "0")))
  detected_cores <- suppressWarnings(parallel::detectCores(logical = FALSE))
  if (is.na(detected_cores) || detected_cores < 1L) detected_cores <- 1L
  n_workers_auto <- max(1L, detected_cores - 1L)
  n_workers <- if (is.na(n_workers_env) || n_workers_env <= 0L) n_workers_auto else n_workers_env
  n_workers <- max(1L, min(as.integer(n_workers), as.integer(n_sims)))

  simulate_one <- function(sim_id) {
    sim <- simulate_bracket(
      season = season,
      slots_df = slots_season,
      seeds_df = seeds_season,
      model = data$model,
      win_pct = data$win_pct,
      points_stats = data$points_stats,
      kenpom_stats = data$kenpom_stats,
      late_win_pct = data$late_win_pct,
      recent_win_pct = data$recent_win_pct,
      recent_mov = data$recent_mov,
      home_away_stats = data$home_away_stats,
      resume_stats = data$resume_stats,
      fte_ratings = data$fte_ratings,
      evanmiya_metrics = data$evanmiya_metrics,
      shooting_style_metrics = data$shooting_style_metrics,
      tourney_location_metrics = data$tourney_location_metrics,
      seed_round_priors = data$seed_round_priors,
      head_to_head = data$head_to_head,
      sos_stats = data$sos_stats,
      rest_stats = data$rest_stats,
      conf_tourney_stats = data$conf_tourney_stats,
      conference_stats = data$conference_stats,
      quadrant_stats = data$quadrant_stats,
      first_four_stats = data$first_four_stats,
      tourney_history_stats = data$tourney_history_stats,
      tourney_h2h = data$tourney_h2h,
      upset_history = data$upset_history,
      deterministic = FALSE,
      use_seed_round_priors = use_seed_round_priors
    )
    list(
      game_results = sim$game_results %>% mutate(sim_id = sim_id),
      champion = sim$champion
    )
  }

  message("Monte Carlo workers: ", n_workers, " (set MONTE_CARLO_WORKERS to override)")
  t_start <- Sys.time()
  if (n_workers <= 1L) {
    set.seed(seed)
    sim_out <- vector("list", n_sims)
    for (i in seq_len(n_sims)) {
      sim_out[[i]] <- simulate_one(i)
      if (i %% 250 == 0 || i == n_sims) {
        elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
        sims_per_sec <- i / max(elapsed, 1e-6)
        eta <- (n_sims - i) / max(sims_per_sec, 1e-6)
        message("  MC sim ", i, " / ", n_sims,
                " | elapsed ", format_seconds(elapsed),
                " | eta ", format_seconds(eta))
      }
    }
  } else {
    project_root <- here::here()
    sim_ids <- seq_len(n_sims)
    n_chunks <- min(n_sims, n_workers * 2L)
    sim_chunks <- split(sim_ids, cut(sim_ids, breaks = n_chunks, labels = FALSE))
    message("Monte Carlo chunks: ", length(sim_chunks))

    cl <- parallel::makeCluster(n_workers)
    on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
    parallel::clusterSetRNGStream(cl, iseed = seed)
    parallel::clusterExport(
      cl,
      varlist = c("project_root", "season", "slots_season", "seeds_season", "data", "use_seed_round_priors"),
      envir = environment()
    )
    parallel::clusterEvalQ(cl, {
      library(dplyr)
      library(tidyr)
      # Required for predict.workflow / tidymodels model objects in workers.
      library(tidymodels)
      source(file.path(project_root, "src", "utils", "feature_engineering.R"))
      source(file.path(project_root, "src", "utils", "bracket_logic.R"))
      NULL
    })
    parallel::clusterExport(cl, varlist = c("simulate_one"), envir = environment())
    chunk_results <- vector("list", length(sim_chunks))
    completed_sims <- 0L
    for (batch_start in seq.int(1L, length(sim_chunks), by = n_workers)) {
      batch_end <- min(length(sim_chunks), batch_start + n_workers - 1L)
      batch_idx <- seq.int(batch_start, batch_end)
      batch_chunks <- sim_chunks[batch_idx]
      batch_results <- parallel::parLapply(cl, batch_chunks, function(ids) {
        lapply(ids, simulate_one)
      })
      chunk_results[batch_idx] <- batch_results
      completed_sims <- completed_sims + sum(vapply(batch_chunks, length, integer(1)))
      elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
      sims_per_sec <- completed_sims / max(elapsed, 1e-6)
      eta <- (n_sims - completed_sims) / max(sims_per_sec, 1e-6)
      message("  MC progress ", completed_sims, " / ", n_sims,
              " sims | elapsed ", format_seconds(elapsed),
              " | eta ", format_seconds(eta))
    }
    sim_out <- unlist(chunk_results, recursive = FALSE)
  }

  sims <- lapply(sim_out, `[[`, "game_results")
  champs <- as.integer(vapply(sim_out, function(x) x$champion, integer(1)))
  sim_games <- bind_rows(sims)
  slot_odds <- sim_games %>%
    count(slot, round, team_id = winner, name = "wins") %>%
    mutate(
      win_rate = wins / n_sims,
      team_name = lookup[as.character(team_id)]
    ) %>%
    arrange(round, slot, desc(win_rate))
  champion_odds <- tibble(team_id = champs) %>%
    count(team_id, name = "titles") %>%
    mutate(
      title_rate = titles / n_sims,
      team_name = lookup[as.character(team_id)]
    ) %>%
    arrange(desc(title_rate), desc(titles))

  mc_base <- if (use_projected_output) paste0("bracket_prediction_projected_monte_carlo_", season) else paste0("bracket_prediction_monte_carlo_", season)
  slot_file <- file.path(OUTPUT_DIR, paste0(mc_base, ".csv"))
  champ_file <- file.path(OUTPUT_DIR, paste0("champion_monte_carlo_", season, ".csv"))
  write_csv(slot_odds, slot_file)
  write_csv(champion_odds, champ_file)
  list(
    slot_odds_file = slot_file,
    champion_odds_file = champ_file,
    champion_odds = champion_odds,
    slot_odds = slot_odds
  )
}

main <- function(season = PREDICT_SEASON, seeds_file = NULL, use_projected_output = FALSE, deterministic = TRUE,
                 run_monte_carlo_output = TRUE, monte_carlo_sims = 1000L, monte_carlo_seed = 2026L,
                 bracket_strategy = BRACKET_STRATEGY, use_seed_round_priors = BRACKET_USE_SEED_PRIORS) {
  if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

  # Use manual seeds when available for prediction season and no seeds_file specified.
  # This supports future seasons (e.g., 2026) without needing hardcoded yearly logic.
  auto_seeds_path <- default_manual_seeds_path(season)
  if (is.null(seeds_file) && file.exists(auto_seeds_path)) {
    seeds_file <- auto_seeds_path
    message("Using manual seeds: ", auto_seeds_path)
  }

  message("Loading model and data...")
  data <- load_for_prediction(seeds_file = seeds_file)

  # Filter seeds and slots for the season
  # Some datasets have slots per season; if not, use all slots
  seeds_season <- data$seeds %>% filter(Season == season)
  if (nrow(seeds_season) == 0) {
    available <- sort(unique(data$seeds$Season))
    expected_manual <- default_manual_seeds_path(season)
    err_msg <- paste0(
      "No seeds found for requested season ", season, ".\n",
      "Run halted to avoid using the wrong bracket season.\n",
      "Expected manual seeds file (if using projected/current bracket): ", expected_manual, "\n",
      "Available seasons in loaded seeds data: ", paste(available, collapse = ", ")
    )
    message(err_msg)
    stop(err_msg, call. = FALSE)
  }

  # Build season-specific bracket slots (supports 68-team First Four templates)
  source(here('src', 'utils', 'bracket_slots.R'), local = TRUE)
  slots_season <- get_slots_for_season(season, data$slots)

  message("Simulating deterministic baseline bracket for season ", season, "...")
  source(here("src", "utils", "bracket_logic.R"), local = TRUE)

  deterministic_result <- simulate_bracket(
    season = season,
    slots_df = slots_season,
    seeds_df = seeds_season,
    model = data$model,
    win_pct = data$win_pct,
    points_stats = data$points_stats,
    kenpom_stats = data$kenpom_stats,
    late_win_pct = data$late_win_pct,
    recent_win_pct = data$recent_win_pct,
    recent_mov = data$recent_mov,
    home_away_stats = data$home_away_stats,
    resume_stats = data$resume_stats,
    fte_ratings = data$fte_ratings,
    evanmiya_metrics = data$evanmiya_metrics,
    shooting_style_metrics = data$shooting_style_metrics,
    tourney_location_metrics = data$tourney_location_metrics,
    seed_round_priors = data$seed_round_priors,
    head_to_head = data$head_to_head,
    sos_stats = data$sos_stats,
    rest_stats = data$rest_stats,
    conf_tourney_stats = data$conf_tourney_stats,
    conference_stats = data$conference_stats,
    quadrant_stats = data$quadrant_stats,
    first_four_stats = data$first_four_stats,
    tourney_history_stats = data$tourney_history_stats,
    tourney_h2h = data$tourney_h2h,
    upset_history = data$upset_history,
    deterministic = deterministic,
    use_seed_round_priors = use_seed_round_priors
  )

  # Add team names to results (handle TeamName or Name column)
  team_name_col <- intersect(names(data$teams), c("TeamName", "Team_Name", "Name"))[1]
  if (is.na(team_name_col)) team_name_col <- names(data$teams)[2]

  lookup <- setNames(data$teams[[team_name_col]], data$teams$TeamID)
  # Fallback names from bracket reference (useful when manual seed TeamIDs are not
  # present in processed teams.csv but are valid for a prediction season).
  ref_path <- file.path(BRACKET_DIR, paste0("bracket_reference_", season, ".csv"))
  if (file.exists(ref_path) && all(c("Seed", "TeamName") %in% names(read_csv(ref_path, n_max = 1, show_col_types = FALSE)))) {
    ref <- read_csv(ref_path, show_col_types = FALSE) %>%
      select(Seed, TeamName) %>%
      distinct(Seed, .keep_all = TRUE)
    ref_lookup <- seeds_season %>%
      select(TeamID, Seed) %>%
      left_join(ref, by = "Seed") %>%
      filter(!is.na(TeamName)) %>%
      mutate(TeamID_chr = as.character(TeamID))
    if (nrow(ref_lookup) > 0) {
      needs <- is.na(lookup[ref_lookup$TeamID_chr])
      if (any(needs)) {
        fill_ids <- ref_lookup$TeamID_chr[needs]
        fill_names <- ref_lookup$TeamName[needs]
        lookup[fill_ids] <- fill_names
        message("Filled ", sum(needs), " team names from ", basename(ref_path))
      }
    }
  }

  message("Deterministic champion: ", lookup[as.character(deterministic_result$champion)],
          " (TeamID ", deterministic_result$champion, ")")

  mc_out <- NULL
  if (isTRUE(run_monte_carlo_output) && monte_carlo_sims > 0) {
    message("Running Monte Carlo simulations (n=", monte_carlo_sims, ", seed=", monte_carlo_seed, ")...")
    mc_out <- run_monte_carlo(
      data = data,
      season = season,
      seeds_season = seeds_season,
      slots_season = slots_season,
      lookup = lookup,
      n_sims = as.integer(monte_carlo_sims),
      seed = as.integer(monte_carlo_seed),
      use_projected_output = use_projected_output,
      use_seed_round_priors = use_seed_round_priors
    )
    top_champ <- mc_out$champion_odds %>% slice(1)
    message("Monte Carlo slot odds saved to ", mc_out$slot_odds_file)
    message("Monte Carlo champion odds saved to ", mc_out$champion_odds_file)
    if (nrow(top_champ) > 0) {
      message("Monte Carlo top champion: ", top_champ$team_name[1], " (", round(100 * top_champ$title_rate[1], 1), "%)")
    }
  }

  result <- deterministic_result
  if (identical(bracket_strategy, "monte_carlo_optimal")) {
    if (!is.null(mc_out) && !is.null(mc_out$slot_odds) && nrow(mc_out$slot_odds) > 0) {
      result <- select_optimal_bracket(
        season = season,
        slots_df = slots_season,
        seeds_df = seeds_season,
        slot_odds = mc_out$slot_odds
      )
      message("Selected final bracket via Monte Carlo expected-points optimizer.")
    } else {
      warning("bracket_strategy='monte_carlo_optimal' requested but Monte Carlo odds are unavailable; falling back to deterministic bracket.")
    }
  } else if (!identical(bracket_strategy, "deterministic")) {
    warning("Unknown BRACKET_STRATEGY='", bracket_strategy, "'. Falling back to deterministic.")
  }

  game_results <- result$game_results %>%
    mutate(
      team_a_name = lookup[as.character(team_a)],
      team_b_name = lookup[as.character(team_b)],
      winner_name = lookup[as.character(winner)]
    )
  champ_name <- lookup[as.character(result$champion)]
  message("Final strategy (", bracket_strategy, ") champion: ", champ_name, " (TeamID ", result$champion, ")")

  # Save deterministic baseline for side-by-side strategy comparison.
  deterministic_results_named <- deterministic_result$game_results %>%
    mutate(
      team_a_name = lookup[as.character(team_a)],
      team_b_name = lookup[as.character(team_b)],
      winner_name = lookup[as.character(winner)]
    )
  det_base <- if (use_projected_output) {
    paste0("bracket_prediction_projected_deterministic_", season)
  } else {
    paste0("bracket_prediction_deterministic_", season)
  }
  write_csv(deterministic_results_named, file.path(OUTPUT_DIR, paste0(det_base, ".csv")))

  # Audit: which teams used default values (missing data) per game
  defaults_audit <- build_prediction_defaults_audit(
    game_results = result$game_results,
    season = season,
    win_pct = data$win_pct,
    points_stats = data$points_stats,
    kenpom_stats = data$kenpom_stats,
    late_win_pct = data$late_win_pct,
    recent_win_pct = data$recent_win_pct,
    recent_mov = data$recent_mov,
    sos_stats = data$sos_stats,
    rest_stats = data$rest_stats,
    conf_tourney_stats = data$conf_tourney_stats,
    conference_stats = data$conference_stats,
    quadrant_stats = data$quadrant_stats,
    resume_stats = data$resume_stats,
    home_away_stats = data$home_away_stats,
    first_four_stats = data$first_four_stats,
    fte_ratings = data$fte_ratings,
    evanmiya_metrics = data$evanmiya_metrics,
    shooting_style_metrics = data$shooting_style_metrics,
    tourney_location_metrics = data$tourney_location_metrics
  ) %>%
    mutate(
      team_a_name = lookup[as.character(team_a)],
      team_b_name = lookup[as.character(team_b)]
    )
  defaults_base <- if (use_projected_output) paste0("prediction_defaults_projected_", season) else paste0("prediction_defaults_", season)
  defaults_file <- file.path(OUTPUT_DIR, paste0(defaults_base, ".csv"))
  write_csv(defaults_audit, defaults_file)
  message("Prediction defaults audit saved to ", defaults_file)

  # Save outputs
  out_base <- if (use_projected_output) paste0("bracket_prediction_projected_", season) else paste0("bracket_prediction_", season)
  out_file <- file.path(OUTPUT_DIR, paste0(out_base, ".csv"))
  write_csv(game_results, out_file)
  message("Bracket predictions saved to ", out_file)

  # Save champion
  champ_base <- if (use_projected_output) paste0("champion_projected_", season) else paste0("champion_", season)
  champ_file <- file.path(OUTPUT_DIR, paste0(champ_base, ".txt"))
  writeLines(c(
    paste("Season:", season),
    paste("Strategy:", bracket_strategy),
    paste("Predicted Champion:", champ_name),
    paste("TeamID:", result$champion)
  ), champ_file)

  invisible(list(
    champion = result$champion,
    champion_name = champ_name,
    game_results = game_results,
    season = season,
    monte_carlo = mc_out,
    strategy = bracket_strategy,
    deterministic_champion = deterministic_result$champion
  ))
}

if (!isTRUE(getOption("bracket.skip_main"))) {
  main()
}
