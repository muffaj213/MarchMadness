library(here)
library(readr)
library(dplyr)
library(tidymodels)

source(here("src", "config.R"))
source(here("src", "utils", "feature_engineering.R"))
source(here("src", "utils", "bracket_logic.R"))
source(here("src", "utils", "bracket_slots.R"))
source(here("src", "utils", "team_id_consolidation.R"))

BACKTEST_SEASONS <- c(2018L, 2019L, 2021L, 2022L, 2023L, 2024L)
ROUND_POINTS <- c(`1` = 10L, `2` = 20L, `3` = 40L, `4` = 80L, `5` = 160L, `6` = 320L)
LEAKAGE_GUARD <- TRUE
LEAKAGE_GUARD_SEASONS <- c(2024L)
LEAKAGE_MIN_POINT_DROP <- 200L
LEAKAGE_MIN_R1_DROP <- 4L
USE_TOURNEY_LOCATION_FEATURES <- FALSE
USE_NISHAA_FEATURES <- FALSE
USE_MINIMAL_FEATURE_SET <- TRUE
MINIMAL_FEATURE_COLS <- c(
  "round", "seed_diff", "seed_diff_sq", "seed_sum",
  "is_upset_matchup", "upset_seed_gap", "round_seed_interaction"
)
FEATURE_PROFILE <- tolower(trimws(Sys.getenv("ROLLING_FEATURE_PROFILE", unset = "minimal")))

read_tourney_results <- function() {
  path_ext <- file.path(RAW_EXTENDED_DIR, "MNCAATourneyCompactResults.csv")
  path_raw <- file.path(RAW_DIR, "MNCAATourneyCompactResults.csv")
  path <- if (file.exists(path_ext)) path_ext else path_raw
  if (!file.exists(path)) stop("Tournament results not found in raw or raw_extended data.")
  out <- read_csv(path, show_col_types = FALSE)
  remap <- load_consolidation_map()
  apply_remap(out, remap, c("WTeamID", "LTeamID"))
}

actual_round_winners <- function(tourney_results, season) {
  season_games <- tourney_results %>% filter(Season == season)
  if (nrow(season_games) == 0) {
    return(tibble(round = integer(), winner = integer()))
  }

  day_counts <- season_games %>%
    count(DayNum, name = "n_games") %>%
    arrange(DayNum) %>%
    mutate(order_idx = row_number())

  has_playin_day <- nrow(day_counts) >= 7 &&
    day_counts$n_games[1] <= 4 &&
    day_counts$n_games[2] >= 30

  day_map <- if (has_playin_day) {
    day_counts %>% mutate(round = order_idx - 1L)
  } else {
    day_counts %>% mutate(round = order_idx)
  }

  season_games %>%
    left_join(day_map %>% select(DayNum, round), by = "DayNum") %>%
    transmute(round = as.integer(round), winner = as.integer(WTeamID)) %>%
    filter(round >= 1L, round <= 6L) %>%
    distinct(round, winner)
}

slot_round <- function(slot) {
  r <- suppressWarnings(as.integer(sub("^R([0-9]+).*", "\\1", slot)))
  ifelse(is.na(r), 0L, r)
}

infer_day_round_map <- function(season_games) {
  day_counts <- season_games %>%
    count(DayNum, name = "n_games") %>%
    arrange(DayNum)
  total_games <- sum(day_counts$n_games)

  has_playin <- total_games >= 66L
  cum_games <- cumsum(day_counts$n_games)
  boundaries <- if (has_playin) c(4L, 36L, 52L, 60L, 64L, 66L, 67L) else c(32L, 48L, 56L, 60L, 62L, 63L)
  rounds <- if (has_playin) c(0L, 1L, 2L, 3L, 4L, 5L, 6L) else c(1L, 2L, 3L, 4L, 5L, 6L)
  out_rounds <- vapply(cum_games, function(x) rounds[which(x <= boundaries)[1]], integer(1))
  day_counts %>% transmute(DayNum, round = as.integer(out_rounds))
}

assign_game_rounds <- function(season_games) {
  total_games <- nrow(season_games)
  if (total_games == 0) return(season_games %>% mutate(round = integer()))

  if (total_games >= 66L) {
    playin_games <- total_games - 63L
    sizes <- c(playin_games, 32L, 16L, 8L, 4L, 2L, 1L)
    round_ids <- c(0L, 1L, 2L, 3L, 4L, 5L, 6L)
  } else {
    sizes <- c(32L, 16L, 8L, 4L, 2L, 1L)
    if (total_games == 62L) sizes[1] <- 31L
    sizes[1] <- sizes[1] + (total_games - sum(sizes))
    round_ids <- c(1L, 2L, 3L, 4L, 5L, 6L)
  }

  round_vec <- rep(round_ids, times = sizes)
  round_vec <- round_vec[seq_len(total_games)]

  season_games %>%
    arrange(DayNum) %>%
    mutate(round = as.integer(round_vec))
}

reconcile_seeds_with_round1_results <- function(seeds_season, season_games) {
  games_r1 <- assign_game_rounds(season_games) %>%
    filter(round == 1L) %>%
    transmute(team_a = as.integer(WTeamID), team_b = as.integer(LTeamID))

  if (nrow(games_r1) == 0) return(seeds_season)

  seeds_norm <- seeds_season %>%
    mutate(
      Seed = as.character(Seed),
      region = substr(Seed, 1, 1),
      seed_num = readr::parse_number(Seed),
      TeamID = as.integer(TeamID)
    )

  replace_seed_team <- function(df, seed_label, team_id) {
    idx <- which(df$Seed == seed_label)
    if (length(idx) == 0) return(df)
    df$TeamID[idx[1]] <- as.integer(team_id)
    df
  }

  regions <- sort(unique(seeds_norm$region))
  for (reg in regions) {
    for (s in 1:8) {
      strong_label <- sprintf("%s%02d", reg, s)
      weak_label <- sprintf("%s%02d", reg, 17 - s)
      strong_team <- seeds_norm %>% filter(Seed == strong_label) %>% pull(TeamID)
      if (length(strong_team) == 0 || is.na(strong_team[1])) next
      strong_team <- strong_team[1]

      g <- games_r1 %>%
        filter(team_a == strong_team | team_b == strong_team)
      if (nrow(g) == 0) next
      opp <- ifelse(g$team_a[1] == strong_team, g$team_b[1], g$team_a[1])
      seeds_norm <- replace_seed_team(seeds_norm, weak_label, opp)
    }
  }

  # Keep first row per seed label after reconciliation.
  seeds_norm %>%
    group_by(Season, Seed) %>%
    summarise(TeamID = first(TeamID), .groups = "drop")
}

build_actual_slot_winners <- function(tourney_results, season, seeds_season, slots_season) {
  season_games <- tourney_results %>% filter(Season == season)
  if (nrow(season_games) == 0) return(tibble(slot = character(), round = integer(), winner = integer()))

  games <- assign_game_rounds(season_games) %>%
    mutate(game_id = row_number()) %>%
    mutate(
      team_low = pmin(as.integer(WTeamID), as.integer(LTeamID)),
      team_high = pmax(as.integer(WTeamID), as.integer(LTeamID))
    )

  slots <- slots_season %>%
    transmute(
      Slot = as.character(Slot),
      Strong = as.character(StrongSeed),
      Weak = as.character(WeakSeed)
    ) %>%
    mutate(is_playin = !grepl("^R[0-9]", Slot)) %>%
    arrange(desc(is_playin), Slot)

  seeds_num <- seeds_season %>%
    transmute(Seed = as.character(Seed), TeamID = as.integer(TeamID))

  slot_winners <- list()
  out <- list()
  used_game_ids <- integer()
  for (i in seq_len(nrow(slots))) {
    slot <- slots$Slot[i]
    strong <- slots$Strong[i]
    weak <- slots$Weak[i]
    round_num <- slot_round(slot)

    team_a <- if (!is.null(slot_winners[[strong]])) {
      slot_winners[[strong]]
    } else if (is_seed_ref(strong)) {
      x <- seeds_num %>% filter(Seed == strong) %>% pull(TeamID)
      if (length(x) > 0) x[1] else NA_integer_
    } else {
      NA_integer_
    }
    team_b <- if (!is.null(slot_winners[[weak]])) {
      slot_winners[[weak]]
    } else if (is_seed_ref(weak)) {
      x <- seeds_num %>% filter(Seed == weak) %>% pull(TeamID)
      if (length(x) > 0) x[1] else NA_integer_
    } else {
      NA_integer_
    }
    if (is.na(team_a) || is.na(team_b)) next

    low <- min(team_a, team_b)
    high <- max(team_a, team_b)
    g <- games %>%
      filter(round == round_num, team_low == low, team_high == high, !(game_id %in% used_game_ids))
    if (nrow(g) == 0) {
      # Some seasons have seed-region labeling drift; allow one-team match while
      # preserving round and one-game-per-slot constraints.
      g <- games %>%
        filter(
          round == round_num,
          (team_low == low | team_high == low | team_low == high | team_high == high),
          !(game_id %in% used_game_ids)
        )
    }
    if (nrow(g) == 0) {
      appears_later_a <- any(games$round > round_num & (games$WTeamID == team_a | games$LTeamID == team_a))
      appears_later_b <- any(games$round > round_num & (games$WTeamID == team_b | games$LTeamID == team_b))
      if (isTRUE(appears_later_a) && !isTRUE(appears_later_b)) {
        winner <- as.integer(team_a)
        slot_winners[[slot]] <- winner
        out[[length(out) + 1]] <- tibble(slot = slot, round = round_num, winner = winner, map_type = "bye_inferred")
        next
      }
      if (isTRUE(appears_later_b) && !isTRUE(appears_later_a)) {
        winner <- as.integer(team_b)
        slot_winners[[slot]] <- winner
        out[[length(out) + 1]] <- tibble(slot = slot, round = round_num, winner = winner, map_type = "bye_inferred")
        next
      }
      # Last-resort fill to keep full bracket alignment when historical slot labels
      # are inconsistent with available seeds/results.
      g <- games %>% filter(round == round_num, !(game_id %in% used_game_ids))
      if (nrow(g) == 0) {
        g <- games %>% filter(round == round_num)
        if (nrow(g) == 0) {
          stop("Could not map actual game to slot ", slot, " for season ", season)
        }
        g <- g %>% slice(1)
        winner <- as.integer(g$WTeamID[1])
        slot_winners[[slot]] <- winner
        out[[length(out) + 1]] <- tibble(slot = slot, round = round_num, winner = winner, map_type = "round_reuse")
        next
      }
      g <- g %>% slice(1)
      winner <- as.integer(g$WTeamID[1])
      used_game_ids <- c(used_game_ids, as.integer(g$game_id[1]))
      slot_winners[[slot]] <- winner
      out[[length(out) + 1]] <- tibble(slot = slot, round = round_num, winner = winner, map_type = "round_fill")
      next
    }
    g <- g %>% slice(1)
    winner <- as.integer(g$WTeamID[1])
    used_game_ids <- c(used_game_ids, as.integer(g$game_id[1]))
    slot_winners[[slot]] <- winner
    exact_match <- (g$team_low[1] == low && g$team_high[1] == high)
    out[[length(out) + 1]] <- tibble(
      slot = slot,
      round = round_num,
      winner = winner,
      map_type = ifelse(exact_match, "exact_pair", "one_team_match")
    )
  }
  bind_rows(out)
}

score_bracket_slot_accurate <- function(pred_games, actual_slots) {
  pred <- pred_games %>%
    transmute(slot = as.character(slot), round = as.integer(round), winner = as.integer(winner)) %>%
    distinct(slot, .keep_all = TRUE)

  scored <- actual_slots %>%
    select(slot, round, actual_winner = winner) %>%
    left_join(pred %>% rename(pred_winner = winner), by = c("slot", "round")) %>%
    mutate(
      correct = !is.na(pred_winner) & pred_winner == actual_winner,
      points_per_game = ifelse(round %in% as.integer(names(ROUND_POINTS)),
                               as.integer(ROUND_POINTS[as.character(round)]), 0L),
      points = if_else(correct, points_per_game, 0L)
    )

  round_breakdown <- scored %>%
    filter(round >= 1L, round <= 6L) %>%
    group_by(round) %>%
    summarise(picks = n(), correct = sum(correct), points = sum(points), .groups = "drop") %>%
    arrange(round)

  list(
    round_breakdown = round_breakdown,
    total_correct = sum(round_breakdown$correct),
    total_points = sum(round_breakdown$points)
  )
}

fit_rolling_model <- function(matchup_data, test_season, shuffle_labels = FALSE, shuffle_seed = 42L) {
  train <- matchup_data %>%
    filter(Season < test_season) %>%
    filter(!is.na(outcome) & !is.infinite(outcome))

  if (nrow(train) < 200) stop("Insufficient training rows before season ", test_season)

  feature_cols <- setdiff(names(train), c("Season", "TeamA", "TeamB", "outcome"))
  for (col in feature_cols) {
    bad <- is.na(train[[col]]) | is.infinite(train[[col]])
    if (any(bad)) train[[col]][bad] <- 0
  }

  if (isTRUE(shuffle_labels)) {
    set.seed(as.integer(shuffle_seed))
    train$outcome <- sample(train$outcome)
  }

  train <- train %>%
    mutate(outcome = factor(outcome, levels = c(0, 1), labels = c("Lose", "Win")))

  formula_str <- paste("outcome ~", paste(feature_cols, collapse = " + "))
  rec <- recipe(as.formula(formula_str), data = train) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_predictors())

  spec <- boost_tree(
    mode = "classification",
    trees = 150,
    min_n = 15,
    learn_rate = 0.05,
    tree_depth = 3,
    stop_iter = 10
  ) %>% set_engine("xgboost", validation = 0.15)

  wf <- workflow() %>% add_recipe(rec) %>% add_model(spec)
  fit(wf, data = train)
}

compute_round1_correct <- function(pred_games, actual_slots) {
  actual_slots %>%
    filter(round == 1L) %>%
    select(slot, winner) %>%
    left_join(
      pred_games %>% filter(round == 1L) %>% select(slot, pred_winner = winner),
      by = "slot"
    ) %>%
    summarise(v = sum(winner == pred_winner, na.rm = TRUE)) %>%
    pull(v) %>%
    as.integer()
}

simulate_model_bracket <- function(data, season, model, seeds_override = NULL, slots_override = NULL) {
  seeds_season <- if (is.null(seeds_override)) {
    data$seeds %>% filter(Season == season)
  } else {
    seeds_override
  }
  if (nrow(seeds_season) == 0) stop("No seeds found for season ", season)

  slots_season <- if (is.null(slots_override)) get_slots_for_season(season, data$slots) else slots_override
  simulate_bracket(
    season = season,
    slots_df = slots_season,
    seeds_df = seeds_season,
    model = model,
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
    deterministic = TRUE
  )
}

simulate_chalk_bracket <- function(season, seeds_season, slots_season) {
  seeds_num <- seeds_season %>%
    mutate(SeedNum = readr::parse_number(as.character(Seed))) %>%
    transmute(Seed = as.character(Seed), TeamID = as.integer(TeamID), SeedNum = as.integer(SeedNum))

  slots <- slots_season %>%
    transmute(
      Slot = as.character(Slot),
      Strong = as.character(StrongSeed),
      Weak = as.character(WeakSeed)
    ) %>%
    mutate(is_playin = !grepl("^R[0-9]", Slot)) %>%
    arrange(desc(is_playin), Slot)

  slot_winners <- list()
  out <- list()
  for (i in seq_len(nrow(slots))) {
    slot <- slots$Slot[i]
    strong <- slots$Strong[i]
    weak <- slots$Weak[i]
    round_num <- slot_round(slot)

    team_a <- if (!is.null(slot_winners[[strong]])) {
      slot_winners[[strong]]
    } else if (is_seed_ref(strong)) {
      x <- seeds_num %>% filter(Seed == strong) %>% pull(TeamID)
      if (length(x) > 0) x[1] else NA_integer_
    } else {
      NA_integer_
    }
    team_b <- if (!is.null(slot_winners[[weak]])) {
      slot_winners[[weak]]
    } else if (is_seed_ref(weak)) {
      x <- seeds_num %>% filter(Seed == weak) %>% pull(TeamID)
      if (length(x) > 0) x[1] else NA_integer_
    } else {
      NA_integer_
    }
    if (is.na(team_a) || is.na(team_b)) next

    seed_a <- seeds_num %>% filter(TeamID == team_a) %>% pull(SeedNum)
    seed_b <- seeds_num %>% filter(TeamID == team_b) %>% pull(SeedNum)
    seed_a <- if (length(seed_a) > 0 && !is.na(seed_a[1])) seed_a[1] else 99L
    seed_b <- if (length(seed_b) > 0 && !is.na(seed_b[1])) seed_b[1] else 99L
    winner <- if (seed_a < seed_b) {
      team_a
    } else if (seed_b < seed_a) {
      team_b
    } else {
      min(team_a, team_b)
    }

    slot_winners[[slot]] <- winner
    out[[length(out) + 1]] <- tibble(slot = slot, round = round_num, winner = as.integer(winner))
  }
  list(game_results = bind_rows(out), champion = as.integer(slot_winners[["R6CH"]]))
}

main <- function(seasons = BACKTEST_SEASONS) {
  if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

  options(bracket.skip_main = TRUE)
  source(here("src", "04_predict_bracket.R"), local = TRUE)
  options(bracket.skip_main = NULL)

  data <- load_for_prediction(seeds_file = file.path(PROC_DIR, "tourney_seeds.csv"))
  matchup_data <- read_csv(file.path(PROC_DIR, "matchup_data.csv"), show_col_types = FALSE)
  tourney_results <- read_tourney_results()

  data_orig <- data
  matchup_orig <- matchup_data
  profile <- FEATURE_PROFILE
  valid_profiles <- c("full", "no_nishaa", "minimal", "kenpom_only", "resume_only", "fte_evan_shooting_only")
  if (!(profile %in% valid_profiles)) {
    stop("Unknown ROLLING_FEATURE_PROFILE='", profile, "'. Valid: ", paste(valid_profiles, collapse = ", "))
  }

  nishaa_feature_cols <- intersect(
    names(matchup_data),
    c(
      "conf_em_diff", "quad1_winpct_diff", "quad12_winpct_diff",
      "home_win_rate_diff", "away_win_rate_diff",
      "elo_diff", "net_diff", "wab_diff", "barthag_diff", "elite_sos_diff",
      "fte_power_diff", "injury_rank_diff", "roster_rank_diff", "evan_killshots_margin_diff",
      "three_share_diff", "three_point_mismatch", "close2_share_diff", "close2_point_mismatch",
      "travel_miles_adv", "timezones_adv",
      "adjem_diff", "adj_off_diff", "adj_def_diff", "tempo_diff", "luck_diff", "off_vs_def_adv",
      "adjem_seed_interaction", "seed_barthag_interaction"
    )
  )
  kenpom_cols <- intersect(names(matchup_data), c("adjem_diff", "adj_off_diff", "adj_def_diff", "tempo_diff", "luck_diff", "off_vs_def_adv", "adjem_seed_interaction"))
  resume_cols <- intersect(names(matchup_data), c("home_win_rate_diff", "away_win_rate_diff", "elo_diff", "net_diff", "wab_diff", "barthag_diff", "elite_sos_diff", "conf_em_diff", "quad1_winpct_diff", "quad12_winpct_diff", "seed_barthag_interaction"))
  fte_evan_shooting_cols <- intersect(names(matchup_data), c("fte_power_diff", "injury_rank_diff", "roster_rank_diff", "evan_killshots_margin_diff", "three_share_diff", "three_point_mismatch", "close2_share_diff", "close2_point_mismatch"))

  # Base for ablations: zero all external columns and clear external tables.
  if (profile != "full") {
    if (length(nishaa_feature_cols) > 0) {
      for (col in nishaa_feature_cols) matchup_data[[col]] <- 0
    }
    data$kenpom_stats <- tibble()
    data$home_away_stats <- tibble()
    data$resume_stats <- tibble()
    data$fte_ratings <- tibble()
    data$evanmiya_metrics <- tibble()
    data$shooting_style_metrics <- tibble()
    data$tourney_location_metrics <- tibble()
    data$conference_stats <- tibble()
    data$quadrant_stats <- tibble()
  }

  if (profile == "kenpom_only") {
    if (length(kenpom_cols) > 0) matchup_data[kenpom_cols] <- matchup_orig[kenpom_cols]
    data$kenpom_stats <- data_orig$kenpom_stats
  } else if (profile == "resume_only") {
    if (length(resume_cols) > 0) matchup_data[resume_cols] <- matchup_orig[resume_cols]
    data$home_away_stats <- data_orig$home_away_stats
    data$resume_stats <- data_orig$resume_stats
    data$conference_stats <- data_orig$conference_stats
    data$quadrant_stats <- data_orig$quadrant_stats
  } else if (profile == "fte_evan_shooting_only") {
    if (length(fte_evan_shooting_cols) > 0) matchup_data[fte_evan_shooting_cols] <- matchup_orig[fte_evan_shooting_cols]
    data$fte_ratings <- data_orig$fte_ratings
    data$evanmiya_metrics <- data_orig$evanmiya_metrics
    data$shooting_style_metrics <- data_orig$shooting_style_metrics
  } else if (profile == "minimal") {
    feature_cols_all <- setdiff(names(matchup_data), c("Season", "TeamA", "TeamB", "outcome"))
    zero_cols <- setdiff(feature_cols_all, MINIMAL_FEATURE_COLS)
    if (length(zero_cols) > 0) {
      for (col in zero_cols) matchup_data[[col]] <- 0
    }
  }

  message("Rolling feature profile: ", profile)

  if (!isTRUE(USE_TOURNEY_LOCATION_FEATURES)) {
    # Tournament locations file includes path-dependent round coverage and can
    # leak realized advancement when used in retrospective backtests.
    if ("travel_miles_adv" %in% names(matchup_data)) matchup_data$travel_miles_adv <- 0
    if ("timezones_adv" %in% names(matchup_data)) matchup_data$timezones_adv <- 0
    data$tourney_location_metrics <- tibble()
    message("Location features disabled for rolling backtest (leakage safeguard).")
  }

  by_round_rows <- list()
  season_rows <- list()
  leakage_rows <- list()

  for (season in seasons) {
    message("Rolling fit for season ", season, " (train on seasons <", season, ")")
    rolling_model <- fit_rolling_model(matchup_data, season)
    season_games <- tourney_results %>% filter(Season == season)
    seeds_season <- data$seeds %>%
      filter(Season == season) %>%
      reconcile_seeds_with_round1_results(season_games)
    slots_season <- get_slots_for_season(season, data$slots)
    if (nrow(season_games) <= 63L) {
      slots_season <- slots_season %>% filter(grepl("^R", Slot))
    }
    actual_slots <- build_actual_slot_winners(tourney_results, season, seeds_season, slots_season)
    map_quality <- actual_slots %>%
      count(map_type, name = "n") %>%
      mutate(pct = n / sum(n))

    model_out <- simulate_model_bracket(
      data,
      season,
      rolling_model,
      seeds_override = seeds_season,
      slots_override = slots_season
    )
    chalk_out <- simulate_chalk_bracket(season, seeds_season, slots_season)

    scoring_mode <- "slot_accurate"
    scored_model <- score_bracket_slot_accurate(model_out$game_results, actual_slots)
    scored_chalk <- score_bracket_slot_accurate(chalk_out$game_results, actual_slots)
    max_points <- sum(c(32, 16, 8, 4, 2, 1) * as.integer(ROUND_POINTS[c("1", "2", "3", "4", "5", "6")]))
    r1_model <- compute_round1_correct(model_out$game_results, actual_slots)

    by_round_rows[[length(by_round_rows) + 1]] <- scored_model$round_breakdown %>%
      mutate(Season = season, Method = "model", .before = 1)
    by_round_rows[[length(by_round_rows) + 1]] <- scored_chalk$round_breakdown %>%
      mutate(Season = season, Method = "chalk", .before = 1)

    season_rows[[length(season_rows) + 1]] <- tibble(
      Season = season,
      Method = "model",
      Scoring_Mode = scoring_mode,
      Train_Through = season - 1L,
      Correct_Games = scored_model$total_correct,
      Total_Games = 63L,
      Total_Points = scored_model$total_points,
      Max_Points = max_points,
      Points_Pct = scored_model$total_points / max_points,
      Champion_Pick = model_out$champion
    ) %>% mutate(
      Mapping_Exact_Pct = map_quality %>% filter(map_type == "exact_pair") %>% pull(pct) %>% {if (length(.) == 0) 0 else .[1]},
      Mapping_RoundFill_Pct = map_quality %>% filter(map_type == "round_fill") %>% pull(pct) %>% {if (length(.) == 0) 0 else .[1]}
    )
    season_rows[[length(season_rows) + 1]] <- tibble(
      Season = season,
      Method = "chalk",
      Scoring_Mode = scoring_mode,
      Train_Through = NA_integer_,
      Correct_Games = scored_chalk$total_correct,
      Total_Games = 63L,
      Total_Points = scored_chalk$total_points,
      Max_Points = max_points,
      Points_Pct = scored_chalk$total_points / max_points,
      Champion_Pick = chalk_out$champion
    ) %>% mutate(
      Mapping_Exact_Pct = map_quality %>% filter(map_type == "exact_pair") %>% pull(pct) %>% {if (length(.) == 0) 0 else .[1]},
      Mapping_RoundFill_Pct = map_quality %>% filter(map_type == "round_fill") %>% pull(pct) %>% {if (length(.) == 0) 0 else .[1]}
    )

    if (isTRUE(LEAKAGE_GUARD) && profile != "minimal" && season %in% LEAKAGE_GUARD_SEASONS) {
      message("Leakage guard: shuffled-label control for season ", season)
      shuffled_model <- fit_rolling_model(
        matchup_data,
        season,
        shuffle_labels = TRUE,
        shuffle_seed = 1000L + season
      )
      shuffled_out <- simulate_model_bracket(
        data,
        season,
        shuffled_model,
        seeds_override = seeds_season,
        slots_override = slots_season
      )
      scored_shuffled <- score_bracket_slot_accurate(shuffled_out$game_results, actual_slots)
      r1_shuffled <- compute_round1_correct(shuffled_out$game_results, actual_slots)
      point_drop <- as.integer(scored_model$total_points - scored_shuffled$total_points)
      r1_drop <- as.integer(r1_model - r1_shuffled)

      leakage_rows[[length(leakage_rows) + 1]] <- tibble(
        Season = season,
        Baseline_Points = as.integer(scored_model$total_points),
        Shuffled_Points = as.integer(scored_shuffled$total_points),
        Point_Drop = point_drop,
        Baseline_R1_Correct = r1_model,
        Shuffled_R1_Correct = r1_shuffled,
        R1_Drop = r1_drop
      )

      if (point_drop < LEAKAGE_MIN_POINT_DROP && r1_drop < LEAKAGE_MIN_R1_DROP) {
        stop(
          "Leakage guard FAILED for season ", season, ": shuffled labels were too strong (",
          "point_drop=", point_drop, ", r1_drop=", r1_drop, "). ",
          "Expected at least one of: point_drop >= ", LEAKAGE_MIN_POINT_DROP,
          " or r1_drop >= ", LEAKAGE_MIN_R1_DROP, "."
        )
      }
    }
  }

  by_round <- bind_rows(by_round_rows) %>% arrange(Method, Season, round)
  by_season <- bind_rows(season_rows) %>% arrange(Method, Season)
  overall <- by_season %>%
    group_by(Method) %>%
    summarise(
      Season = "mean",
      Train_Through = NA_integer_,
      Correct_Games = mean(Correct_Games),
      Total_Games = mean(Total_Games),
      Total_Points = mean(Total_Points),
      Max_Points = mean(Max_Points),
      Points_Pct = mean(Points_Pct),
      Champion_Pick = NA_integer_,
      .groups = "drop"
    )

  write_csv(by_round, file.path(OUTPUT_DIR, "backtest_rolling_round_breakdown.csv"))
  write_csv(by_season, file.path(OUTPUT_DIR, "backtest_rolling_bracket_scores.csv"))
  if (length(leakage_rows) > 0) {
    write_csv(bind_rows(leakage_rows), file.path(OUTPUT_DIR, "backtest_rolling_leakage_guard.csv"))
  }

  report <- c(
    "# Rolling Bracket Backtest",
    "",
    paste0("Backtest seasons: ", paste(seasons, collapse = ", ")),
    "Method: strict out-of-sample rolling fit per season (train only on seasons before test year).",
    "Scoring: slot-accurate ESPN-style (exact slot winner required).",
    "Comparisons: rolling model vs chalk baseline.",
    "Model: xgboost baseline spec.",
    paste0("Feature profile: ", profile),
    "Scoring: ESPN-style round weights (10, 20, 40, 80, 160, 320).",
    "",
    "## Season Scores",
    "",
    "| Season | Method | Train Through | Correct Games | Total Points | Max Points | Points % | Exact Map % | Round Fill % |",
    "|---|---|---:|---:|---:|---:|---:|---:|---:|",
    paste0(
      "| ", by_season$Season,
      " | ", by_season$Method,
      " | ", by_season$Train_Through,
      " | ", by_season$Correct_Games,
      " | ", by_season$Total_Points,
      " | ", by_season$Max_Points,
      " | ", sprintf("%.2f%%", 100 * by_season$Points_Pct),
      " | ", sprintf("%.1f%%", 100 * by_season$Mapping_Exact_Pct),
      " | ", sprintf("%.1f%%", 100 * by_season$Mapping_RoundFill_Pct),
      " |"
    ),
    "",
    "## Mean Across Seasons",
    "",
    paste0(
      "- ", overall$Method, ": ",
      sprintf("%.2f", overall$Correct_Games), " / 63 games, ",
      sprintf("%.1f", overall$Total_Points), " / ", sprintf("%.0f", overall$Max_Points),
      " points (", sprintf("%.2f%%", 100 * overall$Points_Pct), ")"
    ),
    "",
    "## Scoring Mode Notes",
    "",
    paste0(
      "- ",
      paste0(by_season$Season, " (", by_season$Method, "): ", by_season$Scoring_Mode, collapse = "; ")
    ),
    "",
    "## 2023 Diagnostic (Model vs Chalk)",
    "",
    paste0(
      "- Model points: ",
      by_season %>% filter(Season == 2023L, Method == "model") %>% pull(Total_Points) %>% first(),
      " | Chalk points: ",
      by_season %>% filter(Season == 2023L, Method == "chalk") %>% pull(Total_Points) %>% first()
    ),
    paste0(
      "- Model correct games: ",
      by_season %>% filter(Season == 2023L, Method == "model") %>% pull(Correct_Games) %>% first(),
      " | Chalk correct games: ",
      by_season %>% filter(Season == 2023L, Method == "chalk") %>% pull(Correct_Games) %>% first()
    ),
    "",
    "## Leakage Guard",
    "",
    paste0(
      "- Enabled: ", LEAKAGE_GUARD,
      "; Seasons checked: ", paste(LEAKAGE_GUARD_SEASONS, collapse = ", "),
      "; Thresholds -> point_drop >= ", LEAKAGE_MIN_POINT_DROP,
      " OR r1_drop >= ", LEAKAGE_MIN_R1_DROP
    ),
    ""
  )
  writeLines(report, file.path(OUTPUT_DIR, "BRACKET_BACKTEST_ROLLING.md"))

  message("Saved output/backtest_rolling_bracket_scores.csv")
  message("Saved output/backtest_rolling_round_breakdown.csv")
  message("Saved output/BRACKET_BACKTEST_ROLLING.md")
}

if (Sys.getenv("BRACKET_SKIP_MAIN", "0") != "1") {
  main()
}
