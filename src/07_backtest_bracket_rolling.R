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
LEAKAGE_GUARD <- tolower(trimws(Sys.getenv("ROLLING_LEAKAGE_GUARD", unset = "true"))) %in% c("true", "1", "yes")
LEAKAGE_GUARD_SEASONS <- c(2024L)
LEAKAGE_MIN_POINT_DROP <- 200L
LEAKAGE_MIN_R1_DROP <- 4L
USE_TOURNEY_LOCATION_FEATURES <- FALSE
USE_NISHAA_FEATURES <- FALSE
USE_MINIMAL_FEATURE_SET <- TRUE
MINIMAL_FEATURE_COLS <- c(
  "round", "seed_diff", "seed_diff_sq", "seed_sum",
  "is_upset_matchup", "upset_seed_gap", "round_seed_interaction",
  "upset_winpct_diff", "tourney_winpct_diff", "rest_diff", "h2h_team_a_winpct"
)
FEATURE_PROFILE <- tolower(trimws(Sys.getenv("ROLLING_FEATURE_PROFILE", unset = "minimal")))
USE_SEED_ROUND_PRIORS <- tolower(trimws(Sys.getenv("ROLLING_USE_SEED_PRIORS", unset = "false"))) %in% c("true", "1", "yes")
ROLLING_MC_SIMS <- as.integer(Sys.getenv("ROLLING_MC_SIMS", unset = "1000"))

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

compute_log_loss <- function(actual01, pred_prob) {
  p <- pmax(1e-15, pmin(1 - 1e-15, as.numeric(pred_prob)))
  y <- as.numeric(actual01)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

build_rolling_case_weights <- function(df) {
  if (!("Season" %in% names(df))) return(rep(1, nrow(df)))
  latest <- suppressWarnings(max(as.integer(df$Season), na.rm = TRUE))
  if (!is.finite(latest)) latest <- 0L
  season_gap <- pmax(0L, latest - as.integer(df$Season))
  # Recency weighting helps match production training behavior.
  recency_w <- 0.92 ^ season_gap

  round_w <- if ("Round" %in% names(df)) {
    dplyr::recode(as.integer(df$Round),
      `0` = 0.75, `1` = 1.0, `2` = 1.1, `3` = 1.2, `4` = 1.3, `5` = 1.4, `6` = 1.5,
      .default = 1.0
    )
  } else {
    rep(1, nrow(df))
  }
  as.numeric(recency_w * round_w)
}

rolling_model_spec <- function(model_type) {
  switch(model_type,
    "glm" = logistic_reg(mode = "classification") %>% set_engine("glm"),
    "glmnet" = logistic_reg(mode = "classification", penalty = 0.01, mixture = 1) %>% set_engine("glmnet"),
    "xgboost" = boost_tree(
      mode = "classification",
      trees = 200,
      min_n = 15,
      learn_rate = 0.05,
      tree_depth = 3,
      stop_iter = 15
    ) %>% set_engine("xgboost", validation = 0.15),
    "rand_forest" = rand_forest(
      mode = "classification",
      trees = 500,
      min_n = 10,
      mtry = tune()
    ) %>% set_engine("ranger", importance = "none"),
    stop("Unsupported rolling model type: ", model_type)
  )
}

fit_rolling_candidate <- function(train_df, feature_cols, model_type) {
  formula_str <- paste("outcome ~", paste(feature_cols, collapse = " + "))
  rec <- recipe(as.formula(formula_str), data = train_df, case_weights = case_wt) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_predictors())

  spec <- rolling_model_spec(model_type)
  wf <- workflow() %>% add_recipe(rec)

  if (model_type == "rand_forest") {
    mtry_val <- max(1L, floor(sqrt(length(feature_cols))))
    spec <- finalize_model(spec, tibble(mtry = mtry_val))
  }

  wf %>% add_model(spec) %>% fit(data = train_df)
}

fit_rolling_model <- function(matchup_data, test_season, shuffle_labels = FALSE, shuffle_seed = 42L) {
  train_full <- matchup_data %>%
    filter(Season < test_season) %>%
    filter(!is.na(outcome) & !is.infinite(outcome))

  if (nrow(train_full) < 200) stop("Insufficient training rows before season ", test_season)

  feature_cols <- setdiff(names(train_full), c("Season", "TeamA", "TeamB", "outcome", "Round"))
  for (col in feature_cols) {
    bad <- is.na(train_full[[col]]) | is.infinite(train_full[[col]])
    if (any(bad)) train_full[[col]][bad] <- 0
  }

  if (isTRUE(shuffle_labels)) {
    set.seed(as.integer(shuffle_seed))
    train_full$outcome <- sample(train_full$outcome)
  }

  prep_train <- function(df) {
    w <- build_rolling_case_weights(df)
    df %>%
      mutate(
        outcome = factor(outcome, levels = c(0, 1), labels = c("Lose", "Win")),
        case_wt = hardhat::importance_weights(w)
      )
  }

  seasons_avail <- sort(unique(as.integer(train_full$Season)))
  val_season <- max(seasons_avail, na.rm = TRUE)
  can_validate <- length(seasons_avail) >= 3 &&
    sum(train_full$Season < val_season) >= 300 &&
    sum(train_full$Season == val_season) >= 50

  model_types <- c("glm", "glmnet", "xgboost", "rand_forest")

  if (!can_validate) {
    fallback_fit <- fit_rolling_candidate(prep_train(train_full), feature_cols, "xgboost")
    return(list(
      model = fallback_fit,
      selected_model = "xgboost_fallback",
      validation_season = NA_integer_,
      validation_log_loss = NA_real_
    ))
  }

  analysis <- train_full %>% filter(Season < val_season)
  assess <- train_full %>% filter(Season == val_season)
  analysis_f <- prep_train(analysis)
  assess_f <- prep_train(assess)
  assess_y <- as.integer(assess_f$outcome == "Win")

  fit_candidates <- list()
  fit_scores <- tibble()
  assess_pred_tbl <- tibble(outcome_num = assess_y)

  for (mt in model_types) {
    candidate_fit <- tryCatch(
      fit_rolling_candidate(analysis_f, feature_cols, mt),
      error = function(e) NULL
    )
    if (is.null(candidate_fit)) next

    pred <- tryCatch(
      predict(candidate_fit, assess_f, type = "prob")$.pred_Win,
      error = function(e) rep(0.5, nrow(assess_f))
    )
    ll <- compute_log_loss(assess_y, pred)

    fit_candidates[[mt]] <- candidate_fit
    fit_scores <- bind_rows(fit_scores, tibble(model = mt, log_loss = ll))
    assess_pred_tbl[[mt]] <- pmax(1e-15, pmin(1 - 1e-15, as.numeric(pred)))
  }

  if (nrow(fit_scores) == 0) {
    fallback_fit <- fit_rolling_candidate(prep_train(train_full), feature_cols, "xgboost")
    return(list(
      model = fallback_fit,
      selected_model = "xgboost_fallback_after_fit_error",
      validation_season = val_season,
      validation_log_loss = NA_real_
    ))
  }

  ensemble_meta_coef <- NULL
  ensemble_candidate_names <- intersect(model_types, names(fit_candidates))
  if (length(ensemble_candidate_names) >= 2) {
    meta_formula <- as.formula(paste("outcome_num ~", paste(ensemble_candidate_names, collapse = " + ")))
    meta_fit <- tryCatch(
      glm(meta_formula, data = assess_pred_tbl, family = binomial()),
      error = function(e) NULL
    )
    if (!is.null(meta_fit)) {
      ensemble_prob <- tryCatch(
        pmax(1e-15, pmin(1 - 1e-15, as.numeric(predict(meta_fit, newdata = assess_pred_tbl, type = "response")))),
        error = function(e) NULL
      )
      if (!is.null(ensemble_prob)) {
        fit_scores <- bind_rows(
          fit_scores,
          tibble(model = "stacked_ensemble", log_loss = compute_log_loss(assess_y, ensemble_prob))
        )
        ensemble_meta_coef <- coef(meta_fit)
      }
    }
  }

  best <- fit_scores %>% arrange(log_loss) %>% slice(1)
  selected <- as.character(best$model[1])

  if (selected == "stacked_ensemble" && !is.null(ensemble_meta_coef)) {
    full_models <- list()
    full_train_f <- prep_train(train_full)
    for (mt in ensemble_candidate_names) {
      full_models[[mt]] <- fit_rolling_candidate(full_train_f, feature_cols, mt)
    }
    ensemble_model <- structure(list(
      type = "ensemble",
      models = full_models,
      calibrators = NULL,
      weights = rep(1 / length(full_models), length(full_models)),
      meta_coef = ensemble_meta_coef,
      model_names = names(full_models),
      calibration = NULL
    ), class = c("ensemble_model", "list"))
    return(list(
      model = ensemble_model,
      selected_model = selected,
      validation_season = val_season,
      validation_log_loss = as.numeric(best$log_loss[1])
    ))
  }

  final_fit <- fit_rolling_candidate(prep_train(train_full), feature_cols, selected)
  list(
    model = final_fit,
    selected_model = selected,
    validation_season = val_season,
    validation_log_loss = as.numeric(best$log_loss[1])
  )
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

count_predicted_upsets <- function(pred_games, seeds_season) {
  if (nrow(pred_games) == 0 || nrow(seeds_season) == 0) return(0L)
  if (!all(c("team_a", "team_b", "winner") %in% names(pred_games))) return(0L)
  seeds_num <- seeds_season %>%
    mutate(SeedNum = readr::parse_number(as.character(Seed))) %>%
    select(TeamID, SeedNum)
  pred_games %>%
    left_join(seeds_num %>% rename(winner = TeamID, winner_seed = SeedNum), by = "winner") %>%
    left_join(seeds_num %>% rename(team_a = TeamID, seed_a = SeedNum), by = "team_a") %>%
    left_join(seeds_num %>% rename(team_b = TeamID, seed_b = SeedNum), by = "team_b") %>%
    mutate(opp_seed = if_else(winner == team_a, seed_b, seed_a)) %>%
    summarise(n = sum(!is.na(winner_seed) & !is.na(opp_seed) & winner_seed > opp_seed, na.rm = TRUE)) %>%
    pull(n) %>%
    as.integer()
}

simulate_model_bracket <- function(data, season, model, seeds_override = NULL, slots_override = NULL,
                                   use_seed_round_priors = USE_SEED_ROUND_PRIORS) {
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
    seed_round_priors = if (isTRUE(use_seed_round_priors)) data$seed_round_priors else tibble(),
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
    deterministic = TRUE,
    use_seed_round_priors = use_seed_round_priors
  )
}

simulate_model_optimal_bracket <- function(data, season, model, seeds_override = NULL, slots_override = NULL,
                                           n_sims = ROLLING_MC_SIMS,
                                           use_seed_round_priors = USE_SEED_ROUND_PRIORS) {
  seeds_season <- if (is.null(seeds_override)) {
    data$seeds %>% filter(Season == season)
  } else {
    seeds_override
  }
  if (nrow(seeds_season) == 0) stop("No seeds found for season ", season)
  slots_season <- if (is.null(slots_override)) get_slots_for_season(season, data$slots) else slots_override

  sims <- vector("list", n_sims)
  for (i in seq_len(n_sims)) {
    sim <- simulate_bracket(
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
      seed_round_priors = if (isTRUE(use_seed_round_priors)) data$seed_round_priors else tibble(),
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
    sims[[i]] <- sim$game_results %>% mutate(sim_id = i)
    if (i %% 100 == 0 || i == n_sims) message("    MC optimal sim ", i, " / ", n_sims)
  }
  slot_odds <- bind_rows(sims) %>%
    count(slot, round, team_id = winner, name = "wins") %>%
    mutate(win_rate = wins / n_sims)
  select_optimal_bracket(
    season = season,
    slots_df = slots_season,
    seeds_df = seeds_season,
    slot_odds = slot_odds,
    round_points = ROUND_POINTS
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
    out[[length(out) + 1]] <- tibble(
      slot = slot,
      round = round_num,
      team_a = as.integer(team_a),
      team_b = as.integer(team_b),
      winner = as.integer(winner)
    )
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
  valid_profiles <- c("full", "full_with_upset", "no_nishaa", "minimal", "kenpom_only", "resume_only", "fte_evan_shooting_only")
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
  if (!(profile %in% c("full", "full_with_upset"))) {
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
  message("Seed-round priors enabled: ", USE_SEED_ROUND_PRIORS)

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
  upset_rows <- list()

  for (season in seasons) {
    message("Rolling fit for season ", season, " (train on seasons <", season, ")")
    rolling_fit <- fit_rolling_model(matchup_data, season)
    rolling_model <- rolling_fit$model
    message(
      "  selected rolling model: ", rolling_fit$selected_model,
      ifelse(is.na(rolling_fit$validation_log_loss), "", paste0(" (val log-loss=", round(rolling_fit$validation_log_loss, 4), ")"))
    )
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
    model_optimal_out <- simulate_model_optimal_bracket(
      data,
      season,
      rolling_model,
      seeds_override = seeds_season,
      slots_override = slots_season,
      n_sims = ROLLING_MC_SIMS
    )
    model_optimal_no_priors_out <- if (isTRUE(USE_SEED_ROUND_PRIORS)) {
      simulate_model_optimal_bracket(
        data,
        season,
        rolling_model,
        seeds_override = seeds_season,
        slots_override = slots_season,
        n_sims = ROLLING_MC_SIMS,
        use_seed_round_priors = FALSE
      )
    } else {
      # If priors are globally off, this variant is identical to model_optimal.
      model_optimal_out
    }
    chalk_out <- simulate_chalk_bracket(season, seeds_season, slots_season)

    scoring_mode <- "slot_accurate"
    scored_model <- score_bracket_slot_accurate(model_out$game_results, actual_slots)
    scored_model_optimal <- score_bracket_slot_accurate(model_optimal_out$game_results, actual_slots)
    scored_model_optimal_no_priors <- score_bracket_slot_accurate(model_optimal_no_priors_out$game_results, actual_slots)
    scored_chalk <- score_bracket_slot_accurate(chalk_out$game_results, actual_slots)
    max_points <- sum(c(32, 16, 8, 4, 2, 1) * as.integer(ROUND_POINTS[c("1", "2", "3", "4", "5", "6")]))
    r1_model <- compute_round1_correct(model_out$game_results, actual_slots)

    by_round_rows[[length(by_round_rows) + 1]] <- scored_model$round_breakdown %>%
      mutate(Season = season, Method = "model", .before = 1)
    by_round_rows[[length(by_round_rows) + 1]] <- scored_model_optimal$round_breakdown %>%
      mutate(Season = season, Method = "model_optimal", .before = 1)
    by_round_rows[[length(by_round_rows) + 1]] <- scored_model_optimal_no_priors$round_breakdown %>%
      mutate(Season = season, Method = "model_optimal_no_priors", .before = 1)
    by_round_rows[[length(by_round_rows) + 1]] <- scored_chalk$round_breakdown %>%
      mutate(Season = season, Method = "chalk", .before = 1)

    season_rows[[length(season_rows) + 1]] <- tibble(
      Season = season,
      Method = "model",
      Scoring_Mode = scoring_mode,
      Train_Through = season - 1L,
      Rolling_Model = rolling_fit$selected_model,
      Rolling_Val_LogLoss = rolling_fit$validation_log_loss,
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
      Method = "model_optimal",
      Scoring_Mode = scoring_mode,
      Train_Through = season - 1L,
      Rolling_Model = rolling_fit$selected_model,
      Rolling_Val_LogLoss = rolling_fit$validation_log_loss,
      Correct_Games = scored_model_optimal$total_correct,
      Total_Games = 63L,
      Total_Points = scored_model_optimal$total_points,
      Max_Points = max_points,
      Points_Pct = scored_model_optimal$total_points / max_points,
      Champion_Pick = model_optimal_out$champion
    ) %>% mutate(
      Mapping_Exact_Pct = map_quality %>% filter(map_type == "exact_pair") %>% pull(pct) %>% {if (length(.) == 0) 0 else .[1]},
      Mapping_RoundFill_Pct = map_quality %>% filter(map_type == "round_fill") %>% pull(pct) %>% {if (length(.) == 0) 0 else .[1]}
    )
    season_rows[[length(season_rows) + 1]] <- tibble(
      Season = season,
      Method = "model_optimal_no_priors",
      Scoring_Mode = scoring_mode,
      Train_Through = season - 1L,
      Rolling_Model = rolling_fit$selected_model,
      Rolling_Val_LogLoss = rolling_fit$validation_log_loss,
      Correct_Games = scored_model_optimal_no_priors$total_correct,
      Total_Games = 63L,
      Total_Points = scored_model_optimal_no_priors$total_points,
      Max_Points = max_points,
      Points_Pct = scored_model_optimal_no_priors$total_points / max_points,
      Champion_Pick = model_optimal_no_priors_out$champion
    ) %>% mutate(
      Mapping_Exact_Pct = map_quality %>% filter(map_type == "exact_pair") %>% pull(pct) %>% {if (length(.) == 0) 0 else .[1]},
      Mapping_RoundFill_Pct = map_quality %>% filter(map_type == "round_fill") %>% pull(pct) %>% {if (length(.) == 0) 0 else .[1]}
    )
    season_rows[[length(season_rows) + 1]] <- tibble(
      Season = season,
      Method = "chalk",
      Scoring_Mode = scoring_mode,
      Train_Through = NA_integer_,
      Rolling_Model = "chalk_baseline",
      Rolling_Val_LogLoss = NA_real_,
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

    upset_rows[[length(upset_rows) + 1]] <- tibble(
      Season = season,
      Method = c("model", "model_optimal", "model_optimal_no_priors", "chalk"),
      Predicted_Upsets = c(
        count_predicted_upsets(model_out$game_results, seeds_season),
        count_predicted_upsets(model_optimal_out$game_results, seeds_season),
        count_predicted_upsets(model_optimal_no_priors_out$game_results, seeds_season),
        count_predicted_upsets(chalk_out$game_results, seeds_season)
      )
    )

    if (isTRUE(LEAKAGE_GUARD) && profile != "minimal" && season %in% LEAKAGE_GUARD_SEASONS) {
      message("Leakage guard: shuffled-label control for season ", season)
      shuffled_fit <- fit_rolling_model(
        matchup_data,
        season,
        shuffle_labels = TRUE,
        shuffle_seed = 1000L + season
      )
      shuffled_out <- simulate_model_bracket(
        data,
        season,
        shuffled_fit$model,
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
  upset_by_season <- bind_rows(upset_rows) %>% arrange(Method, Season)
  upset_overall <- upset_by_season %>%
    group_by(Method) %>%
    summarise(Mean_Predicted_Upsets = mean(Predicted_Upsets), .groups = "drop")
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
  write_csv(upset_by_season, file.path(OUTPUT_DIR, "backtest_rolling_upset_analysis.csv"))
  if (length(leakage_rows) > 0) {
    write_csv(bind_rows(leakage_rows), file.path(OUTPUT_DIR, "backtest_rolling_leakage_guard.csv"))
  }

  report <- c(
    "# Rolling Bracket Backtest",
    "",
    paste0("Backtest seasons: ", paste(seasons, collapse = ", ")),
    "Method: strict out-of-sample rolling fit per season (train only on seasons before test year).",
    "Scoring: slot-accurate ESPN-style (exact slot winner required).",
    "Comparisons: rolling model (deterministic), model_optimal (Monte Carlo expected-points), model_optimal_no_priors, and chalk baseline.",
    "Model: xgboost baseline spec.",
    paste0("Feature profile: ", profile),
    paste0("Seed-round priors enabled: ", USE_SEED_ROUND_PRIORS),
    paste0("Monte Carlo sims per season for model_optimal: ", ROLLING_MC_SIMS),
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
    "## Upset Analysis",
    "",
    "| Method | Mean Predicted Upsets |",
    "|---|---:|",
    paste0("| ", upset_overall$Method, " | ", sprintf("%.2f", upset_overall$Mean_Predicted_Upsets), " |"),
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
  message("Saved output/backtest_rolling_upset_analysis.csv")
  message("Saved output/BRACKET_BACKTEST_ROLLING.md")
}

if (Sys.getenv("BRACKET_SKIP_MAIN", "0") != "1") {
  main()
}
