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

build_actual_slot_winners <- function(tourney_results, season, seeds_season, slots_season) {
  season_games <- tourney_results %>% filter(Season == season)
  if (nrow(season_games) == 0) return(tibble(slot = character(), round = integer(), winner = integer()))

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
  games <- season_games %>%
    left_join(day_map %>% select(DayNum, round), by = "DayNum") %>%
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
    g <- games %>% filter(round == round_num, team_low == low, team_high == high)
    if (nrow(g) == 0) {
      g <- games %>% filter(team_low == low, team_high == high)
    }
    if (nrow(g) == 0) {
      stop("Could not map actual game to slot ", slot, " for season ", season)
    }
    winner <- as.integer(g$WTeamID[1])
    slot_winners[[slot]] <- winner
    out[[length(out) + 1]] <- tibble(slot = slot, round = round_num, winner = winner)
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

fit_rolling_model <- function(matchup_data, test_season) {
  train <- matchup_data %>%
    filter(Season < test_season) %>%
    filter(!is.na(outcome) & !is.infinite(outcome))

  if (nrow(train) < 200) stop("Insufficient training rows before season ", test_season)

  feature_cols <- setdiff(names(train), c("Season", "TeamA", "TeamB", "outcome"))
  for (col in feature_cols) {
    bad <- is.na(train[[col]]) | is.infinite(train[[col]])
    if (any(bad)) train[[col]][bad] <- 0
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

simulate_model_bracket <- function(data, season, model) {
  seeds_season <- data$seeds %>% filter(Season == season)
  if (nrow(seeds_season) == 0) stop("No seeds found for season ", season)

  slots_season <- get_slots_for_season(season, data$slots)
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
    mutate(SeedNum = as.integer(gsub("^[A-Za-z]+0?", "", Seed))) %>%
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
    seed_a <- if (length(seed_a) > 0) seed_a[1] else 99L
    seed_b <- if (length(seed_b) > 0) seed_b[1] else 99L
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

  by_round_rows <- list()
  season_rows <- list()

  for (season in seasons) {
    message("Rolling fit for season ", season, " (train on seasons <", season, ")")
    rolling_model <- fit_rolling_model(matchup_data, season)
    seeds_season <- data$seeds %>% filter(Season == season)
    slots_season <- get_slots_for_season(season, data$slots)
    actual_slots <- build_actual_slot_winners(tourney_results, season, seeds_season, slots_season)

    model_out <- simulate_model_bracket(data, season, rolling_model)
    chalk_out <- simulate_chalk_bracket(season, seeds_season, slots_season)

    scored_model <- score_bracket_slot_accurate(model_out$game_results, actual_slots)
    scored_chalk <- score_bracket_slot_accurate(chalk_out$game_results, actual_slots)
    max_points <- sum(c(32, 16, 8, 4, 2, 1) * as.integer(ROUND_POINTS[c("1", "2", "3", "4", "5", "6")]))

    by_round_rows[[length(by_round_rows) + 1]] <- scored_model$round_breakdown %>%
      mutate(Season = season, Method = "model", .before = 1)
    by_round_rows[[length(by_round_rows) + 1]] <- scored_chalk$round_breakdown %>%
      mutate(Season = season, Method = "chalk", .before = 1)

    season_rows[[length(season_rows) + 1]] <- tibble(
      Season = season,
      Method = "model",
      Train_Through = season - 1L,
      Correct_Games = scored_model$total_correct,
      Total_Games = 63L,
      Total_Points = scored_model$total_points,
      Max_Points = max_points,
      Points_Pct = scored_model$total_points / max_points,
      Champion_Pick = model_out$champion
    )
    season_rows[[length(season_rows) + 1]] <- tibble(
      Season = season,
      Method = "chalk",
      Train_Through = NA_integer_,
      Correct_Games = scored_chalk$total_correct,
      Total_Games = 63L,
      Total_Points = scored_chalk$total_points,
      Max_Points = max_points,
      Points_Pct = scored_chalk$total_points / max_points,
      Champion_Pick = chalk_out$champion
    )
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

  report <- c(
    "# Rolling Bracket Backtest",
    "",
    paste0("Backtest seasons: ", paste(seasons, collapse = ", ")),
    "Method: strict out-of-sample rolling fit per season (train only on seasons before test year).",
    "Scoring: slot-accurate ESPN-style (exact slot winner required).",
    "Comparisons: rolling model vs chalk baseline.",
    "Model: xgboost baseline spec.",
    "Scoring: ESPN-style round weights (10, 20, 40, 80, 160, 320).",
    "",
    "## Season Scores",
    "",
    "| Season | Method | Train Through | Correct Games | Total Points | Max Points | Points % |",
    "|---|---|---:|---:|---:|---:|---:|",
    paste0(
      "| ", by_season$Season,
      " | ", by_season$Method,
      " | ", by_season$Train_Through,
      " | ", by_season$Correct_Games,
      " | ", by_season$Total_Points,
      " | ", by_season$Max_Points,
      " | ", sprintf("%.2f%%", 100 * by_season$Points_Pct),
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
    ""
  )
  writeLines(report, file.path(OUTPUT_DIR, "BRACKET_BACKTEST_ROLLING.md"))

  message("Saved output/backtest_rolling_bracket_scores.csv")
  message("Saved output/backtest_rolling_round_breakdown.csv")
  message("Saved output/BRACKET_BACKTEST_ROLLING.md")
}

main()
