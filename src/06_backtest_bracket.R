library(here)
library(readr)
library(dplyr)
library(purrr)

source(here("src", "config.R"))
source(here("src", "utils", "feature_engineering.R"))
source(here("src", "utils", "bracket_logic.R"))
source(here("src", "utils", "bracket_slots.R"))

BACKTEST_SEASONS <- c(2022L, 2023L, 2024L)
ROUND_POINTS <- c(`1` = 10L, `2` = 20L, `3` = 40L, `4` = 80L, `5` = 160L, `6` = 320L)

read_tourney_results <- function() {
  path_ext <- file.path(RAW_EXTENDED_DIR, "MNCAATourneyCompactResults.csv")
  path_raw <- file.path(RAW_DIR, "MNCAATourneyCompactResults.csv")
  path <- if (file.exists(path_ext)) path_ext else path_raw
  if (!file.exists(path)) stop("Tournament results not found in raw or raw_extended data.")
  read_csv(path, show_col_types = FALSE)
}

actual_round_winners <- function(tourney_results, season) {
  tourney_results %>%
    filter(Season == season) %>%
    mutate(round = daynum_to_round(DayNum)) %>%
    transmute(round = as.integer(round), winner = as.integer(WTeamID)) %>%
    filter(round >= 1L, round <= 6L) %>%
    distinct(round, winner)
}

score_bracket <- function(pred_games, actual_winners) {
  pred <- pred_games %>%
    transmute(round = as.integer(round), winner = as.integer(winner)) %>%
    filter(round >= 1L, round <= 6L) %>%
    distinct(round, winner)

  scored <- pred %>%
    left_join(actual_winners %>% mutate(actual = TRUE), by = c("round", "winner")) %>%
    mutate(
      correct = !is.na(actual),
      points_per_game = as.integer(ROUND_POINTS[as.character(round)]),
      points = if_else(correct, points_per_game, 0L)
    )

  round_breakdown <- scored %>%
    group_by(round) %>%
    summarise(
      picks = n(),
      correct = sum(correct),
      points = sum(points),
      .groups = "drop"
    ) %>%
    arrange(round)

  list(
    round_breakdown = round_breakdown,
    total_correct = sum(round_breakdown$correct),
    total_points = sum(round_breakdown$points)
  )
}

simulate_model_bracket <- function(data, season) {
  seeds_season <- data$seeds %>% filter(Season == season)
  if (nrow(seeds_season) == 0) stop("No seeds found for season ", season)

  slots_season <- get_slots_for_season(season, data$slots)
  simulate_bracket(
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
    deterministic = TRUE
  )
}

main <- function(seasons = BACKTEST_SEASONS) {
  if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

  options(bracket.skip_main = TRUE)
  source(here("src", "04_predict_bracket.R"), local = TRUE)
  options(bracket.skip_main = NULL)

  data <- load_for_prediction(seeds_file = file.path(PROC_DIR, "tourney_seeds.csv"))
  tourney_results <- read_tourney_results()

  by_round_rows <- list()
  season_rows <- list()

  for (season in seasons) {
    model_out <- simulate_model_bracket(data, season)
    actual <- actual_round_winners(tourney_results, season)
    scored <- score_bracket(model_out$game_results, actual)

    by_round_rows[[length(by_round_rows) + 1]] <- scored$round_breakdown %>%
      mutate(Season = season, .before = 1)

    max_points <- sum(c(32, 16, 8, 4, 2, 1) * as.integer(ROUND_POINTS[c("1", "2", "3", "4", "5", "6")]))
    season_rows[[length(season_rows) + 1]] <- tibble(
      Season = season,
      Correct_Games = scored$total_correct,
      Total_Games = 63L,
      Total_Points = scored$total_points,
      Max_Points = max_points,
      Points_Pct = scored$total_points / max_points,
      Champion_Pick = model_out$champion
    )
  }

  by_round <- bind_rows(by_round_rows) %>% arrange(Season, round)
  by_season <- bind_rows(season_rows) %>% arrange(Season)
  overall <- by_season %>%
    summarise(
      Season = "mean",
      Correct_Games = mean(Correct_Games),
      Total_Games = mean(Total_Games),
      Total_Points = mean(Total_Points),
      Max_Points = mean(Max_Points),
      Points_Pct = mean(Points_Pct),
      Champion_Pick = NA_integer_
    )

  write_csv(by_round, file.path(OUTPUT_DIR, "backtest_round_breakdown.csv"))
  write_csv(by_season, file.path(OUTPUT_DIR, "backtest_bracket_scores.csv"))

  report <- c(
    "# Bracket Backtest",
    "",
    paste0("Backtest seasons: ", paste(seasons, collapse = ", ")),
    "Scoring: ESPN-style round weights (10, 20, 40, 80, 160, 320).",
    "",
    "## Season Scores",
    "",
    "| Season | Correct Games | Total Points | Max Points | Points % |",
    "|---|---:|---:|---:|---:|",
    paste0(
      "| ", by_season$Season,
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
      "- Mean correct games: ", sprintf("%.2f", overall$Correct_Games),
      " / 63"
    ),
    paste0(
      "- Mean points: ", sprintf("%.1f", overall$Total_Points),
      " / ", sprintf("%.0f", overall$Max_Points),
      " (", sprintf("%.2f%%", 100 * overall$Points_Pct), ")"
    ),
    ""
  )
  writeLines(report, file.path(OUTPUT_DIR, "BRACKET_BACKTEST.md"))

  message("Saved output/backtest_bracket_scores.csv")
  message("Saved output/backtest_round_breakdown.csv")
  message("Saved output/BRACKET_BACKTEST.md")
}

main()
