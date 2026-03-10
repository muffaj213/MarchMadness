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

source(here("src", "utils", "feature_engineering.R"))  # compute_matchup_features for simulate_bracket

PROC_DIR <- here("data", "processed")
MODELS_DIR <- here("models")
OUTPUT_DIR <- here("output")
BRACKET_DIR <- here("data", "bracket")

# Which season to predict (use seeds from this year)
PREDICT_SEASON <- 2024L

#' Read CSV if file exists, else NULL
read_optional_csv <- function(path) {
  if (file.exists(path)) read_csv(path, show_col_types = FALSE) else NULL
}

#' Load model and processed data
#' @param seeds_file Optional path to seeds CSV (Season, Seed, TeamID). If NULL, use tourney_seeds.csv.
load_for_prediction <- function(seeds_file = NULL) {
  model_path <- file.path(MODELS_DIR, "bracket_model.rds")
  if (!file.exists(model_path)) {
    stop("Model not found. Run 03_train_model.R first.")
  }

  model <- readRDS(model_path)

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
  head_to_head <- read_optional_csv(file.path(PROC_DIR, "head_to_head.csv"))
  sos_stats <- read_optional_csv(file.path(PROC_DIR, "sos_stats.csv"))
  rest_stats <- read_optional_csv(file.path(PROC_DIR, "rest_stats.csv"))
  conference_stats <- read_optional_csv(file.path(PROC_DIR, "conference_stats.csv"))
  quadrant_stats <- read_optional_csv(file.path(PROC_DIR, "quadrant_stats.csv"))
  first_four_stats <- read_optional_csv(file.path(PROC_DIR, "first_four_stats.csv"))

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
    head_to_head = head_to_head,
    sos_stats = sos_stats,
    rest_stats = rest_stats,
    conference_stats = conference_stats,
    quadrant_stats = quadrant_stats,
    first_four_stats = first_four_stats
  )
}

#' Run bracket simulation
#' @param season Season year
#' @param seeds_file Optional path to seeds CSV for projected bracket. If NULL, use tourney_seeds.csv.
#' @param use_projected_output If TRUE, write to bracket_prediction_projected_YEAR.csv
#' @param deterministic If TRUE, pick higher-probability team; if FALSE, sample
main <- function(season = PREDICT_SEASON, seeds_file = NULL, use_projected_output = FALSE, deterministic = TRUE) {
  if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

  message("Loading model and data...")
  data <- load_for_prediction(seeds_file = seeds_file)

  # Filter seeds and slots for the season
  # Some datasets have slots per season; if not, use all slots
  seeds_season <- data$seeds %>% filter(Season == season)
  if (nrow(seeds_season) == 0) {
    message("No seeds for season ", season, ". Using latest available season.")
    available <- unique(data$seeds$Season)
    season <- max(available)
    seeds_season <- data$seeds %>% filter(Season == season)
  }

  # Slots may be season-independent (same bracket structure each year)
  slots_season <- data$slots
  if ("Season" %in% names(data$slots)) {
    slots_filtered <- data$slots %>% filter(Season == season)
    if (nrow(slots_filtered) > 0) slots_season <- slots_filtered
  }

  message("Simulating bracket for season ", season, "...")
  source(here("src", "utils", "bracket_logic.R"), local = TRUE)

  result <- simulate_bracket(
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
    head_to_head = data$head_to_head,
    sos_stats = data$sos_stats,
    rest_stats = data$rest_stats,
    conference_stats = data$conference_stats,
    quadrant_stats = data$quadrant_stats,
    first_four_stats = data$first_four_stats,
    deterministic = deterministic
  )

  # Add team names to results (handle TeamName or Name column)
  team_name_col <- intersect(names(data$teams), c("TeamName", "Team_Name", "Name"))[1]
  if (is.na(team_name_col)) team_name_col <- names(data$teams)[2]

  lookup <- setNames(data$teams[[team_name_col]], data$teams$TeamID)

  game_results <- result$game_results %>%
    mutate(
      team_a_name = lookup[as.character(team_a)],
      team_b_name = lookup[as.character(team_b)],
      winner_name = lookup[as.character(winner)]
    )

  champ_name <- lookup[as.character(result$champion)]

  message("Predicted champion: ", champ_name, " (TeamID ", result$champion, ")")

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
    paste("Predicted Champion:", champ_name),
    paste("TeamID:", result$champion)
  ), champ_file)

  invisible(list(
    champion = result$champion,
    champion_name = champ_name,
    game_results = game_results,
    season = season
  ))
}

if (!isTRUE(getOption("bracket.skip_main"))) {
  main()
}
