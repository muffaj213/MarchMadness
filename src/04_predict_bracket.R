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

PROC_DIR <- here("data", "processed")
MODELS_DIR <- here("models")
OUTPUT_DIR <- here("output")
BRACKET_DIR <- here("data", "bracket")

# Which season to predict (use seeds from this year)
PREDICT_SEASON <- 2024L

#' Load model and processed data
load_for_prediction <- function() {
  model_path <- file.path(MODELS_DIR, "bracket_model.rds")
  if (!file.exists(model_path)) {
    stop("Model not found. Run 03_train_model.R first.")
  }

  model <- readRDS(model_path)

  win_pct <- read_csv(file.path(PROC_DIR, "win_pct.csv"), show_col_types = FALSE)
  points_stats <- read_csv(file.path(PROC_DIR, "points_stats.csv"), show_col_types = FALSE)
  seeds <- read_csv(file.path(PROC_DIR, "tourney_seeds.csv"), show_col_types = FALSE)
  slots <- read_csv(file.path(PROC_DIR, "tourney_slots.csv"), show_col_types = FALSE)
  teams <- read_csv(file.path(PROC_DIR, "teams.csv"), show_col_types = FALSE)

  list(
    model = model,
    win_pct = win_pct,
    points_stats = points_stats,
    seeds = seeds,
    slots = slots,
    teams = teams
  )
}

#' Run bracket simulation
main <- function(season = PREDICT_SEASON, deterministic = TRUE) {
  if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

  message("Loading model and data...")
  data <- load_for_prediction()

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
  out_file <- file.path(OUTPUT_DIR, paste0("bracket_prediction_", season, ".csv"))
  write_csv(game_results, out_file)
  message("Bracket predictions saved to ", out_file)

  # Save champion
  champ_file <- file.path(OUTPUT_DIR, paste0("champion_", season, ".txt"))
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

main()
