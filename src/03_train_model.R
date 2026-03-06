# =============================================================================
# 03_train_model.R - Train and validate NCAA tournament prediction model
# =============================================================================
# Trains glm, xgboost, and rand_forest. Compares holdout results and saves
# the best model as bracket_model.rds for prediction.
# =============================================================================

library(here)
library(readr)
library(dplyr)
library(tidymodels)

PROC_DIR <- here("data", "processed")
MODELS_DIR <- here("models")
OUTPUT_DIR <- here("output")

# Model configuration
TRAIN_SEASONS_END <- 2023L  # Train on seasons through this year
TEST_SEASONS <- 2024L       # Hold out for validation
MODEL_TYPES <- c("glm", "xgboost", "rand_forest")

BASE_FEATURE_COLS <- c("seed_diff", "seed_diff_sq", "seed_sum", "winpct_diff", "late_winpct_diff",
                       "seed_winpct_interaction", "pf_diff")
KENPOM_FEATURE_COLS <- c("adjem_diff", "adj_off_diff", "adj_def_diff", "tempo_diff")

#' Build recipe and workflow for a given model type
build_workflow <- function(model_type, matchup_data) {
  all_feat <- c(BASE_FEATURE_COLS, KENPOM_FEATURE_COLS)
  avail <- intersect(all_feat, names(matchup_data))
  if (length(avail) == 0) stop("No feature columns found in matchup_data")
  formula_str <- paste("outcome ~", paste(avail, collapse = " + "))

  recipe <- recipe(as.formula(formula_str), data = matchup_data) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_predictors())

  spec <- switch(model_type,
    glm = logistic_reg(penalty = 0, mixture = 0) %>% set_engine("glm"),
    xgboost = boost_tree(mode = "classification", engine = "xgboost",
                        trees = 200, min_n = 5, learn_rate = 0.1),
    rand_forest = rand_forest(mode = "classification", engine = "ranger",
                             trees = 500, min_n = 5),
    stop("Unknown model_type: ", model_type)
  )

  workflow() %>%
    add_recipe(recipe) %>%
    add_model(spec)
}

#' Train model by type
train_model <- function(model_type, matchup_data) {
  matchup_data <- matchup_data %>%
    mutate(outcome = factor(outcome, levels = c(0, 1), labels = c("Lose", "Win")))
  wf <- build_workflow(model_type, matchup_data)
  fit(wf, data = matchup_data)
}

#' Evaluate model on held-out season(s)
evaluate_model <- function(model, matchup_data, test_seasons) {
  test_data <- matchup_data %>%
    filter(Season %in% test_seasons) %>%
    mutate(outcome = factor(outcome, levels = c(0, 1), labels = c("Lose", "Win")))
  if (nrow(test_data) == 0) return(NULL)

  pred <- predict(model, test_data, type = "prob")
  prob_win <- pred$.pred_Win
  test_data$pred_prob <- prob_win
  test_data$pred_class <- as.integer(prob_win >= 0.5)
  test_data$outcome_num <- as.integer(test_data$outcome == "Win")
  test_data$correct <- test_data$pred_class == test_data$outcome_num

  accuracy <- mean(test_data$correct)
  eps <- 1e-15
  probs <- pmax(eps, pmin(1 - eps, prob_win))
  log_loss <- -mean(test_data$outcome_num * log(probs) + (1 - test_data$outcome_num) * log(1 - probs))

  list(accuracy = accuracy, log_loss = log_loss, predictions = test_data, n_games = nrow(test_data))
}

#' Main training pipeline: train all models, compare, save best
main <- function() {
  if (!dir.exists(MODELS_DIR)) dir.create(MODELS_DIR, recursive = TRUE)
  if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

  matchup_path <- file.path(PROC_DIR, "matchup_data.csv")
  if (!file.exists(matchup_path)) stop("Processed data not found. Run 02_process_data.R first.")

  message("Loading processed data...")
  matchup_data <- read_csv(matchup_path, show_col_types = FALSE) %>%
    filter(Season >= 2008, Season <= 2025)

  # Drop rows with NA/Inf in outcome or features (XGBoost/RF need clean numeric data)
  feat_cols <- intersect(c(BASE_FEATURE_COLS, KENPOM_FEATURE_COLS), names(matchup_data))
  n_before <- nrow(matchup_data)
  matchup_data <- matchup_data %>%
    filter(!is.na(outcome) & !is.infinite(outcome)) %>%
    filter(if_all(any_of(feat_cols), ~!is.na(.) & !is.infinite(.)))
  if (n_before != nrow(matchup_data)) {
    message("Dropped ", n_before - nrow(matchup_data), " rows with NA/Inf in outcome or features.")
  }
  if (nrow(matchup_data) < 100) stop("Insufficient training data. Need at least 100 games.")

  train_data <- matchup_data %>% filter(Season <= TRAIN_SEASONS_END)
  if (nrow(train_data) == 0) train_data <- matchup_data %>% filter(Season < max(Season))

  test_years <- TEST_SEASONS
  comparison <- tibble(Model = character(), Accuracy = numeric(), LogLoss = numeric(), N_Games = integer())

  for (mt in MODEL_TYPES) {
    message("\n--- Training ", mt, " ---")
    model <- tryCatch(
      train_model(mt, train_data),
      error = function(e) {
        message("  Error training ", mt, ": ", conditionMessage(e))
        return(NULL)
      }
    )
    if (is.null(model)) next

    saveRDS(model, file.path(MODELS_DIR, paste0("bracket_model_", mt, ".rds")))
    message("  Saved bracket_model_", mt, ".rds")

    eval <- evaluate_model(model, matchup_data, test_years)
    if (!is.null(eval)) {
      message("  Holdout accuracy: ", round(eval$accuracy * 100, 2), "% | Log loss: ", round(eval$log_loss, 4))
      comparison <- bind_rows(comparison, tibble(
        Model = mt,
        Accuracy = eval$accuracy,
        LogLoss = eval$log_loss,
        N_Games = eval$n_games
      ))
      if (mt == "glm") {
        write_csv(eval$predictions, file.path(OUTPUT_DIR, "validation_predictions.csv"))
      }
    }
  }

  # Comparison output
  if (nrow(comparison) > 0) {
    comparison <- comparison %>%
      mutate(
        Accuracy_Pct = round(Accuracy * 100, 2),
        LogLoss = round(LogLoss, 4)
      ) %>%
      select(Model, Accuracy_Pct, LogLoss, N_Games)
    write_csv(comparison, file.path(OUTPUT_DIR, "model_comparison.csv"))
    message("\n--- Model Comparison ---")
    print(comparison)

    # Save best model (by log loss) as bracket_model.rds for 04_predict_bracket
    best <- comparison %>% slice_min(LogLoss, n = 1)
    best_type <- best$Model[1]
    best_model <- readRDS(file.path(MODELS_DIR, paste0("bracket_model_", best_type, ".rds")))
    saveRDS(best_model, file.path(MODELS_DIR, "bracket_model.rds"))
    message("\nBest model (by log loss): ", best_type, " -> saved as bracket_model.rds")
  }

  message("\nTraining complete.")
}

main()
