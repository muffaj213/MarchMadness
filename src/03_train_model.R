# =============================================================================
# 03_train_model.R - Train and validate NCAA tournament prediction model
# =============================================================================
# Uses logistic regression on matchup features. Validates with time-based
# holdout and reports accuracy by round.
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
TEST_SEASONS <- 2024L      # Hold out for validation (or NULL to use latest)
FEATURE_COLS <- c("seed_diff", "winpct_diff", "rating_diff", "pf_diff")

#' Train logistic regression model on matchup data
train_model <- function(matchup_data, formula_str = NULL) {
  # tidymodels classification requires factor outcome
  matchup_data <- matchup_data %>%
    mutate(outcome = factor(outcome, levels = c(0, 1), labels = c("Lose", "Win")))

  if (is.null(formula_str)) {
    formula_str <- paste("outcome ~", paste(FEATURE_COLS, collapse = " + "))
  }

  recipe <- recipe(as.formula(formula_str), data = matchup_data) %>%
    step_normalize(all_predictors())

  spec <- logistic_reg(penalty = 0, mixture = 0) %>%
    set_engine("glm")

  wf <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(spec)

  fit(wf, data = matchup_data)
}

#' Evaluate model on held-out season(s)
#'
#' For each test season, predicts all tournament games and computes accuracy.
#' Optionally breaks down by round.
evaluate_model <- function(model, matchup_data, test_seasons, seeds_df = NULL) {
  # Get test data (outcome as factor for consistency with training)
  test_data <- matchup_data %>%
    filter(Season %in% test_seasons) %>%
    mutate(outcome = factor(outcome, levels = c(0, 1), labels = c("Lose", "Win")))
  if (nrow(test_data) == 0) {
    message("No test data for seasons ", paste(test_seasons, collapse = ", "))
    return(NULL)
  }

  pred <- predict(model, test_data, type = "prob")
  prob_win <- pred$.pred_Win
  test_data$pred_prob <- prob_win
  test_data$pred_class <- as.integer(prob_win >= 0.5)
  test_data$outcome_num <- as.integer(test_data$outcome == "Win")
  test_data$correct <- test_data$pred_class == test_data$outcome_num

  overall_acc <- mean(test_data$correct)
  message("Overall game accuracy: ", round(overall_acc * 100, 2), "%")

  # Log loss for probabilistic evaluation
  eps <- 1e-15
  probs <- pmax(eps, pmin(1 - eps, prob_win))
  actual <- test_data$outcome_num
  log_loss <- -mean(actual * log(probs) + (1 - actual) * log(1 - probs))
  message("Log loss: ", round(log_loss, 4))

  list(
    accuracy = overall_acc,
    log_loss = log_loss,
    predictions = test_data,
    n_games = nrow(test_data)
  )
}

#' Time-based cross-validation: train on 2010..(year-1), test on year
run_time_cv <- function(matchup_data, min_train_year = 2010L, max_test_year = 2023L) {
  results <- list()
  for (test_year in (min_train_year + 1):max_test_year) {
    train_data <- matchup_data %>% filter(Season < test_year, Season >= min_train_year)
    if (nrow(train_data) < 50) next

    model <- train_model(train_data)
    eval <- evaluate_model(model, matchup_data, test_year)
    if (!is.null(eval)) {
      results[[as.character(test_year)]] <- eval
    }
  }
  results
}

#' Main training pipeline
main <- function() {
  if (!dir.exists(MODELS_DIR)) dir.create(MODELS_DIR, recursive = TRUE)
  if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

  matchup_path <- file.path(PROC_DIR, "matchup_data.csv")
  if (!file.exists(matchup_path)) {
    stop("Processed data not found. Run 02_process_data.R first.")
  }

  message("Loading processed data...")
  matchup_data <- read_csv(matchup_path, show_col_types = FALSE)

  # Filter to reasonable season range (2008+ when using extended historical data)
  matchup_data <- matchup_data %>% filter(Season >= 2008, Season <= 2025)

  if (nrow(matchup_data) < 100) {
    stop("Insufficient training data. Need at least 100 games. Found: ", nrow(matchup_data))
  }

  # Train on all data through TRAIN_SEASONS_END (or use last year for holdout)
  train_data <- matchup_data %>% filter(Season <= TRAIN_SEASONS_END)
  if (nrow(train_data) == 0) {
    train_data <- matchup_data %>% filter(Season < max(Season))
  }

  message("Training model on ", nrow(train_data), " games...")
  model <- train_model(train_data)

  # Save model
  model_path <- file.path(MODELS_DIR, "bracket_model.rds")
  saveRDS(model, model_path)
  message("Model saved to ", model_path)

  # Validation
  test_years <- if (!is.null(TEST_SEASONS)) TEST_SEASONS else max(matchup_data$Season)
  if (max(matchup_data$Season) > TRAIN_SEASONS_END) {
    message("\nValidating on held-out season(s)...")
    eval <- evaluate_model(model, matchup_data, test_years)
    if (!is.null(eval)) {
      write_csv(eval$predictions, file.path(OUTPUT_DIR, "validation_predictions.csv"))
    }
  }

  # Optional: quick time-based CV
  message("\nQuick time-based CV (2012-2024)...")
  cv_results <- run_time_cv(matchup_data, min_train_year = 2008L, max_test_year = 2024L)
  if (length(cv_results) > 0) {
    accs <- sapply(cv_results, function(r) r$accuracy)
    valid <- !is.na(accs)
    if (any(valid)) {
      message("CV accuracy by year: ", paste(names(accs)[valid], "=", round(accs[valid] * 100, 1), collapse = ", "))
      message("Mean CV accuracy: ", round(mean(accs[valid]) * 100, 2), "%")
    }
  }

  message("\nTraining complete.")
}

main()
