# =============================================================================
# 03_train_model.R - Train and validate NCAA tournament prediction model
# =============================================================================
# Trains glm, xgboost, and rand_forest. Runs BASELINE (fixed params) and TUNED
# (hyperparameter search) configurations. Saves both for comparison.
# Best model (by log loss) saved as bracket_model.rds for prediction.
# =============================================================================

library(here)
library(readr)
library(dplyr)
library(tidymodels)

PROC_DIR <- here("data", "processed")
MODELS_DIR <- here("models")
OUTPUT_DIR <- here("output")
CONFIG_DIR <- here("config")

# Model configuration
TRAIN_SEASONS_END <- 2023L  # Train on seasons through this year
TEST_SEASONS <- 2024L       # Hold out for validation
MODEL_TYPES <- c("glm", "xgboost", "rand_forest")

BASE_FEATURE_COLS <- c("seed_diff", "seed_diff_sq", "seed_sum", "winpct_diff", "late_winpct_diff",
                       "seed_winpct_interaction", "pf_diff", "round", "h2h_team_a_winpct", "sos_diff", "rest_diff")
EXTRA_FEATURE_COLS <- c("home_win_rate_diff", "away_win_rate_diff", "elo_diff", "net_diff", "wab_diff")
KENPOM_FEATURE_COLS <- c("adjem_diff", "adj_off_diff", "adj_def_diff", "tempo_diff", "luck_diff",
                         "adjem_seed_interaction", "seed_latewinpct_interaction", "round_seed_interaction")

# -----------------------------------------------------------------------------
# BASELINE: Fixed parameters (original setup)
# -----------------------------------------------------------------------------
BASELINE_SPECS <- list(
  glm = list(
    penalty = 0,
    mixture = 0,
    engine = "glm",
    note = "Unregularized logistic regression"
  ),
  xgboost = list(
    trees = 200,
    min_n = 5,
    learn_rate = 0.1,
    engine = "xgboost",
    note = "Fixed params"
  ),
  rand_forest = list(
    trees = 500,
    min_n = 5,
    engine = "ranger",
    note = "Fixed params"
  )
)

#' Build baseline workflow (fixed parameters)
build_baseline_workflow <- function(model_type, matchup_data) {
  all_feat <- c(BASE_FEATURE_COLS, KENPOM_FEATURE_COLS, EXTRA_FEATURE_COLS)
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

#' Build tuned workflow (parameters to be tuned)
build_tuned_workflow <- function(model_type, matchup_data) {
  all_feat <- c(BASE_FEATURE_COLS, KENPOM_FEATURE_COLS, EXTRA_FEATURE_COLS)
  avail <- intersect(all_feat, names(matchup_data))
  if (length(avail) == 0) stop("No feature columns found in matchup_data")
  formula_str <- paste("outcome ~", paste(avail, collapse = " + "))

  recipe <- recipe(as.formula(formula_str), data = matchup_data) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_predictors())

  spec <- switch(model_type,
    glm = logistic_reg(penalty = tune(), mixture = tune()) %>% set_engine("glmnet"),
    xgboost = boost_tree(mode = "classification", engine = "xgboost",
                        trees = tune(), min_n = tune(), learn_rate = tune(),
                        tree_depth = tune()),
    rand_forest = rand_forest(mode = "classification", engine = "ranger",
                             trees = tune(), min_n = tune(), mtry = tune()),
    stop("Unknown model_type: ", model_type)
  )

  workflow() %>%
    add_recipe(recipe) %>%
    add_model(spec)
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

#' Save baseline config to file (for reference)
save_baseline_config <- function() {
  if (!dir.exists(CONFIG_DIR)) dir.create(CONFIG_DIR, recursive = TRUE)
  rows <- list()
  for (mt in MODEL_TYPES) {
    s <- BASELINE_SPECS[[mt]]
    for (nm in names(s)) {
      if (nm != "note") {
        rows[[length(rows) + 1]] <- tibble(
          Config_Type = "baseline",
          Model = mt,
          Parameter = nm,
          Value = as.character(s[[nm]])
        )
      }
    }
  }
  write_csv(bind_rows(rows), file.path(CONFIG_DIR, "model_config_baseline.csv"))
  message("Saved baseline config to config/model_config_baseline.csv")
}

#' Run baseline models and return comparison
run_baseline <- function(train_data, matchup_data, test_years) {
  message("\n========== BASELINE (fixed parameters) ==========")
  comparison <- tibble(
    Config = character(),
    Model = character(),
    Accuracy_Pct = numeric(),
    LogLoss = numeric(),
    N_Games = integer()
  )
  train_fct <- train_data %>% mutate(outcome = factor(outcome, levels = c(0, 1), labels = c("Lose", "Win")))
  for (mt in MODEL_TYPES) {
    message("\n--- Baseline ", mt, " ---")
    wf <- build_baseline_workflow(mt, train_fct)
    model <- tryCatch(
      fit(wf, data = train_fct),
      error = function(e) {
        message("  Error: ", conditionMessage(e))
        return(NULL)
      }
    )
    if (is.null(model)) next
    saveRDS(model, file.path(MODELS_DIR, paste0("bracket_model_", mt, "_baseline.rds")))
    eval <- evaluate_model(model, matchup_data, test_years)
    if (!is.null(eval)) {
      message("  Holdout accuracy: ", round(eval$accuracy * 100, 2), "% | Log loss: ", round(eval$log_loss, 4))
      comparison <- bind_rows(comparison, tibble(
        Config = "baseline",
        Model = mt,
        Accuracy_Pct = round(eval$accuracy * 100, 2),
        LogLoss = round(eval$log_loss, 4),
        N_Games = eval$n_games
      ))
    }
  }
  comparison
}

#' Run tuned models and return comparison + best params
run_tuned <- function(train_data, matchup_data, test_years) {
  message("\n========== TUNED (hyperparameter search) ==========")
  if (!dir.exists(CONFIG_DIR)) dir.create(CONFIG_DIR, recursive = TRUE)
  set.seed(42)
  train_fct <- train_data %>% mutate(outcome = factor(outcome, levels = c(0, 1), labels = c("Lose", "Win")))
  folds <- vfold_cv(train_fct, v = 5, strata = outcome)
  comparison <- tibble()
  tuned_params <- list()

  for (mt in MODEL_TYPES) {
    message("\n--- Tuning ", mt, " ---")
    wf <- build_tuned_workflow(mt, train_fct)

    grid <- switch(mt,
      glm = grid_regular(penalty(), mixture(), levels = 3),
      xgboost = grid_latin_hypercube(
        trees(range = c(100, 500)),
        min_n(range = c(2, 10)),
        learn_rate(range = c(-2, -0.5), trans = log10_trans()),
        tree_depth(range = c(3, 8)),
        size = 12
      ),
      rand_forest = grid_latin_hypercube(
        trees(range = c(300, 800)),
        min_n(range = c(2, 15)),
        mtry(range = c(2, 8)),
        size = 12
      )
    )

    metrics <- metric_set(accuracy, roc_auc, mn_log_loss)
    res <- tryCatch(
      tune_grid(wf, resamples = folds, grid = grid,
                metrics = metrics,
                control = control_grid(verbose = TRUE)),
      error = function(e) {
        message("  Tuning error: ", conditionMessage(e))
        return(NULL)
      }
    )
    if (is.null(res)) next

    best <- select_best(res, metric = "mn_log_loss")
    tuned_params[[mt]] <- best
    message("  Best params: ", paste(names(best), "=", best, collapse = ", "))

    final_wf <- finalize_workflow(wf, best)
    model <- fit(final_wf, data = train_fct)
    saveRDS(model, file.path(MODELS_DIR, paste0("bracket_model_", mt, ".rds")))

    eval <- evaluate_model(model, matchup_data, test_years)
    if (!is.null(eval)) {
      message("  Holdout accuracy: ", round(eval$accuracy * 100, 2), "% | Log loss: ", round(eval$log_loss, 4))
      comparison <- bind_rows(comparison, tibble(
        Config = "tuned",
        Model = mt,
        Accuracy_Pct = round(eval$accuracy * 100, 2),
        LogLoss = round(eval$log_loss, 4),
        N_Games = eval$n_games
      ))
    }
  }

  # Save tuned config
  rows <- list()
  for (mt in names(tuned_params)) {
    b <- tuned_params[[mt]]
    for (i in seq_along(b)) {
      rows[[length(rows) + 1]] <- tibble(
        Config_Type = "tuned",
        Model = mt,
        Parameter = names(b)[i],
        Value = as.character(b[[i]])
      )
    }
  }
  if (length(rows) > 0) {
    write_csv(bind_rows(rows), file.path(CONFIG_DIR, "model_config_tuned.csv"))
    message("\nSaved tuned config to config/model_config_tuned.csv")
  }
  list(comparison = comparison, tuned_params = tuned_params)
}

#' Run ensemble: blend tuned model predictions, optimize weights by log loss
#' @return List with ensemble eval and (if better) the ensemble object to save
run_ensemble <- function(matchup_data, test_years, tuned_comp) {
  message("\n========== ENSEMBLE (blend tuned models) ==========")

  # Load tuned models (prefer tuned over baseline)
  models <- list()
  for (mt in MODEL_TYPES) {
    path <- file.path(MODELS_DIR, paste0("bracket_model_", mt, ".rds"))
    if (file.exists(path)) {
      models[[mt]] <- readRDS(path)
    }
  }
  if (length(models) < 2) {
    message("Need at least 2 tuned models for ensemble. Skipping.")
    return(NULL)
  }

  test_data <- matchup_data %>%
    filter(Season %in% test_years) %>%
    mutate(outcome = factor(outcome, levels = c(0, 1), labels = c("Lose", "Win")))
  if (nrow(test_data) == 0) return(NULL)

  # Get predictions from each model
  preds <- list()
  for (nm in names(models)) {
    p <- predict(models[[nm]], test_data, type = "prob")
    preds[[nm]] <- as.numeric(p$.pred_Win)
  }
  pred_df <- bind_cols(preds)
  outcome_num <- as.integer(test_data$outcome == "Win")
  eps <- 1e-15

  # Optimize weights to minimize log loss (weights >= 0, sum = 1)
  n_models <- ncol(pred_df)
  obj <- function(w) {
    w <- pmax(0, w)
    w <- w / sum(w)
    prob <- as.numeric(as.matrix(pred_df) %*% matrix(w, ncol = 1))
    prob <- pmax(eps, pmin(1 - eps, prob))
    -mean(outcome_num * log(prob) + (1 - outcome_num) * log(1 - prob))
  }
  # Start from inverse log-loss weights (aligned to pred_df column order)
  ll_vals <- tuned_comp$LogLoss[match(names(models), tuned_comp$Model)]
  ll_vals <- replace(ll_vals, is.na(ll_vals), 0.6)
  inv_ll <- 1 / pmax(ll_vals, 0.01)
  w0 <- inv_ll / sum(inv_ll)
  opt <- optim(w0, obj, method = "L-BFGS-B", lower = rep(0, n_models), upper = rep(1, n_models),
               control = list(fnscale = 1))
  opt_weights <- pmax(0, opt$par)
  opt_weights <- opt_weights / sum(opt_weights)
  names(opt_weights) <- names(models)

  # Compute ensemble predictions and metrics
  prob_win <- as.numeric(as.matrix(pred_df) %*% opt_weights)
  prob_win <- pmax(eps, pmin(1 - eps, prob_win))
  pred_class <- as.integer(prob_win >= 0.5)
  correct <- pred_class == outcome_num
  accuracy <- mean(correct)
  log_loss <- -mean(outcome_num * log(prob_win) + (1 - outcome_num) * log(1 - prob_win))

  message("  Optimized weights: ", paste(sprintf("%s=%.3f", names(opt_weights), opt_weights), collapse = ", "))
  message("  Holdout accuracy: ", round(accuracy * 100, 2), "% | Log loss: ", round(log_loss, 4))

  ensemble_obj <- list(
    type = "ensemble",
    models = models,
    weights = opt_weights,
    model_names = names(models)
  )
  class(ensemble_obj) <- c("ensemble_model", "list")

  list(
    comparison = tibble(
      Config = "ensemble",
      Model = "ensemble",
      Accuracy_Pct = round(accuracy * 100, 2),
      LogLoss = round(log_loss, 4),
      N_Games = nrow(test_data)
    ),
    ensemble = ensemble_obj,
    weights = opt_weights
  )
}

#' Main training pipeline
main <- function() {
  if (!dir.exists(MODELS_DIR)) dir.create(MODELS_DIR, recursive = TRUE)
  if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

  matchup_path <- file.path(PROC_DIR, "matchup_data.csv")
  if (!file.exists(matchup_path)) stop("Processed data not found. Run 02_process_data.R first.")

  message("Loading processed data...")
  matchup_data <- read_csv(matchup_path, show_col_types = FALSE) %>%
    filter(Season >= 2008, Season <= 2025)

  feat_cols <- intersect(c(BASE_FEATURE_COLS, KENPOM_FEATURE_COLS), names(matchup_data))
  n_before <- nrow(matchup_data)
  matchup_data <- matchup_data %>%
    filter(!is.na(outcome) & !is.infinite(outcome)) %>%
    filter(if_all(any_of(feat_cols), ~!is.na(.) & !is.infinite(.)))
  if (n_before != nrow(matchup_data)) {
    message("Dropped ", n_before - nrow(matchup_data), " rows with NA/Inf.")
  }
  if (nrow(matchup_data) < 100) stop("Insufficient training data.")

  train_data <- matchup_data %>% filter(Season <= TRAIN_SEASONS_END)
  if (nrow(train_data) == 0) train_data <- matchup_data %>% filter(Season < max(Season))
  test_years <- TEST_SEASONS

  # Ensure baseline config exists
  save_baseline_config()

  # Run baseline
  baseline_comp <- run_baseline(train_data, matchup_data, test_years)
  write_csv(baseline_comp, file.path(OUTPUT_DIR, "model_comparison_baseline.csv"))

  # Run tuned
  tuned_out <- run_tuned(train_data, matchup_data, test_years)
  tuned_comp <- tuned_out$comparison
  write_csv(tuned_comp, file.path(OUTPUT_DIR, "model_comparison_tuned.csv"))

  # Run ensemble (blend tuned models)
  ensemble_out <- run_ensemble(matchup_data, test_years, tuned_comp)
  ensemble_comp <- tibble()
  if (!is.null(ensemble_out)) {
    ensemble_comp <- ensemble_out$comparison
    write_csv(tibble(Model = names(ensemble_out$weights), Weight = ensemble_out$weights),
              file.path(CONFIG_DIR, "ensemble_weights.csv"))
    message("\nSaved ensemble weights to config/ensemble_weights.csv")
  }

  # Combined comparison
  both <- bind_rows(baseline_comp, tuned_comp, ensemble_comp) %>%
    select(Config, Model, Accuracy_Pct, LogLoss, N_Games)
  write_csv(both, file.path(OUTPUT_DIR, "model_comparison.csv"))
  message("\n--- Baseline vs Tuned vs Ensemble Comparison ---")
  print(both)

  # Save best model (ensemble preferred if best; else tuned; else baseline)
  all_comp <- bind_rows(
    baseline_comp %>% mutate(Source = "baseline"),
    tuned_comp %>% mutate(Source = "tuned"),
    ensemble_comp %>% mutate(Source = "ensemble")
  )
  best_row <- all_comp %>% slice_min(LogLoss, n = 1)
  best_type <- best_row$Model[1]
  best_source <- best_row$Source[1]

  if (best_source == "ensemble" && !is.null(ensemble_out)) {
    best_model <- ensemble_out$ensemble
    saveRDS(best_model, file.path(MODELS_DIR, "bracket_model.rds"))
    message("\nBest model: ensemble -> saved as bracket_model.rds")
  } else {
    model_file <- if (best_source == "tuned") {
      file.path(MODELS_DIR, paste0("bracket_model_", best_type, ".rds"))
    } else {
      file.path(MODELS_DIR, paste0("bracket_model_", best_type, "_baseline.rds"))
    }
    best_model <- readRDS(model_file)
    saveRDS(best_model, file.path(MODELS_DIR, "bracket_model.rds"))
    message("\nBest model: ", best_type, " (", best_source, ") -> saved as bracket_model.rds")
  }

  if (best_source == "glm" && inherits(best_model, "workflow")) {
    eval <- evaluate_model(best_model, matchup_data, test_years)
    if (!is.null(eval)) write_csv(eval$predictions, file.path(OUTPUT_DIR, "validation_predictions.csv"))
  }

  # Update model tracker (BEST_MODELS.md)
  save_best_models_report(baseline_comp, tuned_comp, ensemble_out, best_row, test_years)

  message("\nTraining complete. Compare config/model_config_baseline.csv vs config/model_config_tuned.csv")
}

#' Update BEST_MODELS.md with baseline, tuned, ensemble results and weights
save_best_models_report <- function(baseline_comp, tuned_comp, ensemble_out, best_row, test_years) {
  out_path <- file.path(OUTPUT_DIR, "BEST_MODELS.md")
  today <- format(Sys.Date(), "%Y-%m-%d")
  n_games <- if (nrow(baseline_comp) > 0) baseline_comp$N_Games[1] else 63

  # Baseline models table
  baseline_md <- ""
  if (nrow(baseline_comp) > 0) {
    baseline_md <- "| Model       | Config   | Accuracy | Log Loss |\n|-------------|----------|----------|----------|\n"
    for (i in seq_len(nrow(baseline_comp))) {
      r <- baseline_comp[i, ]
      baseline_md <- paste0(baseline_md, "| ", r$Model, " | baseline | ", r$Accuracy_Pct, "% | ",
                           round(r$LogLoss, 4), " |\n")
    }
  }

  # Tuned models table
  tuned_md <- ""
  if (nrow(tuned_comp) > 0) {
    tuned_md <- "| Model       | Config | Accuracy | Log Loss |\n|-------------|--------|----------|----------|\n"
    for (i in seq_len(nrow(tuned_comp))) {
      r <- tuned_comp[i, ]
      tuned_md <- paste0(tuned_md, "| ", r$Model, " | tuned | ", r$Accuracy_Pct, "% | ",
                         round(r$LogLoss, 4), " |\n")
    }
  }

  # Best model and ensemble section
  best_model_name <- best_row$Model[1]
  best_config <- best_row$Source[1]
  best_acc <- best_row$Accuracy_Pct[1]
  best_ll <- best_row$LogLoss[1]

  ensemble_md <- ""
  if (!is.null(ensemble_out)) {
    ec <- ensemble_out$comparison
    ew <- ensemble_out$weights
    ensemble_md <- paste0(
      "\n---\n\n## Ensemble Results\n\n",
      "*Blended predictions from tuned GLM, XGBoost, and Random Forest. ",
      "Weights optimized to minimize log loss on holdout.*\n\n",
      "| Metric   | Accuracy | Log Loss | N Games |\n",
      "|----------|----------|----------|--------|\n",
      "| Ensemble | ", ec$Accuracy_Pct[1], "% | ", round(ec$LogLoss[1], 4), " | ", ec$N_Games[1], " |\n\n",
      "### Ensemble Weights\n\n",
      "| Model       | Weight  |\n",
      "|-------------|--------|\n"
    )
    for (i in seq_along(ew)) {
      ensemble_md <- paste0(ensemble_md, "| ", names(ew)[i], " | ",
                             sprintf("%.3f", ew[i]), " |\n")
    }
    ensemble_md <- paste0(ensemble_md, "\n*Weights updated ", today, "*\n")
  }

  content <- paste0(
    "# March Madness Model Performance\n\n",
    "*Updated ", today, " — holdout: ", paste(test_years, collapse = ", "), ", ", n_games, " games*\n\n",
    "---\n\n",
    "## Baseline Reference (Original Feature Set)\n\n",
    "**This section is fixed and should never change.** It preserves the original baseline metrics ",
    "from the initial model configuration (seed, winpct, KenPom features only—before H2H, SOS, round, rest).\n\n",
    "| Model       | Config   | Accuracy | Log Loss |\n",
    "|-------------|----------|----------|----------|\n",
    "| glm         | baseline | 74.6%    | 0.5425   |\n",
    "| xgboost     | baseline | 68.2%    | 0.6609   |\n",
    "| rand_forest | baseline | 68.2%    | 0.5499   |\n\n",
    "*2024 holdout, 63 games*\n\n",
    "---\n\n",
    "## Baseline Models\n\n",
    "*Current run — fixed parameters.*\n\n",
    baseline_md, "\n",
    "---\n\n",
    "## Tuned Models\n\n",
    "*Current run — hyperparameter tuned.*\n\n",
    tuned_md, "\n",
    "---\n\n",
    "## Best Model\n\n",
    "*Selected by lowest log loss.*\n\n",
    "| Metric         | Model       | Config   | Accuracy | Log Loss |\n",
    "|----------------|-------------|----------|----------|----------|\n",
    "| Best (log loss)| ", best_model_name, " | ", best_config, " | ", best_acc, "% | ",
    round(best_ll, 4), " |\n",
    ensemble_md
  )

  writeLines(content, out_path)
  message("Updated model tracker: output/BEST_MODELS.md")
}

main()
