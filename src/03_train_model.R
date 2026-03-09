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
# Validation strategy: time-based splits avoid overfitting to random folds
# - Tuning: expanding-window CV (train on past seasons, validate on next)
# - Holdout: multiple years for mean +/- SD (reduces variance from single year)
# For production (e.g. predict 2025): set TRAIN_SEASONS_END=2024, TEST_SEASONS=2024
TRAIN_SEASONS_END <- 2021L  # Last season for training (test years must be > this)
TEST_SEASONS <- c(2022L, 2023L, 2024L)  # Holdout years for evaluation (mean +/- SD)
TUNE_VALIDATION_FIRST_YEAR <- 2015L  # First CV validation year (need ~7 train years before)
MODEL_TYPES <- c("glm", "xgboost", "rand_forest")

BASE_FEATURE_COLS <- c("seed_diff", "seed_diff_sq", "seed_sum", "winpct_diff", "late_winpct_diff", "recent_winpct_diff",
                       "is_upset_matchup", "upset_seed_gap", "seed_winpct_interaction", "pf_diff", "round",
                       "h2h_team_a_winpct", "sos_diff", "rest_diff")
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

#' Create time-based resampling splits (expanding window by season)
#' Each fold: train on seasons < validation_year, validate on validation_year
#' Ensures no future data leaks into training.
make_time_folds <- function(data, first_validation_year = TUNE_VALIDATION_FIRST_YEAR) {
  if (!"Season" %in% names(data)) stop("data must have Season column")
  data <- data %>% arrange(Season)
  seasons <- sort(unique(data$Season))
  validation_years <- seasons[seasons >= first_validation_year]
  if (length(validation_years) < 2) {
    message("Fewer than 2 validation years; falling back to vfold_cv")
    return(vfold_cv(data, v = min(5, nrow(data) %/% 20), strata = outcome))
  }

  splits <- list()
  ids <- character(length(validation_years))
  for (i in seq_along(validation_years)) {
    yr <- validation_years[i]
    analysis_idx <- which(data$Season < yr)
    assessment_idx <- which(data$Season == yr)
    if (length(analysis_idx) < 50 || length(assessment_idx) < 10) next
    sp <- rsample::make_splits(
      x = list(analysis = analysis_idx, assessment = assessment_idx),
      data = data
    )
    splits[[length(splits) + 1]] <- sp
    ids[length(splits)] <- paste0("Year_", yr)
  }
  if (length(splits) == 0) return(vfold_cv(data, v = 5, strata = outcome))
  rsample::manual_rset(splits, ids[seq_len(length(splits))])
}

#' Evaluate model on held-out season(s); supports single or multiple years
#' @return List with accuracy, log_loss, and (for multi-year) accuracy_sd, log_loss_sd, per_year
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

  out <- list(
    accuracy = accuracy, log_loss = log_loss,
    predictions = test_data, n_games = nrow(test_data)
  )

  # Multi-year: add per-year stats and SD
  if (length(test_seasons) > 1) {
    per_year <- test_data %>%
      group_by(Season) %>%
      summarise(
        n = n(),
        accuracy = mean(correct),
        log_loss = -mean(outcome_num * log(pmax(eps, pmin(1 - eps, pred_prob))) +
                          (1 - outcome_num) * log(pmax(eps, pmin(1 - eps, 1 - pred_prob)))),
        .groups = "drop"
      )
    out$accuracy_sd <- sd(per_year$accuracy)
    out$log_loss_sd <- sd(per_year$log_loss)
    out$per_year <- per_year
  }
  out
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
      msg <- "  Holdout accuracy: %.2f%% | Log loss: %.4f"
      if (!is.null(eval$accuracy_sd)) msg <- paste0(msg, " (mean across ", length(test_years), " years)")
      message(sprintf(msg, eval$accuracy * 100, eval$log_loss))
      comparison <- bind_rows(comparison, tibble(
        Config = "baseline",
        Model = mt,
        Accuracy_Pct = round(eval$accuracy * 100, 2),
        LogLoss = round(eval$log_loss, 4),
        Accuracy_SD = if (!is.null(eval$accuracy_sd)) round(eval$accuracy_sd * 100, 2) else NA_real_,
        LogLoss_SD = if (!is.null(eval$log_loss_sd)) round(eval$log_loss_sd, 4) else NA_real_,
        N_Games = eval$n_games
      ))
    }
  }
  comparison
}

#' Run tuned models and return comparison + best params
#' Uses time-based CV (expanding window by season) to avoid overfitting to random splits
run_tuned <- function(train_data, matchup_data, test_years) {
  message("\n========== TUNED (hyperparameter search) ==========")
  if (!dir.exists(CONFIG_DIR)) dir.create(CONFIG_DIR, recursive = TRUE)
  set.seed(42)
  train_fct <- train_data %>% mutate(outcome = factor(outcome, levels = c(0, 1), labels = c("Lose", "Win")))
  folds <- make_time_folds(train_fct, first_validation_year = TUNE_VALIDATION_FIRST_YEAR)
  message("  Using time-based CV: ", length(folds$splits), " folds (expanding window by season)")
  comparison <- tibble()
  tuned_params <- list()

  for (mt in MODEL_TYPES) {
    message("\n--- Tuning ", mt, " ---")
    wf <- build_tuned_workflow(mt, train_fct)

    grid <- switch(mt,
      glm = dplyr::bind_rows(
        tibble(penalty = 0, mixture = 0),  # baseline in search space
        grid_regular(penalty(), mixture(), levels = 3)
      ),
      xgboost = dplyr::bind_rows(
        tibble(trees = 200, min_n = 5, learn_rate = 0.1, tree_depth = 6),  # baseline
        grid_space_filling(
          trees(range = c(100, 500)),
          min_n(range = c(2, 10)),
          learn_rate(range = c(-2, -0.5), trans = log10_trans()),
          tree_depth(range = c(3, 8)),
          size = 11
        )
      ),
      rand_forest = dplyr::bind_rows(
        tibble(trees = 500, min_n = 5, mtry = 4),  # baseline
        grid_space_filling(
          trees(range = c(300, 800)),
          min_n(range = c(2, 15)),
          mtry(range = c(2, 8)),
          size = 11
        )
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

    best <- switch(mt,
      glm = select_by_one_std_err(res, metric = "mn_log_loss", desc(penalty)),
      xgboost = select_by_one_std_err(res, metric = "mn_log_loss", desc(trees), min_n),
      rand_forest = select_by_one_std_err(res, metric = "mn_log_loss", desc(trees), min_n),
      select_best(res, metric = "mn_log_loss")
    )
    tuned_params[[mt]] <- best
    message("  Best params: ", paste(names(best), "=", best, collapse = ", "))

    final_wf <- finalize_workflow(wf, best)
    model <- fit(final_wf, data = train_fct)
    saveRDS(model, file.path(MODELS_DIR, paste0("bracket_model_", mt, ".rds")))

    eval <- evaluate_model(model, matchup_data, test_years)
    if (!is.null(eval)) {
      msg <- "  Holdout accuracy: %.2f%% | Log loss: %.4f"
      if (!is.null(eval$accuracy_sd)) msg <- paste0(msg, " (mean across ", length(test_years), " years)")
      message(sprintf(msg, eval$accuracy * 100, eval$log_loss))
      comparison <- bind_rows(comparison, tibble(
        Config = "tuned",
        Model = mt,
        Accuracy_Pct = round(eval$accuracy * 100, 2),
        LogLoss = round(eval$log_loss, 4),
        Accuracy_SD = if (!is.null(eval$accuracy_sd)) round(eval$accuracy_sd * 100, 2) else NA_real_,
        LogLoss_SD = if (!is.null(eval$log_loss_sd)) round(eval$log_loss_sd, 4) else NA_real_,
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

#' Run ensemble: blend baseline + tuned model predictions, optimize weights by log loss
#' Weights tuned on weight_tune_year (last CV fold, e.g. 2021); evaluated on test_years.
#' @return List with ensemble eval and (if better) the ensemble object to save
run_ensemble <- function(matchup_data, test_years, baseline_comp, tuned_comp, weight_tune_year = TRAIN_SEASONS_END) {
  message("\n========== ENSEMBLE (blend baseline + tuned models) ==========")

  # Load both baseline and tuned models into pool
  models <- list()
  for (mt in MODEL_TYPES) {
    path_baseline <- file.path(MODELS_DIR, paste0("bracket_model_", mt, "_baseline.rds"))
    path_tuned <- file.path(MODELS_DIR, paste0("bracket_model_", mt, ".rds"))
    if (file.exists(path_baseline)) {
      models[[paste0(mt, "_baseline")]] <- readRDS(path_baseline)
    }
    if (file.exists(path_tuned)) {
      models[[paste0(mt, "_tuned")]] <- readRDS(path_tuned)
    }
  }
  if (length(models) < 2) {
    message("Need at least 2 models for ensemble. Skipping.")
    return(NULL)
  }
  message("  Pool: ", paste(names(models), collapse = ", "))
  message("  Weight tuning on year ", weight_tune_year, " (CV fold); evaluation on ", paste(test_years, collapse = ", "))

  # Data for weight tuning (last CV fold, not holdout)
  weight_tune_data <- matchup_data %>%
    filter(Season == weight_tune_year) %>%
    mutate(outcome = factor(outcome, levels = c(0, 1), labels = c("Lose", "Win")))
  if (nrow(weight_tune_data) < 10) {
    message("  Insufficient weight-tune data for year ", weight_tune_year, ". Falling back to first test year.")
    weight_tune_year <- test_years[1]
    weight_tune_data <- matchup_data %>%
      filter(Season == weight_tune_year) %>%
      mutate(outcome = factor(outcome, levels = c(0, 1), labels = c("Lose", "Win")))
  }

  # Get predictions from each model for weight tuning
  preds <- list()
  for (nm in names(models)) {
    p <- predict(models[[nm]], weight_tune_data, type = "prob")
    preds[[nm]] <- as.numeric(p$.pred_Win)
  }
  pred_df <- bind_cols(preds)
  outcome_num <- as.integer(weight_tune_data$outcome == "Win")
  eps <- 1e-15

  # Optimize weights to minimize log loss on weight-tune fold (weights >= 0, sum = 1)
  n_models <- ncol(pred_df)
  obj <- function(w) {
    w <- pmax(0, w)
    w <- w / sum(w)
    prob <- as.numeric(as.matrix(pred_df) %*% matrix(w, ncol = 1))
    prob <- pmax(eps, pmin(1 - eps, prob))
    -mean(outcome_num * log(prob) + (1 - outcome_num) * log(1 - prob))
  }
  # Start from inverse log-loss weights (baseline_comp for *_baseline, tuned_comp for *_tuned)
  ll_vals <- numeric(length(models))
  names(ll_vals) <- names(models)
  for (nm in names(models)) {
    base_name <- sub("_baseline$|_tuned$", "", nm)
    if (grepl("_baseline$", nm)) {
      ll_vals[nm] <- baseline_comp$LogLoss[match(base_name, baseline_comp$Model)]
    } else {
      ll_vals[nm] <- tuned_comp$LogLoss[match(base_name, tuned_comp$Model)]
    }
  }
  ll_vals <- replace(ll_vals, is.na(ll_vals), 0.6)
  inv_ll <- 1 / pmax(ll_vals, 0.01)
  w0 <- inv_ll / sum(inv_ll)
  opt <- optim(w0, obj, method = "L-BFGS-B", lower = rep(0, n_models), upper = rep(1, n_models),
               control = list(fnscale = 1))
  opt_weights <- pmax(0, opt$par)
  opt_weights <- opt_weights / sum(opt_weights)
  names(opt_weights) <- names(models)

  # Holdout data for evaluation (2022-2024, never used for weight tuning)
  test_data <- matchup_data %>%
    filter(Season %in% test_years) %>%
    mutate(outcome = factor(outcome, levels = c(0, 1), labels = c("Lose", "Win")))
  if (nrow(test_data) == 0) return(NULL)

  # Get predictions on holdout and apply weights
  test_preds <- list()
  for (nm in names(models)) {
    p <- predict(models[[nm]], test_data, type = "prob")
    test_preds[[nm]] <- as.numeric(p$.pred_Win)
  }
  test_pred_df <- bind_cols(test_preds)
  prob_win <- as.numeric(as.matrix(test_pred_df) %*% opt_weights)
  prob_win <- pmax(eps, pmin(1 - eps, prob_win))

  # Fit probability calibration on weight-tune fold (2021); apply only when beneficial
  # Skip when ensemble collapses to single model (calibration overfits easily on ~63 games)
  cal_obj <- NULL
  n_active <- sum(opt_weights > 0.05)
  if (requireNamespace("probably", quietly = TRUE) && nrow(weight_tune_data) >= 30 && n_active >= 2) {
    weight_tune_prob <- as.numeric(as.matrix(pred_df) %*% opt_weights)
    weight_tune_prob <- pmax(eps, pmin(1 - eps, weight_tune_prob))
    cal_df <- weight_tune_data %>%
      mutate(.pred_Lose = 1 - weight_tune_prob, .pred_Win = weight_tune_prob)
    cal_obj <- tryCatch(
      suppressWarnings(probably::cal_estimate_logistic(cal_df, outcome, c(.pred_Lose, .pred_Win), smooth = FALSE)),
      error = function(e) NULL
    )
    if (!is.null(cal_obj)) {
      wt_calibrated <- probably::cal_apply(cal_df, cal_obj)
      wt_ll_raw <- -mean(outcome_num * log(weight_tune_prob) + (1 - outcome_num) * log(1 - weight_tune_prob))
      wt_ll_cal <- -mean(outcome_num * log(pmax(eps, as.numeric(wt_calibrated$.pred_Win))) +
        (1 - outcome_num) * log(1 - pmin(1 - eps, as.numeric(wt_calibrated$.pred_Win))))
      if (wt_ll_cal < wt_ll_raw * 1.02) {
        test_cal_df <- tibble(.pred_Win = prob_win, .pred_Lose = 1 - prob_win)
        calibrated <- probably::cal_apply(test_cal_df, cal_obj)
        prob_win <- as.numeric(calibrated$.pred_Win)
        prob_win <- pmax(eps, pmin(1 - eps, prob_win))
        message("  Applied Platt scaling (calibration fit on year ", weight_tune_year, ")")
      } else {
        cal_obj <- NULL
      }
    }
  }

  pred_class <- as.integer(prob_win >= 0.5)
  test_data$pred_prob <- prob_win
  test_data$pred_class <- pred_class
  test_data$outcome_num <- as.integer(test_data$outcome == "Win")
  test_data$correct <- pred_class == test_data$outcome_num
  accuracy <- mean(test_data$correct)
  log_loss <- -mean(test_data$outcome_num * log(prob_win) + (1 - test_data$outcome_num) * log(1 - prob_win))

  # Per-year stats for multi-year holdout
  accuracy_sd <- NA_real_
  log_loss_sd <- NA_real_
  if ("Season" %in% names(test_data) && length(unique(test_data$Season)) > 1) {
    per_yr <- test_data %>%
      group_by(Season) %>%
      summarise(
        acc = mean(correct),
        ll = -mean(outcome_num * log(pmax(eps, pmin(1 - eps, pred_prob))) +
               (1 - outcome_num) * log(pmax(eps, pmin(1 - eps, 1 - pred_prob)))),
        .groups = "drop"
      )
    accuracy_sd <- sd(per_yr$acc) * 100
    log_loss_sd <- sd(per_yr$ll)
  }

  message("  Optimized weights: ", paste(sprintf("%s=%.3f", names(opt_weights), opt_weights), collapse = ", "))
  msg <- "  Holdout accuracy: %.2f%% | Log loss: %.4f"
  if (!is.na(accuracy_sd)) msg <- paste0(msg, " (mean +/- SD)")
  message(sprintf(msg, accuracy * 100, log_loss))

  ensemble_obj <- list(
    type = "ensemble",
    models = models,
    weights = opt_weights,
    model_names = names(models),
    calibration = cal_obj  # Platt scaling fit on weight_tune_year; NULL if probably unavailable
  )
  class(ensemble_obj) <- c("ensemble_model", "list")

  list(
    comparison = tibble(
      Config = "ensemble",
      Model = "ensemble",
      Accuracy_Pct = round(accuracy * 100, 2),
      LogLoss = round(log_loss, 4),
      Accuracy_SD = if (!is.na(accuracy_sd)) round(accuracy_sd, 2) else NA_real_,
      LogLoss_SD = if (!is.na(log_loss_sd)) round(log_loss_sd, 4) else NA_real_,
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

  # Run ensemble (blend baseline + tuned models)
  ensemble_out <- run_ensemble(matchup_data, test_years, baseline_comp, tuned_comp)
  ensemble_comp <- tibble()
  if (!is.null(ensemble_out)) {
    ensemble_comp <- ensemble_out$comparison
    write_csv(tibble(Model = names(ensemble_out$weights), Weight = ensemble_out$weights),
              file.path(CONFIG_DIR, "ensemble_weights.csv"))
    message("\nSaved ensemble weights to config/ensemble_weights.csv")
  }

  # Combined comparison (include SD columns when present)
  both <- bind_rows(baseline_comp, tuned_comp, ensemble_comp) %>%
    select(Config, Model, Accuracy_Pct, LogLoss, N_Games, any_of("Accuracy_SD"), any_of("LogLoss_SD"))
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

  fmt_metric <- function(val, sd_val) {
    if (length(sd_val) == 0 || is.na(sd_val)) sprintf("%.2f", val) else sprintf("%.2f ± %.2f", val, sd_val)
  }
  fmt_ll <- function(val, sd_val) {
    if (length(sd_val) == 0 || is.na(sd_val)) sprintf("%.4f", val) else sprintf("%.4f ± %.4f", val, sd_val)
  }

  # Baseline models table
  baseline_md <- ""
  if (nrow(baseline_comp) > 0) {
    hdr <- "| Model       | Config   | Accuracy | Log Loss |\n|-------------|----------|----------|----------|\n"
    baseline_md <- hdr
    for (i in seq_len(nrow(baseline_comp))) {
      r <- baseline_comp[i, ]
      acc_sd <- if ("Accuracy_SD" %in% names(r)) r$Accuracy_SD else NA
      ll_sd <- if ("LogLoss_SD" %in% names(r)) r$LogLoss_SD else NA
      baseline_md <- paste0(baseline_md, "| ", r$Model, " | baseline | ",
                             fmt_metric(r$Accuracy_Pct, acc_sd), "% | ",
                             fmt_ll(r$LogLoss, ll_sd), " |\n")
    }
  }

  # Tuned models table
  tuned_md <- ""
  if (nrow(tuned_comp) > 0) {
    hdr <- "| Model       | Config | Accuracy | Log Loss |\n|-------------|--------|----------|----------|\n"
    tuned_md <- hdr
    for (i in seq_len(nrow(tuned_comp))) {
      r <- tuned_comp[i, ]
      acc_sd <- if ("Accuracy_SD" %in% names(r)) r$Accuracy_SD else NA
      ll_sd <- if ("LogLoss_SD" %in% names(r)) r$LogLoss_SD else NA
      tuned_md <- paste0(tuned_md, "| ", r$Model, " | tuned | ",
                         fmt_metric(r$Accuracy_Pct, acc_sd), "% | ",
                         fmt_ll(r$LogLoss, ll_sd), " |\n")
    }
  }

  # Best model and ensemble section
  best_model_name <- best_row$Model[1]
  best_config <- best_row$Source[1]
  best_acc <- best_row$Accuracy_Pct[1]
  best_ll <- best_row$LogLoss[1]
  best_acc_sd <- if ("Accuracy_SD" %in% names(best_row)) best_row$Accuracy_SD[1] else NA
  best_ll_sd <- if ("LogLoss_SD" %in% names(best_row)) best_row$LogLoss_SD[1] else NA

  ensemble_md <- ""
  if (!is.null(ensemble_out)) {
    ec <- ensemble_out$comparison
    ew <- ensemble_out$weights
    ec_acc <- fmt_metric(ec$Accuracy_Pct[1], if ("Accuracy_SD" %in% names(ec)) ec$Accuracy_SD[1] else NA)
    ec_ll <- fmt_ll(ec$LogLoss[1], if ("LogLoss_SD" %in% names(ec)) ec$LogLoss_SD[1] else NA)
    ensemble_md <- paste0(
      "\n---\n\n## Ensemble Results\n\n",
      "*Blended predictions from baseline + tuned GLM, XGBoost, and Random Forest. ",
      "Weights optimized on last CV fold (2021); probabilities calibrated when beneficial (Platt).*\n\n",
      "| Metric   | Accuracy | Log Loss | N Games |\n",
      "|----------|----------|----------|--------|\n",
      "| Ensemble | ", ec_acc, "% | ", ec_ll, " | ", ec$N_Games[1], " |\n\n",
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
    "*Updated ", today, "*\n\n",
    "**Validation:** Time-based CV for tuning (expanding window by season). ",
    "Holdout: ", paste(test_years, collapse = ", "), " (", n_games, " games total). ",
    "Metrics show mean ± SD across holdout years when multiple.\n\n",
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
    "*Selected by lowest mean log loss across holdout years.*\n\n",
    "| Metric         | Model       | Config   | Accuracy | Log Loss |\n",
    "|----------------|-------------|----------|----------|----------|\n",
    "| Best (log loss)| ", best_model_name, " | ", best_config, " | ",
    fmt_metric(best_acc, best_acc_sd), "% | ", fmt_ll(best_ll, best_ll_sd), " |\n",
    ensemble_md
  )

  writeLines(content, out_path)
  message("Updated model tracker: output/BEST_MODELS.md")
}

main()
