# =============================================================================
# 03_train_model.R - Train and validate NCAA tournament prediction model
# =============================================================================
# Trains glm, glmnet (LASSO), xgboost, rand_forest. Runs BASELINE and TUNED.
# (hyperparameter search) configurations. Saves both for comparison.
# Best model (by log loss) saved as bracket_model.rds for prediction.
# =============================================================================

library(here)
library(readr)
library(dplyr)
library(tidymodels)

source(here("src", "config.R"))
CONFIG_DIR <- here("config")

# Model configuration
# Validation strategy: time-based splits avoid overfitting to random folds
# - Tuning: expanding-window CV (train on past seasons, validate on next)
# - Stricter CV: use last 2-3 years only for validation (reduces overfitting to distant past)
# - Holdout: multiple years for mean +/- SD (reduces variance from single year)
# For production (e.g. predict 2026): train through 2025 for best generalization.
# Holdout years are reserved for out-of-sample evaluation/model selection only.
TRAIN_SEASONS_END <- 2025L  # Last season for final production refit
TEST_SEASONS <- c(2022L, 2023L, 2024L)  # True holdout years (mean +/- SD)
TUNE_VALIDATION_FIRST_YEAR <- 2020L  # Stricter: validate on 2020+ only (last 5 years)
TUNE_VALIDATION_LAST_N_YEARS <- 5L   # Max validation folds (recent years only)
WEIGHT_TUNE_YEARS <- c(2019L, 2020L, 2021L)  # Multiple years for weight tuning (avoids overfitting to single year)
ENTROPY_REGULARIZATION <- 0.03  # Penalize weight concentration; higher = more uniform weights
SKIP_ENSEMBLE_CALIBRATION <- FALSE  # Re-enabled now that holdout years are leakage-free
MODEL_TYPES <- c("glm", "glmnet", "xgboost", "rand_forest")

BASE_FEATURE_COLS <- c("seed_diff", "winpct_diff", "late_winpct_diff", "recent_winpct_diff", "recent_mov_diff",
                       "pf_diff", "round", "sos_diff", "conf_tourney_depth_diff",
                       "conf_em_diff", "quad1_winpct_diff", "quad12_winpct_diff", "first_four_rest_diff",
                       "deepest_run_diff", "tourney_h2h_team_a_winpct", "tourney_h2h_games",
                       "seed_diff_sq", "seed_sum", "is_upset_matchup", "upset_seed_gap",
                       "round_seed_interaction", "upset_winpct_diff", "tourney_winpct_diff",
                       "rest_diff", "h2h_team_a_winpct", "h2h_games",
                       "seed_barthag_interaction", "seed_recentmov_interaction")
EXTRA_FEATURE_COLS <- c("home_win_rate_diff", "away_win_rate_diff", "elo_diff", "net_diff", "wab_diff", "barthag_diff", "elite_sos_diff",
                        "fte_power_diff", "injury_rank_diff", "roster_rank_diff", "evan_killshots_margin_diff",
                        "three_share_diff", "three_point_mismatch", "close2_share_diff", "close2_point_mismatch",
                        "travel_miles_adv", "timezones_adv")
KENPOM_FEATURE_COLS <- c("adjem_diff", "adj_off_diff", "adj_def_diff", "tempo_diff", "luck_diff", "off_vs_def_adv",
                         "adjem_seed_interaction")

ROUND_WEIGHT_MAP <- c(`0` = 0.5, `1` = 1, `2` = 2, `3` = 4, `4` = 8, `5` = 16, `6` = 32)
RECENCY_WEIGHT_DECAY <- 0.08

build_case_weights <- function(data) {
  if (!"Season" %in% names(data)) return(rep(1, nrow(data)))
  max_season <- max(data$Season, na.rm = TRUE)
  round_chr <- if ("round" %in% names(data)) as.character(pmax(0L, pmin(6L, as.integer(replace(data$round, is.na(data$round), 1L))))) else "1"
  round_weight <- as.numeric(ROUND_WEIGHT_MAP[round_chr])
  round_weight[is.na(round_weight)] <- 1
  recency_weight <- exp(RECENCY_WEIGHT_DECAY * (as.numeric(data$Season) - max_season))
  w <- round_weight * recency_weight
  w / mean(w, na.rm = TRUE)
}

# -----------------------------------------------------------------------------
# BASELINE: Regularized to reduce overfitting (~63 games/year)
# Trees: fewer trees, higher min_n, shallower depth, early stopping
# -----------------------------------------------------------------------------
BASELINE_SPECS <- list(
  glm = list(
    penalty = 0,
    mixture = 0,
    engine = "glm",
    note = "Unregularized logistic regression"
  ),
  glmnet = list(
    penalty = 0.01,
    mixture = 1,
    engine = "glmnet",
    note = "LASSO (L1) for feature selection"
  ),
  xgboost = list(
    trees = 150,
    min_n = 15,
    learn_rate = 0.05,
    tree_depth = 3,
    engine = "xgboost",
    stop_iter = 10,
    note = "Regularized: shallow trees, early stopping"
  ),
  rand_forest = list(
    trees = 200,
    min_n = 30,
    engine = "ranger",
    note = "Regularized: fewer trees, much higher min_n"
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
  if ("case_wt" %in% names(matchup_data)) {
    recipe <- recipe %>% update_role(case_wt, new_role = "case_weights")
  }

  spec <- switch(model_type,
    glm = logistic_reg(penalty = 0, mixture = 0) %>% set_engine("glm"),
    glmnet = logistic_reg(penalty = 0.01, mixture = 1) %>% set_engine("glmnet"),
    xgboost = boost_tree(mode = "classification", engine = "xgboost",
                        trees = BASELINE_SPECS$xgboost$trees,
                        min_n = BASELINE_SPECS$xgboost$min_n,
                        learn_rate = BASELINE_SPECS$xgboost$learn_rate,
                        tree_depth = BASELINE_SPECS$xgboost$tree_depth,
                        stop_iter = BASELINE_SPECS$xgboost$stop_iter) %>%
      set_engine("xgboost", validation = 0.15),
    rand_forest = rand_forest(mode = "classification", engine = "ranger",
                             trees = BASELINE_SPECS$rand_forest$trees,
                             min_n = BASELINE_SPECS$rand_forest$min_n) %>%
      set_engine("ranger", importance = "impurity"),
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
  if ("case_wt" %in% names(matchup_data)) {
    recipe <- recipe %>% update_role(case_wt, new_role = "case_weights")
  }

  spec <- switch(model_type,
    glm = logistic_reg(penalty = tune(), mixture = tune()) %>% set_engine("glmnet"),
    glmnet = logistic_reg(penalty = tune(), mixture = 1) %>% set_engine("glmnet"),  # LASSO: mixture=1
    xgboost = boost_tree(mode = "classification", engine = "xgboost",
                        trees = tune(), min_n = tune(), learn_rate = tune(),
                        tree_depth = tune(), stop_iter = 10) %>%
      set_engine("xgboost", validation = 0.15),
    rand_forest = rand_forest(mode = "classification", engine = "ranger",
                             trees = tune(), min_n = tune(), mtry = tune()) %>%
      set_engine("ranger", importance = "impurity"),
    stop("Unknown model_type: ", model_type)
  )

  workflow() %>%
    add_recipe(recipe) %>%
    add_model(spec)
}

#' Create time-based resampling splits (expanding window by season)
#' Each fold: train on seasons < validation_year, validate on validation_year
#' Ensures no future data leaks into training.
#' Uses only last N years for validation (stricter CV to reduce overfitting).
make_time_folds <- function(data, first_validation_year = TUNE_VALIDATION_FIRST_YEAR,
                            last_n_years = TUNE_VALIDATION_LAST_N_YEARS) {
  if (!"Season" %in% names(data)) stop("data must have Season column")
  data <- data %>% arrange(Season)
  seasons <- sort(unique(data$Season))
  validation_years <- seasons[seasons >= first_validation_year]
  # Stricter: use only last N years for validation (reduces overfitting to distant past)
  if (length(validation_years) > last_n_years) {
    validation_years <- tail(validation_years, last_n_years)
    message("  Using last ", last_n_years, " validation years: ", paste(validation_years, collapse = ", "))
  }
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

#' Extract win probabilities from a fitted model object
predict_win_prob <- function(model, new_data) {
  pred <- predict(model, new_data, type = "prob")
  as.numeric(pred$.pred_Win)
}

#' Fit Platt scaling calibrator on a calibration set
#' @return list(intercept, slope) or NULL
fit_platt_calibrator <- function(model, calibration_data, min_rows = 40L) {
  if (is.null(calibration_data) || nrow(calibration_data) < min_rows) return(NULL)
  pred_prob <- tryCatch(predict_win_prob(model, calibration_data), error = function(e) NULL)
  if (is.null(pred_prob)) return(NULL)
  y <- as.integer(calibration_data$outcome == "Win")
  eps <- 1e-6
  p <- pmax(eps, pmin(1 - eps, pred_prob))
  z <- qlogis(p)
  fit <- tryCatch(glm(y ~ z, family = binomial()), error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  co <- coef(fit)
  if (length(co) < 2 || any(is.na(co))) return(NULL)
  if (abs(co[1]) > 5 || co[2] < 0.25 || co[2] > 3) return(NULL)
  raw_ll <- -mean(y * log(p) + (1 - y) * log(1 - p))
  p_cal <- plogis(co[1] + co[2] * z)
  cal_ll <- -mean(y * log(pmax(eps, pmin(1 - eps, p_cal))) + (1 - y) * log(pmax(eps, pmin(1 - eps, 1 - p_cal))))
  if (cal_ll >= raw_ll * 0.995) return(NULL)
  list(intercept = as.numeric(co[1]), slope = as.numeric(co[2]))
}

#' Apply Platt scaling coefficients to probabilities
apply_platt <- function(prob, calibrator) {
  if (is.null(calibrator)) return(prob)
  eps <- 1e-15
  p <- pmax(eps, pmin(1 - eps, as.numeric(prob)))
  plogis(calibrator$intercept + calibrator$slope * qlogis(p))
}

#' Evaluate model on held-out season(s); supports single or multiple years
#' @return List with accuracy, log_loss, and (for multi-year) accuracy_sd, log_loss_sd, per_year
evaluate_model <- function(model, matchup_data, test_seasons, calibrator = NULL) {
  test_data <- matchup_data %>%
    filter(Season %in% test_seasons) %>%
    mutate(outcome = factor(outcome, levels = c(0, 1), labels = c("Lose", "Win")))
  if (nrow(test_data) == 0) return(NULL)

  prob_win <- predict_win_prob(model, test_data)
  prob_win <- apply_platt(prob_win, calibrator)
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
  model_store <- list()
  calibrators <- list()
  train_fct <- train_data %>% mutate(outcome = factor(outcome, levels = c(0, 1), labels = c("Lose", "Win")))
  calibration_data <- matchup_data %>%
    filter(Season %in% WEIGHT_TUNE_YEARS) %>%
    mutate(outcome = factor(outcome, levels = c(0, 1), labels = c("Lose", "Win")))
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
    cal <- if (mt %in% c("glm", "glmnet")) fit_platt_calibrator(model, calibration_data) else NULL
    model_store[[mt]] <- model
    calibrators[[mt]] <- cal
    saveRDS(model, file.path(MODELS_DIR, paste0("bracket_model_", mt, "_baseline.rds")))
    eval <- evaluate_model(model, matchup_data, test_years, calibrator = cal)
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
  list(comparison = comparison, models = model_store, calibrators = calibrators)
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
  model_store <- list()
  calibrators <- list()
  calibration_data <- matchup_data %>%
    filter(Season %in% WEIGHT_TUNE_YEARS) %>%
    mutate(outcome = factor(outcome, levels = c(0, 1), labels = c("Lose", "Win")))

  for (mt in MODEL_TYPES) {
    message("\n--- Tuning ", mt, " ---")
    wf <- build_tuned_workflow(mt, train_fct)

    grid <- switch(mt,
      glm = dplyr::bind_rows(
        tibble(penalty = 0, mixture = 0),  # baseline in search space
        grid_regular(penalty(), mixture(), levels = 3)
      ),
      glmnet = grid_regular(penalty(), levels = 20),  # LASSO: tune penalty only (mixture=1 fixed)
      xgboost = dplyr::bind_rows(
        tibble(trees = 150, min_n = 15, learn_rate = 0.05, tree_depth = 3),  # regularized baseline
        grid_space_filling(
          trees(range = c(80, 200)),
          min_n(range = c(10, 25)),
          learn_rate(range = c(-2, -0.7), trans = log10_trans()),
          tree_depth(range = c(2, 4)),
          size = 11
        )
      ),
      rand_forest = dplyr::bind_rows(
        tibble(trees = 200, min_n = 30, mtry = 4),  # regularized baseline
        grid_space_filling(
          trees(range = c(100, 350)),
          min_n(range = c(20, 40)),
          mtry(range = c(2, 6)),
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
      glmnet = select_by_one_std_err(res, metric = "mn_log_loss", desc(penalty)),
      xgboost = select_by_one_std_err(res, metric = "mn_log_loss", desc(trees), min_n),
      rand_forest = select_by_one_std_err(res, metric = "mn_log_loss", desc(trees), min_n),
      select_best(res, metric = "mn_log_loss")
    )
    tuned_params[[mt]] <- best
    message("  Best params: ", paste(names(best), "=", best, collapse = ", "))

    final_wf <- finalize_workflow(wf, best)
    model <- fit(final_wf, data = train_fct)
    cal <- if (mt %in% c("glm", "glmnet")) fit_platt_calibrator(model, calibration_data) else NULL
    model_store[[mt]] <- model
    calibrators[[mt]] <- cal
    saveRDS(model, file.path(MODELS_DIR, paste0("bracket_model_", mt, ".rds")))

    eval <- evaluate_model(model, matchup_data, test_years, calibrator = cal)
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
  list(comparison = comparison, tuned_params = tuned_params, models = model_store, calibrators = calibrators)
}

#' Run ensemble via calibrated stacking meta-model.
#' @return List with ensemble eval and serialized ensemble object
run_ensemble <- function(matchup_data, test_years, baseline_comp, tuned_comp,
                         baseline_models, baseline_cals, tuned_models, tuned_cals,
                         weight_tune_years = WEIGHT_TUNE_YEARS) {
  message("\n========== ENSEMBLE (robust calibrated blend) ==========")
  ranked <- bind_rows(
    baseline_comp %>% transmute(name = paste0(Model, "_baseline"), LogLoss = LogLoss),
    tuned_comp %>% transmute(name = paste0(Model, "_tuned"), LogLoss = LogLoss)
  ) %>%
    filter(is.finite(LogLoss), LogLoss < 0.8) %>%
    arrange(LogLoss)
  eligible <- head(ranked$name, 4)

  models <- list()
  calibrators <- list()
  for (mt in MODEL_TYPES) {
    if (!is.null(baseline_models[[mt]]) && paste0(mt, "_baseline") %in% eligible) {
      nm <- paste0(mt, "_baseline")
      models[[nm]] <- baseline_models[[mt]]
      calibrators[[nm]] <- baseline_cals[[mt]]
    }
    if (!is.null(tuned_models[[mt]]) && paste0(mt, "_tuned") %in% eligible) {
      nm <- paste0(mt, "_tuned")
      models[[nm]] <- tuned_models[[mt]]
      calibrators[[nm]] <- tuned_cals[[mt]]
    }
  }
  if (length(models) < 2) {
    message("Need at least 2 models for ensemble. Skipping.")
    return(NULL)
  }
  message("  Pool: ", paste(names(models), collapse = ", "))
  weights <- rep(1 / length(models), length(models))
  names(weights) <- names(models)
  meta_coef <- NULL

  tune_data <- matchup_data %>%
    filter(Season %in% weight_tune_years) %>%
    mutate(outcome = factor(outcome, levels = c(0, 1), labels = c("Lose", "Win")))
  if (nrow(tune_data) >= 60) {
    preds_tune <- list()
    for (nm in names(models)) {
      p <- predict_win_prob(models[[nm]], tune_data)
      preds_tune[[nm]] <- apply_platt(p, calibrators[[nm]])
    }
    pred_tune_df <- bind_cols(preds_tune)
    meta_data <- pred_tune_df %>%
      mutate(outcome_num = as.integer(tune_data$outcome == "Win"))
    meta_fit <- tryCatch(
      glm(outcome_num ~ ., data = meta_data, family = binomial()),
      error = function(e) NULL
    )
    if (!is.null(meta_fit)) {
      cf <- coef(meta_fit)
      if (all(c("(Intercept)", names(models)) %in% names(cf))) {
        meta_coef <- as.numeric(cf[c("(Intercept)", names(models))])
      }
    }
  }

  test_data <- matchup_data %>%
    filter(Season %in% test_years) %>%
    mutate(outcome = factor(outcome, levels = c(0, 1), labels = c("Lose", "Win")))
  if (nrow(test_data) == 0) return(NULL)

  preds_test <- list()
  for (nm in names(models)) {
    p <- predict_win_prob(models[[nm]], test_data)
    preds_test[[nm]] <- apply_platt(p, calibrators[[nm]])
  }
  pred_test_df <- bind_cols(preds_test)
  eps <- 1e-15
  if (!is.null(meta_coef)) {
    x <- cbind(1, as.matrix(pred_test_df))
    prob_win <- as.numeric(plogis(x %*% matrix(meta_coef, ncol = 1)))
  } else {
    prob_win <- as.numeric(as.matrix(pred_test_df) %*% matrix(weights, ncol = 1))
  }
  prob_win <- pmax(eps, pmin(1 - eps, prob_win))

  pred_class <- as.integer(prob_win >= 0.5)
  test_data$pred_prob <- prob_win
  test_data$pred_class <- pred_class
  test_data$outcome_num <- as.integer(test_data$outcome == "Win")
  test_data$correct <- pred_class == test_data$outcome_num
  accuracy <- mean(test_data$correct)
  log_loss <- -mean(test_data$outcome_num * log(prob_win) + (1 - test_data$outcome_num) * log(1 - prob_win))

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

  if (!is.null(meta_coef)) {
    message("  Blend method: stacked logistic meta-learner on years ", paste(weight_tune_years, collapse = ", "))
  } else {
    message("  Blend weights: ", paste(sprintf("%s=%.3f", names(weights), weights), collapse = ", "))
  }
  msg <- "  Holdout accuracy: %.2f%% | Log loss: %.4f"
  if (!is.na(accuracy_sd)) msg <- paste0(msg, " (mean +/- SD)")
  message(sprintf(msg, accuracy * 100, log_loss))

  ensemble_obj <- list(
    type = "ensemble",
    models = models,
    calibrators = calibrators,
    weights = weights,
    meta_coef = meta_coef,
    model_names = names(models),
    calibration = NULL
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
    weights = weights
  )
}

#' Compute and save variable importance (permutation or model-based)
#' Uses best rand_forest model for tree importance; glmnet for coefficient-based selection.
save_feature_importance <- function(matchup_data, test_years) {
  message("\n========== FEATURE IMPORTANCE ==========")
  train_fct <- matchup_data %>%
    filter(Season <= TRAIN_SEASONS_END) %>%
    mutate(outcome = factor(outcome, levels = c(0, 1), labels = c("Lose", "Win")))
  all_feat <- c(BASE_FEATURE_COLS, KENPOM_FEATURE_COLS, EXTRA_FEATURE_COLS)
  avail <- intersect(all_feat, names(train_fct))
  if (length(avail) < 2) return(invisible(NULL))

  # 1. Random Forest: model-based importance (ranger impurity)
  rf_path <- file.path(MODELS_DIR, "bracket_model_rand_forest.rds")
  if (file.exists(rf_path) && requireNamespace("vip", quietly = TRUE)) {
    rf <- readRDS(rf_path)
    rf_fit <- extract_fit_parsnip(rf)
    vi_rf <- tryCatch(
      vip::vi(rf_fit, method = "model", sort = TRUE),
      error = function(e) {
        tryCatch(vip::vi(rf_fit, sort = TRUE), error = function(e2) NULL)
      }
    )
    if (!is.null(vi_rf) && nrow(vi_rf) > 0) {
      vi_rf <- vi_rf %>% mutate(Model = "rand_forest", .before = 1)
      write_csv(vi_rf, file.path(OUTPUT_DIR, "feature_importance_rf.csv"))
      message("  Saved rand_forest importance: output/feature_importance_rf.csv")
    }
  }

  # 2. GLMnet: non-zero coefficients (LASSO feature selection)
  glmnet_path <- file.path(MODELS_DIR, "bracket_model_glmnet.rds")
  if (file.exists(glmnet_path)) {
    glmnet_wf <- readRDS(glmnet_path)
    glmnet_fit <- extract_fit_parsnip(glmnet_wf)
    cf_tidy <- tryCatch(broom::tidy(glmnet_fit), error = function(e) NULL)
    if (!is.null(cf_tidy) && nrow(cf_tidy) > 0) {
      # Filter to non-intercept, non-zero coefficients (broom may add 'class' for logistic)
      glmnet_vi <- cf_tidy %>%
        filter(term != "(Intercept)", abs(estimate) > 1e-6) %>%
        rename(Variable = term, Coefficient = estimate) %>%
        select(Variable, Coefficient, any_of("penalty")) %>%
        mutate(Model = "glmnet") %>%
        arrange(desc(abs(Coefficient)))
      if (nrow(glmnet_vi) > 0) {
        write_csv(glmnet_vi, file.path(OUTPUT_DIR, "feature_importance_glmnet.csv"))
        message("  Saved glmnet non-zero coeffs: output/feature_importance_glmnet.csv (", nrow(glmnet_vi), " features)")
      }
    }
  }
  invisible(NULL)
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

  feat_cols <- intersect(c(BASE_FEATURE_COLS, KENPOM_FEATURE_COLS, EXTRA_FEATURE_COLS), names(matchup_data))
  n_before <- nrow(matchup_data)
  # Drop rows with invalid outcome (cannot train)
  matchup_data <- matchup_data %>%
    filter(!is.na(outcome) & !is.infinite(outcome))
  matchup_data$case_wt <- build_case_weights(matchup_data)
  # Impute NA/Inf in features (fix at source preferred; this is safety net for edge cases)
  for (col in feat_cols) {
    if (col %in% names(matchup_data)) {
      x <- matchup_data[[col]]
      bad <- is.na(x) | is.infinite(x)
      if (any(bad)) {
        matchup_data[[col]][bad] <- 0
        message("Imputed ", sum(bad), " NA/Inf in ", col, " (safety net)")
      }
    }
  }
  if (n_before != nrow(matchup_data)) {
    message("Dropped ", n_before - nrow(matchup_data), " rows with invalid outcome.")
  }
  if (nrow(matchup_data) < 100) stop("Insufficient training data.")

  test_years <- TEST_SEASONS
  selection_end <- as.integer(min(test_years) - 1L)
  train_data <- matchup_data %>% filter(Season <= selection_end)
  if (nrow(train_data) == 0) train_data <- matchup_data %>% filter(Season < min(test_years))
  if (nrow(train_data) == 0) train_data <- matchup_data %>% filter(Season < max(Season))
  full_train_data <- matchup_data %>% filter(Season <= TRAIN_SEASONS_END)
  n_2025 <- sum(matchup_data$Season == 2025L, na.rm = TRUE)
  if (TRAIN_SEASONS_END >= 2025L && n_2025 == 0) {
    message("NOTE: No 2025 tournament results in matchup_data. To include 2025 in training, ",
            "ensure data/raw_nishaa/Tournament Matchups.csv has 2025 results (or re-download nishaanamin/march-madness-data).")
  } else if (n_2025 > 0) {
    message("Training includes ", n_2025, " games from 2025 tournament.")
  }
  message("Model-selection train seasons: <= ", selection_end,
          " | Holdout seasons: ", paste(test_years, collapse = ", "),
          " | Final refit seasons: <= ", TRAIN_SEASONS_END)

  # Ensure baseline config exists
  save_baseline_config()

  # Run baseline
  baseline_out <- run_baseline(train_data, matchup_data, test_years)
  baseline_comp <- baseline_out$comparison
  write_csv(baseline_comp, file.path(OUTPUT_DIR, "model_comparison_baseline.csv"))

  # Run tuned
  tuned_out <- run_tuned(train_data, matchup_data, test_years)
  tuned_comp <- tuned_out$comparison
  write_csv(tuned_comp, file.path(OUTPUT_DIR, "model_comparison_tuned.csv"))

  # Run ensemble (blend baseline + tuned models)
  ensemble_out <- run_ensemble(
    matchup_data, test_years, baseline_comp, tuned_comp,
    baseline_models = baseline_out$models,
    baseline_cals = baseline_out$calibrators,
    tuned_models = tuned_out$models,
    tuned_cals = tuned_out$calibrators
  )
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

  # Select best model using strict holdout metrics
  all_comp <- bind_rows(
    baseline_comp %>% mutate(Source = "baseline"),
    tuned_comp %>% mutate(Source = "tuned"),
    ensemble_comp %>% mutate(Source = "ensemble")
  )
  best_row <- all_comp %>% slice_min(LogLoss, n = 1)
  best_type <- best_row$Model[1]
  best_source <- best_row$Source[1]

  # Refit chosen model on all available seasons through TRAIN_SEASONS_END for production inference.
  full_train_fct <- full_train_data %>%
    mutate(outcome = factor(outcome, levels = c(0, 1), labels = c("Lose", "Win")))
  if (best_source == "ensemble" && !is.null(ensemble_out)) {
    # Keep calibrated/optimized ensemble from strict model-selection stage.
    # (Weights are selected leakage-free; final refit of all submodels can be added later if needed.)
    best_model <- ensemble_out$ensemble
    saveRDS(best_model, file.path(MODELS_DIR, "bracket_model.rds"))
    message("\nBest model: ensemble (selection-stage calibrated) -> saved as bracket_model.rds")
  } else {
    if (best_source == "tuned") {
      wf <- build_tuned_workflow(best_type, full_train_fct)
      best_params <- tuned_out$tuned_params[[best_type]]
      if (is.null(best_params)) stop("Missing tuned parameters for best model: ", best_type)
      final_wf <- finalize_workflow(wf, best_params)
      fitted_model <- fit(final_wf, data = full_train_fct)
    } else {
      wf <- build_baseline_workflow(best_type, full_train_fct)
      fitted_model <- fit(wf, data = full_train_fct)
    }
    calib_data <- matchup_data %>%
      filter(Season %in% WEIGHT_TUNE_YEARS) %>%
      mutate(outcome = factor(outcome, levels = c(0, 1), labels = c("Lose", "Win")))
    final_cal <- fit_platt_calibrator(fitted_model, calib_data)
    best_model <- if (!is.null(final_cal)) {
      structure(list(model = fitted_model, calibrator = final_cal), class = c("calibrated_model", "list"))
    } else {
      fitted_model
    }
    saveRDS(best_model, file.path(MODELS_DIR, "bracket_model.rds"))
    message("\nBest model: ", best_type, " (", best_source, ") refit on seasons <= ", TRAIN_SEASONS_END,
            if (!is.null(final_cal)) " with Platt calibration" else "",
            " -> saved as bracket_model.rds")
  }

  if (best_source == "glm" && inherits(best_model, "workflow")) {
    eval <- evaluate_model(best_model, matchup_data, test_years)
    if (!is.null(eval)) write_csv(eval$predictions, file.path(OUTPUT_DIR, "validation_predictions.csv"))
  }

  # Update model tracker (BEST_MODELS.md)
  save_best_models_report(baseline_comp, tuned_comp, ensemble_out, best_row, test_years)

  # Feature importance (RF impurity, glmnet coefficients)
  save_feature_importance(matchup_data, test_years)

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
      "Weights optimized on years 2019-2021 with entropy regularization (calibration disabled; overfits on ~120 games).*\n\n",
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
