# =============================================================================
# bracket_logic.R - Bracket structure and simulation for NCAA tournament
# =============================================================================
# Builds the 64-team single-elimination bracket from seeds/slots and
# simulates games using model predictions. Processes slots sequentially
# so Round 2+ games use winners from prior rounds.
# =============================================================================

library(dplyr)
library(purrr)

#' Predict method for ensemble_model (blends sub-model probabilities)
#' Applies Platt scaling calibration if object$calibration is present.
predict.ensemble_model <- function(object, new_data, type = "prob", ...) {
  if (type != "prob") stop("ensemble_model only supports type = 'prob'")
  probs <- matrix(NA_real_, nrow = nrow(new_data), ncol = length(object$models))
  for (i in seq_along(object$models)) {
    p <- predict(object$models[[i]], new_data, type = "prob")
    probs[, i] <- as.numeric(p$.pred_Win)
  }
  prob_win <- as.numeric(probs %*% object$weights)
  eps <- 1e-15
  prob_win <- pmax(eps, pmin(1 - eps, prob_win))
  if (!is.null(object$calibration) && requireNamespace("probably", quietly = TRUE)) {
    cal_df <- tibble::tibble(.pred_Win = prob_win, .pred_Lose = 1 - prob_win)
    calibrated <- probably::cal_apply(cal_df, object$calibration)
    prob_win <- as.numeric(calibrated$.pred_Win)
    prob_win <- pmax(eps, pmin(1 - eps, prob_win))
  }
  tibble::tibble(.pred_Lose = 1 - prob_win, .pred_Win = prob_win)
}

#' Check if a slot/seed ID refers to a first-round seed (e.g., W01, X16)
#' vs a prior-round slot (e.g., R1W1, R2W2)
is_seed_ref <- function(id) {
  id <- as.character(id)
  # Seeds: W01, X02, Y16, Z16 (region letter + number)
  # Slots: R1W1, R2W2, R3W1, R4W1, R5, R6
  grepl("^[WXYZ][0-9]+$", id, ignore.case = TRUE)
}

#' Get team ID for a seed in a given season
get_team_for_seed <- function(seed_id, seeds_df, season) {
  row <- seeds_df %>% filter(Season == season, Seed == as.character(seed_id))
  if (nrow(row) == 0) return(NA_integer_)
  row$TeamID[1]
}

#' Simulate entire bracket for a season
#'
#' Processes slots in dependency order. For each slot, resolves the two
#' teams (from seeds for R1, from prior slot winners for R2+), predicts
#' winner using the model, and advances winner to the next round.
#'
#' @param season Season year
#' @param slots_df MNCAATourneySlots (Slot, StrongSeed, WeakSeed)
#' @param seeds_df MNCAATourneySeeds (Season, Seed, TeamID)
#' @param model Fitted model
#' @param win_pct Win percentages
#' @param points_stats Points statistics
#' @param kenpom_stats Optional KenPom stats (Season, TeamID, adj_em, adj_o, adj_d, adj_t)
#' @param late_win_pct Optional late-season win pct (Season, TeamID, LateWinPct)
#' @param home_away_stats Optional home/away win rates (Season, TeamID, home_win_rate, away_win_rate)
#' @param resume_stats Optional NET/ELO/WAB (Season, TeamID, elo, net, wab)
#' @param deterministic If TRUE, pick higher-probability team; if FALSE, sample
#' @return List with slot_winners, game_results, champion
simulate_bracket <- function(season, slots_df, seeds_df, model,
                            win_pct, points_stats, kenpom_stats = NULL, late_win_pct = NULL,
                            recent_win_pct = NULL, recent_mov = NULL, home_away_stats = NULL, resume_stats = NULL,
                            head_to_head = NULL, sos_stats = NULL, rest_stats = NULL,
                            conference_stats = NULL, quadrant_stats = NULL, first_four_stats = NULL,
                            deterministic = FALSE) {
  # Load feature engineering (compute_matchup_features)
  fe_path <- here::here("src", "utils", "feature_engineering.R")
  if (file.exists(fe_path)) source(fe_path, local = TRUE)

  # Handle column names (Kaggle datasets vary)
  slots <- as.data.frame(slots_df)
  nm <- names(slots)
  if (length(nm) >= 3) {
    slots <- slots %>%
      rename(Slot = !!sym(nm[1]), Strong = !!sym(nm[2]), Weak = !!sym(nm[3]))
  }

  slots <- slots %>% arrange(Slot)
  slot_winners <- list()
  results <- list()

  for (i in seq_len(nrow(slots))) {
    slot <- as.character(slots$Slot[i])
    strong <- as.character(slots$Strong[i])
    weak <- as.character(slots$Weak[i])

    team_a <- if (is_seed_ref(strong)) {
      get_team_for_seed(strong, seeds_df, season)
    } else {
      slot_winners[[strong]]
    }

    team_b <- if (is_seed_ref(weak)) {
      get_team_for_seed(weak, seeds_df, season)
    } else {
      slot_winners[[weak]]
    }

    if (is.na(team_a) || is.na(team_b)) next

    # Derive round from slot (R1 -> 1, R2 -> 2, ..., R6 -> 6)
    round_num <- as.integer(sub("^R([0-9]+).*", "\\1", slot))
    if (is.na(round_num)) round_num <- 1L
    features <- compute_matchup_features(team_a, team_b, season, seeds_df, win_pct, points_stats, kenpom_stats, late_win_pct,
                                         head_to_head = head_to_head, sos_stats = sos_stats, rest_stats = rest_stats,
                                         home_away_stats = home_away_stats, resume_stats = resume_stats,
                                         recent_win_pct = recent_win_pct, recent_mov = recent_mov,
                                         conference_stats = conference_stats, quadrant_stats = quadrant_stats,
                                         first_four_stats = first_four_stats,
                                         round = round_num)

    pred <- tryCatch(
      {
        pred_prob <- NULL
        if (inherits(model, "ensemble_model")) {
          pred_prob <- predict(model, new_data = features, type = "prob")
        } else if (inherits(model, "workflow")) {
          pred_prob <- predict(model, new_data = features, type = "prob")
        } else if (inherits(model, "model_fit")) {
          pred_prob <- predict(model, features, type = "prob")
        }
        if (!is.null(pred_prob)) {
          prob_col <- if (".pred_Win" %in% names(pred_prob)) pred_prob$.pred_Win else pred_prob$.pred_1
          as.numeric(prob_col[1])
        } else {
          as.numeric(predict(model, as.data.frame(features), type = "response")[1])
        }
      },
      error = function(e) {
        message("Predict error for slot ", slot, ": ", conditionMessage(e))
        0.5
      }
    )

    prob_a_wins <- as.numeric(pred[1])
    if (is.na(prob_a_wins)) prob_a_wins <- 0.5

    if (deterministic) {
      winner <- if (prob_a_wins >= 0.5) team_a else team_b
    } else {
      winner <- if (runif(1) < prob_a_wins) team_a else team_b
    }

    slot_winners[[slot]] <- winner
    results[[length(results) + 1]] <- tibble::tibble(
      slot = slot,
      team_a = team_a,
      team_b = team_b,
      winner = winner,
      prob_a = prob_a_wins
    )
  }

  # Championship is the last slot (typically R6 or similar)
  last_slot <- slots$Slot[nrow(slots)]
  champion <- slot_winners[[as.character(last_slot)]]

  list(
    slot_winners = slot_winners,
    game_results = bind_rows(results),
    champion = champion
  )
}
