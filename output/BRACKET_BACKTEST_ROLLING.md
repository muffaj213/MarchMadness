# Rolling Bracket Backtest

Backtest seasons: 2018, 2019
Method: strict out-of-sample rolling fit per season (train only on seasons before test year).
Scoring: slot-accurate ESPN-style (exact slot winner required).
Comparisons: rolling model (deterministic), model_optimal (Monte Carlo expected-points), model_optimal_no_priors, and chalk baseline.
Model: xgboost baseline spec.
Feature profile: minimal
Seed-round priors enabled: FALSE
Monte Carlo sims per season for model_optimal: 25
Scoring: ESPN-style round weights (10, 20, 40, 80, 160, 320).

## Season Scores

| Season | Method | Train Through | Correct Games | Total Points | Max Points | Points % | Exact Map % | Round Fill % |
|---|---|---:|---:|---:|---:|---:|---:|---:|
| 2018 | chalk | NA | 36 | 810 | 1920 | 42.19% | 100.0% | 0.0% |
| 2019 | chalk | NA | 42 | 1240 | 1920 | 64.58% | 100.0% | 0.0% |
| 2018 | model | 2017 | 36 | 810 | 1920 | 42.19% | 100.0% | 0.0% |
| 2019 | model | 2018 | 44 | 940 | 1920 | 48.96% | 100.0% | 0.0% |
| 2018 | model_optimal | 2017 | 16 | 270 | 1920 | 14.06% | 100.0% | 0.0% |
| 2019 | model_optimal | 2018 | 8 | 80 | 1920 | 4.17% | 100.0% | 0.0% |
| 2018 | model_optimal_no_priors | 2017 | 16 | 270 | 1920 | 14.06% | 100.0% | 0.0% |
| 2019 | model_optimal_no_priors | 2018 | 8 | 80 | 1920 | 4.17% | 100.0% | 0.0% |

## Mean Across Seasons

- chalk: 39.00 / 63 games, 1025.0 / 1920 points (53.39%)
- model: 40.00 / 63 games, 875.0 / 1920 points (45.57%)
- model_optimal: 12.00 / 63 games, 175.0 / 1920 points (9.11%)
- model_optimal_no_priors: 12.00 / 63 games, 175.0 / 1920 points (9.11%)

## Upset Analysis

| Method | Mean Predicted Upsets |
|---|---:|
| chalk | 0.00 |
| model | 4.00 |
| model_optimal | 46.00 |
| model_optimal_no_priors | 46.00 |

## Scoring Mode Notes

- 2018 (chalk): slot_accurate; 2019 (chalk): slot_accurate; 2018 (model): slot_accurate; 2019 (model): slot_accurate; 2018 (model_optimal): slot_accurate; 2019 (model_optimal): slot_accurate; 2018 (model_optimal_no_priors): slot_accurate; 2019 (model_optimal_no_priors): slot_accurate

## 2023 Diagnostic (Model vs Chalk)

- Model points: NA | Chalk points: NA
- Model correct games: NA | Chalk correct games: NA

## Leakage Guard

- Enabled: TRUE; Seasons checked: 2024; Thresholds -> point_drop >= 200 OR r1_drop >= 4

