# Rolling Bracket Backtest

Backtest seasons: 2018, 2019, 2021, 2022, 2023, 2024
Method: strict out-of-sample rolling fit per season (train only on seasons before test year).
Scoring: slot-accurate ESPN-style (exact slot winner required).
Comparisons: rolling model vs chalk baseline.
Model: xgboost baseline spec.
Feature profile: full
Seed-round priors enabled: FALSE
Scoring: ESPN-style round weights (10, 20, 40, 80, 160, 320).

## Season Scores

| Season | Method | Train Through | Correct Games | Total Points | Max Points | Points % | Exact Map % | Round Fill % |
|---|---|---:|---:|---:|---:|---:|---:|---:|
| 2018 | chalk | NA | 36 | 810 | 1920 | 42.19% | 100.0% | 0.0% |
| 2019 | chalk | NA | 42 | 1240 | 1920 | 64.58% | 100.0% | 0.0% |
| 2021 | chalk | NA | 35 | 680 | 1920 | 35.42% | 96.8% | 0.0% |
| 2022 | chalk | NA | 37 | 940 | 1920 | 48.96% | 96.8% | 0.0% |
| 2023 | chalk | NA | 35 | 470 | 1920 | 24.48% | 100.0% | 0.0% |
| 2024 | chalk | NA | 38 | 730 | 1920 | 38.02% | 96.8% | 0.0% |
| 2018 | model | 2017 | 36 | 810 | 1920 | 42.19% | 100.0% | 0.0% |
| 2019 | model | 2018 | 41 | 920 | 1920 | 47.92% | 100.0% | 0.0% |
| 2021 | model | 2020 | 39 | 1030 | 1920 | 53.65% | 96.8% | 0.0% |
| 2022 | model | 2021 | 36 | 620 | 1920 | 32.29% | 96.8% | 0.0% |
| 2023 | model | 2022 | 35 | 470 | 1920 | 24.48% | 100.0% | 0.0% |
| 2024 | model | 2023 | 40 | 720 | 1920 | 37.50% | 96.8% | 0.0% |

## Mean Across Seasons

- chalk: 37.17 / 63 games, 811.7 / 1920 points (42.27%)
- model: 37.83 / 63 games, 761.7 / 1920 points (39.67%)

## Scoring Mode Notes

- 2018 (chalk): slot_accurate; 2019 (chalk): slot_accurate; 2021 (chalk): slot_accurate; 2022 (chalk): slot_accurate; 2023 (chalk): slot_accurate; 2024 (chalk): slot_accurate; 2018 (model): slot_accurate; 2019 (model): slot_accurate; 2021 (model): slot_accurate; 2022 (model): slot_accurate; 2023 (model): slot_accurate; 2024 (model): slot_accurate

## 2023 Diagnostic (Model vs Chalk)

- Model points: 470 | Chalk points: 470
- Model correct games: 35 | Chalk correct games: 35

## Leakage Guard

- Enabled: FALSE; Seasons checked: 2024; Thresholds -> point_drop >= 200 OR r1_drop >= 4

