# Rolling Bracket Backtest

Backtest seasons: 2018, 2019, 2021, 2022, 2023, 2024
Method: strict out-of-sample rolling fit per season (train only on seasons before test year).
Scoring: slot-accurate ESPN-style (exact slot winner required).
Comparisons: rolling model vs chalk baseline.
Model: xgboost baseline spec.
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
| 2018 | model | 2017 | 59 | 1770 | 1920 | 92.19% | 100.0% | 0.0% |
| 2019 | model | 2018 | 62 | 1840 | 1920 | 95.83% | 100.0% | 0.0% |
| 2021 | model | 2020 | 62 | 1900 | 1920 | 98.96% | 96.8% | 0.0% |
| 2022 | model | 2021 | 61 | 1600 | 1920 | 83.33% | 96.8% | 0.0% |
| 2023 | model | 2022 | 35 | 480 | 1920 | 25.00% | 100.0% | 0.0% |
| 2024 | model | 2023 | 62 | 1760 | 1920 | 91.67% | 96.8% | 0.0% |

## Mean Across Seasons

- chalk: 37.17 / 63 games, 811.7 / 1920 points (42.27%)
- model: 56.83 / 63 games, 1558.3 / 1920 points (81.16%)

## Scoring Mode Notes

- 2018 (chalk): slot_accurate; 2019 (chalk): slot_accurate; 2021 (chalk): slot_accurate; 2022 (chalk): slot_accurate; 2023 (chalk): slot_accurate; 2024 (chalk): slot_accurate; 2018 (model): slot_accurate; 2019 (model): slot_accurate; 2021 (model): slot_accurate; 2022 (model): slot_accurate; 2023 (model): slot_accurate; 2024 (model): slot_accurate

## 2023 Diagnostic (Model vs Chalk)

- Model points: 480 | Chalk points: 470
- Model correct games: 35 | Chalk correct games: 35

## Leakage Guard

- Enabled: TRUE; Seasons checked: 2024; Thresholds -> point_drop >= 200 OR r1_drop >= 4

