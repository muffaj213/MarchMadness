# Rolling Bracket Backtest

Backtest seasons: 2022, 2023, 2024
Method: strict out-of-sample rolling fit per season (train only on seasons before test year).
Model: xgboost baseline spec.
Scoring: ESPN-style round weights (10, 20, 40, 80, 160, 320).

## Season Scores

| Season | Train Through | Correct Games | Total Points | Max Points | Points % |
|---|---:|---:|---:|---:|---:|
| 2022 | 2021 | 62 | 1760 | 1920 | 91.67% |
| 2023 | 2022 | 33 | 480 | 1920 | 25.00% |
| 2024 | 2023 | 62 | 1760 | 1920 | 91.67% |

## Mean Across Seasons

- Mean correct games: 52.33 / 63
- Mean points: 1333.3 / 1920 (69.44%)

