# March Madness Model Performance

*Updated 2026-03-16*

**Validation:** Time-based CV for tuning (expanding window by season). Holdout: 2022, 2023, 2024 (188 games total). Metrics show mean ± SD across holdout years when multiple.

---

## Baseline Reference (Original Feature Set)

**This section is fixed and should never change.** It preserves the original baseline metrics from the initial model configuration (seed, winpct, KenPom features only—before H2H, SOS, round, rest).

| Model       | Config   | Accuracy | Log Loss |
|-------------|----------|----------|----------|
| glm         | baseline | 74.6%    | 0.5425   |
| xgboost     | baseline | 68.2%    | 0.6609   |
| rand_forest | baseline | 68.2%    | 0.5499   |

*2024 holdout, 63 games*

---

## Baseline Models

*Current run — fixed parameters.*

| Model       | Config   | Accuracy | Log Loss |
|-------------|----------|----------|----------|
| glm | baseline | 76.06 ± 14.47% | 0.5879 ± 0.3520 |
| glmnet | baseline | 72.87 ± 14.93% | 0.5421 ± 0.2165 |
| xgboost | baseline | 75.53 ± 12.22% | 0.4989 ± 0.1316 |
| rand_forest | baseline | 70.74 ± 10.05% | 0.5124 ± 0.0766 |

---

## Tuned Models

*Current run — hyperparameter tuned.*

| Model       | Config | Accuracy | Log Loss |
|-------------|--------|----------|----------|
| glm | tuned | 74.47 ± 12.31% | 0.5853 ± 0.2831 |
| glmnet | tuned | 73.40 ± 14.05% | 0.5545 ± 0.2360 |
| xgboost | tuned | 73.94 ± 10.92% | 0.5063 ± 0.1048 |
| rand_forest | tuned | 70.74 ± 6.75% | 0.5137 ± 0.0677 |

---

## Best Model

*Selected by lowest mean log loss across holdout years.*

| Metric         | Model       | Config   | Accuracy | Log Loss |
|----------------|-------------|----------|----------|----------|
| Best (log loss)| xgboost | baseline | 75.53 ± 12.22% | 0.4989 ± 0.1316 |

---

## Ensemble Results

*Blended predictions from baseline + tuned GLM, XGBoost, and Random Forest. Weights optimized on years 2019-2021 with entropy regularization (calibration disabled; overfits on ~120 games).*

| Metric   | Accuracy | Log Loss | N Games |
|----------|----------|----------|--------|
| Ensemble | 72.87 ± 10.01% | 0.5015 ± 0.0922 | 188 |

### Ensemble Weights

| Model       | Weight  |
|-------------|--------|
| xgboost_baseline | 0.250 |
| xgboost_tuned | 0.250 |
| rand_forest_baseline | 0.250 |
| rand_forest_tuned | 0.250 |

*Weights updated 2026-03-16*

