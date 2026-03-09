# March Madness Model Performance

*Updated 2026-03-09*

**Validation:** Time-based CV for tuning (expanding window by season). Holdout: 2022, 2023, 2024 (189 games total). Metrics show mean ± SD across holdout years when multiple.

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
| glm | baseline | 71.96 ± 5.10% | 0.5499 ± 0.0369 |
| glmnet | baseline | 71.43 ± 1.59% | 0.5541 ± 0.0178 |
| xgboost | baseline | 66.67 ± 6.92% | 0.6681 ± 0.0933 |
| rand_forest | baseline | 72.49 ± 6.61% | 0.5606 ± 0.0352 |

---

## Tuned Models

*Current run — hyperparameter tuned.*

| Model       | Config | Accuracy | Log Loss |
|-------------|--------|----------|----------|
| glm | tuned | 68.78 ± 1.83% | 0.5854 ± 0.0163 |
| glmnet | tuned | 70.37 ± 2.42% | 0.5559 ± 0.0097 |
| xgboost | tuned | 71.43 ± 5.72% | 0.5629 ± 0.0255 |
| rand_forest | tuned | 74.60 ± 4.20% | 0.5547 ± 0.0366 |

---

## Best Model

*Selected by lowest mean log loss across holdout years.*

| Metric         | Model       | Config   | Accuracy | Log Loss |
|----------------|-------------|----------|----------|----------|
| Best (log loss)| glm | baseline | 71.96 ± 5.10% | 0.5499 ± 0.0369 |

---

## Ensemble Results

*Blended predictions from baseline + tuned GLM, XGBoost, and Random Forest. Weights optimized on years 2019-2021 with entropy regularization (calibration disabled; overfits on ~120 games).*

| Metric   | Accuracy | Log Loss | N Games |
|----------|----------|----------|--------|
| Ensemble | 67.20 ± 6.01% | 0.6275 ± 0.0829 | 189 |

### Ensemble Weights

| Model       | Weight  |
|-------------|--------|
| glm_baseline | 0.000 |
| glm_tuned | 0.000 |
| glmnet_baseline | 0.000 |
| glmnet_tuned | 0.000 |
| xgboost_baseline | 0.841 |
| xgboost_tuned | 0.000 |
| rand_forest_baseline | 0.134 |
| rand_forest_tuned | 0.025 |

*Weights updated 2026-03-09*

