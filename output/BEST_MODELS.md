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
| glm | baseline | 70.90 ± 4.58% | 0.5496 ± 0.0415 |
| glmnet | baseline | 71.43 ± 1.59% | 0.5534 ± 0.0187 |
| xgboost | baseline | 66.67 ± 6.92% | 0.6661 ± 0.0918 |
| rand_forest | baseline | 73.02 ± 5.72% | 0.5554 ± 0.0439 |

---

## Tuned Models

*Current run — hyperparameter tuned.*

| Model       | Config | Accuracy | Log Loss |
|-------------|--------|----------|----------|
| glm | tuned | 68.78 ± 1.83% | 0.5853 ± 0.0195 |
| glmnet | tuned | 70.37 ± 2.42% | 0.5559 ± 0.0097 |
| xgboost | tuned | 72.49 ± 6.01% | 0.5567 ± 0.0356 |
| rand_forest | tuned | 71.96 ± 5.10% | 0.5511 ± 0.0316 |

---

## Best Model

*Selected by lowest mean log loss across holdout years.*

| Metric         | Model       | Config   | Accuracy | Log Loss |
|----------------|-------------|----------|----------|----------|
| Best (log loss)| glm | baseline | 70.90 ± 4.58% | 0.5496 ± 0.0415 |

---

## Ensemble Results

*Blended predictions from baseline + tuned GLM, XGBoost, and Random Forest. Weights optimized on years 2019-2021 with entropy regularization (calibration disabled; overfits on ~120 games).*

| Metric   | Accuracy | Log Loss | N Games |
|----------|----------|----------|--------|
| Ensemble | 66.67 ± 6.92% | 0.6304 ± 0.0834 | 189 |

### Ensemble Weights

| Model       | Weight  |
|-------------|--------|
| glm_baseline | 0.000 |
| glm_tuned | 0.000 |
| glmnet_baseline | 0.000 |
| glmnet_tuned | 0.000 |
| xgboost_baseline | 0.883 |
| xgboost_tuned | 0.000 |
| rand_forest_baseline | 0.094 |
| rand_forest_tuned | 0.023 |

*Weights updated 2026-03-09*

