# March Madness Model Performance

*Updated 2026-03-15*

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
| glm | baseline | 88.83 ± 19.56% | 0.2103 ± 0.3322 |
| glmnet | baseline | 90.43 ± 16.76% | 0.2130 ± 0.3101 |
| xgboost | baseline | 91.49 ± 14.90% | 0.2070 ± 0.2659 |
| rand_forest | baseline | 100.00 ± 0.00% | 0.1300 ± 0.1218 |

---

## Tuned Models

*Current run — hyperparameter tuned.*

| Model       | Config | Accuracy | Log Loss |
|-------------|--------|----------|----------|
| glm | tuned | 89.36 ± 18.62% | 0.2492 ± 0.2768 |
| glmnet | tuned | 90.43 ± 16.76% | 0.3086 ± 0.2539 |
| xgboost | tuned | 89.89 ± 17.69% | 0.2111 ± 0.2734 |
| rand_forest | tuned | 97.87 ± 3.72% | 0.1730 ± 0.1232 |

---

## Best Model

*Selected by lowest mean log loss across holdout years.*

| Metric         | Model       | Config   | Accuracy | Log Loss |
|----------------|-------------|----------|----------|----------|
| Best (log loss)| rand_forest | baseline | 100.00 ± 0.00% | 0.1300 ± 0.1218 |

---

## Ensemble Results

*Blended predictions from baseline + tuned GLM, XGBoost, and Random Forest. Weights optimized on years 2019-2021 with entropy regularization (calibration disabled; overfits on ~120 games).*

| Metric   | Accuracy | Log Loss | N Games |
|----------|----------|----------|--------|
| Ensemble | 90.96 ± 15.83% | 0.1853 ± 0.2435 | 188 |

### Ensemble Weights

| Model       | Weight  |
|-------------|--------|
| glm_baseline | 0.309 |
| glm_tuned | 0.045 |
| glmnet_baseline | 0.178 |
| glmnet_tuned | 0.004 |
| xgboost_baseline | 0.098 |
| xgboost_tuned | 0.098 |
| rand_forest_baseline | 0.210 |
| rand_forest_tuned | 0.058 |

*Weights updated 2026-03-15*

