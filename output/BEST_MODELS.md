# March Madness Model Performance

*Updated 2026-03-13*

**Validation:** Time-based CV for tuning (expanding window by season). Holdout: 2022, 2023, 2024 (187 games total). Metrics show mean ± SD across holdout years when multiple.

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
| glm | baseline | 89.84 ± 17.98% | 0.1917 ± 0.3299 |
| glmnet | baseline | 89.84 ± 17.98% | 0.2033 ± 0.3139 |
| xgboost | baseline | 91.44 ± 15.14% | 0.2033 ± 0.2746 |
| rand_forest | baseline | 99.47 ± 0.95% | 0.1324 ± 0.1296 |

---

## Tuned Models

*Current run — hyperparameter tuned.*

| Model       | Config | Accuracy | Log Loss |
|-------------|--------|----------|----------|
| glm | tuned | 73.26 ± 3.96% | 0.4734 ± 0.0813 |
| glmnet | tuned | 89.30 ± 16.12% | 0.2976 ± 0.2651 |
| xgboost | tuned | 90.37 ± 17.04% | 0.2084 ± 0.2840 |
| rand_forest | tuned | 98.40 ± 2.84% | 0.1648 ± 0.1330 |

---

## Best Model

*Selected by lowest mean log loss across holdout years.*

| Metric         | Model       | Config   | Accuracy | Log Loss |
|----------------|-------------|----------|----------|----------|
| Best (log loss)| rand_forest | baseline | 99.47 ± 0.95% | 0.1324 ± 0.1296 |

---

## Ensemble Results

*Blended predictions from baseline + tuned GLM, XGBoost, and Random Forest. Weights optimized on years 2019-2021 with entropy regularization (calibration disabled; overfits on ~120 games).*

| Metric   | Accuracy | Log Loss | N Games |
|----------|----------|----------|--------|
| Ensemble | 90.91 ± 16.09% | 0.1814 ± 0.2690 | 187 |

### Ensemble Weights

| Model       | Weight  |
|-------------|--------|
| glm_baseline | 0.388 |
| glm_tuned | 0.000 |
| glmnet_baseline | 0.229 |
| glmnet_tuned | 0.006 |
| xgboost_baseline | 0.107 |
| xgboost_tuned | 0.109 |
| rand_forest_baseline | 0.117 |
| rand_forest_tuned | 0.045 |

*Weights updated 2026-03-13*

