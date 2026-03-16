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
| glm | baseline | 74.47 ± 13.34% | 0.4928 ± 0.1804 |
| glmnet | baseline | 75.00 ± 8.28% | 0.4746 ± 0.0918 |
| xgboost | baseline | 76.06 ± 10.66% | 0.5129 ± 0.0881 |
| rand_forest | baseline | 73.40 ± 9.35% | 0.5080 ± 0.0868 |

---

## Tuned Models

*Current run — hyperparameter tuned.*

| Model       | Config | Accuracy | Log Loss |
|-------------|--------|----------|----------|
| glm | tuned | 73.40 ± 10.91% | 0.4778 ± 0.1164 |
| glmnet | tuned | 75.53 ± 8.23% | 0.4768 ± 0.1024 |
| xgboost | tuned | 72.87 ± 10.26% | 0.5062 ± 0.1058 |
| rand_forest | tuned | 74.47 ± 9.14% | 0.5094 ± 0.0753 |

---

## Best Model

*Selected by lowest mean log loss across holdout years.*

| Metric         | Model       | Config   | Accuracy | Log Loss |
|----------------|-------------|----------|----------|----------|
| Best (log loss)| glmnet | baseline | 75.00 ± 8.28% | 0.4746 ± 0.0918 |

---

## Ensemble Results

*Blended predictions from baseline + tuned GLM, XGBoost, and Random Forest. Weights optimized on years 2019-2021 with entropy regularization (calibration disabled; overfits on ~120 games).*

| Metric   | Accuracy | Log Loss | N Games |
|----------|----------|----------|--------|
| Ensemble | 69.68 ± 11.45% | 1.6960 ± 0.8897 | 188 |

### Ensemble Weights

| Model       | Weight  |
|-------------|--------|
| glm_baseline | 0.083 |
| glm_tuned | 0.024 |
| glmnet_baseline | 0.012 |
| glmnet_tuned | 0.016 |
| xgboost_baseline | 0.003 |
| xgboost_tuned | 0.016 |
| rand_forest_baseline | 0.557 |
| rand_forest_tuned | 0.288 |

*Weights updated 2026-03-16*

