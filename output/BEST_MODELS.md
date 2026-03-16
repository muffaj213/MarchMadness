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
| glm | baseline | 73.40 ± 14.05% | 0.5085 ± 0.1953 |
| glmnet | baseline | 73.94 ± 9.03% | 0.4949 ± 0.1097 |
| xgboost | baseline | 73.40 ± 16.69% | 0.5170 ± 0.1400 |
| rand_forest | baseline | 72.34 ± 10.64% | 0.5069 ± 0.0855 |

---

## Tuned Models

*Current run — hyperparameter tuned.*

| Model       | Config | Accuracy | Log Loss |
|-------------|--------|----------|----------|
| glm | tuned | 71.81 ± 13.60% | 0.4976 ± 0.1336 |
| glmnet | tuned | 73.94 ± 9.83% | 0.4966 ± 0.1191 |
| xgboost | tuned | 73.40 ± 10.91% | 0.5154 ± 0.0915 |
| rand_forest | tuned | 73.40 ± 9.35% | 0.5129 ± 0.0742 |

---

## Best Model

*Selected by lowest mean log loss across holdout years.*

| Metric         | Model       | Config   | Accuracy | Log Loss |
|----------------|-------------|----------|----------|----------|
| Best (log loss)| glmnet | baseline | 73.94 ± 9.03% | 0.4949 ± 0.1097 |

---

## Ensemble Results

*Blended predictions from baseline + tuned GLM, XGBoost, and Random Forest. Weights optimized on years 2019-2021 with entropy regularization (calibration disabled; overfits on ~120 games).*

| Metric   | Accuracy | Log Loss | N Games |
|----------|----------|----------|--------|
| Ensemble | 69.68 ± 12.35% | 1.5425 ± 0.8671 | 188 |

### Ensemble Weights

| Model       | Weight  |
|-------------|--------|
| glm_baseline | 0.080 |
| glm_tuned | 0.020 |
| glmnet_baseline | 0.010 |
| glmnet_tuned | 0.013 |
| xgboost_baseline | 0.052 |
| xgboost_tuned | 0.009 |
| rand_forest_baseline | 0.546 |
| rand_forest_tuned | 0.270 |

*Weights updated 2026-03-16*

