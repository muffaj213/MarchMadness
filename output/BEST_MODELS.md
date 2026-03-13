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
| glm | baseline | 89.84 ± 17.98% | 0.1932 ± 0.3327 |
| glmnet | baseline | 89.84 ± 17.98% | 0.2033 ± 0.3139 |
| xgboost | baseline | 99.47 ± 0.95% | 0.0516 ± 0.0745 |
| rand_forest | baseline | 100.00 ± 0.00% | 0.0954 ± 0.0945 |

---

## Tuned Models

*Current run — hyperparameter tuned.*

| Model       | Config | Accuracy | Log Loss |
|-------------|--------|----------|----------|
| glm | tuned | 88.77 ± 19.88% | 0.2353 ± 0.2835 |
| glmnet | tuned | 90.37 ± 17.04% | 0.2244 ± 0.3053 |
| xgboost | tuned | 98.93 ± 1.89% | 0.0705 ± 0.1086 |
| rand_forest | tuned | 99.47 ± 0.95% | 0.1310 ± 0.1140 |

---

## Best Model

*Selected by lowest mean log loss across holdout years.*

| Metric         | Model       | Config   | Accuracy | Log Loss |
|----------------|-------------|----------|----------|----------|
| Best (log loss)| xgboost | baseline | 99.47 ± 0.95% | 0.0516 ± 0.0745 |

---

## Ensemble Results

*Blended predictions from baseline + tuned GLM, XGBoost, and Random Forest. Weights optimized on years 2019-2021 with entropy regularization (calibration disabled; overfits on ~120 games).*

| Metric   | Accuracy | Log Loss | N Games |
|----------|----------|----------|--------|
| Ensemble | 97.33 ± 4.73% | 0.1150 ± 0.1689 | 187 |

### Ensemble Weights

| Model       | Weight  |
|-------------|--------|
| glm_baseline | 0.213 |
| glm_tuned | 0.030 |
| glmnet_baseline | 0.129 |
| glmnet_tuned | 0.060 |
| xgboost_baseline | 0.218 |
| xgboost_tuned | 0.225 |
| rand_forest_baseline | 0.086 |
| rand_forest_tuned | 0.039 |

*Weights updated 2026-03-13*

