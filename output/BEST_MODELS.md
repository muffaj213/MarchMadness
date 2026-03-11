# March Madness Model Performance

*Updated 2026-03-11*

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
| glm | baseline | 97.33 ± 2.44% | 0.6326 ± 0.5729 |
| glmnet | baseline | 100.00 ± 0.00% | 0.0240 ± 0.0025 |
| xgboost | baseline | 100.00 ± 0.00% | 0.0328 ± 0.0075 |
| rand_forest | baseline | 100.00 ± 0.00% | 0.1052 ± 0.0021 |

---

## Tuned Models

*Current run — hyperparameter tuned.*

| Model       | Config | Accuracy | Log Loss |
|-------------|--------|----------|----------|
| glm | tuned | 98.93 ± 0.93% | 0.0357 ± 0.0342 |
| glmnet | tuned | 100.00 ± 0.00% | 0.0107 ± 0.0044 |
| xgboost | tuned | 100.00 ± 0.00% | 0.0114 ± 0.0024 |
| rand_forest | tuned | 100.00 ± 0.00% | 0.0825 ± 0.0015 |

---

## Best Model

*Selected by lowest mean log loss across holdout years.*

| Metric         | Model       | Config   | Accuracy | Log Loss |
|----------------|-------------|----------|----------|----------|
| Best (log loss)| glmnet | tuned | 100.00 ± 0.00% | 0.0107 ± 0.0044 |

---

## Ensemble Results

*Blended predictions from baseline + tuned GLM, XGBoost, and Random Forest. Weights optimized on years 2019-2021 with entropy regularization (calibration disabled; overfits on ~120 games).*

| Metric   | Accuracy | Log Loss | N Games |
|----------|----------|----------|--------|
| Ensemble | 100.00 ± 0.00% | 0.0285 ± 0.0092 | 187 |

### Ensemble Weights

| Model       | Weight  |
|-------------|--------|
| glm_baseline | 0.200 |
| glm_tuned | 0.195 |
| glmnet_baseline | 0.086 |
| glmnet_tuned | 0.133 |
| xgboost_baseline | 0.103 |
| xgboost_tuned | 0.151 |
| rand_forest_baseline | 0.055 |
| rand_forest_tuned | 0.078 |

*Weights updated 2026-03-11*

