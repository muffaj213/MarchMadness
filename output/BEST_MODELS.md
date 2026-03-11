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
| glm | baseline | 97.86 ± 1.86% | 0.4795 ± 0.4525 |
| glmnet | baseline | 100.00 ± 0.00% | 0.0240 ± 0.0025 |
| xgboost | baseline | 100.00 ± 0.00% | 0.0357 ± 0.0099 |
| rand_forest | baseline | 100.00 ± 0.00% | 0.0980 ± 0.0010 |

---

## Tuned Models

*Current run — hyperparameter tuned.*

| Model       | Config | Accuracy | Log Loss |
|-------------|--------|----------|----------|
| glm | tuned | 98.93 ± 0.93% | 0.0392 ± 0.0390 |
| glmnet | tuned | 99.47 ± 0.95% | 0.0107 ± 0.0045 |
| xgboost | tuned | 100.00 ± 0.00% | 0.0121 ± 0.0031 |
| rand_forest | tuned | 100.00 ± 0.00% | 0.0800 ± 0.0022 |

---

## Best Model

*Selected by lowest mean log loss across holdout years.*

| Metric         | Model       | Config   | Accuracy | Log Loss |
|----------------|-------------|----------|----------|----------|
| Best (log loss)| glmnet | tuned | 99.47 ± 0.95% | 0.0107 ± 0.0045 |

---

## Ensemble Results

*Blended predictions from baseline + tuned GLM, XGBoost, and Random Forest. Weights optimized on years 2019-2021 with entropy regularization (calibration disabled; overfits on ~120 games).*

| Metric   | Accuracy | Log Loss | N Games |
|----------|----------|----------|--------|
| Ensemble | 100.00 ± 0.00% | 0.0275 ± 0.0085 | 187 |

### Ensemble Weights

| Model       | Weight  |
|-------------|--------|
| glm_baseline | 0.199 |
| glm_tuned | 0.194 |
| glmnet_baseline | 0.086 |
| glmnet_tuned | 0.133 |
| xgboost_baseline | 0.103 |
| xgboost_tuned | 0.150 |
| rand_forest_baseline | 0.057 |
| rand_forest_tuned | 0.078 |

*Weights updated 2026-03-11*

