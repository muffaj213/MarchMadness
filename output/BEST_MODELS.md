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
| glm | baseline | 97.86 ± 1.86% | 0.6219 ± 0.5415 |
| glmnet | baseline | 100.00 ± 0.00% | 0.0221 ± 0.0055 |
| xgboost | baseline | 100.00 ± 0.00% | 0.0308 ± 0.0085 |
| rand_forest | baseline | 100.00 ± 0.00% | 0.0981 ± 0.0101 |

---

## Tuned Models

*Current run — hyperparameter tuned.*

| Model       | Config | Accuracy | Log Loss |
|-------------|--------|----------|----------|
| glm | tuned | 98.93 ± 0.93% | 0.2104 ± 0.3108 |
| glmnet | tuned | 99.47 ± 0.95% | 0.0425 ± 0.0604 |
| xgboost | tuned | 100.00 ± 0.00% | 0.0110 ± 0.0026 |
| rand_forest | tuned | 99.47 ± 0.95% | 0.0837 ± 0.0118 |

---

## Best Model

*Selected by lowest mean log loss across holdout years.*

| Metric         | Model       | Config   | Accuracy | Log Loss |
|----------------|-------------|----------|----------|----------|
| Best (log loss)| xgboost | tuned | 100.00 ± 0.00% | 0.0110 ± 0.0026 |

---

## Ensemble Results

*Blended predictions from baseline + tuned GLM, XGBoost, and Random Forest. Weights optimized on years 2019-2021 with entropy regularization (calibration disabled; overfits on ~120 games).*

| Metric   | Accuracy | Log Loss | N Games |
|----------|----------|----------|--------|
| Ensemble | 99.47 ± 0.95% | 0.0291 ± 0.0118 | 187 |

### Ensemble Weights

| Model       | Weight  |
|-------------|--------|
| glm_baseline | 0.201 |
| glm_tuned | 0.196 |
| glmnet_baseline | 0.086 |
| glmnet_tuned | 0.134 |
| xgboost_baseline | 0.096 |
| xgboost_tuned | 0.151 |
| rand_forest_baseline | 0.060 |
| rand_forest_tuned | 0.076 |

*Weights updated 2026-03-13*

