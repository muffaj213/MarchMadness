# March Madness Model Performance

*Updated 2026-03-10*

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
| glm | baseline | 94.65 ± 5.08% | 1.0878 ± 1.0392 |
| glmnet | baseline | 98.40 ± 2.75% | 0.0494 ± 0.0357 |
| xgboost | baseline | 98.40 ± 1.64% | 0.0647 ± 0.0338 |
| rand_forest | baseline | 98.40 ± 2.75% | 0.1281 ± 0.0259 |

---

## Tuned Models

*Current run — hyperparameter tuned.*

| Model       | Config | Accuracy | Log Loss |
|-------------|--------|----------|----------|
| glm | tuned | 97.33 ± 2.49% | 0.1060 ± 0.0263 |
| glmnet | tuned | 98.40 ± 2.75% | 0.0464 ± 0.0365 |
| xgboost | tuned | 98.40 ± 1.59% | 0.0462 ± 0.0332 |
| rand_forest | tuned | 98.40 ± 2.75% | 0.1085 ± 0.0282 |

---

## Best Model

*Selected by lowest mean log loss across holdout years.*

| Metric         | Model       | Config   | Accuracy | Log Loss |
|----------------|-------------|----------|----------|----------|
| Best (log loss)| xgboost | tuned | 98.40 ± 1.59% | 0.0462 ± 0.0332 |

---

## Ensemble Results

*Blended predictions from baseline + tuned GLM, XGBoost, and Random Forest. Weights optimized on years 2019-2021 with entropy regularization (calibration disabled; overfits on ~120 games).*

| Metric   | Accuracy | Log Loss | N Games |
|----------|----------|----------|--------|
| Ensemble | 98.40 ± 1.59% | 0.0683 ± 0.0411 | 187 |

### Ensemble Weights

| Model       | Weight  |
|-------------|--------|
| glm_baseline | 0.270 |
| glm_tuned | 0.026 |
| glmnet_baseline | 0.110 |
| glmnet_tuned | 0.122 |
| xgboost_baseline | 0.135 |
| xgboost_tuned | 0.160 |
| rand_forest_baseline | 0.075 |
| rand_forest_tuned | 0.102 |

*Weights updated 2026-03-10*

