# March Madness Model Performance

*Updated 2026-03-10*

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
| glm | baseline | 97.35 ± 3.30% | 0.4456 ± 0.4599 |
| glmnet | baseline | 98.41 ± 2.75% | 0.0516 ± 0.0354 |
| xgboost | baseline | 98.41 ± 1.59% | 0.0587 ± 0.0316 |
| rand_forest | baseline | 98.41 ± 2.75% | 0.1144 ± 0.0302 |

---

## Tuned Models

*Current run — hyperparameter tuned.*

| Model       | Config | Accuracy | Log Loss |
|-------------|--------|----------|----------|
| glm | tuned | 97.35 ± 2.42% | 0.1125 ± 0.0324 |
| glmnet | tuned | 98.41 ± 2.75% | 0.0486 ± 0.0357 |
| xgboost | tuned | 97.88 ± 1.83% | 0.0485 ± 0.0339 |
| rand_forest | tuned | 98.41 ± 2.75% | 0.1195 ± 0.0275 |

---

## Best Model

*Selected by lowest mean log loss across holdout years.*

| Metric         | Model       | Config   | Accuracy | Log Loss |
|----------------|-------------|----------|----------|----------|
| Best (log loss)| xgboost | tuned | 97.88 ± 1.83% | 0.0485 ± 0.0339 |

---

## Ensemble Results

*Blended predictions from baseline + tuned GLM, XGBoost, and Random Forest. Weights optimized on years 2019-2021 with entropy regularization (calibration disabled; overfits on ~120 games).*

| Metric   | Accuracy | Log Loss | N Games |
|----------|----------|----------|--------|
| Ensemble | 98.41 ± 1.59% | 0.0612 ± 0.0296 | 189 |

### Ensemble Weights

| Model       | Weight  |
|-------------|--------|
| glm_baseline | 0.268 |
| glm_tuned | 0.027 |
| glmnet_baseline | 0.110 |
| glmnet_tuned | 0.122 |
| xgboost_baseline | 0.133 |
| xgboost_tuned | 0.159 |
| rand_forest_baseline | 0.088 |
| rand_forest_tuned | 0.094 |

*Weights updated 2026-03-10*

