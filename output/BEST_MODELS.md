# March Madness Model Performance

*Updated 2026-03-17*

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
| glm | baseline | 61.70 ± 9.18% | 0.7680 ± 0.1725 |
| glmnet | baseline | 64.89 ± 9.88% | 0.6529 ± 0.0912 |
| xgboost | baseline | 71.28 ± 2.90% | 0.6030 ± 0.0370 |
| rand_forest | baseline | 67.02 ± 2.68% | 0.5929 ± 0.0199 |

---

## Tuned Models

*Current run — hyperparameter tuned.*

| Model       | Config | Accuracy | Log Loss |
|-------------|--------|----------|----------|
| glm | tuned | 65.43 ± 8.43% | 0.7175 ± 0.1358 |
| glmnet | tuned | 67.02 ± 7.98% | 0.6171 ± 0.0688 |
| xgboost | tuned | 67.55 ± 3.40% | 0.5967 ± 0.0411 |
| rand_forest | tuned | 68.09 ± 3.46% | 0.5869 ± 0.0202 |

---

## Best Model

*Selected by lowest mean log loss across holdout years.*

| Metric         | Model       | Config   | Accuracy | Log Loss |
|----------------|-------------|----------|----------|----------|
| Best (log loss)| rand_forest | tuned | 68.09 ± 3.46% | 0.5869 ± 0.0202 |

---

## Ensemble Results

*Blended predictions from baseline + tuned GLM, XGBoost, and Random Forest. Weights optimized on years 2019-2021 with entropy regularization (calibration disabled; overfits on ~120 games).*

| Metric   | Accuracy | Log Loss | N Games |
|----------|----------|----------|--------|
| Ensemble | 68.09 ± 1.61% | 0.5898 ± 0.0272 | 188 |

### Ensemble Weights

| Model       | Weight  |
|-------------|--------|
| xgboost_baseline | 0.250 |
| xgboost_tuned | 0.250 |
| rand_forest_baseline | 0.250 |
| rand_forest_tuned | 0.250 |

*Weights updated 2026-03-17*

