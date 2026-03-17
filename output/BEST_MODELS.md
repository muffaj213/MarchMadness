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
| glm | baseline | 62.77 ± 10.47% | 0.7949 ± 0.1930 |
| glmnet | baseline | 64.36 ± 9.91% | 0.6504 ± 0.0929 |
| xgboost | baseline | 69.15 ± 2.23% | 0.5947 ± 0.0262 |
| rand_forest | baseline | 69.15 ± 2.39% | 0.5925 ± 0.0164 |

---

## Tuned Models

*Current run — hyperparameter tuned.*

| Model       | Config | Accuracy | Log Loss |
|-------------|--------|----------|----------|
| glm | tuned | 65.43 ± 9.55% | 0.7271 ± 0.1457 |
| glmnet | tuned | 62.77 ± 11.52% | 0.6599 ± 0.0988 |
| xgboost | tuned | 68.09 ± 4.31% | 0.5990 ± 0.0390 |
| rand_forest | tuned | 68.09 ± 4.12% | 0.5849 ± 0.0203 |

---

## Best Model

*Selected by lowest mean log loss across holdout years.*

| Metric         | Model       | Config   | Accuracy | Log Loss |
|----------------|-------------|----------|----------|----------|
| Best (log loss)| rand_forest | tuned | 68.09 ± 4.12% | 0.5849 ± 0.0203 |

---

## Ensemble Results

*Blended predictions from baseline + tuned GLM, XGBoost, and Random Forest. Weights optimized on years 2019-2021 with entropy regularization (calibration disabled; overfits on ~120 games).*

| Metric   | Accuracy | Log Loss | N Games |
|----------|----------|----------|--------|
| Ensemble | 68.09 ± 4.03% | 8.1847 ± 1.3108 | 188 |

### Ensemble Weights

| Model       | Weight  |
|-------------|--------|
| xgboost_baseline | 0.250 |
| xgboost_tuned | 0.250 |
| rand_forest_baseline | 0.250 |
| rand_forest_tuned | 0.250 |

*Weights updated 2026-03-17*

