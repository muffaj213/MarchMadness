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
| glm | baseline | 62.23 ± 9.57% | 0.8029 ± 0.1795 |
| glmnet | baseline | 64.36 ± 9.91% | 0.6501 ± 0.0941 |
| xgboost | baseline | 69.68 ± 3.06% | 0.5979 ± 0.0303 |
| rand_forest | baseline | 69.15 ± 2.00% | 0.5935 ± 0.0131 |

---

## Tuned Models

*Current run — hyperparameter tuned.*

| Model       | Config | Accuracy | Log Loss |
|-------------|--------|----------|----------|
| glm | tuned | 67.02 ± 4.45% | 0.5875 ± 0.0536 |
| glmnet | tuned | 67.02 ± 7.98% | 0.6173 ± 0.0691 |
| xgboost | tuned | 67.02 ± 4.04% | 0.5993 ± 0.0389 |
| rand_forest | tuned | 68.62 ± 3.24% | 0.5851 ± 0.0202 |

---

## Best Model

*Selected by lowest mean log loss across holdout years.*

| Metric         | Model       | Config   | Accuracy | Log Loss |
|----------------|-------------|----------|----------|----------|
| Best (log loss)| rand_forest | tuned | 68.62 ± 3.24% | 0.5851 ± 0.0202 |

---

## Ensemble Results

*Blended predictions from baseline + tuned GLM, XGBoost, and Random Forest. Weights optimized on years 2019-2021 with entropy regularization (calibration disabled; overfits on ~120 games).*

| Metric   | Accuracy | Log Loss | N Games |
|----------|----------|----------|--------|
| Ensemble | 68.09 ± 6.15% | 7.9454 ± 1.4725 | 188 |

### Ensemble Weights

| Model       | Weight  |
|-------------|--------|
| glm_tuned | 0.250 |
| xgboost_baseline | 0.250 |
| rand_forest_baseline | 0.250 |
| rand_forest_tuned | 0.250 |

*Weights updated 2026-03-17*

