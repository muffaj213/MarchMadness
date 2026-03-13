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
| glm | baseline | 88.24 ± 20.82% | 0.2081 ± 0.3340 |
| glmnet | baseline | 90.37 ± 17.04% | 0.2125 ± 0.3101 |
| xgboost | baseline | 90.37 ± 17.04% | 0.2130 ± 0.2740 |
| rand_forest | baseline | 99.47 ± 0.95% | 0.1299 ± 0.1287 |

---

## Tuned Models

*Current run — hyperparameter tuned.*

| Model       | Config | Accuracy | Log Loss |
|-------------|--------|----------|----------|
| glm | tuned | 89.84 ± 17.98% | 0.2466 ± 0.2797 |
| glmnet | tuned | 89.30 ± 16.12% | 0.3050 ± 0.2588 |
| xgboost | tuned | 90.37 ± 17.04% | 0.2087 ± 0.2702 |
| rand_forest | tuned | 98.40 ± 2.84% | 0.1745 ± 0.1275 |

---

## Best Model

*Selected by lowest mean log loss across holdout years.*

| Metric         | Model       | Config   | Accuracy | Log Loss |
|----------------|-------------|----------|----------|----------|
| Best (log loss)| rand_forest | baseline | 99.47 ± 0.95% | 0.1299 ± 0.1287 |

---

## Ensemble Results

*Blended predictions from baseline + tuned GLM, XGBoost, and Random Forest. Weights optimized on years 2019-2021 with entropy regularization (calibration disabled; overfits on ~120 games).*

| Metric   | Accuracy | Log Loss | N Games |
|----------|----------|----------|--------|
| Ensemble | 90.91 ± 16.09% | 0.1941 ± 0.2688 | 187 |

### Ensemble Weights

| Model       | Weight  |
|-------------|--------|
| glm_baseline | 0.369 |
| glm_tuned | 0.054 |
| glmnet_baseline | 0.207 |
| glmnet_tuned | 0.005 |
| xgboost_baseline | 0.095 |
| xgboost_tuned | 0.108 |
| rand_forest_baseline | 0.127 |
| rand_forest_tuned | 0.035 |

*Weights updated 2026-03-13*

