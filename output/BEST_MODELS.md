# March Madness Model Performance

*Updated 2026-03-16*

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
| glm | baseline | 89.36 ± 18.62% | 0.2059 ± 0.3263 |
| glmnet | baseline | 90.43 ± 16.76% | 0.2128 ± 0.3095 |
| xgboost | baseline | 91.49 ± 14.90% | 0.2134 ± 0.2779 |
| rand_forest | baseline | 99.47 ± 0.93% | 0.1315 ± 0.1164 |

---

## Tuned Models

*Current run — hyperparameter tuned.*

| Model       | Config | Accuracy | Log Loss |
|-------------|--------|----------|----------|
| glm | tuned | 89.36 ± 18.62% | 0.2461 ± 0.2740 |
| glmnet | tuned | 90.43 ± 16.76% | 0.3086 ± 0.2539 |
| xgboost | tuned | 90.43 ± 16.76% | 0.2152 ± 0.2781 |
| rand_forest | tuned | 97.34 ± 4.66% | 0.1692 ± 0.1238 |

---

## Best Model

*Selected by lowest mean log loss across holdout years.*

| Metric         | Model       | Config   | Accuracy | Log Loss |
|----------------|-------------|----------|----------|----------|
| Best (log loss)| rand_forest | baseline | 99.47 ± 0.93% | 0.1315 ± 0.1164 |

---

## Ensemble Results

*Blended predictions from baseline + tuned GLM, XGBoost, and Random Forest. Weights optimized on years 2019-2021 with entropy regularization (calibration disabled; overfits on ~120 games).*

| Metric   | Accuracy | Log Loss | N Games |
|----------|----------|----------|--------|
| Ensemble | 92.02 ± 13.97% | 0.1871 ± 0.2465 | 188 |

### Ensemble Weights

| Model       | Weight  |
|-------------|--------|
| glm_baseline | 0.307 |
| glm_tuned | 0.044 |
| glmnet_baseline | 0.182 |
| glmnet_tuned | 0.004 |
| xgboost_baseline | 0.120 |
| xgboost_tuned | 0.103 |
| rand_forest_baseline | 0.188 |
| rand_forest_tuned | 0.053 |

*Weights updated 2026-03-16*

