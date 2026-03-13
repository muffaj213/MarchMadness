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
| glm | baseline | 83.42 ± 26.58% | 4.0161 ± 6.2845 |
| glmnet | baseline | 89.30 ± 18.93% | 0.2080 ± 0.3239 |
| xgboost | baseline | 88.24 ± 20.82% | 0.3130 ± 0.5075 |
| rand_forest | baseline | 88.24 ± 20.82% | 0.2666 ± 0.3020 |

---

## Tuned Models

*Current run — hyperparameter tuned.*

| Model       | Config | Accuracy | Log Loss |
|-------------|--------|----------|----------|
| glm | tuned | 84.49 ± 26.06% | 1.2833 ± 2.2080 |
| glmnet | tuned | 89.30 ± 18.93% | 0.2994 ± 0.5149 |
| xgboost | tuned | 88.24 ± 20.82% | 0.3106 ± 0.5326 |
| rand_forest | tuned | 89.30 ± 18.93% | 0.2632 ± 0.3288 |

---

## Best Model

*Selected by lowest mean log loss across holdout years.*

| Metric         | Model       | Config   | Accuracy | Log Loss |
|----------------|-------------|----------|----------|----------|
| Best (log loss)| glmnet | baseline | 89.30 ± 18.93% | 0.2080 ± 0.3239 |

---

## Ensemble Results

*Blended predictions from baseline + tuned GLM, XGBoost, and Random Forest. Weights optimized on years 2019-2021 with entropy regularization (calibration disabled; overfits on ~120 games).*

| Metric   | Accuracy | Log Loss | N Games |
|----------|----------|----------|--------|
| Ensemble | 87.70 ± 21.77% | 0.2555 ± 0.4102 | 187 |

### Ensemble Weights

| Model       | Weight  |
|-------------|--------|
| glm_baseline | 0.202 |
| glm_tuned | 0.197 |
| glmnet_baseline | 0.087 |
| glmnet_tuned | 0.135 |
| xgboost_baseline | 0.097 |
| xgboost_tuned | 0.151 |
| rand_forest_baseline | 0.055 |
| rand_forest_tuned | 0.076 |

*Weights updated 2026-03-13*

