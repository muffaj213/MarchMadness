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
| glm | baseline | 75.53 ± 12.67% | 0.4775 ± 0.1832 |
| glmnet | baseline | 78.72 ± 10.59% | 0.4613 ± 0.1045 |
| xgboost | baseline | 80.85 ± 15.48% | 0.4100 ± 0.2245 |
| rand_forest | baseline | 77.66 ± 11.32% | 0.4266 ± 0.1559 |

---

## Tuned Models

*Current run — hyperparameter tuned.*

| Model       | Config | Accuracy | Log Loss |
|-------------|--------|----------|----------|
| glm | tuned | 78.19 ± 15.09% | 0.4637 ± 0.1262 |
| glmnet | tuned | 77.13 ± 9.99% | 0.4632 ± 0.1141 |
| xgboost | tuned | 80.32 ± 15.09% | 0.4179 ± 0.2061 |
| rand_forest | tuned | 78.72 ± 13.83% | 0.4224 ± 0.1577 |

---

## Best Model

*Selected by lowest mean log loss across holdout years.*

| Metric         | Model       | Config   | Accuracy | Log Loss |
|----------------|-------------|----------|----------|----------|
| Best (log loss)| xgboost | baseline | 80.85 ± 15.48% | 0.4100 ± 0.2245 |

---

## Ensemble Results

*Blended predictions from baseline + tuned GLM, XGBoost, and Random Forest. Weights optimized on years 2019-2021 with entropy regularization (calibration disabled; overfits on ~120 games).*

| Metric   | Accuracy | Log Loss | N Games |
|----------|----------|----------|--------|
| Ensemble | 81.38 ± 17.33% | 4.8761 ± 4.6188 | 188 |

### Ensemble Weights

| Model       | Weight  |
|-------------|--------|
| glm_baseline | 0.010 |
| glm_tuned | 0.003 |
| glmnet_baseline | 0.002 |
| glmnet_tuned | 0.002 |
| xgboost_baseline | 0.178 |
| xgboost_tuned | 0.077 |
| rand_forest_baseline | 0.282 |
| rand_forest_tuned | 0.445 |

*Weights updated 2026-03-16*

