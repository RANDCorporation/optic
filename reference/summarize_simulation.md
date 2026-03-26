# Summarize simulation results

Computes all performance metrics from a data frame of simulation
iteration results. Returns a single-row data frame with bias, variance,
MSE, rejection rate, coverage, Type S error, and correct rejection rate.

## Usage

``` r
summarize_simulation(
  results,
  true_effect,
  estimate_col = "estimate",
  se_col = "se",
  p_value_col = "p_value",
  t_stat_col = "t_stat"
)
```

## Arguments

- results:

  Data frame with one row per simulation iteration.

- true_effect:

  Scalar true effect value (signed).

- estimate_col:

  Column name for point estimates (default "estimate").

- se_col:

  Column name for standard errors (default "se").

- p_value_col:

  Column name for p-values (default "p_value").

- t_stat_col:

  Column name for t-statistics (default "t_stat").

## Value

Single-row data frame with columns: mean_estimate, mean_bias, mean_se,
mean_variance, simulated_variance, mse, rmse, rejection_rate,
correction_factor, coverage, type_s_error, correct_rejection_rate,
n_valid.

## See also

Other simulation-performance:
[`sim_bias()`](https://randcorporation.github.io/optic/reference/sim_bias.md),
[`sim_correct_rejection_rate()`](https://randcorporation.github.io/optic/reference/sim_correct_rejection_rate.md),
[`sim_correction_factor()`](https://randcorporation.github.io/optic/reference/sim_correction_factor.md),
[`sim_coverage()`](https://randcorporation.github.io/optic/reference/sim_coverage.md),
[`sim_mse()`](https://randcorporation.github.io/optic/reference/sim_mse.md),
[`sim_rejection_rate()`](https://randcorporation.github.io/optic/reference/sim_rejection_rate.md),
[`sim_type_s_error()`](https://randcorporation.github.io/optic/reference/sim_type_s_error.md)
