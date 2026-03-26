# Empirical correction factor

Computes the ratio of the empirical 95th percentile of squared
t-statistics to the theoretical chi-squared(1) 95th percentile. Used to
calibrate standard errors for coverage computation.

## Usage

``` r
sim_correction_factor(test_stats)
```

## Arguments

- test_stats:

  Numeric vector of t-statistics.

## Value

Scalar correction factor. Values \> 1 indicate model SEs are too small.

## See also

Other simulation-performance:
[`sim_bias()`](https://randcorporation.github.io/optic/reference/sim_bias.md),
[`sim_correct_rejection_rate()`](https://randcorporation.github.io/optic/reference/sim_correct_rejection_rate.md),
[`sim_coverage()`](https://randcorporation.github.io/optic/reference/sim_coverage.md),
[`sim_mse()`](https://randcorporation.github.io/optic/reference/sim_mse.md),
[`sim_rejection_rate()`](https://randcorporation.github.io/optic/reference/sim_rejection_rate.md),
[`sim_type_s_error()`](https://randcorporation.github.io/optic/reference/sim_type_s_error.md),
[`summarize_simulation()`](https://randcorporation.github.io/optic/reference/summarize_simulation.md)
