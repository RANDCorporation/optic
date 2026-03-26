# Type S (sign) error rate

Among statistically significant results, the proportion that have the
wrong sign relative to the true effect. Returns NA when true_effect is
0.

## Usage

``` r
sim_type_s_error(estimates, p_values, true_effect, alpha = 0.05)
```

## Arguments

- estimates:

  Numeric vector of point estimates.

- p_values:

  Numeric vector of p-values.

- true_effect:

  Scalar true effect value.

- alpha:

  Significance level (default 0.05).

## Value

Scalar Type S error rate, or NA for null effects.

## See also

Other simulation-performance:
[`sim_bias()`](https://randcorporation.github.io/optic/reference/sim_bias.md),
[`sim_correct_rejection_rate()`](https://randcorporation.github.io/optic/reference/sim_correct_rejection_rate.md),
[`sim_correction_factor()`](https://randcorporation.github.io/optic/reference/sim_correction_factor.md),
[`sim_coverage()`](https://randcorporation.github.io/optic/reference/sim_coverage.md),
[`sim_mse()`](https://randcorporation.github.io/optic/reference/sim_mse.md),
[`sim_rejection_rate()`](https://randcorporation.github.io/optic/reference/sim_rejection_rate.md),
[`summarize_simulation()`](https://randcorporation.github.io/optic/reference/summarize_simulation.md)
