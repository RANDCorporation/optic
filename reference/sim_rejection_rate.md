# Rejection rate (Type I error under the null)

Proportion of p-values below a significance threshold.

## Usage

``` r
sim_rejection_rate(p_values, alpha = 0.05)
```

## Arguments

- p_values:

  Numeric vector of p-values.

- alpha:

  Significance level (default 0.05).

## Value

Scalar: proportion of p-values \< alpha.

## See also

Other simulation-performance:
[`sim_bias()`](https://randcorporation.github.io/optic/reference/sim_bias.md),
[`sim_correct_rejection_rate()`](https://randcorporation.github.io/optic/reference/sim_correct_rejection_rate.md),
[`sim_correction_factor()`](https://randcorporation.github.io/optic/reference/sim_correction_factor.md),
[`sim_coverage()`](https://randcorporation.github.io/optic/reference/sim_coverage.md),
[`sim_mse()`](https://randcorporation.github.io/optic/reference/sim_mse.md),
[`sim_type_s_error()`](https://randcorporation.github.io/optic/reference/sim_type_s_error.md),
[`summarize_simulation()`](https://randcorporation.github.io/optic/reference/summarize_simulation.md)
