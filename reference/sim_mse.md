# Mean squared error

Computes squared deviations of estimates from the true effect.

## Usage

``` r
sim_mse(estimates, true_effect)
```

## Arguments

- estimates:

  Numeric vector of point estimates.

- true_effect:

  Scalar true effect value.

## Value

Numeric vector of squared errors.

## See also

Other simulation-performance:
[`sim_bias()`](https://randcorporation.github.io/optic/reference/sim_bias.md),
[`sim_correct_rejection_rate()`](https://randcorporation.github.io/optic/reference/sim_correct_rejection_rate.md),
[`sim_correction_factor()`](https://randcorporation.github.io/optic/reference/sim_correction_factor.md),
[`sim_coverage()`](https://randcorporation.github.io/optic/reference/sim_coverage.md),
[`sim_rejection_rate()`](https://randcorporation.github.io/optic/reference/sim_rejection_rate.md),
[`sim_type_s_error()`](https://randcorporation.github.io/optic/reference/sim_type_s_error.md),
[`summarize_simulation()`](https://randcorporation.github.io/optic/reference/summarize_simulation.md)
