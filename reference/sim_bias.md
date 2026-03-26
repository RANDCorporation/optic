# Bias of point estimates

Computes the difference between each estimate and the true effect. Pass
the signed true effect directly (negative for negative effects).

## Usage

``` r
sim_bias(estimates, true_effect)
```

## Arguments

- estimates:

  Numeric vector of point estimates.

- true_effect:

  Scalar true effect value.

## Value

Numeric vector of biases (estimate - true_effect).

## See also

Other simulation-performance:
[`sim_correct_rejection_rate()`](https://randcorporation.github.io/optic/reference/sim_correct_rejection_rate.md),
[`sim_correction_factor()`](https://randcorporation.github.io/optic/reference/sim_correction_factor.md),
[`sim_coverage()`](https://randcorporation.github.io/optic/reference/sim_coverage.md),
[`sim_mse()`](https://randcorporation.github.io/optic/reference/sim_mse.md),
[`sim_rejection_rate()`](https://randcorporation.github.io/optic/reference/sim_rejection_rate.md),
[`sim_type_s_error()`](https://randcorporation.github.io/optic/reference/sim_type_s_error.md),
[`summarize_simulation()`](https://randcorporation.github.io/optic/reference/summarize_simulation.md)
