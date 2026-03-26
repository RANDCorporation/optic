# Correct rejection rate

For null effects: proportion of CIs that correctly contain zero. For
non-null effects: proportion of CIs that exclude zero AND have the
correct sign.

## Usage

``` r
sim_correct_rejection_rate(
  estimates,
  ses,
  true_effect,
  correction_factor = 1,
  alpha = 0.05
)
```

## Arguments

- estimates:

  Numeric vector of point estimates.

- ses:

  Numeric vector of standard errors.

- true_effect:

  Scalar true effect value.

- correction_factor:

  Scalar multiplier for SEs (default 1).

- alpha:

  Significance level (default 0.05).

## Value

Scalar correct rejection rate.

## See also

Other simulation-performance:
[`sim_bias()`](https://randcorporation.github.io/optic/reference/sim_bias.md),
[`sim_correction_factor()`](https://randcorporation.github.io/optic/reference/sim_correction_factor.md),
[`sim_coverage()`](https://randcorporation.github.io/optic/reference/sim_coverage.md),
[`sim_mse()`](https://randcorporation.github.io/optic/reference/sim_mse.md),
[`sim_rejection_rate()`](https://randcorporation.github.io/optic/reference/sim_rejection_rate.md),
[`sim_type_s_error()`](https://randcorporation.github.io/optic/reference/sim_type_s_error.md),
[`summarize_simulation()`](https://randcorporation.github.io/optic/reference/summarize_simulation.md)
