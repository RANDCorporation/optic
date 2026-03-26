# Coverage of confidence intervals

Proportion of confidence intervals that contain the true effect.
Optionally applies an empirical correction factor to the standard errors
before constructing intervals.

## Usage

``` r
sim_coverage(
  estimates,
  ses,
  true_effect = 0,
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

  Scalar true effect value (default 0).

- correction_factor:

  Scalar multiplier for SEs (default 1, no correction).

- alpha:

  Significance level for CI width (default 0.05 for 95% CI).

## Value

Scalar coverage rate.

## See also

Other simulation-performance:
[`sim_bias()`](https://randcorporation.github.io/optic/reference/sim_bias.md),
[`sim_correct_rejection_rate()`](https://randcorporation.github.io/optic/reference/sim_correct_rejection_rate.md),
[`sim_correction_factor()`](https://randcorporation.github.io/optic/reference/sim_correction_factor.md),
[`sim_mse()`](https://randcorporation.github.io/optic/reference/sim_mse.md),
[`sim_rejection_rate()`](https://randcorporation.github.io/optic/reference/sim_rejection_rate.md),
[`sim_type_s_error()`](https://randcorporation.github.io/optic/reference/sim_type_s_error.md),
[`summarize_simulation()`](https://randcorporation.github.io/optic/reference/summarize_simulation.md)
