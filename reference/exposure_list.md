# Applies a time-varying treatment effect

Simulates a time-varying treatment effect that starts at zero in time
period zero, then linearly increases to a 'full treatment' effect, based
on analyst-provided choices concerning time until full treatment effect
and 'speed'

## Usage

``` r
exposure_list(
  sampled_time_period,
  mo,
  available_periods,
  policy_speed,
  n_implementation_periods
)
```

## Arguments

- sampled_time_period:

  Year that treatment is first enacted

- mo:

  Month that treatment is first enacted

- available_periods:

  Maximum number of time periods in the data (e.g. if policy is between
  1950-2000, then available_periods == 50)

- policy_speed:

  A string which is either "instant" for the policy going into immediate
  effect or "slow" for the policy effect phasing in linearly across
  n_implement_periods

- n_implementation_periods:

  Number of periods until full treatment effect is applied. Only used if
  policy_speed is 'slow'.

## Value

A list, containing a vector of policy years of implementation, an
integer of the starting policy implementation month, and the effect of
treatment within a given implementation year (as a fraction of the total
policy effect)

## Examples

``` r
# Set up a policy that starts in first-year of data, in July and takes 
# 2 years for full implementation:
exposure_list(1, 7, 3, policy_speed = 'slow', n_implementation_periods = 2)
#> $policy_years
#> [1] 1 2 3
#> 
#> $policy_month
#> [1] 7
#> 
#> $exposure
#> [1] 0.0625000 0.5625000 0.9270833
#> 

# Same scenario but effect happens instantaneously:
exposure_list(1, 7, 3, policy_speed = 'instant')
#> $policy_years
#> [1] 1 2 3
#> 
#> $policy_month
#> [1] 7
#> 
#> $exposure
#> [1] 0.5 1.0 1.0
#> 
```
