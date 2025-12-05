# Execute simulations defined in a optic_simulation object

Execute simulations defined in a optic_simulation object

## Usage

``` r
dispatch_simulations(object, seed = NULL, use_future = FALSE, verbose = 0, ...)
```

## Arguments

- object:

  Simulation scenarios object created using optic_simulation

- seed:

  Specified as either NULL or a numeric. Sets a seed, which is becomes
  an index in results, for each independent set of simulations in
  optic_simulation.

- use_future:

  Runs simulation scenarios in parallel. Default FALSE, set to TRUE if
  you have already setup a future plan (e.g., multiprocess, cluster,
  etc) and would like for the iterations to be run in parallel.

- verbose:

  Default TRUE. IF TRUE, provides details on what's currently running.

- ...:

  additional parameters to be passed to future_apply. User can pass
  future.globals and future.packages if your code relies on additional
  packages

## Value

A list of dataframes, where each list entry contains results for a set
of simulation parameters, with dataframes containing estimated treatment
effects and summary statistics by model and draw.

## Examples

``` r
# Set up a basic model and simulation scenario:
data(overdoses)

eff <- 0.1*mean(overdoses$crude.rate, na.rm = TRUE)
form <- formula(crude.rate ~ state + year + population + treatment_level)
mod <- optic_model(name = 'lin', 
                   type = 'reg', 
                   call = 'lm', 
                   formula = form, 
                   se_adjust = 'none')

sim <- optic_simulation(x = overdoses, 
                        models = list(mod), 
                        method = 'no_confounding', 
                        unit_var = 'state', 
                        treat_var = 'state',
                        time_var = 'year', 
                        effect_magnitude = list(eff), 
                        n_units = 2, 
                        effect_direction = 'pos', 
                        iters = 2,
                        policy_speed = 'instant', 
                        n_implementation_periods = 1)
#> Warning: `cross()` was deprecated in purrr 1.0.0.
#> ℹ Please use `tidyr::expand_grid()` instead.
#> ℹ See <https://github.com/tidyverse/purrr/issues/768>.
#> ℹ The deprecated feature was likely used in the optic package.
#>   Please report the issue at <https://github.com/randcorporation/optic/issues>.
#> Number of Simulations: 1
#> Number of Models: 1
#> Iteration per Simulation : 2
#> Total number of Iterations to Run: 2

# Finally, dispatch the simulation:
dispatch_simulations(sim)
#> [[1]]
#>      outcome se_adjustment    estimate       se variance      t_stat   p_value
#> 1 crude.rate          none  0.05595668 1.186343  1.40741  0.04716736 0.9623902
#> 2 crude.rate          none -0.67953698 1.431831  2.05014 -0.47459304 0.6351914
#>        mse model_name model_call
#> 1 11.02171        lin         lm
#> 2 11.01193        lin         lm
#>                                              model_formula policy_speed
#> 1 crude.rate ~ state + year + population + treatment_level      instant
#> 2 crude.rate ~ state + year + population + treatment_level      instant
#>   n_implementation_periods prior_control effect_magnitude n_units
#> 1                        1         level         1.265073       2
#> 2                        1         level         1.265073       2
#>   effect_direction iter
#> 1              pos    1
#> 2              pos    2
#> 
```
