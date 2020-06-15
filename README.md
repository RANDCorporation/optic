# OPTIC Core

## Installing Package

#### Clone

Clone the repository to your machine. Once you have the project on your local machine, the first thing you want to do is build and install the package. Open the project in RStudio - easiest to just open the optic.Rproj file. Make sure you have `devtools` installed.

```R
# fresh build of the package documentation and source code
devtools::document()
devtools::build()

# install optic package and dependencies
devtools::install()
```

## Running Simulation

Example below dependent on access to `optic_sim_data_exp.Rdata` data object.

#### Configure Simulation

The first step is to congifure the simulation or set of simulations you wish to run.

```R
library(optic)

# load in example data object and convert names to all lowercase
load('data/optic_sim_data_exp.Rdata')
names(x) <- tolower(names(x))

# For this example, we will use a linear (additive) effect and a
# multiplicative effect for a log mode
linear5 <- 690 / ((sum(x$population) / length(unique(x$year))) / 100000)
linear10 <- 1380 / ((sum(x$population) / length(unique(x$year))) / 100000)
linear15 <- 2070 / ((sum(x$population) / length(unique(x$year))) / 100000)

# not necessary, but easier to read if we create the true effect scenarios here
scenario1 <- c(linear10, linear10)
scenario2 <- c(linear5, linear15)


# specifcy a configuration for the set of simulations we want to run
# there are inline comments below for the first config, but see ?configure_simulation for more
# the configure_simulation function will run a series of validation checks on your
# data and configuration arguments and if all is well, return an R6Class object ready to run
two_way_fe_config <- configure_simulation(
  # data.frame of data to use
  x=x,
  
  # sampling variable in x
  unit_var="state",
  
  # time period variable x
  time_var="year",
  
  # model_call, model_formula, and model_args are all related in a 1:1 way
  # model_call is the function that will be called when running the model
  # this can be a list of any length and if greater than 1, model_formula must
  # also be a list of the same length
  # model_formula is the formula that will be run for the model_call in the same
  # index/position. E.g., the first model_call will be paired with the first
  # model_formula. These fields are not combinatorially combined, they are 1:1
  # model_args is optional, but if provided must be the same length as model_call
  # you may use an empty list if you have no args for a certain model
  # NOTE: model_formula must include treatment (for non concurrent runs) or some
  # combination of treatment1 and treatment2 (for concurrent runs).
  model_call=list("lm", "lm"),
  model_formula=list(
    crude.rate ~ unemploymentrate + as.factor(year) + as.factor(state) + treatment1 + treatment2,
    crude.rate ~ unemploymentrate + as.factor(year) + as.factor(state) + treatment1
  ),
  model_args=list(
    list(weights=as.name('population')),
    list(weights=as.name('population'))
  ),
  
  # effect_magnitude, n_units, effect_direction, policy_speed, and rhos
  # (if the run is concurrent) are all expanded combinatorially with the
  # length of model_call when creating the configuration
  
  # the true effects to apply to the sampled treated units
  effect_magnitude=list(scenario1, scenario2),
  
  # number of units to sample for treatment
  n_units=c(5, 30),
  
  # iterations to run for each simulation
  iters=5000,
  
  # null, neg, or pos for no  effect, a negative application of the
  # effect magnitude to treated units, or a positive application of the
  # effect magnitude
  effect_direction=c("null", "neg"),
  
  # if policy_speed is slow, you must provide a value for n_implementation_periods
  # for how many time periods over which the treatment should be phased in.
  # n_implementation periods is ignored for simulations where policy_speed is "instant"
  policy_speed=c("instant", "slow"),
  n_implementation_periods=3,
  
  # is this a concurrent run? the simulation with apply 2 treatment effects if true,
  # treatment1 and treatment2
  # NOTE: if concurrent is true, you must provide at least one value to "rhos"
  # for the simulation to know what correlation to use between treatment start time
  # periods
  # NOTE: this applies to ALL runs, non-concurrent and concurrent simulations
  # must be configured separately
  concurrent=TRUE,
  rhos=c(0, 0.25, 0.5, 0.75, 0.9),
  
  # which standard error adjustments should be provided along with the unadjusted
  # results? for cluster and huber-cluster, the unit_var is used as the clustering variable
  se_adjust=c("cluster", "huber", "huber-cluster"),
  
  # whether or not to change code treatment (useful for autoregressive modeling)
  # NOTE: this applies to ALL runs, autoregressive models should be configured
  # separately
  change_code_treatment=FALSE
  
  # should a lag of the outcome (by one time period) be added to the right hand side of
  # the model?
  lag_outcome=FALSE,
  
  # provides some information when config is created
  verbose=TRUE
)

# as an example without all the documnetation, we do the same configuration here but
# for an autoregressive version of the model where change_code_treatment is true,
# lag_outcome is TRUE, and the model formula don't use a fixed effect for our unit
autoreg_config <- configure_simulation(
  x=x,
  unit_var="state",
  time_var="year",
  model_call=list("lm", "lm"),
  model_formula=list(
    crude.rate ~ unemploymentrate + as.factor(year) + treatment1 + treatment2,
    crude.rate ~ unemploymentrate + as.factor(year) + treatment1
  ),
  model_args=list(
    list(weights=as.name('population')),
    list(weights=as.name('population'))
  ),
  effect_magnitude=list(scenario1, scenario2),
  n_units=c(5, 30),
  iters=5000,
  effect_direction=c("null", "neg"),
  policy_speed=c("instant", "slow"),
  n_implementation_periods=3,
  concurrent=TRUE,
  rhos=c(0, 0.25, 0.5, 0.75, 0.9),
  se_adjust=c("cluster", "huber", "huber-cluster"),
  change_code_treatment=TRUE,
  lag_outcome=TRUE,
  verbose=TRUE
)
```

The configuration object that is created is an `R6Class` object and **is modified in place**. When the object is created with `verbose=TRUE` it will print out the number of simulations and the total number of iterations (simulations * iterations) that
you have just configured. If `verbose=FALSE` or you would like to visualize what you have configured, feel free to inspect the
configuration object.

```R
# print number of simulations
nrow(autoreg_config$combination_args)

# View the simulations you configured
# NOTE: the "model_index" represents the index of the model_call,
# model_formula, and model_args that will be used for that simulation
View(autoreg_config$combination_args)
```

#### Dispatching Simulations

You have a couple of options when dispatching the configuration object. The main decision is whether or not to dispatch them single-threaded or parallelized. Single-threaded should be fine when the number of iterations is low or the total number of
iterations * simulations is low. If that is not the case, you will want to leverage parallelization, which is easily achieved
when dispatching the config object. We will use the `autoreg_config` created above in the example below.

```R
# checkout the help for more details if you would like to
#?dispatch_simulations

# the dispatch_config function is used to actually run the simulations you configured
# above and has a few arguments:
#   sim_config: the config object (in this case, autoreg_config)
#   use_future: uses the future and future.apply libraries to parallelize the simulations
#   seed: used for reproducibility
#   verbose: if TRUE, will update the console with information on which simulation is
#            currently running

# we can dispatch the job without parallization, but this runs each iteration
# sequentially and will likely be very slow.
autoreg_results <- dispatch_config(
  sim_config=autoreg_config,
  use_future=FALSE,
  seed=3892,
  verbose=TRUE
)

# if we have available cores on a machine, we can dramatically speed things up
# NOTE: current implementation parallelizes the iterations of a simulation
# so if your simulation has very few iters but many many simulations, this may
# actually slow your job down given small overhead for starting up the parallel
# connections. Future version of the package will allow you you parallize one or
# the other.
library(future)
library(future.apply)

cl <- parallel::makeCluster(16L)
plan("cluster", workers = cl) 

autoreg_results <- dispatch_config(
  sim_config=autoreg_config,
  use_future=TRUE,
  seed=3892,
  verbose=TRUE
)
```
