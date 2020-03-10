# OPTIC Core

## Installing Package

Once you have the project on your local machine, the first thing you want to do is build and install the package. Open the project in RStudio - easiest to just open the optic.Rproj file. Make sure you have `devtools` installed.

```R
# fresh build of the package documentation and source code
devtools::document()
devtools::build()

# install optic package and dependencies
devtools::install()
```

## Running Simulation

Example below dependent on access to `optic_sim_data_exp.Rdata` data object.

#### Single Threaded Example

The simulation will natively be able to run single or multithreaded, but for now this decision is manually enforced. Below is an example of a single-threaded run.

```R
library(optic)

# load in example data object and convert names to all lowercase
load('data/optic_sim_data_exp.Rdata')
names(x) <- tolower(names(x))

# Here we create the configuration object see ?OpticCOnfig for
# documentation on fields
Sim1 <- OpticConfig$new(
  data=x,
  outcome="crude.rate",
  model_type="linear",
  method_call="lm",
  model_params=list(
    formula=~unemploymentrate + as.factor(year) + as.factor(state),
    weights=as.name('population')
  ),
  iters=5000,
  target_deaths=3500,
  effect_direction="pos",
  n_states=5,
  policy_speed="slow",
  number_implementation_years=3,
  time_periods=2003:2013,
  lag_outcome=FALSE
)


# for this example we clone to new object to not alter original
# in case you want to compare and use ConfigObject below
# NOTE: as an R6Class the object is modified in place so any
# change made in any scope to an R6Class object persists.
ConfigObject <- Sim1$clone()

# This peice will be integrated into the package, but for now we need
# to update the model parameters and formula to pass when running
modelterms <- model_terms(ConfigObject$model_params$formula)
modelterms$lhs <- "outcome"
modelterms$rhs <- c("treatment", modelterms$rhs)
# update model formula
if (ConfigObject$lag_outcome) {
  modelterms$rhs <- c(modelterms$rhs, "lag_outcome")
}
ConfigObject$model_params$formula <- as.formula(paste0(modelterms$lhs, "~", paste(modelterms$rhs, collapse="+")))

# iterate over numbner of iterations specified in config and run
# an independent iteration using the config object
results <- list()
for (i in 1:ConfigObject$iters) {
  results[[i]] <- run_iteration(ConfigObject)
}

# combine results together
full_results <- do.call("rbind", results)

# get summary information
ConfigObject$iter_results <- full_results
summarize_results(ConfigObject)
```
#### Multithreaded Example

For increased performance and parallel compute we leverage the `future` library. Same example as above, but run in parallel using as many cores as are available on the machine.

```R
library(optic)
library(future)
library(future.apply)

# load in example data object and convert names to all lowercase
load('data/optic_sim_data_exp.Rdata')
names(x) <- tolower(names(x))

# Here we create the configuration object see ?OpticCOnfig for
# documentation on fields
Sim1 <- OpticConfig$new(
  data=x,
  outcome="crude.rate",
  model_type="linear",
  method_call="lm",
  model_params=list(
    formula=~unemploymentrate + as.factor(year) + as.factor(state),
    weights=as.name('population')
  ),
  iters=5000,
  target_deaths=3500,
  effect_direction="pos",
  n_states=5,
  policy_speed="slow",
  number_implementation_years=3,
  time_periods=2003:2013,
  lag_outcome=FALSE
)


# for this example we clone to new object to not alter original
# in case you want to compare and use ConfigObject below
# NOTE: as an R6Class the object is modified in place so any
# change made in any scope to an R6Class object persists.
ConfigObject <- Sim1$clone()

# This peice will be integrated into the package, but for now we need
# to update the model parameters and formula to pass when running
modelterms <- model_terms(ConfigObject$model_params$formula)
modelterms$lhs <- "outcome"
modelterms$rhs <- c("treatment", modelterms$rhs)
# update model formula
if (ConfigObject$lag_outcome) {
  modelterms$rhs <- c(modelterms$rhs, "lag_outcome")
}
ConfigObject$model_params$formula <- as.formula(paste0(modelterms$lhs, "~", paste(modelterms$rhs, collapse="+")))

# multi-threaded/parallel implementation
plan(multiprocess)
results <- future_lapply(
  1:ConfigObject$iters,
  FUN=function(i){
    r <- run_iteration(ConfigObject)
    r$iter <- i
    return(r)
  },
  future.seed=1234567
)
plan(sequential)


# combine results together
full_results <- do.call("rbind", results)

# get summary information
ConfigObject$iter_results <- full_results
summarize_results(ConfigObject)
```