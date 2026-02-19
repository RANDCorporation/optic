# Optic Model

Generates model object to apply to each simulated dataset

## Usage

``` r
optic_model(name, type, call, formula, se_adjust, ...)
```

## Arguments

- name:

  Name of the model object, used to identify the model when reviewing
  simulation results

- type:

  Estimator used to identify the treatment effect using simulated data.
  Specified as a string, which can either be 'reg' (regression),
  'autoreg' (autoregression, which adds a lag for the outcome variable
  to a regression model), 'autoeffect' (debiased autoregressive model
  implemented via the autoeffect package), 'drdid' (doubly-robust
  difference-in-difference estimator), or 'multisynth' (augmented
  synthetic control)

- call:

  String which specifies the R function to call for applying the
  estimator. Package currently supports either 'lm' (linear model),
  'feols' (fixed-effect OLS), 'multisynth' (pooled synthetic controls),
  or 'glm.nb' (negative-binomial generalized nearlized linear model)

- formula:

  Model specification, using R formula formatting. Must include a
  variable labeled 'treatment' for the 'nonconf' & 'selbias' simulation
  method or variables labeled 'treatment1' & 'treatment2' for the
  simulation method 'concurrent'

- se_adjust:

  Adjustments applied to standard errors following model estimation.
  Specified as a string, OPTIC currently support 'none' for no
  adjustment or 'cluster' for clustered standard errors. Clustered
  standard errors will use the 'unit_var' specified in optic_simulation
  for determining unit used for clustering standard errors.

- ...:

  Additional arguments that are passed to the model call. Please refer
  to documentation for each model call for additional details. If the
  model call expects a name, you may need to pass your parameter using
  param = as.name("variable_name") as opposed to param = variable_name.

## Value

optic_model An optic_model object to be used as an input within
optic_simulations. Details model calls and parameters.

## Examples

``` r
# Load the overdoses example dataset
data(overdoses)

# Set up a simple linear model with fixed effects
lm_fe <- optic_model(
  name = 'fixed_effect_linear', 
  type = 'reg', 
  call = 'lm', 
  formula = crude.rate ~ as.factor(year) + as.factor(state) + treatment_level, 
  se_adjust = 'cluster'
)

# Deploy an auto-regressive model.
# type = "autoreg" will make AR term 
# automatically when the model is deployed; also note
# in formula the use of "treatment_change" as the treatment variable 
# rather than "treatment_level" like in the previous example:

lm_ar <- optic_model(
  name = "auto_regressive_linear", 
  type = "autoreg", 
  call = "lm", 
  formula = crude.rate ~ unemploymentrate + as.factor(year) + treatment_change,
  se_adjust = "none"
)

# Fixed effects model with covariate adjustment
lm_fe_adj <- optic_model(
  name = "fixed_effect_linear_adj",
  type = "reg",
  call = "lm",
  formula = crude.rate ~ unemploymentrate + as.factor(year) + 
            as.factor(state) + treatment_level,
  se_adjust = "cluster"
)
```
