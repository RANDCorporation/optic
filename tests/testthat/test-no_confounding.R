


#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# Testing an example of the no_confounding method

data(overdoses)

linear0 <- 0
linear5 <- .05*mean(overdoses$opioid_rx, na.rm=T)
linear15 <- .15*mean(overdoses$opioid_rx, na.rm=T)
linear25 <- .25*mean(overdoses$opioid_rx, na.rm=T)

# create optic_model object:
fixedeff_linear <- optic_model(
  name="fixedeff_linear",
  type="reg",
  call="lm",
  formula=opioid_rx ~ treatment_level + unemploymentrate + as.factor(year) + as.factor(state),
  weights=as.name("population"),
  se_adjust=c("none", "cluster")
)


linear_fe_config <- optic_simulation(
  x=overdoses,
  models=list(fixedeff_linear),
  iters=5,
  method = "no_confounding",
  globals=NULL,
  unit_var="state",
  treat_var="state",
  time_var="year",
  effect_magnitude=list(linear0, linear5),
  n_units= c(5),
  effect_direction=c("neg"),
  policy_speed=list("instant"),
  n_implementation_periods=c(0), 
  prior_control=c("mva3", "trend")
)

linear_results <- dispatch_simulations(
  linear_fe_config,
  use_future=T,
  seed=9782,
  verbose=2,
  future.globals=c("cluster_adjust_se"),
  future.packages=c("MASS", "dplyr", "optic")
)

linear_results_df <- do.call(rbind, linear_results)
