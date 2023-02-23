


#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# Testing an example of the no_confounding method


data(overdoses)

# Test based on an old README file left by Adam

modified_data <- overdoses %>%
  arrange(state, year) %>%
  group_by(state) %>%
  mutate(lag1 = lag(opioid_rx, n=1L),
         lag2 = lag(opioid_rx, n=2L),
         lag3 = lag(opioid_rx, n=3L)) %>%
  ungroup() %>%
  rowwise() %>%
  # code in moving average and trend versions of prior control
  mutate(prior_control_mva3_OLD = mean(c(lag1, lag2, lag3)),
         prior_control_trend_OLD = lag1 - lag3) %>%
  ungroup() %>%
  dplyr::select(-lag1, -lag2, -lag3) %>%
  mutate(state = factor(as.character(state)))

linear0 <- 0
linear5 <- .05*mean(modified_data$opioid_rx, na.rm=T)
linear15 <- .15*mean(modified_data$opioid_rx, na.rm=T)
linear25 <- .25*mean(modified_data$opioid_rx, na.rm=T)


fixedeff_linear <- optic_model(
  name="fixedeff_linear",
  type="reg",
  call="lm",
  formula=opioid_rx ~ treatment_level + unemploymentrate + as.factor(year) + as.factor(state),
  weights=as.name("population"),
  se_adjust=c("none", "cluster")
)


linear_fe_config <- optic_simulation(
  # data and models required
  x=modified_data,
  models=list(fixedeff_linear),
  # iterations
  iters=5, # 5000
  # specify functions or S3 class of set of functions
  method = "no_confounding",
  globals=NULL,
  unit_var="state",
  treat_var="state",
  time_var="year",
  effect_magnitude=list(linear0, linear5), # linear15, linear25),
  n_units= c(5), # c(1, 5, 15, 30), #2%, 10%, 30%, 60%
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
