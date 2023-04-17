

#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# Testing an example of the confounding method


data(overdoses)

linear0 <- 0
linear5 <- .05*mean(overdoses$crude.rate, na.rm=T)
linear15 <- .15*mean(overdoses$crude.rate, na.rm=T)
linear25 <- .25*mean(overdoses$crude.rate, na.rm=T)

# create optic_model object:
fixedeff_linear <- optic_model(
  name="fixedeff_linear",
  type="reg",
  call="lm",
  formula=crude.rate ~ treatment_level + unemploymentrate + as.factor(year) + as.factor(state),
  weights=as.name("population"),
  se_adjust=c("none", "cluster")
)


# Creating bias vals object
bias_vals <- list(
  linear = list(
    mva3 = list(
      small=c(b0=-3.9, b1=0.06, b2=0.06, b3=0, b4=0, b5=0,
              a1=0.2, a2=0.05, a3=0, a4=0, a5=0),
      medium=c(b0=-4.3, b1=0.11, b2=0.07, b3=0, b4=0, b5=0,
               a1=0.2, a2=0.05, a3=0, a4=0, a5=0),
      large=c(b0=-4.7, b1=0.16, b2=0.1, b3=0, b4=0, b5=0,
              a1=0.2, a2=0.05, a3=0, a4=0, a5=0),
      none = c(b0=-4.8, b1=0, b2=0, b3=0, b4=0, b5=0,
               a1=0, a2=0, a3=0, a4=0, a5=0)),
    trend = list(
      small=c(b0=-3.7, b1=0.15, b2=0.05, b3=0, b4=0, b5=0,
              a1=0.5, a2=0.11, a3=0, a4=0, a5=0),
      medium=c(b0=-4.5, b1=0.26, b2=0.16, b3=0, b4=0, b5=0,
               a1=0.5, a2=0.11, a3=0, a4=0, a5=0),
      large=c(b0=-5.1, b1=0.37, b2=0.22, b3=0, b4=0, b5=0, 
              a1=0.5, a2=0.11, a3=0, a4=0, a5=0),
      none = c(b0=-5, b1=0, b2=0, b3=0, b4=0, b5=0,
               a1=0, a2=0, a3=0, a4=0, a5=0))),
  nonlinear = list(
    mva3 = list(
      small=c(b0=-3.8, b1=0.05, b2=0.05, b3=0.0003, b4=0.0003, b5=0.000003,
              a1=0.01, a2=0.01, a3=0.01, a4=0.01, a5=0.001),
      medium=c(b0=-4, b1=0.05, b2=0.05, b3=0.003, b4=0.003, b5=0.00003,
               a1=0.01, a2=0.01, a3=0.01, a4=0.01, a5=0.001),              
      large=c(b0=-4.2, b1=0.05, b2=0.05, b3=0.0055, b4=0.0055, b5=0.000055,
              a1=0.01, a2=0.01, a3=0.01, a4=0.01, a5=0.001)),          
    trend = list(
      small=c(b0=-3.6, b1=0.05, b2=0.001, b3=0.005, b4=0.008, b5=0.005,
              a1=0.1, a2=0.05, a3=0.1, a4=0.01, a5=0.01),
      medium=c(b0=-4.4, b1=0.05, b2=0.02, b3=0.018, b4=0.015, b5=0.015,
               a1=0.1, a2=0.05, a3=0.1, a4=0.01, a5=0.01),
      large=c(b0=-5.1, b1=0.05, b2=0.03, b3=0.025, b4=0.025, b5=0.025,
              a1=0.1, a2=0.05, a3=0.1, a4=0.01, a5=0.01)
    )
  )
)

#change to confounding
set.seed(894539)

# Filtering SOuth Dakota and North Dakota because they have NA's in their
# crude rate variable

data <- overdoses %>%
  dplyr::filter(!(state %in% c("South Dakota", "North Dakota")))

linear_fe_config <- optic_simulation(
  x=data,
  models=list(fixedeff_linear),
  iters=5,
  method = "confounding",
  globals=list(bias_vals=bias_vals),
  unit_var="state",
  treat_var="state",
  time_var="year",
  effect_magnitude=list(linear0),
  n_units= c(5),
  effect_direction=c("null"),
  policy_speed=list("instant"),
  n_implementation_periods=c(0), 
  prior_control=c("trend", "mva3"),
  bias_type=c("linear","nonlinear"),
  bias_size=c("small","medium","large")
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

linear_results_df


test_that("confounding simulations work", {
  
  expect_type(linear_results, "list")
  
  expect_false(any(is.na(linear_results_df)))
  
})
