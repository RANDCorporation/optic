
#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# Testing an example of the confounding method

library(optic)
library(glmnet)
library(dplyr)
library(augsynth)
library(did)
library(future)
library(future.apply)
library(glue)
library(arrow)

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

lm_ar <- optic_model(
  name = "auto_regressive_linear",
  type = "autoreg",
  call = "lm",
  formula = crude.rate ~ unemploymentrate + as.factor(year) + treatment_change,
  se_adjust=c("none", "cluster")
)

multisynth_model <- list(
  name="multisynth",
  type="multisynth",
  model_call="multisynth",
  model_formula=crude.rate ~ treatment_level | unemploymentrate,
  model_args=list(unit=as.name("state"), time=as.name("year"), fixedeff=TRUE, 
                  form=crude.rate ~ treatment_level | unemploymentrate),
  se_adjust="none"
)

did_model <- list(
  name="did",
  type="did",
  model_call="att_gt",
  model_formula= ~ unemploymentrate,
  model_args=list(yname="crude.rate", tname="year", idname="state", 
                  gname="treatment_year", xformla = formula("~ unemploymentrate")),
  se_adjust=c("none")
)

# Creating bias vals object - these values are used in confounding paper sim
bias_vals <- list(
  linear = list(
    level = list(
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
    level = list(
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
  models=list(fixedeff_linear, lm_ar, multisynth_model),
  iters=5,
  method = "confounding",
  globals=list(bias_vals=bias_vals),
  unit_var="state",
  treat_var="state",
  time_var="year",
  conf_var = "unemploymentrate",
  effect_magnitude=list(linear0),
  n_units= c(5),
  effect_direction=c("null"), #update and run with nonnull TE below once in good place
  policy_speed=list("instant"),
  n_implementation_periods=c(0), 
  prior_control=c("trend", "level"),
  bias_type=c("linear","nonlinear"),
  bias_size=c("small","medium","large")
)

linear_results <- dispatch_simulations(
  linear_fe_config,
  use_future=F,
  seed=9782,
  verbose=2,
)

linear_results_df <- do.call(rbind, linear_results1)

setwd("C:/Users/griswold/Documents/github/optic/dev/confound_review/updated runs")

write.table(linear_results_df,"test_runs_06222023.csv",sep=",")

#need to summarize results using our fun code + include code for producing figures!!!

#repeat for non-null runs
