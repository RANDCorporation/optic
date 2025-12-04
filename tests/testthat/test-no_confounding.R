#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# Testing an example of the no_confounding method
library(dplyr)
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
  weights= as.name("population"),
  se_adjust=c("none", "cluster-unit", "cluster-treat", "huber", "arellano")
)

fixedeff_linear_two <- optic_model(
  name="fixedeff_linear_two",
  type="reg",
  call="lm",
  formula=crude.rate ~ treatment_level + unemploymentrate + as.factor(year) + as.factor(state),
  weights= as.name("population"),
  se_adjust=c("none", "cluster-unit", "cluster-treat", "huber", "arellano")
)

lm_ar <- optic_model(
  name = "auto_regressive_linear",
  type = "autoreg",
  call = "lm",
  formula = crude.rate ~ unemploymentrate + year + treatment_change,
  se_adjust = "cluster-unit"
)

m_aug <- optic_model(
  name = "augsynth",
  type = "multisynth",
  call = "multisynth",
  formula = crude.rate ~ treatment_level,
  unit = as.name("state"),
  time = as.name("year"),
  lambda = 0.1,
  se_adjust = "none",
  fixedeff = F
)

m_csa <- optic_model(
  name = "csa_did",
  type = "did",
  call = "att_gt",
  formula = crude.rate ~ treatment_level,
  yname = "crude.rate", 
  tname = 'year', 
  idname = 'state',
  gname = 'treatment_date', 
  # xformla = formula(~ unemploymentrate),
  se_adjust = "none"
)

# Remove nebraska for augsynth to work:
# You are including a fixed effect with `fixedeff = T`, but the following units only have one pre-treatment outcome: (Nebraska). Either remove these units or set `fixedeff = F`.
data <- overdoses %>%
  dplyr::filter(!(state %in% c("Nebraska", "Nevada", "Arkansas", "Mississippi", "Oregon")))

models <- list(
               #m_csa
               m_aug
               #fixedeff_linear, 
               #fixedeff_linear_two, 
               #lm_ar
               )

linear_fe_config <- optic_simulation(
  x=data,
  models=models,
  iters=11,
  method = "no_confounding",
  globals=NULL,
  unit_var="state",
  treat_var="state",
  time_var="year",
  effect_magnitude=list(linear0, linear5),
  n_units= c(5),
  effect_direction=c("neg"),
  policy_speed=c("instant"),
  n_implementation_periods=c(1)
)

suppressWarnings({
linear_results <- dispatch_simulations(
  linear_fe_config,
  use_future=T,
  seed=9782,
  verbose=0,
  future.globals=c("cluster_adjust_se"),
  future.packages=c("MASS", "dplyr", "optic", "augsynth", "did")
)
} 
)



test_that("no_confounding results have consistent structure", {
  col_types <- sapply(linear_results, class)
  expect_snapshot(list(
    dim = dim(linear_results),
    colnames = colnames(linear_results),
    nrow = nrow(linear_results),
    col_types = col_types
  ))
})


