library(did)
library(augsynth)
library(did2s)
library(didimputation)
library(fixest)
library(tidyr)

data(overdoses)
overdoses$crude.rate <- 1e5*(overdoses$deaths/overdoses$population)

ascm <- list(
  name="ASCM",
  type="multisynth",
  model_call="multisynth",
  model_formula = crude.rate ~ treatment_level | unemploymentrate,
  model_args = list(unit = as.name("state"), time = as.name("year")),
  se_adjust="none"
)

csa <- list(
  name="did",
  type="did",
  model_call="att_gt",
  model_formula= crude.rate ~ unemploymentrate + treatment_level,
  model_args=list(yname = "crude.rate", tname = "year", 
                  idname = "state", gname = "treatment_date", 
                  xformla = formula(~unemploymentrate)),
  se_adjust=c("none")
)

five_percent_effect <- 0.05*mean(overdoses$crude.rate, na.rm = T)
ten_percent_effect  <- 0.10*mean(overdoses$crude.rate, na.rm = T)

scenarios_no_confounding <- list(five_percent_effect, ten_percent_effect)

sim_config <- optic_simulation(
  x                        = overdoses,
  models                   = list(csa, ascm), #csa, ascm ###want this to work with csa & ascm
  iters                    = 100, #change to 100
  method                   = "no_confounding",
  unit_var                 = "state",
  treat_var                = "state",
  time_var                 = "year",
  effect_magnitude         = scenarios_no_confounding,
  n_units                  = c(5,30),
  effect_direction         = c("neg"),
  policy_speed             = c("instant","slow"),
  n_implementation_periods = c(6) #confirm this is doing what I need

)

sim_results <- dispatch_simulations(
  
  sim_config,
  use_future = F,
  seed = 9781,
  verbose = 0
  
)