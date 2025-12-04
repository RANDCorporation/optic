
devtools::load_all()
library(dplyr)
library(here)
library(augsynth)
library(future)

# read in and select relevant data:
data = read.csv(file.path("./dev/28day_clean_test_grouped_1m_delayRate_exclusively_protocol_not_screened.csv")) %>%
  dplyr::select(-mced_treated) %>%
  rename(year = period) %>%
  rename(y = percentage_not_told_diagnosis_outcome_within_28_days) %>%
  rename(unit = Cancer.Alliance) %>%
  select(year, unit, y)

# treat about half and one quarter of units
n_units_to_treat <- round(c(1,0.5) * (length(unique(data$unit)) / 2))

# Set up effects: 0 and 3 percentage points
effect_scenarios <- list(null = 0, three = 0.03)

# Models:
# Specify 2 models to simulate treatment effects: Linear fixed effect model,
#  and a linear model using ar-terms with no fixed-effects
lm_fe_unadj <- optic_model(
  name = "fixed_effect_linear",
  type = "reg",
  call = "lm",
  formula = y ~ as.factor(year) + as.factor(unit) + treatment_level,
  se_adjust = "cluster-unit"
)


lm_ar <- optic_model(
  name = "auto_regressive_linear",
  type = "autoreg",
  call = "lm",
  formula = y ~ as.factor(year) + treatment_change,
  se_adjust = "none"
)

m_csa <- optic_model(
  name = "csa_did",
  type = "did",
  call = "att_gt",
  formula = y ~ treatment_level,
  yname = "y",
  tname = 'year',
  idname = 'unit',
  gname = 'treatment_date',
  se_adjust = "none"
)

m_aug <- optic_model(
  name = "augsynth",
  type = "multisynth",
  call = "multisynth",
  formula = y ~ treatment_level,
  unit = as.name("unit"),
  time = as.name("year"),
  lambda = 0.1,
  se_adjust = "none"
)

# just do TWFE and AR for now
sim_models <- list(lm_fe_unadj, lm_ar, m_aug, m_csa)

sim_config <- optic_simulation(
  x                        = data,
  models                   = sim_models,
  iters                    = 10, # Use 1000 in real runs
  method                   = "no_confounding",
  unit_var                 = "unit",
  treat_var                = "unit",
  time_var                 = "year",
  effect_magnitude         = effect_scenarios,
  n_units                  = n_units_to_treat,
  effect_direction         = c("neg"),
  policy_speed             = c("instant"),
  n_implementation_periods = c(1) # How many should we do?
)

future::plan(multisession, workers = 3)

cat("Simulation started at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

results <- dispatch_simulations(
  sim_config,
  use_future = T,
  seed = 9782,
  verbose = 2,
  future.globals=c("cluster_adjust_se"),
  future.packages=c("MASS", "dplyr", "optic", "augsynth", "did", "fixest")
)

cat("Simulation ended at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
future::plan(sequential)
