# Generate pre-computed simulation results for the power analysis vignette.
# Run this script from the optic-core directory whenever the vignette
# parameters change. Output is tracked via git-lfs.
#
# Usage: Rscript dev/generate_power_results.R

library(optic)
library(dplyr)
library(augsynth)
library(did)
library(autoeffect)
library(future)

data(overdoses)

sim_data <- overdoses %>%
  filter(!(state %in% c("Nebraska", "Nevada", "Arkansas",
                         "Mississippi", "Oregon", "South Dakota",
                         "North Dakota")))

outcome_mean <- mean(sim_data$crude.rate, na.rm = TRUE)

twfe <- optic_model(
  name = "TWFE", type = "reg", call = "lm",
  formula = crude.rate ~ as.factor(year) + as.factor(state) +
    treatment_level + unemploymentrate,
  se_adjust = "cluster-unit"
)

csa <- optic_model(
  name = "CSA", type = "did", call = "att_gt",
  formula = crude.rate ~ treatment_level,
  se_adjust = "none",
  yname = "crude.rate", tname = "year", idname = "state",
  gname = "treatment_date"
)

ascm <- optic_model(
  name = "ASCM", type = "multisynth", call = "multisynth",
  formula = crude.rate ~ treatment_level,
  se_adjust = "none",
  unit = as.name("state"), time = as.name("year"), lambda = 1e-6
)

debar <- optic_model(
  name = "Debiased AR", type = "autoeffect", call = "autoeffect",
  formula = crude.rate ~ treatment_level + unemploymentrate,
  se_adjust = "cluster-unit", lags = 2
)

effect_pcts <- c(0, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30)
effect_magnitudes <- as.list(effect_pcts * outcome_mean)
n_treated <- c(5, 10, 20, 30)

sim <- optic_simulation(
  x = sim_data,
  # models = list(twfe, debar, ascm, csa),
  models = list(twfe, debar, csa),
  iters = 100,
  method = "no_confounding",
  unit_var = "state",
  treat_var = "state",
  time_var = "year",
  effect_magnitude = effect_magnitudes,
  n_units = n_treated,
  effect_direction = "neg",
  policy_speed = "instant",
  n_implementation_periods = 0,
  verbose = FALSE
)

plan(multisession, workers = 6)
results <- suppressWarnings(
  dispatch_simulations(
    sim, use_future = TRUE, seed = 47201, verbose = 1,
    future.packages = c("optic", "autoeffect", "augsynth", "did", "dplyr")
  )
)
plan(sequential)

if (!dir.exists("inst/extdata")) dir.create("inst/extdata", recursive = TRUE)
saveRDS(results, "inst/extdata/power_vignette_results.rds")
cat("Saved:", nrow(results), "rows to inst/extdata/power_vignette_results.rds\n")
