# Test: autoeffect integration within OPTIC vs. TWFE baseline
# ------------------------------------------------------------------
# This script builds a synthetic panel with a known average treatment
# effect, runs an optic simulation that includes both a TWFE model
# (feols) and the autoeffect debiased AR model, and compares estimated
# effects against the ground truth.
#
# Usage:
#   Rscript dev/test_optic_autoeffect_integration.R
#
# Requirements:
#   - optic package installed (or built locally and added to .libPaths)
#   - autoeffect package installed
#   - fixest package

suppressPackageStartupMessages({
  library(dplyr)
  library(fixest)
  library(optic)
  library(autoeffect)
})

set.seed(1234)

make_synthetic_panel <- function(n_units = 20, years = 2000:2015,
                                 noise_sd = 0.05) {
  df <- expand.grid(
    unit = paste0("unit_", seq_len(n_units)),
    year = years
  ) %>%
    arrange(unit, year) %>%
    group_by(unit) %>%
    mutate(
      epsilon = rnorm(n(), mean = 0, sd = noise_sd),
      baseline = 50 + 0.2 * (year - min(year)),
      outcome = baseline + epsilon
    ) %>%
    ungroup()

  df$population <- 1000
  df$unemploymentrate <- 5
  df$prior_control_level_OLD <- df$baseline
  df$prior_control_trend_OLD <- df$baseline
  # Note: treatment, treatment_level, treatment_change, and trt_ind
  # are created by the simulation, not pre-populated

  df
}

synthetic_data <- make_synthetic_panel()

twfe_model <- optic_model(
  name = "twfe",
  type = "reg",
  call = "feols",
  formula = outcome ~ treatment_level + as.factor(year) + as.factor(unit),
  weights = as.name("population"),
  se_adjust = "none"
)

autoeffect_model <- optic_model(
  name = "autoeffect_debiased",
  type = "autoeffect",
  call = "autoeffect",
  formula = outcome ~ treatment_level,
  se_adjust = "none",
  lags = 2,
  unit_name = "unit",
  date_name = "year",
  trt_name = "trt_ind",
  outcome_name = "outcome",
  effect_lag = 2
)

sim <- optic_simulation(
  x = synthetic_data,
  models = list(twfe_model, autoeffect_model),
  iters = 1,
  method = "no_confounding",
  unit_var = "unit",
  treat_var = "unit",
  time_var = "year",
  effect_magnitude = list(5),
  n_units = c(5),
  effect_direction = c("neg"),
  policy_speed = list("instant"),
  n_implementation_periods = c(0),
  prior_control = c("trend")
)

results <- dispatch_simulations(
  sim,
  use_future = FALSE,
  seed = 42,
  verbose = 0
)

comparison <- results %>%
  select(model_name, estimate, se) %>%
  mutate(true_effect = -5)

print(comparison)

output_path <- file.path("dev", "test_optic_autoeffect_integration_results.csv")
write.csv(comparison, output_path, row.names = FALSE)
cat("Results written to", output_path, "\n")
