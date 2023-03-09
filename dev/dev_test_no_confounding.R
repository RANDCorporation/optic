
devtools::load_all()
# library(optic)

data(overdoses)

fixedeff_linear <- optic_model(
  name="fixedeff_linear",
  type="reg",
  call="lm",
  formula=opioid_rx ~ treatment_level + unemploymentrate + as.factor(year) + as.factor(state),
  weights=as.name("population"),
  se_adjust=c("none", "cluster")
)


no_confounding_fe_config <- optic_simulation(
  x=overdoses,
  models=list(fixedeff_linear),
  iters=50,
  method = "no_confounding",
  globals=NULL,
  unit_var="state",
  treat_var="state",
  time_var="year",
  effect_magnitude=list(0, .05*mean(overdoses$opioid_rx, na.rm=T)),
  n_units= c(5),
  effect_direction=c("neg"),
  policy_speed=list("instant"),
  n_implementation_periods=c(0), 
  prior_control=c("mva3", "trend")
)

no_confounding_results <- dispatch_simulations(
  no_confounding_fe_config,
  use_future=T,
  seed=9782,
  verbose=2,
  future.globals=c("cluster_adjust_se"),
  future.packages=c("MASS", "dplyr", "optic")
)

no_confounding_results_df <- do.call(rbind, no_confounding_results) %>% as.data.frame()


test_that("no_confounding simulations work", {
  
  expect_type(no_confounding_results, "list")
  
  expect_false(any(is.na(no_confounding_results_df)))
  
})

