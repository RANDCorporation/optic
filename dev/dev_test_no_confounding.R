
devtools::load_all()
# library(optic)
library(tictoc)

data(overdoses)

linear0 <- 0
linear5 <- .05*mean(overdoses$opioid_rx, na.rm=T)
linear15 <- .15*mean(overdoses$opioid_rx, na.rm=T)
linear25 <- .25*mean(overdoses$opioid_rx, na.rm=T)


fixedeff_linear <- optic_model(
  name="fixedeff_linear",
  type="reg",
  call="lm",
  formula=opioid_rx ~ treatment_level + unemploymentrate + as.factor(year) + as.factor(state),
  weights=as.name("population"),
  se_adjust=c("none", "cluster")
)


no_confounding_fe_config <- optic_simulation(
  # data and models required
  x=overdoses,
  models=list(fixedeff_linear),
  # iterations
  iters=50, # 5000
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


tic()
no_confounding_results <- dispatch_simulations(
  no_confounding_fe_config,
  use_future=T,
  seed=9782,
  verbose=2,
  future.globals=c("cluster_adjust_se"),
  future.packages=c("MASS", "dplyr", "optic")
)
toc()

no_confounding_results_df <- do.call(rbind, no_confounding_results) %>% as.data.frame()


test_that("no_confounding simulations work", {
  
  expect_type(no_confounding_results, "list")
  
  expect_false(any(is.na(no_confounding_results_df)))
  
})

