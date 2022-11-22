
# Testing the base case of the package:

data(overdoses)
x <- overdoses

# we will define two scenarios for different effect magnitudes for
# the policies using 5, 10, and 15 percent changes in the outcome
linear5 <- 690 / ((sum(x$population) / length(unique(x$year))) / 100000)
linear10 <- 1380 / ((sum(x$population) / length(unique(x$year))) / 100000)
linear15 <- 2070 / ((sum(x$population) / length(unique(x$year))) / 100000)

scenario1 <- c(linear10, linear10)
scenario2 <- c(linear5, linear15)

# Question: Any suggestion for the name of the object: om? optic_model?
# Question: What are constraints that we should set for each of those parameters:

model_1 <- optic_model(
  name="fixedeff_linear",
  type="reg",
  call="lm",
  formula=crude.rate ~ unemploymentrate + as.factor(year) + as.factor(state) + treatment1_level + treatment2_level,
  args=list(weights=as.name('population')),
  se_adjust=c("none", "cluster"))

model_2 <- optic_model(name="autoreg_linear",
                       type="autoreg",
                       call="lm",
                       formula=deaths ~ unemploymentrate + as.factor(year) + treatment1_change + treatment2_change,
                       args=list(weights=as.name('population')),
                       se_adjust=c("none", "cluster"))



test_that("can create optic_model", {
  
  expect_s3_class(model_1, "optic_model")
  
})


optic_sim <- optic_simulation(
  x=overdoses,
  models=list(model_1, model_2),
  iters=2,
  method_sample=concurrent_sample,
  method_pre_model=concurrent_premodel,
  method_model=concurrent_model,
  method_post_model=concurrent_postmodel,
  method_results=concurrent_results,
  
  params=list(
    unit_var="state",
    time_var="year",
    effect_magnitude=list(scenario1, scenario2),
    n_units=c(10),
    effect_direction=c("null", "neg", "pos"),
    policy_speed=c("instant", "slow"),
    n_implementation_periods=c(3),
    rhos=c(0, 0.5, 0.9),
    years_apart=2,
    ordered=TRUE
  )
)

lm_results <- simulate(
  optic_sim,
  use_future=TRUE,
  seed=9782,
  verbose=2,
  future.globals=c("cluster_adjust_se"),
  future.packages=c("dplyr", "optic")
)

results <- do.call(rbind, lm_results) %>% as.data.frame()

test_that("simulation results seem sensible", {
  
  expect_type(lm_results, "list")

  expect_false(any(is.na(results)))
  
})