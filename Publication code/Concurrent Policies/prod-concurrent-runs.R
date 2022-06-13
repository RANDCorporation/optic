# prod-concurrent-runs.R --------------------------------------------------
# 
# Production runs for the concurrent policies simulations.
# 
# Adam Scherling, 4/12/2021
# Based on code by Joe Pane


# load libraries ----------------------------------------------------------

library(optic)
library(augsynth)
library(dplyr)
library(future)
library(future.apply)
library(MASS)


# load data ---------------------------------------------------------------

load("optic_sim_data_exp.Rdata")

names(x) <- tolower(names(x))



# define scenario and model parameters ------------------------------------

linear0 <- 0 / ((sum(x$population) / length(unique(x$year))) / 100000)
linear5 <- 690 / ((sum(x$population) / length(unique(x$year))) / 100000)
linear10 <- 1380 / ((sum(x$population) / length(unique(x$year))) / 100000)
linear15 <- 2070 / ((sum(x$population) / length(unique(x$year))) / 100000)
linear20 <- 2760 / ((sum(x$population) / length(unique(x$year))) / 100000)

scenario0 <- c(linear0, linear0)
scenario1 <- c(linear10, linear10) #10/10
scenario2 <- c(linear5, linear15) #5/15
scenario3 <- c(linear15, linear5) #15/5
scenario4 <- c(linear10, linear20) #10/20
scenario5 <- c(linear15, linear0) #15/0
scenario6 <- c(linear0, linear15) #0/15

lm_concurrent_models <- list(
  list(
    name="fixedeff_linear",
    type="reg",
    model_call="lm",
    model_formula=crude.rate ~ unemploymentrate + as.factor(year) + as.factor(state) + treatment1_level + treatment2_level,
    model_args=list(weights=as.name('population')),
    se_adjust=c("none", "cluster")
  ),
  list(
    name="fixedeff_linear_misspec",
    type="reg",
    model_call="lm",
    model_formula=crude.rate ~ unemploymentrate + as.factor(year) + as.factor(state) + treatment1_level,
    model_args=list(weights=as.name('population')),
    se_adjust=c("none", "cluster")
  ),
  list(
    name="autoreg_linear",
    type="autoreg",
    model_call="lm",
    # the crude.rate lag is automatically added in the methods after calculating new crude.rate
    # based on augmented outcome (after treatment effect applied)
    model_formula=crude.rate ~ unemploymentrate + as.factor(year) + treatment1_change + treatment2_change,
    model_args=list(weights=as.name('population')),
    se_adjust=c("none", "cluster")
  ),
  list(
    name="autoreg_linear_misspec",
    type="autoreg",
    model_call="lm",
    model_formula=crude.rate ~ unemploymentrate + as.factor(year) + treatment1_change,
    model_args=list(weights=as.name('population')),
    se_adjust=c("none", "cluster") 
  )
)

lm_config <- configure_simulation(
  x=x,
  models=lm_concurrent_models,
  iters=5000,
  
  method_sample=concurrent_sample,
  method_pre_model=concurrent_premodel,
  method_model=concurrent_model,
  method_post_model=concurrent_postmodel,
  method_results=concurrent_results,
  
  params=list(
    unit_var="state",
    time_var="year",
    effect_magnitude=list(scenario6),# scenario0, scenario1, scenario2, scenario3, 
                          #scenario4, scenario5, scenario6),
    n_units=c(5, 30),
    effect_direction="neg",
    policy_speed=c("instant", "slow"),
    n_implementation_periods=c(3),
    rhos=c(0, 0.5, 0.9),
    years_apart = c(0, 3, 6, 9),
    ordered = c("yes", "no")
  )
)


# run the simulations -----------------------------------------------------

cl <- parallel::makeCluster((parallel::detectCores()-8))
plan("cluster", workers = cl)

start <- Sys.time()

lm_results <- dispatch_simulations(lm_config, use_future=TRUE, seed=218, verbose=2, future.globals=c("cluster_adjust_se"), future.packages=c("dplyr", "MASS", "optic", "augsynth"))
lm_results2 <- do.call(rbind, lm_results)
rownames(lm_results2) <- NULL
# change the output file name to match the scenario being run
write.csv(lm_results2, paste0("concurrent-lm-scen6_", Sys.Date(), ".csv"), row.names = FALSE)

end <- Sys.time()
print("Completed in:\n")
print(end - start)

