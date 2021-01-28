library(optic)
library(augsynth)
library(dplyr)
library(future)
library(future.apply)
library(MASS)

load("data/optic_sim_data_exp.Rdata")
names(x) <- tolower(names(x))

source("R/concurrent-methods.R")

#==============================================================================
#==============================================================================
# CONCURRENT POLICIES
#==============================================================================
#==============================================================================
linear5 <- 690 / ((sum(x$population) / length(unique(x$year))) / 100000)
linear10 <- 1380 / ((sum(x$population) / length(unique(x$year))) / 100000)
linear15 <- 2070 / ((sum(x$population) / length(unique(x$year))) / 100000)

scenario1 <- c(linear10, linear10)
scenario2 <- c(linear5, linear15)

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
    effect_magnitude=list(scenario1, scenario2),
    n_units=c(30),
    effect_direction=c("null", "neg"),
    policy_speed=c("instant", "slow"),
    n_implementation_periods=c(3),
    rhos=c(0, 0.25, 0.5, 0.75, 0.9),
    years_apart=c(3)#6,9
  )
)

#==============================================================================
#==============================================================================
# NEGATIVE BINOMIAL RUNS
#==============================================================================
#==============================================================================
# could just enter 0.05, 0.10, and 0.15 respectively but want to mimic what
# we put into linear runs
nb5 <- 690 / (sum(x$deaths) / length(unique(x$year)))
nb10 <- 1380 / (sum(x$deaths) / length(unique(x$year)))
nb15 <- 2070 / (sum(x$deaths) / length(unique(x$year)))

scenario1nb <- c(nb10, nb10)
scenario2nb <- c(nb5, nb15)

negbin_concurrent_models <- list(
  list(
    name="fixedeff_negbin",
    type="reg",
    model_call="glm.nb",
    model_formula=deaths ~ unemploymentrate + as.factor(year) + as.factor(state) + offset(log(population)) + treatment1_level + treatment2_level,
    model_args=NULL,
    se_adjust=c("none", "cluster")
  ),
  list(
    name="fixedeff_negbin_misspec",
    type="reg",
    model_call="glm.nb",
    model_formula=deaths ~ unemploymentrate + as.factor(year) + as.factor(state) + offset(log(population)) + treatment1_level,
    model_args=NULL,
    se_adjust=c("none", "cluster")
  ),
  list(
    name="autoreg_negbin",
    type="autoreg",
    model_call="glm.nb",
    model_formula=deaths ~ unemploymentrate + as.factor(year) + offset(log(population)) + treatment1_change + treatment2_change,
    model_args=NULL,
    se_adjust=c("none", "cluster")
  ),
  list(
    name="autoreg_negbin_misspec",
    type="autoreg",
    model_call="glm.nb",
    model_formula=deaths ~ unemploymentrate + as.factor(year) + offset(log(population)) + treatment1_change,
    model_args=NULL,
    se_adjust=c("none", "cluster")
  )
)

negbin_config <- configure_simulation(
  x=x,
  models=negbin_concurrent_models,
  iters=5000,
  
  method_sample=concurrent_sample,
  method_pre_model=concurrent_premodel,
  method_model=concurrent_model,
  method_post_model=concurrent_postmodel,
  method_results=concurrent_results,
  
  params=list(
    unit_var="state",
    time_var="year",
    effect_magnitude=list(scenario1nb, scenario2nb),
    n_units=c(30),
    effect_direction=c("null", "neg"),
    policy_speed=c("instant", "slow"),
    n_implementation_periods=c(3),
    rhos=c(0, 0.25, 0.5, 0.75, 0.9),
    years_apart=c(3)#,6,9
  )
)

#==============================================================================
#==============================================================================
# RUN
#==============================================================================
#==============================================================================
cl <- parallel::makeCluster(parallel::detectCores() - 1)
plan("cluster", workers = cl)

start <- Sys.time()

lm_results <- dispatch_simulations(lm_config, use_future=T, seed=218, verbose=2, future.globals=c("cluster_adjust_se"), future.packages=c("dplyr", "MASS", "optic", "augsynth"))
lm_results2 <- do.call(rbind, lm_results)
rownames(lm_results2) <- NULL
write.csv(lm_results2, "/vincent/b/josephp/OPTIC/output/concurrent-lm-3yrGap.csv", row.names = FALSE)

end <- Sys.time()
print("Completed in:\n")
print(end - start)

start <- Sys.time()

nb_results <- dispatch_simulations(negbin_config, use_future=TRUE, seed=218, verbose=2, future.globals=c("cluster_adjust_se"), future.packages=c("dplyr", "MASS", "optic", "augsynth"))
nb_results2 <- do.call(rbind, nb_results)
rownames(nb_results2) <- NULL
write.csv(nb_results2, "/vincent/b/josephp/OPTIC/output/concurrent-negbin-3yrGap.csv", row.names = FALSE)

end <- Sys.time()
print("Completed in:\n")
print(end - start)
