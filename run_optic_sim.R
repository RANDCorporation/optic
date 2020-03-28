#!/usr/bin/env Rscript
# script for leveraging forked processing
library(methods)
library(optic)
library(future)
library(future.apply)

cl <- parallel::makeCluster(8L)
plan(cluster, workers=cl)

args <- commandArgs(trailingOnly=TRUE)
sim_spec <- args[1]
run_label <- args[2]

sink_file <- paste0("data/logs/", run_label, "_", sim_spec, ".log")
con <- file(sink_file)
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")

load("data/optic_sim_data_exp.Rdata")
#load('C:/Users/bethg/Documents/OPTIC/Simulation Project/results/optic_sim_data_exp.Rdata')
names(x) <- tolower(names(x))

#==============================================================================
#==============================================================================
# SIM1 - NULL, CONCURRENT, LINEAR, 2 WAY
#==============================================================================
#==============================================================================
Sim1 <- OpticConfig$new(
  data=x,
  outcome="crude.rate",
  model_type="linear",
  method_call="lm",
  model_params=list(
    formula=~unemploymentrate + as.factor(year) + as.factor(state),
    weights=as.name('population')
  ),
  iters=5000,
  target_deaths=0,
  effect_direction="null",
  n_states=30,
  policy_speed="instant",
  number_implementation_years=3,
  time_periods=2003:2013,
  lag_outcome=FALSE
)


# This peice will be integrated into the package, but for now we need
# to update the model parameters and formula to pass when running
modelterms <- model_terms(Sim1$model_params$formula)
modelterms$lhs <- "outcome"
modelterms$rhs2 <- c("treatment1", modelterms$rhs)
modelterms$rhs <- c("treatment1","treatment2",modelterms$rhs)
# update model formula
if (Sim1$lag_outcome) {
  modelterms$rhs <- c(modelterms$rhs, "lag_outcome")
  modelterms$rhs2 <- c(modelterms$rhs2, "lag_outcome")
}

Sim1$model_params$formula <- as.formula(paste0(modelterms$lhs, "~", paste(modelterms$rhs, collapse="+")))

#==============================================================================
#==============================================================================
# SIM2 - POS, CONCURRENT, LINEAR, 2 WAY
#==============================================================================
#==============================================================================
Sim2 <- OpticConfig$new(
  data=x,
  outcome="crude.rate",
  model_type="linear",
  method_call="lm",
  model_params=list(
    formula=~unemploymentrate + as.factor(year) + as.factor(state),
    weights=as.name('population')
  ),
  iters=5000,
  target_deaths=3500,
  effect_direction="pos",
  n_states=30,
  policy_speed="instant",
  number_implementation_years=3,
  time_periods=2003:2013,
  lag_outcome=FALSE
)
# This peice will be integrated into the package, but for now we need
# to update the model parameters and formula to pass when running
modelterms <- model_terms(Sim2$model_params$formula)
modelterms$lhs <- "outcome"
modelterms$rhs2 <- c("treatment1", modelterms$rhs)
modelterms$rhs <- c("treatment1","treatment2",modelterms$rhs)
# update model formula
if (Sim2$lag_outcome) {
  modelterms$rhs <- c(modelterms$rhs, "lag_outcome")
  modelterms$rhs2 <- c(modelterms$rhs2, "lag_outcome")
}

Sim2$model_params$formula <- as.formula(paste0(modelterms$lhs, "~", paste(modelterms$rhs, collapse="+")))

#==============================================================================
#==============================================================================
# SIM3 - NULL, CONCURRENT, LINEAR, AUTOREGRESSIVE
#==============================================================================
#==============================================================================
Sim3 <- OpticConfig$new(
  data=x,
  outcome="crude.rate",
  model_type="linear",
  method_call="lm",
  model_params=list(
    formula=~unemploymentrate + as.factor(year),
    weights=as.name('population')
  ),
  iters=5000,
  target_deaths=0,
  effect_direction="null",
  n_states=30,
  policy_speed="instant",
  number_implementation_years=3,
  time_periods=2003:2013,
  lag_outcome=TRUE
)
# This peice will be integrated into the package, but for now we need
# to update the model parameters and formula to pass when running
modelterms <- model_terms(Sim3$model_params$formula)
modelterms$lhs <- "outcome"
modelterms$rhs2 <- c("treatment1", modelterms$rhs)
modelterms$rhs <- c("treatment1","treatment2",modelterms$rhs)
# update model formula
if (Sim3$lag_outcome) {
  modelterms$rhs <- c(modelterms$rhs, "lag_outcome")
  modelterms$rhs2 <- c(modelterms$rhs2, "lag_outcome")
}

Sim3$model_params$formula <- as.formula(paste0(modelterms$lhs, "~", paste(modelterms$rhs, collapse="+")))

#==============================================================================
#==============================================================================
# SIM4 - POS, CONCURRENT, LINEAR, AUTOREGRESSIVE
#==============================================================================
#==============================================================================
Sim4 <- OpticConfig$new(
  data=x,
  outcome="crude.rate",
  model_type="linear",
  method_call="lm",
  model_params=list(
    formula=~unemploymentrate + as.factor(year),
    weights=as.name('population')
  ),
  iters=5000,
  target_deaths=3500,
  effect_direction="pos",
  n_states=30,
  policy_speed="instant",
  number_implementation_years=3,
  time_periods=2003:2013,
  lag_outcome=TRUE
)

# This peice will be integrated into the package, but for now we need
# to update the model parameters and formula to pass when running
modelterms <- model_terms(Sim4$model_params$formula)
modelterms$lhs <- "outcome"
modelterms$rhs2 <- c("treatment1", modelterms$rhs)
modelterms$rhs <- c("treatment1","treatment2",modelterms$rhs)
# update model formula
if (Sim4$lag_outcome) {
  modelterms$rhs <- c(modelterms$rhs, "lag_outcome")
  modelterms$rhs2 <- c(modelterms$rhs2, "lag_outcome")
}

Sim4$model_params$formula <- as.formula(paste0(modelterms$lhs, "~", paste(modelterms$rhs, collapse="+")))

#==============================================================================
#==============================================================================
# SET UP FOR RUN
#==============================================================================
#==============================================================================
if (sim_spec == "Sim1") {
  Sim <- Sim1$clone()
}
if (sim_spec == "Sim2") {
  Sim <- Sim2$clone()
}
if (sim_spec == "Sim3") {
  Sim <- Sim3$clone()
}
if (sim_spec == "Sim4") {
  Sim <- Sim4$clone()
}

# TODO: testing with seed, here is a snipped to generate a list of seeds as long
# as vector X to pre-populate future.seed argument
# seeds <- future_lapply(seq_along(X), FUN = function(x) .Random.seed,
#                        future.chunk.size = Inf, future.seed = 42L)

rhos <- c(0, 0.25, 0.5, 0.75, 0.9)

for (rho in rhos) {
  cat(paste("\nCurrently running simulations for rho:", rho, "and Simulation:", sim_spec, run_label))
  
  r <- future_lapply(
    1:Sim$iters,
    FUN=function(i) {
      rr <- run_iteration_concurrent(Sim, rho=rho)
      rr$iter <- i
      rr$rho <- rho
      rr$model_approach <- run_label
      return(rr)
    }
  )
  
  full_results <- do.call("rbind", r)
  # combine results together
  names(full_results)[14]<-"joint.eff.variance"
  names(full_results)[15]<-"joint.eff.t_stat"
  
  write.csv(
    full_results,
    paste0("data/all_iters_",
           run_label, "_",
           Sim$effect_direction, "_concurrent_n30_rho",
           rho, "_", 
           Sys.Date(), ".csv"),
    row.names = FALSE
  )
}

plan(sequential)
parallel::stopCluster(cl)

sink() 
sink(type="message")
