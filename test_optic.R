#install.packages("/poppy/programs/bethg/concurrent/optic_0.1.tar.gz", repos=NULL, type="source")

library(optic)
library(future)
library(future.apply)

load("data/optic_sim_data_exp.Rdata")
#load('C:/Users/bethg/Documents/OPTIC/Simulation Project/results/optic_sim_data_exp.Rdata')
names(x) <- tolower(names(x))

# set parallel plan
plan(tweak(multiprocess, workers=16))
options(future.globals.onReference = "ignore")

#==============================================================================
#==============================================================================
# SIM OVERHAUL
#==============================================================================
#==============================================================================
# Here's what needs to happen before configure_simulation in terms of data
# transformation and prep
te_linear <- 3500 / ( ( sum(x$population) / length(unique(x$year)) ) / 100000)
te_log <- 3500 / ( sum(x$deaths) / length(unique(x$year)) )


# TODO:
#   - before applying true effect need to make sure:
#       - "lm" use raw input TE, for neg multiply by -1
#       - "glm", "glm.nb" should be percent change as decimal, so for pos do 1 + te, for neg 1 - te

test_config <- configure_simulation(
  x=x,
  unit_var="state",
  time_var="year",
  model_call="lm",
  model_formula=crude.rate ~ unemploymentrate + as.factor(year) + as.factor(state) + treatment1 + treatment2,
  effect_magnitude=list(te_linear),
  n_units=c(5, 15, 30),
  iters=10,
  effect_direction=c("null", "pos"),
  model_args=list(
    list(weights=as.name('population'))
  ),
  policy_speed=c("instant", "slow"),
  n_implementation_periods=3,
  se_adjust=c("cluster", "huber", "huber-cluster"),
  concurrent=TRUE,
  rhos=c(0, 0.25, 0.5, 0.75, 0.9)
)

r <- dispatch_simulations(test_config, seed=372)
r2 <- dispatch_simulations(test_config, seed=372)
r3 <- dispatch_simulations(test_config, seed=143)


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

rhos=c(0,.25,.5,.75,.9) #not - obvious - models will not estimate both policies when corr = 1

for(r in 1:length(rhos))
{
  # define parallel cluster
  cl <- parallel::makeCluster(32L)
  plan("cluster", workers = cl) 
  
  rho=rhos[r]
  print(paste("Currently running simulations for rho:", rho))
  
  results <- future_lapply(
    1:Sim1$iters,
    FUN=function(i){
      rr <- run_iteration_concurrent(Sim1, rho=rho)
      rr$iter <- i
      return(rr)
    },
    future.seed=ceiling((2733 * rho) + 348)
  )
  
  # combine results together
  full_results <- do.call("rbind", results)
  names(full_results)[14]<-"joint.eff.variance"
  names(full_results)[15]<-"joint.eff.t_stat"
  
  write.csv(full_results, paste0("all_iters_linear_2wayfe_null_concurrent_n30_rho", rho, "_", Sys.Date(), ".csv"),
            row.names = FALSE)
  
  plan(sequential)
}

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

rhos=c(.25,.5,.75,.9) #not - obvious - models will not estimate both policies when corr = 1

for(r in 1:length(rhos))
{
  # define parallel cluster
  rho=rhos[r]
  print(paste("Currently running simulations for rho:", rho))
  
  results <- future_lapply(
    1:Sim2$iters,
    FUN=function(i){
      rr <- run_iteration_concurrent(Sim2, rho=rho)
      rr$iter <- i
      return(rr)
    },
    future.seed=ceiling((9814 * rho) + 348)
  )
  
  # combine results together
  full_results <- do.call("rbind", results)
  names(full_results)[14]<-"joint.eff.variance"
  names(full_results)[15]<-"joint.eff.t_stat"
  
  write.csv(full_results, paste0("all_iters_linear_2wayfe_pos_concurrent_n30_rho", rho, "_", Sys.Date(), ".csv"),
            row.names = FALSE)
}

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

rhos=c(0,.25,.5,.75,.9) #not - obvious - models will not estimate both policies when corr = 1

for(r in 1:length(rhos))
{
  rho=rhos[r]
  print(paste("Currently running simulations for rho:", rho))
  
  results <- future_lapply(
    1:Sim3$iters,
    FUN=function(i){
      rr <- run_iteration_concurrent(Sim3, rho=rho)
      rr$iter <- i
      return(rr)
    },
    future.seed=ceiling((982 * rho) + 348)
  )
  
  # combine results together
  full_results <- do.call("rbind", results)
  names(full_results)[14]<-"joint.eff.variance"
  names(full_results)[15]<-"joint.eff.t_stat"
  
  write.csv(full_results, paste0("all_iters_linear_ar_null_concurrent_n30_rho", rho, "_", Sys.Date(), ".csv"),
            row.names = FALSE)
}


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

rhos=c(0,.25,.5,.75,.9) #not - obvious - models will not estimate both policies when corr = 1

for(r in 1:length(rhos))
{
  rho=rhos[r]
  print(paste("Currently running simulations for rho:", rho))
  
  results <- future_lapply(
    1:Sim4$iters,
    FUN=function(i){
      rr <- run_iteration_concurrent(Sim4, rho=rho)
      rr$iter <- i
      return(rr)
    },
    future.seed=ceiling((982 * rho) + 348)
  )
  
  # combine results together
  full_results <- do.call("rbind", results)
  names(full_results)[14]<-"joint.eff.variance"
  names(full_results)[15]<-"joint.eff.t_stat"
  
  write.csv(full_results, paste0("all_iters_linear_ar_pos_concurrent_n30_rho", rho, "_", Sys.Date(), ".csv"),
            row.names = FALSE)
}
