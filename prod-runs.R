library(optic)
library(future)
library(future.apply)

load("data/optic_sim_data_exp.Rdata")
names(x) <- tolower(names(x))

cl <- parallel::makeCluster(16L)
plan("cluster", workers = cl) 

#==============================================================================
#==============================================================================
# LINEAR RUNS - lm
#==============================================================================
#==============================================================================
linear5 <- 690 / ((sum(x$population) / length(unique(x$year))) / 100000)
linear10 <- 1380 / ((sum(x$population) / length(unique(x$year))) / 100000)
linear15 <- 2070 / ((sum(x$population) / length(unique(x$year))) / 100000)

scenario1 <- c(linear10, linear10)
scenario2 <- c(linear5, linear15)

two_way_fe <- configure_simulation(
  x=x,
  unit_var="state",
  time_var="year",
  model_call=list("lm", "lm"),
  model_formula=list(
    crude.rate ~ unemploymentrate + as.factor(year) + as.factor(state) + treatment1 + treatment2,
    crude.rate ~ unemploymentrate + as.factor(year) + as.factor(state) + treatment1
  ),
  model_args=list(
    list(weights=as.name('population')),
    list(weights=as.name('population'))
  ),
  effect_magnitude=list(scenario1, scenario2),
  n_units=c(5, 30),
  iters=5000,
  effect_direction=c("null", "neg"),
  policy_speed=c("instant", "slow"),
  n_implementation_periods=3,
  se_adjust=c("cluster", "huber", "huber-cluster"),
  concurrent=TRUE,
  change_code_treatment=FALSE,
  rhos=c(0, 0.25, 0.5, 0.75, 0.9)
)

autoregressive <- configure_simulation(
  x=x,
  unit_var="state",
  time_var="year",
  model_call=list("lm", "lm"),
  model_formula=list(
    crude.rate ~ unemploymentrate + as.factor(year) + treatment1 + treatment2,
    crude.rate ~ unemploymentrate + as.factor(year) + treatment1
  ),
  effect_magnitude=list(scenario1, scenario2),
  n_units=c(5, 30),
  iters=5000,
  effect_direction=c("null", "neg"),
  model_args=list(
    list(weights=as.name('population')),
    list(weights=as.name('population'))
  ),
  policy_speed=c("instant", "slow"),
  n_implementation_periods=3,
  se_adjust=c("cluster", "huber", "huber-cluster"),
  concurrent=TRUE,
  change_code_treatment=TRUE,
  lag_outcome=TRUE,
  rhos=c(0, 0.25, 0.5, 0.75, 0.9)
)

start2 <- Sys.time()
autoregressive_results <- dispatch_simulations(autoregressive, use_future=TRUE, seed=285)
end2 <- Sys.time()


#==============================================================================
#==============================================================================
# REPLICATION RUN
#==============================================================================
#==============================================================================
rep_two_way_fe <- configure_simulation(
  x=x,
  unit_var="state",
  time_var="year",
  model_call=list("lm"),
  model_formula=list(
    crude.rate ~ unemploymentrate + as.factor(year) + as.factor(state) + treatment
  ),
  model_args=list(
    list(weights=as.name('population'))
  ),
  effect_magnitude=list(0.161433),
  n_units=1,
  iters=5000,
  effect_direction="null",
  policy_speed="instant",
  n_implementation_periods=3,
  time_period_restriction=c(2003:2016),
  se_adjust=c("cluster", "huber", "huber-cluster"),
  concurrent=FALSE,
  change_code_treatment=FALSE
)

replicate_results <- dispatch_simulations(rep_two_way_fe, use_future=FALSE, seed=1234567)
