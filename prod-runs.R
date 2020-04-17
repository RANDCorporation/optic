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
scenario1 <- c(0.10, 0.10)
scenario2 <- c(0.05, 0.15)
  
two_way_fe <- configure_simulation(
  x=x,
  unit_var="state",
  time_var="year",
  model_call=list("lm", "lm", "glm.nb", "glm.nb"),
  model_formula=list(
    crude.rate ~ unemploymentrate + as.factor(year) + as.factor(state) + treatment1 + treatment2,
    crude.rate ~ unemploymentrate + as.factor(year) + as.factor(state) + treatment1,
    crude.rate ~ unemploymentrate + as.factor(year) + as.factor(state) + offset(log(population)) + treatment1 + treatment2,
    crude.rate ~ unemploymentrate + as.factor(year) + as.factor(state) + offset(log(population)) + treatment1
  ),
  model_args=list(
    list(weights=as.name('population')),
    list(weights=as.name('population')),
    list(),
    list()
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
  model_call=list("lm", "lm", "glm.nb", "glm.nb"),
  model_formula=list(
    crude.rate ~ unemploymentrate + as.factor(year) + treatment1 + treatment2,
    crude.rate ~ unemploymentrate + as.factor(year) + treatment1,
    crude.rate ~ unemploymentrate + as.factor(year) + offset(log(population)) + treatment1 + treatment2,
    crude.rate ~ unemploymentrate + as.factor(year) + offset(log(population)) + treatment1
  ),
  effect_magnitude=list(scenario1, scenario2),
  n_units=c(5, 30),
  iters=5000,
  effect_direction=c("null", "neg"),
  model_args=list(
    list(weights=as.name('population')),
    list(weights=as.name('population')),
    list(),
    list()
  ),
  policy_speed=c("instant", "slow"),
  n_implementation_periods=3,
  se_adjust=c("cluster", "huber", "huber-cluster"),
  concurrent=TRUE,
  change_code_treatment=TRUE,
  rhos=c(0, 0.25, 0.5, 0.75, 0.9)
)

start1 <- Sys.time()
two_way_fe_results <- dispatch_simulations(two_way_fe, use_future=TRUE, seed=943)
end1 <- Sys.time()

start2 <- Sys.time()
autoregressive_results <- dispatch_simulations(autoregressive, use_future=TRUE, seed=285)
end2 <- Sys.time()



#==============================================================================
#==============================================================================
# GLM NEGATIVE BINOMIAL RUNS
#==============================================================================
#==============================================================================
glm_nb <- configure_simulation(
  x=x,
  unit_var="state",
  time_var="year",
  model_call=list("glm.nb"),
  model_formula=crude.rate ~ unemploymentrate + as.factor(year) + as.factor(state) + offset(log(population)) + treatment1 + treatment2,
  effect_magnitude=c(0.05, 0.10),
  n_units=c(5, 30),
  iters=50,
  effect_direction=c("null", "neg"),
  policy_speed=c("instant", "slow"),
  n_implementation_periods=3,
  se_adjust=c("cluster", "huber", "huber-cluster"),
  concurrent=TRUE,
  change_code_treatment=FALSE,
  rhos=c(0, 0.25, 0.5, 0.75, 0.9)
)

glm_nb_results <- dispatch_simulations(glm_nb, use_future=TRUE, seed=2734)
