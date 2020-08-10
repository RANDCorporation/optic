library(optic)
library(augsynth)
library(dplyr)
library(future)
library(future.apply)
library(DRDID)

load("data/optic_sim_data_exp.Rdata")
names(x) <- tolower(names(x))

x <- x %>%
  arrange(state, year) %>%
  group_by(state) %>%
  mutate(lag1 = lag(crude.rate, n=1L),
         lag2 = lag(crude.rate, n=2L),
         lag3 = lag(crude.rate, n=3L)) %>%
  ungroup() %>%
  # code in moving average and trend versions of prior control
  mutate(prior_control_mva3 = rowMeans(select(., lag1, lag2, lag3)),
         prior_control_trend = lag1 - lag3) %>%
  select(-lag1, -lag2, -lag3)


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



concurrent_models <- list(
  list(
    name="fixedeff_linear",
    type="reg",
    model_call="lm",
    model_formula=crude.rate ~ unemploymentrate + as.factor(year) + as.factor(state) + treatment1 + treatment2,
    model_args=list(weights=as.name('population')),
    se_adjust=c("none", "huber", "cluster", "arellano")
  ),
  list(
    name="fixedeff_linear",
    type="reg",
    model_call="lm",
    model_formula=crude.rate ~ unemploymentrate + as.factor(year) + as.factor(state) + treatment1 + treatment2,
    model_args=list(weights=as.name('population')),
    se_adjust=c("none", "huber", "cluster", "arellano")
  )
)

linear_fe <- configure_simulation(
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

linear_ar <- configure_simulation(
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
  add_lag="crude.rate",
  rhos=c(0, 0.25, 0.5, 0.75, 0.9)
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

negbin_fe <- configure_simulation(
  x=x,
  unit_var="state",
  time_var="year",
  model_call=list("glm.nb", "glm.nb"),
  model_formula=list(
    deaths ~ unemploymentrate + as.factor(year) + as.factor(state) + offset(log(population)) + treatment1 + treatment2,
    deaths ~ unemploymentrate + as.factor(year) + as.factor(state) + offset(log(population)) + treatment1
  ),
  effect_magnitude=list(scenario1nb, scenario2nb),
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

negbin_ar <- configure_simulation(
  x=x,
  unit_var="state",
  time_var="year",
  model_call=list("glm.nb", "glm.nb"),
  model_formula=list(
    deaths ~ unemploymentrate + as.factor(year) + offset(log(population)) + treatment1 + treatment2,
    deaths ~ unemploymentrate + as.factor(year) + offset(log(population)) + treatment1
  ),
  effect_magnitude=list(scenario1nb, scenario2nb),
  n_units=c(5, 30),
  iters=5000,
  effect_direction=c("null", "neg"),
  policy_speed=c("instant", "slow"),
  n_implementation_periods=3,
  se_adjust=c("cluster"),
  concurrent=TRUE,
  change_code_treatment=TRUE,
  lag_outcome=TRUE,
  rhos=c(0, 0.25, 0.5, 0.75, 0.9)
)


start <- Sys.time()

cl <- parallel::makeCluster(16L)
plan("cluster", workers = cl) 
linear_fe_results <- dispatch_simulations(linear_fe, use_future=TRUE, seed=218)
saveRDS(linear_fe_results, paste0("data/linear-fe-", Sys.Date(), ".rds"))

cl <- parallel::makeCluster(16L)
plan("cluster", workers = cl) 
linear_ar_results <- dispatch_simulations(linear_ar, use_future=TRUE, seed=874)
saveRDS(linear_ar_results, paste0("data/linear-ar-2020-05-09.rds"))

cl <- parallel::makeCluster(16L)
plan("cluster", workers = cl) 
negbin_fe_results <- dispatch_simulations(negbin_fe, use_future=TRUE, seed=981)
saveRDS(negbin_fe_results, paste0("data/negbin-fe-", Sys.Date(), ".rds"))

cl <- parallel::makeCluster(16L)
plan("cluster", workers = cl) 
negbin_ar_results <- dispatch_simulations(negbin_ar, use_future=TRUE, seed=321)
saveRDS(negbin_ar_results, paste0("data/negbin-ar-", Sys.Date(), ".rds"))

end <- Sys.time()

print(end-start)