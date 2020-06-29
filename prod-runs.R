library(optic)
library(dplyr)
library(future)
library(future.apply)

load("data/optic_sim_data_exp.Rdata")
names(x) <- tolower(names(x))

x <- x %>%
  arrange(state, year) %>%
  group_by(state) %>%
  mutate(lag1 = lag(crude.rate, n=1L),
         lag2 = lag(crude.rate, n=2L),
         lag3 = lag(crude.rate, n=3L)) %>%
  ungroup() %>%
  mutate(moving.ave3 = rowMeans(select(., lag1, lag2, lag3))) %>%
  select(-lag1, -lag2, -lag3)


cl <- parallel::makeCluster(16L)
plan("cluster", workers = cl) 

#==============================================================================
#==============================================================================
# SELECTION BIAS
#==============================================================================
#==============================================================================
source("R/selection-bias-methods.R")
source("R/cluster-adjust-se.r")

my_models <- list(
  list(
    name="fixedeff_linear",
    type="reg",
    model_call="lm",
    model_formula=crude.rate ~ treatment_level + unemploymentrate + as.factor(year) + as.factor(state),
    model_args=list(weights=as.name("population")),
    se_adjust=c("none", "huber", "cluster", "arellano")
  ),
  list(
    name="autoreg_linear",
    model_call="lm",
    type="autoreg",
    model_formula=crude.rate ~ treatment_change + unemploymentrate + as.factor(year),
    model_args=list(weights=as.name("population")),
    se_adjust=c("none", "huber", "cluster", "arellano")
  )
  # fixedeff_negbin = list(
  #   model_call="glm.nb",
  #   model_formula=deaths ~ treatment + unemploymentrate + as.factor(year) + as.factor(state) + offset(log(population))
  # ),
  # autoreg_negbin = list(
  #   model_call="glm.nb",
  #   model_formula=deaths ~ change_code_treatment + lag_crude.rate + unemploymentrate + as.factor(year) + offset(log(population))
  # )
)

# test selection bias
test <- configure_simulation(
  # data and models required
  x=x,
  models=my_models,
  # iterations
  iters=5000,
  
  # specify functions or S3 class of set of functions
  method_sample=selbias_sample,
  method_pre_model=selbias_premodel,
  method_model=selbias_model,
  method_post_model=selbias_postmodel,
  method_results=selbias_results,
  
  # parameters that will be expanded and added
  params=list(
    unit_var="state",
    time_var="year",
    policy_speed=list("slow", "instant"),
    n_implementation_periods=list(3),
    b_vals=list(c(b0=-5, b1=0.05, b2=0.1),
                c(b0=-5, b1=0.1, b2=0.15),
                c(b0=-5, b1=0.2, b2=0.3)),
    a_vals=list(c(a1=0.95, a2=0.05))
  )
)

cl <- parallel::makeCluster(16L)
plan("cluster", workers = cl) 

r <- dispatch_simulations(test, use_future = TRUE, verbose = 2, future.globals=c("cluster_adjust_se"), future.packages=c("dplyr", "MASS", "optic"))

full_r <- do.call(rbind, r)
rownames(full_r) <- NULL
write.csv(full_r, "data/sel-bias-linear-trial-runs.csv", row.names = FALSE)

# need to do some tuning for negative binomial models
my_models <- list(
  list(
    name="fixedeff_negbin",
    type="reg",
    model_call="glm.nb",
    model_formula=deaths ~ treatment_level + unemploymentrate + as.factor(year) + as.factor(state) + offset(log(population)),
    se_adjust=c("none")
  )
)

negbin_tuning <- configure_simulation(
  # data and models required
  x=x,
  models=my_models,
  # iterations
  iters=100,
  
  # specify functions or S3 class of set of functions
  method_sample=selbias_sample,
  method_pre_model=selbias_premodel,
  method_model=selbias_model,
  method_post_model=selbias_postmodel,
  method_results=selbias_results,
  
  # parameters that will be expanded and added
  params=list(
    unit_var="state",
    time_var="year",
    policy_speed=list("instant"),
    n_implementation_periods=list(3),
    b_vals=list(c(b0=-5, b1=0.05, b2=0.1)),
    a_vals=list(c(a1=0.95, a2=0.05),
                c(a1=0.90, a2=0.10),
                c(a1=0.85, a2=0.15),
                c(a1=0.80, a2=0.20),
                c(a1=0.75, a2=0.25),
                c(a1=0.70, a2=0.30),
                c(a1=0.65, a2=0.35),
                c(a1=0.60, a2=0.40),
                c(a1=0.55, a2=0.45),
                c(a1=0.50, a2=0.50),
                c(a1=0.45, a2=0.55),
                c(a1=0.40, a2=0.60),
                c(a1=0.35, a2=0.65),
                c(a1=0.30, a2=0.70),
                c(a1=0.25, a2=0.75),
                c(a1=0.20, a2=0.80),
                c(a1=0.15, a2=0.85),
                c(a1=0.10, a2=0.90),
                c(a1=0.05, a2=0.95))
  )
)

single_simulation <- negbin_tuning$setup_single_simulation(1)


cl <- parallel::makeCluster(8L)
plan("cluster", workers = cl) 
r <- dispatch_simulations(negbin_tuning, use_future = TRUE, verbose = 2, future.globals=c("cluster_adjust_se"), future.packages=c("dplyr", "MASS", "optic"))
nb_tuning <- do.call(rbind, r)
rownames(nb_tuning) <- NULL
write.csv(nb_tuning, "data/negbin_tuning_alpha.csv", row.names = FALSE)

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
