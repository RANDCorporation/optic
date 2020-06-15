library(optic)
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


cl <- parallel::makeCluster(8L)
plan("cluster", workers = cl) 

#==============================================================================
#==============================================================================
# SELECTION BIAS
#==============================================================================
#==============================================================================
my_models <- list(
  fixedeff_linear = list(
    model_call="lm",
    model_formula=crude.rate ~ treatment + unemploymentrate + as.factor(year) + as.factor(state),
    model_args=list(weights=as.name("population"))
  ),
  autoreg_linear = list(
    model_call="lm",
    model_formula=crude.rate ~ treatment + unemploymentrate + as.factor(year) + offset(log(population))
  ),
  fixedeff_negbin = list(
    model_call="MASS::glm.nb",
    model_formula=crude.rate ~ treatment + unemploymentrate + as.factor(year) + as.factor(state),
    model_args=list(weights=as.name("population"))
  ),
  autoreg_negbin = list(
    model_call="MASS::glm.nb",
    model_formula=crude.rate ~ treatment + unemploymentrate + as.factor(year) + offset(log(population))
  )
)

# test selection bias
test <- configure_simulation(
  # data and models required
  x=x,
  models=my_models,
  # iterations
  iters=5000,
  
  # specify functions or S3 class of set of functions
  method_class="simulation",
  method_sample=selbias_sample,
  method_te=selbias_te,
  method_pre_model=NULL,
  method_model=selbias_model,
  method_post_model=NULL,
  method_results=selbias_results,
  
  # parameters that will be expanded and added
  params=list(
    unit_var="state",
    time_var="year",
    policy_speed=list("slow", "instant"),
    n_implementation_periods=list(3),
    b_vals=list(c(b0=-5, b1=0.05, b2=0.1),
                c(b0=-5, b1=0.1, b2=0.15),
                c(b0=-5, b1=0.2, b2=0.3))
  )
)

r <- dispatch_simulations(test, use_future = TRUE, verbose = 2)



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
  add_lag="crude.rate",
  rhos=c(0, 0.25, 0.5, 0.75, 0.9)
)

start2 <- Sys.time()
autoregressive_results <- dispatch_simulations(autoregressive, use_future=TRUE, seed=285)
end2 <- Sys.time()

# could just enter 0.05, 0.10, and 0.15 respectively but want to mimic what
# we put into linear runs
nb5 <- 690 / (sum(x$deaths) / length(unique(x$year)))
nb10 <- 1380 / (sum(x$deaths) / length(unique(x$year)))
nb15 <- 2070 / (sum(x$deaths) / length(unique(x$year)))

scenario1nb <- c(nb10, nb10)
scenario2nb <- c(nb5, nb15)

two_way_fe <- configure_simulation(
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

autoregressive <- configure_simulation(
  x=x,
  unit_var="state",
  time_var="year",
  model_call=list("glm.nb", "glm.nb"),
  model_formula=list(
    deaths ~ unemploymentrate + as.factor(year) + offset(log(population)) + treatment1 + treatment2,
    deaths ~ unemploymentrate + as.factor(year) + offset(log(population)) + treatment1
  ),
  effect_magnitude=list(scenario1nb),
  n_units=c(30),
  iters=5000,
  effect_direction=c("null", "neg"),
  policy_speed=c("instant"),
  n_implementation_periods=3,
  se_adjust=c("cluster"),
  concurrent=TRUE,
  change_code_treatment=TRUE,
  lag_outcome=TRUE,
  rhos=c(0, 0.25, 0.5, 0.75, 0.9)
)

start1 <- Sys.time()
two_way_nb_results <- dispatch_simulations(two_way_fe, use_future=TRUE, seed=474)
saveRDS(two_way_nb_results, paste0("data/negbin-two-way-fe-",Sys.Date(), ".rds"))
end1 <- Sys.time()

start2 <- Sys.time()
autoreg_nb_results <- dispatch_simulations(autoregressive, use_future=TRUE, seed=89)
saveRDS(autoreg_nb_results, paste0("data/negbin-autorgressive-",Sys.Date(), ".rds"))
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
