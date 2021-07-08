library(dplyr)
library(optic)
library(augsynth) 
library(future)
library(future.apply)
library(DRDID)

# setwd("/poppy/programs/josephp/optic-core")
load("data/optic_sim_data_exp.Rdata")
names(x) <- tolower(names(x))

x <- x %>%
  select(state, year, fipscode, population, unemploymentrate, povertyrate,
         income, statefipyear, opioid_rx, md_access, insured, uninsur,medicare,
         medicaid, syn_opioid_death, other_opioid_death, her_death, yrslifelost,
         medicaid_ratio, all_opioid_death, overdose, alldeaths, cr.opioid.death,
         opioid_rx.lag1, cr.opioid.death.lag1, crude.rate, cr.adj, deaths,
         cr.adj.lag1) %>%
  mutate(opioid_rx.old = opioid_rx) %>% # new line of code
  arrange(state, year) %>%
  group_by(state) %>%
  mutate(lag1 = lag(opioid_rx, n=1L),
         lag2 = lag(opioid_rx, n=2L),
         lag3 = lag(opioid_rx, n=3L)) %>%
  ungroup() %>%
  rowwise() %>%
  # code in moving average and trend versions of prior control
  mutate(prior_control_mva3_OLD = mean(c(lag1, lag2, lag3)),
         prior_control_trend_OLD = lag1 - lag3) %>%
  ungroup() %>%
  select(-lag1, -lag2, -lag3) %>%
  mutate(state = factor(as.character(state)))

source("R/dispatch-simulations.R")
source("R/no-confounding-methods.R")
source("R/apply-treatment-effect.R")
source("R/cluster-adjust-se.r")

#### 0%, 5%, 10%, 15%, 20% ####
linear0 <- 0
linear5 <- .05*mean(x$opioid_rx, na.rm=T)
linear15 <- .15*mean(x$opioid_rx, na.rm=T)
linear25 <- .25*mean(x$opioid_rx, na.rm=T)

#### Setup various no-confoudning sims ####
linear_models <- list(
  list(
    name="fixedeff_linear",
    type="reg",
    model_call="lm",
    model_formula=opioid_rx ~ treatment_level + unemploymentrate + as.factor(year) + as.factor(state),
    model_args=list(weights=as.name("population")),# NULL
    se_adjust=c("none", "cluster")
  ),
  list(
    name="autoreg_linear",
    model_call="lm",
    type="autoreg",
    model_formula=opioid_rx ~ treatment_change + unemploymentrate + as.factor(year),# + as.factor(state)
    model_args=list(weights=as.name("population")),# NULL
    se_adjust=c("none", "cluster-unit")
  )
)

# for the linear config, need separate ones for now
linear_fe_config <- configure_simulation(
  # data and models required
  x=x,
  models=list(linear_models[[1]]),
  # iterations
  iters=5000,
  
  # specify functions or S3 class of set of functions
  method_sample=noconf_sample,
  method_pre_model=noconf_premodel,
  method_model=noconf_model,
  method_post_model=noconf_postmodel,
  method_results=noconf_results,
  
  globals=NULL,#list(), # no globals as of now.
  
  # parameters that will be expanded and added
  params=list(
    unit_var="state",
    treat_var="state",
    time_var="year",
    effect_magnitude=list(linear0, linear5, linear15, linear25),
    n_units=c(1, 5, 15, 30), #2%, 10%, 30%, 60%
    effect_direction=c("neg"),
    policy_speed=list("instant"),
    n_implementation_periods=list(0), 
    prior_control=c("mva3", "trend")
  )
)

linear_ar_config <- configure_simulation(
  # data and models required
  x=x,
  models=list(linear_models[[2]]),
  # iterations
  iters=5000,
  
  # specify functions or S3 class of set of functions
  method_sample=noconf_sample,
  method_pre_model=noconf_premodel,
  method_model=noconf_model,
  method_post_model=noconf_postmodel,
  method_results=noconf_results,
  
  globals=NULL,#list(),
  
  # parameters that will be expanded and added
  params=list(
    unit_var="state",
    treat_var="state",
    time_var="year",
    effect_magnitude=list(linear0, linear5, linear15, linear25),
    n_units=c(1, 5, 15, 30), #2%, 10%, 30%, 60% 
    effect_direction=c("neg"),
    policy_speed=list("instant"),
    n_implementation_periods=list(0), 
    prior_control=c("mva3", "trend")
  )
)

#########################
### MULTISYNTH MODELS ###
#########################
multisynth_models <- list(
  list(
    name="multisynth",
    type="multisynth",
    model_call="multisynth",
    model_formula=opioid_rx ~ treatment_level,
    model_args=list(unit=as.name("state"), time=as.name("year"), fixedeff=TRUE, form=opioid_rx ~ treatment_level),
    se_adjust="none"
  )
)

msynth_config <- configure_simulation(
  x=x,
  models=multisynth_models,
  iters=500,
  
  # specify functions or S3 class of set of functions
  method_sample=noconf_sample,
  method_pre_model=noconf_premodel,
  method_model=noconf_model,
  method_post_model=noconf_postmodel,
  method_results=noconf_results,
  
  globals=NULL,#list(),
  
  params=list(
    unit_var="state",
    treat_var="state",
    time_var="year",
    effect_magnitude=list(linear0, linear5, linear15, linear25),
    n_units=c(1, 5, 15, 30), #2%, 10%, 30%, 60%
    effect_direction=c("neg"),
    policy_speed=list("instant"),
    n_implementation_periods=list(0), 
    prior_control=c("mva3", "trend")
  )
)

#####################
### DISPATCH JOBS ###
#####################
# setup cluster
cl <- parallel::makeCluster(parallel::detectCores()/4*3)
plan("cluster", workers = cl)

#### 2-Way Fixed Effect Runs ####
dispatch with the same seed (want the same sampled data each run)
proc.time1 = proc.time()
linear_fe_r <- dispatch_simulations(linear_fe_config,
                                    use_future=TRUE,
                                    seed=89721,
                                    verbose=2,
                                    future.globals=c("cluster_adjust_se"),
                                    future.packages=c("MASS", "dplyr", "optic", "augsynth", "DRDID"))
proc.time2 = proc.time()
print(proc.time2-proc.time1)
# clean up and write out results
linear_fe_results <- do.call(rbind, linear_fe_r)
rownames(linear_fe_results) <- NULL
write.csv(linear_fe_results, "/poppy/programs/josephp/output/noconf-opioidrx-linear-fe-weighted-05-25-21.csv", row.names = FALSE)

#### Autoregressive Runs ####
proc.time1 = proc.time()
linear_ar_r <- dispatch_simulations(linear_ar_config,
                                    use_future=TRUE,
                                    seed=89721,
                                    verbose=2,
                                    future.globals=c("cluster_adjust_se"),
                                    future.packages=c("MASS", "dplyr", "optic", "augsynth", "DRDID"))
# clean up and write out results
linear_ar_results <- do.call(rbind, linear_ar_r)
rownames(linear_ar_results) <- NULL
write.csv(linear_ar_results, "/poppy/programs/josephp/output/noconf-opioidrx-linear-ar-weighted-StateFE.csv", row.names = FALSE)
proc.time2 = proc.time()
print(proc.time2-proc.time1)

#### multisynth Runs ####
# proc.time1 = proc.time()
# multisynth_r <- dispatch_simulations(msynth_config,
#                                      use_future=TRUE,
#                                      seed=89721,
#                                      verbose=2,
#                                      future.globals=c("cluster_adjust_se"),
#                                      future.packages=c("MASS", "dplyr", "optic", "augsynth", "DRDID"))
# proc.time2 = proc.time()
# print(proc.time2-proc.time1)
# # clean up and write out results
# multisynth_results <- do.call(rbind, multisynth_r)
# rownames(multisynth_results) <- NULL
# write.csv(multisynth_results, "/poppy/programs/josephp/output/noconf-opioidrx-linear-multisynth-weighted-05-20-21.csv", row.names = FALSE)

#### End-of-file ####
