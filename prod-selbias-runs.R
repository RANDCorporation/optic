library(optic)
library(augsynth) 
library(dplyr)
library(future)
library(future.apply)
library(DRDID)

load("data/optic_sim_data_exp.Rdata")
names(x) <- tolower(names(x))

x <- x %>%
  select(state, year, fipscode, population, unemploymentrate, povertyrate,
         income, statefipyear, opioid_rx, md_access, insured, uninsur,medicare,
         medicaid, syn_opioid_death, other_opioid_death, her_death, yrslifelost,
         medicaid_ratio, all_opioid_death, overdose, alldeaths, cr.opioid.death,
         opioid_rx.lag1, cr.opioid.death.lag1, crude.rate, cr.adj, deaths,
         cr.adj.lag1) %>%
  mutate(crude.rate.new = crude.rate) %>% # new line of code
  arrange(state, year) %>%
  group_by(state) %>%
  mutate(lag1 = lag(crude.rate, n=1L),
         lag2 = lag(crude.rate, n=2L),
         lag3 = lag(crude.rate, n=3L)) %>%
  ungroup() %>%
  rowwise() %>%
  # code in moving average and trend versions of prior control
  mutate(prior_control_mva3_OLD = mean(c(lag1, lag2, lag3)),
         prior_control_trend_OLD = lag1 - lag3) %>%
  ungroup() %>%
  select(-lag1, -lag2, -lag3)

source("R/selection-bias-methods.R")
source("R/cluster-adjust-se.r")

bias_vals <- list(
  # MULTISYNTH METHOD
  multisynth = list(
    linear = list(
      mva3 = list(
        small=c(b0=-5, b1=0.01, b2=0.05, b3=0, b4=0, b5=0,
                a1=0.15, a2=0.05, a3=0, a4=0, a5=0),
        medium=c(b0=-5, b1=0.06, b2=0.05, b3=0, b4=0, b5=0,
                 a1=0.2, a2=0.05, a3=0, a4=0, a5=0),
        large=c(b0=-6, b1=0.0925, b2=0.1, b3=0, b4=0, b5=0,
                a1=0.5, a2=0.1, a3=0, a4=0, a5=0),
        none = c(b0=0, b1=0, b2=0, b3=0, b4=0, b5=0,
                 a1=0, a2=0, a3=0, a4=0, a5=0)),
      trend = list(
        small=c(b0=-5, b1=0.03, b2=0.1, b3=0, b4=0, b5=0,
                a1=0.05, a2=0.05, a3=0, a4=0, a5=0),
        medium=c(b0=-5, b1=0.05, b2=0.1, b3=0, b4=0, b5=0,
                 a1=0.2, a2=0.1, a3=0, a4=0, a5=0),
        large=c(b0=-5, b1=0.1, b2=0.1, b3=0, b4=0, b5=0,
                a1=0.5, a2=0.11, a3=0, a4=0, a5=0),
        none = c(b0=0, b1=0, b2=0, b3=0, b4=0, b5=0,
                 a1=0, a2=0, a3=0, a4=0, a5=0))),
    nonlinear = list(
      mva3 = list(
        small=c(b0=-5, b1=0.04, b2=0.04, b3=0.0003, b4=0.0001, b5=0.00003,
                a1=0.01, a2=0.01, a3=0.01, a4=0.01, a5=0.001),
        medium=c(b0=-5, b1=0.065, b2=0.065, b3=0.0007, b4=0.0004, b5=0.00007,
                 a1=0.01, a2=0.01, a3=0.01, a4=0.01, a5=0.001),
        large=c(b0=-5, b1=0.095, b2=0.095, b3=0.0009, b4=0.0006, b5=0.00009,
                a1=0.01, a2=0.01, a3=0.01, a4=0.01, a5=0.001)),
      trend = list(
        small=c(b0=-5, b1=0.05, b2=0.1, b3=0.001, b4=0.00026, b5=0.0001,
                a1=0.1, a2=0.05, a3=0.2, a4=0.15, a5=0.06),
        medium=c(b0=-5, b1=0.1, b2=0.12, b3=0.002, b4=0.00463, b5=0.0003,
                 a1=0.1, a2=0.05, a3=0.2, a4=0.15, a5=0.06),
        large=c(b0=-5, b1=0.1, b2=0.12, b3=0.002, b4=0.025, b5=0.0003,
                a1=0.1, a2=0.05, a3=0.2, a4=0.15, a5=0.06)))),
  # LINEAR REGRESSION
  fixedeff_linear = list(
    linear = list(
      mva3 = list(
        small=c(b0=-5, b1=0.01, b2=0.05, b3=0, b4=0, b5=0,
                a1=0.15, a2=0.05, a3=0, a4=0, a5=0),
        medium=c(b0=-5, b1=0.06, b2=0.05, b3=0, b4=0, b5=0,
                 a1=0.2, a2=0.05, a3=0, a4=0, a5=0),
        large=c(b0=-6, b1=0.0925, b2=0.1, b3=0, b4=0, b5=0,
                a1=0.5, a2=0.1, a3=0, a4=0, a5=0),
        none = c(b0=0, b1=0, b2=0, b3=0, b4=0, b5=0,
                 a1=0, a2=0, a3=0, a4=0, a5=0)),
      trend = list(
        small=c(b0=-5, b1=0.03, b2=0.1, b3=0, b4=0, b5=0,
                a1=0.05, a2=0.05, a3=0, a4=0, a5=0),
        medium=c(b0=-5, b1=0.05, b2=0.1, b3=0, b4=0, b5=0,
                 a1=0.2, a2=0.1, a3=0, a4=0, a5=0),
        large=c(b0=-5, b1=0.1, b2=0.1, b3=0, b4=0, b5=0,
                a1=0.5, a2=0.11, a3=0, a4=0, a5=0),
        none = c(b0=0, b1=0, b2=0, b3=0, b4=0, b5=0,
                 a1=0, a2=0, a3=0, a4=0, a5=0))),
    nonlinear = list(
      mva3 = list(
        small=c(b0=-5, b1=0.04, b2=0.04, b3=0.0003, b4=0.0001, b5=0.00003,
                a1=0.01, a2=0.01, a3=0.01, a4=0.01, a5=0.001),
        medium=c(b0=-5, b1=0.065, b2=0.065, b3=0.0007, b4=0.0004, b5=0.00007,
                 a1=0.01, a2=0.01, a3=0.01, a4=0.01, a5=0.001),
        large=c(b0=-5, b1=0.095, b2=0.095, b3=0.0009, b4=0.0006, b5=0.00009,
                a1=0.01, a2=0.01, a3=0.01, a4=0.01, a5=0.001)),
      trend = list(
        small=c(b0=-5, b1=0.05, b2=0.1, b3=0.001, b4=0.00026, b5=0.0001,
                a1=0.1, a2=0.05, a3=0.2, a4=0.15, a5=0.06),
        medium=c(b0=-5, b1=0.1, b2=0.12, b3=0.002, b4=0.00463, b5=0.0003,
                 a1=0.1, a2=0.05, a3=0.2, a4=0.15, a5=0.06),
        large=c(b0=-5, b1=0.1, b2=0.12, b3=0.002, b4=0.025, b5=0.0003,
                a1=0.1, a2=0.05, a3=0.2, a4=0.15, a5=0.06)))),
  autoreg_linear = list(
    linear = list(
      mva3 = list(
        small=c(b0=-5, b1=0.01, b2=0.05, b3=0, b4=0, b5=0,
                a1=0.15, a2=0.05, a3=0, a4=0, a5=0),
        medium=c(b0=-5, b1=0.06, b2=0.05, b3=0, b4=0, b5=0,
                 a1=0.2, a2=0.05, a3=0, a4=0, a5=0),
        large=c(b0=-6, b1=0.0925, b2=0.1, b3=0, b4=0, b5=0,
                a1=0.5, a2=0.1, a3=0, a4=0, a5=0),
        none = c(b0=0, b1=0, b2=0, b3=0, b4=0, b5=0,
                 a1=0, a2=0, a3=0, a4=0, a5=0)),
      trend = list(
        small=c(b0=-5, b1=0.03, b2=0.1, b3=0, b4=0, b5=0,
                a1=0.05, a2=0.05, a3=0, a4=0, a5=0),
        medium=c(b0=-5, b1=0.05, b2=0.1, b3=0, b4=0, b5=0,
                 a1=0.2, a2=0.1, a3=0, a4=0, a5=0),
        large=c(b0=-5, b1=0.1, b2=0.1, b3=0, b4=0, b5=0,
                a1=0.5, a2=0.11, a3=0, a4=0, a5=0),
        none = c(b0=0, b1=0, b2=0, b3=0, b4=0, b5=0,
                 a1=0, a2=0, a3=0, a4=0, a5=0))
    ),
    nonlinear = list(
      mva3 = list(
        small=c(b0=-5, b1=0.04, b2=0.04, b3=0.0003, b4=0.0001, b5=0.00003,
                a1=0.01, a2=0.01, a3=0.01, a4=0.01, a5=0.001),
        medium=c(b0=-5, b1=0.065, b2=0.065, b3=0.0007, b4=0.0004, b5=0.00007,
                 a1=0.01, a2=0.01, a3=0.01, a4=0.01, a5=0.001),
        large=c(b0=-5, b1=0.095, b2=0.095, b3=0.0009, b4=0.0006, b5=0.00009,
                a1=0.01, a2=0.01, a3=0.01, a4=0.01, a5=0.001)),
      trend = list(
        small=c(b0=-5, b1=0.05, b2=0.1, b3=0.001, b4=0.00026, b5=0.0001,
                a1=0.1, a2=0.05, a3=0.2, a4=0.15, a5=0.06),
        medium=c(b0=-5, b1=0.1, b2=0.12, b3=0.002, b4=0.00463, b5=0.0003,
                 a1=0.1, a2=0.05, a3=0.2, a4=0.15, a5=0.06),
        large=c(b0=-5, b1=0.1, b2=0.12, b3=0.002, b4=0.025, b5=0.0003,
                a1=0.1, a2=0.05, a3=0.2, a4=0.15, a5=0.06)))),
  # DOUBLY ROBUST DIFFERENCE-IN-DIFFERENCE
  # TODO: currently the package does not allow for longitudinal data, only accepts
  #       one pre and post period
  # NEGATIVE BINOMIAL
  # TODO: currently not implemented for selection bias runs since we need to think
  #       through how to make comparisons to linear models
  fixedeff_negbin = list(),
  autoreg_negbin = list()
)

linear_models <- list(
  list(
    name="fixedeff_linear",
    type="reg",
    model_call="lm",
    model_formula=crude.rate ~ treatment_level + unemploymentrate + as.factor(year) + as.factor(state),
    model_args=NULL,#list(weights=as.name("population"))
    se_adjust=c("none", "huber", "cluster", "arellano")
  ),
  list(
    name="autoreg_linear",
    model_call="lm",
    type="autoreg",
    model_formula=crude.rate ~ treatment_change + unemploymentrate + as.factor(year),
    model_args=list(weights=as.name("population")),#NULL,
    se_adjust=c("none", "huber", "cluster", "arellano")
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
  method_sample=selbias_sample,
  method_pre_model=selbias_premodel,
  method_model=selbias_model,
  method_post_model=selbias_postmodel,
  method_results=selbias_results,
  
  globals=list(
    bias_vals=bias_vals[["fixedeff_linear"]]
  ),
  
  # parameters that will be expanded and added
  params=list(
    unit_var="state",
    time_var="year",
    policy_speed=list("instant"),
    prior_control=c("mva3", "trend"),
    bias_type=c("nonlinear"), #, "linear"
    bias_size=c("small", "medium", "large"),
    n_implementation_periods=list(0)
  )
)

linear_ar_config <- configure_simulation(
  # data and models required
  x=x,
  models=list(linear_models[[2]]),
  # iterations
  iters=5000,
  
  # specify functions or S3 class of set of functions
  method_sample=selbias_sample,
  method_pre_model=selbias_premodel,
  method_model=selbias_model,
  method_post_model=selbias_postmodel,
  method_results=selbias_results,
  
  globals=list(
    bias_vals=bias_vals[["autoreg_linear"]]
  ),
  
  # parameters that will be expanded and added
  params=list(
    unit_var="state",
    time_var="year",
    policy_speed=list("instant"),
    prior_control=c("mva3", "trend"),
    bias_type=c("linear"), #, "nonlinear"
    bias_size= c("small", "medium", "large"), # c("small", "medium", "large"), #as.character(1:nrow(possible_grid))
    n_implementation_periods=list(0)
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
    model_formula=crude.rate ~ treatment_level,
    model_args=list(unit=as.name("state"), time=as.name("year"), fixedeff=TRUE, form=crude.rate ~ treatment_level),
    se_adjust="none"
  )
)

msynth_config <- configure_simulation(
  x=x,
  models=multisynth_models,
  iters=5000,
  method_sample=selbias_sample,
  method_pre_model=selbias_premodel,
  method_model=selbias_model,
  method_post_model=selbias_postmodel,
  method_results=selbias_results,
  
  globals=list(
    bias_vals=bias_vals[["multisynth"]]
  ),
  
  params=list(
    unit_var="state",
    time_var="year",
    policy_speed=list("instant"),
    prior_control=c("mva3", "trend"),
    bias_type=c("nonlinear"),#"linear"
    bias_size=c("small", "medium", "large"), #, "none"
    n_implementation_periods=list(0)
  )
)

#####################
### DISPATCH JOBS ###
#####################
# setup cluster
cl <- parallel::makeCluster((parallel::detectCores()-8))
plan("cluster", workers = cl)

#### Tuning (optional) ####
# proc.time1 = proc.time()
# linear_fe_r <- dispatch_tuning(linear_fe_config, 
#                                use_future=T,
#                                seed=89721,
#                                verbose=2,
#                                future.globals=c("cluster_adjust_se"),
#                                future.packages=c("dplyr", "MASS", "optic", "augsynth", "DRDID"))
# proc.time2 = proc.time()
# print(proc.time2-proc.time1)
# # clean up and write out results
# linear_fe_results <- do.call(rbind, linear_fe_r)
# 
# rownames(linear_fe_results) <- NULL
# write.csv(linear_fe_results, "/vincent/b/josephp/OPTIC/output/sel-bias-nonlinear-fe-unweighted-Round10.csv", row.names = FALSE)
# 
# my_results = read.csv("/vincent/b/josephp/OPTIC/output/sel-bias-nonlinear-fe-unweighted.csv") %>%
#   group_by(prior_control, bias_size) %>%
#   summarize(n=n(),
#             mean = mean(mean_es_outcome))

#### End of Tuning ####

#### 2-Way Fixed Effect Runs ####
# dispatch with the same seed (want the same sampled data each run)
linear_fe_r <- dispatch_simulations(linear_fe_config,
                                    use_future=T,
                                    seed=89721,
                                    verbose=2,
                                    future.globals=c("cluster_adjust_se"),
                                    future.packages=c("dplyr", "MASS", "optic", "augsynth", "DRDID"))
# clean up and write out results
linear_fe_results <- do.call(rbind, linear_fe_r)
rownames(linear_fe_results) <- NULL
write.csv(linear_fe_results, "/vincent/b/josephp/OPTIC/output/sel-bias-linear-fe-unweighted-nonlin.csv", row.names = FALSE)

#### Autoregressive Runs ####
proc.time1 = proc.time()
linear_ar_r <- dispatch_simulations(linear_ar_config,
                                    use_future=T,
                                    seed=89721,
                                    verbose=2,
                                    future.globals=c("cluster_adjust_se"),
                                    future.packages=c("dplyr", "MASS", "optic", "augsynth", "DRDID"))
# clean up and write out results
linear_ar_results <- do.call(rbind, linear_ar_r)
rownames(linear_ar_results) <- NULL
write.csv(linear_ar_results, "/poppy/programs/josephp/output/sel-bias-linear-ar-weighted-lin-03-12-21.csv", row.names = FALSE)
proc.time2 = proc.time()
print(proc.time2-proc.time1)

#### multisynth Runs ####
multisynth_r <- dispatch_simulations(msynth_config,
                                     use_future=T,
                                     seed=89721,
                                     verbose=2,
                                     future.globals=c("cluster_adjust_se"),
                                     future.packages=c("dplyr", "MASS", "optic", "augsynth", "DRDID"))
# clean up and write out results
multisynth_results <- do.call(rbind, multisynth_r)
rownames(multisynth_results) <- NULL
write.csv(multisynth_results, "/vincent/b/josephp/OPTIC/output/sel-bias-multisynth-nonlin.csv", row.names = FALSE)

#### End-of-file ####

