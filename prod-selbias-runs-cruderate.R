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
  mutate(crude.rate.old = crude.rate) %>% # new line of code
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
  select(-lag1, -lag2, -lag3) %>%
  mutate(state = factor(as.character(state)))

source("R/selection-bias-methods.R")
source("R/cluster-adjust-se.r")

linear0 <- 0 / ((sum(x$population) / length(unique(x$year))) / 100000)
linear5 <- 690 / ((sum(x$population) / length(unique(x$year))) / 100000)
linear10 <- 1380 / ((sum(x$population) / length(unique(x$year))) / 100000)
linear15 <- 2070 / ((sum(x$population) / length(unique(x$year))) / 100000)
linear20 <- 2760 / ((sum(x$population) / length(unique(x$year))) / 100000)

scenario1 <- c(linear10) #10/10

bias_vals <- list(
  # MULTISYNTH METHOD
  multisynth = list(
    linear = list(
      mva3 = list(
        small=c(b0=-5, b1=0.05, b2=0.05, b3=0, b4=0, b5=0,
                a1=0.2, a2=0.05, a3=0, a4=0, a5=0),
        medium=c(b0=-5, b1=0.095, b2=0.05, b3=0, b4=0, b5=0,
                 a1=0.2, a2=0.05, a3=0, a4=0, a5=0),
        large=c(b0=-5, b1=0.145, b2=0.1, b3=0, b4=0, b5=0,
                a1=0.2, a2=0.05, a3=0, a4=0, a5=0),
        none = c(b0=0, b1=0, b2=0, b3=0, b4=0, b5=0,
                 a1=0, a2=0, a3=0, a4=0, a5=0)),
      trend = list(
        small=c(b0=-5, b1=0.12, b2=0.1, b3=0, b4=0, b5=0,
                a1=0.5, a2=0.11, a3=0, a4=0, a5=0),
        medium=c(b0=-5, b1=0.202, b2=0.1, b3=0, b4=0, b5=0,
                 a1=0.5, a2=0.11, a3=0, a4=0, a5=0),
        large=c(b0=-5, b1=0.3, b2=0.1, b3=0, b4=0, b5=0,
                a1=0.5, a2=0.11, a3=0, a4=0, a5=0),
        none = c(b0=0, b1=0, b2=0, b3=0, b4=0, b5=0,
                 a1=0, a2=0, a3=0, a4=0, a5=0))),
    nonlinear = list(
      mva3 = list(
        small=c(b0=-5, b1=0.05, b2=0.05, b3=0.0001, b4=0.0001, b5=0.00003,
                a1=0.01, a2=0.01, a3=0.01, a4=0.01, a5=0.001),
        medium=c(b0=-5, b1=0.05, b2=0.05, b3=0.00155, b4=0.00155, b5=0.000065,
                 a1=0.01, a2=0.01, a3=0.01, a4=0.01, a5=0.001),
        large=c(b0=-5, b1=0.05, b2=0.05, b3=0.003, b4=0.003, b5=0.0001,
                a1=0.01, a2=0.01, a3=0.01, a4=0.01, a5=0.001)),
      trend = list(
        small=c(b0=-5, b1=0.05, b2=0.03, b3=0.005, b4=0.001, b5=0.001,
                a1=0.1, a2=0.05, a3=0.1, a4=0.01, a5=0.01),
        medium=c(b0=-5, b1=0.05, b2=0.03, b3=0.0078, b4=0.0028, b5=0.003,
                 a1=0.1, a2=0.05, a3=0.1, a4=0.01, a5=0.01),
        large=c(b0=-5, b1=0.05, b2=0.03, b3=0.012, b4=0.006, b5=0.004,
                a1=0.1, a2=0.05, a3=0.1, a4=0.01, a5=0.01)))),
  # LINEAR REGRESSION
  fixedeff_linear = list(
    linear = list(
      mva3 = list(
        small=c(b0=-5, b1=0.05, b2=0.05, b3=0, b4=0, b5=0,
                a1=0.2, a2=0.05, a3=0, a4=0, a5=0),
        medium=c(b0=-5, b1=0.095, b2=0.05, b3=0, b4=0, b5=0,
                 a1=0.2, a2=0.05, a3=0, a4=0, a5=0),
        large=c(b0=-5, b1=0.145, b2=0.1, b3=0, b4=0, b5=0,
                a1=0.2, a2=0.05, a3=0, a4=0, a5=0),
        none = c(b0=0, b1=0, b2=0, b3=0, b4=0, b5=0,
                 a1=0, a2=0, a3=0, a4=0, a5=0)),
      trend = list(
        small=c(b0=-5, b1=0.12, b2=0.1, b3=0, b4=0, b5=0,
                a1=0.5, a2=0.11, a3=0, a4=0, a5=0),
        medium=c(b0=-5, b1=0.202, b2=0.1, b3=0, b4=0, b5=0,
                 a1=0.5, a2=0.11, a3=0, a4=0, a5=0),
        large=c(b0=-5, b1=0.3, b2=0.1, b3=0, b4=0, b5=0,
                a1=0.5, a2=0.11, a3=0, a4=0, a5=0),
        none = c(b0=0, b1=0, b2=0, b3=0, b4=0, b5=0,
                 a1=0, a2=0, a3=0, a4=0, a5=0))),
    nonlinear = list(
      mva3 = list(
        small=c(b0=-5, b1=0.05, b2=0.05, b3=0.0001, b4=0.0001, b5=0.00003,
                a1=0.01, a2=0.01, a3=0.01, a4=0.01, a5=0.001),
        medium=c(b0=-5, b1=0.05, b2=0.05, b3=0.00155, b4=0.00155, b5=0.000065,
                 a1=0.01, a2=0.01, a3=0.01, a4=0.01, a5=0.001),
        large=c(b0=-5, b1=0.05, b2=0.05, b3=0.003, b4=0.003, b5=0.0001,
                a1=0.01, a2=0.01, a3=0.01, a4=0.01, a5=0.001)),
      trend = list(
        small=c(b0=-5, b1=0.05, b2=0.03, b3=0.005, b4=0.001, b5=0.001,
                a1=0.1, a2=0.05, a3=0.1, a4=0.01, a5=0.01),
        medium=c(b0=-5, b1=0.05, b2=0.03, b3=0.0078, b4=0.0028, b5=0.003,
                 a1=0.1, a2=0.05, a3=0.1, a4=0.01, a5=0.01),
        large=c(b0=-5, b1=0.05, b2=0.03, b3=0.012, b4=0.006, b5=0.004,
                a1=0.1, a2=0.05, a3=0.1, a4=0.01, a5=0.01)))),
  # AUTOREG
  autoreg_linear = list(
    linear = list(
      mva3 = list(
        small=c(b0=-5, b1=0.05, b2=0.05, b3=0, b4=0, b5=0,
                a1=0.2, a2=0.05, a3=0, a4=0, a5=0),
        medium=c(b0=-5, b1=0.095, b2=0.05, b3=0, b4=0, b5=0,
                 a1=0.2, a2=0.05, a3=0, a4=0, a5=0),
        large=c(b0=-5, b1=0.145, b2=0.1, b3=0, b4=0, b5=0,
                a1=0.2, a2=0.05, a3=0, a4=0, a5=0),
        none = c(b0=0, b1=0, b2=0, b3=0, b4=0, b5=0,
                 a1=0, a2=0, a3=0, a4=0, a5=0)),
      trend = list(
        small=c(b0=-5, b1=0.12, b2=0.1, b3=0, b4=0, b5=0,
                a1=0.5, a2=0.11, a3=0, a4=0, a5=0),
        medium=c(b0=-5, b1=0.202, b2=0.1, b3=0, b4=0, b5=0,
                 a1=0.5, a2=0.11, a3=0, a4=0, a5=0),
        large=c(b0=-5, b1=0.3, b2=0.1, b3=0, b4=0, b5=0,
                a1=0.5, a2=0.11, a3=0, a4=0, a5=0),
        none = c(b0=0, b1=0, b2=0, b3=0, b4=0, b5=0,
                 a1=0, a2=0, a3=0, a4=0, a5=0))),
    nonlinear = list(
      mva3 = list(
        small=c(b0=-5, b1=0.05, b2=0.05, b3=0.0001, b4=0.0001, b5=0.00003,
                a1=0.01, a2=0.01, a3=0.01, a4=0.01, a5=0.001),
        medium=c(b0=-5, b1=0.05, b2=0.05, b3=0.00155, b4=0.00155, b5=0.000065,
                 a1=0.01, a2=0.01, a3=0.01, a4=0.01, a5=0.001),
        large=c(b0=-5, b1=0.05, b2=0.05, b3=0.003, b4=0.003, b5=0.0001,
                a1=0.01, a2=0.01, a3=0.01, a4=0.01, a5=0.001)),
      trend = list(
        small=c(b0=-5, b1=0.05, b2=0.03, b3=0.005, b4=0.001, b5=0.001,
                a1=0.1, a2=0.05, a3=0.1, a4=0.01, a5=0.01),
        medium=c(b0=-5, b1=0.05, b2=0.03, b3=0.0078, b4=0.0028, b5=0.003,
                 a1=0.1, a2=0.05, a3=0.1, a4=0.01, a5=0.01),
        large=c(b0=-5, b1=0.05, b2=0.03, b3=0.012, b4=0.006, b5=0.004,
                a1=0.1, a2=0.05, a3=0.1, a4=0.01, a5=0.01)))),
  # DOUBLY ROBUST DIFFERENCE-IN-DIFFERENCE
  # TODO: currently the package does not allow for longitudinal data, only accepts
  #       one pre and post period
  drdid = list(
    linear = list(
      mva3 = list(
        small=c(b0=-5, b1=0.05, b2=0.05, b3=0, b4=0, b5=0,
                a1=0.2, a2=0.05, a3=0, a4=0, a5=0),
        medium=c(b0=-5, b1=0.095, b2=0.05, b3=0, b4=0, b5=0,
                 a1=0.2, a2=0.05, a3=0, a4=0, a5=0),
        large=c(b0=-5, b1=0.145, b2=0.1, b3=0, b4=0, b5=0,
                a1=0.2, a2=0.05, a3=0, a4=0, a5=0),
        none = c(b0=0, b1=0, b2=0, b3=0, b4=0, b5=0,
                 a1=0, a2=0, a3=0, a4=0, a5=0)),
      trend = list(
        small=c(b0=-5, b1=0.12, b2=0.1, b3=0, b4=0, b5=0,
                a1=0.5, a2=0.11, a3=0, a4=0, a5=0),
        medium=c(b0=-5, b1=0.202, b2=0.1, b3=0, b4=0, b5=0,
                 a1=0.5, a2=0.11, a3=0, a4=0, a5=0),
        large=c(b0=-5, b1=0.3, b2=0.1, b3=0, b4=0, b5=0,
                a1=0.5, a2=0.11, a3=0, a4=0, a5=0),
        none = c(b0=0, b1=0, b2=0, b3=0, b4=0, b5=0,
                 a1=0, a2=0, a3=0, a4=0, a5=0))),
    nonlinear = list(
      mva3 = list(
        small=c(b0=-5, b1=0.05, b2=0.05, b3=0.0001, b4=0.0001, b5=0.00003,
                a1=0.01, a2=0.01, a3=0.01, a4=0.01, a5=0.001),
        medium=c(b0=-5, b1=0.05, b2=0.05, b3=0.00155, b4=0.00155, b5=0.000065,
                 a1=0.01, a2=0.01, a3=0.01, a4=0.01, a5=0.001),
        large=c(b0=-5, b1=0.05, b2=0.05, b3=0.003, b4=0.003, b5=0.0001,
                a1=0.01, a2=0.01, a3=0.01, a4=0.01, a5=0.001)),
      trend = list(
        small=c(b0=-5, b1=0.05, b2=0.03, b3=0.005, b4=0.001, b5=0.001,
                a1=0.1, a2=0.05, a3=0.1, a4=0.01, a5=0.01),
        medium=c(b0=-5, b1=0.05, b2=0.03, b3=0.0078, b4=0.0028, b5=0.003,
                 a1=0.1, a2=0.05, a3=0.1, a4=0.01, a5=0.01),
        large=c(b0=-5, b1=0.05, b2=0.03, b3=0.012, b4=0.006, b5=0.004,
                a1=0.1, a2=0.05, a3=0.1, a4=0.01, a5=0.01)))),
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
    model_args=NULL,#list(weights=as.name("population")),
    se_adjust=c("none", "huber", "cluster", "arellano")
  ),
  list(
    name="autoreg_linear",
    model_call="lm",
    type="autoreg",
    model_formula=crude.rate ~ treatment_change + unemploymentrate + as.factor(year),
    model_args=NULL,#list(weights=as.name("population")),
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
    effect_magnitude=list(scenario1),
    effect_direction=c("neg"), # "null"
    prior_control=c("mva3", "trend"),
    bias_type=c("nonlinear"), # "nonlinear"
    bias_size=c("small", "medium", "large"), # "none", 
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
    bias_type=c("nonlinear"), #, "nonlinear"
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
    bias_type=c("nonlinear"),#"nonlinear"
    bias_size=c("small", "medium", "large"), #"none"
    n_implementation_periods=list(0)
  )
)

#########################################
### DOUBLY ROBUST DIFF-in-DIFF MODELS ###
#########################################
# drdid_models <- list(
#   list(
#     name="drdid",
#     type="drdid",
#     model_call="drdid",
#     model_formula= ~ unemploymentrate,
#     model_args=list(yname=as.name("crude.rate"), tname=as.name("year"), idname=as.name("state"), 
#                     dname="treatment_level", xformla = "~ unemploymentrate", panel=TRUE,
#                     weightsname=NULL),
#     se_adjust=c("none", "huber", "cluster", "arellano")
#   )
# )
# 
# # for the linear config, need separate ones for now
# drdid_config <- configure_simulation(
#   # data and models required
#   x=x,
#   models=list(drdid_models[[1]]),
#   # iterations
#   iters=2,
#   
#   # specify functions or S3 class of set of functions
#   method_sample=selbias_sample,
#   method_pre_model=selbias_premodel,
#   method_model=selbias_model,
#   method_post_model=selbias_postmodel,
#   method_results=selbias_results,
#   
#   globals=list(
#     bias_vals=bias_vals[["drdid"]]
#   ),
#   
#   # parameters that will be expanded and added
#   params=list(
#     unit_var="state",
#     time_var="year",
#     policy_speed=list("instant"),
#     prior_control=c("mva3"),#, "trend"
#     bias_type=c("linear"), #, "linear"
#     bias_size=c("none"),#"small", "medium", "large"
#     n_implementation_periods=list(0)
#   )
# )

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
# rownames(linear_fe_results) <- NULL
# write.csv(linear_fe_results, "/poppy/programs/josephp/output/sel-bias-cruderate-nonlinear-fe-weighted-Round6.csv", row.names = FALSE)
# 
# readr::read_csv("/poppy/programs/josephp/output/sel-bias-cruderate-nonlinear-fe-weighted-Round6.csv") %>%
#   dplyr::group_by(prior_control, bias_size) %>%
#   dplyr::summarize(n=n(),
#                    mean = mean(mean_es_outcome))

#### End of Tuning ####

#### 2-Way Fixed Effect Runs ####
# dispatch with the same seed (want the same sampled data each run)
proc.time1 = proc.time()
linear_fe_r <- dispatch_simulations(linear_fe_config,
                                    use_future=T,
                                    seed=89721,
                                    verbose=2,
                                    future.globals=c("cluster_adjust_se"),
                                    future.packages=c("dplyr", "MASS", "optic", "augsynth", "DRDID"))
proc.time2 = proc.time()
print(proc.time2-proc.time1)
# clean up and write out results
linear_fe_results <- do.call(rbind, linear_fe_r)
rownames(linear_fe_results) <- NULL
write.csv(linear_fe_results, "/poppy/programs/josephp/output/sel-bias-cruderate-nonlinear-fe-unweighted-lin-04-29-21.csv", row.names = FALSE)

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
write.csv(linear_ar_results, "/poppy/programs/josephp/output/sel-bias-cruderate-nonlinear-ar-unweighted-lin-04-29-21.csv", row.names = FALSE)
proc.time2 = proc.time()
print(proc.time2-proc.time1)

#### multisynth Runs ####
# proc.time1 = proc.time()
# multisynth_r <- dispatch_simulations(msynth_config,
#                                      use_future=T,
#                                      seed=89721,
#                                      verbose=2,
#                                      future.globals=c("cluster_adjust_se"),
#                                      future.packages=c("dplyr", "MASS", "optic", "augsynth", "DRDID"))
# proc.time2 = proc.time()
# print(proc.time2-proc.time1)
# # clean up and write out results
# multisynth_results <- do.call(rbind, multisynth_r)
# rownames(multisynth_results) <- NULL
# write.csv(multisynth_results, "/poppy/programs/josephp/output/sel-bias-cruderate-multisynth-unweighted-nonlin-04-29-21.csv", row.names = FALSE)

#### End-of-file ####
# TEST = drdid(yname="crude.rate", tname="year", idname="state", dname="treatment",
#              xformla = ~ unemploymentrate, panel=TRUE, data = x)


