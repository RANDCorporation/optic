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
    nonlinear = list(
      trend = list(
        large=c(b0=-5, b1=0.01, b2=0.018, b3=0.006, b4=0.018, b5=0.013,
                a1=0.02, a2=0.02, a3=0.0095, a4=0.0095, a5=0.01475),
        medium=c(b0=-5, b1=0.01, b2=0.009, b3=0.003, b4=0.009, b5=0.00775,
                 a1=0.02, a2=0.02, a3=0.005, a4=0.005, a5=0.0125),
        small=c(b0=-5, b1=0.01, b2=0.006, b3=0.002, b4=0.006, b5=0.006,
                a1=0.02, a2=0.02, a3=0.0032, a4=0.0032, a5=0.0116)))),
  # LINEAR REGRESSION
  fixedeff_linear = list(
    nonlinear = list(
      trend = list(
        large=c(b0=-5, b1=0.01, b2=0.018, b3=0.006, b4=0.018, b5=0.013,
                a1=0.02, a2=0.02, a3=0.0095, a4=0.0095, a5=0.01475),
        medium=c(b0=-5, b1=0.01, b2=0.009, b3=0.003, b4=0.009, b5=0.00775,
                 a1=0.02, a2=0.02, a3=0.005, a4=0.005, a5=0.0125),
        small=c(b0=-5, b1=0.01, b2=0.006, b3=0.002, b4=0.006, b5=0.006,
                a1=0.02, a2=0.02, a3=0.0032, a4=0.0032, a5=0.0116)))),
  # AUTOREGRESSIVE
  autoreg_linear = list(
    nonlinear = list(
      trend = list(
        large=c(b0=-5, b1=0.01, b2=0.018, b3=0.006, b4=0.018, b5=0.013,
                a1=0.02, a2=0.02, a3=0.0095, a4=0.0095, a5=0.01475),
        medium=c(b0=-5, b1=0.01, b2=0.009, b3=0.003, b4=0.009, b5=0.00775,
                 a1=0.02, a2=0.02, a3=0.005, a4=0.005, a5=0.0125),
        small=c(b0=-5, b1=0.01, b2=0.006, b3=0.002, b4=0.006, b5=0.006,
                a1=0.02, a2=0.02, a3=0.0032, a4=0.0032, a5=0.0116)))))

linear_models <- list(
  list(
    name="fixedeff_linear",
    type="reg",
    model_call="lm",
    model_formula=crude.rate ~ treatment_level + unemploymentrate + as.factor(year) + as.factor(state),
    model_args=list(weights=as.name("population")),#NULL
    se_adjust=c("cluster")
  ),
  list(
    name="autoreg_linear",
    model_call="lm",
    type="autoreg",
    model_formula=crude.rate ~ treatment_change + unemploymentrate + as.factor(year),
    model_args=list(weights=as.name("population")),#NULL
    se_adjust=c("none")
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
    prior_control=c("trend"),#"mva3", 
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
    prior_control=c("trend"),#"mva3", 
    bias_type=c("nonlinear"), #, "linear"
    bias_size= c("small", "medium", "large"),
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
    prior_control=c("trend"),# "mva3"
    bias_type=c("nonlinear"),# "linear"
    bias_size=c("small", "medium", "large"), #, "none"
    n_implementation_periods=list(0)
  )
)

#####################
### DISPATCH JOBS ###
#####################
# setup cluster
cl <- parallel::makeCluster((parallel::detectCores()/2-1))
plan("cluster", workers = cl)

#### Linear FE dispatch simulations ####
proc.time1 = proc.time()
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
write.csv(linear_fe_results, "/poppy/programs/josephp/output/sel-bias-trend-fe-weighted-nonlin-final.csv", row.names = FALSE)
proc.time2 = proc.time()
print(proc.time2-proc.time1)

#### AR dispatch simulations ####
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
write.csv(linear_ar_results, "/poppy/programs/josephp/output/sel-bias-trend-ar-weighted-nonlin-final.csv", row.names = FALSE)
proc.time2 = proc.time()
print(proc.time2-proc.time1)

#### Multisynth dispatch simulations ####
# proc.time1 = proc.time()
# multisynth_r <- dispatch_simulations(msynth_config,
#                                      use_future=T,
#                                      seed=89721,
#                                      verbose=2,
#                                      future.globals=c("cluster_adjust_se"),
#                                      future.packages=c("dplyr", "MASS", "optic", "augsynth", "DRDID"))
# # clean up and write out results
# multisynth_results <- do.call(rbind, multisynth_r)
# rownames(multisynth_results) <- NULL
# write.csv(multisynth_results, "/poppy/programs/josephp/output/sel-bias-trend-multisynth-nonlin-final.csv", row.names = FALSE)
# proc.time2 = proc.time()
# print(proc.time2-proc.time1)

#### End-of-file ####

