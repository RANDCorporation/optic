library(dplyr)
library(optic)
library(augsynth) 
library(future)
library(future.apply)
library(DRDID)
library(readxl)
# setwd("/poppy/programs/josephp/optic-core")

county_dta = read_excel("data/CDC_OpioidPrescribing_IMS_AnalyticExtract_122818_rs.xlsx",
                        sheet = "county") %>%
  group_by(county_FIPS) %>%
  mutate(check_NA = any(is.na(opioid_Rxrateper100))) %>%
  filter(check_NA==FALSE) %>%
  select(-check_NA)
names(county_dta) <- tolower(names(county_dta))

county_dta <- county_dta %>%
  mutate(opioid_rxrateper100.new = opioid_rxrateper100) %>% # new line of code
  arrange(county_fips, year) %>%
  group_by(county_fips) %>%
  mutate(lag1 = lag(opioid_rxrateper100, n=1L),
         lag2 = lag(opioid_rxrateper100, n=2L),
         lag3 = lag(opioid_rxrateper100, n=3L)) %>%
  ungroup() %>%
  rowwise() %>%
  # code in moving average and trend versions of prior control
  mutate(prior_control_mva3_OLD = mean(c(lag1, lag2, lag3)),
         prior_control_trend_OLD = lag1 - lag3) %>%
  ungroup() %>%
  select(-lag1, -lag2, -lag3)

# add county-level unemployment rate
county_dta_unemploy = haven::read_dta("data/countyCovariates.dta") %>%
  select(fips, year, unemploymentRate, pop_tot) %>%
  filter(year %in% 2006:2017) %>%
  mutate(fips = as.numeric(fips))

county_dta = county_dta %>%
  left_join(., county_dta_unemploy, by = c("year"="year", "county_fips"="fips")) %>%
  # remove counties that are missing unemployment rate for one of years
  filter(!(county_fips %in% c(2195, 22051, 22071, 22087, 22089, 22095, 22103))) %>%
  rename(unemploymentrate = unemploymentRate,
         population = pop_tot)

source("R/selection-bias-methods.R")
source("R/cluster-adjust-se.r")

bias_vals <- list(
  # MULTISYNTH METHOD
  multisynth = list(
    linear = list(
      mva3 = list(
        small=c(b0=-5, b1=0.0027, b2=0.02, b3=0, b4=0, b5=0,
                a1=0.05, a2=0.01, a3=0, a4=0, a5=0),
        medium=c(b0=-5, b1=0.0054, b2=0.02, b3=0, b4=0, b5=0,
                 a1=0.05, a2=0.01, a3=0, a4=0, a5=0),
        large=c(b0=-5, b1=0.0073, b2=0.02, b3=0, b4=0, b5=0,
                a1=0.05, a2=0.01, a3=0, a4=0, a5=0),
        none = c(b0=0, b1=0, b2=0, b3=0, b4=0, b5=0,
                 a1=0, a2=0, a3=0, a4=0, a5=0)),
      trend = list(
        small=c(b0=-5, b1=0.0155, b2=0.01, b3=0, b4=0, b5=0,
                a1=0.2, a2=0.02, a3=0, a4=0, a5=0),
        medium=c(b0=-5, b1=0.0235, b2=0.01, b3=0, b4=0, b5=0,
                 a1=0.2, a2=0.02, a3=0, a4=0, a5=0),
        large=c(b0=-5, b1=0.0325, b2=0.01, b3=0, b4=0, b5=0,
                a1=0.2, a2=0.02, a3=0, a4=0, a5=0),
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
        small=c(b0=-5, b1=0.0027, b2=0.02, b3=0, b4=0, b5=0,
                a1=0.05, a2=0.01, a3=0, a4=0, a5=0),
        medium=c(b0=-5, b1=0.0054, b2=0.02, b3=0, b4=0, b5=0,
                 a1=0.05, a2=0.01, a3=0, a4=0, a5=0),
        large=c(b0=-5, b1=0.0073, b2=0.02, b3=0, b4=0, b5=0,
                a1=0.05, a2=0.01, a3=0, a4=0, a5=0),
        none = c(b0=0, b1=0, b2=0, b3=0, b4=0, b5=0,
                 a1=0, a2=0, a3=0, a4=0, a5=0)),
      trend = list(
        small=c(b0=-5, b1=0.0155, b2=0.01, b3=0, b4=0, b5=0,
                a1=0.2, a2=0.02, a3=0, a4=0, a5=0),
        medium=c(b0=-5, b1=0.0235, b2=0.01, b3=0, b4=0, b5=0,
                 a1=0.2, a2=0.02, a3=0, a4=0, a5=0),
        large=c(b0=-5, b1=0.0325, b2=0.01, b3=0, b4=0, b5=0,
                a1=0.2, a2=0.02, a3=0, a4=0, a5=0),
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
        small=c(b0=-5, b1=0.0027, b2=0.02, b3=0, b4=0, b5=0,
                a1=0.05, a2=0.01, a3=0, a4=0, a5=0),
        medium=c(b0=-5, b1=0.0054, b2=0.02, b3=0, b4=0, b5=0,
                 a1=0.05, a2=0.01, a3=0, a4=0, a5=0),
        large=c(b0=-5, b1=0.0073, b2=0.02, b3=0, b4=0, b5=0,
                a1=0.05, a2=0.01, a3=0, a4=0, a5=0),
        none = c(b0=0, b1=0, b2=0, b3=0, b4=0, b5=0,
                 a1=0, a2=0, a3=0, a4=0, a5=0)),
      trend = list(
        small=c(b0=-5, b1=0.0155, b2=0.01, b3=0, b4=0, b5=0,
                a1=0.2, a2=0.02, a3=0, a4=0, a5=0),
        medium=c(b0=-5, b1=0.0235, b2=0.01, b3=0, b4=0, b5=0,
                 a1=0.2, a2=0.02, a3=0, a4=0, a5=0),
        large=c(b0=-5, b1=0.0325, b2=0.01, b3=0, b4=0, b5=0,
                a1=0.2, a2=0.02, a3=0, a4=0, a5=0),
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
        small=c(b0=-5, b1=0.0027, b2=0.02, b3=0, b4=0, b5=0,
                a1=0.05, a2=0.01, a3=0, a4=0, a5=0),
        medium=c(b0=-5, b1=0.0054, b2=0.02, b3=0, b4=0, b5=0,
                 a1=0.05, a2=0.01, a3=0, a4=0, a5=0),
        large=c(b0=-5, b1=0.0073, b2=0.02, b3=0, b4=0, b5=0,
                a1=0.05, a2=0.01, a3=0, a4=0, a5=0),
        none = c(b0=0, b1=0, b2=0, b3=0, b4=0, b5=0,
                 a1=0, a2=0, a3=0, a4=0, a5=0)),
      trend = list(
        small=c(b0=-5, b1=0.0155, b2=0.01, b3=0, b4=0, b5=0,
                a1=0.2, a2=0.02, a3=0, a4=0, a5=0),
        medium=c(b0=-5, b1=0.0235, b2=0.01, b3=0, b4=0, b5=0,
                 a1=0.2, a2=0.02, a3=0, a4=0, a5=0),
        large=c(b0=-5, b1=0.0325, b2=0.01, b3=0, b4=0, b5=0,
                a1=0.2, a2=0.02, a3=0, a4=0, a5=0),
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
    model_formula=opioid_rxrateper100 ~ treatment_level + unemploymentrate + as.factor(year) + as.factor(state_abb),
    model_args=list(weights=as.name("population")),#NULL,
    se_adjust=c("none", "huber", "cluster", "arellano")
  ),
  list(
    name="autoreg_linear",
    model_call="lm",
    type="autoreg",
    model_formula=opioid_rxrateper100 ~ treatment_change + unemploymentrate + as.factor(year),
    model_args=list(weights=as.name("population")),#NULL,
    se_adjust=c("none", "huber", "cluster", "arellano")
  )
)

# for the linear config, need separate ones for now
linear_fe_config <- configure_simulation(
  # data and models required
  x=county_dta,
  models=list(linear_models[[1]]),
  # iterations
  iters=500,
  
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
    unit_var="county_fips",
    time_var="year",
    policy_speed=list("instant"),
    prior_control=c("mva3", "trend"),
    bias_type=c("linear"), #, "nonlinear"
    bias_size=c("none", "small", "medium", "large"), #"none"
    n_implementation_periods=list(0)
  )
)

linear_ar_config <- configure_simulation(
  # data and models required
  x=county_dta,
  models=list(linear_models[[2]]),
  # iterations
  iters=500,
  
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
    unit_var="county_fips",
    time_var="year",
    policy_speed=list("instant"),
    prior_control=c("mva3", "trend"),
    bias_type=c("linear"), #, "nonlinear"
    bias_size=c("none","small", "medium", "large"),#"none"
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
#   x=county_dta,
#   models=list(drdid_models[[1]]),
#   # iterations
#   iters=2500,
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

#########################
### MULTISYNTH MODELS ###
#########################
multisynth_models <- list(
  list(
    name="multisynth",
    type="multisynth",
    model_call="multisynth",
    model_formula=opioid_rxrateper100 ~ treatment_level,
    model_args=list(unit=as.name("county_fips"), time=as.name("year"), fixedeff=TRUE, form=opioid_rxrateper100 ~ treatment_level),
    se_adjust="none"
  )
)

msynth_config <- configure_simulation(
  x=county_dta,
  models=multisynth_models,
  iters=500,
  method_sample=selbias_sample,
  method_pre_model=selbias_premodel,
  method_model=selbias_model,
  method_post_model=selbias_postmodel,
  method_results=selbias_results,
  
  globals=list(
    bias_vals=bias_vals[["multisynth"]]
  ),
  
  params=list(
    unit_var="county_fips",
    time_var="year",
    policy_speed=list("instant"),
    prior_control=c("mva3", "trend"),
    bias_type=c("linear"),#"nonlinear"
    bias_size=c("small", "medium", "large"), # "none"
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
# rownames(linear_fe_results) <- NULL
# write.csv(linear_fe_results, "/poppy/programs/josephp/output/sel-bias-linear-fe-cnty-wght-Round10.csv", row.names = FALSE)
# 
# my_results = read.csv("/poppy/programs/josephp/output/sel-bias-linear-fe-cnty-wght-Round10.csv") %>%
#   group_by(prior_control, bias_size) %>%
#   summarize(n=n(),
#             mean = mean(mean_es_outcome),
#             sd = sd(mean_es_outcome))
# my_results
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
# clean up and write out results
linear_fe_results <- do.call(rbind, linear_fe_r)
rownames(linear_fe_results) <- NULL
write.csv(linear_fe_results, "/poppy/programs/josephp/output/sel-bias-linear-fe-weighted-lin-county.csv", row.names = FALSE)
proc.time2 = proc.time()
print(proc.time2-proc.time1)

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
write.csv(linear_ar_results, "/poppy/programs/josephp/output/sel-bias-linear-ar-weighted-lin-county.csv", row.names = FALSE)
proc.time2 = proc.time()
print(proc.time2-proc.time1)

#### multisynth Runs ####
proc.time1 = proc.time()
multisynth_r <- dispatch_simulations(msynth_config,
                                     use_future=T,
                                     seed=89721,
                                     verbose=2,
                                     future.globals=c("cluster_adjust_se"),
                                     future.packages=c("dplyr", "MASS", "optic", "augsynth", "DRDID"))
# clean up and write out results
multisynth_results <- do.call(rbind, multisynth_r)
rownames(multisynth_results) <- NULL
write.csv(multisynth_results, "/poppy/programs/josephp/output/sel-bias-multisynth-lin-county.csv", row.names = FALSE)
proc.time2 = proc.time()
print(proc.time2-proc.time1)

#### End-of-file ####
# TEST = drdid(yname="crude.rate", tname="year", idname="state", dname="treatment",
#              xformla = ~ unemploymentrate, panel=TRUE, data = x)



