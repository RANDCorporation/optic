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

source("R/dispatch-simulations.R")
source("R/no-confounding-methods.R")
source("R/apply-treatment-effect.R")
source("R/cluster-adjust-se.r")

#### 0%, 5%, 10%, 15%, 20% ####
linear0 <- 0
linear5 <- .05*mean(county_dta$opioid_rxrateper100)
linear15 <- .15*mean(county_dta$opioid_rxrateper100)
linear25 <- .25*mean(county_dta$opioid_rxrateper100)

#### Setup various no-confoudning sims ####
linear_models <- list(
  list(
    name="fixedeff_linear",
    type="reg",
    model_call="lm",
    model_formula=opioid_rxrateper100 ~ treatment_level + unemploymentrate + as.factor(year) + as.factor(state_abb),
    model_args=list(weights=as.name("population")),# NULL
    se_adjust=c("cluster")
  ),
  list(
    name="autoreg_linear",
    model_call="lm",
    type="autoreg",
    model_formula=opioid_rxrateper100 ~ treatment_change + unemploymentrate + as.factor(year),
    model_args=list(weights=as.name("population")),# NULL
    se_adjust=c("none")
  )
)

# for the linear config, need separate ones for now
linear_fe_config <- configure_simulation(
  # data and models required
  x=county_dta,
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
    unit_var="county_fips",
    treat_var="county_fips",
    time_var="year",
    effect_magnitude=list(linear0, linear5, linear15, linear25),
    n_units=c(53, 264, 791, 1581), #2%, 10%, 30%, 60%
    effect_direction=c("neg"),
    policy_speed=list("instant"),
    n_implementation_periods=list(0), 
    prior_control=c("mva3", "trend")#
  )
)

linear_ar_config <- configure_simulation(
  # data and models required
  x=county_dta,
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
    unit_var="county_fips",
    treat_var="county_fips",
    time_var="year",
    effect_magnitude=list(linear0, linear5, linear15, linear25),
    n_units=c(53, 264, 791, 1581), #2%, 10%, 30%, 60% 
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
    model_formula=opioid_rxrateper100 ~ treatment_level,
    model_args=list(unit=as.name("county_fips"), time=as.name("year"), fixedeff=TRUE, form=opioid_rxrateper100 ~ treatment_level),
    se_adjust="none"
  )
)

msynth_config <- configure_simulation(
  x=county_dta,
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
    unit_var="county_fips",
    treat_var="county_fips",
    time_var="year",
    effect_magnitude=list(linear0, linear5, linear15, linear25),
    n_units=c(53, 264, 791, 1581), #2%, 10%, 30%, 60%
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
# dispatch with the same seed (want the same sampled data each run)
proc.time1 = proc.time()
linear_fe_r <- dispatch_simulations(linear_fe_config,
                                    use_future=TRUE,
                                    seed=89721,
                                    verbose=2,
                                    future.globals=c("cluster_adjust_se",
                                                     "dispatch_simulations",
                                                     "apply_treatment_effect"),
                                    future.packages=c("MASS", "dplyr", "optic", "augsynth", "DRDID"))
proc.time2 = proc.time()
print(proc.time2-proc.time1)
# clean up and write out results
linear_fe_results <- do.call(rbind, linear_fe_r)
rownames(linear_fe_results) <- NULL
write.csv(linear_fe_results, "/poppy/programs/josephp/output/cnty-noconf-opioidrx-linear-fe-weighted-05-25-21.csv", row.names = FALSE)

#### Autoregressive Runs ####
proc.time1 = proc.time()
linear_ar_r <- dispatch_simulations(linear_ar_config,
                                    use_future=TRUE,
                                    seed=89721,
                                    verbose=2,
                                    future.globals=c("cluster_adjust_se",
                                                     "dispatch_simulations",
                                                     "apply_treatment_effect"),
                                    future.packages=c("MASS", "dplyr", "optic", "augsynth", "DRDID"))
# clean up and write out results
linear_ar_results <- do.call(rbind, linear_ar_r)
rownames(linear_ar_results) <- NULL
write.csv(linear_ar_results, "/poppy/programs/josephp/output/cnty-noconf-opioidrx-linear-ar-weighted-05-25-21.csv", row.names = FALSE)
proc.time2 = proc.time()
print(proc.time2-proc.time1)

#### multisynth Runs ####
# proc.time1 = proc.time()
# multisynth_r <- dispatch_simulations(msynth_config,
#                                      use_future=TRUE,
#                                      seed=89721,
#                                      verbose=2,
#                                      future.globals=c("cluster_adjust_se",
#                                                       "dispatch_simulations",
#                                                       "apply_treatment_effect"),
#                                      future.packages=c("MASS", "dplyr", "optic", "augsynth", "DRDID"))
# proc.time2 = proc.time()
# print(proc.time2-proc.time1)
# # clean up and write out results
# multisynth_results <- do.call(rbind, multisynth_r)
# rownames(multisynth_results) <- NULL
# write.csv(multisynth_results, "/poppy/programs/josephp/output/cnty-noconf-opioidrx-linear-multisynth-weighted-05-20-21.csv", row.names = FALSE)

#### End-of-file ####