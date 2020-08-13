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
# SELECTION BIAS
#==============================================================================
#==============================================================================
source("R/selection-bias-methods.R")
source("R/cluster-adjust-se.r")

###################
### BIAS VALUES ###
###################
bias_vals <- list(
  # MULTISYNTH METHOD
  multisynth = list(
    linear = list(
      mva3 = list(
        small=c(b0=-5, b1=0.06, b2=0.1, b3=0, b4=0, b5=0,
                a1=1.2, a2=0.5, a3=0, a4=0, a5=0),
        medium=c(b0=-5, b1=0.095, b2=0.15, b3=0, b4=0, b5=0,
                 a1=0.95, a2=0.05, a3=0, a4=0, a5=0),
        large=c(b0=-5, b1=0.22, b2=0.3, b3=0, b4=0, b5=0,
                a1=0.95, a2=0.05, a3=0, a4=0, a5=0)),
      trend = list(
        small=c(b0=-5, b1=0.15, b2=0.1, b3=0, b4=0, b5=0,
                a1=1.2, a2=0.5, a3=0, a4=0, a5=0),
        medium=c(b0=-5, b1=0.3, b2=0.2, b3=0, b4=0, b5=0,
                 a1=1.2, a2=0.5, a3=0, a4=0, a5=0),
        large=c(b0=-5, b1=0.7, b2=0.4, b3=0, b4=0, b5=0,
                a1=1.2, a2=0.5, a3=0, a4=0, a5=0))
    ),
    nonlinear = list(
      mva3 = list(
        small=c(b0=-5, b1=0.055, b2=0.03, b3=0.00025, b4=0.00005, b5=0.000025,
                a1=1.2, a2=0.5, a3=0.2, a4=0.15, a5=0.06),
        medium=c(b0=-5, b1=0.085, b2=0.075, b3=0.0005, b4=0.0001, b5=0.00005,
                 a1=1.2, a2=0.5, a3=0.2, a4=0.15, a5=0.06),
        large=c(b0=-5, b1=0.15, b2=0.1, b3=0.001, b4=0.0002, b5=0.0001,
                a1=1.2, a2=0.5, a3=0.2, a4=0.15, a5=0.06)),
      trend = list(
        small=c(b0=-5, b1=0.15, b2=0.09, b3=0.0005, b4=0.0001, b5=0.00005,
                a1=1.2, a2=0.5, a3=0.2, a4=0.15, a5=0.06),
        medium=c(b0=-5, b1=0.265, b2=0.15, b3=0.001, b4=0.0002, b5=0.0001,
                 a1=1.2, a2=0.5, a3=0.2, a4=0.15, a5=0.06),
        large=c(b0=-5, b1=0.5, b2=0.3, b3=0.05, b4=0.025, b5=0.01,
                a1=1.2, a2=0.5, a3=0.4, a4=0.15, a5=0.06))
    )
  ),
  
  # LINEAR REGRESSION
  fixedeff_linear = list(
    linear = list(
      mva3 = list(
        small=c(b0=-5, b1=0.06, b2=0.1, b3=0, b4=0, b5=0,
                a1=0.95, a2=0.05, a3=0, a4=0, a5=0),
        medium=c(b0=-5, b1=0.1, b2=0.15, b3=0, b4=0, b5=0,
                 a1=0.95, a2=0.05, a3=0, a4=0, a5=0),
        large=c(b0=-5, b1=0.195, b2=0.2, b3=0, b4=0, b5=0,
                a1=0.95, a2=0.05, a3=0, a4=0, a5=0)
      ),
      trend = list(
        small=c(b0=-5, b1=0.15, b2=0.1, b3=0, b4=0, b5=0,
                a1=1.2, a2=0.5, a3=0, a4=0, a5=0),
        medium=c(b0=-5, b1=0.3, b2=0.2, b3=0, b4=0, b5=0,
                 a1=1.2, a2=0.5, a3=0, a4=0, a5=0),
        large=c(b0=-5, b1=0.7, b2=0.4, b3=0, b4=0, b5=0,
                a1=1.2, a2=0.5, a3=0, a4=0, a5=0)
      )
    ),
    nonlinear = list(
      mva3 = list(
        small=c(b0=-5, b1=0.055, b2=0.03, b3=0.00025, b4=0.00005, b5=0.000025,
                a1=1.2, a2=0.5, a3=0.2, a4=0.15, a5=0.06),
        medium=c(b0=-5, b1=0.085, b2=0.075, b3=0.0005, b4=0.0001, b5=0.00005,
                 a1=1.2, a2=0.5, a3=0.2, a4=0.15, a5=0.06),
        large=c(b0=-5, b1=0.15, b2=0.1, b3=0.001, b4=0.0002, b5=0.0001,
                a1=1.2, a2=0.5, a3=0.2, a4=0.15, a5=0.06)),
      trend = list(
        small=c(b0=-5, b1=0.17, b2=0.09, b3=0.0005, b4=0.0001, b5=0.00005,
                a1=1.2, a2=0.5, a3=0.2, a4=0.15, a5=0.06),
        medium=c(b0=-5, b1=0.265, b2=0.15, b3=0.001, b4=0.0002, b5=0.0001,
                 a1=1.2, a2=0.5, a3=0.2, a4=0.15, a5=0.06),
        large=c(b0=-5, b1=0.5, b2=0.3, b3=0.05, b4=0.025, b5=0.01,
                a1=1.2, a2=0.5, a3=0.4, a4=0.15, a5=0.06))
    )
  ),
  autoreg_linear = list(
    linear = list(
      mva3 = list(
        small=c(b0=-5, b1=0.06, b2=0.1, b3=0, b4=0, b5=0,
                a1=0.95, a2=0.05, a3=0, a4=0, a5=0),
        medium=c(b0=-5, b1=0.1, b2=0.15, b3=0, b4=0, b5=0,
                 a1=0.95, a2=0.05, a3=0, a4=0, a5=0),
        large=c(b0=-5, b1=0.195, b2=0.2, b3=0, b4=0, b5=0,
                a1=0.95, a2=0.05, a3=0, a4=0, a5=0)
      ),
      trend = list(
        small=c(b0=-5, b1=0.15, b2=0.1, b3=0, b4=0, b5=0,
                a1=1.2, a2=0.5, a3=0, a4=0, a5=0),
        medium=c(b0=-5, b1=0.3, b2=0.2, b3=0, b4=0, b5=0,
                 a1=1.2, a2=0.5, a3=0, a4=0, a5=0),
        large=c(b0=-5, b1=0.7, b2=0.4, b3=0, b4=0, b5=0,
                a1=1.2, a2=0.5, a3=0, a4=0, a5=0)
      )
    ),
    nonlinear = list(
      mva3 = list(
        small=c(b0=-5, b1=0.055, b2=0.03, b3=0.00025, b4=0.00005, b5=0.000025,
                a1=1.2, a2=0.5, a3=0.2, a4=0.15, a5=0.06),
        medium=c(b0=-5, b1=0.085, b2=0.075, b3=0.0005, b4=0.0001, b5=0.00005,
                 a1=1.2, a2=0.5, a3=0.2, a4=0.15, a5=0.06),
        large=c(b0=-5, b1=0.15, b2=0.1, b3=0.001, b4=0.0002, b5=0.0001,
                a1=1.2, a2=0.5, a3=0.2, a4=0.15, a5=0.06)),
      trend = list(
        small=c(b0=-5, b1=0.17, b2=0.09, b3=0.0005, b4=0.0001, b5=0.00005,
                a1=1.2, a2=0.5, a3=0.2, a4=0.15, a5=0.06),
        medium=c(b0=-5, b1=0.265, b2=0.15, b3=0.001, b4=0.0002, b5=0.0001,
                 a1=1.2, a2=0.5, a3=0.2, a4=0.15, a5=0.06),
        large=c(b0=-5, b1=0.5, b2=0.3, b3=0.05, b4=0.025, b5=0.01,
                a1=1.2, a2=0.5, a3=0.4, a4=0.15, a5=0.06))
    )
  ),
  
  # DOUBLY ROBUST DIFFERENCE-IN-DIFFERENCE
  # TODO: currently the package does not allow for longitudinal data, only accepts
  #       one pre and post period
  
  # NEGATIVE BINOMIAL
  # TODO: currently not implemented for selection bias runs since we need to think
  #       through how to make comparisons to linear models
  fixedeff_negbin = list(),
  autoreg_negbin = list()
)

#####################
### LINEAR MODELS ###
#####################
linear_models <- list(
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
    bias_type=c("linear", "nonlinear"),
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
    bias_type=c("linear", "nonlinear"),
    bias_size=c("small", "medium", "large"),
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
    se_adjust=c("none")
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
    bias_type=c("linear", "nonlinear"),
    bias_size=c("small", "medium", "large"),
    n_implementation_periods=list(0)
  )
)

#####################
### DISPATCH JOBS ###
#####################
# setup cluster
cl <- parallel::makeCluster(parallel::detectCores() - 1)
plan("cluster", workers = cl)

# dispatch with the same seed (want the same sampled data each run)
linear_fe_r <- dispatch_simulations(linear_fe_config, use_future=TRUE, seed=89721, verbose=2, future.globals=c("cluster_adjust_se"), future.packages=c("dplyr", "MASS", "optic", "augsynth", "DRDID"))
linear_ar_r <- dispatch_simulations(linear_ar_config, use_future=TRUE, seed=89721, verbose=2, future.globals=c("cluster_adjust_se"), future.packages=c("dplyr", "MASS", "optic", "augsynth", "DRDID"))
multisynth_r <- dispatch_simulations(msynth_config, use_future=TRUE, seed=89721, verbose=2, future.globals=c("cluster_adjust_se"), future.packages=c("dplyr", "MASS", "optic", "augsynth", "DRDID"))

# clean up and write out results
linear_fe_results <- do.call(rbind(linear_fe_r))
linear_ar_results <- do.call(rbind(linear_ar_r))
multisynth_results <- do.call(rbind(multisynth_r))
rownames(linear_fe_results) <- NULL
rownames(linear_ar_results) <- NULL
rownames(multisynth_results) <- NULL

write.csv(linear_fe_results, "/data/sel-bias-linear-fe-all-runs.csv", row.names = FALSE)
write.csv(linear_ar_results, "/data/sel-bias-linear-ar-all-runs.csv", row.names = FALSE)
write.csv(multisynth_results, "/data/sel-bias-multisynth-all-runs.csv", row.names = FALSE)

