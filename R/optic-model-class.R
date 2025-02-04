

#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# Optic Model (optic_model) class constructor:

#' Optic Model 
#' 
#' @description Generates model object to apply to each simulated dataset
#'
#' @param name Name of the model object, used to identify the model when reviewing simulation results
#' @param type Estimator used to identify the treatment effect using simulated data. Specified as a string, which can either be 'reg' (regression), 'autoreg' (autoregression, which adds a lag for the outcome variable to a regression model), 'drdid' (doubly-robust difference-in-difference estimator), or 'multisynth' (augmented synthetic control)
#' @param call String which specifies the R function to call for applying the estimator. Package currently supports  either 'lm' (linear model), 'feols' (fixed-effect OLS), 'multisynth' (pooled synthetic controls), or 'glm.nb' (negative-binomial generalized nearlized linear model)
#' @param formula Model specification, using R formula formatting. Must include a variable labeled 'treatment' for the 'nonconf' & 'selbias' simulation method or variables labeled 'treatment1' & 'treatment2' for the simulation method 'concurrent'
#' @param se_adjust Adjustments applied to standard errors following model estimation. Specified as a string, OPTIC currently support 'none' for no adjustment or 'cluster' for clustered standard errors. Clustered standard errors will use the 'unit_var' specified in optic_simulation for determining unit used for clustering standard errors.
#' @param ... Additional arguments that are passed to the model call. Please refer to documentation for each model call for additional details. If the model call expects a name, you may need to pass your parameter using param = as.name("variable_name") as opposed to param = variable_name.
#' @return optic_model An optic_model object to be used as an input within optic_simulations. Details model calls and parameters.
#' @examples 
#' 
#' # Set up a simple linear model
#' form <- formula(crude.rate ~ state + year + population + treatment_level)
#' mod <- optic_model(name = 'lin', 
#'                    type = 'reg', 
#'                    call = 'lm', 
#'                    formula = form, 
#'                    se_adjust = 'none')
#' 
#' # Deploy an auto-regressive model.
#' # type = "autoreg" will make AR term 
#' # automatically when the model is deployed; also note
#' # in formula the use of "treatment_change" as the treatment variable 
#' # rather than "treatment_level" like in the previous example:
#' 
#' form_ar <- formula(crude.rate ~ state + year + population + treatment_change)
#' mod_ar <- optic_model(name = "auto_regressive_linear", 
#'                       type = "autoreg", 
#'                       call = "lm", 
#'                       formula = form_ar,
#'                       se_adjust = "none")
#' 
#' # One could also use a different call, assuming the right packages 
#' # are installed and the model uses a familiar formula framework. 
#' # Example with random intercept for states, using lme4 package.
#' 
#' form_me <- formula(crude.rate ~ 
#'                    population + year + treatment_level + (1|state))
#'                    
#' mod_me <- optic_model(name = "mixed_effect", 
#'                       type = "reg", 
#'                       call = "lmer", 
#'                       formula = form_me, 
#'                       se_adjust = "none")
#' 
#' @export
#'
optic_model <- function(name, type,call, formula, se_adjust, ...) {
  
  validate_optic_model(new_optic_model(name = name, type = type, call = call, formula = formula, se_adjust = se_adjust, ...))

}


# Constructor:
new_optic_model <- function(
                  name,
                  type= c("reg", "autoreg", "autoreg_debiased", "multisynth", "did", "eventstudy", "did2s", "did_imputation"),
                  call= c("lm", "feols", "autoreg_debiased", "multisynth", "glm.nb", "att_gt", "did2s", "did_imputation", ""),
                  formula,
                  args=list(weights=as.name('population')),
                  se_adjust=c("none", "cluster"),
                  ...
                  ){
  
  # model object
  m <- list()
  
  # basic checks and model assigments
  stopifnot(is.character(name))
  stopifnot(length(name) == 1)
  m$name <- name
  
  m$type <- match.arg(type)
  
  stopifnot(rlang::is_formula(formula))
  m$model_formula <- formula
  
  m$model_call <- call
  
  # Browser
  # Create list of arguments:
  m$model_args <- list(...)
  
  # if weight is passed, make sure it is a name:
  if("weights" %in% names(m$model_args)) {
    m$model_args$weights <- as.name(m$model_args$weights)
  }
  
  stopifnot(is.character(se_adjust))
  m$se_adjust <- se_adjust
  
  # Assign S3 class
  class(m) <- c("optic_model", "list")
  
  return(m)

}

# Validator
validate_optic_model <- function(x) {
  
  # implement validation logic
  return(x)
  
}
