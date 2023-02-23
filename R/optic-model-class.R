

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
#' @return optic_model An optic_model object to be used as an input within optic_simulation
#' @export
#'
optic_model <- function(name, type,call, formula, se_adjust, ...) {
  
  # TODO: perform any needed type conversion here
  validate_optic_model(new_optic_model(name, type, call, formula, se_adjust, ...))

}


# Constructor:
new_optic_model <- function(
                  name,
                  type= c("reg", "autoreg", "multisynth", "drdid"),
                  call= c("lm", "feols", "multisynth", "glm.nb"),
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
