

#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

#' Create a configuration object used to run simulations
#' 
#' @description Performs validation on inputs and produces a configuration object
#'     that contains all required parameters to dispatch simulation runs for the empirical data
#'     provided.
#'
#' @details The resulting configuration object is used to pass simulation scenarios to the 'simulate' function. Provided as a convenience function to the user
#'          so they can investigate simulation arguments prior to running models.
#' 
#' @param x Empirical data used to simulate synthetic datasets with specified treatment effect.
#' @param models List of `optic_model` objects that should be run for each iteration and simulation scenario.
#'     The elements must be created using the `optic_model` function.
#' @param iters Number of iterations for each simulation scenario.
#' @param unit_var char(1) variable specifying the unit to be simulate (i.e., "state")
#' @param time_var char(1) variable specifying the time variable (i.e., "year")
#' @param effect_magnitude document. list of scenarios to consider
#' @param n_units integer. document.
#' @param effect_direction character vector containing the effects direction to consider. can include "null", "pos" and/or "neg"
#' @param policy_speed document. either "instant" or "slow"
#' @param n_implementation_periods document
#' @param rhos document
#' @param years_apart document
#' @param ordered boolean. document
#' @param method_sample function for sampling treated units, should modify the
#'     single_simulation$data object
#' @param method_pre_model optional function that will be applied single_simulation
#'     object prior to modeling
#' @param method_model function to run model, default performs a do.call on the 
#'     model_call, passing model_formula and model_args if provided. Resulting 
#'     model object is added to single_simulation list under named element
#'     "model_result"
#' @param method_post_model optional function for any post-processing on 
#'     single_simulation object
#' @param method_results function that takes the single_simulation object and
#'     the return value is what it returned from run_iteration
#' @param verbose Default True. If TRUE, provides summary details on simulation runs across iterations
#' @param globals Additional globals to pass to the simulate function, such as parallelization packages or additional R packages used by method calls.
#' 
#' @export
#' 
#' @importFrom purrr cross
#' @importFrom purrr transpose
#' 
optic_simulation <- function(x, models, iters,
                             unit_var, time_var, 
                             effect_magnitude, n_units, 
                             effect_direction, 
                             policy_speed, 
                             n_implementation_periods, rhos, years_apart, ordered,  
                             method_sample, method_model, method_results,     
                             method_pre_model=NULL, method_post_model=NULL, 
                             globals=NULL, verbose=TRUE) {
  ###
  # VALIDATION
  ###
  # all model list elements must contain at least model_call and model_formula args,
  # optional model_args - nothing else
  # also should be named, but if not apply names: model1, model2, model3, ...
  for (m in models) {
    if (!"name" %in% names(m)) {
      stop("model must contain `name` named element")
    }
    if (!"model_call" %in% names(m)) {
      stop("model must contain `model_call` named element")
    }
    if (!"model_formula" %in% names(m)) {
      stop("model must contain `model_formula` named element")
    }
  }
  for (i in 1:length(models)) {
    names(models)[i] <- models[[i]][["name"]]
  }
  
  # iters should be integer
  iters <- as.integer(iters)
  
  # confirm functions with arg of single_simulation
  if (!is.function(method_sample)) {
    stop("`method_sample` must be of class 'function'")
  }
  if (!is.function(method_model)) {
    stop("`method_model` must be of class 'function'")
  }
  if (!is.function(method_results)) {
    stop("`method_results` must be of class 'function'")
  }
  if (!is.function(method_sample)) {
    stop("`method_sample` must be of class 'function'")
  }
  if (!is.null(method_pre_model)) {
    if (!is.function(method_pre_model)) {
      stop("`method_pre_model` must be of class 'function' or NULL")
    }
  }
  if (!is.null(method_post_model)) {
    if (!is.function(method_post_model)) {
      stop("`method_post_model` must be of class 'function' or NULL")
    }
  }
  
  # check parameters
  # these checks are likely very stringent and could be relaxed for some
  # combinations of inputs
  stopifnot(is.character(unit_var), length(unit_var) == 1)
  stopifnot(is.character(time_var), length(time_var) == 1)
  stopifnot(is.list(effect_magnitude))
  stopifnot(is.numeric(n_units), length(n_units) == 1)
  stopifnot(all(effect_direction %in% c("null", "neg", "pos")))
  stopifnot(all(policy_speed %in% c("instant", "slow")))
  stopifnot(is.numeric(n_implementation_periods))
  stopifnot(is.numeric(rhos))
  stopifnot(is.logical(ordered))
  
  params <- list(unit_var = unit_var,
                 time_var = time_var,
                 effect_magnitude = effect_magnitude,
                 n_units = n_units,
                 effect_direction = effect_direction,
                 policy_speed = policy_speed,
                 n_implementation_periods = n_implementation_periods,
                 rhos = rhos,
                 years_apart = years_apart,
                 ordered = ordered)
  
  ###
  # create a OpticSim object
  # It is difficult to 
  ###
  conf <- OpticSim$new(
    data=x,
    models=models,
    iters=iters,
    params=params,
    globals=globals,
    method_sample=method_sample,
    method_pre_model=method_pre_model,
    method_model=method_model,
    method_post_model=method_post_model,
    method_results=method_results
  )
  
  if (verbose) {
    # let user know combinations and total individual models
    combs <- length(models) * length(purrr::cross(params))
    runs <- combs * iters
    
    print(conf)
    if (runs > 40000) {
      cat("hey, that's a lot of iterations! we recommend using the parallel options when dispatching this job.\n")
    }
  }
  
  return(conf)
}
