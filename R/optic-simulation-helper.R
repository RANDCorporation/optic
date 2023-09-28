

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
#' @param iters A numeric, specifying number of iterations for each simulation scenario.
#' @param unit_var A string variable, used to determine clusters for clustered standard errors.
#' @param time_var A string variable, specifying time units (e.g. "year", "time to treat", etc). Must be specified in terms of years (fractional years are accepted).
#' @param treat_var A string variable, referring to the unit-of-analysis for treatment (which may not be the same as the unit var argument, e.g. treated classrooms within clustered schools)
#' @param conf_var An unobserved confounding variable. Only used for the 'confound-method'.
#' @param prior_control Only used for confounding method. Adds an additional set of variables which control for the outcome in previous periods (either a moving average of previous time periods or an autoregressive term)
#' @param effect_magnitude A vector of numerics, specifying 'true' effect sizes for treatment scenarios. See vignette for more details. Synthetic datasets will be generated for each entry in the vector.
#' @param n_units  A numeric, determining number of units to simulate treatment effects. Synthetic datasets will be generated for each entry in the vector.
#' @param effect_direction A vector containing either 'neg', 'null', or 'pos'. Determines the direction of the simulated effect. Synthetic datasets will be generated for each entry in the vector. 
#' @param policy_speed A vector of strings, containing either 'instant' or 'slow' entries, determining how quickly treated units obtain the simulated effect. Synthetic datasets will be generated for each entry in the vector. Can either be 'instant" (so treatment effect applies fully in the first treated time period) or 'slow' (treatment effect ramps up linearly to the desired effect size, based on `n_implementation_periods`.
#' @param bias_type A string, either linear" or "nonlinear". Specifies type of bias for 'confounding' method 
#' @param bias_size A string, either "small" "medium" or "large". Specifies relative size of bias for 'confounding' method. 
#' @param n_implementation_periods A vector of numerics, determining number of periods after implementation until treated units reach the desired simulated treatment effect. Synthetic datasets will be generated for each entry in the vector.
#' @param rhos A vector of values between 0-1, indicating the correlation between the primary policy and a concurrent policy. Only applies when 'method' == 'concurrent'. Synthetic datasets will be generated for each entry in the vector.
#' @param years_apart A numeric, for number of years between the primary policy being implemented and the concurrent policy. Only applies when 'method' == 'concurrent'.
#' @param ordered A boolean, determines if the primary policy always occurs before the concurrent policy (`TRUE`) or if the policies are randomly ordered (`FALSE`).
#' @param method A string, determing the simulation method. Can be either 'no_confounding', 'confounding' or 'concurrent'
#' @param method_sample Underlying function for the sampling method to determine treatment status. Provided here for convenience so that the user does not need to modify the actual underlying function's script.
#' @param method_pre_model Similar to method_sample argument, this variable is provided as a convenience for the user. This function transforms the treatment effect, after it's simulated within the synthetic data.
#' @param method_model Another convenience function, which can be modified to control the model call.
#' @param method_post_model Another convenience function, which can be modified to control transformations to the simulated effect, after modeling.
#' @param method_results Another convenience function, which can be modified to control the simulation results that are returned.
#' @param verbose Boolean, default True. If TRUE, provides summary details on simulation runs across iterations
#' @param globals Additional globals to pass to the simulate function, such as parallelization packages or additional R packages used by method calls (e.g. modeling packages, like "FEOLS").
#' @returns An OpticSim object, which contains simulation and model parameters for simulation runs, which is used as an input for dispatch_simulations.
#' @examples 
#' 
#' # Load data for simulation and set up a hypothetical policy effect: 
#'
#' data(overdoses)
#' eff <- 0.1*mean(overdoses$crude.rate, na.rm = TRUE)
#' 
#' # Set up a simple linear model
#' form <- formula(crude.rate ~ state + year + population + treatment_level)
#' mod <- optic_model(name = 'lin', 
#'                    type = 'reg', 
#'                    call = 'lm', 
#'                    formula = form, 
#'                    se_adjust = 'none')
#' 
#' # Create simulation object, with desired parameters for simulations:
#' sim <- optic_simulation(x = overdoses, 
#'                         models = list(mod), 
#'                         method = 'no_confounding', 
#'                         unit_var = 'state', 
#'                         treat_var = 'state',
#'                         time_var = 'year', 
#'                         effect_magnitude = list(eff), 
#'                         n_units = 10, 
#'                         effect_direction = 'pos', 
#'                         iters = 10,
#'                         policy_speed = 'instant', 
#'                         n_implementation_periods = 1)
#' 
#' @export
#' 
#' @importFrom purrr cross
#' @importFrom purrr transpose
#' 
optic_simulation <- function(x, models, iters,
                             unit_var, time_var,
                             conf_var,
                             effect_magnitude, 
                             n_units, 
                             effect_direction, 
                             policy_speed, 
                             prior_control = "level", # level should be levels
                             bias_size = NULL,
                             bias_type = NULL,
                             treat_var = NULL,
                             n_implementation_periods,
                             rhos = NULL, years_apart = NULL, ordered = NULL,
                             method,
                             method_sample, method_model, method_results,     
                             method_pre_model, method_post_model, 
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
  
  # Start setting the default methods:
  if(method == "concurrent") {
    d_method_sample= concurrent_sample
    d_method_pre_model= concurrent_premodel
    d_method_model= concurrent_model
    d_method_post_model= concurrent_postmodel
    d_method_results= concurrent_results
    
    # Check method-specific inputs:
    stopifnot(is.numeric(years_apart))
    stopifnot(is.numeric(rhos))
    stopifnot(is.logical(ordered))
    
  } else if (method == "no_confounding") {
    #stopifnot(is.character(treat_var))
    d_method_sample = noconf_sample
    d_method_pre_model = noconf_premodel
    d_method_model = noconf_model
    d_method_post_model = noconf_postmodel
    d_method_results = noconf_results
  } else if (method == "confounding") {
    # Note: Add these new parameters when incorporating the confounding runs.
    stopifnot(bias_type %in% c("linear","nonlinear"))
    stopifnot(is.character(conf_var))
    stopifnot(length(conf_var) == 1)
    stopifnot(conf_var %in% names(x))
    stopifnot(bias_size %in% c("small","medium","large"))
  
    #stop("Confounding is currently not implemented.")
    # Here, assign confounding methods once they are implemented
    # Uncomment once we bring the confounding code to the package.
    d_method_sample = selbias_sample
    d_method_pre_model = selbias_premodel
    d_method_model = selbias_model
    d_method_post_model = selbias_postmodel
    d_method_results = selbias_results
  } else if (method == "time_varying"){
    d_method_sample = tvary_sample
    d_method_pre_model = tvary_premodel
    d_method_model = tvary_model
    d_method_post_model = tvary_postmodel
    d_method_results = tvary_results
  } else {
    stop("method argument must be either no_confounding, confounding, concurrent, or time-varying")
  }
  
  # Choose among default methods or user-provided methods.
  if(missing(method_sample)) {
    method_sample <- d_method_sample
  } 
  
  if(missing(method_pre_model)) {
    method_pre_model <- d_method_pre_model
  } 
  
  if(missing(method_model)) {
    method_model <- d_method_model
  } 
  
  if(missing(method_post_model)) {
    method_post_model <- d_method_post_model
  } 
  
  if(missing(method_results)) {
    method_results <- d_method_results
  } 
  
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
      stop("`method_pre_model` must be of class 'function'")
    }
  }
  if (!is.null(method_post_model)) {
    if (!is.function(method_post_model)) {
      stop("`method_post_model` must be of class 'function'")
    }
  }
  
  # check parameters
  # these checks are likely very stringent and could be relaxed for some
  # combinations of inputs
  stopifnot(prior_control %in% c("level", "trend"))
  stopifnot(is.character(unit_var), length(unit_var) == 1)
  stopifnot(is.character(time_var), length(time_var) == 1)
  stopifnot(is.list(effect_magnitude))
  stopifnot(is.numeric(n_units))
  stopifnot(all(effect_direction %in% c("null", "neg", "pos")))
  stopifnot(all(policy_speed %in% c("instant", "slow")))
  stopifnot(is.numeric(n_implementation_periods))
  
  # Create list with mandatory parameters
  params <- list(unit_var = unit_var,
                 time_var = time_var,
                 effect_magnitude = effect_magnitude,
                 n_units = n_units,
                 effect_direction = effect_direction,
                 policy_speed = policy_speed,
                 n_implementation_periods = n_implementation_periods,
                 prior_control = prior_control)
  
  # Add method-specific parameters if they are provided:
  if(!missing(rhos)) {
    params$rhos <- rhos
  }
  
  if(!missing(years_apart)) {
    params$years_apart <- years_apart
  }
  
  if(!missing(ordered)) {
    params$ordered <- ordered
  }
  
  if(!missing(treat_var)) {
    params$treat_var <- treat_var
  }
  
  if(!missing(conf_var)) {
    params$conf_var <- conf_var
  }
  
  if(!missing(bias_size)) {
    params$bias_size <- bias_size
  }
  
  if(!missing(bias_type)) {
    params$bias_type <- bias_type
  }
  
  # Compute prior_control variables:
  first_model_outcome <- get_model_outcome(models[[1]])
  
  x <- compute_prior_controls(data = x,
                              unit_var = params$unit_var,
                              time_var = params$time_var,
                              outcome_var = first_model_outcome)
  
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
