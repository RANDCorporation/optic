#' Create a configuration object used to run simulations
#' 
#' @description Performs validation on inputs and produces a configuration object
#'     that contains all required parameters to dispatch simulation runs for data
#'     provided.
#'
#' @details The resulting configuration object is passed to run iteration when
#'     dispatching simulations. The run_iteration method will first create a
#'     single_simulation object from self$setup_single_simulation(i), where i is
#'     the ith simulation that was configured. Then run_iteration will pass the
#'     single_simulation object to each of the method functions. The method
#'     functions should modify elements of the single_simulation list and return
#'     the entire single_simulation object. The method functions are run in the
#'     following order (any that are set to NULL will be skipped): method_sample,
#'     method_pre_model, method_model, method_post_model, method_result.
#'     
#' 
#' @param x data.frame to use for model simulation
#' @param models list of `optic_model` objects that should be run each iteration.
#'     The elements must be created with the `optic_model` function.
#' @param iters number of iterations for each simulation
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
#' @param verbose should I be chatty? Default is yes, I am chatty.
#' @param globals specifies whether to use globals
#' 
#' @export
#' 
#' @importFrom purrr cross
#' @importFrom purrr transpose
#' 
optic_simulation <- function(x, models, iters, params, method_sample, method_model, method_results, 
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
  
  ###
  # create a simconfig object
  # It is difficult to 
  ###
  conf <- SimConfig$new(
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

  browser()
  
  return(conf)
}
