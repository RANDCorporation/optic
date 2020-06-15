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
#'     method_te, method_pre_model, method_model, method_post_model, method_result.
#'     
#' 
#' @param x data.frame to use for model simulation
#' @param models list of lists for each model that should be run each iteration.
#'     The elements must be lists containing `model_call`, `model_formula`, and
#'     optionally `model_args`.
#' @param iters number of iterations for each simulation
#' @param method_sample function for sampling treated units, should modify the
#'     single_simulation$data object
#' @param method_te function for applying a treatment effect to the outcome,
#'     should modify the single_simulation$data object
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
#' @param method_class optional, string name of class that is applied to objects
#'     allowing use of S3 methods
#' @param verbose should I be chatty? Default is yes, I am chatty.
#' 
#' @export
configure_simulation <- function(
  x, models, iters, params,
  method_sample, method_te, method_model, method_results,
  method_pre_model=NULL, method_post_model=NULL,
  method_class="simulation",
  verbose=TRUE
) {
  ###
  # VALIDATION
  ###
  # all model list elements must contain at least model_call and model_formula args,
  # optional model_args - nothing else
  # also should be named, but if not apply names: model1, model2, model3, ...
  for (m in models) {
    if (!"model_call" %in% names(m)) {
      stop("model must contain `model_call` named element")
    }
    if (!"model_formula" %in% names(m)) {
      stop("model must contain `model_formula` named element")
    }
    if (any(!names(m) %in% c("model_call", "model_formula", "model_args"))) {
      stop("model list elements can only contain the following named elements: `model_call`, `model_formula`, `model_args`")
    }
  }
  
  if (is.null(names(models))) {
    message("Warning: model list elements unnamed, using: model1, model2, model3, ...")
    for (i in 1:length(models)) {
      names(models)[i] <- paste0("model", i)
    }
  } else if (any(is.na(names(models)))) {
    message("Warning: `NA` not a valid name for element in models. Renaming all to: model1, model2, model3, ...")
    for (i in 1:length(models)) {
      names(models)[i] <- paste0("model", i)
    }
  }
  
  # iters should be integer
  iters <- as.integer(iters)
  
  # confirm functions with arg of single_simulation
  if (class(method_sample) != "function") {
    stop("`method_sample` must be of class 'function'")
  }
  if (class(method_te) != "function") {
    stop("`method_te` must be of class 'function'")
  }
  if (class(method_model) != "function") {
    stop("`method_model` must be of class 'function'")
  }
  if (class(method_results) != "function") {
    stop("`method_results` must be of class 'function'")
  }
  if (class(method_sample) != "function") {
    stop("`method_sample` must be of class 'function'")
  }
  if (!is.null(method_pre_model)) {
    if (class(method_pre_model) != "function") {
      stop("`method_pre_model` must be of class 'function' or NULL")
    }
  }
  if (!is.null(method_post_model)) {
    if (class(method_post_model) != "function") {
      stop("`method_post_model` must be of class 'function' or NULL")
    }
  }
  
  ###
  # create config object
  ###
  conf <- SimConfig$new(
    data=x,
    models=models,
    iters=iters,
    params=params,
    method_class=method_class,
    method_sample=method_sample,
    method_te=method_te,
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