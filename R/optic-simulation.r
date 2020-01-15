#' Configuration object for OPTIC Simulation project
#' 
#' @param data data.frame or tibble object; must contain the following named 
#'     fields: "population", "deaths", "year", "state"
#' @param model_type Currently supported: "linear", "log", or "loglinear"
#' @param method_call will be used with do.call to run your model with provided
#'     parameters and additional arguments. Currently supported calls: "lm", "glm",
#'     "glm.nb", "geeglm"
#' @param model_params list of named arguments and values to be passed to the
#'     method_call specified.
#' @param outcome charcter name of field on data that should be used as outcome
#' @param iters number of iterations to run model
#' @param target_deaths number of deaths to use when calculating effect magnitude
#' @param effect_direction one of "null", "pos", "neg"
#' @param n_states number of states to sample as treatment; randomly sampled each iteration
#' @param policy_speed "instant" or "slow"
#' @param number_implementation_years required if policy_speed is slow, number of years
#'     until policy is fully phased in
#' @param set_seed if TRUE, will set and store a seed prior to sampling and running
#'     simulation for future replication
#' @param time_periods limit the years available for sampling by specifying a range,
#'     e.g.: 2003:2013
#' 
#' @importFrom R6 R6Class
#' 
#' @export
OpticConfig <- R6Class("OpticConfig", public=list(
  data=NULL,
  model_type=NULL,
  method_call=NULL,
  model_params=NULL,
  outcome=NULL,
  iters=NA,
  target_deaths=NA,
  effect_direction=NULL,
  n_states=NA,
  policy_speed=NULL,
  total_population=NA,
  total_deaths=NA,
  number_implementation_years=NA,
  seed=NA,
  time_periods=NULL,
  iter_results=NULL,
  summary_results=NULL,
  lag_outcome=FALSE,
  se_adjust=NULL,
  
  initialize = function(data, model_type, method_call, model_params, iters, target_deaths, effect_direction, n_states,
                        policy_speed, outcome, se_adjust=c("huber", "cluster", "huber-cluster"),
                        lag_outcome=FALSE, number_implementation_years=NULL, set_seed=FALSE, time_periods=NULL) {
    # input/data validation
    datafields <- c("population", "deaths", "year", "state")
    stopifnot(is.data.frame(data))
    stopifnot(all(datafields %in% names(data)))
    stopifnot(is.character(model_type), length(model_type) == 1)
    stopifnot(is.character(outcome), length(outcome) == 1)
    stopifnot(is.character(method_call), length(method_call) == 1)
    stopifnot(is.list(model_params), rlang::is_formula(model_params[["formula"]]))
    stopifnot(is.numeric(iters))
    stopifnot(is.numeric(target_deaths))
    stopifnot(effect_direction %in% c("null", "pos", "neg"))
    stopifnot(is.numeric(n_states))
    stopifnot(policy_speed %in% c("instant", "slow"))
    stopifnot(is.numeric(number_implementation_years))
    
    self$data <- data
    self$outcome <- outcome
    self$model_type <- model_type
    self$method_call <- method_call
    self$model_params <- model_params
    self$iters <- iters
    self$target_deaths <- target_deaths
    self$effect_direction <- effect_direction
    self$n_states <- n_states
    self$policy_speed <- policy_speed
    self$number_implementation_years <- number_implementation_years
    self$total_population <- sum(data$population, na.rm=TRUE)
    self$total_deaths <- sum(data$deaths, na.rm=TRUE)
    self$time_periods <- time_periods
    self$lag_outcome <- lag_outcome
    self$se_adjust <- se_adjust
    
    if (set_seed) {
      self$seed <- sample(999:99999999, 1)
    }
    
    # dispatch to correct S3 methods
    class(self) <- c(class(self), self$model_type, self$method_call)
  },
  
  print = function(...) {
    # TODO: print method
    cat("TODO: print method.")
  })
)
