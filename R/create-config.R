#' Create a configuration object used to run simulations
#' 
#' @description Performs validation on inputs and produces a configuration object
#'     that contains all required parameters to dispatch simulation runs for data
#'     provided.
#' @param x data.frame to use for model simulation
#' @param unit_var variable in x that represents unit of observation (e.g., state, person, record), character name
#' @param time_var variable in x that represents time (e.g., year, month), character name
#' @param model_call currently supported options are "lm", "glm", "glm.nb"
#' @param model_formula formula or list of formulas to iterate for simulation
#' @param effect_magnitude use for non-null effect directions to modify treatment outcome
#' @param n_units number of units to apply treatment to; may provide vector of integers for multiple sims
#' @param iters number of iterations for each sim, default is 5,000
#' @param effect_direction one or more of "null", "pos", "neg"; if "pos" or "neg" are supplied and no correction
#'     factors are also provided then the simulation will throw a warning and run the null model first anyway to
#'     get correction factors
#' @param model_args optional list of additional arguments to pass to model_call
#' @param policy_speed one or more of "instant", "slow"; if "slow" then must also provide value for
#'     n_implementation_periods
#' @param n_implementation_periods number of time periods (time_var) to phase in treatment from 0 to 1
#'     for "slow" policy speed
#' @param time_period_restriction provide vector of values that exist in time_var to restrict treatmenent
#'     so it only can occur in these time periods
#' @param lag_outcome whether or not to include a lag (1 time period lag) of the outcome variable in
#'     the model
#' @param se_adjust which standard error adjustments to produce, one or more of "cluster", "huber",
#'     "huber-cluster"
#' @param concurrent TRUE if this be a simulation that tests concurrent treatment policies, default
#'     is FALSE
#' @param correction_factors provide correction factors for "pos", "neg" effect directions if not
#'     running "null"; if running "null" this will be ignored and if needed and not provided
#'     will run "null" even if not specified in effect_direction
#' @param rhos requires if concurrent is TRUE, different correlations between start dates of
#'     policies to iterate over
#' @param change_code_treatment boolean for whether or not to code treatment as a change in treatment
#'     from prior period (used for autoregressive modeling), default is FALSE
#' @param verbose should I be chatty? Default is yes, I am chatty.
#' 
#' @export
configure_simulation <- function(
  x, unit_var, time_var, model_call, model_formula, effect_magnitude, n_units,
  iters=5000, effect_direction=c("null", "pos", "neg"), model_args=NULL,
  policy_speed=c("instant"), n_implementation_periods=NA,
  time_period_restriction=NULL, lag_outcome=FALSE,
  se_adjust=c("cluster", "huber", "huber-cluster"),
  concurrent=FALSE, correction_factors=NULL, rhos=NULL, change_code_treatment=FALSE,
  verbose=TRUE
) {
  ###
  # VALIDATION
  ###
  # variables should be in the data
  if (length(unit_var) != 1 | !unit_var %in% names(x)) {
    stop(paste("unit_var must be length one and be named element in data:", unit_var))
  }
  if (length(time_var) != 1 | !time_var %in% names(x)) {
    stop(paste("time_var must be length one and be named element in data:", time_var))
  }
  
  # model call, formula, model_args should all be lists of the same length
  # as they are going to matched 1:1 by position to allow users to leverage 
  # those in one config rather than having to create multiple configs
  if (class(model_call) != "list") {
    if (length(model_call) == 1) {
      model_call <- list(model_call)
    } else {
      stop("'model_call' must be a list or of length 1")
    }
  }
  if (class(model_formula) != "list") {
    if (class(model_formula) == "formula") {
      model_formula <- list(model_formula)
    } else {
      stop("'model_formula' must be a list or of length 1")
    }
  }
  if (!is.null(model_args)) {
    if (class(model_args) != "list") {
      stop("'model_args' must be a list with the same length as 'model_call'")
    }
  }
  if (class(effect_magnitude) != "list") {
    if ( (length(effect_magnitude) == 1 & !concurrent) | (length(effect_magnitude) == 2 & concurrent) ) {
      effect_magnitude <- list(effect_magnitude)
    } else {
      stop("'effect_magnitude' must be a list, a vector length 1 if concurrent is FALSE, ora vector length 2 if concurrent is TRUE")
    }
  }
  
  if (length(model_call) != length(model_formula) | 
        (!is.null(model_args) && length(model_call) != length(model_args)) ) {
    stop("args 'model_call', 'model_formula', and 'model_args' (if provided) must all have the same length")
  }
  
  # effect magnitude and concurrent need to align
  for (i in 1:length(effect_magnitude)) {
    if (length(effect_magnitude[[i]]) == 1 & concurrent) {
      warning("only one effect magnitude provided but concurrent is TRUE, recycling")
      effect_magnitude[[i]] <- rep(effect_magnitude[[i]], 2)
    } else if (length(effect_magnitude[[i]]) > 1 & !concurrent) {
      warning("more than one effect magnitude provided but concurrent is FALSE, only using first element")
      effect_magnitude[[i]] <- effect_magnitude[[i]][1]
    } else if (length(effect_magnitude[[i]]) > 2 & concurrent) {
      warning("more than two true effects provided, only using first two elements")
      effect_magnitude[[i]] <- effect_magnitude[[i]][1:2]
    }
  }
  
  # if null effect direction excluded, need to have correction factors passed from user
  if (length(effect_direction) == 0 | is.null(effect_direction)) {
    stop("must specify at least one value for 'effect_direction'")
  }
  if (!"null" %in% effect_direction & is.null(correction_factors)) {
    warning("'null' not specified in effect_direction but no correction factors provided, adding back 'null'")
    effect_direction <- c("null", effect_direction)
  }
  
  # slow policy speed requires n_implementation_periods to be specified as well
  if ("slow" %in% policy_speed & is.na(n_implementation_periods)) {
    stop("'slow' in policy_speed, but no number provided for n_implementation_periods for length of phase in")
  }
  
  # time period must be number; and test the restriction
  if (!is.numeric(x[[time_var]])) {
    stop("time_var must be numeric")
  }
  if (!is.null(time_period_restriction)) {
    if(min(time_period_restriction) < min(x[[time_var]], na.rm=TRUE) |
       max(time_period_restriction) >  max(x[[time_var]], na.rm=TRUE)) {
      stop("time period restriction is outside the range of actual values")
    }
  }
  
  # number of units to give treatment must be no greater than total unique units in data
  for (n in n_units) {
    if (n > length(unique(x[[unit_var]]))) {
      stop(paste("number of units cannot be greater than unique values of unit_var, error for n_units:", n))
    }
  }
  
  # if concurrent must provide rhos
  if (concurrent & is.null(rhos)) {
    stop("for concurrent policy simulations at least one rho (correlation between policy start dates) must be provided")
  }
  
  # model formula should be list, if only one provided conver to list length 1
  if (class(model_formula) == "formula") {
    model_formula <- list(
      model=model_formula
    )
  }
  
  # let user know combinations and total individual models
  combs <- length(model_call) * length(n_units) * length(effect_direction) *
    length(policy_speed) * length(effect_magnitude)
  
  if (concurrent) {
    combs <- combs * length(rhos)
    runs <- combs * iters
  }
  
  if (verbose) {
    message(paste("configuration created for", combs, "combinations resulting in", runs, "individual iterations"))
    if (runs > 40000) {
      message("hey, that's a lot of iterations! we recommend using the parallel options when dispatching this job.")
    }
  }
  ###
  # create config object
  ###
  SimConfig$new(
    data=x,
    unit_var=unit_var,
    time_var=time_var,
    model_call=model_call,
    model_args=model_args,
    model_formula=model_formula,
    iters=iters,
    effect_direction=effect_direction,
    effect_magnitude=effect_magnitude,
    n_units=n_units,
    policy_speed=policy_speed,
    n_implementation_periods=n_implementation_periods,
    time_period_restriction=time_period_restriction,
    lag_outcome=lag_outcome,
    se_adjust=se_adjust,
    concurrent=concurrent,
    correction_factors=correction_factors,
    rhos=rhos,
    change_code_treatment=change_code_treatment
  )
}