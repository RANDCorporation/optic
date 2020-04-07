#' Create a configuration object used to run simulations
#' 
#' @description Performs validation on inputs and produces a configuration object
#'     that contains all required parameters to dispatch simulation runs for data
#'     provided.
#' @param x data.frame to use for model simulation
#' @param outcome outcome variable in x, character name
#' @param unit_var variable in x that represents unit of observation (e.g., state, person, record), character name
#' @param time_var variable in x that represents time (e.g., year, month), character name
#' @param model_call currently supported options are "lm", "glm", "glm.nb"
#' @param true_effect the true effect that will be used for non-null effect direction to augment outcome
#' @param n_units number of units to apply treatment to; may provide vector of integers for multiple sims
#' @param iters number of iterations for each sim, default is 5,000
#' @param effect_direction one or more of "null", "pos", "neg"; if "pos" or "neg" are supplied and no correction
#'     factors are also provided then the simulation will throw a warning and run the null model first anyway to
#'     get correction factors
#' @param model_args optional list of additional arguments to pass to model_call
#' @param model_formula formula or list of formulas to iterate for simulation
#' @param policy_speed one or more of "instant", "slow"; if "slow" then must also provide value for
#'     n_implementation_periods
#' @param n_implementation_periods number of time periods (time_var) to phase in treatment from 0 to 1
#'     for "slow" policy speed
#' @param set_seed if TRUE will use seed and seed will be included in results for reproducibility;
#'     default is FALSE
#' @param time_period_restriction provide vector of value that exist in time_var to restrict treatmenent
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
#' 
#' @export
configure_simulation <- function(
  x, outcome, unit_var, time_var, model_call, model_formula, true_effect, n_units,
  iters=5000, effect_direction=c("null", "pos", "neg"), model_args=NULL,
  policy_speed=c("instant"), n_implementation_periods=NA, set_seed=FALSE,
  time_period_restriction=NULL, lag_outcome=FALSE,
  se_adjust=c("cluster", "huber", "huber-cluster"),
  concurrent=FALSE, correction_factors=NULL, rhos=NULL, change_code_treatment=FALSE,
  verbose=TRUE
) {
  ###
  # VALIDATION
  ###
  # variables should be in the data
  if (length(outcome) != 1 | !outcome %in% names(x)) {
    stop(paste("outcome must be length one and be named element in data:", outcome))
  }
  if (length(unit_var) != 1 | !unit_var %in% names(x)) {
    stop(paste("unit_var must be length one and be named element in data:", unit_var))
  }
  if (length(time_var) != 1 | !time_var %in% names(x)) {
    stop(paste("time_var must be length one and be named element in data:", time_var))
  }
  if (length(model_call) != 1 | !model_call %in% c("lm", "glm", "glm.nb")) {
    stop("must provide only one model_call and must be one of: 'lm', 'glm' or 'glm.nb'")
  }
  
  # true effect and concurrent need to align
  if (length(true_effect) > 1 & !concurrent) {
    warning("more than one true effect provided but concurrent is FALSE, only using first element")
    true_effect <- true_effect[1]
  } else if (length(true_effect) > 2 & concurrent) {
    warning("more than two true effects provided, only using first two elements")
    true_effect <- true_effect[1:2]
  } else if (length(true_effect) == 1 & concurrent) {
    warning("only one true effect provided and concurrent is TRUE, using for both treatments")
    true_effect <- c(true_effect, true_effect)
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
  
  # model call and formula should be same length as they are going to matched
  # 1:1 by position to allow users to leverage those in one config rather than
  # having to create multiple configs
  if (length(model_call) > length(model_formula)) {
    warning("length of model_call greater than length of model_formula provided, recycling first model_formula")
    n_need <- length(model_call) - length(model_formula)
    for (i in 1:n_need) {
      l <- list()
      l[[paste0("recycled_model", i)]] <- model_formula[[1]]
      model_formula <- c(model_formula, l)
    }
  }
  if (length(model_call) < length(model_formula)) {
    warning("length of model_formula greater than length of model_call provided, recycling first model_call")
    n_need <- length(model_formula) - length(model_call)
    model_call <- c(model_call, rep(model_call[1], n_need))
  }
  
  # let user know combinations and total individual models
  combs <- length(model_formula) * length(n_units) * length(effect_direction) *
    length(policy_speed) * length(model_call)
  if (concurrent) {
    combs <- combs * length(rhos)
    runs <- combs * iters
  }
  if (verbose) {
    message(paste("configuration created for", combs, "combinations resulting in", runs, "individual iterations"))
    if (runs > 20000) {
      message("hey, that's a lot of iterations! we recommend using the parallel options when dispatching this job.")
    }
    if (!set_seed) {
      message("looks like you are not using a seed for this. consider any need for reproducability before dispatching, for science!")
    }
  }
  ###
  # create config object
  ###
  SimConfig$new(
    data=x,
    outcome=outcome,
    unit_var=unit_var,
    time_var=time_var,
    model_call=model_call,
    model_args=model_args,
    model_formula=model_formula,
    iters=iters,
    effect_direction=effect_direction,
    true_effect=true_effect,
    n_units=n_units,
    policy_speed=policy_speed,
    n_implementation_periods=n_implementation_periods,
    set_seed=set_seed,
    time_period_restriction=time_period_restriction,
    lag_outcome=lag_outcome,
    se_adjust=se_adjust,
    concurrent=concurrent,
    correction_factors=correction_factors,
    rhos=rhos,
    change_code_treatment=change_code_treatment
  )
}