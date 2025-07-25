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
                  type= c("reg", "autoreg", "multisynth", "did"),
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
  
  # Basic model structure validation
  required_fields <- c("name", "type", "model_call", "model_formula", "se_adjust")
  missing_fields <- required_fields[!required_fields %in% names(x)]
  if (length(missing_fields) > 0) {
    stop("Missing required model fields: ", paste(missing_fields, collapse = ", "))
  }
  
  # Validate type and call compatibility
  valid_types <- c("reg", "autoreg", "multisynth", "did")
  valid_calls <- c("lm", "feols", "multisynth", "glm.nb", "att_gt")
  
  if (!x$type %in% valid_types) {
    stop("Model type '", x$type, "' not supported. Must be one of: ", paste(valid_types, collapse = ", "))
  }
  
  if (!x$model_call %in% valid_calls) {
    stop("Model call '", x$model_call, "' not supported. Must be one of: ", paste(valid_calls, collapse = ", "))
  }
  
  # Type-specific call compatibility
  validate_type_call_compatibility(x$type, x$model_call)
  
  return(x)
}

#' Validate compatibility between model type and call
#' @param type Model type
#' @param call Model call
#' @noRd
validate_type_call_compatibility <- function(type, call) {
  compatible_combinations <- list(
    "reg" = c("lm", "feols", "glm.nb"),
    "autoreg" = c("lm", "feols", "glm.nb"),
    "multisynth" = c("multisynth"),
    "did" = c("att_gt")
  )
  
  if (!call %in% compatible_combinations[[type]]) {
    stop("Model call '", call, "' is not compatible with type '", type, "'. ",
         "Compatible calls for '", type, "' are: ", 
         paste(compatible_combinations[[type]], collapse = ", "))
  }
}

#' Initial validation when OpticSim is created
#' @param model An optic_model object
#' @param data The simulation data
#' @param params Simulation parameters
#' @noRd
validate_optic_model_init <- function(model, data, params) {
  
  # Basic model validation
  if (!inherits(model, "optic_model")) {
    stop("Model must be an optic_model object")
  }
  
  # Data validation
  if (!is.data.frame(data)) {
    stop("Data must be a data.frame")
  }
  
  # Check required variables exist in data
  required_vars <- c(params$unit_var, params$time_var)
  missing_vars <- required_vars[!required_vars %in% names(data)]
  if (length(missing_vars) > 0) {
    stop("Required variables missing from data: ", paste(missing_vars, collapse = ", "))
  }
  
  # Type-specific validation
  switch(model$type,
    "reg" = validate_reg_init(model, data, params),
    "autoreg" = validate_autoreg_init(model, data, params),
    "multisynth" = validate_multisynth_init(model, data, params),
    "did" = validate_did_init(model, data, params),
    stop("Unknown model type: ", model$type)
  )
  
  # Call-specific validation
  switch(model$model_call,
    "lm" = validate_lm_init(model, data, params),
    "feols" = validate_feols_init(model, data, params),
    "multisynth" = validate_multisynth_call_init(model, data, params),
    "glm.nb" = validate_glm_nb_init(model, data, params),
    "att_gt" = validate_att_gt_init(model, data, params),
    stop("Unknown model call: ", model$model_call)
  )
}

#' Pre-call validation when model is about to be executed
#' @param model An optic_model object
#' @param args Arguments prepared for the model call
#' @noRd
validate_optic_model_pre_call <- function(model, args) {
  
  # Ensure data argument exists
  if (!"data" %in% names(args)) {
    stop("Model args must contain 'data' element")
  }
  
  data <- args$data
  
  # Type-specific pre-call validation
  switch(model$type,
    "reg" = validate_reg_pre_call(model, args),
    "autoreg" = validate_autoreg_pre_call(model, args),
    "multisynth" = validate_multisynth_pre_call(model, args),
    "did" = validate_did_pre_call(model, args),
    stop("Unknown model type: ", model$type)
  )
  
  # Call-specific pre-call validation
  switch(model$model_call,
    "lm" = validate_lm_pre_call(model, args),
    "feols" = validate_feols_pre_call(model, args),
    "multisynth" = validate_multisynth_call_pre_call(model, args),
    "glm.nb" = validate_glm_nb_pre_call(model, args),
    "att_gt" = validate_att_gt_pre_call(model, args),
    stop("Unknown model call: ", model$model_call)
  )
}

# =============================================================================
# TYPE-SPECIFIC INIT VALIDATIONS
# =============================================================================

#' @noRd
validate_reg_init <- function(model, data, params) {
  # Check for treatment variable in formula
  formula_vars <- all.vars(model$model_formula)
  # Accept standard and concurrent treatment variable names
  treatment_vars <- c(
    "treatment_level", "treatment_change", "treatment_date", "time_to_treat",
    "treatment1_level", "treatment2_level", "treatment1_change", "treatment2_change"
  )
  
  if (!any(treatment_vars %in% formula_vars)) {
    stop(
      "Formula must contain at least one treatment variable: ",
      paste(shQuote(treatment_vars), collapse = ", "),
      ". Formula variables are: ",
      paste(shQuote(formula_vars), collapse = ", ")
    )
  }
  
  # Check outcome variable exists in data
  outcome_var <- all.vars(model$model_formula)[1]  # LHS of formula
  if (!outcome_var %in% names(data)) {
    stop("Outcome variable '", outcome_var, "' not found in data")
  }
}

#' @noRd
validate_autoreg_init <- function(model, data, params) {
  # Similar to reg but should use treatment_change
  formula_vars <- all.vars(model$model_formula)
  treatment_change_vars <- c("treatment_change", "treatment1_change", "treatment2_change")
  if (!any(treatment_change_vars %in% formula_vars)) {
    warning("Autoreg models typically use one of 'treatment_change', 'treatment1_change', or 'treatment2_change' as the treatment variable")
  }
  
  # Check outcome variable exists
  outcome_var <- all.vars(model$model_formula)[1]
  if (!outcome_var %in% names(data)) {
    stop("Outcome variable '", outcome_var, "' not found in data")
  }
}

#' @noRd
validate_multisynth_init <- function(model, data, params) {
  # Detailed validation for multisynth
  required_args <- c("unit", "time")
  missing_args <- required_args[!required_args %in% names(model$model_args)]
  if (length(missing_args) > 0) {
    stop("multisynth models require model_args: ", paste(missing_args, collapse = ", "))
  }
  
  # Check unit and time variables exist in data
  unit_arg <- model$model_args$unit
  time_arg <- model$model_args$time
  
  # Handle if arguments are symbols/names
  if (is.name(unit_arg)) unit_arg <- as.character(unit_arg)
  if (is.name(time_arg)) time_arg <- as.character(time_arg)
  
  if (!unit_arg %in% names(data)) {
    stop("Unit variable '", unit_arg, "' specified in model_args not found in data")
  }
  if (!time_arg %in% names(data)) {
    stop("Time variable '", time_arg, "' specified in model_args not found in data")
  }
  
  # Check outcome variable
  outcome_var <- all.vars(model$model_formula)[1]
  if (!outcome_var %in% names(data)) {
    stop("Outcome variable '", outcome_var, "' not found in data")
  }
  
  # Check for treatment variable
  formula_vars <- all.vars(model$model_formula)
  treatment_vars <- c("treatment_level", "treatment")
  if (!any(treatment_vars %in% formula_vars)) {
    stop("multisynth formula must contain a treatment variable")
  }
}

#' @noRd
validate_did_init <- function(model, data, params) {
  # Check required model_args for DID
  required_args <- c("yname", "tname", "idname", "gname")
  missing_args <- required_args[!required_args %in% names(model$model_args)]
  if (length(missing_args) > 0) {
    stop("DID models require model_args: ", paste(missing_args, collapse = ", "))
  }
  
  # Check variables exist in data, except for gname == 'treatment_date'
  for (arg_name in required_args) {
    var_name <- model$model_args[[arg_name]]
    if (arg_name == "gname" && var_name == "treatment_date") {
      next  # skip check for treatment_date at init
    }
    if (!var_name %in% names(data)) {
      stop("Variable '", var_name, "' specified in ", arg_name, " not found in data")
    }
  }
}

# =============================================================================
# CALL-SPECIFIC INIT VALIDATIONS
# =============================================================================

#' @noRd
validate_lm_init <- function(model, data, params) {
  # Basic checks for lm - formula should be valid
  if (!rlang::is_formula(model$model_formula)) {
    stop("lm requires a valid formula")
  }
}

#' @noRd
validate_feols_init <- function(model, data, params) {
  # Similar to lm but may have additional feols-specific requirements
  if (!rlang::is_formula(model$model_formula)) {
    stop("feols requires a valid formula")
  }
}

#' @noRd
validate_multisynth_call_init <- function(model, data, params) {
  # Specific checks for multisynth call
  if (!"lambda" %in% names(model$model_args)) {
    warning("multisynth models typically specify lambda parameter")
  }
}

#' @noRd
validate_glm_nb_init <- function(model, data, params) {
  # Check for family specification or other glm.nb requirements
  if (!rlang::is_formula(model$model_formula)) {
    stop("glm.nb requires a valid formula")
  }
}

#' @noRd
validate_att_gt_init <- function(model, data, params) {
  # Validation for att_gt call - similar to did type validation
  validate_did_init(model, data, params)
}

# =============================================================================
# TYPE-SPECIFIC PRE-CALL VALIDATIONS
# =============================================================================

#' @noRd
validate_reg_pre_call <- function(model, args) {
  # Check data has required variables for regression
  data <- args$data
  formula_vars <- all.vars(model$model_formula)
  missing_vars <- formula_vars[!formula_vars %in% names(data)]
  if (length(missing_vars) > 0) {
    stop("Variables missing from data for regression: ", paste(missing_vars, collapse = ", "))
  }
}

#' @noRd
validate_autoreg_pre_call <- function(model, args) {
  # Check lag_outcome exists (should be created in premodel step)
  data <- args$data
  if (!"lag_outcome" %in% names(data)) {
    stop("lag_outcome variable missing - should be created in premodel step")
  }
  validate_reg_pre_call(model, args)
}

#' @noRd
validate_multisynth_pre_call <- function(model, args) {
  # Check treatment is binary for multisynth
  data <- args$data
  if ("treatment" %in% names(data)) {
    treatment_vals <- unique(data$treatment)
    if (!all(treatment_vals %in% c(0, 1))) {
      stop("multisynth requires binary treatment variable (0/1)")
    }
  }
  
  # Check no missing values in outcome
  outcome_var <- all.vars(model$model_formula)[1]
  if (sum(is.na(data[[outcome_var]])) > 0) {
    stop("multisynth cannot handle missing values in outcome variable")
  }
  
  # Ensure form argument is set correctly
  if (!"form" %in% names(args)) {
    stop("multisynth requires 'form' argument, not 'formula'")
  }
}

#' @noRd
validate_did_pre_call <- function(model, args) {
  # Check treatment is binary
  data <- args$data
  if ("treatment" %in% names(data)) {
    treatment_vals <- unique(data$treatment)
    if (!all(treatment_vals %in% c(0, 1))) {
      stop("DID models require binary treatment variable (0/1)")
    }
  }
  
  # Check treatment_date formatting
  if ("treatment_date" %in% names(data)) {
    # Check for Inf values for untreated units
    untreated_dates <- data$treatment_date[data$treatment == 0]
    if (!all(is.infinite(untreated_dates) | is.na(untreated_dates))) {
      warning("Untreated units should have treatment_date = Inf")
    }
  }
}

# =============================================================================
# CALL-SPECIFIC PRE-CALL VALIDATIONS
# =============================================================================

#' @noRd
validate_lm_pre_call <- function(model, args) {
  # Ensure formula argument exists
  if (!"formula" %in% names(args)) {
    stop("lm requires 'formula' argument")
  }
}

#' @noRd
validate_feols_pre_call <- function(model, args) {
  validate_lm_pre_call(model, args)
}

#' @noRd
validate_multisynth_call_pre_call <- function(model, args) {
  # Ensure required arguments for multisynth call
  required_args <- c("form", "unit", "time")
  missing_args <- required_args[!required_args %in% names(args)]
  if (length(missing_args) > 0) {
    stop("multisynth call missing required arguments: ", paste(missing_args, collapse = ", "))
  }
}

#' @noRd
validate_glm_nb_pre_call <- function(model, args) {
  if (!"formula" %in% names(args)) {
    stop("glm.nb requires 'formula' argument")
  }
}

#' @noRd
validate_att_gt_pre_call <- function(model, args) {
  # Check required arguments for att_gt
  required_args <- c("yname", "tname", "idname", "gname")
  missing_args <- required_args[!required_args %in% names(args)]
  if (length(missing_args) > 0) {
    stop("att_gt call missing required arguments: ", paste(missing_args, collapse = ", "))
  }
}
