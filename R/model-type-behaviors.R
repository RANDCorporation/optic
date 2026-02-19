
#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# Model Type Behavior Registry
#
# Central registry mapping each model type to a set of behavior functions.
# Method files call these via get_behavior(type)$<behavior>(...) instead of
# branching on type with if/else chains.
#
# Each model type provides:
#   get_outcome(model)           - extract outcome variable name
#   prepare_premodel(x, ms)      - data transforms before modeling
#   recode_treatment_date(x, unit_sym) - recode treatment_date per model needs
#   prepare_model_args(args, model, ms) - transform args for the model call
#   extract_results(m, model, ms) - extract estimate/se/p_value from fitted model


# -----------------------------------------------------------------------------
# Lookup
# -----------------------------------------------------------------------------

#' Look up behavior functions for a model type
#'
#' @param model_type Character string identifying the model type
#' @return A named list of behavior functions for that type
#' @noRd
get_behavior <- function(model_type) {
  b <- .model_type_registry[[model_type]]
  if (is.null(b)) {
    stop("Unknown model type '", model_type, "'. ",
         "Registered types: ", paste(names(.model_type_registry), collapse = ", "))
  }
  b
}


# =============================================================================
# BEHAVIOR 1: get_outcome
# =============================================================================

# Default: extract LHS of formula
.get_outcome_formula <- function(model) {
  model_terms(model[["model_formula"]])[["lhs"]]
}

# DID: outcome stored in model_args$yname
.get_outcome_did <- function(model) {
  as.character(model$model_args$yname)
}


# =============================================================================
# BEHAVIOR 2: prepare_premodel
# =============================================================================

# Default (reg): no-op
.prepare_premodel_noop <- function(x, model_simulation) {
  list(x = x, models = model_simulation$models)
}

# Autoreg: add lag_outcome column and update formula
.prepare_premodel_autoreg <- function(x, model_simulation) {
  model <- model_simulation$models
  outcome <- get_behavior(model$type)$get_outcome(model)
  oo <- dplyr::sym(outcome)
  unit_sym <- dplyr::sym(model_simulation$unit_var)
  time_sym <- dplyr::sym(model_simulation$time_var)

  x <- x %>%
    dplyr::arrange(!!unit_sym, !!time_sym) %>%
    dplyr::group_by(!!unit_sym) %>%
    dplyr::mutate(lag_outcome = dplyr::lag(!!oo, n = 1L)) %>%
    dplyr::ungroup()

  model$model_formula <- .update_formula_with_lag(model$model_formula, model$model_call)

  list(x = x, models = model)
}

# Autoeffect: create binary treatment indicator
.prepare_premodel_autoeffect <- function(x, model_simulation) {
  model <- model_simulation$models
  ae_args <- resolve_autoeffect_args(model, model_simulation$unit_var, model_simulation$time_var)
  trt_col <- ae_args$trt_name
  if (!trt_col %in% names(x)) {
    x[[trt_col]] <- as.numeric(x$treatment > 0)
  } else {
    x[[trt_col]] <- as.numeric(x[[trt_col]] > 0)
  }
  list(x = x, models = model)
}

# Multisynth: binarize treatment, filter NAs
.prepare_premodel_multisynth <- function(x, model_simulation) {
  model <- model_simulation$models
  outcome <- get_behavior(model$type)$get_outcome(model)
  oo <- dplyr::sym(outcome)

  x$treatment[x$treatment > 0] <- 1
  x$treatment_level[x$treatment_level > 0] <- 1
  x <- x %>% dplyr::filter(!is.na(!!oo))
  if (sum(is.na(x[[outcome]])) > 0) {
    stop("multisynth method cannot handle missingness in outcome.")
  }
  list(x = x, models = model)
}

# DID: binarize treatment
.prepare_premodel_did <- function(x, model_simulation) {
  model <- model_simulation$models
  x$treatment[x$treatment > 0] <- 1
  x$treatment_level[x$treatment_level > 0] <- 1
  list(x = x, models = model)
}


# =============================================================================
# BEHAVIOR 3: recode_treatment_date
# =============================================================================

# reg/autoreg/eventstudy: set 0 -> Inf
.recode_trt_date_inf <- function(x, unit_sym) {
  x %>% dplyr::mutate(treatment_date = ifelse(treatment_date == 0, Inf, treatment_date))
}

# did: set 0 -> Inf, convert unit to numeric
.recode_trt_date_did <- function(x, unit_sym) {
  x %>%
    dplyr::mutate(
      treatment_date = ifelse(treatment_date == 0, Inf, treatment_date),
      !!unit_sym := as.numeric(as.factor(!!unit_sym))
    )
}

# multisynth/did2s: binarize treatment
.recode_trt_date_binarize <- function(x, unit_sym) {
  x %>% dplyr::mutate(treatment = ifelse(treatment > 0, 1, 0))
}

# did_imputation: no-op (expects treatment_date == 0 for untreated)
.recode_trt_date_noop <- function(x, unit_sym) {
  x
}

# autoeffect: no-op
.recode_trt_date_autoeffect <- function(x, unit_sym) {
  x
}


# =============================================================================
# BEHAVIOR 4: prepare_model_args
# =============================================================================

# reg/autoreg: pass formula as fml (feols) or formula (lm)
.prepare_model_args_reg <- function(args, model, model_simulation) {
  if (model$model_call == "feols") {
    args[["fml"]] <- model$model_formula
    # feols expects weights as a one-sided formula, not a name/symbol
    if (!is.null(args[["weights"]]) && is.name(args[["weights"]])) {
      args[["weights"]] <- stats::as.formula(paste0("~", as.character(args[["weights"]])))
    }
  } else {
    args[["formula"]] <- model$model_formula
  }
  args
}

# autoeffect: resolve ae_args and map params
.prepare_model_args_autoeffect <- function(args, model, model_simulation) {
  ae_args <- resolve_autoeffect_args(model, model_simulation$unit_var, model_simulation$time_var)
  # Remove optic-specific args
  args[["effect_lag"]] <- NULL
  args[["formula"]] <- NULL
  # Map autoeffect-specific args
  args[["lags"]] <- ae_args$lags
  args[["outcome_name"]] <- ae_args$outcome_name
  args[["unit_name"]] <- ae_args$unit_name
  args[["date_name"]] <- ae_args$date_name
  args[["trt_name"]] <- ae_args$trt_name
  if (!is.null(ae_args$cov_names)) {
    args[["cov_names"]] <- ae_args$cov_names
  } else if ("cov_names" %in% names(args)) {
    args[["cov_names"]] <- NULL
  }
  args
}

# multisynth: use 'form' parameter
.prepare_model_args_multisynth <- function(args, model, model_simulation) {
  args[["form"]] <- model$model_formula
  args
}

# did: no formula needed (att_gt gets all from model_args)
.prepare_model_args_did <- function(args, model, model_simulation) {
  args
}

# did_imputation: add horizon = TRUE
.prepare_model_args_did_imputation <- function(args, model, model_simulation) {
  args[["horizon"]] <- TRUE
  args
}

# did2s: add treatment = 'treatment'
.prepare_model_args_did2s <- function(args, model, model_simulation) {
  args[["treatment"]] <- "treatment"
  args
}

# eventstudy: always use fml (feols)
.prepare_model_args_eventstudy <- function(args, model, model_simulation) {
  args[["fml"]] <- model$model_formula
  # feols expects weights as a one-sided formula, not a name/symbol
  if (!is.null(args[["weights"]]) && is.name(args[["weights"]])) {
    args[["weights"]] <- stats::as.formula(paste0("~", as.character(args[["weights"]])))
  }
  args
}


# =============================================================================
# BEHAVIOR 5: extract_results
# =============================================================================

# reg/autoreg/did2s: extract from summary coeftable or coefficients
.extract_results_reg <- function(m, model, model_simulation) {
  if (model$model_call == "feols") {
    cf <- as.data.frame(summary(m)$coeftable)
  } else {
    cf <- as.data.frame(summary(m)$coefficients)
  }
  cf$variable <- row.names(cf)
  rownames(cf) <- NULL

  treatment <- cf$variable[grepl("^treatment", cf$variable)][1]
  cf <- cf[cf$variable == treatment, ]
  estimate <- cf[["Estimate"]]

  if (model$model_call == "lmer") {
    pval <- 1.96 * (1 - pnorm(abs(cf[["t value"]])))
  } else {
    pval <- c(cf[["Pr(>|t|)"]], cf[["Pr(>|z|)"]])
  }

  data.frame(
    outcome = get_behavior(model$type)$get_outcome(model),
    se_adjustment = "none",
    estimate = estimate,
    se = cf[["Std. Error"]],
    variance = cf[["Std. Error"]]^2,
    t_stat = c(cf[["t value"]], cf[["z value"]]),
    p_value = pval,
    mse = mean(stats::resid(m)^2, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

# did: aggte(type='group')
.extract_results_did <- function(m, model, model_simulation) {
  outcome <- get_behavior(model$type)$get_outcome(model)
  m_agg <- did::aggte(m, type = "group", na.rm = TRUE)

  cf <- data.frame(estimate = m_agg$overall.att,
                   se = m_agg$overall.se)
  estimate <- cf$estimate
  se <- cf$se
  variance <- se^2
  p_value <- 2 * pnorm(abs(estimate / se), lower.tail = FALSE)

  data.frame(
    outcome = outcome,
    se_adjustment = "none",
    estimate = estimate,
    se = se,
    variance = variance,
    t_stat = NA,
    p_value = p_value,
    mse = NA,
    stringsAsFactors = FALSE
  )
}

# autoeffect: cumulative_effects at effect_lag
.extract_results_autoeffect <- function(m, model, model_simulation) {
  outcome <- get_behavior(model$type)$get_outcome(model)
  ae_args <- resolve_autoeffect_args(model, model_simulation$unit_var, model_simulation$time_var)
  effect_lag <- ae_args$effect_lag
  cum_effects <- m$cumulative_effects

  if (!isTRUE(m$convergence) || is.null(cum_effects)) {
    estimate <- NA_real_
    se <- NA_real_
  } else {
    if (!effect_lag %in% cum_effects$Lag) {
      stop("Requested effect_lag = ", effect_lag, " not available in cumulative effects output.")
    }
    effect_row <- cum_effects[cum_effects$Lag == effect_lag, ]
    estimate <- effect_row$Estimate
    se <- effect_row$StdError
  }

  variance <- se^2
  t_stat <- ifelse(is.na(se) || se == 0, NA_real_, estimate / se)
  p_value <- ifelse(is.na(t_stat), NA_real_, 2 * pnorm(abs(t_stat), lower.tail = FALSE))
  mse <- if (!is.null(m$model)) {
    mean(stats::residuals(m$model)^2, na.rm = TRUE)
  } else {
    NA_real_
  }

  data.frame(
    outcome = outcome,
    se_adjustment = "none",
    estimate = estimate,
    se = se,
    variance = variance,
    t_stat = t_stat,
    p_value = p_value,
    mse = mse,
    stringsAsFactors = FALSE
  )
}

# multisynth: summary$att
.extract_results_multisynth <- function(m, model, model_simulation) {
  outcome <- get_behavior(model$type)$get_outcome(model)
  cf <- summary(m)
  cf <- cf$att
  estimate <- cf[cf$Level == "Average" & is.na(cf$Time), "Estimate"]
  se <- cf[cf$Level == "Average" & is.na(cf$Time), "Std.Error"]
  variance <- se^2
  p_value <- 2 * pnorm(abs(estimate / se), lower.tail = FALSE)
  mse <- mean(unlist(lapply(m$residuals, function(x) { mean(x^2) })))

  data.frame(
    outcome = outcome,
    se_adjustment = "none",
    estimate = estimate,
    se = se,
    variance = variance,
    t_stat = NA,
    p_value = p_value,
    mse = mse,
    stringsAsFactors = FALSE
  )
}


# =============================================================================
# HELPER: update formula with lag_outcome
# =============================================================================

# Handles both feols pipe syntax and standard lm formulas.
# For feols formulas with "|" (fixed effects), string-splits to add
# lag_outcome before the pipe. For lm, uses update.formula().
.update_formula_with_lag <- function(fml, model_call) {
  formula_components <- as.character(fml)

  if (model_call == "feols" && grepl("|", formula_components[3], fixed = TRUE)) {
    # feols pipe syntax: "rhs_vars | fe_vars"
    parts <- strsplit(formula_components[3], " | ", fixed = TRUE)
    new_fmla <- as.formula(
      paste(formula_components[2], formula_components[1],
            parts[[1]][[1]], "+ lag_outcome |", parts[[1]][[2]])
    )
  } else {
    new_fmla <- update.formula(fml, ~ . + lag_outcome)
  }
  new_fmla
}


# =============================================================================
# REGISTRY
# =============================================================================

.model_type_registry <- list(
  reg = list(
    get_outcome          = .get_outcome_formula,
    prepare_premodel     = .prepare_premodel_noop,
    recode_treatment_date = .recode_trt_date_inf,
    prepare_model_args   = .prepare_model_args_reg,
    extract_results      = .extract_results_reg
  ),
  autoreg = list(
    get_outcome          = .get_outcome_formula,
    prepare_premodel     = .prepare_premodel_autoreg,
    recode_treatment_date = .recode_trt_date_inf,
    prepare_model_args   = .prepare_model_args_reg,
    extract_results      = .extract_results_reg
  ),
  autoeffect = list(
    get_outcome          = .get_outcome_formula,
    prepare_premodel     = .prepare_premodel_autoeffect,
    recode_treatment_date = .recode_trt_date_autoeffect,
    prepare_model_args   = .prepare_model_args_autoeffect,
    extract_results      = .extract_results_autoeffect
  ),
  multisynth = list(
    get_outcome          = .get_outcome_formula,
    prepare_premodel     = .prepare_premodel_multisynth,
    recode_treatment_date = .recode_trt_date_binarize,
    prepare_model_args   = .prepare_model_args_multisynth,
    extract_results      = .extract_results_multisynth
  ),
  did = list(
    get_outcome          = .get_outcome_did,
    prepare_premodel     = .prepare_premodel_did,
    recode_treatment_date = .recode_trt_date_did,
    prepare_model_args   = .prepare_model_args_did,
    extract_results      = .extract_results_did
  ),
  eventstudy = list(
    get_outcome          = .get_outcome_formula,
    prepare_premodel     = .prepare_premodel_noop,
    recode_treatment_date = .recode_trt_date_inf,
    prepare_model_args   = .prepare_model_args_eventstudy,
    extract_results      = .extract_results_reg
  ),
  did2s = list(
    get_outcome          = .get_outcome_formula,
    prepare_premodel     = .prepare_premodel_noop,
    recode_treatment_date = .recode_trt_date_binarize,
    prepare_model_args   = .prepare_model_args_did2s,
    extract_results      = .extract_results_reg
  ),
  did_imputation = list(
    get_outcome          = .get_outcome_formula,
    prepare_premodel     = .prepare_premodel_noop,
    recode_treatment_date = .recode_trt_date_noop,
    prepare_model_args   = .prepare_model_args_did_imputation,
    extract_results      = NULL  # did_imputation result extraction is model-specific
  )
)
