

#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

#' iter results
#' 
#' @param m model object
#' @param model_simulation R6 class object for simulation config
#' @noRd
iter_results <- function(model_simulation) {
  m <- model_simulation$model_result
  
  if (class(m)[1] == "lm") {
    iter_results.lm(model_simulation)
  } else if ("negbin" %in% class(m)) {
    iter_results.negbin.glm(model_simulation)
  }
}

#' Calculate mean squared error
#' 
#' @param x vector of errors (residuals)
#' 
#' @noRd
mse_resid <- function(x) {
  return(mean(x^2, na.rm=T))
}

#' Computes summary statistics for the iteration for linear models.
#'
#' @param model_simulation 
#'
#' @noRd
iter_results.lm <- function(model_simulation) {
  m <- model_simulation$model_result
  
  coeffs <- as.data.frame(summary(m)$coefficients)
  coeffs$variable <- row.names(coeffs)
  rownames(coeffs) <- NULL
  
  treatment <- coeffs$variable[grepl("^treatment", coeffs$variable)][1]
  
  coeffs <- coeffs[coeffs$variable == treatment,]
  
  estimate <- coeffs[["Estimate"]]
  
  r <- data.frame(
    se_adjustment="none",
    estimate=estimate,
    se=coeffs[["Std. Error"]],
    variance=coeffs[["Std. Error"]] ^ 2,
    t_stat=coeffs[["t value"]],
    p_value=coeffs[["Pr(>|t|)"]],
    mse=mse_resid(m[["residuals"]]),
    stringsAsFactors=FALSE
  )
  
  mse_val <- mse_resid(m[["residuals"]])
  r <- apply_se_adjustments_lm(r, m, model_simulation, treatment,
                                estimate, mse = mse_val)

  return(r)
}

#' Computes summary statistics for the iteration for negative binomial models.
#'
#' @param model_simulation 
#'
#' @noRd
iter_results.negbin.glm <- function(model_simulation) {
  m <- model_simulation$model_result
  
  coeffs <- as.data.frame(summary(m)$coefficients)
  coeffs$variable <- row.names(coeffs)
  rownames(coeffs) <- NULL
  
  treatment <- coeffs$variable[grepl("^treatment", coeffs$variable)][1]
  
  coeffs <- coeffs[coeffs$variable == treatment,]
  
  estimate <- coeffs[["Estimate"]]
  
  r <- data.frame(
    se_adjustment="none",
    estimate=estimate,
    se=coeffs[["Std. Error"]],
    variance=coeffs[["Std. Error"]] ^ 2,
    t_stat=coeffs[["z value"]],
    p_value=coeffs[["Pr(>|z|)"]],
    mse=mse_resid(m[["residuals"]]),
    stringsAsFactors=FALSE
  )
  
  mse_val <- mse_resid(m[["residuals"]])
  r <- apply_se_adjustments_lm(r, m, model_simulation, treatment,
                                estimate, mse = mse_val)

  return(r)
}
