

#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

#' calculate percentage of time the 95 percent CI actually covers the true
#' treatment effect estimate
#' 
#' @param beta vector of coefficients
#' @param se vector of standard errors for coefficients provided in beta
#' @param cf correction factor (single constant)
#' @param te true treatment effect
#' @noRd
coverage <- function(beta, se, te, cf=NULL, use_cf=FALSE) {
  if (length(beta) != length(se)) {
    stop("vectors provided for 'beta' and 'se' must be equal length")
  }
  
  if (use_cf) {
    se <- se * cf
  }
  ind <- rep(0, length(beta))
  low95 <- beta - (1.96 * se)
  high95 <- beta + (1.96 * se)
  ind[te > low95 & te < high95] <- 1
  return(sum(ind) / length(beta))
}
