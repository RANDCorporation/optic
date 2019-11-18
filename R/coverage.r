#' calculate percentage of time the 95 percent CI actually covers the true
#' treatment effect estimate
#' 
#' @param beta vector of coefficients
#' @param se vector of standard errors for coefficients provided in beta
#' @param cf correction factor (single constant)
#' @param te true treatment effect
coverage <- function(beta, se, cf, te) {
  if (length(beta) != length(se)) {
    stop("vectors provided for 'beta' and 'se' must be equal length")
  }
  
  se <- se * cf
  ind <- rep(0, length(beta))
  low95 <- beta - (1.96 * se)
  high95 <- beta + (1.96 * se)
  ind[te > low95 & te < high95] <- 1
  return(sum(ind) / length(beta))
}
