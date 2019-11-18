#' Calculate mean squared error
#' 
#' @param x vector of errors (residuals)
mse <- function(x) {
  return(mean(x^2, na.rm=T))
}
