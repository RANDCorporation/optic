#' Calculate mean squared error
#' 
#' @param x vector or errors (residuals)
mse <- function(x) {
  return(mean(x^2,na.rm=T))
}
