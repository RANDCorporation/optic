#' create binary indicator for significance of p-value based on given level
#' 
#' @param p vector of p-values
#' @param level threshold for significance; default is 0.05
pval_flag <- function(p, level=0.05) {
  p[p < level] <- 1
  p[p != 1] <- 0
  return(p)
}
