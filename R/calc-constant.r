#' constant needed for slow coding
#' 
#' sum of vector 1:m, when m (month) is not zero
#' 
#' @param m month integer
calc_constant <- function(m) {
  if (!is.numeric(m)) {
    stop("'m' (month) must be an numeric representation.")
  }
  
  v <- 0
  if (m != 0) {
    v <- sum(1:m)
  }
  return(v)
}

