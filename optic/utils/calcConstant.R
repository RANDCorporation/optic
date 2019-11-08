#' constant needed for slow coding
#' 
#' sum of vector 1:m, when m (month) is not zero
#' 
#' @param m month integer
calcConstant <- function(m) {
  if (!is.integer(m)) {
    stop("'m' (month) must be an integer.")
  }
  
  v <- 0
  if (m != 0) {
    v <- sum(1:m)
  }
  return(v)
}

