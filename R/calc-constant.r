

#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

#' constant needed for slow coding
#' 
#' sum of vector 1:m, when m (month) is not zero
#' 
#' @param m month integer
#' @noRd
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

