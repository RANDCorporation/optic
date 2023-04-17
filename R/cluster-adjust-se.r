

#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

#' Adjust standard errors for clusters
#' We verified that this function correctly computes clustered standard errors.
#' the output of this function matches STATA and R's clustered standard errors.
#' 
#' @param model model object from regression fit
#' @param clsuter vector of cluster assignments; length should equal length of residuals in model object
#' @noRd
cluster_adjust_se <- function(model, cluster) {
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- model$rank
  dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
  uj <- apply(sandwich::estfun(model), 2, function(x) tapply(x, cluster, sum))
  rcse.cov <- dfc * sandwich::sandwich(model, meat = crossprod(uj)/N)
  rcse.se <- lmtest::coeftest(model, rcse.cov)
  return(list(rcse.cov, rcse.se))
}
