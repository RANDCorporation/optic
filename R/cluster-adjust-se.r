#' Adjust standard errors for clusters
#' 
#' @param model model object from regression fit
#' @param clsuter vector of cluster assignments; length should equal length of residuals in model object
#' TODO: need to cite source for this code/method
cluster_adjust_se <- function(model, cluster) {
  require(sandwich)
  require(lmtest)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- model$rank
  dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
  uj <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum))
  rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
  rcse.se <- coeftest(model, rcse.cov)
  return(list(rcse.cov, rcse.se))
}
