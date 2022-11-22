#' Adjust standard errors for clusters
#' TODO: need to cite source for this code/method
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
