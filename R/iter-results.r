#' iter results
#' 
#' @param m model object\
#' 
#' @export
iter_results <- function(m, ConfigObject) {
  UseMethod("iter_results", ConfigObject)
}

iter_results.lm <- function(m, ConfigObject) {
  coeffs <- as.data.frame(summary(m)$coefficients)
  coeffs$variable <- row.names(coeffs)
  rownames(coeffs) <- NULL
  coeffs <- coeffs[coeffs$variable == "treatment",]
  
  estimate <- coeffs[["Estimate"]]
  
  r <- data.frame(
    se_adjustment="none",
    estimate=estimate,
    se=coeffs[["Std. Error"]],
    variance=coeffs[["Std. Error"]] ^ 2,
    t_stat=coeffs[["t value"]],
    p_value=coeffs[["Pr(>|t|)"]],
    stringsAsFactors=FALSE
  )
  
  if ("huber" %in% ConfigObject$se_adjust) {
    cov_h <- sandwich::vcovHC(m, type="HC0")
    h_se <- sqrt(diag(cov_h))[names(diag(cov_h))=="treatment"]
    
    h_r <- data.frame(
      se_adjustment="huber",
      estimate=estimate,
      se=h_se,
      variance=h_se ^ 2,
      t_stat=estimate / h_se,
      p_value=2 * pnorm(abs(estimate / h_se), lower.tail=FALSE),
      stringsAsFactors=FALSE
    )
    r <- rbind(r, h_r)
    rownames(r) <- NULL
  }
  
  if ("cluster" %in% ConfigObject$se_adjust) {
    clust_coeffs <- cluster_adjust_se(m, m$model$`as.factor(state)`)[[2]]
    class(clust_coeffs) <- c("coeftest", "matrix")
    clust_coeffs <- as.data.frame(clust_coeffs)
    clust_coeffs$variable <- row.names(clust_coeffs)
    rownames(clust_coeffs) <- NULL
    clust_coeffs <- clust_coeffs[clust_coeffs$variable == "treatment",]
    
    c_r <- data.frame(
      se_adjustment="cluster",
      estimate=clust_coeffs[["Estimate"]],
      se=clust_coeffs[["Std. Error"]],
      variance=clust_coeffs[["Std. Error"]] ^ 2,
      t_stat=clust_coeffs[["t value"]],
      p_value=clust_coeffs[["Pr(>|t|)"]],
      stringsAsFactors=FALSE
    )
    
    r <- rbind(r, c_r)
  }
  
  if ("huber-cluster" %in% ConfigObject$se_adjust) {
    cov_hc <- sandwich::vcovHC(m, type="HC1", cluster="state", method="arellano")
    hc_se <- sqrt(diag(cov_hc))[names(diag(cov_hc)) == "treatment"]
    
    hc_r <- data.frame(
      se_adjustment="huber-cluster",
      estimate=estimate,
      se=hc_se,
      variance=hc_se ^ 2,
      t_stat=estimate / hc_se,
      p_value=2 * pnorm(abs(estimate / hc_se), lower.tail=FALSE),
      stringsAsFactors=FALSE
    )
    r <- rbind(r, hc_r)
    rownames(r) <- NULL
  }
  
  return(r)
}

