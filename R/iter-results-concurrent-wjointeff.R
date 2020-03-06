#' iter results
#' 
#' @param m model object\
#' 
#' @export
iter_results_concurrent_wjointeff <- function(m, ConfigObject) {
  UseMethod("iter_results_concurrent", ConfigObject)
}

iter_results_concurrent.lm <- function(m, ConfigObject) {
  coeffs <- as.data.frame(summary(m)$coefficients)
  coeffs$variable <- row.names(coeffs)
  rownames(coeffs) <- NULL
  coeffs1 <- coeffs[coeffs$variable == "treatment1",]
  coeffs2 <- coeffs[coeffs$variable == "treatment2",]
  
  estimate1 <- coeffs1[["Estimate"]]
  estimate2 <- coeffs2[["Estimate"]]
  
  joint.eff=estimate1+estimate2
  joint.eff.se=sqrt(vcov(m)[2,2]+vcov(m)[3,3]+2*vcov(m)[2,3]) #not sure how well will work on all models; only confirmed for lm so far
  joint.eff.pvalue=2 * pnorm(abs(joint.eff / joint.eff.se), lower.tail=FALSE) #thought we updated p-values to come from t-distribution. check on this?
  
  r <- data.frame(
    se_adjustment="none",
    estimate1=estimate1,
    se1=coeffs1[["Std. Error"]],
    variance1=coeffs1[["Std. Error"]] ^ 2,
    t_stat1=coeffs1[["t value"]],
    p_value1=coeffs1[["Pr(>|t|)"]],
    estimate2=estimate2,
    se2=coeffs2[["Std. Error"]],
    variance2=coeffs2[["Std. Error"]] ^ 2,
    t_stat2=coeffs2[["t value"]],
    p_value2=coeffs2[["Pr(>|t|)"]],
    joint.eff=joint.eff,
    joint.eff.se=joint.eff.se,
    variancej=joint.eff.se ^ 2,
    t_statj=joint.eff / joint.eff.se,
    joint.eff.pvalue=joint.eff.pvalue,
    stringsAsFactors=FALSE
  )
  
  if ("huber" %in% ConfigObject$se_adjust) {
    cov_h <- sandwich::vcovHC(m, type="HC0")
    h_se1 <- sqrt(diag(cov_h))[names(diag(cov_h))=="treatment1"]
    h_se2 <- sqrt(diag(cov_h))[names(diag(cov_h))=="treatment2"]
    h_sej <- sqrt(h_se1^2+h_se2^2+2*cov_h[2,3]) #need to generalize this better to cov between two treatment indicators
    
    h_r <- data.frame(
      se_adjustment="huber",
      estimate1=estimate1,
      se1=h_se1,
      variance1=h_se1 ^ 2,
      t_stat1=estimate1 / h_se1,
      p_value1=2 * pnorm(abs(estimate1 / h_se1), lower.tail=FALSE),
      estimate2=estimate2,
      se2=h_se2,
      variance2=h_se2 ^ 2,
      t_stat2=estimate2 / h_se1,
      p_value2=2 * pnorm(abs(estimate2 / h_se2), lower.tail=FALSE),
      joint.eff=joint.eff,
      joint.eff.se=h_sej,
      variancej=h_sej ^ 2,
      t_statj=joint.eff / h_sej,
      joint.eff.pvalue=2 * pnorm(abs(joint.eff / h_sej), lower.tail=FALSE),
      stringsAsFactors=FALSE
    )
    r <- rbind(r, h_r)
    rownames(r) <- NULL
  }
  
  if ("cluster" %in% ConfigObject$se_adjust) {
    clust_coeffs <- cluster_adjust_se(m, m$model$`as.factor(state)`)[[2]]
    clust_vcov <- cluster_adjust_se(m, m$model$`as.factor(state)`)[[1]][2,3] #not 100% alginment with SEs from model so worried this is off
    class(clust_coeffs) <- c("coeftest", "matrix")
    clust_coeffs <- as.data.frame(clust_coeffs)
    clust_coeffs$variable <- row.names(clust_coeffs)
    rownames(clust_coeffs) <- NULL
    clust_coeffs1 <- clust_coeffs[clust_coeffs$variable == "treatment1",]
    clust_coeffs2 <- clust_coeffs[clust_coeffs$variable == "treatment2",]
    clust_jointse <- sqrt(clust_coeffs1[["Std. Error"]]^2+clust_coeffs2[["Std. Error"]]^2 + 2*clust_vcov)
    
    c_r <- data.frame(
      se_adjustment="cluster",
      estimate1=clust_coeffs1[["Estimate"]],
      se1=clust_coeffs1[["Std. Error"]],
      variance1=clust_coeffs1[["Std. Error"]] ^ 2,
      t_stat1=clust_coeffs1[["t value"]],
      p_value1=clust_coeffs1[["Pr(>|t|)"]],
      estimate2=clust_coeffs2[["Estimate"]],
      se2=clust_coeffs2[["Std. Error"]],
      variance2=clust_coeffs2[["Std. Error"]] ^ 2,
      t_stat2=clust_coeffs2[["t value"]],
      p_value2=clust_coeffs2[["Pr(>|t|)"]],
      joint.eff=joint.eff,
      joint.eff.se=clust_jointse,
      variancej=clust_jointse ^ 2,
      t_statj=joint.eff / clust_jointse,
      joint.eff.pvalue=2 * pnorm(abs(joint.eff / clust_jointse), lower.tail=FALSE),
      stringsAsFactors=FALSE
    )
    
    r <- rbind(r, c_r)
  }
  
  if ("huber-cluster" %in% ConfigObject$se_adjust) {
    cov_hc <- sandwich::vcovHC(m, type="HC1", cluster="state", method="arellano")
    hc_se1 <- sqrt(diag(cov_hc))[names(diag(cov_hc)) == "treatment1"]
    hc_se2 <- sqrt(diag(cov_hc))[names(diag(cov_hc)) == "treatment2"]
    h_sej <- sqrt(h_se1^2+h_se2^2+2*cov_h[2,3]) 
    
    hc_r <- data.frame(
      se_adjustment="huber-cluster",
      estimate1=estimate1,
      se1=hc_se1,
      variance1=hc_se1 ^ 2,
      t_stat1=estimate1 / hc_se1,
      p_value1=2 * pnorm(abs(estimate1 / hc_se1), lower.tail=FALSE),
      estimate2=estimate2,
      se2=hc_se2,
      variance2=hc_se2 ^ 2,
      t_stat2=estimate2 / hc_se2,
      p_value2=2 * pnorm(abs(estimate2 / hc_se2), lower.tail=FALSE),
      joint.eff=joint.eff,
      joint.eff.se=h_sej,
      variancej=h_sej ^ 2,
      t_statj=joint.eff / h_sej,
      joint.eff.pvalue=2 * pnorm(abs(joint.eff / h_sej), lower.tail=FALSE),
      stringsAsFactors=FALSE
    )
    r <- rbind(r, hc_r)
    rownames(r) <- NULL
  }
  
  return(r)
}
