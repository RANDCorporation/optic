

#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

#' iter results
#' 
#' @param model_simulation
#' 
#' @noRd
iter_results_concurrent_wjointeff <- function(model_simulation) {
  m <- model_simulation$model_result
  
  if (class(m)[1] == "lm") {
    iter_results_concurrent_wjointeff.lm(model_simulation)
  } else if ("negbin" %in% class(m)) {
    iter_results_concurrent_wjointeff.negbin.glm(model_simulation)
  }
}

iter_results_concurrent_wjointeff.lm <- function(model_simulation) {
  m <- model_simulation$model_result
  
  coeffs <- as.data.frame(summary(m)$coefficients)
  coeffs$variable <- row.names(coeffs)
  rownames(coeffs) <- NULL
  coeffs1 <- coeffs[grepl("treatment1", coeffs$variable), ]
  coeffs2 <- coeffs[grepl("treatment2", coeffs$variable), ]
  
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
    test_stat1=coeffs1[["t value"]],
    p_value1=coeffs1[["Pr(>|t|)"]],
    estimate2=estimate2,
    se2=coeffs2[["Std. Error"]],
    variance2=coeffs2[["Std. Error"]] ^ 2,
    test_stat2=coeffs2[["t value"]],
    p_value2=coeffs2[["Pr(>|t|)"]],
    joint.eff=joint.eff,
    joint.eff.se=joint.eff.se,
    variancej=joint.eff.se ^ 2,
    test_statj=joint.eff / joint.eff.se,
    joint.eff.pvalue=joint.eff.pvalue,
    stringsAsFactors=FALSE
  )
  
  if ("huber" %in% model_simulation$models$se_adjust) {
    cov_h <- sandwich::vcovHC(m, type="HC0")
    h_se1 <- sqrt(diag(cov_h))[grepl("treatment1", names(diag(cov_h)))]
    h_se2 <- sqrt(diag(cov_h))[grepl("treatment2", names(diag(cov_h)))]
    h_sej <- sqrt(h_se1^2+h_se2^2+2*cov_h[2,3]) #need to generalize this better to cov between two treatment indicators
    
    h_r <- data.frame(
      se_adjustment="huber",
      estimate1=estimate1,
      se1=h_se1,
      variance1=h_se1 ^ 2,
      test_stat1=estimate1 / h_se1,
      p_value1=2 * pnorm(abs(estimate1 / h_se1), lower.tail=FALSE),
      estimate2=estimate2,
      se2=h_se2,
      variance2=h_se2 ^ 2,
      test_stat2=estimate2 / h_se1,
      p_value2=2 * pnorm(abs(estimate2 / h_se2), lower.tail=FALSE),
      joint.eff=joint.eff,
      joint.eff.se=h_sej,
      variancej=h_sej ^ 2,
      test_statj=joint.eff / h_sej,
      joint.eff.pvalue=2 * pnorm(abs(joint.eff / h_sej), lower.tail=FALSE),
      stringsAsFactors=FALSE
    )
    r <- rbind(r, h_r)
    rownames(r) <- NULL
  }
  
  if ("cluster" %in% model_simulation$models$se_adjust) {
    clust_indices <- as.numeric(rownames(m$model))
    clust_var <- as.character(model_simulation$data[[model_simulation$unit_var]][clust_indices])
    clust_coeffs <- cluster_adjust_se(m, clust_var)[[2]]
    clust_vcov <- cluster_adjust_se(m, clust_var)[[1]][2,3] #not 100% alginment with SEs from model so worried this is off
    class(clust_coeffs) <- c("coeftest", "matrix")
    clust_coeffs <- as.data.frame(clust_coeffs)
    clust_coeffs$variable <- row.names(clust_coeffs)
    rownames(clust_coeffs) <- NULL
    clust_coeffs1 <- clust_coeffs[grepl("treatment1", clust_coeffs$variable), ]
    clust_coeffs2 <- clust_coeffs[grepl("treatment2", clust_coeffs$variable), ]
    clust_jointse <- sqrt(clust_coeffs1[["Std. Error"]]^2+clust_coeffs2[["Std. Error"]]^2 + 2*clust_vcov)
    
    c_r <- data.frame(
      se_adjustment="cluster",
      estimate1=clust_coeffs1[["Estimate"]],
      se1=clust_coeffs1[["Std. Error"]],
      variance1=clust_coeffs1[["Std. Error"]] ^ 2,
      test_stat1=clust_coeffs1[["t value"]],
      p_value1=clust_coeffs1[["Pr(>|t|)"]],
      estimate2=clust_coeffs2[["Estimate"]],
      se2=clust_coeffs2[["Std. Error"]],
      variance2=clust_coeffs2[["Std. Error"]] ^ 2,
      test_stat2=clust_coeffs2[["t value"]],
      p_value2=clust_coeffs2[["Pr(>|t|)"]],
      joint.eff=joint.eff,
      joint.eff.se=clust_jointse,
      variancej=clust_jointse ^ 2,
      test_statj=joint.eff / clust_jointse,
      joint.eff.pvalue=2 * pnorm(abs(joint.eff / clust_jointse), lower.tail=FALSE),
      stringsAsFactors=FALSE
    )
    
    r <- rbind(r, c_r)
  }
  
  if ("arellano" %in% model_simulation$models$se_adjust) {
    clust_indices <- as.numeric(rownames(m$model))
    clust_var <- as.character(model_simulation$data[[model_simulation$unit_var]][clust_indices])
    cov_hc <- sandwich::vcovHC(m, type="HC1", cluster=clust_var, method="arellano")
    hc_se1 <- sqrt(diag(cov_hc))[grepl("treatment1", names(diag(cov_hc)))]
    hc_se2 <- sqrt(diag(cov_hc))[grepl("treatment2", names(diag(cov_hc)))]
    h_sej <- sqrt(h_se1^2+h_se2^2+2*cov_h[2,3]) 
    
    hc_r <- data.frame(
      se_adjustment="arellano",
      estimate1=estimate1,
      se1=hc_se1,
      variance1=hc_se1 ^ 2,
      test_stat1=estimate1 / hc_se1,
      p_value1=2 * pnorm(abs(estimate1 / hc_se1), lower.tail=FALSE),
      estimate2=estimate2,
      se2=hc_se2,
      variance2=hc_se2 ^ 2,
      test_stat2=estimate2 / hc_se2,
      p_value2=2 * pnorm(abs(estimate2 / hc_se2), lower.tail=FALSE),
      joint.eff=joint.eff,
      joint.eff.se=h_sej,
      variancej=h_sej ^ 2,
      test_statj=joint.eff / h_sej,
      joint.eff.pvalue=2 * pnorm(abs(joint.eff / h_sej), lower.tail=FALSE),
      stringsAsFactors=FALSE
    )
    r <- rbind(r, hc_r)
    rownames(r) <- NULL
  }
  
  return(r)
}


iter_results_concurrent_wjointeff.negbin.glm <- function(model_simulation) {
  m <- model_simulation$model_result
  
  coeffs <- as.data.frame(summary(m)$coefficients)
  coeffs$variable <- row.names(coeffs)
  rownames(coeffs) <- NULL
  
  coeffs1 <- coeffs[grepl("treatment1", coeffs$variable), ]
  coeffs2 <- coeffs[grepl("treatment2", coeffs$variable), ]
  
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
    test_stat1=coeffs1[["z value"]],
    p_value1=coeffs1[["Pr(>|z|)"]],
    estimate2=estimate2,
    se2=coeffs2[["Std. Error"]],
    variance2=coeffs2[["Std. Error"]] ^ 2,
    test_stat2=coeffs2[["z value"]],
    p_value2=coeffs2[["Pr(>|z|)"]],
    joint.eff=joint.eff,
    joint.eff.se=joint.eff.se,
    variancej=joint.eff.se ^ 2,
    test_statj=joint.eff / joint.eff.se,
    joint.eff.pvalue=joint.eff.pvalue,
    stringsAsFactors=FALSE
  )
  
  if ("huber" %in% model_simulation$models$se_adjust) {
    cov_h <- sandwich::vcovHC(m, type="HC0")
    h_se1 <- sqrt(diag(cov_h))[grepl("treatment1", names(diag(cov_h)))]
    h_se2 <- sqrt(diag(cov_h))[grepl("treatment2", names(diag(cov_h)))]
    h_sej <- sqrt(h_se1^2+h_se2^2+2*cov_h[2,3]) #need to generalize this better to cov between two treatment indicators
    
    h_r <- data.frame(
      se_adjustment="huber",
      estimate1=estimate1,
      se1=h_se1,
      variance1=h_se1 ^ 2,
      test_stat1=estimate1 / h_se1,
      p_value1=2 * pnorm(abs(estimate1 / h_se1), lower.tail=FALSE),
      estimate2=estimate2,
      se2=h_se2,
      variance2=h_se2 ^ 2,
      test_stat2=estimate2 / h_se1,
      p_value2=2 * pnorm(abs(estimate2 / h_se2), lower.tail=FALSE),
      joint.eff=joint.eff,
      joint.eff.se=h_sej,
      variancej=h_sej ^ 2,
      test_statj=joint.eff / h_sej,
      joint.eff.pvalue=2 * pnorm(abs(joint.eff / h_sej), lower.tail=FALSE),
      stringsAsFactors=FALSE
    )
    r <- rbind(r, h_r)
    rownames(r) <- NULL
  }
  
  if ("cluster" %in% model_simulation$models$se_adjust) {
    clust_indices <- as.numeric(rownames(m$model))
    clust_var <- as.character(model_simulation$data[[model_simulation$unit_var]][clust_indices])
    clust_coeffs <- cluster_adjust_se(m, clust_var)[[2]]
    clust_vcov <- cluster_adjust_se(m, clust_var)[[1]][2,3] #not 100% alginment with SEs from model so worried this is off
    class(clust_coeffs) <- c("coeftest", "matrix")
    clust_coeffs <- as.data.frame(clust_coeffs)
    clust_coeffs$variable <- row.names(clust_coeffs)
    rownames(clust_coeffs) <- NULL
    clust_coeffs1 <- clust_coeffs[grepl("treatment1", clust_coeffs$variable), ]
    clust_coeffs2 <- clust_coeffs[grepl("treatment2", clust_coeffs$variable), ]
    clust_jointse <- sqrt(clust_coeffs1[["Std. Error"]]^2+clust_coeffs2[["Std. Error"]]^2 + 2*clust_vcov)
    
    c_r <- data.frame(
      se_adjustment="cluster",
      estimate1=clust_coeffs1[["Estimate"]],
      se1=clust_coeffs1[["Std. Error"]],
      variance1=clust_coeffs1[["Std. Error"]] ^ 2,
      test_stat1=clust_coeffs1[["z value"]],
      p_value1=clust_coeffs1[["Pr(>|z|)"]],
      estimate2=clust_coeffs2[["Estimate"]],
      se2=clust_coeffs2[["Std. Error"]],
      variance2=clust_coeffs2[["Std. Error"]] ^ 2,
      test_stat2=clust_coeffs2[["z value"]],
      p_value2=clust_coeffs2[["Pr(>|z|)"]],
      joint.eff=joint.eff,
      joint.eff.se=clust_jointse,
      variancej=clust_jointse ^ 2,
      test_statj=joint.eff / clust_jointse,
      joint.eff.pvalue=2 * pnorm(abs(joint.eff / clust_jointse), lower.tail=FALSE),
      stringsAsFactors=FALSE
    )
    
    r <- rbind(r, c_r)
  }
  
  if ("arellano" %in% model_simulation$models$se_adjust) {
    clust_indices <- as.numeric(rownames(m$model))
    clust_var <- as.character(model_simulation$data[[model_simulation$unit_var]][clust_indices])
    cov_hc <- sandwich::vcovHC(m, type="HC1", cluster=clust_var, method="arellano")
    hc_se1 <- sqrt(diag(cov_hc))[grepl("treatment1", names(diag(cov_hc)))]
    hc_se2 <- sqrt(diag(cov_hc))[grepl("treatment2", names(diag(cov_hc)))]
    h_sej <- sqrt(h_se1^2+h_se2^2+2*cov_h[2,3]) 
    
    hc_r <- data.frame(
      se_adjustment="arellano",
      estimate1=estimate1,
      se1=hc_se1,
      variance1=hc_se1 ^ 2,
      test_stat1=estimate1 / hc_se1,
      p_value1=2 * pnorm(abs(estimate1 / hc_se1), lower.tail=FALSE),
      estimate2=estimate2,
      se2=hc_se2,
      variance2=hc_se2 ^ 2,
      test_stat2=estimate2 / hc_se2,
      p_value2=2 * pnorm(abs(estimate2 / hc_se2), lower.tail=FALSE),
      joint.eff=joint.eff,
      joint.eff.se=h_sej,
      variancej=h_sej ^ 2,
      test_statj=joint.eff / h_sej,
      joint.eff.pvalue=2 * pnorm(abs(joint.eff / h_sej), lower.tail=FALSE),
      stringsAsFactors=FALSE
    )
    r <- rbind(r, hc_r)
    rownames(r) <- NULL
  }
  
  return(r)
}

  

