#' iter results
#' 
#' @param m model object
#' @param single_simulation R6 class object for simulation config
iter_results <- function(m, single_simulation) {
  if (class(m)[1] == "lm") {
    iter_results.lm(m, single_simulation)
  } else if ("negbin" %in% class(m)) {
    iter_results.negbin.glm(m, single_simulation)
  }
}

#' Calculate mean squared error
#' 
#' @param x vector of errors (residuals)
#' 
#' @export
mse_resid <- function(x) {
  return(mean(x^2, na.rm=T))
}


iter_results.lm <- function(m, single_simulation) {
  coeffs <- as.data.frame(summary(m)$coefficients)
  coeffs$variable <- row.names(coeffs)
  rownames(coeffs) <- NULL
  
  treatment <- coeffs$variable[grepl("^treatment", coeffs$variable)][1]
  
  coeffs <- coeffs[coeffs$variable == treatment,]
  
  estimate <- coeffs[["Estimate"]]
  
  r <- data.frame(
    se_adjustment="none",
    estimate=estimate,
    se=coeffs[["Std. Error"]],
    variance=coeffs[["Std. Error"]] ^ 2,
    t_stat=coeffs[["t value"]],
    p_value=coeffs[["Pr(>|t|)"]],
    mse=mse_resid(m[["residuals"]]),
    stringsAsFactors=FALSE
  )
  
  if ("huber" %in% single_simulation$se_adjust) {
    cov_h <- sandwich::vcovHC(m, type="HC0")
    h_se <- sqrt(diag(cov_h))[names(diag(cov_h)) == treatment]
    
    h_r <- data.frame(
      se_adjustment="huber",
      estimate=estimate,
      se=h_se,
      variance=h_se ^ 2,
      t_stat=estimate / h_se,
      p_value=2 * pnorm(abs(estimate / h_se), lower.tail=FALSE),
      mse=mse_resid(m[["residuals"]]),
      stringsAsFactors=FALSE
    )
    r <- rbind(r, h_r)
    rownames(r) <- NULL
  }
  
  if ("cluster" %in% single_simulation$se_adjust) {
    clust_indices <- as.numeric(rownames(m$model))
    clust_var <- as.character(single_simulation$data[[single_simulation$unit_var]][clust_indices])
    clust_coeffs <- cluster_adjust_se(m, clust_var)[[2]]
    clust_vcov <- cluster_adjust_se(m, clust_var)[[1]][2,3] #not 100% alginment with SEs from model so worried this is off
    class(clust_coeffs) <- c("coeftest", "matrix")
    clust_coeffs <- as.data.frame(clust_coeffs)
    clust_coeffs$variable <- row.names(clust_coeffs)
    rownames(clust_coeffs) <- NULL
    clust_coeffs <- clust_coeffs[clust_coeffs$variable == treatment,]
    
    c_r <- data.frame(
      se_adjustment="cluster",
      estimate=clust_coeffs[["Estimate"]],
      se=clust_coeffs[["Std. Error"]],
      variance=clust_coeffs[["Std. Error"]] ^ 2,
      t_stat=clust_coeffs[["t value"]],
      p_value=clust_coeffs[["Pr(>|t|)"]],
      mse=mse_resid(m[["residuals"]]),
      stringsAsFactors=FALSE
    )
    
    r <- rbind(r, c_r)
  }
  
  if ("huber-cluster" %in% single_simulation$se_adjust) {
    clust_indices <- as.numeric(rownames(m$model))
    clust_var <- as.character(single_simulation$data[[single_simulation$unit_var]][clust_indices])
    cov_hc <- sandwich::vcovHC(m, type="HC1", cluster=clust_var, method="arellano")
    hc_se <- sqrt(diag(cov_hc))[names(diag(cov_hc)) == treatment]
    
    hc_r <- data.frame(
      se_adjustment="huber-cluster",
      estimate=estimate,
      se=hc_se,
      variance=hc_se ^ 2,
      t_stat=estimate / hc_se,
      p_value=2 * pnorm(abs(estimate / hc_se), lower.tail=FALSE),
      mse=mse_resid(m[["residuals"]]),
      stringsAsFactors=FALSE
    )
    r <- rbind(r, hc_r)
    rownames(r) <- NULL
  }
  
  return(r)
}

iter_results.negbin.glm <- function(m, single_simulation) {
  coeffs <- as.data.frame(summary(m)$coefficients)
  coeffs$variable <- row.names(coeffs)
  rownames(coeffs) <- NULL
  
  treatment <- coeffs$variable[grepl("^treatment", coeffs$variable)][1]
  
  coeffs <- coeffs[coeffs$variable == treatment,]
  
  estimate <- coeffs[["Estimate"]]
  
  r <- data.frame(
    se_adjustment="none",
    estimate=estimate,
    se=coeffs[["Std. Error"]],
    variance=coeffs[["Std. Error"]] ^ 2,
    z_stat=coeffs[["z value"]],
    p_value=coeffs[["Pr(>|z|)"]],
    mse=mse_resid(m[["residuals"]]),
    stringsAsFactors=FALSE
  )
  
  if ("huber" %in% single_simulation$se_adjust) {
    cov_h <- sandwich::vcovHC(m, type="HC0")
    h_se <- sqrt(diag(cov_h))[names(diag(cov_h))==treatment]
    
    h_r <- data.frame(
      se_adjustment="huber",
      estimate=estimate,
      se=h_se,
      variance=h_se ^ 2,
      z_stat=estimate / h_se,
      p_value=2 * pnorm(abs(estimate / h_se), lower.tail=FALSE),
      mse=mse_resid(m[["residuals"]]),
      stringsAsFactors=FALSE
    )
    r <- rbind(r, h_r)
    rownames(r) <- NULL
  }
  
  if ("cluster" %in% single_simulation$se_adjust) {
    clust_coeffs <- cluster_adjust_se(m, m$model$`as.factor(state)`)[[2]]
    class(clust_coeffs) <- c("coeftest", "matrix")
    clust_coeffs <- as.data.frame(clust_coeffs)
    clust_coeffs$variable <- row.names(clust_coeffs)
    rownames(clust_coeffs) <- NULL
    clust_coeffs <- clust_coeffs[clust_coeffs$variable == treatment,]
    
    c_r <- data.frame(
      se_adjustment="cluster",
      estimate=clust_coeffs[["Estimate"]],
      se=clust_coeffs[["Std. Error"]],
      variance=clust_coeffs[["Std. Error"]] ^ 2,
      z_stat=clust_coeffs[["z value"]],
      p_value=clust_coeffs[["Pr(>|z|)"]],
      mse=mse_resid(m[["residuals"]]),
      stringsAsFactors=FALSE
    )
    
    r <- rbind(r, c_r)
  }
  
  if ("huber-cluster" %in% single_simulation$se_adjust) {
    cov_hc <- sandwich::vcovHC(m, type="HC1", cluster="state", method="arellano")
    hc_se <- sqrt(diag(cov_hc))[names(diag(cov_hc)) == treatment]
    
    hc_r <- data.frame(
      se_adjustment="huber-cluster",
      estimate=estimate,
      se=hc_se,
      variance=hc_se ^ 2,
      z_stat=estimate / hc_se,
      p_value=2 * pnorm(abs(estimate / hc_se), lower.tail=FALSE),
      mse=mse_resid(m[["residuals"]]),
      stringsAsFactors=FALSE
    )
    r <- rbind(r, hc_r)
    rownames(r) <- NULL
  }
  
  return(r)
}
