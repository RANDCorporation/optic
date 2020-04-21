# 1. get meta information
# 2. get summary of min/max/mean distances if applicable

#==============================================================================
#==============================================================================
# EXAMPLE SETUP
#==============================================================================
#==============================================================================
results <- readRDS("data/autoregressive-runs-2020-04-20.rds")

meta_vars <- c(
  "model_call", "model_formula", "n_units", "true_effect", "effect_direction",
  "policy_speed", "change_code_treatment", "rho"
)

estimates_map1 <- list(
  treatment1 = c("estimate1", "se1", "variance1", "t_stat1", "p_value1"),
  treatment2 = c("estimate2", "se2", "variance2", "t_stat2", "p_value2"),
  joint_effect = c("joint.eff", "joint.eff.se", "variancej", "t_statj", "joint.eff.pvalue"),
  labels = c("estimate", "se", "variance", "t_stat", "p_value")
)

te_map1 <- list(
  treatment1 = -0.457936376121782,
  treatment2 = -0.457936376121782,
  joint_effect = -0.9158728
)

estimates_map2 <- list(
  treatment1 = c("estimate", "se", "variance", "t_stat", "p_value"),
  labels = c("estimate", "se", "variance", "t_stat", "p_value")
)

te_map2 <- list(
  treatment1 = -0.457936376121782
)

# for now, will need to update
for (i in 1:length(results)) {
  names(results[[i]]) <- gsub("z_stat", "t_stat", names(results[[i]]))
}

# get results first time, use ID to name element
r_list <- list()
for (i in 1:length(results)) {
  if ( grepl("treatment2", unique(results[[i]]$model_formula)) ) {
    s <- summarize_results(
      x=results[[i]],
      meta_vars=meta_vars,
      estimates_map=estimates_map1,
      te_map=te_map1,
      grouping_vars=c("se_adjustment", "iter"),
      cf=NULL
    )
  } else {
    s <- summarize_results(
      x=results[[i]],
      meta_vars=meta_vars,
      estimates_map=estimates_map2,
      te_map=te_map2,
      grouping_vars=c("se_adjustment", "iter"),
      cf=NULL
    )
  }
  
  r_list[[s$id]] <- s$summary
  print(i)
}

# now do again, but use the correction factors calculated the first time through
final_list <- list()
for (i in 1:length(results)) {
  meta_list <- list()
  for (m in meta_vars) {
    meta_list[[m]] <- unique(results[[i]][[m]])[1]
  }
  id <- paste(unlist(meta_list), collapse="-")
  
  if ( meta_list$effect_direction == "null" ) {
    final_list[[id]] <- r_list[[id]]
  } else {
    # get cf from corresponding null run
    cf <- r_list[[gsub("-neg-", "-null-", id)]][, c("coefficient", "se_adjustment", "cf")]
    if ( grepl("treatment2", unique(results[[i]]$model_formula)) ) {
      s <- summarize_results(
        x=results[[i]],
        meta_vars=meta_vars,
        estimates_map=estimates_map1,
        te_map=te_map1,
        grouping_vars=c("se_adjustment", "iter"),
        cf=cf
      )
    } else {
      s <- summarize_results(
        x=results[[i]],
        meta_vars=meta_vars,
        estimates_map=estimates_map2,
        te_map=te_map2,
        grouping_vars=c("se_adjustment", "iter"),
        cf=cf
      )
    }
    
    final_list[[s$id]] <- s$summary
  }
  print(i)
}

one_obj_results <- do.call(dplyr::bind_rows, final_list)
#==============================================================================
#==============================================================================
# SUMMARY METHODS
#==============================================================================
#==============================================================================
#' get data tidy for summarizing
clean_data <- function(x, map, te_map=NULL, grouping_vars=NULL) {
  if ( var(sapply(map, length)) != 0 | !"labels" %in% names(map) ) {
    stop("all map elements must have the same length and one must be named 'labels'")
  }
  
  groups_ <- names(map)[names(map) != "labels"]
  r <- data.frame()
  for (g in groups_) {
    tmp <- x[, map[[g]]]
    names(tmp) <- map[["labels"]]
    if (!is.null(te_map)) {
      tmp$te <- te_map[[g]]
    }
    if (!is.null(grouping_vars)) {
      tmp <- cbind(tmp, x[, grouping_vars, drop=FALSE])
    }
    
    tmp$coefficient <- g
    r <- rbind(r, tmp)
    rm(tmp)
  }
  
  return(r)
}

#' helper for mean squared error
mse <- function(x, te) {
  (x - te)^2
}

#' helper for bias
bias <- function(x, te) {
  (x - te)
}

#' create binary indicator for significance of p-value based on given level
#' 
#' @param p vector of p-values
#' @param level threshold for significance; default is 0.05
pval_flag <- function(p, level=0.05) {
  p[p < level] <- 1
  p[p != 1] <- 0
  return(p)
}

#' @import dplyr
summary_means <- function(x, summarize_vars, grouping_vars=NULL) {
  summarize_vars <- summarize_vars[summarize_vars %in% names(x)]
  if (is.null(grouping_vars)) {
    s <- x %>% 
      dplyr::summarize_at(summarize_vars, mean, na.rm=TRUE)
  } else {
    grouping_vars <- dplyr::syms(grouping_vars)
    s <- x %>%
      dplyr::group_by(!!!grouping_vars) %>%
      dplyr::summarize_at(summarize_vars, mean, na.rm=TRUE) %>%
      dplyr::ungroup()
  }
  
  return(s)
}

#' calculate the correction factor
#' 
#' @param test_stats test statistics
#' @param type what type of test statistic is being supplied; one of "t", "wald"; default is "t"
correction_factor <- function(test_stats, type="t") {
  if (type == "t") {
    f_stats <- (test_stats)^2
    f_stats <- sort(f_stats)
    high_cut <- 0.95 * length(test_stats)
    femp95 <- f_stats[high_cut]
    freal <- qf(0.95, 1, Inf)
    corr_factor <- sqrt(femp95 / freal)
  }
  
  return(corr_factor)
}

#' formula for correcting p-values using correction factor
#' 
#' @param coeffs vector of regression coefficients
#' @param ses vector of standard errors related to provided coefficients
#' @param cf correction factor to use for adjustment
#' @param effect_direction direction of true effect, one of "null", "neg", "pos"
correct_rejection_rate_flag <- function(coeffs, ses, cf, effect_direction="null") {
  adj_ses <- ses * cf
  low95 <- coeffs - 1.96 * adj_ses
  high95 <- coeffs + 1.96 * adj_ses
  
  if (effect_direction == "null") {
    # 1 if confidence interval contains 0
    sig_dummy <- as.integer((low95 < 0 & high95 > 0))
  } else if (effect_direction == "pos") {
    # 1 if confidence interval does not contain 0
    sig_dummy <- as.integer((low95 < 0 & high95 > 0) == FALSE)
    # if significant but in the wrong direction, set to 0
    sig_dummy[sig_dummy == 1 & coeffs < 0] <- 0
  } else if (effect_direction == "neg") {
    # 1 if confidence interval does not contain 0
    sig_dummy <- as.integer((low95 < 0 & high95 > 0) == FALSE)
    # if significant but in the wrong direction, set to 0
    sig_dummy[sig_dummy == 1 & coeffs > 0] <- 0
  }
  
  return(sig_dummy)
}


#' Calculate type S error - how often model gets direction of effect wrong
#' 
#' number of significant betas of certain direction divided by all significant betas
#' 
#' @param betas vector of regression coefficients
#' @param pvals vector of p values for betas
#' @param effect_direction direction of true effect
type_s_error <- function(betas, pvals, effect_direction) {
  if (effect_direction == "null") {
    return(NA)
  }
  if (length(betas[pvals < 0.05]) != 0) {
    if (effect_direction == "neg") {
      s_error <- length(betas[betas > 0 & pvals < 0.05]) / length(betas[pvals < 0.05])
    } else {
      s_error <- length(betas[betas < 0 & pvals < 0.05]) / length(betas[pvals < 0.05])
    }
  }else{
    s_error <- 0
  }
  return(s_error)
}



#' @import dplyr
summarize_results <- function(x, meta_vars, estimates_map, te_map=NULL, grouping_vars=NULL, cf=NULL) {
  
  # get the meta values, should only be one unique but defaulting to first element
  meta_list <- list()
  for (m in meta_vars) {
    meta_list[[m]] <- unique(x[[m]])[1]
  }
  
  if ( sum(grepl("_distance", names(x))) > 0 ) {
    # get the values for each iter
    dist <- x %>%
      dplyr::select(min_distance, max_distance, mean_distance, iter) %>%
      dplyr::distinct(.)
    
    # add to meta information
    meta_list[["avg_min_distance"]] <- mean(dist$min_distance, na.rm=TRUE)
    meta_list[["avg_max_distance"]] <- mean(dist$max_distance, na.rm=TRUE)
    meta_list[["avg_mean_distance"]] <- mean(dist$mean_distance, na.rm=TRUE)
  }
  
  estimates <- clean_data(x, estimates_map, te_map, grouping_vars)
  
  # if null, save the correction factors
  if (meta_list$effect_direction == "null") {
    cf <- estimates %>%
      dplyr::group_by(coefficient, se_adjustment) %>%
      dplyr::summarize(cf=correction_factor(t_stat))
  }
  
  # add in correction factors and correct rejection rate if cf provided or null run
  if (!is.null(cf)) {
    estimates <- dplyr::left_join(estimates, cf, by=c("coefficient", "se_adjustment"))
    
    crr_flagVec <- Vectorize(correct_rejection_rate_flag,
                        vectorize.args = c("coeffs", "ses", "cf"))
    
    estimates$crr <- crr_flagVec(estimates$estimate, estimates$se, estimates$cf, effect_direction = meta_list$effect_direction)
  }
  
  # add in bias and MSE if true effect included
  if ( "te" %in% names(estimates) ) {
    biasVec <- Vectorize(bias)
    mseVec <- Vectorize(mse)
    estimates$bias <- biasVec(estimates$estimate, estimates$te)
    estimates$mse <- mseVec(estimates$estimate, estimates$te)
  }
  
  # type I error flag for null runs
  if ( meta_list$effect_direction == "null" ) {
    estimates$type1_error <- pval_flag(estimates$p_value)
  }
  
  # get means for the main summary statistics
  all_stats <- summary_means(
    estimates,
    c("estimate", "se", "variance", "t_stat", "p_value", "bias", "mse", "crr", "type1_error"),
    c("coefficient", "se_adjustment")
  )
  
  # add in correction factors used
  if (!is.null(cf)) {
    all_stats <- dplyr::left_join(all_stats, cf, by=c("coefficient", "se_adjustment"))
  }
  
  # type S error for non null runs
  if ( meta_list$effect_direction != "null" ) {
    type_s <- estimates %>%
      dplyr::group_by(coefficient, se_adjustment) %>%
      dplyr::group_modify(~as.data.frame(type_s_error(
        .x$estimate,
        .x$p_value,
        meta_list$effect_direction
      )))
    names(type_s)[3] <- "type_s_error"
    
    all_stats <- dplyr::left_join(all_stats, type_s, by=c("coefficient", "se_adjustment"))
  }
  
  # add in meta information
  for ( m in names(meta_list) ) {
    all_stats[[m]] <- meta_list[[m]]
  }
  
  # construct id for run
  dist_ind <- grep("distance", names(meta_list))
  if ( length(dist_ind) > 0 ) {
    id_list <- meta_list[-dist_ind]
  } else {
    id_list <- meta_list
  }
  id <- paste(unlist(id_list), collapse="-")
  
  return(list(summary=all_stats, id=id))
}
