

#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

#######################
### SAMPLING METHOD ###
#######################

#' Perform sampling and coding of treatment effect for no confounding policy simulations
#' 
#' @description Simulates treatment status across units by time (e.g., states, counties, schools), generating treatment times for a single policies without any confounding.
#'
#' @param single_simulation An object created from OpticSim$setup_single_simulation(), which specifies simulation data, number of units, unit variable & time variable, etc.
#' 
#' @noRd
noconf_sample <- function(single_simulation) {
  ##########################################
  ### PULL DATA AND PARAMETERS/VARIABLES ###
  ##########################################
  x <- single_simulation$data
  pc <- single_simulation$prior_control
  
  if (single_simulation$prior_control == "level") {
    x$prior_control <- x$prior_control_level_OLD
    x$prior_control_old <- x$prior_control_level_OLD
  } else if (single_simulation$prior_control == "trend") {
    x$prior_control <- x$prior_control_trend_OLD
    x$prior_control_old <- x$prior_control_trend_OLD
  } else {
    stop("invalid prior control option, must be either 'level' or 'trend'")
  }
  
  n <- single_simulation$n_units
  unit_var <- single_simulation$unit_var
  treat_var <- single_simulation$treat_var
  time_var <- single_simulation$time_var
  policy_speed <- single_simulation$policy_speed
  n_implementation_periods <- single_simulation$n_implementation_periods
  effect_magnitude <- single_simulation$effect_magnitude
  effect_direction <- single_simulation$effect_direction
  model_type = names(single_simulation$models)
  
  # If there is at least one did model
  if(!any(model_type == "drdid")){
    outcomes <- unique(sapply(single_simulation$models, function(x) { optic::model_terms(x[["model_formula"]])[["lhs"]] }))
  }else{
    outcomes <- as.character(single_simulation$models$drdid$model_args$yname)
  }
  ###############################################
  ### IDENTIFY TREATED UNITS AND TIME PERIODS ###
  ###############################################
  ## get year 
  atp <- sort(unique(x[[time_var]]))[-1:-3]
  # take out two post-periods
  atp <- atp[-length(atp):-(length(atp)-1)]
  available_periods <- atp
  
  ## sample treated units
  available_units <- unique(x[[treat_var]])
  n_avail_units <- length(available_units)
  sampled_units <- sample(1:n_avail_units, n, replace=FALSE)
  sampled_units <- available_units[sampled_units]
  trt_years = sample(atp, n, replace = TRUE)
  trt_dta = data.frame(matrix(nrow=n, ncol=2))
  trt_dta = trt_dta %>%
    dplyr::mutate(!!treat_var := sampled_units,
                  !!time_var := trt_years) %>%
    dplyr::select(-c(X1, X2)) %>%
    dplyr::mutate(trt_ind = 1)
  
  x = x %>%
    dplyr::left_join(., trt_dta, by=c(treat_var, time_var)) %>%
    dplyr::arrange(!!as.name(treat_var), !!as.name(time_var)) %>%
    dplyr::group_by(!!as.name(treat_var)) %>%
    dplyr::mutate(isfirst = as.numeric(trt_ind == 1 & !duplicated(trt_ind==1))) %>%
    dplyr::mutate(trt_ind = cumsum(tidyr::replace_na(isfirst,0)))
  
  # Get time_periods and n_treated
  # (1) time periods
  the_treated = x %>%
    filter(isfirst==1) %>%
    dplyr::select(!!as.name(treat_var), !!as.name(time_var)) 
  time_periods = x %>% 
    filter(!!as.name(time_var) == min(!!as.name(time_var))) %>%
    dplyr::select(!!as.name(treat_var)) %>%
    left_join(., the_treated, by = c(treat_var)) %>%
    distinct
  time_periods = time_periods$year
  time_periods[is.na(time_periods)] = Inf
  # (2) n_treated
  n_treated = x %>%
    group_by(!!as.name(treat_var)) %>%
    summarize(trt_ind_sum = sum(trt_ind), .groups='drop')
  n_treated = n_treated$trt_ind_sum
  
  sampled_units <- available_units[n_treated > 0]
  start_periods <- time_periods[n_treated > 0]
  
  treated <- list()
  for (i in 1:length(sampled_units)) { 
    yr <- start_periods[i]
    current_unit <- as.character(sampled_units[i])
    # change if don't want everything to be years/months
    mo <- sample(1:12, 1)
    if (policy_speed == "slow") {
      treated[[current_unit]] <- list(
        policy_years = yr:max(x$year, na.rm=TRUE),
        policy_month = mo,
        exposure = optic::calculate_exposure(mo, n_implementation_periods),
        policy_date = as.Date(paste0(yr, '-', mo, '-01'))
      )
      
      n <- length(treated[[current_unit]][["policy_years"]])
      exposure <- treated[[current_unit]][["exposure"]]
      if (n < length(exposure)) {
        treated[[current_unit]][["exposure"]] <- exposure[1:n]
      } else {
        n_more_years <- n - length(exposure)
        treated[[current_unit]][["exposure"]] <- c(exposure, rep(1, n_more_years))
      }
    } else if (policy_speed == "instant") {
      treated[[current_unit]] <- list(
        policy_years = yr:max(x$year, na.rm=TRUE),
        policy_month = mo,
        exposure = c((12 - mo + 1)/12, rep(1, length(yr:max(x[[time_var]]))-1)),
        policy_date = as.Date(paste0(yr, '-', mo, '-01'))
      )
    }
  }
  # apply treatment to data updated to make quicker
  # Get policy date and exposure in proper format quickly...
  policy_dates = purrr::transpose(treated)$policy_date %>% 
    dplyr::bind_rows() %>% 
    tidyr::gather(!!treat_var, treatment_date) 
  
  if(is.factor(x[[treat_var]])){
    policy_dates = policy_dates %>%
      dplyr::mutate_if(is.character, factor)
  } else{
    policy_dates = policy_dates %>%
      dplyr::mutate_if(is.character, as.double)
  }
  
  x_policy_info = x %>%
    dplyr::filter(trt_ind == 1) %>%
    dplyr::select(!!as.name(treat_var), !!as.name(time_var)) %>%
    distinct
  x_policy_info$treatment = purrr::transpose(treated)$exposure %>% unlist
  
  x_policy_info = x_policy_info %>%
    dplyr::left_join(., policy_dates, by = treat_var)
  
  # apply treatment to data
  x = x %>%
    dplyr::left_join(., x_policy_info, by = c(treat_var, time_var)) %>%
    dplyr::mutate(treatment = case_when(is.na(treatment) ~ 0,
                                        TRUE ~ treatment),
                  treatment_date = case_when(is.na(treatment_date) ~ as.Date(NA),
                                             TRUE ~ treatment_date))
  
  # create level and change code versions of treatment variable
  x$treatment_level <- x$treatment
  
  # add in change code version of treatment variable
  unit_sym <- dplyr::sym(unit_var)
  treat_sym <- dplyr::sym(treat_var)
  time_sym <- dplyr::sym(time_var)
  
  x <- x %>%
    dplyr::arrange(!!unit_sym, !!time_sym) %>%
    dplyr::group_by(!!unit_sym) %>%
    dplyr::mutate(temp_lag = dplyr::lag(treatment, n=1L)) %>%
    dplyr::mutate(treatment_change = treatment - temp_lag) %>%
    dplyr::ungroup() %>%
    dplyr::select(-temp_lag)
  
  # add and update objects to single sim list
  single_simulation$treated_units <- treated
  single_simulation$data <- x
  
  return(single_simulation)
}


########################
### PRE MODEL METHOD ###
########################

#' Functions to apply to simulated treatment effect prior to simulations.
#' 
#' @description Depending on the exact estimator used, an analyst may wish to apply a functional form to the treatment effect prior to modeling
#'     (i.e., estimate effect as linear, logged, etc). Can also apply additional pre-modeling steps to each simulate dataset as required. All 
#'     steps in this function are applied prior to sampling treatment effects.
#' 
#' @param model_simulation An object created from OpticModel, which specifies simulation settings such as model formulas, model call, etc
#' 
#' @noRd
noconf_premodel <- function(model_simulation) {
  ##########################################
  ### PULL DATA AND PARAMETERS/VARIABLES ###
  ##########################################
  
  x <- model_simulation$data
  model <- model_simulation$models
  model_call <- model$model_call
  
  outcome <- optic::model_terms(model$model_formula)[["lhs"]]
  oo <- dplyr::sym(outcome)
  model_type <- model$type
  balance_statistics <- NULL
  
  unit_sym <- dplyr::sym(model_simulation$unit_var)
  time_sym <- dplyr::sym(model_simulation$time_var)
  
  ##############################
  ### APPLY TREATMENT EFFECT ###
  ##############################
  # custom method that works for concurrent and regular single policy eval
  x <- apply_treatment_effect(
    x=x,
    model_formula=model$model_formula,
    model_call=model$model_call,
    te=c(model_simulation$effect_magnitude),
    effect_direction=model_simulation$effect_direction,
    concurrent=FALSE
  )
  
  # PNL note
  # this implementation does not seem to use the lagged crude rate
  
  # if autoregressive, need to add lag for crude rate
  # when outcome is deaths, derive new crude rate from modified outcome
  if (model_type == "autoreg") {
    
    x <- x %>%
      dplyr::arrange(!!unit_sym, !!time_sym) %>%
      dplyr::group_by(!!unit_sym) %>%
      dplyr::mutate(lag_outcome = dplyr::lag(!!oo, n=1L)) %>%
      dplyr::ungroup()
    
    if(model$model_call == "feols"){
      formula_components <- as.character(model_simulation$models$model_formula)
      updated_3 <- strsplit(formula_components[3], " | ", fixed=TRUE)
      if(length(updated_3[[1]]==1)){
        new_fmla <- as.formula(paste(formula_components[2], formula_components[1], updated_3[[1]][1], "+ lag_outcome"))
      }else{
        new_fmla <- as.formula(paste(formula_components[2], formula_components[1], updated_3[[1]][1], "+ lag_outcome |", updated_3[[1]][2]))
      }
      model_simulation$models$model_formula <- new_fmla
    }else{
      model_simulation$models$model_formula <- update.formula(model_simulation$models$model_formula, ~ . + lag_outcome)
    }
  } else if (model_type == "multisynth") {
    x$treatment[x$treatment > 0] <- 1
    x$treatment_level[x$treatment_level > 0] <- 1
    x <- x %>%
      filter(!is.na(!!oo))
    if (sum(is.na(x[[outcome]])) > 0) {
      stop("multisynth method cannot handle missingness in outcome.")
    }
  } else if (model_type == "drdid") {
    x$treatment[x$treatment > 0] <- 1
    x$treatment_level[x$treatment_level > 0] <- 1
  }
  
  # get balance information
  bal_stats <- x %>%
    group_by(year) %>%
    summarize(
      n_trt = sum(treatment > 0),
      mu1_prior = mean(prior_control[treatment > 0]),
      mu0_prior = mean(prior_control[treatment == 0]),
      sd_prior = sd(prior_control),
      mu1 = mean((!!oo)[treatment > 0]),
      mu0 = mean((!!oo)[treatment == 0]),
      sd = sd(!!oo),
      .groups="keep"
    ) %>%
    mutate(
      es_prior = (mu1_prior - mu0_prior) / sd_prior,
      es = (mu1 - mu0) / sd
    ) %>%
    ungroup() %>%
    summarize(n = max(n_trt, na.rm=TRUE),
              mean_es_prior = mean(es_prior, na.rm=TRUE),
              max_es_prior = max(abs(es_prior), na.rm=TRUE),
              mean_es_outcome = mean(es, na.rm=TRUE),
              max_es_outcome = max(abs(es), na.rm=TRUE),
              .groups="drop"
    ) %>%
    mutate(outcome = outcome,
           n_unique_enact_years = length(unique(sapply(model_simulation$treated_units, function(x) {min(x[["policy_years"]])}))))
  
  bal_stats2 = x %>%
    summarize(mu1_prior = mean(prior_control[treatment > 0], na.rm=T),
              mu0_prior = mean(prior_control[treatment == 0], na.rm=T),
              sd_prior = sd(prior_control, na.rm=T),
              mu1_prior_old = mean(prior_control_old[treatment > 0], na.rm=T),
              mu0_prior_old = mean(prior_control_old[treatment == 0], na.rm=T),
              sd_prior_old = sd(prior_control_old, na.rm=T),
              mu1 = mean((!!oo)[treatment > 0], na.rm=T),
              mu0 = mean((!!oo)[treatment == 0], na.rm=T),
              sd = sd(!!oo, na.rm=T))
  bal_stats = bind_cols(bal_stats, bal_stats2)
  
  # Add additional columns required by CSA method, if chosen:
  if (model_call == "att_gt"){
    x <- x %>% 
      group_by(!!unit_sym) %>%
      mutate(treatment_date = ifelse(max(trt_ind == 1), 1 + max(year ^ (1-treatment)), 0)) %>% 
      ungroup() %>%
      mutate(!!unit_sym := as.numeric(as.factor(!!unit_sym)))
  }
  
  model_simulation$balance_statistics <- bal_stats
  model_simulation$data <- x
  
  return(model_simulation)
  
}


####################
### MODEL METHOD ###
####################
#' Runs a given model simulation and stores resuls
#' 
#' @description Runs the model against the prepared simulation data along with
#'     any provided arguments. Stores the model object in the input
#'     list, generating a new element named "model_result" and returns full list
#'
#' @param model_simulation An object created from OpticModel, which specifies simulation settings such as model formulas, model call, etc
#' @noRd
noconf_model <- function(model_simulation) {
  
  model <- model_simulation$models
  model_type <- model_simulation$models$type
  addtl_args <- model$model_args
  
  x <- model_simulation$data
  
  # The only consistent argument across these
  # models is "data". We need to also pass along user provided arguments
  # and set some arguments based off the specific model type 
  
  args <- list(data=x)
  
  if (length(addtl_args) >= 1){
    args <- append(args, addtl_args)
  }
  
  # Change names of provided arguments to meet the needs of respective packages
  if (model_type == "eventstudy"|model_type == "autoreg"){
    args[['fml']] <- model$model_formula
  }else if (model_type == "multisynth"){
    args[['form']] <-  model$model_formula
  }else if (model_type == "did2s"){
    args[['treatment']] <- 'treatment'
  }
  
  m <- do.call(
    model[["model_call"]],
    args
  )
  
  model_simulation$model_result <- m
  
  return(model_simulation)
}


#########################
### POST_MODEL METHOD ###
#########################
#' Post process results from a simulation model(
#' 
#' @description Summarizes the statistical performance of a model by computing summary information on the model fit, estimated effects and standard errors 
#' 
#' @param model_simulation An object created from OpticModel, which specifies simulation settings such as model formulas, model call, etc
#' @importFrom stats resid
#' @noRd
noconf_postmodel <- function(model_simulation) {
  
  outcome <- optic::model_terms(model_simulation$models[["model_formula"]])[["lhs"]]
  # get run metadata to merge in after
  meta_data <- data.frame(
    model_name = model_simulation$models$name,
    model_call = model_simulation$models[["model_call"]],
    outcome = outcome,
    model_formula = Reduce(paste, trimws(deparse(model_simulation$models[["model_formula"]]))),
    policy_speed = model_simulation$policy_speed,
    n_implementation_periods = model_simulation$n_implementation_periods,
    prior_control = model_simulation$prior_control,
    effect_magnitude=model_simulation$effect_magnitude,
    n_units=model_simulation$n_units,
    effect_direction=model_simulation$effect_direction  
  )
  
  # get model result information and apply standard error adjustments
  if (model_simulation$models[["type"]] == "multisynth") {
    
    m <- model_simulation$model_result
    cf <- summary(m)
    cf <- cf$att
    estimate <- cf[cf$Level == "Average" & is.na(cf$Time), "Estimate"]
    se <- cf[cf$Level == "Average" & is.na(cf$Time), "Std.Error"]
    variance <- se ^ 2
    t_stat <- NA
    p_value <- 2 * pnorm(abs(estimate/se), lower.tail = FALSE)
    mse <- mean(unlist(lapply(m$residuals, function(x) {mean(x^2)})))
    
    results <- data.frame(
      outcome=outcome,
      se_adjustment="none",
      estimate=estimate,
      se=se,
      variance=variance,
      t_stat=NA,
      p_value=p_value,
      mse=mse,
      stringsAsFactors=FALSE
    )
    
    
    
  } else if (model_simulation$models[["model_call"]] == "att_gt"){
    
    m <- model_simulation$model_result
    m_agg <- did::aggte(m, type='dynamic', na.rm=T)
    
    cf <- data.frame("estimate" = m_agg$overall.att,
                     "se" = m_agg$overall.se)
    
    estimate <- cf$estimate
    se <- cf$se
    variance <- se ^ 2
    t_stat <- NA
    p_value <- 2 * pnorm(abs(estimate/se), lower.tail = FALSE)
    
    results <- data.frame(
      outcome=outcome,
      se_adjustment="none",
      estimate=estimate,
      se=se,
      variance=variance,
      t_stat=NA,
      p_value=p_value,
      mse = NA,
      stringsAsFactors=FALSE
    )

  } else{
    m <- model_simulation$model_result
    
    if(model_simulation$models$model_call=="feols"){
      cf <- as.data.frame(summary(m)$coeftable)
    } else{
      cf <- as.data.frame(summary(m)$coefficients)
    }
    
    cf$variable <- row.names(cf)
    rownames(cf) <- NULL
    
    treatment <- cf$variable[grepl("^treatment", cf$variable)][1]
    
    cf <- cf[cf$variable == treatment, ]
    estimate <- cf[["Estimate"]]
    
    if(model_simulation$models$model_call == 'lmer'){
      
      # For ME model, below should use Satterthwaite approximation or 
      # Kenward-Roger approximation (which would produce more conservative
      # p-values than assuming t dist is, more or less, converging normal
      # given sample size (See ex. Barr et al., 2013))
      
      pval <- 1.96*(1 - pnorm(abs(cf[["t value"]])))
    }else{
      pval <- c(cf[["Pr(>|t|)"]], cf[["Pr(>|z|)"]])
    }
    
    results <- data.frame(
      outcome=outcome,
      se_adjustment="none",
      estimate=estimate,
      se=cf[["Std. Error"]],
      variance=cf[["Std. Error"]] ^ 2,
      t_stat=c(cf[["t value"]], cf[["z value"]]),
      p_value=pval,
      mse=mean(resid(m)^2, na.rm=T),
      stringsAsFactors=FALSE
    )
  }
  
  if(model_simulation$models$model_call=="feols"){
    model <- model_simulation$models
    x <- model_simulation$data
    if("cluster-unit" %in% model$se_adjust){
      fml = model[["model_formula"]]
      my_weights <- model[["model_args"]]$weights
      m_new <- feols(fml = fml, data = x, weights = my_weights, cluster = model_simulation$unit_var, nthreads=4, notes=FALSE)
      cf <- as.data.frame(summary(m_new)$coeftable)
      cf$variable <- row.names(cf)
      rownames(cf) <- NULL
      treatment <- cf$variable[grepl("^treatment", cf$variable)][1]
      cf <- cf[cf$variable == treatment, ]
      estimate <- cf[["Estimate"]]
      cluster_unit_results <- data.frame(
        outcome=outcome,
        se_adjustment="cluster-unit",
        estimate=estimate,
        se=cf[["Std. Error"]],
        variance=cf[["Std. Error"]] ^ 2,
        t_stat=c(cf[["t value"]], cf[["z value"]]),
        p_value=c(cf[["Pr(>|t|)"]], cf[["Pr(>|z|)"]]),
        mse=mean(m_new[["residuals"]]^2, na.rm=T),
        stringsAsFactors=FALSE
      )
      results <- rbind(results, cluster_unit_results)
      rownames(results) <- NULL
    }
    if("cluster-treat" %in% model$se_adjust){
      fml = model[["model_formula"]]
      my_weights <- model[["model_args"]]$weights
      m_new <- feols(fml = fml, data = x, weights = my_weights, cluster = model_simulation$treat_var, nthreads=4, notes=FALSE)
      cf <- as.data.frame(summary(m_new)$coeftable)
      cf$variable <- row.names(cf)
      rownames(cf) <- NULL
      treatment <- cf$variable[grepl("^treatment", cf$variable)][1]
      cf <- cf[cf$variable == treatment, ]
      estimate <- cf[["Estimate"]]
      cluster_treat_results <- data.frame(
        outcome=outcome,
        se_adjustment="cluster-treat",
        estimate=estimate,
        se=cf[["Std. Error"]],
        variance=cf[["Std. Error"]] ^ 2,
        t_stat=c(cf[["t value"]], cf[["z value"]]),
        p_value=c(cf[["Pr(>|t|)"]], cf[["Pr(>|z|)"]]),
        mse=mean(m_new[["residuals"]]^2, na.rm=T),
        stringsAsFactors=FALSE
      )
      results <- rbind(results, cluster_treat_results)
      rownames(results) <- NULL
    }
    
  }else{
    if ("huber" %in% model_simulation$models[["se_adjust"]]) {
      cov_h <- sandwich::vcovHC(m, type="HC0")
      h_se <- sqrt(diag(cov_h))[names(diag(cov_h)) == treatment]
      
      h_r <- data.frame(
        outcome=outcome,
        se_adjustment="huber",
        estimate=estimate,
        se=h_se,
        variance=h_se ^ 2,
        t_stat=estimate / h_se,
        p_value=2 * pnorm(abs(estimate / h_se), lower.tail=FALSE),
        mse=mean(resid(m)^2, na.rm=T),
        stringsAsFactors=FALSE
      )
      results <- rbind(results, h_r)
      rownames(results) <- NULL
    }
    
    if ("cluster-treat" %in% model_simulation$models[["se_adjust"]]) {
      clust_indices <- as.numeric(rownames(m$model))
      clust_var <- as.character(model_simulation$data[[model_simulation$treat_var]][clust_indices])
      cluster_adjust_se_res <- cluster_adjust_se(m, clust_var)
      clust_coeffs <- cluster_adjust_se_res[[2]]
      clust_vcov <- cluster_adjust_se_res[[1]][2,3] #not 100% alginment with SEs from model so worried this is off
      class(clust_coeffs) <- c("coeftest", "matrix")
      clust_coeffs <- as.data.frame(clust_coeffs)
      clust_coeffs$variable <- row.names(clust_coeffs)
      rownames(clust_coeffs) <- NULL
      clust_coeffs <- clust_coeffs[clust_coeffs$variable == treatment,]
      
      c_r <- data.frame(
        outcome=outcome,
        se_adjustment="cluster-treat",
        estimate=clust_coeffs[["Estimate"]],
        se=clust_coeffs[["Std. Error"]],
        variance=clust_coeffs[["Std. Error"]] ^ 2,
        t_stat=c(clust_coeffs[["z value"]], clust_coeffs[["t value"]]),
        p_value=c(clust_coeffs[["Pr(>|z|)"]], clust_coeffs[["Pr(>|t|)"]]),
        mse=mean(resid(m)^2, na.rm=T),
        stringsAsFactors=FALSE
      )
      
      results <- rbind(results, c_r)
      rownames(results) <- NULL
    }
    
    if ("cluster-unit" %in% model_simulation$models[["se_adjust"]]) {
      clust_indices <- as.numeric(rownames(m$model))
      clust_var <- as.character(model_simulation$data[[model_simulation$unit_var]][clust_indices])
      cluster_adjust_se_res <- cluster_adjust_se(m, clust_var)
      clust_coeffs <- cluster_adjust_se_res[[2]]
      clust_vcov <- cluster_adjust_se_res[[1]][2,3] #not 100% alignment with SEs from model so worried this is off
      class(clust_coeffs) <- c("coeftest", "matrix")
      clust_coeffs <- as.data.frame(clust_coeffs)
      clust_coeffs$variable <- row.names(clust_coeffs)
      rownames(clust_coeffs) <- NULL
      clust_coeffs <- clust_coeffs[clust_coeffs$variable == treatment,]
      
      c_r_unit <- data.frame(
        outcome=outcome,
        se_adjustment="cluster-unit",
        estimate=clust_coeffs[["Estimate"]],
        se=clust_coeffs[["Std. Error"]],
        variance=clust_coeffs[["Std. Error"]] ^ 2,
        t_stat=c(clust_coeffs[["z value"]], clust_coeffs[["t value"]]),
        p_value=c(clust_coeffs[["Pr(>|z|)"]], clust_coeffs[["Pr(>|t|)"]]),
        mse=mean(resid(m)^2, na.rm=T),
        stringsAsFactors=FALSE
      )
      
      results <- rbind(results, c_r_unit)
      rownames(results) <- NULL
    }
    
    if ("arellano" %in% model_simulation$models[["se_adjust"]]) {
      clust_indices <- as.numeric(rownames(m$model))
      clust_var <- as.character(model_simulation$data[[model_simulation$treat_var]][clust_indices])
      cov_hc <- sandwich::vcovHC(m, type="HC1", cluster=clust_var, method="arellano")
      hc_se <- sqrt(diag(cov_hc))[names(diag(cov_hc)) == treatment]
      
      hc_r <- data.frame(
        outcome=outcome,
        se_adjustment="arellano",
        estimate=estimate,
        se=hc_se,
        variance=hc_se ^ 2,
        t_stat=estimate / hc_se,
        p_value=2 * pnorm(abs(estimate / hc_se), lower.tail=FALSE),
        mse=mean(resid(m)^2, na.rm=T),
        stringsAsFactors=FALSE
      )
      results <- rbind(results, hc_r)
      rownames(results) <- NULL
    }
  }
  
  results <- left_join(results, meta_data, by="outcome")
  results <- left_join(results, model_simulation$balance_statistics, by="outcome")
  
  return(results)
}


######################
### RESULTS METHOD ###
######################

#' Compiles final results across simulation runs into a single dataframe
#' 
#' @description A convenience function that takes simulation results and binds into a single table
#' 
#' @param r Results from a single model simulation.
#' @noRd
noconf_results <- function(r) {
  return(dplyr::bind_rows(r))
}
