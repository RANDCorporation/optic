

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
tvary_sample <- function(single_simulation) {
  
  ##########################################
  ### PULL DATA AND PARAMETERS/VARIABLES ###
  ##########################################
  x <- single_simulation$data
  pc <- single_simulation$prior_control
  
  if (single_simulation$prior_control == "level") {
    x$prior_control <- x$prior_control_level_OLD
    x$prior_control_old <- x$prior_control_level_OLD
  } else if (single_simulation$prior_control == "trend") {
    x$prior_control <- x$prior_control_trend_OLDW
    x$prior_control_old <- x$prior_control_trend_OLD
  } else {
    stop("invalid prior control option, must be either 'level' or 'trend'")
  }
  
  n <- single_simulation$n_units
  unit_var <- single_simulation$unit_var
  treat_var <- single_simulation$treat_var
  time_var <- single_simulation$time_var
  n_implementation_periods <- single_simulation$n_implementation_periods
  
  #Subset sim params to effect magnitude
  effs <- names(single_simulation)[grep("effect_magnitude", names(single_simulation))]
  effect_magnitude <- unlist(single_simulation[effs])
  
  # Convert effect magnitude into a percent of the fully-saturated effect.
  # Do this to maintain similar application of effect by time to the
  # implementation used for co-occuring policies & linear interpolation method
  # for n implementation years.
  effect_pct <- effect_magnitude/max(effect_magnitude)
  
  effect_direction <- single_simulation$effect_direction
  model_type = names(single_simulation$models)
  
  # DID package specifies outcome name differently so make sure to pull out from model_args rather than use the formula:
  if(!any(model_type == "did")){
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
    
    treated[[current_unit]] <- list(
      policy_years = yr:max(x$year, na.rm=TRUE),
      policy_month = mo,
      
      # Critical change compared to other simulation methods is below;
      # I am replacing the linear increase in effect induced by "calculate_exposure"
      # with the user-provided effects (converted into a percent)
      exposure = effect_pct,
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
tvary_premodel <- function(model_simulation) {
  ##########################################
  ### PULL DATA AND PARAMETERS/VARIABLES ###
  ##########################################
  
  x <- model_simulation$data
  model <- model_simulation$models
  
  if (model$type != "did") {
    outcome <- optic::model_terms(model$model_formula)[["lhs"]]
    oo <- dplyr::sym(outcome)
  } else if (model$type=='did') {
    outcome <- as.character(model_simulation$models$model_args$model_args$yname)
    oo <- dplyr::sym(outcome)
  }
  
  model_type <- model$type
  balance_statistics <- NULL
  
  effs <- names(model_simulation)[grep("effect_magnitude", names(model_simulation))]
  te <- max(unlist(model_simulation[effs]))
  
  ##############################
  ### APPLY TREATMENT EFFECT ###
  ##############################
  # custom method that works for concurrent and regular single policy eval
  x <- apply_treatment_effect(
    x=x,
    model_formula=model$model_formula,
    model_call=model$model_call,
    te=te,
    effect_direction=model_simulation$effect_direction,
    concurrent=FALSE
  )
  
  unit_sym <- dplyr::sym(model_simulation$unit_var)
  time_sym <- dplyr::sym(model_simulation$time_var)

  # if autoregressive, need to add lagged outcome
  if (model_type == "autoreg"|model_type == "autoreg_debiased") {
    
    x <- x %>%
      dplyr::arrange(!!unit_sym, !!time_sym) %>%
      dplyr::group_by(!!unit_sym) %>%
      dplyr::mutate(lag_outcome = dplyr::lag(!!oo, n=1L)) %>%
      dplyr::ungroup()
    
      formula_components <- as.character(model_simulation$models$model_formula)
      updated_3 <- strsplit(formula_components[3], " | ", fixed=TRUE)
      
      new_fmla <- as.formula(paste(formula_components[2], formula_components[1], updated_3[[1]][[1]], "+ lag_outcome |", updated_3[[1]][[2]]))
    
      model_simulation$models$model_formula <- new_fmla
   
  } else if (model_type == "multisynth") {
    x$treatment[x$treatment > 0] <- 1
    x$treatment_level[x$treatment_level > 0] <- 1
    x <- x %>%
      filter(!is.na(!!oo))
    if (sum(is.na(x[[outcome]])) > 0) {
      stop("multisynth method cannot handle missingness in outcome.")
    }
  } else if (model_type == "did") {
    
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
  
  # Add on a implementation year and time-to-treat variable:
  x <- x %>% 
    group_by(!!unit_sym) %>%
    mutate(treatment_date = ifelse(max(trt_ind == 1), max(year ^ (1-treatment)), 0)) %>% 
    ungroup()
  
  x$time_to_treat <- x$year - x$treatment_date
  
  # Set time-to-treat to Inf for untreated years. 
  x <- x %>%
       mutate(time_to_treat = ifelse(treatment_date == 0,
                                     Inf,
                                     time_to_treat))
  
  # Depending on the model, we may need to recode treatment year or
  # treatment date:
  
  if (model_type == "eventstudy"|model_type == "autoreg"|model_type == "autoreg_debiased"){
    
    # Sun and Abraham models expect treatment year to be Inf, if untreated:
    x <- x %>% 
      mutate(treatment_date = ifelse(treatment_date == 0, Inf, treatment_date))
    
  }else if (model_type == "did"){
    
    # CSA models expect treatment year to be Inf, if untreated and expects
    # location to be a numeric variable
    x <- x %>% 
      mutate(treatment_date = ifelse(treatment_date == 0, Inf, treatment_date),
             !!unit_sym := as.numeric(as.factor(!!unit_sym)))
    
  }else if (model_type == "multisynth"){
    
    # Multisynth expects treatment to be either a 0/1 (sampling process uses this
    # variable instrumentally to produce draws of treatment effect. So reset to 
    # binary)
    
    x <- x %>% mutate(treatment = ifelse(treatment > 0, 1, 0))
    
  }else if (model_type == "did_imputation"){
    
    # I added this section for completeness, in case we want to modify later.
    #Borusyak, Jaravel, and Spiess expects treatment_date to be zero for
    # untreated units. So no need to change data coding.
    
  }else if (model_type == "did2s"){
    
    # Similar to multisynth, we need treatment to again be binary for
    # the first stage 
    x <- x %>% mutate(treatment = ifelse(treatment > 0, 1, 0))
    
  }
  
  model_simulation$balance_statistics <- bal_stats
  model_simulation$data <- x
  
  # Save iterations for debug purposes:
  # x <- x[, c("state", "year", "population", "unemploymentrate", "crude.rate", "time_to_treat")]
  # x$ever_treated <- ifelse(x$time_to_treat >= 0 & x$time_to_treat != Inf, 1, 0)
  # 
  # filepath <- "./time_vary_plots/test_data"
  # 
  # if (length(list.files(filepath)) == 0){
  #   i <- 1
  # }else{
  #   existing_files <- list.files(filepath)
  #   i <- max(as.numeric(gsub("iter_|.csv", "", existing_files))) + 1
  # }
  # 
  # filename <- sprintf("%s/iter_%s.csv", filepath, i)
  # write.csv(x, filename, row.names = F)

  return(model_simulation)
  
}


####################
### MODEL METHOD ###
####################
#' Runs a given model simulation and stores results
#' 
#' @description Runs the model against the prepared simulation data along with
#'     any provided arguments. Stores the model object in the input
#'     list, generating a new element named "model_result" and returns full list
#'
#' @param model_simulation An object created from OpticModel, which specifies simulation settings such as model formulas, model call, etc
#' @noRd
tvary_model <- function(model_simulation) {
  
  model <- model_simulation$models
  model_type <- model_simulation$models$type
  addtl_args <- model$model_args$model_args
  
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
    args[['n_leads']] <- model_simulation$n_implementation_periods
  }else if (model_type == "did_imputation"){
    args[['horizon']] <- T
  }else if (model_type == "did2s"){
    args[['treatment']] <- 'treatment'
  }
  
  # Hold onto the error message. This is meant to protect against
  # possible convergence issues with ASCM & Debiased Autoregressive models
  
  try_model <- function(model, args){
    m <- tryCatch(
      {
        do.call(model[['model_call']], args)
      },
      error = function(e){
        return(e)
      }
    )
    return(m)
  }
  
  m <- try_model(model, args)
  
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

tvary_postmodel <- function(model_simulation) {
  
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
    n_units=model_simulation$n_units,
    effect_direction=model_simulation$effect_direction  
  )
  
  # Add time-to-treat effects as meta-data:
  effect_names <- names(model_simulation)[grep("effect_magnitude", names(model_simulation))]
  effects <- as.data.frame(model_simulation[effect_names])
  
  meta_data <- cbind(meta_data, effects)
  
  m <- model_simulation$model_result
  
  # First, check to see if the model returned an error. If so,
  # save the model error message within the results table. We can filter this
  # out later. 
  
  if (inherits(m, "error")){
    message <- paste("Error: ", m$message)
    
    # Create dummy indicators for estimates to ensure pivot_wider
    # works correctly later.
    dummy_estimates <- rep(NA, model_simulation$n_implementation_periods)
    
    r <- data.frame(
      outcome=outcome,
      se_adjustment="none",
      estimate=dummy_estimates,
      se=NA,
      variance=NA,
      t_stat=NA,
      p_value=NA,
      mse=NA,
      error = message,
      stringsAsFactors=FALSE)
  }else{
    # get model result information and apply standard error adjustments
    if (model_simulation$models[["type"]] == "eventstudy"|model_simulation$models[["type"]] == "autoreg"|model_simulation$models[["type"]] == "did2s") {
      
      cf <- as.data.frame(summary(m)$coeftable)
      
      cf$variable <- row.names(cf)
      rownames(cf) <- NULL
      
      keep_treat <- paste0("time_to_treat::", 1:meta_data$n_implementation_periods)
      
      cf <- cf[cf$variable %in% keep_treat, ]
      estimate <- cf[["Estimate"]]
      
      pval <- c(cf[["Pr(>|t|)"]], cf[["Pr(>|z|)"]])
      
      r <- data.frame(
        outcome=outcome,
        se_adjustment="none",
        estimate=estimate,
        se=cf[["Std. Error"]],
        variance=cf[["Std. Error"]] ^ 2,
        t_stat=c(cf[["t value"]], cf[["z value"]]),
        p_value=pval,
        mse=mean(resid(m)^2, na.rm=T),
        error = NA,
        stringsAsFactors=FALSE
      )
      
    } else if (model_simulation$models[["type"]] == "did"){
      
      m_agg <- did::aggte(m, type='dynamic', na.rm=T)
      
      cf <- data.frame("time" = m_agg$egt,
                       "estimate" = m_agg$att.egt,
                       "se" = m_agg$se.egt)
      
      cf <- cf[cf$time >= 1 & cf$time <= meta_data$n_implementation_periods,]
      
      estimate <- cf$estimate
      se <- cf$se
      variance <- se ^ 2
      t_stat <- NA
      p_value <- 2 * pnorm(abs(estimate/se), lower.tail = FALSE)
      
      r <- data.frame(
        outcome=outcome,
        se_adjustment="none",
        estimate=estimate,
        se=se,
        variance=variance,
        t_stat=NA,
        p_value=p_value,
        mse = NA,
        error = NA,
        stringsAsFactors=FALSE
      )
    }else if (model_simulation$models[['type']] == "multisynth"){
      
      cf <- summary(m)
      cf <- cf$att
      
      cf <- cf[cf$Level == "Average" & cf$Time >= 0 & cf$Time <= meta_data$n_implementation_periods - 1 & !is.na(cf$Time),]
      
      estimate <- cf[["Estimate"]]
      se       <- cf[["Std.Error"]]
      
      variance <- se ^ 2
      
      t_stat <- NA
      p_value <- 2 * pnorm(abs(estimate/se), lower.tail = FALSE)
      
      r <- data.frame(
        outcome=outcome,
        se_adjustment="none",
        estimate=estimate,
        se=se,
        variance=variance,
        t_stat=NA,
        p_value=p_value,
        mse = NA,
        error = NA,
        stringsAsFactors=FALSE
      )
    }else if (model_simulation$models[['type']] == "did_imputation"){
      
      # Note that did imputation doesn't return a usual summary object, hence
      # why we're setting cf to m
      cf <- m
      cf$term <- as.numeric(cf$term)
      
      cf <- cf[cf$term >= 1 & cf$term <= meta_data$n_implementation_periods,]
      
      estimate <- cf$estimate
      se <- cf$std.error
      variance <- se ^ 2
      
      t_stat <- NA
      
      p_value <- 2 * pnorm(abs(estimate/se), lower.tail = FALSE)
      
      r <- data.frame(
        outcome=outcome,
        se_adjustment="none",
        estimate=estimate,
        se=se,
        variance=variance,
        t_stat=NA,
        p_value=p_value,
        mse = NA,
        error = NA,
        stringsAsFactors=FALSE
      )
      
    }else if(model_simulation$models[['type']] == "autoreg_debiased"){
    
        mod_fit <- summary(m)
        
        lags <- model_simulation$models$model_args$model_args$lags
        
        eff_interest <- paste0("theta_t", 1:lags)
        thetas <- coef(mod_fit)[eff_interest, "Estimate"]
        
        # Convert to time-varying effects:
        
        estimate <- c()
        for (i in 1:length(thetas)){
          estimate[i] <- sum(thetas[1:i])
        }
        
        # Get variance-covariance matrix, then calculate joint se:
        # e.g., b1 + b2 = sqrt(var[b1] + var[b2] + 2Cov[b1, b2])
        se <- c()
        
        # Estimated clustered-robust standard errors
        
        vcov_mat <- vcov(m)
        vcov_reduced <- which(rownames(vcov_mat) %in% eff_interest)
        vcov_mat <- vcov_mat[vcov_reduced, vcov_reduced, drop = F]
        
        sum_se <- function(i, vcov_mat){
          
          var_sum <- sum(vcov_mat[1:i])
          cov_sum <- 2*sum(vcov_mat[1:i][upper.tri(vcov_mat[1:i])])
          
          return(sqrt(var_sum + cov_sum))
          
        }
        
        for (i in 1:length(thetas)){
          se[i] <- sum_se(i, vcov_mat)
        }
        
        variance <- se^2
        
        r <- data.frame(
          outcome=outcome,
          se_adjustment="none",
          estimate=estimate,
          se=se,
          variance=variance,
          t_stat=NA,
          p_value=NA,
          mse = NA,
          error = NA,
          stringsAsFactors=FALSE
      )
    }
  }
  
  #Make effect names a little bit smaller:
  effect_names <- gsub("effect_magnitude", "ttt==", effect_names)
  
  r['variable'] <- effect_names
  
  # Remove the se_adjustment variable from the analysis - all models should
  # be clustered at the group-level!
  r <- subset(r, select = -se_adjustment)
  
  # Reshape results so that time-varying effects are kept as columns,
  # rather than rows
  r <- pivot_wider(r, 
                   id_cols = c(outcome, mse, error),
                   names_from = variable,
                   values_from = c(estimate, se, variance, t_stat, p_value))
  
  r <- left_join(r, meta_data, by="outcome")
  r <- left_join(r, model_simulation$balance_statistics, by="outcome")
  
  return(r)
  
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
tvary_results <- function(r) {
  return(dplyr::bind_rows(r))
}
