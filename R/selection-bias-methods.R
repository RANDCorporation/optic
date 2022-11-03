#######################
### SAMPLING METHOD ###
#######################

#' perform sampling and coding of treatment for selection bias simulations
#' 
#' @description uses values of b0, b1, b2 to sample treated units based on
#'     values of two covariates (here moving average and unemployment rate) to induce confounding (selection) bias.
#'     Once treated units are identified, codes level and change version of
#'     treatment that are used in various modeling approaches later on.
#'
#' @param single_simulation object created from SimConfig$setup_single_simulation()
#' 
#' @export
selbias_sample <- function(single_simulation) {
  x <- single_simulation$data
  pc = single_simulation$prior_control
  
  if (single_simulation$prior_control == "mva3") {
    x$prior_control <- x$prior_control_mva3_OLD
    x$prior_control_old <- x$prior_control_mva3_OLD
  } else if (single_simulation$prior_control == "trend") {
    x$prior_control <- x$prior_control_trend_OLD
    x$prior_control_old <- x$prior_control_trend_OLD
  } else {
    stop("invalid prior control option, must be either 'mva3' or 'trend'")
  }
  
  unit_var <- single_simulation$unit_var
  time_var <- single_simulation$time_var
  effect_magnitude <- single_simulation$effect_magnitude
  effect_direction <- single_simulation$effect_direction
  policy_speed <- single_simulation$policy_speed
  number_implementation_years <- as.numeric(single_simulation$n_implementation_periods)
  bias_vals <- single_simulation$globals[["bias_vals"]][[single_simulation$bias_type]][[single_simulation$prior_control]][[single_simulation$bias_size]]
  model_type = names(single_simulation$models)
  
  #############################
  ### AUGMENT OUTCOME FIRST ###
  #############################
  # get the bias values for outcome augmentation
  a1 <- bias_vals["a1"]#bias_vals$a1 
  a2 <- bias_vals["a2"]#bias_vals$a2
  a3 <- bias_vals["a3"]#bias_vals$a3
  a4 <- bias_vals["a4"]#bias_vals$a4
  a5 <- bias_vals["a5"]#bias_vals$a5
  
  # since this is happening before the model loop in run_iteration, we need to make
  # sure we augment all unique outcomes used by models
  if(model_type != "drdid"){
    outcomes <- unique(sapply(single_simulation$models, function(x) { optic::model_terms(x[["model_formula"]])[["lhs"]] }))
  }else{
    outcomes <- as.character(single_simulation$models$drdid$model_args$yname)
  }
  
  # 
  # # apply bias to outcome
  # for (outcome in outcomes) {
  #     #Adj.Y = Y.obs+a1*mva3+a2*unemployment+a3*(mva3*unemployment)+a4*mva^2+a5*unemployment^2
  #     x[[outcome]] <- x[[outcome]] + (a1 * x$prior_control) + (a2 * x$unemploymentrate) +
  #       (a3 * (x$prior_control * x$unemploymentrate)) +
  #       (a4 * (x$prior_control ^ 2)) + (a5 * (x$unemploymentrate ^ 2))
  #   }
  
  ################################
  ### IDENTIFY TREATMENT UNITS ###
  ################################
  # get bias vals for creating probability of selection
  b0 <- bias_vals["b0"]#bias_vals$b0
  b1 <- bias_vals["b1"]#bias_vals$b1 
  b2 <- bias_vals["b2"]#bias_vals$b2
  b3 <- bias_vals["b3"]#bias_vals$b3
  b4 <- bias_vals["b4"]#bias_vals$b4
  b5 <- bias_vals["b5"]#bias_vals$b5
  
  # need to create a matrix of state x year 
  # probabilities of being assigned to enact policy
  available_units <- unique(x[[unit_var]])
  # atp <- sort(unique(x[[time_var]]))[-1:-5]
  # atp <- atp[-length(atp):-(length(atp)-1)]
  atp <- sort(unique(x[[time_var]]))[-1:-3]
  available_time_periods <- atp
  
  # For Each Year
  for(t in 1:length(available_time_periods)){
    
    time = available_time_periods[t]
    # Get treatment assignment
    x_treat = x %>%
      filter(!!as.name(time_var) == time) %>%
      mutate(logits =
               b0 +
               (b1 * prior_control) +
               (b2 * unemploymentrate) +
               (b3 * (prior_control * unemploymentrate)) +
               (b4 * (prior_control ^ 2)) +
               (b5 * (unemploymentrate ^ 2))) %>%
      dplyr::select(!!as.name(unit_var), !!as.name(time_var), logits) %>%
      rowwise() %>%
      mutate(trt_pr = exp(logits) / (1 + exp(logits)),
             One_minus_trt_pr = 1-trt_pr) %>%
      # check for cases where prob is 1
      mutate(trt_pr = case_when(is.na(trt_pr) ~ 1,
                                TRUE ~ trt_pr)) %>%
      mutate(One_minus_trt_pr = 1 - trt_pr) %>%
      mutate(trt_ind = sample(c(0, 1), 1, prob = c(One_minus_trt_pr, trt_pr))) %>%
      dplyr::select(-c(logits, One_minus_trt_pr, trt_pr))
    
    ##############################
    ### APPLY TREATMENT EFFECT ###
    ##############################
    x <- apply_treatment_effect(
      x=x_treat,
      model_formula=single_simulation$models$fixedeff_linear$model_formula,
      model_call=single_simulation$models$fixedeff_linear$model_call,
      te=effect_magnitude,
      effect_direction=effect_direction,
      concurrent=FALSE
    )
    
    # Update Outcomes (augment outcomes)
    x_new = x %>%
      filter(!!as.name(time_var) == time) %>%
      mutate(!!outcomes := 
               !!as.name(outcomes) +
               (a1 * prior_control) +
               (a2 * unemploymentrate) +
               (a3 * (prior_control * unemploymentrate)) +
               (a4 * (prior_control ^ 2)) +
               (a5 * (unemploymentrate ^ 2))) %>%
      dplyr::select(!!as.name(unit_var), !!as.name(time_var), !!as.name(outcomes)) %>%
      rename(new = !!outcomes)
    
    if(t==1){
      x = x %>%
        left_join(., x_treat, by = c(unit_var, time_var)) %>%
        left_join(., x_new, by = c(unit_var, time_var)) %>%
        mutate(!!outcomes := case_when(is.na(new) ~ !!as.name(outcomes),
                                       TRUE ~ new)) %>%
        dplyr::select(-new)
    } else{
      x_treat = x_treat %>%
        rename(trt_ind_new = trt_ind)
      x = x %>%
        left_join(., x_treat, by = c(unit_var, time_var)) %>%
        mutate(trt_ind = case_when(is.na(trt_ind_new) ~ trt_ind,
                                   TRUE ~ trt_ind_new)) %>%
        left_join(., x_new, by = c(unit_var, time_var)) %>%
        mutate(!!outcomes := case_when(is.na(new) ~ !!as.name(outcomes),
                                       TRUE ~ new)) %>%
        dplyr::select(-c(new, trt_ind_new))
    }
    
    # update prior control
    if(pc == 'mva3'){
      x = x %>%
        group_by(!!as.name(unit_var)) %>%
        mutate(lag1 = lag(!!as.name(outcomes), n=1L),
               lag2 = lag(!!as.name(outcomes), n=2L),
               lag3 = lag(!!as.name(outcomes), n=3L)) %>%
        ungroup() %>%
        rowwise() %>%
        mutate(prior_control = mean(c(lag1, lag2, lag3))) %>%
        ungroup() 
    } else if(pc == 'trend'){
      x = x %>%
        group_by(!!as.name(unit_var)) %>%
        mutate(lag1 = lag(!!as.name(outcomes), n=1L),
               lag2 = lag(!!as.name(outcomes), n=2L),
               lag3 = lag(!!as.name(outcomes), n=3L)) %>%
        ungroup() %>%
        rowwise() %>%
        mutate(prior_control = lag1 - lag3) %>%
        ungroup()
    } else{
      stop("invalid prior control option, must be either 'mva3' or 'trend'")
    } 
  }
  
  # Revise trt_ind matrix
  x = x %>%
    arrange(!!as.name(unit_var), !!as.name(time_var)) %>%
    group_by(!!as.name(unit_var)) %>%
    mutate(isfirst = as.numeric(trt_ind == 1 & !duplicated(trt_ind==1))) %>%
    mutate(trt_ind = cumsum(tidyr::replace_na(isfirst,0)))
  
  # Get time_periods and n_treated
  # (1) time periods
  the_treated = x %>%
    filter(isfirst==1) %>%
    dplyr::select(!!as.name(unit_var), !!as.name(time_var)) 
  time_periods = x %>% 
    filter(!!as.name(time_var) == min(!!as.name(time_var))) %>%
    dplyr::select(!!as.name(unit_var)) %>%
    left_join(., the_treated, by = c(unit_var)) 
  time_periods = time_periods$year
  time_periods[is.na(time_periods)] = Inf
  # (2) n_treated
  n_treated = x %>%
    group_by(!!as.name(unit_var)) %>%
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
        exposure = optic:::calculate_exposure(mo, number_implementation_years),
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
  # apply treatment to data
  # Get policy date and exposure in proper format 
  policy_dates = purrr::transpose(treated)$policy_date %>% 
    dplyr::bind_rows() %>% 
    tidyr::gather(!!unit_var, treatment_date) 
  
  if(is.factor(x[[unit_var]])){
    policy_dates = policy_dates %>%
      dplyr::mutate_if(is.character, factor)
  } else{
    policy_dates = policy_dates %>%
      dplyr::mutate_if(is.character, as.double)
  }
  
  x_policy_info = x %>%
    dplyr::filter(trt_ind == 1) %>%
    dplyr::select(!!as.name(unit_var), !!as.name(time_var))
  x_policy_info$treatment = purrr::transpose(treated)$exposure %>% unlist
  
  x_policy_info = x_policy_info %>%
    dplyr::left_join(., policy_dates, by = unit_var)
  
  # apply treatment to data
  x = x %>%
    dplyr::left_join(., x_policy_info, by = c(unit_var, time_var)) %>%
    dplyr::mutate(treatment = case_when(is.na(treatment) ~ 0,
                                        TRUE ~ treatment),
                  treatment_date = case_when(is.na(treatment_date) ~ as.Date(NA),
                                             TRUE ~ treatment_date))
  
  # create level and change code versions of treatment variable
  x$treatment_level <- x$treatment
  
  # add in change code version of treatment variable
  unit_sym <- dplyr::sym(unit_var)
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
### PRE-MODEL METHOD ###
########################

#' Modify the outcome and prepare data for modeling based on model type
#' 
#' @description modifies the outcome with bias from trend and unemployemnt
#'     rate; if autoregressive run, also adds a lag of the crude rate that
#'     is newly derived from modified deaths if using deaths as outcome.
#'     Calculates some balance information that is passed along to later
#'     steps.
#' 
#' @export
selbias_premodel <- function(model_simulation) {
  x <- model_simulation$data
  model <- model_simulation$models
  outcome <- optic:::model_terms(model$model_formula)[["lhs"]]
  oo <- dplyr::sym(outcome)
  model_type <- model$type
  balance_statistics <- NULL
  
  # if autoregressive, need to add lag for crude rate
  # when outcome is deaths, derive new crude rate from modified outcome
  if (model_type == "autoreg") {
    if (outcome == "deaths") {
      x$crude.rate <- (x$deaths * 100000)/ x$population
    }
    
    # get lag of crude rate and add it to the model
    unit_sym <- dplyr::sym(model_simulation$unit_var)
    time_sym <- dplyr::sym(model_simulation$time_var)
    
    x <- x %>%
      dplyr::arrange(!!unit_sym, !!time_sym) %>%
      dplyr::group_by(!!unit_sym) %>%
      dplyr::mutate(lag_outcome = dplyr::lag(!!oo, n=1L)) %>%
      dplyr::ungroup()
    
    model_simulation$models$model_formula <- update.formula(model_simulation$models$model_formula, ~ . + lag_outcome)
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
      mu1_unempl = mean(unemploymentrate[treatment > 0]),
      mu0_unempl = mean(unemploymentrate[treatment == 0]),
      sd_unempl = sd(unemploymentrate),
      mu1 = mean((!!oo)[treatment > 0]),
      mu0 = mean((!!oo)[treatment == 0]),
      sd = sd(!!oo),
      .groups="keep"
    ) %>%
    mutate(
      es_prior = (mu1_prior - mu0_prior) / sd_prior,
      es_unempl = (mu1_unempl - mu0_unempl) / sd_unempl,
      es = (mu1 - mu0) / sd
    ) %>%
    ungroup() %>%
    summarize(n = max(n_trt, na.rm=TRUE),
              mean_es_prior = mean(es_prior, na.rm=TRUE),
              max_es_prior = max(abs(es_prior), na.rm=TRUE),
              mean_es_unempl = mean(es_unempl, na.rm=TRUE),
              max_es_unempl = max(abs(es_unempl), na.rm=TRUE),
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
              sd = sd(!!oo), na.rm=T)
  bal_stats = bind_cols(bal_stats, bal_stats2)
  
  model_simulation$balance_statistics <- bal_stats
  model_simulation$data <- x
  
  return(model_simulation)
}

#' run model and store results
#' 
#' @description runs the model against the prepared data along with
#'     any provided arguments. Stores the model object in the input
#'     list, new element named "model_result" and returns full list
#' 
#' @export
selbias_model <- function(model_simulation) {
  model <- model_simulation$models
  x <- model_simulation$data
  
  if (model_simulation$models$name == "drdid") {
    args = c(list(data=x), model[["model_args"]])
  } else {
    args = c(list(data=x, formula=model[["model_formula"]]), model[["model_args"]])
  }
  
  m <- do.call(
    model[["model_call"]],
    args
  )
  
  model_simulation$model_result <- m
  
  return(model_simulation)
}


##########################
### POST MODELS METHOD ###
##########################

#' process results from model(s) 
#' 
#' @description summarizes the statistical performance of the model(s) being compared by computing summary information on the model fit, estimated effects and standard errors 
#' 
#' @export
selbias_postmodel <- function(model_simulation) {
  outcome <- model_terms(model_simulation$models[["model_formula"]])[["lhs"]]
  bias_vals <- model_simulation$globals[["bias_vals"]][[model_simulation$bias_type]][[model_simulation$prior_control]][[model_simulation$bias_size]]
  # get run metadata to merge in after
  meta_data <- data.frame(
    model_name = model_simulation$models$name,
    model_call = model_simulation$models[["model_call"]],
    outcome = outcome,
    model_formula = Reduce(paste, trimws(deparse(model_simulation$models[["model_formula"]]))),
    policy_speed = model_simulation$policy_speed,
    n_implementation_years = model_simulation$n_implementation_periods,
    prior_control = model_simulation$prior_control,
    bias_type = model_simulation$bias_type,
    bias_size = model_simulation$bias_size,
    b0 = bias_vals["b0"],
    b1 = bias_vals["b1"],
    b2 = bias_vals["b2"],
    b3 = bias_vals["b3"],
    b4 = bias_vals["b4"],
    b5 = bias_vals["b5"],
    a1 = bias_vals["a1"],
    a2 = bias_vals["a2"],
    a3 = bias_vals["a3"],
    a4 = bias_vals["a4"],
    a5 = bias_vals["a5"]
  )
  
  # get model result information and apply standard error adjustments
  if (model_simulation$models[["type"]] != "multisynth") {
    m <- model_simulation$model_result
    cf <- as.data.frame(summary(m)$coefficients)
    cf$variable <- row.names(cf)
    rownames(cf) <- NULL
    
    treatment <- cf$variable[grepl("^treatment", cf$variable)][1]
    
    cf <- cf[cf$variable == treatment, ]
    estimate <- cf[["Estimate"]]
    
    r <- data.frame(
      outcome=outcome,
      se_adjustment="none",
      estimate=estimate,
      se=cf[["Std. Error"]],
      variance=cf[["Std. Error"]] ^ 2,
      t_stat=c(cf[["t value"]], cf[["z value"]]),
      p_value=c(cf[["Pr(>|t|)"]], cf[["Pr(>|z|)"]]),
      mse=mean(m[["residuals"]]^2, na.rm=T),
      stringsAsFactors=FALSE
    )
  } else {
    m <- model_simulation$model_result
    cf <- summary(m)
    cf <- cf$att
    estimate <- cf[cf$Level == "Average" & is.na(cf$Time), "Estimate"]
    se <- cf[cf$Level == "Average" & is.na(cf$Time), "Std.Error"]
    variance <- se ^ 2
    t_stat <- NA
    p_value <- 2 * pnorm(abs(estimate/se), lower.tail = FALSE)
    mse <- mean(unlist(lapply(m$residuals, function(x) {mean(x^2)})))
    
    r <- data.frame(
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
  }
  
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
      mse=mean(m[["residuals"]]^2, na.rm=T),
      stringsAsFactors=FALSE
    )
    r <- rbind(r, h_r)
    rownames(r) <- NULL
  }
  
  if ("cluster" %in% model_simulation$models[["se_adjust"]]) {
    clust_indices <- as.numeric(rownames(m$model))
    clust_var <- as.character(model_simulation$data[[model_simulation$unit_var]][clust_indices])
    clust_coeffs <- cluster_adjust_se(m, clust_var)[[2]]
    clust_vcov <- cluster_adjust_se(m, clust_var)[[1]][2,3] #not 100% alginment with SEs from model so worried this is off
    class(clust_coeffs) <- c("coeftest", "matrix")
    clust_coeffs <- as.data.frame(clust_coeffs)
    clust_coeffs$variable <- row.names(clust_coeffs)
    rownames(clust_coeffs) <- NULL
    clust_coeffs <- clust_coeffs[clust_coeffs$variable == treatment,]
    
    c_r <- data.frame(
      outcome=outcome,
      se_adjustment="cluster",
      estimate=clust_coeffs[["Estimate"]],
      se=clust_coeffs[["Std. Error"]],
      variance=clust_coeffs[["Std. Error"]] ^ 2,
      t_stat=c(clust_coeffs[["z value"]], clust_coeffs[["t value"]]),
      p_value=c(clust_coeffs[["Pr(>|z|)"]], clust_coeffs[["Pr(>|t|)"]]),
      mse=mean(m[["residuals"]]^2, na.rm=T),
      stringsAsFactors=FALSE
    )
    
    r <- rbind(r, c_r)
  }
  
  if ("arellano" %in% model_simulation$models[["se_adjust"]]) {
    clust_indices <- as.numeric(rownames(m$model))
    clust_var <- as.character(model_simulation$data[[model_simulation$unit_var]][clust_indices])
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
      mse=mean(m[["residuals"]]^2, na.rm=T),
      stringsAsFactors=FALSE
    )
    r <- rbind(r, hc_r)
    rownames(r) <- NULL
  }
  
  r <- left_join(r, meta_data, by="outcome")
  r <- left_join(r, model_simulation$balance_statistics, by="outcome")
  
  return(r)
}

######################
### RESULTS METHOD ###
######################

#' compiles the final results 
#' 
#' @description compiles the results into one table for all permutations of the simulation
#' 
#' @export
selbias_results <- function(r) {
  return(do.call(rbind, r))
}


