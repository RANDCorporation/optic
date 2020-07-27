#######################
### SAMPLING METHOD ###
#######################

#' perform sampling and coding of treatment for selection bias simulations
#' 
#' @description uses values of b0, b1, b2 to sample treated units based on
#'     values of moving average and unemployment rate to induce selection bias.
#'     Once treated units are identified, codes level and change version of
#'     treatment that are used in various modeling approaches later on.
#'
#' @param single_simulation object created from SimConfig$setup_single_simulation()
selbias_sample <- function(single_simulation) {
  x <- single_simulation$data
  
  if (single_simulation$prior_control == "mva3") {
    x$prior_control <- x$prior_control_mva3
  } else if (single_simulation$prior_control == "trend") {
    x$prior_control <- x$prior_control_trend
  } else {
    stop("invalid prior control option, must be either 'mva3' or 'trend'")
  }
  
  unit_var <- single_simulation$unit_var
  time_var <- single_simulation$time_var
  policy_speed <- single_simulation$policy_speed
  number_implementation_years <- as.numeric(single_simulation$n_implementation_periods)
  
  # get bias value information
  bias_vals <- single_simulation$globals[["bias_vals"]][[single_simulation$bias_type]][[single_simulation$prior_control]][[single_simulation$bias_size]]
  b0 <- bias_vals["b0"]
  b1 <- bias_vals["b1"]
  b2 <- bias_vals["b2"]
  b3 <- bias_vals["b3"]
  b4 <- bias_vals["b4"]
  b5 <- bias_vals["b5"]
  
  #need to create a matrix of state x year 
  #probabilities of being assigned to enact policy
  available_units <- unique(x[[unit_var]])
  #don't include first 3 years in this version where depends on 3-year moving average
  atp <- sort(unique(x[[time_var]]))[-1:-5]
  atp <- atp[-length(atp):-(length(atp)-2)]
  available_time_periods <- atp
  
  x_simplex <- x[x[[time_var]] %in% available_time_periods, ]
  
  trt_pr <- trt_ind <- matrix(0, length(available_units), length(available_time_periods))
  n_treated <- time_periods <-rep(0, length(available_units))
  
  #might not need a loop here
  for(i in 1:length(available_units)) {
    current_unit <- available_units[i]
    unit_data <- x_simplex[x_simplex[[unit_var]] == current_unit, ]
    
    # b0+b1*mva3+b2*unemployment+b3*(mva3*unemployment)+b4*mva^2+b5*unemployment^2
    logits <- b0 + (b1 * unit_data$prior_control) + (b2 * unit_data$unemploymentrate) +
      (b3 * (unit_data$prior_control * unit_data$unemploymentrate)) +
      (b4 * (unit_data$prior_control ^ 2)) + (b5 * (unit_data$unemploymentrate ^ 2))
    
    trt_pr[i, ] <- exp(logits)/(1+exp(logits))
    # print(trt.pr[s,])
    for(j in 1:length(trt_pr[i, ])) {
      trt_ind[i, j] <- sample(c(0,1), 1, prob=c(1-trt_pr[i, j], trt_pr[i, j]))
    }
    time_periods[i] <- min(available_time_periods[trt_ind[i, ] == 1])
    n_treated[i] <- sum(trt_ind[i, ])
  }
  
  #print(table(time_periods)) #this helps me see range; first run was 2002-2006; all happened early (b0=-1,b1=.5,b2=.05)
  #length(n.trted>0) #and all states implemented early on in time series
  #print(table(n_treated))
  
  sampled_units <- available_units[n_treated > 0]
  start_periods <- time_periods[n_treated > 0]
  
  treated <- list()
  for (i in 1:length(sampled_units)) { 
    yr <- start_periods[i]
    current_unit <- as.character(sampled_units[i])
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
        exposure = c((12 - mo + 1)/12, rep(1, length((yr + 1):max(x$year, na.rm=TRUE)))),
        policy_date = as.Date(paste0(yr, '-', mo, '-01'))
      )
    }
  }
  
  # apply treatment to data
  x$treatment <- 0
  x$treatment_date <- as.Date(NA)
  for (sunit in names(treated)) {
    for (i in 1:length(treated[[sunit]][["policy_years"]])) {
      time_period <- treated[[sunit]][["policy_years"]][i]
      exposure <- treated[[sunit]][["exposure"]][i]
      
      x$treatment <- dplyr::if_else(
        x[[unit_var]] == sunit & x[[time_var]] == time_period,
        exposure,
        x$treatment
      )
      
      x$treatment_date <- dplyr::if_else(
        x[[unit_var]] == sunit,
        treated[[sunit]][["policy_date"]],
        x$treatment_date
      )
    }
  }
  
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
selbias_premodel <- function(model_simulation) {
  x <- model_simulation$data
  effect_direction <- model_simulation$effect_direction
  
  bias_vals <- model_simulation$globals[["bias_vals"]][[model_simulation$bias_type]][[model_simulation$prior_control]][[model_simulation$bias_size]]
  a1 <- bias_vals["a1"]
  a2 <- bias_vals["a2"]
  a3 <- bias_vals["a3"]
  a4 <- bias_vals["a4"]
  a5 <- bias_vals["a5"]
  model <- model_simulation$models
  balance_statistics <- NULL
  
  # get the outcome and type for this model
  outcome <- optic:::model_terms(model$model_formula)[["lhs"]]
  oo <- dplyr::sym(outcome)
  model_type <- model$type
  
  # apply bias to outcome
  #Adj.Y = Y.obs+a1*mva3+a2*unemployment+a3*(mva3*unemployment)+a4*mva^2+a5*unemployment^2
  x[[outcome]] <- x[[outcome]] + (a1 * x$prior_control) + (a2 * x$unemploymentrate) +
    (a3 * (x$prior_control * x$unemploymentrate)) +
    (a4 * (x$prior_control ^ 2)) + (a5 * (x$unemploymentrate ^ 2))
  
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
      dplyr::mutate(lag_crude.rate = dplyr::lag(crude.rate, n=1L)) %>%
      dplyr::ungroup()
    
    model_simulation$models$model_formula <- update.formula(model_simulation$models$model_formula, ~ . + lag_crude.rate)
  } else if (model_type == "multisynth") {
    x$treatment[x$treatment > 0] <- 1
    x$treatment_level[x$treatment_level > 0] <- 1
    x <- x %>%
      filter(!is.na(!!oo))
    if (sum(is.na(x[[outcome]])) > 0) {
      stop("multisynth method cannot handle missingness in outcome.")
    }
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
  
  model_simulation$balance_statistics <- bal_stats
  model_simulation$data <- x
  
  return(model_simulation)
}

#' run model and store results
#' 
#' @description runs the model against the prepared data along with
#'     any provided arguments. Stores the model object in the input
#'     list, new element named "model_result" and returns full list
selbias_model <- function(model_simulation) {
  model <- model_simulation$models
  x <- model_simulation$data
  
  m <- do.call(
    model[["model_call"]],
    c(list(data=x, formula=model[["model_formula"]]), model[["model_args"]])
  )
  
  model_simulation$model_result <- m
  
  return(model_simulation)
}


##########################
### POST MODELS METHOD ###
##########################

#' TODO: documentation
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

selbias_results <- function(r) {
  return(do.call(rbind, r))
}