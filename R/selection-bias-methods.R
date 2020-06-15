#' TODO: documentaion for selection bias sampling method
selbias_sample <- function(single_simulation) {
  x <- single_simulation$data
  unit_var <- single_simulation$unit_var
  time_var <- single_simulation$time_var
  policy_speed <- single_simulation$policy_speed
  number_implementation_years <- as.numeric(single_simulation$n_implementation_periods)
  b0 <- single_simulation$b_vals.b0
  b1 <- single_simulation$b_vals.b1
  b2 <- single_simulation$b_vals.b2
  
  #need to create a matrix of state x year 
  #probabilities of being assigned to enact policy
  available_units <- unique(x[[unit_var]])
  #don't include first 3 years in this version where depends on 3-year moving average
  available_time_periods <- sort(unique(x[[time_var]]))[-1:-3]
  
  x_simplex <- x[x[[time_var]] %in% available_time_periods, ]
  
  trt_pr <- trt_ind <- matrix(0, length(available_units), length(available_time_periods))
  n_treated <- time_periods <-rep(0, length(available_units))
  
  #might not need a loop here
  for(i in 1:length(available_units)) {
    current_unit <- available_units[i]
    unit_data <- x_simplex[x_simplex[[unit_var]] == current_unit, ]
    
    logits <- b0 + (b1 * unit_data$moving.ave3) + (b2 * unit_data$unemploymentrate)
    
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
        exposure = optic:::calculate_exposure(mo, number_implementation_years)
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
        exposure = c((12 - mo + 1)/12, rep(1, length((yr + 1):max(x$year, na.rm=TRUE))))
      )
    }
  }
  
  # apply treatment to data
  x$treatment <- 0
  for (sunit in names(treated)) {
    for (i in 1:length(treated[[sunit]][["policy_years"]])) {
      time_period <- treated[[sunit]][["policy_years"]][i]
      exposure <- treated[[sunit]][["exposure"]][i]
      
      x$treatment <- dplyr::if_else(
        x[[unit_var]] == sunit & x[[time_var]] == time_period,
        exposure,
        x$treatment
      )
    }
  }
  
  # add and update objects to single sim list
  single_simulation$treated_units <- treated
  single_simulation$data <- x
  
  return(single_simulation)
}


#' TODO: documnetation for treatment effect
selbias_te <- function(single_simulation) {
  x <- single_simulation$data
  effect_direction <- single_simulation$effect_direction
  a1 <- .95 #want moving average to have strong positive (orig .5)
  a2 <- .05 #unemployment less  (orig .1)
  balance_statistics <- NULL
  
  # get the unique model_call values and iterate over them to see which
  # outcomes need to be created
  model_calls <- unique(sapply(single_simulation$models, function(x) {x$model_call}))
  for (m in model_calls) {
    if (m == "lm") {
      # grab all the outcomes where lm is used
      outcomes <- unique(sapply(single_simulation$models, function(x) {
        if(x$model_call == "lm") {
          optic:::model_terms(x$model_formula)[["lhs"]]
        } else {}
      }))
      
      for (o in outcomes) {
        oo <- dplyr::sym(o)
        x[[o]] <- x[[o]] + (a1 * x$moving.ave3) + (a2 * x$unemploymentrate)
        
        # get balance information
        bal_stats <- x %>%
          group_by(year) %>%
          summarize(
            n_trt = sum(treatment > 0),
            mu1_mva3 = mean(moving.ave3[treatment > 0]),
            mu0_mva3 = mean(moving.ave3[treatment == 0]),
            sd_mva3 = sd(moving.ave3),
            mu1_unempl = mean(unemploymentrate[treatment > 0]),
            mu0_unempl = mean(unemploymentrate[treatment == 0]),
            sd_unempl = sd(unemploymentrate),
            mu1 = mean((!!oo)[treatment > 0]),
            mu0 = mean((!!oo)[treatment == 0]),
            sd = sd(!!oo)
          ) %>%
          mutate(
            es_mva3 = (mu1_mva3 - mu0_mva3) / sd_mva3,
            es_unempl = (mu1_unempl - mu0_unempl) / sd_unempl,
            es = (mu1 - mu0) / sd
          ) %>%
          ungroup() %>%
          summarize(n = max(n_trt, na.rm=TRUE),
                    mean_es_mva3 = mean(es_mva3, na.rm=TRUE),
                    max_es_mva3 = max(abs(es_mva3), na.rm=TRUE),
                    mean_es_unempl = mean(es_unempl, na.rm=TRUE),
                    max_es_unempl = max(abs(es_unempl), na.rm=TRUE),
                    mean_es_outcome = mean(es, na.rm=TRUE),
                    max_es_outcome = max(abs(es), na.rm=TRUE)
          ) %>%
          mutate(outcome = o)
        
        balance_statistics <- rbind(balance_statistics, bal_stats)
      }
    }
  }
  
  single_simulation$balance_statistics <- balance_statistics
  single_simulation$data <- x
  
  return(single_simulation)
}

#' TODO: documnetation
selbias_model <- function(single_simulation) {
  models <- single_simulation$models
  x <- single_simulation$data
  
  model_objects <- list()
  for (mname in names(models)) {
    m <- do.call(
      models[[mname]][["model_call"]],
      c(list(data=x, formula=models[[mname]][["model_formula"]]), models[[mname]][["model_args"]])
    )
    
    model_objects[[mname]] <- m
  }
  
  single_simulation$model_results <- model_objects
  
  return(single_simulation)
}

selbias_results <- function(single_simulation) {
  # get model result information along with params for run,
  # join with balance statistics, and return df
  results <- NULL
  for (mname in names(single_simulation$model_results)) {
    cf <- summary(single_simulation$model_results[[mname]])$coefficients
    est <- cf[which(rownames(cf) == "treatment"), "Estimate"]
    
    r <- data.frame(
      outcome = model_terms(single_simulation$models[[mname]][["model_formula"]])[["lhs"]],
      estimate = est,
      model_call = single_simulation$models[[mname]][["model_call"]],
      model_formula = Reduce(paste, trimws(deparse(single_simulation$models[[mname]][["model_formula"]]))),
      policy_speed = single_simulation$policy_speed,
      n_implementation_years = single_simulation$n_implementation_periods,
      b0 = single_simulation$b_vals.b0,
      b1 = single_simulation$b_vals.b1,
      b2 = single_simulation$b_vals.b2
    )
    
    results <- rbind(results, r)
  }
  
  results <- left_join(single_simulation$balance_statistics, results, by="outcome")
  
  return(results)
}


