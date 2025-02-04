require(fixest)

# Create model specification. 

mod <- function(df, lag_num, date_name, cov_names = NULL, linear_mod = F){
  
  df <- setDT(df)
  
  if (linear_mod == F){
    y_t <- sprintf("y_t%s", 0:(lag_num - 1))
    a_t <- sprintf("a_t%s", 1:(lag_num))
    
    form <- paste0(y_t[1], sprintf(" ~ factor(%s) + ", date_name), 
                   paste0(y_t[2:lag_num], collapse = " + "), " + ", 
                   paste0(a_t, collapse = " + "))
                   
    if (!is.null(cov_names)){
      form <- paste0(form, " + ", paste0(cov_names, collapse = " + "))
    }
    
  }else{
    
    # First date is the reference category. Depending on number of lags,
    # we also need to trim off observations at the start of the time series
    dates   <- unique(df[, get(date_name)])[-1]
    
    fe_year <- sprintf("alpha_%s", 1:length(dates))
    fe_year <- paste0(paste0(fe_year, "*(year == ", dates, ")"), collapse = " + ")
    
    # Outcome lags
    y_t <- sprintf("y_t%s", 0:(lag_num - 1))
    
    # Delta terms
    delta <- sprintf("delta_%s", 1:(lag_num - 1))
    
    # Time-varying treatment terms
    theta_t <- sprintf("theta_t%s", 1:lag_num)
    a_t     <- sprintf("a_t%s", 0:(lag_num + lag_num - 2))
    
    # Create deltas:
    delta_terms <- c()
    for (i in 1:length(delta)){
      delta_terms[i] <- paste0(delta[i], "*(", y_t[i + 1], " - ", 
                               paste0(theta_t, "*", a_t[(i + 1):(i + lag_num)], 
                                      collapse = " - "), ")")
    }
    
    delta_terms <- paste0(delta_terms, collapse = " + ")
    
    theta_terms <- paste0(theta_t, "*", a_t[1:lag_num], collapse = " + ")
    
    form <- paste0(y_t[1], " ~ beta_0 + ", fe_year, " + ", delta_terms, " + ", theta_terms)
    
    # Add betas for covariates
    if (!is.null(cov_names)){
      
      beta_terms <- sprintf("beta_%s", 1:length(cov_names))
      beta_terms <- paste0(paste0(beta_terms, "*", cov_names), collapse = " + ")
      
      form <- paste0(form, " + ", beta_terms)
      
    }
  }
  
  return(form)
  
}

# Estimate debiased AR model.

autoreg_debiased <- function(data, lags, interact = T, 
                        outcome_name, unit_name, date_name,
                        cov_names = NULL){
  
  df <- setDT(data)

  treat_lag <- paste0("a_t", 0:(lags + lags - 2))
  df[, (treat_lag) := shift(trt_ind, n = 0:(lags + lags - 2))]
  
  outcome_lag <- paste0("y_t", 0:lags)
  df[, (outcome_lag) := shift(get(outcome_name), n = 0:lags)]
  
  df[, (date_name) := as.factor(get(date_name))]
  
  # Get initial starting conditions
  
  init_values <- coef(lm(mod(df, lags, interact = F, 
                             date_name = date_name,
                             cov_names = cov_names), data = df))
  
  term_names <- c("beta_0", 
                  sprintf("alpha_%s", 1:(nlevels(df[, get(date_name)]) - 1)),
                  sprintf("delta_%s", 1:(lags - 1)),
                  sprintf("theta_t%s", 1:lags))
  
  if (!is.null(cov_names)){
    term_names <- c(term_names, sprintf("beta_%s", 1:length(cov_names)))
  }
  
  names(init_values) <- term_names
  
  # Using initial try to fit model over 200
  # iterations, with smallish steps.
  ar_model <- mod(df, lags, linear_mod = T, date_name = date_name,
                  cov_names = cov_names)
  
  try_nls <- function(mod, df){
    res <- tryCatch(
      {
        nls(mod, 
            start = init_values, 
            data = df,
            control = nls.control(minFactor = 1e-4, maxiter = 200)) 
      },
      error = function(e){
        return(e)
      }
    )
    return(res)
  }
  
  
  return(mod_fit)
  
}
