# Minimum example: Debiased AR
# Max Griswold
# 2/3/2025

library(data.table)
library(fixest)

df <- fread("test_sim.csv")

true_effects <- c(1.2554486, 1.0043589, 0.7532691, 0.5021794, 0.2510897, 0.0000000)

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
    
    # First date is the reference category.
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
    
    # Add deltas to specification
    delta_terms <- c()
    for (i in 1:length(delta)){
      delta_terms[i] <- paste0(delta[i], "*(", y_t[i + 1], " - ", 
                               paste0(theta_t, "*", a_t[(i + 1):(i + lag_num)], 
                                      collapse = " - "), ")")
    }
    
    delta_terms <- paste0(delta_terms, collapse = " + ")
    
    theta_terms <- paste0(theta_t, "*", a_t[1:lag_num], collapse = " + ")
    
    form <- paste0(y_t[1], " ~ beta_0 + ", fe_year, " + ", delta_terms, " + ", theta_terms)
    
    # Add betas for covariates to specification
    if (!is.null(cov_names)){
      
      beta_terms <- sprintf("beta_%s", 1:length(cov_names))
      beta_terms <- paste0(paste0(beta_terms, "*", cov_names), collapse = " + ")
      
      form <- paste0(form, " + ", beta_terms)
      
    }
  }
  
  return(form)
  
}

# Estimate debiased AR model.

autoreg_debiased <- function(data, lags, outcome_name, 
                             unit_name, date_name, cov_names = NULL){
  
  df <- setDT(data)
  
  treat_lag <- paste0("a_t", 0:(lags + lags - 2))
  df[, (treat_lag) := shift(trt_ind, n = 0:(lags + lags - 2))]
  
  outcome_lag <- paste0("y_t", 0:lags)
  df[, (outcome_lag) := shift(get(outcome_name), n = 0:lags)]
  
  df[, (date_name) := as.factor(get(date_name))]
  
  # Get initial starting conditions using a linear model
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
  ar_model <- mod(df, lags, interact = T, date_name = date_name,
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

  
  mod_fit <- try_nls(ar_model, df)
  
  return(mod_fit)
  
}

# Run model and use estimated parameters to calculate time-varying effects.

# Specify number of lags
l <- 6

m <- autoreg_debiased(df, lags = l, outcome_name = "crude.rate",
                      unit_name = "state", date_name = "year",
                      cov_names = "unemploymentrate")

mod_fit <- summary(m)

eff_interest <- paste0("theta_t", 1:l)
thetas <- coef(mod_fit)[eff_interest, "Estimate"]

# Convert to time-varying effects:

estimated_effects <- c()
for (i in 1:length(thetas)){
  estimated_effects[i] <- sum(thetas[1:i])
}

# Get variance-covariance matrix, then calculate joint se.
# Simply calculating se of summed RV.
# e.g., b1 + b2 = sqrt(var[b1] + var[b2] + 2Cov[b1, b2])
estimated_se <- c()

vcov_mat <- vcov(m)
vcov_reduced <- which(rownames(vcov_mat) %in% eff_interest)
vcov_mat <- vcov_mat[vcov_reduced, vcov_reduced, drop = F]

sum_se <- function(i, vcov_mat){
  
  var_sum <- sum(vcov_mat[1:i])
  cov_sum <- 2*sum(vcov_mat[1:i][upper.tri(vcov_mat[1:i])])
  
  return(sqrt(var_sum + cov_sum))
  
}

for (i in 1:length(thetas)){
  estimated_se[i] <- sum_se(i, vcov_mat)
}

print(estimated_effects)
print(estimated_se)
