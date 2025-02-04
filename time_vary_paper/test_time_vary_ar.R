# Test out AR specifications for estimating time-varying effects
# Max Griswold

rm(list = ls())

# Using this package purely for the convenience function "i" to create binary indicators
library(fixest)         
library(data.table)
library(ggplot2)

setwd("C:/users/griswold/documents/Github/optic/time_vary_plots/test_data/")

# Get a single simulation run.
# This corresponds to scenario where SME increases from t = 0 to t = 5 by:
# 0, 0.25, 0.5, 0.75, 1, 1.25

dd <- fread("test_data_full.csv")
true_effects <- seq(0, -1.25, -.25)

mod <- function(data, lag_num, lin = F){
  
  if (lin){
    y_t <- sprintf("y_t%s", 0:(lag_num - 1))
    a_t <- sprintf("a_t%s", 1:(lag_num))
    
    form <- paste0(y_t[1], " ~ factor(year) + ", 
                   paste0(y_t[2:lag_num], collapse = " + "), " + ", 
                   paste0(a_t, collapse = " + "))
  }else{
    # First year is the reference category
    years   <- unique(data$year)[-1]
    fe_year <- sprintf("alpha_%s", 1:length(years))
    fe_year <- paste0(paste0(fe_year, "*(year == ", years, ")"), collapse = " + ")
    
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
      delta_terms[i] <- paste0(delta[i], "*(", y_t[i + 1], " - ", paste0(theta_t, "*", a_t[(i + 1):(i + lag_num)], collapse = " - "), ")")
    }
    delta_terms <- paste0(delta_terms, collapse = " + ")
    
    theta_terms <- paste0(theta_t, "*", a_t[1:lag_num], collapse = " + ")
    
    form <- paste0(y_t[1], " ~ beta_0 + ", fe_year, " + ", delta_terms, " + ", theta_terms)
  }
  
  return(form)
  
}

run_models <- function(it){
  
  df <- dd[iter == it, ]
  
  print(sprintf("Running model %s", it))
  
  # Create binary treatment variable:
  df[, treat := ifelse(time_to_treat != Inf & time_to_treat >= 0, 1, 0)]
  
  # Create 6 treatment change variables.
  # These equal 1 if the policy was passed X years ago; 0 otherwise.
  # Following the example in Schell, Griffin, Morral, 2018; along w/
  # explanation in Cefalu et al., 2021.
  
  # https://www.rand.org/pubs/research_reports/RR2685.html
  # https://arxiv.org/pdf/2109.03225
  
  for (i in 0:5){
    var <- paste0("treat_change_", i)
    df[, (var) := ifelse(time_to_treat == i, 1, 0)]
  }
  
  df[, lag_outcome := shift(crude.rate)]
  
  # Test out several specifications & pull out estimates of treatment effects.
  # For now, I'm not worrying about standard errors & clustering - I'm only trying
  # to figure out which specification best recovers point estimates
  
  # Model 1: Event-study approach, -1 as reference year, using one lag and treatment levels:
  
  mod1 <- formula("crude.rate ~ unemploymentrate + lag_outcome + i(time_to_treat, ref = -1) + as.factor(year)")
  
  res1 <- summary(lm(mod1, df))
  
  eff_interest <- paste0("i(time_to_treat, ref = -1)", 0:5)
  est_effs1 <- coef(res1)[eff_interest, "Estimate"]
  
  # Model 2: Same as 1, following Sun and Abraham to include additional reference period:
  
  mod2 <- formula("crude.rate ~ unemploymentrate + lag_outcome + i(time_to_treat, ref = c(-1, Inf)) + as.factor(year)")
  
  res2 <- summary(lm(mod2, df))
  
  eff_interest <- paste0("i(time_to_treat, ref = c(-1, Inf))", 0:5)
  est_effs2 <- coef(res2)[eff_interest, "Estimate"]
  
  # Model 3: Same as 2 but setting all time_to_treat prior to treatment as -1:
  df[, time_to_treat_no_pre_period := ifelse(time_to_treat <= -1, -1, time_to_treat)]
  
  mod3 <- formula("crude.rate ~ unemploymentrate + lag_outcome + i(time_to_treat_no_pre_period, ref = c(-1, Inf)) + as.factor(year)")
  
  res3 <- summary(lm(mod3, df))
  
  eff_interest <- paste0("i(time_to_treat_no_pre_period, ref = c(-1, Inf))", 0:5)
  est_effs3 <- coef(res3)[eff_interest, "Estimate"]
  
  # Model 4: Only using binary treatment change variables:
  add_terms <- paste0("treat_change_", 0:5, collapse = " + ")
  mod4 <- formula(paste0("crude.rate ~ unemploymentrate + lag_outcome + as.factor(year) + ", add_terms))
  
  res4 <- summary(lm(mod4, df))
  
  eff_interest <- paste0("treat_change_", 0:5)
  est_effs4 <- coef(res4)[eff_interest, "Estimate"]
  
  # Model 5: Estimate using specification provided by Max Rubinstein:
  
  # Create additional outcome and policy lags for this model:
  lags <- 6
  
  treat_lag <- paste0("a_t", 0:(lags + lags - 2))
  df[, (treat_lag) := shift(treat, n = 0:(lags + lags - 2))]
  
  outcome_lag <- paste0("y_t", 0:lags)
  df[, (outcome_lag) := shift(crude.rate, n = 0:lags)]
  
  df[, year := as.factor(year)]
  
  # Get initial starting conditions based off LM model:
  init_values <- coef(lm(mod(data, lags, lin = T), data = df))
  names(init_values) <- c("beta_0", 
                          sprintf("alpha_%s", 1:(nlevels(df$year) -1)),
                          sprintf("delta_%s", 1:(lags - 1)),
                          sprintf("theta_t%s", 1:lags))
  
  fit_mod <- try({nls(mod(df, lags), 
                 start = init_values, 
                 data = df,
                 control = nls.control(minFactor = 1e-20, maxiter = 200))})
  
  if (class(fit_mod) == "try-error"){
    est_effs5 <- rep(NA, 6)
  }else{
    res5 <- summary(fit_mod)
    
    eff_interest <- paste0("theta_t", 1:lags)
    thetas <- coef(res5)[eff_interest, "Estimate"]
    
    # Convert to time-varying effects:
    est_effs5 <- c()
    for (i in 1:length(thetas)){
      est_effs5[i] <- sum(thetas[1:i])
    }
  }

  # Combine results and plot:
  df_res <- data.table("iter" = it,
                       "time_period" = 0:5,
                       "true_effect" = true_effects,
                       # "event_study_one_reference_period" = est_effs1,
                       # "event_study_two_reference_periods" = est_effs2,
                       "ar_no_preperiod_terms" = est_effs3,
                       "treatment_change_coding" = est_effs4,
                       "debiased_ar" = est_effs5)
  
  return(df_res)
  
}

res <- rbindlist(lapply(1:1000, run_models))
res <- melt(res, id.vars = c("iter", "time_period"), variable.name = "model", value.name = "effect")

res[, mean_effect := mean(effect, na.rm = T), by = c("model", "time_period")]
res[, lower := quantile(effect, probs = 0.025, na.rm = T), by = c("model", "time_period")]
res[, upper := quantile(effect, probs = 0.975, na.rm = T), by = c("model", "time_period")]

res <- res[model %in% c("true_effect", "Debiased AR", "AR (no preperiods)")]

ggplot(res, aes(x = time_period, y = mean_effect, color = model)) +
  geom_point(position = position_dodge(width = 0.8)) +
  geom_linerange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.8)) +
  theme_bw() 
