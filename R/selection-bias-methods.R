

#------------------------------------------------------------------------------#
# OPTIC R Package Code Repository
# Copyright (C) 2023 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

#######################
### SAMPLING METHOD ###
#######################

#' perform sampling and coding of treatment for selection bias simulations
#' 
#' @description uses values of b0, b1, b2 to sample treated units based on
#'     values of two covariates (here moving average and the variable passed under the "var_conf" parameter) to induce confounding (selection) bias.
#'     Once treated units are identified, codes level and change version of
#'     treatment that are used in various modeling approaches later on.
#'
#' @param single_simulation object created from SimConfig$setup_single_simulation()
#' 
#' @noRd
selbias_sample <- function(single_simulation) {
  
  # get simulation specifications
  x <- single_simulation$data
  pc <- single_simulation$prior_control
  unit_var <- single_simulation$unit_var
  time_var <- single_simulation$time_var
  conf_var <- single_simulation$conf_var
  effect_magnitude <- single_simulation$effect_magnitude
  effect_direction <- single_simulation$effect_direction
  policy_speed <- single_simulation$policy_speed
  number_implementation_years <- as.numeric(single_simulation$n_implementation_periods)
  model_type <- names(single_simulation$models)
  
  # since this is happening before the model loop in run_iteration, we need to 
  # make sure we augment all unique outcomes used by models
  if (all(model_type != "did")) {
    outcomes <- unique(sapply(single_simulation$models, function(x) { optic::model_terms(x[["model_formula"]])[["lhs"]] }))
  } else if (any(model_type == 'did')) {
    outcomes <- as.character(single_simulation$models$did$model_args$yname)
  }
  
  bias_vals <- single_simulation$globals[["bias_vals"]][[single_simulation$bias_type]][[single_simulation$prior_control]][[single_simulation$bias_size]]
  
  # parameters for outcome augmentation
  a1 <- bias_vals["a1"] 
  a2 <- bias_vals["a2"]
  a3 <- bias_vals["a3"]
  a4 <- bias_vals["a4"]
  a5 <- bias_vals["a5"]
  
  # parameters for treatment selection
  b0 <- bias_vals["b0"]
  b1 <- bias_vals["b1"]
  b2 <- bias_vals["b2"]
  b3 <- bias_vals["b3"]
  b4 <- bias_vals["b4"]
  b5 <- bias_vals["b5"]
  
  # save values of prior control variable prior to augmenting the outcome
  if (pc == "level") {
    x$prior_control <- x$prior_control_level_OLD
    x$prior_control_old <- x$prior_control_level_OLD
  } else if (pc == "trend") {
    x$prior_control <- x$prior_control_trend_OLD
    x$prior_control_old <- x$prior_control_trend_OLD
  } else {
    stop("invalid prior control option, must be either 'level' or 'trend'")
  }
  
  # unique study units
  available_units <- unique(x[[unit_var]])
  
  # unique study time periods, excluding the first 3 which are used for prior control
  available_time_periods <- sort(unique(x[[time_var]]))[-1:-3]
  
  # Generate treatment assignments one year at a time
  for (t in 1:length(available_time_periods)) {
    
    # subset the data to this year
    time = available_time_periods[t]
    
    x_t <- x[x[[time_var]]==time,]
    
    # generate treatment assignments
    logits <- b0 +
      (b1 * x_t$prior_control) +
      (b2 * x_t[[conf_var]]) +
      (b3 * (x_t$prior_control * x_t[[conf_var]])) +
      (b4 * (x_t$prior_control ^ 2)) +
      (b5 * (x_t[[conf_var]] ^ 2))
    
    trt_pr <- exp(logits) / (1 + exp(logits))
    
    x_t$trt_ind = sapply(trt_pr, function(p) sample(c(0, 1), 1, prob = c(1 - p, p)))
    
    # add the modified data for this time period to the full dataset
    x <- dplyr::bind_rows(x[x[[time_var]]!=time,], x_t) %>%
      arrange(!!as.name(unit_var), !!as.name(time_var))
    
  }
  
  # For the first three time periods, treatment indicator will be missing. 
  # Set to 0.
  x$trt_ind[x[[time_var]] %in% sort(unique(x[[time_var]]))[1:3]] <- 0
  
  # For treated units, set the treatment indicator to 1 from the 
  # earliest treatment date onwards
  x$trt_ind <- unlist(tapply(x$trt_ind, x[[unit_var]], 
                             function(x) pmin(1, cumsum(x))))
  
  # get the time of first treatment for each unit
  # min() automatically returns Inf for untreated units
  # Here we suppress warnings because that's the behavior we want.
  
  # Minimum time period for treated units
  suppressWarnings({
    time_periods <- unlist(tapply(x[[time_var]] * x$trt_ind, x[[unit_var]],
                                  function(x) min(x[x > 0])))  
  })
  
  # calculate the number of treatment periods for each unit
  n_treated <- unlist(tapply(x$trt_ind, x[[unit_var]], sum))
  
  # subset to only the treated units
  sampled_units <- available_units[n_treated > 0]
  start_periods <- time_periods[n_treated > 0]
  
  # for treated units, randomly pick an implementation month and
  # calculate the amount of exposure based on implementation speed
  if (length(sampled_units) > 0) {
    
    treated <- list()
    
    for (i in 1:length(sampled_units)) { 
      
      yr <- start_periods[i]
      current_unit <- as.character(sampled_units[i])
      mo <- sample(1:12, 1)
      
      if (policy_speed == "slow") {
        treated[[current_unit]] <- list(
          policy_years = yr:max(x$year, na.rm=TRUE),
          policy_month = mo,
          exposure = optic::calculate_exposure(mo, number_implementation_years),
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
    
    # create a new treatment indicator variable based on the policy exposure
    policy_dates = purrr::transpose(treated)$policy_date %>% 
      dplyr::bind_rows() %>% 
      tidyr::gather(!!unit_var, treatment_date) 
    
    if (is.factor(x[[unit_var]])) {
      policy_dates = policy_dates %>%
        dplyr::mutate_if(is.character, factor)
    } else{
      policy_dates = policy_dates %>%
        dplyr::mutate_if(is.character, as.double)
    }
    
    x_policy_info <- x[x$trt_ind==1, c(unit_var, time_var)]
    
    x_policy_info$treatment = purrr::transpose(treated)$exposure %>% unlist
    
    x_policy_info = x_policy_info %>%
      dplyr::left_join(., policy_dates, by = unit_var)
    
    # merge the new treatment indicator into the data
    x = x %>%
      dplyr::left_join(., x_policy_info, by = c(unit_var, time_var)) %>%
      dplyr::mutate(treatment = case_when(is.na(treatment) ~ 0,
                                          TRUE ~ treatment),
                    treatment_date = case_when(is.na(treatment_date) ~ as.Date(NA),
                                               TRUE ~ treatment_date))
  } else {
    x$treatment <- 0
    x$treatment_date <- as.Date(NA)
  }
  
  # Augment outcomes one year at a time
  for (t in 1:length(available_time_periods)) {
    
    # subset the data to this year
    time = available_time_periods[t]
    
    x_t <- x[x[[time_var]]==time,]
    
    # augment outcomes
    x_t[[outcomes]] <- x_t[[outcomes]] +
      (a1 * x_t$prior_control) +
      (a2 * x_t[[conf_var]]) +
      (a3 * (x_t$prior_control * x_t[[conf_var]])) +
      (a4 * (x_t$prior_control ^ 2)) +
      (a5 * (x_t[[conf_var]] ^ 2)) +
      (-1)^(effect_direction=='neg') * effect_magnitude * x_t$treatment
    
    # add the modified data for this time period to the full dataset
    x <- dplyr::bind_rows(x[x[[time_var]]!=time,], x_t) %>%
      arrange(!!as.name(unit_var), !!as.name(time_var))
    
    # update prior control
    x$lag1 <- unlist(tapply(x[[outcomes]], x[[unit_var]], function(x) dplyr::lag(x, n=1)), use.names = F)
    x$lag2 <- unlist(tapply(x[[outcomes]], x[[unit_var]], function(x) dplyr::lag(x, n=2)), use.names = F)
    x$lag3 <- unlist(tapply(x[[outcomes]], x[[unit_var]], function(x) dplyr::lag(x, n=3)), use.names = F)
    
    if(pc == 'level'){
      x$prior_control = (x$lag1 + x$lag2 + x$lag3) / 3
    } else if(pc == 'trend'){
      x$prior_control = x$lag1 - x$lag3
    } else{
      stop("invalid prior control option, must be either 'level' or 'trend'")
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
#' 
#' @noRd
selbias_premodel <- function(model_simulation) {
  x <- model_simulation$data
  model <- model_simulation$models
  conf <- dplyr::sym(model_simulation$conf_var)
  
  if (model$type != "did") {
    outcome <- optic::model_terms(model$model_formula)[["lhs"]]
    oo <- dplyr::sym(outcome)
  } else if (model$type=='did') {
    outcome <- as.character(model_simulation$models$model_args$yname)
    oo <- dplyr::sym(outcome)
  }
  model_type <- model$type
  balance_statistics <- NULL
  
  # PNL note
  # this implementation does not seem to use 
  # the lagged crude rate
  # It uses the lagged outcome.

  # if autoregressive, need to add lag for crude rate
  # when outcome is deaths, derive new crude rate from modified outcome
  if (model_type == "autoreg") {
    
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
  } else if (model_type == "did") {
    x$treatment[x$treatment > 0] <- 1
    x$treatment_level[x$treatment_level > 0] <- 1
    x <- x %>% 
      group_by(!!unit_sym) %>%
      mutate(treatment_year = 1 + max(year ^ (1-treatment))) %>% 
      ungroup()
  }
  
  # get balance information
  bal_stats <- x %>%
    group_by(year) %>%
    summarize(
      n_trt = sum(treatment > 0),
      mu1_prior = mean(prior_control[treatment > 0]),
      mu0_prior = mean(prior_control[treatment == 0]),
      sd_prior = sd(prior_control),
      mu1_conf = mean((!!conf)[treatment > 0]),
      mu0_conf = mean((!!conf)[treatment == 0]),
      sd_conf = sd(!!conf),
      mu1 = mean((!!oo)[treatment > 0]),
      mu0 = mean((!!oo)[treatment == 0]),
      sd = sd(!!oo),
      .groups="keep"
    ) %>%
    mutate(
      es_prior = (mu1_prior - mu0_prior) / sd_prior,
      es_conf = (mu1_conf - mu0_conf) / sd_conf,
      es = (mu1 - mu0) / sd
    ) %>%
    ungroup() %>%
    summarize(n = max(n_trt, na.rm=TRUE),
              mean_es_prior = mean(es_prior, na.rm=TRUE),
              max_es_prior = max(abs(es_prior), na.rm=TRUE),
              mean_es_conf = mean(es_conf, na.rm=TRUE),
              max_es_conf = max(abs(es_conf), na.rm=TRUE),
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
#' @noRd
selbias_model <- function(model_simulation) {
  model <- model_simulation$models
  x <- model_simulation$data
  
  if (model_simulation$models$type %in% c("did", "multisynth")) {
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
#' @noRd
selbias_postmodel <- function(model_simulation) {
  
  
  if (model_simulation$models$type != "did") {
    outcome <- model_terms(model_simulation$models[["model_formula"]])[["lhs"]]
  } else if (model_simulation$models$type == "did") {
    outcome <- as.character(model_simulation$models$model_args$yname)
  }
  
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
    effect_direction = model_simulation$effect_direction,
    effect_magnitude = model_simulation$effect_magnitude,
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
  if (!(model_simulation$models[["type"]] %in% c("multisynth","did"))) {
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
      # mse=mean(m[["residuals"]]^2, na.rm=T),
      stringsAsFactors=FALSE
    )
  } else if (model_simulation$models[["type"]] == "multisynth") {
    m <- model_simulation$model_result
    cf <- summary(m)
    cf <- cf$att
    estimate <- cf[cf$Level == "Average" & is.na(cf$Time), "Estimate"]
    se <- cf[cf$Level == "Average" & is.na(cf$Time), "Std.Error"]
    variance <- se ^ 2
    t_stat <- NA
    p_value <- 2 * pnorm(abs(estimate/se), lower.tail = FALSE)
    # mse <- mean(unlist(lapply(m$residuals, function(x) {mean(x^2)})))
    
    r <- data.frame(
      outcome=outcome,
      se_adjustment="none",
      estimate=estimate,
      se=se,
      variance=variance,
      t_stat=NA,
      p_value=p_value,
      # mse=mse,
      stringsAsFactors=FALSE
    )
  } else if (model_simulation$models[["type"]] == "did") {
    m <- model_simulation$model_result
    m_agg <- did::aggte(m, type='dynamic', na.rm=T)
    cf <- summary(m_agg, returnOutput = TRUE, verbose = FALSE)[[1]]
    colnames(cf) <- trimws(colnames(cf))
    estimate <- cf$ATT
    se <- cf[["Std. Error"]]
    variance <- se ^ 2
    t_stat <- NA
    p_value <- 2 * pnorm(abs(estimate/se), lower.tail = FALSE)
    # mse <- mean(unlist(lapply(m$residuals, function(x) {mean(x^2)})))
    
    r <- data.frame(
      outcome=outcome,
      se_adjustment="none",
      estimate=estimate,
      se=se,
      variance=variance,
      t_stat=NA,
      p_value=p_value,
      # mse=mse,
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
      # mse=mean(m[["residuals"]]^2, na.rm=T),
      stringsAsFactors=FALSE
    )
    
    r <- rbind(r, h_r)
    rownames(r) <- NULL
  }
  
  if ("cluster" %in% model_simulation$models[["se_adjust"]]) {
    clust_indices <- as.numeric(rownames(m$model))
    clust_var <- as.character(model_simulation$data[[model_simulation$unit_var]][clust_indices])
    clust_coeffs <- cluster_adjust_se(m, clust_var)[[2]]
    
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
      # mse=mean(m[["residuals"]]^2, na.rm=T),
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
      # mse=mean(m[["residuals"]]^2, na.rm=T),
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
#' @noRd
selbias_results <- function(r) {
  return(do.call(rbind, r))
}


#' @title summary.MP
#'
#' @description prints a summary of a \code{MP} object.
#' This modifies the version in the `did` library to return
#' output as a data frame.
#' Code copied from https://github.com/bcallaway11/did/blob/master/R/MP.R.
#'
#' @param object an \code{MP} object
#' @param ... extra arguments
#'
#' @noRd
#' @importFrom utils citation
summary.MP <- function(object, ..., returnOutput = FALSE, verbose = TRUE) {
  mpobj <- object
  
  # call
  if (verbose) {
    cat("\n")
    cat("Call:\n")
    print(mpobj$DIDparams$call)
    cat("\n")
    
    # citation
    citation()
    cat("\n")
    
    # group time average treatment effects
    cat("Group-Time Average Treatment Effects:\n")
  }
  
  cband_text1a <- paste0(100*(1-mpobj$alp),"% ")
  cband_text1b <- ifelse(mpobj$DIDparams$bstrap,
                         ifelse(mpobj$DIDparams$cband, "Simult. ", "Pointwise "),
                         "Pointwise ")
  cband_text1 <- paste0("[", cband_text1a, cband_text1b)
  
  cband_lower <- mpobj$att - mpobj$c*mpobj$se
  cband_upper <- mpobj$att + mpobj$c*mpobj$se
  
  sig <- (cband_upper < 0) | (cband_lower > 0)
  sig[is.na(sig)] <- FALSE
  sig_text <- ifelse(sig, "*", "")
  
  out <- cbind.data.frame(mpobj$group, mpobj$t, mpobj$att, mpobj$se, cband_lower, cband_upper)
  out <- round(out,4)
  out <- cbind.data.frame(out, sig_text)
  
  
  colnames(out) <- c("Group", "Time", "ATT(g,t)","Std. Error", cband_text1, "Conf. Band]", "")
  
  if (verbose) {
    print(out, row.names=FALSE)
    cat("---\n")
    cat("Signif. codes: `*' confidence band does not cover 0")
    cat("\n\n")
  }
  
  # report pre-test
  if (!is.null(mpobj$Wpval) & verbose) {
    cat("P-value for pre-test of parallel trends assumption:  ")
    cat(as.character(mpobj$Wpval))
    cat("\n")
  }
  
  
  
  # set control group text
  control_group <- mpobj$DIDparams$control_group
  control_group_text <- NULL
  if (control_group == "nevertreated") {
    control_group_text <- "Never Treated"
  } else if (control_group == "notyettreated") {
    control_group_text <- "Not Yet Treated"
  }
  
  if (!is.null(control_group) & verbose) {
    cat("Control Group:  ")
    cat(control_group_text)
    cat(",  ")
  }
  
  if (verbose) {
    # anticipation periods
    cat("Anticipation Periods:  ")
    cat(mpobj$DIDparams$anticipation)
    cat("\n")
  }
  
  # estimation method text
  est_method <- mpobj$DIDparams$est_method
  cl_est_method <- as.character(class(est_method))
  if (cl_est_method=="character") {
    est_method_text <- est_method
    if (est_method == "dr") {
      est_method_text <- "Doubly Robust"
    } else if (est_method == "ipw") {
      est_method_text <- "Inverse Probability Weighting"
    } else if (est_method == "reg") {
      est_method_text <- "Outcome Regression"
    }
    
    if (verbose) {
      cat("Estimation Method:  ")
      cat(est_method_text)
      cat("\n")
    }
    
  }
  
  if (returnOutput) return(out)
}


#' @title Summary Aggregate Treatment Effect Parameter Objects
#'
#' @description A function to summarize aggregated treatment effect parameters.
#' This modifies the version in the `did` library to return
#' output as a data frame.
#' Code copied from https://github.com/bcallaway11/did/blob/master/R/AGGTEobj.R.
#'
#' @param object an \code{AGGTEobj} object
#' @param ... other arguments
#'
#' @noRd
#' @importFrom stats qnorm
summary.AGGTEobj <- function(object, ..., returnOutput = FALSE, verbose = TRUE) {
  
  if (verbose) {
    # call
    cat("\n")
    cat("Call:\n")
    print(object$call)
    cat("\n")
    
    #citation
    citation()
    cat("\n")
  }
  
  # overall estimates
  alp <- object$DIDparams$alp
  pointwise_cval <- qnorm(1-alp/2)
  overall_cband_upper <- object$overall.att + pointwise_cval*object$overall.se
  overall_cband_lower <- object$overall.att - pointwise_cval*object$overall.se
  out1 <- cbind.data.frame(object$overall.att, object$overall.se, overall_cband_lower, overall_cband_upper)
  out1 <- round(out1, 4)
  overall_sig <- (overall_cband_upper < 0) | (overall_cband_lower > 0)
  overall_sig[is.na(overall_sig)] <- FALSE
  overall_sig_text <- ifelse(overall_sig, "*", "")
  out1 <- cbind.data.frame(out1, overall_sig_text)
  colnames(out1) <- c("ATT","   Std. Error", paste0("    [ ",100*(1-object$DIDparams$alp),"% "), "Conf. Int.]","")
  
  if (verbose) {
    cat("\n")
    #cat("Overall ATT:  \n")
    if (object$type=="dynamic") cat("Overall summary of ATT\'s based on event-study/dynamic aggregation:  \n")
    if (object$type=="group") cat("Overall summary of ATT\'s based on group/cohort aggregation:  \n")
    if (object$type=="calendar") cat("Overall summary of ATT\'s based on calendar time aggregation:  \n")
    print(out1, row.names=FALSE)
    cat("\n\n")
  }
  
  
  # handle cases depending on type
  if (object$type %in% c("group","dynamic","calendar")) {
    
    # header
    if (object$type=="dynamic") { c1name <- "Event time"; if (verbose) cat("Dynamic Effects:") }
    if (object$type=="group") { c1name <- "Group"; if (verbose) cat("Group Effects:") }
    if (object$type=="calendar") { c1name <- "Time"; if (verbose) cat("Time Effects:") }
    
    if (verbose) cat("\n")
    cband_text1a <- paste0(100*(1-object$DIDparams$alp),"% ")
    cband_text1b <- ifelse(object$DIDparams$bstrap,
                           ifelse(object$DIDparams$cband, "Simult. ", "Pointwise "),
                           "Pointwise ")
    cband_text1 <- paste0("[", cband_text1a, cband_text1b)
    
    cband_lower <- object$att.egt - object$crit.val.egt*object$se.egt
    cband_upper <- object$att.egt + object$crit.val.egt*object$se.egt
    
    sig <- (cband_upper < 0) | (cband_lower > 0)
    sig[is.na(sig)] <- FALSE
    sig_text <- ifelse(sig, "*", "")
    
    out2 <- cbind.data.frame(object$egt, object$att.egt, object$se.egt, cband_lower, cband_upper)
    out2 <- round(out2, 4)
    out2 <- cbind.data.frame(out2, sig_text)
    
    colnames(out2) <- c(c1name, "Estimate","Std. Error", cband_text1, "Conf. Band]", "")
    if (verbose) print(out2, row.names=FALSE, justify = "centre")
  }
  
  if (verbose) {
    cat("---\n")
    cat("Signif. codes: `*' confidence band does not cover 0")
    cat("\n\n")
  }
  
  # set control group text
  control_group <- object$DIDparams$control_group
  control_group_text <- NULL
  if (control_group == "nevertreated") {
    control_group_text <- "Never Treated"
  } else if (control_group == "notyettreated") {
    control_group_text <- "Not Yet Treated"
  }
  
  if (!is.null(control_group) & verbose) {
    cat("Control Group:  ")
    cat(control_group_text)
    cat(",  ")
  }
  
  if (verbose) {
    # anticipation periods
    cat("Anticipation Periods:  ")
    cat(object$DIDparams$anticipation)
    cat("\n")
  }
  
  # estimation method text
  est_method <- object$DIDparams$est_method
  cl_est_method <- as.character(class(est_method))
  if (cl_est_method=="character") {
    est_method_text <- est_method
    if (est_method == "dr") {
      est_method_text <- "Doubly Robust"
    } else if (est_method == "ipw") {
      est_method_text <- "Inverse Probability Weighting"
    } else if (est_method == "reg") {
      est_method_text <- "Outcome Regression"
    }
    
    if (verbose) {
      cat("Estimation Method:  ")
      cat(est_method_text)
      cat("\n")
    }
    
  }
  
  if (returnOutput) return(list(out1, out2))
}
