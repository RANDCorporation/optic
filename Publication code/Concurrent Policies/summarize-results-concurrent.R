# summarize-results-concurrent.R ------------------------------------------
# 
# Summarize the concurrent simulation output so that it can be used to 
# produce the graphics for the report, and to be used in the Shiny app.
# 
# Adam Scherling, 2/15/2022
# Based on prior code by Joe Pane


# set up directories ------------------------------------------------------

revisionPath <- ''


# load libraries ----------------------------------------------------------

library(arrow)
library(dplyr)
library(glue)
library(tidyr)


# load data ---------------------------------------------------------------

files_to_summarize <- c(
  # "slow_unordered" = "from_BethAnn_2.8.2022/concurrent-lm-scen1-slow-30-notordered2022-02-07.csv",
  # "slow_ordered" = "from_BethAnn_2.8.2022/concurrent-lm-scen1-slow-30-ordered2022-02-07.csv",
  # "n30_nextra5_unordered" = "from_BethAnn_2.8.2022/concurrent_nextra_n30_nextra5_notordered2022-02-11.csv",
  # "n30_nextra5_ordered" = "from_BethAnn_2.8.2022/concurrent_nextra_n30_nextra5_ordered2022-02-11.csv",
  # "n30_nextra10_unordered" = "from_BethAnn_2.8.2022/concurrent_nextra_n30_nextra10_notordered2022-02-11.csv",
  # "n30_nextra10_ordered" = "from_BethAnn_2.8.2022/concurrent_nextra_n30_nextra10_ordered2022-02-11.csv",
  # "n10_nextra10_unordered" = "from_BethAnn_2.23.2022/concurrent_nextra_n10_nextra10_notordered2022-02-23.csv",
  # "n10_nextra10_ordered" = "from_BethAnn_2.23.2022/concurrent_nextra_n10_nextra10_ordered2022-02-23.csv",
  # "n20_nextra5_unordered" = "from_BethAnn_2.23.2022/concurrent_nextra_n20_nextra5_notordered2022-02-23.csv",
  # "n20_nextra5_ordered" = "from_BethAnn_2.23.2022/concurrent_nextra_n20_nextra5_ordered2022-02-24.csv",
  # "n3_nextra1_unordered" = "from_BethAnn_2.23.2022/concurrent_nextra_n3_nextra1_notordered2022-02-23.csv",
  # "n3_nextra1_ordered" = "from_BethAnn_2.23.2022/concurrent_nextra_n3_nextra1_ordered2022-02-23.csv",
  # "lm-99to04-notordered_n30" = "from_BethAnn_2.23.2022/concurrent-lm-99to04-notordered_n302022-02-23.csv",
  # "lm-05to10-notordered_n30" = "from_BethAnn_2.23.2022/concurrent-lm-05to10-notordered_n302022-02-23.csv",
  # "lm-11to16-notordered_n30" = "from_BethAnn_2.23.2022/concurrent-lm-11to16-notordered_n302022-02-23.csv",
  # "scen0_ordered" = "replication/concurrent-lm-scen0_ordered2022-04-29.csv",
  # "scen0_unordered" = "replication/concurrent-lm-scen0_unordered2022-05-03.csv",
  # "scen1" = "replication/concurrent-lm-scen12022-05-03.csv",
  # "scen2" = "replication/concurrent-lm-scen2_2022-05-04.csv",
  # "scen3" = "replication/concurrent-lm-scen3_2022-05-05.csv",
  # "scen4" = "replication/concurrent-lm-scen4_2022-05-05.csv",
  "scen5" = "replication/concurrent-lm-scen5_2022-05-09.csv",
  "scen6" = "replication/concurrent-lm-scen6_2022-05-10.csv"
)

data_to_summarize <- lapply(files_to_summarize, function(ff) read_csv_arrow(glue("{revisionPath}/{ff}")))


# add an n_extra column, based on filenames -------------------------------

# If the name has a string like "nextra10" in it, this will pull the number.
# If the name does not have such a string, it will assign 0.

nextra <- gsub('.*nextra([0-9]*).*', '\\1', files_to_summarize)
has_nextra <- grepl('.*nextra([0-9]*).*', files_to_summarize)
nextra[!has_nextra] <- 0
nextra <- as.numeric(nextra)

for (i in 1:length(data_to_summarize)) {
  data_to_summarize[[i]]$n_extra <- nextra[i]
}


# set effect_direction to null when effect magnitudes are zero ------------

data_to_summarize <- lapply(data_to_summarize, function(dd) {
  dd$effect_direction[dd <- dd$effect_magnitude1==0 & dd$effect_magnitude2==0] <- 'null'
  return(dd)
})


# check what defines a unique run in each ---------------------------------

# each combination of model_name, se_adjustment, and years_apart has 5000 rows (iterations)
# lapply(data_to_summarize, function(dd) dd %>% count(model_name, se_adjustment, years_apart) %>% count(n)) %>%
lapply(data_to_summarize, function(dd) dd %>% count(model_name, policy_speed, ordered, rho, n_units, se_adjustment, years_apart) %>% count(n)) %>%
  bind_rows() %>% 
  mutate(sim = names(data_to_summarize)) %>% 
  select(sim, n, nn)


# define summary functions ------------------------------------------------

coverage <- function(betas, ses, cf, te=0) {
  adj_ses <- ses#*cf
  ind <- rep(0, length(betas))
  low95 <- betas - 1.96 * adj_ses
  high95 <- betas + 1.96 * adj_ses
  ind[te > low95 & te < high95] <- 1
  return(sum(ind) / length(betas))
}

#' helper for mean squared error
mse <- function(x, te, effect_direction) {
  if (effect_direction == "null") {
    (x - 0) ^ 2
  } else if(effect_direction == "neg") {
    (x - (-te))^2
  } else{
    (x - te)^2
  }
}

#' helper for bias
bias <- function(x, te, effect_direction) {
  if (effect_direction == "null") {
    x - 0
  } else if(effect_direction == "neg"){
    (x - (-te))
  } else{
    (x - te)
  }
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


#' formula for correcting p-values using correction factor
#' 
#' @param coeffs vector of regression coefficients
#' @param ses vector of standard errors related to provided coefficients
#' @param cf correction factor to use for adjustment
#' @param effect_direction direction of true effect, one of "null", "neg", "pos"
correct_rejection_rate_flag <- function(coeffs, ses, cf, effect_direction="null") {
  adj_ses <- ses #* cf
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

#' summary method for concurrent runs
summarize_concurrent <- function(x) {
  
  # need to vectorize some functions
  biasVec <- Vectorize(bias, vectorize.args = c("x", "te"))
  mseVec <- Vectorize(mse, vectorize.args = c("x", "te"))
  
  # add a couple variables to the data
  x <- x %>%
    mutate(true_effect = paste0(effect_magnitude1, ", ", effect_magnitude2))
  x$specification <- ifelse(grepl("treatment2", x$model_formula), "correct", "misspec")
  
  # summarize the data
  r <- x %>%
    summarize(
      estimate1=mean(na.omit(estimate1)),
      se1=mean(na.omit(se1)),
      variance1=mean(na.omit(variance1)),
      test_stat1=mean(na.omit(test_stat1)),
      mse1=mean(mseVec(estimate1, as.numeric(effect_magnitude1), unique(effect_direction))),
      bias1=mean(biasVec(estimate1, as.numeric(effect_magnitude1), unique(effect_direction))),
      estimate2=mean(na.omit(estimate2)),
      se2=mean(na.omit(se2)),
      variance2=mean(na.omit(variance2)),
      test_stat2=mean(na.omit(test_stat2)),
      mse2=mean(mseVec(estimate2, as.numeric(effect_magnitude2), unique(effect_direction))),
      bias2=mean(biasVec(estimate2, as.numeric(effect_magnitude2), unique(effect_direction))),
      joint.eff=mean(na.omit(joint.eff)),
      joint.eff.se=mean(na.omit(joint.eff.se)),
      joint.eff.var=mean(na.omit(variancej)),
      joint.test.stat=mean(na.omit(test_statj)),
      joint.mse=mean(mseVec(joint.eff, (as.numeric(effect_magnitude1)+as.numeric(effect_magnitude2)), unique(effect_direction))),
      joint.bias=mean(biasVec(joint.eff, (as.numeric(effect_magnitude1)+as.numeric(effect_magnitude2)), unique(effect_direction))),
      avg_min_distance=mean(min_distance),
      avg_max_distance=mean(max_distance),
      avg_mean_distance=mean(mean_distance)
    ) %>%
    mutate(
      se_adjustment=unique(x$se_adjustment),
      effect_magnitude1=as.numeric(unique(x$effect_magnitude1)),
      effect_magnitude2=as.numeric(unique(x$effect_magnitude2)),
      effect_direction=unique(x$effect_direction),
      policy_speed=unique(x$policy_speed),
      rho=unique(x$rho),
      n_units = unique(x$n_units),
      n_extra = unique(x$n_extra),
      ordered = unique(x$ordered),
      n_implementation_periods=unique(x$n_units),
      model_name=unique(x$model_name),
      model_call=unique(x$model_call),
      model_formula=unique(x$model_formula),
      specification=unique(x$specification),
      years_apart=unique(x$years_apart)
    ) %>%
    mutate(
      orderedTx = case_when(
        ordered=="yes" ~ TRUE,
        ordered=="no" ~ FALSE
      )
    )
  
  if (unique(x$effect_direction) == "null") {
    r$type1_error1 <- mean(pval_flag(x$p_value1))
    r$type1_error2 <- mean(pval_flag(x$p_value2))
    r$type1_error_joint <- mean(pval_flag(x$joint.eff.pvalue))
    
    r$type_s_error1 <- NA
    r$type_s_error2 <- NA
    r$type_s_error_joint <- NA
    
    r$coverage1 <- coverage(betas = x$estimate1, ses = x$se1, cf = 1, te = 0)
    r$coverage2 <- coverage(betas = x$estimate2, ses = x$se2, cf = 1, te = 0)
    r$coverage_joint <- coverage(betas = x$joint.eff, ses = x$joint.eff.se, cf = 1, te=0)
  } else if(unique(x$effect_direction) == "neg"){
    r$type1_error1 <- NA
    r$type1_error2 <- NA
    r$type1_error_joint <- NA
    
    r$type_s_error1 <- type_s_error(x$estimate1, x$p_value1, unique(x$effect_direction))
    r$type_s_error2 <- type_s_error(x$estimate2, x$p_value2, unique(x$effect_direction))
    r$type_s_error_joint <- type_s_error(x$joint.eff, x$joint.eff.pvalue, unique(x$effect_direction))
    
    r$coverage1 <- coverage(betas = x$estimate1, ses = x$se1, cf = 1, 
                            te = -1*unique(x$effect_magnitude1))
    r$coverage2 <- coverage(betas = x$estimate2, ses = x$se2, cf = 1,
                            te = -1*unique(x$effect_magnitude2))
    r$coverage_joint <- coverage(betas = x$joint.eff, ses = x$joint.eff.se, cf = 1,
                                 te=-1*(unique(x$effect_magnitude1)+unique(x$effect_magnitude2)))
    
  }
  
  
  r$power1 <- mean(correct_rejection_rate_flag(coeffs = x$estimate1, 
                                               ses = x$se1, 
                                               cf = 1, 
                                               effect_direction = unique(x$effect_direction)))
  r$power2 <- mean(correct_rejection_rate_flag(coeffs = x$estimate2, 
                                               ses = x$se2, 
                                               cf = 1, 
                                               effect_direction = unique(x$effect_direction)))
  r$power_joint <- mean(correct_rejection_rate_flag(coeffs = x$joint.eff, 
                                                    ses = x$joint.eff.se, 
                                                    cf = 1, 
                                                    effect_direction = unique(x$effect_direction)))
  
  return(r)
}

# split the data into a separate list for each run simulation
# (i.e. each list should be a single run with 5000 iterations)
# then calculate the summary for each, and combine
# lastly, convert to long format
apply_summary <- function(x) {
  # x <- split(x, list(x$model_name, x$se_adjustment, x$years_apart))
  x <- split(x, list(x$model_name, x$policy_speed, x$ordered, x$rho, x$n_units, x$se_adjustment, x$years_apart))
  
  summarized_results <- lapply(x, summarize_concurrent)
  
  combined_results <- bind_rows(summarized_results) %>%
    mutate(id = 1:n()) %>%
    data.frame()
  
  reshape(data = combined_results, 
          idvar = "coefficient", 
          varying = list(estimate=c("estimate1", "estimate2", "joint.eff"),
                         se=c("se1", "se2", "joint.eff.se"),
                         variance=c("variance1", "variance2", "joint.eff.var"),
                         test_stat=c("test_stat1", "test_stat2", "joint.test.stat"),
                         mse=c("mse1", "mse2", "joint.mse"),
                         bias=c("bias1", "bias2", "joint.bias"),
                         type1_error=c("type1_error1", "type1_error2", "type1_error_joint"),
                         types_error=c("type_s_error1", "type_s_error2", "type_s_error_joint"),
                         coverage=c("coverage1", "coverage2", "coverage_joint"),
                         crr=c("power1", "power2", "power_joint")), 
          direction="long", 
          v.names = c("estimate","se","variance", "test_stat", "mse", 
                      "bias", "type1_error", "types_error", "coverage", "crr")) %>%
    mutate(coefficient = c(rep("treatment1", nrow(combined_results)), 
                           rep("treatment2", nrow(combined_results)), 
                           rep("joint_effect", nrow(combined_results))),
           true_effect = paste0(effect_magnitude1, ", ", effect_magnitude2)) %>%
    dplyr::select(model_name, model_call, model_formula, specification, n_units, n_extra,
                  true_effect, effect_magnitude1, effect_magnitude2, effect_direction, 
                  policy_speed, rho, n_units, orderedTx, years_apart, avg_min_distance, 
                  avg_max_distance, avg_mean_distance, coefficient, se_adjustment, 
                  estimate, se, variance, test_stat, bias, mse, types_error, type1_error,
                  coverage, crr) 
  
}


# summarize the data ------------------------------------------------------

summarized_data <- lapply(data_to_summarize, apply_summary)


# save the results --------------------------------------------------------

for (i in 1:length(summarized_data)) {
  write.csv(summarized_data[[i]], 
            file=glue("{revisionPath}/{names(summarized_data)[i]}.csv"), 
            row.names = FALSE)
}

