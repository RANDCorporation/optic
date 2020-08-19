# For concurrent, need to get the matching runs that are null and neg;
# null runs provide the correction factors that need to be applied to neg runs

#==============================================================================
#==============================================================================
# SETUP AND METHODS
#==============================================================================
#==============================================================================
library(dplyr)
library(tidyr)

#' calculate the correction factor
#' 
#' @param test_stats test statistics
#' @param type what type of test statistic is being supplied; one of "t", "wald"; default is "t"
correction_factor <- function(test_stats, type="t") {
  if (type == "t") {
    f_stats <- (test_stats)^2
    f_stats <- sort(f_stats)
    high_cut <- 0.95 * length(test_stats)
    femp95 <- f_stats[high_cut]
    freal <- qf(0.95, 1, Inf)
    corr_factor <- sqrt(femp95 / freal)
  }
  
  return(corr_factor)
}


#' helper for mean squared error
mse <- function(x, te, effect_direction) {
  if (effect_direction == "null") {
    (x - 0) ^ 2
  } else {
    (x - te)^2
  }
}


#' helper for bias
bias <- function(x, te, effect_direction) {
  if (effect_direction == "null") {
    x - 0
  } else {
    x - te 
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
  adj_ses <- ses * cf
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
  
  # get the null and neg runs separately
  null <- x %>%
    filter(effect_direction == "null")
  neg <- x %>%
    filter(effect_direction == "neg")
  
  # calculate correction factors from null run
  cf1 <- correction_factor(na.omit(null$test_stat1))
  cf2 <- correction_factor(na.omit(null$test_stat2))
  cfj <- correction_factor(na.omit(null$test_statj))
  
  results <- NULL
  for (tmp in list(null, neg)) {
    r <- tmp %>%
      summarize(
        estimate1=mean(na.omit(estimate1)),
        se1=mean(na.omit(se1)),
        variance1=mean(na.omit(variance1)),
        test_stat1=mean(na.omit(test_stat1)),
        mse1=mean(mseVec(estimate1, effect_magnitude1, unique(effect_direction))),
        bias1=mean(biasVec(estimate1, effect_magnitude1, unique(effect_direction))),
        estimate2=mean(na.omit(estimate2)),
        se2=mean(na.omit(se2)),
        variance2=mean(na.omit(variance2)),
        test_stat2=mean(na.omit(test_stat2)),
        mse2=mean(mseVec(estimate2, effect_magnitude2, unique(effect_direction))),
        bias2=mean(biasVec(estimate2, effect_magnitude2, unique(effect_direction))),
        joint.eff=mean(na.omit(joint.eff)),
        joint.eff.se=mean(na.omit(joint.eff.se)),
        joint.eff.var=mean(na.omit(variancej)),
        joint.test.stat=mean(na.omit(test_statj)),
        joint.mse=mean(mseVec(joint.eff, effect_magnitude1, unique(effect_direction))),
        joint.bias=mean(biasVec(joint.eff, effect_magnitude1, unique(effect_direction))),
        avg_min_distance=mean(min_distance),
        avg_max_distance=mean(max_distance),
        avg_mean_distance=mean(mean_distance)
      ) %>%
      mutate(
        se_adjustment=unique(tmp$se_adjustment),
        effect_magnitude1=unique(tmp$effect_magnitude1),
        effect_magnitude2=unique(tmp$effect_magnitude2),
        effect_direction=unique(tmp$effect_direction),
        policy_speed=unique(tmp$policy_speed),
        rho=unique(tmp$rho),
        n_implementation_periods=unique(tmp$n_implementation_periods),
        model_name=unique(tmp$model_name),
        model_call=unique(tmp$model_call),
        model_formula=unique(tmp$model_formula)
      )
    
    if (unique(tmp$effect_direction) == "null") {
      r$type1_error1 <- mean(pval_flag(tmp$p_value1))
      r$type1_error2 <- mean(pval_flag(tmp$p_value2))
      r$type1_error_joint <- mean(pval_flag(tmp$joint.eff.pvalue))
      
      r$type_s_error1 <- NA
      r$type_s_error2 <- NA
      r$type_s_error_joint <- NA
    } else {
      r$type1_error1 <- NA
      r$type1_error2 <- NA
      r$type1_error_joint <- NA
      
      r$type_s_error1 <- type_s_error(tmp$estimate1, tmp$p_value1, unique(tmp$effect_direction))
      r$type_s_error2 <- type_s_error(tmp$estimate2, tmp$p_value2, unique(tmp$effect_direction))
      r$type_s_error_joint <- type_s_error(tmp$joint.eff, tmp$joint.eff.pvalue, unique(tmp$effect_direction))
    }
    
    results <- rbind(results, r)
  }
  
  return(results)
}


#==============================================================================
#==============================================================================
# SUMMARIZE DATA
#==============================================================================
#==============================================================================

# final prod results
lm <- readRDS("data/concurrent-lm-results-2020-08-13.rds")
nb <- readRDS("data/concurrent-nb-results-2020-08-13.rds")


all_results <- NULL
for (set in list(lm, nb)) {
  results <- set
  results <- do.call(rbind, results)
  
  # create flag for the mis-specified models
  results$specification <- ifelse(grepl("treatment2", results$model_formula), "correct", "misspec")
  
  # create an "id" for the run leaving out effect direction, this will marry
  # identical null and neg runs
  results <- results %>%
    unite(run_id, se_adjustment, effect_magnitude1, effect_magnitude2, policy_speed, rho,
          n_implementation_periods, model_name, sep="-", remove=FALSE)
  
  # split to pair the same runs together, only difference is effect direction, at 5000 iters
  # each element in the list should have 10,000 rows for the two runs
  results <- split(
    results,
    results$run_id
  )
  
  summarized_results <- lapply(results, summarize_concurrent)
  
  set_results <- do.call(rbind, summarized_results)
  
  all_results <- rbind(all_results, set_results)
}

write.csv(all_results, paste0("data/concurrent-summarized-results-", Sys.Date(), ".csv"), row.names = FALSE)

#==============================================================================
#==============================================================================
# GRAPHICS
#==============================================================================
#==============================================================================
# NOTE: old code, updated summary methods produce new structure of clean data,
# this is for records only will need to modify data structure to long then
# this could still work
one_obj_results <- do.call(dplyr::bind_rows, final_list)
main_obj_results <- one_obj_results %>%
  filter(grepl("0.457", effect_magnitude1)) %>%
  filter(policy_speed == "instant") %>%
  filter(se_adjustment == "cluster")

write.csv(
  main_obj_results,
  "~/Downloads/linear-autoregressive-main-runs-2020-08-13.csv",
  row.names = FALSE
)

graph_data <- main_obj_results %>%
  mutate(specified = ifelse(grepl("treatment2", model_formula), "Correct Specification", "Misspecified")) %>%
  mutate(coefficient = factor(
    coefficient,
    levels=c("treatment1", "treatment2", "joint_effect"),
    labels=c("Treatment 1", "Treatment 2", "Joint Effect"))) %>%
  select(coefficient, effect_direction, specified, rho, type1_error, bias, variance, mse, crr, type_s_error) %>%
  gather(key=metric, value=value, type1_error:type_s_error) %>%
  mutate(metric = factor(
    metric,
    levels=c("type1_error", "bias", "variance", "mse", "crr", "type_s_error"),
    labels=c("Type 1 Error", "Bias", "Variance", "MSE", "Power", "Type S Error"))
  ) %>%
  mutate(effect_direction = factor(effect_direction, levels=c("neg", "null"), labels=c("Negative Effect", "Null effect")))

dummy_data <- data.frame(
  specified=c(rep("Correct Specification", 4), rep("Misspecified", 4)),
  metric=c("Bias", "Bias", "Variance", "Power",
           "Bias", "Bias", "Variance", "Power"),
  rho=c(rep(0, 8)),
  value=c(-0.35, 0.35, 0.5, 1.0, -0.35, 0.35, 0.5, 1.0),
  stringsAsFactors = FALSE
)

# bias, variance, power for for negative effect, by specification
figure_1 <- ggplot(graph_data %>%
                     filter(metric %in% c("Bias", "Power", "Variance")) %>%
                     filter(effect_direction == "Negative Effect"),
                   aes(x=factor(rho), y=value)) +
  geom_bar(stat="identity", aes(fill=coefficient), position=position_dodge2(preserve="single")) +
  facet_grid(specified ~ metric, scales="free_y") +
  theme_bw() +
  theme(
    legend.title = element_blank()
  ) +
  ylab("Value") +
  xlab("Rho")

figure_2 <- ggplot(graph_data %>%
                     filter(metric %in% c("Type 1 Error")) %>%
                     filter(effect_direction == "Null effect") %>%
                     filter(specified == "Correct Specification"),
                   aes(x=factor(rho), y=value)) +
  geom_bar(stat="identity", aes(fill=coefficient), position="dodge") +
  facet_grid( ~ metric) +
  theme_bw() +
  theme(
    legend.title = element_blank()
  ) +
  ylab("Value") +
  xlab("Rho")

ggsave("~/Downloads/figure_1_negbin-ar-negative-raw.deaths.lag.png", figure_1, width=14, height=8, dpi=300)
ggsave("~/Downloads/figure_2_glm.nb-twfe-null.png", figure_2, width=14, height=8, dpi=300)





