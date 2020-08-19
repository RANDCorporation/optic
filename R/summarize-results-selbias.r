# 1. get meta information
# 2. get summary of min/max/mean distances if applicable
library(dplyr)


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

coverage <- function(betas, ses, cf, te=0) {
  adj_ses <- ses*cf
  ind <- rep(0, length(betas))
  low95 <- betas - 1.96 * adj_ses
  high95 <- betas + 1.96 * adj_ses
  ind[te > low95 & te < high95] <- 1
  return(sum(ind) / length(betas))
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


#==============================================================================
#==============================================================================
# SELECTION BIAS SUMMARY METHODS
#==============================================================================
#==============================================================================
summarize_results_selbias <- function(x) {
  # get means and range values
  summary_results <- x %>%
    summarize_at(vars(estimate:mse, mean_es_prior, mean_es_unempl, mean_es_outcome), mean)
  summary_results <- cbind(
    summary_results,
    x %>% summarize_at(vars(max_es_prior, max_es_unempl, max_es_outcome), max)
  )
  summary_results <- cbind(
    summary_results,
    x %>% summarize(min_n_unique_enact_years=min(n_unique_enact_years),
                    max_n_unique_enact_years=max(n_unique_enact_years),
                    min_n_treated=min(n),
                    max_n_treated=max(n))
  )
  
  # bias
  summary_results <- cbind(
    summary_results,
    data.frame(bias=mean(x$estimate - 0))
  )
  
  # type 1 error
  summary_results <- cbind(
    summary_results,
    data.frame(type_1_error=mean(ifelse(x$p_value < 0.05, 1, 0)))
  )
  
  # power
  summary_results <- cbind(
    summary_results,
    data.frame(
      power=mean(
        correct_rejection_rate_flag(
          x$estimate,
          x$se,
          correction_factor(x$test_stat),
          effect_direction = "null"
        )
      )
    )
  )
  
  # coverage
  summary_results <- cbind(
    summary_results,
    data.frame(
      coverage=coverage(
        x$estimate,
        ses=x$se,
        cf=correction_factor(x$test_stat),
        te=0
      )
    )
  )
  
  summary_results <- cbind(
    data.frame(
      outcome=unique(x$outcome),
      se_adjustment=unique(x$se_adjustment),
      model_name=unique(x$model_name),
      model_call=unique(x$model_call),
      moel_formula=unique(x$model_formula),
      policy_speed=unique(x$policy_speed),
      n_implementation_years=unique(x$n_implementation_years),
      prior_control=unique(x$prior_control),
      bias_type=unique(x$bias_type),
      bias_size=unique(x$bias_size),
      b0=unique(x$b0),
      b1=unique(x$b1),
      b2=unique(x$b2),
      b3=unique(x$b3),
      b4=unique(x$b4),
      b5=unique(x$b5),
      a1=unique(x$a1),
      a2=unique(x$a2),
      a3=unique(x$a3),
      a4=unique(x$a4),
      a5=unique(x$a5),
      stringsAsFactors = FALSE
    ),
    summary_results
  )
  
  return(summary_results)
}

trial <- read.csv("data/sel-bias-linear-fe-all-runs.csv")
trial <- read.csv("data/sel-bias-linear-ar-all-runs.csv")
trial <- read.csv('data/sel-bias-multisynth-all-runs.csv')


trial_sims <- split(
  trial,
  list(trial$model_name, trial$policy_speed, trial$n_implementation_years,
       trial$prior_control, trial$bias_type, trial$bias_size, trial$se_adjustment)
)

trial_sims <- trial_sims[which(sapply(trial_sims, nrow) > 0)]

summarized <- lapply(trial_sims, summarize_results_selbias)
summarized <- do.call('rbind', summarized)
rownames(summarized) <- NULL

write.csv(summarized, "~/Downloads/sel-bias-multisynth-summary.csv", row.names = FALSE)


lmar <- read.csv("~/Downloads/sel-bias-linear-ar-summary.csv", stringsAsFactors = FALSE)
lmfe <- read.csv("~/Downloads/sel-bias-linear-fe-summary.csv", stringsAsFactors = FALSE)
msyn <- read.csv("~/Downloads/sel-bias-multisynth-summary.csv", stringsAsFactors = FALSE)

r <- rbind(lmar, lmfe, msyn)

r <- r %>%
  filter(se_adjustment == "cluster" | model_name == "multisynth") %>%
  filter(bias_type == "nonlinear") %>%
  mutate(coverage = 1 - type_1_error) %>%
  mutate(
    bias_size = factor(
      bias_size, levels=c("small", "medium", "large"),
      labels=c("Small Selection Bias", "Moderate Selection Bias", "Large Selection Bias")
    )
  ) %>%
  mutate(
    model_name = factor(
      model_name, levels=c("fixedeff_linear", "autoreg_linear", "multisynth"),
      labels=c("Linear 2-way\nFixed Effects Model", "Linear Autoregressive\nModel", "Augmented SCM")
    )
  )


library(ggplot2)
cr_sd <- 4.358559
text_size <- 12

bias_mva3 <- ggplot(r %>% filter(prior_control == "mva3"), aes(x=model_name, y=abs(bias / cr_sd), fill=bias_size)) +
  geom_col(position=position_dodge(), width=0.8) +
  scale_fill_manual(values=c("lightblue", "blue", "darkblue")) +
  scale_y_continuous(limits=c(0, 1)) +
  theme_bw() +
  xlab(NULL) +
  ylab("Bias on an effect size scale") +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    text = element_text(size=text_size)
  )

bias_trend <- ggplot(r %>% filter(prior_control == "trend"), aes(x=model_name, y=abs(bias / cr_sd), fill=bias_size)) +
  geom_col(position=position_dodge(), width=0.8) +
  scale_fill_manual(values=c("lightblue", "blue", "darkblue")) +
  scale_y_continuous(limits=c(0, 1)) +
  theme_bw() +
  xlab(NULL) +
  ylab("Bias on an effect size scale") +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    text = element_text(size=text_size)
  )

coverage_mva3 <- ggplot(r %>% filter(prior_control == "mva3"), aes(x=model_name, y=coverage, fill=bias_size)) +
  geom_col(position=position_dodge(), width=0.8) +
  scale_fill_manual(values=c("lightblue", "blue", "darkblue")) +
  scale_y_continuous(limits=c(0, 1)) +
  theme_bw() +
  xlab(NULL) +
  ylab("Coverage") +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    text = element_text(size=text_size)
  )

coverage_trend <- ggplot(r %>% filter(prior_control == "trend"), aes(x=model_name, y=coverage, fill=bias_size)) +
  geom_col(position=position_dodge(), width=0.8) +
  scale_fill_manual(values=c("lightblue", "blue", "darkblue")) +
  scale_y_continuous(limits=c(0, 1)) +
  theme_bw() +
  xlab(NULL) +
  ylab("Coverage") +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    text = element_text(size=text_size)
  )

var_mva3 <- ggplot(r %>% filter(prior_control == "mva3"), aes(x=model_name, y=variance, fill=bias_size)) +
  geom_col(position=position_dodge(), width=0.8) +
  scale_fill_manual(values=c("lightblue", "blue", "darkblue")) +
  scale_y_continuous(limits=c(0, 85)) +
  theme_bw() +
  xlab(NULL) +
  ylab("Variance") +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    text = element_text(size=text_size)
  )

var_trend <- ggplot(r %>% filter(prior_control == "trend"), aes(x=model_name, y=variance, fill=bias_size)) +
  geom_col(position=position_dodge(), width=0.8) +
  scale_fill_manual(values=c("lightblue", "blue", "darkblue")) +
  scale_y_continuous(limits=c(0, 85)) +
  theme_bw() +
  xlab(NULL) +
  ylab("Variance") +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    text = element_text(size=text_size)
  )

rmse_mva3 <- ggplot(r %>% filter(prior_control == "mva3"), aes(x=model_name, y=sqrt(mse), fill=bias_size)) +
  geom_col(position=position_dodge(), width=0.8) +
  scale_fill_manual(values=c("lightblue", "blue", "darkblue")) +
  scale_y_continuous(limits=c(0, 15)) +
  theme_bw() +
  xlab(NULL) +
  ylab("RMSE") +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    text = element_text(size=text_size)
  )

rmse_trend <- ggplot(r %>% filter(prior_control == "trend"), aes(x=model_name, y=sqrt(mse), fill=bias_size)) +
  geom_col(position=position_dodge(), width=0.8) +
  scale_fill_manual(values=c("lightblue", "blue", "darkblue")) +
  scale_y_continuous(limits=c(0, 15)) +
  theme_bw() +
  xlab(NULL) +
  ylab("RMSE") +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    text = element_text(size=text_size)
  )

w=6
h=3
ggsave("~/Downloads/bias-mva3-nonlinear.png", bias_mva3, dpi=300, width=w, height=h)
ggsave("~/Downloads/bias-trend-nonlinear.png", bias_trend, dpi=300, width=w, height=h)
ggsave("~/Downloads/coverage-mva3-nonlinear.png", coverage_mva3, dpi=300, width=w, height=h)
ggsave("~/Downloads/coverage-trend-nonlinear.png", coverage_trend, dpi=300, width=w, height=h)
ggsave("~/Downloads/variance-mva3-nonlinear.png", var_mva3, dpi=300, width=w, height=h)
ggsave("~/Downloads/variance-trend-nonlinear.png", var_trend, dpi=300, width=w, height=h)
ggsave("~/Downloads/rmse-mva3-nonlinear.png", rmse_mva3, dpi=300, width=w, height=h)
ggsave("~/Downloads/rmse-trend-nonlinear.png", rmse_trend, dpi=300, width=w, height=h)
