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
          correction_factor(x$t_stat),
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
        cf=correction_factor(x$t_stat),
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

#==============================================================================
#==============================================================================
# EXAMPLE SETUP
#==============================================================================
#==============================================================================
#results <- readRDS("data/autoregressive-runs-2020-04-21.rds")
#results <- readRDS("data/negbin-two-way-fe-2020-04-28.rds")
#results <- readRDS("data/negbin-autorgressive-2020-04-30.rds")
# this is the negbin with lag outcome as crude.rate
#results <- readRDS("data/negbin-autorgressive-2020-05-05.rds")
# this is the negbin with lag outcome as deaths
#results <- readRDS("data/negbin-autorgressive-2020-05-06.rds")

# final prod results
linear_fe <- readRDS("data/linear-fe-2020-05-09.rds")
linear_ar <- readRDS("data/linear-ar-2020-05-09.rds")
negbin_fe <- readRDS("data/negbin-fe-2020-05-09.rds")
negbin_ar <- readRDS("data/negbin-ar-2020-05-11.rds")

meta_vars <- c(
  "model_call", "model_formula", "n_units", "true_effect", "effect_direction",
  "policy_speed", "change_code_treatment", "rho"
)

estimates_map1 <- list(
  treatment1 = c("estimate1", "se1", "variance1", "t_stat1", "p_value1"),
  treatment2 = c("estimate2", "se2", "variance2", "t_stat2", "p_value2"),
  joint_effect = c("joint.eff", "joint.eff.se", "variancej", "t_statj", "joint.eff.pvalue"),
  labels = c("estimate", "se", "variance", "t_stat", "p_value")
)

estimates_map2 <- list(
  treatment1 = c("estimate", "se", "variance", "t_stat", "p_value"),
  labels = c("estimate", "se", "variance", "t_stat", "p_value")
)

# for now, will need to update
for (i in 1:length(results)) {
  names(results[[i]]) <- gsub("z_stat", "t_stat", names(results[[i]]))
}

# get results first time, use ID to name element
r_list <- list()
for (i in 1:length(results)) {
  tes <- as.numeric(strsplit(results[[i]]$true_effect, ", ")[[1]])
  if (unique(results[[i]]$effect_direction) == "null") {
    tes <- c(0, 0)
  }
  if ( grepl("treatment2", unique(results[[i]]$model_formula)) ) {
    s <- summarize_results(
      x=results[[i]],
      meta_vars=meta_vars,
      estimates_map=estimates_map1,
      te_map=list(treatment1=tes[1], treatment2=tes[2], joint_effect=sum(tes)),
      grouping_vars=c("se_adjustment", "iter"),
      cf=NULL
    )
  } else {
    s <- summarize_results(
      x=results[[i]],
      meta_vars=meta_vars,
      estimates_map=estimates_map2,
      te_map=list(treatment1=tes[1]),
      grouping_vars=c("se_adjustment", "iter"),
      cf=NULL
    )
  }
  
  r_list[[s$id]] <- s$summary
  print(i)
}

# now do again, but use the correction factors calculated the first time through
final_list <- list()
for (i in 1:length(results)) {
  tes <- as.numeric(strsplit(results[[i]]$true_effect, ", ")[[1]])
  if (unique(results[[i]]$effect_direction) == "null") {
    tes <- c(0, 0)
  }
  meta_list <- list()
  for (m in meta_vars) {
    meta_list[[m]] <- unique(results[[i]][[m]])[1]
  }
  id <- paste(unlist(meta_list), collapse="-")
  
  if ( meta_list$effect_direction == "null" ) {
    final_list[[id]] <- r_list[[id]]
  } else {
    # get cf from corresponding null run
    cf <- r_list[[gsub("-neg-", "-null-", id)]][, c("coefficient", "se_adjustment", "cf")]
    if ( grepl("treatment2", unique(results[[i]]$model_formula)) ) {
      s <- summarize_results(
        x=results[[i]],
        meta_vars=meta_vars,
        estimates_map=estimates_map1,
        te_map=list(treatment1=tes[1], treatment2=tes[2], joint_effect=sum(tes)),
        grouping_vars=c("se_adjustment", "iter"),
        cf=cf
      )
    } else {
      s <- summarize_results(
        x=results[[i]],
        meta_vars=meta_vars,
        estimates_map=estimates_map2,
        te_map=list(treatment1=tes[1]),
        grouping_vars=c("se_adjustment", "iter"),
        cf=cf
      )
    }
    
    final_list[[s$id]] <- s$summary
  }
  print(i)
}

one_obj_results <- do.call(dplyr::bind_rows, final_list)
main_obj_results <- one_obj_results %>%
  filter(n_units == 30) %>%
  filter(grepl("0.099", true_effect)) %>%
  filter(policy_speed == "instant") %>%
  filter(se_adjustment == "cluster")

write.csv(
  main_obj_results,
  "~/Downloads/negbin-autoregressive-main-runs-2020-04-30.csv",
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

#==============================================================================
#==============================================================================
# SUMMARY METHODS
#==============================================================================
#==============================================================================
#' get data tidy for summarizing
clean_data <- function(x, map, te_map=NULL, grouping_vars=NULL) {
  if ( var(sapply(map, length)) != 0 | !"labels" %in% names(map) ) {
    stop("all map elements must have the same length and one must be named 'labels'")
  }
  
  groups_ <- names(map)[names(map) != "labels"]
  r <- data.frame()
  for (g in groups_) {
    tmp <- x[, map[[g]]]
    names(tmp) <- map[["labels"]]
    if (!is.null(te_map)) {
      tmp$te <- te_map[[g]]
    }
    if (!is.null(grouping_vars)) {
      tmp <- cbind(tmp, x[, grouping_vars, drop=FALSE])
    }
    
    tmp$coefficient <- g
    r <- rbind(r, tmp)
    rm(tmp)
  }
  
  return(r)
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

#' @import dplyr
summary_means <- function(x, summarize_vars, grouping_vars=NULL) {
  summarize_vars <- summarize_vars[summarize_vars %in% names(x)]
  if (is.null(grouping_vars)) {
    s <- x %>% 
      dplyr::summarize_at(summarize_vars, mean, na.rm=TRUE)
  } else {
    grouping_vars <- dplyr::syms(grouping_vars)
    s <- x %>%
      dplyr::group_by(!!!grouping_vars) %>%
      dplyr::summarize_at(summarize_vars, mean, na.rm=TRUE) %>%
      dplyr::ungroup()
  }
  
  return(s)
}

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



#' @import dplyr
summarize_results <- function(x, meta_vars, estimates_map, te_map=NULL, grouping_vars=NULL, cf=NULL) {
  
  # get the meta values, should only be one unique but defaulting to first element
  meta_list <- list()
  for (m in meta_vars) {
    meta_list[[m]] <- unique(x[[m]])[1]
  }
  
  if ( sum(grepl("_distance", names(x))) > 0 ) {
    # get the values for each iter
    dist <- x %>%
      dplyr::select(min_distance, max_distance, mean_distance, iter) %>%
      dplyr::distinct(.)
    
    # add to meta information
    meta_list[["avg_min_distance"]] <- mean(dist$min_distance, na.rm=TRUE)
    meta_list[["avg_max_distance"]] <- mean(dist$max_distance, na.rm=TRUE)
    meta_list[["avg_mean_distance"]] <- mean(dist$mean_distance, na.rm=TRUE)
  }
  
  estimates <- clean_data(x, estimates_map, te_map, grouping_vars)
  
  # if null, save the correction factors
  if (meta_list$effect_direction == "null") {
    cf <- estimates %>%
      dplyr::group_by(coefficient, se_adjustment) %>%
      dplyr::summarize(cf=correction_factor(t_stat))
  }
  
  # add in correction factors and correct rejection rate if cf provided or null run
  if (!is.null(cf)) {
    estimates <- dplyr::left_join(estimates, cf, by=c("coefficient", "se_adjustment"))
    
    crr_flagVec <- Vectorize(correct_rejection_rate_flag,
                        vectorize.args = c("coeffs", "ses", "cf"))
    
    estimates$crr <- crr_flagVec(estimates$estimate, estimates$se, estimates$cf, effect_direction = meta_list$effect_direction)
  }
  
  # add in bias and MSE if true effect included
  if ( "te" %in% names(estimates) ) {
    biasVec <- Vectorize(bias, vectorize.args = c("x", "te"))
    mseVec <- Vectorize(mse, vectorize.args = c("x", "te"))
    estimates$bias <- biasVec(estimates$estimate, estimates$te, meta_list$effect_direction)
    estimates$mse <- mseVec(estimates$estimate, estimates$te, meta_list$effect_direction)
  }
  
  # type I error flag for null runs
  if ( meta_list$effect_direction == "null" ) {
    estimates$type1_error <- pval_flag(estimates$p_value)
  }
  
  # get means for the main summary statistics
  all_stats <- summary_means(
    estimates,
    c("estimate", "se", "variance", "t_stat", "p_value", "bias", "mse", "crr", "type1_error"),
    c("coefficient", "se_adjustment")
  )
  
  # add in correction factors used
  if (!is.null(cf)) {
    all_stats <- dplyr::left_join(all_stats, cf, by=c("coefficient", "se_adjustment"))
  }
  
  # type S error for non null runs
  if ( meta_list$effect_direction != "null" ) {
    type_s <- estimates %>%
      dplyr::group_by(coefficient, se_adjustment) %>%
      dplyr::group_modify(~as.data.frame(type_s_error(
        .x$estimate,
        .x$p_value,
        meta_list$effect_direction
      )))
    names(type_s)[3] <- "type_s_error"
    
    all_stats <- dplyr::left_join(all_stats, type_s, by=c("coefficient", "se_adjustment"))
  }
  
  # add in meta information
  for ( m in names(meta_list) ) {
    all_stats[[m]] <- meta_list[[m]]
  }
  
  # construct id for run
  dist_ind <- grep("distance", names(meta_list))
  if ( length(dist_ind) > 0 ) {
    id_list <- meta_list[-dist_ind]
  } else {
    id_list <- meta_list
  }
  id <- paste(unlist(id_list), collapse="-")
  
  return(list(summary=all_stats, id=id))
}
