rm(list = ls())

library(optic)

library(did)
library(augsynth)
library(did2s)
library(didimputation)
library(fixest)

library(tidyr)
library(ggplot2)
library(gridExtra)
library(plyr)

data(overdoses)

regenerate <- F
plots <- F
synthetic <- F

setwd("C:/users/griswold/documents/GitHub/optic/")

# For whatever reason, crude.rate is missing observations for North Dakota.
# So recompute this vector from the data:
df <- setDT(overdoses)

df$crude.rate <- 1e5*(df$deaths/df$population)

# Construct a linear increase and decrease in the effect size through time,
# building towards or away from a 10% effect over a 5-year span:

full_effect <- 0.1*mean(df$crude.rate, na.rm = T)
effect_sd   <- sd(df$crude.rate, na.rm = T)

# Construct scenarios with time-varying patterns but same 
# average effect post-treatment.

linear_ramp_up <- seq(0, 1, 0.2)*full_effect
linear_sunset <- rev(seq(0, 1, 0.2))*full_effect

instant_plateau  <- c(0, 0.3, 0.7, 1, 0.8, 0.2)*full_effect
slow_plateau     <- c(0, 0.5, 0.6, 0.8, 0.5, 0.6)*full_effect

time_varying_scenarios <- list(linear_ramp_up, linear_sunset, instant_plateau, slow_plateau)
scenario_names <- c("I", "II", "III", "IV")

scenarios <- list()

for (i in 1:4){
  scene <- time_varying_scenarios[[i]]
  scenarios[[i]] <- data.table("ttt" = seq(0, length(scene) - 1),
                                       "smd" = scene/effect_sd,
                                       "true_effect" = scene,
                                       "scenario_name" = scenario_names[[i]])
}

scenarios <- rbindlist(scenarios)

long_names <- c("Ramp Up", "Ramp Down", "Temporary", "Inconsistent")
scenarios[, scenario_name_label := mapvalues(scenario_name, 
                                             c("I", "II", "III", "IV"),
                                             long_names)]

scenarios[, scenario_name_label := factor(scenario_name_label,
                                          levels = long_names)]

#scenarios[, smd := -1*smd]

p <- ggplot(scenarios, aes(x = ttt, y = smd)) +
    geom_point(size = 2.5) +
    geom_line(linewidth = 0.8) +
    facet_wrap(~scenario_name_label) +
    labs(x = "Time since treatment",
         y = "Standardized \nMean Difference") +
    scale_y_continuous(breaks = seq(0., 0.2, 0.05),
                       limits = c(0, 0.2)) +
    theme_bw() +
    theme(strip.background = element_blank(),
          strip.text = element_text(family = 'sans', size = 14),
          axis.text = element_text(family = 'sans', size = 10),
          axis.title.x = element_text(family = 'sans', size = 12),
          axis.title.y = element_text(family = 'sans', size = 12, angle = 0, vjust = 0.5))

ggsave(filename = "./time_vary_paper/plots/Fig_1_policy_scenarios.svg", plot = p, height = 6, width = 8, units = "in")

if (synthetic){
  
  library(synthpop)
  
  # Number of replicates:
  i <- 10
  
  # Generate an additional 510 synthetic states based off observed data:
  pred_mat <- matrix(c(0, 0, 0, 0,
                       0, 0, 0, 0,
                       1, 1, 1, 0,
                       1, 1, 1, 0), 
                     byrow = T, nrow = 4)
  
  df_true <- df[, .(state, year, unemploymentrate, crude.rate)]
  
  df_sim <- syn(df_true, m = i, proper = F, method = "rf", seed = 191,
                visit.sequence = c(3, 4), predictor.matrix = pred_mat,
                smoothing = "spline")
  
  # For each synthetic dataset, swap state names with a sequential number of
  # fake names
  fake_states <- fread("./time_vary_paper/sim_data/fake_states.csv", fill = T)
  
  hold <- list()
  
  for (j in 1:i){
    old_states <- unique(df_sim$syn[[j]]$state)
    new_states <- fake_states[((j - 1)*51 + 1):(j*51)]$new_name
    
    swap <- as.data.table(df_sim$syn[[i]])
    swap[, state := mapvalues(state, old_states, new_states)]
    
    hold[[j]] <- swap
    
  }
  
  df_sim <- rbindlist(hold)
  
  # Add on indicator for fake states:
  df_true[, observed_data := T]
  df_sim[, observed_data := F]
  
  df_sim <- rbind(df_true, df_sim)
  
  if (plots){
    cruderate <- ggplot(df_sim, aes(x = crude.rate, color = observed_data)) + 
      geom_density(size = 1) + 
      facet_wrap(~year) + 
      theme_bw() +
      labs(x = "Crude Mortality Rate", y = "Density",
           color = "Observed data") +
      theme(strip.background = element_blank(),
            legend.position = "bottom")
    
    unemploymentrate <- ggplot(df_sim, aes(x = unemploymentrate, color = observed_data)) + 
      geom_density(size = 1) + 
      facet_wrap(~year) + 
      theme_bw() +
      labs(x = "Unemployment Rate", y = "Density",
           color = "Observed data") +
      theme(strip.background = element_blank(),
            legend.position = "bottom")
    
    ggsave(filename = "./time_vary_paper/plots/appendix_synthetic/density_unemploymentrate.pdf", 
           plot = unemploymentrate,
           height = 8.27, width = 11.69, units = "in")
    
    ggsave(filename = "./time_vary_paper/plots/appendix_synthetic/density_cruderate.pdf", 
           plot = cruderate,
           height = 8.27, width = 11.69, units = "in")  
  }
}

# Specify  models to simulate treatment effects:
if (regenerate){
  
  m_es <- optic_model(
    name = "twfe",
    type = "eventstudy",
    call = "feols",
    formula = crude.rate ~ unemploymentrate + i(time_to_treat, ref = c(-1, Inf)) | state + year,
    model_args = list(cluster = "state"),
    se_adjust = "none",
  )
  
  m_sa <- optic_model(
    name = "sa",
    type = "eventstudy",
    call = "feols",
    formula = crude.rate ~ unemploymentrate + sunab(treatment_date, time_to_treat, ref.p = c(-1, .F)) | state + year,
    model_args = list(cluster = "state"),
    se_adjust = "none"
  )
  
  m_csa <- optic_model(
    name = "csa_did",
    type = "did",
    call = "att_gt",
    
    #Formula below technically doesn't do anything but OPTIC requires it; we might want to fix this? Same for several other calls like multisynth
    formula = crude.rate ~ unemploymentrate + treatment_level,
    model_args = list(yname = "crude.rate", tname = 'year', idname = 'state',
                      gname = 'treatment_date', xformla = formula(~ unemploymentrate)),
    se_adjust = "none"
    
  )
  
  m_aug <- optic_model(
    name = "augsynth",
    type = "multisynth",
    call = "multisynth",
    formula = crude.rate ~ treatment_level | unemploymentrate,
    model_args = list(unit = as.name("state"),
                      time = as.name("year"),
                      lambda = 0.1),
    se_adjust = "none"
    
  )
  
  m_bjs <- optic_model(
    name = "bjs",
    type = "did_imputation",
    call = "did_imputation",
    formula = crude.rate ~ unemploymentrate + treatment_level,
    model_args = list(yname = "crude.rate",
                      gname = "treatment_date",
                      tname = "year",
                      idname = "state"),
    se_adjust = "none"
  )
  
  m_g <- optic_model(
    name = "gardner",
    type = "did2s",
    call = "did2s",
    formula = crude.rate ~ unemploymentrate + treatment_level,
    model_args = list(yname = "crude.rate", treatment = "treatment",
                      first_stage = ~ 0 | state + year,
                      second_stage = ~ i(time_to_treat, ref = c(-1, Inf)),
                      cluster_var = "state"),
    se_adjust = "none"
  )
  
  m_ar_db <- optic_model(
    
    name = "autoregressive",
    type = "autoreg_debiased",
    call = "autoreg_debiased",
    formula = crude.rate ~ unemploymentrate + i(time_to_treat, ref = c(-1)) | year,
    model_args = list(outcome_name = "crude.rate", date_name = 'year', unit_name = 'state',
                      cov_names = "unemploymentrate", lags = 6),
    se_adjust = "none"
    
  )
  
  sim_models <- list(m_es, m_ar_db, m_sa, m_csa, m_g, m_bjs, m_aug)

  sim_config <- optic_simulation(
    
    x                        = df_sim,
    models                   = sim_models,
    iters                    = 1000,
    method                   = "time_varying",
    unit_var                 = "state",
    treat_var                = "state",
    time_var                 = "year",
    effect_magnitude         = time_varying_scenarios,
    n_units                  = c(5, 25, 45),
    effect_direction         = c("pos"),
    policy_speed             = c("instant"),
    n_implementation_periods = c(6)
    
  )
  
  sim_results <- dispatch_simulations(
    
    sim_config,
    seed = 9780,
    verbose = 0,
    use_future = T,
    future.packages = c("didimputation", "did2s", "optic","augsynth", "fixest", "tidyr")
    
  )
  
  sim_final <- rbindlist(sim_results)
  
  write.csv(sim_final,  "./time_vary_paper/sim_data/sim_results_v4_synthetic.csv", row.names = F)
  
}

# myTryCatch <- function(expr) {
#   warn <- err <- NULL
#   value <- withCallingHandlers(
#     tryCatch(expr, error=function(e) {
#       err <<- e
#       NULL
#     }), warning=function(w) {
#       warn <<- w
#       invokeRestart("muffleWarning")
#     })
#   list(value=value, warning=warn, error=err)
# }

if (plots){
  
  df_main <- fread("./time_vary_paper/sim_data/sim_results_v4.csv")
  df_tunits <- fread("./time_vary_paper/sim_data/sim_results_v4_5_45_treated.csv")
  df_synth  <- fread("./time_vary_paper/sim_data/sim_results_v4_synthetic.csv")
  
  prep_summary <- function(df_sim){
    
    df_sim[, scenario := paste(effect_magnitude1, effect_magnitude2, effect_magnitude3, effect_magnitude4, effect_magnitude5, effect_magnitude6)]
    
    # Reshape effect columns long; this should probably occur during the simulation procedure
    id_cols <- c("outcome", "n_implementation_periods", "n_units", "policy_speed", 
                 "effect_direction", "model_formula", "model_name", "model_call", "scenario", "iter")
    
    sum_cols <- c("n_units", "scenario", "model_name", "ttt")
    
    df_sim <- melt(df_sim, id.vars = id_cols, 
                   measure = patterns(estimate = "^estimate_ttt==", 
                                      se = "^se_ttt==", 
                                      variance = "^variance_ttt==", 
                                      t_stat = "^t_stat_ttt==", 
                                      p_value = "^p_value_ttt==",  
                                      true_effect = "^effect_magnitude"), variable.name = "ttt")
    
    # Index time to treat correctly:
    df_sim[, ttt := as.numeric(ttt)]
    df_sim[, ttt := ttt - 1]
    
    # Remove rows where errors occurred.
    # This affects 68 autoregressive models out of 84k sims and 1 CSA run.
    df_sim <- df_sim[!is.na(estimate) & !is.na(se),]
    
    # Calculate GoF statistics
    
    #Median sim error & 2.5th/97.5th percentiles
    df_sim[, sim_error := abs(true_effect - estimate)]
    df_sim[, sim_error_std := abs((true_effect - estimate)/sd(overdoses$crude.rate, na.rm = T))]
    
    df_sim[, sim_error_50 := quantile(.SD$sim_error, 0.5), by = sum_cols]
    df_sim[, sim_error_025 := quantile(.SD$sim_error, 0.025), by = sum_cols]
    df_sim[, sim_error_975 := quantile(.SD$sim_error, 0.975), by = sum_cols]
    
    df_sim[, sim_error_50_std := quantile(.SD$sim_error_std, 0.5), by = sum_cols]
    df_sim[, sim_error_025_std := quantile(.SD$sim_error_std, 0.025), by = sum_cols]
    df_sim[, sim_error_975_std := quantile(.SD$sim_error_std, 0.975), by = sum_cols]
    
    #Median sim variance & 2.5th/97.5th percentiles
    df_sim[, sim_variance := var(.SD$estimate), by = sum_cols]
    
    #RMSE
    df_sim[, sim_rmse := sum(.SD$sim_error^2)/.N, by = sum_cols]
    
    #Model coverage
    df_sim[, covered := ifelse((estimate + 1.96*se >= true_effect) & (estimate - 1.96*se <= true_effect), 1, 0)]
    df_sim[, coverage := sum(.SD$covered)/.N, by = sum_cols]
    
    # Average model estimate and variance
    df_sim[, model_est_mean := mean(.SD$estimate), by = sum_cols]
    df_sim[, model_est_025 := mean(.SD$estimate) - 1.96*(sd(.SD$estimate)), by = sum_cols]
    df_sim[, model_est_975 := mean(.SD$estimate) + 1.96*(sd(.SD$estimate)), by = sum_cols]
    
    df_sim[, model_se_50  := quantile(.SD$se, 0.5), by = sum_cols]
    df_sim[, model_se_025 := quantile(.SD$se, 0.025), by = sum_cols]
    df_sim[, model_se_975 := quantile(.SD$se, 0.975), by = sum_cols]
    
    # Change variable coding to be a little more informative on scenarios, model names,
    # and time to treat
    
    scenario_names <- data.frame("scenario_name" = c("Ramp Up", "Ramp Down",
                                                     "Temporary", "Inconsistent"),
                                 "scenario" = sapply(time_varying_scenarios, paste, collapse = " "))
    
    df_summary <- merge(df_sim, scenario_names, by = "scenario")
    df_summary[, scenario_name_label := factor(scenario_name, levels = scenario_names$scenario_name)]
    
    model_names_remap <- data.frame("new_model_name" = c("DID-ES", "AR-DB",  "DID-HT", "DID-SA", 
                                                         "DID-2S", "DID-IMP", "ASCM"),
                                    "model_name" = c("twfe", "autoregressive", "sa", "csa_did", "gardner", "bjs", "augsynth"))
    
    df_summary <- merge(df_summary, model_names_remap, by = "model_name")
    df_summary[, new_model_name := factor(new_model_name, levels = model_names_remap$new_model_name)]
    
    df_summary[, n_units_name := paste0(n_units, " treated units")]
    df_summary[, n_units_name := factor(n_units_name, levels = unique(df_summary$n_units_name))]
    
    df_summary[, scenario := NULL]
    df_summary[, model_name := NULL]
    setnames(df_summary, "new_model_name", "model_name")
    
    # Collapse to summaries:
    df_summary <- unique(df_summary[, .(n_units, n_units_name, scenario_name_label, model_name, ttt,
                                        sim_error_50, sim_error_025,
                                        sim_error_975, sim_error_50_std, sim_error_025_std, sim_error_975_std,
                                        model_est_mean, model_est_025, model_est_975, model_se_50, model_se_025, model_se_975,
                                        coverage, sim_rmse, sim_variance)])
    
    # Add on true effects to calculate percentages for plots:
    df_summary <- join(df_summary, scenarios, by = c("ttt", "scenario_name_label"), type = "left")
    
    return(df_summary)
    
  }
  
  df_summary        <- prep_summary(df_main)
  df_summary_tunits <- prep_summary(df_tunits)
  df_summary_synth  <- prep_summary(df_synth)
  
  # Hold onto results for 25 units in df_summary; separate out units
  # in appendix df:
  df_summary <- df_summary[n_units_name == "25 treated units",]
  
  df_summary_tunits_5  <- df_summary_tunits[n_units_name == "5 treated units",]
  df_summary_tunits_45 <- df_summary_tunits[n_units_name == "45 treated units",]
  
  plot_colors_bw <- c('#d9d9d9','#bdbdbd','#969696','#737373','#525252','#252525','#000000')
  plot_colors_col <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f', '#cab2d6')
  
  plot_colors <- plot_colors_col
  
  #############
  # GOF Plots #
  #############
  
  #####################
  # Sim error v. se  #
  #####################

  error_se <- function(dd, scene){

    dd <- dd[n_units == 25 & scenario_name_label == scene,]

    plot_scatter <- ggplot(dd, aes(x = model_se_50, y= sim_error_50, color = model_name)) +
      facet_wrap(~ttt, nrow = 2) +
      geom_point(size = 3, shape = 19, alpha = 0.9) +
      labs(title = sprintf("Median absolute error and standard errors\n by time-since-treatment: %s scenario", scene),
           y = "Error",
           x = "Standard errors",
           color = "Estimator") +
      theme_bw() +
      scale_x_continuous(limits = c(0, 2.5),
                         breaks = seq(0, 2.5, 0.5)) +
      scale_y_continuous(limits = c(0, 1.75),
                         breaks = seq(0, 1.75, 0.25)) +
      scale_color_manual(values = plot_colors) +
      theme(plot.title = element_text(hjust = 0.5, family = 'sans', size = 16),
            strip.background = element_blank(),
            strip.text = element_text(family = "sans", size = 10),
            legend.position = "bottom",
            legend.text = element_text(family = 'sans', size = 10),
            axis.ticks = element_line(linewidth = 1),
            axis.ticks.length = unit(5.6, "points"),
            axis.title = element_text(size = 12, family = 'sans'),
            axis.title.y = element_text(size = 12, family = 'sans', angle = 0),
            axis.text = element_text(size = 10, family = 'sans'),
            axis.text.x = element_text(size = 10, family = 'sans',
                                       margin = margin(t = 5, r = 0, b = 10, l = 0)),
            legend.title = element_text(family = 'sans', size = 14))

    return(plot_scatter)

  }

  args <- c("Ramp Up", "Ramp Down", "Temporary", "Inconsistent")

  bias_v_variance_plots <- lapply(args, error_se, dd = df_summary)

  ggsave(filename = "./time_vary_paper/plots/fig_s1_model_error_v_se.pdf", plot = marrangeGrob(bias_v_variance_plots, nrow=1, ncol=1, top=NULL),
         height = 8.27, width = 11.69, units = "in")
  
  ############
  # Sim bias #
  ############

  sim_bias <- function(dd){
    ggplot(dd, aes(x = ttt, y = sim_error_50_std, color = model_name)) +
                 geom_point(size = 2, shape = 19) +
                 geom_line(size = 0.5, linetype = 2) +
                 facet_wrap(~scenario_name_label) +
                 theme_bw() +
                 labs(
                      y = "Bias  ",
                      x = "Years since treatment",
                      color = "Estimator") +
                 scale_color_manual(values = plot_colors) +
                 guides(color = guide_legend(nrow = 2, byrow = T)) +
                 theme(plot.title = element_text(hjust = 0.5, family = 'sans', size = 18),
                       strip.background = element_blank(),
                       strip.text = element_text(family = "sans", size = 14),
                       legend.position = "bottom",
                       legend.text = element_text(family = 'sans', size = 12),
                       axis.ticks = element_line(linewidth = 1),
                       axis.ticks.length = unit(5.6, "points"),
                       axis.title = element_text(size = 14, family = 'sans'),
                       axis.title.y = element_text(size = 14, family = 'sans', angle = 0),
                       axis.text = element_text(size = 10, family = 'sans'),
                       axis.text.x = element_text(size = 10, family = 'sans',
                                                  margin = margin(t = 5, r = 0, b = 10, l = 0)),
                       legend.title = element_text(family = 'sans', size = 12))
  }
  
  fig_2 <- sim_bias(df_summary) +
          scale_y_continuous(breaks = seq(0, 0.3, 0.1),
                     limits = c(0, 0.3)) 
     
  ggsave(filename = "./time_vary_paper/plots/main_paper/fig_2_bias.pdf", 
          plot = fig_2, height = 8.27, width = 11.69, units = "in")
  
  fig_s2_5 <- sim_bias(df_summary_tunits_5) +
              scale_y_continuous(breaks = seq(0, 0.5, 0.1),
                                 limits = c(0, 0.5)) 
            
  ggsave(filename = "./time_vary_paper/plots/appendix_5_treated/fig_2_bias.pdf", 
         plot = fig_s2_5, height = 8.27, width = 11.69, units = "in")
  
  fig_s2_45 <- sim_bias(df_summary_tunits_45) +
                scale_y_continuous(breaks = seq(0, 0.4, 0.1),
                                   limits = c(0, 0.4)) 
  
  ggsave(filename = "./time_vary_paper/plots/appendix_45_treated/fig_2_bias.pdf", 
         plot = fig_s2_45, height = 8.27, width = 11.69, units = "in")
  
  fig_s2_synth <- sim_bias(df_summary_synth) +
                  scale_y_continuous(breaks = seq(0, 0.3, 0.1),
                                     limits = c(0, 0.3)) 
                
  ggsave(filename = "./time_vary_paper/plots/appendix_synthetic/fig_2_bias.pdf", 
         plot = fig_s2_synth, height = 8.27, width = 11.69, units = "in")
  
  # Summary statistics for paper:
  overall <- df_summary[, mean(.SD$sim_error_50_std), by = "model_name"]
  setorder(overall, V1)
  
  overall_scenario <- df_summary[, mean(.SD$sim_error_50_std), by = "scenario_name_label"]
  
  scenario <- df_summary[, mean(.SD$sim_error_50_std), by = c("model_name", "scenario_name_label")]
  setorder(scenario, scenario_name_label, V1)
   
  ################
  # Sim variance #
  ################
  
  sim_se <- function(dd){
    ggplot(dd, aes(x = ttt, y = model_se_50, color = model_name)) +
                  geom_point(size = 2, shape = 19) +
                  geom_line(size = 0.5, linetype = 2) +
                  facet_wrap(~scenario_name_label) +
                  theme_bw() +
                  labs(y = "Standard \nError",
                       x = "Years since treatment",
                       color = "Estimator") +
                  scale_color_manual(values = plot_colors) +
                  theme(plot.title = element_text(hjust = 0.5, family = 'sans', size = 16),
                        strip.background = element_blank(),
                        strip.text = element_text(family = "sans", size = 10),
                        legend.position = "bottom",
                        legend.text = element_text(family = 'sans', size = 10),
                        axis.ticks = element_line(linewidth = 1),
                        axis.ticks.length = unit(5.6, "points"),
                        axis.title = element_text(size = 12, family = 'sans'),
                        axis.title.y = element_text(size = 12, family = 'sans', angle = 0),
                        axis.text = element_text(size = 10, family = 'sans'),
                        axis.text.x = element_text(size = 10, family = 'sans',
                                                   margin = margin(t = 5, r = 0, b = 10, l = 0)),
                        legend.title = element_text(family = 'sans', size = 14))
  }
  
  fig_3 <- sim_se(df_summary)  +
            scale_y_continuous(breaks = seq(0, 2.5, 0.5),
                               limits = c(0, 2.5)) 
  
  ggsave(filename = "./time_vary_paper/plots/main_paper/fig_3_model_se.pdf", plot = fig_3, 
         height = 8.27, width = 11.69, units = "in")
  
  fig_s3_5 <- sim_se(df_summary_tunits_5)  +
                scale_y_continuous(breaks = seq(0, 3.5, 0.5),
                                   limits = c(0, 3.5)) 
  
  ggsave(filename = "./time_vary_paper/plots/appendix_5_treated/fig_3_model_se.pdf", plot = fig_s3_5, 
         height = 8.27, width = 11.69, units = "in")
  
  fig_s3_45 <- sim_se(df_summary_tunits_45)  +
                scale_y_continuous(breaks = seq(0, 2.5, 0.5),
                                   limits = c(0, 2.5)) 
  
  ggsave(filename = "./time_vary_paper/plots/appendix_45_treated/fig_3_model_se.pdf", plot = fig_s3_45, 
         height = 8.27, width = 11.69, units = "in")
  
  fig_s3_synth <- sim_se(df_summary_synth)  +
                  scale_y_continuous(breaks = seq(0, 2.5, 0.5),
                                     limits = c(0, 2.5)) 
  
  ggsave(filename = "./time_vary_paper/plots/appendix_synthetic/fig_3_model_se.pdf", plot = fig_s3_synth, 
         height = 8.27, width = 11.69, units = "in")
  
  overall <- df_summary[, mean(.SD$model_se_50), by = "model_name"]
  setorder(overall, V1)
  
  overall_scenario <- df_summary[, mean(.SD$model_se_50), by = "scenario_name_label"]
  
  scenario <- df_summary[, mean(.SD$model_se_50), by = c("model_name", "scenario_name_label")]
  setorder(scenario, scenario_name_label, V1)
  
  ############
  # Coverage #
  ############
  
  coverage <- function(dd){
    ggplot(df_summary_2x2, aes(x = ttt, y = coverage, color = model_name)) +
              geom_line(size = 0.5, linetype = 2) +
              geom_point(size = 2, shape = 19) +
              geom_hline(size = 0.5, linetype = 3, yintercept = 0.95, alpha = 0.7) +
              facet_wrap(~scenario_name_label) +
              theme_bw() +
              labs(y = "Coverage",
                   x = "Years since treatment",
                   color = "Estimator") +
              scale_color_manual(values = plot_colors) +
              scale_y_continuous(labels = scales::percent) +
              theme(plot.title = element_text(hjust = 0.5, family = 'sans', size = 16),
                    strip.background = element_blank(),
                    strip.text = element_text(family = "sans", size = 10),
                    legend.position = "bottom",
                    legend.text = element_text(family = 'sans', size = 10),
                    axis.ticks = element_line(linewidth = 1),
                    axis.ticks.length = unit(5.6, "points"),
                    axis.title = element_text(size = 12, family = 'sans'),
                    axis.title.y = element_text(size = 12, family = 'sans', angle = 0),
                    axis.text = element_text(size = 10, family = 'sans'),
                    axis.text.x = element_text(size = 10, family = 'sans',
                                               margin = margin(t = 5, r = 0, b = 10, l = 0)),
                    legend.title = element_text(family = 'sans', size = 14))
  }
  
  ggsave(filename = "./time_vary_paper/plots/main_paper/fig_4_coverage.pdf", 
         plot = coverage(df_summary), 
         height = 8.27, width = 11.69, units = "in")
  
  ggsave(filename = "./time_vary_paper/plots/appendix_5_treated/fig_4_coverage.pdf", 
         plot = coverage(df_summary_tunits_5), 
         height = 8.27, width = 11.69, units = "in")
  
  ggsave(filename = "./time_vary_paper/plots/appendix_45_treated/fig_4_coverage.pdf", 
         plot = coverage(df_summary_tunits_45), 
         height = 8.27, width = 11.69, units = "in")
  
  ggsave(filename = "./time_vary_paper/plots/appendix_synthetic/fig_4_coverage.pdf", 
         plot = coverage(df_summary_synth), 
         height = 8.27, width = 11.69, units = "in")
  
  overall <- df_summary[, mean(.SD$coverage), by = "model_name"]
  setorder(overall, V1)
  
  overall_scenario <- df_summary[, mean(.SD$coverage), by = "scenario_name_label"]
  
  scenario <- df_summary[, mean(.SD$coverage), by = c("model_name", "scenario_name_label")]
  setorder(scenario, scenario_name_label, V1)
  
  ########
  # RMSE #
  ########
  
  rmse <- function(dd){
    ggplot(dd, aes(x = ttt, y = sim_rmse, color = model_name)) +
            geom_point(size = 2, shape = 19) +
            geom_line(size = 0.5, linetype = 2) +
            facet_wrap(~scenario_name_label) +
            theme_bw() +
            labs(y = "RMSE",
                 x = "Years since treatment",
                 color = "Estimator") +
            scale_color_manual(values = plot_colors) +
            theme(plot.title = element_text(hjust = 0.5, family = 'sans', size = 16),
                  strip.background = element_blank(),
                  strip.text = element_text(family = "sans", size = 10),
                  legend.position = "bottom",
                  legend.text = element_text(family = 'sans', size = 10),
                  axis.ticks = element_line(linewidth = 1),
                  axis.ticks.length = unit(5.6, "points"),
                  axis.title = element_text(size = 12, family = 'sans'),
                  axis.title.y = element_text(size = 12, family = 'sans', angle = 0),
                  axis.text = element_text(size = 10, family = 'sans'),
                  axis.text.x = element_text(size = 10, family = 'sans',
                                             margin = margin(t = 5, r = 0, b = 10, l = 0)),
                  legend.title = element_text(family = 'sans', size = 14))
  }
  
  fig_5 <- rmse(df_summary)  +
          scale_y_continuous(breaks = seq(0, 8, 2),
                             limits = c(0, 8)) 
  
  ggsave(filename = "./time_vary_paper/plots/main_paper/fig_5_rmse.pdf", plot = fig_5, 
         height = 8.27, width = 11.69, units = "in")
  
  fig_s5_5 <- rmse(df_summary_tunits_5) +
              scale_y_continuous(breaks = seq(0, 50, 10),
                                 limits = c(0, 50)) 
  
  ggsave(filename = "./time_vary_paper/plots/appendix_5_treated/fig_5_rmse.pdf", plot = fig_s5_5, 
         height = 8.27, width = 11.69, units = "in")
  
  fig_s5_45 <- rmse(df_summary_tunits_45) +
                scale_y_continuous(breaks = seq(0, 10, 2),
                                   limits = c(0, 10)) 
              
  ggsave(filename = "./time_vary_paper/plots/appendix_45_treated/fig_5_rmse.pdf", plot = fig_s5_45, 
         height = 8.27, width = 11.69, units = "in")
  
  fig_s5_synth <- rmse(df_summary_synth)  +
                    scale_y_continuous(breaks = seq(0, 8, 2),
                                       limits = c(0, 8))
  
  ggsave(filename = "./time_vary_paper/plots/appendix_synthetic/fig_5_rmse.pdf", plot = fig_s5_synth, 
         height = 8.27, width = 11.69, units = "in")
  
  overall <- df_summary[, mean(.SD$sim_rmse), by = "model_name"]
  setorder(overall, V1)
  
  overall_scenario <- df_summary[, mean(.SD$sim_rmse), by = "scenario_name_label"]
  
  scenario <- df_summary[, mean(.SD$sim_rmse), by = c("model_name", "scenario_name_label")]
  setorder(scenario, scenario_name_label, V1)
  
}
