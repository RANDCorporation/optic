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

regenerate <- T
plots <- T
synthetic <- T

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

long_names <- c("Ramp up", "Sunset", "Temporary", "Variable")
scenarios[, scenario_name_label := mapvalues(scenario_name, 
                                             c("I", "II", "III", "IV"),
                                             long_names)]

scenarios[, scenario_name_label := factor(scenario_name_label,
                                          levels = long_names)]

scenarios[, smd := -1*smd]

p <- ggplot(scenarios, aes(x = ttt, y = smd)) +
    geom_point(size = 2.5) +
    geom_line(linewidth = 0.8) +
    facet_wrap(~scenario_name_label) +
    labs(x = "Time since treatment",
         y = "Standardized \nMean Difference") +
    scale_y_continuous(breaks = seq(-0.2, 0, 0.05),
                       limits = c(-0.2, 0)) +
    theme_bw() +
    theme(strip.background = element_blank(),
          strip.text = element_text(family = 'sans', size = 14),
          axis.text = element_text(family = 'sans', size = 10),
          axis.title.x = element_text(family = 'sans', size = 12),
          axis.title.y = element_text(family = 'sans', size = 12, angle = 0, vjust = 0.5))

ggsave(filename = "./time_vary_plots/time_vary_scenarios_10_16.svg", plot = p, height = 6, width = 8, units = "in")


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
  fake_states <- fread("./time_vary_plots/fake_states.csv", fill = T)
  
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
  
  #ggplot(df_sim, aes(x = crude.rate, color = observed_data)) + geom_density(size = 1) + facet_wrap(~year) + theme_bw()
  #ggplot(df_sim, aes(x = unemploymentrate, color = observed_data)) + geom_density(size = 1) + facet_wrap(~year) + theme_bw()
  
  ggplot(df_sim[1:1938,], aes(y = crude.rate, x = year, color = state)) + geom_line() + facet_wrap(~observed_data) + theme_bw() + theme(legend.position = "none")
  
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
                      time = as.name("year")),
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
  
  m_ar <- optic_model(
    name = "autoregressive",
    type = "autoreg",
    call = "feols",
    formula = crude.rate ~ unemploymentrate + i(time_to_treat, ref = c(-1)) | year,
    se_adjust = "none"
    
  )
  
  m_ar_cluster <- optic_model(
    name = "autoregressive_cluster",
    type = "autoreg",
    call = "feols",
    formula = crude.rate ~ unemploymentrate + i(time_to_treat, ref = c(-1)) | year,
    model_args = list(cluster = "state"),
    se_adjust = "none"
    
  )
  
  # Adding in ar w/o clustering
  sim_models <- list(m_es, m_ar, m_sa, m_csa, m_g, m_bjs, m_aug, m_ar_cluster)

  sim_config <- optic_simulation(
    
    x                        = df_sim,
    models                   = sim_models,
    iters                    = 100,
    method                   = "time_varying",
    unit_var                 = "state",
    treat_var                = "state",
    time_var                 = "year",
    effect_magnitude         = time_varying_scenarios,
    n_units                  = c(280),
    effect_direction         = c("neg"),
    policy_speed             = c("instant"),
    n_implementation_periods = c(6)
    
  )
  
  sim_results <- dispatch_simulations(
    
    sim_config,
    seed = 9780,
    verbose = 0,
    use_future = F,
    future.packages = c("didimputation", "did2s", "optic","augsynth", "fixest", "tidyr")
    
  )
  
  sim_results <- rbindlist(sim_results)
  
  write.csv(sim_results,  "sim_results_synthetic_10_18.csv", row.names = F)
  
}

if (plots){
  
  df_sim <- fread("./sim_results2.csv")
  
  df_sim[, scenario := paste(effect_magnitude1, effect_magnitude2, effect_magnitude3, effect_magnitude4, effect_magnitude5, effect_magnitude6)]
  
  # Reshape effect columns long; this should probably occur during the simulation procedure
  id_cols <- c("outcome", "n_implementation_periods", "n_units", "policy_speed", "effect_direction", "model_formula", "model_name", "model_call", "scenario", "iter")
  
  sum_cols <- c("n_units", "scenario", "model_name", "ttt")
  
  df_sim <- melt(df_sim, id.vars = id_cols, measure = patterns(estimate = "^estimate_ttt==", se = "^se_ttt==", variance = "^variance_ttt==", t_stat = "^t_stat_ttt==", p_value = "^p_value_ttt==",  true_effect = "^effect_magnitude"), variable.name = "ttt")
  
  # Index time to treat correctly:
  df_sim[, ttt := as.numeric(ttt)]
  df_sim[, ttt := ttt - 1]
  
  # Calculate GoF statistics
  
  #Median sim error & 2.5th/97.5th percentiles
  df_sim[, true_effect := -1*true_effect]
  df_sim[, sim_error := true_effect - estimate]
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
  
  df_sim[, model_se_mean := mean(.SD$se), by = sum_cols]
  df_sim[, model_se_025 := mean(.SD$se) - 1.96*(sd(.SD$se)), by = sum_cols]
  df_sim[, model_se_975 := mean(.SD$se) + 1.96*(sd(.SD$se)), by = sum_cols]
  
  # Change variable coding to be a little more informative on scenarios, model names,
  # and time to treat
  
  scenario_names <- data.frame("scenario_name" = c("I", "II",
                                                   "III", "IV"),
                               "scenario" = sapply(time_varying_scenarios, paste, collapse = " "))
  
  df_summary <- merge(df_sim, scenario_names, by = "scenario")
  df_summary[, scenario_name := factor(scenario_name, levels = scenario_names$scenario_name)]
  
  model_names_remap <- data.frame("new_model_name" = c("Event study: Two-way Fixed Effect", "Event study: AR(1)", "Event study: AR(1) (No clustering)", "Event study: Sun & Abraham", "Callaway & Sant'Anna", 
                                                       "Two-stage DiD", "DiD imputation", "Augmented synthetic controls"),
                                  "model_name" = c("twfe", "autoregressive_cluster", "autoregressive_no_cluster", "sa", "csa_did", "gardner", "bjs", "augsynth"))
  
  df_summary <- merge(df_summary, model_names_remap, by = "model_name")
  df_summary[, model_name := factor(model_name, levels = model_names_remap$new_model_name)]
  
  df_summary[, n_units_name := paste0(n_units, " treated units")]
  df_summary[, n_units_name := factor(n_units_name, levels = unique(df_summary$n_units_name))]
  
  df_summary[, scenario := NULL]
  df_summary[, model_name := NULL]
  setnames(df_summary, "new_model_name", "model_name")
  
  # Collapse to summaries:
  df_summary <- unique(df_summary[, .(n_units, n_units_name, scenario_name, model_name, ttt,
                                      sim_error_50, sim_error_025,
                                      sim_error_975, sim_error_50_std, sim_error_025_std, sim_error_975_std,
                                      model_est_mean, model_est_025, model_est_975, model_se_mean, model_se_025, model_se_975,
                                      coverage, sim_rmse, sim_variance)])
  
  # Remove ar model w/o clustering and adjust time-to-treatment name
  df_summary <- df_summary[model_name != "Event study: AR(1) (No clustering)"]
  
  # Add on true effects to calculate percentages for plots:
  df_summary <- join(df_summary, scenarios, by = c("ttt", "scenario_name"), type = "left")
  
  # 2x2, holding number of treated units fixed:
  df_summary_2x2 <- df_summary[n_units_name == "25 treated units"]
  
  plot_colors_bw <- c('#d9d9d9','#bdbdbd','#969696','#737373','#525252','#252525','#000000')
  plot_colors_col <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f', '#cab2d6')
  
  plot_colors <- plot_colors_col
  
  #############
  # GOF Plots #
  #############
  
  ################################
  # Mean error v. variance error #
  ################################
  
  # Scale size of PDF to improve sizing of points for canvas.
  # I played around with this number to find an aesthetic I preferred.
  
  mult <- 1.1
  
  bias_variance <- function(dd, n_treat, scene){
    
    dd <- dd[n_units == n_treat & scenario_name == scene,]
    
    plot_scatter <- ggplot(dd, aes(y = model_se_mean, x = sim_error_mean, color = model_name)) +
      facet_wrap(~ttt, nrow = 2) +
      geom_hline(yintercept = 0, linetype = 21) +
      geom_point(size = 3, shape = 19) +
      labs(title = sprintf("Model error v. model variance: \n%s treated units and %s effects", n_treat, scene),
           y = "Mean model \nerror",
           x = "Mean model \nvariance",
           color = "Estimator") +
      geom_errorbar(aes(ymin = model_se_025, ymax = model_se_975),  linewidth = 0.5, width = 0) +
      geom_errorbarh(aes(xmin = sim_error_025, xmax = sim_error_975), linewidth = 0.5, width = 0) +
      theme_bw() +
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
  
  args <- expand.grid(n_treat = unique(df_summary$n_units),
                      scene = unique(df_summary$scenario_name))
  
  args <- with(args, args[order(scene),])
  
  bias_v_variance_plots <- mapply(bias_variance, n_treat = args$n_treat, scene = args$scene, MoreArgs = list(dd = df_summary),
                                  SIMPLIFY = F, USE.NAMES = F)
  
  ggsave(filename = "./time_vary_plots/time_vary_model_error_v_variance.pdf", plot = marrangeGrob(bias_v_variance_plots, nrow=1, ncol=1, top=NULL), 
         height = 8.27*mult, width = 11.69*mult, units = "in")
  
  ############
  # Sim bias #
  ############

  plot_bias <- ggplot(df_summary, aes(x = ttt, y = sim_error_50_std, color = model_name)) +
                geom_hline(yintercept = 0, linetype = 21) +
                geom_point(size = 1, shape = 19, position = position_dodge(width = 0.8)) +
                geom_errorbar(aes(ymin = sim_error_025_std, ymax = sim_error_975_std), linewidth = 0.5, 
                              width = 0, position = position_dodge(width = 0.8)) +
                facet_grid(rows = vars(n_units_name), cols = vars(scenario_name), labeller = "label_value") +
                theme_bw() +
                labs(title = "Median standardized bias (2.5th, 97.5th percentiles) across simulation runs",
                     y = "Bias",
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
  
  ggsave(filename = "./time_vary_plots/time_vary_sim_bias_ui.pdf", plot = plot_bias, height = 8.27*mult, width = 11.69*mult, units = "in")
  
  plot_bias_2x2 <- ggplot(df_summary_2x2, aes(x = ttt, y = sim_error_50_std, color = model_name)) +
                geom_hline(yintercept = 0, linetype = 21) +
                geom_point(size = 1, shape = 19, position = position_dodge(width = 0.8)) +
                geom_errorbar(aes(ymin = sim_error_025_std, ymax = sim_error_975_std), linewidth = 0.5, 
                              width = 0, position = position_dodge(width = 0.8)) +
                facet_wrap(~scenario_name) +
                theme_bw() +
                labs(title = "Median standardized bias (2.5th, 97.5th percentiles) across simulation runs",
                     y = "Bias",
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
  
  ggsave(filename = "./time_vary_plots/time_vary_sim_bias_ui_2x2.pdf", plot = plot_bias, height = 8.27*mult, width = 11.69*mult, units = "in")
  
  # W/O uncertainty:
  plot_bias_no <- ggplot(df_summary_2x2, aes(x = as.factor(ttt), y = sim_error_50_std, color = model_name)) +
                  geom_hline(yintercept = 0, linetype = 21) +
                  geom_point(size = 2, shape = 19, position = position_dodge(width = 0.8)) +
                 geom_linerange(aes(ymin = 0, ymax = sim_error_50_std,
                                    xmin = as.factor(ttt), xmax = as.factor(ttt)), 
                                position = position_dodge(width = 0.8), size = 1) +
                  facet_wrap(~scenario_name) +
                  theme_bw() +
                  labs(title = "Median standardized bias (2.5th, 97.5th percentiles) across simulation runs",
                       y = "Bias",
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
 
   ggsave(filename = "./time_vary_plots/time_vary_sim_bias_no_ui.pdf", plot = plot_bias_no, height = 8.27*mult, width = 11.69*mult, units = "in")
   
   # Simplified figure for presentation:
   sanzo_cols <- sanzo::quads$c252
   selected <- c("Augmented synthetic controls", "Event study: AR(1)",  "Event study: Two-way Fixed Effect",
                 "Callaway & Sant'Anna")
   
   df_summary_simple <- df_summary_2x2[scenario_name %in% c("I", "II") & model_name %in% selected]
   df_summary_simple[, model_name := factor(model_name, selected)]
   
   plot_bias_simple1 <- ggplot(df_summary_simple, aes(x = as.factor(ttt), y = sim_error_50_std, color = model_name)) +
               geom_linerange(aes(ymin = 0, ymax = sim_error_50_std,
                                  xmin = as.factor(ttt), xmax = as.factor(ttt)),
                              position = position_dodge(width = 0.7), size = 3) +
               facet_wrap(~scenario_name_label) +
               theme_bw() +
               labs(title = "Median standardized bias across simulation runs\n",
                    y = "Bias  ",
                    x = "Years since treatment",
                    color = "Estimator") +
               scale_color_manual(values = sanzo_cols) +
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
   
   ggsave(filename = "./time_vary_plots/bias_simple_advisory_opt1.svg", 
          plot = plot_bias_simple1, height = 8.27*mult, width = 11.69*mult, units = "in")
   
   plot_bias_simple2 <- ggplot(df_summary_simple, aes(x = ttt, y = sim_error_50_std, color = model_name)) +
                       geom_point(size = 2, shape = 19) +
                       geom_line(size = 1) +
                       facet_wrap(~scenario_name_label) +
                       theme_bw() +
                       labs(title = "Median standardized bias across simulation runs\n",
                            y = "Bias  ",
                            x = "Years since treatment",
                            color = "Estimator") +
                       scale_color_manual(values = sanzo_cols) +
                       guides(color = guide_legend(nrow = 2, byrow = T)) +
                       scale_y_continuous(breaks = seq(0, 0.4, 0.1),
                                          limits = c(0, 0.4)) +
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
   
   ggsave(filename = "./time_vary_plots/bias_simple_advisory_opt2.svg", 
          plot = plot_bias_simple2, height = 8.27*mult, width = 11.69*mult, units = "in")
   
   # As percent diff:
   df_summary_2x2[, bias_rate := sim_error_50_std/true_effect]
   
   plot_bias_percent <- ggplot(df_summary_2x2, aes(x = as.factor(ttt), y = bias_rate, color = model_name)) +
                         geom_hline(yintercept = 0, linetype = 21) +
                         geom_point(size = 2, shape = 19, position = position_dodge(width = 0.8)) +
                         geom_linerange(aes(ymin = 0, ymax = bias_rate,
                                            xmin = as.factor(ttt), xmax = as.factor(ttt)), 
                                        position = position_dodge(width = 0.8), size = 1) +
                         facet_wrap(~scenario_name) +
                         theme_bw() +
                         labs(title = "Median standardized bias (2.5th, 97.5th percentiles) across simulation runs",
                              y = "Bias",
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
   
   ggsave(filename = "./time_vary_plots/time_vary_sim_bias_no_ui.pdf", plot = plot_bias_no, height = 8.27*mult, width = 11.69*mult, units = "in")
    
  ################
  # Sim variance #
  ################
   
   plot_var_2x2 <- ggplot(df_summary_2x2, aes(x = ttt, y = sim_variance, color = model_name)) +
                   geom_hline(yintercept = 0, linetype = 21) +
                   geom_point(size = 1, shape = 19, position = position_dodge(width = 0.8)) +
                   geom_errorbar(aes(ymin = 0, ymax = sim_variance), linewidth = 0.5, 
                                 width = 0, position = position_dodge(width = 0.8)) +
                   facet_wrap(~scenario_name) +
                   theme_bw() +
                   labs(title = "Variance of estimated effects across simulation runs",
                        y = "Variance",
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
   
   ggsave(filename = "./time_vary_plots/time_vary_sim_variance_ui_2x2.pdf", plot = plot_var_2x2, 
          height = 8.27*mult, width = 11.69*mult, units = "in")
   
  ########################
  # Model standard error #
  ########################
  
  plot_var <- ggplot(df_summary, aes(x = ttt, y = model_se_mean, color = model_name)) +
              geom_hline(yintercept = 0, linetype = 21) +
              geom_point(size = 1, shape = 19, position = position_dodge(width = 0.8)) +
              geom_errorbar(aes(ymin = model_se_025, ymax = model_se_975), linewidth = 0.5, 
                            width = 0, position = position_dodge(width = 0.8)) +
              facet_grid(rows = vars(n_units_name), cols = vars(scenario_name), labeller = "label_value") +
              theme_bw() +
              labs(title = "Median standard error (2.5th, 97.5th percentiles) across simulation runs",
                   y = "Standard error",
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
  
  ggsave(filename = "./time_vary_plots/time_vary_se.pdf", plot = plot_var, height = 8.27*mult, width = 11.69*mult, units = "in")
  
  plot_var_no <- ggplot(df_summary_2x2, aes(x = as.factor(ttt), y = model_se_mean, color = model_name)) +
    geom_hline(yintercept = 0, linetype = 21) +
    geom_point(size = 2, shape = 19, position = position_dodge(width = 0.8)) +
    geom_linerange(aes(ymin = 0, ymax = model_se_mean,
                       xmin = as.factor(ttt), xmax = as.factor(ttt)), 
                   position = position_dodge(width = 0.8), size = 1) +
    facet_wrap(~scenario_name) +
    theme_bw() +
    labs(title = "Median Standard Error (2.5th, 97.5th percentiles) across simulation runs",
         y = "Standard Error",
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
  
  ggsave(filename = "./time_vary_plots/time_vary_se_no_ui.pdf", plot = plot_var_no, height = 8.27*mult, width = 11.69*mult, units = "in")
  
  
  ############
  # Coverage #
  ############
  
  plot_cover <- ggplot(df_summary, aes(x = ttt, y = coverage, color = model_name)) +
    geom_point(size = 1.5, shape = 19) +
    geom_line(aes(group = model_name), linewidth = 0.7) +
    facet_grid(rows = vars(n_units_name), cols = vars(scenario_name), labeller = "label_value") +
    theme_bw() +
    labs(title = "Percent of sim runs with true effect \ncovered by estimated effect",
         y = "Coverage",
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
  
  plot_cover_2x2 <- ggplot(df_summary_2x2, aes(x = ttt, y = coverage, color = model_name)) +
              geom_point(size = 1.5, shape = 19) +
              geom_line(aes(group = model_name), linewidth = 0.7) +
              facet_wrap(~scenario_name, labeller = "label_value") +
              theme_bw() +
              labs(title = "Percent of sim runs with true effect \ncovered by estimated effect",
                   y = "Coverage",
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
  
  
  ggsave(filename = "./time_vary_plots/time_vary_sim_coverage.pdf", plot = plot_cover, height = 8.27*mult, width = 11.69*mult, units = "in")
  ggsave(filename = "./time_vary_plots/time_vary_sim_coverage_2x2.pdf", plot = plot_cover_2x2, height = 8.27*mult, width = 11.69*mult, units = "in")
  
  ########
  # RMSE #
  ########
  
  plot_rmse <- ggplot(df_summary, aes(x = ttt, y = sim_rmse, color = model_name)) +
              geom_point(size = 1, shape = 19, position = position_dodge(width = 0.8)) +
              geom_linerange(aes(x = ttt, ymin = 0, ymax = sim_rmse), 
                             linewidth = 0.6, position = position_dodge(width = 0.8)) +
              facet_grid(rows = vars(n_units_name), cols = vars(scenario_name), labeller = "label_value") +
              theme_bw() +
              labs(title = "RMSE of sim runs",
                   y = "RMSE",
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
  
  ggsave(filename = "./time_vary_plots/time_vary_sim_rmse.pdf", plot = plot_rmse, height = 8.27*mult, width = 11.69*mult, units = "in")

  plot_rmse_2x2 <- ggplot(df_summary_2x2, aes(x = ttt, y = sim_rmse, color = model_name)) +
    geom_point(size = 1, shape = 19, position = position_dodge(width = 0.8)) +
    geom_linerange(aes(x = ttt, ymin = 0, ymax = sim_rmse), 
                   linewidth = 0.6, position = position_dodge(width = 0.8)) +
    facet_wrap(~scenario_name) +
    theme_bw() +
    labs(title = "RMSE of sim runs",
         y = "RMSE",
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
  
  ggsave(filename = "./time_vary_plots/time_vary_sim_rmse_2x2.pdf", plot = plot_rmse_2x2, height = 8.27*mult, width = 11.69*mult, units = "in")
  
}
