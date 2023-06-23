

#------------------------------------------------------------------------------#
# Paper Title
# Author: Pedro Nascimento de Lima
# Copyright (C) 2022
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

library(showtext)
library(ggplot2)

g_font <- "Noto Sans" 
mono_font <- "Roboto Mono"

font_add_google(name = g_font,family = g_font)
font_add_google(name = mono_font,family = mono_font)

# Plot Theme from The RAND Corporation
# This theme is based on a theme available from randplot
# This is intended to be used by RANDites.
# If you are not affiliated in any way with RAND, I recommend you should use another theme.

base_theme <- randplot::theme_rand(font = g_font) + theme(
                                                          # Use mono font when aligning axis.text
                                                          #axis.text = element_text(family = mono_font),
                                                          legend.position="top",
                                                          axis.text.x = element_text(size = 8),
                                                          legend.spacing.x = unit(0, 'cm'),
                                                          panel.spacing.y=unit(0.5, "lines"),
                                                          panel.grid.minor=element_blank(),
                                                          panel.grid.major=element_blank())

showtext_auto()

# Define a cohesive pallete if needed:
# n_models = 2
# From color brewer:
# https://colorbrewer2.org/#type=sequential&scheme=BuPu&n=3
#models_colors = c("#8856a7", "#9ebcda")

# prepare the data for plotting -------------------------------------------

prep_plot_data = function(sim_summaries, adjust_coverage = T) {
  
  sim_summaries %>%
    mutate(
      # PNL note: should we comment this out?
      # Max edit: I removed the mutation to calculate coverage, which for whatever reason was setting all values equal to the calculation for the first value.
      rmse = sqrt(mse),
      bias_size = factor(
        bias_size, levels=c("small", "medium", "large"), 
        labels=c(" Small Confounding", " Moderate Confounding", " Large Confounding")
      ),
      model_name = factor(
        model_name, levels=c("fixedeff_linear", "fixedeff_linear_wt",
                             "autoreg_linear", "autoreg_linear_wt",
                             "multisynth", 
                             "did", "did_wt"),
        labels=c("Linear Two-Way\nFixed Effects Model", "Linear Two-Way\nFixed Effects Model (Weighted)", 
                 "Linear Autoregressive\nModel", "Linear Autoregressive\nModel (Weighted)", 
                 "Augmented SCM", 
                 "Callaway Sant'Anna", "Callaway Sant'Anna (Weighted)")
      ),
      effect_magnitude_rounded = round(effect_magnitude, 2)
    ) %>% 
    select(model_name, effect_magnitude_rounded, prior_control, bias_type, bias_size, bias_es, bias, variance, rmse, var, coverage) %>%
    pivot_longer(cols = c(bias_es, bias, variance, rmse, var, coverage),
                 names_to = "stat", values_to = "value") %>%
    group_by(stat) %>%
    mutate(y_min=0,
           y_max = case_when(stat == "bias_es" ~ 0.4,
                             stat == "bias" ~ 3,
                             stat == "variance" ~ 4,
                             stat == "rmse" ~ 3,
                             stat == "var" ~ 4,
                             stat == "coverage" ~ 1),
           stat = case_when(stat == "bias_es" ~ "Bias (effect size scale)",
                            stat == "bias" ~ "Bias (original scale)",
                            stat == "variance" ~ "Variance",
                            stat == "rmse" ~ "RMSE",
                            stat == "var" ~ "Empirical variance",
                            stat == "coverage" ~ "Coverage"),
           bias_type = case_when(bias_type == "linear" ~ "Linear",
                                 bias_type == "nonlinear" ~ "Nonlinear"),
           prior_control = case_when(prior_control == "mva3" ~ "levels",
                                 prior_control == "trend" ~ "trends"),
           bias_type_prior_control = paste0(bias_type, " - ", prior_control)
           )
          
}

# define a function for plotting results ----------------------------------

makeplot <- function(plotdata, statistic, ncol =1) {
  plotdata %>% 
    filter(
           #effect_magnitude_rounded == effectMagnitude,
           #prior_control %in% priorControl,
           #bias_type %in% biasType,
           stat == statistic) %>%
    ggplot(aes(x=model_name, y=value, fill=bias_size)) +
    geom_col(position=position_dodge(), width=0.8) +
    facet_wrap(~ bias_type_prior_control, ncol=ncol, scales = "free_x") +
    geom_blank(aes(y = y_min)) +
    geom_blank(aes(y = y_max)) +
    scale_fill_manual(values=c("lightblue", "blue", "darkblue")) +#"lightskyblue1","lightskyblue", "blue", "darkblue"
    #scale_y_continuous(scales="free_y") +
    #theme_bw() +
    base_theme +
    xlab(NULL) +
    ylab(statistic) +
    theme(
      axis.title.y = element_text(size = 12),
      legend.title = element_blank(),
      legend.position = "top",
      text = element_text(size=text_size)
    ) +
    guides(fill=guide_legend(nrow=1,byrow=TRUE))
}
