# Figure2.R ---------------------------------------------------------------
# 
# Run the new Figure 2 for the revision of the concurrent simulation paper.
# 
# Adam Scherling, 3/29/2022
# Based on prior code by Joe Pane


# set up directories ------------------------------------------------------

outputPath <- ''
dataPath <- ''


# load libraries ----------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(glue)
library(gridExtra)
library(magrittr)
library(patchwork)


# load the data -----------------------------------------------------------

concurrent_instant_unordered <- read.csv(glue("{dataPath}/instant_unordered.csv"), stringsAsFactors = FALSE)

concurrent_runs <- bind_rows(
  concurrent_instant_unordered %>% mutate(simulation = 'instant_unordered')
)


# prepare the data for plotting -------------------------------------------

# keep only the linear models
concurrent_runs %<>% filter(
  model_name %in% c("autoreg_linear", "autoreg_linear_misspec", "fixedeff_linear", "fixedeff_linear_misspec")
)

# keep only the desired SEs
concurrent_runs %<>% filter(
  (model_name %in% c("autoreg_linear", "autoreg_linear_misspec") & se_adjustment == "none") |
    (model_name %in% c("fixedeff_linear", "fixedeff_linear_misspec") & se_adjustment == "cluster")
)

# drop estimates of the joint effect
concurrent_runs %<>% filter(coefficient!='joint_effect')

# keep only the -10% effect
concurrent_runs %<>% filter(effect_direction=='neg', 
                            true_effect=='0.457936376121782, 0.457936376121782') %>%
  mutate(effect_size = "-10%/-10%")

# clean up some variables for the plot
concurrent_runs <- concurrent_runs %>%
  mutate(specification = if_else(grepl("treatment2", model_formula),
                                 "Correctly Specified (Secondary Policy Included)", 
                                 "Misspecified (Secondary Policy Omitted)"),
         policy_speed = if_else(policy_speed == "instant", "Instant", "Slow"),
         coefficient = if_else(coefficient == "treatment1", "Primary Policy", "Co-occurring Policy"),
         coefficient = factor(coefficient, levels=c("Primary Policy", "Co-occurring Policy")),
         avg_mean_distance = round(avg_mean_distance),
         class = case_when(model_name %in% c("autoreg_linear", "autoreg_linear_misspec") ~ "Linear Autoregressive",
                           model_name %in% c("fixedeff_linear", "fixedeff_linear_misspec") ~ "Linear Two-Way Fixed Effects",
                           TRUE ~ NA_character_),
         true_effect_numeric = case_when(coefficient == "Primary Policy" ~ effect_magnitude1,
                                         coefficient == "Co-occurring Policy" ~ effect_magnitude2,
                                         TRUE ~ NA_real_),
         true_effect_numeric = case_when(effect_direction == "neg" ~ -1*true_effect_numeric,
                                         effect_direction == "pos" ~ true_effect_numeric,
                                         TRUE ~ 0),
         rel_bias = bias/true_effect_numeric,
         RMSE = sqrt(mse),
         bias = abs(bias)) %>%
  rename(type_s_error = types_error)


# Figure 1: instant, unordered policies, correctly specified linear models ----

fig_data <- concurrent_runs %>%
  filter(coefficient=='Primary Policy') %>% 
  nest_by(class) %>%
  mutate(data = list(data %>% 
           filter(n_units==30,
                  specification == "Misspecified (Secondary Policy Omitted)",
                  rho==0.9) %>% 
           select(coefficient, effect_direction, effect_size, specification,
                  policy_speed, n_units, rho, years_apart, avg_mean_distance, 
                  type_s_error, rel_bias, variance, RMSE, coverage) %>%
           gather(key=metric, value=value, type_s_error:coverage) %>%
           mutate(metric = factor(
             metric,
             levels=c("type_s_error", "rel_bias", "variance", "RMSE", "coverage"),
             labels=c("Type S Error", "Relative Bias","Variance", "RMSE", "Coverage"))) %>%
           filter(metric %in% c("Relative Bias","Variance")))) %>%
  unnest(cols = 'data')

autoreg_relbias <- fig_data %>%
  filter(class=='Linear Autoregressive',
         metric=='Relative Bias') %>% 
  ggplot(aes(x=factor(years_apart), y=value)) + #x=factor(rho)
  geom_bar(stat="identity", aes(fill=coefficient), 
           position=position_dodge2(width=0.9, preserve="single")) +
  facet_wrap(~ metric, scales="free_y") +
  scale_x_discrete(drop=FALSE, labels=c("0-1", "3-4", "6-7", "9-10")) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L), 
                     limits = c(-0.2,1)) + 
  theme_bw() +
  theme(legend.title = element_blank(),
        text = element_text(size=16)) +
  ylab("") + #Value
  xlab("Years Apart") + 
  scale_fill_grey()

autoreg_variance <- fig_data %>%
  filter(class=='Linear Autoregressive',
         metric=='Variance') %>% 
  ggplot(aes(x=factor(years_apart), y=value)) + #x=factor(rho)
  geom_bar(stat="identity", aes(fill=coefficient), 
           position=position_dodge2(width=0.9, preserve="single")) +
  facet_wrap(~ metric, scales="free_y") +
  scale_x_discrete(drop=FALSE, labels=c("0-1", "3-4", "6-7", "9-10")) +
  scale_y_continuous(limits = c(0,0.3)) + 
  theme_bw() +
  theme(legend.title = element_blank(),
        text = element_text(size=16)) +
  ylab("") + #Value
  xlab("Years Apart") + 
  scale_fill_grey()

twowayfe_relbias <- fig_data %>%
  filter(class=='Linear Two-Way Fixed Effects',
         metric=='Relative Bias') %>% 
  ggplot(aes(x=factor(years_apart), y=value)) + #x=factor(rho)
  geom_bar(stat="identity", aes(fill=coefficient), 
           position=position_dodge2(width=0.9, preserve="single")) +
  facet_wrap(~ metric, scales="free_y") +
  scale_x_discrete(drop=FALSE, labels=c("0-1", "3-4", "6-7", "9-10")) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L), 
                     limits = c(-0.2,1)) + 
  theme_bw() +
  theme(legend.title = element_blank(),
        text = element_text(size=16)) +
  ylab("") + #Value
  xlab("Years Apart") + 
  scale_fill_grey()

twowayfe_variance <- fig_data %>%
  filter(class=='Linear Two-Way Fixed Effects',
         metric=='Variance') %>% 
  ggplot(aes(x=factor(years_apart), y=value)) + #x=factor(rho)
  geom_bar(stat="identity", aes(fill=coefficient), 
           position=position_dodge2(width=0.9, preserve="single")) +
  facet_wrap(~ metric, scales="free_y") +
  scale_x_discrete(drop=FALSE, labels=c("0-1", "3-4", "6-7", "9-10")) +
  scale_y_continuous(limits = c(0,1.5)) + 
  theme_bw() +
  theme(legend.title = element_blank(),
        text = element_text(size=16)) +
  ylab("") + #Value
  xlab("Years Apart") + 
  scale_fill_grey()


fig_top <- autoreg_relbias + theme(legend.position = 'none') + autoreg_variance + theme(legend.position = 'none') + 
  plot_annotation(title = 'Autoregressive model',
                  theme = theme(plot.title = element_text(hjust=0.55, face='bold.italic')))

fig_bottom <- twowayfe_relbias + theme(legend.position = 'none') + twowayfe_variance + theme(legend.position = 'none') + 
  plot_annotation(title = '2-way fixed effects model',
                  theme = theme(plot.title = element_text(hjust=0.55, face='bold.italic')))

wrap_elements(fig_top) / wrap_elements(fig_bottom)

ggsave(glue('{outputPath}/new_figures/Figure2.png'), width=10, height=9)



