# Figure6.R ---------------------------------------------------------------
# 
# Run the new Figure 6 for the revision of the concurrent simulation paper.
# 
# Adam Scherling, 3/30/2022
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

concurrent_n30_nextra5_unordered <- read.csv(glue("{dataPath}/n30_nextra5_unordered.csv"), stringsAsFactors = FALSE)
concurrent_n10_nextra10_unordered <- read.csv(glue("{dataPath}/n10_nextra10_unordered.csv"), stringsAsFactors = FALSE)

concurrent_runs <- bind_rows(
  concurrent_n30_nextra5_unordered %>% mutate(simulation = 'n30_nextra5_unordered'),
  concurrent_n10_nextra10_unordered %>% mutate(simulation = 'n10_nextra10_unordered')
)


# prepare the data for plotting -------------------------------------------

# keep only the correctly specified linear AR models
concurrent_runs %<>% filter(model_name=="autoreg_linear",
                            se_adjustment == "none")

# drop estimates of the joint effect
concurrent_runs %<>% filter(coefficient!='joint_effect')

# keep only the -10% effect
concurrent_runs %<>% filter(effect_direction=='neg', 
                            true_effect=='0.457936376121782, 0.457936376121782') %>%
  mutate(effect_size = "-10%/-10%")

# clean up some variables for the plot
concurrent_runs <- concurrent_runs %>%
  mutate(policy_speed = if_else(policy_speed == "instant", "Instant", "Slow"),
         coefficient = if_else(coefficient == "treatment1", "Primary Policy", "Co-occurring Policy"),
         coefficient = factor(coefficient, levels=c("Primary Policy", "Co-occurring Policy")),
         avg_mean_distance = round(avg_mean_distance),
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


# Figure 6: instant, unordered policies, correctly specified linear models ----

fig_data <- concurrent_runs %>%
  nest_by(n_units, n_extra) %>%
  mutate(data = list(data %>% 
           filter(rho==0.9) %>% 
           select(coefficient, effect_direction, effect_size, specification,
                  policy_speed, rho, years_apart, avg_mean_distance, 
                  type_s_error, rel_bias, variance, RMSE, coverage) %>%
           gather(key=metric, value=value, type_s_error:coverage) %>%
           mutate(metric = factor(
             metric,
             levels=c("type_s_error", "rel_bias", "variance", "RMSE", "coverage"),
             labels=c("Type S Error", "Relative Bias","Variance", "RMSE", "Coverage"))) %>%
           filter(metric %in% c("Relative Bias","Variance")))) %>%
  unnest(cols = 'data')

n30_relbias <- fig_data %>%
  filter(n_units==30,
         metric=='Relative Bias') %>% 
  ggplot(aes(x=factor(years_apart), y=value)) + #x=factor(rho)
  geom_bar(stat="identity", aes(fill=coefficient), 
           position=position_dodge2(width=0.9, preserve="single")) +
  facet_wrap(~ metric, scales="free_y") +
  scale_x_discrete(drop=FALSE, labels=c("0-1", "3-4", "6-7", "9-10")) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L), 
                     limits = c(-.25,.25)) + 
  theme_bw() +
  theme(legend.title = element_blank(),
        text = element_text(size=16)) +
  ylab("") + #Value
  xlab("Years Apart") + 
  scale_fill_grey()

n30_variance <- fig_data %>%
  filter(n_units==30,
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

n10_relbias <- fig_data %>%
  filter(n_units==10,
         metric=='Relative Bias') %>% 
  ggplot(aes(x=factor(years_apart), y=value)) + #x=factor(rho)
  geom_bar(stat="identity", aes(fill=coefficient), 
           position=position_dodge2(width=0.9, preserve="single")) +
  facet_wrap(~ metric, scales="free_y") +
  scale_x_discrete(drop=FALSE, labels=c("0-1", "3-4", "6-7", "9-10")) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L), 
                     limits = c(-.25,.25)) + 
  theme_bw() +
  theme(legend.title = element_blank(),
        text = element_text(size=16)) +
  ylab("") + #Value
  xlab("Years Apart") + 
  scale_fill_grey()

n10_variance <- fig_data %>%
  filter(n_units==10,
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


fig_base <- (n30_relbias + plot_spacer()) / plot_spacer() + 
  plot_layout(widths = c(2, 2, 1), guides = 'collect')

fig_top <- n30_relbias + theme(legend.position = 'none') + n30_variance + theme(legend.position = 'none') + 
  plot_annotation(title = '30 states both policies, plus\n5 extra states only primary policy, 5 extra only co-occurring',
                  theme = theme(plot.title = element_text(hjust=0.57, face='bold.italic')))

fig_bottom <- n10_relbias + theme(legend.position = 'none') + n10_variance + theme(legend.position = 'none') + 
  plot_annotation(title = '30 states: 10 both policies; 10 only primary; 10 only co-occurring',
                  theme = theme(plot.title = element_text(hjust=0.57, face='bold.italic')))

fig_h <- 2.2

fig_base + 
  inset_element(wrap_elements(fig_bottom), left=0, bottom=0, right=0.8, top=fig_h/2, align_to = 'full', clip=FALSE) + 
  inset_element(wrap_elements(fig_top), left=0, bottom=fig_h/2, right=0.8, top=fig_h, align_to = 'full', clip=FALSE)

ggsave(glue('{outputPath}/new_figures/Figure6.png'), width=10, height=9)



