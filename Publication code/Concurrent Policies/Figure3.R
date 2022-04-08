# Figure3.R ---------------------------------------------------------------
# 
# Run the new Figure 3 for the revision of the concurrent simulation paper.
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

concurrent_instant_ordered <- read.csv(glue("{dataPath}/instant_ordered.csv"), stringsAsFactors = FALSE)
concurrent_instant_unordered <- read.csv(glue("{dataPath}/instant_unordered.csv"), stringsAsFactors = FALSE)

concurrent_runs <- bind_rows(
  concurrent_instant_ordered %>% mutate(simulation = 'instant_ordered'),
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

# code effect sizes
concurrent_runs %<>% 
  mutate(effect_size = case_when(
    true_effect=="0, 0.686904564182673" ~ "0%/-15%",
    true_effect=="0.228968188060891, 0.686904564182673" ~ "-5%/-15%",
    true_effect=="0.457936376121782, 0.457936376121782" ~ "-10%/-10%",
    true_effect=="0.457936376121782, 0.915872752243563" ~ "-10%/-20%",
    true_effect=="0.686904564182673, 0" ~ "-15%/0%",
    true_effect=="0.686904564182673, 0.228968188060891" ~ "-15%/-5%"
  ),
  effect_size = factor(effect_size, 
                       levels = c("0%/-15%","-5%/-15%","-10%/-10%","-10%/-20%","-15%/0%","-15%/-5%"))
  )

# code specification, class, OrderedTx
concurrent_runs %<>%
  mutate(specification = if_else(grepl("treatment2", model_formula),
                                 "Correctly Specified (Secondary Policy Included)", 
                                 "Misspecified (Secondary Policy Omitted)"),
         class = case_when(model_name %in% c("autoreg_linear", "autoreg_linear_misspec") ~ "Linear Autoregressive",
                           model_name %in% c("fixedeff_linear", "fixedeff_linear_misspec") ~ "Linear Two-Way Fixed Effects",
                           TRUE ~ NA_character_),
         OrderedTx = ifelse(simulation=='instant_ordered', 'Ordered', 'Unordered'))

# keep only the non-null effects
concurrent_runs %<>% filter(effect_direction=='neg')

# keep only the desired runs
concurrent_runs %<>%
  filter(n_units==30,
         years_apart==0,
         specification == "Correctly Specified (Secondary Policy Included)",
         rho==0.9,
         class=='Linear Autoregressive')

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

# create the data for the figure
fig_data <- concurrent_runs %>%
  filter(n_units==30,
         specification == "Correctly Specified (Secondary Policy Included)",
         rho==0.9) %>% 
  nest_by(OrderedTx, effect_size) %>%
  mutate(data = list(data %>% 
           select(coefficient, effect_direction, specification,
                  policy_speed, n_units, rho, years_apart, avg_mean_distance, 
                  type_s_error, rel_bias, variance, RMSE, coverage) %>%
           gather(key=metric, value=value, type_s_error:coverage) %>%
           mutate(metric = factor(
             metric,
             levels=c("type_s_error", "rel_bias", "variance", "RMSE", "coverage"),
             labels=c("Type S Error", "Relative Bias","Variance", "RMSE", "Coverage"))) %>%
           filter(metric %in% c("Relative Bias","Variance")))) %>%
  unnest(cols = 'data')

# set relative bias to NA when the effect size is 0
fig_data <- fig_data %>%
  mutate(value = case_when(
    metric=='Relative Bias' & effect_size=="0%/-15%" & coefficient=="Primary Policy" ~ as.double(NA),
    metric=='Relative Bias' & effect_size=="-15%/0%" & coefficient=="Co-occurring Policy" ~ as.double(NA),
    TRUE ~ value
  ))

# reformat the effect size variable
fig_data$effect_size <- factor(gsub('%', '', fig_data$effect_size),
                               levels = c('0/-15', '-5/-15', '-10/-10', '-10/-20', '-15/0', '-15/-5'))

ordered <- fig_data %>%
  filter(OrderedTx=='Ordered',
         metric=='Relative Bias') %>% 
  ggplot(aes(x=effect_size, y=value)) + #x=factor(rho)
  geom_bar(stat="identity", aes(fill=coefficient), 
           position=position_dodge2(width=0.9, preserve="single")) +
  facet_wrap(~ metric, scales="free_y") +
  # scale_x_discrete(drop=FALSE, labels=c("0-1", "3-4", "6-7", "9-10")) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L), 
                     limits = c(-0.3,0.3)) + 
  labs(title = "Ordered") + 
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        plot.title = element_text(hjust=0.5, face = 'bold.italic'),
        text = element_text(size=16)) +
  ylab("") + #Value
  xlab("Treatment Effect Sizes") + 
  scale_fill_grey()

unordered <- fig_data %>%
  filter(OrderedTx=='Unordered',
         metric=='Relative Bias') %>% 
  ggplot(aes(x=effect_size, y=value)) + #x=factor(rho)
  geom_bar(stat="identity", aes(fill=coefficient), 
           position=position_dodge2(width=0.9, preserve="single")) +
  facet_wrap(~ metric, scales="free_y") +
  # scale_x_discrete(drop=FALSE, labels=c("0-1", "3-4", "6-7", "9-10")) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L), 
                     limits = c(-0.3,0.3)) + 
  labs(title = "Unordered") + 
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        plot.title = element_text(hjust=0.5, face = 'bold.italic'),
        text = element_text(size=16)) +
  ylab("") + #Value
  xlab("Treatment Effect Sizes") + 
  scale_fill_grey()

(ordered + unordered) / guide_area() + 
  plot_layout(heights = c(6,1), guides = 'collect') +
  plot_annotation(caption = 'Note: Relative bias is undefined when the effect size is 0.') & 
  theme(plot.caption = element_text(hjust = 0.1, size = 12))

ggsave(glue('{outputPath}/new_figures/Figure3.png'), width=10, height=5)


