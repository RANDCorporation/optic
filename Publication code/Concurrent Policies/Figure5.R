# Figure5.R ---------------------------------------------------------------
# 
# Run the new Figure 5 for the revision of the concurrent simulation paper.
# 
# Adam Scherling, 3/31/2022
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

concurrent_99_to_04 <- read.csv(glue("{dataPath}/lm-99to04-notordered_n30.csv"), stringsAsFactors = FALSE)
concurrent_05_to_10 <- read.csv(glue("{dataPath}/lm-05to10-notordered_n30.csv"), stringsAsFactors = FALSE)
concurrent_11_to_16 <- read.csv(glue("{dataPath}/lm-11to16-notordered_n30.csv"), stringsAsFactors = FALSE)

concurrent_runs <- bind_rows(
  concurrent_99_to_04 %>% mutate(simulation = 'concurrent_99_to_04'),
  concurrent_05_to_10 %>% mutate(simulation = 'concurrent_05_to_10'),
  concurrent_11_to_16 %>% mutate(simulation = 'concurrent_11_to_16')
)



# prepare the data for plotting -------------------------------------------

# keep only the linear AR models
concurrent_runs %<>% filter(
  model_name %in% c("autoreg_linear", "autoreg_linear_misspec")
)

# keep only the desired SEs
concurrent_runs %<>% filter(se_adjustment == "none")

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


# create the dataset to be used in the figure
fig_data <- concurrent_runs %>%
  nest_by(simulation, specification) %>%
  mutate(data = list(data %>% 
           filter(n_units==30,
                  rho==0.9) %>% 
           select(coefficient, effect_direction, effect_size, 
                  policy_speed, n_units, rho, years_apart, avg_mean_distance, 
                  type_s_error, rel_bias, variance, RMSE, coverage) %>%
           gather(key=metric, value=value, type_s_error:coverage) %>%
           mutate(metric = factor(
             metric,
             levels=c("type_s_error", "rel_bias", "variance", "RMSE", "coverage"),
             labels=c("Type S Error", "Relative Bias","Variance", "RMSE", "Coverage"))) %>%
           filter(metric=="Relative Bias", !is.na(value)))) %>%
  unnest(cols = 'data')


correct_lab <- ggplot(data=data.frame(lab='Correctly-\nspecified AR\nmodel:', x=1, y=1),
                      aes(x=x, y=y, label=lab, fontface='bold.italic')) + 
  geom_text(size=7) +
  theme(legend.position = 'none') + 
  theme_void()

correct_early <- fig_data %>%
  filter(specification=='Correctly Specified (Secondary Policy Included)',
         simulation=='concurrent_99_to_04') %>% 
  ggplot(aes(x=factor(years_apart), y=value)) + #x=factor(rho)
  geom_bar(stat="identity", aes(fill=coefficient), 
           position=position_dodge2(width=0.9, preserve="single")) +
  facet_wrap(~ metric, scales="free_y") +
  scale_x_discrete(drop=FALSE, labels=c("0-1", "3-4", "6-7", "9-10")) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L), 
                     limits = c(-.25,.25)) + 
  labs(title = "Early: first 6 yrs.", x="Years Apart", y="") + 
  theme_bw() +
  theme(plot.title = element_text(face='bold.italic', hjust=0.5),
        legend.title = element_blank(),
        # legend.position = 'bottom',
        text = element_text(size=16)) +
  scale_fill_grey()

correct_middle <- fig_data %>%
  filter(specification=='Correctly Specified (Secondary Policy Included)',
         simulation=='concurrent_05_to_10') %>% 
  ggplot(aes(x=factor(years_apart), y=value)) + #x=factor(rho)
  geom_bar(stat="identity", aes(fill=coefficient), 
           position=position_dodge2(width=0.9, preserve="single")) +
  facet_wrap(~ metric, scales="free_y") +
  scale_x_discrete(drop=FALSE, labels=c("0-1", "3-4", "6-7", "9-10")) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L), 
                     limits = c(-.25,.25)) + 
  labs(title = "Middle: middle 6 yrs.", x="Years Apart", y="") + 
  theme_bw() +
  theme(plot.title = element_text(face='bold.italic', hjust=0.5),
        legend.title = element_blank(),
        # legend.position = 'bottom',
        text = element_text(size=16)) +
  scale_fill_grey()

correct_late <- fig_data %>%
  filter(specification=='Correctly Specified (Secondary Policy Included)',
         simulation=='concurrent_11_to_16') %>% 
  ggplot(aes(x=factor(years_apart), y=value)) + #x=factor(rho)
  geom_bar(stat="identity", aes(fill=coefficient), 
           position=position_dodge2(width=0.9, preserve="single")) +
  facet_wrap(~ metric, scales="free_y") +
  scale_x_discrete(drop=FALSE, labels=c("0-1", "3-4", "6-7", "9-10")) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L), 
                     limits = c(-.25,.25)) + 
  labs(title = "Late: last 6 yrs.", x="Years Apart", y="") + 
  theme_bw() +
  theme(plot.title = element_text(face='bold.italic', hjust=0.5),
        legend.title = element_blank(),
        # legend.position = 'bottom',
        text = element_text(size=16)) +
  scale_fill_grey()

misspec_lab <- ggplot(data=data.frame(lab='Misspecified\nAR model:', x=1, y=1),
                      aes(x=x, y=y, label=lab, fontface='bold.italic')) + 
  geom_text(size=7) +
  theme(legend.position = 'none') + 
  theme_void()

misspec_early <- fig_data %>%
  filter(specification=='Misspecified (Secondary Policy Omitted)',
         simulation=='concurrent_99_to_04') %>% 
  ggplot(aes(x=factor(years_apart), y=value)) + #x=factor(rho)
  geom_bar(stat="identity", aes(fill=coefficient), 
           position=position_dodge2(width=0.9, preserve="single")) +
  facet_wrap(~ metric, scales="free_y") +
  scale_x_discrete(drop=FALSE, labels=c("0-1", "3-4", "6-7", "9-10")) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L), 
                     limits = c(-.2,1)) + 
  labs(x="Years Apart", y="") + 
  theme_bw() +
  theme(legend.position = 'none',
        text = element_text(size=16)) +
  scale_fill_grey()

misspec_middle <- fig_data %>%
  filter(specification=='Misspecified (Secondary Policy Omitted)',
         simulation=='concurrent_05_to_10') %>% 
  ggplot(aes(x=factor(years_apart), y=value)) + #x=factor(rho)
  geom_bar(stat="identity", aes(fill=coefficient), 
           position=position_dodge2(width=0.9, preserve="single")) +
  facet_wrap(~ metric, scales="free_y") +
  scale_x_discrete(drop=FALSE, labels=c("0-1", "3-4", "6-7", "9-10")) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L), 
                     limits = c(-.2,1)) + 
  labs(x="Years Apart", y="") + 
  theme_bw() +
  theme(legend.position = 'none',
        text = element_text(size=16)) +
  scale_fill_grey()

misspec_late <- fig_data %>%
  filter(specification=='Misspecified (Secondary Policy Omitted)',
         simulation=='concurrent_11_to_16') %>% 
  ggplot(aes(x=factor(years_apart), y=value)) + #x=factor(rho)
  geom_bar(stat="identity", aes(fill=coefficient), 
           position=position_dodge2(width=0.9, preserve="single")) +
  facet_wrap(~ metric, scales="free_y") +
  scale_x_discrete(drop=FALSE, labels=c("0-1", "3-4", "6-7", "9-10")) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L), 
                     limits = c(-.2,1)) + 
  labs(x="Years Apart", y="") + 
  theme_bw() +
  theme(legend.position = 'none',
        text = element_text(size=16)) +
  scale_fill_grey()


(correct_lab + correct_early + correct_middle + correct_late + plot_layout(nrow=1, guides = 'collect')) /
  (misspec_lab + misspec_early + misspec_middle + misspec_late + plot_layout(nrow=1))

# for legend on side 
(correct_lab + correct_early + correct_middle + correct_late + plot_layout(nrow=1)) /
  (misspec_lab + misspec_early + misspec_middle + misspec_late + plot_layout(nrow=1)) + 
  plot_layout(guides = 'collect')

# for legend on bottom 
(correct_lab + correct_early + correct_middle + correct_late + plot_layout(nrow=1)) /
  (misspec_lab + misspec_early + misspec_middle + misspec_late + plot_layout(nrow=1)) / 
  guide_area() + 
  plot_layout(guides = 'collect')


ggsave(glue('{outputPath}/new_figures/Figure5.png'), width=16, height=7)

