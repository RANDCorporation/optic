library(readxl)
library(ggplot2)
library(gridExtra)

##############
## Figure 1 ##
##############

## Read in Directional Bias data in long format
dirbias <- as.data.frame(read.csv("dirbias_long_v2.csv"))
dirbias$es <- factor(dirbias$es,levels = c("Small (5%)","Moderate (15%)","Large (25%)"),labels = c("Small Effect Size (5%)","Moderate Effect Size (15%)","Large Effect Size (25%)"))

## This defines the subset of the Directional Bias data that I want to plot
subset_dirbias_1 <- subset(dirbias,link == "Linear" & mod == "Two Way Fixed Effects" & wt == "Weighted" & Detrended == "No")
subset_dirbias_2 <- subset(dirbias,link == "Linear" & mod == "Two Way Fixed Effects" & wt == "Weighted" & Detrended == "Yes")
subset_dirbias_3 <- subset(dirbias,link == "Linear" & mod == "Autoregressive" & wt == "Weighted" & Detrended == "No")
subset_dirbias_4 <- subset(dirbias,link == "Linear" & mod == "GEE" & Detrended == "No" & wt == "Weighted")

data_to_plot <- rbind(subset_dirbias_1,subset_dirbias_2,subset_dirbias_3,subset_dirbias_4)
data_to_plot$description <- factor(data_to_plot$description,levels = c("Linear Two Way Fixed Effects Model - Weighted","Linear Two Way Fixed Effects + Detrended Model - Weighted","Linear Autoregressive Model - Weighted","Linear GEE Model - Weighted"),
                                   labels = c("a) Linear Two-Way Fixed Effects Model - Weighted","b) Detrended Linear Two-Way Fixed Effects Model - Weighted","c) Linear Autoregressive Model - Weighted","d) Linear GEE Model - Weighted"))

p1 <- ggplot(data_to_plot,mapping = aes(x = n.states, y = dirbias_pct,linetype = es)) + 
  geom_line() +
  geom_point() +
  ylim(c(-50,350)) + 
  ylab("% Directional Bias") +
  xlab("") +
  guides(linetype=guide_legend(title = "Effect Size")) +
  scale_linetype_manual(values = c("solid","longdash","dotted")) +
  # scale_color_manual(values = c("#4472C4","#ED7D31","#A5A5A5")) + 
  scale_x_continuous(name = "States",breaks = c(1,5,15,30),labels = c("1","5","15","30"),minor_breaks = c(5,10,15,20,25,30)) +
  facet_wrap(~description,nrow = 2) +
  theme(legend.position = "bottom",panel.background = element_blank(),panel.grid.major.y = element_line(size=.1,color = "gray"))

ggsave("Figure1_bw.png",p1)

##############
## Figure 2 ##
##############

## Read in data for tornado plots
tornado <- as.data.frame(read.csv("tornado_plots.csv"))

## Get data for directional bias
dirbias <- subset(tornado,metric == "dirbias")

dirbias$mod <- factor(dirbias$mod,
                      levels = c("Log Y 2-way FE Pop Weighted","Log Y AR Unweighted","Poisson 2-way FE",
                                 "Log Y AR Pop Weighted","Linear GEE Pop Weighted","Linear 2-way FE Pop Weighted",
                                 "Poisson AR","Negative Binomial Detrended","Log Y 2-way FE Unweighted",
                                 "Negative Binomial AR","Negative Binomial 2-way FE","Linear 2-way FE Unweighted",
                                 "Linear AR Pop Weighted","Linear GEE Unweighted","Linear AR Unweighted",
                                 "Linear Detrended Pop Weighted","Linear Detrended Unweighted"),
                      labels = c("Log-Y Two-Way FE; Pop Weighted","Log-Y AR; Unweighted","Poisson; Two-Way FE",
                                 "Log-Y AR; Pop Weighted","Linear GEE; Pop Weighted","Linear Two-Way FE; Pop Weighted",
                                 "Poisson; AR","Negative Binomial; Detrended","Log-Y Two-Way FE; Unweighted",
                                 "Negative Binomial; AR","Negative Binomial; Two-Way FE","Linear Two-Way FE; Unweighted",
                                 "Linear AR; Pop Weighted","Linear GEE; Unweighted","Linear AR; Unweighted",
                                 "Linear Detrended; Pop Weighted","Linear Detrended; Unweighted"))

p2 <- ggplot(dirbias,mapping = aes(y = result,x = mod)) + 
  geom_bar(stat = 'identity',color = "black",fill = "black",width = 0.6) + 
  coord_flip() + 
  xlab("") + 
  ylim(c(-50,200)) + 
  ylab("% Directional Bias") +
  theme(panel.background = element_blank(),axis.ticks = element_blank(),panel.grid.major.x = element_line(size=.1,color = "gray"))

ggsave("Figure2_bw.png",p2)

##############
## Figure 3 ##
##############

## Get data for magnitude bias
magbias <- subset(tornado,metric == "magbias")

magbias$mod <- factor(magbias$mod,
                      levels = c("Linear AR Pop Weighted","Linear AR Unweighted","Linear Detrended Pop Weighted",
                                 "Linear 2-way FE Unweighted","Linear GEE Pop Weighted","Linear 2-way FE Pop Weighted",
                                 "Linear GEE Unweighted","Linear Detrended Unweighted","Log Y 2-way FE Unweighted",
                                 "Negative Binomial 2-way FE","Negative Binomial Detrended","Poisson 2-way FE",
                                 "Log Y 2-way FE Pop Weighted","Negative Binomial AR","Log Y AR Pop Weighted",
                                 "Log Y AR Unweighted","Poisson AR"),
                      labels = c("Linear AR; Pop Weighted","Linear AR; Unweighted","Linear Detrended; Pop Weighted",
                                 "Linear Two-Way FE; Unweighted","Linear GEE; Pop Weighted","Linear Two-Way FE; Pop Weighted",
                                 "Linear GEE; Unweighted","Linear Detrended; Unweighted","Log-Y Two-Way FE; Unweighted",
                                 "Negative Binomial; Two-Way FE","Negative Binomial; Detrended","Poisson; Two-Way FE",
                                 "Log-Y Two-Way FE; Pop Weighted","Negative Binomial; AR","Log-Y AR; Pop Weighted",
                                 "Log-Y AR; Unweighted","Poisson; AR"))

p3 <- ggplot(magbias,mapping = aes(y = result,x = mod)) + 
  geom_bar(stat = 'identity',color = "black",fill = "black",width = 0.6) + 
  coord_flip() + 
  xlab("") + 
  ylim(c(-10,30)) + 
  ylab("% Magnitude Bias") +
  theme(panel.background = element_blank(),axis.ticks = element_blank(),panel.grid.major.x = element_line(size=.1,color = "gray"))

ggsave("Figure3_bw.png",p3)

##############
## Figure 4 ##
##############

## Get data for RMSE
linear_rmse <- subset(tornado, metric == "rmse" & grepl("Linear",tornado$mod))
nlinear_rmse <- subset(tornado, metric == "rmse" & !grepl("Linear",tornado$mod))

linear_rmse$mod <- factor(linear_rmse$mod,
                          levels = c("Linear GEE; Weighhted","Linear Two-Way FE; Population Wted","Linear Detrended; Population Wted",
                                     "Linear Two-Way FE; Unwted","Linear Detrended; Unwted","Linear GEE; Unweighted","Linear AR; Unwted",
                                     "Linear AR; Population Wted"),
                          labels = c("Linear GEE; Pop Weighted","Linear Two-Way FE; Pop Weighted","Linear Detrended; Pop Weighted",
                                     "Linear Two-Way FE; Unweighted","Linear Detrended; Unweighted","Linear GEE; Unweighted","Linear AR; Unweighted",
                                     "Linear AR; Pop Weighted"))
nlinear_rmse$mod <- factor(nlinear_rmse$mod,
                           levels = c("Poisson; AR","Log-Y AR; Population Wted","Log-Y AR; Unwted",
                                      "Negative Binomial; AR","Log-Y Two-Way FE; Population Wted","Poisson; Two-Way FE",
                                      "Negative Binomial; Detrended","Log-Y Two-Way FE; Unwted","Negative Binomial; Two-Way FE"),
                           labels = c("Poisson; AR","Log-Y AR; Pop Weighted","Log-Y AR; Unweighted",
                                      "Negative Binomial; AR","Log-Y Two-Way FE; Pop Weighted","Poisson; Two-Way FE",
                                      "Negative Binomial; Detrended","Log-Y Two-Way FE; Unweighted","Negative Binomial; Two-Way FE"))
linear_rmse$description <- "a) Linear Models"
nlinear_rmse$description <- "b) Nonlinear Models"


linear_plot <- ggplot(linear_rmse,mapping = aes(y = result,x = mod)) + 
  geom_bar(stat = 'identity',color = "black",fill = "black",width = 0.6) + 
  coord_flip() + 
  xlab("") + 
  ylim(c(0,2.5)) + 
  ylab("RMSE") +
  facet_wrap(~description)
theme(panel.background = element_blank(),axis.ticks = element_blank(),panel.grid.major.x = element_line(size=.1,color = "gray"))
nlinear_plot <- ggplot(nlinear_rmse,mapping = aes(y = result,x = mod)) + 
  geom_bar(stat = 'identity',color = "black",fill = "black",width = 0.6) + 
  coord_flip() + 
  xlab("") + 
  ylim(c(0,2.5)) + 
  ylab("RMSE") +
  facet_wrap(~description)
theme(panel.background = element_blank(),axis.ticks = element_blank(),panel.grid.major.x = element_line(size=.1,color = "gray"))

p4 <- grid.arrange(linear_plot,nlinear_plot,respect = TRUE,nrow = 1)

ggsave("Figure4_bw.png",p4)

##############
## Figure 5 ##
##############

## Read in Type I Error data in long format
TypeIerror <- as.data.frame(read_xlsx("TypeIerror_long.xlsx"))
TypeIerror$typeIerror <- TypeIerror$typeIerror*100
TypeIerror$states <- factor(TypeIerror$states,levels = c("1","5","15","30"),labels = c("1","5","15","30"))
TypeIerror$model <- as.factor(TypeIerror$model)

## Get rid of Huber-Cluster
TypeIerror <- subset(TypeIerror,!se %in% c("Huber-Cluster","Huber-\nCluster"))

## This defines the subset of the Type I Error data that I want to plot
subset_t1e_1 <- subset(TypeIerror,link == "Linear" & mod == "Two Way Fixed Effects" & wt == "Weighted" & Detrended == "No")
subset_t1e_2 <- subset(TypeIerror,link == "Linear" & mod == "Two Way Fixed Effects" & wt == "Weighted" & Detrended == "Yes")
subset_t1e_3 <- subset(TypeIerror,link == "Linear" & mod == "Autoregressive" & wt == "Weighted" & Detrended == "No")
subset_t1e_4 <- subset(TypeIerror,link == "Linear" & mod == "GEE" & Detrended == "No")

data_to_plot <- rbind(subset_t1e_1,subset_t1e_2,subset_t1e_3,subset_t1e_4)
data_to_plot$x <- ifelse(data_to_plot$mod %in% c("Two Way Fixed Effects","Autoregressive"),data_to_plot$se,data_to_plot$wt)
data_to_plot$x <- factor(data_to_plot$x,levels = c("None","Huber","Cluster","Unweighted","Weighted"),labels = c("No Adjustment","Huber","Cluster","Unweighted",""))
data_to_plot$description <- factor(data_to_plot$description,levels = c("Linear Two Way Fixed Effects Model - Weighted","Linear Two Way Fixed Effects + Detrended Model - Weighted","Linear Autoregressive Model - Weighted","Linear GEE Model"),
                                   labels = c("a) Linear Two Way Fixed Effects Model - Weighted","b) Detrended Linear Two-Way Fixed Effects Model - Weighted","c) Linear Autoregressive Model - Weighted","d) Linear GEE Model - Weighted"))
data_to_plot <- data_to_plot[-which(data_to_plot$mod == "GEE" & data_to_plot$wt == "Unweighted"),]

p5 <- ggplot(data_to_plot,mapping = aes(x = x, y = typeIerror,fill = states)) + 
  geom_col(position = "dodge",color = "black") +
  geom_hline(yintercept = 5) +
  ylim(c(0,100)) + 
  ylab("") +
  xlab("") +
  guides(fill=guide_legend(title = "Number of States Implementing the Policy")) +
  # scale_fill_manual(values = c("#4472C4","#ED7D31","#A5A5A5","#FFC000")) + 
  scale_fill_grey(start = 0.8,end = 0.2) +
  facet_wrap(~description,scales = "free_x") +
  theme(legend.position = "bottom",axis.ticks = element_blank(),panel.background = element_blank(),panel.grid.major.y = element_line(size=.1,color = "gray"))

ggsave("Figure5_bw.png",p5)

##############
## Figure 6 ##
##############

## This defines the subset of the Type I Error data that I want to plot
subset_t1e_1 <- subset(TypeIerror,link == "Linear" & mod == "Autoregressive" & wt == "Unweighted" & Detrended == "No")
subset_t1e_2 <- subset(TypeIerror,link == "Log Y" & mod == "Autoregressive" & wt == "Unweighted" & Detrended == "No")
subset_t1e_3 <- subset(TypeIerror,link == "Poisson" & mod == "Autoregressive")
subset_t1e_4 <- subset(TypeIerror,link == "Negative Binomial" & mod == "Autoregressive" & Detrended == "No")

data_to_plot <- rbind(subset_t1e_1,subset_t1e_2,subset_t1e_3,subset_t1e_4)
data_to_plot$se <- factor(data_to_plot$se,levels = c("None","Huber","Cluster"),labels = c("No Adjustment","Huber","Cluster"))
data_to_plot$description <- factor(data_to_plot$description,levels = c("Linear Autoregressive Model - Unweighted","Log Y Autoregressive Model - Unweighted","Poisson Autoregressive Model","Negative Binomial Autoregressive Model"),
                                   labels = c("a) Linear Autoregressive Model - Unweighted","b) Log Linear Autoregressive Model - Unweighted","c) Poisson Autoregressive Model","d) Negative Binomial Autoregressive Model"))

p6 <- ggplot(data_to_plot,mapping = aes(x = se, y = typeIerror,fill = states)) + 
  geom_col(position = "dodge",color = "black") +
  geom_hline(yintercept = 5) +
  ylim(c(0,100)) + 
  ylab("") +
  xlab("") +
  guides(fill=guide_legend(title = "Number of States Implementing the Policy")) +
  # scale_fill_manual(values = c("#4472C4","#ED7D31","#A5A5A5","#FFC000")) + 
  scale_fill_grey(start = 0.8,end = 0.2) +
  facet_wrap(~description,scales = "free_x") +
  theme(legend.position = "bottom",axis.ticks = element_blank(),panel.background = element_blank(),panel.grid.major.y = element_line(size=.1,color = "gray"))

ggsave("Figure6_bw.png",p6)

##############
## Figure 7 ##
##############

## Read in Power data in long format
PowerCurves <- as.data.frame(read_xlsx("PowerCurves_long.xlsx"))
PowerCurves$power <- PowerCurves$power*100
PowerCurves$es <- factor(PowerCurves$es,levels = c("Small (5%)","Moderate (15%)","Large (25%)"),labels = c("Small Effect Size (5%)","Moderate Effect Size (15%)","Large Effect Size (25%)"))

## This defines the subset of the Power data that I want to plot
subset_power_1 <- subset(PowerCurves,link == "Linear" & mod == "Two Way Fixed Effects" & wt == "Weighted" & Detrended == "No")
subset_power_2 <- subset(PowerCurves,link == "Linear" & mod == "Two Way Fixed Effects" & wt == "Weighted" & Detrended == "Yes")
subset_power_3 <- subset(PowerCurves,link == "Linear" & mod == "Autoregressive" & wt == "Weighted" & Detrended == "No")
subset_power_4 <- subset(PowerCurves,link == "Linear" & mod == "GEE" & Detrended == "No" & wt == "Weighted")

data_to_plot <- rbind(subset_power_1,subset_power_2,subset_power_3,subset_power_4)
data_to_plot$description <- factor(data_to_plot$description,levels = c("Linear Two Way Fixed Effects Model - Weighted; Cluster Adjusted","Linear Two Way Fixed Effects + Detrended Model - Weighted","Linear Autoregressive Model - Weighted","Linear GEE Model - Weighted"),
                                   labels = c("a) Linear Two-Way Fixed Effects Model - Weighted","b) Detrended Linear Two-Way Fixed Effects Model - Weighted","c) Linear Autoregressive Model - Weighted","d) Linear GEE Model - Weighted"))

p7 <- ggplot(data_to_plot,mapping = aes(x = states, y = power,linetype = es)) + 
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 80, color = "gray") +
  ylim(c(0,100)) + 
  ylab("Power") +
  xlab("") +
  guides(linetype=guide_legend(title = "Effect Size")) +
  scale_linetype_manual(values = c("solid","longdash","dotted")) +
  # scale_color_manual(values = c("#4472C4","#ED7D31","#A5A5A5")) + 
  scale_x_continuous(name = "States",breaks = c(1,5,15,30),labels = c("1","5","15","30"),minor_breaks = c(5,10,15,20,25,30)) +
  facet_wrap(~description,nrow = 2) +
  theme(legend.position = "bottom",panel.background = element_blank(),panel.grid.major.y = element_line(size=.1,color = "gray"))

ggsave("Figure7_bw.png",p7)

##############
## Figure 8 ##
##############

## Get data for power
power <- subset(tornado,metric == "power")

power$mod <- factor(power$mod,
                    levels = c("Linear 2-way FE Wted","Linear GEE Wted","Log Y 2-way FE Wted","Linear 2-way FE Unwt",
                               "Linear Detrended Wted","Poisson 2-way FE","Linear Detrended Unwt","Log Y AR Unwt",
                               "Log Y AR Wted","Poisson AR","Linear GEE Unwt","Log Y 2-way FE Unwt",
                               "Negative Binomial 2-way FE","Negative Binomial AR","Linear AR Unwt",
                               "Negative Binomial Detrended","Linear AR Wted"),
                    labels = c("Linear Two-Way FE; Pop Weighted","Linear GEE; Pop Weighted","Log-Y Two-Way FE; Pop Weighted",
                               "Linear Two-Way FE; Unweighted","Linear Detrended; Pop Weighted","Poisson; Two-Way FE",
                               "Linear Detrended; Unweighted","Log-Y AR; Unweighted","Log-Y AR; Pop Weighted","Poisson; AR",
                               "Linear GEE; Unweighted","Log-Y Two-Way FE; Unweighted","Negative Binomial; Two-Way FE","Negative Binomial; AR",
                               "Linear AR; Unweighted","Negative Binomial; Detrended","Linear AR; Pop Weighted"))

p8 <- ggplot(power,mapping = aes(y = result,x = mod)) + 
  geom_bar(stat = 'identity',color = "black",fill = "black",width = 0.6) + 
  coord_flip() + 
  xlab("") + 
  ylim(c(0,30)) + 
  ylab("Power") +
  theme(panel.background = element_blank(),axis.ticks = element_blank(),panel.grid.major.x = element_line(size=.1,color = "gray"))

ggsave("Figure8_bw.png",p8)

