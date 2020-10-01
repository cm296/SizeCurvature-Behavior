rm(list=ls())

library(tidyverse)
library(ggthemes)
library(scales)
library(langcog)

d.raw = data.frame()

setwd("/Users/caterinamagri/Dropbox (KonkLab)/Research-SizeCurvature/Project-SizeCurvatureBehavior_RerunbyStella/SizeCurv_Rerun_Reanalysis_ByCat/")
Data_conds <- read.csv2("Analysis_cm/AllData_conds.csv", header = TRUE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
d.raw <- bind_rows(d.raw,Data_conds);

d_rt <- d.raw %>%
  group_by(participant,SizeCond,CurvCond,current_task,condition) %>%
  summarize(meanRT = mean(RT), meanRT_log = mean(RT_log))

### Create subsets of data for plotting and model fitting

d_size_judge <- d_rt %>%
  filter(current_task == "JudgeSize")

d_size_judge_plot <- d_size_judge %>%
  group_by(condition) %>%
  multi_boot_standard(col = "meanRT")

d_curv_judge <- d_rt %>%
  filter(current_task == "JudgeCurviness")

d_curv_judge_plot <- d_curv_judge %>%
  group_by(condition) %>%
  multi_boot_standard(col = "meanRT")



(size_judge_plot_h <- ggplot(d_size_judge_plot,aes(x = condition, y = mean, fill=condition)) +
    geom_bar(stat="identity", alpha=.6) +
    scale_y_continuous(limits=c(400,950),oob = rescale_none) + 
    # geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper, col=condition), width=0) +
    scale_color_manual(values = c("#3841E8",'#5EA8F1', "#E15207",'#EEA019')) +
    scale_fill_manual(values = c("#3056E8",'#5EA8F1', "#E15207",'#EEA019')) +
    geom_point(data = d_size_judge,aes(x = condition, y = meanRT, fill=condition, color = condition), alpha=.4) +
    theme_few() + 
    ggtitle("Task: Big or Small?") + 
    labs(y="Average RT", x="") +
    theme(legend.position = "none", aspect=1.1))



ggplot(d_curv_judge,aes(x = condition, y = meanRT)) + 
  geom_point(aes(colour=factor(condition))) +
  geom_line(data=d_curv_judge, aes(group = factor(participant)), color="grey", alpha=.2)  +
  geom_bar(data = d_size_judge_plot,stat="identity", alpha=.6,aes(x = condition, y = mean, fill=condition)) +
  scale_color_manual(values = c("#00008B",'#B5D3E7', "#CC5500",'#EEA019')) +
  scale_fill_manual(values = c("#00008B",'#B5D3E7', "#CC5500",'#EEA019')) +
  theme_few() + 
  ggtitle("Task: Big or Small?") + 
  labs(y="Average RT", x="") +
  theme(legend.position = "none", aspect=1.1) + 
  scale_y_continuous(limits=c(400,1000),oob = rescale_none) 


ggplot(d_curv_judge,aes(x = condition, y = meanRT)) + 
  geom_point(aes(colour=factor(condition))) +
  geom_line(data=d_curv_judge, aes(group = factor(participant)), color="grey", alpha=.2)  +
  geom_bar(data = d_size_judge_plot,stat="identity", alpha=.6,aes(x = condition, y = mean, fill=condition)) +
  scale_color_manual(values = c("#00008B",'#B5D3E7', "#CC5500",'#EEA019')) +
  scale_fill_manual(values = c("#00008B",'#B5D3E7', "#CC5500",'#EEA019')) +
  theme_few() + 
  ggtitle("Task: Big or Small?") + 
  labs(y="Average RT", x="") +
  theme(legend.position = "none", aspect=1.1) + 
  scale_y_continuous(limits=c(400,1000),oob = rescale_none) 

### Plot RT effects by curvature
(combined_plot_curv <- ggplot(d_curv_judge_plot, aes(condition, mean, col = condition)) +
    geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper)) +
    geom_bar(stat="identity") + 
    ylim(c(400,1000)) +
    theme_few() + 
    ggtitle("Task: Boxy or Curvy?") + 
    scale_color_manual(values = c("#00008B",'#B5D3E7', "#CC5500",'#FFB347'))+
    theme(legend.position = "none", aspect.ratio = 1) +
    labs(y="Average RT", x="")  +
    geom_point(data = d_curv_judge,aes(x = condition, y = meanRT, col=condition), alpha=.2))


d_judge_curv <- d_rt %>%
  filter(current_task=="JudgeCurviness")


