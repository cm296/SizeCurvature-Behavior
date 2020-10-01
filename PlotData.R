rm(list=ls())

library(tidyverse)
library(ggthemes)
library(scales)
library(langcog)
library(Rmisc)

d.raw = data.frame()

setwd("/Users/caterinamagri/Dropbox (KonkLab)/Research-SizeCurvature/SizeCurvinessfMRI/Experiment-Behavior-RerunbyStella/SizeCurv_Rerun_Reanalysis_ByCat/")
Data_conds <- read.csv2("Analysis_cm/AllData_conds.csv", header = TRUE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
d.raw <- Data_conds;

d_rt <- d.raw %>%
  group_by(participant,SizeCond,CurvCond,current_task,condition) %>%
  mutate(SizexCurvature = interaction(SizeCond,CurvCond)) %>%
  group_by(participant,SizeCond,CurvCond,current_task,condition,SizexCurvature) #%>%
  #  summarize(meanRT = mean(RT), meanRT_log = mean(RT_log))

### Create subsets of data for plotting and model fitting

d_size_judge <- d_rt %>%
  filter(current_task == "JudgeSize")

d_size_judge_plot <- d_size_judge %>%
  group_by(condition) %>%
  multi_boot_standard(col = "RT")

d_curv_judge <- d_rt %>%
  filter(current_task == "JudgeCurviness")

d_curv_judge_plot <- d_curv_judge %>%
  group_by(condition) %>%
  multi_boot_standard(col = "RT")



(size_judge_plot_h <- ggplot(d_size_judge_plot,aes(x = condition, y = mean, fill=condition)) +
    geom_bar(stat="identity", alpha=.6) +
    scale_y_continuous(limits=c(400,800),oob = rescale_none) + 
    geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper, col=condition), width=0) +
    scale_color_manual(values = c("#3841E8",'#5EA8F1', "#E15207",'#EEA019')) +
    scale_fill_manual(values = c("#3056E8",'#5EA8F1', "#E15207",'#EEA019')) +
    geom_point(data = d_size_judge,aes(x = condition, y = RT, fill=condition, color = condition), alpha=.2) +
    theme_few() + 
    ggtitle("Task: Big or Small?") + 
    labs(y="Average RT", x="") +
    theme(legend.position = "none", aspect=1.1))

ggsave('Size_RTData_individualscatter.png',width = 4, height = 4,unit =  "in", plot = size_judge_plot_h, path="./Figures-R/", device = "png",dpi = 300)


(curv_judge_plot_h <- ggplot(d_curv_judge_plot,aes(x = condition, y = mean, fill=condition)) +
    geom_bar(stat="identity", alpha=.6) +
    scale_y_continuous(limits=c(400,800),oob = rescale_none) + 
    geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper, col=condition), width=0) +
    scale_color_manual(values = c("#3841E8",'#5EA8F1', "#E15207",'#EEA019')) +
    scale_fill_manual(values = c("#3056E8",'#5EA8F1', "#E15207",'#EEA019')) +
    geom_point(data = d_curv_judge,aes(x = condition, y = RT, fill=condition, color = condition), alpha=.2) +
    theme_few() + 
    ggtitle("Task: Curvy or Boxy?") + 
    labs(y="Average RT", x="") +
    theme(legend.position = "none", aspect=1.1))

ggsave('Curvature_RTData_individualscatter.png',width = 4, height = 4,unit =  "in", plot = curv_judge_plot_h, path="./Figures-R/", device = "png",dpi = 300)



ggplot(d_curv_judge,aes(x = condition, y = RT)) + 
  geom_point(aes(colour=factor(condition))) +
  geom_line(data=d_curv_judge, aes(group = factor(participant)), color="grey", alpha=.2)  +
  geom_bar(data = d_curv_judge_plot,stat="identity", alpha=.6,aes(x = condition, y = mean, fill=condition)) +
  scale_color_manual(values = c("#00008B",'#B5D3E7', "#CC5500",'#EEA019')) +
  scale_fill_manual(values = c("#00008B",'#B5D3E7', "#CC5500",'#EEA019')) +
  theme_few() + 
  ggtitle("Task: Curvy or Boxy?") + 
  labs(y="Average RT", x="") +
  theme(legend.position = "none", aspect=1.1) + 
  scale_y_continuous(limits=c(400,900),oob = rescale_none) 

ggplot(d_size_judge,aes(x = condition, y = RT)) + 
  geom_point(aes(colour=factor(condition))) +
  geom_line(data=d_size_judge, aes(group = factor(participant)), color="grey", alpha=.2)  +
  geom_bar(data = d_size_judge_plot,stat="identity", alpha=.6,aes(x = condition, y = mean, fill=condition)) +
  scale_color_manual(values = c("#00008B",'#B5D3E7', "#CC5500",'#EEA019')) +
  scale_fill_manual(values = c("#00008B",'#B5D3E7', "#CC5500",'#EEA019')) +
  theme_few() + 
  ggtitle("Task: Big or Small?") + 
  labs(y="Average RT", x="") +
  theme(legend.position = "none", aspect=1.1) + 
  scale_y_continuous(limits=c(400,900),oob = rescale_none) 


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
    geom_point(data = d_curv_judge,aes(x = condition, y = RT, col=condition), alpha=.2))


Data_item <- read.csv2("Analysis_cm/databyItem_table.csv", header = TRUE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
d_rt_item <- Data_item;

d_rt_item = d_rt_item[!is.na(d_rt_item$correctRTs),];


##compute se at the item level -curvature
d_judge_curv_item <- d_rt_item %>%
 filter(current_task=="JudgeCurviness")

curv_judge_se=summarySEwithin(data = d_judge_curv_item, 'correctRTs', withinvars = c('condition'), idvar = c('participant'))
curv_judge_se <- curv_judge_se %>%
  mutate(upper = correctRTs + se, lower = correctRTs - se)


(curv_judge_plot_h <- ggplot(curv_judge_se,aes(x = condition, y = correctRTs, fill=condition)) +
    geom_bar(stat="identity", alpha=.6) +
    scale_y_continuous(limits=c(500,650),oob = rescale_none) +
    geom_errorbar(aes(ymin=lower, ymax=upper, col=condition), width=0) +
    scale_color_manual(values = c("#3841E8",'#5EA8F1', "#E15207",'#EEA019')) +
    scale_fill_manual(values = c("#3056E8",'#5EA8F1', "#E15207",'#EEA019')) +
    # geom_point(data = d_curv_judge,aes(x = condition, y = meanRT, fill=condition, color = condition), alpha=.4) +
    theme_few() + 
    ggtitle("Task: Boxy or Curvy?") + 
    labs(y="Average RT", x="") +
    theme(legend.position = "none", aspect=1.1))

ggsave('Curvature_RTData.png',width = 4, height = 4,unit =  "in", plot = curv_judge_plot_h, path="./Figures-R/", device = "png",dpi = 300)

#compute se at the item level -size
d_judge_size_item <- d_rt_item %>%
  filter(current_task=="JudgeSize")

size_judge_se=summarySEwithin(data = d_judge_size_item, 'correctRTs', withinvars = c('condition'), idvar = c('participant'))
size_judge_se <- size_judge_se %>%
  mutate(upper = correctRTs + se, lower = correctRTs - se)


(size_judge_plot_h <- ggplot(size_judge_se,aes(x = condition, y = correctRTs, fill=condition)) +
    geom_bar(stat="identity", alpha=.6) +
    scale_y_continuous(limits=c(500,650),oob = rescale_none) +
    geom_errorbar(aes(ymin=lower, ymax=upper, col=condition), width=0) +
    scale_color_manual(values = c("#3841E8",'#5EA8F1', "#E15207",'#EEA019')) +
    scale_fill_manual(values = c("#3056E8",'#5EA8F1', "#E15207",'#EEA019')) +
    # geom_point(data = d_curv_judge,aes(x = condition, y = meanRT, fill=condition, color = condition), alpha=.4) +
    theme_few() + 
    ggtitle("Task: Big or Small?") + 
    labs(y="Average RT", x="") +
    theme(legend.position = "none", aspect=1.1))

ggsave('Size_RTData.png',width = 4, height = 4,unit =  "in", plot = size_judge_plot_h, path="./Figures-R/", device = "png",dpi = 300)

### Check that interaction effects replicate in standard within-subjects ANOVA
library(ez)
## for curvature judgements
curv_judge_aov = ezANOVA(dv= .(RT), wid= .(participant), within= .(SizeCond, CurvCond), detailed=TRUE, data=d_curv_judge, type=3)
print(curv_judge_aov)
curv_judge_accuracy_aov = ezANOVA(dv= .(accuracy), wid= .(participant), within= .(SizeCond, CurvCond), detailed=TRUE, data=d_curv_judge, type=3)
print(curv_judge_accuracy_aov)

## for size judgements
size_judge_aov = ezANOVA(dv= .(RT), wid= .(participant), within= .(SizeCond, CurvCond), detailed=TRUE, data=d_size_judge, type=3)
print(size_judge_aov)

size_judge_accuracy_aov = ezANOVA(dv= .(accuracy), wid= .(participant), within= .(SizeCond, CurvCond), detailed=TRUE, data=d_size_judge, type=3)
print(size_judge_accuracy_aov)

judge_aov = ezANOVA(dv= .(correctRTs), wid= .(participant), within= .(SizeCond, CurvCond,current_task ), detailed=TRUE, data=d_rt_item, type=3)
print(judge_aov)

##regroup
d_size_judge <- d_rt %>%
  filter(current_task == "JudgeSize")

d_size_judge_plot <- d_size_judge %>%
  group_by(SizeCond,CurvCond,condition) %>%
  multi_boot_standard(col = "RT")

d_curv_judge <- d_rt %>%
  filter(current_task == "JudgeCurviness")

d_curv_judge_plot <- d_curv_judge %>%
  group_by(SizeCond,CurvCond,condition) %>%
  multi_boot_standard(col = "RT")

#compute se at the item level -size
d_judge_size_item <- d_rt_item %>%
  filter(current_task=="JudgeSize")



size_judge_se=summarySEwithin(data = d_judge_size_item, 'correctRTs', withinvars = c('SizeCond','CurvCond','condition'), idvar = c('participant'))
size_judge_se <- size_judge_se %>%
  mutate(upper = correctRTs + se, lower = correctRTs - se)



(size_judge_plot_h <- ggplot(size_judge_se,aes(x = SizeCond, y = correctRTs, fill=condition)) +
    geom_line(aes(group = CurvCond),colour="gray", size = 1)  +
    #geom_point(aes(group = condition,colour = condition),size = 3) +
    geom_errorbar(aes(ymin=lower, ymax=upper, col=condition),size = 1, width=.05) +
    scale_y_continuous(limits=c(550,650),oob = rescale_none) + 
    scale_color_manual(values = c("#3841E8",'#5EA8F1', "#E15207",'#EEA019')) +
    scale_fill_manual(values = c("#3056E8",'#5EA8F1', "#E15207",'#EEA019')) +
    geom_point(data = size_judge_se,aes(x = SizeCond, y = correctRTs, fill=condition, color = condition), alpha=1,size = 3) +
    theme_few() + 
    ggtitle("Task: Big or Small?") + 
    labs(y="Average RT", x="") +
    theme(legend.position = "none", aspect=1))

ggsave('Size_RTData_interactionplot.png',width = 4, height = 4,unit =  "in", plot = size_judge_plot_h, path="./Figures-R/", device = "png",dpi = 300)


curv_judge_se=summarySEwithin(data = d_judge_curv_item, 'correctRTs', withinvars = c('SizeCond','CurvCond','condition'), idvar = c('participant'))
curv_judge_se <- curv_judge_se %>%
  mutate(upper = correctRTs + se, lower = correctRTs - se)


(curv_judge_plot_h <- ggplot(curv_judge_se,aes(x = CurvCond, y = correctRTs, fill=condition)) +
    geom_line(aes(group = SizeCond),colour="gray", size = 1)  +
    #geom_point(aes(group = condition,colour = condition),size = 3) +
    geom_errorbar(aes(ymin=lower, ymax=upper, col=condition),size = 1, width=.05) +
    scale_y_continuous(limits=c(550,650),oob = rescale_none,position = "left") + 
    scale_color_manual(values = c("#3841E8",'#5EA8F1', "#E15207",'#EEA019')) +
    scale_fill_manual(values = c("#3056E8",'#5EA8F1', "#E15207",'#EEA019'))  +
    geom_point(data = curv_judge_se,aes(x = CurvCond, y = correctRTs, fill=condition, color = condition), alpha=1,size = 3) +
    theme_few() + 
    ggtitle("Task: Boxy or Curvy?") + 
    labs(y="Average RT", x="") +
    theme(legend.position = "none", aspect=1))

ggsave('Curvature_RTData_interactionplot.png',width = 4, height = 4,unit =  "in", plot = curv_judge_plot_h, path="./Figures-R/", device = "png",dpi = 300)



size_judge_se=summarySEwithin(data = d_judge_size_item, 'accuracy', withinvars = c('SizeCond','CurvCond','condition'), idvar = c('participant'))
size_judge_se <- size_judge_se %>%
  mutate(upper = accuracy + se, lower = accuracy - se)

(size_judge_plot_h <- ggplot(size_judge_se,aes(x = SizeCond, y = accuracy, fill=condition)) +
    geom_line(aes(group = CurvCond),colour="gray", size = 1)  +
    #geom_point(aes(group = condition,colour = condition),size = 3) +
    geom_errorbar(aes(ymin=lower, ymax=upper, col=condition),size = 1, width=.05) +
    scale_y_continuous(limits=c(0.9,1),oob = rescale_none) + 
    scale_color_manual(values = c("#3841E8",'#5EA8F1', "#E15207",'#EEA019')) +
    scale_fill_manual(values = c("#3056E8",'#5EA8F1', "#E15207",'#EEA019')) +
    geom_point(data = size_judge_se,aes(x = SizeCond, y = accuracy, fill=condition, color = condition), alpha=1,size = 3) +
    theme_few() + 
    ggtitle("Task: Big or Small?") + 
    labs(y="Accuracy", x="") +
    theme(legend.position = "none", aspect=1))

ggsave('Size_accuracyData_interactionplot.png',width = 4, height = 4,unit =  "in", plot = size_judge_plot_h, path="./Figures-R/", device = "png",dpi = 300)

curv_judge_se=summarySEwithin(data = d_judge_curv_item, 'accuracy', withinvars = c('SizeCond','CurvCond','condition'), idvar = c('participant'))
curv_judge_se <- curv_judge_se %>%
  mutate(upper = accuracy + se, lower = accuracy - se)

(curv_judge_plot_h <- ggplot(curv_judge_se,aes(x = CurvCond, y = accuracy, fill=condition)) +
    geom_line(aes(group = SizeCond),colour="gray", size = 1)  +
    #geom_point(aes(group = condition,colour = condition),size = 3) +
    geom_errorbar(aes(ymin=lower, ymax=upper, col=condition),size = 1, width=.05) +
    scale_y_continuous(limits=c(0.90,1),oob = rescale_none,position = "left") + 
    scale_color_manual(values = c("#3841E8",'#5EA8F1', "#E15207",'#EEA019')) +
    scale_fill_manual(values = c("#3056E8",'#5EA8F1', "#E15207",'#EEA019'))  +
    geom_point(data = curv_judge_se,aes(x = CurvCond, y = accuracy, fill=condition, color = condition), alpha=1,size = 3) +
    theme_few() + 
    ggtitle("Task: Boxy or Curvy?") + 
    labs(y="Accuracy", x="") +
    theme(legend.position = "none", aspect=1))

ggsave('Curvature_accuracyData_interactionplot.png',width = 4, height = 4,unit =  "in", plot = curv_judge_plot_h, path="./Figures-R/", device = "png",dpi = 300)

#################Only first block
Data_block <- read.csv2("Analysis_cm/AllData_conds_block.csv", header = TRUE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
d_rt_block <- Data_block;

d_rt_block = d_rt_block[!is.na(d_rt_block$RT),];

#compute se at the item level -size
d_judge_size_block <- d_rt_block %>%
  filter(current_task=="JudgeSize") %>%
  filter(block==1)

size_block_judge_se=summarySEwithin(data = d_judge_size_block, 'accuracy', withinvars = c('SizeCond','CurvCond','condition'), idvar = c('participant'))
size_block_judge_se <- size_block_judge_se %>%
  mutate(upper = accuracy + se, lower = accuracy - se)

(size_judge_plot_h <- ggplot(size_block_judge_se,aes(x = SizeCond, y = accuracy, fill=condition)) +
    geom_line(aes(group = CurvCond),colour="gray", size = 1)  +
    #geom_point(aes(group = condition,colour = condition),size = 3) +
    geom_errorbar(aes(ymin=lower, ymax=upper, col=condition),size = 1, width=.05) +
    scale_y_continuous(limits=c(0.9,1),oob = rescale_none) + 
    scale_color_manual(values = c("#3841E8",'#5EA8F1', "#E15207",'#EEA019')) +
    scale_fill_manual(values = c("#3056E8",'#5EA8F1', "#E15207",'#EEA019')) +
    geom_point(data = size_block_judge_se,aes(x = SizeCond, y = accuracy, fill=condition, color = condition), alpha=1,size = 3) +
    theme_few() + 
    ggtitle("Task: Big or Small?") + 
    labs(y="Accuracy", x="") +
    theme(legend.position = "none", aspect=1))

ggsave('Size_accuracyData_block1_interactionplot.png', width = 4,height = 4,unit =  "in", plot = size_judge_plot_h, path="./Figures-R/", device = "png",dpi = 300)

size_block_judge_se=summarySEwithin(data = d_judge_size_block, 'RT', withinvars = c('SizeCond','CurvCond','condition'), idvar = c('participant'))
size_block_judge_se <- size_block_judge_se %>%
  mutate(upper = RT + se, lower = RT - se)


(size_judge_plot_h <- ggplot(size_block_judge_se,aes(x = SizeCond, y = RT, fill=condition)) +
    geom_line(aes(group = CurvCond),colour="gray", size = 1)  +
    #geom_point(aes(group = condition,colour = condition),size = 3) +
    geom_errorbar(aes(ymin=lower, ymax=upper, col=condition),size = 1, width=.05) +
    scale_y_continuous(limits=c(500,650),oob = rescale_none) + 
    scale_color_manual(values = c("#3841E8",'#5EA8F1', "#E15207",'#EEA019')) +
    scale_fill_manual(values = c("#3056E8",'#5EA8F1', "#E15207",'#EEA019')) +
    geom_point(data = size_block_judge_se,aes(x = SizeCond, y = RT, fill=condition, color = condition), alpha=1,size = 3) +
    theme_few() + 
    ggtitle("Task: Big or Small?") + 
    labs(y="Average RT", x="") +
    theme(legend.position = "none", aspect=1))

ggsave('Size_RTData_block1_interactionplot.png',width = 4, height = 4,unit =  "in", plot = size_judge_plot_h, path="./Figures-R/", device = "png",dpi = 300)

#ANOVAs
## for size judgements
size_judge_block_aov = ezANOVA(dv= .(RT), wid= .(participant), within= .(SizeCond, CurvCond), detailed=TRUE, data=d_judge_size_block, type=3)
print(size_judge_block_aov)
size_judge_accuracy_block_aov = ezANOVA(dv= .(accuracy), wid= .(participant), within= .(SizeCond, CurvCond), detailed=TRUE, data=d_judge_size_block, type=3)
print(size_judge_accuracy_block_aov)

judge_block_aov = ezANOVA(dv= .(RT), wid= .(participant), within= .(SizeCond, CurvCond,current_task ), detailed=TRUE, data=d_rt_block, type=3)
print(judge_block_aov)


##Now with curvature task
d_judge_curv_block <- d_rt_block %>%
  filter(current_task=="JudgeCurviness") %>%
  filter(block==1)

curv_block_judge_se=summarySEwithin(data = d_judge_curv_block, 'accuracy', withinvars = c('SizeCond','CurvCond','condition'), idvar = c('participant'))
curv_block_judge_se <- curv_block_judge_se %>%
  mutate(upper = accuracy + se, lower = accuracy - se)

(curv_judge_plot_h <- ggplot(curv_block_judge_se,aes(x = CurvCond, y = accuracy, fill=condition)) +
    geom_line(aes(group = SizeCond),colour="gray", size = 1)  +
    #geom_point(aes(group = condition,colour = condition),size = 3) +
    geom_errorbar(aes(ymin=lower, ymax=upper, col=condition),size = 1, width=.05) +
    scale_y_continuous(limits=c(0.90,1),oob = rescale_none,position = "left") + 
    scale_color_manual(values = c("#3841E8",'#5EA8F1', "#E15207",'#EEA019')) +
    scale_fill_manual(values = c("#3056E8",'#5EA8F1', "#E15207",'#EEA019'))  +
    geom_point(data = curv_block_judge_se,aes(x = CurvCond, y = accuracy, fill=condition, color = condition), alpha=1,size = 3) +
    theme_few() + 
    ggtitle("Task: Boxy or Curvy?") + 
    labs(y="Accuracy", x="") +
    theme(legend.position = "none", aspect=1))

ggsave('Curvature_accuracyData_block1_interactionplot.png',width = 4, height = 4,unit =  "in", plot = curv_judge_plot_h, path="./Figures-R/", device = "png",dpi = 300)


curv_block_judge_se=summarySEwithin(data = d_judge_curv_block, 'RT', withinvars = c('SizeCond','CurvCond','condition'), idvar = c('participant'))
curv_block_judge_se <- curv_block_judge_se %>%
  mutate(upper = RT + se, lower = RT - se)

(curv_judge_plot_h <- ggplot(curv_block_judge_se,aes(x = CurvCond, y = RT, fill=condition)) +
    geom_line(aes(group = SizeCond),colour="gray", size = 1)  +
    #geom_point(aes(group = condition,colour = condition),size = 3) +
    geom_errorbar(aes(ymin=lower, ymax=upper, col=condition),size = 1, width=.05) +
    scale_y_continuous(limits=c(500,650),oob = rescale_none,position = "left") + 
    scale_color_manual(values = c("#3841E8",'#5EA8F1', "#E15207",'#EEA019')) +
    scale_fill_manual(values = c("#3056E8",'#5EA8F1', "#E15207",'#EEA019'))  +
    geom_point(data = curv_block_judge_se,aes(x = CurvCond, y = RT, fill=condition, color = condition), alpha=1,size = 3) +
    theme_few() + 
    ggtitle("Task: Boxy or Curvy?") + 
    labs(y="Average RT", x="") +
    theme(legend.position = "none", aspect=1))

ggsave('Curvature_RTData_block1_interactionplot.png',width = 4, height = 4,unit =  "in", plot = curv_judge_plot_h, path="./Figures-R/", device = "png",dpi = 300)

curv_judge_block_aov = ezANOVA(dv= .(RT), wid= .(participant), within= .(SizeCond, CurvCond), detailed=TRUE, data=d_judge_curv_block, type=3)
print(curv_judge_block_aov)
curv_judge_block_accuracy_aov = ezANOVA(dv= .(accuracy), wid= .(participant), within= .(SizeCond, CurvCond), detailed=TRUE, data=d_judge_curv_block, type=3)
print(curv_judge_block_accuracy_aov)


#Two-sample t-test on curvature judgments

d_judge_curv_block$SizeCurv <- with(d_judge_curv_block, interaction(SizeCond,  CurvCond), drop = TRUE )
levels(d_judge_curv_block$SizeCurv)[1:2]


##paired test for Small and Big Boxy
t.test(RT ~ SizeCurv, data = d_judge_curv_block[d_judge_curv_block$SizeCurv %in% levels(d_judge_curv_block$SizeCurv)[1:2],],paired = TRUE, alternative = "two.sided")
t.test(accuracy ~ SizeCurv, data = d_judge_curv_block[d_judge_curv_block$SizeCurv %in% levels(d_judge_curv_block$SizeCurv)[1:2],],paired = TRUE, alternative = "two.sided")

##paired test for Small and Big Curvy
t.test(RT ~ SizeCurv, data = d_judge_curv_block[d_judge_curv_block$SizeCurv %in% levels(d_judge_curv_block$SizeCurv)[3:4],],paired = TRUE, alternative = "two.sided")
t.test(accuracy ~ SizeCurv, data = d_judge_curv_block[d_judge_curv_block$SizeCurv %in% levels(d_judge_curv_block$SizeCurv)[3:4],],paired = TRUE, alternative = "two.sided")



