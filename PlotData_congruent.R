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

d_rt$congruent <- factor(d_rt$congruent,levels = c("0", "1")) 
levels(d_rt$congruent)
levels(d_rt$congruent)[1]<-"no"
levels(d_rt$congruent)[2]<-"yes"



### Check that interaction effects replicate in standard within-subjects ANOVA
library(ez)

#d_rt$participant <- factor(d_rt$participant) 
d_rt_aov = ezANOVA(dv= .(RT), wid= .(participant), within= .(SizeCond, CurvCond), between = .(congruent), detailed=TRUE, data=d_rt, type=3)
print(d_rt_aov)

d_accuracy_aov = ezANOVA(dv= .(accuracy), wid= .(participant), within= .(SizeCond, CurvCond), between = .(congruent), detailed=TRUE, data=d_rt, type=3)
print(d_accuracy_aov)

### Create subsets of data for plotting and model fitting
d_judge_congruent <- d_rt %>%
  filter(congruent == "yes")


d_judge_incongruent <- d_rt %>%
  filter(congruent == "no")


##Overall, across task:
##significant interaction for congruent
cong_judge_aov = ezANOVA(dv= .(RT), wid= .(participant), within= .(SizeCond, CurvCond), detailed=TRUE, data=d_judge_congruent, type=3)
print(cong_judge_aov)
cong_judge_accuracy_aov = ezANOVA(dv= .(accuracy), wid= .(participant), within= .(SizeCond, CurvCond), detailed=TRUE, data=d_judge_congruent, type=3)
print(cong_judge_accuracy_aov)

## not significant interaction for incongruent
inc_judge_aov = ezANOVA(dv= .(RT), wid= .(participant), within= .(SizeCond, CurvCond), detailed=TRUE, data=d_judge_incongruent, type=3)
print(inc_judge_aov)
inc_judge_accuracy_aov = ezANOVA(dv= .(accuracy), wid= .(participant), within= .(SizeCond, CurvCond), detailed=TRUE, data=d_judge_incongruent, type=3)
print(inc_judge_accuracy_aov)


d_curv_judge_congruent <- d_rt %>%
  filter(current_task == "JudgeCurviness")%>%
  filter(congruent == "yes")

d_curv_judge_incongruent <- d_rt %>%
  filter(current_task == "JudgeCurviness")%>%
  filter(congruent == "no")


## for curvature judgements, congruent group --> still significant
curv_c_judge_aov = ezANOVA(dv= .(RT), wid= .(participant), within= .(SizeCond, CurvCond), detailed=TRUE, data=d_curv_judge_congruent, type=3)
print(curv_c_judge_aov)
curv_c_judge_accuracy_aov = ezANOVA(dv= .(accuracy), wid= .(participant), within= .(SizeCond, CurvCond), detailed=TRUE, data=d_curv_judge_congruent, type=3)
print(curv_c_judge_accuracy_aov)

## for curvature judgements, incongruent group --> not significant interaction (but accuracy main effect for RT)
curv_inc_judge_aov = ezANOVA(dv= .(RT), wid= .(participant), within= .(SizeCond, CurvCond), detailed=TRUE, data=d_curv_judge_incongruent, type=3)
print(curv_inc_judge_aov)
curv_inc_judge_accuracy_aov = ezANOVA(dv= .(accuracy), wid= .(participant), within= .(SizeCond, CurvCond), detailed=TRUE, data=d_curv_judge_incongruent, type=3)
print(curv_inc_judge_accuracy_aov)


d_size_judge_congruent <- d_rt %>%
  filter(current_task == "JudgeSize")%>%
  filter(congruent == "yes")

d_size_judge_incongruent <- d_rt %>%
  filter(current_task == "JudgeSize")%>%
  filter(congruent == "no")


## for curvature judgements, congruent group --> still significant
size_c_judge_aov = ezANOVA(dv= .(RT), wid= .(participant), within= .(SizeCond, CurvCond), detailed=TRUE, data=d_size_judge_congruent, type=3)
print(size_c_judge_aov)
size_c_judge_accuracy_aov = ezANOVA(dv= .(accuracy), wid= .(participant), within= .(SizeCond, CurvCond), detailed=TRUE, data=d_size_judge_congruent, type=3)
print(size_c_judge_accuracy_aov)

## for curvature judgements, incongruent group --> not significant interaction (but accuracy main effect for RT)
size_inc_judge_aov = ezANOVA(dv= .(RT), wid= .(participant), within= .(SizeCond, CurvCond), detailed=TRUE, data=d_size_judge_incongruent, type=3)
print(size_inc_judge_aov)
size_inc_judge_accuracy_aov = ezANOVA(dv= .(accuracy), wid= .(participant), within= .(SizeCond, CurvCond), detailed=TRUE, data=d_size_judge_incongruent, type=3)
print(size_inc_judge_accuracy_aov)


########################################
##compute se at the item level -congruent

Data_item <- read.csv2("Analysis_cm/databyItem_table.csv", header = TRUE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
d_rt_item <- Data_item;

d_rt_item = d_rt_item[!is.na(d_rt_item$correctRTs),];

d_rt_item$SizeCurv <- with(d_rt_item, interaction(SizeCond,  CurvCond), drop = TRUE )


d_judge_cong_item <- d_rt_item %>%
  filter(congruent == "1")

cong_judge_se=summarySEwithin(data = d_judge_cong_item, 'correctRTs', withinvars = c('SizeCond','CurvCond','SizeCurv'), idvar = c('participant'))
cong_judge_se <- cong_judge_se %>%
  mutate(upper = correctRTs + se, lower = correctRTs - se)


(cong_judge_plot_h <- ggplot(cong_judge_se,aes(x = SizeCond, y = correctRTs, fill=SizeCurv)) +
    geom_line(aes(group = CurvCond),colour="gray", size = 1)  +
    geom_errorbar(aes(ymin=lower, ymax=upper, col=SizeCurv),size = 1, width=.05) +
    scale_y_continuous(limits=c(500,650),oob = rescale_none) + 
    scale_color_manual(values = c("#3841E8","#E15207",'#5EA8F1', '#EEA019')) +
    #scale_fill_manual(values = c("#3841E8","#E15207",'#5EA8F1', '#EEA019')) +
    geom_point(data = cong_judge_se,aes(x = SizeCond, y = correctRTs, fill=SizeCurv, color = SizeCurv), alpha=1,size = 3) +
    theme_few() + 
    ggtitle("Across tasks, congruent group")+
    labs(y="Average RT", x="") +
    theme(legend.position = "none", aspect=1))

ggsave('CongruentGroups_AcrossTasks_RTData_interactionplot.png',width = 4, height = 4,unit =  "in", plot = cong_judge_plot_h, path="./Figures-R/", device = "png",dpi = 300)

cong_judge_accuracy_se=summarySEwithin(data = d_judge_cong_item, 'accuracy', withinvars = c('SizeCond','CurvCond','SizeCurv'), idvar = c('participant'))
cong_judge_accuracy_se <- cong_judge_accuracy_se %>%
  mutate(upper = accuracy + se, lower = accuracy - se)

(cong_judge_plot_h <- ggplot(cong_judge_accuracy_se,aes(x = SizeCond, y = accuracy, fill=SizeCurv)) +
    geom_line(aes(group = CurvCond),colour="gray", size = 1)  +
    geom_errorbar(aes(ymin=lower, ymax=upper, col=SizeCurv),size = 1, width=.05) +
    scale_y_continuous(limits=c(0.9,1),oob = rescale_none) + 
    scale_color_manual(values = c("#3841E8","#E15207",'#5EA8F1', '#EEA019')) +
    #scale_fill_manual(values = c("#3841E8","#E15207",'#5EA8F1', '#EEA019')) +
    geom_point(data = cong_judge_accuracy_se,aes(x = SizeCond, y = accuracy, fill=SizeCurv, color = SizeCurv), alpha=1,size = 3) +
    theme_few() + 
    ggtitle("Across Tasks, congruent group") + 
    labs(y="Accuracy", x="") +
    theme(legend.position = "none", aspect=1))

ggsave('CongruentGroups_AcrossTasks_accuracyData_interactionplot.png',width = 4, height = 4,unit =  "in", plot = cong_judge_plot_h, path="./Figures-R/", device = "png",dpi = 300)



d_judge_incong_item <- d_rt_item %>%
  filter(congruent == "0")

incong_judge_se=summarySEwithin(data = d_judge_incong_item, 'correctRTs', withinvars = c('SizeCond','CurvCond','SizeCurv'), idvar = c('participant'))
incong_judge_se <- incong_judge_se %>%
  mutate(upper = correctRTs + se, lower = correctRTs - se)


(incong_judge_plot_h <- ggplot(incong_judge_se,aes(x = SizeCond, y = correctRTs, fill=SizeCurv)) +
    geom_line(aes(group = CurvCond),colour="gray", size = 1)  +
    geom_errorbar(aes(ymin=lower, ymax=upper, col=SizeCurv),size = 1, width=.05) +
    scale_y_continuous(limits=c(500,650),oob = rescale_none,position = "right") + 
    scale_color_manual(values = c("#3841E8","#E15207",'#5EA8F1', '#EEA019')) +
    #scale_fill_manual(values = c("#3841E8","#E15207",'#5EA8F1', '#EEA019')) +
    geom_point(data = incong_judge_se,aes(x = SizeCond, y = correctRTs, fill=SizeCurv, color = SizeCurv), alpha=1,size = 3) +
    theme_few() + 
    ggtitle("Across tasks, incongruent group")+
    labs(y="Average RT", x="") +
    theme(legend.position = "none", aspect=1))

ggsave('IncongruentGroups_AcrossTasks_RTData_interactionplot.png',width = 4, height = 4,unit =  "in", plot = incong_judge_plot_h, path="./Figures-R/", device = "png",dpi = 300)

incong_judge_accuracy_se=summarySEwithin(data = d_judge_incong_item, 'accuracy', withinvars = c('SizeCond','CurvCond','SizeCurv'), idvar = c('participant'))
incong_judge_accuracy_se <- incong_judge_accuracy_se %>%
  mutate(upper = accuracy + se, lower = accuracy - se)

(incong_judge_plot_h <- ggplot(incong_judge_accuracy_se,aes(x = SizeCond, y = accuracy, fill=SizeCurv)) +
    geom_line(aes(group = CurvCond),colour="gray", size = 1)  +
    geom_errorbar(aes(ymin=lower, ymax=upper, col=SizeCurv),size = 1, width=.05) +
    scale_y_continuous(limits=c(0.90,1),oob = rescale_none,position = "right") + 
    scale_color_manual(values = c("#3841E8","#E15207",'#5EA8F1', '#EEA019')) +
    #scale_fill_manual(values = c("#3841E8","#E15207",'#5EA8F1', '#EEA019')) +
    geom_point(data = incong_judge_accuracy_se,aes(x = SizeCond, y = accuracy, fill=SizeCurv, color = SizeCurv), alpha=1,size = 3) +
    theme_few() + 
    ggtitle("Across Tasks, incongruent group") + 
    labs(y="Accuracy", x="") +
    theme(legend.position = "none", aspect=1))

ggsave('IncongruentGroups_AcrossTasks_accuracyData_interactionplot.png',width = 4, height = 4,unit =  "in", plot = incong_judge_plot_h, path="./Figures-R/", device = "png",dpi = 300)


