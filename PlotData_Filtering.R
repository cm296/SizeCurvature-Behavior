rm(list=ls())


setwd("/Users/caterinamagri/Dropbox (KonkLab)/Research-SizeCurvature/Project-SizeCurvatureBehavior_RerunbyStella/SizeCurv_Rerun_Reanalysis_ByCat/")



library(tidyverse)
library(readr)
library(knitr)
library(ggplot2)
library(reshape2)
library(ggthemes)
library(lme4)
library(stringr)
library(langcog)
library(lmerTest)
library(scales)

## Setup
### Import all data
files <- dir("DataFiles")
d.raw = data.frame()
for (f in files) {
  jf <- paste("DataFiles/",f,sep="")
  id <- read.csv(jf)
  id$participant = f
  d.raw <- bind_rows(d.raw, id)
}


## Make 
d.raw <- d.raw %>%
  mutate(condition = str_split_fixed(image,'_',2)[,1]) %>%
  mutate(big_or_small = grepl("Big",condition)) %>%
  mutate(boxy_or_curvy = grepl("Boxy",condition))

d.raw$big_or_small[d.raw$big_or_small=="TRUE"]="Big"
d.raw$big_or_small[d.raw$big_or_small=="FALSE"]="Small"

d.raw$boxy_or_curvy[d.raw$boxy_or_curvy=="TRUE"]="Boxy"
d.raw$boxy_or_curvy[d.raw$boxy_or_curvy=="FALSE"]="Curvy"


d_rt_raw <- d.raw %>%
  filter(RT>300 & RT<2000) %>%
  filter(ACC==1) %>% ## gets rid of timeout and incorrect
  mutate(logRT = log(RT))

d_rt <- d.raw %>%
  filter(RT>300 & RT<2000) %>%
  filter(ACC==1) %>% ## gets rid of timeout and incorrect
  mutate(logRT = log(RT)) %>%
  group_by(participant,big_or_small,boxy_or_curvy,current_task,condition) %>%
  summarize(meanRT = mean(RT), meanLogRT = mean(logRT))

d_rt_all_items <- d.raw %>%
  filter(RT>300 & RT<2000) %>%
  filter(ACC==1) %>%
  mutate(logRT = log(RT)) %>%
  group_by(participant,image,big_or_small,boxy_or_curvy,current_task,condition) %>%
  summarize(meanRT = mean(RT), meanLogRT = mean(logRT))


(combined_plot_size <- ggplot(d_size_judge_plot, aes(condition, mean, col = condition)) +
    geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper)) +
    ylim(c(400,1000)) +
    theme_few() + 
    scale_color_manual(values = c("#00008B",'#B5D3E7', "#CC5500",'#FFB347'))+
    theme(legend.position = "none", aspect.ratio = 1) +
    ggtitle("Real-world size judgements") + 
    labs(y="Average RT", x="")  +
    geom_point(data = d_size_judge,aes(x = condition, y = meanRT, col=condition), alpha=.2)  +
    geom_bar(data = d_size_judge_plot,stat="identity",aes(x = condition, y = mean, fill=condition)
    ))

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
