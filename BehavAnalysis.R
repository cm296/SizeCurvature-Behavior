#---
#  title: "Real-world size & curvature: behavioral analyses"
#author: "Cat Magri (followin Bria Long)"
#date: "12/1/2018"
#output: 
#  html_document:
#  toc: true # make table of contents
#toc_depth: 3
#theme: united
#highlight: tango
#---
install.packages("lmerTest")
  
#  ```{r setup, include=FALSE}
#knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(readr)
library(knitr)
#library(ggplot2)
library(reshape2)
library(ggthemes)
library(lme4)
library(stringr)
library(langcog)
library(lmerTest)
library(scales)
#```


## Setup
### Import all data
#```{r}
files <- dir("DataFiles")
d.raw = data.frame()
for (f in files) {
  jf <- paste("DataFiles/",f,sep="")
  id <- read.csv(jf)
  id$subid = f
  d.raw <- bind_rows(d.raw, id)
}
#```