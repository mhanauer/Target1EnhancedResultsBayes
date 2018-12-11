---
title: "Enhanced Results"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Library the packages
```{r}
library(MCMCpack)
library(descr)
```

Prep the data
```{r}
setwd("P:/Evaluation/TN Lives Count_Writing/4_Target1_EnhancedCrisisFollow-up/3_Data & Data Analyses")
dat = read.csv("EnhancedDataSet.csv", header = TRUE)
head(dat)

dat_wide = reshape(dat, v.names = c("RASTotalScoreF1", "RASTotalScoreF2", "RASTotalScoreF3", "RASTotalScoreF5", "INQTotalScoreF1", "INQTotalScoreF2", "SSMITotalScore"),  timevar = "Time", direction = "wide", idvar = "ID")

f

dat_wide$RASDiffF1 =dat_wide$RASTotalScoreF1.1-dat_wide$RASTotalScoreF1.0
dat_wide$RASDiffF2 =dat_wide$RASTotalScoreF2.1-dat_wide$RASTotalScoreF2.0
dat_wide$RASDiffF3 =dat_wide$RASTotalScoreF3.1-dat_wide$RASTotalScoreF3.0
dat_wide$RASDiffF5 =dat_wide$RASTotalScoreF5.1-dat_wide$RASTotalScoreF5.0
dat_wide$INQDiffF1 =dat_wide$INQTotalScoreF1.1-dat_wide$INQTotalScoreF1.0  
dat_wide$INQDiffF2 =dat_wide$INQTotalScoreF2.1-dat_wide$INQTotalScoreF2.0
dat_wide$SSMIDiff =dat_wide$SSMITotalScore.1-dat_wide$SSMITotalScore.0

dat_wide = na.omit(dat_wide)
dim(dat_wide)
```
Regression to make sure the results match up with what we found
```{r}
compmeans(dat_wide$RASDiffF5,dat_wide$Treatment)

post = MCMCregress(RASDiffF5 ~ factor(Treatment), data = dat_wide)
summary(post)
plot(post)



```
Two steps: 
1. Transform the outcome to be standardized so we can interpret it closer to an effect size
```{r}
dat_wide$RASDiffF5_stand = scale(dat_wide$RASDiffF5)
hist(dat_wide$RASDiffF5_stand)
qqnorm(dat_wide$RASDiffF5_stand)

dat_wide$INQDiffF1_stand = scale(dat_wide$INQDiffF1)
hist(dat_wide$INQDiffF1_stand)
qqnorm(dat_wide$INQDiffF1_stand)

dat_wide$RASDiffF2_stand = scale(dat_wide$RASDiffF2)
hist(dat_wide$RASDiffF2_stand)
qqnorm(dat_wide$RASDiffF2_stand)

```
Now run analysis
```{r}
compmeans(dat_wide$RASDiffF5_stand,dat_wide$Treatment_recode)

post_stand_RAS = MCMCregress(RASDiffF5_stand ~ factor(Treatment), data = dat_wide)
summary(post_stand_RAS)
plot(post_stand_RAS)


```
Now try a different measure (INQ1)
```{r}
compmeans(dat_wide$INQDiffF1,dat_wide$Treatment)
post_stand_inq = MCMCregress(INQDiffF1_stand ~ factor(Treatment), data = dat_wide)
summary(post_stand_inq)
plot(post_stand)

```
Now try RASDiff2
```{r}
compmeans(dat_wide$RASDiffF2_stand,dat_wide$Treatment)
post_stand_RAS2 = MCMCregress(RASDiffF2_stand ~ factor(Treatment), data = dat_wide)
summary(post_stand_RAS2)
plot(post_stand_RAS2)

```


