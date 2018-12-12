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
##########################
Gatekeeper results
##########################
Load and clean data
```{r}

setwd("P:/Evaluation/TN Lives Count_Writing/3_Target1_SUICClinicalTrainingComparison/3_Data & Analyses")
gate_pre_post = read.csv("datPrePostAnalysis.csv", header = TRUE)

## Get rid of Sec1 and Sec4, because lots of missing data
describe(gate_pre_post)
gate_pre_post$Sec1Total = NULL
gate_pre_post$Sec4Total = NULL
gate_pre_post_complete = na.omit(gate_pre_post)
dim(gate_pre_post_complete)
dim(gate_pre_post)
head(gate_pre_post_complete)

### Make wide format
gate_pre_post_complete_wide = reshape(gate_pre_post_complete, v.names = c("Sec2Total", "Sec3TotalF1", "Sec3TotalF2"),  timevar = "Time", direction = "wide", idvar = "ID")
head(gate_pre_post_complete_wide)


### Create differences for each of the variables
gate_pre_post_complete_wide$Sec2TotalDiff =gate_pre_post_complete_wide$Sec2Total.1-gate_pre_post_complete_wide$Sec2Total.0
gate_pre_post_complete_wide$Sec3TotalF1Diff =gate_pre_post_complete_wide$Sec3TotalF1.1-gate_pre_post_complete_wide$Sec3TotalF1.0
gate_pre_post_complete_wide$Sec3TotalF2Diff =gate_pre_post_complete_wide$Sec3TotalF2.1-gate_pre_post_complete_wide$Sec3TotalF2.0

### create standardized version of the three outcomes
gate_pre_post_complete_wide$Sec2TotalDiff_stand = scale(gate_pre_post_complete_wide$Sec2TotalDiff)
gate_pre_post_complete_wide$Sec3TotalF1Diff_stand = scale(gate_pre_post_complete_wide$Sec3TotalF1Diff)
gate_pre_post_complete_wide$Sec3TotalF2Diff_stand = scale(gate_pre_post_complete_wide$Sec3TotalF2Diff)

### Get rid of nas now that you have wide format
gate_pre_post_complete_wide = na.omit(gate_pre_post_complete_wide)
dim(gate_pre_post_complete_wide)
```
Checking assumptions and means
```{r}
## check normality
hist(gate_pre_post_complete_wide$Sec2TotalDiff)
hist(gate_pre_post_complete_wide$Sec3TotalF1Diff)
hist(gate_pre_post_complete_wide$Sec3TotalF2Diff)


compmeans(gate_pre_post_complete_wide$Sec2TotalDiff, gate_pre_post_complete_wide$Treatment)
compmeans(gate_pre_post_complete_wide$Sec3TotalF1Diff, gate_pre_post_complete_wide$Treatment)
compmeans(gate_pre_post_complete_wide$Sec3TotalF2Diff, gate_pre_post_complete_wide$Treatment)


```
Sec2 Data analysis
```{r}
post_stand_Sec2 = MCMCregress(Sec2TotalDiff_stand~ factor(Treatment), data = gate_pre_post_complete_wide)
summary(post_stand_Sec2)

post_stand_Sec3F1 = MCMCregress(Sec3TotalF1Diff_stand~ factor(Treatment), data = gate_pre_post_complete_wide)
summary(post_stand_Sec3F1)

post_stand_Sec3F2 = MCMCregress(Sec3TotalF2Diff_stand~ factor(Treatment), data = gate_pre_post_complete_wide)
summary(post_stand_Sec3F2)

```


