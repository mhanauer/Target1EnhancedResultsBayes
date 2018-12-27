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
library(ggplot2)
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

dim(dat_wide)
dat_wide = data.frame(ID = dat_wide$ID, Age = dat_wide$Age, Gender = as.factor(dat_wide$Gender), Race = as.factor(dat_wide$Race), Treatment = as.factor(dat_wide$Treatment),Employment = as.factor(dat_wide$Employment),  RASDiffF5 = dat_wide$RASDiffF5)

dat_wide = na.omit(dat_wide)
dim(dat_wide)

describe(dat_wide)

range(dat_wide$RASDiffF5)
sd(dat_wide$RASDiffF5)

dat_wide$RASDiffF5_scaled = scale(dat_wide$RASDiffF5) 
hist(dat_wide$RASDiffF5_scaled)

```
Regression to make sure the results match up with what we found
```{r}
compmeans(dat_wide$RASDiffF5_scaled,dat_wide$Treatment)

prior_mean = as.vector(c(0,.5, .8))
prior_sd = as.vector(c(0,.1, .1))

post_prior = MCMCregress(RASDiffF5_scaled ~ factor(Treatment), b0 = prior_mean, B0 = prior_sd,data = dat_wide, seed = 12345)
summary(post_prior)
HPDinterval(post_prior)

post = MCMCregress(RASDiffF5_scaled ~ factor(Treatment),data = dat_wide, seed = 12345)
summary(post)
HPDinterval(post)

```
Diagnostics
```{r}
autocorr.plot(post_prior)
autocorr.diag(post_prior)

effectiveSize(post_prior)

# Not sure what this means
raftery.diag(post_prior)
# Should be low this is a way to tell if we sampled all the possible spaces
rejectionRate(post_prior)

```
Now try to get graphs for the posterior
```{r}
post_treatment2 =  data.frame(post_prior[,2])
names(post_treatment2)[1] = "posterior"
post_treatment3 = data.frame(post_prior[,3])
names(post_treatment3)[1] = "posterior"

quatile_post_treatment2 = quantile(post_treatment2$posterior,probs = c(.25, .95))
quatile_post_treatment2 = data.frame(quatile = quatile_post_treatment2)
ROPE = data.frame(ROPE = c(-.2, .2))

ggplot(post_treatment2, aes(x = posterior)) +
  geom_histogram() +
  geom_vline(data = quatile_post_treatment2, aes(xintercept = quatile), linetype = "dashed") +
  geom_vline(data = ROPE, aes(xintercept = ROPE))


```


