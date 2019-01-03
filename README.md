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
library(BEST)
library(psych)
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
Remember that precision is the standard deviation and is 1/o^2
```{r}
compmeans(dat_wide$RASDiffF5_scaled,dat_wide$Treatment)

prior_mean = as.vector(c(0,.4, .8))
prior_sd = as.vector(c(0,1/.2^2, 1/.2^2))

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
Posterior graph for informed T1 v T2
Now try to get graphs for the posterior
```{r}
post_treatment2 =  data.frame(post_prior[,2])
names(post_treatment2)[1] = "posterior"
post_treatment3 = data.frame(post_prior[,3])
names(post_treatment3)[1] = "posterior"

quatile_post_treatment2 = quantile(post_treatment2$posterior,probs = c(.025, .975))
quatile_post_treatment2 = data.frame(quatile = quatile_post_treatment2)
ROPE = data.frame(ROPE = c(-.2, .2))
quatile_post_treatment2
ggplot(post_treatment2, aes(x = posterior)) +
  geom_histogram() +
  geom_vline(data = quatile_post_treatment2, aes(xintercept = quatile), linetype = "dashed") +
  geom_vline(data = ROPE, aes(xintercept = ROPE))


```
Graph for treatment 3
```{r}
post_treatment3 = data.frame(post_prior[,3])
names(post_treatment3)[1] = "posterior"

quatile_post_treatment2 = quantile(post_treatment3$posterior,probs = c(.025, .975))
quatile_post_treatment2 = data.frame(quatile = quatile_post_treatment2)
ROPE = data.frame(ROPE = c(-.2, .2))

ggplot(post_treatment3, aes(x = posterior)) +
  geom_histogram() +
  geom_vline(data = quatile_post_treatment2, aes(xintercept = quatile), linetype = "dashed") +
  geom_vline(data = ROPE, aes(xintercept = ROPE))
```
Evaluate the difference between the parameter estimates to see if they are practically and statistically significanly different from each other
```{r}
post_treatment2 =  data.frame(post_prior[,2])
names(post_treatment2)[1] = "posterior2"
post_treatment3 = data.frame(post_prior[,3])
names(post_treatment3)[1] = "posterior3"

post_treat23 = cbind(post_treatment2, post_treatment3)


post_treat23_sample = post_treat23[sample(nrow(post_treat23),500),]
dim(post_treat23_sample)


# Version of 2015 Dec 02.
# John K. Kruschke  
# johnkruschke@gmail.com
# http://www.indiana.edu/~kruschke/BEST/
#
# This program is believed to be free of errors, but it comes with no guarantee! 
# The user bears all responsibility for interpreting the results. 
# Please check the webpage above for updates or corrections.
#
################################################################################
### To run this program, please prepare your computer as follows.
### 1. Install the general-purpose programming language R from  
###      http://www.r-project.org/
###    Install the version of R appropriate for your computer's operating
###    system (Windows, MacOS, or Linux).   
### 2. Install the Bayesian MCMC sampling program JAGS from
###      http://mcmc-jags.sourceforge.net/
###    Again, intall the version appropriate for your operating system.
### 3. Install the R editor, RStudio, from
###      http://rstudio.org/
###    This editor is not necessary, but highly recommended.
### 4. Make sure that the following programs are all
###    in the same folder as this file:
###      BESTexample.R (this file)
###      BEST.R
###      DBDA2E-utilities.R
###      BESTexamplePower.R 
### 5. Make sure that R's working directory is the folder in which those 
###    files reside. In RStudio, use menu tabs Tools -> Set Working Directory.
###    If working in R, use menu tabs File -> Change Dir.
### 6. After the above actions are accomplished, this program should
###    run as-is in R. You may "source" it to run the whole thing at once,
###    or, preferably, run lines one at a time in order.
################################################################################

# OPTIONAL: Clear R's memory and graphics:
rm(list=ls())  # Careful! This clears all of R's memory!
graphics.off() # This closes all of R's graphics windows.

# Get the functions loaded into R's working memory:
setwd("C:/Users/Matthew.Hanauer/Desktop/BEST")
source("BEST.R")

# Specify data as vectors. Replace with your own data as needed. 
# (R can read many formats of data files, see the commands "read.csv" or "scan"
# etc. Also see the R package "foreign".)
y1 = post_treat23_sample$posterior2
y2 = post_treat23_sample$posterior3

#----------------------------------------------------------------------------
# Run the Bayesian analysis using the default broad prior:
mcmcChain = BESTmcmc( y1,  y2, priorOnly=FALSE ,numSavedSteps=12000 , thinSteps=5 , showMCMC=TRUE ) 
postInfo = BESTplot( y1 , y2 , mcmcChain , ROPEeff=c(-0.1,0.1) ) 
#----------------------------------------------------------------------------
# Show detailed summary info on console, output from BESTplot, above:
show( postInfo ) 
# You can save the plot(s) using the pull-down menu in the R graphics window,
# or by using the following:
saveGraph( file="BESTexample" , type="png" )

#----------------------------------------------------------------------------
## Save the data and results for future use, e.g. for power computation:
save( y1, y2, mcmcChain, postInfo, file="BESTexampleMCMC.Rdata" )
## To re-load the saved data and MCMC chain, type: 
# load( "BESTexampleMCMC.Rdata" ) 

#----------------------------------------------------------------------------
# Frequentist tests:
# t.test(y1,y2)
# var.test(y1,y2)


#-------------------------------------------------------------------------------




```


Power for treatment two versus treatment one
This is observed power 
```{r}
matt_power = function(){
n = 109
intercept = -0.4215
treat2v1 = .583
treat3v1 = .815
intervention  = c(rep(1,round(n*42/109,0)), rep(2,round(n*38/109,0)), rep(3,round(n*(29)/109,0)))
length(intervention)

intervention1 = ifelse(intervention == 1, 1, 0)
intervention2 = ifelse(intervention == 2,1,0)
intervention3 = ifelse(intervention == 3, 1, 0)

y = intercept + intervention2*treat2v1 + intervention3*treat3v1 + rnorm(n =n, mean = 0, sd = 1)

dat_bayes_power = data.frame(y = y, intervention2, intervention3)


prior_mean = as.vector(c(0,.4, .8))
prior_sd = as.vector(c(0,.2, .2))

post_prior = MCMCregress(y ~ intervention2 + intervention3,data = dat_bayes_power)

post_prior_summary = summary(post_prior)
cred_inter_2.5 = post_prior_summary$quantiles[2,c(1)]

# This should world, because want to know whether the value is great .2 or less than -.2
cred_inter_2.5 = ifelse(cred_inter_2.5 > .2 |  cred_inter_2.5 < -.2, 1, 0)

cred_inter_97.5 = post_prior_summary$quantiles[2,c(5)]

# This should world, because want to know whether the value is great .2 or less than -.2
cred_inter_97.5 = ifelse(cred_inter_97.5 > .2 |  cred_inter_97.5 < -.2, 1, 0)
#
cred_inter = sum(cred_inter_2.5, cred_inter_97.5)
cred_inter = ifelse(cred_inter == 2, 1,0)
cred_inter
}

reps = 1000
power = data.frame(replicate(reps, matt_power()))
power = apply(power, 2, sum)/reps
power
```
Predictive probability (observed power with the addition of new participants)
```{r}
matt_predict_power = function(){
n = 290
intercept = -0.4215
treat2v1 = .583
treat3v1 = .815
intervention  = c(rep(1,round(n*42/109,0)), rep(2,round(n*38/109,0)), rep(3,round(n*(29)/109,0)))
length(intervention)

intervention1 = ifelse(intervention == 1, 1, 0)
intervention2 = ifelse(intervention == 2,1,0)
intervention3 = ifelse(intervention == 3, 1, 0)

y = intercept + intervention2*treat2v1 + intervention3*treat3v1 + rnorm(n =n, mean = 0, sd = 1)

dat_bayes_power = data.frame(y = y, intervention2, intervention3)


prior_mean = as.vector(c(0,.4, .8))
prior_sd = as.vector(c(0,.2, .2))

post_prior = MCMCregress(y ~ intervention2 + intervention3,data = dat_bayes_power)

post_prior_summary = summary(post_prior)
cred_inter_2.5 = post_prior_summary$quantiles[2,c(1)]

# This should world, because want to know whether the value is great .2 or less than -.2
cred_inter_2.5 = ifelse(cred_inter_2.5 > .2 |  cred_inter_2.5 < -.2, 1, 0)

cred_inter_97.5 = post_prior_summary$quantiles[2,c(5)]

# This should world, because want to know whether the value is great .2 or less than -.2
cred_inter_97.5 = ifelse(cred_inter_97.5 > .2 |  cred_inter_97.5 < -.2, 1, 0)
#
cred_inter = sum(cred_inter_2.5, cred_inter_97.5)
cred_inter = ifelse(cred_inter == 2, 1,0)
cred_inter
}

reps = 1000
power = data.frame(replicate(reps, matt_predict_power()))
power = apply(power, 2, sum)/reps
power
```

```



Evaluate the difference between the parameter estimates to see if they are practically and statistically significanly different from each other
```{r}
post_treatment2 =  data.frame(post[,2])
names(post_treatment2)[1] = "posterior2"
post_treatment3 = data.frame(post[,3])
names(post_treatment3)[1] = "posterior3"

post_treat23 = cbind(post_treatment2, post_treatment3)


post_treat23_sample = post_treat23[sample(nrow(post_treat23),500),]
dim(post_treat23_sample)



# OPTIONAL: Clear R's memory and graphics:
#rm(list=ls())  # Careful! This clears all of R's memory!
graphics.off() # This closes all of R's graphics windows.

# Get the functions loaded into R's working memory:
setwd("C:/Users/Matthew.Hanauer/Desktop/BEST")
source("BEST.R")

# Specify data as vectors. Replace with your own data as needed. 
# (R can read many formats of data files, see the commands "read.csv" or "scan"
# etc. Also see the R package "foreign".)
y1 = post_treat23_sample$posterior2
y2 = post_treat23_sample$posterior3

#----------------------------------------------------------------------------
# Run the Bayesian analysis using the default broad prior:
mcmcChain = BESTmcmc( y1,  y2, priorOnly=FALSE ,numSavedSteps=12000 , thinSteps=5 , showMCMC=TRUE ) 
postInfo = BESTplot( y1 , y2 , mcmcChain , ROPEeff=c(-0.1,0.1) ) 
#----------------------------------------------------------------------------
# Show detailed summary info on console, output from BESTplot, above:
show( postInfo ) 
```

Graph for uninformed T1 v T2

Graph for uninformed T1 v T3

