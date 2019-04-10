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
library(ggplot2)
```
Load the data
```{r}
setwd("P:/Evaluation/TN Lives Count_Writing/4_Target1_EnhancedCrisisFollow-up/3_Data & Data Analyses")
datPreAdult = read.csv("Target1EnhancedBaseAdult.csv", header = TRUE)
datPostAdult = read.csv("Target1EnhancedPostAdult.csv", header = TRUE)
datPreYouth = read.csv("Target1EnhancedBaseYouth.csv", header= FALSE, row.names = NULL)
datPostYouth = read.csv("Target1EnhancedPostYouth.csv", header = FALSE, row.names = NULL)
datAdultTreat = read.csv("AdultTreatments.csv", header = TRUE)
library(foreign)
library(nnet)
library(ggplot2)
library(prettyR)
library(nlme)
library(prettyR)
library(descr)
library(Amelia)
library(mitools)
library(BaylorEdPsych)
library(openxlsx)
library(lavaan)
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(prettyR)
library(semTools)
library(GPArotation)
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(lordif)
library(Amelia)
library(plyr)
library(DescTools)
library(MissMech)
library(jtools)
library(paran)
library(effsize)
library(multcomp)
library(MuMIn)
library(installr)
library(konfound)
library(multcomp)





head(datPreAdult)
# subset the variables that you want
datPreAdult =datPreAdult[c(1, 3:4, 6,8,10, 12:13, 15:34, 36:45, 47:51, 53:59)]
dim(datPreAdult)
head(datPostAdult)
datPostAdult = datPostAdult[c(1, 3:22,24:33, 35:39, 41:47)]
head(datPostAdult)
dim(datPostAdult)
# Rename added variables otherwise everything else should be the same
colnames(datPreAdult)[colnames(datPreAdult) == "Added.V2..Thinking.of.Ways.to.Kill.Self"] = "Added"   

## Now merge everything
datAdult = merge(datPreAdult, datPostAdult, by = "Adult.ID", sort = TRUE)
head(datAdult)
dim(datAdult)

# Need to er
#datAdultTreat = read.csv("AdultTreatment.csv", header = TRUE)
#Now we need to merge the treatment variable before we transform into long format to avoid duplication.

head(datAdultTreat)

## If you don't have a treatment you cannot be included
datAdult = merge(datAdult, datAdultTreat, by = "Adult.ID", sort = TRUE)
head(datAdult)
dim(datAdult)

### This is the actual sample size, because you cannot be included in the study if you do not have a treatment
dim(datAdult)


datAdult = reshape(datAdult, varying = list(c("Desire.to.succeed.x", "Desire.to.succeed.y"), c("My.own.plan.to.stay.well.x", "My.own.plan.to.stay.well.y"), c("Goals.in.life.x", "Goals.in.life.y"), c("Believe.I.can.meet.personal.goals.x", "Believe.I.can.meet.personal.goals.y"), c("Purpose.in.life.x", "Purpose.in.life.y"), c("Fear.doesn.t.stop.me......x", "Fear.doesn.t.stop.me......y"), c("I.can.handle.my.life.x", "I.can.handle.my.life.y"), c("I.like.myself.x", "I.like.myself.y"), c("If.people.really.knew.me.......x", "If.people.really.knew.me.......y"), c("Who.I.want.to.become.x", "Who.I.want.to.become.y"), c("Something.good.will.happen.x", "Something.good.will.happen.y"), c("I.m.hopeful.about.future.x", "I.m.hopeful.about.future.y"), c("Continue.to.have.new.interests.x", "Continue.to.have.new.interests.y"), c("Coping.with.mental.illness.not.focus.of.life.x", "Coping.with.mental.illness.not.focus.of.life.y"), c("Symptoms.interfere.less.and.less.x", "Symptoms.interfere.less.and.less.y"), c("Symptoms.problem.for.shorter.periods.x", "Symptoms.problem.for.shorter.periods.y"), c("Know.when.to.ask.for.help.x", "Know.when.to.ask.for.help.y"), c("Willing.to.ask.for.help.x", "Willing.to.ask.for.help.y"), c("I.ask.for.help.when.I.need.it..x", "I.ask.for.help.when.I.need.it..y"), c("I.can.handle.stress..x", "I.can.handle.stress..y"), c("Better.off.if.I.were.gone.x", "Better.off.if.I.were.gone.y"), c("Happier.without.me.x", "Happier.without.me.y"), c("Death.would.be.a.relief.x", "Death.would.be.a.relief.y"), c("Wish.they.could.be.rid.of.me.x", "Wish.they.could.be.rid.of.me.y"), c("Make.things.worse.x", "Make.things.worse.y"), c("Feel.like.I.belong.x", "Feel.like.I.belong.y"), c("Have.many.caring.and.supportive.friends.x", "Have.many.caring.and.supportive.friends.y"), c("Feel.disconnected.x", "Feel.disconnected.y"), c("Feel.like.an.outsider.x", "Feel.like.an.outsider.y"), c("Close.to.other.people.x", "Close.to.other.people.y"), c("Unable.to.take.care.of.self.x", "Unable.to.take.care.of.self.y"), c("Not.recover.or.get.better.x", "Not.recover.or.get.better.y"), c("I.am.to.blame.x", "I.am.to.blame.y"), c("Unpredictable.x", "Unpredictable.y"), c("Dangerous.x", "Dangerous.y"), c("Wish.life.would.end..x", "Wish.life.would.end..y"), c("Life.not.worth.living.x", "Life.not.worth.living.y"), c("Life.so.bad..feel.like.giving.up..x", "Life.so.bad..feel.like.giving.up..y"), c("Better.for.everyone.if.I.were.to.die..x", "Better.for.everyone.if.I.were.to.die..y"), c("Added.x", "Added.y"), c("No.solution.to.my.problems.x", "No.solution.to.my.problems.y"), c("Believe.my.life.will.end.in.suicide..x", "Believe.my.life.will.end.in.suicide..y")), direction = "long", times = c(0,1))



colnames(datAdult) = c("ID", "Age", "Gender", "Race", "SexualOrientation", "RelationshipStatus", "Edu", "Employment", "Treatment", "Time", "RAS1", "RAS2", "RAS3", "RAS4", "RAS5", "RAS6", "RAS7", "RAS8", "RAS9", "RAS10", "RAS11", "RAS12", "RAS13", "RAS14", "RAS15", "RAS16", "RAS17", "RAS18", "RAS19", "RAS20", "INQ1", "INQ2", "INQ3", "INQ4", "INQ5", "INQ6", "INQ7", "INQ8", "INQ9", "INQ10", "SSMI1", "SSMI2", "SSMI3", "SSMI4", "SSMI5", "SIS1", "SIS2", "SIS3", "SIS4", "SIS5", "SIS6", "SIS7")

# Drop last column id 
datAdult = data.frame(datAdult)
head(datAdult)
datAdult$NA. = NULL
dim(datAdult)

datAdult = datAdult[order(datAdult$ID),]


describe.factor(datAdult$Treatment)

# Two treatments have B with space first so try and recode those as just B's
datAdult$Treatment = ifelse(datAdult$Treatment == "A", 1, ifelse(datAdult$Treatment =="B", 2, ifelse(datAdult$Treatment == " B", 2, ifelse(datAdult$Treatment == "C", 3, datAdult$Treatment)))) 
describe.factor(datAdult$Treatment)

dim(subset(datAdult, Time  == 1))
dim(subset(datAdult, Time  == 0))


# Three items are reversed scored: f = 6, g = 7, j = 10
datAdult$INQ6 = ifelse(datAdult$INQ6 == 1, 5, ifelse(datAdult$INQ6 == 2,4, ifelse(datAdult$INQ6  == 3,3, ifelse(datAdult$INQ6  == 4,2, ifelse(datAdult$INQ6  == 5,1,datAdult$INQ6)))))

datAdult$INQ7= ifelse(datAdult$INQ7== 1, 5, ifelse(datAdult$INQ7== 2,4, ifelse(datAdult$INQ7 == 3,3, ifelse(datAdult$INQ7 == 4,2, ifelse(datAdult$INQ7 == 5,1,datAdult$INQ7)))))

datAdult$INQ10= ifelse(datAdult$INQ10== 1, 5, ifelse(datAdult$INQ10== 2,4, ifelse(datAdult$INQ10 == 3,3, ifelse(datAdult$INQ10 == 4,2, ifelse(datAdult$INQ10 == 5,1,datAdult$INQ10)))))


head(datAdult)

#Checking for issues with adult data set
## In the paper start with 115, because we have three less people later
## 763.0 likely double entry 1131.0, 1272
datAdult[c(187:189, 227:229), c(1:5)]

datAdult = datAdult[-c(187,189, 227,229),]

## Two people dropped, because of two dups
dim(datAdult)
describe.factor(datAdult$Time)
### SO 116 is the analytical starting place#######

datDemo = datAdult[,c(1:10)]


# Create pre data sets for psychometrics
head(datAdult)
RAS = datAdult[c(11:30)]
head(RAS)

head(datAdult)
INQ = datAdult[c(31:40)]
head(INQ)

SSMI = datAdult[c(41:45)]
head(SSMI)

SIS = datAdult[c(46:52)]
SIS

# Subscale one: f =6, g = 7, h = 8, I = 9,  j = 10, k = 11, l = 12, m = 13, t = 20
RASSub1 = RAS[c(6:13, 20)]


# Subscale two q = 17, r= 18, s= 19
head(RAS)
RASSub2 = RAS[c(17:19)]
# Subscale three: a = 1, b = 2, c = 3, d= 4, e = 5
head(RAS)
RASSub3 = RAS[c(1:5)]

# Subscale five: n = 14, o = 15, p = 16
head(RAS)
RASSub5 = RAS[c(14:16)]


#Subscale 1 for INQ: a = 1, b = 2, c = 3, d = 4, e = 5
INQSub1 = INQ[c(1:5)]

#Subscale 2 for INQ: f-j: 6-10
INQSub2 = INQ[c(6:10)]

#Subscale 1 for SIS: a-d: 1:4
SISSub1 = SIS[c(1:4)]

#Subscale 2 for SIS: e-g: 5:7
SISSub2 = SIS[c(5:7)]

SSMIPrePost = datAdult[c(41:45)]
# Creating sum scores for the data analysis that contains all data not just pre data
datAdultDemos = datAdult[c(1:10)]


RASTotalScoreF1 = rowSums(RASSub1)
RASTotalScoreF2 = rowSums(RASSub2)
RASTotalScoreF3 = rowSums(RASSub3)
RASTotalScoreF5 = rowSums(RASSub5)
INQTotalScoreF1 = rowSums(INQSub1)
INQTotalScoreF2 = rowSums(INQSub2)
SISTotalScoreF1 = rowSums(SISSub1)
SISTotalScoreF2 = rowSums(SISSub2)
SSMITotalScore = rowSums(SSMIPrePost)



datAdultAnalysis = data.frame(datDemo, RASTotalScoreF1, RASTotalScoreF2, RASTotalScoreF3, RASTotalScoreF5, INQTotalScoreF1, INQTotalScoreF2, SISTotalScoreF1, SISTotalScoreF2, SSMITotalScore)
dim(datAdultAnalysis)

#Need code gender, race, sexual orientation, edu, employment, RelationshipStatus as binary
#Gender: 2 = 1, 1 = 0 female
#Race: 7 = 0, all else 1 non-white
#Sex Orien: 3 = 0, all else 1 sexual minrotiry
#Edu: 2 = 1, all else = 0; high school over lower for one
#Employment 1 = 1 else = 0; unemployed versus everyone else
#Relationship Status: 1,2,3,4 = 1 else = 0 single


datAdultAnalysis$Gender = ifelse(datAdultAnalysis$Gender == 2,1, 0)
datAdultAnalysis$Race = ifelse(datAdultAnalysis$Race == 7,0, 1)
datAdultAnalysis$SexualOrientation = ifelse(datAdultAnalysis$SexualOrientation == 3,0, 1)
datAdultAnalysis$Edu = ifelse(datAdultAnalysis$Edu <= 2,1, 0)
datAdultAnalysis$Employment = ifelse(datAdultAnalysis$Employment == 1,1, 0)

datAdultAnalysis$RelationshipStatus = ifelse(datAdultAnalysis$RelationshipStatus <= 4, 1, 0)
describe.factor(datAdultAnalysis$RelationshipStatus)
# For the complete data set I need to drop SIS, because there is a ton of missing data
dim(datAdultAnalysis)
describe.factor(datAdultAnalysis$SISTotalScoreF2)
datAdultAnalysis$SISTotalScoreF2 = NULL
 
```
Prep the data
```{r}

dat_wide = reshape(datAdultAnalysis, v.names = c("RASTotalScoreF1", "RASTotalScoreF2", "RASTotalScoreF3", "RASTotalScoreF5", "INQTotalScoreF1", "INQTotalScoreF2", "SSMITotalScore"),  timevar = "Time", direction = "wide", idvar = "ID")



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

1/2^2
```
Try models with rstan package
```{r}
library(rstanarm)
post_stan = stan_glm(RASDiffF5_scaled ~ factor(Treatment),data = dat_wide)
summary(post_stan)


### Need to figure out how to include multiple priors 
my_prior_stan <- student_t(df = 10, location = c(.4,.8), scale = c(.2, .2), autoscale = FALSE)
post_prior_stan = stan_glm(RASDiffF5_scaled ~ factor(Treatment),data = dat_wide, prior = my_prior)
summary(post_prior)
prior_summary(post_prior_stan)

### Bayesian R^2
median(bayes_R2(post_stan))
median(bayes_R2(post_prior_stan))

### Track draws from the distribution and see if those draws are similar to actual value
pp_check(post_prior_stan)
pp_check(post_stan)



###Try LOO 
loo_post = loo(post_stan, cores = 3)
loo_post_prior = loo(post_prior_stan, cores = 3)

compare_models(loo_post_prior, loo_post)
```


Diagnostics
```{r}
autocorr.plot(post_prior)
 
autocorr.diag(post_prior)

effectiveSize(post_prior)
effectiveSize(post)


# Not sure what this means
raftery.diag(post_prior)
# Should be low this is a way to tell if we sampled all the possible spaces
rejectionRate(post_prior)

```
Get ready for informed graphs
```{r}

post_treatment2 =  data.frame(post_prior[,2])
names(post_treatment2)[1] = "posterior"
post_treatment3 = data.frame(post_prior[,3])
names(post_treatment3)[1] = "posterior"



quatile_post_treatment2 = quantile(post_treatment2$posterior,probs = c(.025, .975))
quatile_post_treatment2 = data.frame(quatile = quatile_post_treatment2)
ROPE = data.frame(ROPE = c(-.2, .2))

post_treatment3 = data.frame(post_prior[,3])
names(post_treatment3)[1] = "posterior"

quatile_post_treatment3 = quantile(post_treatment3$posterior,probs = c(.025, .975))
quatile_post_treatment3 = data.frame(quatile = quatile_post_treatment3)
ROPE = data.frame(ROPE = c(-.2, .2))

```


Posterior graph for informed T1 v T2 and T1 v T3
Now try to get graphs for the posterior
```{r}
library(gridExtra)

plot1 = ggplot(post_treatment2, aes(x = posterior)) +
  geom_histogram() +
  geom_vline(data = quatile_post_treatment2, aes(xintercept = quatile), linetype = "dashed") +
  geom_vline(data = ROPE, aes(xintercept = ROPE)) +
  ggtitle("Figure 1: Informed Posterior Program 1 v Program 2")

## Now treatment three

plot2 = ggplot(post_treatment3, aes(x = posterior)) +
  geom_histogram() +
  geom_vline(data = quatile_post_treatment3, aes(xintercept = quatile), linetype = "dashed") +
  geom_vline(data = ROPE, aes(xintercept = ROPE))+
  ggtitle("Figure 2: Informed Posterior Program 1 v Program 3")

grid.arrange(plot1,plot2)

```
Create Figure 1 for Paper Demonstrating accepting the null hypothesis
```{r}
posterior = rnorm(n = 10000, mean = 0, sd = .05)
ROPE = data.frame(ROPE = c(-.2, .2))
quantile_fig1 = quantile(posterior,probs = c(.025, .975))
quantile_fig1 = data.frame(quantile_fig1 = quantile_fig1)
posterior = data.frame(posterior)

plot_fig1 = ggplot(posterior, aes(x = posterior)) +
  geom_histogram() +
  geom_vline(data = quantile_fig1, aes(xintercept = quantile_fig1), linetype = "dashed") +
  geom_vline(data = ROPE, aes(xintercept = ROPE)) +
  ggtitle("Example of Equilvance Test")


posterior = rnorm(n = 10000, mean = .5, sd = .1)
ROPE = data.frame(ROPE = c(-.2, .2))
quantile_fig1 = quantile(posterior,probs = c(.025, .975))
quantile_fig1 = data.frame(quantile_fig1 = quantile_fig1)
posterior = data.frame(posterior)

plot_fig1 = ggplot(posterior, aes(x = posterior)) +
  geom_histogram() +
  geom_vline(data = quantile_fig1, aes(xintercept = quantile_fig1), linetype = "dashed") +
  geom_vline(data = ROPE, aes(xintercept = ROPE)) +
  ggtitle("Practical Significance Example")

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

post_prior = MCMCregress(y ~ intervention2 + intervention3,data = dat_bayes_power)

post_prior_summary = summary(post_prior)
cred_inter_2.5 = post_prior_summary$quantiles[2,c(1)]
cred_inter_97.5 = post_prior_summary$quantiles[2,c(5)]

### Now we want to get rid of really wide CI so we want everything this lower than -.2 on the lower 2.5 and is higher than .2 on the 97.5
cred_inter_check = ifelse(cred_inter_97.5 > .2 & cred_inter_2.5 < -.2, 1,0)

# This should world, because want to know whether the value is great .2 or less than -.2
cred_inter_2.5 = ifelse(cred_inter_2.5 > .2 |  cred_inter_2.5 < -.2, 1, 0)

# This should world, because want to know whether the value is great .2 or less than -.2
cred_inter_97.5 = ifelse(cred_inter_97.5 > .2 |  cred_inter_97.5 < -.2, 1, 0)
#
## This works, because the only way you can have two is if you have ones for both of the above.  Then if you get three, this means you also meet the criteria where you are above and below each threshold 
cred_inter = sum(cred_inter_2.5, cred_inter_97.5, cred_inter_check)
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
cred_inter_97.5 = post_prior_summary$quantiles[2,c(5)]

### Now we want to get rid of really wide CI so we want everything this lower than -.2 on the lower 2.5 and is higher than .2 on the 97.5
cred_inter_check = ifelse(cred_inter_97.5 > .2 & cred_inter_2.5 < -.2, 1,0)

# This should world, because want to know whether the value is great .2 or less than -.2
cred_inter_2.5 = ifelse(cred_inter_2.5 > .2 |  cred_inter_2.5 < -.2, 1, 0)

# This should world, because want to know whether the value is great .2 or less than -.2
cred_inter_97.5 = ifelse(cred_inter_97.5 > .2 |  cred_inter_97.5 < -.2, 1, 0)
#

cred_inter = sum(cred_inter_2.5, cred_inter_97.5, cred_inter_check)
cred_inter = ifelse(cred_inter == 2, 1,0)
cred_inter
}

reps = 1000
power = data.frame(replicate(reps, matt_predict_power()))
power = apply(power, 2, sum)/reps
power
```
Type one error rate for Power for treatment two versus treatment one
This is the observed type one error rate, because it is the number of times we see a significant slope for the effect of intervention two versus one if it were zero.  So only a few times should this be the case 
```{r}
matt_power = function(){
n = 109
intercept = -0.4215
treat2v1 = 0
treat3v1 = 0
intervention  = c(rep(1,round(n*42/109,0)), rep(2,round(n*38/109,0)), rep(3,round(n*(29)/109,0)))
length(intervention)

intervention1 = ifelse(intervention == 1, 1, 0)
intervention2 = ifelse(intervention == 2,1,0)
intervention3 = ifelse(intervention == 3, 1, 0)

y = intercept + intervention2*treat2v1 + intervention3*treat3v1 + rnorm(n =n, mean = 0, sd = 1)

dat_bayes_power = data.frame(y = y, intervention2, intervention3)


post_prior = MCMCregress(y ~ intervention2 + intervention3, data = dat_bayes_power)

post_prior_summary = summary(post_prior)


cred_inter_2.5 = post_prior_summary$quantiles[2,c(1)]
cred_inter_97.5 = post_prior_summary$quantiles[2,c(5)]

### Now we want to get rid of really wide CI so we want everything this lower than -.2 on the lower 2.5 and is higher than .2 on the 97.5
cred_inter_check = ifelse(cred_inter_97.5 > .2 & cred_inter_2.5 < -.2, 1,0)

# This should world, because want to know whether the value is great .2 or less than -.2
cred_inter_2.5 = ifelse(cred_inter_2.5 > .2 |  cred_inter_2.5 < -.2, 1, 0)

# This should world, because want to know whether the value is great .2 or less than -.2
cred_inter_97.5 = ifelse(cred_inter_97.5 > .2 |  cred_inter_97.5 < -.2, 1, 0)
#

cred_inter = sum(cred_inter_2.5, cred_inter_97.5, cred_inter_check)
cred_inter = ifelse(cred_inter == 2, 1,0)
cred_inter
}

reps = 1000
power = data.frame(replicate(reps, matt_power()))
power = apply(power, 2, sum)/reps
power
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

Graph for uninformed T1 v T2 and T1 v T3
Get ready for informed graphs
```{r}

post_treatment2 =  data.frame(post[,2])
names(post_treatment2)[1] = "posterior"

quatile_post_treatment2 = quantile(post_treatment2$posterior,probs = c(.025, .975))
quatile_post_treatment2 = data.frame(quatile = quatile_post_treatment2)
ROPE = data.frame(ROPE = c(-.2, .2))

post_treatment3 = data.frame(post[,3])
names(post_treatment3)[1] = "posterior"

quatile_post_treatment3 = quantile(post_treatment3$posterior,probs = c(.025, .975))
quatile_post_treatment3 = data.frame(quatile = quatile_post_treatment3)
ROPE = data.frame(ROPE = c(-.2, .2))
```
Posterior graph for informed T1 v T2 and T1 v T3
Now try to get graphs for the posterior
```{r}
library(gridExtra)

plot1 = ggplot(post_treatment2, aes(x = posterior)) +
  geom_histogram() +
  geom_vline(data = quatile_post_treatment2, aes(xintercept = quatile), linetype = "dashed") +
  geom_vline(data = ROPE, aes(xintercept = ROPE)) +
  ggtitle("Figure 3: Uninformed Posterior Program 1 v Program 2")
  
  plot2 = ggplot(post_treatment3, aes(x = posterior)) +
  geom_histogram() +
  geom_vline(data = quatile_post_treatment3, aes(xintercept = quatile), linetype = "dashed") +
  geom_vline(data = ROPE, aes(xintercept = ROPE))+
  ggtitle("Figure 4: Uninformed Posterior Program 1 v Program 3")

grid.arrange(plot1,plot2)
```



