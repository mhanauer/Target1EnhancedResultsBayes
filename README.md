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
#######Reviewing additionally missing data.  Some in pre not matched with post and why?


## Now merge everything
dim(datPreAdult)
dim(datPostAdult)
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

describe.factor(datAdult$Treatment)

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

dim(datAdultAnalysis)

dim(subset(datAdultAnalysis, Time == 0))
datAdultAnalysis_complete = na.omit(datAdultAnalysis)
library(prettyR)
base = subset(datAdultAnalysis, Time == 0)
describe.factor(base$Employment)
```
Create scaled versions and get rid of missing data
```{r}

datAdultAnalysis_complete$RASTotalScoreF1_scaled = scale(datAdultAnalysis_complete$RASTotalScoreF1)
datAdultAnalysis_complete$RASTotalScoreF2_scaled = scale(datAdultAnalysis_complete$RASTotalScoreF2)
datAdultAnalysis_complete$RASTotalScoreF3_scaled = scale(datAdultAnalysis_complete$RASTotalScoreF3)
datAdultAnalysis_complete$RASTotalScoreF5_scaled = scale(datAdultAnalysis_complete$RASTotalScoreF5)
datAdultAnalysis_complete$SSMITotalScore_scaled = scale(datAdultAnalysis_complete$SSMITotalScore)

```
Make data wide 
```{r}
datAdultAnalysis_complet_wide = data.frame(ID = datAdultAnalysis_complete$ID, Time = datAdultAnalysis_complete$Time, RASTotalScoreF5 = datAdultAnalysis_complete$RASTotalScoreF5, Treatment= datAdultAnalysis_complete$Treatment)
datAdultAnalysis_complete_wide = reshape(datAdultAnalysis_complet_wide, timevar = "Time",idvar = "ID", direction = "wide")
datAdultAnalysis_complete_wide = na.omit(datAdultAnalysis_complete_wide)
dim(datAdultAnalysis_complete_wide)
datAdultAnalysis_complete_wide$diff_score = datAdultAnalysis_complete_wide$RASTotalScoreF5.1-datAdultAnalysis_complete_wide$RASTotalScoreF5.0
head(datAdultAnalysis_complete_wide)
datAdultAnalysis_complete_wide[,c(2,4,5)] = NULL
head(datAdultAnalysis_complete_wide)
treatment = dummy.code(datAdultAnalysis_complete_wide$Treatment.0)
colnames(treatment) = c("phone", "phone_text", "phone_face")
datAdultAnalysis_complete_wide = data.frame(datAdultAnalysis_complete_wide, treatment)
datAdultAnalysis_complete_wide$Treatment.0 = NULL
head(datAdultAnalysis_complete_wide)
### Try first model then second model with prior set to that, move every 20 data points
dat_20_clients = datAdultAnalysis_complete_wide[1:20,]
dat_20_clients

dat_40_clients = datAdultAnalysis_complete_wide[1:40,]
dat_40_clients

dat_60_clients = datAdultAnalysis_complete_wide[1:60,]
dat_60_clients

dat_80_clients = datAdultAnalysis_complete_wide[1:80,]
dat_80_clients
```
Generate Bayes models with priors based on previous 20 data points
```{r}
library(rstanarm)
## First with 20
model_20_clients = stan_glm(diff_score ~ phone_text + phone_face, family = gaussian(link = "identity"), data = dat_20_clients)
summary(model_20_clients)
summary_model_20_clients =  summary(model_20_clients)
summary_model_20_clients


### Now try 40
my_prior_20 = student_t(df = 25, location = c(summary_model_20_clients[2,1], summary_model_20_clients[3,1]), scale = c(summary_model_20_clients[2,3], summary_model_20_clients[3,3]))
my_prior_20_intercept = student_t(df = 25, location = summary_model_20_clients[1,1], scale = summary_model_20_clients[1,3])
my_prior_error_20 = student_t(df = 25, location = summary_model_20_clients[4,1], scale = summary_model_20_clients[4,3])

model_40_clients = stan_glm(diff_score ~ phone_text + phone_face, prior = my_prior_20, prior_intercept = my_prior_20_intercept, prior_aux = my_prior_error_20, data = dat_40_clients)
summary_model_40_clients = summary(model_40_clients)
summary_model_40_clients

### Now try 60
my_prior_40 = student_t(df = 25, location = c(summary_model_40_clients[2,1], summary_model_40_clients[3,1]), scale = c(summary_model_40_clients[2,3], summary_model_40_clients[3,3]))
my_prior_40_intercept = student_t(df = 25, location = summary_model_40_clients[1,1], scale = summary_model_40_clients[1,3])
my_prior_error_40 = student_t(df = 25, location = summary_model_40_clients[4,1], scale = summary_model_40_clients[4,3])

model_60_clients = stan_glm(diff_score ~ phone_text + phone_face, prior = my_prior_40, prior_intercept = my_prior_40_intercept, prior_aux = my_prior_error_40, data = dat_60_clients)
summary_model_60_clients = summary(model_60_clients)
summary_model_60_clients

### Now try 80
my_prior_60 = student_t(df = 25, location = c(summary_model_60_clients[2,1], summary_model_60_clients[3,1]), scale = c(summary_model_60_clients[2,3], summary_model_60_clients[3,3]))
my_prior_60_intercept = student_t(df = 25, location = summary_model_60_clients[1,1], scale = summary_model_60_clients[1,3])
my_prior_error_60 = student_t(df = 25, location = summary_model_60_clients[4,1], scale = summary_model_60_clients[4,3])

model_80_clients = stan_glm(diff_score ~ phone_text + phone_face, prior = my_prior_60, prior_intercept = my_prior_60_intercept, prior_aux = my_prior_error_60, data = dat_80_clients)
summary_model_80_clients = summary(model_80_clients)
summary_model_80_clients

## Grab the mean, sd, 2.5 and 97.5
sum_bayes = rbind(summary_model_20_clients, summary_model_40_clients, summary_model_60_clients, summary_model_80_clients)
sum_bayes
sum_bayes = sum_bayes[,c(1,3,4,8)]
sum_bayes = sum_bayes[c(2:3, 8:9, 14:15, 20:21),]
sum_bayes = round(sum_bayes, 3)
colnames(sum_bayes) = c("Estimate", "Standard_Deviation", "Lower_95", "Upper_95")
rownames(sum_bayes) = c("phone_text_20", "phone_face_20", "phone_text_40", "phone_face_40", "phone_text_60", "phone_face_60", "phone_text_80", "phone_face_80")
sum_bayes = data.frame(sum_bayes)
sum_bayes
## Short if parm divide by sd is greater than 2 then sig double check
sum_bayes$Significant = ifelse(abs(sum_bayes$Estimate / sum_bayes$Standard_Deviation) > 2, "*", "")
write.csv(sum_bayes, "sum_bayes.csv")
```
Now run freq models
```{r}
model_20_clients_freq = lm(diff_score ~ phone_text + phone_face,data = dat_20_clients)
summary(model_20_clients_freq)
summary_model_20_clients_freq =  summary(model_20_clients_freq)
summary_model_20_clients_freq


model_40_clients_freq = lm(diff_score ~ phone_text + phone_face,data = dat_40_clients)
summary(model_40_clients_freq)
summary_model_40_clients_freq =  summary(model_40_clients_freq)
summary_model_40_clients_freq


model_60_clients_freq = lm(diff_score ~ phone_text + phone_face,data = dat_60_clients)
summary(model_60_clients_freq)
summary_model_60_clients_freq =  summary(model_60_clients_freq)
summary_model_60_clients_freq


model_80_clients_freq = lm(diff_score ~ phone_text + phone_face,data = dat_80_clients)
summary(model_80_clients_freq)
summary_model_80_clients_freq =  summary(model_80_clients_freq)
summary_model_80_clients_freq

sum_freq = rbind(summary_model_20_clients_freq$coefficients[2:3,c(1,2,4)], summary_model_40_clients_freq$coefficients[2:3,c(1,2,4)], summary_model_60_clients_freq$coefficients[2:3,c(1,2,4)], summary_model_80_clients_freq$coefficients[2:3,c(1,2,4)])

sum_freq = data.frame(round(sum_freq, 3))
colnames(sum_freq) = c("Estimate", "Standard_Error", "P_value")
rownames(sum_freq) = c("phone_text_20", "phone_face_20", "phone_text_40", "phone_face_40", "phone_text_60", "phone_face_60", "phone_text_80", "phone_face_80")
sum_freq
sum_freq$Significant = ifelse(sum_freq$P_value < .05/4, "*", "")
write.csv(sum_freq, "sum_freq.csv")
```



New plan:
Four models
1. Uniformed general change over time across all interventions
2. Informed that is spot on
3. Informed that is under zelous (procection factor)
4. Comparison for accepting not informed
5. Comparison for accepting informed (two programs should similar but one is way cheaper)

1. Uninformed prior
```{r}
library(brms)
brms_model_overall = brm(RASTotalScoreF5_scaled~ Time + (1|ID), data = datAdultAnalysis_complete)
summary(brms_model_overall)
prior_summary(brms_model_overall)
```
2. Correctly informed prior
```{r}
brms_model_overall_prior = brm(RASTotalScoreF5_scaled~ Time + (1|ID), data = datAdultAnalysis_complete, prior = set_prior("student_t(3,.4,.2)", class = "b", coef = "Time"))
summary(brms_model_overall_prior)

```

3. Protective prior
```{r}
brms_model_overall_small = brm(RASTotalScoreF5_scaled~ Time + (1|ID), data = datAdultAnalysis_complete, prior = set_prior("student_t(3,0,.2)", class = "b", coef = "Time"))
summary(brms_model_overall_small)

```
4. Comparison for accepting not informed
```{r}
datAdultAnalysis_complete$treat_accept = ifelse(datAdultAnalysis_complete$Treatment == 1, 2, ifelse(datAdultAnalysis_complete$Treatment == 2,1, datAdultAnalysis_complete$Treatment))

brms_model_treat = brm(RASTotalScoreF3_scaled~ Time*factor(treat_accept) + (1|ID), data = datAdultAnalysis_complete)
summary(brms_model_treat)
prior_summary(brms_model_treat)
```

5. Comparison for accepting not informed (comparing treatment two and three)
```{r}
datAdultAnalysis_complete$treat_accept = ifelse(datAdultAnalysis_complete$Treatment == 1, 2, ifelse(datAdultAnalysis_complete$Treatment == 2,1, datAdultAnalysis_complete$Treatment))

brms_model_treat_prior = brm(RASTotalScoreF3_scaled~ Time*factor(treat_accept) + (1|ID), data = datAdultAnalysis_complete, prior = set_prior("student_t(3,0,.2)", class = "b", coef = "Time:factortreat_accept3"))
summary(brms_model_treat_prior)

```
Try models with rstan package
```{r}
brms_model_null = brm(RASTotalScoreF5_scaled~ + (1|ID), data = datAdultAnalysis_complete)
library(rstanarm)
### Bayesian R^2
median(bayes_R2(brms_model_overall))
median(bayes_R2(brms_model_null))
autocorr(brms_model_overall)
### Track draws from the distribution and see if those draws are similar to actual value
pp_check(brms_model_overall)
pp_check(brms_model_null)


```


Diagnostics
```{r}
library(coda)
brms_model_overall_coda = as.mcmc(brms_model_overall)
brms_model_overall_coda[[1]]
brms_model_overall

brms_model_overall_coda

effectiveSize(brms_model_overall_coda)
brms_model_overall
autocorr.diag(brms_model_overall_coda)

# Not sure what this means
#raftery.diag(post_prior)
# Should be low this is a way to tell if we sampled all the possible spaces
rejectionRate(brms_model_overall_coda)
plot(brms_model_overall)


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
