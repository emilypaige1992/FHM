#FHM Analysis
#DH5
#09Aug2017
options(scipen = 0)

#Libraries
library(ggplot2)
library(MASS)

#read in the cleaned data
source("C:\\Users\\Emily\\Desktop\\DH5\\FHM Research\\1 - Data Manipulation.R")

#Logistic Regression - Currently Use MFP
analysis2 = analysis[,-2]


logit1 = glm(use_MFP ~ ., data=analysis2, family = "binomial")
summary(logit1)

logit1_OR = exp(cbind(OR = coef(logit1),confint(logit1)))

step1 <- stepAIC(logit1, direction="backward", K = 2.7)
step1$anova
summary(step1)
step1_OR = exp(cbind(OR = coef(step1),confint(step1)))


#Logistic Regression - Will Ever Use MFP
analysis3 = analysis[,-1]

logit2 = glm(everuse_MFP ~ ., data=analysis3, family = "binomial")
summary(logit2)
logit2_OR = exp(cbind(OR = coef(logit2),confint(logit2)))

step2 <- stepAIC(logit2, direction="backward", K = 2.7)
summary(step2)
step2_OR = exp(cbind(OR = coef(step2),confint(step)))


