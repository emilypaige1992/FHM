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



logit1 = glm(use_MFP ~ ., data=analysis, family = "binomial")
summary(logit1)

step1 <- stepAIC(logit1, direction="backward", K = 2.7)
step1$anova
summary(step1)


#Logistic Regression - Will Ever Use MFP

logit2 = glm(everuse_MFP ~ ., data=analysis, family = "binomial")
summary(logit2)

step2 <- stepAIC(logit2, direction="backward", K = 2.7)
step2$anova
summary(step2)


