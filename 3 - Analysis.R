#FHM Analysis
#DH5
#09Aug2017
options(scipen = 0)

#Libraries
library(ggplot2)
library(MASS)
library("glmnet")
library("nnet")


#read in the cleaned data
source("C:\\Users\\Emily\\Desktop\\DH5\\FHM Research\\1 - Data Manipulation.R")

#Logistic Regression - Currently Use MFP
myvars <- names(analysis) %in% c("everuse_MFP")
analysis2 <- analysis[!myvars]


analysis2$`A3=education_level` = factor(analysis2$`A3=education_level`)

logit1 = glm(use_MFP ~ ., data=analysis2, family = "binomial")
summary(logit1)

logit1_OR = exp(cbind(OR = coef(logit1),confint(logit1)))

#reduced
step1 <- stepAIC(logit1, direction="backward", K = 2.7)
step1$anova
summary(step1)
step1_OR = exp(cbind(OR = coef(step1),confint(step1)))


#Logistic Regression - Will Ever Use MFP

myvars3 <- names(analysis) %in% c("use_MFP")
analysis3 <- analysis[!myvars3]

logit2 = glm(everuse_MFP ~ ., data=analysis3, family = "binomial")
summary(logit2)
logit2_OR = exp(cbind(OR = coef(logit2),confint(logit2)))

#reduced
step2 <- stepAIC(logit2, direction="backward", K = 2.7)
summary(step2)
step2_OR = exp(cbind(OR = coef(step2),confint(step2)))


#####################################################
# LASSO
#####################################################


myvars2 <- names(analysis2) %in% c("use_MFP")
covars <- analysis2[!myvars2]

newc <- covars[-c(1,2,10)]

analysis2$`C34=FP_effective` <- ifelse(analysis2$`C34=FP_effective` == "No" , 0, 1)
analysis2$`C34=FP_effective`<- factor(analysis2$`C34=FP_effective`, levels = c(0,1), labels = c("No", "Yes"))

h <- factor(analysis2$`C34=FP_effective`)

d <- data.frame(analysis2$`C34=FP_effective`, test)

set.seed(235)
lassop <- glmnet(x=as.matrix(newc),y=analysis2$"use_MFP",
                    family = "binomial",alpha=1)
summary(lassop)
plot(lassop)

# Lasso
set.seed(123)
lassopath <- glmnet(x=as.matrix(covars),y=outcome,
                    family = "binomial",alpha=1)
summary(lassopath)
plot(lassopath, font=2, font.lab=2, main="LASSOpath")


