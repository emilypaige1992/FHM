#FHM Analysis
#DH5
#09Aug2017
options(scipen = 0)

#Libraries
library(ggplot2)
library(MASS)

#read in the cleaned data
source("C:\\Users\\Emily\\Desktop\\DH5\\FHM Research\\1 - Data Manipulation.R")
analysis$`A3=education_level` = factor(analysis$`A3=education_level`)

#demographics
#seperate datasets by MFP
MFP = analysis[analysis$use_MFP == 1,]
noMFP = analysis[analysis$use_MFP == 0,]
#TFP Effective
var = MFP$`C34=FP_effective`
table(var)
prop.table(table(var))
var = noMFP$`C34=FP_effective`
table(var)
prop.table(table(var))
#Age
var = MFP$AgeBin
table(var)
prop.table(table(var))
var = noMFP$AgeBin
table(var)
prop.table(table(var))
#Cellphone
var = MFP$`A2=phone`
table(var)
prop.table(table(var))
var = noMFP$`A2=phone`
table(var)
prop.table(table(var))
#Education
var = MFP$`A3=education_level`
table(var)
prop.table(table(var))
var = noMFP$`A3=education_level`
table(var)
prop.table(table(var))
#Employed
var = MFP$job
table(var)
prop.table(table(var))
var = noMFP$job
table(var)
prop.table(table(var))
#Live with Partner
var = MFP$`A7=partner`
table(var)
prop.table(table(var))
var = noMFP$`A7=partner`
table(var)
prop.table(table(var))
#Sexually Active
var = MFP$`A8=sexually_active`
table(var)
prop.table(table(var))
var = noMFP$`A8=sexually_active`
table(var)
prop.table(table(var))
#Number of Kids
var = MFP$`A9=children`
table(var)
prop.table(table(var))
var = noMFP$`A9=children`
table(var)
prop.table(table(var))
#More Kids
var = MFP$`A10=more_kids`
table(var)
prop.table(table(var))
var = noMFP$`A10=more_kids`
table(var)
prop.table(table(var))
#Plan for Kids
var = MFP$`A12=plan_kids`
table(var)
prop.table(table(var))
var = noMFP$`A12=plan_kids`
table(var)
prop.table(table(var))

#Logistic Regression - Currently Use MFP
analysis2 = analysis[,-2]
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
analysis3 = analysis[,-1]
analysis3$`A3=education_level` = factor(analysis3$`A3=education_level`)

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
