#FHM Analysis
#DH5
#09Aug2017
options(scipen = 0)

#Libraries
library(ggplot2)
library(MASS)

#read in the cleaned data
source("C:\\Users\\Emily\\Desktop\\DH5\\FHM Research\\1 - Data Manipulation.R")
source("C:\\Users\\Emily\\Desktop\\DH5\\FHM Research\\2 - Data Visualization.R")
#Logistic Regression - Currently Use FP
logit1 = glm(use_MFP ~ haiti$`C34=FP_effective` + AgeBin + haiti$`A2=phone` + haiti$`A3=education_level` + 
               haiti$`A4=work_status`+ 
               haiti$`A7=partner` + haiti$`A8=sexually_active` + haiti$`A9=children`+
               haiti$`A9.1=number_kids`, data=haiti, family = "binomial")
summary(logit1)

logit2 = glm(haiti$use_MFP ~ `C34=FP_effective` + AgeBin + `A2=phone` + `A3=education_level` + 
               `A4=work_status` + `A7=partner` + `A8=sexually_active` + 
               `A9=children` + `A10=more_kids` + `A12=plan_kids` + `C34=FP_effective`:`A2=phone` + 
               `C34=FP_effective`:`A3=education_level` + `C34=FP_effective`:`A4=work_status` + 
               `C34=FP_effective`:`A12=plan_kids` + AgeBin:`A2=phone` + 
               AgeBin:`A3=education_level` + AgeBin:`A8=sexually_active` + 
               `A2=phone`:`A3=education_level` + `A2=phone`:`A4=work_status` + 
               `A2=phone`:`A7=partner` + `A2=phone`:`A8=sexually_active` + 
               `A2=phone`:`A9=children` + `A2=phone`:`A10=more_kids` + `A2=phone`:`A12=plan_kids` + 
               `A3=education_level`:`A9=children` + `A7=partner`:`A8=sexually_active` + 
               `A8=sexually_active`:`A10=more_kids` + `A8=sexually_active`:`A12=plan_kids`, data=haiti, family = "binomial")
summary(logit2)


haiti2 = haiti[,c("use_MFP", "C34=FP_effective","AgeBin","A2=phone","A3=education_level","A4=work_status", 
                   "A7=partner","A8=sexually_active","A9=children",
                   "A10=more_kids","A12=plan_kids")]

logit3 = glm(haiti2$use_MFP ~ .*., data=haiti2, family = "binomial")

step <- stepAIC(logit3, direction="both")
step$anova
summary(step)




