#FHM Analysis
#DH5
#09Aug2017
options(scipen = 0)

#Libraries
library(ggplot2)
library(MASS)

#read in the cleaned data
source("C:\\Users\\Emily\\Desktop\\DH5\\FHM Research\\1 - Data Manipulation.R")

#========== demographics ============#
library("stringr")

#seperate datasets by MFP
MFP = analysis[analysis$use_MFP == 1,]
noMFP = analysis[analysis$use_MFP == 0,]


numkids <- MFP$`A9.1=number_kids`
MFP_nkid <- paste(round(mean(numkids),1),"(",round(sd(numkids),2),")")

noMFPnk <- noMFP$`A9.1=number_kids`
noMFP_nkid <- paste(round(mean(noMFPnk),1),"(",round(sd(noMFPnk),2),")")



#making MFP demographics column
anames <- names(analysis)[3:12]

demo <- list()
for(i in 1:length(anames)){
  mfp <- MFP[,i+2]
  t <- table(mfp)
  pt <- round((prop.table(table(mfp))*100),2)
  result <- paste(t,"(",trimws(pt),"%)")      #get results xx(x.xx%)
  out <-data.frame(names(pt),result)
  names(out) <- c(as.character(names(MFP[,i+2])),"MFP Result")  #output into list
  
  demo[[i]] <- out
}


# making no MFP demographics column
demo_noMFP <- list()
for(i in 1:length(anames)){
  nomfp <- noMFP[,i+2]
  t <- table(nomfp)
  pt <- round((prop.table(table(nomfp))*100),2)
  result <- paste(t,"(",trimws(pt),"%)")      #get results xx(x.xx%)
  out <-data.frame(names(pt),result)
  names(out) <- c(as.character(names(noMFP[,i+2])),"MFP Result")  #output into list
  
  demo_noMFP[[i]] <- out
}





#===========  Logistic Regression - Currently Use MFP ============#
#delete everuse MFP variable
analysis2 <- analysis[, -which(names(analysis) == "everuse_MFP")]

analysis2$`A3=education_level` = factor(analysis2$`A3=education_level`)

logit1 = glm(use_MFP ~ ., data=analysis2, family = "binomial")
summary(logit1)

logit1_OR = exp(cbind(OR = coef(logit1),confint(logit1)))
logit1_OR

#reduced
step1 <- stepAIC(logit1, direction="backward", K = 2.7)
step1$anova
summary(step1)
step1_OR = exp(cbind(OR = coef(step1),confint(step1)))
step1_OR



#===========   Logistic Regression - Will Ever Use MFP ============#
#delete currently use MFP variable
analysis3 <- analysis[, -which(names(analysis) == "use_MFP")]

analysis3$`A3=education_level` = factor(analysis3$`A3=education_level`)

logit2 = glm(everuse_MFP ~ ., data=analysis3, family = "binomial")
summary(logit2)
logit2_OR = exp(cbind(OR = coef(logit2),confint(logit2)))
logit2_OR

#reduced
step2 <- stepAIC(logit2, direction="backward", K = 2.7)
summary(step2)
step2_OR = exp(cbind(OR = coef(step2),confint(step2)))
step2_OR

#####################################################
# LASSO
#####################################################
