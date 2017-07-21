#FHM Analysis
#Emily Perry
#19Jul2017

#import libraries
library(rJava)

#read in the data
FP = read_xlsx("C:/Users/Emily/Desktop/FHM Research/Contraception Data Analysis SRT 2015-16-Grand Challenges.xlsx",sheet = "Population-Table-Ever Used FP",
                   col_names = TRUE, range = "A1:EM498")
nomorekids = read_xlsx("C:/Users/Emily/Desktop/FHM Research/Contraception Data Analysis SRT 2015-16-Grand Challenges.xlsx",sheet = "Population-Women-No More Kids",
               col_names = TRUE, range = "A1:EM390")
nokids = read_xlsx("C:/Users/Emily/Desktop/FHM Research/Contraception Data Analysis SRT 2015-16-Grand Challenges.xlsx",sheet = "Population-Women-Nulliparous",
                       col_names = TRUE, range = "A1:EM147")
full = read_xlsx("C:/Users/Emily/Desktop/FHM Research/Contraception Data Analysis SRT 2015-16-Grand Challenges.xlsx",sheet = "Data with Age Coded",
                   col_names = TRUE, range = "A1:EM711")

#add dataset flag
FP$FPset = "Have Ever Used FP"
FP$set = NA
nomorekids$FPset = NA
nomorekids$set = "No More Chlidren"
nokids$FPset = NA
nokids$set = "No Children"


temp = rbind(FP,nomorekids,nokids)

haiti = temp[match(unique(temp$`survey #`),temp$'survey #'),]

#check missingness for belief in 
#traditional family planning
table(haiti$`C34=FP_effective`)
table(haiti$`C36=TFP_vs_MFP`)
#large amount of missing for TFP vs. MFP

#add flag for TFP as or more effective
haiti$TFP_asormore_effective = 0
haiti$TFP_asormore_effective[haiti$`C36=TFP_vs_MFP` <= 1] = 1


#potential covariates of interest:
#gender, age, education, work status
#income, partner, sexually active,
#children, # children, FP important,
#FP prevent, 
#familiar/know how to use any modern method
#(create binary variable from Q21)
#side effects MFP,  Q31: religion allow FP,
#FP affordable, FP convenient, 
#***TFP effective***, 


#Schatzi covariates
#main interest: TFP methods effective
#socio-economic status - education, employment status, etc.
#prior pregnancy, wanting no more children,
#never had children.

#Questions of interest (for us
haiti$`C34=FP_effective`
haiti$gender
haiti$`A1=age (years)`
haiti$`A2=phone`
haiti$`A3=education_level`
haiti$`A4=work_status`
haiti$`A5=income` #character
haiti$`A7=partner`
haiti$`A8=sexually_active`
haiti$`A9.1=number_kids`
#recode pregant to 1s and
haiti$`A9.1=number_kids`[(haiti$`A9.1=number_kids`=="pregnant") | 
                           (haiti$`A9.1=number_kids`=="6 month pregnant baby")] = "1"
haiti$`A9.1=number_kids`[(haiti$`A9.1=number_kids`=="4 living, 11 deceased")] = "15"
haiti$`A9.1=number_kids` = as.numeric(haiti$`A9.1=number_kids`)
haiti$`A10=more_kids`#important
haiti$`A11=decides_number_kids`
haiti$`A11.1=decides__kids_specify` #needs SERIOUS WORK
haiti$`A12=plan_kids`
haiti$`B13=aware_FP`
#if no, skip to question #21
#Q21 - could be used for flag of people who only know about surgery modern FP 
haiti$`B22=know_side_effects`
haiti$`B24=traditional_FP`



