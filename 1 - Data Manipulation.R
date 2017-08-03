#FHM Analysis
#Emily Perry
#19Jul2017

#import libraries
library(rJava)
library(readxl)
#read in the data
FP = read_xlsx("C:/Users/Emily/Desktop/DH5/FHM Research/Contraception Data Analysis SRT 2015-16-Grand Challenges.xlsx",sheet = "Population-Table-Ever Used FP",
                   col_names = TRUE, range = "A1:EM498")
nomorekids = read_xlsx("C:/Users/Emily/Desktop/DH5/FHM Research/Contraception Data Analysis SRT 2015-16-Grand Challenges.xlsx",sheet = "Population-Women-No More Kids",
               col_names = TRUE, range = "A1:EM390")
nokids = read_xlsx("C:/Users/Emily/Desktop/DH5/FHM Research/Contraception Data Analysis SRT 2015-16-Grand Challenges.xlsx",sheet = "Population-Women-Nulliparous",
                       col_names = TRUE, range = "A1:EM147")
full = read_xlsx("C:/Users/Emily/Desktop/DH5/FHM Research/Contraception Data Analysis SRT 2015-16-Grand Challenges.xlsx",sheet = "Data Entry Sheet",
                   col_names = TRUE, range = "A1:EM711")

haiti = full

#add dataset flag
#FP$FPset = "Have Ever Used FP"
#FP$set = NA
#nomorekids$FPset = NA
#nomorekids$set = "No More Chlidren"
#nokids$FPset = NA
#nokids$set = "No Children"


#temp = rbind(FP,nomorekids,nokids)

#haiti = temp[match(unique(temp$`survey #`),temp$'survey #'),]

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

##########################
#covariates of interest
##########################
#Schatzi covariates
#main interest: TFP methods effective
#socio-economic status - education, employment status, etc.
#prior pregnancy, wanting no more children,
#never had children.

#Questions of interest (for us)
haiti$`C34=FP_effective`
haiti$gender
haiti$`A1=age (years)` #make numeric (fix stuff)
haiti$`A2=phone`
haiti$`A3=education_level`
haiti$`A4=work_status`
table(haiti$`A5=income`) #character - change unsure to 9
haiti$`A7=partner`
haiti$`A8=sexually_active`
haiti$`A9.1=number_kids`
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
haiti$`C25=spacing`
haiti$`C26=no_protection`
haiti$`C27=unwanted_pregnancy_bad` #recode 0,1 to 9
haiti$`C28=FP_befort_1st_kid`
haiti$`C31=allow_FP` #break up into two variables - religion allow, community allow
haiti$`C32=FP_expensive`
haiti$`C32=FP_expensive`[haiti$`C32=FP_expensive` == "1 (is cheap)" ] = "1"
haiti$`C32=FP_expensive` = as.numeric(haiti$`C32=FP_expensive`)
haiti$`C33=FP_accessible`
haiti$`C34=FP_effective` #subset to 1's
haiti$`C35=TFP_continue_ineff`
haiti$`C36=TFP_vs_MFP`
haiti$`C38.1=FP_hidden` #would hide = no, would not hide = yes
haiti$`C38.2=FP_surgery` #it depends category for weird ones
haiti$`D41=sexual_debut` #categorize??
#haiti$`D43a.4.1=what_side_effect`
#haiti$`D43a.4.2=other_method`#hmmmmmm....
#haiti$`D43a.5.1=self_access_how`

table(haiti$`D43a.5.1=self_access_how`) 
table(haiti$`D45=why_not`)

##########################
#outcomes of interest
##########################
haiti$`D42=ever_use_FP`
table(haiti$`D42=ever_use_FP`)

table(haiti$`D43a.1=which_FP`)
table(haiti$`D43a.1.1=which_FP_other`) #ask if traditional
#create outcome for "currently use MFP"
haiti$use_MFP = NA
haiti$use_MFP[haiti$`D43a.1=which_FP` == "0,1" |
                haiti$`D43a.1=which_FP` == "1" |
                haiti$`D43a.1=which_FP` == "1 and 4" |
                haiti$`D43a.1=which_FP` == "1,4" |
                haiti$`D43a.1=which_FP` == "10" |
                haiti$`D43a.1=which_FP` == "2" |
                haiti$`D43a.1=which_FP` == "3" |
                haiti$`D43a.1=which_FP` == "3,4" | 
                haiti$`D43a.1=which_FP` == "4" |
                haiti$`D43a.1=which_FP` == "5" |
                haiti$`D43a.1=which_FP` == "6" |
                haiti$`D43a.1=which_FP` == "7" ] = 1
                #haiti$`D43a.1=which_FP` == "8" |
haiti$use_MFP[haiti$`D43a.1=which_FP` == "11" |
                haiti$`D43a.1=which_FP` == "0" |
                haiti$`D43a.1=which_FP` == "9" |
                haiti$`D42=ever_use_FP` == 0] = 0  #used Q42 to fill in those who don't use MFP
table(haiti$use_MFP)

#Is using a calendar considered TFP??? 
#Need to discuss recoding "Other" fill in answers
table(haiti$`D43a.1=which_FP`)
table(haiti$`D43a.1.1=which_FP_other`)


#logistic regression model
#logit = glm(use_MFP ~ , data=haiti, family = "binomial")


