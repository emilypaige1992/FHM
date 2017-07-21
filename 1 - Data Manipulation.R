#FHM Analysis
#Emily Perry
#19Jul2017

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
FP$set = "Have Ever Used FP"
nomorekids$set = "No More Chlidren"
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
#
