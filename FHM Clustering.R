# clustering for demographic information 

# variables: 34, age_bin, A2, A3, A4, A7, A8, A9, A10, A12, use_mfp (outcome)

# libraries
library(FactoMineR)

# subset dataset based on variables of interest
demo <- haiti[complete.cases(haiti$use_MFP),c("C34=FP_effective", "AgeBin", "A2=phone",
                  "A3=education_level", "A4=work_status",
                  "A7=partner", "A8=sexually_active",
                  "A9=children", "A10=more_kids",
                  "A12=plan_kids", "use_MFP")]

# turn all columns into factors
demo <- data.frame(lapply(demo, factor))

# multiple correspondence analysis
res.mca <- MCA(X = demo)
