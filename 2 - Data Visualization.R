#FHM Analysis
#DH5
#09Aug2017

#Libraries
library(ggplot2)

#read in the cleaned data
source("C:\\Users\\Emily\\Desktop\\DH5\\FHM Research\\1 - Data Manipulation.R")


#Visualizations
haiti$`A11=decides_number_kids`
table(haiti$`A4.1=occupation`)


#=======    Q11 who decides how many children you have?   =======#
ggplot(data=haiti) + geom_bar(position = "dodge", aes(haiti$decide_kids, fill = haiti$`C34=FP_effective`)) + 
                     scale_fill_manual(values=c("Yes"="blue","No"="black","Don't Know"="darkgrey"), name = "TFP Effective") + 
  theme_bw() + xlab("Who Decides How Many Children You Have?") + ylab("Count")
  
ggplot(data=haiti) + geom_bar(position = "dodge", aes(haiti$decide_kids, fill = haiti$`D42=ever_use_FP`)) + 
  scale_fill_manual(values=c("Yes"="blue","No"="black","NA"="darkgray"), name = "Ever Used MFP") + 
  theme_bw() + xlab("Who Decides How Many Children You Have?") + ylab("Count")



#===================    Occupation   ======================#
ggplot(data=haiti) + geom_bar(position = "dodge", aes(haiti$occupation2, fill = haiti$`C34=FP_effective`)) + 
  scale_fill_manual(values=c("Yes"="blue","No"="black","Don't Know"="darkgrey"), name = "TFP Effective") + 
  theme_bw() + xlab("Who Decides How Many Children You Have?") + ylab("Count")



#===================      Income     ======================#
ggplot(data=haiti) + geom_bar(aes(haiti$clean_inc, fill=haiti$'C34=FP_effective'))+
  theme_bw() + xlab("Income") + ylab("No standard monetary value yet") +
  scale_fill_discrete(name="TFP Effective")

ggplot(data=haiti, aes(x=haiti$'C34=FP_effective',y=haiti$clean_inc, fill=haiti$'C34=FP_effective')) +
  geom_boxplot() + xlab("TFP Effective") + ylab("Income") + 
  scale_fill_discrete(name="TFP \nEffective")


#===================       Age     ========================#
hist(haiti$`A1=age (years)`)



#===================      Phone     =======================#
# phone 1 = yes
haiti$`A2=phone`[haiti$`A2=phone` == 0] <- "No Phone"
haiti$`A2=phone`[haiti$`A2=phone` == 1] <- "Yes has Phone"
haiti$`A2=phone`[haiti$`A2=phone` == 9] <- "NA"


ggplot(data=haiti) + geom_bar(position="dodge", aes(haiti$`A2=phone`,fill=haiti$`C34=FP_effective`))+
  scale_fill_discrete(name="TFP Effective") + xlab("Phone")



#===================    Education    ======================#
ggplot(data=haiti) + geom_bar(position="dodge", aes(haiti$`A3=education_level`,fill=haiti$`C34=FP_effective`))+
  scale_fill_discrete(name="TFP Effective") + xlab("Education Level") +
  scale_x_discrete(labels=c("Never Went\nto School","Elementary\nGrade7-11","Middle\nGrade3-6","High School\nGrade0-2","Post-High School"))



#===================    Work Status    ======================#
job <- haiti$`A4=work_status`
job[job==0] <- "Not Employed"
job[job==1] <- "Salary"
job[job==2] <- "Unpaid"
job[job==8] <- "Other"

haiti$`A4=work_status` <- job

ggplot(data=haiti) + geom_bar(position="dodge", aes(haiti$`A4=work_status`,fill=haiti$`C34=FP_effective`))+
  scale_fill_discrete(name="TFP Effective") + xlab("Work Status")


#===================    Partner    ======================#
ggplot(data=haiti) + geom_bar(position="dodge", aes(as.factor(haiti$`A7=partner`),fill=haiti$`C34=FP_effective`))+
  scale_fill_discrete(name="TFP Effective") + xlab("Partner")


#================    Sexually Active   ==================#
ggplot(data=haiti) + geom_bar(position="dodge", aes(as.factor(haiti$`A8=sexually_active`),fill=haiti$`C34=FP_effective`))+
  scale_fill_discrete(name="TFP Effective") + xlab("Sexually Active")


#================    Number of Children   ==================#
numk <- haiti$`A9.1=number_kids`
numk[numk=="6 month pregnant baby"] <- NA
numk[numk=="pregnant"] <- NA
haiti$`A9.1=number_kids` <- numk

haiti$`A9.1=number_kids` <- as.numeric(haiti$`A9.1=number_kids`)


ggplot(data=haiti) + geom_bar(position="dodge",aes(haiti$`A9.1=number_kids`, fill=haiti$'C34=FP_effective'))+
  theme_bw() + xlab("Number of Kids") + ylab("Count") +
  scale_fill_discrete(name="TFP Effective")

ggplot(data=haiti, aes(x=haiti$'C34=FP_effective',y=haiti$`A9.1=number_kids`, fill=haiti$'C34=FP_effective')) +
  geom_boxplot() + xlab("TFP Effective") + ylab("Number of Kids") + 
  scale_fill_discrete(name="TFP \nEffective")


#theme(legend.position = "bottom", panel.border = element_blank(), 
#                                                        panel.grid.major = element_blank(),
#                                                        panel.grid.minor = element_blank(),
#                                                        axis.line = element_line(colour = "black"))