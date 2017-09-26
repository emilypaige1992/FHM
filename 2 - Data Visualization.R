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
# don't use this
#hist(haiti$clean_inc)

#ggplot(data=haiti) + geom_bar(aes(haiti$clean_inc, fill=haiti$'C34=FP_effective'))+
#  theme_bw() + xlab("Income") + ylab("No standard monetary value yet") +
#  scale_fill_discrete(name="TFP Effective")

#ggplot(data=haiti, aes(x=haiti$'C34=FP_effective',y=haiti$clean_inc, fill=haiti$'C34=FP_effective')) +
#  geom_boxplot() + xlab("TFP Effective") + ylab("Income") + 
#  scale_fill_discrete(name="TFP \nEffective")


#===================       Age     ========================#
hist(haiti$`A1=age (years)`)


png("/Users/Kirsten/Desktop/dh5/Haiti_Contracept/github/FHM/figures/age_hist.png",width=5, height=5, units="in", res=300)
ggplot(data=haiti, aes(x=haiti$'A1=age (years)')) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.5,
                 colour="black", fill="white") + xlab("Age") +
  geom_density(alpha=.2, fill="#FF6666")
graphics.off()



table(haiti$`A2=phone`)

#===================      Phone     =======================#
# phone 1 = yes
haiti$`A2=phone`[haiti$`A2=phone` == 0] <- "No Phone"
haiti$`A2=phone`[haiti$`A2=phone` == 1] <- "Yes has Phone"

png("/Users/Kirsten/Desktop/dh5/Haiti_Contracept/github/FHM/figures/age_hist.png",width=5, height=5, units="in", res=300)

ggplot(data=haiti) + geom_bar(position="dodge", aes(haiti$`A2=phone`,fill=haiti$`C34=FP_effective`))+
  scale_fill_discrete(name="TFP Effective") + xlab("Phone")
graphics.off()



#===================    Education    ======================#
ed <- haiti$`A3=education_level`        ### seems like categories are messed up
ed[ed==0]<- "1Never Went to School"
ed[ed==1]<- "2Elementary (Grade 7-11)"
ed[ed==2] <- "3Middle (Grade 3-6)"
ed[ed==3] <- "4High School (Grade 0-2)"
ed[ed==4] <- "5Post-High School"


haiti$`A3=education_level` <- ed




ggplot(data=haiti) + geom_bar(position="dodge", aes(haiti$`A3=education_level`,fill=as.factor(haiti$use_MFP)))+
  scale_fill_discrete(name="Use MFP") + xlab("Education Level") 
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