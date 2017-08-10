#FHM Analysis
#DH5
#09Aug2017

#Input Data
library(ggplot2)

#read in the cleaned data
source("C:\\Users\\Emily\\Desktop\\DH5\\FHM Research\\1 - Data Manipulation.R")


#Visualizations
haiti$`A11=decides_number_kids`
table(haiti$`A4.1=occupation`)

#Q11: Who Decides How Many Children You Have?
ggplot(data=haiti) + geom_bar(position = "dodge", aes(haiti$decide_kids, fill = haiti$`C34=FP_effective`)) + 
                     scale_fill_manual(values=c("Yes"="blue","No"="black","Don't Know"="darkgrey"), name = "TFP Effective") + 
  theme_bw() + xlab("Who Decides How Many Children You Have?") + ylab("Count")
  
ggplot(data=haiti) + geom_bar(position = "dodge", aes(haiti$decide_kids, fill = haiti$`D42=ever_use_FP`)) + 
  scale_fill_manual(values=c("Yes"="blue","No"="black","NA"="darkgray"), name = "Ever Used MFP") + 
  theme_bw() + xlab("Who Decides How Many Children You Have?") + ylab("Count")

#Occupations

ggplot(data=haiti) + geom_bar(position = "dodge", aes(haiti$occupation2, fill = haiti$`C34=FP_effective`)) + 
  scale_fill_manual(values=c("Yes"="blue","No"="black","Don't Know"="darkgrey"), name = "TFP Effective") + 
  theme_bw() + xlab("Who Decides How Many Children You Have?") + ylab("Count")


#theme(legend.position = "bottom", panel.border = element_blank(), 
#                                                        panel.grid.major = element_blank(),
#                                                        panel.grid.minor = element_blank(),
#                                                        axis.line = element_line(colour = "black"))