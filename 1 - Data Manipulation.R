#FHM Analysis
#DH5
#19Jul2017

#import libraries
library(rJava)
library(readxl)

#read in the data
haiti = read_xlsx("C:/Users/Emily/Desktop/DH5/FHM Research/Contraception Data Analysis SRT 2015-16-Grand Challenges.xlsx",sheet = "Data Entry Sheet",
                   col_names = TRUE, range = "A1:EM711")


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
haiti$`C34=FP_effective`[haiti$`C34=FP_effective`=="NA"] = NA
haiti$`C34=FP_effective`[haiti$`C34=FP_effective`=="0"] = "No"
haiti$`C34=FP_effective`[haiti$`C34=FP_effective`=="1"] = "Yes"
haiti$`C34=FP_effective`[haiti$`C34=FP_effective`=="9"] = NA
Men = haiti[(haiti$gender == 0),]
haiti = haiti[(haiti$gender == 1),]
table(haiti$`C34=FP_effective`)
haiti$`A1=age (years)` #make numeric (fix stuff)
haiti <- haiti[haiti$`A1=age (years)`!="doesn't remember",]
haiti$`A1=age (years)`[haiti$`A1=age (years)`=="70-80"] <- "75" 
haiti$`A1=age (years)`[haiti$`A1=age (years)`=="somewhere between 36-40"] <- "38"
haiti$`A1=age (years)`[haiti$`A1=age (years)`=="over 60"] <- "60"
haiti$`A1=age (years)`<-as.numeric(haiti$`A1=age (years)`)
haiti <- haiti[haiti$`A1=age (years)`>=18,]
haiti <- haiti[haiti$`A1=age (years)`<=49,]
haiti$AgeBin <- ifelse(haiti$`A1=age (years)`<20,"18-19",
                        ifelse(haiti$`A1=age (years)`<30,"20-29",
                               ifelse(haiti$`A1=age (years)`<40,"30-39",
                                      "40-49")))
haiti$`A2=phone`[haiti$`A2=phone` == "9"] = NA
table(haiti$`A2=phone`)
haiti$`A3=education_level`
table(haiti$`A3=education_level`)

haiti[haiti == 9] = NA
# creating names for work status;
haiti$`A4=work_status`

table(haiti$`A4=work_status`)

cnames <- c("None", "Accountant", "Assistant", "Advocate", "Burser",
            "Cashier", "Clothes Maker", "Computer Specialist",
            "Constable", "Cook/Chef", "Pastry Chef", "Cosmetologist",
            "Farmer", "Florist", "Gardener", "Health Agent",
            "Housewife", "Laboratory Technician", "Laundry", "Management", "Messenger",
            "Mill Worker", "Nurse", "Nursing Aide", "Nutritionist", "Pharmacist", 
            "Receptionist", "Saleswoman", "Secretary", "Statistician", 
            "Student", "Teacher", "Videographer", "Waitress", "Librarian", "Typist", 
            "Musician", "Librarian", "filler1" , "filler2", "Actress", "Bank Technician",
            "Medical Technician", "Factory Operator", "Other", "Business", "Archivist",
            "Agronomist", "Marine Inspector", "Entry Operator", "Janitor/Cleaner", 
            "Journalist" ,"TV Presenter", "Hostess", 
            "Housekeeper", "Preacher", "Counselor", "Principal/Headmistress")


haiti$"A4.1=occupation"[haiti$"A4.1=occupation" == "18, 27"] = 18
haiti$"A4.1=occupation"[haiti$"A4.1=occupation" == "27, 1"] = 1
haiti$"A4.1=occupation"[haiti$"A4.1=occupation" == "27, 12"] = 12
haiti$"A4.1=occupation"[haiti$"A4.1=occupation" == "27, 30" | haiti$"A4.1=occupation" == "27,30"] = 30
haiti$"A4.1=occupation"[haiti$"A4.1=occupation" == "27,35"] = 35
haiti$"A4.1=occupation"[haiti$"A4.1=occupation" == "6,27"] = 6
haiti$"A4.1=occupation"[haiti$"A4.1=occupation" == "9 and 27"] = 9
haiti$"A4.1=occupation"[haiti$"A4.1=occupation" == "student"] = "Student"
haiti$"A4.1=occupation"[haiti$"A4.1=occupation" == "construction"] = "Construction"
haiti$"A4.1=occupation"[haiti$"A4.1=occupation" == "woodworker"] = "Woodworker"

for (i in 0:57){
  haiti$"A4.1=occupation"[haiti$"A4.1=occupation" == i] <- cnames[i+1]
}


haiti$occupation2 = haiti$"A4.1=occupation"
haiti$occupation2[haiti$"A4.1=occupation" %in% c("Accountant","Business","Journalist","Typist","Nurse","Teacher")] = "Technical"
haiti$occupation2[haiti$"A4.1=occupation" %in% c("Farmer","Housekeeper","Laundry","Clothes Maker","Woodworker","Construction", "Cook/Chef","Cosmetologist")] = "Trade"
haiti$occupation2[is.na(haiti$`A4.1=occupation`)] = NA
haiti$occupation2[haiti$`A4.1=occupation`=="None" | haiti$`A4=work_status` == "0"] = "No Job"


haiti$job = NA
haiti$job[haiti$`A4=work_status` == 0] = 0
haiti$job[haiti$occupation2 != "None"] = 1

table(haiti$`A5=income`) #character - change unsure to 9
haiti$`A7=partner`
haiti$`A8=sexually_active`
haiti$`A9.1=number_kids`
haiti$`A9.1=number_kids`[(haiti$`A9.1=number_kids`=="pregnant") | 
                           (haiti$`A9.1=number_kids`=="6 month pregnant baby")] = "1"
haiti$`A9.1=number_kids`[(haiti$`A9.1=number_kids`=="4 living, 11 deceased")] = "15"
haiti$`A9.1=number_kids` = as.numeric(haiti$`A9.1=number_kids`)
haiti$`A10=more_kids` #important
haiti$`A11=decides_number_kids`
table(haiti$`A11.1=decides__kids_specify`) #needs SERIOUS WORK
haiti$decide_kids = ifelse(haiti$`A11=decides_number_kids`==8,haiti$`A11.1=decides__kids_specify`,
                           haiti$`A11=decides_number_kids`)
x = haiti$decide_kids
x[x==0] = "Myself"
x[x==1] = "Partner"
x[x==2] = "Family"
x[x==3] = "Religion"
x[x==4] = "Community"
x[x==5] = "Couple"
x[x==6] = "Fate"
x[x==7] = "God"
x[x==9] = NA
x[x=="doctor"] = "Doctor"
x[x=="none/couldn't get pregnant" | 
    x=="can't have kids because she had to have her cervix removed" | 
    x=="had fertility issues" | 
    x=="her health"] = "Health"
x[x=="herself and god" | x=="myself and god"] = "Myself & God"
x[x=="doesn't have kids"] = NA
haiti$decide_kids = x

haiti$`A12=plan_kids`[haiti$`A12=plan_kids` == "9"] = NA
haiti$`A12=plan_kids`[haiti$`A12=plan_kids` == "NA"] = NA

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
haiti$`D42=ever_use_FP`[is.na(haiti$`D42=ever_use_FP`)]="NA"
haiti$`D42=ever_use_FP`[haiti$`D42=ever_use_FP`=="0"]="No"
haiti$`D42=ever_use_FP`[haiti$`D42=ever_use_FP`=="1"]="Yes"
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
                haiti$`D43a.1=which_FP` == "7" |
                haiti$`D43a.1.1=which_FP_other` == "4"] = 1
                #haiti$`D43a.1=which_FP` == "8" |
haiti$use_MFP[haiti$`D43a.1=which_FP` == "11" |
                haiti$`D43a.1=which_FP` == "0" |
                haiti$`D43a.1=which_FP` == "9" |
                haiti$`D42=ever_use_FP` == "No"] = 0  #used Q42 to fill in those who don't use MFP
table(haiti$use_MFP)

#Outcome "will ever use MFP"
table(haiti$`D44=everuse_MFP`) 
haiti$everuse_MFP = NA
haiti$everuse_MFP[haiti$`D44=everuse_MFP` == "0"] = 0
haiti$everuse_MFP[haiti$`D44=everuse_MFP` == "1"] = 1
haiti$everuse_MFP[haiti$`D44=everuse_MFP` == "9"] = NA
haiti$everuse_MFP[haiti$`D44=everuse_MFP` == "maybe"] = NA

#impute missing will ever use with current MFP users
haiti$everuse_MFP[haiti$use_MFP == 1] = 1


#create analysis data set
analysis = haiti[,c("use_MFP", "everuse_MFP","C34=FP_effective","AgeBin","A2=phone","A3=education_level","job", 
                    "A7=partner","A8=sexually_active","A9=children",
                    "A10=more_kids","A12=plan_kids")]
analysis = analysis[complete.cases(analysis),]

