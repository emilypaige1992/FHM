# Treemap

# Library
library(treemap)

subs <- haiti[,c("A4.1=occupation","C34=FP_effective","D42=ever_use_FP")]
subs$FP_eff <- ifelse(subs$`C34=FP_effective`=='Yes',1,
                       ifelse(subs$`C34=FP_effective`=='No',0,NA))
subs$FP_use <- ifelse(subs$`D42=ever_use_FP`=='Yes',1,
                        ifelse(subs$`D42=ever_use_FP`=='No',0,NA))

subs$`C34=FP_effective`<-NULL
subs$`D42=ever_use_FP` <- NULL

a <- aggregate(subs$FP_eff, list(subs$`A4.1=occupation`), FUN=mean, na.rm=T)
b <- aggregate(subs$FP_use, list(subs$`A4.1=occupation`), FUN=mean, na.rm=T)
a$use <- b$x
a$count<-as.data.frame(table(subs$`A4.1=occupation`))$Freq
colnames(a)[1:2] <- c('Occupations','eff')

treemap(a,
        index="Occupations",
        vSize="count",
        vColor="eff",
        type="value",
        palette="RdYlBu",
        fontsize.labels=7,
        title='% Believe Effectiveness in \n Traditional FP by Occupation')

treemap(a,
        index="Occupations",
        vSize="count",
        vColor="use",
        type="value",
        palette="RdYlGn",
        fontsize.labels=7,
        title='% Use of Modern FP by Occupation')
