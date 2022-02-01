library(survey)
library(splines)
library(tidyverse)

#### Prepare the survey objects ####
load('phqbyyear_allenrolled_blitimputed_weighted.RData')
pheno.all <- pheno.total.i[which(pheno.total.i$followup==1),]
pheno.all <- within(pheno.all,{
  dPHQ.s <- scale(dPHQ)
  Year.c <- scale(Year,scale=F)
  Year.f <- as.factor(Year)
  Age.s <- scale(Age.imp)
})
pheno.all$nflup <- rowSums(!is.na(pheno.all[c("PHQtot1","PHQtot2","PHQtot3","PHQtot4")]))

load('phqbyyear_16965_blitimputed_weighted.RData') #include resident questionnaire variables (see the last section for imputation of these two vars)
pheno.all <- within(pheno.all,{
  error_1=as.factor(pheno.all$error1_1)
  error_2=as.factor(pheno.all$error1_2)
  error_3=as.factor(pheno.all$error1_3)
  error_4=as.factor(pheno.all$error1_4)
  SLE_1=as.factor(pheno.all$SLE1)
  SLE_2=as.factor(pheno.all$SLE2)
  SLE_3=as.factor(pheno.all$SLE3)
  SLE_4=as.factor(pheno.all$SLE4)
  help_1=as.factor(pheno.all$help_1)
  help_2=as.factor(pheno.all$help_2)
  help_3=as.factor(pheno.all$help_3)
  help_4=as.factor(pheno.all$help_4)
})

pheno.noflup <- pheno.total.i[which(pheno.total.i$followup==0),]
pheno.noflup$count=1
#### Descriptive Stats ####
a=pheno.all[which(pheno.all$UserID!=pheno.all$InsStd),]
length(unique(a$InsStd))

ninvite=34860-nrow(noneligible)
nrow(pheno.total.i);nrow(pheno.total.i)/ninvite
nrow(pheno.all);nrow(pheno.all)/nrow(pheno.total.i)

pheno.2019 <- pheno.all[which(pheno.all$Year==2019),]
pheno.2019$PHQ10above0 <- as.factor(pheno.2019$PHQ10above0)
pheno.2019$PHQtottover10 <- as.factor(pheno.2019$PHQtottover10)
pheno.2019.svy <- svydesign(ids = ~InsStd,data=pheno.2019,weights =~finalweight.trim)
svytotal(~PHQ10above0,pheno.2019.svy,deff=TRUE)
svytotal(~PHQtottover10,pheno.2019.svy,deff=TRUE)

#### Prepare the survey objects ####
sum(pheno.all$finalweight.trim) / nrow(pheno.all)

pheno.all$euro.imp <- as.factor(ifelse(pheno.all$Race.imp=="European",1,0))

#subj.include <- read.csv('INS0719_subjs_12outof13.csv')
#subj.include <- read.csv('INS0719_subjs_all13.csv')
#subj.include <- read.csv('INS0719_subjs_all2007.csv')
subj.include <- read.csv('INS0719_subjs_2008to2019.csv') %>% 
  filter(Year!=2007)
pheno.common <- pheno.all[which(pheno.all$UserID %in% subj.include$UserID),]
nrow(pheno.common);nrow(pheno.all)
#save(pheno.common,file='phqbyyear_subsample_12outof13_V3.RData')
save(pheno.common,file='phqbyyear_subsample_all13.RData')

pheno.all.svy <- svydesign(ids = ~Year+InsStd,data=pheno.all,weights =~finalweight.trim)
pheno.all.svy <- svydesign(ids = ~Year,data=pheno.all,weights =~finalweight.trim)
pheno.women.svy <- svydesign(ids = ~Year+InsStd,data=pheno.all[which(pheno.all$Sex.imp==1),],weights = ~finalweight.trim)
pheno.men.svy <- svydesign(ids = ~Year+InsStd,data=pheno.all[which(pheno.all$Sex.imp==0),],weights = ~finalweight.trim)
pheno.surg.svy <- svydesign(ids = ~Year+InsStd,data=pheno.all[which(pheno.all$specb.imp==1),],weights = ~finalweight.trim)
pheno.nonsurg.svy <- svydesign(ids = ~Year+InsStd,data=pheno.all[which(pheno.all$specb.imp==0),],weights = ~finalweight.trim)
pheno.depr.svy <- svydesign(ids = ~Year+InsStd,data=pheno.all[which(pheno.all$PHQtottover10==1),],weights = ~finalweight.trim)
pheno.nondepr.svy <- svydesign(ids = ~Year+InsStd,data=pheno.all[which(pheno.all$PHQtottover10==0),],weights = ~finalweight.trim)

pheno.all.svy.unweighted <- svydesign(ids = ~Year+InsStd,data=pheno.all)

pheno.common.svy <- svydesign(ids = ~Year+InsStd,data=pheno.common,weights =~finalweight.trim)
pheno.common.women.svy <- svydesign(ids = ~Year+InsStd,data=pheno.common[which(pheno.common$Sex.imp==1),],weights = ~finalweight.trim)
pheno.common.men.svy <- svydesign(ids = ~Year+InsStd,data=pheno.common[which(pheno.common$Sex.imp==0),],weights = ~finalweight.trim)
pheno.common.surg.svy <- svydesign(ids = ~Year+InsStd,data=pheno.common[which(pheno.common$specb.imp==1),],weights = ~finalweight.trim)
pheno.common.nonsurg.svy <- svydesign(ids = ~Year+InsStd,data=pheno.common[which(pheno.common$specb.imp==0),],weights = ~finalweight.trim)
pheno.common.depr.svy <- svydesign(ids = ~Year+InsStd,data=pheno.common[which(pheno.common$PHQtottover10==1),],weights = ~finalweight.trim)

pheno.08to19.svy <- svydesign(ids = ~Year+InsStd,data=pheno.all[which(pheno.all$Year!=2007),],weights =~finalweight.trim)

pheno.1flup.svy <- svydesign(ids = ~Year+InsStd,data=pheno.all[which(pheno.all$nflup==1),],weights =~finalweight.trim)
pheno.2flup.svy <- svydesign(ids = ~Year+InsStd,data=pheno.all[which(pheno.all$nflup==2),],weights =~finalweight.trim)
pheno.3flup.svy <- svydesign(ids = ~Year+InsStd,data=pheno.all[which(pheno.all$nflup==3),],weights =~finalweight.trim)
pheno.4flup.svy <- svydesign(ids = ~Year+InsStd,data=pheno.all[which(pheno.all$nflup==4),],weights =~finalweight.trim)
pheno.0flup.svy <- svydesign(ids = ~Year,data=pheno.noflup,weights =~finalweight.trim)

# Table 1 weighted summary ####
ihssvy=pheno.all.svy
ihssvy=pheno.common.svy
ihssvy=pheno.1flup.svy
ihssvy=pheno.2flup.svy
ihssvy=pheno.3flup.svy
ihssvy=pheno.4flup.svy
ihssvy=pheno.0flup.svy

a=svytotal(~as.factor(nflup),ihssvy,deff=TRUE)

svytotal(~count,ihssvy,deff=TRUE)

svyquantile(~Age.imp,ihssvy,c(.25,.5,.75))  
svymean(~Age.imp,ihssvy)
a=svyby(formula = ~PHQ0raw.imp,by = ~Age.imp>27,design = pheno.all.svy,svymean);a
a$PHQ0raw.imp[1]-1.96*a$se[1];a$PHQ0raw.imp[1]+1.96*a$se[1]
a$PHQ0raw.imp[2]-1.96*a$se[2];a$PHQ0raw.imp[2]+1.96*a$se[2]
a=svyby(formula = ~PHQtott,by = ~Age.imp>27,design = pheno.all.svy,svymean);a
a$PHQtott[1]-1.96*a$se[1];a$PHQtott[1]+1.96*a$se[1]
a$PHQtott[2]-1.96*a$se[2];a$PHQtott[2]+1.96*a$se[2]
a=svyby(formula = ~dPHQ,by = ~Age.imp>27,design = pheno.all.svy,svymean);a
a$dPHQ[1]-1.96*a$se[1];a$dPHQ[1]+1.96*a$se[1]
a$dPHQ[2]-1.96*a$se[2];a$dPHQ[2]+1.96*a$se[2]
svyttest(dPHQ ~ Age.imp>27,pheno.all.svy)

quantile(pheno.all$Age.imp)

a=svytotal(~Sex.imp,ihssvy,deff=TRUE)
a;a[1]/(a[1]+a[2]);a[2]/(a[1]+a[2])
svyby(formula = ~PHQ0raw.imp,by = ~Sex.imp,design = pheno.all.svy,svymean)
svyby(formula = ~PHQtott,by = ~Sex.imp,design = pheno.all.svy,svymean)
a=svyby(formula = ~dPHQ,by = ~Sex.imp,design = pheno.all.svy,svymean);a
a$dPHQ[1]-1.96*a$se[1];a$dPHQ[1]+1.96*a$se[1]
a$dPHQ[2]-1.96*a$se[2];a$dPHQ[2]+1.96*a$se[2]
svyttest(dPHQ ~ Sex.imp,pheno.all.svy)

table(pheno.all$Sex.imp)
table(pheno.all$Sex.imp)/nrow(pheno.all)

a=svytotal(~specb.imp,ihssvy,deff=TRUE)
a;a[1]/(a[1]+a[2]);a[2]/(a[1]+a[2])
svyby(formula = ~PHQ0raw.imp,by = ~specb.imp,design = pheno.all.svy,svymean)
svyby(formula = ~PHQtott,by = ~specb.imp,design = pheno.all.svy,svymean)
a=svyby(formula = ~dPHQ,by = ~specb.imp,design = pheno.all.svy,svymean);a
a$dPHQ[1]-1.96*a$se[1];a$dPHQ[1]+1.96*a$se[1]
a$dPHQ[2]-1.96*a$se[2];a$dPHQ[2]+1.96*a$se[2]
svyttest(dPHQ ~ specb.imp,pheno.all.svy)

table(pheno.all$specb.imp)
table(pheno.all$specb.imp)/nrow(pheno.all)

a=svytotal(~Race.imp,ihssvy,deff=TRUE)
a;a[1]/(a[1]+a[2]+a[3]);a[2]/(a[1]+a[2]+a[3]);a[3]/(a[1]+a[2]+a[3])
svyby(formula = ~PHQ0raw.imp,by = ~Race.imp,design = pheno.all.svy,svymean)
svyby(formula = ~PHQtott,by = ~Race.imp,design = pheno.all.svy,svymean)
a=svyby(formula = ~dPHQ,by = ~Race.imp,design = pheno.all.svy,svymean);a
a$dPHQ[1]-1.96*a$se[1];a$dPHQ[1]+1.96*a$se[1]
a$dPHQ[2]-1.96*a$se[2];a$dPHQ[2]+1.96*a$se[2]
a$dPHQ[3]-1.96*a$se[3];a$dPHQ[3]+1.96*a$se[3]
pheno.ea.svy <- svydesign(ids = ~Year+InsStd,data=pheno.all[which(pheno.all$Race.imp!='Other'),],weights = ~finalweight.trim)
svyttest(dPHQ ~ Race.imp,pheno.ea.svy)
pheno.eo.svy <- svydesign(ids = ~Year+InsStd,data=pheno.all[which(pheno.all$Race.imp!='Asian'),],weights = ~finalweight.trim)
svyttest(dPHQ ~ Race.imp,pheno.eo.svy)

table(pheno.all$Race.imp)
table(pheno.all$Race.imp)/nrow(pheno.all)

svyquantile(~PHQ0raw.imp,ihssvy,c(.25,.5,.75))
svyquantile(~PHQtott,ihssvy,c(.25,.5,.75)) 
svyquantile(~dPHQ,ihssvy,c(.25,.5,.75)) 

quantile(pheno.all$PHQ0raw.imp)
quantile(pheno.all$PHQtott)

svymean(~PHQ0raw.imp,ihssvy)
svymean(~PHQtott,ihssvy)

a=svytotal(~deprhis.imp,ihssvy,deff=TRUE)
a;a[1]/(a[1]+a[2]);a[2]/(a[1]+a[2])
svyby(formula = ~PHQ0raw.imp,by = ~deprhis.imp,design = pheno.all.svy,svymean)
svyby(formula = ~PHQtott,by = ~deprhis.imp,design = pheno.all.svy,svymean)
a=svyby(formula = ~dPHQ,by = ~deprhis.imp,design = pheno.all.svy,svymean);a
a$dPHQ[1]-1.96*a$se[1];a$dPHQ[1]+1.96*a$se[1]
a$dPHQ[2]-1.96*a$se[2];a$dPHQ[2]+1.96*a$se[2]
svyttest(dPHQ ~ deprhis.imp,pheno.all.svy)

table(pheno.all$deprhis.imp)
table(pheno.all$deprhis.imp)/nrow(pheno.all)

svyquantile(~Neu0.imp,ihssvy,c(.25,.5,.75))
svyby(formula = ~PHQ0raw.imp,by = ~Neu0.imp>22,design = pheno.all.svy,svymean)
svyby(formula = ~PHQtott,by = ~Neu0.imp>22,design = pheno.all.svy,svymean)
a=svyby(formula = ~dPHQ,by = ~Neu0.imp>22,design = pheno.all.svy,svymean);a
a$dPHQ[1]-1.96*a$se[1];a$dPHQ[1]+1.96*a$se[1]
a$dPHQ[2]-1.96*a$se[2];a$dPHQ[2]+1.96*a$se[2]
svyttest(dPHQ ~ Neu0.imp>22,pheno.all.svy)

quantile(pheno.all$Neu0.imp)

svyquantile(~EFE0raw.imp,ihssvy,c(.25,.5,.75))
svyby(formula = ~PHQ0raw.imp,by = ~EFE0raw.imp>11,design = pheno.all.svy,svymean)
svyby(formula = ~PHQtott,by = ~EFE0raw.imp>11,design = pheno.all.svy,svymean)
a=svyby(formula = ~dPHQ,by = ~EFE0raw.imp>11,design = pheno.all.svy,svymean);a
a$dPHQ[1]-1.96*a$se[1];a$dPHQ[1]+1.96*a$se[1]
a$dPHQ[2]-1.96*a$se[2];a$dPHQ[2]+1.96*a$se[2]
svyttest(dPHQ ~ EFE0raw.imp>11,pheno.all.svy)

quantile(pheno.all$EFE0raw.imp)

svyquantile(~hourst.imp,ihssvy,c(.25,.5,.75))
svyby(formula = ~PHQ0raw.imp,by = ~hourst.imp>65,design = pheno.all.svy,svymean)
svyby(formula = ~PHQtott,by = ~hourst.imp>65,design = pheno.all.svy,svymean)
a=svyby(formula = ~dPHQ,by = ~hourst.imp>65,design = pheno.all.svy,svymean);a
a$dPHQ[1]-1.96*a$se[1];a$dPHQ[1]+1.96*a$se[1]
a$dPHQ[2]-1.96*a$se[2];a$dPHQ[2]+1.96*a$se[2]
svyttest(dPHQ ~ hourst.imp>65,pheno.all.svy)

quantile(pheno.all$hourst.imp)

svyquantile(~sleepAvet.imp,ihssvy,c(.25,.5,.75))
svyby(formula = ~PHQ0raw.imp,by = ~sleepAvet.imp>6.5,design = pheno.all.svy,svymean)
svyby(formula = ~PHQtott,by = ~sleepAvet.imp>6.5,design = pheno.all.svy,svymean)
a=svyby(formula = ~dPHQ,by = ~sleepAvet.imp>6.5,design = pheno.all.svy,svymean);a
a$dPHQ[1]-1.96*a$se[1];a$dPHQ[1]+1.96*a$se[1]
a$dPHQ[2]-1.96*a$se[2];a$dPHQ[2]+1.96*a$se[2]
svyttest(dPHQ ~ sleepAvet.imp>6.5,pheno.all.svy)

quantile(pheno.all$sleepAvet.imp)

a=svytotal(~evererror.imp,ihssvy,deff=TRUE)
a;a[1]/(a[1]+a[2]);a[2]/(a[1]+a[2])
svyby(formula = ~PHQ0raw.imp,by = ~evererror.imp,design = pheno.all.svy,svymean)
svyby(formula = ~PHQtott,by = ~evererror.imp,design = pheno.all.svy,svymean)
a=svyby(formula = ~dPHQ,by = ~evererror.imp,design = pheno.all.svy,svymean);a
a$dPHQ[1]-1.96*a$se[1];a$dPHQ[1]+1.96*a$se[1]
a$dPHQ[2]-1.96*a$se[2];a$dPHQ[2]+1.96*a$se[2]
svyttest(dPHQ ~ evererror.imp,pheno.all.svy)

table(pheno.all$evererror.imp)
table(pheno.all$evererror.imp)/nrow(pheno.all)

a=svytotal(~error_1,ihssvy,deff=TRUE,na.rm = T);a
b=svytotal(~error_2,ihssvy,deff=TRUE,na.rm = T);b
c=svytotal(~error_3,ihssvy,deff=TRUE,na.rm = T);c
d=svytotal(~error_4,ihssvy,deff=TRUE,na.rm = T);d
a[1]+b[1]+c[1]+d[1]
a[2]+b[2]+c[2]+d[2]
(a[1]+b[1]+c[1]+d[1])/(a[1]+b[1]+c[1]+d[1]+a[2]+b[2]+c[2]+d[2])
(a[2]+b[2]+c[2]+d[2])/(a[1]+b[1]+c[1]+d[1]+a[2]+b[2]+c[2]+d[2])

a=svytotal(~everSLE.imp,ihssvy,deff=TRUE)
a;a[1]/(a[1]+a[2]);a[2]/(a[1]+a[2])
svyby(formula = ~PHQ0raw.imp,by = ~everSLE.imp,design = pheno.all.svy,svymean)
svyby(formula = ~PHQtott,by = ~everSLE.imp,design = pheno.all.svy,svymean)
a=svyby(formula = ~dPHQ,by = ~everSLE.imp,design = pheno.all.svy,svymean);a
a$dPHQ[1]-1.96*a$se[1];a$dPHQ[1]+1.96*a$se[1]
a$dPHQ[2]-1.96*a$se[2];a$dPHQ[2]+1.96*a$se[2]
svyttest(dPHQ ~ everSLE.imp,pheno.all.svy)

table(pheno.all$everSLE.imp)
table(pheno.all$everSLE.imp)/nrow(pheno.all)

a=svytotal(~SLE_1,ihssvy,deff=TRUE,na.rm = T);a
b=svytotal(~SLE_2,ihssvy,deff=TRUE,na.rm = T);b
c=svytotal(~SLE_3,ihssvy,deff=TRUE,na.rm = T);c
d=svytotal(~SLE_4,ihssvy,deff=TRUE,na.rm = T);d
a[1]+b[1]+c[1]+d[1]
a[2]+b[2]+c[2]+d[2]
(a[1]+b[1]+c[1]+d[1])/(a[1]+b[1]+c[1]+d[1]+a[2]+b[2]+c[2]+d[2])
(a[2]+b[2]+c[2]+d[2])/(a[1]+b[1]+c[1]+d[1]+a[2]+b[2]+c[2]+d[2])

a=svytotal(~everhelp.imp,ihssvy[which(ihssvy$variables$PHQtottover10==1),],deff=TRUE)
a;a[1]/(a[1]+a[2]);a[2]/(a[1]+a[2])
svyby(formula = ~PHQ0raw.imp,by = ~everhelp.imp,design = pheno.depr.svy,svymean)
svyby(formula = ~PHQtott,by = ~everhelp.imp,design = pheno.depr.svy,svymean)
a=svyby(formula = ~dPHQ,by = ~everhelp.imp,design = pheno.depr.svy,svymean);a
a$dPHQ[1]-1.96*a$se[1];a$dPHQ[1]+1.96*a$se[1]
a$dPHQ[2]-1.96*a$se[2];a$dPHQ[2]+1.96*a$se[2]
svyttest(dPHQ ~ everhelp.imp,pheno.depr.svy)

pheno.all.depr=pheno.all %>% filter(PHQtottover10==1)
table(pheno.all.depr$everhelp.imp)
table(pheno.all.depr$everhelp.imp)/nrow(pheno.all.depr)

a=svytotal(~help_1,ihssvy[which(ihssvy$variables$PHQtottover10==1),],deff=TRUE,na.rm = T);a
b=svytotal(~help_2,ihssvy[which(ihssvy$variables$PHQtottover10==1),],deff=TRUE,na.rm = T);b
c=svytotal(~help_3,ihssvy[which(ihssvy$variables$PHQtottover10==1),],deff=TRUE,na.rm = T);c
d=svytotal(~help_4,ihssvy[which(ihssvy$variables$PHQtottover10==1),],deff=TRUE,na.rm = T);d
a[1]+b[1]+c[1]+d[1]
a[2]+b[2]+c[2]+d[2]
(a[1]+b[1]+c[1]+d[1])/(a[1]+b[1]+c[1]+d[1]+a[2]+b[2]+c[2]+d[2])
(a[2]+b[2]+c[2]+d[2])/(a[1]+b[1]+c[1]+d[1]+a[2]+b[2]+c[2]+d[2])

svyquantile(~faculFeedb4.imp,ihssvy,c(.25,.5,.75))
a=svyby(formula = ~PHQ0raw.imp,by = ~faculFeedb4.imp>4,design = pheno.all.svy,svymean);a
a=svyby(formula = ~PHQtott,by = ~faculFeedb4.imp>4,design = pheno.all.svy,svymean);a
a=svyby(formula = ~dPHQ,by = ~faculFeedb4.imp>4,design = pheno.all.svy,svymean);a
a$dPHQ[1]-1.96*a$se[1];a$dPHQ[1]+1.96*a$se[1]
a$dPHQ[2]-1.96*a$se[2];a$dPHQ[2]+1.96*a$se[2]
svyttest(dPHQ ~ faculFeedb4.imp>4,pheno.all.svy)

quantile(pheno.all$faculFeedb4.imp)

svyquantile(~rotationValue4.imp,ihssvy,c(.25,.5,.75))
a=svyby(formula = ~PHQ0raw.imp,by = ~rotationValue4.imp>4,design = pheno.all.svy,svymean);a
a=svyby(formula = ~PHQtott,by = ~rotationValue4.imp>4,design = pheno.all.svy,svymean);a
a=svyby(formula = ~dPHQ,by = ~rotationValue4.imp>4,design = pheno.all.svy,svymean);a
a$dPHQ[1]-1.96*a$se[1];a$dPHQ[1]+1.96*a$se[1]
a$dPHQ[2]-1.96*a$se[2];a$dPHQ[2]+1.96*a$se[2]
svyttest(dPHQ ~ rotationValue4.imp>4,pheno.all.svy)

quantile(pheno.all$rotationValue4.imp)

# Appendix Table 2. comparisons of follow up and non-followup ####
pheno.total.i$followup <- as.factor(pheno.total.i$followup)
pheno.total.i$euro.imp <- as.factor(ifelse(pheno.total.i$Race.imp=="European",1,0))
pheno.total.svy <- svydesign(ids = ~Year+InsStd,data=pheno.total.i,weights =~finalweight.trim)

aggregate(Age.imp~followup,pheno.total.i,mean)
aggregate(Age.imp~followup,pheno.total.i,se)
t.test(Age.imp~followup,pheno.total.i)
svyby(formula = ~Age.imp,by = ~followup,design = pheno.total.svy,svymean)
svyttest(Age.imp ~ followup,pheno.total.svy)

table(pheno.total.i$Sex.imp,pheno.total.i$followup)
chisq.test(table(pheno.total.i$Sex.imp,pheno.total.i$followup))
svytable(~Sex.imp+followup,pheno.total.svy)
svychisq(~Sex.imp+followup, pheno.total.svy)

table(pheno.total.i$euro.imp,pheno.total.i$followup)
chisq.test(table(pheno.total.i$euro.imp,pheno.total.i$followup))
svytable(~euro.imp+followup,pheno.total.svy)
svychisq(~euro.imp+followup, pheno.total.svy)

table(pheno.total.i$specb.imp,pheno.total.i$followup)
chisq.test(table(pheno.total.i$specb.imp,pheno.total.i$followup))
svytable(~specb.imp+followup,pheno.total.svy)
svychisq(~specb.imp+followup, pheno.total.svy)

aggregate(PHQ0raw.imp~followup,pheno.total.i,mean)
aggregate(PHQ0raw.imp~followup,pheno.total.i,se)
t.test(PHQ0raw.imp~followup,pheno.total.i)
svyby(formula = ~PHQ0raw.imp,by = ~followup,design = pheno.total.svy,svymean)
svyttest(PHQ0raw.imp ~ followup,pheno.total.svy)

aggregate(Neu0.imp~followup,pheno.total.i,mean)
aggregate(Neu0.imp~followup,pheno.total.i,se)
t.test(Neu0.imp~followup,pheno.total.i)
svyby(formula = ~Neu0.imp,by = ~followup,design = pheno.total.svy,svymean)
svyttest(Neu0.imp ~ followup,pheno.total.svy)

aggregate(EFE0raw.imp~followup,pheno.total.i,mean)
aggregate(EFE0raw.imp~followup,pheno.total.i,se)
t.test(EFE0raw.imp~followup,pheno.total.i)
svyby(formula = ~EFE0raw.imp,by = ~followup,design = pheno.total.svy,svymean)
svyttest(EFE0raw.imp ~ followup,pheno.total.svy)

table(pheno.total.i$deprhis.imp,pheno.total.i$followup)
chisq.test(table(pheno.total.i$deprhis.imp,pheno.total.i$followup))
svytable(~deprhis.imp+followup,pheno.total.svy)
svychisq(~deprhis.imp+followup, pheno.total.svy)

svytotal(~followup,pheno.total.svy)

# between residency institution differences #### 
pheno.all$dPHQw=pheno.all$dPHQ*pheno.all$finalweight.trim
pheno.all$dPHQresid=resid(svyglm(dPHQ ~ Sex.imp+Race.imp+specb.imp,pheno.all.svy))
pheno.all$dPHQwresid=resid(svyglm(dPHQw ~ Sex.imp+Race.imp+specb.imp,pheno.all.svy))
pheno.all2 <- pheno.all[which(pheno.all$UserID!=pheno.all$InsStd),]
pheno.all2 %>% kruskal_test(dPHQw ~ InsStd)
pheno.all2 %>% kruskal_effsize(dPHQw ~ InsStd)
pheno.all2 %>% kruskal_test(dPHQwresid ~ InsStd)
pheno.all2 %>% kruskal_effsize(dPHQwresid ~ InsStd)

pheno.all2 %>% kruskal_test(dPHQw ~ Year)
pheno.all2 %>% kruskal_effsize(dPHQw ~ Year)
pheno.all2 %>% kruskal_test(dPHQwresid ~ Year)
pheno.all2 %>% kruskal_effsize(dPHQwresid ~ Year)

pheno.all.svy2 <- svydesign(ids = ~1,data=pheno.all2,weights =~finalweight.trim)
anova(svyglm(dPHQ~InsStd,pheno.all.svy2))
anova(lm(dPHQw~InsStd,pheno.all2))
anova(lm(dPHQw~Sex.imp+Race.imp+specb.imp+InsStd,pheno.all2))
anova(lm(dPHQw~Sex.imp+Race.imp+specb.imp+InsStd,pheno.all2))
summary(m1 <- lm(dPHQw~InsStd,pheno.all2))
summary(m2 <- lm(dPHQw~InsStd,pheno.icc2))
view(as.data.frame(summary(m2)$coefficients))

anova(svyglm(dPHQ~Sex.imp+Race.imp+specb.imp,pheno.all.svy))
anova(lm(dPHQ~Sex.imp+Race.imp+specb.imp,pheno.all))

a=as.data.frame(table(pheno.all$InsStd)) %>% 
  filter(Freq>2)

pheno.all$dPHQresid=resid(lm(dPHQ ~ Sex.imp+Race.imp+specb.imp,pheno.all))
pheno.icc2 <- pheno.all[which(pheno.all$InsStd %in% a$Var1),]
pheno.icc2$InsStd <- factor(as.character(pheno.icc2$InsStd), levels = unique(pheno.icc2$InsStd))
pheno.icc2.svy=svydesign(ids = ~Year+InsStd,data=pheno.icc2,weights = ~finalweight.trim)
m1=svyglm(dPHQ ~ Sex.imp+Race.imp+specb.imp,pheno.all.svy)
m1=svyglm(dPHQ ~ InsStd,pheno.icc2.svy)
anova(m1)

library(car)
pheno.icc2$Sex.imp.num <- ifelse(pheno.icc2$Sex.imp==1,1,0)
pheno.icc2$specb.imp.num <- ifelse(pheno.icc2$specb.imp==1,1,0)

leveneTest(dPHQw ~ InsStd,data=pheno.all)
leveneTest(dPHQ ~ InsStd,data=pheno.icc2)
summary(aov(dPHQ~InsStd,data=pheno.icc2))
summary(aov(dPHQ~InsStd,data=pheno.common))
oneway_FY(formula = dPHQw~InsStd,data=pheno.icc2,var.equal = F)
oneway_FY(formula = dPHQresid~InsStd,data=pheno.icc2,var.equal = F)
oneway_FY(formula = dPHQ~InsStd,data=pheno.icc2)
oneway_FY(dPHQ ~ InsStd,data=pheno.common)

#a non-parametric alternative to one-way ANOVA is Kruskal-Wallis rank sum test, which can be used when ANOVA assumptions are not met.
kruskal.test(dPHQ ~ InsStd, data = pheno.all)
kruskal.test(dPHQresid ~ InsStd, data = pheno.all)
oneway_FY(formula = dPHQresid~InsStd,data=pheno.icc2)
kruskal.test(dPHQ ~ InsStd, data = pheno.all)

for (y in 2007:2019){
  b=pheno.all[which(pheno.all$Year==y),]
  a=as.data.frame(table(b$InsStd)) %>% 
    filter(Freq>2)
  pheno.icc2 <- b[which(b$InsStd %in% a$Var1),]
  pheno.icc2$InsStd <- factor(as.character(pheno.icc2$InsStd), levels = unique(pheno.icc2$InsStd))
  print(y)
  #print(leveneTest(dPHQ ~ InsStd,data=pheno.icc2))
  print(oneway_FY(formula = dPHQ~InsStd,data=pheno.icc2))
}


leveneTest(PHQ0raw.imp ~ InsStd,data=pheno.icc2)
summary(aov(PHQ0raw.imp ~ InsStd,data=pheno.icc2))

leveneTest(PHQtott ~ InsStd,data=pheno.icc2)
summary(aov(PHQtott ~ InsStd,data=pheno.icc2))

leveneTest(Sex.imp.num ~ InsStd,data=pheno.icc2)
summary(aov(Sex.imp.num ~ InsStd,data=pheno.icc2))

leveneTest(specb.imp.num ~ InsStd,data=pheno.icc2)
summary(aov(specb.imp.num ~ InsStd,data=pheno.icc2))

# Table 2, Table 3 marginal prediction ####
library(BSDA)
options(digits=2)
pheno.all$deprhis.imp.num <- ifelse(pheno.all$deprhis.imp==0,FALSE,TRUE)
adjustdemos=as.formula('~Sex.imp+Race.imp+specb.imp')
adjustdemos2=as.formula('~Race.imp+specb.imp')
adjustdemos3=as.formula('~Sex.imp+Race.imp+specb.imp+hourst.imp+sleepAvet.imp+everSLE.imp+evererror.imp')
n=sum(pheno.all$finalweight.trim) #actually doesn't matter

ihsvars=c("dPHQ", #1
          "hourst.imp", #2
          "sleepAvet.imp", #3
          "everSLE.imp", #4
          "evererror.imp", #5
          "everhelp.imp",#6
          "PHQ0raw.imp", #7
          "Neu0.imp", #8
          "deprhis.imp",#9
          "EFE0raw.imp", #10
          "Sex.imp",#11
          "helpAsked3",#12
          "helpAsked4",#13
          "helpAsked5",#14
          "helpAsked6",#15
          "faculFeedb4.imp",#16
          "rotationValue4.imp") #17
var.use=ihsvars[16];var.use
OutcomePredict <- as.formula(paste0(var.use,' ~ ns(Year,df=3)'))
OutcomePredict <- as.formula(paste0(var.use,' ~ ns(Year,df=3)+Year*specb.imp'))
OutcomePredict <- as.formula(paste0(var.use,' ~ ns(Year,df=3)+Year*Sex.imp'))
OutcomePredict <- as.formula(paste0(var.use,' ~ specb.imp*ns(Year,df=3)'))
OutcomePredict <- as.formula(paste0(var.use,' ~ Sex.imp*ns(Year,df=3)'))
ihssvy=pheno.all.svy
ihssvy=pheno.men.svy
ihssvy=pheno.women.svy
ihssvy=pheno.surg.svy
ihssvy=pheno.nonsurg.svy
pheno.depr.0919 = pheno.all[which(pheno.all$PHQtottover10==1 & pheno.all$Year>=2009 & pheno.all$Sex.imp==0),]
pheno.depr.0919 = pheno.all[which(pheno.all$PHQtottover10==1 & pheno.all$Year>=2009 & pheno.all$specb.imp==1),]
pheno.depr.0919 = pheno.all[which(pheno.all$PHQtottover10==1 & pheno.all$Year>=2009),]
#pheno.depr.0919 = pheno.common[which(pheno.common$PHQtottover10==1 & pheno.common$Year>=2009),]
ihssvy <- svydesign(ids = ~Year+InsStd,data=pheno.depr.0919,weights = ~finalweight.trim)
ihssvy=pheno.common.svy
ihssvy=pheno.08to19.svy
ihssvy=pheno.4flup.svy
ihssvy=pheno.0flup.svy

newdf=data.frame(Year=c(2007:2019))
newdf=data.frame(Year=c(2009:2019))
newdf=data.frame(Year=c(2008:2019))

model <- svyglm(formula = OutcomePredict,ihssvy,rescale=TRUE);summary(model) # continuous variable
model <- svyglm(formula = OutcomePredict,family=quasibinomial,ihssvy,rescale=TRUE);summary(model) # binary variable
source('~/Dropbox (University of Michigan)/phq by year/Scripts/Additive interactions function.r')
additive_interactions(model)
additive_interactions(model,recode = T,dat=pheno.all.svy)

pred.all=marginpred(model,adjustfor=adjustdemos,predictat = newdf,type = "response");pred.all #main
pred.all=marginpred(model,adjustfor=adjustdemos2,predictat = newdf,type='response');pred.all#Sex

u=as.data.frame(svyby(formula = ~hourst.imp,by = ~Year,design = ihssvy,svymean,se=TRUE));u
plot(u$hourst)

y=as.data.frame(pred.all);plot(y$response);View(y)
start=1;end=13
start=1;end=11
start=1;end=12
mid=5 #2011
y$response[start];y$response[start]-1.96*y$SE[start];y$response[start]+1.96*y$SE[start]
y$response[end];y$response[end]-1.96*y$SE[end];y$response[end]+1.96*y$SE[end]
y$response[mid];y$response[mid]-1.96*y$SE[mid];y$response[mid]+1.96*y$SE[mid]
#adjusted Sp, assuming N1=N2=n, cancel sqrt(2/n) and sqrt(n/2)
n1=1200;n2=1200;

SE=sqrt((y$SE[start]^2+y$SE[end]^2));diffmean=y$response[end]-y$response[start];lower=diffmean-(1.96*SE);upper=diffmean+(1.96*SE);diffmean;lower;upper
SE=sqrt((y$SE[start]^2+y$SE[mid]^2));diffmean=y$response[mid]-y$response[start];lower=diffmean-(1.96*SE);upper=diffmean+(1.96*SE);diffmean;lower;upper
SE=sqrt((y$SE[mid]^2+y$SE[end]^2));diffmean=y$response[end]-y$response[mid];lower=diffmean-(1.96*SE);upper=diffmean+(1.96*SE);diffmean;lower;upper

tsum.test(mean.x=y$response[start], s.x=y$SE[start]*sqrt(n1), n.x=n1,mean.y=y$response[end], s.y=y$SE[end]*sqrt(n2), n.y=n2,var.equal = F)
tsum.test(mean.x=y$response[start], s.x=y$SE[start]*sqrt(n1), n.x=n1,mean.y=y$response[mid], s.y=y$SE[mid]*sqrt(n2), n.y=n2,var.equal = F)
tsum.test(mean.x=y$response[mid], s.x=y$SE[mid]*sqrt(n1), n.x=n1,mean.y=y$response[end], s.y=y$SE[end]*sqrt(n2), n.y=n2,var.equal = F)

tsum.test(mean.x=3.5,s.x=0.1*sqrt(510),n.x=510,
          mean.y=4.7,s.y=0.2*sqrt(490),n.y=490)
tsum.test(mean.x=2.9,s.x=0.3*sqrt(n),n.x=n,
          mean.y=3.3,s.y=0.09*sqrt(n),n.y=n)

tsum.test(mean.x=-0.6,s.x=((-0.05+1.2)/2/1.96)*sqrt(n),n.x=n,
          mean.y=-1.4,s.y=((-0.9+1.9)/2/1.96)*sqrt(n),n.y=n)
tsum.test(mean.x=-0.8,s.x=((-0.4+1.2)/2/1.96)*sqrt(n),n.x=n,
          mean.y=-1.1,s.y=((-0.6+1.6)/2/1.96)*sqrt(n),n.y=n)

# raw data
pheno.all.svy <- svydesign(ids = ~InsStd,data=pheno.all,weights = ~finalweight.trim)
pheno.women.svy <- svydesign(ids = ~InsStd,data=pheno.all[which(pheno.all$Sex.imp==1),],weights = ~finalweight.trim)
pheno.men.svy <- svydesign(ids = ~InsStd,data=pheno.all[which(pheno.all$Sex.imp==0),],weights = ~finalweight.trim)
pheno.surg.svy <- svydesign(ids = ~InsStd,data=pheno.all[which(pheno.all$specb.imp==1),],weights = ~finalweight.trim)
pheno.nonsurg.svy <- svydesign(ids = ~InsStd,data=pheno.all[which(pheno.all$specb.imp==0),],weights = ~finalweight.trim)

ihssvy=pheno.all.svy
ihssvy=pheno.men.svy
ihssvy=pheno.women.svy
ihssvy=pheno.surg.svy
ihssvy=pheno.nonsurg.svy

u=as.data.frame(svyby(formula = ~dPHQ,by = ~Year,design = ihssvy,svymean,se=TRUE));u
plot(u$dPHQ)

# sleep or work hour 2010vs2011, 2016vs2017
tsum.test(mean.x=y$response[4], s.x=y$SE[4]*sqrt(n), n.x=n,
          mean.y=y$response[5], s.y=y$SE[5]*sqrt(n), n.y=n)
tsum.test(mean.x=y$response[10], s.x=y$SE[10]*sqrt(n), n.x=n,
          mean.y=y$response[11], s.y=y$SE[11]*sqrt(n), n.y=n)

# Figure 1 ####
ihssvy <- svydesign(ids = ~InsStd,data=pheno.all,weights =~finalweight.trim)
u=as.data.frame(svyby(formula = ~dPHQ,by = ~Year,design = ihssvy,svymean,se=TRUE));u
u$group='Raw'
y=as.data.frame(pred.all)
names(y) <- c("dPHQ","se")
y$Year=c(2007:2019)
y$group='Predicted'
fig1data <- rbind(u,y)
fig1data$CIl <- fig1data$dPHQ-1.96*fig1data$se
fig1data$CIu <- fig1data$dPHQ+1.96*fig1data$se
save(fig1data,file='figure1data.RData')
load('figure1data.RData')
ggplot(fig1data,aes(x=Year,y=dPHQ,group=group,color=group))+
  geom_point()+
  geom_errorbar(aes(ymin=CIl,ymax=CIu),width=.2,position = position_dodge(0.05))+
  ylim(0,4.65)

#### Appendix Table 3. linear trend ####
ihssvy=pheno.all.svy
ihssvy=pheno.men.svy
ihssvy=pheno.women.svy
ihssvy=pheno.surg.svy
ihssvy=pheno.nonsurg.svy
ihssvy=pheno.common.svy
pheno.depr.0919 = pheno.all[which(pheno.all$PHQtottover10==1 & pheno.all$Year>=2009),]
pheno.depr.0919 = pheno.all[which(pheno.all$PHQtottover10==1 & pheno.all$Year>=2009 & pheno.all$specb.imp==0),]
ihssvy <- svydesign(ids = ~Year+InsStd,data=pheno.depr.0919,weights = ~finalweight.trim)

ihsvars=c("dPHQ", #1
          "hourst.imp", #2
          "sleepAvet.imp", #3
          "everSLE.imp", #4
          "evererror.imp", #5
          "everhelp.imp") #6
var.use=ihsvars[4];var.use
OutcomePredict <- as.formula(paste0(var.use,' ~ Year'))
OutcomePredict <- as.formula(paste0(var.use,' ~ Year.c*Sex.imp'))
OutcomePredict <- as.formula(paste0(var.use,' ~ Year.c*specb.imp'))
OutcomePredict <- as.formula(paste0(var.use,' ~ Year.c*Sex.imp*specb.imp'))
OutcomePredict <- as.formula(paste0(var.use,' ~ ns(Year,df=3)*specb.imp'))

summary(m1 <- svyglm(OutcomePredict,ihssvy,rescale=TRUE))
confint(m1)
summary(m2 <- svyglm(OutcomePredict,ihssvy,family=quasibinomial,rescale=TRUE))
confint(m2)

# ceiling effect check ####
library(e1071)
skewcompare <- data.frame(year=numeric(13),skewt=numeric(13))
years=c(2007:2019)
for (i in 1:13){
  year=years[i]
  pheno.sub=pheno.all[which(pheno.all$Year==year),]
  skewcompare$year[i]=year
  #skewcompare$skewt[i]=skewness(pheno.sub$PHQtott)
  skewcompare$skewt[i]=skewness(pheno.sub$finalweight.trim*pheno.sub$PHQtott)
}
summary(m1 <- lm(skewt ~ year,skewcompare))
confint(m1,'year',level = 0.95)

# 4 Follow-ups ####
library(plotrix)
nofollowup <- pheno.total.i[which(pheno.total.i$followup==0),]
View(nofollowup[c("UserID","PHQtot1","PHQtot2","PHQtot3","PHQtot4","PHQtott","PHQtot0","PHQ0raw.imp")])

table(is.na(pheno.all$PHQtot1))
table(is.na(pheno.all$PHQtot2))
table(is.na(pheno.all$PHQtot3))
table(is.na(pheno.all$PHQtot4))
mean(pheno.all$PHQtot1,na.rm=T)
mean(pheno.all$PHQtot2,na.rm=T)
mean(pheno.all$PHQtot3,na.rm=T)
mean(pheno.all$PHQtot4,na.rm=T)
mean(pheno.all$PHQtott)
std.error(pheno.all$PHQtot1,na.rm=T)
std.error(pheno.all$PHQtot2,na.rm=T)
std.error(pheno.all$PHQtot3,na.rm=T)
std.error(pheno.all$PHQtot4,na.rm=T)
phq1=data.frame(phq=pheno.all$PHQtot1,Q="Q1")
phq2=data.frame(phq=pheno.all$PHQtot2,Q="Q2")
phq3=data.frame(phq=pheno.all$PHQtot3,Q="Q3")
phq4=data.frame(phq=pheno.all$PHQtot4,Q="Q4")
phq=rbind(phq1,phq2,phq3,phq4)
summary(aov(phq ~ Q,data=phq))#p=3.3e-6

a=pheno.all %>% 
  filter(!is.na(PHQtot1)&!is.na(PHQtot2)&!is.na(PHQtot3)&!is.na(PHQtot4))
mean(a$PHQtot1,na.rm=T)
mean(a$PHQtot2,na.rm=T)
mean(a$PHQtot3,na.rm=T)
mean(a$PHQtot4,na.rm=T)

View(aggregate(PHQtot1~Year,pheno.all,mean))
View(aggregate(PHQtot2~Year,pheno.all,mean))
View(aggregate(PHQtot3~Year,pheno.all,mean))
View(aggregate(PHQtot4~Year,pheno.all,mean))
View(aggregate(PHQtott~Year,pheno.all,mean))

View(pheno.all[c("UserID","PHQtot1","PHQtot2","PHQtot3","PHQtot4","PHQtott","nflup","PHQtot0","PHQ0raw.imp")])
table(pheno.all$nflup)
aggregate(PHQtott~nflup,pheno.all,mean)
aggregate(PHQ0raw~nflup,pheno.all,mean)
aggregate(dPHQ~nflup,pheno.all,mean)
aggregate(dPHQ~nflup,pheno.all,std.error)
aggregate(dPHQ~as.factor(nflup==1),pheno.all,mean)
View(as.data.frame.matrix(table(pheno.all$Year,pheno.all$nflup)))

a=pheno.all %>% 
  filter(nflup==1)
sum(!is.na(a$PHQtot1));sum(!is.na(a$PHQtot2));sum(!is.na(a$PHQtot3));sum(!is.na(a$PHQtot4))

a=pheno.all %>% 
  filter(nflup==2)
sum(!is.na(a$PHQtot1)&!is.na(a$PHQtot2));
sum(!is.na(a$PHQtot1)&!is.na(a$PHQtot3));
sum(!is.na(a$PHQtot1)&!is.na(a$PHQtot4));
sum(!is.na(a$PHQtot3)&!is.na(a$PHQtot2));
sum(!is.na(a$PHQtot4)&!is.na(a$PHQtot2));
sum(!is.na(a$PHQtot3)&!is.na(a$PHQtot4));

a=pheno.all %>% 
  filter(nflup==3)
sum(is.na(a$PHQtot4));sum(is.na(a$PHQtot3));sum(is.na(a$PHQtot2));sum(is.na(a$PHQtot1))

a=pheno.all %>% 
  filter(nflup>1)
b=a[c("PHQtot1","PHQtot2","PHQtot3","PHQtot4")]
c=numeric(nrow(b))
for (i in 1:nrow(b)){
  c[i]=sd(as.vector(b[i,]),na.rm = T)
}
b$sd=c
summary(b$sd)
summary(lm(PHQtott~nflup,pheno.all))

yearref=2007
yearcomp=2008
pheno.test=pheno.all %>% filter(Year==yearref|Year==yearcomp)
chisq.test(table(pheno.test$nflup,pheno.test$Year))

# residency questionnaire ####
rq=read.csv('data0719_RE.csv',stringsAsFactors = F)
pheno.all <- merge(pheno.all,rq,by=c("UserID","Year"),sort=F)
sum(is.na(pheno.all$caseLoad4))
a=pheno.all %>% 
  filter(!is.na(caseLoad4))
length(unique(a$InsStd))
b=as.data.frame(table(a$InsStd)) %>% 
  filter(Freq>0)
cor.test(pheno.all$faculFeedb4,pheno.all$dPHQ)
cor.test(pheno.all$rotationValue4,pheno.all$dPHQ)

# Imputation for RQ variables ####
library(mice)
library(PracTools)
library(pwr)
library(glmnet)
lambda_seq <- 10^seq(2, -2, by = -.1)
set.seed(301)
pheno.all.first <- pheno.all[c("UserID","Year","Age.imp","Race.imp","specb.imp","sleepAve0.imp","hours0.imp","SLE0.imp","Sex.imp","PHQ0raw.imp","Neu0.imp","deprhis.imp","EFE0raw.imp","PHQtott.imp","hourst.imp","everSLE.imp","sleepAvet.imp","evererror.imp","everhelp.imp","faculFeedb4","rotationValue4")]
pheno.first.complete <- pheno.all.first[complete.cases(pheno.all.first),]

x_vars <- model.matrix(faculFeedb4~., pheno.first.complete)[,-c(1:2)]
y_var <- pheno.first.complete$faculFeedb4
cv_output <- cv.glmnet(x_vars, y_var,
                       alpha = 1, lambda = lambda_seq, 
                       nfolds = 5)
coef(cv_output, s="lambda.min")
summary(glm(faculFeedb4~Year+Age.imp+specb.imp+sleepAve0.imp+hours0.imp+SLE0.imp+Neu0.imp+EFE0raw.imp+PHQtott.imp+hourst.imp+sleepAvet.imp+evererror.imp+everhelp.imp+rotationValue4,data=pheno.first.complete))
summary(glm(faculFeedb4~Year+Age.imp+specb.imp+sleepAve0.imp+Neu0.imp+EFE0raw.imp+PHQtott.imp+hourst.imp+evererror.imp+everhelp.imp+rotationValue4,data=pheno.first.complete))

x_vars <- model.matrix(rotationValue4~., pheno.first.complete)[,-c(1:2)]
y_var <- pheno.first.complete$rotationValue4
cv_output <- cv.glmnet(x_vars, y_var,
                       alpha = 1, lambda = lambda_seq, 
                       nfolds = 5)
coef(cv_output, s="lambda.min")
summary(glm(rotationValue4~Age.imp+specb.imp+Sex.imp+EFE0raw.imp+PHQtott.imp+hourst.imp+evererror.imp+everhelp.imp+faculFeedb4,data=pheno.first.complete))

md.pattern(pheno.all.first[c("faculFeedb4","rotationValue4")])
ini <- mice(pheno.all.first, maxit=0, print=F)
pred <- ini$predictorMatrix
metd <- ini$method

pred[,] <- 0
pred["faculFeedb4",c("Year","Age.imp","specb.imp","sleepAve0.imp","Neu0.imp","EFE0raw.imp","PHQtott.imp","hourst.imp","evererror.imp","everhelp.imp","rotationValue4")] <- 1
pred["rotationValue4",c("Age.imp","specb.imp","Sex.imp","EFE0raw.imp","PHQtott.imp","hourst.imp","evererror.imp","everhelp.imp","faculFeedb4")] <- 1

pheno.first.imp <- mice(pheno.all.first,method = metd,maxit=10,predictorMatrix = pred)
plot(pheno.first.imp)
stripplot(pheno.first.imp, faculFeedb4~.imp)
stripplot(pheno.first.imp, rotationValue4~.imp)

pheno.first.i <- complete(pheno.first.imp)
names(pheno.first.i)[c(20,21)] <- paste0(names(pheno.first.i)[c(20,21)],'.imp')

pheno.all <- merge(pheno.all,pheno.first.i[c("UserID","faculFeedb4.imp","rotationValue4.imp")],by="UserID",sort=F)
save(pheno.all,file='~/Dropbox (University of Michigan)/phq by year/data/phqbyyear_16965_blitimputed_weighted.RData')

# followup rate by institution ####
a=as.data.frame.matrix(table(pheno.total.i$InsStd,pheno.total.i$followup)) 
a$ins=row.names(a)
a=a %>% filter(!(ins %in% pheno.total.i$UserID))
a$total=a$`0`+a$`1`
a$flup=a$`1`/a$total
hist(a$flup)
b=a %>% filter(flup<0.7)
# AAMC distributions ####
set.seed(1985)
aamc.men=sum(aamc$men);aamc.women=sum(aamc$women)
aamc.surg=sum(aamc$surgical);aamc.nonsurg=sum(aamc$nonsurgical)
aamc.euro=sum(aamc$white);aamc.asian=sum(aamc$asian);aamc.urm=sum(aamc$urm);aamc.race=aamc.euro+aamc.asian+aamc.urm
race.matrix=as.data.frame(rmultinom(newlength,size=1,prob=c(aamc.euro/aamc.race,aamc.asian/aamc.race,aamc.urm/aamc.race)));race.matrix.ind <- as.data.frame(which(race.matrix>0,arr.ind=TRUE))
newdf <- data.frame(Sex.imp=as.factor(rbinom(newlength,size=1,prob = aamc.women/(aamc.men+aamc.women))),specb.imp=as.factor(rbinom(newlength,size=1,prob=aamc.surg/(aamc.surg+aamc.nonsurg))),Race.imp=as.factor(race.matrix.ind$row),Age.imp=rnorm(newlength,29.1,1.76))
newdf$Race.imp <- ifelse(newdf$Race.imp==1,"European",ifelse(newdf$Race.imp==2,'Asian',"Other"))

library(Rmisc)
pheno.subsets=list()
ihssvy.subsets=list()
for (y in 2007:2019){
  i=y-2006
  pheno.subsets[[i]]<- pheno.all[which(pheno.all$Year==y),]
  ihssvy.subsets[[i]] <- svydesign(ids = ~InsStd,data=pheno.subsets[[i]],weights =~finalweight.trim)
}
adjustcovs <- as.formula('~Age.imp')
predicted=data.frame(high=NA,mean=NA,low=NA);raw=data.frame(high=NA,mean=NA,low=NA)
for (y in 2007:2019){
  i=y-2006
  model.subset <- svyglm(OutcomePredict,design=ihssvy.subsets[[i]])
  pred.subset <- marginpred(model.subset,adjustfor=adjustcovs,predictat = newdf2)
  assign(paste0('model.',y),model.subset)
  assign(paste0('pred.',y),pred.subset)
  
  predicted[i,]=CI(pred.subset,0.95)
  raw[i,]=CI(pheno.all[which(pheno.all$Year==y),var.use],0.95)
}

# intra-class correlation (deprecated) ####
library(ICC)
library(ICCbin)
pheno.icc <- pheno.all[which(pheno.all$UserID!=pheno.all$InsStd),]
pheno.icc$Sex.imp.num <- ifelse(pheno.icc$Sex.imp==0,0,1)
pheno.icc$specb.imp.num <- ifelse(pheno.icc$specb.imp==0,0,1)
pheno.icc$InsStd <- factor(as.character(pheno.icc$InsStd), levels = unique(pheno.icc$InsStd))
ihsvars=c("dPHQ", #1
          "PHQ0raw.imp", #2
          "PHQtott", #3
          "Sex.imp.num",#4
          "specb.imp.num") #5
var.use=ihsvars[1];var.use
ICCbare(InsStd,var.use,pheno.icc)#0.003753794
ICCbare(Year,var.use,pheno.all)#0.005024751

ICCperyear=data.frame(year=c(2007:2019),icc=numeric(13))
for (y in c(2007:2019)){
  i=y-2006;
  ICCperyear$icc[i]=ICCbare(InsStd,var.use,pheno.icc[which(pheno.icc$Year==y),])
}
ICCest(InsStd,dPHQ,pheno.icc)
ICCest(InsStd,PHQ0raw.imp,pheno.icc)
ICCest(InsStd,PHQtott,pheno.icc)

a=as.data.frame.matrix(table(pheno.icc$InsStd,pheno.icc$specb.imp))
a.complete <- a[which(a$`0`>0 & a$`1`>0),]
pheno.icc.complete <- pheno.icc[which(pheno.icc$InsStd %in% row.names(a.complete)),]
pheno.icc.complete$specb.imp.num <- ifelse(pheno.icc.complete$specb.imp==0,0,1)
pheno.icc.complete$InsStd <- factor(as.character(pheno.icc.complete$InsStd), levels = unique(pheno.icc.complete$InsStd))
iccbin(InsStd,specb.imp.num,pheno.icc.complete,method='sim')

ICCperyear=data.frame(year=c(2007:2019),icc=numeric(13))
for (y in c(2007:2019)){
  i=y-2006;print(y)
  pheno.icc.sub=pheno.icc[which(pheno.icc$Year==y),]
  a=as.data.frame.matrix(table(pheno.icc.sub$InsStd,pheno.icc.sub$specb.imp.num))
  a.complete <- a[which(a$`0`>0 & a$`1`>0),]
  pheno.icc.sub.complete <- pheno.icc.sub[which(pheno.icc.sub$InsStd %in% row.names(a.complete)),]
  pheno.icc.sub.complete$InsStd <- factor(as.character(pheno.icc.sub.complete$InsStd), levels = unique(pheno.icc.sub.complete$InsStd))
  iccresult=iccbin(InsStd,specb.imp.num,pheno.icc.sub.complete,method='sim')
  ICCperyear$icc[i]=iccresult$estimates$ICC
  print(ICCperyear$icc[i])
}
