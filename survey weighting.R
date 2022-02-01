library(survey)
library(weights)
library(anesrake)
library(glmnet)
library(readxl)

#### Load data ####
# Enrolled subjects 
load('phqbyyear_allenrolled_blitimputed.RData');nrow(pheno.total.i)
ins0719 <- read_excel('Complete dataset - 2007 to 2019.xlsx')
pheno.total.i <- merge(pheno.total.i,ins0719[c("UserID","Year","New_Standardized_Name")],by=c("UserID","Year"),all.x = TRUE);nrow(pheno.total.i)

pheno.total.i$InsStd <- pheno.total.i$New_Standardized_Name
length(unique(pheno.total.i$InsStd))
pheno.total.i$InsStd[which(is.na(pheno.total.i$InsStd))] <- 
  pheno.total.i$UserID[which(is.na(pheno.total.i$InsStd))]
pheno.total.i$InsStd <- as.factor(pheno.total.i$InsStd)
length(unique(pheno.total.i$InsStd))

sum(!is.na(pheno.total.i$PHQtott) & !is.na(pheno.total.i$PHQtot0)) # included in original analysis
pheno.total.i$followup <- ifelse((is.na(pheno.total.i$PHQtott) | is.na(pheno.total.i$PHQtot0)),0,1)
table(pheno.total.i$followup)

ihs <- pheno.total.i[c("UserID","Year","Sex.imp","Race.imp","specb.imp","InsStd")]
names(ihs) <- c("UserID","Year","Sex","Race","specb","InsStd")

# AAMC data ####
aamc <- read.csv('Aggregated AAMC_by year and specialty.csv',stringsAsFactors = F)

#### between-cohorts weighting ####
pop.year <- aggregate(total~year,aamc,sum)
pop.year$freq <- pop.year$total / sum(pop.year$total)
# by hand (for double check)
#ihs.year <- as.data.frame(table(ihs$Year))
#ihs.year$freq <- ihs.year$Freq / sum(ihs.year$Freq)
#ihs.year$weight.year <- pop.year$freq / ihs.year$freq
# use anesrake (in use)
pop.year$fraction <- pop.year$total/sum(pop.year$total)
target <- with(pop.year,list(
  year=wpct(year,fraction)
))
survey <- ihs[c("UserID","Year")];names(survey)[2] <- "year"
survey$year <- as.factor(survey$year)
wpct(survey$year)
set.seed(2021)
raking <- anesrake(target,
                   survey,
                   survey$UserID,
                   cap = 5, # default setting with reference
                   choosemethod = "total")
raking_summary <- summary(raking)
raking_summary$year
raking_summary$weight.summary
raking_summary$general.design.effect
raking$weightvec
survey$weight.between <- raking$weightvec
unique(survey$weight.between)

#### within cohorts ####
for (year in 2007:2019){
  set.seed(2022)
  ihs.sub=ihs[which(ihs$Year==year),c("UserID","Sex","Race","specb")]
  aamc.sub <- aamc[which(aamc$year==year),]
  target.specb <- list(specb=c(aamc.sub$total/sum(aamc.sub$total)))
  names(target.specb$specb) <- levels(ihs.sub$specb)
  raking <- anesrake(target.specb,
                     ihs.sub,
                     ihs.sub$UserID,
                     cap=5,
                     choosemethod = "total",
                     pctlim = .01)
  raking_summary <- summary(raking)
  raking_summary$raking.variables
  raking_summary$specb
  raking_summary$weight.summary
  raking_summary$general.design.effect
  raking$weightvec
  ihs.sub$weight.within.specb <- raking$weightvec
  unique(ihs.sub$weight.within.specb)
  
  for (s in 0:1){
    ihs.sub2 <- ihs.sub[which(ihs.sub$specb==s),]
    aamc.sub2 <- aamc.sub[which(aamc.sub$specb==s),]
    target.sub <- list(Sex=c(aamc.sub2$men/(aamc.sub2$men+aamc.sub2$women),aamc.sub2$women/(aamc.sub2$men+aamc.sub2$women)),
                       Race=c(aamc.sub2$asian/(aamc.sub2$asian+aamc.sub2$white+aamc.sub2$urm),aamc.sub2$white/(aamc.sub2$asian+aamc.sub2$white+aamc.sub2$urm),aamc.sub2$urm/(aamc.sub2$asian+aamc.sub2$white+aamc.sub2$urm)))
    names(target.sub$Sex) <- levels(ihs.sub$Sex)
    names(target.sub$Race) <- levels(ihs.sub$Race)
    raking <- anesrake(target.sub,
                       ihs.sub2,
                       ihs.sub2$UserID,
                       cap=5,
                       choosemethod = "total",
                       pctlim = .01)
    raking_summary <- summary(raking)
    raking_summary$raking.variables
    raking_summary$Sex
    raking_summary$Race
    raking_summary$weight.summary
    raking_summary$general.design.effect
    raking$weightvec
    ihs.sub2$weight.within.sexrace <- raking$weightvec
    unique(ihs.sub2$weight.within.sexrace)
    if (s==0){
      ihs.sub.wtd <- ihs.sub2
    }
    else{
      ihs.sub.wtd <- rbind(ihs.sub.wtd,ihs.sub2)
    }
  }
  
  ihs.sub.wtd$weight.within <- ihs.sub.wtd$weight.within.specb * ihs.sub.wtd$weight.within.sexrace
  
  ihs.sub.wtd$year=year
  
  if (year==2007){
    ihs.wtd <- ihs.sub.wtd
  }
  else{
    ihs.wtd <- rbind(ihs.wtd,ihs.sub.wtd)
  }
}

#### combine post-stratification weights ####
ihs.wtd <- merge(ihs.wtd,survey,by=c("UserID","year"),sort=F)
save(ihs.wtd,file='AAMC/ihsenrolled_betweenandwithinweights.RData')

#### attrition weights ####
# LASSO for picking covariates in propensity model ####
pheno.bl.complete <- pheno.total.i[c("Year.imp","Age.imp","Race.imp","specb.imp","sleepAve0.imp","hours0.imp","SLE0.imp","Sex.imp","PHQ0raw.imp","Neu0.imp","deprhis.imp","EFE0raw.imp","followup")]
x_vars <- model.matrix(followup~., pheno.bl.complete)[,-1]
y_var <- pheno.bl.complete$followup
lambda_seq <- 10^seq(2, -2, by = -.1)
set.seed(53)
cv_output <- cv.glmnet(x_vars, y_var,
                       alpha = 1, lambda = lambda_seq, 
                       nfolds = 5,family='binomial')
coef(cv_output, s="lambda.min")

# follow-up rate each year ####
flup <- as.data.frame.matrix(table(pheno.total.i$Year,pheno.total.i$followup))
names(flup) <- c("nofollowup","followup")
flup <- within(flup,{
  total=nofollowup+followup
  fluprate=followup/total
})

# use twang package ####
library(twang)
pheno.total.i <- merge(pheno.total.i,ihs.wtd[c("UserID","weight.within","weight.between")],by="UserID",sort=F);nrow(pheno.total.i)
pheno.total.i$Year2019 <- as.factor(ifelse(pheno.total.i$Year==2019,1,0))
pscore <- ps(followup ~ Year2019+Race.imp+specb.imp+PHQ0raw.imp+Neu0.imp,data = pheno.total.i,estimand = "ATE",verbose = F)
bal.table(pscore)
pheno.total.i$attrweight <- get.weights(pscore,stop.method = 'es.mean')
summary(pheno.total.i$attrweight)
quantile(pheno.total.i$attrweight, .99)
pheno.total.i$attrweight.trim=ifelse(pheno.total.i$attrweight>quantile(pheno.total.i$attrweight, .99),quantile(pheno.total.i$attrweight, .99),pheno.total.i$attrweight)
summary(pheno.total.i$attrweight.trim)
# final weights ####
pheno.total.i <- within(pheno.total.i,{
  finalweight <- weight.within*weight.between*attrweight.trim
})
summary(pheno.total.i$finalweight)
quantile(pheno.total.i$finalweight, .99)
pheno.total.i$finalweight.trim=ifelse(pheno.total.i$finalweight>quantile(pheno.total.i$finalweight, .99),quantile(pheno.total.i$finalweight, .99),pheno.total.i$finalweight)
save(pheno.total.i,file='phqbyyear_allenrolled_blitimputed_weighted.RData')
# the trimming didn't change any followup=1 subjects, all the big ones came from followup=0's attrweight

#design effect
sum(pheno.total.i$finalweight.trim) / nrow(pheno.total.i)

design.ps <- svydesign(ids=~1,weights = ~finalweight.trim,data=pheno.total.i)
summary(svyglm(PHQ0raw.imp ~ followup, design=design.ps))
summary(svyglm(Neu0.imp ~ followup, design=design.ps))

# check balance situation after final weighting ####
yearbalance=as.data.frame(svytotal(~as.factor(Year),pheno.total.svy))
yearbalance$freq=yearbalance$total/sum(yearbalance$total)
yearbalance

specbalance = svyby(formula = ~specb.imp,by=~Year,design=pheno.all.svy,svytotal)
specbalance$surg=specbalance$specb.imp1/(specbalance$specb.imp0+specbalance$se.specb.imp1)
specbalance$nonsurg=specbalance$specb.imp0/(specbalance$specb.imp0+specbalance$se.specb.imp1)
specbalance

sexbalance = svyby(formula = ~Sex.imp,by = ~specb.imp+Year,design = pheno.all.svy,svytotal)
sexbalance$sex0 <- sexbalance$Sex.imp0/(sexbalance$Sex.imp0+sexbalance$Sex.imp1)
sexbalance$sex1 <- sexbalance$Sex.imp1/(sexbalance$Sex.imp0+sexbalance$Sex.imp1)
racebalance = svyby(formula = ~Race.imp,by = ~specb.imp+Year,design = pheno.all.svy,svytotal)
racebalance$racew <- racebalance$Race.impEuropean/(racebalance$Race.impEuropean+racebalance$Race.impAsian+racebalance$Race.impOther) 
racebalance$racea <- racebalance$Race.impAsian/(racebalance$Race.impEuropean+racebalance$Race.impAsian+racebalance$Race.impOther)
racebalance$raceu <- racebalance$Race.impOther/(racebalance$Race.impEuropean+racebalance$Race.impAsian+racebalance$Race.impOther)
View(sexbalance)
View(racebalance)
