require(mice)
require(PracTools)
require(pwr)
require(glmnet)
require(hdi)
require(nnet)
library(readxl)
set.seed(12345)
source('FY_function.R')

load('phqbyyear_allenrolled.RData')

#### Correct specialty info based on double check ####
corrected <- read_excel('Specialty was wrong.xlsx')
foundmissing <- read_excel('Found Missing Specialty.xlsx')
toupdate <- rbind(corrected[c("UserID","specb")],foundmissing[c("UserID","specb")])
names(toupdate) <- c("UserID","specb_new")
pheno.total <- merge(pheno.total,toupdate,by="UserID",all.x = TRUE);nrow(pheno.total)
pheno.total$specb[which(!is.na(pheno.total$specb_new) & (pheno.total$specb!=pheno.total$specb_new))] <- pheno.total$specb_new[which(!is.na(pheno.total$specb_new) & (pheno.total$specb!=pheno.total$specb_new))]
pheno.total$specb[which(is.na(pheno.total$specb))] <- pheno.total$specb_new[which(is.na(pheno.total$specb))]

#### remove non-eligible subjects (dental, Canadian programs, etc.)####
noneligible <- read_excel('Non-eligible.xlsx')
pheno.total <- pheno.total[which(!(pheno.total$UserID %in% noneligible$UserID)),]

#### remove quarterly variables ####
pheno.total.first <- pheno.total[c("UserID","Year","Age","Race","specb","sleepAve0","hours0","SLE0","Sex","PHQ0raw","Neu0","deprhis","EFE0raw","PHQtott","hourst","everSLE","sleepAvet","evererror","everhelp")]
pheno.total.second <- pheno.total[c("UserID","PHQ10above0","PHQtottover10","dPHQ","PHQtot0","EFE0")]

#### Decide which variable to use for imputing missing variables ####
pheno.total.first <- within(pheno.total.first,{
  Race <- as.factor(pheno.total.first$Race)
  specb <- as.factor(pheno.total.first$specb)
  SLE0 <- as.factor(pheno.total.first$SLE0)
  Sex <- as.factor(pheno.total.first$Sex)
  deprhis <- as.factor(pheno.total.first$deprhis)
  everSLE <- as.factor(pheno.total.first$everSLE)
  evererror <- as.factor(pheno.total.first$evererror)
  everhelp <- as.factor(pheno.total.first$everhelp)
})
pheno.first.complete <- pheno.total.first[complete.cases(pheno.total.first),]

lambda_seq <- 10^seq(2, -2, by = -.1)
set.seed(301)

# LASSO for Age ####
x_vars <- model.matrix(Age~., pheno.first.complete)[,-c(1:2)]
y_var <- pheno.first.complete$Age
cv_output <- cv.glmnet(x_vars, y_var,
                       alpha = 1, lambda = lambda_seq, 
                       nfolds = 5)
coef(cv_output, s="lambda.min")
summary(glm(Age~Year+Race+specb+sleepAve0+hours0+SLE0+Sex+PHQ0raw+Neu0+deprhis+EFE0raw+PHQtott+hourst+everSLE+sleepAvet+evererror+everhelp,data=pheno.first.complete))
summary(glm(Age~Year+Race+specb+sleepAve0+hours0+SLE0+Sex+Neu0+deprhis+EFE0raw+everSLE+sleepAvet+evererror+everhelp,data=pheno.first.complete))

# LASSO for Race ####
x_vars <- model.matrix(Race~., pheno.first.complete)[,-c(1:2)]
y_var <- pheno.first.complete$Race
cv_output <- cv.glmnet(x_vars, y_var,
                       alpha = 1, lambda = lambda_seq, 
                       nfolds = 5,family='multinomial')
coef(cv_output, s="lambda.min")
racemodel <- multinom(Race~Age+specb+SLE0+PHQ0raw+Neu0+deprhis+EFE0raw+everSLE+everhelp+Year+sleepAve0+Sex+hourst+sleepAvet+evererror,data=pheno.first.complete)
z <- summary(racemodel)$coefficients/summary(racemodel)$standard.errors
p <- (1-pnorm(abs(z),0,1))*2
p

# LASSO for specb ####
x_vars <- model.matrix(specb~., pheno.first.complete)[,-c(1:2)]
y_var <- pheno.first.complete$specb
cv_output <- cv.glmnet(x_vars, y_var,
                       alpha = 1, lambda = lambda_seq, 
                       nfolds = 5,family='binomial')
coef(cv_output, s="lambda.min")
summary(glm(specb~Year+Age+Race+hours0+Neu0+PHQtott+hourst+sleepAvet+evererror,data=pheno.first.complete,family = binomial(link='logit')))

# LASSO for sleepAve0 ####
x_vars <- model.matrix(sleepAve0~., pheno.first.complete)[,-c(1:2)]
y_var <- pheno.first.complete$sleepAve0
cv_output <- cv.glmnet(x_vars, y_var,
                       alpha = 1, lambda = lambda_seq, 
                       nfolds = 5)
coef(cv_output, s="lambda.min")
summary(glm(sleepAve0~Year+Age+Race+hours0+SLE0+Sex+PHQ0raw+deprhis+EFE0raw+PHQtott+hourst+sleepAvet+everhelp,data=pheno.first.complete))
summary(glm(sleepAve0~Year+Age+Race+hours0+SLE0+Sex+PHQ0raw+deprhis+PHQtott+hourst+sleepAvet,data=pheno.first.complete))

# LASSO for hours0 ####
x_vars <- model.matrix(hours0~., pheno.first.complete)[,-c(1:2)]
y_var <- pheno.first.complete$hours0
cv_output <- cv.glmnet(x_vars, y_var,
                       alpha = 1, lambda = lambda_seq, 
                       nfolds = 5)
coef(cv_output, s="lambda.min")
summary(glm(hours0~Year+Age+Race+specb+sleepAve0+SLE0+Sex+PHQ0raw+Neu0+deprhis+EFE0raw+PHQtott+hourst+everSLE+sleepAvet+evererror,data=pheno.first.complete))
summary(glm(hours0~Year+Age+specb+sleepAve0+SLE0+PHQ0raw+Neu0+PHQtott+hourst+everSLE+sleepAvet+evererror,data=pheno.first.complete))

# LASSO for SLE0 ####
x_vars <- model.matrix(SLE0~., pheno.first.complete)[,-c(1:2)]
y_var <- pheno.first.complete$SLE0
cv_output <- cv.glmnet(x_vars, y_var,
                       alpha = 1, lambda = lambda_seq, 
                       nfolds = 5,family='binomial')
coef(cv_output, s="lambda.min")
summary(glm(SLE0~Age+Race+sleepAve0+PHQ0raw+EFE0raw+PHQtott+everSLE,data=pheno.first.complete,family = binomial(link='logit')))

# LASSO for Sex ####
x_vars <- model.matrix(Sex~., pheno.first.complete)[,-c(1:2)]
y_var <- pheno.first.complete$Sex
cv_output <- cv.glmnet(x_vars, y_var,
                       alpha = 1, lambda = lambda_seq, 
                       nfolds = 5,family='binomial')
coef(cv_output, s="lambda.min")
summary(glm(Sex~Age+Race+specb+sleepAve0+PHQ0raw+Neu0+EFE0raw+sleepAvet+evererror+everhelp,data=pheno.first.complete,family = binomial(link='logit')))

# LASSO for PHQ0raw ####
x_vars <- model.matrix(PHQ0raw~., pheno.first.complete)[,-c(1:2)]
y_var <- pheno.first.complete$PHQ0raw
cv_output <- cv.glmnet(x_vars, y_var,
                       alpha = 1, lambda = lambda_seq, 
                       nfolds = 5)
coef(cv_output, s="lambda.min")
summary(glm(PHQ0raw~Year+Age+Race+sleepAve0+hours0+SLE0+Sex+Neu0+deprhis+EFE0raw+PHQtott+hourst+everSLE+sleepAvet+evererror+everhelp,data=pheno.first.complete))
summary(glm(PHQ0raw~Year+Race+sleepAve0+hours0+SLE0+Sex+Neu0+deprhis+EFE0raw+PHQtott+hourst+evererror,data=pheno.first.complete))

# LASSO for Neu0 ####
x_vars <- model.matrix(Neu0~., pheno.first.complete)[,-c(1:2)]
y_var <- pheno.first.complete$Neu0
cv_output <- cv.glmnet(x_vars, y_var,
                       alpha = 1, lambda = lambda_seq, 
                       nfolds = 5)
coef(cv_output, s="lambda.min")
summary(glm(Neu0~Year+Age+Race+specb+sleepAve0+hours0+SLE0+Sex+PHQ0raw+deprhis+EFE0raw+PHQtott+hourst+everSLE+sleepAvet+evererror+everhelp,data=pheno.first.complete))
summary(glm(Neu0~Year+Age+Race+specb+hours0+Sex+PHQ0raw+deprhis+EFE0raw+PHQtott+hourst+everSLE+sleepAvet+evererror+everhelp,data=pheno.first.complete))

# LASSO for deprhis ####
x_vars <- model.matrix(deprhis~., pheno.first.complete)[,-c(1:2)]
y_var <- pheno.first.complete$deprhis
cv_output <- cv.glmnet(x_vars, y_var,
                       alpha = 1, lambda = lambda_seq, 
                       nfolds = 5,family='binomial')
coef(cv_output, s="lambda.min")
summary(glm(deprhis~Age+Race+sleepAve0+PHQ0raw+Neu0+EFE0raw+PHQtott+hourst+everSLE+everhelp,data=pheno.first.complete,family = binomial(link='logit')))

# LASSO for EFE0raw ####
x_vars <- model.matrix(EFE0raw~., pheno.first.complete)[,-c(1:2)]
y_var <- pheno.first.complete$EFE0raw
cv_output <- cv.glmnet(x_vars, y_var,
                       alpha = 1, lambda = lambda_seq, 
                       nfolds = 5)
coef(cv_output, s="lambda.min")
summary(glm(EFE0raw~Year+Age+Race+specb+sleepAve0+hours0+SLE0+Sex+PHQ0raw+Neu0+deprhis+PHQtott+hourst+everSLE+sleepAvet+evererror+everhelp,data=pheno.first.complete))
summary(glm(EFE0raw~Age+Race+hours0+SLE0+Sex+PHQ0raw+Neu0+deprhis+PHQtott+everSLE,data=pheno.first.complete))

# LASSO for PHQtott ####
x_vars <- model.matrix(PHQtott~., pheno.first.complete)[,-c(1:2)]
y_var <- pheno.first.complete$PHQtott
cv_output <- cv.glmnet(x_vars, y_var,
                       alpha = 1, lambda = lambda_seq, 
                       nfolds = 5)
coef(cv_output, s="lambda.min")
summary(glm(PHQtott~Year+Age+Race+specb+sleepAve0+hours0+SLE0+Sex+PHQ0raw+Neu0+deprhis+EFE0raw+hourst+everSLE+sleepAvet+evererror+everhelp,data=pheno.first.complete))
summary(glm(PHQtott~Year+specb+sleepAve0+hours0+Sex+PHQ0raw+Neu0+deprhis+EFE0raw+hourst+everSLE+sleepAvet+evererror+everhelp,data=pheno.first.complete))

# LASSO for hourst ####
x_vars <- model.matrix(hourst~., pheno.first.complete)[,-c(1:2)]
y_var <- pheno.first.complete$hourst
cv_output <- cv.glmnet(x_vars, y_var,
                       alpha = 1, lambda = lambda_seq, 
                       nfolds = 5)
coef(cv_output, s="lambda.min")
summary(glm(hourst~Year+Age+Race+specb+sleepAve0+hours0+Sex+PHQ0raw+Neu0+deprhis+EFE0raw+PHQtott+everSLE+sleepAvet+evererror+everhelp,data=pheno.first.complete))
summary(glm(hourst~Year+Race+specb+sleepAve0+hours0+PHQ0raw+Neu0+deprhis+PHQtott+everSLE+sleepAvet+everhelp,data=pheno.first.complete))

# LASSO for everSLE ####
x_vars <- model.matrix(everSLE~., pheno.first.complete)[,-c(1:2)]
y_var <- pheno.first.complete$everSLE
cv_output <- cv.glmnet(x_vars, y_var,
                       alpha = 1, lambda = lambda_seq, 
                       nfolds = 5,family='binomial')
coef(cv_output, s="lambda.min")
summary(glm(everSLE~Year+Age+Race+hours0+SLE0+PHQ0raw+deprhis+EFE0raw+PHQtott+sleepAvet+evererror+everhelp,data=pheno.first.complete,family = binomial(link='logit')))
summary(glm(everSLE~Year+Age+Race+hours0+SLE0+deprhis+EFE0raw+PHQtott+sleepAvet+evererror+everhelp,data=pheno.first.complete,family = binomial(link='logit')))

# LASSO for sleepAvet ####
x_vars <- model.matrix(sleepAvet~., pheno.first.complete)[,-c(1:2)]
y_var <- pheno.first.complete$sleepAvet
cv_output <- cv.glmnet(x_vars, y_var,
                       alpha = 1, lambda = lambda_seq, 
                       nfolds = 5)
coef(cv_output, s="lambda.min")
summary(glm(sleepAvet~Year+Age+Race+specb+sleepAve0+hours0+Sex+Neu0+EFE0raw+PHQtott+hourst+everSLE+everhelp,data=pheno.first.complete))
summary(glm(sleepAvet~Age+Race+specb+sleepAve0+hours0+Sex+Neu0+PHQtott+hourst+everSLE+everhelp,data=pheno.first.complete))

# LASSO for evererror ####
x_vars <- model.matrix(evererror~., pheno.first.complete)[,-c(1:2)]
y_var <- pheno.first.complete$evererror
cv_output <- cv.glmnet(x_vars, y_var,
                       alpha = 1, lambda = lambda_seq, 
                       nfolds = 5,family='binomial')
coef(cv_output, s="lambda.min")
summary(glm(evererror~Race+specb+hours0+Sex+Neu0+PHQtott+everSLE,data=pheno.first.complete,family = binomial(link='logit')))

# LASSO for everhelp ####
x_vars <- model.matrix(everhelp~., pheno.first.complete)[,-c(1:2)]
y_var <- pheno.first.complete$everhelp
cv_output <- cv.glmnet(x_vars, y_var,
                       alpha = 1, lambda = lambda_seq, 
                       nfolds = 5,family='binomial')
coef(cv_output, s="lambda.min")
summary(glm(everhelp~Year+Age+Race+specb+Sex+Neu0+deprhis+PHQtott+hourst+everSLE+sleepAvet,data=pheno.first.complete,family = binomial(link='logit')))

#### imputing missing variables ####
md.pattern(pheno.total.first)
ini <- mice(pheno.total.first, maxit=0, print=F)
pred <- ini$predictorMatrix
metd <- ini$method

pred[,] <- 0
pred["Age",c("Year","Race","specb","sleepAve0","hours0","SLE0","Sex","Neu0","deprhis","EFE0raw","everSLE","sleepAvet","evererror","everhelp")] <- 1
pred["Race",c("Age","specb","SLE0","PHQ0raw","Neu0","deprhis","EFE0raw","everSLE","everhelp","Year","sleepAve0","Sex","hourst","sleepAvet","evererror")] <- 1
pred["specb",c("Year","Age","Race","hours0","Neu0","PHQtott","hourst","sleepAvet","evererror")] <- 1
pred["sleepAve0",c("Year","Age","Race","hours0","SLE0","Sex","PHQ0raw","deprhis","PHQtott","hourst","sleepAvet")] <- 1
pred["hours0",c("Year","Age","specb","sleepAve0","SLE0","PHQ0raw","Neu0","PHQtott","hourst","everSLE","sleepAvet","evererror")] <- 1
pred["SLE0",c("Age","Race","sleepAve0","PHQ0raw","EFE0raw","PHQtott","everSLE")] <- 1
pred["Sex",c("Age","Race","specb","sleepAve0","PHQ0raw","Neu0","EFE0raw","sleepAvet","evererror","everhelp")] <- 1
pred["PHQ0raw",c("Year","Race","sleepAve0","hours0","SLE0","Sex","Neu0","deprhis","EFE0raw","PHQtott","hourst","evererror")] <- 1
pred["Neu0",c("Year","Age","Race","specb","hours0","Sex","PHQ0raw","deprhis","EFE0raw","PHQtott","hourst","everSLE","sleepAvet","evererror","everhelp")] <- 1
pred["deprhis",c("Age","Race","sleepAve0","PHQ0raw","Neu0","EFE0raw","PHQtott","hourst","everSLE","everhelp")] <- 1
pred["EFE0raw",c("Age","Race","hours0","SLE0","Sex","PHQ0raw","Neu0","deprhis","PHQtott","everSLE")] <- 1
pred["PHQtott",c("Year","specb","sleepAve0","hours0","PHQ0raw","Neu0","deprhis","EFE0raw","hourst","everSLE","sleepAvet","evererror","everhelp")] <- 1
pred["hourst",c("Year","Race","specb","sleepAve0","hours0","PHQ0raw","Neu0","deprhis","PHQtott","everSLE","sleepAvet","everhelp")] <- 1
pred["everSLE",c("Year","Age","Race","hours0","SLE0","deprhis","EFE0raw","PHQtott","sleepAvet","evererror","everhelp")] <- 1
pred["sleepAvet",c("Age","Race","specb","sleepAve0","hours0","Sex","Neu0","PHQtott","hourst","everSLE","everhelp")] <- 1
pred["evererror",c("Race","specb","hours0","Sex","Neu0","PHQtott","everSLE")] <- 1
pred["everhelp",c("Year","Age","Race","specb","Sex","Neu0","deprhis","PHQtott","hourst","everSLE","sleepAvet")] <- 1

pheno.first.imp <- mice(pheno.total.first,method = metd,maxit=10,predictorMatrix = pred)
plot(pheno.first.imp)
stripplot(pheno.first.imp, Age~.imp)
stripplot(pheno.first.imp, Race~.imp)
stripplot(pheno.first.imp, specb~.imp)
stripplot(pheno.first.imp, sleepAve0~.imp)
stripplot(pheno.first.imp, hours0~.imp)
stripplot(pheno.first.imp, SLE0~.imp)
stripplot(pheno.first.imp, Sex~.imp)
stripplot(pheno.first.imp, PHQ0raw~.imp)
stripplot(pheno.first.imp, Neu0~.imp)
stripplot(pheno.first.imp, deprhis~.imp)
stripplot(pheno.first.imp, EFE0raw~.imp)
stripplot(pheno.first.imp, PHQtott~.imp)
stripplot(pheno.first.imp, hourst~.imp)
stripplot(pheno.first.imp, everSLE~.imp)
stripplot(pheno.first.imp, sleepAvet~.imp)
stripplot(pheno.first.imp,evererror~.imp)
stripplot(pheno.first.imp, everhelp~.imp)

pheno.first.i <- complete(pheno.first.imp)
names(pheno.first.i) <- paste0(names(pheno.first.i),'.imp')

pheno.total.i <- cbind(pheno.total,pheno.first.i)
sum(pheno.total.i$UserID==pheno.total.i$UserID.imp);nrow(pheno.total.i)
pheno.total.i <- within(pheno.total.i,{
  dPHQ.imp <- PHQtott.imp - PHQ0raw.imp
  PHQtot0.imp <- inversenormal(PHQ0raw.imp)
  EFE0.imp <- inversenormal(EFE0raw.imp)
})
save(pheno.total.i,file='~/Box/phq by year/data/phqbyyear_allenrolled_blitimputed.RData')

## Below was not used in the formal analysis ####
#### propensity class ####
pheno.total.i$followup <- ifelse(is.na(pheno.total.i$PHQtott),0,1)
summary(glm(followup ~ Age.imp + Sex.imp + PHQ0raw.imp + Neu0.imp + deprhis.imp + EFE0raw.imp + hours0.imp + sleepAve0.imp + specb.imp + SLE0.imp + Year,pheno.total.i,family = binomial(link='logit')))
out <- pclass(formula = followup ~ Sex.imp + PHQ0raw.imp + Neu0.imp + sleepAve0.imp + specb.imp + Year,
              data = pheno.total.i,
              type = 'unwtd',
              link = 'logit',
              numcl = 5)
table(out$p.class,useNA = 'always')
boxplot(out$propensities~out$p.class)
pheno.total.i$p.class <- out$p.class
pheno.total.i$propensity <- out$propensities
propmedian <- aggregate(propensity~p.class,pheno.total.i,FUN=median)
propmedian$wt <- 1/propmedian$propensity

pheno.total.i=subset(pheno.total.i,select=-c(wt))
pheno.total.i <- merge(pheno.total.i,propmedian[c("p.class","wt")],by='p.class')
save(pheno.total.i,file='phqbyyear_allenrolled_blimputed.RData')

#### repeat the analysis ####
load('phqbyyear_allenrolled_blimputed.RData')
pheno <- pheno.total.i[which(!is.na(pheno.total.i$PHQtott)),]
pheno <- within(pheno,{
  PHQtot0.imp <- inversenormal(PHQ0raw.imp)
  dPHQ.imp <- PHQtott - PHQ0raw.imp
  wAge <- Age.imp*wt})

# baseline PHQ increase ####
aggregate(PHQ0raw.imp*wt ~ Year, pheno, mean)
summary(m1 <- lm(PHQtot0.imp*wt ~ wAge + Year, pheno))
confint(m1,'Year',0.95)
# dPHQ ####
aggregate(dPHQ.imp*wt ~ Year, pheno, mean)
aggregate(dPHQ.imp ~ Year,pheno,sd)
summary(model1 <- lm(dPHQ.imp*wt ~ wAge + Year, pheno))
confint(model1,'Year',level=0.95)
# group difference ####
summary(m2 <- lm(dPHQ.imp ~ Age.imp + Year, pheno[which(pheno$Sex.imp==1),]))
summary(m2 <- lm(dPHQ.imp ~ Age.imp + Year, pheno[which(pheno$Sex.imp==0),]))
summary(m2 <- lm(dPHQ.imp ~ Age.imp + Sex.imp*scale(Year,scale = F), pheno))
confint(m2,"Sex.imp1:scale(Year, scale = F)",.95)
#specialty difference ####
summary(m2 <- lm(dPHQ.imp ~ Age.imp + Year, pheno[which(pheno$specb.imp==1),]))
summary(m2 <- lm(dPHQ.imp ~ Age.imp + Year, pheno[which(pheno$specb.imp==0),]))
summary(m2 <- lm(dPHQ.imp ~ Age.imp + specb.imp*scale(Year,scale = F), pheno))

#sample size ####
20014/34860
nrow(pheno)
nrow(pheno)/20014
mean(pheno$Age.imp);sd(pheno$Age.imp)
table(pheno$Sex.imp);8780/nrow(pheno)
#ceiling effect check ####
skewcompare <- data.frame(year=numeric(13),skewt=numeric(13))
years=c(2007:2019)
for (i in 1:13){
  year=years[i]
  skewcompare$year[i]=year
  skewcompare$skewt[i]=skewness(pheno$PHQtott[which(pheno$Year==year)])
}
summary(m1 <- lm(skewt ~ year,skewcompare))
confint(m1,'year',level = 0.95)
#baseline other risk factors ####
