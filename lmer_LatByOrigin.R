###Mat FX mixed FX models, focused on Origin BY Latitude###
#Stress Tolerance, REML, using lme4
#mixed effect models 
library(lme4)

# #for each normal trait, compare this general set of models
# model1<-lmer(trait  ~ Origin* Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
# model2<-lmer(trait  ~ Origin* Latitude + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(trait  ~ Origin* Latitude + (1|blank), family=gaussian,data=modeldata) # Test population effect
# anova(model2,model1) # Mom is sig!
# anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# modelI <- lmer(trait  ~ Origin + Latitude + (1|PopID), family=family,data=modeldata)
# anova(modelI,model2)
# modelL<-lmer(trait  ~ Origin + (1|PopID), family=gaussian,data=modeldata)
# anova(modelL, model1)
# modelO<-lmer(trait ~ Latitude +(1|PopID), family=gaussian,data=modeldata)
# anova(modelO,model1) #test for significance of origin - origin only marginally sig....!
# 
# #test for one trait, one df, specify non default family
# lfcountLR<- CGtrait.LR.int("LfCount1",al, family=poisson)
# 
# #test for one trait, one df
# shootmod <- CGtrait.models.int("ShootMass.gA", al) #test one trait
# 
# #for all traits in a df
# #make sure all traits analyzed this way are the same distribution
# names(al)#find col numbers for traits of interestes
# alLR <- lapply(names(al)[8:13],function(n) CGtrait.LR.int(n,al))#apply func to all things in list
# names(alLR) <- names(al)[8:13]
# almodels <- lapply(names(al)[8:13],function(n) CGtrait.models.int(n,al))#apply func to all things in list
# names(almodels) <- names(al)[8:13]
# 
# #to get one model
# almodels[[1]][1] #first number is trait in column order of df, second number is model number
# names(almodels[1]) #to verify trait

######Allo, Origin * Lat models######
mfallo.dk<-read.table("MFallo.dk.txt", header=T, sep="\t", quote='"', row.names=1) #allo, dk only
head(mfallo.dk)
#mfallo.dk$lxw <- mfallo.dk$LfLgth1 * mfallo.dk$LfWdth1
str(mfallo.dk)
alLR <- lapply(names(mfallo.dk)[c(10:11)],function(n) CGtrait.LR.int(n,mfallo.dk)) #crow, shoot, all gaussian
#names(alLR) <- names(mfallo.dk)[c(11:13, 20)]
alLR #check out LRs of models. Model progression logical?
almodels <- CGtrait.models.int("Mass.gA",mfallo.dk)
almodels

#allo, shoot mass
modeldata<-mfallo.dk[!is.na(mfallo.dk$Mass.gA),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(Mass.gA ~ Origin *Latitude+(1|PopID/Mom), family=gaussian,data=modeldata)
model2<-lmer(Mass.gA ~ Origin *Latitude+ (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Mass.gA ~ Origin *Latitude+ (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # Mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(0.1406,1)

modelI <- lmer(Mass.gA  ~ Origin + Latitude + (1|blank), family=gaussian,data=modeldata)
anova(modelI,model3)

modelL<-lmer(Mass.gA ~ Origin +(1|blank), family=gaussian,data=modeldata)
anova(modelL, modelI)

modelO<-lmer(Mass.gA ~ (1|blank), family=gaussian,data=modeldata)
anova(modelO,modelL) #test for significance of origin - origin not sig....?
#Mom and popID sig, but not Origin! for either log or raw data

#try glm
modelg <- glm(Mass.gA ~ Origin*Latitude, family=gaussian,data=modeldata)
modelg1 <- glm(Mass.gA ~ Origin+Latitude, family=gaussian,data=modeldata)
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq(0.050583, 1)

modelg3<- glm(Mass.gA ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1)
1-pchisq(0.075716, 1)
anova(modelg3)
# modelg2<- glm(Mass.gA ~ Latitude, family=gaussian,data=modeldata)
# anova(modelg2,modelg1)
1-pchisq(0.75479, 1)

lsmeans(modelg3, ~Origin, conf=95)

p1 <- ggplot(modeldata,aes(Trt, Mass.gA, fill=Origin))+
  geom_boxplot()+xlab("Stress Treatment")+
  ylab("shoot mass")+ 
  theme(legend.justification=c(1,1), legend.position=c(1,1))
p1

# checking the normality of residuals e_i:
plot(resid(modelg3) ~ fitted(modelg3),main="residual plot")
abline(h=0)
qqnorm(resid(modelg3), main="Q-Q plot for residuals")
qqline(resid(modelg3))

# #mass.log
# modeldata$mass.log <- log(modeldata$Mass.gA)
# #try glm
# modelg <- glm(mass.log ~ Origin*Latitude, family=gaussian,data=modeldata)
# modelg1 <- glm(mass.log ~ Origin+Latitude, family=gaussian,data=modeldata)
# anova(modelg1, modelg) #'Deviance' is chisq value
# 1-pchisq(0.050583, 1)
# 
# modelg3<- glm(mass.log ~ Origin, family=gaussian,data=modeldata)
# anova(modelg3,modelg1)
# 1-pchisq(0.075716, 1)
# anova(modelg3)
# # modelg2<- glm(Mass.gA ~ Latitude, family=gaussian,data=modeldata)
# # anova(modelg2,modelg1)
# 1-pchisq(0.75479, 1)
# 
# lsmeans(modelg3, ~Origin, conf=95)
# 
# p1 <- ggplot(modeldata,aes(Trt, mass.log, fill=Origin))+
#   geom_boxplot()+xlab("Stress Treatment")+
#   ylab("shoot mass")+ 
#   theme(legend.justification=c(1,1), legend.position=c(1,1))
# p1
# 
# # checking the normality of residuals e_i:
# plot(resid(modelg3) ~ fitted(modelg3),main="residual plot")
# abline(h=0)
# qqnorm(resid(modelg3), main="Q-Q plot for residuals")
# qqline(resid(modelg3))


#allo, root crown
modeldata<-mfallo.dk[!is.na(mfallo.dk$Crown.mmA),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(Crown.mmA ~ Origin *Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(Crown.mmA ~ Origin *Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(Crown.mmA ~ Origin *Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
print(anova(model2raw,model1raw), digits=22) # mom not sig
(lambda <- (-2)*(-22.01667490716931752104 - (-22.01666587320908163861)))
1-pchisq(1.806792e-05,1)
print(anova(model2raw,model3raw), digits=22) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
(lambda <- (-2)*(-22.01667490716931752104 - (-22.36640315798491229771)))
1-pchisq(-0.6994565,1)

modelI<-lmer(Crown.mmA ~ Origin + Latitude +(1|blank), family=gaussian,data=modeldata)
print(anova(model3raw, modelI),digits=22)
(lambda <- (-2)*(-21.73459105068136665295 - (-22.01666587320908163861)))
1-pchisq(-0.5641496,1)

modelL<-lmer(Crown.mmA ~ Origin +(1|blank), family=gaussian,data=modeldata)
anova(modelL, modelI)

modelOraw<-lmer(Crown.mmA ~ (1|blank), family=gaussian,data=modeldata)
anova(modelOraw,modelL) #test for significance of origin - origin NOT sig....!

#try glm
modelg <- glm(Crown.mmA ~ Origin*Latitude, family=gaussian,data=modeldata)
modelg1 <- glm(Crown.mmA ~ Origin+Latitude, family=gaussian,data=modeldata)
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq(0.0037776, 1)

modelg3<- glm(Crown.mmA ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1)
1-pchisq(0.1535, 1)
anova(modelg3)
# modelg2<- glm(Crown.mmA ~ Latitude, family=gaussian,data=modeldata)
# anova(modelg2,modelg1)
1-pchisq(0.24012, 1)
lsmeans(modelg3, ~Origin, conf=95)

# #####m1, Origin * Lat#####
mfcom1<-read.table("MatFxBonusCtrlM1.txt", header=T, sep="\t", quote='"', row.names=1) #m1 all plants in analysis, balanced, dk only
head(mfcom1)
#mfcom1$lxw <- mfcom1$LfLgth1*mfcom1$LfWdth1
write.table(mfcom1, file="MatFxBonusCtrlM1.txt", sep="\t", quote=F)
m1lxw <- CGtrait.LR.int("lxw", mfcom1)
m1lf <- CGtrait.LR.int("LfCount1", mfcom1, family=poisson)#poisson distribution

###m1, lxw, mom sig, do by hand###
modeldata<-mfcom1[!is.na(mfcom1$lxw),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(lxw ~ Origin *Latitude+(1|PopID/Mom), family=gaussian,data=modeldata)
model2<-lmer(lxw ~ Origin *Latitude+ (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lxw ~ Origin *Latitude+ (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # Mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(16.094, 1)

modelI <- lmer(lxw  ~ Origin + Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
print(anova(modelI,model1), digits=22)
(lambda <- (-2)*(-1042.731492149016730764 - (-1042.976122705798161405)))
1-pchisq(-0.4892611, 1)

modelL<-lmer(lxw ~ Origin +(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelL, modelI)

modelO<-lmer(lxw ~ (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelO,modelL) #test for significance of origin - origin not sig....?
#Mom and popID sig, but not Origin! for either log or raw data
lsmeans(modelL, ~Origin, conf=95)

###m1, lfcount, mom sig, do by hand###
modeldata<-mfcom1[!is.na(mfcom1$LfCount1),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(LfCount1 ~ Origin *Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
model2raw<-lmer(LfCount1 ~ Origin *Latitude+(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(LfCount1 ~ Origin *Latitude+(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # Mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(1.4497,1)
modelI <- lmer(LfCount1  ~ Origin + Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelI,model1raw)

modelL<-lmer(LfCount1 ~ Origin +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelL, modelI)

modelO<-lmer(LfCount1 ~ (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelL, modelO)

modelOraw<-lmer(LfCount1 ~ Latitude+(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelOraw,modelO) #test for significance of origin - origin NOT sig....!
model1raw
int<-5.27022 #inv mean
B<--4.79533 #Originnat estimate from model summary
pI<-exp(int)
pN<-exp(int+B)
pI
pN

CI.LS.poisson(modelI) #exclude sig interactions

####Control, Origin * Lat####
mfco.dk1<-read.table("MatFxBonusCtrl.txt", header=T, sep="\t", quote='"', row.names=1) #largest balanced control
head(mfco.dk1)
#mfco.dk1$lxwH <- mfco.dk1$LfLgthH * mfco.dk1$LfWdthH

#merge mf ctrl and mom df
str(mfmom.dk)
str(mfco.dk1)
totmf <- merge(mfmom.dk,mfco.dk1, all=TRUE )
str(totmf)
#tidy
totmf <- totmf[,-c(16:18,26,29:30, 32:38, 40:44, 46:57)]
totmf <- totmf[!is.na(totmf$Trt),]
totmf$Exp<-droplevels(totmf$Exp)
totmf$Trt<-droplevels(totmf$Trt)
levels(totmf$PopID)

coLR <- lapply(names(mfco.dk1)[c(12:13,21:22,41, 44)],function(n) CGtrait.LR.int(n,mfco.dk1)) #crow, shoot, root, RootH.log, lxw, all gaussian
#names(coLR) <- names(mfco.dk1)[c(15:17,52:53)]
coLR #check out LRs of models. Model progression logical?
xtabs(~Origin+BoltedatH, mfco.dk1)

# mfco.dk1$bolt.bin <- as.numeric(mfco.dk1$BoltedatH)-1
# mfco.dk1$BoltDay.adj <- mfco.dk1$BoltDay + 3
# write.table(mfco.dk1, file="MatFxBonusCtrl.txt", sep="\t", quote=F)
coBatH <- CGtrait.LR.int("bolt.bin", totmf, family=binomial)
comodels <- CGtrait.models.int("bolt.bin", mfco.dk1, family=binomial)
int<-9.5348 #inv mean
B<--39.8539 #Originnat estimate from model summary
pN<-exp(int+B)/(exp(int+B)+1) # Native
pI<-exp(int)/(exp(int)+1) # Introduced (B=0)
pI 
pN

coP <- lapply(names(mfco.dk1)[c(11,46)],function(n) CGtrait.LR.int(n,mfco.dk1, family=poisson)) #lfcountH, boltdate, all poisson

#control, shootmass
modeldata<-mfco.dk1[!is.na(mfco.dk1$ShootMass.g),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(ShootMass.g ~ Origin *Latitude+(1|PopID/Mom), family=gaussian,data=modeldata)
model2<-lmer(ShootMass.g ~ Origin *Latitude+ (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(ShootMass.g ~ Origin *Latitude+ (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # Mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(1.3743,1)

modelI <- lmer(ShootMass.g  ~ Origin + Latitude + (1|blank), family=gaussian,data=modeldata)
anova(modelI,model3)

modelL<-lmer(ShootMass.g ~ Origin +(1|blank), family=gaussian,data=modeldata)
anova(modelL, modelI)

modelO<-lmer(ShootMass.g ~ (1|blank), family=gaussian,data=modeldata)
anova(modelO,modelL) #test for significance of origin - origin not sig....?
#try glm
modelg <- glm(ShootMass.g ~ Origin*Latitude, family=gaussian,data=modeldata)
modelg1 <- glm(ShootMass.g ~ Origin+Latitude, family=gaussian,data=modeldata)
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq(0.29711, 1)

modelg3<- glm(ShootMass.g ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1)
1-pchisq(2.919, 1)
modelg2<- glm(ShootMass.g ~ Latitude, family=gaussian,data=modeldata)
anova(modelg3)
1-pchisq(4.5969, 1)

lsmeans(modelg3, ~Origin, conf=95)

####control, lxw, mom sig, so do by hand####
modeldata<-mfco.dk1[!is.na(mfco.dk1$lxwH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(lxwH ~ Origin *Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(lxwH ~ Origin *Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(lxwH ~ Origin *Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # Mom not sig
print(anova(model3raw,model2raw), digits=22) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
(lambda <- (-2)*(-571.8468485550226887426 - (-571.2004667420127361765)))
1-pchisq(1.292764,1)

modelI <- lmer(lxwH ~ Origin +Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelI, model1raw)

modelL<-lmer(lxwH ~ Origin +(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelL, modelI)

modelOraw<-lmer(lxwH ~ (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelOraw,modelL) #test for significance of origin - origin NOT sig....!

lsmeans(modelL,~Origin, conf=95)

####control, lf count, mom sig so do by hand#####
#poisson on raw data
modeldata<-totmf[!is.na(totmf$LfCountH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(LfCountH ~ Origin *Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
model2raw<-lmer(LfCountH ~Origin *Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(LfCountH ~ Origin *Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # Mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(4.4799,1)
modelI <-lmer(LfCountH ~ Origin +Latitude +(1|PopID/Mom), family=poisson,data=modeldata) 
anova(modelI,model1raw)

modelL<-lmer(LfCountH ~ Origin +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelL, modelI)

modelOraw<-lmer(LfCountH ~(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelOraw,modelL) #test for significance of origin - origin NOT sig....!
modelL
int<-3.0162#inv mean
B<--0.5529#Originnat estimate from model summary
pI<-exp(int)
pN<-exp(int+B)
pI
pN
CI.LS.poisson(modelL)

###control, BatH###
modeldata<-totmf[!is.na(totmf$bolt.bin),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(bolt.bin ~ Origin * Latitude +(1|PopID/Mom), family=binomial,data=modeldata)
model2<-lmer(bolt.bin ~ Origin * Latitude +(1|PopID), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin ~ Origin * Latitude +(1|blank), family=binomial,data=modeldata) # Test population effect
anova(model2,model1) # Mom sig
print(anova(model3,model2),digits=22) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
(lambda <- (-2)*(-55.85861292662346500038 - (-55.85861291849705168033)))
1-pchisq(1.625283e-08,1)
# model4<-lmer(bolt.bin ~ Origin * Latitude +(1|blank/PopID), family=binomial,data=modeldata)
# anova(model4,model2)

modelI<-lmer(bolt.bin ~ Origin + Latitude +(1|blank), family=binomial,data=modeldata)
anova(modelI, model3)
# modelI.2<-lmer(bolt.bin ~ Origin + Latitude +(1|blank/PopID), family=binomial,data=modeldata)
# anova(modelI.2, model4)

modelL<-lmer(bolt.bin ~ Origin + (1|blank), family=binomial,data=modeldata)
anova(modelL, modelI)

modelO<-lmer(bolt.bin ~  (1|blank), family=binomial,data=modeldata)
anova(modelO,modelL) #test for significance of origin - origin not sig!
modelL
# anova(model1raw,ddf="Kenward-Roger")

int<--1.7707 #inv mean
B<-2.2060#Originnat estimate from model summary
pN<-exp(int+B)/(exp(int+B)+1) # Native
# Note that if Origin was a continuous variable you would substitute B with B*Origin
pI<-exp(int)/(exp(int)+1)# Introduced (B=0)
pI # 14.5% 
pN # 60%
#report effect size as separate percentages, difference in percentage, or log-odds ratio: log(pI/pN)
#could also include the standard errors in equations to add upper/lower confidence intervals.
#check by looking at percentages
summary(mfco.dk1[mfco.dk1$Origin=="nat",]) #56 rows, 34 boltedatH = 60%
summary(mfco.dk1[mfco.dk1$Origin=="inv",]) #55 rows, 8 boltedatH = 14.5%

#try glm
modelg <- glm(bolt.bin ~ Origin*Latitude, family=binomial,data=modeldata)
modelg1 <- glm(bolt.bin ~ Origin+Latitude, family=binomial,data=modeldata)
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq(5.7821, 1)

modelg3<- glm(bolt.bin ~ Origin, family=binomial,data=modeldata)
anova(modelg3,modelg1)
1-pchisq(5.5154, 1)
anova(modelg3)
# modelg2<- glm(bolt.bin ~ Latitude, family=binomial,data=modeldata)
# anova(modelg2,modelg1)
# 1-pchisq(9.0533, 1)

CI.LS.binomial(modelg1) #exclude sig int

###control, boltday.adj, cross sig, do by hand###
#only bolters
modeldata<-mfco.dk1[!is.na(mfco.dk1$BoltDay.adj),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(BoltDay.adj ~ Origin * Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
model2<-lmer(BoltDay.adj ~ Origin * Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(BoltDay.adj ~ Origin * Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # Mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# (lambda <- (-2)*(logLik1 - (logLik2)))
1-pchisq(1.8168,1)

modelI<-lmer(BoltDay.adj ~ Origin + Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelI, model1)

modelL<-lmer(BoltDay.adj ~ Origin + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelL, modelI)

modelO<-lmer(BoltDay.adj ~  (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelO,modelL) #test for significance of origin - origin not sig!
modelL
int<-4.26642#inv mean
B<--0.17859#Originnat estimate from model summary
pI<-exp(int)
pN<-exp(int+B)
pI
pN
CI.LS.poisson(modelL)

# ###control, crown, cross sig, do by hand###
modeldata<-mfco.dk1[!is.na(mfco.dk1$CrownDiam.mm),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(CrownDiam.mm ~ Origin *Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(CrownDiam.mm ~ Origin *Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(CrownDiam.mm ~ Origin *Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model2raw,model3raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(0.8185,1)

modelI<-lmer(CrownDiam.mm ~ Origin + Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
anova(model1raw, modelI)

modelL<-lmer(CrownDiam.mm ~ Origin +(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelL, modelI)

modelOraw<-lmer(CrownDiam.mm ~ (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelOraw,modelL) #test for significance of origin - origin NOT sig....!

lsmeans(modelL, ~Origin, conf=95)

###control, sla, cross sig, do by hand###
#ratios should be log transformed
modeldata<-mfco.dk1[!is.na(mfco.dk1$sla),]
modeldata$sla.log <- log(modeldata$sla)
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(sla.log ~ Origin *Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(sla.log ~ Origin *Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(sla.log ~ Origin *Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
print(anova(model2raw,model3raw), digits=22) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
(lambda <- (-2)*(-2.578845030579588026853 - (-2.730199207582871601119)))
1-pchisq(-0.3027084,1)

modelI <- lmer(sla.log ~ Origin +Latitude +(1|blank), family=gaussian,data=modeldata)
anova(modelI, model3raw)

modelL<-lmer(sla.log ~ Origin +(1|blank), family=gaussian,data=modeldata)
anova(modelL, modelI)

modelOraw<-lmer(sla.log ~ (1|blank), family=gaussian,data=modeldata)
anova(modelOraw,modelL) #test for significance of origin - origin NOT sig....!

#try glm
modelg <- glm(sla.log ~ Origin*Latitude, family=gaussian,data=modeldata)
modelg1 <- glm(sla.log ~ Origin+Latitude, family=gaussian,data=modeldata)
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq(0.17795, 1)

modelg3<- glm(sla.log ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1)
1-pchisq(0.024404, 1)
anova(modelg3)
# modelg2<- glm(sla.log ~ Latitude, family=gaussian,data=modeldata)
# anova(modelg2,modelg1)
1-pchisq(9.0533, 1)
CI.LS.poisson(modelg3)

####Nut def, Origin + Lat####
mfn.dk<-read.table("MatFxNut.dk.txt", header=T, sep="\t", quote='"', row.names=1) #nut, dk only
head(mfn.dk)
xtabs(~Origin+BoltedatH, mfn.dk) # only one bolter... leaving in
# modeldata<-modeldata[modeldata$BoltedatH!="y",]#remove 7 bolted plants from nat SHOULD I???
# mfn.dk$lxwH <- mfn.dk$LfLgthH * mfn.dk$LfWdthH
# mfn.dk$bolt.bin <- as.numeric(mfn.dk$BoltedatH)-1
# mfn.dk$BoltDay.adj <- mfco.dk1$BoltDay + 3
# write.table(mfn.dk, file="MatFxNut.dk.txt", sep="\t", quote=F)

#merge mfn and mom df
str(mfmom.dk)
str(mfn.dk)
totmfn <- merge(mfmom.dk,mfn.dk, all.y=TRUE )
str(totmfn)
#tidy
# totmfn <- totmfn[,-c(16:18,26,29:30, 32:38, 40:44, 46:57)]
totmfn$Exp<-droplevels(totmfn$Exp)
totmfn$Trt<-droplevels(totmfn$Trt)
levels(totmfn$PopID)

nLR <- lapply(names(totmfn)[c(44:45,51)],function(n) CGtrait.LR.int(n, totmfn)) 
#lflgthH, lfwdthH, crown, shoot, lxwH, all gaussian
# names(nLR) <- names(al)[c(11:12, 15:17, 50:51)]
nLR #check out LRs of models. Model progression logical?
# nmodels <- CGtrait.models.int("RootMass.g",mfn.dk)
# nmodels2 <- CGtrait.models.int("RootH.log",mfn.dk)
# nmodels
# nRoot.lmer <- nmodels$model2
# nRootlog.lmer <- nmodels2$model2
# qqnorm(resid(nRootlog.lmer), main="Q-Q plot for residuals")
# qqline(resid(nRootlog.lmer))

#non-gaussian?
# mfn.dk <- cbind(mfn.dk, bolt.bin=as.numeric(mfn.dk$BoltedatH)-1)
# write.table(n, file="STNutsubset.txt", sep="\t", quote=F)
# 
# #nBatH <- CGtrait.LR.int("bolt.bin", mfn.dk, family=binomial)# no invasive bolted!
nlfcount <- CGtrait.LR.int("LfCountH",mfn.dk, family=poisson) #lfcountH, all poisson

###nut, lxw###
modeldata<-totmfn[!is.na(totmfn$lxwH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(lxwH ~ Origin *Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(lxwH ~ Origin *Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(lxwH ~ Origin *Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
print(anova(model2raw,model1raw),digits=22) # Mom not sig
(lambda <- (-2)*(-109.0070901470973012692 - (-109.0071559301389783059)))
1-pchisq(-0.0001315661,1)
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(0.1311,1)

modelI <- lmer(lxwH ~ Origin +Latitude +(1|blank), family=gaussian,data=modeldata)
anova(modelI, model3raw)

modelL<-lmer(lxwH ~ Origin +(1|blank), family=gaussian,data=modeldata)
anova(modelL, modelI)

modelOraw<-lmer(lxwH ~ (1|blank), family=gaussian,data=modeldata)
anova(modelOraw,modelL)

#try glm
# modeldata <- modeldata[modeldata$lxwH<111,] #try without outlier
modelg <- glm(lxwH ~ Origin*Latitude, family=gaussian,data=modeldata)
modelg1 <- glm(lxwH ~ Origin+Latitude, family=gaussian,data=modeldata)
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq(776.89, 1)
1-pchisq(78.216, 1)

modelg3<- glm(lxwH ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1)
1-pchisq(668.06, 1)
# anova(modelg3)
modelg2<- glm(lxwH ~ Latitude, family=gaussian,data=modeldata)
anova(modelg2,modelg1)
1-pchisq(3059.3, 1)

lsmeans(modelg1, ~Origin, conf=95)

# checking the normality of residuals e_i:
qqnorm(resid(modelg), main="Q-Q plot for residuals")
qqline(resid(modelg))

##nut, shoot mass
modeldata<-totmfn[!is.na(totmfn$ShootMass.g),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(ShootMass.g ~ Origin *Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(ShootMass.g ~ Origin *Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(ShootMass.g ~ Origin *Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
print(anova(model2raw,model1raw),digits=22) # Mom not sig
(lambda <- (-2)*(-20.56186352173015308153 - (-20.95029136041199535612)))
1-pchisq(-0.7768557,1)
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(1.2991,1)

modelI <- lmer(ShootMass.g ~ Origin +Latitude +(1|blank), family=gaussian,data=modeldata)
anova(modelI, model3raw)

modelL<-lmer(ShootMass.g ~ Origin +(1|blank), family=gaussian,data=modeldata)
anova(modelL, modelI)

modelOraw<-lmer(ShootMass.g ~ (1|blank), family=gaussian,data=modeldata)
anova(modelOraw,modelL)

#try glm
modelg <- glm(ShootMass.g ~ Origin*Latitude, family=gaussian,data=modeldata)
modelg1 <- glm(ShootMass.g ~ Origin+Latitude, family=gaussian,data=modeldata)
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq(0.24717, 1)

modelg3<- glm(ShootMass.g ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1)
1-pchisq(0.0033353, 1)
anova(modelg3)
# modelg2<- glm(ShootMass.g ~ Latitude, family=gaussian,data=modeldata)
# anova(modelg2,modelg1)
1-pchisq(0.56918, 1)

lsmeans(modelg3, ~Origin, conf=95)

##nut, root crown
modeldata<-totmfn[!is.na(totmfn$CrownDiam.mm),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(CrownDiam.mm ~ Origin *Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(CrownDiam.mm ~ Origin *Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(CrownDiam.mm ~ Origin *Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
print(anova(model2raw,model1raw),digits=22) # Mom not sig
(lambda <- (-2)*(-109.0070897781463088450 - (-109.0071141940671139992)))
1-pchisq(-4.883184e-05,1)
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(0.2437,1)

modelI <- lmer(CrownDiam.mm ~ Origin +Latitude +(1|blank), family=gaussian,data=modeldata)
anova(modelI, model3raw)

modelL<-lmer(CrownDiam.mm ~ Origin +(1|blank), family=gaussian,data=modeldata)
anova(modelL, modelI)

modelOraw<-lmer(CrownDiam.mm ~ (1|blank), family=gaussian,data=modeldata)
anova(modelOraw,modelL)

#try glm
modelg <- glm(CrownDiam.mm ~ Origin*Latitude, family=gaussian,data=modeldata)
modelg1 <- glm(CrownDiam.mm ~ Origin+Latitude, family=gaussian,data=modeldata)
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq(0.069898, 1)

modelg3<- glm(CrownDiam.mm ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1)
1-pchisq(0.048201, 1)
anova(modelg3)
# modelg2<- glm(CrownDiam.mm ~ Latitude, family=gaussian,data=modeldata)
# anova(modelg2,modelg1)
1-pchisq(0.015, 1)

lsmeans(modelg3, ~Origin, conf=95)

##nut lf count
modeldata<-totmfn[!is.na(totmfn$LfCountH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(LfCountH ~ Origin *Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
model2raw<-lmer(LfCountH ~ Origin *Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(LfCountH ~ Origin *Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
print(anova(model2raw,model1raw),digits=22) # Mom not sig
(lambda <- (-2)*(-109.0070897781463088450 - (-109.0071141940671139992)))
1-pchisq(-4.883184e-05,1)
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(7.7244,1)

modelI <- lmer(LfCountH ~ Origin +Latitude +(1|PopID), family=poisson,data=modeldata)
anova(modelI, model2raw)

modelL<-lmer(LfCountH ~ Origin +(1|PopID), family=poisson,data=modeldata)
anova(modelL, modelI)

modelOraw<-lmer(LfCountH ~ (1|PopID), family=poisson,data=modeldata)
anova(modelOraw,modelL)

CI.LS.poisson(modelL)

####Cut, Origin * Lat####
mfcu.dk<-read.table("MatFxCut.dk.txt", header=T, sep="\t", quote='"', row.names=1) #cut, dk only
head(mfcu.dk)

#merge mfcu and mom df
str(mfmom.dk)
str(mfcu.dk)
totmfcu <- merge(mfmom.dk,mfcu.dk, all.y=TRUE )
str(totmfcu)
#tidy
# totmfn <- totmfn[,-c(16:18,26,29:30, 32:38, 40:44, 46:57)]
totmfcu$Exp<-droplevels(totmfcu$Exp)
totmfcu$Trt<-droplevels(totmfcu$Trt)
levels(totmfcu$PopID)

cuLR <- CGtrait.LR.int("CrownDiam.mm",totmfcu) #crown all gaussian
1-pchisq(0.3559, 1)

#non-gaussian?
# mfcu.dk<- cbind(mfcu.dk, bolt.bin=as.numeric(mfcu.dk$BoltedatH)-1)
# write.table(mfcu.dk, file="MatFxCut.dk.txt", sep="\t", quote=F)
xtabs(~Origin+BoltedatH, mfcu.dk)
cuBatH <- CGtrait.LR.int("bolt.bin", totmfcu, family=binomial)
cuP <- lapply(names(mfcu.dk)[c(19,34)],function(n) CGtrait.LR.int(n,mfcu.dk, family=poisson)) #lfcountH, boltdate, all poisson

cumodels <- CGtrait.models.int("bolt.bin",mfcu.dk, family=binomial)
cumodels
int<-2.868e+01 #inv mean
B<--1.204e+03 #Originnat estimate from model summary
pN<-exp(int+B)/(exp(int+B)+1) # Native
pI<-exp(int)/(exp(int)+1) # Introduced (B=0)
pI # 14.5% 
pN 
# qqnorm(resid(cumodels$model2), main="Q-Q plot for residuals")
# qqline(resid(cumodels$model2))
# shapiro.test(resid(cumodels$model2))

###cut, bolt.bin###
modeldata<-mfcu.dk[!is.na(mfcu.dk$bolt.bin),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1<-lmer(bolt.bin ~ Origin * Latitude +(1|PopID/Mom), family=binomial,data=modeldata)
model2<-lmer(bolt.bin ~ Origin * Latitude +(1|PopID), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin ~ Origin * Latitude +(1|blank), family=binomial,data=modeldata) # Test population effect
print(anova(model2,model1),digits=22) # Mom sig
(lambda <- (-2)*(-5.165307531272739893780 - (-5.165307534694778546225)))
1-pchisq(-6.844077e-09,1)
print(anova(model3,model2),digits=22) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
(lambda <- (-2)*(-5.165307531796844209282 - (-5.165307531272739893780)))
1-pchisq(1.048209e-09,1)
# model4<-lmer(bolt.bin ~ Origin * Latitude +(1|blank/PopID), family=binomial,data=modeldata)
# anova(model4,model2)


modelI<-lmer(bolt.bin ~ Origin + Latitude +(1|blank), family=binomial,data=modeldata)
anova(modelI, model3)
# modelI.2<-lmer(bolt.bin ~ Origin + Latitude +(1|blank/PopID), family=binomial,data=modeldata)
# anova(modelI.2, model4)

modelL<-lmer(bolt.bin ~ Origin + (1|blank), family=binomial,data=modeldata)
anova(modelL, modelI)

modelO<-lmer(bolt.bin ~  (1|blank), family=binomial,data=modeldata)
anova(modelO,modelL) #test for significance of origin - origin not sig!
modelL

int<--2.398 #inv mean
B<-2.061 #Originnat estimate from model summary
pN<-exp(int+B)/(exp(int+B)+1) # Native
# Note that if Origin was a continuous variable you would substitute B with B*Origin
pI<-exp(int)/(exp(int)+1)# Introduced (B=0)
pI # 14.5% 
pN # 60%
#report effect size as separate percentages, difference in percentage, or log-odds ratio: log(pI/pN)
#could also include the standard errors in equations to add upper/lower confidence intervals.
#check by looking at percentages
summary(mfco.dk1[totmfcu$Origin=="nat",]) #56 rows, 34 boltedatH = 60%
summary(mfco.dk1[totmfcu$Origin=="inv",]) #55 rows, 8 boltedatH = 14.5%

# modelI.2<-lmer(bolt.bin ~ Origin + Latitude +(1|PopID), family=binomial,data=modeldata)
# modelL.2<-lmer(bolt.bin ~ Origin + (1|PopID), family=binomial,data=modeldata)
# anova(modelL.2, modelI.2)
# modelI.2<-lmer(bolt.bin ~ Origin + Latitude +(1|PopID), family=binomial,data=modeldata)
# anova(model2, modelI.2)
# modelO.2<-lmer(bolt.bin ~  (1|PopID), family=binomial,data=modeldata)
# anova(modelO.2,modelL.2) 

#try glm
modelg <- glm(bolt.bin ~ Origin*Latitude, family=binomial,data=modeldata)
modelg1 <- glm(bolt.bin ~ Origin+Latitude, family=binomial,data=modeldata)
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq(7.3387, 1)
modelg3<- glm(bolt.bin ~ Origin, family=binomial,data=modeldata)
anova(modelg3,modelg1)
1-pchisq(5.5154, 1)
modelg2<- glm(bolt.bin ~ Latitude, family=binomial,data=modeldata)
anova(modelg2,modelg1)
1-pchisq(9.0533, 1)

CI.LS.binomial(modelg)
CI.LS.binomial(modelg1)

####cut, lf count, harvest, mom is sig, do by hand###
modeldata<-mfcu.dk[!is.na(mfcu.dk$LfCountH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(LfCountH ~ Origin * Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
model2raw<-lmer(LfCountH ~ Origin * Latitude + (1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(LfCountH ~ Origin * Latitude + (1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # Mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(16.177,1)
modelI <- lmer(LfCountH ~ Origin + Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelI,model1raw)

modelL <- lmer(LfCountH ~ Origin +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelL,modelI) 

modelOraw<-lmer(LfCountH ~ (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelOraw,modelL) #test for significance of origin 

CI.LS.poisson(modelL)

##cut, crown
modeldata<-mfcu.dk[!is.na(mfcu.dk$CrownDiam.mm),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

model1raw<-lmer(CrownDiam.mm ~ Origin *Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
model2raw<-lmer(CrownDiam.mm ~ Origin *Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(CrownDiam.mm ~ Origin *Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
print(anova(model2raw,model1raw),digits=22) # Mom not sig
(lambda <- (-2)*(-109.0070897781463088450 - (-109.0071141940671139992)))
1-pchisq(-4.883184e-05,1)
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(0.2437,1)

modelI <- lmer(CrownDiam.mm ~ Origin +Latitude +(1|blank), family=gaussian,data=modeldata)
anova(modelI, model3raw)

modelL<-lmer(CrownDiam.mm ~ Origin +(1|blank), family=gaussian,data=modeldata)
anova(modelL, modelI)

modelOraw<-lmer(CrownDiam.mm ~ (1|blank), family=gaussian,data=modeldata)
anova(modelOraw,modelL)

#try glm
modelg <- glm(CrownDiam.mm ~ Origin*Latitude, family=gaussian,data=modeldata)
modelg1 <- glm(CrownDiam.mm ~ Origin+Latitude, family=gaussian,data=modeldata)
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq(1.0027, 1)

modelg3<- glm(CrownDiam.mm ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1)
1-pchisq(0.045188, 1)
anova(modelg3)
# modelg2<- glm(CrownDiam.mm ~ Latitude, family=gaussian,data=modeldata)
# anova(modelg2,modelg1)
1-pchisq(0.16667, 1)

lsmeans(modelg3, ~Origin, conf=95)

# checking the normality of residuals e_i:
plot(resid(modelg3) ~ fitted(modelg3),main="residual plot")
abline(h=0)
qqnorm(resid(modelg3), main="Q-Q plot for residuals")
qqline(resid(modelg3))

####Drought, Origin * Lat####
mfd.dk<-read.table("MatFxDrought.dk.txt", header=T, sep="\t", quote='"', row.names=1) #drought, dk only
head(mfd.dk)
#no gaussian
dLR <- lapply(names(mfd.dk)[17:19],function(n) CGtrait.LR.int(n,mfd.dk, family=poisson)) #wilt, totwilt, death, all poisson
names(dLR) <- names(d)[17:19]

# dmodels <- lapply(names(d)[8:9],function(n) CGtrait.models.int(n,d))
# dmodels

# int<-10.69128#inv mean
# B<-0.28995#Originnat estimate from model summary
# pI<-exp(int)
# pN<-exp(int+B)
# pI
# pN
# summary(d[d$Origin=="inv",]$TotWilt)
# summary(d[d$Origin=="nat",]$TotWilt)

#drought, death
modeldata<-mfd.dk[!is.na(mfd.dk$DeathDay),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
model1raw<-lmer(DeathDay ~ Origin *Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
model2raw<-lmer(DeathDay ~ Origin *Latitude+(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(DeathDay ~ Origin *Latitude+(1|blank), family=poisson,data=modeldata) # Test population effect
print(anova(model2raw,model1raw), digits=22) # mom not sig
(lambda <- (-2)*(-1.913311475677763828784 - (-1.913311474508387233229)))
1-pchisq(2.338753e-09,1)
print(anova(model3raw,model2raw), digits = 22)
(lambda <- (-2)*(-1.913311474517363830472 - (-1.913311475677763828784)))
1-pchisq(-2.3208e-09,1)

modelI<-lmer(DeathDay ~ Origin + Latitude+(1|blank), family=poisson,data=modeldata)
anova(modelI, model3raw)

modelL<-lmer(DeathDay ~ Origin +(1|blank), family=poisson,data=modeldata)
anova(modelL, modelI)

modelOraw<-lmer(DeathDay ~ (1|blank), family=poisson,data=modeldata)
anova(modelOraw,modelL) #test for significance of origin - origin NOT sig....!

#try glm
modelg <- glm(DeathDay ~ Origin*Latitude, family=poisson,data=modeldata)
modelg1 <- glm(DeathDay ~ Origin+Latitude, family=poisson,data=modeldata)
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq(0.57219, 1)

modelg3<- glm(DeathDay ~ Origin, family=poisson,data=modeldata)
anova(modelg3,modelg1)
1-pchisq(2.2441, 1)
# modelg2<- glm(DeathDay ~ Latitude, family=poisson,data=modeldata)
# anova(modelg2,modelg1)
anova(modelg3)
1-pchisq(1.078, 1)

CI.LS.poisson(modelg3)

#drought, tot wilt
modeldata<-mfd.dk[!is.na(mfd.dk$TotWiltDay),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
model1raw<-lmer(TotWiltDay ~ Origin *Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
model2raw<-lmer(TotWiltDay ~ Origin *Latitude+(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(TotWiltDay ~ Origin *Latitude+(1|blank), family=poisson,data=modeldata) # Test population effect
print(anova(model2raw,model1raw), digits=22) # mom not sig
(lambda <- (-2)*(-6.385330357455019090196 - (-6.385485092397927608943)))
1-pchisq(-0.0003094699,1)
print(anova(model3raw,model2raw), digits = 22)
(lambda <- (-2)*(-6.385330358488191748734 - (-6.385330357455019090196)))
1-pchisq(2.066345e-09,1)

modelI<-lmer(TotWiltDay ~ Origin + Latitude+(1|blank), family=poisson,data=modeldata)
anova(modelI, model3raw)

modelL<-lmer(TotWiltDay ~ Origin +(1|blank), family=poisson,data=modeldata)
anova(modelL, modelI)

modelOraw<-lmer(TotWiltDay ~ (1|blank), family=poisson,data=modeldata)
anova(modelOraw,modelL) #test for significance of origin - origin NOT sig....!

#try glm
modelg <- glm(TotWiltDay ~ Origin*Latitude, family=poisson,data=modeldata)
modelg1 <- glm(TotWiltDay ~ Origin+Latitude, family=poisson,data=modeldata)
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq(0.73285, 1)

modelg3<- glm(TotWiltDay ~ Origin, family=poisson,data=modeldata)
anova(modelg3,modelg1)
1-pchisq(0.39014, 1)
# modelg2<- glm(TotWiltDay ~ Latitude, family=poisson,data=modeldata)
# anova(modelg2,modelg1)
anova(modelg3)
1-pchisq(0.16672, 1)

CI.LS.poisson(modelg3)

#drought, 1st wilt
modeldata<-mfd.dk[!is.na(mfd.dk$WiltDay),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
model1raw<-lmer(WiltDay ~ Origin *Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
model2raw<-lmer(WiltDay ~ Origin *Latitude+(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(WiltDay ~ Origin *Latitude+(1|blank), family=poisson,data=modeldata) # Test population effect
print(anova(model2raw,model1raw), digits=22) # mom not sig
(lambda <- (-2)*(-4.306174387866642305767 - (-4.306174388680275022523)))
1-pchisq(-1.627265e-09,1)
print(anova(model3raw,model2raw), digits = 22)
(lambda <- (-2)*(-4.306174387612385245916 - (-4.306174387866642305767)))
1-pchisq(-5.085141e-10,1)

modelI<-lmer(WiltDay ~ Origin + Latitude+(1|blank), family=poisson,data=modeldata)
anova(modelI, model3raw)

modelL<-lmer(WiltDay ~ Origin +(1|blank), family=poisson,data=modeldata)
anova(modelL, modelI)

modelOraw<-lmer(WiltDay ~ (1|blank), family=poisson,data=modeldata)
anova(modelOraw,modelL) #test for significance of origin - origin NOT sig....!

#try glm
modelg <- glm(WiltDay ~ Origin*Latitude, family=poisson,data=modeldata)
modelg1 <- glm(WiltDay ~ Origin+Latitude, family=poisson,data=modeldata)
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq(0.0022259, 1)

modelg3<- glm(WiltDay ~ Origin, family=poisson,data=modeldata)
anova(modelg3,modelg1)
1-pchisq(0.1059, 1)
# modelg2<- glm(WiltDay ~ Latitude, family=poisson,data=modeldata)
# anova(modelg2,modelg1)
anova(modelg3)
1-pchisq(2.0958, 1)

CI.LS.poisson(modelg3)

####Flood, Origin + Lat####
mff.dk<-read.table("MatFxFlood.dk.txt", header=T, sep="\t", quote='"', row.names=1) #flood, dk only
head(mff.dk)
#no gaussian
fLR <- lapply(names(mff.dk)[17:19],function(n) CGtrait.LR.int(n,mff.dk, family=poisson)) #wilt, totwilt, death, all poisson
# names(fLR) <- names(f)[20:21]
# 
# fmodels <- lapply(names(f)[20:21],function(n) CGtrait.models.int(n,f))
# fmodels

#flood, death
modeldata<-mff.dk[!is.na(mff.dk$DeathDay),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
model1raw<-lmer(DeathDay ~ Origin *Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
model2raw<-lmer(DeathDay ~ Origin *Latitude+(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(DeathDay ~ Origin *Latitude+(1|blank), family=poisson,data=modeldata) # Test population effect
print(anova(model2raw,model1raw), digits=22) # mom not sig
(lambda <- (-2)*(-1.844905329599548560182 - (-1.844905329460677645415)))
1-pchisq(2.777418e-10,1)
print(anova(model3raw,model2raw), digits = 22)
(lambda <- (-2)*(-1.844905329532508853063 - (-1.844905329599548560182)))
1-pchisq(-1.340794e-10,1)

modelI<-lmer(DeathDay ~ Origin + Latitude+(1|blank), family=poisson,data=modeldata)
anova(modelI, model3raw)

modelL<-lmer(DeathDay ~ Origin +(1|blank), family=poisson,data=modeldata)
anova(modelL, modelI)

modelOraw<-lmer(DeathDay ~ (1|blank), family=poisson,data=modeldata)
anova(modelOraw,modelL) #test for significance of origin - origin NOT sig....!

#try glm
modelg <- glm(DeathDay ~ Origin*Latitude, family=poisson,data=modeldata)
modelg1 <- glm(DeathDay ~ Origin+Latitude, family=poisson,data=modeldata)
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq(0.57219, 1)

modelg3<- glm(DeathDay ~ Origin, family=poisson,data=modeldata)
anova(modelg3,modelg1)
1-pchisq(2.2441, 1)
# modelg2<- glm(DeathDay ~ Latitude, family=poisson,data=modeldata)
# anova(modelg2,modelg1)
anova(modelg3)
1-pchisq(1.078, 1)
CI.LS.poisson(modelg3)

#flood, root death
modeldata<-mff.dk[!is.na(mff.dk$FloatDay),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
model1raw<-lmer(FloatDay ~ Origin *Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
model2raw<-lmer(FloatDay ~ Origin *Latitude+(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(FloatDay ~ Origin *Latitude+(1|blank), family=poisson,data=modeldata) # Test population effect
print(anova(model2raw,model1raw), digits=22) # mom not sig
(lambda <- (-2)*(-1.117466617470247447486 - (-1.117466617475713519525)))
1-pchisq(-1.093214e-11,1)
print(anova(model3raw,model2raw), digits = 22)
(lambda <- (-2)*(-1.117466617605779255484 - (-1.117466617470247447486)))
1-pchisq(2.710636e-10,1)

modelI<-lmer(FloatDay ~ Origin + Latitude+(1|blank), family=poisson,data=modeldata)
anova(modelI, model3raw)

modelL<-lmer(FloatDay ~ Origin +(1|blank), family=poisson,data=modeldata)
anova(modelL, modelI)

modelOraw<-lmer(FloatDay ~ (1|blank), family=poisson,data=modeldata)
anova(modelOraw,modelL) #test for significance of origin - origin NOT sig....!

#try glm
modelg <- glm(FloatDay ~ Origin*Latitude, family=poisson,data=modeldata)
modelg1 <- glm(FloatDay ~ Origin+Latitude, family=poisson,data=modeldata)
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq(0.57219, 1)

modelg3<- glm(FloatDay ~ Origin, family=poisson,data=modeldata)
anova(modelg3,modelg1)
1-pchisq(2.2441, 1)
# modelg2<- glm(FloatDay ~ Latitude, family=poisson,data=modeldata)
# anova(modelg2,modelg1)
anova(modelg3)
1-pchisq(1.078, 1)

CI.LS.poisson(modelg3)

#flood, yellow
modeldata<-mff.dk[!is.na(mff.dk$YellowDay),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
model1raw<-lmer(YellowDay ~ Origin *Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
model2raw<-lmer(YellowDay ~ Origin *Latitude+(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(YellowDay ~ Origin *Latitude+(1|blank), family=poisson,data=modeldata) # Test population effect
print(anova(model2raw,model1raw), digits=22) # mom not sig
(lambda <- (-2)*(-3.771305061738555064466 - (-3.771305061690886528680)))
1-pchisq(9.533707e-11,1)
print(anova(model3raw,model2raw), digits = 22)
(lambda <- (-2)*(-3.771305061749520071146 - (-3.771305061738555064466)))
1-pchisq(2.193001e-11,1)

modelI<-lmer(YellowDay ~ Origin + Latitude+(1|blank), family=poisson,data=modeldata)
print(anova(modelI, model3raw), digits = 22)
(lambda <- (-2)*(-3.771305241910035910280 - (-3.771305061749520071146)))
1-pchisq(3.60321e-07,1)

modelL<-lmer(YellowDay ~ Origin +(1|blank), family=poisson,data=modeldata)
anova(modelL, modelI)

modelOraw<-lmer(YellowDay ~ (1|blank), family=poisson,data=modeldata)
anova(modelOraw,modelL) #test for significance of origin - origin NOT sig....!

#try glm
modelg <- glm(YellowDay ~ Origin*Latitude, family=poisson,data=modeldata)
modelg1 <- glm(YellowDay ~ Origin+Latitude, family=poisson,data=modeldata)
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq(0.57219, 1)

modelg3<- glm(YellowDay ~ Origin, family=poisson,data=modeldata)
anova(modelg3,modelg1)
1-pchisq(2.2441, 1)
# modelg2<- glm(YellowDay ~ Latitude, family=poisson,data=modeldata)
# anova(modelg2,modelg1)
anova(modelg3)
1-pchisq(1.078, 1)

CI.LS.poisson(modelg3)

####Mom, Origin + Lat####
mfmom.dk<-read.table("MatFxMom.dk.txt", header=T, sep="\t", quote='"', row.names=1) 
head(mfmom.dk)

####Mom, seedwt###
str(mfmom.dk)
mfmom.dk$Origin<-droplevels(mfmom.dk$Origin)
mfmom.dk$Mom<-as.factor(mfmom.dk$Mom)

modeldata<-mfmom.dk[!is.na(mfmom.dk$SeedWt),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model1 <- lmer(SeedWt ~ Origin *Latitude+(1|PopID/Mom), family=gaussian,data=modeldata)
model2<-lmer(SeedWt ~ Origin *Latitude+(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(SeedWt ~ Origin *Latitude+(1|blank), family=gaussian,data=modeldata) # Test population effect
print(anova(model1,model2),digits=20)
(lambda <- (-2)*(569.10692604820803808 - (569.10692605475560413)))
1-pchisq(1.309513e-08,1)
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(1.2983,1)

modelI <- lmer(SeedWt ~ Origin +Latitude+(1|blank), family=gaussian,data=modeldata)
anova(modelI, model3)
modelL<-lmer(SeedWt ~ Origin + (1|blank), family=gaussian, data=modeldata)
anova(modelL, modelI)
modelO<-lmer(SeedWt ~ (1|blank), family=gaussian,data=modeldata)
anova(modelO,modelL) #test for significance of origin - origin not sig....?

#try glm
modelg <- glm(SeedWt ~ Origin*Latitude, family=gaussian,data=modeldata)
modelg1 <- glm(SeedWt ~ Origin+Latitude, family=gaussian,data=modeldata)
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq(3.0924e-07, 1)
modelg3<- glm(SeedWt ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1)
1-pchisq(2.897e-07, 1)
anova(modelg3)
# modelg2<- glm(SeedWt ~ Latitude, family=gaussian,data=modeldata)
# anova(modelg2,modelg1)
1-pchisq(1.5037e-08, 1)

CI.LS.poisson(modelg3)

####Mom, germ count###
modeldata<-mfmom.dk[!is.na(mfmom.dk$GermCount),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model1<-lmer(GermCount ~ Origin * Latitude+ SeedCount +(1|PopID/Mom), family=poisson, data=modeldata)
model2<-lmer(GermCount ~ Origin * Latitude+ SeedCount +(1|PopID), family=poisson, data=modeldata)
model3<-lmer(GermCount ~ Origin * Latitude+SeedCount + (1|blank), family=poisson, data=modeldata)
anova(model1,model2)
print(anova(model3, model2), digits=22)
(lambda <- (-2)*(-60.04988021221652871873 - (-60.04988021182497703876)))
1-pchisq(7.831034e-10,1)

modelI <- lmer(GermCount ~ Origin + Latitude+ SeedCount +(1|blank), family=poisson, data=modeldata)
anova(modelI, model3)

modelL<-lmer(GermCount ~ Origin +SeedCount+ (1|blank), family=poisson, data=modeldata)
anova(modelL, modelI)

modelO<-lmer(GermCount ~ SeedCount + (1|blank), family=poisson, data=modeldata)
anova(modelO, modelL)

# modelL
# int<- -0.42767
# #inv mean
# B<-0.52217
# #Originnat estimate from model summary
# pI<-exp(int)
# pN<-exp(int+B)
# pI
# pN
#try glm
modelg <- glm(GermCount ~ Origin*Latitude + SeedCount, family=poisson,data=modeldata)
modelg1 <- glm(GermCount ~ Origin+Latitude +SeedCount, family=poisson,data=modeldata)
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq(0.84755, 1)

modelg3<- glm(GermCount ~ Origin+ SeedCount, family=poisson,data=modeldata)
anova(modelg3,modelg1)
1-pchisq(0.77593, 1)
modelg2<- glm(GermCount ~ SeedCount, family=poisson,data=modeldata)
anova(modelg2,modelg3)
1-pchisq(9.0533, 1)

CI.LS.poisson(modelg3)

3.9003268057357/6.59*100
3.36395583408704/6.59*100
4.52222024956194/6.59*100
5.26181515938283/6.59*100
4.68924819994306/6.59*100
5.90429373558156/6.59*100

####Mom, germ date####avg, so can't use poisson
modeldata<-mfmom.dk[!is.na(mfmom.dk$GermAvgDate),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model1<-lmer(GermAvgDate ~ Origin *Latitude+(1|PopID/Mom), family=gaussian,data=modeldata)
model2<-lmer(GermAvgDate ~ Origin *Latitude+(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(GermAvgDate ~ Origin *Latitude+(1|blank), family=gaussian,data=modeldata) # Test population effect
print(anova(model1,model2), digits=22)
(lambda <- (-2)*(-27.70942445255505859336 - (-27.70960540428043117345)))
1-pchisq(-0.0003619035,1)
print(anova(model3,model2), digits=22) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
(lambda <- (-2)*(-27.49945599442735399975 - (-27.70942445255505859336)))
1-pchisq(-0.4199369,1)

modelI <- lmer(GermAvgDate ~ Origin +Latitude+(1|blank), family=gaussian,data=modeldata) 
anova(modelI,model3)

modelL<-lmer(GermAvgDate ~ Origin +(1|blank), family=, data=modeldata)
anova(modelL,modelI)

modelO<-lmer(GermAvgDate ~ (1|blank), family=gaussian,data=modeldata)
anova(modelO,modelL) #test for significance of origin - origin not sig....?

# # momfam
# modeldata$MomFam <- as.factor(modeldata$MomFam)
# model1<-lmer(GermAvgDate ~ Origin *Latitude+(1|PopID/MomFam), family=gaussian,data=modeldata)
# print(anova(model1,model2), digits=22)

#try glm
modelg <- glm(GermAvgDate ~ Origin*Latitude, family=gaussian,data=modeldata)
modelg1 <- glm(GermAvgDate ~ Origin+Latitude, family=gaussian,data=modeldata)
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq(0.087761, 1)


modelg3<- glm(GermAvgDate ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1)
1-pchisq(0.57582, 1)
anova(modelg3)
1-pchisq( 0.040802, 1)
# modelg2<- glm(GermAvgDate ~ Latitude, family=gaussian,data=modeldata)
# anova(modelg2,modelg1)
# 1-pchisq(9.0533, 1)

lsmeans(modelg3, ~Origin, conf=95)


###################################

# #germ count with more covariates#
# ####Mom, germ count###
# modeldata<-mom[!is.na(mom$GermCount),]
# modeldata$blank<-1
# modeldata$blank<-as.factor(modeldata$blank)
# # xtabs(~Origin+SeedAgeYrs, modeldata)
# 
# model2<-lmer(GermCount ~ Origin + SeedCount + Latitude+SeedWt +SeedAgeYrs+(SeedWt|PopID), family=poisson, data=modeldata)
# model3<-lmer(GermCount ~ Origin + SeedCount + Latitude+SeedWt + SeedAgeYrs+(SeedWt|blank), family=poisson, data=modeldata)
# anova(model3, model2)
# 
# modelY<-lmer(GermCount ~ Origin + SeedCount + Latitude+ SeedWt + (SeedWt|PopID), family=poisson, data=modeldata)
# anova(model2, modelY)
# 
# modelY2<-lmer(GermCount ~ Origin + SeedCount+ Latitude + SeedWt + (1|PopID), family=poisson, data=modeldata)
# anova(modelY2, modelY)
# 
# 
# modelW<-lmer(GermCount ~ Origin + SeedCount+ + Latitude(SeedWt|PopID), family=poisson, data=modeldata)
# anova(modelW, modelY)
# 
# modelC<-lmer(GermCount ~ Origin + SeedWt + Latitude+(SeedWt|PopID), family=poisson, data=modeldata)
# anova(modelC, modelY)
# 
# modelL<-lmer(GermCount ~ Origin + SeedCount + SeedWt + (SeedWt|PopID), family=poisson, data=modeldata)
# anova(modelL, modelY)
# 
# modelO<-lmer(GermCount ~ SeedCount +SeedWt+ (SeedWt|PopID), family=poisson, data=modeldata)
# anova(modelO, modelL)
# 
# modelY
# int<- -1.33561
# #inv mean
# B<-0.37899
# #Originnat estimate from model summary
# pI<-exp(int)
# pN<-exp(int+B)
# pI
# pN
# 
# 
# ####Mom, germ date####avg, so can't use poisson
# modeldata<-mom[!is.na(mom$AvgGermDate.log),]
# modeldata$blank<-1
# modeldata$blank<-as.factor(modeldata$blank)