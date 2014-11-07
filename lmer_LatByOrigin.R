###Mat FX mixed FX models, focused on Origin BY Latitude###
#Mat fx, REML, using lme4
#mixed effect models 
library(lme4.0) #or
# library(lme4)
library(lsmeans)
library(ggplot2)
library(plyr)

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

#merge mf ctrl and mom df
mfmom.dk<-read.table("MatFxMom.dk.txt", header=T, sep="\t", quote='"', row.names=1) 
str(mfmom.dk)
str(mfallo.dk)
totmfallo <- merge(mfmom.dk,mfallo.dk, all.y=TRUE )
str(totmfallo)
#tidy
# totmf <- totmf[,-c(16:18,26,29:30, 32:38, 40:44, 46:57)]
# totmfallo <- totmfallo[!is.na(totmf$Trt),]
# totmf$Exp<-droplevels(totmf$Exp)
# totmf$Trt<-droplevels(totmf$Trt)
levels(totmf$PopID)

#allo, shoot mass
modeldata<-totmfallo[!is.na(totmfallo$Mass.gA),]
#head(modeldata)
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

# modelplus<-lmer(Mass.gA ~ Origin *Latitude+(1|PopID) + (Origin|PopID/Mom), family=gaussian,data=modeldata)
modelobar<-lmer(Mass.gA ~ Origin *Latitude+(Origin|PopID/Mom), family=gaussian,data=modeldata)
# anova(modelplus, modelobar)
model1<-lmer(Mass.gA ~ Origin *Latitude+(1|PopID/Mom), family=gaussian,data=modeldata)
# modelplus2 <- lmer(Mass.gA ~ Origin *Latitude+(1|PopID) + (1|PopID/Mom), family=gaussian,data=modeldata)
# anova(modelplus2, model1)
anova(modelobar, model1)
model2<-lmer(Mass.gA ~ Origin *Latitude+ (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Mass.gA ~ Origin *Latitude+ (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # Mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(0.1406,1)

# modelI <- lmer(Mass.gA  ~ Origin + Latitude + (1|blank), family=gaussian,data=modeldata)
# anova(modelI,model3)
# 
# modelL<-lmer(Mass.gA ~ Origin +(1|blank), family=gaussian,data=modeldata)
# anova(modelL, modelI)
# 
# modelO<-lmer(Mass.gA ~ (1|blank), family=gaussian,data=modeldata)
# anova(modelO,modelL) #test for significance of origin - origin not sig....?

#try glm
modelg <- glm(Mass.gA ~ Origin*Latitude, family=gaussian,data=modeldata)
modelg1 <- glm(Mass.gA ~ Origin+Latitude, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.335,1,lower=FALSE)#chisq value

modelg3<- glm(Mass.gA ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(0.2373,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
# modelg2<- glm(Mass.gA ~ Latitude, family=gaussian,data=modeldata)
# anova(modelg2,modelg1)
qchisq(0.0002189,1,lower=FALSE)#chisq value

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
modeldata<-totmfallo[!is.na(totmfallo$Crown.mmA),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar<-lmer(Crown.mmA ~ Origin *Latitude +(Origin|PopID/Mom), family=gaussian,data=modeldata)
model1raw<-lmer(Crown.mmA ~ Origin *Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1raw)
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
anova(modelg1, modelg, test="LRT") 
qchisq(0.9284,1,lower=FALSE)#chisq value

modelg3<- glm(Crown.mmA ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(0.5571,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
# modelg2<- glm(Crown.mmA ~ Latitude, family=gaussian,data=modeldata)
# anova(modelg2,modelg1)
qchisq(0.4556,1,lower=FALSE)#chisq value

lsmeans(modelg3, ~Origin, conf=95)

# #####m1, Origin * Lat#####
mfcom1<-read.table("MatFxBonusCtrlM1.txt", header=T, sep="\t", quote='"', row.names=1) #m1 all plants in analysis, balanced, dk only
head(mfcom1)
#mfcom1$lxw <- mfcom1$LfLgth1*mfcom1$LfWdth1
write.table(mfcom1, file="MatFxBonusCtrlM1.txt", sep="\t", quote=F)
m1lxw <- CGtrait.LR.int("lxw", mfcom1)
m1lf <- CGtrait.LR.int("LfCount1", mfcom1, family=poisson)#poisson distribution

#merge mf ctrl and mom df
str(mfmom.dk)
str(mfcom1)
totmfm1 <- merge(mfmom.dk,mfcom1, all.y=TRUE )
str(totmfm1)
#tidy
# totmf <- totmf[,-c(16:18,26,29:30, 32:38, 40:44, 46:57)]
# totmf <- totmf[!is.na(totmf$Trt),]
# totmf$Exp<-droplevels(totmf$Exp)
# totmf$Trt<-droplevels(totmf$Trt)
# levels(totmf$PopID)

###m1, lxw, mom sig, do by hand###
modeldata<-totmfm1[!is.na(totmfm1$lxw),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar<-lmer(lxw ~ Origin *Latitude+(Origin|PopID/Mom), family=gaussian,data=modeldata)
model1<-lmer(lxw ~ Origin *Latitude+(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1)
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
modeldata<-totmfm1[!is.na(totmfm1$LfCount1),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar<-lmer(LfCount1 ~ Origin *Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1raw<-lmer(LfCount1 ~ Origin *Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1raw)
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
summary(model1raw)
summary(glmmPQL(LfCount1 ~ Origin*Latitude, random = ~ 1 | PopID/Mom,
                family = poisson, data = modeldata))
xtabs(modeldata)

int<-5.27022 #inv mean
B<--4.79533 #Originnat estimate from model summary
pI<-exp(int)
pN<-exp(int+B)
pI
pN

CI.LS.poisson(modelI) #exclude sig interactions

interaction.plot(response = modeldata$LfCount1, x.factor = modeldata$Latitude, trace.factor = modeldata$Origin)
plot(modeldata$Latitude, modeldata$Origin)
qplot(data=modeldata, Latitude, LfCount1, color=Origin, geom = "jitter")

moddata <- ddply(modeldata, .(PopID, Origin, Latitude), summarize, popCount=length(PopID), popLfCount1=mean(LfCount1, na.rm=TRUE))

#png("MF_    .png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,Latitude, popLfCount1, color = Origin, 
      xlab="latitude", 
      ylab="Population mean lf count treatment", main="") +geom_smooth(method=glm, se=TRUE)
# dev.off()

modelI2 <- lmer(LfCount1  ~ Latitude+ Origin + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelI, modelI2)

####Control, Origin * Lat####
mfco.dk1<-read.table("MatFxBonusCtrl.txt", header=T, sep="\t", quote='"', row.names=1) #largest balanced control
head(mfco.dk1)
#mfco.dk1$lxwH <- mfco.dk1$LfLgthH * mfco.dk1$LfWdthH

#merge mf ctrl and mom df
str(mfmom.dk)
str(mfco.dk1)
totmf <- merge(mfmom.dk,mfco.dk1, all.y=TRUE )
str(totmf)
# #tidy
# totmf <- totmf[,-c(16:18,26,29:30, 32:38, 40:44, 46:57)]
# totmf <- totmf[!is.na(totmf$Trt),]
# totmf$Exp<-droplevels(totmf$Exp)
# totmf$Trt<-droplevels(totmf$Trt)
# levels(totmf$PopID)

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
modeldata<-totmf[!is.na(totmf$ShootMass.g),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar<-lmer(ShootMass.g ~ Origin *Latitude+(Origin|PopID/Mom), family=gaussian,data=modeldata)
model1<-lmer(ShootMass.g ~ Origin *Latitude+(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1)
model2<-lmer(ShootMass.g ~ Origin *Latitude+ (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(ShootMass.g ~ Origin *Latitude+ (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # Mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(1.3743,1)

# modelI <- lmer(ShootMass.g  ~ Origin + Latitude + (1|blank), family=gaussian,data=modeldata)
# anova(modelI,model3)
# 
# modelL<-lmer(ShootMass.g ~ Origin +(1|blank), family=gaussian,data=modeldata)
# anova(modelL, modelI)
# 
# modelO<-lmer(ShootMass.g ~ (1|blank), family=gaussian,data=modeldata)
# anova(modelO,modelL) #test for significance of origin - origin not sig....?
#try glm
modelg <- glm(ShootMass.g ~ Origin*Latitude, family=gaussian,data=modeldata)
modelg1 <- glm(ShootMass.g ~ Origin+Latitude, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.5731,1,lower=FALSE)#chisq value

modelg3<- glm(ShootMass.g ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(0.07642,1,lower=FALSE)#chisq value
# modelg2<- glm(ShootMass.g ~ Latitude, family=gaussian,data=modeldata)
anova(modelg3, test="LRT")
qchisq(0.02771,1,lower=FALSE)#chisq value

lsmeans(modelg3, ~Origin, conf=95)

#lat confounded?
modelg4 <- glm(ShootMass.g ~ Latitude +Origin, family=gaussian,data=modeldata)
anova(modelg1, modelg4)
anova(modelg3, modelg4)
anova(modelg2, modelg4)
anova(modelg2)

####control, lxw, mom sig, so do by hand####
modeldata<-totmf[!is.na(totmf$lxwH),]
modeldata <- modeldata[modeldata$lxwH>0,]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar<-lmer(lxwH ~ Origin *Latitude +(Origin|PopID/Mom), family=gaussian,data=modeldata)
model1raw<-lmer(lxwH ~ Origin *Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(lxwH ~ Origin *Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(lxwH ~ Origin *Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # Mom not sig
print(anova(model3raw,model2raw), digits=22) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# (lambda <- (-2)*(-571.8468485550226887426 - (-571.2004667420127361765)))
1-pchisq(1.298289999999999944084,1)
# 
# modelI <- lmer(lxwH ~ Origin +Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
# anova(modelI, model1raw)
# 
# modelL<-lmer(lxwH ~ Origin +(1|PopID/Mom), family=gaussian,data=modeldata)
# anova(modelL, modelI)
# 
# modelOraw<-lmer(lxwH ~ (1|PopID/Mom), family=gaussian,data=modeldata)
# anova(modelOraw,modelL) #test for significance of origin - origin NOT sig....!

#try glm
modelg <- glm(lxwH ~ Origin*Latitude, family=gaussian,data=modeldata)
modelg1 <- glm(lxwH ~ Origin+Latitude, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.03858,1,lower=FALSE)#chisq value

modelg3<- glm(lxwH ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(0.1811,1,lower=FALSE)#chisq value
# modelg2<- glm(lxwH ~ Latitude, family=gaussian,data=modeldata)
anova(modelg3, test="LRT")
qchisq(0.03194,1,lower=FALSE)#chisq value

lsmeans(modelg3,~Origin, conf=95)
interaction.plot(response = modeldata$lxwH, x.factor = modeldata$Latitude, trace.factor = modeldata$Origin)

summary(modelg)
moddata <- ddply(modeldata, .(PopID, Origin, Latitude), summarize, popCount=length(PopID), poplxwH=mean(lxwH, na.rm=TRUE))
qplot(data=moddata,Latitude, poplxwH, color = Origin, xlab="latitude", ylab="Population mean lxwH", main="") +geom_smooth(method=glm, se=TRUE)

####control, lf count, mom sig so do by hand#####
#poisson on raw data
modeldata<-totmf[!is.na(totmf$LfCountH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar<-lmer(LfCountH ~ Origin *Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1raw<-lmer(LfCountH ~ Origin *Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1raw)
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

modelobar<-lmer(bolt.bin ~ Origin * Latitude +(Origin|PopID/Mom), family=binomial,data=modeldata)
model1<-lmer(bolt.bin ~ Origin * Latitude +(1|PopID/Mom), family=binomial,data=modeldata)
anova(modelobar, model1)
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
anova(modelg1, modelg, test="LRT") #'Deviance' is chisq value
1-pchisq(5.7821, 1)

modelg3<- glm(bolt.bin ~ Origin, family=binomial,data=modeldata)
anova(modelg3,modelg1)
1-pchisq(5.5154, 1)
anova(modelg3)
# modelg2<- glm(bolt.bin ~ Latitude, family=binomial,data=modeldata)
# anova(modelg2,modelg1)
# 1-pchisq(9.0533, 1)

CI.LS.binomial(modelg1) #exclude sig int

#overdispersed?
deviance(modelg) ## [1] 111.7172
summary(modelg)$dispersion ## 1 (by definition)
dfr <- df.residual(modelg)
deviance(modelg)/dfr ## [1] 1.044086
d_2 <- sum(residuals(modelg,"pearson")^2) 
(disp2 <- d_2/dfr)  ## [1] 1.040685

pchisq(d_2,df=dfr,lower.tail=FALSE) ##[1] 0.3672733

# gg2 <- update(modelg,family=quasipoisson)
# summary(gg2)$dispersion  ##[1] 0.6419276
# all.equal(coef(modelg),coef(gg2)) ## [1] "Mean relative difference: 0.3883268"
# se1 <- coef(summary(modelg))[,"Std. Error"]
# se2 <- coef(summary(gg2))[,"Std. Error"]
# se2/se1
# sqrt(disp2)

# dfr <- df.residual(gg2)
# deviance(gg2)/dfr ## [1] 1.044086
# d_2 <- sum(residuals(gg2,"pearson")^2) 
# (disp2 <- d_2/dfr)  ## [1] 1.040685


summary(modelg)
moddata <- ddply(modeldata, .(PopID, Origin, Latitude), summarize, popCount=length(PopID), popBolt=mean(bolt.bin, na.rm=TRUE))
qplot(data=moddata,Latitude, popBolt, color = Origin, xlab="latitude", ylab="Population mean bolt.bin", main="") +geom_smooth(method=glm, se=TRUE)

###control, boltday.adj, cross sig, do by hand###
#only bolters
modeldata<-totmf[!is.na(totmf$BoltDay.adj),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar<-lmer(BoltDay.adj ~ Origin * Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1<-lmer(BoltDay.adj ~ Origin * Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1)
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
modeldata<-totmf[!is.na(totmf$CrownDiam.mm),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar<-lmer(CrownDiam.mm ~ Origin *Latitude +(Origin|PopID/Mom), family=gaussian,data=modeldata)
model1raw<-lmer(CrownDiam.mm ~ Origin *Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1raw)
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
modeldata<-totmf[!is.na(totmf$sla),]
modeldata$sla.log <- log(modeldata$sla)
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar<-lmer(sla.log ~ Origin *Latitude +(Origin|PopID/Mom), family=gaussian,data=modeldata)
model1raw<-lmer(sla.log ~ Origin *Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(sla.log ~ Origin *Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(sla.log ~ Origin *Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
print(anova(model2raw,model3raw), digits=22) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
(lambda <- (-2)*(-2.578845030579588026853 - (-2.730199207582871601119)))
1-pchisq(-0.3027084,1)

# modelI <- lmer(sla.log ~ Origin +Latitude +(1|blank), family=gaussian,data=modeldata)
# anova(modelI, model3raw)
# 
# modelL<-lmer(sla.log ~ Origin +(1|blank), family=gaussian,data=modeldata)
# anova(modelL, modelI)
# 
# modelOraw<-lmer(sla.log ~ (1|blank), family=gaussian,data=modeldata)
# anova(modelOraw,modelL) #test for significance of origin - origin NOT sig....!

#try glm
modelg <- glm(sla.log ~ Origin*Latitude, family=gaussian,data=modeldata)
modelg1 <- glm(sla.log ~ Origin+Latitude, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.0964,1,lower=FALSE)#chisq value

modelg3<- glm(sla.log ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(0.9672,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
# modelg2<- glm(sla.log ~ Latitude, family=gaussian,data=modeldata)
# anova(modelg2,modelg1)
qchisq(0.5399,1,lower=FALSE)#chisq value

CI.LS.poisson(modelg3)


####Mom, Origin + Lat####
mfmom.dk<-read.table("MatFxMom.dk.txt", header=T, sep="\t", quote='"', row.names=1) 
head(mfmom.dk)

####Mom, seedwt###
# str(mfmom.dk)
# mfmom.dk$Origin<-droplevels(mfmom.dk$Origin)
mfmom.dk$Mom<-as.factor(mfmom.dk$Mom)

modeldata<-mfmom.dk[!is.na(mfmom.dk$SeedWt),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

modelobar <- lmer(SeedWt ~ Origin *Latitude+(Origin|PopID/Mom), family=gaussian,data=modeldata)
model1 <- lmer(SeedWt ~ Origin *Latitude+(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1)
model2<-lmer(SeedWt ~ Origin *Latitude+(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(SeedWt ~ Origin *Latitude+(1|blank), family=gaussian,data=modeldata) # Test population effect
print(anova(model1,model2),digits=20)
(lambda <- (-2)*(569.10692604820803808 - (569.10692605475560413)))
1-pchisq(1.309513e-08,1)
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(1.2983,1)

# modelI <- lmer(SeedWt ~ Origin +Latitude+(1|blank), family=gaussian,data=modeldata)
# anova(modelI, model3)
# modelL<-lmer(SeedWt ~ Origin + (1|blank), family=gaussian, data=modeldata)
# anova(modelL, modelI)
# modelO<-lmer(SeedWt ~ (1|blank), family=gaussian,data=modeldata)
# anova(modelO,modelL) #test for significance of origin - origin not sig....?

#try glm
modelg <- glm(SeedWt ~ Origin*Latitude, family=gaussian,data=modeldata)
modelg1 <- glm(SeedWt ~ Origin+Latitude, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.6106,1,lower=FALSE)#chisq value

modelg3<- glm(SeedWt ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(0.6209,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
# modelg2<- glm(SeedWt ~ Latitude, family=gaussian,data=modeldata)
# anova(modelg2,modelg1)
qchisq(0.91,1,lower=FALSE)#chisq value

CI.LS.poisson(modelg3)

####Mom, germ count###
modeldata<-mfmom.dk[!is.na(mfmom.dk$GermCount),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

modelobar<-lmer(GermCount ~ Origin * Latitude+ SeedCount +(Origin|PopID/Mom), family=poisson, data=modeldata)
model1<-lmer(GermCount ~ Origin * Latitude+ SeedCount +(1|PopID/Mom), family=poisson, data=modeldata)
anova(modelobar, model1)
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

dispersiontest(modelg3)

summary(modeldata$SeedCount) #% of global seed count
3.9003268057357/6.59*100
3.36395583408704/6.59*100
4.52222024956194/6.59*100
5.26181515938283/6.59*100
4.68924819994306/6.59*100
5.90429373558156/6.59*100

summary(subset(modeldata, Origin=="nat", select= c(Origin,SeedCount))) #origin specific avg seed count
summary(subset(modeldata, Origin=="inv", select= c(Origin,SeedCount)))
3.9003268057357/4.02*100
3.36395583408704/4.02*100
4.52222024956194/4.02*100
5.26181515938283/8.839*100
4.68924819994306/8.839*100
5.90429373558156/8.839*100

####Mom, germ date####avg, so can't use poisson
modeldata<-mfmom.dk[!is.na(mfmom.dk$GermAvgDate),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

modelobar<-lmer(GermAvgDate ~ Origin *Latitude+(Origin|PopID/Mom), family=gaussian,data=modeldata)
model1<-lmer(GermAvgDate ~ Origin *Latitude+(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1)
model2<-lmer(GermAvgDate ~ Origin *Latitude+(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(GermAvgDate ~ Origin *Latitude+(1|blank), family=gaussian,data=modeldata) # Test population effect
print(anova(model1,model2), digits=22)
(lambda <- (-2)*(-27.70942445255505859336 - (-27.70960540428043117345)))
1-pchisq(-0.0003619035,1)
print(anova(model3,model2), digits=22) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
(lambda <- (-2)*(-27.49945599442735399975 - (-27.70942445255505859336)))
1-pchisq(-0.4199369,1)

# modelI <- lmer(GermAvgDate ~ Origin +Latitude+(1|blank), family=gaussian,data=modeldata) 
# anova(modelI,model3)
# 
# modelL<-lmer(GermAvgDate ~ Origin +(1|blank), family=, data=modeldata)
# anova(modelL,modelI)
# 
# modelO<-lmer(GermAvgDate ~ (1|blank), family=gaussian,data=modeldata)
# anova(modelO,modelL) #test for significance of origin - origin not sig....?

# # momfam
# modeldata$MomFam <- as.factor(modeldata$MomFam)
# model1<-lmer(GermAvgDate ~ Origin *Latitude+(1|PopID/MomFam), family=gaussian,data=modeldata)
# print(anova(model1,model2), digits=22)

#try glm
modelg <- glm(GermAvgDate ~ Origin*Latitude, family=gaussian,data=modeldata)
modelg1 <- glm(GermAvgDate ~ Origin+Latitude, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.3577,1,lower=FALSE)#chisq value

modelg3<- glm(GermAvgDate ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(0.01839,1,lower=FALSE)#chisq value
# anova(modelg3, test="LRT")
# qchisq(pval,1,lower=FALSE)#chisq value
modelg2<- glm(GermAvgDate ~ Latitude, family=gaussian,data=modeldata)
anova(modelg2,modelg1, test="LRT")
qchisq(0.01889,1,lower=FALSE)#chisq value

lsmeans(modelg1, ~Origin, conf=95)

##germ count with more covariates#
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


##seed count###########
modeldata<-mfmom.dk[!is.na(mfmom.dk$SeedCount),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

modelobar<-lmer(SeedCount ~ Origin * Latitude+(Origin|PopID/Mom), family=poisson, data=modeldata)
model1<-lmer(SeedCount ~ Origin * Latitude+(1|PopID/Mom), family=poisson, data=modeldata)
anova(modelobar, model1)
model2<-lmer(SeedCount ~ Origin * Latitude+ (1|PopID), family=poisson, data=modeldata)
model3<-lmer(SeedCount ~ Origin * Latitude+ (1|blank), family=poisson, data=modeldata)
anova(model1,model2)
print(anova(model3, model2), digits=22)
(lambda <- (-2)*(-60.04988021221652871873 - (-60.04988021182497703876)))
1-pchisq(2.380729999999999790816,1)

modelI <- lmer(SeedCount ~ Origin + Latitude+ (1|PopID/Mom), family=poisson, data=modeldata)
anova(modelI, model1)

modelL<-lmer(SeedCount ~ Origin + (1|PopID/Mom), family=poisson, data=modeldata)
anova(modelL, modelI)

modelO<-lmer(SeedCount ~ (1|PopID/Mom), family=poisson, data=modeldata)
anova(modelO, modelL)

modeldata$MomFam <- as.factor(modeldata$MomFam)
modelM <- lmer(SeedCount ~ Origin + (1|PopID/MomFam), family=poisson, data=modeldata)
anova(modelL, modelM)
# # modelL
# # int<- -0.42767
# # #inv mean
# # B<-0.52217
# # #Originnat estimate from model summary
# # pI<-exp(int)
# # pN<-exp(int+B)
# # pI
# # pN
# #try glm
# modelg <- glm(SeedCount ~ Origin*Latitude , family=poisson,data=modeldata)
# modelg1 <- glm(SeedCount ~ Origin+Latitude , family=poisson,data=modeldata)
# anova(modelg1, modelg) #'Deviance' is chisq value
# 1-pchisq(0.84755, 1)
# 
# modelg3<- glm(SeedCount ~ Origin, family=poisson,data=modeldata)
# anova(modelg3,modelg1)
# 1-pchisq(0.77593, 1)
# modelg2<- glm(SeedCount ~ Latitude, family=poisson,data=modeldata)
# anova(modelg2,modelg3)
# 1-pchisq(9.0533, 1)

CI.LS.poisson(modelL)


modelg <- glm(seednum~Origin, family=poisson, data=seed)
anova(modelg)