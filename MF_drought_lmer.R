#MF stress treatment - drought
#Mat fx, REML, using lme4
#mixed effect models 
library(lme4.0) #or
# library(lme4)
library(lsmeans)
library(ggplot2)
library(plyr)

#pop control means to look at stress treatments
mfco.dk1<-read.table("MatFxBonusCtrl.txt", header=T, sep="\t", quote='"', row.names=1) #largest balanced control
mfmom.dk<-read.table("MatFxMom.dk.txt", header=T, sep="\t", quote='"', row.names=1) 
#merge mf ctrl and mom df
str(mfmom.dk)
str(mfco.dk1)
totmf <- merge(mfmom.dk,mfco.dk1, all.y=TRUE )

se <- function(x) sqrt(var(x)/length(x)) #chokes on NAs
comeans<- ddply(totmf, .(PopID, Origin, Latitude), summarize, CtrlPopCount=length(PopID), CtrlPopLf=mean(LfCountH, na.rm=TRUE), CtrlPopLfSE=se(LfCountH),
                CtrlPopShoot=mean(ShootMass.g, na.rm=TRUE), CtrlPopShootSE=se(ShootMass.g))

####Drought, Origin * Lat####
mfd.dk<-read.table("MatFxDrought.dk.txt", header=T, sep="\t", quote='"', row.names=1) #drought, dk only
head(mfd.dk)
#no gaussian
dLR <- lapply(names(mfd.dk)[17:19],function(n) CGtrait.LR.int(n,mfd.dk, family=poisson)) #wilt, totwilt, death, all poisson
names(dLR) <- names(d)[17:19]


#merge mfcu and mom df
str(mfmom.dk)
str(mfd.dk)
totmfd <- merge(mfmom.dk,mfd.dk, all.y=TRUE )
# str(totmfcu)
# #tidy
# # totmfn <- totmfn[,-c(16:18,26,29:30, 32:38, 40:44, 46:57)]
# totmfcu$Exp<-droplevels(totmfcu$Exp)
# totmfcu$Trt<-droplevels(totmfcu$Trt)
# levels(totmfcu$PopID)

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

###drought, death
modeldata<-totmfd[!is.na(totmfd$DeathDay),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

modelobar<-lmer(DeathDay ~ Origin *Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1raw<-lmer(DeathDay ~ Origin *Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1raw)
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

#overdispersion
library(AER)
dispersiontest(modelg3)

#explicit trade-off using shootmass
modeldata <- merge(modeldata, comeans, all.x=TRUE)
modeldata <- modeldata[!is.na(modeldata$CtrlPopShoot),]

modelobar<-lmer(DeathDay ~ Origin * CtrlPopShoot*Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1raw<-lmer(DeathDay ~ Origin * CtrlPopShoot* Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(DeathDay ~ Origin * CtrlPopShoot* Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(DeathDay ~ Origin * CtrlPopShoot* Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(56.023,1)

# 
modelg <- glm(DeathDay ~ Origin*CtrlPopShoot*Latitude, family=poisson,data=modeldata)
modelg1 <- glm(DeathDay ~ Origin*CtrlPopShoot+Latitude, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(pval,1,lower=FALSE)#chisq value
modelg3<- glm(DeathDay ~ Origin*CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(pval,1,lower=FALSE)#chisq value
modelg2<- glm(DeathDay ~Origin +CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg2,modelg3, test="LRT")
qchisq(pval,1,lower=FALSE)#chisq value
modelg4 <- glm(DeathDay ~Origin, family=poisson, data=modeldata)
anova(modelg4, modelg2, test="LRT")
modelg5 <- glm(DeathDay~CtrlPopShoot, family=poisson, data=modeldata)
anova(modelg5, modelg2, test="LRT")

qplot(data=modeldata,CtrlPopShoot, DeathDay, color = Origin)+geom_point(position="jitter")
moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopShoot), summarize, popCount=length(PopID), popDeathDay=mean(DeathDay))

png("MF_performance_drdeath_shoot.png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,CtrlPopShoot, popDeathDay, color = Origin, 
      xlab="Population mean shoot mass in control treatment", 
      ylab="Population mean days to DeathDay in drought treatment", main="Performance in drought vs. control treatments") +geom_smooth(method=glm, se=TRUE)
dev.off()

###drought, tot wilt
modeldata<-totmfd[!is.na(totmfd$TotWiltDay),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

modelobar<-lmer(TotWiltDay ~ Origin *Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1raw<-lmer(TotWiltDay ~ Origin *Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1raw)
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

#overdispersion
library(AER)
dispersiontest(modelg3)
 
#explicit trade-off using shootmass
modeldata <- merge(modeldata, comeans, all.x=TRUE)
modeldata <- modeldata[!is.na(modeldata$CtrlPopShoot),]

modelobar<-lmer(TotWiltDay ~ Origin * CtrlPopShoot*Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1raw<-lmer(TotWiltDay ~ Origin * CtrlPopShoot* Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(TotWiltDay ~ Origin * CtrlPopShoot* Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(TotWiltDay ~ Origin * CtrlPopShoot* Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(56.023,1)

modelg <- glm(TotWiltDay ~ Origin*CtrlPopShoot*Latitude, family=poisson,data=modeldata)
modelg1 <- glm(TotWiltDay ~ Origin*CtrlPopShoot+Latitude, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(pval,1,lower=FALSE)#chisq value
modelg3<- glm(TotWiltDay ~ Origin*CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(pval,1,lower=FALSE)#chisq value
dispersiontest(modelg3)
modelg2<- glm(TotWiltDay ~Origin +CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg2,modelg3, test="LRT")
qchisq(pval,1,lower=FALSE)#chisq value
modelg4 <- glm(TotWiltDay ~Origin, family=poisson, data=modeldata)
anova(modelg4, modelg2, test="LRT")
anova(modelg4)

summary(modelg3)

qplot(data=modeldata,CtrlPopShoot, TotWiltDay, color = Origin)+geom_point(position="jitter")
moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopShoot), summarize, popCount=length(PopID), popTotWiltDay=mean(TotWiltDay))

png("MF_performance_totwilt_shoot.png", height = 600, width = 600, pointsize = 16)
qplot(data=moddata,CtrlPopShoot, popTotWiltDay, color = Origin, 
      xlab="Population mean shoot mass in control treatment", 
      ylab="Population mean days to TotWiltDay in drought treatment",
      main="Performance in drought vs. control treatments") +geom_smooth(method=glm,se=TRUE)
dev.off()

###drought, 1st wilt
modeldata<-totmfd[!is.na(totmfd$WiltDay),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

modelobar<-lmer(WiltDay ~ Origin *Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1raw<-lmer(WiltDay ~ Origin *Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1raw)
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

#overdispersion
library(AER)
dispersiontest(modelg3)

 
#explicit trade-off using shootmass
modeldata <- merge(modeldata, comeans, all.x=TRUE)
modeldata <- modeldata[!is.na(modeldata$CtrlPopShoot),]

modelobar<-lmer(WiltDay ~ Origin * CtrlPopShoot*Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1raw<-lmer(WiltDay ~ Origin * CtrlPopShoot* Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(WiltDay ~ Origin * CtrlPopShoot* Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(WiltDay ~ Origin * CtrlPopShoot* Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(56.023,1)

modelg <- glm(WiltDay ~ Origin*CtrlPopShoot*Latitude, family=poisson,data=modeldata)
modelg1 <- glm(WiltDay ~ Origin*CtrlPopShoot+Latitude, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(pval,1,lower=FALSE)#chisq value
modelg3<- glm(WiltDay ~ Origin*CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(pval,1,lower=FALSE)#chisq value
modelg2<- glm(WiltDay ~Origin +CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg2,modelg3, test="LRT")
qchisq(pval,1,lower=FALSE)#chisq value
modelg4 <- glm(WiltDay ~Origin, family=poisson, data=modeldata)
anova(modelg4, modelg2, test="LRT")
# modelg5 <- glm(WiltDay~CtrlPopShoot, family=poisson, data=modeldata)
anova(modelg4, test="LRT")

qplot(data=modeldata,CtrlPopShoot, WiltDay, color = Origin)+geom_point(position="jitter")
moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopShoot), summarize, popCount=length(PopID), popWiltDay=mean(WiltDay))
qplot(data=moddata,CtrlPopShoot, popWiltDay, color = Origin, 
      xlab="Population mean shoot mass in control treatment", 
      ylab="Population mean days to WiltDay in drought treatment", main="Performance in drought vs. control treatments") +geom_smooth(method=glm, se=TRUE)

