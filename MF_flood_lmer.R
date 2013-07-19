#MF stress treatment - flooding
#Mat fx, REML, using lme4
#mixed effect models 
library(lme4)
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
comeans<- ddply(totmf, .(PopID, Origin, Latitude), summarize, CtrlPopCount=length(PopID), CtrlPopLf=mean(LfCountH, na.rm=TRUE), CtrlPopLfSE=se(LfCountH, na.rm=TRUE),
                CtrlPopShoot=mean(ShootMass.g, na.rm=TRUE), CtrlPopShootSE=se(ShootMass.g, na.rm=TRUE))

####Flood, Origin + Lat####
mff.dk<-read.table("MatFxFlood.dk.txt", header=T, sep="\t", quote='"', row.names=1) #flood, dk only
head(mff.dk)
#no gaussian
fLR <- lapply(names(mff.dk)[17:19],function(n) CGtrait.LR.int(n,mff.dk, family=poisson)) #wilt, totwilt, death, all poisson
# names(fLR) <- names(f)[20:21]
# 
# fmodels <- lapply(names(f)[20:21],function(n) CGtrait.models.int(n,f))
# fmodels

#merge mfcu and mom df
str(mfmom.dk)
str(mff.dk)
totmff <- merge(mfmom.dk,mff.dk, all.y=TRUE )

#flood, death
modeldata<-totmff[!is.na(totmff$DeathDay),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

modelobar<-lmer(DeathDay ~ Origin *Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1raw<-lmer(DeathDay ~ Origin *Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1raw)
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

# #modelI <- lmer(DeathDay ~ Origin * CtrlPopShoot+ Latitude + (1|PopID), family=poisson,data=modeldata)
# #anova(modelI, model2raw)
# #modelL <- lmer(DeathDay ~ Origin * CtrlPopShoot+(1|PopID), family=poisson,data=modeldata)
# #anova(modelL, modelI)
# #modelCint <- lmer(DeathDay ~ Origin + CtrlPopShoot+Latitude+(1|PopID), family=poisson,data=modeldata)
# #anova(modelI, modelCint)
# #modelC <- lmer(DeathDay ~ Origin +Latitude+(1|PopID), family=poisson,data=modeldata)
# #anova(modelCint, modelC)
# #modelOraw<-lmer(DeathDay ~ Latitude+(1|PopID), family=poisson,data=modeldata)
# #anova(modelOraw,modelC) #test for significance of origin - origin NOT sig....!
# 
modelg <- glm(DeathDay ~ Origin*CtrlPopShoot*Latitude, family=poisson,data=modeldata)
modelg1 <- glm(DeathDay ~ Origin*CtrlPopShoot+Latitude, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(pval,1,lower=FALSE)#chisq value
modelg3<- glm(DeathDay ~ Origin*CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(pval,1,lower=FALSE)#chisq value
modelg2<- glm(DeathDay ~Origin +CtrlPopShoot+Latitude, family=poisson,data=modeldata)
anova(modelg2,modelg1, test="LRT")
qchisq(pval,1,lower=FALSE)#chisq value
modelg4 <- glm(DeathDay ~Origin+Latitude, family=poisson, data=modeldata)
anova(modelg4, modelg2, test="LRT")
modelg5 <- glm(DeathDay~CtrlPopShoot+Latitude, family=poisson, data=modeldata)
anova(modelg2, modelg5, test="LRT")
# # 
# # qplot(data=modeldata,CtrlPopShoot, DeathDay, color = Origin)+geom_point(position="jitter")
# # moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopShoot), summarize, popCount=length(PopID), popDeathDay=mean(DeathDay))
# # qplot(data=moddata,CtrlPopShoot, popDeathDay, color = Origin, 
# #       xlab="Population mean shoot mass in control treatment", 
# #       ylab="Population mean days to DeathDay in herbivory treatment", main="Performance in herbivory vs. control treatments") +geom_smooth(method=glm, se=TRUE)
# # 

#flood, root death
modeldata<-totmff[!is.na(totmff$FloatDay),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

modelobar<-lmer(FloatDay ~ Origin *Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1raw<-lmer(FloatDay ~ Origin *Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1raw)
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


#explicit trade-off using shootmass
modeldata <- merge(modeldata, comeans, all.x=TRUE)
modeldata <- modeldata[!is.na(modeldata$CtrlPopShoot),]

modelobar<-lmer(FloatDay ~ Origin * CtrlPopShoot*Latitude +(Origin|PopID), family=poisson,data=modeldata)
model1raw<-lmer(FloatDay ~ Origin * CtrlPopShoot* Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(FloatDay ~ Origin * CtrlPopShoot* Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(FloatDay ~ Origin * CtrlPopShoot* Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(56.023,1)

modelg <- glm(FloatDay ~ Origin*CtrlPopShoot*Latitude, family=poisson,data=modeldata)
modelg1 <- glm(FloatDay ~ Origin*CtrlPopShoot+Latitude, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(pval,1,lower=FALSE)#chisq value
modelg3<- glm(FloatDay ~ Origin*CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(pval,1,lower=FALSE)#chisq value
modelg2<- glm(FloatDay ~Origin +CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg2,modelg3, test="LRT")
qchisq(pval,1,lower=FALSE)#chisq value
modelg4 <- glm(FloatDay ~Origin+Latitude, family=poisson, data=modeldata)
anova(modelg4, modelg2, test="LRT")
modelg5 <- glm(FloatDay~CtrlPopShoot+Latitude, family=poisson, data=modeldata)
anova(modelg2, modelg5, test="LRT")
# # 
# # qplot(data=modeldata,CtrlPopShoot, FloatDay, color = Origin)+geom_point(position="jitter")
# # moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopShoot), summarize, popCount=length(PopID), popFloatDay=mean(FloatDay))
# # qplot(data=moddata,CtrlPopShoot, popFloatDay, color = Origin, 
# #       xlab="Population mean shoot mass in control treatment", 
# #       ylab="Population mean days to FloatDay in herbivory treatment", main="Performance in herbivory vs. control treatments") +geom_smooth(method=glm, se=TRUE)
# # 

#flood, yellow
modeldata<-totmff[!is.na(totmff$YellowDay),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

modelobar<-lmer(YellowDay ~ Origin *Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1raw<-lmer(YellowDay ~ Origin *Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1raw)
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


#explicit trade-off using shootmass
modeldata <- merge(modeldata, comeans, all.x=TRUE)
modeldata <- modeldata[!is.na(modeldata$CtrlPopShoot),]

modelobar<-lmer(YellowDay ~ Origin * CtrlPopShoot*Latitude +(Origin|PopID), family=poisson,data=modeldata)
model1raw<-lmer(YellowDay ~ Origin * CtrlPopShoot* Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(YellowDay ~ Origin * CtrlPopShoot* Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(YellowDay ~ Origin * CtrlPopShoot* Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(56.023,1)

modelg <- glm(YellowDay ~ Origin*CtrlPopShoot*Latitude, family=poisson,data=modeldata)
modelg1 <- glm(YellowDay ~ Origin*CtrlPopShoot+Latitude, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(pval,1,lower=FALSE)#chisq value
modelg3<- glm(YellowDay ~ Origin*CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(pval,1,lower=FALSE)#chisq value
modelg2<- glm(YellowDay ~Origin +CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg2,modelg3, test="LRT")
qchisq(pval,1,lower=FALSE)#chisq value
modelg4 <- glm(YellowDay ~Origin+Latitude, family=poisson, data=modeldata)
anova(modelg4, modelg2, test="LRT")
modelg5 <- glm(YellowDay~CtrlPopShoot+Latitude, family=poisson, data=modeldata)
anova(modelg2, modelg5, test="LRT")
# # 
# # qplot(data=modeldata,CtrlPopShoot, YellowDay, color = Origin)+geom_point(position="jitter")
# # moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopShoot), summarize, popCount=length(PopID), popYellowDay=mean(YellowDay))
# # qplot(data=moddata,CtrlPopShoot, popYellowDay, color = Origin, 
# #       xlab="Population mean shoot mass in control treatment", 
# #       ylab="Population mean days to YellowDay in herbivory treatment", main="Performance in herbivory vs. control treatments") +geom_smooth(method=glm, se=TRUE)
# # 
