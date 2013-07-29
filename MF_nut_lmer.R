#MF stress treatment - nutrient deficiency
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

modelobar<-lmer(lxwH ~ Origin *Latitude +(Origin|PopID/Mom), family=gaussian,data=modeldata)
model1raw<-lmer(lxwH ~ Origin *Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(lxwH ~ Origin *Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(lxwH ~ Origin *Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
print(anova(model2raw,model1raw),digits=22) # Mom not sig
(lambda <- (-2)*(-109.0070901470973012692 - (-109.0071559301389783059)))
1-pchisq(-0.0001315661,1)
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(0.1311,1)

# modelI <- lmer(lxwH ~ Origin +Latitude +(1|blank), family=gaussian,data=modeldata)
# anova(modelI, model3raw)
# 
# modelL<-lmer(lxwH ~ Origin +(1|blank), family=gaussian,data=modeldata)
# anova(modelL, modelI)
# 
# modelOraw<-lmer(lxwH ~ (1|blank), family=gaussian,data=modeldata)
# anova(modelOraw,modelL)
# 
# model1.1<-lmer(lxwH ~ Origin +Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
# step(model1.1)

interaction.plot(response = modeldata$lxwH, x.factor = modeldata$Latitude, trace.factor = modeldata$Origin)
qplot(data=modeldata, Latitude, lxwH, color=Origin, geom="jitter")
#try glm
# modeldata <- modeldata[modeldata$lxwH<111,] #try without outlier
modelg <- glm(lxwH ~ Origin*Latitude, family=gaussian,data=modeldata)
modelg1 <- glm(lxwH ~ Origin+Latitude, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.2511,1,lower=FALSE)#chisq value

modelg3<- glm(lxwH ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(0.2908,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
# modelg2<- glm(lxwH ~ Latitude, family=gaussian,data=modeldata)
# anova(modelg2,modelg1, test="LRT")
qchisq(0.00866,1,lower=FALSE)#chisq value

lsmeans(modelg3, ~Origin, conf=95)

# checking the normality of residuals e_i:
qqnorm(resid(modelg), main="Q-Q plot for residuals")
qqline(resid(modelg))


#explicit trade-off using shootmass
modeldata <- merge(modeldata, comeans, all.x=TRUE)
modeldata <- modeldata[!is.na(modeldata$CtrlPopShoot),]

modelobar<-lmer(lxwH ~ Origin * CtrlPopShoot*Latitude +(Origin|PopID/Mom), family=gaussian,data=modeldata)
model1raw<-lmer(lxwH ~ Origin * CtrlPopShoot* Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(lxwH ~ Origin * CtrlPopShoot* Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(lxwH ~ Origin * CtrlPopShoot* Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(56.023,1)

modelI <- lmer(lxwH ~ Origin * CtrlPopShoot+ Latitude + (1|PopID), family=gaussian,data=modeldata)
anova(modelI, model2raw)
modelL <- lmer(lxwH ~ Origin * CtrlPopShoot+(1|PopID), family=gaussian,data=modeldata)
anova(modelL, modelI)
modelCint <- lmer(lxwH ~ Origin + CtrlPopShoot+(1|PopID), family=gaussian,data=modeldata)
anova(modelL, modelCint)
modelC <- lmer(lxwH ~ Origin +(1|PopID), family=gaussian,data=modeldata)
anova(modelCint, modelC)
modelOraw<-lmer(lxwH ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelOraw,modelC) #test for significance of origin - origin NOT sig....!

qplot(data=modeldata,CtrlPopShoot, lxwH, color = Origin)+geom_point(position="jitter")
moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopShoot), summarize, popCount=length(PopID), poplxwH=mean(lxwH))
qplot(data=moddata,CtrlPopShoot, poplxwH, color = Origin, 
      xlab="Population mean shoot mass in control treatment", 
      ylab="Population mean days to lxwH in herbivory treatment", main="Performance in herbivory vs. control treatments") +geom_smooth(method=glm, se=TRUE)


##nut, shoot mass
modeldata<-totmfn[!is.na(totmfn$ShootMass.g),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar<-lmer(ShootMass.g ~ Origin *Latitude +(Origin|PopID/Mom), family=gaussian,data=modeldata)
model1raw<-lmer(ShootMass.g ~ Origin *Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1raw)
1-pchisq(0.0838,4)
model2raw<-lmer(ShootMass.g ~ Origin *Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(ShootMass.g ~ Origin *Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
print(anova(model2raw,model1raw),digits=22) # Mom not sig
(lambda <- (-2)*(-20.56186352173015308153 - (-20.95029136041199535612)))
1-pchisq(-0.7768557,1)
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(1.2991,1)

# modelI <- lmer(ShootMass.g ~ Origin +Latitude +(1|blank), family=gaussian,data=modeldata)
# anova(modelI, model3raw)
# 
# modelL<-lmer(ShootMass.g ~ Origin +(1|blank), family=gaussian,data=modeldata)
# anova(modelL, modelI)
# 
# modelOraw<-lmer(ShootMass.g ~ (1|blank), family=gaussian,data=modeldata)
# anova(modelOraw,modelL)

#try glm
modelg <- glm(ShootMass.g ~ Origin*Latitude, family=gaussian,data=modeldata)
modelg1 <- glm(ShootMass.g ~ Origin+Latitude, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.4259,1,lower=FALSE)#chisq value

modelg3<- glm(ShootMass.g ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(0.9257,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
# modelg2<- glm(ShootMass.g ~ Latitude, family=gaussian,data=modeldata)
# anova(modelg2,modelg1)
qchisq(0.2122,1,lower=FALSE)#chisq value

lsmeans(modelg3, ~Origin, conf=95)


#explicit trade-off using shootmass
modeldata <- merge(modeldata, comeans, all.x=TRUE)
modeldata <- modeldata[!is.na(modeldata$CtrlPopShoot),]

modelobar<-lmer(ShootMass.g ~ Origin * CtrlPopShoot*Latitude +(Origin|PopID/Mom), family=gaussian,data=modeldata)
model1raw<-lmer(ShootMass.g ~ Origin * CtrlPopShoot* Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(ShootMass.g ~ Origin * CtrlPopShoot* Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(ShootMass.g ~ Origin * CtrlPopShoot* Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(56.023,1)

modelI <- lmer(ShootMass.g ~ Origin * CtrlPopShoot+ Latitude + (1|PopID), family=gaussian,data=modeldata)
anova(modelI, model2raw)
modelL <- lmer(ShootMass.g ~ Origin * CtrlPopShoot+(1|PopID), family=gaussian,data=modeldata)
anova(modelL, modelI)
modelCint <- lmer(ShootMass.g ~ Origin + CtrlPopShoot+(1|PopID), family=gaussian,data=modeldata)
anova(modelL, modelCint)
modelC <- lmer(ShootMass.g ~ Origin +(1|PopID), family=gaussian,data=modeldata)
anova(modelCint, modelC)
modelOraw<-lmer(ShootMass.g ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelOraw,modelC) #test for significance of origin - origin NOT sig....!

# # qplot(data=modeldata,CtrlPopShoot, ShootMass.g, color = Origin)+geom_point(position="jitter")
# # moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopShoot), summarize, popCount=length(PopID), popShootMass.g=mean(ShootMass.g))
# # qplot(data=moddata,CtrlPopShoot, popShootMass.g, color = Origin, 
# #       xlab="Population mean shoot mass in control treatment", 
# #       ylab="Population mean days to ShootMass.g in herbivory treatment", main="Performance in herbivory vs. control treatments") +geom_smooth(method=glm, se=TRUE)
# # 

##nut, root crown
modeldata<-totmfn[!is.na(totmfn$CrownDiam.mm),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar<-lmer(CrownDiam.mm ~ Origin *Latitude +(Origin|PopID/Mom), family=gaussian,data=modeldata)
model1raw<-lmer(CrownDiam.mm ~ Origin *Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar,model1raw)
model2raw<-lmer(CrownDiam.mm ~ Origin *Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(CrownDiam.mm ~ Origin *Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
print(anova(model2raw,model1raw),digits=22) # Mom not sig
(lambda <- (-2)*(-109.0070897781463088450 - (-109.0071141940671139992)))
1-pchisq(-4.883184e-05,1)
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(0.2437,1)

# modelI <- lmer(CrownDiam.mm ~ Origin +Latitude +(1|blank), family=gaussian,data=modeldata)
# anova(modelI, model3raw)
# 
# modelL<-lmer(CrownDiam.mm ~ Origin +(1|blank), family=gaussian,data=modeldata)
# anova(modelL, modelI)
# 
# modelOraw<-lmer(CrownDiam.mm ~ (1|blank), family=gaussian,data=modeldata)
# anova(modelOraw,modelL)

#try glm
modelg <- glm(CrownDiam.mm ~ Origin*Latitude, family=gaussian,data=modeldata)
modelg1 <- glm(CrownDiam.mm ~ Origin+Latitude, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.14,1,lower=FALSE)#chisq value

modelg3<- glm(CrownDiam.mm ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(0.2331,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
# modelg2<- glm(CrownDiam.mm ~ Latitude, family=gaussian,data=modeldata)
# anova(modelg2,modelg1, test="LRT")
qchisq(0.5099,1,lower=FALSE)#chisq value

lsmeans(modelg3, ~Origin, conf=95)

 
#explicit trade-off using shootmass
modeldata <- merge(modeldata, comeans, all.x=TRUE)
modeldata <- modeldata[!is.na(modeldata$CtrlPopShoot),]

modelobar<-lmer(CrownDiam.mm ~ Origin * CtrlPopShoot+Latitude +(Origin|PopID/Mom), family=gaussian,data=modeldata)
model1raw<-lmer(CrownDiam.mm ~ Origin * CtrlPopShoot+ Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(CrownDiam.mm ~ Origin * CtrlPopShoot+ Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(CrownDiam.mm ~ Origin * CtrlPopShoot+ Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(56.023,1)

modelI <- lmer(CrownDiam.mm ~ Origin * CtrlPopShoot+ Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelI, model1raw)
modelL <- lmer(CrownDiam.mm ~ Origin * CtrlPopShoot+(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelL, modelI)
modelCint <- lmer(CrownDiam.mm ~ Origin + CtrlPopShoot+(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelL, modelCint)
modelC <- lmer(CrownDiam.mm ~ Origin+(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelCint, modelC)
modelOraw<-lmer(CrownDiam.mm ~ (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelOraw,modelC) #test for significance of origin - origin NOT sig....!

qplot(data=modeldata,CtrlPopShoot, CrownDiam.mm, color = Origin)+geom_point(position="jitter")
moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopShoot), summarize, popCount=length(PopID), popCrownDiam.mm=mean(CrownDiam.mm))
qplot(data=moddata,CtrlPopShoot, popCrownDiam.mm, color = Origin, 
      xlab="Population mean shoot mass in control treatment", 
      ylab="Population mean days to CrownDiam.mm in herbivory treatment", main="Performance in herbivory vs. control treatments") +geom_smooth(method=glm, se=TRUE)


##nut lf count
modeldata<-totmfn[!is.na(totmfn$LfCountH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar<-lmer(LfCountH ~ Origin *Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1raw<-lmer(LfCountH ~ Origin *Latitude +(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1raw)
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


#explicit trade-off using shootmass
modeldata <- merge(modeldata, comeans, all.x=TRUE)
modeldata <- modeldata[!is.na(modeldata$CtrlPopShoot),]

modelobar<-lmer(LfCountH ~ Origin * CtrlPopShoot*Latitude +(Origin|PopID/Mom), family=poisson,data=modeldata)
model1raw<-lmer(LfCountH ~ Origin * CtrlPopShoot* Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(LfCountH ~ Origin * CtrlPopShoot* Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(LfCountH ~ Origin * CtrlPopShoot* Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(56.023,1)

modelg <- glm(LfCountH ~ Origin*CtrlPopShoot*Latitude, family=poisson,data=modeldata)
modelg1 <- glm(LfCountH ~ Origin*CtrlPopShoot+Latitude, family=poisson,data=modeldata)
anova(modelg1, modelg, test="LRT") #'Deviance' is chisq value
qchisq(pval,1,lower=FALSE)#chisq value
modelg3<- glm(LfCountH ~ Origin*CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(pval,1,lower=FALSE)#chisq value
modelg2<- glm(LfCountH ~Origin +CtrlPopShoot, family=poisson,data=modeldata)
anova(modelg2,modelg3, test="LRT")
qchisq(pval,1,lower=FALSE)#chisq value
modelg4 <- glm(LfCountH ~Origin, family=poisson, data=modeldata)
anova(modelg4, modelg2, test="LRT")
modelg5 <- glm(LfCountH~CtrlPopShoot, family=poisson, data=modeldata)
anova(modelg2, modelg5, test="LRT")
# # 
# # qplot(data=modeldata,CtrlPopShoot, LfCountH, color = Origin)+geom_point(position="jitter")
# # moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopShoot), summarize, popCount=length(PopID), popLfCountH=mean(LfCountH))
# # qplot(data=moddata,CtrlPopShoot, popLfCountH, color = Origin, 
# #       xlab="Population mean shoot mass in control treatment", 
# #       ylab="Population mean days to LfCountH in herbivory treatment", main="Performance in herbivory vs. control treatments") +geom_smooth(method=glm, se=TRUE)
# # 
