#MF stress treatment - simulated herbivory
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
modeldata<-totmfcu[!is.na(totmfcu$bolt.bin),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar<-lmer(bolt.bin ~ Origin * Latitude +(Origin|PopID/Mom), family=binomial,data=modeldata)
model1<-lmer(bolt.bin ~ Origin * Latitude +(1|PopID/Mom), family=binomial,data=modeldata)
anova(modelobar, model1)
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

 
#explicit trade-off using shootmass
modeldata <- merge(modeldata, comeans, all.x=TRUE)
modeldata <- modeldata[!is.na(modeldata$CtrlPopShoot),]
xtabs(bolt.bin~Origin+CtrlPopShoot, data=modeldata)

# modelobar<-lmer(bolt.bin ~ Origin * CtrlPopShoot*Latitude +(Origin|PopID/Mom), family=binomial,data=modeldata)
# model1raw<-lmer(bolt.bin ~ Origin * CtrlPopShoot* Latitude + (1|PopID/Mom), family=binomial,data=modeldata)
# anova(modelobar, model1raw)
# model2raw<-lmer(bolt.bin ~ Origin * CtrlPopShoot* Latitude +(1|PopID), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3raw<-lmer(bolt.bin ~ Origin * CtrlPopShoot* Latitude +(1|blank), family=binomial,data=modeldata) # Test population effect
# anova(model2raw,model1raw) # mom not sig
# anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# 1-pchisq(56.023,1)

modelg <- glm(bolt.bin ~ Origin*CtrlPopShoot*Latitude, family=binomial,data=modeldata)
modelg1 <- glm(bolt.bin ~ Origin*CtrlPopShoot+Latitude, family=binomial,data=modeldata)
anova(modelg1, modelg, test="LRT") #'Deviance' is chisq value
qchisq(pval,1,lower=FALSE)#chisq value
modelg3<- glm(bolt.bin ~ Origin*CtrlPopShoot, family=binomial,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(pval,1,lower=FALSE)#chisq value
modelg2<- glm(bolt.bin ~Origin +CtrlPopShoot+Latitude, family=binomial,data=modeldata)
anova(modelg2,modelg1, test="LRT")
qchisq(0.02068,1,lower=FALSE)#chisq value
modelg4 <- glm(bolt.bin ~Origin+Latitude, family=binomial, data=modeldata)
anova(modelg4, modelg2)
modelg5 <- glm(bolt.bin~CtrlPopShoot+Latitude, family=binomial, data=modeldata)
anova(modelg5, modelg2)
modelg6 <- glm(bolt.bin ~ Origin+CtrlPopShoot, family=binomial,data=modeldata)
anova(modelg6, modelg3)
modelg7 <- glm(bolt.bin ~Origin, family=binomial, data=modeldata)
anova(modelg4, modelg7)

summary(modelg1)

qplot(data=modeldata,CtrlPopShoot, bolt.bin, color = Origin)+geom_point(position="jitter")
moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopShoot), summarize, popCount=length(PopID), popbolt.bin=mean(bolt.bin))
qplot(data=moddata,CtrlPopShoot, popbolt.bin, color = Origin, 
      xlab="Population mean shoot mass in control treatment", 
      ylab="Population mean days to bolt.bin in herbivory treatment", main="Performance in herbivory vs. control treatments") +geom_smooth(method=glm, se=TRUE)


####cut, lf count, harvest, mom is sig, do by hand###
modeldata<-totmfcu[!is.na(totmfcu$LfCountH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar<-lmer(LfCountH ~ Origin * Latitude + (Origin|PopID/Mom), family=poisson,data=modeldata)
model1raw<-lmer(LfCountH ~ Origin * Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelobar, model1raw)
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

modelI <- lmer(LfCountH ~ Origin * CtrlPopShoot+ Latitude + (1|PopID/Mom), family=poisson,data=modeldata)
anova(modelI, model1raw)
modelL <- lmer(LfCountH ~ Origin * CtrlPopShoot+(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelL, modelI)
modelCint <- lmer(LfCountH ~ Origin + CtrlPopShoot+Latitude+(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelI, modelCint)
modelC <- lmer(LfCountH ~ Origin +Latitude+(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelCint, modelC)
modelOraw<-lmer(LfCountH ~ Latitude+(1|PopID/Mom), family=poisson,data=modeldata)
anova(modelOraw,modelC) #test for significance of origin - origin NOT sig....!


# # qplot(data=modeldata,CtrlPopShoot, LfCountH, color = Origin)+geom_point(position="jitter")
# # moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopShoot), summarize, popCount=length(PopID), popLfCountH=mean(LfCountH))
# # qplot(data=moddata,CtrlPopShoot, popLfCountH, color = Origin, 
# #       xlab="Population mean shoot mass in control treatment", 
# #       ylab="Population mean days to LfCountH in herbivory treatment", main="Performance in herbivory vs. control treatments") +geom_smooth(method=glm, se=TRUE)
# # 

##cut, crown
modeldata<-totmfcu[!is.na(totmfcu$CrownDiam.mm),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$Mom<-as.factor(modeldata$Mom)

modelobar<-lmer(CrownDiam.mm ~ Origin *Latitude +(Origin|PopID/Mom), family=gaussian,data=modeldata)
model1raw<-lmer(CrownDiam.mm ~ Origin *Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1raw)
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
qchisq(0.8629,1,lower=FALSE)#chisq value

modelg3<- glm(CrownDiam.mm ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(0.8342,1,lower=FALSE)#chisq value
anova(modelg3, test="LRT")
# modelg2<- glm(CrownDiam.mm ~ Latitude, family=gaussian,data=modeldata)
# anova(modelg2,modelg1, test="LRT")
qchisq(0.316,1,lower=FALSE)#chisq value

lsmeans(modelg3, ~Origin, conf=95)

# checking the normality of residuals e_i:
plot(resid(modelg3) ~ fitted(modelg3),main="residual plot")
abline(h=0)
qqnorm(resid(modelg3), main="Q-Q plot for residuals")
qqline(resid(modelg3))


#explicit trade-off using shootmass
modeldata <- merge(modeldata, comeans, all.x=TRUE)
modeldata <- modeldata[!is.na(modeldata$CtrlPopShoot),]

modelobar<-lmer(CrownDiam.mm ~ Origin * CtrlPopShoot*Latitude +(Origin|PopID/Mom), family=gaussian,data=modeldata)
model1raw<-lmer(CrownDiam.mm ~ Origin * CtrlPopShoot* Latitude + (1|PopID/Mom), family=gaussian,data=modeldata)
anova(modelobar, model1raw)
model2raw<-lmer(CrownDiam.mm ~ Origin * CtrlPopShoot* Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(CrownDiam.mm ~ Origin * CtrlPopShoot* Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
(lambda <- (-2)*(-70.909 - (-75.435)))
1-pchisq(9.052,1)

modelI <- lmer(CrownDiam.mm ~ Origin * CtrlPopShoot+ Latitude + (1|PopID), family=gaussian,data=modeldata)
anova(modelI, model2raw)
modelL <- lmer(CrownDiam.mm ~ Origin * CtrlPopShoot+(1|PopID), family=gaussian,data=modeldata)
anova(modelL, modelI)
modelCint <- lmer(CrownDiam.mm ~ Origin + CtrlPopShoot+(1|PopID), family=gaussian,data=modeldata)
anova(modelL, modelCint)
modelC <- lmer(CrownDiam.mm ~ Origin +(1|PopID), family=gaussian,data=modeldata)
anova(modelCint, modelC)
modelOraw<-lmer(CrownDiam.mm ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelOraw,modelC) #test for significance of origin - origin NOT sig....!

# # qplot(data=modeldata,CtrlPopShoot, CrownDiam.mm, color = Origin)+geom_point(position="jitter")
# # moddata <- ddply(modeldata, .(PopID, Origin, Latitude, CtrlPopShoot), summarize, popCount=length(PopID), popCrownDiam.mm=mean(CrownDiam.mm))
# # qplot(data=moddata,CtrlPopShoot, popCrownDiam.mm, color = Origin, 
# #       xlab="Population mean shoot mass in control treatment", 
# #       ylab="Population mean days to CrownDiam.mm in herbivory treatment", main="Performance in herbivory vs. control treatments") +geom_smooth(method=glm, se=TRUE)
# # 