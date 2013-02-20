###Mat FX mixed FX models, focused on Origin (no latitude)###
#Stress Tolerance, REML, using lme4
#mixed effect models 
library(lme4)

# #for each normal trait, compare this general set of models
# model1<-lmer(trait  ~ Origin +(1|PopID/Mom), family=gaussian,data=modeldata)
# model2<-lmer(trait  ~ Origin + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(trait  ~ Origin + (1|blank), family=gaussian,data=modeldata) # Test population effect
# anova(model2,model1) # mom is sig!
# anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# modelO<-lmer(trait ~ (1|PopID), family=gaussian,data=modeldata)
# anova(modelO,model1) #test for significance of origin - origin only marginally sig....!
# 
# #test for one trait, one df, specify non default family
# lfcountLR<- CGtrait.LR.O("LfCount1",al, family=poisson)
# 
# #test for one trait, one df
# shootmod <- CGtrait.models.O("ShootMass.gA", al) #test one trait
# 
# #for all traits in a df
# #make sure all traits analyzed this way are the same distribution
# names(al)#find col numbers for traits of interestes
# alLR <- lapply(names(al)[8:13],function(n) CGtrait.LR.O(n,al))#apply func to all things in list
# names(alLR) <- names(al)[8:13]
# almodels <- lapply(names(al)[8:13],function(n) CGtrait.models.O(n,al))#apply func to all things in list
# names(almodels) <- names(al)[8:13]
# 
# #to get one model
# almodels[[1]][1] #first number is trait in column order of df, second number is model number
# names(almodels[1]) #to verify trait
# 

######Allo, Origin only models######
mfallo.dk<-read.table("MFallo.dk.txt", header=T, sep="\t", quote='"', row.names=1) #allo, dk only
head(mfallo.dk)
#mfallo.dk$lxw <- mfallo.dk$LfLgth1 * mfallo.dk$LfWdth1
str(mfallo.dk)
alLR <- lapply(names(mfallo.dk)[c(10:11)],function(n) CGtrait.LR.O(n,mfallo.dk)) #crow, shoot, all gaussian
#names(alLR) <- names(mfallo.dk)[c(11:13, 20)]
alLR #check out LRs of models. Model progression logical?
almodels <- CGtrait.models.O("Mass.gA",mfallo.dk)
alMass.lmer <- almodels$model2
qqnorm(resid(alMass.lmer), main="Q-Q plot for residuals")
qqline(resid(alMass.lmer))

# #####m1, Origin only#####
mfcom1<-read.table("MatFxBonusCtrlM1.txt", header=T, sep="\t", quote='"', row.names=1) #m1 all plants in analysis, balanced, dk only
head(mfcom1)
# mfcom1$lxw <- mfcom1$LfLgth1*mfcom1$LfWdth1
# write.table(mfcom1, file="MatFxBonusCtrlM1.txt", sep="\t", quote=F)
m1lxw <- CGtrait.LR.O("lxw", mfcom1)
m1lxwMod <- CGtrait.models.O("lxw", mfcom1)
m1lxw.lmer <- m1lxwMod$model1
qqnorm(resid(m1lxw.lmer), main="Q-Q plot resid MF m1 lxw")
qqline(resid(m1lxw.lmer))

m1lf <- CGtrait.LR.O("LfCount1", mfcom1, family=poisson)#poisson distribution

###m1, lxw, cross sig, do by hand###
modeldata<-mfcom1[!is.na(mfcom1$lxw),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$CrossNum<-as.factor(modeldata$CrossNum)

model1<-lmer(lxw ~ Origin +(1|PopID/CrossNum), family=gaussian,data=modeldata)
model2<-lmer(lxw ~ Origin + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lxw ~ Origin + (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelO<-lmer(lxw ~ (1|PopID/CrossNum), family=gaussian,data=modeldata)
anova(modelO,model1) #test for significance of origin - origin not sig....?

###m1, lfcount, cross sig, do by hand###
modeldata<-mfcom1[!is.na(mfcom1$LfCount1),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$CrossNum<-as.factor(modeldata$CrossNum)

model1<-lmer(LfCount1  ~ Origin+ (1|PopID/CrossNum), family=poisson,data=modeldata)
model2<-lmer(LfCount1  ~ Origin+  (1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(LfCount1  ~ Origin+ (1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelO<-lmer(LfCount1 ~ (1|PopID/CrossNum), family=poisson,data=modeldata)
anova(modelO,model1) #test for significance of origin - origin only marginally sig....!

####Control, Origin only####
mfco.dk1<-read.table("MatFxBonusCtrl.txt", header=T, sep="\t", quote='"', row.names=1) #largest balanced control
head(mfco.dk1)
# mfco.dk1$lxwH <- mfco.dk1$LfLgthH * mfco.dk1$LfWdthH
coLR <- lapply(names(mfco.dk1)[c(12:13,21:22,41, 44)],function(n) CGtrait.LR.O(n,mfco.dk1)) #lfwdth, lflgth, crown, shoot, lxw, all gaussian
# names(coLR) <- names(mfco.dk1)[c(15:17,52:53)]
coLR #check out LRs of models. Model progression logical?


# comodels <- lapply(names(mfco.dk1)[c(12:13,21:22,41, 44)],function(n) CGtrait.models.O(n,mfco.dk1))
# m1lxw.lmer <- m1lxwMod$model1
# qqnorm(resid(m1lxw.lmer), main="Q-Q plot for residuals")
# qqline(resid(m1lxw.lmer))
# mfco.dk1$bolt.bin <- as.numeric(mfco.dk1$BoltedatH)-1
# mfco.dk1$BoltDay.adj <- mfco.dk1$BoltDay + 3
# write.table(mfco.dk1, file="MatFxBonusCtrl.txt", sep="\t", quote=F)
coBatH <- CGtrait.LR.O("bolt.bin", mfco.dk1, family=binomial)
coP <- lapply(names(mfco.dk1)[c(11,46)],function(n) CGtrait.LR.O(n,mfco.dk1, family=poisson)) #lfcountH, boltdate, all poisson

coBatHMod <- CGtrait.models.O("bolt.bin",mfco.dk1)
coBatHMod

int<- 0.14461 #inv mean
B<-0.38942 #Originnat estimate from model summary
pN<-exp(int+B)/(exp(int+B)+1) # Native
pI<-exp(int)/(exp(int)+1) # Introduced (B=0)
pI # 14.5% 
pN # 60%
#check by looking at percentages
summary(mfco.dk1[mfco.dk1$Origin=="nat",]) #56 rows, 34 boltedatH = 60%
summary(mfco.dk1[mfco.dk1$Origin=="inv",]) #55 rows, 8 boltedatH = 14.5%

####control, lxw, mom sig, so do by hand####
modeldata<-mfco.dk1[!is.na(mfco.dk1$lxwH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$CrossNum<-as.factor(modeldata$CrossNum)

model1raw<-lmer(lxwH ~ Origin  +(1|PopID/CrossNum), family=gaussian,data=modeldata)
model2raw<-lmer(lxwH ~ Origin  +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(lxwH ~ Origin  +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelOraw<-lmer(lxwH ~ (1|PopID/CrossNum), family=gaussian,data=modeldata)
anova(modelOraw,model1raw) #test for significance of origin - origin NOT sig....!

####control, lf count, cross sig so do by hand#####
#poisson on raw data
modeldata<-mfco.dk1[!is.na(mfco.dk1$LfCountH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$CrossNum<-as.factor(modeldata$CrossNum)

model1raw<-lmer(LfCountH ~ Origin  +(1|PopID/CrossNum), family=poisson,data=modeldata)
model2raw<-lmer(LfCountH ~Origin  +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(LfCountH ~ Origin  +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelOraw<-lmer(LfCountH ~(1|PopID/CrossNum), family=poisson,data=modeldata)
anova(modelOraw,model1raw) #test for significance of origin - origin NOT sig....!
model1raw
int<- 2.9941 #inv mean
B<--0.5973 #Originnat estimate from model summary
pI<-exp(int)
pN<-exp(int+B)
pI
pN

###control, boltday.adj, cross sig, do by hand###
#only bolters
modeldata<-mfco.dk1[!is.na(mfco.dk1$BoltDay.adj),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$CrossNum<-as.factor(modeldata$CrossNum)

model1<-lmer(BoltDay.adj ~ Origin +(1|PopID/CrossNum), family=poisson,data=modeldata)
model2<-lmer(BoltDay.adj ~ Origin +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(BoltDay.adj ~ Origin +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelO<-lmer(BoltDay.adj ~  (1|PopID/CrossNum), family=poisson,data=modeldata)
anova(modelO,model1) #test for significance of origin - origin not sig!
model1
int<- 4.2839#inv mean
B<--0.2360#Originnat estimate from model summary
pI<-exp(int)
pN<-exp(int+B)
pI
pN

###control, crown, cross sig, do by hand###
modeldata<-mfco.dk1[!is.na(mfco.dk1$CrownDiam.mm),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$CrossNum<-as.factor(modeldata$CrossNum)

model1raw<-lmer(CrownDiam.mm ~ Origin +(1|PopID/CrossNum), family=gaussian,data=modeldata)
model2raw<-lmer(CrownDiam.mm ~ Origin  +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(CrownDiam.mm ~ Origin  +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelOraw<-lmer(CrownDiam.mm ~ (1|PopID/CrossNum), family=gaussian,data=modeldata)
anova(model1raw,modelOraw) #test for significance of origin - origin NOT sig....!

###control, sla, cross sig, do by hand###
modeldata<-mfco.dk1[!is.na(mfco.dk1$sla),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$CrossNum<-as.factor(modeldata$CrossNum)

model1raw<-lmer(sla ~ Origin  +(1|PopID/CrossNum), family=gaussian,data=modeldata)
model2raw<-lmer(sla ~ Origin  +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(sla ~ Origin +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelOraw<-lmer(sla ~ (1|PopID/CrossNum), family=gaussian,data=modeldata)
anova(modelOraw,model1raw) #test for significance of origin - origin NOT sig....!

#####control, mass, extra covariates####
str(mfco.dk1)
modeldata<-mfco.dk1[!is.na(mfco.dk1$ShootMass.g),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$CrossNum<-as.factor(modeldata$CrossNum)

model1<-lmer(ShootMass.g ~ Origin * BoltedatH+(BoltedatH|PopID/CrossNum), family=gaussian,data=modeldata)
model2<-lmer(ShootMass.g ~ Origin * BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(ShootMass.g ~ Origin * BoltedatH+(BoltedatH|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(ShootMass.g ~ Origin + BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata)
anova(modelInt,model2) #test for significant interaction btw Origin and Bolted - not sig

modelB0<-lmer(ShootMass.g ~ Origin + (1|PopID), family=gaussian,data=modeldata)
modelB1<-lmer(ShootMass.g ~ Origin + BoltedatH+(1|PopID), family=gaussian,data=modeldata)
modelB2<-lmer(ShootMass.g ~ Origin + (BoltedatH|PopID), family=gaussian,data=modeldata)
anova(modelB0, modelInt)#test for sig of bolting - bolting sig
anova(modelB1, modelInt)#test for sig of bolting - bolting sig
anova(modelB2, modelInt)#test for sig of bolting - bolting sig

modelO<-lmer(ShootMass.g ~ BoltedatH+(1|PopID), family=gaussian,data=modeldata)
anova(modelO,modelB1) #test for significance of origin - origin not sig....?
modelB1

qqnorm(resid(modelB1), main="Q-Q plot resid MF ctrl mass covar")
qqline(resid(modelB1))

##tranform, control, mass, extra covar##
modeldata<-mfco.dk1[!is.na(mfco.dk1$ShootMass.g),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$CrossNum<-as.factor(modeldata$CrossNum)
modeldata$mass.log <- log(modeldata$ShootMass.g)

model1<-lmer(mass.log ~ Origin * BoltedatH+(BoltedatH|PopID/CrossNum), family=gaussian,data=modeldata)
model2<-lmer(mass.log ~ Origin * BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(mass.log ~ Origin * BoltedatH+(BoltedatH|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(mass.log ~ Origin + BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata)
anova(modelInt,model2) #test for significant interaction btw Origin and Bolted - not sig

modelB0<-lmer(mass.log ~ Origin + (1|PopID), family=gaussian,data=modeldata)
modelB1<-lmer(mass.log ~ Origin + BoltedatH+(1|PopID), family=gaussian,data=modeldata)
modelB2<-lmer(mass.log ~ Origin + (BoltedatH|PopID), family=gaussian,data=modeldata)
anova(modelB0, modelInt)#test for sig of bolting - bolting sig
anova(modelB1, modelInt)#test for sig of bolting - bolting sig
anova(modelB2, modelInt)#test for sig of bolting - bolting sig

modelO<-lmer(mass.log ~ BoltedatH+(1|PopID), family=gaussian,data=modeldata)
anova(modelO,modelB1) #test for significance of origin - origin not sig....?
modelB1

qqnorm(resid(modelB1), main="Q-Q plot resid MF ctrl mass.log covar")
qqline(resid(modelB1))

#####control, lf count, extra covariates######
str(mfco.dk1)
modeldata<-mfco.dk1[!is.na(mfco.dk1$LfCountH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$CrossNum<-as.factor(modeldata$CrossNum)

model1<-lmer(LfCountH ~ Origin * BoltedatH+(BoltedatH|PopID/CrossNum), family=poisson,data=modeldata)
model2<-lmer(LfCountH ~ Origin * BoltedatH+(BoltedatH|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(LfCountH ~ Origin * BoltedatH+(BoltedatH|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1)
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(LfCountH ~ Origin + BoltedatH+(BoltedatH|PopID/CrossNum), family=poisson,data=modeldata)
anova(modelInt,model1) #test for significant interaction btw Origin and Bolted - not sig

modelB0<-lmer(LfCountH ~ Origin + (1|PopID/CrossNum), family=poisson,data=modeldata)
modelB1<-lmer(LfCountH ~ Origin * BoltedatH+(1|PopID/CrossNum), family=poisson,data=modeldata)
modelB2<-lmer(LfCountH ~ Origin + (BoltedatH|PopID/CrossNum), family=poisson,data=modeldata)
anova(modelB0, model1)#test for sig of bolting - bolting not sig
anova(modelB1, model1)#test for sig of bolting - bolting not sig
anova(modelB2, model1)#test for sig of bolting - bolting not sig
#
modelO<-lmer(LfCountH ~ BoltedatH+(BoltedatH|PopID/CrossNum), family=poisson,data=modeldata)
anova(modelO,model1) #test for significance of origin - origin IS sig....!
model1
int<-2.95190#inv mean
B<--0.02217#Originnat estimate from model summary
pI<-exp(int)
pN<-exp(int+B)
pI
pN

#####control, boltday, extra covariates####
#given that it's bolted....
str(mfco.dk1)
modeldata<-mfco.dk1[!is.na(mfco.dk1$BoltDay.adj),] 
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$CrossNum<-as.factor(modeldata$CrossNum)

model1<-lmer(BoltDay.adj ~ Origin *CrownDiam.mm+(CrownDiam.mm|PopID/CrossNum), family=poisson,data=modeldata)
model2<-lmer(BoltDay.adj ~ Origin *CrownDiam.mm+(CrownDiam.mm|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(BoltDay.adj ~ Origin *CrownDiam.mm+(CrownDiam.mm|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(BoltDay.adj ~ Origin + CrownDiam.mm+(CrownDiam.mm|PopID/CrossNum), family=poisson,data=modeldata)
anova(modelInt,model1) #test for significant interaction btw Origin and Bolted - not sig
modelC0<-lmer(BoltDay.adj ~ Origin * CrownDiam.mm+(1|PopID/CrossNum), family=poisson,data=modeldata)
anova(modelC0, model1)#test for sig of crown - crown not sig
modelC1<-lmer(BoltDay.adj ~ Origin +(CrownDiam.mm|PopID/CrossNum), family=poisson,data=modeldata)
anova(modelC1,model1)

modelO<-lmer(BoltDay.adj ~ (CrownDiam.mm|PopID/CrossNum), family=poisson,data=modeldata)
anova(modelO,modelC1) #test for significance of origin - origin not sig!
modelC1

int<-4.33448#inv mean
B<--0.21767#Originnat estimate from model summary
pI<-exp(int)
pN<-exp(int+B)
pI
pN

####Nut def, Origin only####
mfn.dk<-read.table("MatFxNut.dk.txt", header=T, sep="\t", quote='"', row.names=1) #nut, dk only
head(mfn.dk)
xtabs(~Origin+BoltedatH, mfn.dk) # only one bolter... leaving in
# mfn.dk$lxwH <- mfn.dk$LfLgthH * mfn.dk$LfWdthH
# mfn.dk$bolt.bin <- as.numeric(mfn.dk$BoltedatH)-1
# mfn.dk$BoltDay.adj <- mfco.dk1$BoltDay + 3
# write.table(mfn.dk, file="MatFxNut.dk.txt", sep="\t", quote=F)

nLR <- lapply(names(mfn.dk)[c(20:21,27:28, 36)],function(n) CGtrait.LR.O(n, mfn.dk)) 
#lflgthH, lfwdthH, crown, shoot, lxwH, all gaussian
# names(nLR) <- names(al)[c(11:12, 15:17, 50:51)]
nLR #check out LRs of models. Model progression logical?
nmodels <- CGtrait.models.O("lxwH",mfn.dk)
# nmodels2 <- CGtrait.models("RootH.log",mfn.dk)
nmodels
nlxwh.lmer <- nmodels$model2
qqnorm(resid(nlxwh.lmer), main="Q-Q plot for residuals")
qqline(resid(nlxwh.lmer))

#non-gaussian?
# mfn.dk <- cbind(mfn.dk, bolt.bin=as.numeric(mfn.dk$BoltedatH)-1)
# write.table(n, file="STNutsubset.txt", sep="\t", quote=F)
# 
# #nBatH <- CGtrait.LR("bolt.bin", mfn.dk, family=binomial)# no invasive bolted!
nlfcount <- CGtrait.LR.O("LfCountH",mfn.dk, family=poisson) #lfcountH, all poisson

####Cut, Origin only####
mfcu.dk<-read.table("MatFxCut.dk.txt", header=T, sep="\t", quote='"', row.names=1) #cut, dk only
head(mfcu.dk)
cuLR <- CGtrait.LR.O("CrownDiam.mm",mfcu.dk) #crown all gaussian

#non-gaussian?
# mfcu.dk<- cbind(mfcu.dk, bolt.bin=as.numeric(mfcu.dk$BoltedatH)-1)
# write.table(mfcu.dk, file="MatFxCut.dk.txt", sep="\t", quote=F)
xtabs(~Origin+BoltedatH, mfcu.dk)
cuBatH <- CGtrait.LR.O("bolt.bin", mfcu.dk, family=binomial)
cuP <- lapply(names(mfcu.dk)[c(19,34)],function(n) CGtrait.LR.O(n,mfcu.dk, family=poisson)) #lfcountH, boltdate, all poisson

# cumodels <- CGtrait.models.O("RootH.log",cu)
# cumodels
# qqnorm(resid(cumodels$model2), main="Q-Q plot for residuals")
# qqline(resid(cumodels$model2))
# shapiro.test(resid(cumodels$model2))

####cut, lf count, harvest, mom is sig, do by hand###
modeldata<-mfcu.dk[!is.na(mfcu.dk$LfCountH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$CrossNum<-as.factor(modeldata$CrossNum)

model1raw<-lmer(LfCountH ~ Origin + (1|PopID/CrossNum), family=poisson,data=modeldata)
model2raw<-lmer(LfCountH ~ Origin  + (1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(LfCountH ~ Origin + (1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelOraw<-lmer(LfCountH ~ (1|PopID/CrossNum), family=poisson,data=modeldata)
anova(modelOraw,model1raw) #test for significance of origin 

###cut, harvest, bolt day, PopID not sig, do by hand###
#given that it's bolted....only 6 bolted total.

####Drought, Origin only####
mfd.dk<-read.table("MatFxDrought.dk.txt", header=T, sep="\t", quote='"', row.names=1) #drought, dk only
head(mfd.dk)
#no gaussian
dLR <- lapply(names(mfd.dk)[17:19],function(n) CGtrait.LR.O(n,mfd.dk, family=poisson)) #wilt, totwilt, death, all poisson
names(dLR) <- names(d)[17:19]

#drought, tot wilt
modeldata<-mfd.dk[!is.na(mfd.dk$TotWiltDay),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$CrossNum<-as.factor(modeldata$CrossNum)

model1raw<-lmer(TotWiltDay ~ Origin + (1|PopID/CrossNum), family=poisson,data=modeldata)
model2raw<-lmer(TotWiltDay ~ Origin  + (1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(TotWiltDay ~ Origin + (1|blank), family=poisson,data=modeldata) # Test population effect
anova(model1raw,model2raw) # mom not sig
anova(model2raw,model3raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelOraw<-lmer(TotWiltDay ~ (1|blank), family=poisson,data=modeldata)
anova(model3raw,modelOraw) #test for significance of origin 

####Flood, Origin only####
mff.dk<-read.table("MatFxFlood.dk.txt", header=T, sep="\t", quote='"', row.names=1) #flood, dk only
head(mff.dk)
#no gaussian
fLR <- lapply(names(mff.dk)[17:19],function(n) CGtrait.LR.O(n,mff.dk, family=poisson)) #wilt, totwilt, death, all poisson
# names(fLR) <- names(f)[20:21]
# 
# fmodels <- lapply(names(f)[20:21],function(n) CGtrait.models(n,f))
# fmodels

#flood, death
modeldata<-mff.dk[!is.na(mff.dk$DeathDay),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$CrossNum<-as.factor(modeldata$CrossNum)

model1raw<-lmer(DeathDay ~ Origin + (1|PopID/CrossNum), family=poisson,data=modeldata)
model2raw<-lmer(DeathDay ~ Origin  + (1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(DeathDay ~ Origin + (1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelOraw<-lmer(DeathDay ~ (1|blank), family=poisson,data=modeldata)
anova(model3raw,modelOraw) #test for significance of origin 


####Mom, Origin only####
mfmom.dk<-read.table("MatFxMom.dk.txt", header=T, sep="\t", quote='"', row.names=1) 
head(mfmom.dk)
# colnames(mfmom.dk)[7]<- "Mom"
# write.table(mfmom.dk, file="MatFxMom.dk.txt", sep="\t", quote=F)
# momLR <- lapply(names(mfmom.dk)[c(17, 6)],function(n) CGtrait.LR.O(n,mfmom.dk))
# germcount by hand
####Mom, seedwt###
str(mfmom.dk)
mfmom.dk$Origin<-droplevels(mfmom.dk$Origin)
mfmom.dk$CrossNum<-as.factor(mfmom.dk$CrossNum)
modeldata<-mfmom.dk[!is.na(mfmom.dk$SeedWt),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model1<-lmer(SeedWt ~ Origin  + (1|PopID/Mom), family=gaussian,data=modeldata)
model2<-lmer(SeedWt ~ Origin  +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(SeedWt ~ Origin  +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1)
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelO<-lmer(SeedWt ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,model2) #test for significance of origin - origin not sig....?

####Mom, germ count###
modeldata<-mfmom.dk[!is.na(mfmom.dk$GermCount),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model1<-lmer(GermCount ~ Origin + SeedCount +(1|PopID/Mom), family=poisson,data=modeldata)
model2<-lmer(GermCount ~ Origin  + SeedCount +(1|PopID), family=poisson, data=modeldata)
model3<-lmer(GermCount ~ Origin  + SeedCount + (1|blank), family=poisson, data=modeldata)
anova(model2, model1)
anova(model3, model2)

modelS0<-lmer(GermCount ~ Origin + (1|PopID), family=poisson, data=modeldata)
anova(modelS0, model2)

# modelW<-lmer(GermCount ~ Origin + SeedCount +SeedWt+(1|PopID), family=poisson, data=modeldata)
# anova(modelW, model2)

modelO<-lmer(GermCount ~ SeedCount + (1|PopID), family=poisson, data=modeldata)
anova(modelO, model2)
model2
int<-1.103892
#inv mean
B<--0.299416
#Originnat estimate from model summary
pI<-exp(int)
pN<-exp(int+B)
pI
pN

####Mom, germ date####avg, so can't use poisson
modeldata<-mfmom.dk[!is.na(mfmom.dk$GermAvgDate),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model1<-lmer(GermAvgDate ~ Origin  +(1|PopID/Mom), family=gaussian,data=modeldata)
model2<-lmer(GermAvgDate ~ Origin +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(GermAvgDate ~ Origin +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2, model1)
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

# modelW<-lmer(GermAvgDate ~ Origin +SeedWt+(1|PopID), family=gaussian,data=modeldata)
# anova(model2, modelW)

modelO<-lmer(GermAvgDate ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,model2) #test for significance of origin - origin not sig....?