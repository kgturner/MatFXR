###Mat FX mixed FX models, focused on Origin and Latitude###
#Stress Tolerance, REML, using lme4
#mixed effect models 
library(lme4)

#REWRITE FOR MAT FX FILES#

mfmom.dk<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #mom, dk only


# #for each normal trait, compare this general set of models
# model1<-lmer(trait  ~ Origin+ Latitude +(1|PopID/Mom), family=gaussian,data=modeldata)
# model2<-lmer(trait  ~ Origin+ Latitude + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(trait  ~ Origin+ Latitude + (1|blank), family=gaussian,data=modeldata) # Test population effect
# anova(model2,model1) # mom is sig!
# anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# modelL<-lmer(trait  ~ Origin + (1|PopID), family=gaussian,data=modeldata)
# anova(modelL, model1)
# modelO<-lmer(trait ~ Latitude +(1|PopID), family=gaussian,data=modeldata)
# anova(modelO,model1) #test for significance of origin - origin only marginally sig....!
# 
# #test for one trait, one df, specify non default family
# lfcountLR<- CGtrait.LR("LfCount1",al, family=poisson)
# 
# #test for one trait, one df
# shootLR<- CGtrait.LR("ShootMass.gA",al, family=gaussian)
# 
# #test for one trait, one df
# shootmod <- CGtrait.models("ShootMass.gA", al) #test one trait
# 
# #for all traits in a df
# #make sure all traits analyzed this way are the same distribution
# names(al)#find col numbers for traits of interestes
# alLR <- lapply(names(al)[8:13],function(n) CGtrait.LR(n,al))#apply func to all things in list
# names(alLR) <- names(al)[8:13]
# almodels <- lapply(names(al)[8:13],function(n) CGtrait.models(n,al))#apply func to all things in list
# names(almodels) <- names(al)[8:13]
# 
# #to get one model
# almodels[[1]][1] #first number is trait in column order of df, second number is model number
# names(almodels[1]) #to verify trait
# 
######Allo, Origin + Lat models######
mfallo.dk<-read.table("MFallo.dk.txt", header=T, sep="\t", quote='"', row.names=1) #allo, dk only
head(mfallo.dk)
mfallo.dk$lxw <- mfallo.dk$LfLgth1 * mfallo.dk$LfWdth1
str(mfallo.dk)
alLR <- lapply(names(mfallo.dk)[c(10:11)],function(n) CGtrait.LR(n,mfallo.dk)) #crow, shoot, all gaussian
#names(alLR) <- names(mfallo.dk)[c(11:13, 20)]
alLR #check out LRs of models. Model progression logical?
almodels <- CGtrait.models("CrownDiam.mmA",mfallo.dk)
almodels

# #####m1, Origin + Lat#####
mfcom1<-read.table("MF bonus control m1.txt", header=T, sep="\t", quote='"', row.names=1) #m1 all plants in analysis, balanced, dk only
head(mfcom1)
mfcom1$lxw <- mfcom1$LfLgth1*mfcom1$LfWdth1
write.table(mfcom1, file="MF bonus control m1.txt", sep="\t", quote=F)
m1lxw <- CGtrait.LR("lxw", mfcom1)
m1lf <- CGtrait.LR("LfCount1", mfcom1, family=poisson)#poisson distribution

###m1, lxw, cross sig, do by hand###
modeldata<-mfcom1[!is.na(mfcom1$lxw),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$CrossNum<-as.factor(modeldata$CrossNum)

model1<-lmer(lxw ~ Origin +Latitude+(1|PopID/CrossNum), family=gaussian,data=modeldata)
model2<-lmer(lxw ~ Origin +Latitude+ (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lxw ~ Origin +Latitude+ (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL<-lmer(lxw ~ Origin +(1|PopID/CrossNum), family=gaussian,data=modeldata)
anova(modelL, model1)
# 
# modelO<-lmer(lxw ~ (1|PopID/CrossNum), family=gaussian,data=modeldata)
# anova(modelO,modelL) #test for significance of origin - origin not sig....?
# #mom and popID sig, but not Origin! for either log or raw data

###m1, lfcount, cross sig, do by hand###
modeldata<-mfcom1[!is.na(mfcom1$LfCount1),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$CrossNum<-as.factor(modeldata$CrossNum)

model1<-lmer(LfCount1  ~ Origin+ Latitude +(1|PopID/CrossNum), family=poisson,data=modeldata)
model2<-lmer(LfCount1  ~ Origin+ Latitude + (1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(LfCount1  ~ Origin+ Latitude + (1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL<-lmer(LfCount1  ~ Origin + (1|PopID/CrossNum), family=poisson,data=modeldata)
anova(modelL, model1)
# 
# modelO<-lmer(LfCount1 ~ Latitude +(1|PopID/CrossNum), family=poisson,data=modeldata)
# anova(modelO,model1) #test for significance of origin - origin only marginally sig....!

####Control, Origin + Lat####
mfco.dk1<-read.table("Mat fx bonus control.txt", header=T, sep="\t", quote='"', row.names=1) #m1 all plants in analysis, balanced, dk only
head(mfco.dk1)
mfco.dk1$lxwH <- mfco.dk1$LfLgthH * mfco.dk1$LfWdthH
coLR <- lapply(names(mfco.dk1)[c(12:13,21:22,41, 44)],function(n) CGtrait.LR(n,mfco.dk1)) #crow, shoot, root, RootH.log, lxw, all gaussian
names(coLR) <- names(mfco.dk1)[c(15:17,52:53)]
coLR #check out LRs of models. Model progression logical?

mfco.dk1$bolt.bin <- as.numeric(mfco.dk1$BoltedatH)-1
mfco.dk1$BoltDay.adj <- mfco.dk1$BoltDay + 3
write.table(mfco.dk1, file="Mat fx bonus control.txt", sep="\t", quote=F)
coBatH <- CGtrait.LR("bolt.bin", mfco.dk1, family=binomial)
coP <- lapply(names(mfco.dk1)[c(11,46)],function(n) CGtrait.LR(n,mfco.dk1, family=poisson)) #lfcountH, boltdate, all poisson

####control, lxw, mom sig, so do by hand####
modeldata<-mfco.dk1[!is.na(mfco.dk1$lxwH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$CrossNum<-as.factor(modeldata$CrossNum)

model1raw<-lmer(lxwH ~ Origin +Latitude +(1|PopID/CrossNum), family=gaussian,data=modeldata)
model2raw<-lmer(lxwH ~ Origin +Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(lxwH ~ Origin +Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL<-lmer(lxwH ~ Origin +(1|PopID/CrossNum), family=gaussian,data=modeldata)
anova(modelL, model1raw)
# 
# modelOraw<-lmer(lxwH ~ Latitude +(1|PopID/CrossNum), family=gaussian,data=modeldata)
# anova(modelOraw,modelL) #test for significance of origin - origin NOT sig....!

####control, lf count, mom sig so do by hand#####
#poisson on raw data
modeldata<-mfco.dk1[!is.na(mfco.dk1$LfCountH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$CrossNum<-as.factor(modeldata$CrossNum)

model1raw<-lmer(LfCountH ~ Origin +Latitude +(1|PopID/CrossNum), family=poisson,data=modeldata)
model2raw<-lmer(LfCountH ~Origin +Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(LfCountH ~ Origin +Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL<-lmer(LfCountH ~ Origin +(1|PopID/CrossNum), family=poisson,data=modeldata)
anova(modelL, model1raw)

# modelOraw<-lmer(LfCountH ~Latitude+(1|PopID/CrossNum), family=poisson,data=modeldata)
# anova(modelOraw,model1raw) #test for significance of origin - origin NOT sig....!
# 
# int<-1.510174
# #inv mean
# B<--0.217268
# #Originnat estimate from model summary
# pI<-exp(int)
# pN<-exp(int+B)
# pI
# pN

###control, boltday.adj, cross sig, do by hand###
#only bolters
modeldata<-mfco.dk1[!is.na(mfco.dk1$BoltDay.adj),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$CrossNum<-as.factor(modeldata$CrossNum)

model1<-lmer(BoltDay.adj ~ Origin + Latitude +(1|PopID/CrossNum), family=poisson,data=modeldata)
model2<-lmer(BoltDay.adj ~ Origin + Latitude +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(BoltDay.adj ~ Origin + Latitude +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL<-lmer(BoltDay.adj ~ Origin + (1|PopID/CrossNum), family=poisson,data=modeldata)
anova(modelL, model1)
# 
# modelO<-lmer(BoltDay.adj ~  (1|PopID/CrossNum), family=poisson,data=modeldata)
# anova(modelO,modelL) #test for significance of origin - origin not sig!

###control, crown, cross sig, do by hand###
modeldata<-mfco.dk1[!is.na(mfco.dk1$CrownDiam.mm),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$CrossNum<-as.factor(modeldata$CrossNum)

model1raw<-lmer(CrownDiam.mm ~ Origin +Latitude +(1|PopID/CrossNum), family=gaussian,data=modeldata)
model2raw<-lmer(CrownDiam.mm ~ Origin +Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(CrownDiam.mm ~ Origin +Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL<-lmer(CrownDiam.mm ~ Origin +(1|PopID/CrossNum), family=gaussian,data=modeldata)
anova(modelL, model1raw)
# 
# modelOraw<-lmer(CrownDiam.mm ~ Latitude +(1|PopID/CrossNum), family=gaussian,data=modeldata)
# anova(modelOraw,modelL) #test for significance of origin - origin NOT sig....!

###control, sla, cross sig, do by hand###
modeldata<-mfco.dk1[!is.na(mfco.dk1$sla),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$CrossNum<-as.factor(modeldata$CrossNum)

model1raw<-lmer(sla ~ Origin +Latitude +(1|PopID/CrossNum), family=gaussian,data=modeldata)
model2raw<-lmer(sla ~ Origin +Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(sla ~ Origin +Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL<-lmer(sla ~ Origin +(1|PopID/CrossNum), family=gaussian,data=modeldata)
anova(modelL, model1raw)
# 
# modelOraw<-lmer(sla ~ Latitude +(1|PopID/CrossNum), family=gaussian,data=modeldata)
# anova(modelOraw,modelL) #test for significance of origin - origin NOT sig....!

####Nut def, Origin + Lat####
mfn.dk<-read.table("Mat fx nut def.dk.txt", header=T, sep="\t", quote='"', row.names=1) #nut, dk only
head(mfn.dk)
xtabs(~Origin+BoltedatH, mfn.dk) # only one bolter... leaving in
# modeldata<-modeldata[modeldata$BoltedatH!="y",]#remove 7 bolted plants from nat SHOULD I???
mfn.dk$lxwH <- mfn.dk$LfLgthH * mfn.dk$LfWdthH
# mfn.dk$bolt.bin <- as.numeric(mfn.dk$BoltedatH)-1
# mfn.dk$BoltDay.adj <- mfco.dk1$BoltDay + 3
write.table(mfn.dk, file="Mat fx nut def.dk.txt", sep="\t", quote=F)

nLR <- lapply(names(mfn.dk)[c(20:21,27:28, 36)],function(n) CGtrait.LR(n, mfn.dk)) 
#lflgthH, lfwdthH, crown, shoot, lxwH, all gaussian
# names(nLR) <- names(al)[c(11:12, 15:17, 50:51)]
nLR #check out LRs of models. Model progression logical?
# nmodels <- CGtrait.models("RootMass.g",mfn.dk)
# nmodels2 <- CGtrait.models("RootH.log",mfn.dk)
# nmodels
# nRoot.lmer <- nmodels$model2
# nRootlog.lmer <- nmodels2$model2
# qqnorm(resid(nRootlog.lmer), main="Q-Q plot for residuals")
# qqline(resid(nRootlog.lmer))


#non-gaussian?
# mfn.dk <- cbind(mfn.dk, bolt.bin=as.numeric(mfn.dk$BoltedatH)-1)
# write.table(n, file="STNutsubset.txt", sep="\t", quote=F)
# 
# #nBatH <- CGtrait.LR("bolt.bin", mfn.dk, family=binomial)# no invasive bolted!
nlfcount <- CGtrait.LR("LfCountH",mfn.dk, family=poisson) #lfcountH, all poisson


####Cut, Origin + Lat####
mfcu.dk<-read.table("Mat fx cut.dk.txt", header=T, sep="\t", quote='"', row.names=1) #cut, dk only
head(mfcu.dk)
cuLR <- CGtrait.LR("CrownDiam.mm",mfcu.dk) #crown all gaussian

#non-gaussian?
mfcu.dk<- cbind(mfcu.dk, bolt.bin=as.numeric(mfcu.dk$BoltedatH)-1)
write.table(mfcu.dk, file="Mat fx cut.dk.txt", sep="\t", quote=F)
xtabs(~Origin+BoltedatH, mfcu.dk)
cuBatH <- CGtrait.LR("bolt.bin", mfcu.dk, family=binomial)
cuP <- lapply(names(mfcu.dk)[c(19,34)],function(n) CGtrait.LR(n,mfcu.dk, family=poisson)) #lfcountH, boltdate, all poisson

cumodels <- CGtrait.models("RootH.log",cu)
cumodels
qqnorm(resid(cumodels$model2), main="Q-Q plot for residuals")
qqline(resid(cumodels$model2))
shapiro.test(resid(cumodels$model2))


####cut, lf count, harvest, mom is sig, do by hand###
modeldata<-mfcu.dk[!is.na(mfcu.dk$LfCountH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$CrossNum<-as.factor(modeldata$CrossNum)

model1raw<-lmer(LfCountH ~ Origin + Latitude + (1|PopID/CrossNum), family=poisson,data=modeldata)
model2raw<-lmer(LfCountH ~ Origin + Latitude + (1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(LfCountH ~ Origin + Latitude + (1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL <- lmer(LfCountH ~ Origin +(1|PopID/CrossNum), family=poisson,data=modeldata)
anova(modelL,model1raw) 

modelOraw<-lmer(LfCountH ~ Latitude + (1|PopID/CrossNum), family=poisson,data=modeldata)
anova(modelOraw,model1raw) #test for significance of origin 

###cut, harvest, bolt day, PopID not sig, do by hand###
#given that it's bolted....only 6 bolted total.


####Drought, Origin + Lat####
mfd.dk<-read.table("Mat fx drought.dk.txt", header=T, sep="\t", quote='"', row.names=1) #drought, dk only
head(mfd.dk)
#no gaussian
dLR <- lapply(names(mfd.dk)[17:19],function(n) CGtrait.LR(n,mfd.dk, family=poisson)) #wilt, totwilt, death, all poisson
names(dLR) <- names(d)[17:19]

# dmodels <- lapply(names(d)[8:9],function(n) CGtrait.models(n,d))
# dmodels

# int<-10.69128#inv mean
# B<-0.28995#Originnat estimate from model summary
# pI<-exp(int)
# pN<-exp(int+B)
# pI
# pN
# summary(d[d$Origin=="inv",]$TotWilt)
# summary(d[d$Origin=="nat",]$TotWilt)

####Flood, Origin + Lat####
mff.dk<-read.table("Mat fx flood.dk.txt", header=T, sep="\t", quote='"', row.names=1) #flood, dk only
head(mff.dk)
#no gaussian
fLR <- lapply(names(mff.dk)[17:19],function(n) CGtrait.LR(n,mff.dk, family=poisson)) #wilt, totwilt, death, all poisson
# names(fLR) <- names(f)[20:21]
# 
# fmodels <- lapply(names(f)[20:21],function(n) CGtrait.models(n,f))
# fmodels
###flood, yellow, pop not sig###
modeldata<-mff.dk[!is.na(mff.dk$YellowDay),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$CrossNum<-as.factor(modeldata$CrossNum)

model1raw<-lmer(YellowDay ~ Origin + Latitude + (1|PopID/CrossNum), family=poisson,data=modeldata)
model2raw<-lmer(YellowDay ~ Origin + Latitude + (1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(YellowDay ~ Origin + Latitude + (1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL <- lmer(YellowDay ~ Origin +(1|blank), family=poisson,data=modeldata)
anova(modelL,model3raw) 

modelOraw<-lmer(YellowDay ~ Latitude + (1|PopID/CrossNum), family=poisson,data=modeldata)
anova(modelOraw,model1raw) #test for significance of origin 

###flood, float, pop not sig###
modeldata<-mff.dk[!is.na(mff.dk$FloatDay),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$CrossNum<-as.factor(modeldata$CrossNum)

model1raw<-lmer(FloatDay ~ Origin + Latitude + (1|PopID/CrossNum), family=poisson,data=modeldata)
model2raw<-lmer(FloatDay ~ Origin + Latitude + (1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3raw<-lmer(FloatDay ~ Origin + Latitude + (1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2raw,model1raw) # mom not sig
anova(model3raw,model2raw) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL <- lmer(FloatDay ~ Origin +(1|blank), family=poisson,data=modeldata)
anova(modelL,model3raw) 

modelOraw<-lmer(FloatDay ~ Latitude + (1|PopID/CrossNum), family=poisson,data=modeldata)
anova(modelOraw,model1raw) #test for significance of origin 

####Mom, Origin + Lat####
mfmom.dk<-read.table("Mat fx mom.dk.txt", header=T, sep="\t", quote='"', row.names=1) 
head(mfmom.dk)


####Mom, seedwt###
str(mfmom.dk)
mfmom.dk$Origin<-droplevels(mfmom.dk$Origin)
mfmom.dk$CrossNum<-as.factor(mfmom.dk$CrossNum)
modeldata<-mfmom.dk[!is.na(mfmom.dk$SeedWt),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model1<-lmer(SeedWt ~ Origin +Latitude + (1|PopID/MomID), family=gaussian,data=modeldata)
model2<-lmer(SeedWt ~ Origin +Latitude +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(SeedWt ~ Origin +Latitude +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1)
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL <-lmer(SeedWt ~ Origin +(1|PopID), family=gaussian,data=modeldata)
anova(modelL, model2)
# 
# modelO<-lmer(SeedWt ~ (1|PopID), family=gaussian,data=modeldata)
# anova(modelO,model2) #test for significance of origin - origin not sig....?

####Mom, germ count###
modeldata<-mfmom.dk[!is.na(mfmom.dk$GermCount),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model1<-lmer(GermCount ~ Origin +Latitude + SeedCount +(1|PopID/MomID), family=poisson,data=modeldata)
model2<-lmer(GermCount ~ Origin +Latitude + SeedCount +(1|PopID), family=poisson, data=modeldata)
model3<-lmer(GermCount ~ Origin +Latitude + SeedCount + (1|blank), family=poisson, data=modeldata)
anova(model2, model1)
anova(model3, model2)

modelL <- lmer(GermCount ~ Origin + SeedCount +(1|PopID), family=poisson, data=modeldata)
anova(modelL, model2)

# modelS0<-lmer(GermCount ~ Origin + (1|PopID), family=poisson, data=modeldata)
# anova(modelS0, model2)
# 
# # modelW<-lmer(GermCount ~ Origin + SeedCount +SeedWt+(1|PopID), family=poisson, data=modeldata)
# # anova(modelW, model2)
# 
# modelO<-lmer(GermCount ~ SeedCount + (1|PopID), family=poisson, data=modeldata)
# anova(modelO, model2)
# model2
# int<-1.103892
# #inv mean
# B<--0.299416
# #Originnat estimate from model summary
# pI<-exp(int)
# pN<-exp(int+B)
# pI
# pN

####Mom, germ date####avg, so can't use poisson
modeldata<-mfmom.dk[!is.na(mfmom.dk$GermAvgDate),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model1<-lmer(GermAvgDate ~ Origin + Latitude +(1|PopID/MomID), family=gaussian,data=modeldata)
model2<-lmer(GermAvgDate ~ Origin + Latitude+(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(GermAvgDate ~ Origin + Latitude+(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2, model1)
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelL <- lmer(GermAvgDate ~ Origin +(1|PopID), family=gaussian,data=modeldata)
anova(modelL, model2)
# # modelW<-lmer(GermAvgDate ~ Origin +SeedWt+(1|PopID), family=gaussian,data=modeldata)
# # anova(model2, modelW)
# 
# modelO<-lmer(GermAvgDate ~ (1|PopID), family=gaussian,data=modeldata)
# anova(modelO,model2) #test for significance of origin - origin not sig....?