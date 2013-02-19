#Mat FX mixed models
library(lme4)

#open tables

#mfco.dk<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) 
# #Mat fx strict contorl.txt, control only, dk only
mfco.dk1<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #Mat fx bonus control.txt
# #mat fx plus some ass map, control only, dk only
# mfco.full<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) 
# #full set of plants under control throughout, include mat fx, ass map, dna array, and sk
# 
# mfm1<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) 
# #m1, largest data set, early control, includes sk
#mfm1.dk<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #m1 total plants, dk only, unbalanced
mfcom1<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #m1 all plants in analysis, balanced, dk only


# mfallo<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #allo
mfallo.dk<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #allo, dk only

# mfn<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #nut
mfn.dk<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #nut, dk only

# mfcu<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #cut, with sk
mfcu.dk<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #cut, dk only

# mfd<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #drought
mfd.dk<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #drought, dk only

# mff<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #flood
mff.dk<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #flood, dk only

# mfmom<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #mom
mfmom.dk<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #mom, dk only

####Allo#####
#####allo, shoot mass######
str(mfallo.dk)
mfallo.dk$CrossNum<-as.factor(mfallo.dk$CrossNum)
modeldata<-mfallo.dk[!is.na(mfallo.dk$Mass.gA),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model1<-lmer(Mass.gA ~ Origin +(1|PopID/CrossNum), family=gaussian,data=modeldata)# gives false convergence error???
model2<-lmer(Mass.gA ~ Origin + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Mass.gA ~ Origin + (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelO<-lmer(Mass.gA ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,model2) #test for significance of origin - origin sig!
model2

#transformed
modeldata$mass.log<-log(modeldata$Mass.gA)

model1<-lmer(mass.log ~ Origin +(1|PopID/CrossNum), family=gaussian,data=modeldata)# gives false convergence error???
model2<-lmer(mass.log ~ Origin + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(mass.log ~ Origin + (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelO<-lmer(mass.log ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,model2) #test for significance of origin - origin sig!
model2

qqnorm(resid(model2), main="Q-Q plot for residuals")
qqline(resid(model2))

####allo, crown diam#####
modeldata<-mfallo.dk[!is.na(mfallo.dk$Crown.mmA),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model1<-lmer(Crown.mmA ~ Origin +(1|PopID/CrossNum), family=gaussian,data=modeldata)
model2<-lmer(Crown.mmA ~ Origin + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(Crown.mmA ~ Origin + (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelO<-lmer(Crown.mmA ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,model2) #test for significance of origin - origin not sig....?
#transform
modeldata$crown.log<-log(modeldata$Crown.mmA)
model1<-lmer(crown.log ~ Origin +(1|PopID/CrossNum), family=gaussian,data=modeldata)
model2<-lmer(crown.log ~ Origin + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(crown.log ~ Origin + (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelO<-lmer(crown.log ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,model2) #test for significance of origin - origin not sig....?

####m1,largest, early control####
###m1, control, lf count####
# str(mfm1.dk)
# modeldata<-mfm1.dk[!is.na(mfm1.dk$LfCount1),]
modeldata<-mfcom1[!is.na(mfcom1$LfCount1),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model1<-lmer(LfCount1 ~ Origin +(1|PopID/CrossNum), family=poisson,data=modeldata)
model2<-lmer(LfCount1 ~ Origin +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(LfCount1 ~ Origin +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom not sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelO<-lmer(LfCount1 ~ (1|PopID), family=poisson,data=modeldata)
anova(modelO,model2) #test for significance of origin - origin NOT sig....!
#transformed data, can't use poisson
modeldata$lfCount.sq<-sqrt(modeldata$LfCount1)
model1<-lmer(lfCount.sq ~ Origin +(1|PopID/CrossNum), family=gaussian,data=modeldata)
model2<-lmer(lfCount.sq ~ Origin + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lfCount.sq ~ Origin + (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
modelO<-lmer(lfCount.sq ~ (1|PopID/CrossNum), family=gaussian,data=modeldata)
anova(modelO,model1) #test for significance of origin - origin not sig....?

###m1, control, lxw###
# modeldata<-mfm1.dk[!is.na(mfm1.dk$LfLgth1),]
# modeldata<-modeldata[!is.na(modeldata$LfWdth1),]
# modeldata<-mfm1.dk[!is.na(mfm1.dk$lxw),]
modeldata<-mfcom1[!is.na(mfcom1$LfLgth1),]
modeldata<-modeldata[!is.na(modeldata$LfWdth1),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$lxw<-modeldata$LfLgth1*modeldata$LfWdth1
modeldata$CrossNum<-as.factor(modeldata$CrossNum)

model1<-lmer(lxw ~ Origin +(1|PopID/CrossNum), family=gaussian,data=modeldata)
model2<-lmer(lxw ~ Origin + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lxw ~ Origin + (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelO<-lmer(lxw ~ (1|PopID/CrossNum), family=gaussian,data=modeldata)
anova(modelO,model1) #test for significance of origin - origin not sig....?
model1

#transformed m1, lxw#
modeldata<-mfcom1[!is.na(mfcom1$lxw),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$lxw.log<-log(mfcom1$lxw)
modeldata$CrossNum<-as.factor(modeldata$CrossNum)

model1<-lmer(lxw.log ~ Origin +(1|PopID/CrossNum), family=gaussian,data=modeldata)
model2<-lmer(lxw.log ~ Origin + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lxw.log ~ Origin + (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelO<-lmer(lxw.log ~ (1|PopID/CrossNum), family=gaussian,data=modeldata)
anova(modelO,model1) #test for significance of origin - origin not sig....?
model1

qqnorm(resid(model1), main="Q-Q plot for residuals")
qqline(resid(model1))

####CONTROL####
#include BoltedatH!
###control m1, control, lxw###
modeldata<-mfco.dk1[!is.na(mfco.dk1$LfLgth1),]
modeldata<-modeldata[!is.na(modeldata$LfWdth1),]
# modeldata<-mfm1.dk[!is.na(mfm1.dk$lxw),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$lxw<-modeldata$LfLgth1*modeldata$LfWdth1
modeldata$CrossNum<-as.factor(modeldata$CrossNum)

model1<-lmer(lxw ~ Origin +(1|PopID/CrossNum), family=gaussian,data=modeldata)
model2<-lmer(lxw ~ Origin + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lxw ~ Origin + (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelO<-lmer(lxw ~ (1|PopID/CrossNum), family=gaussian,data=modeldata)
anova(modelO,model1) #test for significance of origin - origin not sig....?

#####strict control, crown####
str(mfco.dk)
modeldata<-mfco.dk[!is.na(mfco.dk$CrownDiam.mm),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model1<-lmer(CrownDiam.mm ~ Origin * BoltedatH+(BoltedatH|PopID/CrossNum), family=gaussian,data=modeldata)
model2<-lmer(CrownDiam.mm ~ Origin * BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(CrownDiam.mm ~ Origin * BoltedatH+(BoltedatH|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(CrownDiam.mm ~ Origin + BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata)
anova(modelInt,model2) #test for significant interaction btw Origin and Bolted - not sig

modelB0<-lmer(CrownDiam.mm ~ Origin + (1|PopID), family=gaussian,data=modeldata)
modelB1<-lmer(CrownDiam.mm ~ Origin + BoltedatH+(1|PopID), family=gaussian,data=modeldata)
modelB2<-lmer(CrownDiam.mm ~ Origin + (BoltedatH|PopID), family=gaussian,data=modeldata)
anova(modelB0, modelInt)#test for sig of bolting - bolting sig
anova(modelB1, modelInt)#test for sig of bolting - bolting sig
anova(modelB2, modelInt)#test for sig of bolting - bolting sig

modelO<-lmer(CrownDiam.mm ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,modelB0) #test for significance of origin - origin not sig....?
#transform
modeldata$crown.log<-log(modeldata$CrownDiam.mm)
model1<-lmer(crown.log ~ Origin * BoltedatH+(BoltedatH|PopID/CrossNum), family=gaussian,data=modeldata)
model2<-lmer(crown.log ~ Origin * BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(crown.log ~ Origin * BoltedatH+(BoltedatH|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(crown.log ~ Origin + BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata)
anova(modelInt,model2) #test for significant interaction btw Origin and Bolted - not sig

modelB0<-lmer(crown.log ~ Origin + (1|PopID), family=gaussian,data=modeldata)
modelB1<-lmer(crown.log ~ Origin + BoltedatH+(1|PopID), family=gaussian,data=modeldata)
modelB2<-lmer(crown.log ~ Origin + (BoltedatH|PopID), family=gaussian,data=modeldata)
anova(modelB0, modelInt)#test for sig of bolting - bolting sig
anova(modelB1, modelInt)#test for sig of bolting - bolting sig
anova(modelB2, modelInt)#test for sig of bolting - bolting sig

modelO<-lmer(crown.log ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,modelB0) #test for significance of origin - origin not sig....?

#####bonus control, crown####
str(mfco.dk1)
modeldata<-mfco.dk1[!is.na(mfco.dk1$CrownDiam.mm),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model1<-lmer(CrownDiam.mm ~ Origin * BoltedatH+(BoltedatH|PopID/CrossNum), family=gaussian,data=modeldata)
model2<-lmer(CrownDiam.mm ~ Origin * BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(CrownDiam.mm ~ Origin * BoltedatH+(BoltedatH|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(CrownDiam.mm ~ Origin + BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata)
anova(modelInt,model2) #test for significant interaction btw Origin and Bolted - not sig

modelB0<-lmer(CrownDiam.mm ~ Origin + (1|PopID), family=gaussian,data=modeldata)
modelB1<-lmer(CrownDiam.mm ~ Origin + BoltedatH+(1|PopID), family=gaussian,data=modeldata)
modelB2<-lmer(CrownDiam.mm ~ Origin + (BoltedatH|PopID), family=gaussian,data=modeldata)
anova(modelB0, modelInt)#test for sig of bolting - bolting sig
anova(modelB1, modelInt)#test for sig of bolting - bolting sig
anova(modelB2, modelInt)#test for sig of bolting - bolting sig

modelO<-lmer(CrownDiam.mm ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,modelB0) #test for significance of origin - origin not sig....?
#transform
modeldata$crown.log<-log(modeldata$CrownDiam.mm)
model1<-lmer(crown.log ~ Origin * BoltedatH+(BoltedatH|PopID/CrossNum), family=gaussian,data=modeldata)
model2<-lmer(crown.log ~ Origin * BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(crown.log ~ Origin * BoltedatH+(BoltedatH|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(crown.log ~ Origin + BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata)
anova(modelInt,model2) #test for significant interaction btw Origin and Bolted - not sig

modelB0<-lmer(crown.log ~ Origin + (1|PopID), family=gaussian,data=modeldata)
modelB1<-lmer(crown.log ~ Origin + BoltedatH+(1|PopID), family=gaussian,data=modeldata)
modelB2<-lmer(crown.log ~ Origin + (BoltedatH|PopID), family=gaussian,data=modeldata)
anova(modelB0, modelInt)#test for sig of bolting - bolting sig
anova(modelB1, modelInt)#test for sig of bolting - bolting sig
anova(modelB2, modelInt)#test for sig of bolting - bolting sig

modelO<-lmer(crown.log ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,modelB0) #test for significance of origin - origin not sig....?

######strict control, shoot mass#####
str(mfco.dk)
modeldata<-mfco.dk[!is.na(mfco.dk$ShootMass.g),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

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

modelO<-lmer(ShootMass.g ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,modelB0) #test for significance of origin - origin not sig....?
#transform
modeldata$mass.log<-log(modeldata$ShootMass.g)
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

#####bonus control, mass####
str(mfco.dk1)
modeldata<-mfco.dk1[!is.na(mfco.dk1$ShootMass.g),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

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
#transform
modeldata$mass.log<-log(modeldata$ShootMass.g)
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

#####strict control, lf count######
str(mfco.dk)
modeldata<-mfco.dk[!is.na(mfco.dk$LfCountH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model1<-lmer(LfCountH ~ Origin * BoltedatH+(BoltedatH|PopID/CrossNum), family=poisson,data=modeldata)
model2<-lmer(LfCountH ~ Origin * BoltedatH+(BoltedatH|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(LfCountH ~ Origin * BoltedatH+(BoltedatH|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom not sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(LfCountH ~ Origin + BoltedatH+(BoltedatH|PopID/CrossNum), family=poisson,data=modeldata)
anova(modelInt,model1) #test for significant interaction btw Origin and Bolted - not sig

modelB0<-lmer(LfCountH ~ Origin + (1|PopID/CrossNum), family=poisson,data=modeldata)
modelB1<-lmer(LfCountH ~ Origin + BoltedatH+(1|PopID/CrossNum), family=poisson,data=modeldata)
modelB2<-lmer(LfCountH ~ Origin + (BoltedatH|PopID/CrossNum), family=poisson,data=modeldata)
anova(modelB0, modelInt)#test for sig of bolting - bolting not sig
anova(modelB1, modelInt)#test for sig of bolting - bolting not sig
anova(modelB2, modelInt)#test for sig of bolting - bolting not sig
#
modelO<-lmer(LfCountH ~ BoltedatH+(BoltedatH|PopID/CrossNum), family=poisson,data=modeldata)
anova(modelO,modelInt) #test for significance of origin - origin IS sig....!
#transformed data, can't use poisson
# modeldata$lfCounth.sq<-sqrt(modeldata$LfCountH)
# model1<-lmer(lfCounth.sq ~ Origin +(1|PopID/CrossNum), family=gaussian,data=modeldata)
# model2<-lmer(lfCounth.sq ~ Origin + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(lfCounth.sq ~ Origin + (1|blank), family=gaussian,data=modeldata) # Test population effect
# anova(model2,model1) # mom is sig!
# anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# modelO<-lmer(lfCounth.sq ~ (1|PopID), family=gaussian,data=modeldata)
# anova(modelO,model2) #test for significance of origin - origin not sig....?

#####bonus control, lf count######
str(mfco.dk1)
modeldata<-mfco.dk1[!is.na(mfco.dk1$LfCountH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model1<-lmer(LfCountH ~ Origin * BoltedatH+(BoltedatH|PopID/CrossNum), family=poisson,data=modeldata)
model2<-lmer(LfCountH ~ Origin * BoltedatH+(BoltedatH|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(LfCountH ~ Origin * BoltedatH+(BoltedatH|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom not sig
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
# #transformed data, can't use poisson
# modeldata$lfCounth.sq<-sqrt(modeldata$LfCountH)
# model1<-lmer(lfCounth.sq ~ Origin +(1|PopID/CrossNum), family=gaussian,data=modeldata)
# model2<-lmer(lfCounth.sq ~ Origin + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(lfCounth.sq ~ Origin + (1|blank), family=gaussian,data=modeldata) # Test population effect
# anova(model2,model1) # mom is sig!
# anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# modelO<-lmer(lfCounth.sq ~ (1|PopID), family=gaussian,data=modeldata)
# anova(modelO,model2) #test for significance of origin - origin not sig....?


###strict control, lxwH###
modeldata<-mfco.dk[!is.na(mfco.dk$LfLgthH),]
modeldata<-modeldata[!is.na(modeldata$LfWdthH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$lxw<-modeldata$LfLgthH*modeldata$LfWdthH
modeldata$CrossNum<-as.factor(modeldata$CrossNum)

model1<-lmer(lxw ~ Origin * BoltedatH+(BoltedatH|PopID/CrossNum), family=gaussian,data=modeldata)
model2<-lmer(lxw ~ Origin * BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lxw ~ Origin * BoltedatH+(BoltedatH|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(lxw ~ Origin + BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata)
anova(modelInt,model2) #test for significant interaction btw Origin and Bolted - not sig

modelB0<-lmer(lxw ~ Origin + (1|PopID), family=gaussian,data=modeldata)
modelB1<-lmer(lxw ~ Origin + BoltedatH+(1|PopID), family=gaussian,data=modeldata)
modelB2<-lmer(lxw ~ Origin + (BoltedatH|PopID), family=gaussian,data=modeldata)
anova(modelB0, modelInt)#test for sig of bolting - bolting sig
anova(modelB1, modelInt)#test for sig of bolting - bolting sig
anova(modelB2, modelInt)#test for sig of bolting - bolting sig

modelO<-lmer(lxw ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,modelB0) #test for significance of origin - origin not sig....?

#####strict control, boltday####
#given that it's bolted....
modeldata<-mfco.dk[!is.na(mfco.dk$BoltDay),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model1<-lmer(BoltDay ~ Origin * CrownDiam.mm+(1|PopID/CrossNum), family=poisson,data=modeldata)
model2<-lmer(BoltDay ~ Origin * CrownDiam.mm+(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(BoltDay ~ Origin * CrownDiam.mm+(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(BoltDay ~ Origin + CrownDiam.mm+(1|PopID), family=poisson,data=modeldata)
anova(modelInt,model2) #test for significant interaction btw Origin and Bolted - not sig

modelC0<-lmer(BoltDay ~ Origin + (1|PopID), family=poisson,data=modeldata)
anova(modelC0, model2)#test for sig of crown - crown not sig

modelO<-lmer(BoltDay ~ CrownDiam.mm+(1|PopID), family=poisson,data=modeldata)
anova(modelO,model2) #test for significance of origin - origin not sig!

#####bonus control, boltday####
#given that it's bolted....
modeldata<-mfco.dk1[!is.na(mfco.dk1$BoltDay),] 
#one bolted before start of stress, poisson doesn't take negatives, so add 3
modeldata$BoltDay<-modeldata$BoltDay+3
modeldata$BoltDay<-as.integer(modeldata$BoltDay)
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$CrossNum<-as.factor(modeldata$CrossNum)

model1<-lmer(BoltDay ~ Origin *CrownDiam.mm+(CrownDiam.mm|PopID/CrossNum), family=poisson,data=modeldata)
model2<-lmer(BoltDay ~ Origin *CrownDiam.mm+(CrownDiam.mm|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(BoltDay ~ Origin *CrownDiam.mm+(CrownDiam.mm|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(BoltDay ~ Origin + CrownDiam.mm+(CrownDiam.mm|PopID/CrossNum), family=poisson,data=modeldata)
anova(modelInt,model1) #test for significant interaction btw Origin and Bolted - not sig
modelC0<-lmer(BoltDay ~ Origin * CrownDiam.mm+(1|PopID/CrossNum), family=poisson,data=modeldata)
anova(modelC0, model1)#test for sig of crown - crown not sig
modelC1<-lmer(BoltDay ~ Origin +(CrownDiam.mm|PopID/CrossNum), family=poisson,data=modeldata)
anova(modelC1,model1)
modelC1

modelO<-lmer(BoltDay ~ (CrownDiam.mm|PopID/CrossNum), family=poisson,data=modeldata)
anova(modelO,modelC1) #test for significance of origin - origin not sig!

###strict control, boltedatH, binomial####
modeldata<-mfco.dk[!is.na(mfco.dk$BoltedatH),]
xtabs(~Origin + BoltedatH, modeldata)
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$bolt.bin<-as.numeric(modeldata$BoltedatH)-1
str(modeldata$bolt.bin)

model1<-lmer(bolt.bin ~ Origin * CrownDiam.mm+(1|PopID/CrossNum), family=binomial,data=modeldata)
model2<-lmer(bolt.bin ~ Origin * CrownDiam.mm+(1|PopID), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin ~ Origin * CrownDiam.mm+(1|blank), family=binomial,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model2,model3) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(bolt.bin ~ Origin + CrownDiam.mm+(1|blank), family=binomial,data=modeldata)
anova(modelInt,model3) #test for significant interaction btw Origin and Bolted - not sig

modelC0<-lmer(bolt.bin ~ Origin + (1|blank), family=binomial,data=modeldata)
anova(modelC0, modelInt)#test for sig of crown - crown not sig

modelO<-lmer(bolt.bin ~ (1|blank), family=binomial,data=modeldata)
anova(modelO,modelC0) #test for significance of origin - origin sig!

###bonus control, boltedatH, binomial####
modeldata<-mfco.dk1[!is.na(mfco.dk1$BoltedatH),]
xtabs(~Origin + BoltedatH, modeldata)
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$bolt.bin<-as.numeric(modeldata$BoltedatH)-1
str(modeldata$bolt.bin)

model1<-lmer(bolt.bin ~ Origin * CrownDiam.mm+(1|PopID/CrossNum), family=binomial,data=modeldata)
model2<-lmer(bolt.bin ~ Origin * CrownDiam.mm+(1|PopID), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin ~ Origin * CrownDiam.mm+(1|blank), family=binomial,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model2,model3) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(bolt.bin ~ Origin + CrownDiam.mm+(1|blank), family=binomial,data=modeldata)
anova(modelInt,model3) #test for significant interaction btw Origin and Bolted - not sig

modelC0<-lmer(bolt.bin ~ Origin + (1|blank), family=binomial,data=modeldata)
anova(modelC0, modelInt)#test for sig of crown - crown not sig

modelO<-lmer(bolt.bin ~ (1|blank), family=binomial,data=modeldata)
anova(modelO,modelC0) #test for significance of origin - origin sig!
modelC0
#to get effect size
int<--1.7707 #Originnat estimate from model summary
B<-2.2060

# Native
pN<-exp(int+B)/(exp(int+B)+1)
# Note that if Origin was a continuous variable you would substitute B with B*Origin

# Introduced (B=0)
pI<-exp(int)/(exp(int)+1)

pN # 50%
pI # 10% 
#report effect size as separate percentages, difference in percentage, or log-odds ratio: log(pI/pN)
#could also include the standard errors in equations to add upper/lower confidence intervals.
#check
summary(mfco.dk1[mfco.dk1$Origin=="nat",]) #56 rows, 34 boltedatH = 60%
summary(mfco.dk1[mfco.dk1$Origin=="inv",]) #55 rows, 8 boltedatH = 14.5%
#also check glm
glm(bolt.bin ~ Origin, family=binomial,data=modeldata)

####strict control, sla###
modeldata<-mfco.dk[!is.na(mfco.dk$SLAarea),]
modeldata<-modeldata[!is.na(modeldata$SLAmass.g),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model1<-lmer(SLAarea ~ Origin * SLAmass.g+(1|PopID/CrossNum), family=gaussian,data=modeldata)
model2<-lmer(SLAarea ~ Origin * SLAmass.g+(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(SLAarea ~ Origin * SLAmass.g+(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(SLAarea ~ Origin + SLAmass.g+(1|PopID), family=gaussian,data=modeldata)
anova(modelInt,model2) #test for significant interaction btw Origin and Bolted - not sig

modelS<-lmer(SLAarea ~ Origin + (1|PopID), family=gaussian,data=modeldata)
anova(modelS,model2)

modelO<-lmer(SLAarea ~ SLAmass.g+(1|PopID), family=gaussian,data=modeldata)
anova(modelO,model2) #test for significance of origin - origin not sig....?

####bonus control, sla area###
modeldata<-mfco.dk1[!is.na(mfco.dk1$SLAarea),]
modeldata<-modeldata[!is.na(modeldata$SLAmass.g),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$CrossNum<-as.factor(modeldata$CrossNum)

model1<-lmer(SLAarea ~ Origin * SLAmass.g+(1|PopID/CrossNum), family=gaussian,data=modeldata)
model2<-lmer(SLAarea ~ Origin * SLAmass.g+(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(SLAarea ~ Origin * SLAmass.g+(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(SLAarea ~ Origin + SLAmass.g+(1|PopID), family=gaussian,data=modeldata)
anova(modelInt,model2) #test for significant interaction btw Origin and Bolted - not sig

modelS<-lmer(SLAarea ~ Origin + (1|PopID), family=gaussian,data=modeldata)
anova(modelS,modelInt)

modelO<-lmer(SLAarea ~ SLAmass.g+(1|PopID), family=gaussian,data=modeldata)
anova(modelO,modelInt) #test for significance of origin - origin not sig....?

###bonus control sla###
modeldata<-mfco.dk1[!is.na(mfco.dk1$sla),]
# modeldata<-modeldata[!is.na(modeldata$SLAmass.g),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$CrossNum<-as.factor(modeldata$CrossNum)

model1<-lmer(sla ~ Origin +(1|PopID/CrossNum), family=gaussian,data=modeldata)
model2<-lmer(sla ~ Origin+(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(sla ~ Origin+(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelO<-lmer(sla ~ (1|PopID/CrossNum), family=gaussian,data=modeldata)
anova(modelO,model2) #test for significance of origin - origin not sig....?

######NUT#######
###nut, shoot###
str(mfn.dk)
mfn.dk$CrossNum<-as.factor(mfn.dk$CrossNum)
modeldata<-mfn.dk[!is.na(mfn.dk$ShootMass.g),]
xtabs(~Origin+BoltedatH, modeldata) # only one bolter...
modeldata<-modeldata[modeldata$BoltedatH!="y",]#remove 1 bolted plants from inv
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model1<-lmer(ShootMass.g ~ Origin +(1|PopID/CrossNum), family=gaussian,data=modeldata)
model2<-lmer(ShootMass.g ~ Origin +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(ShootMass.g ~ Origin +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom not sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelO<-lmer(ShootMass.g ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,model2) #test for significance of origin - origin not sig....?
#transformed
modeldata$shoot.log<-log(modeldata$ShootMass.g)
model1<-lmer(shoot.log ~ Origin +(1|PopID/CrossNum), family=gaussian,data=modeldata)
model2<-lmer(shoot.log ~ Origin +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(shoot.log ~ Origin +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom not sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelO<-lmer(shoot.log ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,model2)

####nut, crown###
# str(mfn.dk)
mfn.dk$CrossNum<-as.factor(mfn.dk$CrossNum)
modeldata<-mfn.dk[!is.na(mfn.dk$CrownDiam.mm),]
xtabs(~Origin+BoltedatH, modeldata) # only one bolter...
modeldata<-modeldata[modeldata$BoltedatH!="y",]#remove 1 bolted plants from inv
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model1<-lmer(CrownDiam.mm ~ Origin +(1|PopID/CrossNum), family=gaussian,data=modeldata)
model2<-lmer(CrownDiam.mm ~ Origin +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(CrownDiam.mm ~ Origin +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom not sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelO<-lmer(CrownDiam.mm ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,model2) #test for significance of origin - origin not sig....?
#transformed
modeldata$crown.log<-log(modeldata$CrownDiam.mm)
model1<-lmer(crown.log ~ Origin +(1|PopID/CrossNum), family=gaussian,data=modeldata)
model2<-lmer(crown.log ~ Origin +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(crown.log ~ Origin +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom not sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelO<-lmer(crown.log ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,model2)

####nut, lfcounth#####
mfn.dk$CrossNum<-as.factor(mfn.dk$CrossNum)
mfn.dk$Origin<-droplevels(mfn.dk$Origin)
modeldata<-mfn.dk[!is.na(mfn.dk$LfCountH),]
xtabs(~Origin+BoltedatH, modeldata) # only one bolter...
modeldata<-modeldata[modeldata$BoltedatH!="y",]#remove 1 bolted plants from inv
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model1<-lmer(LfCountH ~ Origin +(1|PopID/CrossNum), family=poisson,data=modeldata)
model2<-lmer(LfCountH ~ Origin +(1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(LfCountH ~ Origin +(1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom not sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelO<-lmer(LfCountH ~ +(1|PopID), family=poisson,data=modeldata)
anova(modelO,model2)

####nut, lxwh####
modeldata<-mfn.dk[!is.na(mfn.dk$LfLgthH),]
modeldata<-modeldata[!is.na(modeldata$LfWdthH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$lxw<-modeldata$LfLgthH*modeldata$LfWdthH
modeldata$CrossNum<-as.factor(modeldata$CrossNum)

model1<-lmer(lxw ~ Origin +(1|PopID/CrossNum), family=gaussian,data=modeldata)
model2<-lmer(lxw ~ Origin +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lxw ~ Origin +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelO<-lmer(lxw ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,modelB0)

#tranformed, nut, lxwh#
modeldata<-mfn.dk[!is.na(mfn.dk$lxwH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$lxw.log<-log(modeldata$lxwH)
modeldata$CrossNum<-as.factor(modeldata$CrossNum)

model1<-lmer(lxw.log ~ Origin +(1|PopID/CrossNum), family=gaussian,data=modeldata)
model2<-lmer(lxw.log ~ Origin +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lxw.log ~ Origin +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelO<-lmer(lxw.log ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,model2)

qqnorm(resid(model2), main="Q-Q plot resid MF nut lxwH")
qqline(resid(model2))

#################CUT#######################3
####cut, crown#####
xtabs(~Origin+BoltedatH, mfcu.dk)
mfcu.dk$Origin<-droplevels(mfcu.dk$Origin)
mfcu.dk$CrossNum<-as.factor(mfcu.dk$CrossNum)
modeldata<-mfcu.dk[!is.na(mfcu.dk$CrownDiam.mm),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model1<-lmer(CrownDiam.mm ~ Origin * BoltedatH+(BoltedatH|PopID/CrossNum), family=gaussian,data=modeldata)
model2<-lmer(CrownDiam.mm ~ Origin * BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(CrownDiam.mm ~ Origin * BoltedatH+(BoltedatH|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(CrownDiam.mm ~ Origin + BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata)
anova(modelInt,model2) #test for significant interaction btw Origin and Bolted - not sig

modelB0<-lmer(CrownDiam.mm ~ Origin + (1|PopID), family=gaussian,data=modeldata)
modelB1<-lmer(CrownDiam.mm ~ Origin + BoltedatH+(1|PopID), family=gaussian,data=modeldata)
modelB2<-lmer(CrownDiam.mm ~ Origin + (BoltedatH|PopID), family=gaussian,data=modeldata)
anova(modelB0, modelInt)#test for sig of bolting - bolting sig
anova(modelB1, modelInt)#test for sig of bolting - bolting sig
anova(modelB2, modelInt)#test for sig of bolting - bolting sig

modelO<-lmer(CrownDiam.mm ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,modelB0) #test for significance of origin - origin not sig....?
#transform
modeldata$crown.log<-log(modeldata$CrownDiam.mm)
model1<-lmer(crown.log ~ Origin * BoltedatH+(BoltedatH|PopID/CrossNum), family=gaussian,data=modeldata)
model2<-lmer(crown.log ~ Origin * BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(crown.log ~ Origin * BoltedatH+(BoltedatH|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(crown.log ~ Origin + BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata)
anova(modelInt,model2) #test for significant interaction btw Origin and Bolted - not sig

modelB0<-lmer(crown.log ~ Origin + (1|PopID), family=gaussian,data=modeldata)
modelB1<-lmer(crown.log ~ Origin + BoltedatH+(1|PopID), family=gaussian,data=modeldata)
modelB2<-lmer(crown.log ~ Origin + (BoltedatH|PopID), family=gaussian,data=modeldata)
anova(modelB0, modelInt)#test for sig of bolting - bolting sig
anova(modelB1, modelInt)#test for sig of bolting - bolting sig
anova(modelB2, modelInt)#test for sig of bolting - bolting sig

modelO<-lmer(crown.log ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,modelB0)

####cut, lfcountH####
str(mfcu.dk)
modeldata<-mfcu.dk[!is.na(mfcu.dk$LfCountH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model1<-lmer(LfCountH ~ Origin * BoltedatH+(BoltedatH|PopID/CrossNum), family=poisson,data=modeldata)
model2<-lmer(LfCountH ~ Origin * BoltedatH+(BoltedatH|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(LfCountH ~ Origin * BoltedatH+(BoltedatH|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom not sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(LfCountH ~ Origin + BoltedatH+(BoltedatH|PopID/CrossNum), family=poisson,data=modeldata)
anova(modelInt,model1) #test for significant interaction btw Origin and Bolted - not sig

modelB0<-lmer(LfCountH ~ Origin + (1|PopID/CrossNum), family=poisson,data=modeldata)
modelB1<-lmer(LfCountH ~ Origin + BoltedatH+(1|PopID/CrossNum), family=poisson,data=modeldata)
modelB2<-lmer(LfCountH ~ Origin + (BoltedatH|PopID/CrossNum), family=poisson,data=modeldata)
anova(modelB0, modelInt)#test for sig of bolting - bolting not sig
anova(modelB1, modelInt)#test for sig of bolting - bolting not sig
anova(modelB2, modelInt)#test for sig of bolting - bolting not sig
#
modelO<-lmer(LfCountH ~ (1|PopID/CrossNum), family=poisson,data=modeldata)
anova(modelO,modelB0)

####cut, boltday###
xtabs(~Origin+BoltedatH, mfcu.dk)
#given that it's bolted....
#not enough bolters! Only one inv bolter

####cut, boltedatH####
modeldata<-mfcu.dk[!is.na(mfcu.dk$BoltedatH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$bolt.bin<-as.numeric(modeldata$BoltedatH)-1
str(modeldata$bolt.bin)

model1<-lmer(bolt.bin ~ Origin +(1|PopID/CrossNum), family=binomial,data=modeldata)
model2<-lmer(bolt.bin ~ Origin +(1|PopID), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin ~ Origin +(1|blank), family=binomial,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model2,model3) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelO<-lmer(bolt.bin ~ (1|blank), family=binomial,data=modeldata)
anova(modelO,model3) #test for significance of origin - origin sig!

#also check glm
glm(bolt.bin ~ Origin, family=binomial,data=modeldata)


###############DROUGHT####################
####Drought, death######
str(mfd.dk)
mfd.dk$Origin<-droplevels(mfd.dk$Origin)
mfd.dk$CrossNum<-as.factor(mfd.dk$CrossNum)
modeldata<-mfd.dk[!is.na(mfd.dk$DeathDay),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model1<-lmer(DeathDay ~ Origin * LfLgth1+(LfLgth1|PopID/CrossNum), family=poisson,data=modeldata)
model2<-lmer(DeathDay ~ Origin * LfLgth1+(LfLgth1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(DeathDay ~ Origin * LfLgth1+(LfLgth1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(DeathDay ~ Origin + LfLgth1+(LfLgth1|PopID), family=poisson,data=modeldata)
anova(modelInt,model2) #test for significant interaction btw Origin and Bolted - not sig

modelL0<-lmer(DeathDay ~ Origin + (1|PopID), family=poisson,data=modeldata)
modelL1<-lmer(DeathDay ~ Origin + LfLgth1+(1|PopID), family=poisson,data=modeldata)
modelL2<-lmer(DeathDay ~ Origin + (LfLgth1|PopID), family=poisson,data=modeldata)
anova(modelL0, modelInt)#test for sig of crown - crown not sig
anova(modelL1, modelInt)#test for sig of bolting - bolting not sig
anova(modelL2, modelInt)#test for sig of bolting - bolting not sig

modelO<-lmer(DeathDay ~(1|PopID), family=poisson,data=modeldata)
anova(modelO,modelL0) #test for significance of origin - origin not sig!

####Drought, wilt######
modeldata<-mfd.dk[!is.na(mfd.dk$WiltDay),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model1<-lmer(WiltDay ~ Origin * LfLgth1+(LfLgth1|PopID/CrossNum), family=poisson,data=modeldata)
model2<-lmer(WiltDay ~ Origin * LfLgth1+(LfLgth1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(WiltDay ~ Origin * LfLgth1+(LfLgth1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(WiltDay ~ Origin + LfLgth1+(LfLgth1|PopID), family=poisson,data=modeldata)
anova(modelInt,model2) #test for significant interaction btw Origin and Bolted - not sig

modelL0<-lmer(WiltDay ~ Origin + (1|PopID), family=poisson,data=modeldata)
modelL1<-lmer(WiltDay ~ Origin + LfLgth1+(1|PopID), family=poisson,data=modeldata)
modelL2<-lmer(WiltDay ~ Origin + (LfLgth1|PopID), family=poisson,data=modeldata)
anova(modelL0, modelInt)#test for sig of crown - crown not sig
anova(modelL1, modelInt)#test for sig of bolting - bolting not sig
anova(modelL2, modelInt)#test for sig of bolting - bolting not sig

modelO<-lmer(WiltDay ~(1|PopID), family=poisson,data=modeldata)
anova(modelO,modelL0)

####Drought, totwilt######
modeldata<-mfd.dk[!is.na(mfd.dk$TotWiltDay),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model1<-lmer(TotWiltDay ~ Origin * LfLgth1+(LfLgth1|PopID/CrossNum), family=poisson,data=modeldata)
model2<-lmer(TotWiltDay ~ Origin * LfLgth1+(LfLgth1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(TotWiltDay ~ Origin * LfLgth1+(LfLgth1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(TotWiltDay ~ Origin + LfLgth1+(LfLgth1|PopID), family=poisson,data=modeldata)
anova(modelInt,model2) #test for significant interaction btw Origin and Bolted - not sig

modelL0<-lmer(TotWiltDay ~ Origin + (1|PopID), family=poisson,data=modeldata)
modelL1<-lmer(TotWiltDay ~ Origin + LfLgth1+(1|PopID), family=poisson,data=modeldata)
modelL2<-lmer(TotWiltDay ~ Origin + (LfLgth1|PopID), family=poisson,data=modeldata)
anova(modelL0, modelInt)#test for sig of crown - crown not sig
anova(modelL1, modelInt)#test for sig of bolting - bolting not sig
anova(modelL2, modelInt)#test for sig of bolting - bolting not sig

modelO<-lmer(TotWiltDay ~(1|PopID), family=poisson,data=modeldata)
anova(modelO,modelL0)

################FLOOD###################
####flood, death######
str(mff.dk)
mff.dk$Origin<-droplevels(mff.dk$Origin)
mff.dk$CrossNum<-as.factor(mff.dk$CrossNum)
modeldata<-mff.dk[!is.na(mff.dk$DeathDay),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model1<-lmer(DeathDay ~ Origin * LfLgth1+(LfLgth1|PopID/CrossNum), family=poisson,data=modeldata)
model2<-lmer(DeathDay ~ Origin * LfLgth1+(LfLgth1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(DeathDay ~ Origin * LfLgth1+(LfLgth1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(DeathDay ~ Origin + LfLgth1+(LfLgth1|blank), family=poisson,data=modeldata)
anova(modelInt,model3) #test for significant interaction btw Origin and Bolted - not sig

modelL0<-lmer(DeathDay ~ Origin + (1|blank), family=poisson,data=modeldata)
modelL1<-lmer(DeathDay ~ Origin + LfLgth1+(1|blank), family=poisson,data=modeldata)
modelL2<-lmer(DeathDay ~ Origin + (LfLgth1|blank), family=poisson,data=modeldata)
anova(modelL0, modelInt)#test for sig of crown - crown not sig
anova(modelL1, modelInt)#test for sig of bolting - bolting not sig
anova(modelL2, modelInt)#test for sig of bolting - bolting not sig

modelO<-lmer(DeathDay ~(1|blank), family=poisson,data=modeldata)
anova(modelO,modelL0)

####flood, yellowday####
modeldata<-mff.dk[!is.na(mff.dk$YellowDay),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model1<-lmer(YellowDay ~ Origin * LfLgth1+(LfLgth1|PopID/CrossNum), family=poisson,data=modeldata)
model2<-lmer(YellowDay ~ Origin * LfLgth1+(LfLgth1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(YellowDay ~ Origin * LfLgth1+(LfLgth1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(YellowDay ~ Origin + LfLgth1+(LfLgth1|PopID), family=poisson,data=modeldata)
anova(modelInt,model3) #test for significant interaction btw Origin and Bolted - not sig

modelL0<-lmer(YellowDay ~ Origin + (1|PopID), family=poisson,data=modeldata)
modelL1<-lmer(YellowDay ~ Origin + LfLgth1+(1|PopID), family=poisson,data=modeldata)
modelL2<-lmer(YellowDay ~ Origin + (LfLgth1|PopID), family=poisson,data=modeldata)
anova(modelL0, modelInt)#test for sig of crown - crown not sig
anova(modelL1, modelInt)#test for sig of bolting - bolting not sig
anova(modelL2, modelInt)#test for sig of bolting - bolting not sig

modelO<-lmer(YellowDay ~(1|PopID), family=poisson,data=modeldata)
anova(modelO,modelInt)

####flood, floatday####
modeldata<-mff.dk[!is.na(mff.dk$FloatDay),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

# model1<-lmer(FloatDay ~ Origin +(1|PopID), family=poisson,data=modeldata)
model2<-lmer(FloatDay ~ Origin * LfLgth1+(LfLgth1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(FloatDay ~ Origin * LfLgth1+(LfLgth1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # mom sig
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(FloatDay ~ Origin + LfLgth1+(LfLgth1|PopID), family=poisson,data=modeldata)
anova(modelInt,model3) #test for significant interaction btw Origin and Bolted - not sig

modelL0<-lmer(FloatDay ~ Origin + (1|PopID), family=poisson,data=modeldata)
modelL1<-lmer(FloatDay ~ Origin + LfLgth1+(1|PopID), family=poisson,data=modeldata)
modelL2<-lmer(FloatDay ~ Origin + (LfLgth1|PopID), family=poisson,data=modeldata)
anova(modelL0, modelInt)#test for sig of crown - crown not sig
anova(modelL1, modelInt)#test for sig of bolting - bolting not sig
anova(modelL2, modelInt)#test for sig of bolting - bolting not sig

modelO<-lmer(FloatDay ~(1|PopID), family=poisson,data=modeldata)
anova(modelO,modelInt)

#########MOM################
####Mom, seedwt###
str(mfmom.dk)
mfmom.dk$Origin<-droplevels(mfmom.dk$Origin)
mfmom.dk$CrossNum<-as.factor(mfmom.dk$CrossNum)
modeldata<-mfmom.dk[!is.na(mfmom.dk$SeedWt),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model1<-lmer(SeedWt ~ Origin +(1|PopID/MomID), family=gaussian,data=modeldata)
model2<-lmer(SeedWt ~ Origin +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(SeedWt ~ Origin +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1)
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelO<-lmer(SeedWt ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,model2) #test for significance of origin - origin not sig....?

####Mom, germ count###
modeldata<-mfmom.dk[!is.na(mfmom.dk$GermCount),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)

model1<-lmer(GermCount ~ Origin +SeedCount +(1|PopID/MomID), family=poisson,data=modeldata)
model2<-lmer(GermCount ~ Origin + SeedCount +(1|PopID), family=poisson, data=modeldata)
model3<-lmer(GermCount ~ Origin + SeedCount + (1|blank), family=poisson, data=modeldata)
anova(model2, model1)
anova(model3, model2)

modelS0<-lmer(GermCount ~ Origin + (1|PopID), family=poisson, data=modeldata)
anova(modelS0, model2)

modelW<-lmer(GermCount ~ Origin + SeedCount +SeedWt+(1|PopID), family=poisson, data=modeldata)
anova(modelW, model2)

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

model1<-lmer(GermAvgDate ~ Origin +(1|PopID/MomID), family=gaussian,data=modeldata)
model2<-lmer(GermAvgDate ~ Origin +(1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(GermAvgDate ~ Origin +(1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2, model1)
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelW<-lmer(GermAvgDate ~ Origin +SeedWt+(1|PopID), family=gaussian,data=modeldata)
anova(model2, modelW)

modelO<-lmer(GermAvgDate ~ (1|PopID), family=gaussian,data=modeldata)
anova(modelO,model2) #test for significance of origin - origin not sig....?

##############FULL CONTROL################
str(mfco)
modeldata<-mfco[mfco$Origin!="SK",]
modeldata<-modeldata[modeldata$Exp!="array",]
modeldata<-modeldata[!is.na(modeldata$ShootMass.g),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$CrossNum<-as.factor(modeldata$CrossNum)

model1<-lmer(ShootMass.g ~ Origin * BoltedatH+(BoltedatH|PopID/CrossNum), family=gaussian,data=modeldata)
model2<-lmer(ShootMass.g ~ Origin * BoltedatH+(BoltedatH|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(ShootMass.g ~ Origin * BoltedatH+(BoltedatH|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # mom is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.

modelInt<-lmer(ShootMass.g ~ Origin + BoltedatH+(BoltedatH|PopID/CrossNum), family=gaussian,data=modeldata)
anova(modelInt,model2) #test for significant interaction btw Origin and Bolted - not sig

modelB0<-lmer(ShootMass.g ~ Origin + (1|PopID/CrossNum), family=gaussian,data=modeldata)
modelB1<-lmer(ShootMass.g ~ Origin + BoltedatH+(1|PopID/CrossNum), family=gaussian,data=modeldata)
modelB2<-lmer(ShootMass.g ~ Origin + (BoltedatH|PopID/CrossNum), family=gaussian,data=modeldata)
anova(modelB0, modelInt)#test for sig of bolting - bolting sig
anova(modelB1, modelInt)#test for sig of bolting - bolting sig
anova(modelB2, modelInt)#test for sig of bolting - bolting sig

modelO<-lmer(ShootMass.g ~ BoltedatH+(BoltedatH|PopID/CrossNum), family=gaussian,data=modeldata)
anova(modelO,modelInt) #test for significance of origin - origin not sig....?
