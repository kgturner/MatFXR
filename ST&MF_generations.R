###ST and MF generational correlation###

#make datasets#
#####MERGE ST AND MF###########
#FOR ULTIMATE POWER#

#merge mf ctrl and mom df
str(mfmom.dk)
str(mfco.dk1)
totmf <- merge(mfmom.dk,mfco.dk1, all=TRUE )
str(totmf)
#tidy
totmf <- totmf[,-c(16:18,26,29:30, 32:38, 40:44, 46:57)]
totmf$Exp<-droplevels(totmf$Exp)
totmf$Trt<-droplevels(totmf$Trt)
levels(totmf$PopID)

#tidy st ctrl file... need mom?
stmom<-read.table("STMomsubset.txt", header=T, sep="\t", quote='"', row.names=1)
stco <- read.table("STControlsubset.txt", header=T, sep="\t", quote='"', row.names=1)
#only pops in mf
stco <- stco[stco$PopID %in% c("BG001", "CA001", "GR002", "RU008", "TR001", "US001", "US002", "US003"), ]

#tidy cols
stco <- stco[,-c(9,11:12,14:15,17:22, 24:26, 28:52, 54)]
head(stco)
#make pop_momfam column
totmf$pop_momfam <- paste0(totmf$PopID, "_",totmf$MomFam)
stco$pop_momfam <- paste0(stco$PopID, "_", stco$Mom)

#only families in mf
families <- levels(as.factor(totmf$pop_momfam))
stco <- stco[stco$pop_momfam %in% families,]
#add mom data
stco <- merge(stmom, stco)
head(stco)
stco <- stco[,-c(7,13, 15:16)]
#make columns agree
stco$Exp <- "st"
totmf$Trt <- "cont"
stco$MomFam <- stco$Mom
colnames(stco)[18] <- "ID_barcode"
colnames(totmf)[20] <- "ID_barcode"
colnames(stco)[23] <- "BoltDay.adj"
stco$BoltDay.adj <- as.integer(stco$BoltDay.adj) + 3 # to make it comparable to mf, which is adj for early bolters
totmf <- totmf[,-28]
colnames(stco)[6] <- "Cross_MomID"
colnames(totmf)[6] <- "Cross_MomID"
#same cols
totmf <- totmf[,-c(16)]
stco <- stco[, -c(13)]
stco$CrossNum <- NA
stco$MomIndiv <- NA
stco$DadID <- NA
stco$DadFam <- NA
stco$DadIndiv <- NA
totmf$CollYear <- 2010
totmf$SeedAgeYrs <- 0.17

stco$GermPercent <- as.numeric(stco$GermPercent)
stco$Trt <- as.factor(stco$Trt)
stco$BoltDay.adj <- as.integer(stco$BoltDay.adj)
stco$Exp <- as.factor(stco$Exp)
totmf$Indiv <- as.factor(totmf$Indiv)
totmf$MomFam <- as.factor(totmf$MomFam)
stco$Generation <- 0
totmf$Generation <- 1
#combine!
setdiff(colnames(stco), colnames(totmf))
setdiff(colnames(totmf), colnames(stco))
gen <- rbind(stco, totmf)
str(gen)
gen$pop_momfam <- as.factor(gen$pop_momfam)
gen$CrossNum <- as.factor(gen$CrossNum)
gen$MomIndiv <- as.factor(gen$MomIndiv)
gen$DadID <- as.factor(gen$DadID)
gen$DadFam <- as.factor(gen$DadFam)
gen$DadIndiv <- as.factor(gen$DadIndiv)
summary(gen)
gen[is.na(gen$Exp),]
gen <- gen[!is.na(gen$Exp),]
write.table(gen, file="ST&MFgenerations.txt", sep="\t", quote=F)

###ST and MF mixed FX models, focused on Origin BY Latitude###
#control, m1, or allo only
#Mat fx, REML, using lme4
#mixed effect models 
library(lme4)
library(lsmeans)
library(ggplot2)
library(plyr)

# #for each normal trait, compare this general set of models
# model1<-lmer(trait  ~ Origin* Generation +Latitude +(1|PopID/MomFam), family=gaussian,data=modeldata)
# model2<-lmer(trait  ~ Origin* Generation+ Latitude + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(trait  ~ Origin* Generation+Latitude + (1|blank), family=gaussian,data=modeldata) # Test population effect
# anova(model2,model1) # CrossNum is sig!
# anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# modelI <- lmer(trait  ~ Origin + Generation+Latitude + (1|PopID), family=family,data=modeldata)
# anova(modelI,model2)
# modelG<-lmer(trait  ~ Origin + Latitude+ (1|PopID), family=gaussian,data=modeldata)
# anova(modelG, model1)
# modelL<-lmer(trait  ~ Origin + (1|PopID), family=gaussian,data=modeldata)
# anova(modelL, model1)
# modelO<-lmer(trait ~ Latitude +(1|PopID), family=gaussian,data=modeldata)
# anova(modelO,model1) #test for significance of origin - origin only marginally sig....!



###LfCountH, poisson
modeldata<-gen[!is.na(gen$LfCountH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$MomFam<-as.factor(modeldata$MomFam)
modeldata$Generation <- as.factor(modeldata$Generation)

modelobar<-lmer(LfCountH  ~ Origin* Generation +(Origin|PopID/MomFam), family=poisson,data=modeldata)
# modelobarLint<-lmer(LfCountH  ~ Origin* Generation *Latitude +(Origin|PopID/MomFam), family=poisson,data=modeldata)

model1<-lmer(LfCountH  ~ Origin* Generation +(1|PopID/MomFam), family=poisson,data=modeldata)
anova(modelobar, model1)
model2<-lmer(LfCountH  ~ Origin* Generation +  (Origin|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(LfCountH  ~ Origin* Generation +(Origin|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,modelobar) # CrossNum is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(19.605,1)

# modelL<-lmer(LfCountH  ~ Origin* Generation  +(Origin|PopID/MomFam), family=poisson,data=modeldata)
# anova(modelobar, modelL)

modelI <- lmer(LfCountH  ~ Origin + Generation +(Origin|PopID/MomFam), family=poisson,data=modeldata)
anova(modelI,modelobar)

modelG<-lmer(LfCountH  ~ Origin +(Origin|PopID/MomFam), family=poisson,data=modeldata)
anova(modelG, modelI)
# modelI.1<-lmer(LfCountH  ~ Origin + Generation +(Origin|PopID), family=poisson,data=modeldata)
# modelG.1<-lmer(LfCountH  ~ Origin + (Origin|PopID), family=poisson,data=modeldata)
# anova(modelI.1, modelG.1)

modelO<-lmer(LfCountH ~ Generation +(Origin|PopID/MomFam), family=poisson,data=modeldata)
anova(modelO,modelI)
# modelInt<-lmer(LfCountH  ~ Origin* Generation +(1|PopID/MomFam), family=poisson,data=modeldata)
# anova(modelInt, modelL)


modelInt
int<-2.75258+0.29199
# -0.10115
#inv mean
B<--0.24940+0.29199
#Originnat estimate from model summary
pI<-exp(int)
pN<-exp(int+B)
pI
pN

# #try glm
# modelg <- glm(LfCountH ~ Origin*Generation, family=poisson,data=modeldata)
# modelg1 <- glm(LfCountH ~ Origin+Generation, family=poisson,data=modeldata)
# anova(modelg1, modelg) #'Deviance' is chisq value
# 1-pchisq( 0.78597, 1)
# 
# modelg3<- glm(LfCountH ~ Origin, family=poisson,data=modeldata)
# anova(modelg3,modelg1)
# 1-pchisq(44.724, 1)
# modelg2<- glm(LfCountH ~ Generation, family=poisson,data=modeldata)
# anova(modelg2,modelg1)
# 1-pchisq(0.32473, 1)

modelobar

ls <- as.data.frame(lsmeans(model1, ~ Origin+Generation, conf=95))
ls
CI.LS.poisson(modelI, conf = 95)
CI.LS.poisson.2term(model1, conf = 95)
interaction.plot(x.factor=modeldata$Generation, response=modeldata$LfCountH, trace.factor=modeldata$Origin, fun = mean)
# model1L<-lmer(LfCountH  ~ Origin* Generation +(1|PopID/MomFam), family=poisson,data=modeldata)
# modelIL <- lmer(LfCountH  ~ Origin + Generation + (1|PopID/MomFam), family=poisson,data=modeldata)
# anova(modelIL,model1L)

###ShootMass.g, gaussian... log?
modeldata<-gen[!is.na(gen$ShootMass.g),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$MomFam<-as.factor(modeldata$MomFam)

modelobar<-lmer(ShootMass.g  ~ Origin* Generation  +(Origin|PopID/MomFam), family=gaussian,data=modeldata)
model1<-lmer(ShootMass.g  ~ Origin* Generation  +(1|PopID/MomFam), family=gaussian,data=modeldata)
anova(modelobar, model1)
model2<-lmer(ShootMass.g  ~ Origin* Generation+  (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(ShootMass.g  ~ Origin* Generation+ (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # CrossNum is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
modelI <- lmer(ShootMass.g  ~ Origin + Generation + (1|PopID), family=gaussian,data=modeldata)
anova(modelI,model2)
modelG<-lmer(ShootMass.g  ~ Origin + (1|PopID), family=gaussian,data=modeldata)
anova(modelG, modelI)
# modelL<-lmer(ShootMass.g  ~ Origin + Generation+ (1|PopID), family=gaussian,data=modeldata)
# anova(modelL, modelI)
modelO<-lmer(ShootMass.g ~ Generation + (1|PopID), family=gaussian,data=modeldata)
anova(modelO,modelI)

qqnorm(resid(modelL), main="Q-Q plot for residuals")
qqline(resid(modelL))

# #transformed
# modeldata$Shoot.log <- log(modeldata$ShootMass.g)
# 
# model1<-lmer(Shoot.log  ~ Origin* Generation +Latitude +(1|PopID/MomFam), family=gaussian,data=modeldata)
# model2<-lmer(Shoot.log  ~ Origin* Generation+ Latitude + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(Shoot.log  ~ Origin* Generation+Latitude + (1|blank), family=gaussian,data=modeldata) # Test population effect
# anova(model2,model1) # CrossNum is sig!
# anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# modelI <- lmer(Shoot.log  ~ Origin + Generation+Latitude + (1|PopID), family=gaussian,data=modeldata)
# anova(modelI,model2)
# modelG<-lmer(Shoot.log  ~ Origin + Latitude+ (1|PopID), family=gaussian,data=modeldata)
# anova(modelG, modelI)
# modelL<-lmer(Shoot.log  ~ Origin + Generation+ (1|PopID), family=gaussian,data=modeldata)
# anova(modelL, modelI)
# modelO<-lmer(Shoot.log ~ Generation + (1|PopID), family=gaussian,data=modeldata)
# anova(modelO,modelL)
# 
# qqnorm(resid(modelL), main="Q-Q plot for residuals")
# qqline(resid(modelL))

###LfCount1, poisson
modeldata<-gen[!is.na(gen$LfCount1),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$MomFam<-as.factor(modeldata$MomFam)

modelobar<-lmer(LfCount1  ~ Origin* Generation  +(Origin|PopID/MomFam), family=poisson,data=modeldata)
model1<-lmer(LfCount1  ~ Origin* Generation  +(1|PopID/MomFam), family=poisson,data=modeldata)
anova(modelobar, model1)
model2<-lmer(LfCount1  ~ Origin* Generation  + (1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(LfCount1  ~ Origin* Generation + (1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # CrossNum is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(2.4694,1)
modelI <- lmer(LfCount1  ~ Origin + Generation + (1|blank), family=poisson,data=modeldata)
anova(modelI,model3)
modelG<-lmer(LfCount1  ~ Origin + (1|blank), family=poisson,data=modeldata)
anova(modelG, modelI)
# modelL<-lmer(LfCount1  ~ Origin + Generation+ (1|PopID), family=poisson,data=modeldata)
# anova(modelL, modelI)
modelO<-lmer(LfCount1 ~ Generation +(1|blank), family=poisson,data=modeldata)
anova(modelO,modelI)

#try glm
modelg <- glm(LfCount1 ~ Origin*Generation, family=poisson,data=modeldata)
modelg1 <- glm(LfCount1 ~ Origin+Generation, family=poisson,data=modeldata)
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq( 0.78597, 1)
dispersiontest(modelg1)
modelg3<- glm(LfCount1 ~ Origin, family=poisson,data=modeldata)
anova(modelg3,modelg1)
1-pchisq(44.724, 1)
modelg2<- glm(LfCount1 ~ Generation, family=poisson,data=modeldata)
anova(modelg2,modelg1)
1-pchisq(0.32473, 1)

CI.LS.poisson.2term(modelI, conf=95)

###lxwH, gaussian
modeldata<-gen[!is.na(gen$lxwH),]
# modeldata <- modeldata[modeldata$lxwH>0,]
modeldata <- modeldata[modeldata$lxwH>3,] #drop unlikely outlier

modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$MomFam<-as.factor(modeldata$MomFam)

modelobar<-lmer(lxwH  ~ Origin* Generation   +(Origin|PopID/MomFam), family=gaussian,data=modeldata)
model1<-lmer(lxwH  ~ Origin* Generation   +(1|PopID/MomFam), family=gaussian,data=modeldata)
anova(modelobar, model1)
model2<-lmer(lxwH  ~ Origin* Generation  + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(lxwH  ~ Origin* Generation  + (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # CrossNum is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(1.2993, 1)
# modelI <- lmer(lxwH  ~ Origin + Generation  + (1|blank), family=gaussian,data=modeldata)
# anova(modelI,model3)
# modelG<-lmer(lxwH  ~ Origin  + (1|blank), family=gaussian,data=modeldata)
# anova(modelG, modelI)
# # modelL<-lmer(lxwH  ~ Origin + Generation+ (1|blank), family=gaussian,data=modeldata)
# # anova(modelL, modelI)
# modelO<-lmer(lxwH ~ Generation +(1|blank), family=gaussian,data=modeldata)
# anova(modelO,modelI)
# modelL
# qqnorm(resid(modelL), main="Q-Q plot for residuals")
# qqline(resid(modelL))

#try glm
modelg <- glm(lxwH ~ Origin*Generation, family=gaussian,data=modeldata)
modelg1 <- glm(lxwH ~ Origin+Generation, family=gaussian,data=modeldata)
anova(modelg1, modelg, test="LRT") 
qchisq(0.8868,1,lower=FALSE)#chisq value

modelg3<- glm(lxwH ~ Origin, family=gaussian,data=modeldata)
anova(modelg3,modelg1, test="LRT")
qchisq(9.258e-11,1,lower=FALSE)#chisq value
modelg2<- glm(lxwH ~ Generation, family=gaussian,data=modeldata)
anova(modelg2,modelg1, test="LRT")
qchisq(0.004856,1,lower=FALSE)#chisq value

# checking the normality of residuals e_i:
qqnorm(resid(modelg), main="Q-Q plot for residuals")
qqline(resid(modelg))

interaction.plot(x.factor=modeldata$Generation, response=modeldata$lxwH, trace.factor=modeldata$Origin, fun = mean)
# #transformed
# modeldata$lxwH.log <- log(modeldata$lxwH)
# 
# model1<-lmer(lxwH.log  ~ Origin* Generation +(1|PopID/MomFam), family=gaussian,data=modeldata)
# model2<-lmer(lxwH.log  ~ Origin* Generation + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(lxwH.log  ~ Origin* Generation  + (1|blank), family=gaussian,data=modeldata) # Test population effect
# anova(model2,model1) # CrossNum is sig!
# anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# 
# #try glm
# modelg <- glm(lxwH.log ~ Origin*Generation, family=gaussian,data=modeldata)
# modelg1 <- glm(lxwH.log ~ Origin+Generation, family=gaussian,data=modeldata)
# anova(modelg1, modelg) #'Deviance' is chisq value
# 1-pchisq(111.09, 1)
# 
# modelg3<- glm(lxwH.log ~ Origin, family=gaussian,data=modeldata)
# anova(modelg3,modelg1)
# 1-pchisq(3.4843, 1)
# # modelg2<- glm(lxwH.log ~ Generation, family=gaussian,data=modeldata)
# # anova(modelg2,modelg1)
# anova(modelg3)
# 1-pchisq(2.4098, 1)
# 
# qqnorm(resid(modelg3), main="Q-Q plot for residuals")
# qqline(resid(modelg3))

###BoltDay.adj, poisson
modeldata<-gen[!is.na(gen$BoltDay.adj),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$MomFam<-as.factor(modeldata$MomFam)
modeldata$Generation<-as.factor(modeldata$Generation)

modelobar<-lmer(BoltDay.adj  ~ Origin* Generation  +(Origin|PopID/MomFam), family=poisson,data=modeldata)
model1<-lmer(BoltDay.adj  ~ Origin* Generation  +(1|PopID/MomFam), family=poisson,data=modeldata)
anova(modelobar, model1)
model2<-lmer(BoltDay.adj  ~ Origin* Generation  + (1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(BoltDay.adj  ~ Origin* Generation+  + (1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # CrossNum is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(2.0479, 1)
modelI <- lmer(BoltDay.adj  ~ Origin + Generation + (1|PopID/MomFam), family=poisson,data=modeldata)
anova(modelI,model1)
modelG<-lmer(BoltDay.adj  ~ Origin  + (1|PopID/MomFam), family=poisson,data=modeldata)
anova(modelG, modelI)
# modelL<-lmer(BoltDay.adj  ~ Origin + Generation+ (1|PopID/MomFam), family=poisson,data=modeldata)
# anova(modelL, modelI)
modelO<-lmer(BoltDay.adj ~ Generation +(1|PopID/MomFam), family=poisson,data=modeldata)
anova(modelO,modelI)
# modelO2 <- lmer(BoltDay.adj  ~ Origin* Generation +(1|PopID/MomFam), family=poisson,data=modeldata)
# modelO2
# int<-4.29826#inv mean
# B<-0.02222#Originnat estimate from model summary
# pI<-exp(int)
# pN<-exp(int+B)
# pI
# pN
# 
# model1L<-lmer(BoltDay.adj  ~ Origin* Generation  +(1|PopID/MomFam), family=poisson,data=modeldata)
# modelIL <- lmer(BoltDay.adj  ~ Origin + Generation + (1|PopID/MomFam), family=poisson,data=modeldata)
# anova(modelIL,model1L)

model1
CI.LS.poisson.2term(model1, conf=95)

moddata <- ddply(modeldata, .(PopID, Origin, Generation), summarize, popCount=length(PopID), popBoltDay=mean(BoltDay.adj))
qplot(data=moddata,popBoltDay, Generation, color = Origin) +geom_smooth(method=glm)

###bolt.bin, binomial
modeldata<-gen[!is.na(gen$bolt.bin),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$MomFam<-as.factor(modeldata$MomFam)

modelobar<-lmer(bolt.bin  ~ Origin* Generation +(Origin|PopID/MomFam), family=binomial,data=modeldata)
model1<-lmer(bolt.bin  ~ Origin* Generation +(1|PopID/MomFam), family=binomial,data=modeldata)
anova(modelobar, model1)
model2<-lmer(bolt.bin  ~ Origin* Generation  + (1|PopID), family=binomial,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(bolt.bin  ~ Origin* Generation + (1|blank), family=binomial,data=modeldata) # Test population effect
anova(model2,model1) # CrossNum is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
1-pchisq(1.2856, 1)
modelI <- lmer(bolt.bin  ~ Origin + Generation + (1|blank), family=binomial,data=modeldata)
anova(modelI,model3)
modelG<-lmer(bolt.bin  ~ Origin  + (1|blank), family=binomial,data=modeldata)
anova(modelG, modelI)
# modelL<-lmer(bolt.bin  ~ Origin + (1|PopID), family=binomial,data=modeldata)
# anova(modelL, modelG)
modelO<-lmer(bolt.bin ~ (1|blank), family=binomial,data=modeldata)
anova(modelO,modelG)

# modelL
# int<--2.1043 #inv mean
# B<-2.2708 #Originnat estimate from model summary
# pN<-exp(int+B)/(exp(int+B)+1) # Native
# # Note that if Origin was a continuous variable you would substitute B with B*Origin
# pI<-exp(int)/(exp(int)+1)# Introduced (B=0)
# pI # 14.5% 
# pN 

#try glm
modelg <- glm(bolt.bin ~ Origin*Generation, family=binomial,data=modeldata)
modelg1 <- glm(bolt.bin ~ Origin+Generation, family=binomial,data=modeldata)
anova(modelg1, modelg) #'Deviance' is chisq value
1-pchisq(0.22414, 1)

modelg3<- glm(bolt.bin ~ Origin, family=binomial,data=modeldata)
anova(modelg3,modelg1)
1-pchisq(5.5154, 1)
anova(modelg3)

# modelg2<- glm(bolt.bin ~ Generation, family=binomial,data=modeldata)
# anova(modelg2,modelg1)
# 1-pchisq(9.0533, 1)

#overdispersed?
deviance(modelg3) ## [1] 111.7172
summary(modelg3)$dispersion ## 1 (by definition)
dfr <- df.residual(modelg3)
deviance(modelg3)/dfr ## [1] 1.044086
d_2 <- sum(residuals(modelg3,"pearson")^2) 
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

###Germination analysis across gen... but seeds handled differently####
###doesn't include obar###
###GermAvgDate, gaussian
# # modeldata<-gen[!is.na(gen$GermAvgDate),]
# # modeldata$blank<-1
# # modeldata$blank<-as.factor(modeldata$blank)
# # modeldata$MomFam<-as.factor(modeldata$MomFam)
# # 
# # model1<-lmer(GermAvgDate  ~ Origin* Generation +(1|PopID/MomFam), family=gaussian,data=modeldata)
# # model2<-lmer(GermAvgDate  ~ Origin* Generation + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# # model3<-lmer(GermAvgDate  ~ Origin* Generation + (1|blank), family=gaussian,data=modeldata) # Test population effect
# # anova(model2,model1) # MomFam is sig!
# # anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# # 1-pchisq(3.5238, 1)
# # modelI <- lmer(GermAvgDate  ~ Origin + Generation + (1|PopID/MomFam), family=gaussian,data=modeldata)
# # anova(modelI,model1)
# # modelG<-lmer(GermAvgDate  ~ Origin  + (1|PopID/MomFam), family=gaussian,data=modeldata)
# # anova(modelG, modelI)
# # # modelL<-lmer(GermAvgDate  ~ Origin +Generation + (1|PopID/MomFam), family=gaussian,data=modeldata)
# # # anova(modelI, modelL)
# # modelO<-lmer(GermAvgDate ~ Generation +(1|PopID/MomFam), family=gaussian,data=modeldata)
# # anova(modelO,modelI)
# # 
# # qqnorm(resid(modelL), main="Q-Q plot for residuals")
# # qqline(resid(modelL))
# 
# #transformed
# modeldata<-gen[!is.na(gen$GermAvgDate),]
# modeldata$GermAvgDate.log <- log(modeldata$GermAvgDate)
# modeldata$blank<-1
# modeldata$blank<-as.factor(modeldata$blank)
# modeldata$MomFam<-as.factor(modeldata$MomFam)
# 
# model1<-lmer(GermAvgDate.log  ~ Origin* Generation +(1|PopID/MomFam), family=gaussian,data=modeldata)
# model2<-lmer(GermAvgDate.log  ~ Origin* Generation + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(GermAvgDate.log  ~ Origin* Generation + (1|blank), family=gaussian,data=modeldata) # Test population effect
# anova(model2,model1) # MomFam is sig!
# anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# 1-pchisq(3.7293, 1)
# modelI <- lmer(GermAvgDate.log  ~ Origin + Generation + (1|PopID/MomFam), family=gaussian,data=modeldata)
# anova(modelI,model1)
# modelG<-lmer(GermAvgDate.log  ~ Origin  + (1|PopID/MomFam), family=gaussian,data=modeldata)
# anova(modelG, modelI)
# # modelL<-lmer(GermAvgDate.log ~ Origin +Generation + (1|PopID/MomFam), family=gaussian,data=modeldata)
# # anova(modelI, modelL)
# modelO<-lmer(GermAvgDate.log ~ Generation +(1|PopID/MomFam), family=gaussian,data=modeldata)
# anova(modelO,modelI)
# # 
# # qqnorm(resid(modelL), main="Q-Q plot for residuals")
# # qqline(resid(modelL))
# 
# ###GermCount, gaussian
# modeldata<-gen[!is.na(gen$GermCount),]
# modeldata$blank<-1
# modeldata$blank<-as.factor(modeldata$blank)
# modeldata$MomFam<-as.factor(modeldata$MomFam)
# 
# model1<-lmer(GermCount  ~ Origin* Generation +SeedCount+(1|PopID/MomFam), family=gaussian,data=modeldata)
# model2<-lmer(GermCount  ~ Origin* Generation  +SeedCount+ (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
# model3<-lmer(GermCount  ~ Origin* Generation +SeedCount+ (1|blank), family=gaussian,data=modeldata) # Test population effect
# anova(model2,model1) # CrossNum is sig!
# anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
# 1-pchisq(3.7267, 1)
# modelI <- lmer(GermCount  ~ Origin + Generation +SeedCount+ (1|PopID/MomFam), family=gaussian,data=modeldata)
# anova(modelI,model1)
# modelG<-lmer(GermCount  ~ Origin  +SeedCount+ (1|PopID/MomFam), family=gaussian,data=modeldata)
# anova(modelG, modelI)
# # modelL<-lmer(GermCount  ~ Origin  +SeedCount+(1|PopID/MomFam), family=gaussian,data=modeldata)
# # anova(modelL, modelG)
# # modelS <- lmer(GermCount  ~ Origin+(1|PopID/MomFam), family=gaussian,data=modeldata)
# # anova(modelS, modelL)
# modelO<-lmer(GermCount ~ SeedCount+(1|PopID/MomFam), family=gaussian,data=modeldata)
# anova(modelO,modelG)