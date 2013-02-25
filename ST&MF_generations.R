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
totmf <- totmf[,-c(7:8,16:18,26,29:30, 32:38, 40:44, 46:57)]
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
stco <- stco[,-c(7,9,13:16)]
#make columns agree
stco$Exp <- "st"
totmf$Trt <- "cont"
stco$MomFam <- stco$Mom
colnames(stco)[16] <- "ID_barcode"
colnames(totmf)[18] <- "ID_barcode"
colnames(stco)[21] <- "BoltDay.adj"
stco$BoltDay.adj <- stco$BoltDay.adj + 3 # to make it comparable to mf, which is adj for early bolters
totmf <- totmf[,-26]
colnames(stco)[6] <- "Cross_MomID"
colnames(totmf)[6] <- "Cross_MomID"
#same cols
totmf <- totmf[,-c(14)]
stco <- stco[, -c(11)]
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
#REML, using lme4
library(lme4)

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

model1<-lmer(LfCountH  ~ Origin* Generation +Latitude +(1|PopID/MomFam), family=poisson,data=modeldata)
model2<-lmer(LfCountH  ~ Origin* Generation+ Latitude + (1|PopID), family=poisson,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(LfCountH  ~ Origin* Generation+Latitude + (1|blank), family=poisson,data=modeldata) # Test population effect
anova(model2,model1) # CrossNum is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
modelI <- lmer(LfCountH  ~ Origin + Generation+Latitude + (1|PopID/MomFam), family=poisson,data=modeldata)
anova(modelI,model1)
modelG<-lmer(LfCountH  ~ Origin + Latitude+ (1|PopID/MomFam), family=poisson,data=modeldata)
anova(modelG, modelI)
modelL<-lmer(LfCountH  ~ Origin + Generation +(1|PopID/MomFam), family=poisson,data=modeldata)
anova(modelL, modelI)
modelO<-lmer(LfCountH ~ Generation +(1|PopID/MomFam), family=poisson,data=modeldata)
anova(modelO,modelL)
modelInt<-lmer(LfCountH  ~ Origin* Generation +(1|PopID/MomFam), family=poisson,data=modeldata)
anova(modelInt, modelL)
modelInt
int<-2.75258#inv mean
B<--0.24940#Originnat estimate from model summary
pI<-exp(int)
pN<-exp(int+B)
pI
pN


###ShootMass.g, gaussian... log?
modeldata<-gen[!is.na(gen$ShootMass.g),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$MomFam<-as.factor(modeldata$MomFam)

model1<-lmer(ShootMass.g  ~ Origin* Generation +Latitude +(1|PopID/MomFam), family=gaussian,data=modeldata)
model2<-lmer(ShootMass.g  ~ Origin* Generation+ Latitude + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(ShootMass.g  ~ Origin* Generation+Latitude + (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # CrossNum is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
modelI <- lmer(ShootMass.g  ~ Origin + Generation+Latitude + (1|PopID), family=gaussian,data=modeldata)
anova(modelI,model2)
modelG<-lmer(ShootMass.g  ~ Origin + Latitude+ (1|PopID), family=gaussian,data=modeldata)
anova(modelG, modelI)
modelL<-lmer(ShootMass.g  ~ Origin + (1|PopID), family=gaussian,data=modeldata)
anova(modelL, modelI)
modelO<-lmer(ShootMass.g ~ Latitude +(1|PopID), family=gaussian,data=modeldata)
anova(modelO,modelI)

###LfCount1, poisson

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
# anova(modelO,model1)

###BoltDay.adj, poisson

###lxwH, gaussian
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
# anova(modelO,model1)

###bolt.bin, binomial

###GermAvgDate, gaussian
modeldata<-gen[!is.na(gen$GermAvgDate),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$MomFam<-as.factor(modeldata$MomFam)

model1<-lmer(GermAvgDate  ~ Origin* Generation +(1|PopID/MomFam), family=gaussian,data=modeldata)
model2<-lmer(GermAvgDate  ~ Origin* Generation + (1|PopID), family=gaussian,data=modeldata) # Removes maternal family variance to test if it is a significant random effect
model3<-lmer(GermAvgDate  ~ Origin* Generation + (1|blank), family=gaussian,data=modeldata) # Test population effect
anova(model2,model1) # MomFam is sig!
anova(model3,model2) # pop is sig. If it says there are 0 d.f. then what you want to do is a Chi-square test using the X2 value and 1 d.f. freedom to get the p value.
modelI <- lmer(GermAvgDate  ~ Origin + Generation + (1|PopID/MomFam), family=gaussian,data=modeldata)
anova(modelI,model1)
modelG<-lmer(GermAvgDate  ~ Origin + (1|PopID/MomFam), family=gaussian,data=modeldata)
anova(modelG, modelI)
# modelL<-lmer(GermAvgDate  ~ Origin +Generation + (1|PopID/MomFam), family=gaussian,data=modeldata)
# anova(modelI, modelL)
modelO<-lmer(GermAvgDate ~ Generation +(1|PopID/MomFam), family=gaussian,data=modeldata)
anova(modelO,modelI)

###GermPercent, gaussian
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
# anova(modelO,model1)