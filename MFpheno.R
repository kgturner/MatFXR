#Mat FX pheno analysis

strptime(data2010$Bolt.date,format="%d/%m/%y")  # converts dates to numeric

#load spreadsheets

mfm1<- read.csv(file.choose(), header=T, sep=",", quote='"') #MF measure 1.csv
str(mfm1)
head(mfm1)
row.names(mfm1)<-mfm1[,1]
mfm1<-mfm1[,1:13]
mfm1$Indiv<-as.factor(mfm1$Indiv)
mfm1$CrossNum<-as.factor(mfm1$CrossNum)
mfm1[mfm1$Barcode=="",]


write.table(mfm1, file="MFm1.txt", sep="\t", quote=F)
mfm1.dk<-mfm1[mfm1$Origin!="SK",]
mfm1.dk$Origin<-droplevels(mfm1.dk$Origin)
mfm1.dk$Trt<-droplevels(mfm1.dk$Trt)
mfm1.dk$Exp<-droplevels(mfm1.dk$Exp)
mfm1.dk$CrossNum<-as.factor(mfm1.dk$CrossNum)
mfm1.dk$lxw<-mfm1.dk$LfLgth1 * mfm1.dk$LfWdth1
max(mfm1.dk$lxw)
mfm1.dk[mfm1.dk$lxw==max(mfm1.dk$lxw),]
mfm1.dk["CA001-10-6","LfLgth1"]<-16.1
mfm1.dk["CA001-9-27","LfWdth1"]<-4.2
write.table(mfm1.dk, file="MFm1.dk.txt", sep="\t", quote=F)

mfm2<- read.csv(file.choose(), header=T, sep=",", quote='"') #MF measure 2.csv
str(mfm2)
head(mfm2)
row.names(mfm2)<-mfm2[,1]
mfm2<-mfm2[,1:13]
mfm2$Indiv<-as.factor(mfm2$Indiv)
mfm2$CrossNum<-as.factor(mfm2$CrossNum)
mfm2[mfm2$Barcode=="",]
mfm2<-mfm2[mfm2$Barcode!="",]
mfm2$Barcode<-droplevels(mfm2$Barcode)
mfm2$PopID<-droplevels(mfm2$PopID)
mfm2$Origin<-droplevels(mfm2$Origin)
mfm2$Exp<-droplevels(mfm2$Exp)
mfm2$Trt<-droplevels(mfm2$Trt)
write.table(mfm2, file="MFm2.txt", sep="\t", quote=F)

mfmh<- read.csv(file.choose(), header=T, sep=",", quote='"') #MF measure harvest.csv
str(mfmh)
head(mfmh)
row.names(mfmh)<-mfmh[,3]
mfmh[duplicated(mfmh$Barcode),]
mfmh<-mfmh[,1:28]
mfmh[123,"Rack"]<-20
mfmh[123,"Position"]<-27
mfmh[123,"MsrHarvest"]<-"10/21/2010"
mfmh[123,"ShootMass.g"]<-4.831
mfmh<-mfmh[-131,]
mfmh$Indiv<-as.factor(mfmh$Indiv)
mfmh$CrossNum<-as.factor(mfmh$CrossNum)
mfmh<-mfmh[mfmh$Barcode!="",]
mfmh["CA001-3-19","BoltedatH"]<-"y"
mfmh["CA001-3-19","FlrHeadCount"]<-NA
mfmh["CA001-3-19","CrownDiam.mm"]<-0.9
mfmh<-mfmh[mfmh$Barcode!="",]
mfmh$BoltedatH<-droplevels(mfmh$BoltedatH)
mfmh$Barcode<-droplevels(mfmh$Barcode)
mfmh[mfmh$Origin=="",]
mfmh$Origin<-droplevels(mfmh$Origin)
mfmh$Exp<-droplevels(mfmh$Exp)
mfmh$Trt<-droplevels(mfmh$Trt)
mfmh$Pop<-droplevels(mfmh$Pop)
colnames(mfmh)[2]<-"Pos"
colnames(mfmh)[4]<-"PopID"
colnames(mfmh)[14]<-"BoltCountH"
colnames(mfmh)[15]<-"MaxBoltHtH"
colnames(mfmh)[16]<-"HBoltHt1"
colnames(mfmh)[17]<-"HBoltHt2"
colnames(mfmh)[18]<-"HBoltHt3"
colnames(mfmh)[19]<-"HBoltHt4"
colnames(mfmh)[20]<-"HBoltHt5"
colnames(mfmh)[21]<-"HBoltHt6"
colnames(mfmh)[22]<-"HBoltHt7"
mfmh$MsrHarvest2<-strptime(mfmh$MsrHarvest,format="%m/%d/%Y")  # converts dates to numeric
# strptime(mfmh[300,]$MsrHarvest,format="%m/%d/%y")
# date<-strptime("7/27/2010",format="%m/%d/%Y") #day zero for MF
# strptime(date,format="%j") #day zero for MF
# mydates <- as.Date(c("2010-07-27", "2010-07-29"))
# days <- mydates[2] - mydates[1]
# as.numeric(days)
mfmh$MsrHarvest2<-as.Date(mfmh$MsrHarvest2)
day0<-as.Date("2010-07-27")
# daytest<-as.Date(mfmh[6,]$MsrHarvest2)
# dayresult<-daytest-day0
mfmh$MsrHarvest3<-as.numeric(mfmh$MsrHarvest2-day0)
head(mfmh$MsrHarvest3)

write.table(mfmh, file="MFmH.txt", sep="\t", quote=F)

mfdeath<- read.csv(file.choose(), header=T, sep=",", quote='"') #MF death.csv
str(mfdeath)
row.names(mfdeath)<-mfdeath[,3]
mfdeath$Indiv<-as.factor(mfdeath$Indiv)
mfdeath$CrossNum<-as.factor(mfdeath$CrossNum)
###dates...?
#mfdeath$Wilt.r<-strptime(mfdeath$Wilt,format="%m/%d/%y")  # converts dates to numeric
write.table(mfdeath, file="MFdeath.txt", sep="\t", quote=F)

mfsla<- read.csv(file.choose(), header=T, sep=",", quote='"') #MFsla.csv
str(mfsla)
row.names(mfsla)<-mfsla[,1]
mfsla[duplicated(mfsla$Barcode),]
mfsla[mfsla$Barcode=="US003-7-13",]
mfsla[168,"Barcode"]<-"US003-7-13"
mfsla[168,"Origin"]<-"inv"
mfsla<-mfsla[row.names(mfsla)!="US003-6-16",]
mfsla<-mfsla[row.names(mfsla)!="CA001-20-6",]
head(mfsla)
mfsla[mfsla$Origin=="",]
mfsla["US003-6-16","Origin"]<-"inv"
mfsla["CA001-20-6","Origin"]<-"inv"
mfsla["CA001-20-6","Trt"]<-"control"
mfsla$Origin<-droplevels(mfsla$Origin)
mfsla$Trt<-droplevels(mfsla$Trt)
write.table(mfsla, file="MFsla.txt", sep="\t", quote=F)
mfsla<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1)

mfallo<- read.csv(file.choose(), header=T, sep=",", quote='"') #MFallo.csv
str(mfallo)
row.names(mfallo)<-mfallo[,1]
#mfallo[duplicated(mfallo$Barcode),]
mfallo$Indiv<-as.factor(mfallo$Indiv)
mfallo$CrossNum<-as.factor(mfallo$CrossNum)

write.table(mfallo, file="MFAllo.txt", sep="\t", quote=F)
mfallo.dk<-mfallo[mfallo$Origin!="SK",]
mfallo.dk$Origin<-droplevels(mfallo.dk$Origin)
mfallo.dk$PopCross<-as.factor(paste(mfallo.dk$PopID,mfallo.dk$CrossNum))
mean(mfallo.dk[mfallo.dk$PopCross=="BG001 14",]$Mass.gA)
sd(mfallo.dk[mfallo.dk$PopCross=="BG001 17",]$Mass.gA)
write.table(mfallo.dk, file="MFallo.dk.txt", sep="\t", quote=F)

mfmom<- read.csv(file.choose(), header=T, sep=",", quote='"') #MFmom.csv
str(mfmom)
row.names(mfmom)<-mfmom[,1]
mfmom<-mfmom[,1:17]

write.table(mfmom, file="MFmom.txt", sep="\t", quote=F)
mfmom.dk<-mfmom[mfmom$Origin!="SK",]
mfmom.dk$Origin<-droplevels(mfmom.dk$Origin)
mfmom.dk$GermPerc<-mfmom.dk$GermCount/mfmom.dk$SeedCount*100
write.table(mfmom.dk, file="MFmom.dk.txt", sep="\t", quote=F)


#####make stress tables######
#mfl -- lf choice - done!
#mfallo -- allo - done!
#mfm1 -- largest early control - done!

#mfco -- control
#mfmh[control] + mfm1[control] + mfsla[control] + mfdeath[control] + mfm2[control]
conth<-mfmh[mfmh$Trt=="control",]
cont2<-mfm2[mfm2$Trt=="control",]
setdiff(conth$Barcode,cont2$Barcode)
setdiff(cont2$Barcode, conth$Barcode)
mfco<-merge(conth, cont2, all.x=TRUE, all.y=TRUE)
row.names(mfco)<-mfco$Barcode
mfco<-merge(mfco,mfm1, all.x=TRUE) # only take from mfm1 what is already in mfco
contd<-mfdeath[mfdeath$Trt=="control",]
setdiff(mfco$Barcode,contd$Barcode)
setdiff(contd$Barcode, mfco$Barcode)
mfco<-merge(mfco, contd, all.x=TRUE, all.y=TRUE)
row.names(mfco)<-mfco$Barcode
summary(mfco)
xtabs(~Origin + Exp, mfco)
mfco$Exp<-droplevels(mfco$Exp)
mfco[is.na(mfco$PopID),]
contsla<-mfsla[mfsla$Trt=="control",]
setdiff(contsla$Barcode,mfco$Barcode)
setdiff(mfco$Barcode, contsla$Barcode)
contsla[is.na(contsla$PopID),]
mfco<-merge(mfco, contsla, all.x=TRUE)
summary(mfco)
colnames(mfco)[31]<-"HarvestDay"
mfco2<-mfco[,-c(26,29,39:43,49:60)]
mfco<-mfco2
mfco[is.na(mfco$PopID),]
mfco$BoltDay<-strptime(mfco$BoltDate,format="%m/%d/%Y")  # converts dates to POSIXlt
str(mfco$BoltDay)
mfco$BoltDay<-as.Date(mfco$BoltDay)
day0<-as.Date("2010-07-27") #experimental day zero
mfco$BoltDay<-as.numeric(mfco$BoltDay-day0)
str(mfco$BoltDay)
summary(mfco$BoltDay)
huh<-mfco[!is.na(mfco$BoltDay),]
huh[huh$BoltDay<0,]
mfco<-mfco[!is.na(mfco$Barcode),]
mfco$Trt<-droplevels(mfco$Trt)
#cross check BoltDay and BoltedatH
error<-mfco[mfco$BoltedatH=="n",]
error[!is.na(error$BoltDay),]
dateerror<-mfco[is.na(mfco$BoltDay),]
dateerror[dateerror$BoltedatH=="y",]
summary(dateerror)
mfco[263,"BoltDay"]<-85
mfco[267,"BoltDay"]<-85
mfco[271,"BoltDay"]<-85
mfco[17, "BoltedatH"]<-"y"
mfco[259, "BoltedatH"]<-"y"
huh<-mfco[mfco$PopID=="US003",]
huh

write.table(mfco, file="Mat fx and Ass map control.txt", sep="\t", quote=F) 
mfco<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1)

####subsets of data
###control#####
#mat fx only, diffuse only
mfco.dk<-mfco[mfco$Exp=="mat fx",]
mfco.dk<-mfco.dk[mfco.dk$Origin!="SK",]
mfco.dk$Exp<-droplevels(mfco.dk$Exp)
mfco.dk$Origin<-droplevels(mfco.dk$Origin)
summary(mfco.dk)
mfco.dk<-mfco.dk[,-c(19:23,36:37,41)]
mfco.dk$CrossNum<-as.factor(mfco.dk$CrossNum)
huh<-mfco.dk[mfco.dk$PopID=="GR002",]
huh

write.table(mfco.dk, file="Mat fx strict control.txt", sep="\t", quote=F) 

#bonus mat fx, balanced add'n of ass map plants, diffuse only
mfco.dk1<-mfco[mfco$Exp!="mat fx",]
mfco.dk1<-mfco.dk1[mfco.dk1$Origin=="nat",]
summary(mfco.dk1$PopID)
mfco.am<-mfco[mfco$Exp!="mat fx",]
summary(mfco.am$PopID)
mfco.dk1<-merge(mfco.dk1, mfco.dk, all=TRUE)
bonusinv<-c("CA001-12-2","CA001-8-23","CA001-4-6","CA001-20-12","CA001-5-18","CA001-11-9","CA001-7-1","CA001-15-2",
            "US001-2-4","US001-3-1","US001-10-1","US001-5-1","US001-7-11","US001-4-1","US001-6-2","US001-7-3",
            "US002-3-8","US002-1-1","US002-5-11","US002-6-5","US002-3-14","US002-2-2","US002-4-1","US002-5-8",
            "US003-16-1","US003-9-5","US003-3-5","US003-14-9","US003-13-4","US003-11-11","US003-4-2","US003-14-7")
binv<-mfco.am[mfco.am$Barcode %in% bonusinv,]
mfco.dk1<-merge(mfco.dk1, binv, all=TRUE)
mfco.dk1<-mfco.dk1[,-c(39:42,44)]
mfco.dk1$CrossNum<-as.factor(mfco.dk1$CrossNum)
mfco.dk1$PopID<-droplevels(mfco.dk1$PopID)
mfco.dk1$Origin<-droplevels(mfco.dk1$Origin)
mfco.dk1$sla<-mfco.dk1$SLAarea/mfco.dk1$SLAmass.g
max(mfco.dk1$SLAmass.g)
mfco.dk1[mfco.dk1$SLAmass.g==0.95,]
mfco.dk1[38, "SLAmass.g"]<-0.095

write.table(mfco.dk1, file="Mat fx bonus control.txt", sep="\t", quote=F) 

#mfcom1 - all plants in balanced mat fx analyses, for m1 traits
#mfco.dk1+mfcu.dk+mfallo.dk+mfn.dk+mfd.dk+mff.dk
mfcom1<-merge(mfco.dk1, mfallo.dk, all.x=TRUE, all.y=TRUE)
mfcom1<-merge(mfcom1, mfcu.dk, all.x=TRUE, all.y=TRUE)
mfcom1<-merge(mfcom1, mfn.dk, all.x=TRUE, all.y=TRUE)
mfcom1<-merge(mfcom1, mfd.dk, all.x=TRUE, all.y=TRUE)
mfcom1<-merge(mfcom1, mff.dk, all.x=TRUE, all.y=TRUE)

mfcom1$PopCross<-as.factor(paste(mfcom1$PopID,mfcom1$CrossNum))
summary(mfcom1$PopCross)

write.table(mfcom1, file="MF bonus control m1.txt", sep="\t", quote=F)


#full set under control conditions - include SK and dna trt
#mfco + #mfmh[dna] + mfm1[dna] + mfsla[dna] + mfdeath[dna] + mfm2[dna]
hdna<-mfmh[mfmh$Trt=="dna",]
dna1<-mfm1[mfm1$Trt=="dna",]
dna2<-mfm2[mfm2$Trt=="dna",]
ddna<-mfdeath[mfdeath$Trt=="dna",]
sdna<-mfsla[mfsla$Trt=="dna",]
dna<-merge(hdna,dna1, all=TRUE)
dna<-merge(dna, dna2)
dna<-merge(dna, ddna)
dna<-merge(dna, sdna, all.x=TRUE)
error<-dna[dna$BoltedatH=="n",]
error[!is.na(error$BoltDay),]
dateerror<-dna[is.na(dna$BoltDay),]
dateerror[dateerror$BoltedatH=="y",]

write.table(dna, file="Mat fx dna array control.txt", sep="\t", quote=F) 
mfco.full<-merge(mfco, dna,all=TRUE)
mfco.full[mfco.full$Trt=="dna",]
write.table(mfco.full, file="Mat fx and Ass map full control.txt", sep="\t", quote=F) 

####cut#####
# mfm1[cut] + mfsla[cut] + mfdeath[cut] + mfm2[cut]+mfmh[cut]
cut1<-mfm1[mfm1$Trt=="cut",]
cut2<-mfm2[mfm2$Trt=="cut",]
cuts<-mfsla[mfsla$Trt=="cut",]
cuth<-mfmh[mfmh$Trt=="cut",]
cutd<-mfdeath[mfdeath$Trt=="cut",]
mfcu<-merge(cut1, cut2)
mfcu<-merge(mfcu, cuts,all.x=TRUE)
mfcu<-merge(mfcu, cuth)
mfcu<-merge(mfcu, cutd)
summary(mfcu)
mfcu<-mfcu[,-c(10,14,28:33,35:36,39,42:47,51:63)]
mfcu$BoltDay<-strptime(mfcu$BoltDate,format="%m/%d/%Y")  # converts dates to POSIXlt
str(mfcu$BoltDay)
mfcu$BoltDay<-as.Date(mfcu$BoltDay)
day0<-as.Date("2010-07-27") #experimental day zero
mfcu$BoltDay<-as.numeric(mfcu$BoltDay-day0)
str(mfcu$BoltDay)
summary(mfcu$BoltDay)
mfcu$Trt<-droplevels(mfcu$Trt)
#cross check BoltDay and BoltedatH
error<-mfcu[mfcu$BoltedatH=="n",]
error[!is.na(error$BoltDay),]
dateerror<-mfcu[is.na(mfcu$BoltDay),]
dateerror[dateerror$BoltedatH=="y",]
summary(dateerror)

write.table(mfcu, file="Mat fx cut.txt", sep="\t", quote=F) 
mfcu.dk<-mfcu[mfcu$Origin!="SK",]
write.table(mfcu.dk, file="Mat fx cut.dk.txt", sep="\t", quote=F)

########nut def######
# mfm1[nut] + mfsla[nut] + mfdeath[nut] + mfm2[nut]+mfmh[nut]
nut1<-mfm1[mfm1$Trt=="nut def",]
nut2<-mfm2[mfm2$Trt=="nut def",]
nuts<-mfsla[mfsla$Trt=="nut def",]
nuth<-mfmh[mfmh$Trt=="nut def",]
nutd<-mfdeath[mfdeath$Trt=="nut def",]
mfn<-merge(nut1, nut2)
mfn<-merge(mfn, nuts,all.x=TRUE)
mfn<-merge(mfn, nuth)
mfn<-merge(mfn, nutd)
summary(mfn)
mfn<-mfn[,-c(10,14,28:33,35:36,39,42:43,45:47,49:63)]
mfn$BoltDay<-strptime(mfn$BoltDate,format="%m/%d/%Y")  # converts dates to POSIXlt
str(mfn$BoltDay)
mfn$BoltDay<-as.Date(mfn$BoltDay)
day0<-as.Date("2010-07-27") #experimental day zero
mfn$BoltDay<-as.numeric(mfn$BoltDay-day0)
str(mfn$BoltDay)
summary(mfn$BoltDay)
mfn$YellowDay<-strptime(mfn$Yellow,format="%m/%d/%Y")  # converts dates to POSIXlt
str(mfn$YellowDay)
mfn$YellowDay<-as.Date(mfn$YellowDay)
day0<-as.Date("2010-07-27") #experimental day zero
mfn$YellowDay<-as.numeric(mfn$YellowDay-day0)
str(mfn$YellowDay)
summary(mfn$YellowDay)
mfn$Trt<-droplevels(mfn$Trt)
#cross check BoltDay and BoltedatH
error<-mfn[mfn$BoltedatH=="n",]
error[!is.na(error$BoltDay),]
dateerror<-mfcu[is.na(mfcu$BoltDay),]
dateerror[dateerror$BoltedatH=="y",]
summary(dateerror)
mfn[11,"CrownDiam.mm"]<-0.75
mfn[12,"CrownDiam.mm"]<-0.65

write.table(mfn, file="Mat fx nut def.txt", sep="\t", quote=F)
mfn.dk<-mfn[mfn$Origin!="SK",]
write.table(mfn.dk, file="Mat fx nut def.dk.txt", sep="\t", quote=F)

#####drought#####
# mfm1[dr] + mfdeath[dr] 
dr1<-mfm1[mfm1$Trt=="drought",]
drd<-mfdeath[mfdeath$Trt=="drought",]
mfd<-merge(dr1, drd)
summary(mfd)
mfd<-mfd[,-c(16,18:35)]
mfd$DeathDay<-strptime(mfd$Death,format="%m/%d/%Y")  # converts dates to POSIXlt
str(mfd$DeathDay)
mfd$DeathDay<-as.Date(mfd$DeathDay)
day0<-as.Date("2010-07-27") #experimental day zero
mfd$DeathDay<-as.numeric(mfd$DeathDay-day0)
str(mfd$DeathDay)
summary(mfd$DeathDay)
mfd$WiltDay<-strptime(mfd$Wilt,format="%m/%d/%Y")  # converts dates to POSIXlt
str(mfd$WiltDay)
mfd$WiltDay<-as.Date(mfd$WiltDay)
day0<-as.Date("2010-07-27") #experimental day zero
mfd$WiltDay<-as.numeric(mfd$WiltDay-day0)
str(mfd$WiltDay)
summary(mfd$WiltDay)
mfd$TotWiltDay<-strptime(mfd$TotWilt,format="%m/%d/%Y")  # converts dates to POSIXlt
str(mfd$TotWiltDay)
mfd$TotWiltDay<-as.Date(mfd$TotWiltDay)
day0<-as.Date("2010-07-27") #experimental day zero
mfd$TotWiltDay<-as.numeric(mfd$TotWiltDay-day0)
str(mfd$TotWiltDay)
summary(mfd$TotWiltDay)
mfd$Trt<-droplevels(mfd$Trt)

write.table(mfd, file="Mat fx drought.txt", sep="\t", quote=F)
mfd.dk<-mfd[mfd$Origin!="SK",]
write.table(mfd.dk, file="Mat fx drought.dk.txt", sep="\t", quote=F)

######flood########
# mfm1[fl] + mfdeath[fl] 
fl1<-mfm1[mfm1$Trt=="flood",]
fld<-mfdeath[mfdeath$Trt=="flood",]
mff<-merge(fl1, fld)
summary(mff)
mff<-mff[,-c(14:15,19:35)]
mff$DeathDay<-strptime(mff$Death,format="%m/%d/%Y")  # converts dates to POSIXlt
str(mff$DeathDay)
mff$DeathDay<-as.Date(mff$DeathDay)
day0<-as.Date("2010-07-27") #experimental day zero
mff$DeathDay<-as.numeric(mff$DeathDay-day0)
str(mff$DeathDay)
summary(mff$DeathDay)
mff$YellowDay<-strptime(mff$Yellow,format="%m/%d/%Y")  # converts dates to POSIXlt
str(mff$YellowDay)
mff$YellowDay<-as.Date(mff$YellowDay)
day0<-as.Date("2010-07-27") #experimental day zero
mff$YellowDay<-as.numeric(mff$YellowDay-day0)
str(mff$YellowDay)
summary(mff$YellowDay)
mff$FloatDay<-strptime(mff$Float,format="%m/%d/%Y")  # converts dates to POSIXlt
str(mff$FloatDay)
mff$FloatDay<-as.Date(mff$FloatDay)
day0<-as.Date("2010-07-27") #experimental day zero
mff$FloatDay<-as.numeric(mff$FloatDay-day0)
str(mff$FloatDay)
summary(mff$FloatDay)
mff$Trt<-droplevels(mff$Trt)

write.table(mff, file="Mat fx flood.txt", sep="\t", quote=F)
mff.dk<-mff[mff$Origin!="SK",]
write.table(mff.dk, file="Mat fx flood.dk.txt", sep="\t", quote=F)

#####mom############
summary(mfmom)
mfmom[is.na(mfmom$GermAvgDate),]
summary(mfmom$PopID)
mfmom$GermPercent<-mfmom$GermCount/mfmom$SeedCount*100
max(mfmom$GermPercent)
mfmom[mfmom$GermPercent==max(mfmom$GermPercent),]
mfmom["BG001-3","SeedCount"]<-4
mfmom["BG001-3","GermPercent"]<-100
plot(mfmom$PopID, mfmom$GermPercent, col=mfmom$Origin)
plot(mfmom$Origin, mfmom$GermPercent, col=mfmom$PopID)

write.table(mfmom, file="Mat fx mom.txt", sep="\t", quote=F)
mfmom.dk<-mfmom[mfmom$Origin!="sk",]
write.table(mfmom.dk, file="Mat fx mom.dk.txt", sep="\t", quote=F)


###########family means table#############
mfcom1$PopCross<-as.factor(paste(mfcom1$PopID,mfcom1$CrossNum))
summary(mfcom1$PopCross)
mean(mfcom1[mfcom1$PopCross=="BG001 17",]$LfLgth1)
sd(mfcom1[mfcom1$PopCross=="BG001 17",]$LfLgth1)

se <- function(x) sqrt(var(x, na.rm=TRUE)/(length(na.omit(x))-1))

# tapply(mfcom1$LfLgth1, mfcom1$PopCross,mean,na.rm=TRUE)
# tapply(mfcom1$LfLgth1, mfcom1$PopCross,se)
# plt <- barplot(tapply(mfcom1$LfLgth1, mfcom1$PopCross,mean,na.rm=TRUE), ylim=c(0, 30))
# y.se <- tapply(mfcom1$LfLgth1, mfcom1$PopCross,se)
# y.mean <- tapply(mfcom1$LfLgth1, mfcom1$PopCross,mean,na.rm=TRUE)
# # y.mean + y.se
# # max(y.mean + y.se)
# # c(0, max(y.mean + y.se, na.rm=TRUE))
# ylim <- c(0, max(y.mean + y.se, na.rm=TRUE))

mffam<-as.data.frame(tapply(mfcom1$LfLgth1, mfcom1$PopCross,mean))
mffam$PopCross<-row.names(mffam)
colnames(mffam)[1]<-"LfLgth1"
mffam$LfLgth1<-as.numeric(mffam$LfLgth1)

mffam<-cbind(mffam, tapply(mfcom1$LfCount1, mfcom1$PopCross,mean))
colnames(mffam)[3]<-"LfCount1"
mffam$LfCount1<-as.integer(mffam$LfCount1)

mffam<-cbind(mffam, tapply(mfcom1$LfWdth1, mfcom1$PopCross,mean,na.rm=TRUE))
colnames(mffam)[4]<-"LfWdth1"
mffam$LfWdth1<-as.numeric(mffam$LfWdth1)

mffam<-cbind(mffam,tapply(mfcom1$Crown.mmA, mfcom1$PopCross,mean,na.rm=TRUE))
colnames(mffam)[5]<-"Crown.mmA"
mffam$Crown.mmA<-as.numeric(mffam$Crown.mmA)

mffam<-cbind(mffam,tapply(mfcom1$Mass.gA, mfcom1$PopCross,mean,na.rm=TRUE))
colnames(mffam)[6]<-"Mass.gA"
mffam$LfLgth1<-as.numeric(mffam$Mass.gA)

#repeat for all traits....don't forget SE!
#need origin and pop columns....
pops<-as.data.frame(mfcom1$PopID)
pops<-cbind(pops,mfcom1$Origin)


write.table(mffam.dk, file="MF family means control.txt", sep="\t", quote=F)

####add lat/long to data tables
lat<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #popcoord.txt
colnames(lat)[3]<-"PopID"

mfco.dk1<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #Mat fx bonus control.txt
mfcom1<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #m1 all plants in analysis, balanced, dk only, MF bonus control m1.txt
mfallo.dk<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #allo, dk only
mfn.dk<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #nut, dk only
mfcu.dk<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #cut, dk only
mfd.dk<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #drought, dk only
mff.dk<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #flood, dk only
mfmom.dk<-read.table(file.choose(), header=T, sep="\t", quote='"', row.names=1) #mom, dk only

mfallo.dk<-merge(mfallo.dk,lat, all.x=TRUE)
mfmom.dk<-merge(mfmom.dk,lat, all.x=TRUE)
mfco.dk1<-merge(mfco.dk1,lat, all.x=TRUE)
mfcom1<-merge(mfcom1,lat, all.x=TRUE)
mfn.dk<-merge(mfn.dk,lat, all.x=TRUE)
mfcu.dk<-merge(mfcu.dk,lat, all.x=TRUE)
mfd.dk<-merge(mfd.dk,lat, all.x=TRUE)
mff.dk<-merge(mff.dk,lat, all.x=TRUE)

write.table(mfmom.dk, file="Mat fx mom.dk.txt", sep="\t", quote=F)
write.table(mff.dk, file="Mat fx flood.dk.txt", sep="\t", quote=F)
write.table(mfd.dk, file="Mat fx drought.dk.txt", sep="\t", quote=F)
write.table(mfn.dk, file="Mat fx nut def.dk.txt", sep="\t", quote=F)
write.table(mfcu.dk, file="Mat fx cut.dk.txt", sep="\t", quote=F)
write.table(mfcom1, file="MF bonus control m1.txt", sep="\t", quote=F)
write.table(mfco.dk1, file="Mat fx bonus control.txt", sep="\t", quote=F)
write.table(mfallo.dk, file="MFallo.dk.txt", sep="\t", quote=F)


#####################
#add mom info to data sets
mominfo <- data.frame(PopID=mfmom.dk$PopID, CrossNum = as.factor(mfmom.dk$CrossNum), Mom = as.factor(mfmom.dk$Mom), 
                         MomFam = mfmom.dk$MomFam, MomIndiv = mfmom.dk$MomIndiv, DadID = mfmom.dk$DadID, 
                         DadFam = mfmom.dk$DadFam, DadIndiv = mfmom.dk$DadIndiv)


mfco.dk1<-merge(mfco.dk1,mominfo, all.x=TRUE)

mfallo.dk<-merge(mfallo.dk,mominfo, all.x=TRUE)
mfcom1<-merge(mfcom1,mominfo, all.x=TRUE)
mfn.dk<-merge(mfn.dk,mominfo, all.x=TRUE)
mfcu.dk<-merge(mfcu.dk,mominfo, all.x=TRUE)
mfd.dk<-merge(mfd.dk,mominfo, all.x=TRUE)
mff.dk<-merge(mff.dk,mominfo, all.x=TRUE)

write.table(mff.dk, file="Mat fx flood.dk.txt", sep="\t", quote=F)
write.table(mfd.dk, file="Mat fx drought.dk.txt", sep="\t", quote=F)
write.table(mfn.dk, file="Mat fx nut def.dk.txt", sep="\t", quote=F)
write.table(mfcu.dk, file="Mat fx cut.dk.txt", sep="\t", quote=F)
write.table(mfcom1, file="MF bonus control m1.txt", sep="\t", quote=F)
write.table(mfco.dk1, file="Mat fx bonus control.txt", sep="\t", quote=F)
write.table(mfallo.dk, file="MFallo.dk.txt", sep="\t", quote=F)
##############################
#pop ranks





#remove small pops (<3)
# summary(d$Pop)
# d<-d[d$Pop!="CA008",]
