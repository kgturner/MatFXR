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

# #tidy columns
# lf<-subset(lf,select=-c(mom.indiv,Eaten.arsq, EatenRatioI.N, Eatenlog,def.round.tray, def.round.tray.or))
# lf$Exp<-"st"
# colnames(lf)[4]<-"TrayPos"#changed from TrayID
# colnames(lf)[7]<-"Rem.pro"#changed from Remains.pro
# mfl <- subset(mfl, select=-c(LfCount1, LfLgth1, LfWdth1, comments,def.ro.tray.or))
# 
# totlf<-merge(lf,mfl, all=TRUE )
# subset(totlf, subset=PopID=="")
# totlf<-totlf[totlf$PopID!="",]
# 
# write.table(totlf, file="ST&MFleaf.txt", sep="\t", quote=F)
# lf<-read.table("ST&MFleaf.txt", header=T, sep="\t", quote='"', row.names=1) 
# mflf<-read.table("MFleafchoicelong.txt", header=T, sep="\t", quote='"', row.names=1) 
# 
# lf[lf$defense=="Induced 1",]$defense <- "ind"
# lf$defense <- droplevels(lf$defense)