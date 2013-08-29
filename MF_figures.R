####MF figures#####
gen<-read.table("ST&MFgenerations.txt", header=T, sep="\t", quote='"', row.names=1) #allo, dk only

##box whisker plot of size trait###
#make dataframe
grdat <- mfcom1[,c(1:12,55:56,58:63)]
grdat <- merge(grdat, mfallo.dk,all=TRUE)
grdat$Trt <- as.factor("Early Control")
colnames(grdat)[10]<-"LfCountH"
grdat$lxwH <- grdat$LfLgth1*grdat$LfWdth1
#colnames(grdat)[24]<-"lxwH"

grdat <- merge(grdat, mfco.dk1, all=TRUE)
grdat <- merge(grdat,mfcu.dk, all=TRUE)
grdat <- merge(grdat,mfn.dk, all=TRUE )
# grdat <- merge(grdat,mfallo.dk, all=TRUE )
# grdat <- merge(grdat,mfcom1, all=TRUE )
# grdat$Trt <- droplevels(grdat$Trt)
# levels(grdat$Trt)
levels(grdat$Trt)[levels(grdat$Trt)=="control"] <- "Control"
levels(grdat$Trt)[levels(grdat$Trt)=="cut"] <- "Herbivory"
levels(grdat$Trt)[levels(grdat$Trt)=="nut def"] <- "Nutrient"
levels(grdat$Trt)[levels(grdat$Trt)=="Nutrient Stress"] <- "Nutr. Stress"

levels(grdat$Origin)[levels(grdat$Origin)=="inv"] <- "Invasive"
levels(grdat$Origin)[levels(grdat$Origin)=="nat"] <- "Native"
grdat$Trt <- factor(grdat$Trt, c("Early Control", "Control", "Nutrient", "Herbivory"))

summary(grdat)
levels(grdat$PopID)
grdat <- grdat[grdat$PopID!="<NA>",]
grdat[grdat$lxwH %in% 0,]$lxwH <- NA


###color plot###
<<<<<<< HEAD
=======

>>>>>>> d457119972c8431de375815e18111cfb0b5b5cd5
pdf("KTurnerFig4.pdf", useDingbats=FALSE, width=13.38)
# png("MFsizebox_color.png", height = 600, width = 600, pointsize = 16)
postscript("KTurnerFig4.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 7, width = 13.38)


p1 <- ggplot(grdat[grdat$Trt!="Herbivory",],aes(Trt, lxwH, fill=Origin))+
  geom_boxplot()+
  xlab("Treatment")+ylab("Approximate area of longest leaf [cm2]")+
  theme_bw()+
  theme(legend.justification=c(1,1), legend.position=c(1,1),
        legend.title = element_text(size=14, face="bold"),legend.text = element_text(size = 13))

p1 <- p1 + annotate('point',x = "Early Control", y = 110, pch=8, color="red",parse=T, size=4)+
  annotate('point',x = "Control", y = 160, pch=8, color="red",parse=T, size=4)+
  annotate('point',x = "Nutr. Stress", y = 110, pch=8, color="red",parse=T, size=4)+
  annotate('point',x = "Nutr. Stress", y = 120, pch=8, color="red",parse=T, size=4)+
  annotate(geom="text", x="Early Control", y=285, label="(a)",fontface="bold", size=5)+
  theme(axis.title.x = element_text(size=15, face="bold", vjust=-0.4), 
        axis.title.y = element_text(size=15, face="bold"),axis.text.x = element_text(size=12 ))

# p1
p2 <- ggplot(grdat, aes(Trt, LfCountH, fill=Origin))+
  geom_boxplot()+xlab("Treatment")+ylab("Number of basal leaves")+
  theme_bw()+
  theme(legend.position="none")
#legend position(left/right,top/bottom)
p2 <- p2 +  annotate('point',x = "Control", y = 30, pch=8, color="red",parse=T, size=4)+
  annotate(geom="text", x="Early Control", y=87.5, label="(b)",fontface="bold", size=5)+
  theme(axis.title.x = element_text(size=15, face="bold", vjust=-0.4), 
        axis.title.y = element_text(size=15, face="bold"),axis.text.x = element_text(size=12 ))

# p2
# multiplot(p1,p2, cols=2) #size only plots

multiplot(p1,p2,p3, cols=3) #all MF plots, see code below for p3 LH trait

dev.off()

# ###b&w plot###
# pdf("MF size box_bw.pdf", useDingbats=FALSE)
# p1 <- ggplot(grdat[grdat$Trt!="Herbivory",],aes(Trt, lxwH, fill=Origin))+theme_bw()+
#   geom_boxplot()+xlab("Stress Treatment")+ylab("Approximate area of longest leaf (cm2)")+
#   theme(legend.justification=c(1,1), legend.position=c(1,1),
#         legend.title = element_text(size=14, face="bold"),legend.text = element_text(size = 13))+
#   scale_fill_manual(values=c("grey51","grey84"))
# p1 <- p1 + annotate('point',x = "Early Control", y = 110, pch=8, color="black",parse=T, size=4)+
#   annotate('point',x = "Control", y = 160, pch=8, color="black",parse=T, size=4)+annotate('point',x = "Control", y = 170, pch=8, color="black",parse=T, size=4)+annotate('point',x = "Control", y = 180, pch=8, color="black",parse=T, size=4)+
#   annotate('point',x = "Nutrient", y = 110, pch=8, color="black",parse=T, size=4)+annotate('point',x = "Nutrient", y = 120, pch=8, color="black",parse=T, size=4)+annotate('point',x = "Nutrient", y = 130, pch=8, color="black",parse=T, size=4)+
#   theme(axis.title.x = element_text(size=15, face="bold", vjust=-0.4), 
#         axis.title.y = element_text(size=15, face="bold"),axis.text.x = element_text(size=12 ))
# # p1
# 
# p2 <- ggplot(grdat, aes(Trt, LfCountH, fill=Origin))+ theme_bw()+
#   geom_boxplot()+xlab("Stress Treatment")+ylab("Number of basal leaves")+
#   theme(legend.position="none")+
# #   scale_fill_manual(values=c("grey51","grey84"))
# #legend position(left/right,top/bottom)
# p2 <- p2 +  annotate('point',x = "Control", y = 30, pch=8, color="black",parse=T, size=4)+
#   theme(axis.title.x = element_text(size=15, face="bold", vjust=-0.4), 
#         axis.title.y = element_text(size=15, face="bold"),axis.text.x = element_text(size=12 ))
# # p2
# 
# multiplot(p1,p2, cols=2)
# dev.off()

#######LH bolted mosaic plot###
#control, cut, nut
grdatB <- merge(mfco.dk1,mfcu.dk, all=TRUE)
grdatB <- merge(grdatB,mfn.dk, all=TRUE )

#grB<- ddply(grdatB, .(Trt, Origin, BoltedatH), summarize, count = length(BoltedatH))

grB2 <- ddply(grdatB, .(Trt, Origin), summarize, totcount = length(BoltedatH))
grB2$xmax <- cumsum(grB2$totcount)
grB2$xmin <- grB2$xmax-grB2$totcount
grB3 <- ddply(grdatB, .(Trt, Origin, BoltedatH), summarize, count = length(BoltedatH))
grB <- merge(grB2,grB3, all.y=TRUE)
grB$Trt <- factor(grB$Trt, c("control","nut def","cut"))
grB$Treatment <- paste(grB$Trt, grB$Origin, grB$BoltedatH)

# grB$xmin <- 0
# grB$xmax <- 96
# grB[1:2,]$xmax<- 16
# grB[3:4,]$xmin<- 16
# grB[3:4,]$xmax<- 32
# grB[5:6,]$xmin<- 32
# grB[5:6,]$xmax<- 48
# grB[7:8,]$xmin<- 48
# grB[7:8,]$xmax<- 64
# grB[9:10,]$xmin<- 64
# grB[9:10,]$xmax<- 80
# grB[11,]$xmin<- 80

#percentages, here for y and n
grBn <- grB[grB$BoltedatH=="n",]
grBn<- ddply(grBn, .(Treatment), transform, ymax = cumsum(count/totcount*100))
grBn <- ddply(grBn, .(Treatment), transform,
                 ymin = ymax-(count/totcount*100))

grBy <- grB[grB$BoltedatH=="y",]
grBy<- ddply(grBy, .(Treatment), transform, ymax = 100)
grBy <- ddply(grBy, .(Treatment), transform,
                 ymin = ymax-cumsum(count/totcount*100))

grBatH1 <- merge(grBn, grBy, all=TRUE)

#labels and tidying
levels(grBatH1$Origin)[levels(grBatH1$Origin)=="inv"] <- "Invasive"
levels(grBatH1$Origin)[levels(grBatH1$Origin)=="nat"] <- "Native"
# grBatH1[grBatH1$xmax==100,]$xmax <- 96
levels(grBatH1$BoltedatH)[levels(grBatH1$BoltedatH)=="n"] <- "Not Bolted"
levels(grBatH1$BoltedatH)[levels(grBatH1$BoltedatH)=="y"] <- "Bolted"
# origins <- c("Invasive", "Native","Invasive", "Native","Invasive", "Native")

# ###color plot###
# col= with( grBatH1, interaction(Origin, Trt, BoltedatH))
# # define a color set with a set value for each of the levels
# colors()[c(552,555,503,506,26,30,563,566,96,99,368,371, 48,51, 494, 497, 468, 471, 590, 618)]
# colorset <- c("chartreuse4","olivedrab4", "darkorchid4","mediumpurple4","steelblue3","royalblue4","chartreuse1","olivedrab1","mediumpurple1","skyblue1","royalblue1","darkorchid1")
# #for order here, going col1 top, bottom, col 2 top, bottom, etc
# #c(2,4,10,12,6,8,1,3,5,7,9) 9 is not used
# # create a special ggplot color scale with your colorset
# cscale = scale_fill_manual(values=colorset)
# 
# # pdf("MF bolted mosaic_color.pdf", useDingbats=FALSE)
# p1 <- ggplot(grBatH1, aes(ymin = ymin, ymax = ymax, xmin=xmin, xmax=xmax, fill=factor(col)))+
#   geom_rect(colour = I("grey"), size=1.5)+
#   scale_x_continuous(breaks=c(55,123,147),labels=c("Control", "Herbivory", "Nutrient"), name="Stress Treatments")+
#   scale_y_continuous(name="Percent Bolted at Harvest")+ cscale
# 
# p1 +theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank())+
#   annotate(geom="text", x=(grBatH1$xmax-grBatH1$xmin)/2 + grBatH1$xmin, y=105, label=grBatH1$Origin, size=5) +
#   annotate(geom="text", x=(grBatH1$xmax-grBatH1$xmin)/2 + grBatH1$xmin, y=grBatH1$ymin+2, label=grBatH1$BoltedatH, size=4)+ 
#   theme(legend.position="none", axis.title.x = element_text(size=15, face="bold", vjust=-0.4), 
#         axis.title.y = element_text(size=15, face="bold"),axis.text.x = element_text(size=15 ))+ 
#   annotate('point',x = 55, y = 102, pch=8, color="red",parse=T, size=3)+
#   annotate('point',x = 123, y = 102, pch=8, color="red",parse=T, size=3)
# dev.off()
# 
# ###b&w plot###
# col= with( grBatH1, interaction(Origin, Trt, BoltedatH))
# 
# colors()[c(312,336,350,366,1,176)]
# colorset <- c("grey51","grey84", "grey51","grey84", "grey51","grey84","white","white","white","white","white","white")
# cscale = scale_fill_manual(values=colorset)
# 
# # pdf("MF bolted mosaic_bw.pdf", useDingbats=FALSE)
# p1 <- ggplot(grBatH1, aes(ymin = ymin, ymax = ymax, xmin=xmin, xmax=xmax, fill=factor(col)))+
#   geom_rect(colour = I("grey"), size=1.5)+
#   scale_x_continuous(breaks=c(55,123,147),labels=c("Control", "Herbivory", "Nutrient"), name="Stress Treatments")+
#   scale_y_continuous(name="Percent Bolted at Harvest")+ theme_bw()+cscale
# 
# p1 +theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank())+
#   annotate(geom="text", x=(grBatH1$xmax-grBatH1$xmin)/2 + grBatH1$xmin, y=105, label=grBatH1$Origin, size=5) +
#   annotate(geom="text", x=(grBatH1$xmax-grBatH1$xmin)/2 + grBatH1$xmin, y=grBatH1$ymin+2, label=grBatH1$BoltedatH, size=4)+ 
#   theme(legend.position="none", axis.title.x = element_text(size=15, face="bold", vjust=-0.4), 
#         axis.title.y = element_text(size=15, face="bold"),axis.text.x = element_text(size=15 ))+ 
#   annotate('point',x = 55, y = 102, pch=8, color="black",parse=T, size=3)+
#   annotate('point',x = 123, y = 102, pch=8, color="black",parse=T, size=3)
# dev.off()
# 
# #####black and white mosaic, col width standard#####
# col= with( grBatH1, interaction(Origin, Trt, BoltedatH))
# 
# colors()[c(312,336,350,366,1,176)]
# colorset <- c("white","white","white","white","white","white","grey51","grey84", "grey51", "grey51","grey84","grey51")
# cscale = scale_fill_manual(values=colorset)
# 
# grBatHStd <- grBatH1
# grBatHStd$xmin <- c(0,0,20,20,80,80,100,40,40,60,60)
# grBatHStd$xmax <- grBatHStd$xmin + 20
# #reverse stacking, not bolted comes out as white?
# grBatHStd$RevStackymax  <-  grBatHStd$ymax - grBatHStd$ymin
# grBatHStd[grBatHStd$BoltedatH=="Not Bolted",]$RevStackymax  <-  100
# grBatHStd$RevStackymin <- grBatHStd$RevStackymax-grBatHStd$ymax
# grBatHStd[grBatHStd$RevStackymin<0,]$RevStackymin <- 0
# 
# # pdf("MF bolted mosaic_bw.pdf", useDingbats=FALSE)
# 
# p1 <- ggplot(grBatHStd, aes(ymin = RevStackymin, ymax = RevStackymax, xmin=xmin, xmax=xmax, fill=factor(col)))+
#   geom_rect(colour = I("black"))+
#   scale_x_continuous(breaks=c(20,60,100),labels=c("Control", "Herbivory", "Nutrient"), name="Stress Treatments") +
#   scale_y_continuous(name="Percent Bolted at Harvest")+
#   #scale_color_manual(breaks=levels(grBatHStd$Origin))+ 
#   theme_bw()+cscale
# p1
# # annotate 
# p1 + theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank())+
#   annotate(geom="text", x=(grBatHStd$xmax-grBatHStd$xmin)/2 + grBatHStd$xmin, y=105, label=grBatHStd$Origin, size=4) +
#   #annotate(geom="text", x=(grBatHStd$xmax-grBatHStd$xmin)/2 + grBatHStd$xmin, y=grBatHStd$ymin+2, label=grBatHStd$BoltedatH, size=4)+
# #   theme(legend.justification=c(1,1), legend.position=c(1,1),
# #         legend.title = element_text(size=14, face="bold"),legend.text = element_text(size = 13),
# #         axis.title.x = element_text(size=15, face="bold", vjust=-0.4), 
# #         axis.title.y = element_text(size=15, face="bold"),axis.text.x = element_text(size=15 ))+
#   theme(legend.position="none", axis.title.x = element_text(size=15, face="bold", vjust=-0.4), 
#         axis.title.y = element_text(size=15, face="bold"),axis.text.x = element_text(size=15 ))+ 
#   annotate('point',x = 20, y = 102, pch=8, color="black",parse=T, size=3)+annotate('point',x = 23, y = 102, pch=8, color="black",parse=T, size=3)+annotate('point',x = 17, y = 102, pch=8, color="black",parse=T, size=3)+
#   annotate('point',x = 58, y = 102, pch=8, color="black",parse=T, size=3) +annotate('point',x = 62, y = 102, pch=8, color="black",parse=T, size=3)
# 
# dev.off()

####color mosaic, rev stack, std col width####
col= with( grBatH1, interaction(Origin, Trt, BoltedatH))
colorset <- c("white","white","white","white","white","white","#F8766D","#00BFC4", "#F8766D", "#F8766D","#00BFC4","#F8766D")
cscale = scale_fill_manual(values=colorset)

grBatHStd <- grBatH1
grBatHStd$xmin <- c(0,0,20,20,80,80,100,40,40,60,60)
grBatHStd$xmax <- grBatHStd$xmin + 20
#reverse stacking, not bolted comes out as white?
grBatHStd$RevStackymax  <-  grBatHStd$ymax - grBatHStd$ymin
grBatHStd[grBatHStd$BoltedatH=="Not Bolted",]$RevStackymax  <-  100
grBatHStd$RevStackymin <- grBatHStd$RevStackymax-grBatHStd$ymax
grBatHStd[grBatHStd$RevStackymin<0,]$RevStackymin <- 0



# pdf("MFboltedmosaic_color.pdf", useDingbats=FALSE)
png("MFboltedmosaic_color.png", height = 600, width = 600, pointsize = 16)

p3 <- ggplot(grBatHStd, aes(ymin = RevStackymin, ymax = RevStackymax, xmin=xmin, xmax=xmax, fill=factor(col))) +
  geom_rect(colour = I("white"))+
<<<<<<< HEAD
  scale_x_continuous(breaks=c(20,60,100),labels=c("Control", "Herbivory", "Nutr. Stress"), name="Treatment") +
=======
  scale_x_continuous(breaks=c(20,60,100),labels=c("Control", "Herbivory", "Nutr. Stress"), name="Treatments") +
>>>>>>> d457119972c8431de375815e18111cfb0b5b5cd5
  scale_y_continuous(name="Percent Bolted at Harvest")+
  theme_bw()+cscale
# p3
# annotate 
p3 <- p3 + theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank())+
#   annotate(geom="text", x=(grBatHStd$xmax-grBatHStd$xmin)/2 + grBatHStd$xmin, y=105, label=grBatHStd$Origin, size=4) +
  theme(legend.position="none", axis.title.x = element_text(size=15, face="bold", vjust=-0.4), 
        axis.title.y = element_text(size=15, face="bold"),axis.text.x = element_text(size=12 ))+ 
  annotate('point',x = 20, y = 70, pch=8, color="red",parse=T, size=4)+
  annotate('point',x = 25, y = 70, pch=8, color="red",parse=T, size=4)+
  annotate('point',x = 15, y = 70, pch=8, color="red",parse=T, size=4)+
  annotate('point',x = 57.5, y = 70, pch=8, color="red",parse=T, size=4) +
  annotate('point',x = 62.5, y = 70, pch=8, color="red",parse=T, size=4)+
  annotate(geom="text", x=2.5, y=98, label="(c)",fontface="bold", size=5)

p3

dev.off()


#########generation effects###########
#lfcount
graphdata <- gen[!is.na(gen$LfCountH),]
graphdata$Generation <- as.factor(graphdata$Generation)
# 
# ggplot(graphdata, aes(x = Generation, y = LfCountH, group = Origin, color = Origin)) + geom_line(data=graphdata, aes(group=Origin))
# ggplot(graphdata, aes(x = Generation, y = LfCountH, color = Origin)) + stat_summary(fun.y=mean)+geom_line( aes(x=as.numeric(Generation), y=LfCountH))
# #geom_path()+theme_bw(
# 
# lfmean <- tapply(graphdata$LfCountH,graphdata$Origin, mean)
# ggplot(graphdata, aes(x = Generation, y = lfmean, group = Origin, color = Origin)) + geom_point() +geom_line(data=graphdata, aes(group=Origin))
# # ggplot(d, aes(x = hp, y = mpg)) +
# #   geom_point() +
# #   geom_line(aes(x = x, y = y, colour = g))
# 
# fake <- data.frame(or=c("i","n","i","n","i","n"), gen=c(0,0,1,1,0,1), lf=c(17,13,20,16,2,2))
# # fake <- data.frame(or=c("i","n","i","n"), gen=c(0,0,1,1), lf=c(17,13,20,16))
# # fake$lf <- fake$lf + 1
# 
# fake$or <- as.factor(fake$or)
# fake$gen <- as.factor(fake$gen)
# fake$lf <- as.integer(fake$lf)
# fakeI <- fake[fake$Origin=="i",]
# fakeN <- fake[fake$Origin=="n",]
# #lfmean <- tapply(fake$lf,fake$or, mean)
# ggplot(fakeI, aes(x = gen, y = lf, color = "red")) +geom_path( aes(x=as.numeric(gen), y=lf))
# qplot(gen, lf, data = fake, colour = or,
#       stat = "summary", fun.y = "mean", geom = "line")

# If you just want to represent one value for each gen and or combination then it would be easier to calculate mean values before plotting. 
# That can be achieved with function ddply() from library plyr.

library(plyr)    
# fake.sum<-ddply(fake,.(or,gen), summarize, lf = mean(lf))
# fake.sum
# 
# # Now with this new data frame you just have to give x and y values. 
# # or should be used in colour= and group= to ensure that each group has different color and that lines are connected.
# 
# ggplot(fake.sum,aes(x=gen,y=lf,colour=or,group=or))+
#   geom_point()+geom_line()
# 
# se <- function(x) sqrt(var(x, na.rm=TRUE)/(length(na.omit(x))-1))
# graphdata.sum <- ddply(graphdata, .(Origin, Generation),summarize, LfCountH = mean(LfCountH))
# 
# se(graphdata$LfCountH)
# tapply(graphdata$LfCountH,graphdata$, max)
# 
# 
# graphdata.sum
# 
# graphdata.sum$Generation <- as.factor(graphdata.sum$Generation)
# #mean:inv, gen0 is intercept, inv gen1 int+gen, nat gen0 int-nat, nat gen0 int-nat+gen
# 
# ggplot(graphdata.sum, aes(x=Generation, y=LfCountH, color=Origin, group=Origin))+geom_line()


#make graph dataframe
# #####IDK, maybe???cls mean +/- 1.96*sqrt(mean/count)
# #mean:inv, gen0 : intercept, inv gen1: int+gen, nat gen0: int-nat, nat gen1: int-nat:gen
# #SEs: inv gen0: int +SE, 
# #     inv gen1:int+gen+gen1SE, 
# #     nat gen0:int-nat+natSE, 
# #     nat gen1:int-nat:gen+nat:genSE
# #SE
# int<-2.75258
# #inv mean
# B<--0.36134-0.09134
# #Originnat estimate from model summary
# pI<-exp(int)
# pN<-exp(int+B)
# pI
# pN
# #from modelInt:LfCountH ~ Origin * Generation + (1 | PopID/MomFam) , poisson
# # Fixed effects:
# #                       Estimate Std. Error z value Pr(>|z|)    
# # (Intercept)            2.75258    0.10115  27.213  < 2e-16 ***
# #   Originnat            -0.24940    0.14393  -1.733   0.0831 .  
# # Generation1            0.29199    0.05519   5.290 1.22e-07 ***
# # Originnat:Generation1 -0.36134    0.09134  -3.956 7.62e-05 ***
# 
# genlf <- data.frame(Origin=c("Invasive","Invasive","Native","Native"), Generation=c(0,1,0,1), lfmean=c(15.68304,21.001,12.2213,10.92704), 
#                     uSE=c(17.35239,22.19262,14.11319, 11.97211),lSE=c(14.17429, 19.87336, 10.58301,9.973185) )
# genlf <- merge(genlf, ddply(graphdata, .(Generation, Origin), summarize, count = length(Origin)), all.x=TRUE)

#or from lsmeans func 
modeldata<-gen[!is.na(gen$LfCountH),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$MomFam<-as.factor(modeldata$MomFam)
modeldata$Generation <- as.factor(modeldata$Generation)

model1<-lmer(LfCountH  ~ Origin* Generation  +(Origin|PopID/MomFam), family=poisson,data=modeldata)

genlf <- CI.LS.poisson.2term(model1, conf=95)

# #use numbers to make table
# genlf <- data.frame(Origin=c("Native","Invasive","Native","Invasive"), Generation=c(0,0,1,1), 
#                     lfmean=c( ), 
#                     uCL=c(),lCL=c() )

###color plot
# pdf("STMFGenleaf_color.pdf", useDingbats=FALSE)
png("STMFGenleaf_color.png", height = 600, width = 800, pointsize = 16)

p1 <- ggplot(genlf, aes(x=Generation, y=mean, color=Origin, group=Origin)) +
  geom_errorbar(aes(ymin=lCL, ymax=uCL),color="black", width=.1, position=position_dodge(0.1))+
  geom_line(position=position_dodge(0.1))+geom_point(size=5, position=position_dodge(0.1))+
  ylab("Mean Number of Basal Leaves")+
  scale_x_continuous(breaks=seq(0,1,1), name="Generation")+ 
  ggtitle("Number of basal leaves at harvest\nCross-generational analysis") + 
  theme_bw()+
  theme(plot.title = element_text(lineheight=.8, face="bold"),legend.justification=c(0,1), legend.position=c(0,1),
        legend.title = element_text(size=14, face="bold"),legend.text = element_text(size = 13),
        axis.title.x = element_text(size=15, face="bold", vjust=-0.4), 
        axis.title.y = element_text(size=15, face="bold"),axis.text.x = element_text(size=12 ))
# p1
p1 <- p1 + annotate(geom="text", x=0, y=20.2, label="Broad CG", fontface="italic",size=5) +
  annotate(geom="text", x=0, y=19.4, label="Origin", fontface="italic",size=5) +
  annotate('point',x = -0.04, y = 18.6, pch=8, color="red",parse=T,size=4)+
  annotate('point',x = 0, y = 18.6, pch=8, color="red",parse=T,size=4)+
  annotate('point',x = 0.04, y = 18.6, pch=8, color="red",parse=T,size=4)+
  
  annotate('point',x = 0.95, y = 15, pch=8, color="red",parse=T,size=4)+
  annotate(geom="text", x=0.95, y=16.6, label="Origin",fontface="italic", size=5) +
  annotate(geom="text", x=0.95, y=15.8, label="Maternal CG", fontface="italic",size=5) +
  
  annotate('point',x = 0.50, y = 16, pch=8, color="red",parse=T,size=4)+
  annotate(geom="text", x=0.5, y=16.8, label="Origin", size=5)+ 
  annotate(geom="text", x=0.5, y=15.2, label="Origin*Generation", size=5)+
  annotate('point',x = 0.5, y = 14.4, pch=8, color="red",parse=T,size=4)+
  annotate('point',x = 0.54, y = 14.4, pch=8, color="red",parse=T,size=4)+
  annotate('point',x = 0.46, y = 14.4, pch=8, color="red",parse=T,size=4)+
  
  annotate(geom="text", x=-0.05, y=8.75, label="(a)",fontface="bold", size=5)

p1
dev.off()

###plot in bw####
# pdf("STMF Gen leaf_bw.pdf", useDingbats=FALSE)
# p1 <- ggplot(genlf, aes(x=Generation, y=lfmean, group=Origin))+theme_bw()+
#   geom_errorbar(aes(ymin=lSE, ymax=uSE),color="black", width=.1)+
#   geom_line()+geom_point(aes(shape=Origin),size=5)+ylab("Mean Number of Basal Leaves")+
#   scale_x_continuous(breaks=seq(0,1,1), name="Generation")+ 
#   ggtitle("Number of basal leaves\nCross-generational analysis") + theme(plot.title = element_text(lineheight=.8, face="bold"))+
#   theme(legend.justification=c(0,1), legend.position=c(0,1),
#         legend.title = element_text(size=14, face="bold"),legend.text = element_text(size = 13),
#         axis.title.x = element_text(size=15, face="bold", vjust=-0.4), 
#         axis.title.y = element_text(size=15, face="bold"),axis.text.x = element_text(size=15 ))+
#   scale_linetype_manual(values=c("grey51","grey84"))
# p1 + annotate(geom="text", x=0, y=9.7, label="Full data set", fontface="italic",size=5) +
#   annotate(geom="text", x=0, y=10.3, label="Origin", fontface="italic",size=5) +
#   annotate('point',x = -0.02, y = 9.1, pch=8, color="black",parse=T,size=3)+
#   annotate('point',x = 0.02, y = 9.1, pch=8, color="black",parse=T,size=3)+
#   annotate('point',x = 1, y = 16, pch=8, color="black",parse=T,size=3)+
#   annotate(geom="text", x=1, y=16.6, label="Origin",fontface="italic", size=5) +
#   annotate('point',x = 0.48, y = 16, pch=8, color="black",parse=T,size=3)+
#   annotate('point',x = 0.52, y = 16, pch=8, color="black",parse=T,size=3)+
#   annotate(geom="text", x=0.5, y=16.6, label="Origin", size=5)+ 
#   annotate(geom="text", x=0.5, y=15.5, label="Origin*Generation", size=5)+
#   annotate('point',x = 0.5, y = 14.9, pch=8, color="black",parse=T,size=3)+
#   annotate('point',x = 0.54, y = 14.9, pch=8, color="black",parse=T,size=3)+
#   annotate('point',x = 0.46, y = 14.9, pch=8, color="black",parse=T,size=3)
# dev.off()


# intplot +geom_boxplot(graphdata,aes(x=Generation, y=LfCountH, color=Origin, group=Origin))
# ggplot(graphdata, aes(x=Generation, y=LfCountH, color=Origin, group=Origin))+geom_boxplot()
#+geom_boxplot(group=Generation)

#int plot with SEs
# #qplot(Session, DEs2mPre, data = Dummy.Data, colour = Drug, facets = Group~.,
# stat = "summary", fun.y = "mean", geom = "line")
# # ggplot(data1, aes(x=group, y=estimate)) + 
#   geom_errorbar(aes(ymin=estimate-SE, ymax=estimate+SE), 
#                 colour="black", width=.1, position=pd) +
#   geom_line( aes(x=as.numeric(group), y=estimate)) + 
#   geom_point(position=pd, size=4)

#jitter in boxplots
# dat <- data.frame(group=c('a', 'b', 'c'), values = runif(90))
# 
# ggplot(dat, aes(group, values)) + 
#   geom_boxplot(outlier.size = 0) + 
#   geom_jitter(position=position_jitter(width=0), aes(colour=group), alpha=0.7) + 
#   ylim(0, 1) + stat_summary(fun.y=mean, shape=3, col='red', geom='point') +
#   opts(legend.position = "right") + ylab("values") + xlab("group")
# 
# interaction.plot(graphdata$Generation, graphdata$Origin, graphdata$LfCountH, fun=mean,main = "Interaction plot of number of basal leaves at harvest")
# interaction.plot(fake$gen, fake$or, fake$lf, fun=mean)

# thedata <- data.frame(predict(thelm), thelm$model$x, thelm$model$f)
# y = trait, x = origin, f=generation
# ggplot(thedata, aes(x = x, y = yhat, group = f, color = f)) + geom_line()



###bolt date####
#base graphics
graphdata <- gen[!is.na(gen$BoltDay.adj),]
summary(graphdata)
interaction.plot(graphdata$Generation, graphdata$Origin, graphdata$BoltDay.adj, fun=mean, main = "Interaction plot of bolting date")

#make graphdata
# #BoltDay.adj ~ Origin * Generation + (1 | PopID/MomFam), poisson
# # Fixed effects:
# #   Estimate Std. Error z value Pr(>|z|)    
# # (Intercept)           4.29826    0.10253   41.92  < 2e-16 ***
# #   Originnat           0.02222    0.11812    0.19  0.85081    
# # Generation           -0.04531    0.08147   -0.56  0.57812    
# # Originnat:Generation -0.27966    0.09620   -2.91  0.00365 ** 
# 
# #mean:inv, gen0 : intercept, inv gen1: int+gen, nat gen0: int-nat, nat gen1: int-nat:gen
# #SEs: inv gen0: int +SE, 
# #     inv gen1:int+gen+gen1SE, 
# #     nat gen0:int-nat+natSE, 
# #     nat gen1:int-nat:gen+nat:genSE
# #SE
# int<-4.298261-0.04531-0.08147#inv mean
# B<-0.02222+0.11812#Originnat estimate from model summary
# pI<-exp(int)
# pN<-exp(int+B)
# pI
# pN
# 
# genb <- data.frame(Origin=c("Invasive","Invasive","Native","Native"), Generation=c(0,1,0,1), boltmean=c(73.57167,70.31253,75.22473,56.87302), 
#                     uSE=c(81.51532,76.28078,84.65642,61.24015),lSE=c(66.40225,64.81137,66.84396,50.5215) )
#genb <- merge(genb, ddply(graphdata, .(Generation, Origin), summarize, count = length(Origin)), all.x=TRUE)

#or use lsmeans
modeldata<-gen[!is.na(gen$BoltDay.adj),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$MomFam<-as.factor(modeldata$MomFam)
modeldata$Generation<-as.factor(modeldata$Generation)

model1<-lmer(BoltDay.adj  ~ Origin* Generation  +(1|PopID/MomFam), family=poisson,data=modeldata)

genb <- CI.LS.poisson.2term(model1, conf=95)

# pd <- position_dodge(.1)

###color plot###
# pdf("KTurnerFig5.pdf", useDingbats=FALSE, width=6.29, height=11)
postscript("KTurnerFig5.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 11, width = 6.29)
# png("STMFGenboltdate_color.png", height = 600, width = 600, pointsize = 16)

p2 <- ggplot(genb, aes(x=Generation, y=mean, color=Origin, group=Origin, ymax=90))+
  geom_errorbar(aes(ymin=lCL, ymax=uCL),color="black", width=.1, position=position_dodge(0.1))+
  geom_line(position=position_dodge(0.1))+
  geom_point(size=5, position=position_dodge(0.1))+
  ylab("Mean Bolt Date")+
  scale_x_continuous(breaks=seq(0,1,1), name="Generation")+ 
  ggtitle("Bolt Date\nCross-generational analysis") + 
  theme_bw()+
  theme(plot.title = element_text(lineheight=.8, face="bold"),legend.position="none",
        axis.title.x = element_text(size=15, face="bold", vjust=-0.4), 
        axis.title.y = element_text(size=15, face="bold"),axis.text.x = element_text(size=12 ))
# legend.justification=c(1,1), legend.position=c(1,1),
# legend.title = element_text(size=14, face="bold"),legend.text = element_text(size = 13)

p2 <- p2 +  annotate('point',x = 0.95, y = 85, pch=16, color="red",parse=T,size=4)+
  annotate(geom="text", x=0.95, y=89, label="Origin",fontface="italic", size=5) +
  annotate(geom="text", x=0.95, y=87, label="Maternal CG",fontface="italic", size=5)+
  
  annotate(geom="text", x=0, y=58, label="Origin",fontface="italic", size=5)+
  annotate(geom="text", x=0, y=54, label="NS",fontface="italic", size=5)+
  annotate(geom="text", x=0, y=56, label="Broad CG", fontface="italic",size=5)+
  
  annotate('point',x = 0.5, y = 78, pch=16, color="red",parse=T,size=4)+
  annotate(geom="text", x=0.5, y=80, label="Origin", size=5)+ 
  annotate(geom="text", x=0.5, y=76, label="Origin*Generation", size=5)+
  annotate('point',x = 0.48, y = 74, pch=8, color="red",parse=T,size=4)+
  annotate('point',x = 0.52, y = 74, pch=8, color="red",parse=T,size=4)+
  
  annotate(geom="text", x=-0.05, y=50, label="(b)",fontface="bold", size=5)

p2
multiplot(p1,p2, cols=1)
dev.off()

# ###bw plot###
# pdf("STMF Gen boltdate_bw.pdf", useDingbats=FALSE)
# p1 <- ggplot(genb, aes(x=Generation, y=boltmean, group=Origin, ymax=90))+theme_bw()+
#   geom_errorbar(aes(ymin=lSE, ymax=uSE),color="black", width=.1, position=pd)+
#   geom_line(position=pd)+geom_point(aes(shape=Origin),size=5, position=pd)+ylab("Mean Bolt Date")+
#   scale_x_continuous(breaks=seq(0,1,1), name="Generation")+ 
#   ggtitle("Bolt Date\nCross-generational analysis") + theme(plot.title = element_text(lineheight=.8, face="bold"))+
#   theme(legend.justification=c(1,1), legend.position=c(1,1),
#         legend.title = element_text(size=14, face="bold"),legend.text = element_text(size = 13),
#         axis.title.x = element_text(size=15, face="bold", vjust=-0.4), 
#         axis.title.y = element_text(size=15, face="bold"),axis.text.x = element_text(size=15 ))
# p1 +  annotate('point',x = 1, y = 80, pch=16, color="black",parse=T,size=3)+
#   annotate(geom="text", x=1, y=82, label="Origin",fontface="italic", size=5) +
#   annotate(geom="text", x=0, y=60, label="Origin",fontface="italic", size=5)+
#   annotate(geom="text", x=0, y=58, label="NS",fontface="italic", size=5)+
#   annotate('point',x = 0.5, y = 78, pch=16, color="black",parse=T,size=3)+
#   annotate(geom="text", x=0.5, y=80, label="Origin", size=5)+ 
#   annotate(geom="text", x=0.5, y=76, label="Origin*Generation", size=5)+
#   annotate('point',x = 0.48, y = 74, pch=8, color="black",parse=T,size=3)+
#   annotate('point',x = 0.52, y = 74, pch=8, color="black",parse=T,size=3)+
#   annotate(geom="text", x=0, y=56, label="Full data set", fontface="italic",size=5)
# dev.off()

####bolted at H, crossgen###########

#or use lsmeans
modeldata<-gen[!is.na(gen$bolt.bin),]
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
modeldata$MomFam<-as.factor(modeldata$MomFam)
modeldata$Generation<-as.factor(modeldata$Generation)

model1<-glm(bolt.bin  ~ Origin+ Generation, family=binomial,data=modeldata)

genbatH <- CI.LS.binomial.2term(model1, conf=95)
genbatH$mean <- genbatH$mean*100
genbatH$uCL <- genbatH$uCL*100
genbatH$lCL <- genbatH$lCL*100

# ###color plot###
# pdf("STMFGenbatH_color.pdf", useDingbats=FALSE)
png("STMFGenbatH_color.png", height = 600, width = 800, pointsize = 16)

p1 <- ggplot(genbatH, aes(x=Generation, y=mean, color=Origin, group=Origin, ymax=90))+
  geom_errorbar(aes(ymin=lCL, ymax=uCL),color="black", width=.1, 
                position=position_dodge(0.1))+
  geom_line(position=position_dodge(0.1))+
  geom_point(size=5, position=position_dodge(0.1))+
  ylab("Percent bolted at harvest")+
  scale_x_continuous(breaks=seq(0,1,1), name="Generation")+ 
  ggtitle("Bolting Status\nCross-generational analysis") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"),
        legend.justification=c(0,1), legend.position=c(0,1),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size = 13),
        axis.title.x = element_text(size=15, face="bold", vjust=-0.4), 
        axis.title.y = element_text(size=15, face="bold"),
        axis.text.x = element_text(size=15 ))
# p1
p1 +  annotate('point',x = 1, y = 33, pch=8, color="red",parse=T,size=3)+
  annotate(geom="text", x=1, y=42, label="Origin",fontface="italic", size=5) +
  annotate(geom="text", x=1, y=37.5, label="Maternal CG",fontface="italic", size=5)+
  
  annotate(geom="text", x=0, y=29.5, label="Origin",fontface="italic", size=5)+
  annotate(geom="point", x=0, y=20.5, pch=8, color="red",parse=T,size=3)+
  annotate(geom="point", x=0.05, y=20.5, pch=8, color="red",parse=T,size=3)+
  annotate(geom="point", x=-0.05, y=20.5, pch=8, color="red",parse=T,size=3)+
  annotate(geom="text", x=0, y=25, label="Broad CG", fontface="italic",size=5)+
  
  annotate('point',x = 0.5, y = 42, pch=8, color="red",parse=T,size=3)+
  annotate('point',x = 0.55, y = 42, pch=8, color="red",parse=T,size=3)+
  annotate('point',x = 0.45, y = 42, pch=8, color="red",parse=T,size=3)+
  annotate(geom="text", x=0.5, y=46.5, label="Origin", size=5)+ 
  annotate(geom="text", x=0.5, y=37.5, label="Origin*Generation", size=5)+
  annotate(geom="text", x=0.5, y=33, label="NS",fontface="italic", size=5)
  

dev.off()