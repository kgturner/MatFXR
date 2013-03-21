###mat fx graphs

###
plot(mfco.dk$PopID, mfco.dk$ShootMass.g, col=mfco.dk$Origin)

mfmom.dk$PopID<-droplevels(mfmom.dk$PopID)
mfmom.dk$PopID<-factor(mfmom.dk$PopID, levels=c("BG001" , "GR002", "RU008", "TR001","CA001", "US001", "US002", "US003"))
# plot(mfmom.dk$PopID, mfmom.dk$GermPercent, col=as.numeric(mfmom$Origin))
# plot(mfmom.dk$Origin, mfmom.dk$GermPercent, col=mfmom$PopID)
qplot(PopID, GermPercent, data = mfmom.dk, geom="boxplot", col=Origin)


# levels(mfco.dk1$PopID)<-c("BG001" , "GR002", "RU008", "TR001","CA001", "US001", "US002", "US003")
mfco.dk1$PopID <- factor(mfco.dk1$PopID, levels = c("BG001" , "GR002", "RU008", "TR001","CA001", "US001", "US002", "US003"))
# mfco.dk1$PopID<-reorder(mfco.dk1$PopID, as.factor(c("BG001","GR002", "RU008", "TR001","CA001", "US001", "US002", "US003")))
mfco.dk1$lxwH<-mfco.dk1$LfLgthH*mfco.dk1$LfWdthH
# plot(mfco.dk1$PopID, mfco.dk1$lxwH, col=mfco.dk1$Origin)
# boxplot(lxwH~PopID, data=mfco.dk1, col=as.numeric(mfco.dk1$Origin))

library(ggplot2)

qplot(PopID, lxwH, data = mfco.dk1, geom="boxplot", col=as.numeric(mfco.dk1$Origin))

plotdata<-mfco.dk1[mfco.dk1$BoltedatH=="n",]
qplot(PopID, LfCountH, data = plotdata, geom="boxplot", col=Origin)

plot(mfco.dk1$Origin,mfco.dk1$BoltDay,  ylab="Bolted by harvest?", xlab="Origin", 
     main="Proportion bolted at harvest")
plot(mfco.dk1$PopID,mfco.dk1$BoltedatH,  ylab="Bolted by harvest?", xlab="Origin", 
     main="Proportion bolted at harvest", col=mfco.dk1$Origin)
mosaicplot(~ PopID + BoltedatH, data = mfco.dk1, color = TRUE)
plot(mfco.dk1$PopID, mfco.dk1$BoltDay)
plot(mfco.dk1$PopID, mfco.dk1$CrownDiam.mm)

plotdata<-mfn.dk[mfn.dk$BoltedatH=="n",]
qplot(PopID, CrownDiam.mm, data = mfco.dk1, geom="boxplot", col=Origin)

str(mfm1.dk)
qplot(PopID, lxw, data = mfm1.dk, geom="boxplot", col=Origin)

str(mfco.dk1)
mfco.dk1$PopID<-droplevels(mfco.dk1$PopID)
mfco.dk1$PopID<-factor(mfco.dk1$PopID, levels=c("BG001" , "GR002", "RU008", "TR001","CA001", "US001", "US002", "US003"))
min(mfco.dk1$BoltDay)
qplot(PopID, BoltDay, data = mfco.dk1, geom="boxplot", col=Origin)

plot(mfco.dk1$SLAarea, mfco.dk1$SLAmass.g, col=mfco.dk1$Origin)
# abline(lm(formula=SLAarea~SLAmass.g, data=mfco.dk1), col="green")
# lmsla<-glm(SLAarea~Origin+SLAmass.g, data=mfco.dk1, family=gaussian)


#tiled trait distribution graphs in ggplot2
mfco.dk1$PopID <- factor(mfco.dk1$PopID, levels = c("BG001" , "GR002", "RU008", "TR001","CA001", "US001", "US002", "US003"))
mfallo.dk$PopID <- factor(mfallo.dk$PopID, levels = c("BG001" , "GR002", "RU008", "TR001","CA001", "US001", "US002", "US003"))
mfm1.dk$PopID <- factor(mfm1.dk$PopID, levels = c("BG001" , "GR002", "RU008", "TR001","CA001", "US001", "US002", "US003"))
mfmom.dk$PopID <- factor(mfmom.dk$PopID, levels = c("BG001" , "GR002", "RU008", "TR001","CA001", "US001", "US002", "US003"))

#par(mfrow=c(2,3))
p1<-qplot(PopID, GermPercent, data = mfmom.dk, col=Origin, size = I(6))
# qplot(PopID, GermPercent, data = mfmom.dk, col=Origin, geom="boxplot")
p2<-qplot(PopID, Mass.gA, data = mfallo.dk, geom="boxplot", col=Origin)
p3<-qplot(PopID, lxw, data = mfm1.dk, geom="boxplot", col=Origin,)
p4<-qplot(PopID, LfCountH, data = mfco.dk1, geom="boxplot", col=Origin)
# qplot(PopID, BoltedatH, data = mfco.dk1, geom="histogram",col=Origin)
# ggplot(mfco.dk1, aes(x=PopID, fill=BoltedatH)) + geom_histogram(binwidth=.5, alpha=.5, position="identity")
p5<-mosaicplot(~ PopID + BoltedatH, data = mfco.dk1, color = TRUE)
p6<-qplot(PopID, SLAarea, data = mfco.dk1, geom="boxplot", col=Origin)

multiplot(p1, p6, p2, p4, p3, p5, cols=3)


#checking normality
mfcom1$lxw<-mfcom1$LfLgth1*mfcom1$LfWdth1
histogram(mfcom1$lxw)
qqnorm(mfcom1$lxw)
qqline(mfcom1$lxw)


histogram(mfallo.dk$Mass.gA)
qqnorm(mfallo.dk$Mass.gA)
qqline(mfallo.dk$Mass.gA)

histogram(mfco.dk1$ShootMass.g)
qqnorm(mfco.dk1$ShootMass.g)
qqline(mfco.dk1$ShootMass.g)








#####################
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

####combined lfchoice###
modeldata<-lf[!is.na(lf$Eaten.mm),]
modeldata<-modeldata[modeldata$eat.bin<2,]#exclude ties and failed trials
is.na(modeldata$Eaten.mm)
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
plot(modeldata$Eaten.mm)
ag <- aggregate(Eaten.mm ~ defense, data= modeldata, FUN = "mean")
plot(ag)

# p <- tapply(modeldata$Eaten.mm,modeldata$defense, mean)
# plot(p, xlab=levels(modeldata$defense))

#mf lf choice, double check trend.... huh
modeldata<-mflf[!is.na(mflf$Eaten.mm),]
modeldata<-modeldata[modeldata$eat.bin<2,]#exclude ties and failed trials
is.na(modeldata$Eaten.mm)
modeldata$blank<-1
modeldata$blank<-as.factor(modeldata$blank)
ag2 <- aggregate(Eaten.mm ~ defense, data= modeldata, FUN = "mean")
plot(ag2)
##################
#generation effects
graphdata <- gen[!is.na(gen$LfCountH),]
graphdata$Generation <- as.factor(graphdata$Generation)

ggplot(graphdata, aes(x = Generation, y = LfCountH, group = Origin, color = Origin)) + geom_line(data=graphdata, aes(group=Origin))
ggplot(graphdata, aes(x = Generation, y = LfCountH, color = Origin)) + stat_summary(fun.y=mean)+geom_line( aes(x=as.numeric(Generation), y=LfCountH))
#geom_path()+theme_bw(

lfmean <- tapply(graphdata$LfCountH,graphdata$Origin, mean)
ggplot(graphdata, aes(x = Generation, y = lfmean, group = Origin, color = Origin)) + geom_point() +geom_line(data=graphdata, aes(group=Origin))
# ggplot(d, aes(x = hp, y = mpg)) +
#   geom_point() +
#   geom_line(aes(x = x, y = y, colour = g))

fake <- data.frame(or=c("i","n","i","n","i","n"), gen=c(0,0,1,1,0,1), lf=c(17,13,20,16,2,2))
# fake <- data.frame(or=c("i","n","i","n"), gen=c(0,0,1,1), lf=c(17,13,20,16))
# fake$lf <- fake$lf + 1

fake$or <- as.factor(fake$or)
fake$gen <- as.factor(fake$gen)
fake$lf <- as.integer(fake$lf)
fakeI <- fake[fake$Origin=="i",]
fakeN <- fake[fake$Origin=="n",]
#lfmean <- tapply(fake$lf,fake$or, mean)
ggplot(fakeI, aes(x = gen, y = lf, color = "red")) +geom_path( aes(x=as.numeric(gen), y=lf))
qplot(gen, lf, data = fake, colour = or,
      stat = "summary", fun.y = "mean", geom = "line")

# If you just want to represent one value for each gen and or combination then it would be easier to calculate mean values before plotting. 
# That can be achieved with function ddply() from library plyr.

library(plyr)    
fake.sum<-ddply(fake,.(or,gen), summarize, lf = mean(lf))
fake.sum

# Now with this new data frame you just have to give x and y values. 
# or should be used in colour= and group= to ensure that each group has different color and that lines are connected.

ggplot(fake.sum,aes(x=gen,y=lf,colour=or,group=or))+
  geom_point()+geom_line()

se <- function(x) sqrt(var(x, na.rm=TRUE)/(length(na.omit(x))-1))
graphdata.sum <- ddply(graphdata, .(Origin, Generation),summarize, LfCountH = mean(LfCountH))

se(graphdata$LfCountH)
tapply(graphdata$LfCountH,graphdata$, max)


graphdata.sum
graphdata.sum$Generation <- as.factor(graphdata.sum$Generation)
ggplot(graphdata.sum, aes(x=Generation, y=LfCountH, color=Origin, group=Origin))+geom_line()
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

graphdata <- gen[!is.na(gen$BoltDay.adj),]
interaction.plot(graphdata$Generation, graphdata$Origin, graphdata$BoltDay.adj, fun=mean, main = "Interaction plot of bolting date")

#######################
#france graphing

# d <- read.table(file.choose(), header=T, sep="\t",quote='"', row.names=1) #measure1 Frm1DKdatdes.txt
# d2<-read.table(file.choose(), header=T, sep="\t",quote='"', row.names=1)#measure 2 Frm2DKdatdes.txt
# h <- read.table(file.choose(), header=T, sep="\t",quote='"', row.names=1) #measure harvest FrmHDKdatdes.txt
# 
# #plots of pop means from means table
# head(PopMeansMh)
# Hcont<-PopMeansMh[PopMeansMh$Trt=="control",]
# plot(Hcont$Pop, Hcont$MassH)
# title(main="Shoot mass at harvest", sub="Control treatment",
#       xlab="Population", ylab="mass(g)") 
# text(Hcont$Pop, Hcont$MassH, Hcont$Pop, cex=0.6, pos=4, col="red")
# 
# #plots of pop means from data, grouped by pop, trt
# library("gplot")
# library("ggplot2")
# 
# str(h)
# unique(h$Pop)
# h$Pop<-factor(h$Pop, c("CA001","CA008","CA009","CA010", "US001", "US002","US003", "BG001","GR001","GR002","GR003","HU001","RO001", "RO005","RU008","TR001","UA004"))
# print(levels(h$Pop))
# 
# png(filename="FrmassMeans.png", width=800, bg="white")
# p <- ggplot(data=h, aes(Pop, Shoot.mass.gH, fill=Trt)) + 
#   geom_boxplot()  
# plot(p)
# dev.off()
# 
# png(filename="FrcrownMeans.png", width=800, bg="white")
# p <- ggplot(data=h, aes(Pop, CrownDiam.mm, fill=Trt)) + 
#   geom_boxplot()  
# plot(p)
# dev.off()
# 
# str(d)
# unique(d$Pop)
# d$Pop<-factor(d$Pop, c("CA001","CA008","CA009","CA010", "US001", "US002","US003", "BG001","GR001","GR002","GR003","HU001","RO001", "RO005","RU008","TR001","UA004"))
# print(levels(d$Pop))
# 
# png(filename="FrDlfMeans.png", width=800, bg="white")
# p <- ggplot(data=d, aes(Pop, MaxLfLgth1)) + 
#   geom_boxplot()  
# plot(p)
# dev.off()
# 
# #barplot with se bars
# #harvest control shoot mass
# se <- function(x) sqrt(var(x, na.rm=TRUE)/(length(na.omit(x))-1))
# 
# Hcont2<-h[h$Trt=="control",]
# tapply(Hcont2$Shoot.mass.gH, Hcont2$Pop,mean,na.rm=TRUE)
# tapply(Hcont2$Shoot.mass.gH, Hcont2$Pop,se)
# plt <- barplot(tapply(Hcont2$Shoot.mass.gH, Hcont2$Pop,mean,na.rm=TRUE), ylim=c(0, 30))
# y.se <- tapply(Hcont2$Shoot.mass.gH, Hcont2$Pop,se)
# y.mean <- tapply(Hcont2$Shoot.mass.gH, Hcont2$Pop, mean, na.rm=TRUE)
# # y.mean + y.se
# # max(y.mean + y.se)
# # c(0, max(y.mean + y.se, na.rm=TRUE))
# ylim <- c(0, max(y.mean + y.se, na.rm=TRUE))
# 
# png(filename="Frmassbar.png", width=800, bg="white")
# x<- barplot(y.mean,ylim=ylim, main="Shoot mass at harvest, control", col="blue")
# arrows(x, y.mean - y.se, x, y.mean + y.se,code=3, length=0.03, angle=90)
# dev.off()
# #axis(1, at=1:17, lab=Hcont$Pop)
# 
# #overall
# tapply(h$Shoot.mass.gH, h$Pop,mean,na.rm=TRUE)
# tapply(h$Shoot.mass.gH, h$Pop,se)
# plt <- barplot(tapply(h$Shoot.mass.gH, h$Pop,mean,na.rm=TRUE), ylim=c(0, 30))
# y.se <- tapply(h$Shoot.mass.gH, h$Pop,se)
# y.mean <- tapply(h$Shoot.mass.gH, h$Pop, mean, na.rm=TRUE)
# # y.mean + y.se
# # max(y.mean + y.se)
# c(0, max(y.mean + y.se, na.rm=TRUE))
# ylim <- c(0, max(y.mean + y.se, na.rm=TRUE))
# x<- barplot(y.mean,ylim=ylim, main="Shoot mass at harvest, control", col="blue", beside=TRUE)
# arrows(x, y.mean - y.se, x, y.mean + y.se,code=3, length=0.03, angle=90)
# 
# 
# #########
# 
# #summary
# 
# summary(d)
# dpop<-as.data.frame(d)
# dpop<-dpop[order(dpop$Origin, decreasing=FALSE),]
# 
# dpop$Pop <- factor(dpop$Pop, c("", "", "","", ""))
# 
# plot(dpop$Pop)
# 
# plot(sort(PopMeansM1$Latitude))
# #axis(1, at=1:17, lab=as.vector(PopMeansM1$Pop))
# 
# plot(PopMeansM1$Latitude)
# plot(PopMeansM1$Pop,PopMeansM1$Latitude,col=ifelse(PopMeansM1$Latitude==3,"red", "black"))
# #col=ifelse(PopMeansM1$Origin=="inv", "red", "black")
# 
# plot(PopMeansM1$Latitude)
# # > axis(1, at=1:17, lab=as.vector(PopMeansM1$Pop))
# 
# # > PopMeansM1$Origin<-factor(PopMeansM1$Origin)
# # > PopMeansM1$col[PopMeansM1$Origin=="inv"]<-"red"
# #PopMeansM1$col[PopMeansM1$Origin=="nat"]<-"black"
# # > dotchart(PopMeansM1$Latitude, labels=PopMeansM1$Pop, groups=PopMeansM1$Origin, color=PopMeansM1$col)
# # > dotchart(PopMeansM1$Latitude, labels=PopMeansM1$Pop, color=PopMeansM1$col)
# # > dotchart(sort(PopMeansM1$Latitude), labels=PopMeansM1$Pop, color=PopMeansM1$col)
# # > dotchart(order(PopMeansM1$Latitude), labels=PopMeansM1$Pop, color=PopMeansM1$col)
# 
# 
# # summary(Frm1DKdatdes[Frm1DKdatdes$Origin=="nat"])
# # 
# # source("http://bioconductor.org/biocLite.R")
# # biocLite("psych")
# # library(psych)
# # describe.by(Frm1DKdatdes$LfCount1, Frm1DKdatdes$Origin)
# 
# #library(doBy)
# #summaryBy(mpg + wt ~ cyl + vs, data = mtcars,FUN = function(x) { c(m = mean(x), s = sd(x)) } )
# # produces mpg.m wt.m mpg.s wt.s for each
# # combination of the levels of cyl and vs 
# 
# tapply(Frm1DKcont$LfCount1, INDEX = Frm1DKcont$Origin, FUN = mean, na.rm=TRUE)
# tapply(Frm1DKcont$LfCount1, Frm1DKcont$Origin, sd, na.rm = TRUE)
# tapply(Frm1DKdatdes$LfCount1, INDEX = list(Frm1DKdatdes$Origin,Frm1DKdatdes$Trt), 
#        FUN = mean, na.rm=TRUE)
# # #barplots
# barplot(agdatm1$x, main="Leaf Count- m 1",names.arg=paste(agdatm1$Group.1,agdatm1$Group.2),
#         col="blue", axis.lty=1, xlab="groups", ylab="lf count") 
# 
# # aggregate data frame returning means
# # for numeric variables
# 
# agdatm1 <-aggregate(Frm1DKdatdes$LfCount1, by=list(Frm1DKdatdes$Origin,Frm1DKdatdes$Trt) ,FUN=mean, na.rm=TRUE)
# print(agdatm1)
# 
# #barplot with se bars
# #harvest root crown
# h <- FrmHDKdatdes
# head(h)
# h$group <- paste(h$Origin, h$Trt)
# class(h$group)
# h$group <- factor(h$group, levels=c("nat control","inv control","nat drought","inv drought"))
# tapply(h$CrownDiam.mm, h$group,mean,na.rm=TRUE)
# se <- function(x) sqrt(var(x, na.rm=TRUE)/(length(na.omit(x))-1))
# tapply(h$CrownDiam.mm, h$group,se)
# plt <- barplot(tapply(h$CrownDiam.mm, h$group,mean,na.rm=TRUE), ylim=c(0, 30))
# plt
# y.se <- tapply(h$CrownDiam.mm, h$group,se)
# y.mean <- tapply(h$CrownDiam.mm, h$group, mean, na.rm=TRUE)
# y.mean + y.se
# c(0, max(y.mean + y.se))
# ylim <- c(0, max(y.mean + y.se))
# x<- barplot(y.mean,ylim=ylim, main="Root crown diameter at harvest", col="blue")
# arrows(x, y.mean - y.se, x, y.mean + y.se,code=3, length=0.03, angle=90)
# 
# #m1 lf count
# d <- Frm1DKdatdes
# d$Origin<-factor(d$Origin, levels=c("nat","inv"))
# tapply(d$LfCount1, d$Origin,mean,na.rm=TRUE)
# se <- function(x) sqrt(var(x, na.rm=TRUE)/(length(na.omit(x))-1))
# tapply(d$LfCount1, d$Origin,se)
# barplot(tapply(d$LfCount1, d$Origin,mean,na.rm=TRUE), ylim=c(0, 10))
# plt <- barplot(tapply(d$LfCount1, d$Origin,mean,na.rm=TRUE), ylim=c(0, 10))
# plt
# y.se <- tapply(d$LfCount1, d$Origin,se)
# y.mean <- tapply(d$LfCount1, d$Origin, mean, na.rm=TRUE)
# y.mean + y.se
# c(0, max(y.mean + y.se))
# ylim <- c(0, max(y.mean + y.se))
# plt<- barplot(y.mean,ylim=ylim, main="Leaf No., week 5",cex.main=2.5, 
#               col=1:length(unique(Frm2DKcont$Origin)),xlab="Range", ylab="Leaf number",
#               cex.lab=1.5)
# arrows(plt, y.mean - y.se, plt, y.mean + y.se,code=3, length=0.03, angle=90)
# 
# #m2 lf width
# Frm2DKcont$Origin<-factor(Frm2DKcont$Origin,levels=c("nat", "inv"))
# 
# tapply(Frm2DKcont$MaxLfWdth2, Frm2DKcont$Origin,mean,na.rm=TRUE)
# se <- function(x) sqrt(var(x, na.rm=TRUE)/(length(na.omit(x))-1))
# tapply(Frm2DKcont$MaxLfWdth2, Frm2DKcont$Origin,se)
# barplot(tapply(Frm2DKcont$MaxLfWdth2, Frm2DKcont$Origin,mean,na.rm=TRUE), ylim=c(0, 10))
# plt <- barplot(tapply(Frm2DKcont$MaxLfWdth2, Frm2DKcont$Origin,mean,na.rm=TRUE), ylim=c(0, 10))
# plt
# y.se <- tapply(Frm2DKcont$MaxLfWdth2, Frm2DKcont$Origin,se)
# y.mean <- tapply(Frm2DKcont$MaxLfWdth2, Frm2DKcont$Origin, mean, na.rm=TRUE)
# y.mean + y.se
# c(0, max(y.mean + y.se))
# ylim <- c(0, 5)
# # Frm2DKcont$color[Frm2DKcont$Origin=="inv"]<-"red"
# # Frm2DKcont$color[Frm2DKcont$Origin=="nat"]<-"black"
# plt<- barplot(y.mean,ylim=ylim, main="Leaf width, week 8 ",  
#               col=1:length(unique(Frm2DKcont$Origin)), xlab="Range", ylab="Leaf width (cm)",
#               cex.main=2.5,cex.lab=1.5)
# arrows(plt, y.mean - y.se, plt, y.mean + y.se,code=3, length=0.03, angle=90)
# 
# 
# #Grouped and colored dot plot
# #Group and color data by genotype
# Frm1DKdatdes<-Frm1DKdatdes[order(Frm1DKdatdes$Origin),]
# Frm1DKdatdes$Origin<-factor(Frm1DKdatdes$Origin)
# Frm1DKdatdes$color[Frm1DKdatdes$Origin=="inv"]<-"red"
# Frm1DKdatdes$color[Frm1DKdatdes$Origin=="nat"]<-"black"
# # Frm2datTag$color[Frm2datTag$Origin=="sk"]<-"blue"
# # 
# par(mar=c(5,6,4,2)+0.1,mgp=c(7,1,0))
# dotchart(Frm1DKdatdes$LfCount1, ylab="indiv", xlab="lf number",cex=.7,labels=row.names(Frm1DKdatdes),groups= Frm1DKdatdes$Origin,main="lf number by origin", gcolor="black", color=Frm1DKdatdes$color)
# mtext("lf number", side=1,line=4)
# 
# # #lf length
# # 
# # par(mar=c(5,6,4,2)+0.1,mgp=c(7,1,0))
# # dotchart(Frm2datTag$lf.length, ylab="indiv", xlab="lf number",cex=.7,labels=row.names(Frm2datTag),groups= Frm2datTag$Origin,main="lf number by origin", gcolor="black", color=Frm2datTag$color)
# # mtext("lf length", side=1,line=4)
# # 
# # #lf width
# # class(Frm2datTag$lf.width)
# # Frm2datTag$lf.width<-as.numeric(Frm2datTag$lf.width)
# # dotchart(Frm2datTag$lf.width, ylab="indiv", xlab="lf number",cex=.7,labels=row.names(Frm2datTag),groups= Frm2datTag$Origin,main="lf number by origin", gcolor="black", color=Frm2datTag$color)
# # mtext("lf width", side=1,line=4)
# # 
# # #rosette diameter
# # class(Frm2datTag$rosette.diam)
# # Frm2datTag$rosette.diam<-as.numeric(Frm2datTag$rosette.diam)
# # dotchart(Frm2datTag$rosette.diam, ylab="indiv", xlab="lf number",cex=.7,labels=row.names(Frm2datTag),groups= Frm2datTag$Origin,main="lf number by origin", gcolor="black", color=Frm2datTag$color)
# # mtext("rosette diam", side=1,line=4)
# # 
# 
# # #avg
# # class()
# # m2means<-as.data.frame(aggregate(Frm2Imp$lf.number, list(Frm2Imp$Origin) , mean))
# # m2means$lf.number <- aggregate(Frm2Imp$lf.number, list(Frm2Imp$Origin) , mean)
# # m2means$lf.width <- aggregate(Frm2Imp$lf.width, list(Frm2Imp$Origin) , mean)
# # m2means$lf.length <- aggregate(Frm2Imp$lf.length, list(Frm2Imp$Origin) , mean)
# # m2means$rosette.diam <- aggregate(Frm2Imp$rosette.diam, list(Frm2Imp$Origin) , mean)
# # m2means
# # #names(m2means) <- c('dnase.conc', 'dens.avg')
# # 
# # 
# # plot(m2means$Group.1, m2means$x)