####MF figures#####

##box whisker plotof size trait###
#make dataframe
grdat <- mfcom1[,c(1:12,55:56,58:63)]
grdat <- merge(grdat, mfallo.dk,all=TRUE)
grdat$Trt <- as.factor("Early Control")
colnames(grdat)[10]<-"LfCountH"
colnames(grdat)[24]<-"lxwH"

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
levels(grdat$Origin)[levels(grdat$Origin)=="inv"] <- "Invasive"
levels(grdat$Origin)[levels(grdat$Origin)=="nat"] <- "Native"
grdat$Trt <- factor(grdat$Trt, c("Early Control", "Control", "Nutrient", "Herbivory"))

pdf("MF size box.pdf", useDingbats=FALSE)
p1 <- ggplot(grdat[grdat$Trt!="Herbivory",],aes(Trt, lxwH, fill=Origin))+geom_boxplot()+xlab("Stress Treatment")+ylab("Approximate area of longest leaf (cm2)")+ theme(legend.justification=c(1,1), legend.position=c(1,1))
p1 <- p1 + annotate('point',x = "Early Control", y = 110, pch=8, color="red",parse=T, size=3)+annotate('point',x = "Nutrient", y = 110, pch=8, color="red",parse=T, size=3)+
  theme(axis.text.x=element_text(size=10))

p2 <- ggplot(grdat, aes(Trt, LfCountH, fill=Origin))+geom_boxplot()+xlab("Stress Treatment")+ylab("Number of basal leaves")+theme(legend.position="none")
#legend position(left/right,top/bottom)
p2 <- p2 +  annotate('point',x = "Control", y = 30, pch=8, color="red",parse=T, size=3)
multiplot(p1,p2, cols=2)
dev.off()

#######bolted mosaic plot###
#control, cut, nut
grdatB <- merge(mfco.dk1,mfcu.dk, all=TRUE)
grdatB <- merge(grdatB,mfn.dk, all=TRUE )

#grB<- ddply(grdatB, .(Trt, Origin, BoltedatH), summarize, count = length(BoltedatH))

grB2 <- ddply(grdatB, .(Trt, Origin), summarize, totcount = length(BoltedatH))
grB3 <- ddply(grdatB, .(Trt, Origin, BoltedatH), summarize, count = length(BoltedatH))
grB <- merge(grB2,grB3, all.y=TRUE)
grB$Treatment <- paste(grB$Trt, grB$Origin, grB$BoltedatH)

grB$xmin <- 0
grB$xmax <- 96
grB[1:2,]$xmax<- 16
grB[3:4,]$xmin<- 16
grB[3:4,]$xmax<- 32
grB[5:6,]$xmin<- 32
grB[5:6,]$xmax<- 48
grB[7:8,]$xmin<- 48
grB[7:8,]$xmax<- 64
grB[9:10,]$xmin<- 64
grB[9:10,]$xmax<- 80
grB[11,]$xmin<- 80

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

pdf("MF bolted mosaic.pdf", useDingbats=FALSE)
p1 <- ggplot(grBatH1, aes(ymin = ymin, ymax = ymax, xmin=xmin, xmax=xmax, fill=Treatment))+ geom_rect(colour = I("grey"), size=1.5)+
  scale_x_continuous(breaks=seq(16,80,32),labels=c("Control", "Herbivory", "Nutrient"), name="Stress Treatments")+
  scale_y_continuous(name="Percent Bolted at Harvest")

p1 + annotate(geom="text", x=grBatH1$xmin+8, y=105, label=grBatH1$Origin, size=3) +
  annotate(geom="text", x=grBatH1$xmin+8, y=grBatH1$ymin+2, label=grBatH1$BoltedatH, size=2)+ 
  theme(legend.position="none", axis.title.x = element_text(size=15, face="bold", vjust=-0.4), 
        axis.title.y = element_text(size=15, face="bold"),axis.text.x = element_text(size=15 ))+ 
  annotate('point',x = 16, y = 101, pch=8, color="red",parse=T, size=3)
  
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
#####IDK, maybe???cls mean +/- 1.96*sqrt(mean/count)
#mean:inv, gen0 : intercept, inv gen1: int+gen, nat gen0: int-nat, nat gen1: int-nat:gen
#SEs: inv gen0: int +SE, 
#     inv gen1:int+gen+gen1SE, 
#     nat gen0:int-nat+natSE, 
#     nat gen1:int-nat:gen+nat:genSE
#SE
int<-2.75258
#inv mean
B<--0.36134-0.09134
#Originnat estimate from model summary
pI<-exp(int)
pN<-exp(int+B)
pI
pN
#from modelInt:LfCountH ~ Origin * Generation + (1 | PopID/MomFam) , poisson
# Fixed effects:
#                       Estimate Std. Error z value Pr(>|z|)    
# (Intercept)            2.75258    0.10115  27.213  < 2e-16 ***
#   Originnat            -0.24940    0.14393  -1.733   0.0831 .  
# Generation1            0.29199    0.05519   5.290 1.22e-07 ***
# Originnat:Generation1 -0.36134    0.09134  -3.956 7.62e-05 ***

genlf <- data.frame(Origin=c("Invasive","Invasive","Native","Native"), Generation=c(0,1,0,1), lfmean=c(15.68304,21.001,12.2213,10.92704), 
                    uSE=c(17.35239,22.19262,14.11319, 11.97211),lSE=c(14.17429, 19.87336, 10.58301,9.973185) )
genlf <- merge(genlf, ddply(graphdata, .(Generation, Origin), summarize, count = length(Origin)), all.x=TRUE)

pdf("STMF Gen leaf.pdf", useDingbats=FALSE)
p1 <- ggplot(genlf, aes(x=Generation, y=lfmean, color=Origin, group=Origin))+
  geom_errorbar(aes(ymin=lSE, ymax=uSE),color="black", width=.1)+
  geom_line()+geom_point(size=5)+ylab("Mean Number of Basal Leaves")+
  scale_x_continuous(breaks=seq(0,1,1), name="Generation")+ 
  ggtitle("Number of basal leaves\nCross-generational analysis") + theme(plot.title = element_text(lineheight=.8, face="bold"))+
  theme(legend.justification=c(0,1), legend.position=c(0,1))
p1 + annotate(geom="text", x=0, y=10.1, label="Full data set", fontface="italic",size=3) +annotate(geom="text", x=0, y=10.4, label="Origin", fontface="italic",size=3) +
  annotate('point',x = -0.02, y = 9.8, pch=8, color="red",parse=T,size=3)+annotate('point',x = 0.02, y = 9.8, pch=8, color="red",parse=T,size=3)+
  annotate('point',x = 1, y = 16, pch=8, color="red",parse=T,size=3)+annotate(geom="text", x=1, y=16.4, label="Origin",fontface="italic", size=3) +
  annotate('point',x = 0.48, y = 16, pch=8, color="red",parse=T,size=3)+annotate('point',x = 0.52, y = 16, pch=8, color="red",parse=T,size=3)+
  annotate(geom="text", x=0.5, y=16.4, label="Origin", size=3)+ annotate(geom="text", x=0.5, y=15.6, label="Origin*Generation", size=3)+
  annotate('point',x = 0.5, y = 15.2, pch=8, color="red",parse=T,size=3)+annotate('point',x = 0.54, y = 15.2, pch=8, color="red",parse=T,size=3)+annotate('point',x = 0.46, y = 15.2, pch=8, color="red",parse=T,size=3)
dev.off()

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
interaction.plot(graphdata$Generation, graphdata$Origin, graphdata$BoltDay.adj, fun=mean, main = "Interaction plot of bolting date")

#make graphdata
#BoltDay.adj ~ Origin * Generation + (1 | PopID/MomFam), poisson
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           4.29826    0.10253   41.92  < 2e-16 ***
#   Originnat           0.02222    0.11812    0.19  0.85081    
# Generation           -0.04531    0.08147   -0.56  0.57812    
# Originnat:Generation -0.27966    0.09620   -2.91  0.00365 ** 

#mean:inv, gen0 : intercept, inv gen1: int+gen, nat gen0: int-nat, nat gen1: int-nat:gen
#SEs: inv gen0: int +SE, 
#     inv gen1:int+gen+gen1SE, 
#     nat gen0:int-nat+natSE, 
#     nat gen1:int-nat:gen+nat:genSE
#SE
int<-4.298261-0.04531-0.08147#inv mean
B<-0.02222+0.11812#Originnat estimate from model summary
pI<-exp(int)
pN<-exp(int+B)
pI
pN

genb <- data.frame(Origin=c("Invasive","Invasive","Native","Native"), Generation=c(0,1,0,1), boltmean=c(73.57167,70.31253,75.22473,56.87302), 
                    uSE=c(81.51532,76.28078,84.65642,61.24015),lSE=c(66.40225,64.81137,66.84396,50.5215) )
#genb <- merge(genb, ddply(graphdata, .(Generation, Origin), summarize, count = length(Origin)), all.x=TRUE)
pd <- position_dodge(.1)

pdf("STMF Gen boltdate.pdf", useDingbats=FALSE)
p1 <- ggplot(genb, aes(x=Generation, y=boltmean, color=Origin, group=Origin, ymax=90))+
  geom_errorbar(aes(ymin=lSE, ymax=uSE),color="black", width=.1, position=pd)+
  geom_line(position=pd)+geom_point(size=5, position=pd)+ylab("Mean Bolt Date")+
  scale_x_continuous(breaks=seq(0,1,1), name="Generation")+ 
  ggtitle("Bolt Date\nCross-generational analysis") + theme(plot.title = element_text(lineheight=.8, face="bold"))+
  theme(legend.justification=c(1,1), legend.position=c(1,1))
p1 +  annotate('point',x = 1, y = 80, pch=8, color="red",parse=T,size=3)+annotate(geom="text", x=1, y=82, label="Origin",fontface="italic", size=3) +
  annotate(geom="text", x=0, y=60, label="Origin",fontface="italic", size=3)+annotate(geom="text", x=0, y=58, label="NS",fontface="italic", size=3)+
  annotate('point',x = 0.5, y = 78, pch=16, color="red",parse=T,size=3)+
  annotate(geom="text", x=0.5, y=80, label="Origin", size=3)+ annotate(geom="text", x=0.5, y=76, label="Origin*Generation", size=3)+
  annotate('point',x = 0.48, y = 74, pch=8, color="red",parse=T,size=3)+annotate('point',x = 0.52, y = 74, pch=8, color="red",parse=T,size=3)
dev.off()