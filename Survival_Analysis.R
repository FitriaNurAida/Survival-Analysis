Dataku<-read.csv("E:/2018_2019/SEMESTER 7/TA/OUTPUT NEW/dataku.csv",sep=";")
View(Dataku)
str(Dataku)
library(survival)
library(survminer)
library(AICcmodavg)

Dataku$id<-as.factor(Dataku$id)
Dataku$d<-as.factor(Dataku$d)
Dataku$X2<-as.factor(Dataku$X2)
Dataku$X3<-as.factor(Dataku$X3)

Y<-Surv(Dataku$T,Dataku$d==1)
#Kurva KM#
kmfit1<-survfit(Y~1)
summary(kmfit1)
ggsurvplot(kmfit1,data=Dataku,surv.median.line="hv",pval=F,conf.int = F, xlab="Usia Pertama Melahirkan", legend="none", palette = "black",break.time.by=2)

kmfitX2<-survfit(Y~Dataku$X2)
summary(kmfitX2)
ggsurvplot(kmfitX2,data=Dataku,surv.median.line="hv",pval=F,conf.int = F, xlab="Usia Pertama Melahirkan",break.time.by=2,legend.labs=c("Tidak Sekolah","Tidak tamat SD","Tamat SD","Tidak tamat SLTA","Tamat SLTA","Perguruan Tinggi"))

kmfitX3<-survfit(Y~Dataku$X3)
summary(kmfitX3)
ggsurvplot(kmfitX3,data=Dataku,surv.median.line="hv",pval=F,conf.int = F, xlab="Usia Pertama Melahirkan",break.time.by=2,legend.labs=c("Perkotaan","Pedesaan"))

#Uji LOG-RANK#
LRX2<-survdiff(Y~Dataku$X2)
LRX2

LRX3<-survdiff(Y~Dataku$X3)
LRX3

#kurva -log(-logS(t))
s_kmfitX2<-summary(kmfitX2)
d_kmfitX2<-data.frame(s_kmfitX2$strata,s_kmfitX2$time,s_kmfitX2$surv)
names(d_kmfitX2)<-c("Pendidikan","Usia Pertama Melahirkan","KM Survival Estimate")
Pendidikan0<-d_kmfitX2[d_kmfitX2$Pendidikan=="Dataku$X2=0", ]
Pendidikan1<-d_kmfitX2[d_kmfitX2$Pendidikan=="Dataku$X2=1", ]
Pendidikan2<-d_kmfitX2[d_kmfitX2$Pendidikan=="Dataku$X2=2", ]
Pendidikan3<-d_kmfitX2[d_kmfitX2$Pendidikan=="Dataku$X2=3", ]
Pendidikan4<-d_kmfitX2[d_kmfitX2$Pendidikan=="Dataku$X2=4", ]
Pendidikan5<-d_kmfitX2[d_kmfitX2$Pendidikan=="Dataku$X2=5", ]
X2y0<--log(-log(Pendidikan0$`KM Survival Estimate`))
X2y1<--log(-log(Pendidikan1$`KM Survival Estimate`))
X2y2<--log(-log(Pendidikan2$`KM Survival Estimate`))
X2y3<--log(-log(Pendidikan3$`KM Survival Estimate`))
X2y4<--log(-log(Pendidikan4$`KM Survival Estimate`))
X2y5<--log(-log(Pendidikan5$`KM Survival Estimate`))
plot(Pendidikan0$`Usia Pertama Melahirkan`,X2y0,xlab="Usia Pertama Melahirkan",ylab="-ln(-ln(S(t)))",ylim=range(c(X2y0,X2y1,X2y2,X2y3,X2y4,X2y5)),xlim=range(c(Pendidikan0$`Usia Pertama Melahirkan`,Pendidikan1$`Usia Pertama Melahirkan`,Pendidikan2$`Usia Pertama Melahirkan`,Pendidikan3$`Usia Pertama Melahirkan`,Pendidikan4$`Usia Pertama Melahirkan`,Pendidikan5$`Usia Pertama Melahirkan`)),col="red",type="s",lty="solid")
par(new=T)
plot(Pendidikan1$`Usia Pertama Melahirkan`,X2y1,ylim=range(c(X4y0,X4y1,X4y2,X4y3,X4y4,X4y5)),xlim=range(c(Pendidikan0$`Usia Pertama Melahirkan`,Pendidikan1$`Usia Pertama Melahirkan`,Pendidikan2$`Usia Pertama Melahirkan`,Pendidikan3$`Usia Pertama Melahirkan`,Pendidikan4$`Usia Pertama Melahirkan`,Pendidikan5$`Usia Pertama Melahirkan`)),xlab="Usia Pertama Melahirkan",ylab="-ln(-ln(S(t)))",col="chocolate4",type="s",lty="solid")
par(new=T)
plot(Pendidikan2$`Usia Pertama Melahirkan`,X2y2,ylim=range(c(X4y0,X4y1,X4y2,X4y3,X4y4,X4y5)),xlim=range(c(Pendidikan0$`Usia Pertama Melahirkan`,Pendidikan1$`Usia Pertama Melahirkan`,Pendidikan2$`Usia Pertama Melahirkan`,Pendidikan3$`Usia Pertama Melahirkan`,Pendidikan4$`Usia Pertama Melahirkan`,Pendidikan5$`Usia Pertama Melahirkan`)),xlab="Usia Pertama Melahirkan",ylab="-ln(-ln(S(t)))",col="green",type="s",lty="solid")
par(new=T)
plot(Pendidikan3$`Usia Pertama Melahirkan`,X2y3,ylim=range(c(X4y0,X4y1,X4y2,X4y3,X4y4,X4y5)),xlim=range(c(Pendidikan0$`Usia Pertama Melahirkan`,Pendidikan1$`Usia Pertama Melahirkan`,Pendidikan2$`Usia Pertama Melahirkan`,Pendidikan3$`Usia Pertama Melahirkan`,Pendidikan4$`Usia Pertama Melahirkan`,Pendidikan5$`Usia Pertama Melahirkan`)),xlab="Usia Pertama Melahirkan",ylab="-ln(-ln(S(t)))",col="deepskyblue",type="s",lty="solid")
par(new=T)
plot(Pendidikan4$`Usia Pertama Melahirkan`,X2y4,ylim=range(c(X4y0,X4y1,X4y2,X4y3,X4y4,X4y5)),xlim=range(c(Pendidikan0$`Usia Pertama Melahirkan`,Pendidikan1$`Usia Pertama Melahirkan`,Pendidikan2$`Usia Pertama Melahirkan`,Pendidikan3$`Usia Pertama Melahirkan`,Pendidikan4$`Usia Pertama Melahirkan`,Pendidikan5$`Usia Pertama Melahirkan`)),xlab="Usia Pertama Melahirkan",ylab="-ln(-ln(S(t)))",col="blue",type="s",lty="solid")
par(new=T)
plot(Pendidikan5$`Usia Pertama Melahirkan`,X2y5,ylim=range(c(X4y0,X4y1,X4y2,X4y3,X4y4,X4y5)),xlim=range(c(Pendidikan0$`Usia Pertama Melahirkan`,Pendidikan1$`Usia Pertama Melahirkan`,Pendidikan2$`Usia Pertama Melahirkan`,Pendidikan3$`Usia Pertama Melahirkan`,Pendidikan4$`Usia Pertama Melahirkan`,Pendidikan5$`Usia Pertama Melahirkan`)),xlab="Usia Pertama Melahirkan",ylab="-ln(-ln(S(t)))",col="deeppink",type="s",lty="solid")
legend("topright", c("Tidak sekolah","Tidak tamat SD","Tamat SD","Tidak tamat SLTA","Tamat SLTA","Perguruan Tinggi"),lty=c("solid","solid","solid","solid","solid","solid"),col=c("red","chocolate4","green","deepskyblue","blue","deeppink"),lwd=3,cex=0.8)
par(new=F)

s_kmfitX3<-summary(kmfitX3)
d_kmfitX3<-data.frame(s_kmfitX3$strata,s_kmfitX3$time,s_kmfitX3$surv)
names(d_kmfitX3)<-c("Region","Usia Pertama Melahirkan","KM Survival Estimate")
Region1<-d_kmfitX3[d_kmfitX3$Region=="Dataku$X3=1", ]
Region2<-d_kmfitX3[d_kmfitX3$Region=="Dataku$X3=2", ]
X3y1<--log(-log(Region1$`KM Survival Estimate`))
X3y2<--log(-log(Region2$`KM Survival Estimate`))
plot(Region1$`Usia Pertama Melahirkan`,X3y1,xlab="Usia Pertama Melahirkan",ylab="-ln(-ln(S(t)))",ylim=range(c(X3y1,X3y2)),xlim=range(c(Region1$`Usia Pertama Melahirkan`,Region2$`Usia Pertama Melahirkan`)),col="red",type="s",lty="solid")
par(new=T)
plot(Region2$`Usia Pertama Melahirkan`,X3y2,ylim=range(c(X3y1,X3y2)),xlim=range(c(Region1$`Usia Pertama Melahirkan`,Region2$`Usia Pertama Melahirkan`)),xlab="Usia Pertama Melahirkan",ylab="-ln(-ln(S(t)))",col="blue",type="s",lty="solid")
legend("topright", c("Perkotaan","Pedesaan"),lty=c("solid","solid"),col=c("red","blue"),lwd=3,cex=0.8)
par(new=F)

##PEMODELAN COX PH##
mod1<-coxph(Y~X1+X2+X3,data=Dataku,method="breslow")
summary(mod1)

##Uji GOF##
gof.mod1<-cox.zph(mod1,transform="rank")
gof.mod1

##PEMODELAN COX EXTENDED HEAVYSIDE SEMUA##
#martingale residual#
martingale<-resid(mod1,type='martingale')
plot(Dataku$X1, martingale,xlab="Usia Kawin Pertama", ylab="Martingale Residuals")
lines(lowess(Dataku$X1, martingale),col='red')
abline(v=c(15,20,25),lty="dotted", lwd=2, col="darkgrey")
#1:kurang dari 15 tahun
#2:15-19 tahun
#3:20-24 tahun
#4:di atas 24 tahun
Datakunew<-read.csv("E:/2018_2019/SEMESTER 7/TA/OUTPUT NEW/datakunew.csv",sep=";")
View(Datakunew)
Datakunew$id<-as.factor(Datakunew$id)
Datakunew$d<-as.factor(Datakunew$d)
Datakunew$X2<-as.factor(Datakunew$X2)
Datakunew$X3<-as.factor(Datakunew$X3)
Datakunew$codeX1<-as.factor(Datakunew$codeX1)
#PEMODELAN COX PH#
Y<-Surv(Datakunew$T,Datakunew$d==1)
mod2<-coxph(Y~codeX1+X2+X3,data=Datakunew,method="breslow")
summary(mod2)
#Uji GOF#
gof.mod2<-cox.zph(mod2,transform="rank")
gof.mod2
#KM#
kmfitX1<-survfit(Y~Datakunew$codeX1)
summary(kmfitX1)
ggsurvplot(kmfitX1,data=Datakunew,surv.median.line="hv",pval=F,conf.int = F, xlab="Usia Pertama Melahirkan",break.time.by=2,legend.labs=c("Kurang dari 15 tahun","15-19 tahun","20-24 tahun","Di atas 24 tahun"))
#LOG RANK#
LRcodeX1<-survdiff(Y~Datakunew$codeX1)
LRcodeX1
#kurva loglog#
s_kmfitX1<-summary(kmfitX1)
d_kmfitX1<-data.frame(s_kmfitX1$strata,s_kmfitX1$time,s_kmfitX1$surv)
names(d_kmfitX1)<-c("Kelompok Usia Kawin Pertama","Usia Pertama Melahirkan","KM Survival Estimate")
KelUKP0<-d_kmfitX1[d_kmfitX1$`Kelompok Usia Kawin Pertama`=="Datakunew$codeX1=0", ]
KelUKP1<-d_kmfitX1[d_kmfitX1$`Kelompok Usia Kawin Pertama`=="Datakunew$codeX1=1", ]
KelUKP2<-d_kmfitX1[d_kmfitX1$`Kelompok Usia Kawin Pertama`=="Datakunew$codeX1=2", ]
KelUKP3<-d_kmfitX1[d_kmfitX1$`Kelompok Usia Kawin Pertama`=="Datakunew$codeX1=3", ]
X1y0<--log(-log(KelUKP0$`KM Survival Estimate`))
X1y1<--log(-log(KelUKP1$`KM Survival Estimate`))
X1y2<--log(-log(KelUKP2$`KM Survival Estimate`))
X1y3<--log(-log(KelUKP3$`KM Survival Estimate`))
plot(KelUKP0$`Usia Pertama Melahirkan`,X1y0,xlab="Usia Pertama Melahirkan",ylab="-ln(-ln(S(t)))",ylim=range(c(X1y0,X1y1,X1y2,X1y3)),xlim=range(c(KelUKP0$`Usia Pertama Melahirkan`,KelUKP1$`Usia Pertama Melahirkan`,KelUKP2$`Usia Pertama Melahirkan`,KelUKP3$`Usia Pertama Melahirkan`)),col="red",type="s",lty="solid")
par(new=T)
plot(KelUKP1$`Usia Pertama Melahirkan`,X1y1,ylim=range(c(X1y0,X1y1,X1y2,X1y3)),xlim=range(c(KelUKP0$`Usia Pertama Melahirkan`,KelUKP1$`Usia Pertama Melahirkan`,KelUKP2$`Usia Pertama Melahirkan`,KelUKP3$`Usia Pertama Melahirkan`)),xlab="Usia Pertama Melahirkan",ylab="-ln(-ln(S(t)))",col="green",type="s",lty="solid")
par(new=T)
plot(KelUKP2$`Usia Pertama Melahirkan`,X1y2,ylim=range(c(X1y0,X1y1,X1y2,X1y3)),xlim=range(c(KelUKP0$`Usia Pertama Melahirkan`,KelUKP1$`Usia Pertama Melahirkan`,KelUKP2$`Usia Pertama Melahirkan`,KelUKP3$`Usia Pertama Melahirkan`)),xlab="Usia Pertama Melahirkan",ylab="-ln(-ln(S(t)))",col="blue",type="s",lty="solid")
par(new=T)
plot(KelUKP3$`Usia Pertama Melahirkan`,X1y3,ylim=range(c(X1y0,X1y1,X1y2,X1y3)),xlim=range(c(KelUKP0$`Usia Pertama Melahirkan`,KelUKP1$`Usia Pertama Melahirkan`,KelUKP2$`Usia Pertama Melahirkan`,KelUKP3$`Usia Pertama Melahirkan`)),xlab="Usia Pertama Melahirkan",ylab="-ln(-ln(S(t)))",col="darkorchid1",type="s",lty="solid")
legend("topright", c("Kurang dari 15 tahun","15-19 tahun","20-24 tahun","Di atas 24 tahun"),lty=c("solid","solid","solid","solid"),col=c("red","green","blue","darkorchid1"),lwd=3,cex=0.8)
par(new=F)

Datakunew<-read.csv("E:/2018_2019/SEMESTER 7/TA/OUTPUT NEW/datakunew.csv",sep=";")
Datakunew3<-survSplit(Datakunew,cut = c(18,23),end="T", event="d",start="start")
Datakunew3$X1hv1=Datakunew3$codeX1*(Datakunew3$start<18)
Datakunew3$X1hv2=Datakunew3$codeX1*(Datakunew3$start>=18)
Datakunew3$X2hv1=Datakunew3$X2*(Datakunew3$start<23)
Datakunew3$X2hv2=Datakunew3$X2*(Datakunew3$start>=23)

Y2=Surv(Datakunew3$start,Datakunew3$T,Datakunew3$d)
modske2<-coxph(Y2 ~ as.factor(X3)+as.factor(X1hv1)+as.factor(X2hv1)+as.factor(X1hv2)+as.factor(X2hv2)+cluster(id),data=Datakunew3,method = "breslow")
summary(modske2)

extractAIC(modske2)
AICc(modske2, return.K = FALSE, second.ord = TRUE, nobs = NULL)

##PEMODELAN COX EXTENDED LN(t) dan HEAVYSIDE##
Dataku<-read.csv("E:/2018_2019/SEMESTER 7/TA/OUTPUT NEW/datakunew.csv",sep=";")
Dataku2<-survSplit(Dataku,cut = c(23),end="T", event="d",start="start")
cut.points<-unique(Dataku2$T[Dataku$d==1])
Datakunew4=survSplit(Dataku2,cut=cut.points,end="T",start="start", event="d")
View(Dataku2)
View(Datakunew4)
Datakunew4$logtX1=Datakunew4$X1*log(Datakunew4$T)
Datakunew4$X2hv1=Datakunew4$X2*(Datakunew4$start<23)
Datakunew4$X2hv2=Datakunew4$X2*(Datakunew4$start>=23)

Y3=Surv(Datakunew4$start,Datakunew4$T,Datakunew4$d)
modske3<-coxph(Y3 ~ as.factor(X3)+X1+as.factor(X2hv1)+logtX1+as.factor(X2hv2)+cluster(id),data=Datakunew4,method = "breslow")
summary(modske3) #model lengkap

modske3_1<-coxph(Y3 ~ X1+as.factor(X2hv1)+logtX1+as.factor(X2hv2)+cluster(id),data=Datakunew4,method = "breslow")
summary(modske3_1) #tanpa X3

extractAIC(modske3) #model lengkap
extractAIC(modske3_1) #tanpa X3
AICc(modske3, return.K = FALSE, second.ord = TRUE, nobs = NULL)
AICc(modske3_1, return.K = FALSE, second.ord = TRUE, nobs = NULL)

##PEMODELAN COX EXTENDED t dan HEAVYSIDE##
Dataku<-read.csv("E:/2018_2019/SEMESTER 7/TA/OUTPUT NEW/datakunew.csv",sep=";")
Dataku3<-survSplit(Dataku,cut = c(23),end="T", event="d",start="start")
cut.points<-unique(Dataku3$T[Dataku$d==1])
Datakunew5=survSplit(Dataku3,cut=cut.points,end="T",start="start", event="d")
View(Dataku3)
View(Datakunew5)
Datakunew5$tX1=Datakunew5$X1*Datakunew5$T
Datakunew5$X2hv1=Datakunew5$X2*(Datakunew5$start<23)
Datakunew5$X2hv2=Datakunew5$X2*(Datakunew5$start>=23)

Y4=Surv(Datakunew5$start,Datakunew5$T,Datakunew5$d)
modske4<-coxph(Y4 ~ as.factor(X3)+X1+as.factor(X2hv1)+tX1+as.factor(X2hv2)+cluster(id),data=Datakunew5,method = "breslow")
summary(modske4) #model lengkap

modske4_1<-coxph(Y4 ~ X1+as.factor(X2hv1)+tX1+as.factor(X2hv2)+cluster(id),data=Datakunew5,method = "breslow")
summary(modske4_1) #tanpa X3

extractAIC(modske4) #model lengkap
extractAIC(modske4_1) #tanpa X3
AICc(modske4, return.K = FALSE, second.ord = TRUE, nobs = NULL)
AICc(modske4_1, return.K = FALSE, second.ord = TRUE, nobs = NULL)

##Kuva HR UKP untuk fungsi waktu Ln(t)##
beta<-modske3_1$coefficients[1]
psi<-modske3_1$coefficients[7]
T<-sort(Dataku$T)
HR_X1lnt=exp(beta+psi*log(T))
plot(T,HR_X1lnt,xlab="Usia Pertama Melahirkan",ylab="estimated HR",col="black",type="o",lty="solid",lwd=1.5)

##Kuva HR UKP untuk fungsi waktu T##
beta<-modske4_1$coefficients[1]
psi<-modske4_1$coefficients[7]
T<-sort(Dataku$T)
HR_X1t=exp(beta+psi*T)
plot(T,HR_X1t,xlab="Usia Pertama Melahirkan",ylab="estimated HR",col="black",type="o",lty="solid",lwd=1.5)

##################################################################################
resisca<-data.frame(gof.mod1$y)
View(resisca)
write.csv(resisca,file="resischsca.csv")
rank<-data.frame(gof.mod1$x)
View(rank)

##################################################################################
dta<-read.csv("E:/2018_2019/SEMESTER 7/TA/OUTPUT NEW/dataku.csv",sep=";")
View(dta)
dim(dta)
library(plyr)
install.packages("radiant.data")
library(radiant.data)
dta$wt<-dta$Weight/1000000
dta1<-filter(dta,d==1)
dta0<-filter(dta,d==0)
ddply(dta,~d,summarise,mean=weighted.mean(T,wt),sd=weighted.sd(T,wt))

ddply(dta,~d,summarise,mean=weighted.mean(X1,wt),sd=weighted.sd(X1,wt))

ddply(dta,~X2,summarise,mean=weighted.mean(T,wt),sd=weighted.sd(T,wt))
ddply(dta1,~X2,summarise,mean=weighted.mean(T,wt),sd=weighted.sd(T,wt))
ddply(dta0,~X2,summarise,mean=weighted.mean(T,wt),sd=weighted.sd(T,wt))

ddply(dta,~X3,summarise,mean=weighted.mean(T,wt),sd=weighted.sd(T,wt))
ddply(dta1,~X3,summarise,mean=weighted.mean(T,wt),sd=weighted.sd(T,wt))
ddply(dta0,~X3,summarise,mean=weighted.mean(T,wt),sd=weighted.sd(T,wt))

ddply(dta,~X2,summarise,mean=weighted.mean(X1,wt),sd=weighted.sd(X1,wt))
ddply(dta1,~X2,summarise,mean=weighted.mean(X1,wt),sd=weighted.sd(X1,wt))
ddply(dta0,~X2,summarise,mean=weighted.mean(X1,wt),sd=weighted.sd(X1,wt))

ddply(dta,~X3,summarise,mean=weighted.mean(X1,wt),sd=weighted.sd(X1,wt))
ddply(dta1,~X3,summarise,mean=weighted.mean(X1,wt),sd=weighted.sd(X1,wt))
ddply(dta0,~X3,summarise,mean=weighted.mean(X1,wt),sd=weighted.sd(X1,wt))

