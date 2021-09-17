####################################################################

#######################  Figs for talks  ###########################

####################################################################


### setup

library(rptR)

thick<-5  ##line thickness
### colours

library(wesanderson)

wes_palettes

Col1<-"#3B9AB2"
Col2<-"#EBCC2A"
Col3<-"#F21A00"

layout(1)

par(mgp=c(0.5,1,0),mar=c(2,2,0.5,0.5))

### Figure 1 - 

png("TalkFigs/Plasticity1.png",width=5,height=5,pointsize=20,units="in", res=300)
par(mgp=c(0.5,1,0),mar=c(2,2,0.5,0.5))
plot(c(0,1),c(0,1),type="l",xlab="Environment (timing of spring)",ylab="Behaviour (timing of birth)",xaxt='n',yaxt='n',lwd=thick,bty="l",col="white")
dev.off()

png("TalkFigs/Plasticity2.png",width=5,height=5,pointsize=20,units="in", res=300)
par(mgp=c(0.5,1,0),mar=c(2,2,0.5,0.5))
plot(c(0,1),c(0,1),type="l",xlab="Environment (timing of spring)",ylab="Behaviour (timing of birth)",xaxt='n',yaxt='n',lwd=thick,col="white",bty="l")
abline(a=0,b=0,col=Col2,lwd=thick)
legend(legend="No",col=Col2,title="Plasticity:",x=0.05,y=1,lty=1,lwd=5,bty="n")

dev.off()


png("TalkFigs/Plasticity3.png",width=5,height=5,pointsize=20,units="in", res=300)
par(mgp=c(0.5,1,0),mar=c(2,2,0.5,0.5))
plot(c(0,1),c(0,1),type="l",xlab="Environment (timing of spring)",ylab="Behaviour (timing of birth)",xaxt='n',yaxt='n',lwd=thick,col=Col1,bty="l")
abline(a=0,b=0,col=Col2,lwd=thick)
legend(legend=c("No","A lot"),col=c(Col2,Col1),title="Plasticity:",x=0.05,y=1,lty=1,lwd=5,bty="n")

dev.off()


png("TalkFigs/Plasticity4.png",width=5,height=5,pointsize=20,units="in", res=300)
par(mgp=c(0.5,1,0),mar=c(2,2,0.5,0.5))
plot(c(0,1),c(0,1),type="l",xlab="Environment (timing of spring)",ylab="Behaviour (timing of birth)",xaxt='n',yaxt='n',lwd=thick,col=Col1,bty="l")
abline(a=0,b=0,col=Col2,lwd=thick)
abline(a=0,b=0.5,col=Col3,lwd=thick)
legend(legend=c("No","A lot","Some"),col=c(Col2,Col1,Col3),title="Plasticity:",x=0.05,y=1,lty=1,lwd=5,bty="n")

dev.off()








sd<-5
n<-10
Ind1<-rnorm(n,140,sd)
Ind2<-rnorm(n,150,sd)
Ind3<-rnorm(n,160,sd)

cols<-sample(c(Col3,Col1,Col2),n*3,replace=T)
noRep<-data.frame(c(Ind1,Ind2,Ind3),cols)
colnames(noRep)<-c("Obs","ID")

m<-c(1,1,1,1,2)

All<-c(Ind1)


layout(m)

jit<-jitter(rep(1,n*3))
symbol=16
buffer<-5
months<-c("January","February","March","April","May","June","July","August","September",
          "October","November","Decemeber")

### Repeatability Fig 1: population level

png("TalkFigs/Rep1.png",width=5,height=5,pointsize=14,units="in",res=300)
par(mgp=c(1,1,0),mar=c(1,1,0,0),oma=c(2,2,0.5,0.5))
layout(m)

plot(density(c(Ind1,Ind2,Ind3)),xlab=NA,ylab="Density", main=NA,lwd=5,yaxt="n",xaxt='n',
     xlim=c(min(noRep$Obs-buffer),max(noRep$Obs+buffer)))
plot(jit~c(Ind1,Ind2,Ind3),xlab="Day of parturition",ylab="Observations",yaxt='n',pch=symbol,
     xlim=c(min(noRep$Obs-buffer),max(noRep$Obs+buffer)),xaxt='n')
axis(1,at=c(0,31,60,91,121,152,182,213,244,274,305,335,365),labels=NA,tck=-0.09,lwd.ticks = 2)
axis(1,at=c(15,46,75,106,136,167,197,228,259,289,320,350),labels=months,tck=0,padj=-1,cex.axis=1.5)
mtext("Date",side=1,outer=T,padj=0.3)
mtext("Observations",side=2,outer=T,adj=-0.03)
mtext("Density",side=2,outer=T,adj=0.6)

dev.off()

### Repeatability Fig 2: No repeatability

png("TalkFigs/Rep2-2.png",width=5,height=5,pointsize=14,units="in",res=300)
par(mgp=c(1,1,0),mar=c(1,1,0,0),oma=c(2,2,0.5,0.5))
layout(m)

plot(density(subset(noRep,cols==Col2)$Obs),xlab=NA,ylab="Density", main=NA,lwd=5,yaxt="n",xaxt='n',col=Col2,
     xlim=c(min(noRep$Obs-15),max(noRep$Obs+15)),
     ylim=c(0,max(c(max(density(subset(noRep,cols==Col2)$Obs)$y),
                                                          max(density(subset(noRep,cols==Col1)$Obs)$y),
                                                          max(density(subset(noRep,cols==Col3)$Obs)$y)))))
lines(density(subset(noRep,cols==Col3)$Obs),xlab=NA,ylab="Density", main=NA,lwd=5,yaxt="n",xaxt='n',col=Col3)
lines(density(subset(noRep,cols==Col1)$Obs),xlab=NA,ylab="Density", main=NA,lwd=5,yaxt="n",xaxt='n',col=Col1)
plot(jit~noRep$Obs,col=as.character(noRep$ID),xlab="Day of parturition",ylab="Observations",yaxt='n',
     xlim=c(min(noRep$Obs-buffer),max(noRep$Obs+buffer)),pch=symbol,xaxt='n')
axis(1,at=c(0,31,60,91,121,152,182,213,244,274,305,335,365),labels=NA,tck=-0.09,lwd.ticks = 2)
axis(1,at=c(15,46,75,106,136,167,197,228,259,289,320,350),labels=months,tck=0,padj=-1,cex.axis=1.5)
mtext("Date",side=1,outer=T,padj=0.3)
mtext("Observations",side=2,outer=T,adj=-0.03)
mtext("Density",side=2,outer=T,adj=0.6)

dev.off()





### Repeatability Fig 3: Repeatable

png("TalkFigs/Rep3.png",width=5,height=5,pointsize=14,units="in",res=300)
par(mgp=c(1,1,0),mar=c(1,1,0,0),oma=c(2,2,0.5,0.5))
layout(m)

plot(density(Ind1),xlab=NA,ylab="Density", main=NA,lwd=5,yaxt="n",xaxt='n',col=Col1,
     xlim=c(min(noRep$Obs-buffer),max(noRep$Obs+buffer)),ylim=c(0,max(c(max(density(Ind1)$y),
                                                                max(density(Ind2)$y),
                                                                max(density(Ind3)$y)))))
lines(density(Ind2),xlab=NA,ylab="Density", main=NA,lwd=5,yaxt="n",xaxt='n',col=Col2)
lines(density(Ind3),xlab=NA,ylab="Density", main=NA,lwd=5,yaxt="n",xaxt='n',col=Col3)

plot(jit~c(Ind1,Ind2,Ind3),col=rep(c(Col1,Col2,Col3),times=1,each=n),xlab="Day of parturition",
     ylab="Observations",yaxt='n',xlim=c(min(noRep$Obs-buffer),max(noRep$Obs+buffer)),pch=symbol,xaxt='n')

axis(1,at=c(0,31,60,91,121,152,182,213,244,274,305,335,365),labels=NA,tck=-0.09,lwd.ticks = 2)
axis(1,at=c(15,46,75,106,136,167,197,228,259,289,320,350),labels=months,tck=0,padj=-1,cex.axis=1.5)
mtext("Date",side=1,outer=T,padj=0.3)
mtext("Observations",side=2,outer=T,adj=-0.03)
mtext("Density",side=2,outer=T,adj=0.6)

dev.off()



### Calculating repeatabilities

repData<-data.frame(c(Ind1,Ind2,Ind3),rep(c(Col2,Col3,Col1),times=1,each=n))
colnames(repData)<-c("Obs","ID")
rpt1<-rpt(Obs~(1|ID),grname="ID",data=noRep,datatype = "Gaussian")
rpt2<-rpt(Obs~(1|ID),grname="ID",data=repData,datatype = "Gaussian")

rpt1
rpt2



### BRN plots

pdf("TalkFigs/BRN.pdf",width=6,height=5,pointsize=14)
par(mgp=c(2,1,0),mar=c(3,3,0.5,0.5))
plot(c(110,170),c(110,170),type="l",col=Col1,lwd=5,ylim=c(110,180),xlab="Environment, mean-centred",
     ylab="Timing of parturition",xaxt="n",yaxt="n",cex.lab=1.1)
axis(2,at=c(0,31,60,91,121,152,182,213,244,274,305,335,365),labels=NA,tck=-0.09,lwd.ticks = 2)
axis(2,at=c(15,46,75,106,136,167,197,228,259,289,320,350),labels=months,tck=0,padj=0.5,cex.axis=1.1)
axis(1,at=seq(110,170,by=10),labels=seq(-30,30,by=10))
abline(h=150,col=Col2,lwd=5)
lines(c(110,170),c(145,175),col=Col3,lwd=5)
abline(v=140,lty=2,lwd=3)
dev.off()



### Plasticity for hypotheses

pdf("TalkFigs/PlasticityHyp.pdf",width=5,height=5,pointsize=20)
par(mgp=c(0.5,1,0),mar=c(2,2,0.5,0.5))
plot(c(0,1),c(0,1),type="l",xlab="Timing of spring",ylab="Timing of parturition",xaxt='n',yaxt='n',lwd=8,col=Col1,bty="l")
#abline(a=0,b=0,col=Col2,lwd=thick)
#abline(a=0,b=0.5,col=Col3,lwd=thick)
#legend(legend=c("No","Some","A lot"),col=c(Col2,Col3,Col1),title="Plasticity:",x=0.05,y=1,lty=1,lwd=5)

dev.off()



### Mismatch for hypotheses

pdf("TalkFigs/MismatchHyp.pdf",width=5,height=5,pointsize=20)
par(mgp=c(0.5,1,0),mar=c(2,2,0.5,0.5))
plot(c(0,1),c(0,1),type="l",xlab="Timing of spring",ylab="Reproductive success (calf survival)",xaxt='n',yaxt='n',lwd=8,col="red",bty="l")
#abline(a=0,b=0,col=Col2,lwd=thick)
#abline(a=0,b=0.5,col=Col3,lwd=thick)
#legend(legend=c("No","Some","A lot"),col=c(Col2,Col3,Col1),title="Plasticity:",x=0.05,y=1,lty=1,lwd=5)

dev.off()
