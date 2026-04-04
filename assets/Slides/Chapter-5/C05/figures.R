x = (1:60)/6
s = 4
Q1 = qgamma(.25,shape=s)
h = dgamma(Q1,shape=s)
pdf("25Percentile.pdf",width=5,height=1.25)
par(mai=rep(.01,4))
plot(x,dgamma(x,shape=s),type="l",xlab="",ylab="",axes=F,ylim=c(-h*.15,h*1.05))
abline(h=0)
lines(rep(Q1,2),c(0,h))
arrows(Q1,-h*.25,Q1,-h*.05,length=0.1)
text(c(.75,1.5)*Q1,rep(.3,2)*h,c("25%","75%"))
dev.off()
#========================
barh = c(1.5,3.5,1.5,0.5)
nc = c(0,10,20,40,80)
barw = nc[-1] - nc[-length(nc)]
pdf("smokingtab.pdf",width=6,height=2.8)
par(mai=c(.6,.8,.02,0))
x = barplot(height=barh,width=barw,space=0,col=0,ylim=c(0,4.5),ylab="% per cigarette")
axis(side=1,pos=0,at=nc,label=nc,tck=-0.05)
mtext("Number of cigarettes",side=1,line=2)
text(x,barh+.3,paste("(",barh,")",sep=""))
d = .6
st = 4.5
text(rep(52,6),st-c(.3,1:5)*d,labels=c("point on", "horizontal axis",10:13),cex=1)
text(rep(70,2),st-c(.3,1)*d,labels=c("area to", "the left of it"),cex=1)
text(rep(70,4),st-(2:5)*d,labels=paste("15%",c("","+ 3.5%","+ 7.0%","+ 10.5%")),cex=1)
lines(c(42,80),rep(st-1.5*d,2))
dev.off()
#========================
HANES = c(0,.1,.25,1,1.5,3.5,6.76,10.25,17,19,23,19.5,18,10.75,7,3.25,2,.5,.25)
HANES = HANES/sum(HANES)
m = sum(HANES*(54:72))+.5
s = sqrt(sum(HANES*(54:72-m+.5)^2))
x = (-35:35)/10
pdf("HANESh.pdf",width=6,height=2.5)
par(mai=c(.6,.01,.01,.01))
imp = c(rep(0,8),rep(1,5),rep(0,6))
barplot(height=HANES,space=0,axes=F,xlim=c(.5,20.5),density=imp*20,col=imp)
lines(x*s+m-54,dnorm(x)/s)
axis(side=1,pos=0,at=.5+(0:8)*2.5,label=53.5+(0:8)*2.5,tck=-0.04)
mtext("HEIGHT (INCHES)",side=1,line=2)
text(rep(17,2),c(1,0)*.015+HANES[14],labels=c("Approximating","normal curve"))
text(rep(3,3),c(1,0,-1.15)*.015+HANES[9],labels=c("Histogram of heights","of women in HANES","(1976-1980)"))
arrows(c(15,5.9),c(HANES[14]+.008,HANES[9]),c(13.5,7.8),c(HANES[14]+.008,HANES[9]),length=0.1)
dev.off()
#========================
x = (-38:38)/10
pdf("normal.pdf",width=5,height=2.2)
par(mai=c(.78,.8,.14,.0))
plot(x,100*dnorm(x),xlab="STANDARD UNITS",ylab="",type="l",axes=F)
axis(side=1,at=-4:4,label=-4:4,tck=-0.04)
axis(side=2,at=c(0,20,40),label=c(0,20,40),tck=-0.04)
mtext(side=2,"PERCENT PER",line=3)
mtext(side=2,"STANDARD UNITS",line=2)
dev.off()
#========================
x = (-38:38)/10
pdf("6895.pdf",width=5,height=3)
par(mai=c(.01,.01,.01,.01))
plot(x,100*dnorm(x),xlab="",ylab="",type="l",axes=F,ylim=c(-35,38),xlim=c(-3.83,4.5))
x0 = (-10:10)/10
polygon(c(x0[1],x0,x0[length(x0)]),c(0,100*dnorm(x0),0),density=20)
axis(side=1,pos=0,at=-4:4,label=-4:4)
d0=-15
d1=8
pox = c(1:3,-(1:3))
poy = rep(d0-(0:2)*d1,2)
text(rep(0,3),poy,paste(c(68.27,95.45,99.73),"%",sep=""))
text(rep(3.1,4),d0-c(0,1,2,2.6)*d1,c("~ 68%","~ 95%","~ All but","   1/4 of 1%"),pos=4)
d2=.6
arrows(c(rep(1,3),rep(-1,3))*d2,poy,pox,poy,angle=15,length=.1)
for(i in 1:6){lines(rep(pox[i],2),c(-10,poy[i]-2))}
dev.off()
#========================
NormPlot <- function(from, to, lab,pdffilename){
pdf(pdffilename,width=2,height=1.35)
par(mai=c(.01,.01,0,.01))
plot((-15:15)/5,dnorm((-15:15)/5),xlab="",ylab="",type="l",axes=F,lwd=2,ylim=c(-.3,1)*dnorm(0))
lines(c(-3.2,3.2),rep(0,2),lwd=2)
x = seq(from, to, by=.2)
polygon(c(from,x,to),c(0,dnorm(x),0),lwd=2,density=20)
axis(side=1,pos=0,at=lab,label=F)
text(lab,rep(-.25*dnorm(0),length(lab)),labels=lab,cex=1.5)
dev.off()
}
NormPlot(-3.2,3.2,c(-4),"normAll.pdf")
NormPlot(-1,1,c(-1,1),"normm11.pdf")
NormPlot( 1,2,c( 1,2),"norm12.pdf")
NormPlot(-2,2,c(-2,2),"normm22.pdf")
NormPlot( 1,3.2,c(1),"norm1i.pdf")
NormPlot( 2,3.2,c(2),"norm2i.pdf")
NormPlot(-1,2,c(-1,2),"normm12.pdf")
NormPlot(-1,0,c(-1,0),"normm10.pdf")
NormPlot( 0,2,c(0,2),"norm02.pdf")

pdf("normm11c.pdf",width=2,height=1.35)
par(mai=c(.01,.01,0,.01))
plot((-15:15)/5,dnorm((-15:15)/5),xlab="",ylab="",type="l",axes=F,lwd=2,ylim=c(-.3,1)*dnorm(0))
lines(c(-3.2,3.2),rep(0,2),lwd=2)
x=(5:16)/5
lab=c(-1,1)
polygon(c(x[1],x,x[length(x)]),c(0,dnorm(x),0),density=20,lwd=2)
polygon(-c(x[1],x,x[length(x)]),c(0,dnorm(-x),0),density=20,lwd=2)
axis(side=1,pos=0,at=lab,label=F)
text(lab,rep(-.25*dnorm(0),length(lab)),labels=lab,cex=1.5)
dev.off()
#========================
pdf("NormTab.pdf",width=2.7,height=1.35)
par(mai=c(.01,.01,0,.01))
plot((-15:15)/5,dnorm((-15:15)/5),xlab="",ylab="",type="l",axes=F,lwd=2,ylim=c(-.3,1)*dnorm(0),xlim=c(-5,3))
lines(c(-3.2,3.2),rep(0,2),lwd=2)
lab=c(-1.4,1.4)
from = -1.4
to = 1.4
x = seq(from, to, by=.2)
polygon(c(from,x,to),c(0,dnorm(x),0),density=20,lwd=2)
axis(side=1,pos=0,at=lab,label=F)
text(lab,rep(-.25*dnorm(0),length(lab)),labels=c("-z","z"),cex=1.5)
d = .2
text(-1.4,d,label="Area (percent)",pos=2)
arrows(-1.6,d,-.2,d,length=.05,lwd=2)
dev.off()
#========================
pdf("Norm3Q.pdf",width=2.6,height=1.35)
par(mai=c(.01,.01,0,.01))
plot((-15:15)/5,dnorm((-15:15)/5),xlab="",ylab="",type="l",axes=F,lwd=2,ylim=c(-.3,1)*dnorm(0),xlim=c(-4,4))
lines(c(-3.2,3.2),rep(0,2),lwd=2)
x=c((-16:3)/5,qnorm(.75))
polygon(c(x[1],x,qnorm(.75)),c(0,dnorm(x),0),density=20,lwd=2)
axis(side=1,pos=0,at=qnorm(.75),label=F)
text(qnorm(.75),-.25*dnorm(0),labels="?",cex=1.5)
d = .2
text(-1.4,d,label="Area 75%",pos=2)
arrows(-1.6,d,-.2,d,length=.05,lwd=2)
dev.off()
#========================
pdf("Norm3Q2.pdf",width=4,height=2)
par(mai=c(.01,.01,0,.01))
plot((-15:15)/5,dnorm((-15:15)/5),xlab="",ylab="",type="l",axes=F,lwd=2,ylim=c(-.28,1)*dnorm(0),xlim=c(-3.8,3.8))
lines(c(-3.2,3.2),rep(0,2),lwd=2)
x=c(-qnorm(.75),(-3:3)/5,qnorm(.75))
polygon(c(x[1],x,qnorm(.75)),c(0,dnorm(x),0),density=20,lwd=2)
axis(side=1,pos=0,at=c(-qnorm(.75),qnorm(.75)),label=F)
d = .15
s = .03
d3 = .35
ax0 = 1.6
ax1 = 1
text(c(-1,1)*qnorm(.75),rep(-.25*dnorm(0),2),labels=c("-?","?"),cex=1.5)
text( ax0,d+s,label="Step 1.",pos=4)
text( ax0,d-s,label="Area ______%",pos=4)
text(-ax0,d+s,label="Step 2.        ",pos=2)
text(-ax0,d-s,label="Area ______%",pos=2)
text(-.7,d3+s,label="Step 3.        ",pos=2)
text(-.7,d3-s,label="Area ______%",pos=2)
text(qnorm(.75),-.12*dnorm(0),label="Step 4.        ",pos=4)
text(qnorm(.75),-.27*dnorm(0),label=" = ______",pos=4)
arrows(c(-ax0,ax0,-.7),c(d,d,d3),c(-ax1,ax1,-.1),c(d,d,d3),length=.05,lwd=2)
dev.off()

#========================
pdf("SU.pdf",width=6,height=1.2)
par(mai=c(.2,.0,.0,.0))
plot(-3:3,rep(0,7),type="n",axes=F,ylab="",xlab="",xlim=c(-5,5.1),ylim=c(-0.22,1.25))
axis(side=1,pos=0,at=c(-1,0,2),label=c(-1,0,2),tick=F,col.axis=2)
axis(side=1,pos=0,at=c(-3,-2.4,-2:3),label=c("","-2.4",rep("",6)),tck=-0.1)
axis(side=1,pos=1,at=-2.4,label=57.5,tck=-0.1,col=2,col.axis=2)
axis(side=1,pos=1,at=-3:3,label=c("","",61,63.5,"",68.5,""),tck=-0.1)
x = c(-2.4,-1,0,2)
yu = .35
yl = .1
arrows(x,c(yl,rep(yu,3)),x,c(yu,rep(yl,3)),length=.05)
lh = .15
mk = 3.05
text(rep( mk,2),c(1,0)+lh,labels=paste("Average = ",c("63.5\"","0")),pos=4)
text(rep(-mk,2),c(1,0)+lh,labels=c("Height","Standard Units"),pos=2)
text(rep( mk,2),c(1,0)-lh,labels=paste("SD = ",c("2.5\"","1")),pos=4)
text(rep(-mk,2),c(1,0)-lh,labels=c("(inches)","(Dimensionless)"),pos=2)
dev.off()
#========================
HANES = c(0,.1,.25,1,1.5,3.5,6.76,10.25,17,19,23,19.5,18,10.75,7,3.25,2,.5,.25)
HANES = HANES[3:18]/sum(HANES[3:18])
m = sum(HANES*(55:70))+.5
s = sqrt(sum(HANES*(55:70-m+.5)^2))
x = (-30:30)/10
pdf("NorApp.pdf",width=6,height=2)
par(mai=c(.02,.01,.01,.01))
imp = c(rep(0,6),rep(1,7),rep(0,3))
d2 = 0.12
barplot(height=HANES,space=0,axes=F,xlim=8.5+c(-5,5.1)*2.5,ylim=c(-d2,.162),col=imp,density=imp*20)
lines(x*s+8.5,dnorm(x)/s)
polygon(c(13,13,13.5,13.5),c(0,HANES[14],HANES[14],0),density=20,border=NA)
d1 = d2*.73
d = d1/4
x0 = c(-1,0,2)
axis(side=1,pos=0,at=8.5+(-3:3)*2.5,label=F,tck=-0.04)
axis(side=1,pos=d1/5,at=8.5+x0*2.5,label=63.5+x0*2.5,tick=F)
axis(side=1,pos=-d1,at=8.5+(-3:3)*2.5,label=F,tck=-0.04)
axis(side=1,pos=-d1+d1/5,at=8.5+x0*2.5,label=x0,tick=F)
arrows(8.5+2.5*x0,rep(-.55*d1,3),8.5+2.5*x0,rep(-.9*d1,3),length=.1,angle=15)
lh = d1*.15
mk = 3.05*2.5
text(rep(8.5+mk,2),c(0,-d1)+lh,labels=paste("Average = ",c("63.5\"","0")),pos=4)
text(rep(8.5-mk,2),c(0,-d1)+lh,labels=c("Height","Standard Units"),pos=2)
text(rep(8.5+mk,2),c(0,-d1)-lh,labels=paste("SD = ",c("2.5\"","1")),pos=4)
text(rep(8.5-mk,2),c(0,-d1)-lh,labels=c("(inches)","(Dimensionless)"),pos=2)
dev.off()
#========================
pdf("NorApp2.pdf",width=5,height=2.3)
par(mai=c(.02,.01,.01,.01))
x = (-15:15)/5
plot(x,dnorm(x)/dnorm(0)*.6,axes=F,xlim=c(-2.75,5.3),ylim=c(-1.3,.95*.6),type="l")
lines(c(-3,3),rep(0,2))
x1 = (-15:(-5))/5
polygon(c(x1[1],x1,x1[length(x1)]),c(0,dnorm(x1)/dnorm(0)*.6,0),density=20)
d1 = .6
d = d1/4
x0 = c(-1,0)
lines(c(-3,3),rep(-d1,2))
axis(side=1,pos=0,  at=x0,label=c(66,69))
axis(side=1,pos=-d1,at=x0,label=F)
arrows(x0,rep(-.55*d1,3),x0,rep(-.9*d1,3),length=.1,angle=15)
lh = d1/8
mk = 3
text(rep(mk,2),c(lh,-lh),labels=c("Height (inches)", "Ave = 69\", SD = 3\""),pos=4)
text(mk,-d1,labels="Standard units",pos=4)
text(0,-1.3,labels="Answer = 16% (see page Ch05 - 10)")
arrows(-1.8,.2,-1.2,.2,length=.05)
text(-1.8,.2,labels="?",pos=2)
dev.off()
#========================
pdf("NorApp3.pdf",width=5,height=1.8)
par(mai=c(.02,.01,.01,.01))
x = (-15:15)/5
plot(x,dnorm(x)/dnorm(0)*.6,axes=F,xlim=c(-2.75,5.3),ylim=c(-.8,.95*.6),type="l")
lines(c(-3,3),rep(0,2))
x1 = c(qnorm(.95),(9:15)/5)
polygon(c(x1[1],x1,x1[length(x1)]),c(0,dnorm(x1)/dnorm(0)*.6,0),density=20)
d1 = .7
d = d1/4
x0 = c(-1,0,1)*qnorm(.95)
lines(c(-3,3),rep(-d1,2))
axis(side=1,pos=0,  at=x0,label=c("","69","?"))
axis(side=1,pos=-d1,at=x0,label=F)
arrows(x0[3],rep(-.9*d1,1),x0[3],rep(-.5*d1,1),length=.1,angle=15)
lh = d1/10
mk = 3
text(rep(mk,2),c(lh,-lh),labels=c("Height (inches)", "Ave = 69\", SD = 3\""),pos=4)
text(mk,-d1,labels="Standard units",pos=4)
arrows(2.3,.22*.6,1.8,.08*.6,length=.05)
text(2.2,.25*.6,labels="5%",pos=4)
dev.off()
#========================
pdf("NorApp4.pdf",width=5,height=1.8)
par(mai=c(.02,.01,.01,.01))
x = (-15:15)/5
plot(x,dnorm(x)/dnorm(0)*.6,axes=F,xlim=c(-2.75,5.3),ylim=c(-.8,.95*.6),type="l")
lines(c(-3,3),rep(0,2))
x1 = c(qnorm(.95),(9:15)/5)
d1 = .7
d = d1/4
x0 = 0
lines(c(-3,3),rep(-d1,2))
axis(side=1,pos=0,  at=x0,label="Ave")
axis(side=1,pos=-d1,at=x0,label="0")
lh = d1/10
mk = 3
text(rep(mk,2),c(lh,-lh),labels=c("Name? (units?)", "Ave = ?, SD = ?"),pos=4)
text(mk,-d1,labels="Standard units",pos=4)
dev.off()
