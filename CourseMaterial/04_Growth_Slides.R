## Growth Slides
library(FSA)
library(FSAdata)

data(TroutBR)
rbt <- Subset(TroutBR,species=="Rainbow")

## simple fit with means
vb1 <- vbFuns("typical",msg=TRUE)
svb1 <- vbStarts(tl~age,data=rbt,type="typical")
fit1 <- nls(tl~vb1(t=age,Linf,K,t0),data=rbt,start=svb1)
windows(5,5); par(mar=c(3,3,0.25,0.25),mgp=c(1.75,0.5,0),tcl=-0.2)
plot(tl~age,data=rbt,pch=16,col=rgb(0,0,0,0.05),main="",
        xlab="Age (years)",ylab="Total Length (in)")
sum1 <- Summarize(tl~age,data=rbt)
points(mean~fact2num(age),data=sum1,pch="+",col="blue",cex=1.5)
curve(vb1(x,Linf=coef(fit1)),from=3,to=10,lwd=2,col="red",add=TRUE,n=500)

## typical fit with parameters shown
windows(5,5); par(mar=c(3,3,0.25,0.25),mgp=c(1.75,0.5,0),tcl=-0.2)
plot(tl~age,data=rbt,pch=16,col=rgb(0,0,0,0.05),main="",
     xlab="Age (years)",ylab="Total Length (in)",
     ylim=c(-2,32), xlim=c(1.5,10))
curve(vb1(x,Linf=coef(fit1)),from=0,to=10,lwd=2,col="red",add=TRUE,n=500)
Linf <- coef(fit1)[1]; to <- coef(fit1)[3]
lines(c(to,to),c(-5,2),lwd=2,lty=3,col="blue")
lines(c(1,2),c(0,0),lwd=2,lty=3,col="blue")
points(to,0,col="blue",pch=19,cex=1.25)
text(to,-4.5,expression(t[o]),xpd=TRUE,col="blue",cex=1.25)
abline(h=Linf,lwd=2,lty=3,col="blue")
text(0.8,Linf,expression(L[infinity]),xpd=TRUE,col="blue",cex=1.25)

## Francis fit with parameters shown
ages <- c(3,5.5,8)
vb2 <- vbFuns("Francis")
sv2 <- vbStarts(tl~age,data=rbt,type="Francis",tFrancis=ages)
fit2 <- nls(tl~vb2(age,L1,L2,L3,t1=ages[1],t2=ages[2],t3=ages[3]),
                  data=rbt,start=sv2)
plot(tl~age,data=rbt,pch=16,col=rgb(0,0,0,0.05),main="",
     xlab="Age (years)",ylab="Total Length (in)",
     ylim=c(3,32), xlim=c(2,10),xaxt="n",yaxt="n")
axis(1,c(2,10))
axis(2,c(5,30))
curve(vb2(x,L1=coef(fit2),t1=ages),from=0,to=10,lwd=2,col="red",add=TRUE,n=500)
L1 <- coef(fit2)[1]; L2 <- coef(fit2)[2]; L3 <- coef(fit2)[3]
lines(c(ages[1],ages[1]),c(0,L1),lwd=2,lty=3,col="blue")
lines(c(2,ages[1]),c(L1,L1),lwd=2,lty=3,col="blue")
points(ages[1],L1,col="blue",pch=19,cex=1.25)
lines(c(ages[2],ages[2]),c(0,L2),lwd=2,lty=3,col="blue")
lines(c(2,ages[2]),c(L2,L2),lwd=2,lty=3,col="blue")
points(ages[2],L2,col="blue",pch=19,cex=1.25)
lines(c(ages[3],ages[3]),c(0,L3),lwd=2,lty=3,col="blue")
lines(c(2,ages[3]),c(L3,L3),lwd=2,lty=3,col="blue")
points(ages[3],L3,col="blue",pch=19,cex=1.25)
axis(1,at=ages,label=c(expression(t[1]),expression(t[2]),expression(t[3])),cex.axis=1.25,col.axis="blue")
axis(2,at=coef(fit2),label=c(expression(L[1]),expression(L[2]),expression(L[3])),cex.axis=1.25,las=1,col.axis="blue")
