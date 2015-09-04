
source("c:/aaaWork/zGnrlLatex/knitr_setup.R")

library(FSA)

setwd("C:/aaaWork/Web/fishR/courses/MNAFS2013/CourseMaterial/")
df <- read.csv("MNBCData.csv",header=TRUE)
str(df)
levels(df$species)
blcT <- Subset(df,species=="BLC" & lake=="Talcot")
view(blcT)

lm.SL <- lm(radcap~lencap,data=blcT)
coef(lm.SL)
( a <- coef(lm.SL)[1] )
( b <- coef(lm.SL)[2] )
lm.LS <- lm(lencap~radcap,data=blcT)
coef(lm.LS)
( c <- coef(lm.LS)[1] )
( d <- coef(lm.LS)[2] )

plot(radcap~lencap,data=blcT,ylab="Scale Radius",xlab="Fish Length",pch=16) # Left
curve(a+b*x,from=110,to=300,col="red",lwd=2,add=TRUE)
plot(lencap~radcap,data=blcT,ylab="Fish Length",xlab="Scale Radius",pch=16) # Right
curve(c+d*x,from=1.8,to=6,col="red",lwd=2,add=TRUE)

blcT2 <- gReshape(blcT,in.pre="anu",last.plus="agecap")
str(blcT2)
view(blcT2)

blcT2 <- within(blcT2, lenFL <- (anu/radcap)*(lencap-c)+c)
view(blcT2)

blcT2 <- within(blcT2, lenSPH <- (anu/radcap)*(lencap+(a/b))-(a/b))
view(blcT2)

( sum1 <- Summarize(lenFL~age,data=blcT2,digits=2) )
( sum2 <- Summarize(lenFL~age*agecap,data=blcT2,digits=2) )

plot(mean~fact2num(age),data=sum1,type="l",lwd=2,ylim=c(40,280),
     ylab="Back-Calculated Length (mm)",xlab="Age")
lines(mean~fact2num(age),data=Subset(sum2,agecap==5),lwd=2,col="blue")
lines(mean~fact2num(age),data=Subset(sum2,agecap==4),lwd=2,col="green")
lines(mean~fact2num(age),data=Subset(sum2,agecap==3),lwd=2,col="red")
lines(mean~fact2num(age),data=Subset(sum2,agecap==2),lwd=2,col="orange")
points(mean~fact2num(age),data=Subset(sum2,agecap==1),pch=16,col="salmon")

sumTable(lenFL~agecap*age,data=blcT2,digits=2)

