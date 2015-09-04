
library(FSA)

setwd("C:/aaaWork/Web/fishR/courses/MNAFS2013/CourseMaterial/")
d <- read.csv("MnFats.csv",header=TRUE)
str(d)
view(d)
nrow(d)

d[5,]
d[c(5,11,17),]
d$age
d$age[c(5,11,17)]

d03 <- Subset(d,year==2003)
nrow(d03)
dmale03 <- Subset(d,year==2003 & sex=="M")
nrow(dmale03)
d0003 <- Subset(d,year==2000 | year==2003)
nrow(d0003)
dMF <- Subset(d,sex!="UNK")
nrow(dMF)
dgt500 <- Subset(d,len>500)
nrow(dgt500)

d$loglen <- log(d$len)
d$logwt <- log(d$wt)
view(d)
d$fyear <- factor(d$year)
str(d)
d <- lencat(~len,data=d,startcat=290,w=10)
view(d)

Summarize(~age,data=d,digits=2)
Summarize(age~fyear,data=d,digits=2)

hist(~age,data=d)
hist(~age,data=d,xlab="Age",breaks=seq(5,35,2),col="gray90")
plot(wt~len,data=d,xlab="Total Length",ylab="Weight",pch=16)
plot(logwt~loglen,data=d,xlab="log Total Length",ylab="log Weight",pch=16)
plot(len~age,data=d,xlab="Age",ylab="Total Length",pch=16,col=rgb(0,0,0,0.25))

lm1 <- lm(logwt~loglen,data=d)
coef(lm1)
summary(lm1)
( p1 <- predict(lm1,data.frame(loglen=log(400))) )
exp(p1)

plot(wt~len,data=d,xlab="Total Length",ylab="Weight",pch=16)
(loga <- coef(lm1)[1])
( a <- exp(loga) )
( b <- coef(lm1)[2] )
curve(a*x^b,from=250,to=900,col="red",lwd=2,add=TRUE)

