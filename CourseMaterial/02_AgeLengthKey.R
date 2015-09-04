
library(FSA)

setwd("C:/aaaWork/Web/fishR/courses/MNAFS2013/CourseMaterial/")
d <- read.csv("SpotVA2.csv",header=TRUE)
str(d)
view(d)

sp.len <- Subset(d,is.na(age))
str(sp.len)
sp.age <- Subset(d,!is.na(age))
str(sp.age)

Summarize(~tl,data=sp.age,digits=1)
sp.age.mod <- lencat(~tl,data=sp.age,startcat=6,w=1)
view(sp.age.mod)
( AL.raw <- table(sp.age.mod$LCat,sp.age.mod$age) )
( AL.key <- prop.table(AL.raw,margin=1) )

sp.len.mod <- ageKey(AL.key,age~tl,data=sp.len)
view(sp.len.mod)
sp.comb <- rbind(sp.age,sp.len.mod)
str(sp.comb)

agefreq <- table(sp.comb$age)
prop.table(agefreq)

( sp.sum <- Summarize(tl~age,data=sp.comb,digits=2) )
hist(~age,data=sp.comb,breaks=0:5,xlab="Age (yrs)",col="gray90")
plot(tl~age,data=sp.comb,ylab="Total Length (mm)",xlab="Age",pch=16,col=rgb(0,0,0,0.1))
lines(mean~fact2num(age),data=sp.sum,col="blue",lwd=2)

