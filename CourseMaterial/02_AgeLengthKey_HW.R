
library(FSA)

setwd("C:/aaaWork/Web/fishR/Courses/MNAFS2013/CourseMaterial")
rb <- read.csv("RockBassLO2.csv",header=TRUE)
rb.len <- Subset(rb,is.na(age))
rb.age <- Subset(rb,!is.na(age))

rb.age <- lencat(~tl,data=rb.age,startcat=110,w=10)
( tbl <- with(rb.age,table(LCat,age)) )

( ak <- prop.table(tbl,margin=1) )

rb.len.mod <- ageKey(ak,age~tl,data=rb.len)
rb.len.mod <- lencat(~tl,data=rb.len.mod,startcat=110,w=10)
rb.comb <- rbind(rb.age,rb.len.mod)

( af <- table(rb.comb$age) )

hist(~age,data=rb.comb,breaks=3:11,xlab="Age (yrs)",col="gray90")

( lf <- table(rb.comb$LCat) )

( sl <- Summarize(tl~age,data=rb.comb,digits=1))

plot(tl~age,data=rb.comb,ylab="Total Length (mm)",xlab="Age",pch=16,col=rgb(0,0,0,0.05))
lines(mean~fact2num(age),data=sl,col="blue",lwd=2)


