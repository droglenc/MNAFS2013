
library(FSA)

setwd("C:/aaaWork/Web/fishR/courses/MNAFS2013/CourseMaterial/")
d <- read.csv("AlewifeLH.csv",header=TRUE)
str(d)
view(d)

ac1 <- ageComp(otoliths~scales,data=d,col.lab="Otolith Age",row.lab="Scale Age")
summary(ac1,what="agreement")
summary(ac1,what="symmetry",flip.table=TRUE)
summary(ac1,what="bias")

plot(ac1)                    # LEFT
plot(ac1,what="difference")  # RIGHT

summary(ac1,what="detail")   #only a portion shown

view(ac1$detail)
summary(ac1,what="prec.stats")

