## Age Comparison Slides

library(FSA)
library(FSAdata)
data(AlewifeLH)
str(AlewifeLH)
ac.ale <- ageComp(otoliths~scales,data=AlewifeLH,col.lab="Otolith Age",
                  row.lab="Scale Age")
summary(ac.ale,what="agreement")
summary(ac.ale,what="agreement",flip.table=TRUE)

windows(5,4); par(mar=c(3,3,0.25,0.25),mgp=c(1.75,0.5,0),tcl=-0.2)
plot(ac.ale)
plot(ac.ale,what="difference")

summary(ac.ale,what="bias")
summary(ac.ale,what="precision")
summary(ac.ale,what="detail")
