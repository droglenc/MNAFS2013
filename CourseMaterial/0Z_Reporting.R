
# THIS CAN BE IGNORED IF RUNNING AS A SCRIPT ###################################
source("c:/aaaWork/zGnrlLatex/knitr_setup.R")
rqrd <- c("FSA")
# END OF SECTION TO BE IGNORED #################################################
library(FSA)      # catchCurve

df <- data.frame(age=0:5,catch=c(47,201,126,104,81,64))
df$logct <- log(df$catch)

ttl.catch <- sum(df$catch)
max.catch <- max(df$catch)
age.at.max.catch <- df$age[which(df$catch==max.catch)]

cc1 <- catchCurve(catch~age,data=df,ages=age.at.max.catch:5)
sum1 <- summary(cc1)
ci1 <- confint(cc1)

plot(cc1)

