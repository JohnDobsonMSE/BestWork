rm(list=ls())
load("car.RData")
library(mlogit)
suppressMessages(library(AER))
data.for.logit = mlogit.data(data,chid.var="situation",choice="choice", alt.levels = c("o1","o2","o3"),shape ="long")
mod = mlogit(choice ~ price+cost+hybrid+electric+highperf+medperf+range| 0, data=data.for.logit)
1000*15*.0129

.3555/(-.0416)

(.477/-.0416)*1000

(.1099/-.417)*1000
  u1<-(10*-.0417)+(100*-.0129)+.3555+.1099
u2<-(15*-.0417)+(60*-.0129)-1.3924+.3841+(3*.477)
u1
u2
(exp(u1))/(exp(u1)+exp(u2))
