#John Anthony Dobson
#Zambrano PS2
rm(list=ls())
library(rio)
temp = import('temp data.txt')
export(temp, 'temp.csv')
temp = read.csv('temp.csv')
df = read.csv('zdata.csv')
temp$Date = temp$V1
temp$Median = temp$V2

Trainingdf = df[,14:15]
Trainingdf = Trainingdf[1:125,]
Trainingdf$Temperature = temp$V2[1:125]

Testdf = df[,16:17]
Testdf = Testdf[1:42,]
Testdf$temp = temp$V2[126:167]

mod = lm(Trainingdf$Temperature ~ Trainingdf$Training.CO2)
summary(mod)
confidicenceinterval = c(mean(Trainingdf$Temperature) + 1.96*0.1295,mean(Trainingdf$Temperature) - 1.96*0.1295)

s = sqrt(mean((mod$fitted.values - Trainingdf$Temperature)^2))
smean = sqrt(mean((mod$fitted.values - Trainingdf$Temperature)^2))/sqrt(125)

sst = ((Testdf$Test.CO2 - mean(Trainingdf$Training.CO2))^2)
#Confidence Interval

0.0870885701 + (Testdf$Test.CO2[1]^2 * 9.571086e-07) + 2*Testdf$Test.CO2[1]*-0.0002884872

ygivenx = -2.907944 + (0.008888*Testdf$Test.CO2)
vyhat = 0.0870885701 + (Testdf$Test.CO2^2 * 9.571086e-07) + 2*Testdf$Test.CO2*-0.0002884872


#Sxx = std(x)^2 * (n-1)
Sxx = var(Trainingdf$Training.CO2)*124

z = 1.96
t=qt(.025,123,lower.tail = FALSE)
T = (Trainingdf$Temperature -  (-2.907944 + (0.008888*Trainingdf$Training.CO2)))/ (s * sqrt(1 + (1/125) + (sst/Sxx)))


#C # denotes first value
confidenceuncertantyupper = -2.907944279  + (0.008887751*Testdf$Test.CO2) + (t*s*sqrt((1/125)+(sst/Sxx)))
#0.09196507
confidenceuncertantlower = -2.907944279  + (0.008887751*Testdf$Test.CO2) - (t*s*sqrt((1/125)+(sst/Sxx)))
#-0.029406243


confidencecertantyupper = -2.907944279  + (0.008887751*Testdf$Test.CO2) + (z*smean)
# 0.05379484
confidencecertantylower = -2.907944279  + (0.008887751*Testdf$Test.CO2) - (z*smean)
#0.008763992

df3 = as.data.frame(rep(0,42))
df3$upper = confidencecertantyupper
df3$lower = confidencecertantylower

df3 = as.data.frame(rep(0,42))
df3$upper = confidenceuncertantyupper
df3$lower = confidenceuncertantlower
View(df3)
#D # denotes first value
predictioncertaintyupper = -2.907944279  + (0.008887751*Testdf$Test.CO2) + (z*s)
#0.2830095
predictioncertaintylower = -2.907944279  + (0.008887751*Testdf$Test.CO2) - (z*s)
#-0.2204506731
df3 = as.data.frame(rep(0,42))
df3$upper = predictioncertaintyupper
df3$lower = predictioncertaintylower
View(df3)
predictionuncertaintyupper = -2.907944279  + (0.008887751*Testdf$Test.CO2) + (t*s*sqrt(1+(1/125)+(sst/Sxx)))
#0.2926488
predictionuncertaintlower = -2.907944279  + (0.008887751*Testdf$Test.CO2) - (t*s*sqrt(1+(1/125)+(sst/Sxx)))
#-0.230089958 
df3 = as.data.frame(rep(0,42))
df3$upper = predictionuncertaintyupper
df3$lower = predictionuncertaintlower
View(df3)

length(which(predictionuncertaintyupper > Testdf$temp & predictionlower < Testdf$temp ))

c = 0
for (i in 1:42) {
  if(predictionuncertaintyupper[i] > Testdf$temp[i] & predictionlower[i] < Testdf$temp[i]){
    c = c+1
  }
}
c
#c reports 41


length(which(predictioncertaintyupper > Testdf$temp & predictionuncertaintlower < Testdf$temp ))

c = 0
for (i in 1:42) {
  if(predictionuncertaintyupper[i] > Testdf$temp[i] & predictionuncertaintlower[i] < Testdf$temp[i]){
    c = c+1
  }
}
c
#c reports 41


# E
Testdf2 = Testdf
Trainingdf2 = Trainingdf

Testdf2$COtwo = Testdf2$Test.CO2
Trainingdf2$COtwo = Trainingdf2$Training.CO2
Testdf2$Temp = Testdf2$temp
Trainingdf2$Temp = Trainingdf2$Temperature


mod2 = lm(Temp~COtwo, data = Trainingdf2)
trainingpredictionci = predict(mod2,Trainingdf2, interval = "confidence")
trainingpredictionpi = predict(mod2,Trainingdf2, interval = "prediction")
testpredictci = predict(mod2,Testdf2, interval = "confidence")
testpredictpi = predict(mod2,Testdf2, interval = "predict")

x = rbind(trainingpredictionpi, testpredictpi)
y = c((Trainingdf2$Temp),(Testdf2$Temp))
df3 = cbind(x,y)
df3 = as.data.frame(df3)
Date = c(Trainingdf2$Training.Year,Testdf2$Test.Year)
df3$Year = c(1850:2016)
fit = as.ts(df3$fit)
plot(df3$Year,df3$y, xlab = "Year",ylab = "deviations")
points(df3$Year[126:167],df3$y[126:167], xlab = "Year",ylab = "Deviations", cex = .75, col = 2)

lines(df3$Year[1:125],df3$fit[1:125], col = 1, cex = 3)
lines(df3$Year[126:167],df3$fit[126:167], col = 4)

lines(df3$Year[1:125],df3$lwr[1:125], col = 3)
lines(df3$Year[126:167],df3$upr[126:167], col = 2)

lines(df3$Year[126:167],df3$lwr[126:167], col = 2)
lines(df3$Year[1:125],df3$upr[1:125], col = 3)

legend("topleft", c("Confidence interval" , "Fitted Training", "Fitted Test", "Training Temp" , "Test Temp", "Prediction Interval"),lty = c(1,1,1,0,0,1), col = c(3,1,4,1,2,2),pch = c(NA,NA,NA,1,1,NA),cex =.75)
#F
qnorm(df3$y, mean = mean(df3$y), var(df3$y))
pnorm(df3$y,df3$fit,s)
#actual y, fitted y, rootmean squared error
pnorm(Testdf2$Temp,df3$fit[125:167],s)
pnorms = pnorm(df3$y[126:167],df3$fit[126:167],s)
pnorms = sort(pnorms)
PITvalues = pnorms
Probability = (1:42/42)
plot(PITvalues,Probability)
abline(0,1, col = 3)
hist(PITvalues)
