Practice Final
#1. Create a time series object with start date January 1950 and monthly frequency using this data.

#install.packages('forecast')
library('forecast')

df = read.csv('nino.csv')

nino = ts(df, start = 1950, frequency = 12)
auto.arima(nino)


#2. Plot the data.

plot(nino)

#3. Do you think it would be good to do a Box-Cox transformation? If so why? Do you think it would be better to analyze the differenced data? Why? Either way, for the rest of the problem use the original data (not transformed or differenced).
acf(nino)
pacf(nino)

#The data does not look exponential so I don't think a box-cox transformation will be helpful

#based on ACF and PACF I think we have an AR(3) model


#4. Smooth the data. Use a kernel or lowess smoother. Find an appropriate smoothing parameter. Plot the smoothed data and original data on the same axes. Do you notice any cyclicality with period longer than 1 year?
plot(nino,typle="l", xlab="Time", ylab="Nino")
lines(ksmooth(time(nino), nino, "normal", bandwidth=1), lwd=2, col=2)

#Yes there does appear to be cyclicality beyond one year, seems to cycle around twice a decade

#5. Plot a periodogram. Discuss any peaks that you notice. What do they tell you?

plot(nino, type="l", xlab="Frequency", ylab="Scaled Periodogram")


#6. Show a monthplot of the data. Do you see any seasonality?
plot(nino)
#7. Fit a regression model with monthly dummies and nothing else (no arima).
dum = factor(cycle(nino))
dummod = lm(nino ~ dum)

ts.plot(dummod$residuals)
#8. Fit several seasonal arima models and say which one you like best. Try with and without monthly dummies. Do monthly dummies help? What diagnostics are helpful?
fit01 = sarima(nino,0,0,1, 0,0,1,12)
fit02 = sarima(nino, 0,0,2, 0,0,1,12)
fit03 = sarima(nino,0,0,3, 1,0,1,12)
fit11 = sarima(nino,1,0,1, 0,0,1,12)
fit21 = sarima(nino,2,0,1,1,0,1,12 )
fit31 = sarima(nino,3,0,1,1,0,1,12 )
fit12 = sarima(nino,1,0,2,1,0,1,12 )
fit13 = sarima(nino, 1,0,3,1,0,1,12 )
fit22 = sarima(nino,2,0,2,1,0,1,12 )
fit33 = sarima(nino, 3,0,3,1,0,1,12 )
fit111 = sarima(nino, 1,1,1,1,0,1,12 )
fit211 = sarima(nino, 2,1,1,1,0,1,12 )
fit212 = sarima(nino,2,1,2,1,0,1,12 )
fit402 = sarima(nino,4,0,2,2,0,0,12 )

AIC(fit01, fit02,fit03,fit11,fit21,fit31,fit12,fit13,fit22,fit33,fit111, fit211,fit212)
