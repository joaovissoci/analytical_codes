### Time series interrupted time series

library(TSA)
library(tseries)
library(forecast)



data=read.table("data.csv",sep=",",header=T)
head(data)

timeseries_data <- ts(data$prev_crashes_month, #time_series_month$impaired, 
                      start=c(2009,1),frequency=12)

plot(timeseries_data)
acf(timeseries_data)
pacf(timeseries_data)
#intervention<-c(rep(0,21),rep(1,39))
intervention<-c(rep(0,21),1:39)

fit<-arima(timeseries_data, order=c(1,0,2), seasonal=list(order=c(0,0,1)), xreg = intervention)
fit
acf(fit$residuals)
pacf(fit$residuals)

Box.test(fit$residuals, lag=15, type='Box-Pierce')

z=fit$residuals
hist(z,freq=F)
d = seq(range(z)[1]-3*sd(z),range(z)[2]+3*sd(z),0.001)
lines(d,dnorm(d,0,sd(z)))  

qqnorm(z)
qqline(z)

shapiro.test(z)
jarque.bera.test(z)

#modelo anterior
intervention<-c(rep(0,21),rep(1,39))

fit1<-arima(timeseries_data, xreg = intervention)
fit1

plot(timeseries_data,col=1,type='l')
lines(timeseries_data-fit$residuals,col='red',type='l')
lines(timeseries_data-fit1$residuals,col='blue',type='l')
