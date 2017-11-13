### References and Tutorials

#http://www.r-bloggers.com/plotting-time-series-in-r-using-yahoo-finance-data/
#http://stats.stackexchange.com/questions/40905/arima-model-interpretation

###

# Time Series Decomposition
## A time series is a sum of Seasonality, Trend and Noise.
## This code will show you how to decompose a time series
## All decomposition is based on the nature of the time series:
## a) Additive (seasonality keeps constant over time);
## b) Multiplicative (seasonality increases over time)
## ref - https://anomaly.io/seasonal-trend-decomposition-in-r/

#Packages
install.packages("fpp") #Additive
install.packages("Ecdat") #Multiplicative
install.packages("forecast")

# 1. Trend

#Additive
library(fpp)
data(ausbeer)
timeserie_beer = tail(head(ausbeer, 17*4+2),17*4-4)
plot(as.ts(timeserie_beer))

library(forecast)
trend_beer = ma(timeserie_beer, order = 4, centre = T)
plot(as.ts(timeserie_beer))
lines(trend_beer)
plot(as.ts(trend_beer))

#Multiplicative
library(Ecdat)
data(AirPassengers)
timeserie_air = AirPassengers
plot(as.ts(timeserie_air))

install.packages("forecast")
library(forecast)
trend_air = ma(timeserie_air, order = 12, centre = T)
plot(as.ts(timeserie_air))
lines(trend_air)
plot(as.ts(trend_air))

# 2. Isolate seasonality

detrend_beer = timeserie_beer - trend_beer
plot(as.ts(detrend_beer))

m_beer = t(matrix(data = detrend_beer, nrow = 4))
seasonal_beer = colMeans(m_beer, na.rm = T)
plot(as.ts(rep(seasonal_beer,16)))

detrend_air = timeserie_air / trend_air
plot(as.ts(detrend_air))

m_air = t(matrix(data = detrend_air, nrow = 12))
seasonal_air = colMeans(m_air, na.rm = T)
plot(as.ts(rep(seasonal_air,12)))

# 3. Random or noise

random_beer = timeserie_beer - trend_beer - seasonal_beer
plot(as.ts(random_beer))

random_air = timeserie_air / (trend_air * seasonal_air)
plot(as.ts(random_air))

## Full decomposition with the decopmose() and STL()

#decompose()
ts_beer = ts(timeserie_beer, frequency = 4)
decompose_beer = decompose(ts_beer, "additive")
 
plot(as.ts(decompose_beer$seasonal))
plot(as.ts(decompose_beer$trend))
plot(as.ts(decompose_beer$random))
plot(decompose_beer)

ts_beer = ts(timeserie_beer, frequency = 4)
decompose_beer = decompose(ts_beer, "additive")
 
plot(as.ts(decompose_beer$seasonal))
plot(as.ts(decompose_beer$trend))
plot(as.ts(decompose_beer$random))
plot(decompose_beer)

ts_air = ts(timeserie_air, frequency = 12)
decompose_air = decompose(ts_air, "multiplicative")
 
plot(as.ts(decompose_air$seasonal))
plot(as.ts(decompose_air$trend))
plot(as.ts(decompose_air$random))
plot(decompose_air)

#STL()

ts_beer = ts(timeserie_beer, frequency = 4)
stl_beer = stl(ts_beer, "periodic")
seasonal_stl_beer   <- stl_beer$time.series[,1]
trend_stl_beer     <- stl_beer$time.series[,2]
random_stl_beer  <- stl_beer$time.series[,3]
 
plot(ts_beer)
plot(as.ts(seasonal_stl_beer))
plot(trend_stl_beer)
plot(random_stl_beer)
plot(stl_beer)

##### ARIMA

#seasonal figures
f <- decompose(AirPassengers)

plot(f$figure, type="b", xaxt="n", xlab="") 
monthNames <- months(ISOdate(2011,1:12,1))
axis(1, at=1:12, labels=monthNames, las=2)

plot(f)

fit <- arima(AirPassengers, order=c(1,0,0), 
	list(order=c(2,1,0), 
		period=12))

fore <- predict(fit, n.ahead=24)

# error bounds at 95% confidence level
U <- fore$pred + 2*fore$se
L <- fore$pred - 2*fore$se

ts.plot(AirPassengers, fore$pred, U, L, 
	col=c(1,2,4,4), lty = c(1,1,2,2))
legend("topleft", c("Actual", "Forecast", 
	"Error Bounds (95% Confidence)"), 
col=c(1,2,4), lty=c(1,1,2))

#ARIMA MODEL

ARIMA = p d q

p = 

d = numer of diferentiations needed to reach a stationary time series

q = 

skirtsseriesdiff1 <- diff(skirtsseries, differences=1)
plot.ts(skirtsseriesdiff1)

skirtsseriesdiff2 <- diff(skirtsseries, differences=2)
plot.ts(skirtsseriesdiff2)

If you need to difference your original time series data d times in order to obtain a stationary time series, this means that you can use an ARIMA(p,d,q) model for your time series, where d is the order of differencing used. For example, for the time series of the diameter of women’s skirts, we had to difference the time series twice, and so the order of differencing (d) is 2. This means that you can use an ARIMA(p,2,q) model for your time series. The next step is to figure out the values of p and q for the ARIMA model.



#### OUTRA
library(forecast)
fit <- auto.arima(WWWusage)
plot(forecast(fit,h=20))



#http://www.statoek.wiso.uni-goettingen.de/veranstaltungen/zeitreihen/sommer03/ts_r_intro.pdf

# Generating random data
sim.ar<-arima.sim(list(ar=c(0.4,0.4)),n=1000)
sim.ma<-arima.sim(list(ma=c(0.6,-0.4)),n=1000)

#Plotting autocorrelations (ACFs) and partial autocorrelations (PACF)
par(mfrow=c(2,2))
acf(sim.ar,main="ACF of AR(2) process")
acf(sim.ma,main="ACF of MA(2) process")
pacf(sim.ar,main="PACF of AR(2) process")
pacf(sim.ma,main="PACF of MA(2) process")

#Specify ARIMA model
data(LakeHuron)
fit<-arima(LakeHuron,order=c(1,0,1))

#Residual analysis for any sing of non-randomness
tsdiag(fit)

Box.test(fit$residuals,lag=1) #The Box–Pierce (and Ljung–Box) 
#test examines the Null of independently distributed
#residuals. It’s derived from the idea that the residuals of a 
#“correctly specified” model are independently distributed. 
#If the residuals are not, then they come from a miss–specified model.

#Prediction

#Prediction base on the model (fit) and 
#the number os years to be predicted (8) in n.ahead fun
LH.pred<-predict(fit,n.ahead=8)

#plotting
plot(LakeHuron,xlim=c(1875,1980),ylim=c(575,584))
LH.pred<-predict(fit,n.ahead=8)
lines(LH.pred$pred,col="red")
lines(LH.pred$pred+2*LH.pred$se,col="red",lty=3)
lines(LH.pred$pred-2*LH.pred$se,col="red",lty=3)

### Change Plot

set.seed(10)
m.data=c(rnorm(100,0,1),rnorm(100,1,1),rnorm(100,0,1),rnorm(100,0.2,1))
ts.plot(m.data,xlab='Index')

m.pelt=cpt.meanvar(timeseries_data,test.stat='Poisson',method='BinSeg')
plot(m.pelt,type='l',cpt.col='blue',xlab='Index',cpt.width=4)
cpts(m.pelt)

data(Lai2005fig4)
Lai.default=cpt.mean(Lai2005fig4[,5],method='PELT')
plot(Lai.default,pch=20,col='grey',cpt.col='black',type='p',xlab='Index')
cpts(Lai.default)

coef(Lai.default)

## Interrupted time series

intervention <- rep(c(0, 1), each = 100)

# set up an autocorrelation dataset
yprocess <- arima.sim(list(order = c(1, 0, 0), 
                                     ar = c(0.7)),
                       200)
yobs <- yprocess + rnorm(200, 0.5) * intervention

#Look at http://stats.stackexchange.com/questions/108374/arima-intervention-transfer-function-how-to-visualize-the-effect?rq=1
#Look at http://stats.stackexchange.com/questions/225138/method-for-quantifying-intervention-effect-in-time-series?rq=1

# naive way, doesn't account for autcorr
summary(lm(yobs ~ intervention))

# using arima
arima(yobs, c(1, 0, 0), xreg = intervention)

### Time series Clustering

# https://www.r-bloggers.com/time-series-analysis-and-mining-with-r/



