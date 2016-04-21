### References and Tutorials

#http://www.r-bloggers.com/plotting-time-series-in-r-using-yahoo-finance-data/
#http://stats.stackexchange.com/questions/40905/arima-model-interpretation

###

# Time Series Plotting
library(ggplot2)
library(xts)
library(dygraphs)
 
# Get IBM and Linkedin stock data from Yahoo Finance
ibm_url <- "http://real-chart.finance.yahoo.com/table.csv?s=IBM&a=07&b=24&c=2010&d=07&e=24&f=2015&g=d&ignore=.csv"
lnkd_url <- "http://real-chart.finance.yahoo.com/table.csv?s=LNKD&a=07&b=24&c=2010&d=07&e=24&f=2015&g=d&ignore=.csv"

yahoo.read <- function(url){
   dat <- read.table(url,header=TRUE,sep=",")
   df <- dat[,c(1,5)]
   df$Date <- as.Date(as.character(df$Date))
   return(df)}
 
ibm  <- yahoo.read(ibm_url)
lnkd2 <- yahoo.read(lnkd_url)

ggplot(ibm,aes(Date,Close)) + 
  geom_line(aes(color="ibm")) +
  geom_line(data=lnkd2,aes(color="lnkd")) +
  labs(color="Legend") +
  scale_colour_manual("", breaks = c("ibm", "lnkd"),
                          values = c("blue", "brown")) +
  ggtitle("Closing Stock Prices: IBM & Linkedin") + 
  theme(plot.title = element_text(lineheight=.7, face="bold"))

# Plot with the htmlwidget dygraphs
# dygraph() needs xts time series objects
ibm_xts <- xts(ibm$Close,order.by=ibm$Date,frequency=365)
lnkd_xts <- xts(lnkd2$Close,order.by=lnkd2$Date,frequency=365)
 
stocks <- cbind(ibm_xts,lnkd_xts)
 
dygraph(stocks,ylab="Close", 
        main="IBM and Linkedin Closing Stock Prices") %>%
  dySeries("..1",label="IBM") %>%
  dySeries("..2",label="LNKD") %>%
  dyOptions(colors = c("blue","brown")) %>%
  dyRangeSelector()

births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")

birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
birthstimeseries


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

#Another example

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