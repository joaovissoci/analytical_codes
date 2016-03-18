#### ARIMA MODEL TRYOUT #####

# Time Series Plotting
library(ggplot2)
library(xts)
library(dygraphs)
 
# Get IBM and Linkedin stock data from Yahoo Finance
data2009<-read.csv("/Users/joaovissoci/Dropbox/datasets/DGHI/baltimore_gis/crashdata2009_datecoded.csv")
data2010<-read.csv("/Users/joaovissoci/Dropbox/datasets/DGHI/baltimore_gis/crashdata2010_datecoded.csv")
data2011<-read.csv("/Users/joaovissoci/Dropbox/datasets/DGHI/baltimore_gis/crashdata2011_datecoded.csv")
data2012<-read.csv("/Users/joaovissoci/Dropbox/datasets/DGHI/baltimore_gis/crashdata2012_datecoded.csv")
data2013<-read.csv("/Users/joaovissoci/Dropbox/datasets/DGHI/baltimore_gis/crashdata2013_datecoded.csv")

#############################################################################
#DATA MANAGEMENT
#############################################################################
data2009$date <- as.Date(as.character(data2009$date))
data2010$date <- as.Date(as.character(data2010$date))
data2011$date <- as.Date(as.character(data2011$date))
data2012$date <- as.Date(as.character(data2012$date))
data2013$date <- as.Date(as.character(data2013$date))

crashes2009<-table(data2009$date)
crashes2010<-table(data2010$date)
crashes2011<-table(data2011$date)
crashes2012<-table(data2012$date)
crashes2013<-table(data2013$date)

dates<-c(names(crashes2009),
		 names(crashes2010),
		 names(crashes2011),
		 names(crashes2012),
		 names(crashes2013))

crashes<-c(crashes2009,
		   crashes2010,
		   crashes2011,
		   crashes2012,
		   crashes2013)

# calculating Vunerable Road Users
data2009$vru<-with(data2009,rowSums(data.frame(PEDS,BIKES,MC))) 
data2010$vru<-with(data2010,rowSums(data.frame(PEDS,BIKES,MC))) 
data2011$vru<-with(data2011,rowSums(data.frame(PEDS,BIKES,MC))) 
data2012$vru<-with(data2012,rowSums(data.frame(PEDS,BIKES,MC))) 
data2013$vru<-with(data2013,rowSums(data.frame(PEDS,BIKES,MC))) 

vru2009<-prop.table(table(data2009$date,data2009$vru),1)[,2]
vru2010<-prop.table(table(data2010$date,data2010$vru),1)[,2]
vru2011<-prop.table(table(data2011$date,data2011$vru),1)[,2]
vru2012<-prop.table(table(data2012$date,data2012$vru),1)[,2]
vru2013<-prop.table(table(data2013$date,data2013$vru),1)[,2]

vru<-c(vru2009,
	   vru2010,
	   vru2011,
	   vru2012,
	   vru2013)

# calculating Distracted driving
data2009$distracteddriving<-with(data2009,rowSums(
	data.frame(SLEEPY,INATT,AGG))) 
data2010$distracteddriving<-with(data2010,rowSums(
	data.frame(SLEEPY,INATT,AGG))) 
data2011$distracteddriving<-with(data2011,rowSums(
	data.frame(SLEEPY,INATT,AGG))) 
data2012$distracteddriving<-with(data2012,rowSums(
	data.frame(SLEEPY,INATT,AGG))) 
data2013$distracteddriving<-with(data2013,rowSums(
	data.frame(SLEEPY,INATT,AGG))) 

distracteddriving2009<-prop.table(table(data2009$date,
	data2009$distracteddriving),1)[,2]
distracteddriving2010<-prop.table(table(data2010$date,
	data2010$distracteddriving),1)[,2]
distracteddriving2011<-prop.table(table(data2011$date,
	data2011$distracteddriving),1)[,2]
distracteddriving2012<-prop.table(table(data2012$date,
	data2012$distracteddriving),1)[,2]
distracteddriving2013<-prop.table(table(data2013$date,
	data2013$distracteddriving),1)[,2]

distracteddriving<-c(distracteddriving2009,
	   distracteddriving2010,
	   distracteddriving2011,
	   distracteddriving2012,
	   distracteddriving2013)

# calculating extreme extremeage
data2009$extremeage<-with(data2009,rowSums(
	data.frame(YOUNGER,OLDER)))
data2010$extremeage<-with(data2010,rowSums(
	data.frame(YOUNGER,OLDER))) 
data2011$extremeage<-with(data2011,rowSums(
	data.frame(YOUNGER,OLDER))) 
data2012$extremeage<-with(data2012,rowSums(
	data.frame(YOUNGER,OLDER))) 
data2013$extremeage<-with(data2013,rowSums(
	data.frame(YOUNGER,OLDER))) 

extremeage2009<-prop.table(table(data2009$date,
	data2009$extremeage),1)[,2]
extremeage2010<-prop.table(table(data2010$date,
	data2010$extremeage),1)[,2]
extremeage2011<-prop.table(table(data2011$date,
	data2011$extremeage),1)[,2]
extremeage2012<-prop.table(table(data2012$date,
	data2012$extremeage),1)[,2]
extremeage2013<-prop.table(table(data2013$date,
	data2013$extremeage),1)[,2]

extremeage<-c(extremeage2009,
	   extremeage2010,
	   extremeage2011,
	   extremeage2012,
	   extremeage2013)

# Speeding
speeding2009<-prop.table(table(data2009$date,
	data2009$SPEED),1)[,2]
speeding2010<-prop.table(table(data2010$date,
	data2010$SPEED),1)[,2]
speeding2011<-prop.table(table(data2011$date,
	data2011$SPEED),1)[,2]
speeding2012<-prop.table(table(data2012$date,
	data2012$SPEED),1)[,2]
speeding2013<-prop.table(table(data2013$date,
	data2013$SPEED),1)[,2]

speeding<-c(speeding2009,
	   speeding2010,
	   speeding2011,
	   speeding2012,
	   speeding2013)

# Impaired
impaired2009<-prop.table(table(data2009$date,
	data2009$IMPAIRED),1)[,2]
impaired2010<-prop.table(table(data2010$date,
	data2010$IMPAIRED),1)[,2]
impaired2011<-prop.table(table(data2011$date,
	data2011$IMPAIRED),1)[,2]
impaired2012<-prop.table(table(data2012$date,
	data2012$IMPAIRED),1)[,2]
impaired2013<-prop.table(table(data2013$date,
	data2013$IMPAIRED),1)[,2]

impaired<-c(impaired2009,
	   impaired2010,
	   impaired2011,
	   impaired2012,
	   impaired2013)


#############################################################

time_series_plot<-data.frame(crashes,dates=as.Date(dates))

ggplot(time_series_plot,aes(dates,crashes)) + 
  geom_line() +
#  geom_line(data=data2,aes(color="Speeding")) +
#  labs(color="Legend") +
#  scale_colour_manual("", breaks = c("Distracted Driving", "Speeding"),
#                          values = c("blue", "brown")) +
  #ggtitle("Closing Stock Prices: IBM & Linkedin") + 
  #scale_x_date(format = "%b-%Y") +
  theme(plot.title = element_text(lineheight=.7, face="bold"))

#### ARIMA
library(forecast)
timeseries_data <- ts(time_series_plot$crashes, 
	start=c(2009,1),frequency=365)
fit <- auto.arima(timeseries_data)
summary(fit)
forecast(fit,h=30)
plot(forecast(fit,h=30))

#BY Month
library(lubridate)

time_series_plot$date_month <- floor_date(time_series_plot$dates, 
	"month")

library(plyr)
time_series_month<-ddply(time_series_plot, "date_month", summarise, 
	crashes_month = sum(crashes))

ggplot(time_series_month,aes(date_month,crashes_month)) + 
  geom_line() +
#  geom_line(data=data2,aes(color="Speeding")) +
#  labs(color="Legend") +
#  scale_colour_manual("", breaks = c("Distracted Driving", "Speeding"),
#                          values = c("blue", "brown")) +
  #ggtitle("Closing Stock Prices: IBM & Linkedin") + 
  #scale_x_date(format = "%b-%Y") +
  theme(plot.title = element_text(lineheight=.7, face="bold"))

#### ARIMA
library(forecast)
timeseries_data <- ts(time_series_month$crashes_month, 
	start=c(2009,1),frequency=12)
fit <- auto.arima(timeseries_data)
summary(fit)
forecast(fit,h=20)
plot(forecast(fit,h=20))

####################################
# Time trend by category
####################################

time_series_plot<-data.frame(vru,distracteddriving,
	extremeage,speeding,impaired,dates=as.Date(dates))

#ggplot(time_series_plot,aes(dates,vru)) + 
#  geom_line() +
#  geom_line(data=data2,aes(color="Speeding")) +
#  labs(color="Legend") +
#  scale_colour_manual("", breaks = c("Distracted Driving", "Speeding"),
#                          values = c("blue", "brown")) +
  #ggtitle("Closing Stock Prices: IBM & Linkedin") + 
  #scale_x_date(format = "%b-%Y") +
#  theme(plot.title = element_text(lineheight=.7, face="bold"))

#### ARIMA
#library(forecast)
#timeseries_data <- ts(time_series_plot$crashes, 
#	start=c(2009,1),frequency=365)
#fit <- auto.arima(timeseries_data)
#summary(fit)
#forecast(fit,h=30)
#plot(forecast(fit,h=30))

#BY Month
library(lubridate)

time_series_plot$date_month <- floor_date(time_series_plot$dates, 
	"month")

library(plyr)
time_series_month<-ddply(time_series_plot, "date_month", summarise, 
	vru_month = mean(vru),
	speed_month = mean(speeding),
	distracted_month=mean(distracteddriving),
	extremeage_month=mean(extremeage),
	impaired_month=mean(impaired))

ggplot(time_series_month,aes(date_month,vru_month)) + 
  geom_line(aes(colour="VRU")) +
  geom_line(data=time_series_month,
  	aes(date_month,speed_month,color="Speeding")) +
  geom_line(data=time_series_month,
  	aes(date_month,distracted_month,color="Distracted")) +
  geom_line(data=time_series_month,
  	aes(date_month,extremeage_month,color="Extreme Ages")) +
  geom_line(data=time_series_month,
  	aes(date_month,impaired_month,color="Impaired")) +
  labs(color="Legend") +
#  scale_colour_manual("", breaks = c("Distracted Driving", "Speeding"),
#                          values = c("blue", "brown")) +
  #ggtitle("Closing Stock Prices: IBM & Linkedin") + 
  #scale_x_date(format = "%b-%Y") +
  theme(plot.title = element_text(lineheight=.7, face="bold"))

#### ARIMA
library(forecast)
timeseries_data <- ts(time_series_month$crashes_month, 
	start=c(2009,1),frequency=12)
fit <- auto.arima(timeseries_data)
summary(fit)
x<-forecast(fit,h=20)
plot(forecast(fit,h=20))

mean(x$mean)









# Plot with the htmlwidget dygraphs
# dygraph() needs xts time series objects
ibm_xts <- xts(data1$outcome,order.by=data1$date,frequency=365)
lnkd_xts <- xts(data2$outcome,order.by=data2$date,frequency=365)
 
stocks <- cbind(ibm_xts,lnkd_xts)
 
dygraph(stocks,ylab="$ crashes", 
        main="Crashes in Baltimore from 2009") %>%
  dySeries("..1",label="Distracted driving") %>%
  dySeries("..2",label="Speeding") %>%
  dyOptions(colors = c("blue","brown")) %>%
  dyRangeSelector()



outcomedata2009<-prop.table(table(data$date,data$INATT),1)[,2]
outcome2<-prop.table(table(data$date,data$SPEED),1)[,2]
date1<-as.Date(names(outcome1))
date2<-as.Date(names(outcome2))

data1<-data.frame(outcome=outcome1,date=date1)
data2<-data.frame(outcome=outcome2,date=date2)

#ibm  <- yahoo.read(ibm_url)
#lnkd2 <- yahoo.read(lnkd_url)


ggplot(data1,aes(date,outcome)) + 
  geom_line(aes(color="Distracted driving")) +
  geom_line(data=data2,aes(color="Speeding")) +
  labs(color="Legend") +
  scale_colour_manual("", breaks = c("Distracted Driving", "Speeding"),
                          values = c("blue", "brown")) +
  #ggtitle("Closing Stock Prices: IBM & Linkedin") + 
  #scale_x_date(format = "%b-%Y") +
  theme(plot.title = element_text(lineheight=.7, face="bold"))

# Plot with the htmlwidget dygraphs
# dygraph() needs xts time series objects
ibm_xts <- xts(data1$outcome,order.by=data1$date,frequency=365)
lnkd_xts <- xts(data2$outcome,order.by=data2$date,frequency=365)
 
stocks <- cbind(ibm_xts,lnkd_xts)
 
dygraph(stocks,ylab="$ crashes", 
        main="Crashes in Baltimore from 2009") %>%
  dySeries("..1",label="Distracted driving") %>%
  dySeries("..2",label="Speeding") %>%
  dyOptions(colors = c("blue","brown")) %>%
  dyRangeSelector()

#### TRYOUT 2


#### WAVELET TRYOUT #####

data<-read.csv("/Users/joaovissoci/Desktop/Vulnerabilidade_pointsbypolygons.csv")

data<-read.csv("/home/joao/Desktop/Vulnerabilidade_pointsbypolygons.csv")

temp_data<-with(data,c(1000*(vuln2013/POP2010),
	1000*(vulner2012/POP2010),
	1000*(vulner2011/POP2010),
	1000*(vulner2010/POP2010),
	1000*(vulner2009/POP2010)))
year<-c(rep(200))

#temp_data<-t(temp_data)
#colnames(temp_data)<-c(1:200)
time_series_data<-as.ts(t(temp_data))

timeseries_data <- ts(temp_data, start=c(2009,1),frequency=199)
birthstimeseriescomponents <- decompose(timeseries_data)
plot(birthstimeseriescomponents)

# Seasonal decomposition
fit <- stl(timeseries_data, s.window="period")
plot(fit)

# additional plots
monthplot(timeseries_data)
library(forecast)
seasonplot(timeseries_data,col=rainbow(12),year.labels=TRUE)




plot(timeseries_data, type="b")
diff12 = diff(time_series_data)
x<-acf(timeseries_data,type="partial",lag.max=199)