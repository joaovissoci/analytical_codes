#### ARIMA MODEL TRYOUT #####

# Time Series Plotting
library(ggplot2)
library(xts)
library(dygraphs)
library(forecast)
library(lubridate)
library(plyr)
library(reshape)
library(reshape2)

 
# Get data
data2009<-read.csv("/Users/jnv4/OneDrive - Duke University/datasets/Global EM/baltimore_gis/paper 1/crashdata2009_datecoded.csv")
data2010<-read.csv("/Users/jnv4/OneDrive - Duke University/datasets/Global EM/baltimore_gis/paper 1/crashdata2010_datecoded.csv")
data2011<-read.csv("/Users/jnv4/OneDrive - Duke University/datasets/Global EM/baltimore_gis/paper 1/crashdata2011_datecoded.csv")
data2012<-read.csv("/Users/jnv4/OneDrive - Duke University/datasets/Global EM/baltimore_gis/paper 1/crashdata2012_datecoded.csv")
data2013<-read.csv("/Users/jnv4/OneDrive - Duke University/datasets/Global EM/baltimore_gis/paper 1/crashdata2013_datecoded.csv")

#Data on points vs. polygons by category
all_data<-rbind(
	with(data2009,data.frame(date,REPORT_NO)),
	with(data2010,data.frame(date,REPORT_NO)),
	with(data2011,data.frame(date,REPORT_NO)),
	with(data2012,data.frame(date,REPORT_NO)),
	with(data2013,data.frame(date,REPORT_NO)))

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

vru2009<-table(data2009$date,data2009$vru)[,2]
vru2010<-table(data2010$date,data2010$vru)[,2]
vru2011<-table(data2011$date,data2011$vru)[,2]
vru2012<-table(data2012$date,data2012$vru)[,2]
vru2013<-table(data2013$date,data2013$vru)[,2]

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

distracteddriving2009<-table(data2009$date,
	data2009$distracteddriving)[,2]
distracteddriving2010<-table(data2010$date,
	data2010$distracteddriving)[,2]
distracteddriving2011<-table(data2011$date,
	data2011$distracteddriving)[,2]
distracteddriving2012<-table(data2012$date,
	data2012$distracteddriving)[,2]
distracteddriving2013<-table(data2013$date,
	data2013$distracteddriving)[,2]

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

extremeage2009<-table(data2009$date,
	data2009$extremeage)[,2]
extremeage2010<-table(data2010$date,
	data2010$extremeage)[,2]
extremeage2011<-table(data2011$date,
	data2011$extremeage)[,2]
extremeage2012<-table(data2012$date,
	data2012$extremeage)[,2]
extremeage2013<-table(data2013$date,
	data2013$extremeage)[,2]

extremeage<-c(extremeage2009,
	   extremeage2010,
	   extremeage2011,
	   extremeage2012,
	   extremeage2013)

# Speeding
speeding2009<-table(data2009$date,
	data2009$SPEED)[,2]
speeding2010<-table(data2010$date,
	data2010$SPEED)[,2]
speeding2011<-table(data2011$date,
	data2011$SPEED)[,2]
speeding2012<-table(data2012$date,
	data2012$SPEED)[,2]
speeding2013<-table(data2013$date,
	data2013$SPEED)[,2]

speeding<-c(speeding2009,
	   speeding2010,
	   speeding2011,
	   speeding2012,
	   speeding2013)

# Impaired
impaired2009<-table(data2009$date,
	data2009$IMPAIRED)[,2]
impaired2010<-table(data2010$date,
	data2010$IMPAIRED)[,2]
impaired2011<-table(data2011$date,
	data2011$IMPAIRED)[,2]
impaired2012<-table(data2012$date,
	data2012$IMPAIRED)[,2]
impaired2013<-table(data2013$date,
	data2013$IMPAIRED)[,2]

impaired<-c(impaired2009,
	   impaired2010,
	   impaired2011,
	   impaired2012,
	   impaired2013)

# Severity
data2009$severity<-car::recode(data2009$CRASH_SEVE,"4:5=1;else=0")
severity2009<-table(data2009$date,
	data2009$severity)[,2]
data2010$severity<-car::recode(data2010$CRASH_SEVE,"4:5=1;else=0")
severity2010<-table(data2010$date,
	data2010$severity)[,2]
data2011$severity<-car::recode(data2011$CRASH_SEVE,"4:5=1;else=0")
severity2011<-table(data2011$date,
	data2011$severity)[,2]
data2012$severity<-car::recode(data2012$CRASH_SEVE,"4:5=1;else=0")
severity2012<-table(data2012$date,
	data2012$severity)[,2]
data2013$severity<-car::recode(data2013$CRASH_SEVE,"4:5=1;else=0")
severity2013<-table(data2013$date,
	data2013$severity)[,2]

severity<-c(severity2009,
	   severity2010,
	   severity2011,
	   severity2012,
	   severity2013)

time_series_plot<-data.frame(crashes, vru, distracteddriving,
	extremeage,speeding,impaired,severity,dates=as.Date(dates))

############################################################
#DESCRIPTIVES
#############################################################

sum(crashes)
sum(vru)
sum(distracteddriving)
sum(extremeage)
sum(speeding)
sum(impaired)
sum(severity)

sum(vru)*100/sum(crashes)
sum(distracteddriving)*100/sum(crashes)
sum(extremeage)*100/sum(crashes)
sum(speeding)*100/sum(crashes)
sum(impaired)*100/sum(crashes)
sum(severity)*100/sum(crashes)

########################################################
#DECOMPOSITION
########################################################
#BY week
#time_series_plot$date_week <- floor_date(time_series_plot$dates, 
#	"week")
#time_series_week<-ddply(time_series_plot, "date_week", summarise, 
#	crashes_week = sum(crashes))
#psych::describe(time_series_week)

#ggplot(time_series_week,aes(date_week,crashes_week)) + 
#  geom_line() +
#  geom_line(data=data2,aes(color="Speeding")) +
#  labs(color="Legend") +
#  scale_colour_manual("", breaks = c("Distracted Driving", "Speeding"),
#                          values = c("blue", "brown")) +
  #ggtitle("Closing Stock Prices: IBM & Linkedin") + 
  #scale_x_date(format = "%b-%Y") +
#  theme(plot.title = element_text(lineheight=.7, face="bold"))

#time_series_week_vru<-ddply(time_series_plot, "date_week", summarise, 
# 	crashes_week = sum(vru))
# psych::describe(time_series_week_vru)

# time_series_week_distracteddriving<-ddply(time_series_plot, "date_week", summarise, 
# 	crashes_week = sum(distracteddriving))
# psych::describe(time_series_week_distracteddriving)

# time_series_week_extremeage<-ddply(time_series_plot, "date_week", summarise, 
# 	crashes_week = sum(extremeage))
# psych::describe(time_series_week_extremeage)

# time_series_week_speeding<-ddply(time_series_plot, "date_week", summarise, 
# 	crashes_week = sum(speeding))
# psych::describe(time_series_week_speeding)

# time_series_week_impaired<-ddply(time_series_plot, "date_week", summarise, 
# 	crashes_week = sum(impaired))
# psych::describe(time_series_week_impaired)

# time_series_week_severity<-ddply(time_series_plot, "date_week", summarise, 
# 	crashes_week = sum(severity))
# psych::describe(time_series_week_severity)

#BY month
#recoding data to decompose time series into month based time series
time_series_plot$date_month <- floor_date(time_series_plot$dates, 
	"month")

# summarise crash data by month
time_series_month<-ddply(time_series_plot, "date_month", summarise, 
	crashes_month = sum(crashes))
#get descriptives
# psych::describe(time_series_month)

#creating year category vector
time_series_month$years<-c(rep("Year1",12),rep("Year2",12),rep("Year3",12),
	rep("Year4",12),rep("Year5",12))

#creating month category vector
time_series_month$months<-rep(c("Jan","Feb","Mar","Apr","May",
	"Jun","Jul","Aug","Sep","Oct","Nov","Dec"),5)

# time_series_month$months<-rep(c("Month1","Month2","Month3","Month4","Month5",
# 	"Month6","Month7","Month8","Month9","Month10","Month11","Month12"),5)

#creating population by year vector
time_series_month$pop<-c(rep(637418,12),
					rep(621210,12),
					rep(620987,12),
					rep(622417,12),
					rep(622104,12))

#Normalizing month data by population
time_series_month$prev_crashes_month<-with(time_series_month,
	(crashes_month/pop)*100000)

#plotting time series by month
ggplot(time_series_month,aes(date_month,prev_crashes_month)) + 
  geom_line() +
#  geom_line(data=data2,aes(color="Speeding")) +
#  labs(color="Legend") +
#  scale_colour_manual("", breaks = c("Distracted Driving", "Speeding"),
#                          values = c("blue", "brown")) +
  #ggtitle("Closing Stock Prices: IBM & Linkedin") + 
  #scale_x_date(format = "%b-%Y") +
  theme(plot.title = element_text(lineheight=.7, face="bold"))

time_series_month_vru<-ddply(time_series_plot, "date_month", summarise, 
	crashes_month = sum(vru))
# psych::describe(time_series_month_vru)
time_series_month$vru<-time_series_month_vru$crashes_month
time_series_month$prev_vru<-with(time_series_month,
	(vru/pop)*100000)

time_series_month_distracteddriving<-ddply(time_series_plot, "date_month", summarise, 
	crashes_month = sum(distracteddriving))
# psych::describe(time_series_month_distracteddriving)
time_series_month$distracteddriving<-time_series_month_distracteddriving$crashes_month
time_series_month$prev_distracteddriving<-with(time_series_month,
	(distracteddriving/pop)*100000)

time_series_month_extremeage<-ddply(time_series_plot, "date_month", summarise, 
	crashes_month = sum(extremeage))
# psych::describe(time_series_month_extremeage)
time_series_month$extremeage<-time_series_month_extremeage$crashes_month
time_series_month$prev_extremeage<-with(time_series_month,
	(extremeage/pop)*100000)

time_series_month_speeding<-ddply(time_series_plot, "date_month", summarise, 
	crashes_month = sum(speeding))
# psych::describe(time_series_month_speeding)
time_series_month$speeding<-time_series_month_speeding$crashes_month
time_series_month$prev_speeding<-with(time_series_month,
	(speeding/pop)*100000)

time_series_month_impaired<-ddply(time_series_plot, "date_month", summarise, 
	crashes_month = sum(impaired))
# psych::describe(time_series_month_impaired)
time_series_month$impaired<-time_series_month_impaired$crashes_month
time_series_month$prev_impaired<-with(time_series_month,
	(impaired/pop)*100000)

time_series_month_severity<-ddply(time_series_plot, "date_month", summarise, 
	crashes_month = sum(severity))
# psych::describe(time_series_month_severity)
time_series_month$severity<-time_series_month_severity$crashes_month
time_series_month$prev_severity<-with(time_series_month,
	(severity/pop)*100000)

# #BY year
time_series_plot$date_year <- floor_date(time_series_plot$dates, 
 	"year")
time_series_year<-ddply(time_series_plot, "date_year", summarise, 
 	crashes_year = sum(crashes))

#creating population by year vector
time_series_year$pop<-c(637418,
					621210,
					620987,
					622417,
					622104)

#Normalizing month data by population
time_series_year$prev_crashes_year<-with(time_series_year,
	(crashes_year/pop)*100000)

#plotting time series by month
ggplot(time_series_year,aes(date_year,prev_crashes_year)) + 
  geom_line() +
#  geom_line(data=data2,aes(color="Speeding")) +
#  labs(color="Legend") +
#  scale_colour_manual("", breaks = c("Distracted Driving", "Speeding"),
#                          values = c("blue", "brown")) +
  #ggtitle("Closing Stock Prices: IBM & Linkedin") + 
  #scale_x_date(format = "%b-%Y") +
  theme(plot.title = element_text(lineheight=.7, face="bold"))

time_series_year_vru<-ddply(time_series_plot, "date_year", summarise, 
	crashes_year = sum(vru))
# psych::describe(time_series_year_vru)
time_series_year$vru<-time_series_year_vru$crashes_year
time_series_year$prev_vru<-with(time_series_year,
	(vru/pop)*100000)

time_series_year_distracteddriving<-ddply(time_series_plot, "date_year", summarise, 
	crashes_year = sum(distracteddriving))
# psych::describe(time_series_year_distracteddriving)
time_series_year$distracteddriving<-time_series_year_distracteddriving$crashes_year
time_series_year$prev_distracteddriving<-with(time_series_year,
	(distracteddriving/pop)*100000)

time_series_year_extremeage<-ddply(time_series_plot, "date_year", summarise, 
	crashes_year = sum(extremeage))
# psych::describe(time_series_year_extremeage)
time_series_year$extremeage<-time_series_year_extremeage$crashes_year
time_series_year$prev_extremeage<-with(time_series_year,
	(extremeage/pop)*100000)

time_series_year_speeding<-ddply(time_series_plot, "date_year", summarise, 
	crashes_year = sum(speeding))
# psych::describe(time_series_year_speeding)
time_series_year$speeding<-time_series_year_speeding$crashes_year
time_series_year$prev_speeding<-with(time_series_year,
	(speeding/pop)*100000)

time_series_year_impaired<-ddply(time_series_plot, "date_year", summarise, 
	crashes_year = sum(impaired))
# psych::describe(time_series_year_impaired)
time_series_year$impaired<-time_series_year_impaired$crashes_year
time_series_year$prev_impaired<-with(time_series_year,
	(impaired/pop)*100000)

time_series_year_severity<-ddply(time_series_plot, "date_year", summarise, 
	crashes_year = sum(severity))
# psych::describe(time_series_year_severity)
time_series_year$severity<-time_series_year_severity$crashes_year
time_series_year$prev_severity<-with(time_series_year,
	(severity/pop)*100000)
#############################################################
# ARIMA
#############################################################

#daily
#timeseries_data <- ts(time_series_plot$crashes,
#	start=c(2009,1),frequency=7)
#fit <- auto.arima(timeseries_data)
#summary(fit)
#forecast(fit,h=30)
#plot(forecast(fit,h=30))


# GRAPH 2 - Types
byyear_data<-with(time_series_month,data.frame(prev_crashes_month,
	years,months))

#with(byyear_data,by(prev_crashes_month,months,summary))

prev_crashes_month<-c(243.6,243.6,281.2,277.1,293.6,273.5,268.6,
	265.5,269.8,271.1,258.4,258.8)
#creating month category vector
months<-c("Jan","Feb","Mar","Apr","May",
	"Jun","Jul","Aug","Sep","Oct","Nov","Dec")
years<-rep("Average",12)
data2<-data.frame(prev_crashes_month,months,years)

graph_data<-rbind(byyear_data,data2)

# byyear_data2<-dcast(byyear_data,months~variable)
# colnames(byyear_data2)<-c("months","Year1","Year2","Year3",
# 	"Year4","Year5")

tiff("/Users/joaovissoci/Desktop/figure2.tiff", 
	width = 500, height = 300,compression = 'lzw')
ggplot(graph_data,aes(x=months,y=prev_crashes_month)) + 
  geom_line(aes(group=years,colour=years)) +
  geom_point(aes(colour=years),size=1.3) + 
  scale_x_discrete(limits=c("Jan","Feb","Mar","Apr","May",
	"Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  ylab("Crashes/100.000 habitants") +
  xlab("Months") +
  scale_colour_manual("Time series", 
  	breaks = c("Year1","Year2","Year3","Year4","Year5","Average"),
  	labels = c("2009", "2010","2011","2012","2013","Average"),
  	values = c("red","gold","grey","green","blue","black")) +
  theme_bw()
dev.off()


ggplot(data2,aes(y=mean,x=months)) + geom_path()+ 
geom_point(color="black")

# #                          values = c("blue", "brown")) +
#   #ggtitle("Closing Stock Prices: IBM & Linkedin") + 
  #scale_x_date(format = "%b-%Y") +
  # theme(plot.title = element_text(lineheight=.7, face="bold")) +

#### ARIMA
timeseries_data <- ts(time_series_month$crashes_month, 
 	start=c(2009,1),frequency=12)
# seasonplot(timeseries_data,col=c("red","green","blue","gold",
# 	"black"),
# 	,main=NULL,
# 	year.labels.left=TRUE,labelgap=0.5)
# #monthplot(timeseries_data)

#
fit <- auto.arima(timeseries_data)
pacf(timeseries_data, lag.max=48) 
summary(fit)
accuracy(fit)
forecast(fit,h=20)
plot(forecast(fit,h=20))

#### ARIMA
timeseries_data <- ts(time_series_year$crashes_year, 
	start=c(2009,1),frequency=5)
seasonplot(timeseries_data)#,
	#,main=NULL,
	#year.labels.left=TRUE,labelgap=0.5)
#monthplot(timeseries_data)

#
fit <- auto.arima(timeseries_data)
pacf(timeseries_data, lag.max=7) 
summary(fit)
accuracy(fit)
forecast(fit,h=5)
plot(forecast(fit,h=5))

####################################
# Time trend by category
####################################

# GRAPH 2 - Types
tiff("/Users/joaovissoci/Desktop/figure3.tiff", 
	width = 500, height = 300,compression = 'lzw')
ggplot(time_series_month,aes(date_month,prev_vru)) + 
  geom_line(aes(colour="VRU")) +
  geom_line(data=time_series_month,
  	aes(date_month,prev_speeding,color="Speeding")) +
  geom_line(data=time_series_month,
  	aes(date_month,prev_distracteddriving,color="Distracted")) +
  geom_line(data=time_series_month,
  	aes(date_month,prev_extremeage,color="Extreme Ages")) +
  geom_line(data=time_series_month,
  	aes(date_month,prev_impaired,color="Impaired")) +
   geom_line(data=time_series_month,
  	aes(date_month,prev_severity,color="Severe crash")) +
  labs(color="Type of crash") +
  xlab("Time series") +
  ylab("Crashes/100.000 habitants") +
#  scale_colour_manual("", breaks = c("Distracted Driving", "Speeding"),
#                          values = c("blue", "brown")) +
  #ggtitle("Closing Stock Prices: IBM & Linkedin") + 
  #scale_x_date(format = "%b-%Y") +
  theme(plot.title = element_text(lineheight=.7, face="bold")) +
  theme_bw()
dev.off()

####################################




CHANGE points

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