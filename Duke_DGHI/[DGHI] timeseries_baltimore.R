
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