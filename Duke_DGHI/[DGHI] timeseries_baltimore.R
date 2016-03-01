
data<-read.csv("/Users/joaovissoci/Desktop/Vulnerabilidade_pointsbypolygons.csv")

temp_data<-with(data,data.frame(100000*(vuln2013/POP2010),
	100000*(vulner2012/POP2010),
	100000*(vulner2011/POP2010),
	100000*(vulner2010/POP2010),
	100000*(vulner2009/POP2010)))


#temp_data<-t(temp_data)
#colnames(temp_data)<-c(1:200)
time_series_data<-as.ts(temp_data)

timeseries_data <- ts(temp_data, frequency=200)
birthstimeseriescomponents <- decompose(timeseries_data)
plot(birthstimeseriescomponents)

# Seasonal decomposition
fit <- stl(timeseries_data, s.window="period")
plot(fit)

# additional plots
monthplot(timeseries_data)
library(forecast)
seasonplot(timeseries_data)




plot(time_series_data, type="b")
diff12 = diff(time_series_data,12)
acf2(diff12, 48)