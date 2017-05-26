

load("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/snakebites/snakebites_GIS/br_SIAsnakebite_data.RData")


str(apendectomy)

data<-apendectomy

levels(as.factor(data$PROC_REA_nome))

table(as.factor(data$INFEHOSP))


snakebite_mortality_data<-read.csv("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/snakebites/snakebites_GIS/br_SIMsnakebite_data.csv", sep=",")

snakebite_prebalence_data<-read.csv("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/snakebites/snakebites_GIS/br_SINANsnakebinteprev_data.csv",sep=",")

income_level_data<-read.csv("/Users/joaovissoci/Desktop/br_incomelevel_data.csv",sep=",")

population_data<-read.csv("/Users/joaovissoci/Desktop/populacao2008-2015.csv",sep=",")

population_data$ibge6<-population_data$ibge

population_cities<-merge(x = income_level_data, 
					   y = population_data, 
					   by = "ibge6", 
					   all.x = TRUE)

population_cities_prev<-merge(x = population_cities, 
					   y = snakebite_prebalence_data, 
					   by = "ibge6", 
					   all.x = TRUE)


population_cities_prev$prev2008<-(as.numeric(as.character(population_cities_prev$X2008)) / 
	 							population_cities_prev$pop_2008)*100000
population_cities_prev$prev2009<-(as.numeric(as.character(population_cities_prev$X2009)) / 
	 							population_cities_prev$pop_2009)*100000
population_cities_prev$prev2010<-(as.numeric(as.character(population_cities_prev$X2010)) / 
	 							population_cities_prev$pop_2010)*100000
population_cities_prev$prev2011<-(as.numeric(as.character(population_cities_prev$X2011)) / 
	 							population_cities_prev$pop_2011)*100000
population_cities_prev$prev2012<-(as.numeric(as.character(population_cities_prev$X2012)) / 
	 							population_cities_prev$pop_2012)*100000
population_cities_prev$prev2013<-(as.numeric(as.character(population_cities_prev$X2013)) / 
	 							population_cities_prev$pop_2013)*100000
population_cities_prev$prev2014<-(as.numeric(as.character(population_cities_prev$X2014)) / 
	 							population_cities_prev$pop_2014)*100000
population_cities_prev$prev2015<-(as.numeric(as.character(population_cities_prev$X2015)) / 
	 							population_cities_prev$pop_2015)*100000

NAto0<-function(x){
	car::recode(x,"NA=0")
	}

temp<-with(population_cities_prev,data.frame(
										prev2008,
										prev2009,
										prev2010,
										prev2011,
										prev2012,
										prev2013,
										prev2014,
										prev2015))

prev_data_noNA<-lapply(temp,NAto0)


population_cities_prev$avg_prev<-with(prev_data_noNA,rowMeans(data.frame(
										prev2008,
										prev2009,
										prev2010,
										prev2011,
										prev2012,
										prev2013,
										prev2014,
										prev2015)))

library(Hmisc)
library(psych)
with(population_cities_prev,describe(avg_prev))
with(population_cities_prev,describeBy(avg_prev,Income_level_2010))


popuilation_cities_mortality<-merge(x = population_cities_prev, 
					   y = snakebite_mortality_data, 
					   by = "ibge6", 
					   all.x = TRUE)

# NAto0<-function(x){
# 	car::recode(x,"NA=0")
# 	}

popuilation_cities_mortality$mortality2008<-(as.numeric(as.character(popuilation_cities_mortality$X2008.y)) / 
	 							popuilation_cities_mortality$pop_2008)*100000
popuilation_cities_mortality$mortality2009<-(as.numeric(as.character(popuilation_cities_mortality$X2009.y)) / 
	 							popuilation_cities_mortality$pop_2009)*100000
popuilation_cities_mortality$mortality2010<-(as.numeric(as.character(popuilation_cities_mortality$X2010.y)) / 
	 							popuilation_cities_mortality$pop_2010)*100000
popuilation_cities_mortality$mortality2011<-(as.numeric(as.character(popuilation_cities_mortality$X2011.y)) / 
	 							popuilation_cities_mortality$pop_2011)*100000
popuilation_cities_mortality$mortality2012<-(as.numeric(as.character(popuilation_cities_mortality$X2012.y)) / 
	 							popuilation_cities_mortality$pop_2012)*100000
popuilation_cities_mortality$mortality2013<-(as.numeric(as.character(popuilation_cities_mortality$X2013.y)) / 
	 							popuilation_cities_mortality$pop_2013)*100000
popuilation_cities_mortality$mortality2014<-(as.numeric(as.character(popuilation_cities_mortality$X2014.y)) / 
	 							popuilation_cities_mortality$pop_2014)*100000
popuilation_cities_mortality$mortality2015<-(as.numeric(as.character(popuilation_cities_mortality$X2015.y)) / 
	 							popuilation_cities_mortality$pop_2015)*100000

NAto0<-function(x){
	car::recode(x,"NA=0")
	}


temp<-with(popuilation_cities_mortality,data.frame(
										mortality2008,
										mortality2009,
										mortality2010,
										mortality2011,
										mortality2012,
										mortality2013,
										mortality2014,
										mortality2015))

prev_data_noNA<-lapply(temp,NAto0)


popuilation_cities_mortality$avg_mortality<-with(prev_data_noNA,
					rowMeans(data.frame(
										mortality2008,
										mortality2009,
										mortality2010,
										mortality2011,
										mortality2012,
										mortality2013,
										mortality2014,
										mortality2015)))

with(population_cities_prev,describe(avg_mortality))
with(population_cities_prev,describeBy(avg_mortality,Income_level_2010))

write.csv(popuilation_cities_mortality,"/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/snakebites/snakebites_GIS/br_SINANsnakebitecitiesFULL_data.csv")

##### ISOLATE TIME

timetocare_data<-NULL

#Prev T1
#load data
prev_data_T1<-read.csv("/Users/joaovissoci/Desktop/br_SINANsnakebites0-1hroas_data.csv", sep=",")

#create connecting data
prev_data_T1$ibge6<-prev_data_T1[,1]

#merge with population data
population_cities_T1<-merge(x = population_data, 
					   y = prev_data_T1, 
					   by = "ibge6", 
					   all.x = TRUE)

#calculate rates by 100000
population_cities_T1$prev_data_T1_2008<-(as.numeric(as.character(population_cities_T1$X2008)) / 
	 							population_data$pop_2008)*100000
population_cities_T1$prev_data_T1_2009<-(as.numeric(as.character(population_cities_T1$X2009)) / 
	 							population_data$pop_2009)*100000
population_cities_T1$prev_data_T1_2010<-(as.numeric(as.character(population_cities_T1$X2010)) / 
	 							population_data$pop_2010)*100000
population_cities_T1$prev_data_T1_2011<-(as.numeric(as.character(population_cities_T1$X2011)) / 
	 							population_data$pop_2011)*100000
population_cities_T1$prev_data_T1_2012<-(as.numeric(as.character(population_cities_T1$X2012)) / 
	 							population_data$pop_2012)*100000
population_cities_T1$prev_data_T1_2013<-(as.numeric(as.character(population_cities_T1$X2013)) / 
	 							population_data$pop_2013)*100000
population_cities_T1$prev_data_T1_2014<-(as.numeric(as.character(population_cities_T1$X2014)) / 
	 							population_data$pop_2014)*100000
population_cities_T1$prev_data_T1_2015<-(as.numeric(as.character(population_cities_T1$X2015)) / 
	 							population_data$pop_2015)*100000

#Adding to the dataframe
NAto0<-function(x){
	car::recode(x,"NA=0")
	}

temp<-with(population_cities_T1,data.frame(
										prev_data_T1_2008,
										prev_data_T1_2009,
										prev_data_T1_2010,
										prev_data_T1_2011,
										prev_data_T1_2012,
										prev_data_T1_2013,
										prev_data_T1_2014,
										prev_data_T1_2015))

prev_data_noNA<-lapply(temp,NAto0)
prev_data_noNA<-as.data.frame(prev_data_noNA)

#Adding to the dataframe
timetocare_data$T1<-rowMeans(prev_data_noNA)

#Prev T2
#load data
prev_data_T2<-read.csv("/Users/joaovissoci/Desktop/br_SINANsnakebites1-3hroas_data.csv", sep=",")

#create connecting data
prev_data_T2$ibge6<-prev_data_T2[,1]

#merge with population data
population_cities_T2<-merge(x = population_data, 
					   y = prev_data_T2, 
					   by = "ibge6", 
					   all.x = TRUE)

#calculate rates by 100000
population_cities_T2$prev_data_T2_2008<-(as.numeric(as.character(population_cities_T2$X2008)) / 
	 							population_data$pop_2008)*100000
population_cities_T2$prev_data_T2_2009<-(as.numeric(as.character(population_cities_T2$X2009)) / 
	 							population_data$pop_2009)*100000
population_cities_T2$prev_data_T2_2010<-(as.numeric(as.character(population_cities_T2$X2010)) / 
	 							population_data$pop_2010)*100000
population_cities_T2$prev_data_T2_2011<-(as.numeric(as.character(population_cities_T2$X2011)) / 
	 							population_data$pop_2011)*100000
population_cities_T2$prev_data_T2_2012<-(as.numeric(as.character(population_cities_T2$X2012)) / 
	 							population_data$pop_2012)*100000
population_cities_T2$prev_data_T2_2013<-(as.numeric(as.character(population_cities_T2$X2013)) / 
	 							population_data$pop_2013)*100000
population_cities_T2$prev_data_T2_2014<-(as.numeric(as.character(population_cities_T2$X2014)) / 
	 							population_data$pop_2014)*100000
population_cities_T2$prev_data_T2_2015<-(as.numeric(as.character(population_cities_T2$X2015)) / 
	 							population_data$pop_2015)*100000

#Adding to the dataframe
NAto0<-function(x){
	car::recode(x,"NA=0")
	}

temp<-with(population_cities_T2,data.frame(
										prev_data_T2_2008,
										prev_data_T2_2009,
										prev_data_T2_2010,
										prev_data_T2_2011,
										prev_data_T2_2012,
										prev_data_T2_2013,
										prev_data_T2_2014,
										prev_data_T2_2015))

prev_data_noNA<-lapply(temp,NAto0)
prev_data_noNA<-as.data.frame(prev_data_noNA)

#Adding to the dataframe
timetocare_data$T2<-rowMeans(prev_data_noNA)


#Prev T3
#load data
prev_data_T3<-read.csv("/Users/joaovissoci/Desktop/br_SINANsnakebites3-6hroas_data.csv", sep=",")

#create connecting data
prev_data_T3$ibge6<-prev_data_T3[,1]

#merge with population data
population_citiesT3<-merge(x = population_data, 
					   y = prev_data_T3, 
					   by = "ibge6", 
					   all.x = TRUE)

#calculate rates by 100000
population_citiesT3$prev_data_T3_2008<-(as.numeric(as.character(population_citiesT3$X2008)) / 
	 							population_data$pop_2008)*100000
population_citiesT3$prev_data_T3_2009<-(as.numeric(as.character(population_citiesT3$X2009)) / 
	 							population_data$pop_2009)*100000
population_citiesT3$prev_data_T3_2010<-(as.numeric(as.character(population_citiesT3$X2010)) / 
	 							population_data$pop_2010)*100000
population_citiesT3$prev_data_T3_2011<-(as.numeric(as.character(population_citiesT3$X2011)) / 
	 							population_data$pop_2011)*100000
population_citiesT3$prev_data_T3_2012<-(as.numeric(as.character(population_citiesT3$X2012)) / 
	 							population_data$pop_2012)*100000
population_citiesT3$prev_data_T3_2013<-(as.numeric(as.character(population_citiesT3$X2013)) / 
	 							population_data$pop_2013)*100000
population_citiesT3$prev_data_T3_2014<-(as.numeric(as.character(population_citiesT3$X2014)) / 
	 							population_data$pop_2014)*100000
population_citiesT3$prev_data_T3_2015<-(as.numeric(as.character(population_citiesT3$X2015)) / 
	 							population_data$pop_2015)*100000

#Adding to the dataframe
NAto0<-function(x){
	car::recode(x,"NA=0")
	}

temp<-with(population_citiesT3,data.frame(
										prev_data_T3_2008,
										prev_data_T3_2009,
										prev_data_T3_2010,
										prev_data_T3_2011,
										prev_data_T3_2012,
										prev_data_T3_2013,
										prev_data_T3_2014,
										prev_data_T3_2015))

prev_data_noNA<-lapply(temp,NAto0)
prev_data_noNA<-as.data.frame(prev_data_noNA)


#Adding to the dataframe
timetocare_data$T3<-rowMeans(prev_data_noNA)

#Prev T4
#load data
prev_data_T4<-read.csv("/Users/joaovissoci/Desktop/br_SINANsnakebites6-12hroas_data.csv", sep=",")

#create connecting data
prev_data_T4$ibge6<-prev_data_T4[,1]

#merge with population data
population_citiesT4<-merge(x = population_data, 
					   y = prev_data_T4, 
					   by = "ibge6", 
					   all.x = TRUE)

#calculate rates by 100000
population_citiesT4$prev_data_T4_2008<-(as.numeric(as.character(population_citiesT4$X2008)) / 
	 							population_data$pop_2008)*100000
population_citiesT4$prev_data_T4_2009<-(as.numeric(as.character(population_citiesT4$X2009)) / 
	 							population_data$pop_2009)*100000
population_citiesT4$prev_data_T4_2010<-(as.numeric(as.character(population_citiesT4$X2010)) / 
	 							population_data$pop_2010)*100000
population_citiesT4$prev_data_T4_2011<-(as.numeric(as.character(population_citiesT4$X2011)) / 
	 							population_data$pop_2011)*100000
population_citiesT4$prev_data_T4_2012<-(as.numeric(as.character(population_citiesT4$X2012)) / 
	 							population_data$pop_2012)*100000
population_citiesT4$prev_data_T4_2013<-(as.numeric(as.character(population_citiesT4$X2013)) / 
	 							population_data$pop_2013)*100000
population_citiesT4$prev_data_T4_2014<-(as.numeric(as.character(population_citiesT4$X2014)) / 
	 							population_data$pop_2014)*100000
population_citiesT4$prev_data_T4_2015<-(as.numeric(as.character(population_citiesT4$X2015)) / 
	 							population_data$pop_2015)*100000

#Adding to the dataframe
NAto0<-function(x){
	car::recode(x,"NA=0")
	}

temp<-with(population_citiesT4,data.frame(
										prev_data_T4_2008,
										prev_data_T4_2009,
										prev_data_T4_2010,
										prev_data_T4_2011,
										prev_data_T4_2012,
										prev_data_T4_2013,
										prev_data_T4_2014,
										prev_data_T4_2015))

prev_data_noNA<-lapply(temp,NAto0)
prev_data_noNA<-as.data.frame(prev_data_noNA)


#Adding to the dataframe
timetocare_data$T4<-rowMeans(prev_data_noNA)

#Prev T5
#load data
prev_data_T5<-read.csv("/Users/joaovissoci/Desktop/br_SINANsnakebites12-24hroas_data.csv", sep=",")

#create connecting data
prev_data_T5$ibge6<-prev_data_T5[,1]

#merge with population data
population_citiesT5<-merge(x = population_data, 
					   y = prev_data_T5, 
					   by = "ibge6", 
					   all.x = TRUE)

#calculate rates by 100000
population_citiesT5$prev_data_T5_2008<-(as.numeric(as.character(population_citiesT5$X2008)) / 
	 							population_data$pop_2008)*100000
population_citiesT5$prev_data_T5_2009<-(as.numeric(as.character(population_citiesT5$X2009)) / 
	 							population_data$pop_2009)*100000
population_citiesT5$prev_data_T5_2010<-(as.numeric(as.character(population_citiesT5$X2010)) / 
	 							population_data$pop_2010)*100000
population_citiesT5$prev_data_T5_2011<-(as.numeric(as.character(population_citiesT5$X2011)) / 
	 							population_data$pop_2011)*100000
population_citiesT5$prev_data_T5_2012<-(as.numeric(as.character(population_citiesT5$X2012)) / 
	 							population_data$pop_2012)*100000
population_citiesT5$prev_data_T5_2013<-(as.numeric(as.character(population_citiesT5$X2013)) / 
	 							population_data$pop_2013)*100000
population_citiesT5$prev_data_T5_2014<-(as.numeric(as.character(population_citiesT5$X2014)) / 
	 							population_data$pop_2014)*100000
population_citiesT5$prev_data_T5_2015<-(as.numeric(as.character(population_citiesT5$X2015)) / 
	 							population_data$pop_2015)*100000

#Adding to the dataframe
NAto0<-function(x){
	car::recode(x,"NA=0")
	}

temp<-with(population_citiesT5,data.frame(
										prev_data_T5_2008,
										prev_data_T5_2009,
										prev_data_T5_2010,
										prev_data_T5_2011,
										prev_data_T5_2012,
										prev_data_T5_2013,
										prev_data_T5_2014,
										prev_data_T5_2015))

prev_data_noNA<-lapply(temp,NAto0)
prev_data_noNA<-as.data.frame(prev_data_noNA)

#Adding to the dataframe
timetocare_data$T5<-rowMeans(prev_data_noNA)

#Prev T6
#load data
prev_data_T6<-read.csv("/Users/joaovissoci/Desktop/br_SINANsnakebites24oumorehroas_data.csv", sep=",")

#create connecting data
prev_data_T6$ibge6<-prev_data_T6[,1]

#merge with population data
population_citiesT6<-merge(x = population_data, 
					   y = prev_data_T6, 
					   by = "ibge6", 
					   all.x = TRUE)

#calculate rates by 100000
population_citiesT6$prev_data_T6_2008<-(as.numeric(as.character(population_citiesT6$X2008)) / 
	 							population_data$pop_2008)*100000
population_citiesT6$prev_data_T6_2009<-(as.numeric(as.character(population_citiesT6$X2009)) / 
	 							population_data$pop_2009)*100000
population_citiesT6$prev_data_T6_2010<-(as.numeric(as.character(population_citiesT6$X2010)) / 
	 							population_data$pop_2010)*100000
population_citiesT6$prev_data_T6_2011<-(as.numeric(as.character(population_citiesT6$X2011)) / 
	 							population_data$pop_2011)*100000
population_citiesT6$prev_data_T6_2012<-(as.numeric(as.character(population_citiesT6$X2012)) / 
	 							population_data$pop_2012)*100000
population_citiesT6$prev_data_T6_2013<-(as.numeric(as.character(population_citiesT6$X2013)) / 
	 							population_data$pop_2013)*100000
population_citiesT6$prev_data_T6_2014<-(as.numeric(as.character(population_citiesT6$X2014)) / 
	 							population_data$pop_2014)*100000
population_citiesT6$prev_data_T6_2015<-(as.numeric(as.character(population_citiesT6$X2015)) / 
	 							population_data$pop_2015)*100000

NAto0<-function(x){
	car::recode(x,"NA=0")
	}

temp<-with(population_citiesT6,data.frame(
										prev_data_T6_2008,
										prev_data_T6_2009,
										prev_data_T6_2010,
										prev_data_T6_2011,
										prev_data_T6_2012,
										prev_data_T6_2013,
										prev_data_T6_2014,
										prev_data_T6_2015))

prev_data_noNA<-lapply(temp,NAto0)
prev_data_noNA<-as.data.frame(prev_data_noNA)

#Adding to the dataframe
timetocare_data$T6<-rowMeans(prev_data_noNA)


#sum the values of T1 and T2, T3 to T6

# timetocare_data$minus3<-with(timetocare_data,
# 				rowSums(data.frame(T1,T2)))

timetocare_data$more6<-with(timetocare_data,
				rowSums(data.frame(T3,T4,T5,T6)))


timetocare_data<-as.data.frame(timetocare_data)

#compare sum1 and sum2 to classify cities into >3 hrs or <3hrs

timetocare_data$cat<-colnames(timetocare_data[,c(1,2,7)])[max.col(timetocare_data[,c(1,2,7)],ties.method="last")]

timetocare_data$ibge6<-population_citiesT5$ibge6

all_data<-merge(x = popuilation_cities_mortality, 
					   y = timetocare_data, 
					   by = "ibge6", 
					   all.x = TRUE)


all_data2<-with(all_data,data.frame(
				ibge6,
				cat,
				avg_mortality))

write.csv(all_data2,"/Users/joaovissoci/Desktop/snakebiteFOREVER.csv")

