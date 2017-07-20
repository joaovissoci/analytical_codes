
library("plyr")
library("lubridate")

#WEEKS
data_zika1<-read.csv("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/Brazil/zika_gis/br_zikaepi_data.csv")
data_micro<-read.csv("/Users/joaovissoci/Downloads/microcefalia.csv")
data_nascvivos<-read.csv("/Users/joaovissoci/Desktop/nascvivos.csv")

data_zika_weeks_merged0<-merge(x = data_zika1, 
					   y = data_micro, 
					   by = "X...IBGE_6", 
					   all.x = TRUE)

data_zika_weeks_merged<-merge(x = data_zika_weeks_merged0, 
					   y = data_nascvivos, 
					   by = "X...IBGE_6", 
					   all.x = TRUE)


#adjusting ZIka data
data_zika_weeks_clean<-remove.vars(data_zika_weeks_merged,
	c("X...IBGE_6",
	  "REGIAO",
	  "UF",
	  "MUNICIPIO",
	  "ACUM_ZIKA_TRIM1",
	  "ACUM_ZIKA_TRIM2",
	  "ACUM_ZIKA_TRIM3",
	  "ACUM_ZIKA_TRIM4",
	  "POP_ZIKA",
	  "INCI_ZIKA",
	  "nascvivos"))

#transforming NAs to 0
NAto0<-function(x){
	car::recode(x,"NA=0")
}

#applying function
data_zika_weeks_clean<-lapply(data_zika_weeks_clean,NAto0)
data_zika_weeks_clean<-as.data.frame(data_zika_weeks_clean) #return to data,frame format

#normalize by population
normalized_zika<-lapply(data_zika_weeks_clean[,-c(53:96)], 
	function(i) (i/data_zika_weeks_merged$POP_ZIKA)*10000) #zika by 100,000
normalized_zika<-as.data.frame(normalized_zika) #micro by 1,000,000

normalized_micro<-lapply(data_zika_weeks_clean[,-c(1:52)], 
	function(i) (i/data_zika_weeks_merged$nascvivos)*10000)
normalized_micro<-as.data.frame(normalized_micro)

data_zikamicro<-data.frame(
				REGIAO=data_zika_weeks_merged$REGIAO,
				normalized_zika,
				normalized_micro)

data_zikamicro_melted <- melt(data_zikamicro, 
	id=c("REGIAO"))

time_series_data<-ddply(data_zikamicro_melted,
							 c("REGIAO", "variable"),
							 summarise,
							 cases = mean(value)
							 # micro_cases = mean(Microcefalia)
    						)


microdata<-ddply(data_zika_weeks_clean[,-c(1:52)],
							 c("REGIAO"),
							 summarise,
							 cases = sum(value)
							 # micro_cases = mean(Microcefalia)
    						)

time_series_data<-ddply(data_zikamicro_melted,
							 c("REGIAO", "variable"),
							 summarise,
							 cases = mean(value)
							 # micro_cases = mean(Microcefalia)
    						)

time_series_data$source<-c(rep("Zika cases/100,000",52),
							  rep("Microcephaly cases/1,000,000",44))

time_series_data$time<-rep(c(1:52,6:49),5)

#Option 1 - with facets for columns and lines
#plotting time series by month
ggplot(time_series_data,aes(time,
							 cases)) + 
  geom_line(aes(group=1)) +
#  geom_line(data=data2,aes(color="Speeding")) +
#  labs(color="Legend") +
#  scale_colour_manual("", breaks = c("Distracted Driving", "Speeding"),
#                          values = c("blue", "brown")) +
  #ggtitle("Closing Stock Prices: IBM & Linkedin") + 
  # scale_x_discrete(limits=) +
  facet_grid(REGIAO ~ source, scales="free_x") +
  theme(plot.title = element_text(lineheight=.7, 
  	face="bold")) +
  	xlab("Surveillance Week") +
  	ylab("Average municipality incidences by region") +
  theme_bw()

#Option 2 - mergin lines
#plotting time series by month
ggplot(time_series_data,aes(time,
							 cases)) + 
  geom_line(aes(group=source,color=source)) +
#  geom_line(data=data2,aes(color="Speeding")) +
#  labs(color="Legend") +
#  scale_colour_manual("", breaks = c("Distracted Driving", "Speeding"),
#                          values = c("blue", "brown")) +
  #ggtitle("Closing Stock Prices: IBM & Linkedin") + 
  # scale_x_discrete(limits=) +
  facet_grid(REGIAO ~ ., scales="free_x") +
  theme(plot.title = element_text(lineheight=.7, 
  	face="bold")) +
  	xlab("Surveillance Week") +
  	ylab("Average municipality incidences by region") +
  theme_bw()



#BIMESTERS
data_zika2<-read.csv("/Users/joaovissoci/Downloads/zika_stacked.csv") 
# data_nascvivos<-read.csv("/Users/joaovissoci/Desktop/nascvivos.csv")

# data_zika3<-merge(x = data_zika2, 
# 					   y = data_nascvivos, 
# 					   by = "X...IBGE_6", 
# 					   all.x = TRUE)

# data_zika<-merge(x = data_zika1, 
# 					   y = data_micro, 
# 					   by = "X...IBGE_6", 
# 					   all.x = TRUE)

# time_series_month_O01<-ddply(data_zika,
# 							 "REGIAO",
# 							 # summarise,
# 							 sem_01 = sum(SEM_01)
#     						)

#adjusting ZIka data
data_zika<-remove.vars(data_zika2,
	c("X...ibge6",
	  "estado",
	  "income_level"))
	  # "ACUM_ZIKA_TRIM1",
	  # "ACUM_ZIKA_TRIM2",
	  # "ACUM_ZIKA_TRIM3",
	  # "ACUM_ZIKA_TRIM4",
	  # "POP_ZIKA",
	  # "INCI_ZIKA",
	  # "Total.Geral"))

# NAto0<-function(x){
# 	car::recode(x,"NA=0")
# }

# data_zika_weeks<-lapply(data_zika_weeks,NAto0)
# data_zika_weeks<-as.data.frame(data_zika_weeks)

# # x <- list(1:3, 4:6)
# normalized_zika<-lapply(data_zika_weeks[,-c(1,54:95)], 
# 	function(i) (i/data_zika$POP_ZIKA)*100000)
# normalized_zika<-as.data.frame(normalized_zika)

# normalized_micro<-lapply(data_zika_weeks[,-c(1,2:53)], 
# 	function(i) (i/data_zika$POP_ZIKA)*1000000)
# normalized_micro<-as.data.frame(normalized_micro)

# data_zikamicro<-data.frame(
# 				REGIAO=data_zika_weeks$REGIAO,
# 				normalized_zika,
# 				normalized_micro)


time_series_data<-ddply(data_zika,
							 c("regiao","bimester"),
							 summarise,
							 zika_cases = mean(Zika),
							 micro_cases = mean(Microcefalia)
    						)

time_series_data$zika_cases<-time_series_data$zika_cases/100

time_series_data2 <- melt(time_series_data, id=c("regiao","bimester"))

time_series_data2$variable<-car::recode(time_series_data2$variable,"
	'zika_cases'='Zika cases/1,000';
	'micro_cases'='Microcephaly cases/100,000'")

#Option 1 - with facets for columns and lines
#plotting time series by month
ggplot(time_series_data2,aes(bimester,
							 value)) + 
  geom_line(aes(group=1)) +
#  geom_line(data=data2,aes(color="Speeding")) +
#  labs(color="Legend") +
#  scale_colour_manual("", breaks = c("Distracted Driving", "Speeding"),
#                          values = c("blue", "brown")) +
  #ggtitle("Closing Stock Prices: IBM & Linkedin") + 
  scale_x_discrete(limits=c("First",
  							"Second",
  							"Third",
  							"Fourth",
  							"Fifth",
  							"Sixth")) +
  facet_grid(regiao ~ variable, scales="free_x") +
  theme(plot.title = element_text(lineheight=.7, 
  	face="bold")) +
  	xlab("Bimesters") +
  	ylab("Average municipality incidences by region") +
  theme_bw()

library(scales)    

#Option 2 - mergin lines
#plotting time series by month
ggplot(time_series_data2,aes(bimester,
							 value)) + 
  geom_line(aes(group=variable, color=variable)) +
#  geom_line(data=data2,aes(color="Speeding")) +
#  labs(color="Legend") +
#  scale_colour_manual("", breaks = c("Distracted Driving", "Speeding"),
#                          values = c("blue", "brown")) +
  #ggtitle("Closing Stock Prices: IBM & Linkedin") + 
  scale_x_discrete(limits=c("First",
  							"Second",
  							"Third",
  							"Fourth",
  							"Fifth",
  							"Sixth")) +
  facet_grid(regiao ~ ., scales="free_x") +
  theme_bw() +
  theme(plot.title = element_text(lineheight=.7, 
  	face="bold"),
  	legend.position=c(.7,.95),
  	legend.title=element_blank(),
  	legend.background = element_rect(fill=alpha('white',0.1))) +
  xlab("Bimester") +
  ylab("Average municipality incidences by region")
  

