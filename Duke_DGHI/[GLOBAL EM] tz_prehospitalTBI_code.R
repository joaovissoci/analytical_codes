######################################################
#suicide_anxiety.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. see full license at the end of this file.
######################################################
#this script follows a combination of the guidelines proposed by Hadley Wickham http://goo.gl/c04kq as well as using the formatR package http://goo.gl/ri6ky
#if this is the first time you are conducting an analysis using this protocol, please watch http://goo.gl/DajIN while following step by step

#link to manuscript

 #The general plan is to compare the fibrinogen and platelet curves of RS vs Copperhead snakes.  The times points are Baseline, nadir during hospitalization, day 5, day 8, day 15.  There is some missing mess.   I am hoping we can get it done in time for an abstract deadline soon.  Let me know what is best.

######################################################
#SETTING ENVIRONMENT
######################################################
 #install.packages("VIM")
 #install.packages("VIMGUI")
 #install.packages("miP")
 #install.packages("gWidgetsRGtk2")
 #install.packages("mi")
 #install.packages("epicalc")

#Load packages neededz for the analysis
#All packages must be installes with install.packages() function
lapply(c("sem","ggplot2", "psych", "RCurl", "irr", "nortest", 
	"moments","GPArotation","nFactors","boot","psy", "car",
	"vcd", "gridExtra","mi","VIM","epicalc","gdata","sqldf",
	"reshape2","mclust","foreign","survival","memisc","lme4",
	"lmerTest","dplyr","QCA","VennDiagram","qgraph","igraph",
	"ltm","gmodels","eRm","mirt","dplyr","devtools","reshape",
	"survival","mice"),
library, character.only=T)

#Package and codes to pull data from goodle sheets
#devtools::install_github("jennybc/googlesheets")
#library(googlesheets)
#suppressMessages(library(dplyr))

######################################################
#IMPORTING DATA
######################################################

data<-read.csv("/Users/Joao/Box Sync/Home Folder jnv4/Data/Global EM/Africa/Tz/tbi_registry/tz_TBIregistry_data.csv")

######################################################
#DATA MANAGEMENT
######################################################

data_prehospital<-subset(data,
				data$prehospital_course_complete==2)

names(data)

#getting time data

t1_time<-injury_time <- as.POSIXlt(data_prehospital$transport_time_1,
                      format='%H:%M')
t2_time<-injury_time <- as.POSIXlt(data_prehospital$transport_time_2,
                      format='%H:%M')
t3_time<-injury_time <- as.POSIXlt(data_prehospital$transport_time_3,
                      format='%H:%M')
t4_time<-injury_time <- as.POSIXlt(data_prehospital$transport_time_4,
                      format='%H:%M')

#Time to treatment
#tranforming time from hrs into min
T1<-(t1_time$hour*60)+t1_time$min
T2<-(t2_time$hour*60)+t2_time$min
T3<-(t3_time$hour*60)+t3_time$min
T4<-(t4_time$hour*60)+t4_time$min

#aggregating times
T_total<-data.frame(T1,T2,T3,T4)

#summing time from all legs
time_transport<-base::rowSums(T_total,na.rm=TRUE)

time_transport_recoded<-car::recode(time_transport,"0=NA")
time_transport_recoded<-time_transport_recoded/60

#Time to care
#transforming time 
# Tt1<-(data$time_injury1*60)+data$time_injury2
# Tt2<-(data$time_arrival1*60)+data$time_arrival2

# Tt_total<-na.omit(data.frame(Tt1,Tt2))
# Tt_total<-Tt_total[1]-

# T_total<-base::rowSums(T_total,na.rm=TRUE)

#Time to reach hospital
injuryT<-with(data_prehospital,paste(inj_date, inj_time, sep=" "))
injury_time <- as.POSIXct(injuryT,
                      format='%m/%d/%y %H:%M')

arrivalT<-with(data_prehospital,paste(date_arrival,time_arrival, sep=" "))
arrival_time <- as.POSIXct(arrivalT,
                      format='%m/%d/%y %H:%M')

dif_time<-difftime(injury_time, arrival_time,
         units = c("min"))*-1

time_to_care<-NULL

for (i in 1:nrow(data_prehospital))
{
 if (is.na(dif_time)[i] == TRUE)

 	{ 
       
       time_to_care[i] <- NA

 	} else if (dif_time[i] < 0) {

       time_to_care[i] <- NA

 	} else {
	   
	   time_to_care[i] <- dif_time[i]

 	}
} # for (i in 1:nrow(dataframe)


time_to_care[1]<-(7*60)+40
time_to_care[292]<-(2*60)+5
time_to_care[952]<-40
time_to_care[311]<-60
time_to_care[312]<-(1*60)+30
time_to_care[314]<-45
time_to_care[317]<-60
time_to_care[317]<-120
time_to_care[318]<-(3*60)
time_to_care[316]<-(6*60)
time_to_care[825]<-(15*60)+30
time_to_care[315]<-(17+24+12)*60
time_to_care[1053]<-60

time_to_care<-time_to_care/60

# quantile(dif_time, probs=seq(0,1,0.10),na.rm=TRUE)

# errors<-car::recode(dif_time,"-524400:0='weird';
# 							 0.0001:10000='normal';
# 							 10000:1057080='weird'")

# error_log<-data.frame(errors,dif_time,id=data$study_id)

# error_log_weird<-subset(error_log,error_log$errors=='weird')

# write.csv(error_log_weird,"/Users/jnv4/Desktop/error.csv")

#transport cost
transport_cost<-with(data_prehospital,rowSums(
	data.frame(transport_cost_1,
			   transport_cost_2,
			   transport_cost_3,
			   transport_cost_4),
	na.rm=TRUE))

#creating transportation legs variable
transport_leg_temp<-with(data_prehospital,data.frame(T1,
										 T2,
										 T3,
										 T4))

transport_leg_temp$T1<-car::recode(
	transport_leg_temp$T1,
	"NA=0;else=1")
transport_leg_temp$T2<-car::recode(
	transport_leg_temp$T2,
	"NA=0;else=1")
transport_leg_temp$T3<-car::recode(
	transport_leg_temp$T3,
	"NA=0;else=1")
transport_leg_temp$T4<-car::recode(
	transport_leg_temp$T4,
	"NA=0;else=1")

transport_legs<-rowSums(transport_leg_temp)

#transportation mode
data_prehospital$transport_leg1<-car::recode(data_prehospital$transport_mode_1,
	"1='Personal vehicle';
	 2='Hired transport';
	 3='Other';
	 4='Bodaboda';
	 5='Hired transport';
	 6='Police';
	 7='Ambulance';
	 8='Hired transport';
	 9='Other';
	 91:99=NA")

data_prehospital$transport_leg2<-car::recode(data_prehospital$transport_mode_2,
	"1='Personal vehicle';
	 2='Hired transport';
	 3='Other';
	 4='Bodaboda';
	 5='Hired transport';
	 6='Police';
	 7='Ambulance';
	 8='Hired transport';
	 9='Other';
	 91:99=NA")

data_prehospital$transport_leg3<-car::recode(data_prehospital$transport_mode_3,
	"1='Personal vehicle';
	 2='Hired transport';
	 3='Other';
	 4='Bodaboda';
	 5='Hired transport';
	 6='Police';
	 7='Ambulance';
	 8='Hired transport';
	 9='Other';
	 91:99=NA")

data_prehospital$transport_leg4<-car::recode(data_prehospital$transport_mode_3,
	"1='Personal vehicle';
	 2='Hired transport';
	 3='Other';
	 4='Bodaboda';
	 5='Hired transport';
	 6='Police';
	 7='Ambulance';
	 8='Hired transport';
	 9='Other';
	 91:99=NA")

#analysis data set
analysis_data<-with(data_prehospital,data.frame(age,
									male,
									moi,
									alcohol,
									gcs_tot,
									# death,
									gos,
									transport_leg1,
									transport_cost,            
									# transport_mode_other_1 ,    
									# transport_cost_1,            
									# transport_start_name_1,     
									# transport_start_1,           
									# transport_end_name_1,       
									# transport_end_district_1,    
									# t1_hrs,                     
									# T1,                      
									# transport_leg2,           
									# transport_mode_other_2,      
									# transport_cost_2,           
									# transport_start_name_2,      
									# transport_start_2,          
									# transport_end_name_2,        
									# transport_end_district_2,   
									# t2_hrs,                      
									# T2,                     
									# transport_cost_3,            
									# transport_start_name_3,     
									# transport_start_3,           
									# transport_end_name_3,       
									# transport_end_district_3,    
									# t3_hrs,                     
									# T3,                      
									# transport_cost_4,            
									# transport_start_name_4,     
									# transport_start_4,           
									# transport_end_name_4,       
									# transport_end_district_4,    
									# t4_hrs,                     
									# T4,                                             
									# ref,                 
									time_transport_recoded,
									time_to_care,
									transport_legs))

#recoding gos
analysis_data$gos_cat<-as.factor(car::recode(
	analysis_data$gos,"1:4='Death';
					   5='Alive'"))

#recoding gcs
analysis_data$gcs_cat<-as.factor(car::recode(
	analysis_data$gcs_tot,"1:12='Severe';
					   13:15='Non-severe'"))

#recoding gcs
analysis_data$alcohol<-as.factor(car::recode(
	analysis_data$alcohol,"0='No';
					   	   1='Yes';
					   	   2='No'"))

#recoding gcs
analysis_data$moi<-as.factor(car::recode(
	analysis_data$moi,"0='yes';NA=NA;else='no'"))

analysis_data$transport_legs_cat<-as.factor(car::recode(
	analysis_data$transport_legs,"
					0:1='0 to 1';
					else='1 or more'"))

analysis_data$male<-as.factor(analysis_data$male)

#Imputation
# analysis_data<-na.omit(analysis_data)


# # argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
imp <- mice(analysis_data, seed = 2222, m=5)

# # reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
analysis_data<-mice::complete(imp,4)

analysis_data$time_to_care_cat<-car::recode(analysis_data$time_to_care,"
					0:1='0-1hrs';
					1.01:2='1-2hrs';
					2.01:3='2-3hrs';
					3.000001:4='3-4hrs';
					else='more 4hrs'")

##############################################################
#TABLE 1
#############################################################

#Table 1
library(bnpa) #have to install from the physical package
# Show a descritive table to all data
variables.to.table1 <- c("age", 
						 "male",
						 "moi",
						 "alcohol",
						 "transport_cost",
						 "time_transport_recoded",
						 "time_to_care_cat",
						 "transport_legs_cat")
strata <- "gos_cat"
descritive.table(analysis_data, variables.to.table1)
descritive.table(analysis_data, variables.to.table1, strata)

#gcs
table<-with(analysis_data,table(time_to_care_cat))
table
prop.table(table)

#gcs
table<-with(analysis_data,table(gcs_cat))
table
prop.table(table)

#gcs
table<-with(analysis_data,table(gos_cat))
table
prop.table(table)

#Age
with(analysis_data,describe(age))
#ad.test(bea_data$density_pedestrian)
#hist(bea_data$density_pedestrian)
#ci_func(bea_data$density_pedestrian,.95)
with(analysis_data,describeBy(age,gcs_cat))
with(analysis_data,t.test(age~gcs_cat))
with(analysis_data,describeBy(age,gos_cat))
with(analysis_data,t.test(age~gos_cat))

# Gender
table<-with(analysis_data,table(male))
table
prop.table(table)
table<-with(analysis_data,table(male,gcs_cat))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package
table<-with(analysis_data,table(male,gos_cat))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# MOI
table<-with(analysis_data,table(moi))
table
prop.table(table)
table<-with(analysis_data,table(moi,gcs_cat))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package
table<-with(analysis_data,table(moi,gos_cat))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# # GOS
# table<-with(analysis_data,table(gos_cat))
# table
# prop.table(table)
# table<-with(analysis_data,table(gos_cat,gcs_cat))
# table
# prop.table(table,2)
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package

# Alcohol use
table<-with(analysis_data,table(alcohol))
table
prop.table(table)
table<-with(analysis_data,table(alcohol,gcs_cat))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package
table<-with(analysis_data,table(alcohol,gos_cat))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

### Time
#Transport time
with(analysis_data,summary(time_transport_recoded))

#ad.test(bea_data$density_pedestrian)
#hist(bea_data$density_pedestrian)
#ci_func(bea_data$density_pedestrian,.95)
with(analysis_data,by(time_transport_recoded,gcs_cat,summary))
with(analysis_data,wilcox.test(time_transport_recoded~gcs_cat))
with(analysis_data,by(time_transport_recoded,gos_cat,summary))
with(analysis_data,wilcox.test(time_transport_recoded~gos_cat))

#Transport time
with(analysis_data,summary(time_to_care))
#ad.test(bea_data$density_pedestrian)
#hist(bea_data$density_pedestrian)
#ci_func(bea_data$density_pedestrian,.95)
with(analysis_data,by(time_to_care,gcs_cat,summary))
with(analysis_data,wilcox.test(time_to_care~gcs_cat))
with(analysis_data,by(time_to_care,gos_cat,summary))
with(analysis_data,wilcox.test(time_to_care~gos_cat))

#Transport cost
with(analysis_data,summary(transport_cost))
#ad.test(bea_data$density_pedestrian)
#hist(bea_data$density_pedestrian)
#ci_func(bea_data$density_pedestrian,.95)
with(analysis_data,by(transport_cost,gcs_cat,summary))
with(analysis_data,wilcox.test(transport_cost~gcs_cat))
with(analysis_data,by(transport_cost,gos_cat,summary))
with(analysis_data,wilcox.test(transport_cost~gos_cat))

#Transport legs
with(analysis_data,summary(transport_legs))
with(analysis_data,table(transport_legs))
with(analysis_data,prop.table(table(transport_legs)))
#ad.test(bea_data$density_pedestrian)
#hist(bea_data$density_pedestrian)
#ci_func(bea_data$density_pedestrian,.95)
with(analysis_data,by(transport_legs,gcs_cat,summary))
with(analysis_data,wilcox.test(transport_legs~gcs_cat))
with(analysis_data,by(transport_legs,gos_cat,summary))
with(analysis_data,wilcox.test(transport_legs~gos_cat))

##############################################################
#Figure 1
#############################################################

data_transport<-with(data_prehospital,data.frame(
	transport_mode_1,
	transport_start_1,
	transport_mode_2,
	transport_start_2,
	transport_mode_3,
	transport_start_3,
	transport_mode_4,
	transport_start_4))

analysis_data$lat_leg1<-car::recode(data_transport$transport_start_1,"
					1=-3.5286807;
					2=-3.3227432;
					3=-3.166825;
					4=-3.1413061;
					5=-3.6608888;
					6=-4.2118703;
					7=-3.186389;
					8=NA;
					NA=NA")

analysis_data$long_leg1<-car::recode(data_transport$transport_start_1,"
					1=37.118976;
					2=37.3285435;
					3=36.8856703;
					4=37.212256;
					5=37.5662467;
					6=37.4419273;
					7=37.068611;
					8=NA;
					NA=NA")

analysis_data$lat_leg2<-car::recode(data_transport$transport_start_2,"
					1=-3.5286807;
					2=-3.3227432;
					3=-3.166825;
					4=-3.1413061;
					5=-3.6608888;
					6=-4.2118703;
					7=-3.186389;
					8=NA;
					NA=NA")

analysis_data$long_leg2<-car::recode(data_transport$transport_start_2,"
					1=37.118976;
					2=37.3285435;
					3=36.8856703;
					4=37.212256;
					5=37.5662467;
					6=37.4419273;
					7=37.068611;
					8=NA;
					NA=NA")

analysis_data$lat_leg3<-car::recode(data_transport$transport_start_3,"
					1=-3.5286807;
					2=-3.3227432;
					3=-3.166825;
					4=-3.1413061;
					5=-3.6608888;
					6=-4.2118703;
					7=-3.186389;
					8=NA;
					NA=NA")

analysis_data$long_leg3<-car::recode(data_transport$transport_start_3,"
					1=37.118976;
					2=37.3285435;
					3=36.8856703;
					4=37.212256;
					5=37.5662467;
					6=37.4419273;
					7=37.068611;
					8=NA;
					NA=NA")

analysis_data$lat_leg4<-car::recode(data_transport$transport_start_4,"
					1=-3.5286807;
					2=-3.3227432;
					3=-3.166825;
					4=-3.1413061;
					5=-3.6608888;
					6=-4.2118703;
					7=-3.186389;
					8=NA;
					NA=NA")

analysis_data$long_leg4<-car::recode(data_transport$transport_start_4,"
					1=37.118976;
					2=37.3285435;
					3=36.8856703;
					4=37.212256;
					5=37.5662467;
					6=37.4419273;
					7=37.068611;
					8=NA;
					NA=NA")

#Transport mode Leg 1
table<-with(data_prehospital,table(transport_leg1))
table
prop.table(table)
table<-with(data_prehospital,table(transport_leg1,gcs_cat))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#Transport mode leg 2
table<-with(data_prehospital,table(transport_leg2))
table
prop.table(table)
table<-with(data_prehospital,table(transport_leg2,gcs_cat))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

library(ggplot2)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)

data_bargraph<-with(data_prehospital,data.frame(
	transport_mode_1,
	transport_mode_2,
	transport_mode_3,
	transport_mode_4))

data_bargraph$transport_mode_1<-car::recode(
	data_bargraph$transport_mode_1,"
	3=9;
	91=NA;
	99=NA")
data_bargraph$transport_mode_2<-car::recode(
	data_bargraph$transport_mode_2,"
	3=9;
	91=NA;
	99=NA")
data_bargraph$transport_mode_3<-car::recode(
	data_bargraph$transport_mode_3,"
	3=9;	
	91=NA;
	99=NA")

table1<-count(data_bargraph,'transport_mode_1')
names(table1)[1]<-"mode"
table1$cat<-"Leg 1"	
table2<-count(data_bargraph,'transport_mode_2')
names(table2)[1]<-"mode"
table2$cat<-"Leg 2"	
table3<-count(data_bargraph,'transport_mode_3')
names(table3)[1]<-"mode"
table3$cat<-"Leg 3"	
table4<-count(data_bargraph,'transport_mode_4')
names(table4)[1]<-"mode"
table4$cat<-"Leg 4"

table_data<-rbind(table1,table2,table3,table4)	
table_data<-na.omit(table_data)

table_data$sum<-table_data$cat
table_data[table_data$cat=='Leg 1',]$sum<-sum(table_data
	[table_data$cat=='Leg 1',]$freq)
table_data[table_data$cat=='Leg 2',]$sum<-sum(table_data
	[table_data$cat=='Leg 2',]$freq)
table_data[table_data$cat=='Leg 3',]$sum<-sum(table_data
	[table_data$cat=='Leg 3',]$freq)
table_data[table_data$cat=='Leg 4',]$sum<-sum(table_data
	[table_data$cat=='Leg 4',]$freq)
# table_data[table_data$cat==5,]$sum<-sum(table_data
# 	[table_data$cat==5,]$freq)
# table_data[table_data$cat==6,]$sum<-sum(table_data
# 	[table_data$cat==6,]$freq)
# table_data[table_data$cat==7,]$sum<-sum(table_data
# 	[table_data$cat==7,]$freq)
# table_data[table_data$cat==8,]$sum<-sum(table_data
# 	[table_data$cat==8,]$freq)
# table_data[table_data$cat==9,]$sum<-sum(table_data
# 	[table_data$cat==9,]$freq)
table_data$sum<-as.numeric(table_data$sum)

table_data$mode<-car::recode(table_data$mode,"
	1='Car';
	2='Taxi';
	4='Boda Boda';
	5='Dala Dala';
	6='Police vehicle';
	7='Ambulance';
	8='Bus';
	9='Other'")

table_data$percentage<-round((table_data$freq/as.numeric(table_data$sum))*100,1)

table_data<-table_data[order(table_data$cat,table_data$mode,decreasing = c(FALSE, TRUE),method="radix"),]

table_data <- ddply(table_data, .(cat),
                    transform, 
                    pos = cumsum(percentage) - (0.5 * percentage))

setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
postscript("/Users/Joao/Desktop/[GLOBAL EM] tz_prehospitalTBI_figure1.eps",
	width = 8, height = 12)
#Add plot
ggplot() + 
# theme_bw() + 
geom_bar(aes(y = percentage, 
							  x = cat, 
							  fill = mode), 
							  data = table_data,
                           	  stat="identity") +
geom_text(data=table_data, aes(x = cat, 
										   y = pos,
                                           label = paste0(percentage,"%")), 
										   size=4) +
labs(x="Journey legs", y="Percentage") +
scale_y_continuous(labels = dollar_format(
	suffix = "%", prefix = "")) +
theme_minimal() +
scale_fill_brewer(palette="Paired") #+
#coord_flip()
dev.off()

# library(rworldmap)
# library(ggmap)


##############################################################
#Table 2
#############################################################

logmodel_gcs<-glm(gcs_cat ~ time_to_care_cat + 
						transport_legs_cat + 
						age + 
						male +
                        moi + 
                        alcohol +
                        transport_leg1	
                       ,family=binomial, data=analysis_data)
summary(logmodel_gcs)
#anova(reglogGEU)
#exp(coef(model1_death)) # exponentiated coefficients
#exp(confint(model1_death)) # 95% CI for exponentiated coefficients
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
#logistic.display(baselineXFUP3)

logmodel_gos<-glm(gos_cat ~ time_to_care_cat + 
						transport_legs_cat + 
						age + 
						male +
                        moi + 
                        alcohol +
                        transport_leg1 +
                        gcs_cat
                       ,family=binomial, data=analysis_data)
summary(logmodel_gos)
#anova(reglogGEU)
#exp(coef(model1_death)) # exponentiated coefficients
#exp(confint(model1_death)) # 95% CI for exponentiated coefficients
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
# logistic.display(logmodel)

# plot_odds<-function(x, title = NULL){
tmp_gcs<-data.frame(cbind(exp(coef(logmodel_gcs)),
	exp(confint(logmodel_gcs))))
tmp_gos<-data.frame(cbind(exp(coef(logmodel_gos)),
	exp(confint(logmodel_gos))))
tmp<-rbind(tmp_gcs,tmp_gos)
odds<-tmp[-c(1,16,31),]
names(odds)<-c('OR', 'lower', 'upper')
odds$vars<-rep(c("1-2 hrs vs. 0-1 hrs",
			 "2-3 hrs vs. 0-1 thrs",
			 "3-4 hrs vs. 0-1 hrs",
			 "4 or more hrs vs. 0-1 hrs",
			 "# Journey legs",
			 "Age",
			 "Male vs. Female",
			 "RTI vs. non-RTI",
			 "Alcohol use vc. Abstainer",
			 "Moshi Rural vs. Moshi Urban",
			 "Hai vs. Moshi Urban",
			 "Rombo vs. Moshi Urban",
			 "Mwanga vs. Moshi Urban",
			 "Same vs. Moshi Urban"),2)
odds$groups<-rep(c(rep("Time to care",4),
					"Course points",
					"Age", 
					"Gender",
					"MOI",
					"Alcohol use",
					rep("District",5)),2)
odds$models<-c(rep("Mild vs. Moderate/Severe TBI",14),
			   rep("Good vs. Poor Outcome",14))
#ticks<-c(seq(.1, 1, by =.1), seq(0, 10, by =1), seq(10, 100, by =10))

setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
postscript("/Users/Joao/Desktop/[GLOBAL EM] tz_prehospitalTBI_figure2.eps",
	width = 8, height = 6)
ggplot(odds, aes(y= OR, x = reorder(vars, OR))) +
geom_point() +
geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
#scale_y_log10(breaks=ticks, labels = ticks) +
geom_hline(yintercept = 1, linetype=2) +
scale_x_discrete(limits=c(
			 "4 or more hrs vs. 0-1 hrs",
			 "3-4 hrs vs. 0-1 hrs",
			 "2-3 hrs vs. 0-1 thrs",
			 "1-2 hrs vs. 0-1 hrs",
			 "# Journer legs",
			 "Age",
			 "Male vs. Female",
			 "RTI vs. non-RTI",
			 "Alcohol use vc. Abstainer",
			 "Moshi Rural vs. Moshi Urban",
			 "Hai vs. Moshi Urban",
			 "Rombo vs. Moshi Urban",
			 "Mwanga vs. Moshi Urban",
			 "Same vs. Moshi Urban")) +
facet_grid(.~models, scales="free_y") +
coord_flip() +
labs(x = 'Predictors of TBI Outcomes', y = 'OR (CI 95%)') +
theme_bw()
# }
dev.off()

write.csv(analysis_data,"/Users/Joao/Desktop/template_odds_forestplot_ggplot2.csv")
	
# plot_odds(logmodel)
