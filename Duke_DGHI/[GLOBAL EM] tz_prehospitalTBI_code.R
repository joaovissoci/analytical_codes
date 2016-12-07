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
	"ltm","gmodels","eRm","mirt","dplyr","devtools","reshape"),
library, character.only=T)

#Package and codes to pull data from goodle sheets
#devtools::install_github("jennybc/googlesheets")
#library(googlesheets)
#suppressMessages(library(dplyr))

######################################################
#IMPORTING DATA
######################################################

data<-read.csv("/Users/jnv4/Desktop/TBI_2015-09-25_1209_Clean.csv")

######################################################
#DATA MANAGEMENT
######################################################

names(data)

#getting time data

#Time to treatment
#tranforming time from hrs into min
T1<-(data$t1_hrs*60)+data$t1_min
T2<-(data$t2_hrs*60)+data$t2_min
T3<-(data$t3_hrs*60)+data$t3_min
T4<-(data$t4_hrs*60)

#aggregating times
T_total<-data.frame(T1,T2,T3,T4)

#summing time from all legs
time_transport<-base::rowSums(T_total,na.rm=TRUE)

#Time to care
#transforming time 
# Tt1<-(data$time_injury1*60)+data$time_injury2
# Tt2<-(data$time_arrival1*60)+data$time_arrival2

# Tt_total<-na.omit(data.frame(Tt1,Tt2))
# Tt_total<-Tt_total[1]-

# T_total<-base::rowSums(T_total,na.rm=TRUE)

#Time to reach hospital
injuryT<-with(data,paste(inj_date, inj_time, sep=" "))
injury_time <- as.POSIXct(injuryT,
                      format='%m/%d/%y %H:%M')

arrivalT<-with(data,paste(date_arrival,time_arrival, sep=" "))
arrival_time <- as.POSIXct(arrivalT,
                      format='%m/%d/%y %H:%M')

dif_time<-difftime(injury_time, arrival_time,
         units = c("min"))*-1

time_to_care<-car::recode(dif_time,"-9435=NA;-15=NA;
	-524370=NA;-2730=NA;-510=NA")

# quantile(dif_time, probs=seq(0,1,0.10),na.rm=TRUE)

# errors<-car::recode(dif_time,"-524400:0='weird';
# 							 0.0001:10000='normal';
# 							 10000:1057080='weird'")

# error_log<-data.frame(errors,dif_time,id=data$study_id)

# error_log_weird<-subset(error_log,error_log$errors=='weird')

# write.csv(error_log_weird,"/Users/jnv4/Desktop/error.csv")

#transport cost
transport_cost<-with(data,rowSums(
	data.frame(transport_cost_1,
			   transport_cost_2,
			   transport_cost_3,
			   transport_cost_4),
	na.rm=TRUE))

#creating transportation legs variable
transport_leg_temp<-with(data,data.frame(t1_min,
										 t2_min,
										 t3_min))

transport_leg_temp$t1_min<-car::recode(
	transport_leg_temp$t1_min,
	"NA=0;else=1")
transport_leg_temp$t2_min<-car::recode(
	transport_leg_temp$t2_min,
	"NA=0;else=1")
transport_leg_temp$t3_min<-car::recode(
	transport_leg_temp$t3_min,
	"NA=0;else=1")

transport_legs<-rowSums(transport_leg_temp)

#transportation mode
data$transport_leg1<-car::recode(data$transport_mode_1,
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

data$transport_leg2<-car::recode(data$transport_mode_2,
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
analysis_data<-with(data,data.frame(age,
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
									# t1_min,                      
									transport_leg2,           
									# transport_mode_other_2,      
									# transport_cost_2,           
									# transport_start_name_2,      
									# transport_start_2,          
									# transport_end_name_2,        
									# transport_end_district_2,   
									# t2_hrs,                      
									# t2_min,                     
									# transport_cost_3,            
									# transport_start_name_3,     
									# transport_start_3,           
									# transport_end_name_3,       
									# transport_end_district_3,    
									# t3_hrs,                     
									# t3_min,                      
									# transport_cost_4,            
									# transport_start_name_4,     
									# transport_start_4,           
									# transport_end_name_4,       
									# transport_end_district_4,    
									# t4_hrs,                     
									# t4_min,                                             
									# ref,                 
									time_transport,
									time_to_care,
									transport_legs))

#recoding gos
analysis_data$gos_cat<-car::recode(
	analysis_data$gos,"1='Death';
					   2:5='Alive'")

#recoding gcs
analysis_data$gcs_cat<-car::recode(
	analysis_data$gcs_tot,"1:8='Severe';
					   9:15='Non-severe'")

#recoding gcs
analysis_data$alcohol<-car::recode(
	analysis_data$alcohol,"0='No';
					   	   1='Yes';
					   	   2='No'")

#Imputation

##############################################################
#Descriptives
#############################################################
#gcs
table<-with(analysis_data,table(gcs_cat))
table
prop.table(table)

#Age
with(analysis_data,describe(age))
#ad.test(bea_data$density_pedestrian)
#hist(bea_data$density_pedestrian)
#ci_func(bea_data$density_pedestrian,.95)
with(analysis_data,describeBy(age,gcs_cat))
with(analysis_data,t.test(age~gcs_cat))

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

# GOS
table<-with(analysis_data,table(gos_cat))
table
prop.table(table)
table<-with(analysis_data,table(gos_cat,gcs_cat))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

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

### Time
#Transport time
with(analysis_data,summary(time_transport))
#ad.test(bea_data$density_pedestrian)
#hist(bea_data$density_pedestrian)
#ci_func(bea_data$density_pedestrian,.95)
with(analysis_data,by(time_transport,gcs_cat,summary))
with(analysis_data,wilcox.test(time_transport~gcs_cat))

#Transport time
with(analysis_data,summary(time_to_care))
#ad.test(bea_data$density_pedestrian)
#hist(bea_data$density_pedestrian)
#ci_func(bea_data$density_pedestrian,.95)
with(analysis_data,by(time_to_care,gcs_cat,summary))
with(analysis_data,wilcox.test(time_to_care~gcs_cat))

#Transport cost
with(analysis_data,summary(transport_cost))
#ad.test(bea_data$density_pedestrian)
#hist(bea_data$density_pedestrian)
#ci_func(bea_data$density_pedestrian,.95)
with(analysis_data,by(transport_cost,gcs_cat,summary))
with(analysis_data,wilcox.test(transport_cost~gcs_cat))

#Transport legs
with(analysis_data,summary(transport_legs))
#ad.test(bea_data$density_pedestrian)
#hist(bea_data$density_pedestrian)
#ci_func(bea_data$density_pedestrian,.95)
with(analysis_data,by(transport_legs,gcs_cat,summary))
with(analysis_data,wilcox.test(transport_legs~gcs_cat))

#Transport mode Leg 1
table<-with(analysis_data,table(transport_leg1))
table
prop.table(table)
table<-with(analysis_data,table(transport_leg1,gcs_cat))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#Transport mode leg 2
table<-with(analysis_data,table(transport_leg2))
table
prop.table(table)
table<-with(analysis_data,table(transport_leg2,gcs_cat))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package
