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
 
 install.packages("VIM")
 install.packages("VIMGUI")
 install.packages("miP")
 install.packages("gWidgetsRGtk2")
 install.packages("mi")
 install.packages("epicalc")
 install.packages("vcd")
 install.packages("grid")
 
#Load packages neededz for the analysis
#All packages must be installes with install.packages() function
lapply(c("sem","ggplot2", "psych", "RCurl", "irr", "nortest", 
	"moments","GPArotation","nFactors","boot","psy", "car",
	"vcd", "gridExtra","mi","VIM","epicalc","gdata","sqldf",
	"reshape2","mclust","foreign","survival","memisc","lme4",
	"lmerTest","dplyr"),library, character.only=T)
library(vcd)
#Package and codes to pull data from goodle sheets
#devtools::install_github("jennybc/googlesheets")
#library(googlesheets)
#suppressMessages(library(dplyr))

######################################################
#IMPORTING DATA
######################################################

#uploading data ---------------------------------------------------------------------
#Load the data set.
#All data are stored in  http://figshare.com/articles/The_reliability_of_AO_classification_on_femur_fractures_among_orthopedic_residents/103664
#Note that the data set has been reorganized to be applied to some functions

#baseline_lena<-read.csv("/Users/rpietro/Google Drive/research_groups/RoR/IPq/Suicide_Anxiety/baseline.csv",sep=",")
 
#Pulling data from dropbox
#data_hamilton <- repmis::source_DropboxData("lena_hamilton.csv","r31zt5zeiygsc23",sep = ",",header = TRUE)

#pulling data from Google Spreadsheet - using http://goo.gl/VV4o1g
#authorize(new_user = TRUE)
#my_sheets <- list_sheets()
	#sheet_rs <- register_ss("Copy of Rattlesnake data")
#data_rs  <- get_via_lf(sheet_rs, ws = "Coagulation Data")
#demographics_rs<-get_via_lf(sheet_rs, ws = "Demographics") 

#sheet_cp <- register_ss("Copy of Copperhead data")#
#data_cp <- get_via_lf(sheet_cp, ws = "Coagulation Data") 
#demographics_cp<-get_via_lf(sheet_cp, ws = "Demographics") 

#data_cp<-read.csv("/home/joao/Desktop/data_cp.csv",sep=',')
#data_rs<-read.csv("/home/joao/Desktop/data_rs.csv",sep=',')
data <- read.csv("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/Africa/Rwanda/RwandaSurvey_DATA_2016-08-15_1346.csv")

######################################################
#DATA MANAGEMENT
######################################################
summary(data)
numeric <- with(data, data.frame(age, male, occupation, hours_wkvehicle, day_wkvehicle,
                                 time_start_wk, time_stop_wk, time_motodr, vehicle_home,
                                 hours_hmvehicle, crash_lifetime, rem_date_crash, day_crash,
                                 time_crash, type_crash, type_crash_other, work_crash,
                                 injured_crash, injuries_crash___0, injuries_crash___1, 
                                 injuries_crash___2, injuries_crash___3, injuries_crash___4, 
                                 injuries_crash___5, injuries_crash___6, injuries_crash___7, 
                                 injuries_crash___8, injuries_crash___9, injuries_crash___10,
                                 injuries_crash___90, injuries_crash___99, injuries_crash_other,
                                 hosp_crash, day_hosp_crash, disability_crash, type_disability_crash,
                                 type_disability_other, rehabrec_crash, rehab_crash, crash_year,
                                 number_crash_yr, rem_date_ycrash, date_ycrash, day_ycrash,
                                 time_ycrash, type_ycrash_other, wk_ycrash, injured_ycrash, 
                                 hosp_ycrash, day_hosp_ycrash, injuries_ycrash___0, injuries_ycrash___1, 
                                 injuries_ycrash___2, injuries_ycrash___3, injuries_ycrash___4, 
                                 injuries_ycrash___5, injuries_ycrash___6, injuries_ycrash___7, 
                                 injuries_ycrash___8, injuries_ycrash___9, injuries_ycrash___10,
                                 injuries_ycrash___90, injuries_ycrash___99, injuries_ycrash_other,
                                 disability_ycrash, missed_work_ycrash, rehabrec_ycrash,  rehab_ycrash,
                                 near_miss_month, crash_opinion___0, crash_opinion___1, crash_opinion___2, 
                                 crash_opinion___3, crash_opinion___4, crash_opinion___5, crash_opinion___6, 
                                 crash_opinion___90, crash_moto, crash_car, crash_bus, crash_peds, helmet_mc,
                                 helmet_colleagues, colleagues_risks, helmet_strap_use, hairnets_available, 
                                 headlights_always, headlights_night, helmet_damage))

#sub-sets for numeric
numeric
numericRTC <- subset(numeric,numeric$crash_lifetime=="1")
summary(numericRTC)
numericNRTC <- subset(numeric, numeric$crash_lifetime=="0")
summary(numericNRTC)

#recoding safe hab to N/A NR and , 0,1,2,3 = uso inadequado
numeric$helmet_mc<-car::recode(numeric$helmet_mc, "c(0,1,2,3)='0'; 4='1'; NA='2'")
numeric$helmet_strap_use<-car::recode(numeric$helmet_strap_use, "c(0,1,2,3)='0'; 4='1'; NA='2'")
numeric$hairnets_available<-car::recode(numeric$hairnets_available, "c(0,1,2,3)='0'; 4='1'; NA='2'")
numeric$headlights_always<-car::recode(numeric$headlights_always, "c(0,1,2,3)='0'; 4='1'; NA='2'")
numeric$headlights_night<-car::recode(numeric$headlights_night, "c(0,1,2,3)='0'; 4='1'; NA='2'")
numeric$helmet_damage<-car::recode(numeric$helmet_damage, "c(0,1,2,3)='0'; 4='1'; NA='2'")


#recoding outcomes
numeric$crash_lifetime<-as.factor(numeric$crash_lifetime)

#recoding hours worked per week
numeric$hour_week<-numeric$hours_wkvehicle*numeric$day_wkvehicle
######################################################
#DESCRIPTIVE ANALYSIS
######################################################
## demographics ##
#age
describe(numeric$age)
describe(numericRTC$age)
describe(numericNRTC$age)
#hours per day 
describe(numeric$hours_wkvehicle)
describe(numericRTC$hours_wkvehicle)
describe(numericNRTC$hours_wkvehicle)
#days per wk
describe(numeric$day_wkvehicle)
describe(numericRTC$day_wkvehicle)
describe(numericNRTC$day_wkvehicle)
#hours per wk
describe(numeric$hour_week)
describe(numericRTC$hour_week)
describe(numericNRTC$hour_week)
#work start time - make 19:30 - 19,5...
describe(numeric$time_start_wk)
describe(numericRTC$time_start_wk)
describe(numericNRTC$time_start_wk)
#work end time
describe(numeric$time_stop_wk)
describe(numericRTC$time_stop_wk)
describe(numericNRTC$time_stop_wk)
#years worked
describe(numeric$time_motodr)
describe(numericRTC$time_motodr)
describe(numericNRTC$time_motodr)

#helmet_mc
describe(numeric$helmet_mc)
describe(numericRTC$helmet_mc)
describe(numericNRTC$helmet_mc)
#helmet_strap_use
describe(numeric$helmet_strap_use)
describe(numericRTC$helmet_strap_use)
describe(numericNRTC$helmet_strap_use)

## safety_habits
#total
table(numeric$helmet_mc)
table(numeric$helmet_strap_use)
table(numeric$hairnets_available)
table(numeric$headlights_always)
table(numeric$headlights_night)
table(numeric$helmet_damage)

#RTC - Y 
table(numericRTC$helmet_mc)
table(numericRTC$helmet_strap_use)
table(numericRTC$hairnets_available)
table(numericRTC$headlights_always)
table(numericRTC$headlights_night)
table(numericRTC$helmet_damage)

#RTC - N
table(numericNRTC$helmet_mc)
table(numericNRTC$helmet_strap_use)
table(numericNRTC$hairnets_available)
table(numericNRTC$headlights_always)
table(numericNRTC$headlights_night)
table(numericNRTC$helmet_damage)

#---------------

#sufered a RTI
RTI <- table(numericRTC$injured_crash)
RTI
prop.table(RTI)
#near_miss
NM <- table(numericRTC$near_miss_month)
NM
prop.table(NM)

## p.values ##
#demographics
t.test(numeric$age~numeric$crash_lifetime,paired=FALSE) # where y is numeric and x is a binary factora
t.test(numeric$hours_wkvehicle~numeric$crash_lifetime,paired=FALSE)
t.test(numeric$day_wkvehicle~numeric$crash_lifetime,paired=FALSE)
t.test(numeric$time_start_wk~numeric$crash_lifetime,paired=FALSE)
t.test(numeric$time_stop_wk~numeric$crash_lifetime,paired=FALSE)
t.test(numeric$time_motodr~numeric$crash_lifetime,paired=FALSE)
t.test(numeric$hour_week~numeric$crash_lifetime,paired=FALSE)
#
t.test(numeric$helmet_mc~numeric$crash_lifetime,paired=FALSE)
t.test(numeric$helmet_strap_use~numeric$crash_lifetime,paired=FALSE)
t.test(numeric$hairnets_avaliable~numeric$crash_lifetime,paired=FALSE)
t.test(numeric$headlights_always~numeric$crash_lifetime,paired=FALSE)
t.test(numeric$headlights_nights~numeric$crash_lifetime,paired=FALSE)
t.test(numeric$helmet_damage~numeric$crash_lifetime,paired=FALSE)

#safe habs
helmet_mctable <- table(numeric$helmet_mc ,numeric$crash_lifetime)
assocstats(helmet_mctable)

helmet_strap_use <- table(numeric$helmet_strap_use ,numeric$helmet_strap_use)
assocstats(helmet_strap_use)

######################################################
#TABLE 3.
######################################################
## OR table ##
#crashlifetime
crashlifetime <-glm(as.factor(numeric$crash_lifetime) ~ numeric$age + 
					# numeric$hours_wkvehicle + 
					# numeric$day_wkvehicle +
     #          		# numeric$time_start_wk + 
     #          		# numeric$time_stop_wk + 
              		numeric$time_motodr + 
              		numeric$helmet_mc +
              		numeric$belt_driver + 
              		numeric$belt_back + 
              		numeric$belt_passenger +
            	  	numeric$stop_fast + 
            	  	numeric$road_wrongside
                   ,family=binomial, data=numeric)
summary(crashlifetime)
exp(coef(crashlifetime))
exp(confint(crashlifetime))
logistic.display(crashlifetime)

#injured_crash
num_injuried<-subset(numeric,numeric$crash_lifetime=="1")
injuredcrash <-glm(as.factor(num_injuried$injured_crash) ~ 
					num_injuried$age + 
					# num_injuried$hours_wkvehicle + 
					# num_injuried$day_wkvehicle +
              		# num_injuried$time_start_wk + 
              		# num_injuried$time_stop_wk + 
              		num_injuried$time_motodr + 
              		num_injuried$helmet_mc +
              		num_injuried$belt_driver + 
              		num_injuried$belt_back + 
              		num_injuried$belt_passenger +
            	  	num_injuried$stop_fast + 
            	  	num_injuried$road_wrongside
                   ,family=binomial, data=num_injuried)
summary(injuredcrash)
exp(coef(injuredcrash))
exp(confint(injuredcrash))
logistic.display(injuredcrash)

#near miss
# num_nearmiss<-subset(numeric,numeric$crash_lifetime=="1")
nearmiss <-glm(as.factor(numeric$near_miss_month) ~ 
					numeric$age + 
					# numeric$hours_wkvehicle + 
					# numeric$day_wkvehicle +
              		# numeric$time_start_wk + 
              		# numeric$time_stop_wk + 
              		numeric$time_motodr + 
              		numeric$helmet_mc +
              		numeric$belt_driver + 
              		numeric$belt_back + 
              		numeric$belt_passenger +
            	  	numeric$stop_fast + 
            	  	numeric$road_wrongside
                   ,family=binomial, data=numeric)
summary(nearmiss)
exp(coef(nearmiss))
exp(confint(nearmiss))
logistic.display(nearmiss)

#disab
num_hosp<-subset(data,data$crash_lifetime=="1")
disab <-glm(as.factor(num_disab$disability_crash) ~ 
					num_disab$age + 
					# num_disab$hours_wkvehicle + 
					# num_disab$day_wkvehicle +
              		# num_disab$time_start_wk + 
              		# num_disab$time_stop_wk + 
              		num_disab$time_motodr + 
              		num_disab$helmet_mc +
              		num_disab$belt_driver + 
              		# num_disab$belt_back + 
              		num_disab$belt_passenger +
            	  	num_disab$stop_fast
            	  	# num_disab$road_wrongside
                   ,family=binomial, data=num_disab)
summary(disab)
exp(coef(disab))
exp(confint(disab))
logistic.display(disab)

#los
num_hosp<-subset(data,data$injured_crash=="1")
los <-glm(as.factor(num_hosp$day_hosp_crash) ~ 
					num_hosp$age + 
					num_hosp$hours_wkvehicle + 
					num_hosp$day_wkvehicle +
              		# num_hosp$time_start_wk + 
              		# num_hosp$time_stop_wk + 
              		num_hosp$time_motodr + 
              		num_hosp$helmet_mc +
              		# num_hosp$belt_driver + 
              		# num_hosp$belt_back + 
              		num_hosp$belt_passenger +
            	  	num_hosp$stop_fast
            	  	# num_hosp$road_wrongside
                   ,family=binomial, data=num_hosp)
summary(los)
exp(coef(los))
exp(confint(los))

#days/work
num_work<-subset(data,data$injured_crash=="1")
dayswork <-glm(as.factor(num_work$missed_work_crash) ~ 
					num_work$age + 
					num_work$hours_wkvehicle + 
					num_work$day_wkvehicle +
              		# num_work$time_start_wk + 
              		# num_work$time_stop_wk + 
              		num_work$time_motodr + 
              		num_work$helmet_mc +
              		# num_work$belt_driver + 
              		# num_work$belt_back + 
              		num_work$belt_passenger +
            	  	num_work$stop_fast
            	  	# num_work$road_wrongside
                   ,family=binomial, data=num_work)
summary(dayswork)
exp(coef(dayswork))
exp(confint(dayswork))

######################################################
#PRINCIPAL COMPONENTS
######################################################