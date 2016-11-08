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
 
 # install.packages("VIM")
 # install.packages("VIMGUI")
 # install.packages("miP")
 # install.packages("gWidgetsRGtk2")
 # install.packages("mi")
 # install.packages("epicalc")
 # install.packages("vcd")
 # install.packages("grid")
 
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
data <- read.csv("/Users/jnv4/Box Sync/Home Folder jnv4/Data/Global EM/Africa/Rwanda/RwandaSurvey_DATA_2016-08-15_1346.csv")

######################################################
#DATA MANAGEMENT
######################################################
summary(data)

ses_data<-with(data, data.frame(age,
                                male,
                                occupation,
                                hours_wkvehicle,
                                day_wkvehicle,
                                # time_start_wk,
                                # time_stop_wk,
                                years_wkvehicle=time_motodr
                                # vehicle_home,
                                # hours_hmvehicle
                                ))


outcome_data<-with(data,data.frame(crash_lifetime,
                                   work_crash,
                                   injured_crash,
                                   injuries_crash___0,
                                   injuries_crash___1,
                                   injuries_crash___2,
                                   injuries_crash___3,
                                   injuries_crash___4,
                                   injuries_crash___5,
                                   injuries_crash___6,
                                   injuries_crash___7,
                                   injuries_crash___8,
                                   injuries_crash___9,
                                   injuries_crash___10,
                                   hosp_crash,
                                   day_hosp_crash,
                                   disability_crash,
                                   missed_work_crash,
                                   rehabrec_crash,
                                   crash_year,
                                   # number_crash_yr,
                                   near_miss_month
                                   ))

safety_habits_data<-with(data,data.frame(crash_opinion___0,
                                         crash_opinion___1,
                                         crash_opinion___2, 
                                         crash_opinion___3,
                                         crash_opinion___4,
                                         crash_opinion___5,
                                         crash_opinion___6, 
                                         crash_opinion___90,
                                         crash_moto,
                                         crash_car,
                                         crash_bus,
                                         crash_peds,
                                         helmet_mc,
                                         helmet_colleagues,
                                         colleagues_risks,
                                         helmet_strap_use,
                                         hairnets_available, 
                                         headlights_always,
                                         headlights_night,
                                         helmet_damage,
                                         cracks_dhelmet,
                                         scratches_dhelmet,
                                         strap_dhelmet,
                                         glass_helmet,
                                         fit_helmet
                                         ))

#sub-sets for numeric
# numeric
# numericRTC <- subset(numeric,numeric$crash_lifetime=="1")
# summary(numericRTC)
# numericNRTC <- subset(numeric, numeric$crash_lifetime=="0")
# summary(numericNRTC)

#recoding safe hab to N/A NR and , 0,1,2,3 = uso inadequado
safety_habits_data$helmet_mc<-car::recode(safety_habits_data$helmet_mc, "c(0,1,2,3)='0'; 4='1'; NA='2'")
safety_habits_data$helmet_strap_use<-car::recode(safety_habits_data$helmet_strap_use, "c(0,1,2,3)='0'; 4='1'; NA='2'")
safety_habits_data$hairnets_available<-car::recode(safety_habits_data$hairnets_available, "c(0,1,2,3)='0'; 4='1'; NA='2'")
safety_habits_data$headlights_always<-car::recode(safety_habits_data$headlights_always, "c(0,1,2,3)='0'; 4='1'; NA='2'")
safety_habits_data$headlights_night<-car::recode(safety_habits_data$headlights_night, "c(0,1,2,3)='0'; 4='1'; NA='2'")
safety_habits_data$helmet_damage<-car::recode(safety_habits_data$helmet_damage, "c(0,1,2,3)='0'; 4='1'; NA='2'")


#recoding outcomes
safety_habits_data$crash_lifetime<-as.factor(safety_habits_data$crash_lifetime)

#recoding hours worked per week
ses_data$hour_week<-ses_data$hours_wkvehicle*ses_data$day_wkvehicle
######################################################
#TABLE 1
######################################################
## demographics ##

table(safety_habits_data$crash_lifetime)

#age
describe(ses_data$age)
describeBy(ses_data$age,safety_habits_data$crash_lifetime)
t.test(ses_data$age~safety_habits_data$crash_lifetime)

#hours per day 
describe(ses_data$hours_wkvehicle)
describeBy(ses_data$hours_wkvehicle,safety_habits_data$crash_lifetime)
t.test(ses_data$hours_wkvehicle~safety_habits_data$crash_lifetime)

#days per wk
describe(ses_data$day_wkvehicle)
describeBy(ses_data$day_wkvehicle,safety_habits_data$crash_lifetime)
t.test(ses_data$day_wkvehicle~safety_habits_data$crash_lifetime)

#hours per wk
describe(ses_data$hour_week)
describeBy(ses_data$hour_week,safety_habits_data$crash_lifetime)
t.test(ses_data$hour_week~safety_habits_data$crash_lifetime)

# #work start time - make 19:30 - 19,5...
# table(numeric$time_start_wk)
# table(ses_data$time_start_wk)
# describe(numericNRTC$time_start_wk)

# #work end time
# describe(numeric$time_stop_wk)
# describe(ses_data$time_stop_wk)
# describe(numericNRTC$time_stop_wk)

#years worked
describe(ses_data$years_wkvehicle)
describeBy(ses_data$years_wkvehicle,safety_habits_data$crash_lifetime)
t.test(ses_data$years_wkvehicle~safety_habits_data$crash_lifetime)

#helmet_mc
table(safety_habits_data$helmet_mc)
describe(ses_data$helmet_mc)
describe(numericNRTC$helmet_mc)

#helmet_strap_use
describe(numeric$helmet_strap_use)
describe(ses_data$helmet_strap_use)
describe(numericNRTC$helmet_strap_use)

## safety_habits

#safe habs
helmet_mctable <- table(numeric$helmet_mc ,numeric$crash_lifetime)
assocstats(helmet_mctable)

helmet_strap_use <- table(numeric$helmet_strap_use ,numeric$helmet_strap_use)
assocstats(helmet_strap_use)

######################################################
#TABLE 2
######################################################

#sufered a RTC
RTI <- table(outcome_data$crash_lifetime)
RTI
prop.table(RTI)

#sufered a RTI
RTI <- table(outcome_data$injured_crash)
RTI
prop.table(RTI)

#near_miss
NM <- table(outcome_data$near_miss_month)
NM
prop.table(NM)

#Type of injury
injury1 <- table(outcome_data$injuries_crash___0)
injury1
prop.table(injury1)

injury1 <- table(outcome_data$injuries_crash___1)
injury1
prop.table(injury1)

injury1 <- table(outcome_data$injuries_crash___2)
injury1
prop.table(injury1)

injury1 <- table(outcome_data$injuries_crash___3)
injury1
prop.table(injury1)

injury1 <- table(outcome_data$injuries_crash___4)
injury1
prop.table(injury1)

injury1 <- table(outcome_data$injuries_crash___5)
injury1
prop.table(injury1)

injury1 <- table(outcome_data$injuries_crash___6)
injury1
prop.table(injury1)

injury1 <- table(outcome_data$injuries_crash___7)
injury1
prop.table(injury1)

injury1 <- table(outcome_data$injuries_crash___8)
injury1
prop.table(injury1)

injury1 <- table(outcome_data$injuries_crash___9)
injury1
prop.table(injury1)

injury1 <- table(outcome_data$injuries_crash___10)
injury1
prop.table(injury1)

#Hospital crash
hosp <- table(outcome_data$hosp_crash)
hosp
prop.table(hosp)

#LOS
describe(outcome_data$day_hosp_crash)

#disability_crash
disab <- table(outcome_data$disability_crash)
disab
prop.table(disab)

#rehab
describe(outcome_data$rehabrec_crash)

#crash_year
crash_year <- table(outcome_data$crash_year)
crash_year
prop.table(crash_year)

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