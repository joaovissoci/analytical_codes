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
	"lmerTest","dplyr","mice"),library, character.only=T)
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
                                years_wkvehicle=time_motodr,
                                # vehicle_home,
                                # hours_hmvehicle,
                                time_crash
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
                                   type_disability_crash,
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
                                         fit_helmet,
                                         helmet_value,
                                         helmet_strap_value
                                         ))

#sub-sets for numeric
# numeric
# numericRTC <- subset(numeric,numeric$crash_lifetime=="1")
# summary(numericRTC)
# numericNRTC <- subset(numeric, numeric$crash_lifetime=="0")
# summary(numericNRTC)

#recoding safe hab to N/A NR and , 0,1,2,3 = uso inadequado
safety_habits_data$helmet_mc<-car::recode(safety_habits_data$helmet_mc, "c(0,1,2,3)='0'; 4='1'")
safety_habits_data$helmet_strap_use<-car::recode(safety_habits_data$helmet_strap_use, "c(0,1,2,3)='0'; 4='1'")
safety_habits_data$hairnets_available<-car::recode(safety_habits_data$hairnets_available, "c(0,1,2,3)='0'; 4='1'")
safety_habits_data$headlights_always<-car::recode(safety_habits_data$headlights_always, "c(0,1,2,3)='0'; 4='1'")
safety_habits_data$headlights_night<-car::recode(safety_habits_data$headlights_night, "c(0,1,2,3)='0'; 4='1'")
safety_habits_data$helmet_damage<-car::recode(safety_habits_data$helmet_damage, "c(0,1,2,3)='0'; 4='1'")
safety_habits_data$helmet_value<-car::recode(safety_habits_data$helmet_value, "c(1,2,3,4)='0'; 5='1'")
safety_habits_data$helmet_strap_value<-car::recode(safety_habits_data$helmet_strap_value, "c(1,2,3,4)='0'; 5='1'")
safety_habits_data$colleagues_risks<-car::recode(safety_habits_data$colleagues_risks, "c(0,1,2,3)='0'; 4='1'")
safety_habits_data$helmet_colleagues<-car::recode(safety_habits_data$helmet_colleagues, "c(0,1,2,3)='0'; 4='1'")


#recoding outcomes
outcome_data$crash_lifetime<-as.factor(outcome_data$crash_lifetime)

#recoding hours worked per week
ses_data$hour_week<-ses_data$hours_wkvehicle*ses_data$day_wkvehicle

#Imputing missing data

ses_data_imputed<-mice(data.frame(ses_data,data$crash_lifetime,data$near_miss_month), seed = 2222, m=10)
ses_data<-complete(ses_data_imputed,4)

safety_habits_data_imputed<-mice(safety_habits_data, seed = 2222, m=10)
safety_habits_data<-complete(safety_habits_data_imputed,4)
safety_habits_data$helmet_mc<-car::recode(
  safety_habits_data$helmet_mc,"NA=1")

outcome_data$crash_lifetime<-ses_data$data.crash_lifetime
outcome_data$near_miss_month<-ses_data$data.near_miss_month
outcome_data$injured_crash<-car::recode(
  outcome_data$injured_crash,"NA=0")
outcome_data$hosp_crash<-car::recode(
  outcome_data$hosp_crash,"NA=0")
outcome_data$disability_crash<-car::recode(
  outcome_data$disability_crash,"NA=0")


######################################################
#TABLE 1
######################################################
## demographics ##

table(safety_habits_data$crash_lifetime)

#age
describe(ses_data$age)
describeBy(ses_data$age,outcome_data$crash_lifetime)
t.test(ses_data$age~outcome_data$crash_lifetime)

#hours per day 
describe(ses_data$hours_wkvehicle)
describeBy(ses_data$hours_wkvehicle,outcome_data$crash_lifetime)
t.test(ses_data$hours_wkvehicle~outcome_data$crash_lifetime)

#days per wk
describe(ses_data$day_wkvehicle)
describeBy(ses_data$day_wkvehicle,outcome_data$crash_lifetime)
t.test(ses_data$day_wkvehicle~outcome_data$crash_lifetime)

#hours per wk
describe(ses_data$hour_week)
describeBy(ses_data$hour_week,outcome_data$crash_lifetime)
t.test(ses_data$hour_week~outcome_data$crash_lifetime)

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
describeBy(ses_data$years_wkvehicle,outcome_data$crash_lifetime)
t.test(ses_data$years_wkvehicle~outcome_data$crash_lifetime)

#helmet_mc
table(safety_habits_data$helmet_mc)
describe(ses_data$helmet_mc)
describe(numericNRTC$helmet_mc)

#helmet_strap_use
describe(numeric$helmet_strap_use)
describe(ses_data$helmet_strap_use)
describe(numericNRTC$helmet_strap_use)

#safe habs
time_crash <- table(ses_data$time_crash)
time_crash
prop.table(time_crash)
helmet_mctable2 <- table(safety_habits_data$helmet_mc,
                        outcome_data$crash_lifetime)
helmet_mctable2
prop.table(helmet_mctable2)
assocstats(helmet_mctable1)

## safety_habits

#safe habs
helmet_mctable1 <- table(safety_habits_data$helmet_mc)
helmet_mctable1
prop.table(helmet_mctable1)
helmet_mctable2 <- table(safety_habits_data$helmet_mc,
                        outcome_data$crash_lifetime)
helmet_mctable2
prop.table(helmet_mctable2)
assocstats(helmet_mctable1)

#helmet_strap_use
table1 <- table(safety_habits_data$helmet_strap_use)
table1
prop.table(table1)
table2 <- table(safety_habits_data$helmet_strap_use,
                        outcome_data$crash_lifetime)
table2
prop.table(table2,2)
assocstats(table2)
fisher.test(table2)

#hairnets_available
table1 <- table(safety_habits_data$hairnets_available)
table1
prop.table(table1)
table2 <- table(safety_habits_data$hairnets_available,
                        outcome_data$crash_lifetime)
table2
prop.table(table2,2)
assocstats(table)
fisher.test(table2)

#headlights_always
table1 <- table(safety_habits_data$headlights_always)
table1
prop.table(table1)
table2 <- table(safety_habits_data$headlights_always,
                        outcome_data$crash_lifetime)
table2
prop.table(table2,2)
assocstats(table2)

#headlights_night
table1 <- table(safety_habits_data$headlights_night)
table1
prop.table(table1)
table2 <- table(safety_habits_data$headlights_night,
                        outcome_data$crash_lifetime)
table2
prop.table(table2,2)
assocstats(table2)

#helmet_damage
table1 <- table(safety_habits_data$helmet_damage)
table1
prop.table(table1)

table2 <- table(safety_habits_data$helmet_damage,
                        outcome_data$crash_lifetime)
table2
prop.table(table2,2)
assocstats(table2)

#cracks_dhelmet
table1 <- table(safety_habits_data$cracks_dhelmet)
table1
prop.table(table1)

table2 <- table(safety_habits_data$cracks_dhelmet,
                        outcome_data$crash_lifetime)
table2
prop.table(table2,2)
assocstats(table2)

#scratches_dhelmet
table1 <- table(safety_habits_data$scratches_dhelmet)
table1
prop.table(table1)

table2 <- table(safety_habits_data$scratches_dhelmet,
                        outcome_data$crash_lifetime)
table2
prop.table(table2,2)
assocstats(table2)

#glass_helmet
table1 <- table(safety_habits_data$glass_helmet)
table1
prop.table(table1)

table2 <- table(safety_habits_data$glass_helmet,
                        outcome_data$crash_lifetime)
table2
prop.table(table2,2)
assocstats(table2)

#fit_helmet
table1 <- table(safety_habits_data$fit_helmet)
table1
prop.table(table1)

table2 <- table(safety_habits_data$fit_helmet,
                        outcome_data$crash_lifetime)
table2
prop.table(table2,2)
assocstats(table2)

#helmet_colleagues
table1 <- table(safety_habits_data$helmet_colleagues)
table1
prop.table(table1)

table2 <- table(safety_habits_data$helmet_colleagues,
                        outcome_data$crash_lifetime)
table2
prop.table(table2,2)
assocstats(table2)

#colleagues_risks
table1 <- table(safety_habits_data$colleagues_risks)
table1
prop.table(table1)

table2 <- table(safety_habits_data$colleagues_risks,
                        outcome_data$crash_lifetime)
table2
prop.table(table2,2)
assocstats(table2)

#helmet_value
table1 <- table(safety_habits_data$helmet_value)
table1
prop.table(table1)

table2 <- table(safety_habits_data$helmet_value,
                        outcome_data$crash_lifetime)
table2
prop.table(table2)
assocstats(table2)

#helmet_strap_value
table1 <- table(safety_habits_data$helmet_strap_value)
table1
prop.table(table1)
table2 <- table(safety_habits_data$helmet_strap_value,
                        outcome_data$crash_lifetime)
table2
prop.table(table2,2)
assocstats(table2)

######################################################
#TABLE 2
######################################################
#crash_opinion___0
table1 <- table(safety_habits_data$crash_opinion___0)
table1
prop.table(table1)

table2 <- table(safety_habits_data$crash_opinion___0,
                        outcome_data$crash_lifetime)
table2
prop.table(table2,2)
assocstats(table2)

#crash_opinion___1
table1 <- table(safety_habits_data$crash_opinion___1)
table1
prop.table(table1)
table2 <- table(safety_habits_data$crash_opinion___1,
                        outcome_data$crash_lifetime)
table2
prop.table(table2,2)
assocstats(table2)

#crash_opinion___2
table1 <- table(safety_habits_data$crash_opinion___2)
table1
prop.table(table1)
table2 <- table(safety_habits_data$crash_opinion___2,
                        outcome_data$crash_lifetime)
table2
prop.table(table2,2)
assocstats(table2)

#crash_opinion___3
table1 <- table(safety_habits_data$crash_opinion___3)
table1
prop.table(table1)
table2 <- table(safety_habits_data$crash_opinion___3,
                        outcome_data$crash_lifetime)
table2
prop.table(table2,2)
assocstats(table2)

#crash_opinion___4
table1 <- table(safety_habits_data$crash_opinion___4)
table1
prop.table(table1)
table2 <- table(safety_habits_data$crash_opinion___4,
                        outcome_data$crash_lifetime)
table2
prop.table(table2,2)
assocstats(table2)

#crash_opinion___5
table1 <- table(safety_habits_data$crash_opinion___5)
table1
prop.table(table1)
table2 <- table(safety_habits_data$crash_opinion___5,
                        outcome_data$crash_lifetime)
table2
prop.table(table2,2)
assocstats(table2)

#crash_opinion___6
table1 <- table(safety_habits_data$crash_opinion___6)
table1
prop.table(table1)
table2 <- table(safety_habits_data$crash_opinion___6,
                        outcome_data$crash_lifetime)
table2
prop.table(table2,2)
assocstats(table2)


######################################################
#TABLE 3
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
summary(outcome_data$day_hosp_crash)

#disability_crash
disab <- table(outcome_data$disability_crash)
disab
prop.table(disab)

#disability_crash
disab <- table(outcome_data$type_disability_crash)
disab
prop.table(disab)

#rehab
summary(data$missed_work_crash)

#crash_year
crash_year <- table(outcome_data$crash_year)
crash_year
prop.table(crash_year)



######################################################
#TABLE 3.
######################################################
## OR table ##

reg_data<-data.frame(ses_data,
                     safety_habits_data,
                     outcome_data)

#crashlifetime
crashlifetime <-glm(as.factor(crash_lifetime) ~ 
                  age + 
					        years_wkvehicle +                  
					          # day_wkvehicle +
     #          		# time_start_wk + 
     #          		# time_stop_wk + 
              		# time_motodr + 
              		hairnets_available +
              		headlights_always + 
              		helmet_damage +
                  helmet_colleagues +
                  helmet_strap_value +
                  helmet_value
                  ,family=binomial, data=reg_data)
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

######################################################
#PRINCIPAL COMPONENTS
######################################################