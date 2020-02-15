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
	"lmerTest","dplyr"),library, character.only=T)

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
data <- read.csv("/Users/Joao/Downloads/kuth_tbi_rwanda_data.csv")

write.csv(data,"/Users/Joao/Downloads/kuth_tbi_rwanda_data_full.csv")

######################################################
#DATA MANAGEMENT
######################################################

data$age_cat<-car::recode(data$age,"10:20='20 or less';
                                     21:30='20 to 30';
                                     31:40='30 to 40';
                                     41:50='40 to 50';
                                     51:89='50ormore'")

data$outcome<-car::recode(data$gos,"5='death';
                                    else='alive'")

table(data$age_cat)
prop.table(table(data$age_cat))
table<-table(data$age_cat,data$outcome)
prop.table(table(data$age_cat,data$outcome),1)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

describeBy(data$age,data$outcome)
wilcox.test(data$age~data$outcome)


or<-c(3.80,6.54,41.60,7.36,11.29,5.20)
low_limit<-c(1.21,2.30,14.97,2.20,2.90,2.23)
upper_limit<-c(11.80,18.60,115.59,24.57,43.96,12.12)
variables<-c("Age >50","GCS 9-13","GCS 3-8", "Hypoxic (<90%)",
  "Bradycardic (<60%)","Tachycardic (>100)")
pvalues<-c(0.05,0.01,0.01,0.01,0.01,0.01)

plot<-data.frame(or,low_limit,upper_limit,variables,pvalues)

ggplot(plot, aes(y= reorder(variables,or), 
  x=or)) + #use to round numbers - x = round(median,2)
# facet_grid( measure ~ .,space="free") + #use in case you want to add facets (e.g. labels in columns or lines)
geom_point() +
geom_errorbarh(aes(xmin=low_limit, xmax=upper_limit), height=.2) +
geom_vline(xintercept = 1, linetype=2) +
geom_text(aes(label=format(round(or,2),nsmall=2)), 
  vjust=-0.5, hjust=0, size=3) +
#coord_flip() +
#facet_grid(measure ~ ., scales="free_x", space="free") +
labs(y = 'Predictos of TBI mortality', 
  x = 'OR (CI 95%)') +
# scale_x_continuous(breaks=seq(0, 2000, 200)) + #use in case you want to re-arrange the X axis limits
#portion below is to add an arrow when you want to cut the plot area
# annotate("segment", x = 2000, xend=2020, y = 13, yend=13,
#   colour = "black",
#   arrow=arrow(length=unit(0.2,"cm"),type = "closed")) +
# annotate("segment", x = 2000, xend=2020, y = 10, yend=10,
#   colour = "black",
#   arrow=arrow(length=unit(0.2,"cm"),type = "closed")) +
# annotate("text", x = 1990, y = 12.7,
#   colour = "black",label="2940.52",size=3) +
# annotate("text", x = 1990, y = 9.7,
#   colour = "black",label="2014.76",size=3) +
theme_bw()


670*100/867



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

## safety_habits
#total
table(numeric$helmet_mc)
table(numeric$helmet_bike)
table(numeric$belt_driver)
table(numeric$belt_back)
table(numeric$belt_passenger)
table(numeric$stop_fast)
table(numeric$road_wrongside)
#RTC - Y (NAs - RTC - valores achadados na tabela - lazy ways)
table(numericRTC$helmet_mc)
table(numericRTC$helmet_bike)
table(numericRTC$belt_driver)
table(numericRTC$belt_back)
table(numericRTC$belt_passenger)
table(numericRTC$stop_fast)
table(numericRTC$road_wrongside)
#RTC - N
table(numericNRTC$helmet_mc)
table(numericNRTC$helmet_bike)
table(numericNRTC$belt_driver)
table(numericNRTC$belt_back)
table(numericNRTC$belt_passenger)
table(numericNRTC$stop_fast)
table(numericNRTC$road_wrongside)
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

#safe habs
helmet_mctable <- table(numeric$helmet_mc ,numeric$crash_lifetime)
assocstats(helmet_mctable)
helmet_biketable <- table(numeric$helmet_bike ,numeric$crash_lifetime)
assocstats(helmet_biketable)
belt_drivertable <- table(numeric$belt_driver ,numeric$crash_lifetime)
fisher.test(belt_drivertable)
belt_backtable <- table(numeric$belt_back ,numeric$crash_lifetime)
fisher.test(belt_backtable)
belt_passengertable <- table(numeric$belt_passenger,numeric$crash_lifetime)
assocstats(belt_passengertable)
stop_fasttable <- table(numeric$stop_fast ,numeric$crash_lifetime)
fisher.test(stop_fasttable)
road_wrongsidetable <- table(numeric$road_wrongside ,numeric$crash_lifetime)
fisher.test(road_wrongsidetable)

######################################################
# EXPLORATORY DATA ANALYSIS
######################################################
plot(data)

hist(data)

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