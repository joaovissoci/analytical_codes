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
data <- read.csv("/Users/Joao/Box Sync/Home Folder jnv4/Data/Global EM/Asia/Sri lanka/safe_habits_srilanka_data.csv")

######################################################
#DATA MANAGEMENT
######################################################
numeric <- with(data, data.frame(age, hours_wkvehicle, day_wkvehicle, 
                                 time_start_wk, time_stop_wk, time_motodr, vehicle_home, 
                                 hours_hmvehicle, crash_lifetime, injured_crash , 
                                 near_miss_month, 
                                 helmet_mc, helmet_bike, belt_driver, belt_passenger, 
                                 belt_back, road_wrongside, stop_fast, lat_1, long_1, 
                                 danger1, lat_2, long_2, danger2, lat_3, long_3, danger3,
                                 lat_4, long_4, danger4, lat_5, long_5, danger5, 
                                 lat_6, long_6, danger6, lat_7, long_7, danger7, 
                                 lat_8, long_8, danger8, lat_9, long_9, danger9, 
                                 lat_10, long_10, danger10, rem_date_crash, date_crash,
                                 day_crash, month_crash, year_crash, crash_in_last_year,
                                 time_crash, type_crash, work_crash, injuries_crash___0,
                                 injuries_crash___1, injuries_crash___2, injuries_crash___3,
                                 injuries_crash___4, injuries_crash___5, injuries_crash___6,
                                 injuries_crash___7, injuries_crash___8, injuries_crash___9,
                                 injuries_crash___10, injuries_crash___90, injuries_crash___99,
                                 injuries_crash_other, hosp_crash, day_hosp_crash, disability_crash,
                                 type_disability_crash, missed_work_crash, rehabrec_crash, rehab_crash, 
                                 crash_year))

#sub-sets for numeric
numeric
numericRTC <- subset(numeric,numeric$crash_lifetime=="1")
summary(numericRTC)
numericNRTC <- subset(numeric, numeric$crash_lifetime=="0")
summary(numericNRTC)

#recoding safe hab to N/A NR and A
numeric$helmet_mc<-car::recode(numeric$helmet_mc, "c(0,1,2,3)='0'; 4='1'; NA='2'")
numeric$helmet_bike<-car::recode(numeric$helmet_bike, "c(0,1,2,3)='0'; 4='1'; NA='2'")
numeric$belt_driver<-car::recode(numeric$belt_driver, "c(0,1,2,3)='0'; 4='1'; NA='2'")
numeric$belt_back<-car::recode(numeric$belt_back, "c(0,1,2,3)='0'; 4='1'; NA='2'")
numeric$belt_passenger<-car::recode(numeric$belt_passenger, "c(0,1,2,3)='0'; 4='1'; NA='2'")
numeric$stop_fast<-car::recode(numeric$stop_fast, "c(0,1,2,3)='0'; 4='1'; NA='2'")
numeric$road_wrongside<-car::recode(numeric$road_wrongside, "c(0,1,2,3)='0'; 4='1'; NA='2'")

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
t.test(numeric$hour_week~numeric$crash_lifetime,paired=FALSE)

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

###################################################
#Latent class analysis
###################################################

## Questions
# http://rfunction.com/archives/1499
# https://drive.google.com/open?id=0B4TReYGK49h_X09ZYno1OG5aUVk

# Correlation

# Building the formula

safety_perceptions<-remove.vars(safety_habits_data,
  c("helmet_mc",
    "crash_opinion___90",
    "helmet_strap_use",
    "hairnets_available",
    "headlights_night",
    "helmet_colleagues"))

safety1<-with(safety_perceptions,data.frame(
      crash_opinion___0,
      crash_opinion___1,
      crash_opinion___2,
      crash_opinion___3,
      crash_opinion___4,
      crash_opinion___5,
      crash_opinion___6))
      # crash_moto,
      # crash_bus,
      # crash_peds))

# To specify a latent class model, poLCA uses the standard, symbolic R model formula expres- sion. The response variables are the manifest variables of the model. Because latent class models have multiple manifest variables, these variables must be “bound” as cbind(Y1, Y2, Y3, ...) in the model formula. For the basic latent class model with no covariates, the formula definition takes the form

ses_data_cat<-sapply(safety_perceptions,function(x) as.factor(x))
ses_data_cat<-as.data.frame(ses_data_cat)
ses_data_cat2<-sapply(ses_data_cat,function(x) as.numeric(x))
ses_data_cat2<-as.data.frame(ses_data_cat2)

f <- cbind(colleagues_risks,
           headlights_always,
           helmet_damage,
           cracks_dhelmet,
           scratches_dhelmet,
           strap_dhelmet,
           glass_helmet,
           fit_helmet,
           helmet_value,
           helmet_strap_value) ~ 1

# The ~ 1 instructs poLCA to estimate the basic latent class model. For the latent class regres- sion model, replace the ~ 1 with the desired function of covariates, as, for example:
# f <- cbind(Y1, Y2, Y3) ~ X1 + X2 * X3

# To estimate the specified latent class model, the default poLCA command is:
# poLCA(formula, data, ncl pass = 2, maxiter = 1000, graphs = FALSE,
#     tol = 1e-10, na.rm = TRUE, probs.start = NULL, nrep = 1,
#     verbose = TRUE, calc.se = TRUE)

#========================================================= 
# Fit for 3 latent classes: 
#========================================================= 
set.seed(1988)

# ses_data<-na.omit(ses_data)
lcamodel <- poLCA(f, ses_data_cat2, nclass = 3)

# Entropy
entropy<-function (p) sum(-p*log(p))
error_prior <- entropy(lcamodel$P) # Class proportions
error_post <- mean(apply(lcamodel$posterior, 1, entropy))
R2_entropy <- (error_prior - error_post) / error_prior
R2_entropy

### results
# number of observations: 245 
# number of estimated parameters: 27 
# residual degrees of freedom: 218 
# maximum log-likelihood: -1243.071 
 
# AIC(2): 2540.142
# BIC(2): 2634.676
# G^2(2): 284.7812 (Likelihood ratio/deviance statistic) 
# X^2(2): 908.1992 (Chi-square goodness of fit)  

# Entropy = [1] 0.7747763

# Example from https://github.com/ricardo-bion/ggradar
library(ggplot2)
library(ggradar)
library(dplyr)
library(scales)
library(tibble)

# configured to work on a Mac, change directory to Unix or Windows

radialplot_data<-as.data.frame(lcamodel$probs)[,c(1,3,5,7,9,11,13,15,17,19)]

rownames(radialplot_data)<-c("Low","High","Mid")

radialplot_data %>%
     rownames_to_column( var = "group" ) -> radialplot_data2

axis_lables<-c("Peers at risk",
               "Use headlights",
               "Demaged\n helmet",
               "Cracked\n helmet",
               "Scratched\n helmet",
               "Strapped helmet",
               "Clear helmet\n glass",
               "Good helmet fit",
               "Values helmet\n use",
               "Values helmet\n strap use")

ggradar(radialplot_data2,
        font.radar="sans",
         grid.label.size=7,
         axis.label.size=5,
         axis.labels=axis_lables,
         legend.text.size=10) 

## OR table ##
reg_data2<-data.frame(ses_data,
                     safety_habits_data,
                     outcome_data,
                     class=lcamodel$predclass)
reg_data2$class_recoded<-car::recode(
  reg_data2$class,"1='Low';
                 2='High';
                 3='Mid'")
#crashlifetime
crashlifetime <-glm(as.factor(crash_year) ~ 
                  age + 
                  years_wkvehicle +                  
                    # day_wkvehicle +
     #              # time_start_wk + 
     #              # time_stop_wk + 
                  # time_motodr + 
                  # hairnets_available +
                  class_recoded,
                  family=binomial, data=reg_data2)
summary(crashlifetime)
odds_model_1<-exp(cbind(Odds=coef(crashlifetime),
                confint(crashlifetime,level=0.95))) 
colnames(odds_model_1)<-c("OR","LowCI","HighCI")

#nearmiss

reg_data2$near_miss_month_bin<-car::recode(
    reg_data2$near_miss_month,"
    0='no';
    else='yes'")
reg_data2$near_miss_month_bin<-as.factor(reg_data2$near_miss_month_bin)

nearmiss <-glm(near_miss_month_bin ~ 
                  age + 
                  years_wkvehicle +                  
                    # day_wkvehicle +
     #              # time_start_wk + 
     #              # time_stop_wk + 
                  # time_motodr + 
                  # hairnets_available +
                  class_recoded,
                  family=binomial, data=reg_data2)
summary(nearmiss)
odds_model_2<-exp(cbind(Odds=coef(nearmiss),
                confint(nearmiss,level=0.95))) 
colnames(odds_model_2)<-c("OR","LowCI","HighCI")

odds_all<-rbind(odds_model_1,
           odds_model_2)

# plot_odds<-function(x, title = NULL){
odds<-odds_model_1[-c(1,2,3),]
odds<-as.data.frame(odds)
colnames(odds)<-c('OR', 'lower', 'upper')
odds$vars<-c("Low safety","Mid safety")

#ticks<-c(seq(.1, 1, by =.1), seq(0, 10, by


ggplot(odds, aes(y= OR, x = reorder(vars, OR))) +
geom_point() +
geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
#scale_y_log10(breaks=ticks, labels = ticks) +
geom_hline(yintercept = 1, linetype=2) +
# scale_x_discrete(limits=c(
#        "4 or more hrs vs. 0-1 hrs",
#        "3-4 hrs vs. 0-1 hrs",
#        "2-3 hrs vs. 0-1 thrs",
#        "1-2 hrs vs. 0-1 hrs",
#        "# course points",
#        "Age",
#        "Male vs. Female",
#        "RTI vs. non-RTI",
#        "Alcohol use vc. Abstainer",
#        "Moshi Rural vs. Moshi Urban",
#        "Hai vs. Moshi Urban",
#        "Rombo vs. Moshi Urban",
#        "Mwanga vs. Moshi Urban",
#        "Same vs. Moshi Urban")) +
# facet_grid(.~models, scales="free_y") +
# coord_flip() +
labs(x = 'Boda boda drivers safety behavior classes', 
     y = 'OR (CI 95%)') +
theme_bw()
