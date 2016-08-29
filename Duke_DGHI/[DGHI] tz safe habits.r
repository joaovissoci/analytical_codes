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
lapply(c("sem","ggplot2", "psych", "irr", "nortest", "moments",
	"GPArotation","nFactors","boot","psy", "car","vcd", "gridExtra",
	"mi","VIM","epicalc","gdata","sqldf","reshape2","mclust","foreign",
	"survival","memisc","lme4","lmerTest","dplyr","qgraph"),library, 
character.only=T)

#Package and codes to pull data from goodle sheets
#devtools::install_github("jennybc/googlesheets")
#library(googlesheets)
#suppressMessages(library(dplyr))

######################################################
#IMPORTING DATA
######################################################
#Pulling data from dropbox
#data_hamilton <- repmis::source_DropboxData("lena_hamilton.csv","r31zt5zeiygsc23",sep = ",",header = TRUE)

data<-read.csv("/Users/joaovissoci/OneDrive - Duke University/datasets/Global EM/Africa/safe_habits_tz/tz_safet_habits_data.csv",sep=',')

######################################################
#DATA MANAGEMENT
######################################################
safe_habits<-with(data,data.frame(safety_helmet_use,safety_helmet_use_colleague,safety_risk_driving,safety_headlight_use_day,safety_headlight_use_night,safety_purchase_helmet_after_use,safety_buckle_helmet,safety_belief_helmet_reduce_risk,safety_belief_helmetstraps_reduce_risk,helmet_cracks,helmet_scratch,helmet_broken_chin,helmet_face_shield,helmet_obscure_face_shield,helmet_fit))

# P1. How often do you wear a helmet when you are on a boda-boda?
safe_habits$safety_helmet_use<-car::recode(
	safe_habits$safety_helmet_use,"'Always'=1;else=0")

#P2. How often do you think your boda-boda driver colleagues wear helmets?
safe_habits$safety_helmet_use_colleague<-car::recode(safe_habits$safety_helmet_use_colleague,"'Always'=1;'Sometimes'=1;'Often'=1;else=0")

#P3. How often do you think your boda-boda driver colleagues take risks while driving?
safe_habits$safety_risk_driving<-car::recode(data$safety_risk_driving,"'Always'=0;'Sometimes'=0;'Often'=0;else=1")

#P6. Do you run your headlight at all times when using your boda-boda?
safe_habits$safety_headlight_use_day<-car::recode(safe_habits$safety_headlight_use_day,"'Always'=1;else=0")

#P7. Do you run your headlight on your boda-boda after dark?
safe_habits$safety_headlight_use_night<-car::recode(safe_habits$safety_headlight_use_night,"'Always'=1;else=0")

#P8. Do boda-boda drivers purchase a new helmet after a crash or if it has been damaged?
safe_habits$safety_purchase_helmet_after_use<-car::recode(safe_habits$safety_purchase_helmet_after_use,"'Always'=1;else=0")

#P9. Do you buckle the helmet strap when you use your helmet?
safe_habits$safety_buckle_helmet<-car::recode(safe_habits$safety_buckle_helmet,"'Always'=1;else=0")

#P10. Do you believe helmets reduce injuries from crashes?	
safe_habits$safety_belief_helmet_reduce_risk<-car::recode(safe_habits$safety_belief_helmet_reduce_risk,"'Agree'=1;'Strongly Agree'=1;else=0")

#P11. Do you believe helmet straps are important to reduce injuries from crashes?
safe_habits$safety_belief_helmetstraps_reduce_risk<-car::recode(safe_habits$safety_belief_helmetstraps_reduce_risk,"'Agree'=1;'Strongly Agree'=1;else=0")

#P17. Does the drivers helmet have cracks or dents in the shell of the helmet? 
safe_habits$helmet_cracks<-car::recode(safe_habits$helmet_cracks,"'Yes'=1;'No'=0;else=NA")

#P18. Does the drivers helmet have scratches in the paint?
safe_habits$helmet_scratch<-car::recode(safe_habits$helmet_scratch,"'Yes'=0;'No'=1;else=NA")

#P19. Does the drivers helmet have a broken chin strap?
safe_habits$helmet_broken_chin<-car::recode(safe_habits$helmet_broken_chin,"'Yes'=0;'No'=1;else=NA")

#p20. Is there a face shield? 
safe_habits$helmet_face_shield<-car::recode(safe_habits$helmet_face_shield,"'Yes'=1;'No'=0;else=NA")

#P20a. Is the face shield of the drivers helmet obscured by scratches, paint, or a graphic?
safe_habits$helmet_obscure_face_shield<-car::recode(safe_habits$helmet_obscure_face_shield,"'Yes'=0;'No'=1;else=NA")

#P21. Does the drivers helmet fit the driver well?
safe_habits$helmet_fit<-car::recode(safe_habits$helmet_fit,"'Yes'=1;'No'=0;else=NA")

#turning data back to numeric
safe_habits_numeric <-lapply(safe_habits,function(x) as.numeric(as.character(x)))

#creating datasets
reasons_danger<-with(data,data.frame(reason_trafficlane,reason_roadcond,reason_less_density,reason_regulation,reason_others_awareness,reason_walkways,reason_trainning,reason_lighting,reason_reflectorvests,reason_helmet,reason_education,reason_roadrules,reason_carefulness,reason_alcoholuse,reason_respectforBB,reason_reducespeed,	reason_roadsigns,	reason_widerroads,	reason_confidentdriving,	reason_inspectlicenses,	reason_properuseindicators,	reason_agelimit,	reason_policeaccountability,	reason_distraction,	reason_headlights,	reason_punishment,	reason_riskbehavior,	reason_vehiclecondition))

demographics<-with(data,data.frame(age,gender,alternative_transportation,hours_alternative_transportation))

work_experience<-with(data,data.frame(hours_work_onbodaboda,days_work_bodaboda,years_work_onbodaboda))

work_experience$hours_work_week<-work_experience$days_work_bodaboda*work_experience$hours_work_onbodabod

#recoding outcome variable
outcomes<-with(data,data.frame(rtc_involvement,injury,hospitalization,los,nearmissmonth,out_of_work,nearmissmonth))
outcomes$nearmissmonth<-car::recode(outcomes$nearmissmonth,"0=0;1:4=1;else=1")
outcomes$rtc_involvement<-car::recode(outcomes$rtc_involvement,"'No'=0;'Yes'=1;else=0")
outcomes$rtc_involvement<-as.numeric(as.character(outcomes$rtc_involvement))
outcomes$hospitalization<-car::recode(data$hospitalization,"'No'='No';'Yes'='Yes';else=NA")

data_all<-data.frame(safe_habits,outcomes[,c(1,2,7)],work_experience,
	age=data$age)

data_all<-na.omit(data_all)

data_all$injury<-car::recode(
	data_all$injury,"'No'=0;'Yes'=1;else=0")
######################################################
#TABLE 1.
######################################################

# Age
summary(demographics$age)
ad.test(demographics$age)
#hist(demographics$age)
#ci_func(demographics$age,.95)
by(demographics$age,outcomes$rtc_involvement,describe)
wilcox.test(demographics$age~outcomes$rtc_involvement)

# Experience (Years)
summary(work_experience$years_work_onbodaboda)
ad.test(work_experience$years_work_onbodaboda)
#hist(work_experience$years_work_onbodaboda)
#ci_func(work_experience$years_work_onbodaboda,.95)
by(work_experience$years_work_onbodaboda,outcomes$rtc_involvement,describe)
wilcox.test(work_experience$years_work_onbodaboda~outcomes$rtc_involvement)

# Hours of Work
summary(work_experience$hours_work_onbodaboda)
ad.test(work_experience$hours_work_onbodaboda)
#hist(work_experience$hours_work_onbodaboda)
#ci_func(work_experience$hours_work_onbodaboda,.95)
by(work_experience$hours_work_onbodaboda,outcomes$rtc_involvement,describe)
wilcox.test(work_experience$hours_work_onbodaboda~outcomes$rtc_involvement)

# Days of Work
summary(work_experience$days_work_bodaboda)
ad.test(work_experience$days_work_bodaboda)
#hist(work_experience$days_work_bodaboda)
#ci_func(work_experience$days_work_bodaboda,.95)
by(work_experience$days_work_bodaboda,outcomes$rtc_involvement,describe)
wilcox.test(work_experience$days_work_bodaboda~outcomes$rtc_involvement)

# Hours of Work per Week 
summary(work_experience$hours_work_week)
sd(work_experience$hours_work_week)
ad.test(work_experience$hours_work_week)
#hist(work_experience$hours_work_week)
#ci_func(work_experience$hours_work_week,.95)
by(work_experience$hours_work_week,outcomes$rtc_involvement,describe)
wilcox.test(work_experience$hours_work_week~outcomes$rtc_involvement)

# Injury
table<-with(outcomes,table(injury))
table
prop.table(table)
#table<-with(data_bea,table(hospitalization,risk_classification))
#table
#prop.table(table,2)
#chisq.test(table)
#fisher.test(table)
#assocstats(table) #vcd package

# Hospitalization
table<-with(outcomes,table(hospitalization))
table
prop.table(table)
#table<-with(data_bea,table(hospitalization,risk_classification))
#table
#prop.table(table,2)
#chisq.test(table)
#fisher.test(table)
#assocstats(table) #vcd package

# Near Miss
table<-with(outcomes,table(nearmissmonth))
table
prop.table(table)
#table<-with(data_bea,table(hospitalization,risk_classification))
#table
#prop.table(table,2)
#chisq.test(table)
#fisher.test(table)
#assocstats(table) #vcd package

# Days out of work
table<-with(outcomes,table(out_of_work))
table
prop.table(table)
#table<-with(data_bea,table(hospitalization,risk_classification))
#table
#prop.table(table,2)
#chisq.test(table)
#fisher.test(table)
#assocstats(table) #vcd package

# Days out of work
summary(outcomes$out_of_work)
ad.test(outcomes$out_of_work)
#table<-with(data_bea,table(hospitalization,risk_classification))
#table
#prop.table(table,2)
#chisq.test(table)
#fisher.test(table)
#assocstats(table) #vcd package

# LOS
summary(outcomes$los)
ad.test(outcomes$los)
#hist(work_experience$hospitalization)
#ci_func(work_experience$hospitalization,.95)
#by(work_experience$hospitalization,outcomes$rtc_involvement,describe)
#wilcox.test(work_experience$hospitalization~outcomes$rtc_involvement)

######################################################
#TABLE 2.
######################################################

# # Age
# by(demographics$age,outcomes$rtc_involvement,describe)
# wilcox.test(demographics$age~outcomes$rtc_involvement)

# # Experience (Years)
# by(work_experience$years_work_onbodaboda,outcomes$rtc_involvement,describe)
# wilcox.test(work_experience$years_work_onbodaboda~outcomes$rtc_involvement)

# # Hours of Work
# by(work_experience$hours_work_onbodaboda,outcomes$rtc_involvement,describe)
# wilcox.test(work_experience$hours_work_onbodaboda~outcomes$rtc_involvement)

# # Days of Work
# by(work_experience$days_work_bodaboda,outcomes$rtc_involvement,describe)
# wilcox.test(work_experience$days_work_bodaboda~outcomes$rtc_involvement)

# # Hours of Work per Week
# by(work_experience$hours_work_week,outcomes$rtc_involvement,describe)
# wilcox.test(work_experience$hours_work_week~outcomes$rtc_involvement)

#Helmet use
table<-with(data_all,table(safety_helmet_use,
	rtc_involvement))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package
logmodel<-glm(rtc_involvement ~ 
								safety_helmet_use,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)

#Cracks
table<-with(data_all,table(helmet_cracks,
	rtc_involvement))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# #helmet_scratch
# table<-with(data_all,table(helmet_scratch,
# 	rtc_involvement))
# table
# prop.table(table,2)
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package

#helmet_broken_chin
table<-with(data_all,table(helmet_broken_chin,
	rtc_involvement))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#helmet_broken_chin
table<-with(data_all,table(helmet_broken_chin,
	rtc_involvement))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#helmet_obscure_face_shield
table<-with(data_all,table(helmet_obscure_face_shield,
	rtc_involvement))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#helmet_face_shield
table<-with(data_all,table(helmet_face_shield,
	rtc_involvement))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#helmet_fit
table<-with(data_all,table(helmet_fit,
	rtc_involvement))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#helmet_broken_chin
table<-with(data_all,table(safety_risk_driving,
	rtc_involvement))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#safety_headlight_use_day
table<-with(data_all,table(safety_headlight_use_day,
	rtc_involvement))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#safety_headlight_use_night
table<-with(data_all,table(safety_headlight_use_night,
	rtc_involvement))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#safety_purchase_helmet_after_use
table<-with(data_all,table(safety_purchase_helmet_after_use,
	rtc_involvement))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# #safety_purchase_helmet_after_use
# table<-with(data_all,table(safety_purchase_helmet_after_use,
# 	rtc_involvement))
# table
# prop.table(table,2)
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package

#safety_belief_helmet_reduce_risk
table<-with(data_all,table(safety_belief_helmet_reduce_risk,
	rtc_involvement))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#safety_belief_helmetstraps_reduce_risk
table<-with(data_all,table(safety_belief_helmetstraps_reduce_risk,
	rtc_involvement))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#### LOGISTIC REGRESSION MODELS ######################################

logmodel<-glm(rtc_involvement ~ 
								safety_helmet_use,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients

logmodel<-glm(rtc_involvement ~ 
								helmet_cracks,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients

logmodel<-glm(rtc_involvement ~ 
								safety_buckle_helmet,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients

logmodel<-glm(rtc_involvement ~ 
								helmet_obscure_face_shield,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients

logmodel<-glm(rtc_involvement ~ 
								helmet_obscure_face_shield,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients

logmodel<-glm(rtc_involvement ~ 
								helmet_face_shield,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients

logmodel<-glm(rtc_involvement ~ 
								helmet_fit,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients

logmodel<-glm(rtc_involvement ~ 
								safety_headlight_use_day,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients

logmodel<-glm(rtc_involvement ~ 
								safety_headlight_use_night,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients

logmodel<-glm(rtc_involvement ~ 
								safety_purchase_helmet_after_use,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients

logmodel<-glm(rtc_involvement ~ 
								safety_belief_helmet_reduce_risk,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients

logmodel<-glm(rtc_involvement ~ 
								safety_belief_helmet_reduce_risk,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients

logmodel<-glm(rtc_involvement ~ 
								safety_belief_helmetstraps_reduce_risk,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients

logmodel<-glm(rtc_involvement ~ 
								age +
								years_work_onbodaboda +
								hours_work_week + 
								helmet_cracks + 
								safety_helmet_use +
								safety_buckle_helmet + 
								helmet_obscure_face_shield + 
								helmet_face_shield + 
								helmet_fit +
								safety_headlight_use_day + 
								safety_headlight_use_night + 
								safety_purchase_helmet_after_use +
								safety_belief_helmet_reduce_risk +
								safety_buckle_helmet +
								safety_belief_helmetstraps_reduce_risk,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)

######################################################
#TABLE 3.
######################################################
# Age
# by(demographics$age,outcomes$injury,describe)
# wilcox.test(demographics$age~outcomes$injury)

# # Experience (Years)
# by(work_experience$years_work_onbodaboda,outcomes$injury,describe)
# wilcox.test(work_experience$years_work_onbodaboda~outcomes$injury)

# # Hours of Work
# by(work_experience$hours_work_onbodaboda,outcomes$injury,describe)
# wilcox.test(work_experience$hours_work_onbodaboda~outcomes$injury)

# # Days of Work
# by(work_experience$days_work_bodaboda,outcomes$injury,describe)
# wilcox.test(work_experience$days_work_bodaboda~outcomes$injury)

# # Hours of Work per Week
# by(work_experience$hours_work_week,outcomes$injury,describe)
# wilcox.test(work_experience$hours_work_week~outcomes$injury)

#Helmet use
table<-with(data_all,table(safety_helmet_use,
	injury))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#Cracks
table<-with(data_all,table(helmet_cracks,
	injury))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# #helmet_scratch
# table<-with(data_all,table(helmet_scratch,
# 	injury))
# table
# prop.table(table,2)
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package

#helmet_broken_chin
table<-with(data_all,table(helmet_broken_chin,
	injury))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#helmet_obscure_face_shield
table<-with(data_all,table(helmet_obscure_face_shield,
	injury))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#helmet_face_shield
table<-with(data_all,table(helmet_face_shield,
	injury))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#helmet_fit
table<-with(data_all,table(helmet_fit,
	injury))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# #helmet_broken_chin
# table<-with(data_all,table(helmet_broken_chin,
# 	injury))
# table
# prop.table(table,2)
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package

# #safety_risk_driving
# table<-with(data_all,table(safety_risk_driving,
# 	injury))
# table
# prop.table(table,2)
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package

#safety_headlight_use_day
table<-with(data_all,table(safety_headlight_use_day,
	injury))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#safety_headlight_use_night
table<-with(data_all,table(safety_headlight_use_night,
	injury))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#safety_purchase_helmet_after_use
table<-with(data_all,table(safety_purchase_helmet_after_use,
	injury))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#safety_purchase_helmet_after_use
table<-with(data_all,table(safety_purchase_helmet_after_use,
	injury))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#safety_belief_helmet_reduce_risk
# table<-with(data_all,table(safety_belief_helmet_reduce_risk,
# 	injury))
# table
# prop.table(table,2)
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package

#safety_belief_helmetstraps_reduce_risk
table<-with(data_all,table(safety_belief_helmetstraps_reduce_risk,
	injury))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#### LOGISTIC REGRESSION MODELS ######################################

logmodel<-glm(injury ~ 
								safety_helmet_use,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients

logmodel<-glm(injury ~ 
								helmet_cracks,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients

logmodel<-glm(injury ~ 
								safety_buckle_helmet,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients

logmodel<-glm(injury ~ 
								helmet_obscure_face_shield,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients

logmodel<-glm(injury ~ 
								helmet_face_shield,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients

logmodel<-glm(injury ~ 
								helmet_fit,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients

logmodel<-glm(injury ~ 
								safety_headlight_use_day,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients

logmodel<-glm(injury ~ 
								safety_headlight_use_night,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients

logmodel<-glm(injury ~ 
								safety_purchase_helmet_after_use,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients

logmodel<-glm(injury ~ 
								safety_belief_helmet_reduce_risk,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients

logmodel<-glm(injury ~ 
								safety_buckle_helmet,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients

logmodel<-glm(injury ~ 
								age +
								years_work_onbodaboda +
								hours_work_week + 
								helmet_cracks + 
								safety_helmet_use +
								safety_buckle_helmet + 
								helmet_obscure_face_shield + 
								helmet_face_shield + 
								helmet_fit +
								safety_headlight_use_day + 
								safety_headlight_use_night + 
								safety_purchase_helmet_after_use +
								safety_belief_helmet_reduce_risk +
								safety_buckle_helmet +
								safety_belief_helmetstraps_reduce_risk,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)

######################################################
#TABLE 4.
######################################################

# Age
# by(demographics$age,outcomes$injury,describe)
# wilcox.test(demographics$age~outcomes$injury)

# # Experience (Years)
# by(work_experience$years_work_onbodaboda,outcomes$injury,describe)
# wilcox.test(work_experience$years_work_onbodaboda~outcomes$injury)

# # Hours of Work
# by(work_experience$hours_work_onbodaboda,outcomes$injury,describe)
# wilcox.test(work_experience$hours_work_onbodaboda~outcomes$injury)

# # Days of Work
# by(work_experience$days_work_bodaboda,outcomes$injury,describe)
# wilcox.test(work_experience$days_work_bodaboda~outcomes$injury)

# # Hours of Work per Week
# by(work_experience$hours_work_week,outcomes$injury,describe)
# wilcox.test(work_experience$hours_work_week~outcomes$injury)

#Helmet use
table<-with(data_all,table(safety_helmet_use,
	nearmissmonth))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#Cracks
table<-with(data_all,table(helmet_cracks,
	nearmissmonth))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# #helmet_scratch
# table<-with(data_all,table(helmet_scratch,
# 	nearmissmonth))
# table
# prop.table(table,2)
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package

#helmet_broken_chin
table<-with(data_all,table(helmet_broken_chin,
	nearmissmonth))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#helmet_obscure_face_shield
table<-with(data_all,table(helmet_obscure_face_shield,
	nearmissmonth))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#helmet_face_shield
table<-with(data_all,table(helmet_face_shield,
	nearmissmonth))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#helmet_fit
table<-with(data_all,table(helmet_fit,
	nearmissmonth))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# #helmet_broken_chin
# table<-with(data_all,table(helmet_broken_chin,
# 	nearmissmonth))
# table
# prop.table(table,2)
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package

# #safety_risk_driving
# table<-with(data_all,table(safety_risk_driving,
# 	nearmissmonth))
# table
# prop.table(table,2)
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package

#safety_headlight_use_day
table<-with(data_all,table(safety_headlight_use_day,
	nearmissmonth))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#safety_headlight_use_night
table<-with(data_all,table(safety_headlight_use_night,
	nearmissmonth))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#purchase_helmet_after_use
table<-with(data_all,table(safety_purchase_helmet_after_use,
	nearmissmonth))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# #safety_purchase_helmet_after_use
# table<-with(data_all,table(safety_purchase_helmet_after_use,
# 	nearmissmonth))
# table
# prop.table(table,2)
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package

#safety_belief_helmet_reduce_risk
table<-with(data_all,table(safety_belief_helmet_reduce_risk,
	nearmissmonth))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#safety_belief_helmetstraps_reduce_risk
table<-with(data_all,table(safety_belief_helmetstraps_reduce_risk,
	nearmissmonth))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#### LOGISTIC REGRESSION MODELS ######################################

logmodel<-glm(nearmissmonth ~ 
								safety_helmet_use,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients

logmodel<-glm(nearmissmonth ~ 
								helmet_cracks,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients

# logmodel<-glm(nearmissmonth ~ 
# 								safety_buckle_helmet,
# 	family=binomial, data=data_all)
# summary(logmodel)
# #anova(reglogGEU)
# exp(coef(logmodel)) # exponentiated coefficients
# exp(confint(logmodel)) # 95% CI for exponentiated coefficients

logmodel<-glm(nearmissmonth ~ 
								helmet_obscure_face_shield,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients

logmodel<-glm(nearmissmonth ~ 
								helmet_face_shield,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients

# logmodel<-glm(nearmissmonth ~ 
# 								helmet_fit,
# 	family=binomial, data=data_all)
# summary(logmodel)
# #anova(reglogGEU)
# exp(coef(logmodel)) # exponentiated coefficients
# exp(confint(logmodel)) # 95% CI for exponentiated coefficients

logmodel<-glm(nearmissmonth ~ 
								safety_headlight_use_day,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients

logmodel<-glm(nearmissmonth ~ 
								safety_headlight_use_night,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients

logmodel<-glm(nearmissmonth ~ 
								safety_purchase_helmet_after_use,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients

# logmodel<-glm(nearmissmonth ~ 
# 								safety_belief_helmet_reduce_risk,
# 	family=binomial, data=data_all)
# summary(logmodel)
# #anova(reglogGEU)
# exp(coef(logmodel)) # exponentiated coefficients
# exp(confint(logmodel)) # 95% CI for exponentiated coefficients

logmodel<-glm(nearmissmonth ~ 
								safety_buckle_helmet,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients

logmodel<-glm(nearmissmonth ~ 
								age +
								years_work_onbodaboda +
								hours_work_week + 
								# safety_helmet_use +
								helmet_cracks + 
								# safety_buckle_helmet + 
								helmet_obscure_face_shield + 
								helmet_face_shield + 
								# helmet_fit +
								# safety_headlight_use_day + 
								safety_headlight_use_night,
								# safety_purchase_helmet_after_use +
								# safety_belief_helmet_reduce_risk,
								# safety_belief_helmetstraps_reduce_risk,
	family=binomial, data=data_all)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)

######################################################
#PRINCIPAL COMPONENT ANALYSIS - From psych package - http://twt.lk/bdAQ or http://twt.lk/bdAR or http://twt.lk/bdAS
######################################################
# Define the amout of factor to retain
#Group of functinos to determine the number os items to be extracted
par(mfrow=c(2,2)) #Command to configure the plot area for the scree plot graph
ev <- eigen(cor_auto(safe_habits_numeric)) # get eigenvalues - insert the data you want to calculate the scree plot for
ev # Show eigend values
ap <- parallel(subject=nrow(safe_habits_numeric),var=ncol(safe_habits_numeric),rep=100,cent=.05) #Calculate the acceleration factor
summary(ap)
nS <- nScree(ev$values) #Set up the Scree Plot 
plotnScree(nS) # Plot the ScreePlot Graph
my.vss <- VSS(cor_auto(safe_habits_numeric),title="VSS of BEA data")
print(my.vss[,1:12],digits =2)
VSS.plot(my.vss, title="VSS of 24 mental tests")
scree(safe_habits_numeric)
VSS.scree(cor_auto(safe_habits_numeric))
fa.parallel(cor_auto(safe_habits_numeric),n.obs=36)

# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 
fit <- principal(as.data.frame(safe_habits_numeric),4,rotate="varimax",scores=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
safety_scores<-fit$scores
#predict(fit,safe_habits_numeric)
#safety_scores<-scoreItems(fit$weights,as.data.frame(safe_habits_numeric))$scores
describe(safety_scores)
#by(scores$scores,data_bea$risk_classification,summary)
#wilcox.test(scores$scores[,1]~data_bea$risk_classification)

# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix work
fit <- principal(as.data.frame(work_experience),1,rotate="varimax",scores=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
experience_scores<-fit$scores
#predict(fit,safe_habits_numeric)
#experience_scores<-scoreItems(fit$weights,as.data.frame(work_experience))$scores
describe(experience_scores)
#by(scores$scores,data_bea$risk_classification,summary)
#wilcox.test(scores$scores[,1]~data_bea$risk_classification)

######################################################
#PATH ANALYSIS - From lavaan package - https://gist.github.com/joaovissoci/7838f4808885e506527e
######################################################
logistic_data<-data.frame(outcome=outcomes$injury,rtc=outcomes$rtc_involvement,nm=outcomes$nearmissmonth,safety_use,helmet_condition,peer_safety,safety_belief,age=demographics$age,hours=work_experience$hours,years=work_experience$years,hosp=outcomes$hospitalization)

logmodel<-glm(outcome ~ safety_use + helmet_condition + peer_safety + safety_belief + age + hours + years,family=binomial, data=logistic_data)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)

logmodel<-glm(nm ~ safety_use + helmet_condition + peer_safety + safety_belief + age + hours + years,family=binomial, data=logistic_data)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)

logmodel<-glm(outcome ~ safety_use + helmet_condition + peer_safety + safety_belief + age + hours + years,family=binomial, data=logistic_data)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)

logmodel<-glm(hosp ~ safety_use + helmet_condition + peer_safety + safety_belief + age + hours + years,family=binomial, data=logistic_data)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)


######################################################
#NETWORK ANALYSIS
####################################################

safety_use<-safety_scores[,1]
helmet_condition<-safety_scores[,2]
peer_safety<-safety_scores[,3]
safety_belief<-safety_scores[,4]
outcome1<-outcomes$rtc_involvement
outcome2<-outcomes$nearmissmonth
exp<-experience_scores[,1]#work_experience$years_work_onbodaboda#car::recode(work_experience$years_work_onbodaboda,"0:3=0;else=1")
#hours<-work_experience$hours_work_onbodaboda#car::recode(work_experience$hours_work_onbodaboda,"0:8=0;else=1")


network_data<-data.frame(safety_use,helmet_condition,
	peer_safety,safety_belief,outcome1,outcome2,exp)#,hours)

cor_data<-cor_auto(network_data)

groups<-list(Outcomes=c(5,6),Work_Hours=c(7),Safety_Behavior=c(1,2,3,4))
varLabels<-c("Safety use","Helmet condition","Peer safety",
	"Safety belief","Road Traffic Crash","Near miss","Work hours")
#varNames<-c("SF1","SF2","SF3","SF4","OUT1","OUT2","EXP")
#normalize<-function(x){(x-min(x))/(max(x)-min(x))}
#mean_data<-sapply(as.data.frame(sapply(network_data,normalize)),mean)
#vSize<-normalize(colSums(reasons_danger))*7

tiff("/home/joao/Desktop/tz_safehabits_figure3.tiff", units='in', width = 11, height = 6,compression = 'lzw',res=1200,bg = "white")
PcorGRAPH<-qgraph(cor_data,layout="spring",
	graph="pcor",sampleSize=nrow(network_data),
	legend.cex = 0.6,cut = 0.1, maximum = 0.4,
	 minimum = 0.1, esize = 5,vsize = 5, repulsion = 0.8,
	 labels=varLabels,borders = FALSE,legend=TRUE,
	 groups=groups,color=c("brown1","gold3","royalblue"),
	 label.scale=FALSE,label.cex=2,edge.labels=FALSE,
	 posCol="black",negCol="gray50")#,gray=T,)
	 #,nodeNames=nomesqsg, layout=Lqsg,,groups=qsggr,vsize=vSize*3
     #,,color=c("gold","steelblue","red","grey80"),
     #labels=rownames(pca_data)
dev.off()

#not run
#blabla<-function(x){
#	as.numeric(as.character(x))
#}
#
#blabla2<-lapply(safe_habits,blabla)

#network_data<-data.frame(work_experience,outcome1,outcome2,blabla2)#,hours)

#cor_data<-cor_auto(network_data)

#PcorGRAPH<-qgraph(cor_data,layout="spring",graph="glasso",sampleSize=nrow(network_data),legend.cex = 0.6,cut = 0.1, maximum = 0.4, minimum = 0.1, esize = 5,vsize = 5, repulsion = 0.8)#,labels=varLabels,borders = FALSE,legend=TRUE,groups=groups,color=c("brown1","gold3","royalblue"),label.scale=FALSE,label.cex=2,edge.labels=TRUE,posCol="black",negCol="gray50")#,gray=T,)#,nodeNames=nomesqsg, layout=Lqsg,,groups=qsggr,vsize=vSize*3,,color=c("gold","steelblue","red","grey80"),labels=rownames(pca_data)

# Para identificar no qgraph o resultado do algortimo de comunidade, criar objeto de "groups"
# com o resultado de wcG1
predictors<-centrality(PcorGRAPH)$ShortestPaths[,5]
predictors
#centralityPlot(qsgG3)

predictors<-centrality(PcorGRAPH)$ShortestPaths[,6]
predictors
