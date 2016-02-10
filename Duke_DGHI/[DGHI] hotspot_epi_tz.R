#######################################################################################
#epi_rti_sri_lanka.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. see full license at the end of this file.
#######################################################################################
#this script follows a combination of the guidelines proposed by Hadley Wickham http://goo.gl/c04kq as well as using the formatR package http://goo.gl/ri6ky
#if this is the first time you are conducting an analysis using this protocol, please watch http://goo.gl/DajIN while following step by step

#link to manuscript

#####################################################################################
#SETTING ENVIRONMENT
#####################################################################################
 #install.packages("VIM")
 #install.packages("VIMGUI")
 #install.packages("miP")
 #install.packages("gWidgetsRGtk2")
 #install.packages("mi")
 #install.packages("epicalc")

#Load packages neededz for the analysis
#All packages must be installes with install.packages() function
lapply(c("sem","ggplot2", "psych", "irr", "nortest", "moments",
	"GPArotation","nFactors","boot","psy", "car","vcd", "gridExtra","mi",
	"VIM","epicalc","gdata","sqldf","reshape2","mclust","foreign","survival"
	,"memisc","foreign","mice","MissMech"), library, character.only=T)

#######################################################
#IMPORTING DATA
#######################################################

#Linux path
data <- read.csv("/home/joao/Dropbox/datasets/DGHI/Africa_DGHI/Tz/epi_rti_tz_police_data.csv")

#Mac path
#data <- read.csv("/Users/joaovissoci/Dropbox/datasets/DGHI/Africa_DGHI/Tz/epi_rti_tz_police_data.csv")
holiday
#data set with information for Hamiton's Anxiety Symptoms
#data <- repmis::source_DropboxData("epi_sri_lanka_data.csv","5b3k7j4du69tmt2",sep = ",",header = TRUE)

#######################################################
#DATA MANAGEMENT
#######################################################
data2<-with(data,data.frame(age,male,time_crash,class_crash,
	urban_location,holiday,day_week,crash_type,rd_condition,weather,
	light_condition,type_location,loc_ped_invol,traffic_control,
	posted_speed_limit,speed_limit,severe,element_type,human_crash_factor,
	ped_precrash,vehicle_factors,alcohol,victim_classification,protection))

#creating the dataset 
data_epi<-NULL

#AGE
data_epi$age<-data2$age

#MALE GENDER
data_epi$male<-car::recode(data2$male
,"99=NA")

#TIME OF CRASH
#recoding hour of crash
hour_crash<-sapply(strsplit(as.character(data2$time_crash), ":"), "[", 1)
hour_crash<-as.numeric(hour_crash)
data_epi$hour_crash<-car::recode(hour_crash,"8:16='aDay';17:24='Night';
	0='Night';1:7='Dawn';else=NA")
data_epi$hour_crash<-as.factor(data_epi$hour_crash)

#CLASS OF CRASH
data_epi$class_crash<-car::recode(data2$class_crash,
	"0='fata'l;1='fatal';2='non-fatal';3='non-fatal';else=NA")
data_epi$class_crash<-as.factor(data_epi$class_crash)

# LOCATION OF CRASH
data_epi$urban_location<-car::recode(data2$urban_location,
	"0='rural';1='urban';else=NA")

# NORMAL DAY Vs. HOLIDAY
data_epi$holiday<-car::recode(data2$holiday,"0='normalday';1='weekend';
	2='holiday';3='holiday';4='holiday';else=NA")

# Day OF the WEEK
data_epi$day_week<-data2$day_week

# TYPE OF CRASH
data_epi$crash_type<-car::recode(data2$crash_type,"0='collision';
	1='collision';2='collision';3='collisionVRU';4='collisionVRU';
	5='collision';6='collision';7='collisionVRU';8='lostcontrol';
	9='lostcontrol';10='lostcontrol';11='lostcontrol',90='Other';else=NA")

# Road condition
data_epi$rd_condition<-car::recode(data2$rd_condition,"0='clear';1='unclear';
	2='unclear';3='rain';4='unclear';90='Others';else=NA")

# Weather
data_epi$weather<-car::recode(data2$weather,"0='clear';1='unclear';
	2='unclear';3='unclear';4='unclear';90='Others';else=NA")

# Light condition
data_epi$light_condition<-car::recode(data2$light_condition,"0='daylight';
	1='nightnolight';2='duskdawn';3='nightnolight';4='nightgooglight';
	else=NA")

# Type of Location
data_epi$type_location<-car::recode(data2$type_location,"0='directroad';
	1='junction';2='junction';3='junction';4='roudabout';5='junction';
	6='entrance';7='railroad crossing';90='Other';else=NA")

# Type of Location
data_epi$loc_ped_invol<-car::recode(data2$loc_ped_invol,"0='pedestrian crossing'; 1='pedestrian crossing';2='pedestrian crossing';
	3='pedestrian crossing';4='outside sidewalk';5='outside sidewalk';
	89='No pedestrian';90='Other';else=NA")

# Traffic Control
data_epi$traffic_control<-car::recode(data2$traffic_control, "
	0='policeorwarden';1='traffic_lights';2='signs';3='sign';
	4='policeorwarden';5='no_control'")

# Speed limit
data_epi$speed_limit<-data2$speed_limit

# Speed limit sign
data_epi$speed_limit_sign<-as.factor(data2$posted_speed_limit)

# type of vechile
data_epi$type_vehicle<-car::recode(data2$element_type,"1='acar';2='bus';3='")

# human_crash_factor
data_epi$human_crash_factor<-data2$human_crash_factor

# ped_precrash
data_epi$ped_precrash<-data2$ped_precrash

#vehicle_factors
data_epi$vehicle_factors<-data2$vehicle_factors

#alcohol
data_epi$alcohol<-data2$alcohol

#victim_classification
data_epi$victim_classification<-car::recode(data2$victim_classification,"0='driver';1='pedestrian';2='passenger';3='passenger';4='passenger';
	9=NA")

#protection
data_epi$protection<-data2$protection

#######################################################
#ANALYZING MISSING DATA
#######################################################
#Studying missing data
#Calculating frequency of missing data per variable
propmiss <- function(dataframe) lapply(dataframe,function(x) data.frame(nmiss=sum(is.na(x)), n=length(x), propmiss=sum(is.na(x))/length(x)))

propmiss(data_epi)

#inspecting measure random of missing data
#Inspectif Weather Conditions
#weather_missing<-car::recode(data_epi$weather_condition,"NA=0;else=1")
#logmodel<-glm(weather_missing ~  data_epi$day_crash,family=binomial)
#summary(logmodel)
#logmodel<-glm(weather_missing ~  data_epi$hour_crash,family=binomial)
#summary(logmodel)
#logmodel<-glm(weather_missing ~  data_epi$road_type,family=binomial)
#summary(logmodel)
#logmodel<-glm(weather_missing ~  data_epi$road_condition,family=binomial)
#summary(logmodel)
#logmodel<-glm(weather_missing ~  data_epi$visibility,family=binomial)
#summary(logmodel)
#logmodel<-glm(weather_missing ~  data_epi$type_vehicle,family=binomial)
#summary(logmodel)
#logmodel<-glm(weather_missing ~  data_epi$type_vehicle2,family=binomial)
#summary(logmodel)
#logmodel<-glm(weather_missing ~  data_epi$gender,family=binomial)
#summary(logmodel)
#logmodel<-glm(weather_missing ~  data_epi$crash_type,family=binomial)
#summary(logmodel)

#Inspecting Visibility
visibility_missing<-car::recode(data_epi$visibility,"NA=0;else=1")
logmodel<-glm(visibility_missing ~  data_epi$day_crash,family=binomial)
summary(logmodel)
logmodel<-glm(visibility_missing ~  data_epi$hour_crash,family=binomial)
summary(logmodel)
logmodel<-glm(visibility_missing ~  data_epi$road_type,family=binomial)
summary(logmodel)
logmodel<-glm(visibility_missing ~  data_epi$type_vehicle,family=binomial)
summary(logmodel)
logmodel<-glm(visibility_missing ~  data_epi$type_vehicle2,family=binomial)
summary(logmodel)
logmodel<-glm(visibility_missing ~  data_epi$gender,family=binomial)
summary(logmodel)
logmodel<-glm(visibility_missing ~  data_epi$crash_type,family=binomial)
summary(logmodel)

#Inspecting road_condition
#roadcondition_missing<-car::recode(data_epi$road_condition,"NA=0;else=1")
#logmodel<-glm(roadcondition_missing ~  data_epi$day_crash,family=binomial)
#summary(logmodel)
#logmodel<-glm(roadcondition_missing ~  data_epi$hour_crash,family=binomial)
#summary(logmodel)
#logmodel<-glm(roadcondition_missing ~  data_epi$road_type,family=binomial)
#summary(logmodel)
#logmodel<-glm(roadcondition_missing ~  data_epi$road_condition,family=binomial)
#summary(logmodel)
#logmodel<-glm(roadcondition_missing ~  data_epi$weather_condition,family=binomial)
#summary(logmodel)
#logmodel<-glm(roadcondition_missing ~  data_epi$type_vehicle,family=binomial)
#summary(logmodel)
#logmodel<-glm(roadcondition_missing ~  data_epi$type_vehicle2,family=binomial)
#summary(logmodel)
#logmodel<-glm(roadcondition_missing ~  data_epi$gender,family=binomial)
#summary(logmodel)
#logmodel<-glm(roadcondition_missing ~  data_epi$crash_type,family=binomial)
#summary(logmodel)
#out <- TestMCARNormality(data_epi)

#Inspecting road_condition
type_vehicle2_missing<-car::recode(data_epi$type_vehicle2,"NA=0;else=1")
logmodel<-glm(type_vehicle2_missing ~  data_epi$day_crash,family=binomial)
summary(logmodel)
logistic.display(logmodel)
logmodel<-glm(type_vehicle2_missing ~  data_epi$hour_crash,family=binomial)
summary(logmodel)
logistic.display(logmodel)
logmodel<-glm(type_vehicle2_missing ~  data_epi$road_type,family=binomial)
summary(logmodel)
logmodel<-glm(type_vehicle2_missing ~  data_epi$road_condition,family=binomial)
summary(logmodel)
logmodel<-glm(type_vehicle2_missing ~  data_epi$weather_condition,family=binomial)
summary(logmodel)
logmodel<-glm(type_vehicle2_missing ~  data_epi$visibility,family=binomial)
summary(logmodel)
logmodel<-glm(type_vehicle2_missing ~  data_epi$type_vehicle,family=binomial)
summary(logmodel)
logistic.display(logmodel)

logmodel<-glm(type_vehicle2_missing ~  data_epi$gender,family=binomial)
summary(logmodel)
logmodel<-glm(type_vehicle2_missing ~  data_epi$crash_type,family=binomial)
summary(logmodel)

#out <- TestMCARNormality(data_epi)
#missing.pattern.plot(data_epi)
#MICE framework for imputation
# describing the pattern of missingnesss
md.pattern(data_epi)

# showing pairs of missingines
md.pairs(data_epi)

# plots impact of missing data for a set of pairs - works better for numerical data
marginplot(data.frame(data_epi$outcome,data_epi$visibility), col = mdc(1:2), cex = 1.2, cex.lab = 1.2, cex.numbers = 1.3, pch = 19)

# generate imputations
# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
imp <- mice(data_epi, seed = 2222, m=50)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
data_imputed<-complete(imp,1)

#Plost the distrbution of each of the 5 possibilities of imputations
stripplot(imp,pch=20,cex=1.2)

#plots a scatter plot of pairs of variables
xyplot(imp, outcome ~ visibility | .imp, pch = 20, cex = 1.4)

#returns the matrix specifying each variable used to -predict imputation - columns 1=predictor 0=not predictor. rows are the variables of interest
imp$predictorMatrix
pred <- imp$predictorMatrix #if you want to exclude  variable from the prediction model for imputation then assign an obect to pred
pred[, "bmi"] <- 0 #transform the column values into 0's for not predictiong
imp <- mice(nhanes, pred = pred, pri = FALSE) # rerun the model specifying pred argumento witht eh matriz recoded.

#######################################################
#DESCRIPTIVE ANALYSIS
#######################################################
#####Victmis descriptives
# Gender
table<-table(victms_gender)
table
prop.table(table)
table<-table(victms_gender,victimis_outcome_driversonly)
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package
logmodel<-glm(victimis_outcome_driversonly ~   as.factor(victms_gender),family=binomial)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)

# Age
describe(age_victims)
describeBy(age_victims,victimis_outcome_all)
# t-test: # independent 2-group, 2 level IV
testName <- t.test(age_victims ~ victimis_outcome_all)

# Crashes with victims
table<-with(data_epi,table(outcome))
table
prop.table(table)

# Gender
table<-with(data_epi,table(gender))
table
prop.table(table)
table<-with(data_epi,table(gender,outcome))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Hour of Crash
table<-with(data_epi,table(hour_crash))
table
prop.table(table)
table<-with(data_epi,table(hour_crash,outcome))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Day of the Week
table<-with(data_epi,table(day_crash))
table
prop.table(table)
table<-with(data_epi,table(day_crash,outcome))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Crash Type
table<-with(data_epi,table(crash_type))
table
prop.table(table)
table<-with(data_epi,table(crash_type,outcome))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Visibility
table<-with(data_epi,table(visibility))
table
prop.table(table)
table<-with(data_epi,table(visibility,outcome))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Weather
table<-with(data_epi,table(weather_condition))
table
prop.table(table)
table<-with(data_epi,table(weather_condition,outcome))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Road Pavement
table<-with(data_epi,table(road_type))
table
prop.table(table)
table<-with(data_epi,table(road_type,outcome))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Road Condition
table<-with(data_epi,table(road_condition))
table
prop.table(table)
table<-with(data_epi,table(road_condition,outcome))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Type of Vehicle
table<-with(data_epi,table(type_vehicle))
table
prop.table(table)
table<-with(data_epi,table(type_vehicle,outcome))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Type of Vehicle 2
table<-with(data_epi,table(type_vehicle2))
table
prop.table(table)
table<-with(data_epi,table(type_vehicle2,outcome))
table
prop.table(tabulatle,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Age
ad.test(data_epi$age)
describe(data_epi$age)
#hist(data_epi$age )
#ci_func(data_epi$age ,.95)
by(data_epi$age ,data_epi$outcome,describe)
wilcox.test(data_epi$age ~data_epi$outcome)

#######################################################
#BIVARIATE ANALYSIS
#######################################################
#Numeric Variables
#Age
#data_epi$type_vehicle<-car::recode(data_epi$type_vehicle,"'pedestrian'=NA;'bicycle'=NA")
logmodel<-glm(outcome ~   age+gender+day_crash+hour_crash+road_type+visibility+type_vehicle+type_vehicle2+crash_type,family=binomial, data=data_epi)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)

logmodel<-with(data_imputed,glm(outcome ~  age+gender+day_crash+hour_crash+road_type+visibility+type_vehicle+type_vehicle2+crash_type,family=binomial))
summary(logmodel)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients
################################################################################
#epi_rti_sri_lanka.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. see full license at the end of this file.
################################################################################