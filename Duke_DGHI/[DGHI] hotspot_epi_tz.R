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

#######################################################
#DATA MANAGEMENT
#######################################################
data2<-with(data,data.frame(age,male,time_crash,class_crash,
	urban_location,holiday,day_week,crash_type,rd_condition,weather,
	light_condition,type_location,loc_ped_invol,traffic_control,
	posted_speed_limit,speed_limit,severe,element_type,human_crash_factor,
	ped_precrash,vehicle_factors,alcohol,victim_classification,protection,rd_size))

#creating the dataset 
data_epi<-NULL

#AGE
data_epi$age<-data2$age

#MALE GENDER
data_epi$gender<-car::recode(data2$male
,"99=NA")

#TIME OF CRASH
#recoding hour of crash
hour_crash<-sapply(strsplit(as.character(data2$time_crash), ":"), "[", 1)
hour_crash<-as.numeric(hour_crash)
data_epi$hour_crash<-car::recode(hour_crash,"8:16='aDay';17:24='Night';
	0='Night';1:7='Dawn';else=NA")
data_epi$hour_crash<-as.factor(data_epi$hour_crash)

#CLASS OF CRASH
data_epi$outcome<-car::recode(data2$class_crash,
	"0='afatal';1='afatal';2='non-fatal';3='non-fatal';else=NA")
data_epi$outcome<-as.factor(data_epi$outcome)

# LOCATION OF CRASH
data_epi$urban_location<-car::recode(data2$urban_location,
	"0='rural';1='urban';else=NA")

# NORMAL DAY Vs. HOLIDAY
data_epi$holiday<-car::recode(data2$holiday,"0='normalday';1='holiday';
	2='holiday';3='holiday';4='holiday';else=NA")

# Day OF the WEEK
data_epi$day_week<-data2$day_week

# TYPE OF CRASH
data_epi$crash_type<-car::recode(data2$crash_type,"0='collision';
	1='collision';2='collision';3='collisionVRU';4='collisionVRU';
	5='collision';6='collision';7='collisionVRU';8='lostcontrol';
	9='lostcontrol';10='lostcontrol';11='lostcontrol';90='Other';else=NA")

# Road condition
data_epi$rd_condition<-car::recode(data2$rd_condition,"0='dry';1='slippery';
	2='slippery';3='slippery';4='slippery';90='slippery';else=NA")

# Weather
data_epi$weather<-car::recode(data2$weather,"0='clear';1='unclear';
	2='unclear';3='unclear';4='unclear';90='unclear';else=NA")

# Light condition
data_epi$light_condition<-car::recode(data2$light_condition,"0='daylight';
	1='nightnolight';2='duskdawn';3='nightnolight';4='nightgooglight';
	else=NA")

# Type of Location
data_epi$type_location<-car::recode(data2$type_location,"0='directroad';
	1='junction';2='junction';3='junction';4='junction';5='junction';
	6='junction';7='junction';90='junction';else=NA")

# Type of Location
data_epi$loc_ped_invol<-car::recode(data2$loc_ped_invol,"0='pedestrian crossing'; 1='pedestrian crossing';2='pedestrian crossing';
	3='pedestrian crossing';4='outside sidewalk';5='outside sidewalk';
	89='No pedestrian';90='outside sidewalk';else=NA")

# Traffic Control
data_epi$traffic_control<-car::recode(data2$traffic_control, "
	0='policeorwarden';1='signs';2='signs';3='signs';
	4='policeorwarden';5='no_control'")

# Speed limit
data_epi$speed_limit<-data2$speed_limit

# Speed limit sign
data_epi$speed_limit_sign<-as.factor(data2$posted_speed_limit)

# type of vechile
data_epi$type_vehicle<-car::recode(data2$element_type,"1='acar';
	2='bus';3='truck';4='Other';5='motorcycle';6='motorcycle';
	7='truck';8='bus';9='bus';10='bus';11='Other';12='Other';
	13='pedestrian';90='Other';99=NA")

# human_crash_factor
data_epi$human_crash_factor<-car::recode(data2$human_crash_factor,"
	0='speeding or aggressive driving';1='speeding or aggressive driving';
	2='Other'; 3='Other'; 4='Other'; 5='Other';6='Other';
	7='Other';8='Other'; 90='Other';99=NA")

# ped_precrash
data_epi$ped_precrash<-data2$ped_precrash

#vehicle_factors
data_epi$vehicle_factors<-data2$vehicle_factors

#Tested for alcohol
data_epi$alcohol_tested<-car::recode(data2$alcohol,"0='tested';
	1='tested';2='not tested'")

#alcohol positive
data_epi$alcohol_pos<-car::recode(data2$alcohol,"0='negative';
	1='positive';else=NA")

#victim_classification
data_epi$victim_classification<-car::recode(data2$victim_classification,"0='driver';1='pedestrian';2='passenger';3='passenger';4='passenger';
	9=NA")

#Seat Belt Use
#data_epi$protection<-car::recode(data2$protection,"99=NA")
car_drivers<-subset(data_epi,data2$element_type==1)
car_drivers$seat_belt_use<-car::recode(car_drivers$protection,"1='yes';
	2='no';else=NA")

#helmet use
motorcycle_drivers<-subset(data_epi,data2$element_type==5)
motorcycle_drivers$seat_belt_use<-car::recode(motorcycle_drivers$protection,"3='yes'; 4='no';else=NA")

#alcohol positive
data_epi$rd_size<-car::recode(data2$rd_size,"0='single';
	1='two';else=NA")

#tranform dlist into a data.frame
data_epi<-as.data.frame(data_epi)

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

# organize data set to be imputed
data_tobeimp<-with(data_epi,data.frame(hour_crash,urban_location,outcome,
	holiday,day_week,crash_type,rd_condition,weather,light_condition,
	type_location,traffic_control,speed_limit_sign,type_vehicle,
	human_crash_factor,ped_precrash,alcohol_tested,victim_classification,
	rd_size))

# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
imp <- mice(data_tobeimp, seed = 2222, m=50)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
data_imputed<-complete(imp,4)

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
# Age
with(data_epi,describe(age))
with(data_epi,describeBy(age,class_crash))
# t-test: # independent 2-group, 2 level IV
with(data_epi,t.test(age ~ class_crash))

# Crashes with victims
table<-with(data_epi,table(class_crash))
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

# Location
table<-with(data_epi,table(urban_location))
table
prop.table(table)
table<-with(data_epi,table(urban_location,outcome))
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
table<-with(data_epi,table(day_week))
table
prop.table(table)
table<-with(data_epi,table(day_week,outcome))
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

# Road condition
table<-with(data_epi,table(rd_condition))
table
prop.table(table)
table<-with(data_epi,table(rd_condition,outcome))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Weather
table<-with(data_epi,table(weather))
table
prop.table(table)
table<-with(data_epi,table(weather,outcome))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Light Condition
table<-with(data_epi,table(light_condition))
table
prop.table(table)
table<-with(data_epi,table(light_condition,outcome))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# type_location
table<-with(data_epi,table(type_location))
table
prop.table(table)
table<-with(data_epi,table(type_location,outcome))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Traffic control
table<-with(data_epi,table(traffic_control))
table
prop.table(table)
table<-with(data_epi,table(traffic_control,outcome))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Speed limit
table<-with(data_epi,table(speed_limit))
table
prop.table(table)
table<-with(data_epi,table(speed_limit,outcome))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Speed limit Sign
table<-with(data_epi,table(speed_limit_sign))
table
prop.table(table)
table<-with(data_epi,table(speed_limit_sign,outcome))
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

# Pedestrian involved
table<-with(data_epi,table(loc_ped_invol))
table
prop.table(table)
table<-with(data_epi,table(loc_ped_invol,outcome))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# human factor involved
table<-with(data_epi,table(human_crash_factor))
table
prop.table(table)
table<-with(data_epi,table(human_crash_factor,outcome))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# pedestrian factor involved
table<-with(data_epi,table(ped_precrash))
table
prop.table(table)
table<-with(data_epi,table(ped_precrash,outcome))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# vehicle factor involved
table<-with(data_epi,table(vehicle_factors))
table
prop.table(table)
table<-with(data_epi,table(vehicle_factors,outcome))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# alcohol
table<-with(data_epi,table(alcohol_tested))
table
prop.table(table)
table<-with(data_epi,table(alcohol_tested,outcome))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# alcohol
table<-with(data_epi,table(alcohol_pos))
table
prop.table(table)
table<-with(data_epi,table(alcohol_pos,outcome))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# TYpe of victim
table<-with(data_epi,table(victim_classification))
table
prop.table(table)
table<-with(data_epi,table(victim_classification,outcome))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Protection used
table<-with(car_drivers,table(seat_belt_use))
table
prop.table(table)
table<-with(car_drivers,table(seat_belt_use,outcome))
table
prop.table(table,2)-
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Protection used
table<-with(motorcycle_drivers,table(helmet_use))
table
prop.table(table)
table<-with(motorcycle_drivers,table(helmet_use,outcome))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Holiday
table<-with(data_epi,table(holiday))
table
prop.table(table)
table<-with(data_epi,table(holiday,outcome))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Road Size
table<-with(data_epi,table(rd_size))
table
prop.table(table)
table<-with(data_epi,table(rd_size,outcome))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#######################################################
#MULTIVARIATE ANALYSIS
#######################################################

#MODEL 1 - Adding every variable
#traffic control was not added because had cases with 0 observations
# age and gender becaise the missing rate wsa to high

logmodel<-glm(outcome ~ hour_crash +
						urban_location +
						holiday +
						as.factor(day_week) +
						crash_type +
						rd_condition +
						weather +
						light_condition +
						type_location +
						speed_limit_sign +
						type_vehicle +
						human_crash_factor +
						alcohol_tested +
						victim_classification +
						rd_size
			,family=binomial, data=data_imputed)

summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)

#MODEL 2 - Excluding vars with less then 0.20 i the bivariate analysis
#traffic control was not added because had cases with 0 observations
# age and gender becaise the missing rate wsa to high

logmodel<-glm(outcome ~ 
						urban_location +
						crash_type +
						light_condition +
						type_location +
						speed_limit_sign +
						type_vehicle +
						human_crash_factor +
						alcohol_tested
			,family=binomial, data=data_imputed)

summary(logmodel)
#anova(reglogGEU)
# exponentiated coefficients
# and 95% CI for exponentiated coefficients
exp(cbind(Odds=coef(logmodel),confint(logmodel))) 
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)

#############################################################################
#epi_rti_sri_lanka.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. see full license at the end of this file.
#############################################################################