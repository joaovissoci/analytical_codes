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
lapply(c("sem","ggplot2", "psych", "RCurl", "irr", "nortest", "moments","GPArotation","nFactors","boot","psy", "car","vcd", "gridExtra","mi","VIM","epicalc","gdata","sqldf","reshape2","mclust","foreign","survival","memisc","foreign","mice","MissMech"), library, character.only=T)

#######################################################
#IMPORTING DATA
#######################################################

#Linux path
#data <- read.csv("/home/joao/Dropbox/datasets/DGHI/Africa_DGHI/rwanda/hotspot_epi_rwanda_data.csv")

data <- read.csv("/Users/joaovissoci/Dropbox/datasets/DGHI/Africa_DGHI/Rwanda/hotspot_epi_rwanda_data.csv")

#data set with information for Hamiton's Anxiety Symptoms
#data <- repmis::source_DropboxData("epi_sri_lanka_data.csv","5b3k7j4du69tmt2",sep = ",",header = TRUE)

#######################################################
#DATA MANAGEMENT
#######################################################
#creating the dataset 
data_epi<-NULL

#recoding outcome variable
outcome1<-car::recode(data$driver1_injuries,"0=1;1=100;3=1;2=100;else=0")
outcome2<-car::recode(data$driver2_injuries,"0=1;1=100;3=1;2=100;else=0")
outcome3<-car::recode(data$victim1_injury,"0=1;1=100;3=1;2=100;else=0")
outcome4<-car::recode(data$victim2_injury,"0=1;1=100;3=1;2=100;else=0")
outcome5<-car::recode(data$victim3_injury,"0=1;1=100;3=1;2=100;else=0")
outcome6<-rowSums(data.frame(outcome1,outcome2,outcome3,outcome4,outcome5))
data_epi$outcome<-car::recode(outcome6,"0=NA;1:99=0;100:500=1")

#recoding outcome variable - Number of victims
outcome1a<-car::recode(data$driver1_injuries,"0=0;1=1;3=0;2=1;else=NA")
outcome2a<-car::recode(data$driver2_injuries,"0=0;1=1;3=0;2=1;else=NA")
outcome3a<-car::recode(data$victim1_injury,"0=0;1=1;3=0;2=1;else=NA")
outcome4a<-car::recode(data$victim2_injury,"0=0;1=1;3=0;2=1;else=NA")
outcome5a<-car::recode(data$victim3_injury,"0=0;1=1;3=0;2=1;else=NA")
#soutcome6a<-rowSums(data.frame(outcome1,outcome2,outcome3,outcome4,outcome5))
#data_epi$outcome<-car::recode(outcome6,"0=NA;1:99=0;100:500=1")
victimis_outcome_driversonly<-c(outcome1a,outcome2a)
victimis_outcome_all<-c(outcome1a,outcome2a,outcome3a,outcome4a,outcome5a)

#number of victims in all crashes
#outcome 1 = 2452
#outcome 2 = 2119
#outcome 3 = 106
#outcome 4 = 10
#outcome 5 = 2

#merging more then one vector

#data_epi$outcome<-as.factor(data_epi$outcome)

#recoding day of crash
data_epi$day_crash<-car::recode(data$day_crash,"0='1';1='2';2='3';3='4';4='5';5='6';6='7';else=NA")
data_epi$day_crash<-as.factor(data_epi$day_crash)

#recoding hour of crash
data$hour_crash<-as.numeric(as.character(data$hour_crash))
data_epi$hour_crash<-car::recode(data$hour_crash,"8:16='aDay';17:24='Night';0='Night';1:7='Dawn'")
data_epi$hour_crash<-as.factor(data_epi$hour_crash)

#recoding road type
road_type1<-data$road_condition___4
road_type2<-car::recode(data$road_condition___5,'1=2')
road_type3<-rowSums(data.frame(road_type1,road_type2))
data_epi$road_type<-car::recode(road_type3,"0=NA;1='Asphalt';2='Soil';3=NA")
data_epi$road_type<-as.factor(data_epi$road_type)

#recoding road_condition
#road_condition1<-data$road_condition2___0
#road_condition2<-car::recode(data$road_condition2___6,'1=2')
#road_condition3<-car::recode(data$road_condition2___1,'1=3')
#road_condition4<-car::recode(data$road_condition2___7,'1=4')
#road_condition5<-rowSums(data.frame(road_condition1,road_condition2,road_condition3,road_condition4))
#data_epi$road_condition<-car::recode(road_condition5,"0=NA;1='Dry';2='Wet or Damaged';3='Wet or Damaged';4='Wet or Damaged'")
#data_epi$road_condition<-as.factor(data_epi$road_condition)

# weather condition
#weather_condition1<-data$weather_condition___2
#weather_condition2<-car::recode(data$weather_condition___3,"1=2")
#weather_condition3<-car::recode(data$weather_condition___4,"1=3")
#weather_condition4<-rowSums(data.frame(weather_condition1,weather_condition2,weather_condition3))
#weather_condition<-car::recode(weather_condition4,"1='Unclear';2='Unclear';3='Clear';else=NA")
#data_epi$weather_condition<-as.factor(weather_condition)

#recoding special conditions
#data_epi$special_condition1<-data$special_conditions___0
#data_epi$special_condition2<-data$special_conditions___1
#data_epi$special_condition3<-data$special_conditions___2
#data_epi$special_condition4<-data$special_conditions___3
#data_epi$special_condition5<-data$special_conditions___4
#data_epi$special_condition6<-data$special_conditions___5
#data_epi$special_condition7<-data$special_conditions___6
#data_epi$special_condition8<-data$special_conditions___7

#recoding visibility conditions
visibility1<-data$visibility_conditions___0
visibility2<-car::recode(data$visibility_conditions___1,'1=2')
visibility3<-car::recode(data$visibility_conditions___2,'1=3')
visibility4<-car::recode(data$visibility_conditions___3,'1=4')
visibility5<-car::recode(data$visibility_conditions___4,'1=5')
visibility6<-rowSums(data.frame(visibility1,visibility2,visibility3,visibility4,visibility5))
data_epi$visibility<-car::recode(visibility6,"0=NA;1='1';2='3';3='2';4='2';5='3'")
data_epi$visibility<-as.factor(data_epi$visibility)

#recoding type_vehicle
data_epi$type_vehicle<-car::recode(data$class1,"0=NA;1='acar';2='bus';3='truck';4=NA;5='motorcycle';6='bus';7='bus';8='acar';9=NA;else=NA")
data_epi$type_vehicle<-as.factor(data_epi$type_vehicle)

#recoding type_vehicle 2
data_epi$type_vehicle2<-car::recode(data$class2,"0=NA;1='acar';2='bus';3='truck';4='bicycle/pedestrian';5='motorcycle';6='bus';7='bus';8='acar';9='bicycle/pedestrian';else=NA")
data_epi$type_vehicle2<-as.factor(data_epi$type_vehicle2)

#recoding gender
data_epi$gender<-car::recode(data$driver1_sex,"0='female';1='male';else=NA")
#data_epi$gender<-as.factor(data_epi$gender)
data_epi$gender2<-car::recode(data$driver2_sex,"0='female';1='male';else=NA")
#data_epi$gender2<-as.factor(data_epi$gender2)
victms_gender<-c(data_epi$gender,data_epi$gender2)
victms_gender<-car::recode(victms_gender,"1='female';2='male'")

#recoding type of crash
crash_type1SUM<-rowSums(with(data,data.frame(crash_type___0,crash_type___1,crash_type___2,crash_type___5,crash_type___6)))
crash_type_collision<-car::recode(crash_type1SUM,"0=0;else=1")

crash_type2SUM<-rowSums(with(data,data.frame(crash_type___3,crash_type___4,crash_type___7)))
crash_type_runover<-car::recode(crash_type2SUM,"0=0;else=1")

crash_type3SUM<-rowSums(with(data,data.frame(crash_type___8,crash_type___9,crash_type___10,crash_type___11)))
crash_type_fallloss<-car::recode(crash_type3SUM,"0=0;else=1")

crash_type<-rowSums(data.frame(crash_type3SUM,crash_type2SUM,crash_type1SUM))
data_epi$crash_type<-car::recode(crash_type,"0=NA;1='collision';2='runover';3='runover'")
data_epi$crash_type<-as.factor(data_epi$crash_type)

date_crash<-as.Date(data$date_crash)

#recoding ages
driver1_dob<-car::recode(data$driver1_dob,"'Kicukiro'=NA;99=NA;21=1992;29=1984;974=NA;19=1994")
driver1_dob<-as.Date(driver1_dob,"%Y")
data_epi$age<-as.numeric((date_crash-driver1_dob)/360)

driver2_dob<-car::recode(data$driver2_dob,"'nyarugenge'=NA;'Kicukiro'=NA;7=NA;99=NA;'63 years'=1950;6=2007;5=2008;46=1967;4=2009;31=1982;30=1981;3=2010;21=1992;22=1991;29=1984;28=1985;974=NA;19=1994;199=NA;0=NA")
driver2_dob<-as.Date(driver2_dob,"%Y")
data_epi$age2<-as.numeric((date_crash-driver2_dob)/360)

driver3_dob<-car::recode(data$victim1_age,"99=NA;1=2012;5=2008;23=1990;24=1989;31=1982;32=1981;37=1976;39=1974;41=1972;55=1958;22=1991;28=1985;19=1994;0=NA")
driver3_dob<-as.Date(as.factor(driver3_dob),"%Y")
data_epi$age3<-as.numeric((date_crash-driver3_dob)/360)

driver4_dob<-car::recode(data$victim2_age,"99=NA;10=2003;33=1980;34=1979")
driver4_dob<-as.Date(as.factor(driver4_dob),"%Y")
data_epi$age4<-as.numeric((date_crash-driver4_dob)/360)

driver5_dob<-car::recode(data$victim3_age,"99=NA")
driver5_dob<-as.Date(as.factor(driver5_dob),"%Y")
data_epi$age5<-as.numeric((date_crash-driver5_dob)/360)

age_victims<-c(data_epi$age,data_epi$age2,data_epi$age3,data_epi$age4,data_epi$age5)

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