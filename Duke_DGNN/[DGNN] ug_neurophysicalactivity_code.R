######################################################################
#BASIC R STATISTICS TEMPLATE
######################################################################
#
#
#
#
#
######################################################################
#SETTING ENVIRONMENT
######################################################################
#PASCKAGES INSTALLATION CODES
#install.packages("Hmisc")
#install.packages("car")
#install.packages("psych")
#install.packages("nortest")
#install.packages("ggplot2")
#install.packages("pastecs")
#install.packages("repmis")
#install.packages("mvnormtest")
#install.packages("polycor")

#PACKAGES LOADING CODE
#Load packages neededz for the analysis
#library(Hmisc)

#All packages must be installes with install.packages() function
lapply(c("ggplot2", "psych", "RCurl", "irr", "nortest", 
  "moments","GPArotation","nFactors","boot","psy", "car",
  "vcd", "gridExtra","mi","VIM","gdata",
  "reshape2","mclust","foreign","survival","memisc","lme4",
  "lmerTest","dplyr","eRm","mirt","dplyr","devtools","reshape",
  "mice","jsonlite","tidyverse","pROC","Epi"),
library, character.only=T)

######################################################################
#IMPORTING DATA
######################################################################
#LOADING DATA FROM A .CSV FILE
data<-read.csv("/Users/Joao/Box Sync/Home Folder jnv4/Data/DGNN/Ug_neuropatientsphysicalactivity/ug_neurophysicalactivity_data.csv",sep=",")
#information between " " are the path to the directory in your computer where the data is stored

######################################################################
#DATA MANAGEMENT
######################################################################

# Changing the names of the vectors
colnames(data)<-c("day",
				  "patient_id",
				  "time",
				  "no_movement",
				  "lying_in_bed",
				  "prom",
				  "non_purpose_movement",
				  "purpose_head_trunk_upper",
				  "purpose_lower_extremity",
				  "sit_exercisein_bed",
				  "sitting_edge_bed",
				  "sitting_bucket",
				  "standing",
				  "moving_bedtobucket",
				  "stat_marching",
				  "walking_assistance_2",
				  "walking_assistance_1",
				  "walking_contact_guard",
				  "walking_independently_w_assistance",
				  "walking_independently",
				  "deleteme1",
				  "deleteme2",
				  "bed",
				  "patient_room",
				  "patient_br",
				  "chair",
				  "or",
				  "off_unit",
				  "alone",
				  "family",
				  "nursing",
				  "neurosurgeon",
				  "pt",
				  "social_worker",
				  "other_medical",
				  "change_mv",
				  "icu_nsw",
				  "deleteme3",
				  "freq_observations",
				  "date_admission",
				  "time_admission",
				  "date_unit_admission",
				  "deleteme4",
				  "disposition_after_trauma",
				  "surgery",
				  "age",
				  "gender",
				  "pre_morbid_mobility",
				  "gcs_admission",
				  "gcs_observation",
				  "sedation",
				  "mv02_type",
				  "current_unit",
				  "date_admission_currentunit",
				  "deleteme5",
				  "medical_activity_restriction",
				  "deleteme6",
				  "deleteme7",
				  "deleteme8",
				  "freq_rehab_sessions",
				  "highest_activity_level_rehab",
				  "los_hospitalization",
				  "los_gu",
				  "los_hd",
				  "los_icu",
				  "los_trauma",
				  "los_sedation",
				  "los_mv",
				  "deleteme9"
				 )

# Getting unique data to calculate descriptives

data_descriptives<-NULL

for(i in 1:8){

	temp<-subset(data,data$day==i)
	temp2<-temp[1:10,]

	data_descriptives<-rbind(data_test,temp2)

}

#######################################################
#ANALYZING MISSING DATA
#######################################################
#Studying missing data
#Calculating frequency of missing data per variable
# propmiss <- function(dataframe) lapply(dataframe,function(x) data.frame(nmiss=sum(is.na(x)), n=length(x), propmiss=sum(is.na(x))/length(x)))

# propmiss(data_epi)

# #inspecting measure random of missing data
# #Inspectif Weather Conditions
# #weather_missing<-car::recode(data_epi$weather_condition,"NA=0;else=1")
# #logmodel<-glm(weather_missing ~  data_epi$day_crash,family=binomial)
# #summary(logmodel)
# #logmodel<-glm(weather_missing ~  data_epi$hour_crash,family=binomial)
# #summary(logmodel)
# #logmodel<-glm(weather_missing ~  data_epi$road_type,family=binomial)
# #summary(logmodel)
# #logmodel<-glm(weather_missing ~  data_epi$road_condition,family=binomial)
# #summary(logmodel)
# #logmodel<-glm(weather_missing ~  data_epi$visibility,family=binomial)
# #summary(logmodel)
# #logmodel<-glm(weather_missing ~  data_epi$type_vehicle,family=binomial)
# #summary(logmodel)
# #logmodel<-glm(weather_missing ~  data_epi$type_vehicle2,family=binomial)
# #summary(logmodel)
# #logmodel<-glm(weather_missing ~  data_epi$gender,family=binomial)
# #summary(logmodel)
# #logmodel<-glm(weather_missing ~  data_epi$crash_type,family=binomial)
# #summary(logmodel)

# #out <- TestMCARNormality(data_epi)
# #missing.pattern.plot(data_epi)
# #MICE framework for imputation
# # describing the pattern of missingnesss
# md.pattern(data_epi)

# # showing pairs of missingines
# md.pairs(data_epi)

# # plots impact of missing data for a set of pairs - works better for numerical data
# marginplot(data.frame(data_epi$outcome,data_epi$visibility), col = mdc(1:2), cex = 1.2, cex.lab = 1.2, cex.numbers = 1.3, pch = 19)

# # generate imputations

# # organize data set to be imputed
# data_tobeimp<-with(data_epi,data.frame(hour_crash,urban_location,outcome,
# 	holiday,day_week,crash_type,rd_condition,weather,light_condition,
# 	type_location,traffic_control,speed_limit_sign,type_vehicle,
# 	human_crash_factor,ped_precrash,alcohol_tested,victim_classification,
# 	rd_size))

# # argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
# imp <- mice(data_tobeimp, seed = 2222, m=50)

# # reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
# data_imputed<-complete(imp,4)

# #Plost the distrbution of each of the 5 possibilities of imputations
# #stripplot(imp,pch=20,cex=1.2)

# #plots a scatter plot of pairs of variables
# #xyplot(imp, outcome ~ visibility | .imp, pch = 20, cex = 1.4)

# #returns the matrix specifying each variable used to -predict imputation - columns 1=predictor 0=not predictor. rows are the variables of interest
# #imp$predictorMatrix
# #pred <- imp$predictorMatrix #if you want to exclude  variable from the prediction model for imputation then assign an obect to pred
# #pred[, "bmi"] <- 0 #transform the column values into 0's for not predictiong
# #imp <- mice(nhanes, pred = pred, pri = FALSE) # rerun the model specifying pred argumento witht eh matriz recoded.

######################################################################
#TABLE 1
######################################################################

# Age
with(data_descriptives,describe(age))
with(data_descriptives,describeBy(age,class_crash))
# t-test: # independent 2-group, 2 level IV
with(data_descriptives,t.test(age ~ class_crash))

# Gender
table<-with(data_descriptives,table(gender))
table
prop.table(table)
# table<-with(data_descriptives,table(gender,outcome))
# table
# prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Diagnosis
table<-with(data_descriptives,table(surgery))
table
prop.table(table)
# table<-with(data_descriptives,table(surgery,outcome))
# table
# prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# GCS at admission
with(data_descriptives,describe(gcs_admission))
# with(data_descriptives,describeBy(gcs_admission,class_crash))
# t-test: # independent 2-group, 2 level IV
with(data_descriptives,t.test(gcs_admission ~ class_crash))

# GCS at observation
with(data_descriptives,describe(gcs_observation))
# with(data_descriptives,describeBy(gcs_observation,class_crash))
# t-test: # independent 2-group, 2 level IV
with(data_descriptives,t.test(gcs_observation ~ class_crash))

# medical_activity_restriction
table<-with(data_descriptives,table(medical_activity_restriction))
table
prop.table(table)
# table<-with(data_descriptives,table(medical_activity_restriction,outcome))
# table
# prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Mechanical Ventilation 02_type
table<-with(data_descriptives,table(mv02_type))
table
prop.table(table)
# table<-with(data_descriptives,table(mv02_type,outcome))
# table
# prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# current_unit
table<-with(data_descriptives,table(current_unit))
table
prop.table(table)
# table<-with(data_descriptives,table(current_unit,outcome))
# table
# prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# los_hospitalization
with(data_descriptives,describe(los_hospitalization))
# with(data_descriptives,describeBy(los_hospitalization,class_crash))
# t-test: # independent 2-group, 2 level IV
with(data_descriptives,t.test(los_hospitalization ~ class_crash))

# LOS general unit
with(data_descriptives,describe(los_gu))
# with(data_descriptives,describeBy(los_gu,class_crash))
# t-test: # independent 2-group, 2 level IV
with(data_descriptives,t.test(los_gu ~ class_crash))

# LOS High dependency
with(data_descriptives,describe(los_hd))
# with(data_descriptives,describeBy(los_hd,class_crash))
# t-test: # independent 2-group, 2 level IV
with(data_descriptives,t.test(los_hd ~ class_crash))

# LOS ICU
with(data_descriptives,describe(los_icu))
# with(data_descriptives,describeBy(los_icu,class_crash))
# t-test: # independent 2-group, 2 level IV
with(data_descriptives,t.test(los_icu ~ class_crash))

# LOS Casualty
with(data_descriptives,describe(los_trauma))
# with(data_descriptives,describeBy(los_trauma,class_crash))
# t-test: # independent 2-group, 2 level IV
with(data_descriptives,t.test(los_trauma ~ class_crash))

# # of patients previously on MV (sedetaion)?
# with(data_descriptives,describe(los_trauma))
# # with(data_descriptives,describeBy(los_trauma,class_crash))
# # t-test: # independent 2-group, 2 level IV
# with(data_descriptives,t.test(los_trauma ~ class_crash))

# days on Mechanical ventilation
with(data_descriptives,describe(los_mv))
# with(data_descriptives,describeBy(los_mv,class_crash))
# t-test: # independent 2-group, 2 level IV
# with(data_descriptives,t.test(los_mv ~ class_crash))

# number of patients sedated during hospitalization
with(data_descriptives,describe(sedation))
table<-with(data_descriptives,table(current_unit))
table
prop.table(table)
# table<-with(data_descriptives,table(current_unit,outcome))
# table
# prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# days on Sedation
with(data_descriptives,describe(los_sedation))
# with(data_descriptives,describeBy(los_mv,class_crash))
# t-test: # independent 2-group, 2 level IV
# with(data_descriptives,t.test(los_mv ~ class_crash))

#OUTCOME ASSOCIATION AND BIVARIATE ANALYSIS
###########################################
#PARAMETRIC
# one sample t-test
t.test(data$IMC,mu=25) # Ho: mu=3

# independent 2-group t-test
t.test(data$IMC~data$Sexo,paired=FALSE) # where y is numeric and x is a binary factor

# paired t-test
IMC2<-data$IMC*2
t.test(data$IMC,IMC2,paired=TRUE) # where y is numeric and x is a binary factor

#NONPARAMETRIC
wilcox.test(data$IMC~data$Sexo,paired=FALSE) # where y is numeric and x is a binary factor

# paired t-test
wilcox.test(data$IMC,IMC2,paired=TRUE) 

######################################################################
#MULTIVARIATE ANALYSIS
######################################################################

# ANALYSIS OF VARIANCE
##################################
# One Way Anova (Completely Randomized Design)
fit <- aov(Idade ~ Classificacao, data=data)
summary(fit)

# Randomized Block Design (B is the blocking factor) 
fit <- aov(Idade ~ Classificacao+Sexo, data=data)
summary(fit)

# Two Way Factorial Design 
fit <- aov(Idade ~ Classificacao*Sexo, data=data)
summary(fit)

# Tukey Honestly Significant Differences
TukeyHSD(fit) # where fit comes from aov()

# Analysis of Covariance 
fit <- aov(Idade ~ Classificacao + IMC, data=data)
summary(fit)

# Kruskal Wallis Test One Way Anova by Ranks 
kruskal.test(Idade ~ Classificacao, data=data) # where y1 is numeric and A is a factor


#CORRELATIONS
##############################
#Pearson
cor(numeric, use="complete.obs", method="pearson") 
#Spearman
cor(numeric, use="complete.obs", method="spearman") 
#Kendall
cor(numeric, use="complete.obs", method="kendall")

#Significance testing
rcorr(as.matrix(numeric), type="pearson") # type can be pearson or spearman

cor.test(numeric$Peso,numeric$Altura) #Used for a single test of significance

# heterogeneous correlations in one matrix 
# pearson (numeric-numeric), 
# polyserial (numeric-ordinal), 
# and polychoric (ordinal-ordinal)
# x is a data frame with ordered factors 
# and numeric variables
hetcor(data) 

# polychoric correlation
# x is a contingency table of counts
polychor(data) 

#GLM
############################################
logmodel<-glm(gcs_cat ~ Anxiety_presence + 
						AGE + 
						SEX + 
						MARSTAT +
                        ATTEMPT_baseline + 
                        Diagnostic
                       ,family=binomial, data=analysis_data)
summary(baselineXFUP3)
#anova(reglogGEU)
#exp(coef(model1_death)) # exponentiated coefficients
#exp(confint(model1_death)) # 95% CI for exponentiated coefficients
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
#logistic.display(baselineXFUP3)

######################################################################
#COMPLEX ANALYSIS AND OTHER FIGURES
######################################################################


######################################################################
#END
######################################################################