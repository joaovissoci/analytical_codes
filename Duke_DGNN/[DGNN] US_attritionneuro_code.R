######################################################################
#NEUROSURGERY RESIDENTS ATTRITION 
######################################################################
#
# US neurosurgery residents data
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
lapply(c("sem","ggplot2", "psych", "irr", "nortest", "moments",
	"GPArotation","nFactors","boot","psy", "car","vcd", "gridExtra",
	"mi","VIM","epicalc","gdata","sqldf","reshape2","mclust",
	"foreign","survival","memisc","foreign","mice","MissMech","lubridate"), 
library, character.only=T)

######################################################################
#IMPORTING DATA
######################################################################
#LOADING DATA FROM A .CSV FILE
data<-read.csv("/Users/Joao/Desktop/US_attritionneuro_data.csv",sep=",")
#information between " " are the path to the directory in your computer where the data is stored

######################################################################
#DATA MANAGEMENT
######################################################################
colnames(data)<-c("last_name",
				  "diplay_name",
				  "country",
				  "subclass",
				  "status",
				  "abns",
				  "affiliation",
				  "affiliation_country",
				  "start",
				  "end",
				  "employee_title",
				  "gender",
				  "date_of_birth",
				  "deceased")

#recoding date data
data$start_recoded<-as.POSIXct(data$start,
                      format='%m/%d/%Y')

year(data$start_recoded)

data$end_recoded<-as.POSIXct(data$end,
                      format='%m/%d/%Y')

data$date_of_birth_recoded<-as.POSIXct(data$date_of_birth,
                      format='%m/%d/%Y')

#SUBSET ONLY RESIDENTS
data_residents <- subset(data,data$employee_title=="Resident")

#

######################################################################
#QUESTIONS
######################################################################

# ENTER ONLY AFTER 2000
  # KEEP ONLY THE US BASED SCHOOLS
    # CLEAN UP DIFFERENT NAMES FOR EMPLOYEE STATUS
      # KEEP ONLY THE RESIDENTS BUT GET THEIR SCHOOL

#3. How to recode the Subclass variable
#4. What do you mean by US trained only?

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
#stripplot(imp,pch=20,cex=1.2)

#plots a scatter plot of pairs of variables
#xyplot(imp, outcome ~ visibility | .imp, pch = 20, cex = 1.4)

#returns the matrix specifying each variable used to -predict imputation - columns 1=predictor 0=not predictor. rows are the variables of interest
#imp$predictorMatrix
#pred <- imp$predictorMatrix #if you want to exclude  variable from the prediction model for imputation then assign an obect to pred
#pred[, "bmi"] <- 0 #transform the column values into 0's for not predictiong
#imp <- mice(nhanes, pred = pred, pri = FALSE) # rerun the model specifying pred argumento witht eh matriz recoded.

######################################################################
#BASIC DESCRIPTIVES and EXPLORATORY ANALYSIS
######################################################################


###################################################
#Latent class analysis
###################################################

## Questions
# http://rfunction.com/archives/1499
# https://drive.google.com/open?id=0B4TReYGK49h_X09ZYno1OG5aUVk

# Correlation

# Building the formula

safety_perceptions<-with(numeric,data.frame(helmet_mc,
    belt_driver,
    belt_passenger,
    stop_fast,
    road_wrongside,
    vehicle_home))

#To specify a latent class model, poLCA uses the standard, symbolic R model formula expres- sion. The response variables are the manifest variables of the model. Because latent class models have multiple manifest variables, these variables must be “bound” as cbind(Y1, Y2, Y3, ...) in the model formula. For the basic latent class model with no covariates, the formula definition takes the form

# ses_data_cat<-sapply(safety_perceptions,function(x) as.factor(x))
# ses_data_cat<-as.data.frame(ses_data_cat)
# ses_data_cat2<-sapply(ses_data_cat,function(x) as.numeric(x))
# ses_data_cat2<-as.data.frame(ses_data_cat2)

f <- cbind(helmet_mc,
           belt_driver,
           belt_passenger,
           stop_fast,
           road_wrongside) ~ 1

# The ~ 1 instructs poLCA to estimate the basic latent class model. For the latent class regres- sion model, replace the ~ 1 with the desired function of covariates, as, for example:
# f <- cbind(Y1, Y2, Y3) ~ X1 + X2 * X3

# To estimate the specified latent class model, the default poLCA command is:
# poLCA(formula, data, ncl pass = 2, maxiter = 1000, graphs = FALSE,
#     tol = 1e-10, na.rm = TRUE, probs.start = NULL, nrep = 1,
#     verbose = TRUE, calc.se = TRUE)

#========================================================= 
# Fit for 3 latent classes: 
#========================================================= 
set.seed(198833333333333333333)

# install.packages("poLCA")
# library(poLCA)

# ses_data<-na.omit(ses_data)
lcamodel <- poLCA(f, safety_perceptions, nclass = 4)

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

radialplot_data<-as.data.frame(lcamodel$probs)[,c(1,3,5,7,9)]

rownames(radialplot_data)<-c("Driving poor","Overall low","Overall 
  good","Personal good")

radialplot_data %>%
     rownames_to_column( var = "group" ) -> radialplot_data2

axis_lables<-c("Helmet use",
               "Seat belt use\n as driver",
               "Seat belt use\n as passenger",
               "Believe drivers\n stop fast",
               "Believe drivers\n use wrong road\n side")

ggradar(radialplot_data2,
        font.radar="sans",
         grid.label.size=7,
         axis.label.size=5,
         axis.labels=axis_lables,
         legend.text.size=10) 

## OR table ##
numeric$classes<-lcamodel$predclass
numeric$classes_recoded<-car::recode(
  numeric$classes,"1='Driver poor';
                   2='AOverall low';
                   3='Overall good';
                   4='Personal good'")
#nearmiss
crashlifetime <-glm(as.factor(crash_lifetime) ~ 
                  # age + 
                  # time_motodr +                  
                    # day_wkvehicle +
     #              # time_start_wk + 
     #              # time_stop_wk + 
                  # time_motodr + 
                  # hairnets_available +
                  classes_recoded,
                  family=binomial, data=numeric)
summary(crashlifetime)
odds_model_1<-exp(cbind(Odds=coef(crashlifetime),
                confint(crashlifetime,level=0.95))) 
colnames(odds_model_1)<-c("OR","LowCI","HighCI")



######################################################################
#END
######################################################################