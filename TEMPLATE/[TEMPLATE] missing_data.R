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
# #MICE framework for imputation
# # describing the pattern of missingnesss
# md.pattern(data_epi)

# # showing pairs of missingines
# md.pairs(data_epi)

# # plots impact of missing data for a set of pairs - works better for numerical data
# marginplot(data.frame(data_epi$outcome,data_epi$visibility), col = mdc(1:2), cex = 1.2, cex.lab = 1.2, cex.numbers = 1.3, pch = 19)

# generate imputations
# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
imp <- mice(data_epi, seed = 2222, m=50)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
data_imputed<-complete(imp,1)

#Plost the distrbution of each of the 5 possibilities of imputations
# stripplot(imp,pch=20,cex=1.2)

# #plots a scatter plot of pairs of variables
# xyplot(imp, outcome ~ visibility | .imp, pch = 20, cex = 1.4)

# #returns the matrix specifying each variable used to -predict imputation - columns 1=predictor 0=not predictor. rows are the variables of interest
# imp$predictorMatrix
# pred <- imp$predictorMatrix #if you want to exclude  variable from the prediction model for imputation then assign an obect to pred
# pred[, "bmi"] <- 0 #transform the column values into 0's for not predictiong
# imp <- mice(nhanes, pred = pred, pri = FALSE) # rerun the model specifying pred argumento witht eh matriz recoded.
