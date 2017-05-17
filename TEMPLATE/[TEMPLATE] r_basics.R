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
lapply(c("sem","ggplot2", "psych", "irr", "nortest", "moments",
	"GPArotation","nFactors","boot","psy", "car","vcd", "gridExtra",
	"mi","VIM","epicalc","gdata","sqldf","reshape2","mclust",
	"foreign","survival","memisc","foreign","mice","MissMech"), 
library, character.only=T)

######################################################################
#IMPORTING DATA
######################################################################
#LOADING DATA FROM A .CSV FILE
data<-read.csv("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/AMPED/US_AMPEDPain_data.csv",sep=",")
#information between " " are the path to the directory in your computer where the data is stored

######################################################################
#DATA MANAGEMENT
######################################################################
#Creating a data frame (group of variables)
#numeric<-with(data, data.frame(Peso,Altura,IMC,
#                          Idade))
#
##Change variables properties
##Change variable to factor
#data$Classificacao<-as.factor(data$Classificacao)
#
##Change variable to character
#data$Classificacao<-as.character(data$Classificacao)
#
##Change variable to numeric
#data$Classificacao<-as.numeric(data$Classificacao)
#
##Recoding variables
#data$Classificacao<-car::recode(data$Classificacao,"#1='baixo';2='medio';
#	3='alto'")

data <- base::merge(data1,data2,by=c("nome"))

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


######################################################################
#TABLE 1
######################################################################

# Age
with(data_epi,describe(age))
with(data_epi,describeBy(age,class_crash))
# t-test: # independent 2-group, 2 level IV
with(data_epi,t.test(age ~ class_crash))

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
baselineXFUP3<-glm(ATTEMPT_P ~ Anxiety_presence + AGE + SEX + MARSTAT +
                            ATTEMPT_baseline + Diagnostic
                            ,family=binomial, data=FUP3)
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