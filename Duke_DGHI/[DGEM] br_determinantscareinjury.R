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
# install.packages("readstata13")

#PACKAGES LOADING CODE
#Load packages neededz for the analysis
#library(Hmisc)

#All packages must be installes with install.packages() function
lapply(c("readstata13","Hmisc","tidyverse","survey"), 
library, character.only=T)

######################################################################
#IMPORTING DATA
######################################################################
#LOADING DATA FROM A .SATA FILE
raw_data<-read.dta13("/Users/Joao/Box/Home Folder jnv4/Data/Global EM/Brazil/care_seeking_br/care_seeking_br.dta")
#information between " " are the path to the directory in your computer where the data is stored

######################################################################
#DATA MANAGEMENT
######################################################################

#Occupation

raw_data$occupation_recoded<-recode(raw_data$occupation,
							'0' = "Unemployed",
					        '1' = "Student",
					        '2' = "Unemployed",
					        '3' = "Employed",
					        '4' = "Self-employed",
					        '5' = "Self-employed",
					        '6' = "Employed",
					        '7' = "Employed",
					        '8' = "Others")

#Education

raw_data$education_recoded<-recode(raw_data$education,
							'0' = "First Incomplete",
					        '1' = "First Complete",
					        '2' = "First Complete",
					        '3' = "Secondary Complete",
					        '4' = "Secondary Complete",
					        '5' = "College or higher")

#Injury on the face

raw_data %>%
mutate(injuryprev = 
   		case_when(injury_f == 1 |
   				  injury_c == 1 |
   				  injury_b == 1 |
   				  injury_a == 1 |
   				  injury_g == 1 |
   				  injury_e == 1 ~ 1,
   				  TRUE ~ 0)) -> raw_data

raw_data %>%
mutate(injurycombinations = 
   		case_when(injury_f == 1 &
   				  injury_c == 1 &	
   				  injury_b == 1 &
   				  injury_a == 1 &
   				  injury_e == 1 &
   				  injury_g == 1 ~ "all",
   				  injury_f == 1 &	
   				  injury_a == 1 &
   				  injury_g == 1 &
   				  injury_e == 1 ~ "face, abdomin, groin anbd extremities",
   				  injury_f == 1 &
   				  injury_c == 1 &	
   				  injury_b == 1 &
   				  injury_a == 1 &
   				  injury_g == 1 ~ "face, chect, back, abdomin and groin",
   				  injury_f == 1 &
   				  injury_c == 1 &	
   				  injury_b == 1 &
   				  injury_a == 1 &
   				  injury_e == 1 ~ "face, chect, back, abdomin and extremities",
   				  injury_f == 1 &
   				  injury_e == 1 &	
   				  injury_b == 1 &
   				  injury_a == 1 &
   				  injury_g == 1 ~ "face, back, abdomin, extremities and groin",
   				  injury_f == 1 &	
   				  injury_c == 1 &
   				  injury_b == 1 &
   				  injury_e == 1 ~ "face, chest, back and extremities",
   				  injury_f == 1 &	
   				  injury_c == 1 &
   				  injury_a == 1 &
   				  injury_g == 1 ~ "face, chest, abdomin and groin",
   				  injury_f == 1 &	
   				  injury_c == 1 &
   				  injury_a == 1 &
   				  injury_e == 1 ~ "face, chest, abdomin and groin",
   				  injury_f == 1 &	
   				  injury_c == 1 &
   				  injury_g == 1 &
   				  injury_e == 1 ~ "face, chest, groin and extremities",
   				  injury_f == 1 &	
   				  injury_b == 1 &
   				  injury_a == 1 &
   				  injury_g == 1 ~ "face, back, abdomin and groin",
   				  injury_f == 1 &	
   				  injury_b == 1 &
   				  injury_a == 1 &
   				  injury_e == 1 ~ "face, back, abdomin and extremities",
   				  injury_f == 1 &	
   				  injury_b == 1 &
   				  injury_e == 1 &
   				  injury_g == 1 ~ "face, back, extremities and groin",
   				  injury_f == 1 &
   				  injury_a == 1 &
   				  injury_e == 1 ~ "face, abdomin and extremities",
   				  injury_f == 1 &
   				  injury_g == 1 &
   				  injury_e == 1 ~ "face, groin and extremities",
   				  injury_f == 1 &	
   				  injury_c == 1 &
   				  injury_b == 1 &
   				  injury_a == 1 ~ "face, chest, back and abdomin",
   				  injury_f == 1 &	
   				  injury_c == 1 &
   				  injury_b == 1 &
   				  injury_g == 1 ~ "face, chest, back and groin",
   				  injury_f == 1 &
   				  injury_c == 1 ~ "face and chest",
   				  injury_f == 1 &
   				  injury_b == 1 ~ "face and back",
   				  injury_f == 1 &
   				  injury_a == 1 ~ "face and abdomin",
   				  injury_f == 1 &
   				  injury_g == 1 ~ "face and groin",
   				  injury_f == 1 &
   				  injury_e == 1 ~ "face and extremities",
   				  injury_c == 1 &
   				  injury_b == 1 ~ "chest and back",
   				  injury_c == 1 &
   				  injury_a == 1 ~ "chest and abdomin",
   				  injury_c == 1 &
   				  injury_g == 1 ~ "chest and groin",
   				  injury_c == 1 &
   				  injury_e == 1 ~ "chest and extremities",
   				  injury_b == 1 &
   				  injury_a == 1 ~ "back and abdomin",
   				  injury_b == 1 &
   				  injury_g == 1 ~ "back and groin",
   				  injury_b == 1 &
   				  injury_e == 1 ~ "back and extremities",
   				  injury_a == 1 &
   				  injury_g == 1 ~ "abdomin and groin",
   				  injury_a == 1 &
   				  injury_e == 1 ~ "abdomin and extremities",
   				  injury_g == 1 &
   				  injury_e == 1 ~ "groin and extremities",
   				  injury_f == 1 &
   				  injury_c == 1 &
   				  injury_b == 1 ~ "face, chest and back",
   				  injury_f == 1 &
   				  injury_c == 1 &
   				  injury_a == 1 ~ "face, chest and abdomin",
   				  injury_f == 1 &
   				  injury_c == 1 &
   				  injury_g == 1 ~ "face, chest and groin",
   				  injury_f == 1 &
   				  injury_c == 1 &
   				  injury_e == 1 ~ "face, chest and extremities",
   				  injury_f == 1 &
   				  injury_b == 1 &
   				  injury_a == 1 ~ "faceb, back and abdomin",
   				  injury_f == 1 &
   				  injury_b == 1 &
   				  injury_g == 1 ~ "face, back and groin",
   				  injury_f == 1 &
   				  injury_b == 1 &
   				  injury_e == 1 ~ "face, back and extremities",
   				  injury_f == 1 &
   				  injury_a == 1 &
   				  injury_g == 1 ~ "face, abdomin and groin",
   				  injury_f == 1 ~ "face",
   				  injury_c == 1 ~ "chest",
   				  injury_b == 1 ~ "back",
   				  injury_a == 1 ~ "abdomin",
   				  injury_g == 1 ~ "groin",
   				  injury_e == 1 ~ "extremities")) -> raw_data

raw_data %>%
mutate(injurycategorical = 
   		case_when(injury_f == 1 &
   				  injury_c == 1 &	
   				  injury_b == 1 &
   				  injury_a == 1 &
   				  injury_e == 1 &
   				  injury_g == 1 ~ "multiple",
   				  injury_f == 1 &	
   				  injury_a == 1 &
   				  injury_g == 1 &
   				  injury_e == 1 ~ "multiple",
   				  injury_f == 1 &
   				  injury_c == 1 &	
   				  injury_b == 1 &
   				  injury_a == 1 &
   				  injury_g == 1 ~ "multiple",
   				  injury_f == 1 &
   				  injury_c == 1 &	
   				  injury_b == 1 &
   				  injury_a == 1 &
   				  injury_e == 1 ~ "multiple",
   				  injury_f == 1 &
   				  injury_e == 1 &	
   				  injury_b == 1 &
   				  injury_a == 1 &
   				  injury_g == 1 ~ "multiple",
   				  injury_f == 1 &	
   				  injury_c == 1 &
   				  injury_b == 1 &
   				  injury_e == 1 ~ "multiple",
   				  injury_f == 1 &	
   				  injury_c == 1 &
   				  injury_a == 1 &
   				  injury_g == 1 ~ "multiple",
   				  injury_f == 1 &	
   				  injury_c == 1 &
   				  injury_a == 1 &
   				  injury_e == 1 ~ "multiple",
   				  injury_f == 1 &	
   				  injury_c == 1 &
   				  injury_g == 1 &
   				  injury_e == 1 ~ "multiple",
   				  injury_f == 1 &	
   				  injury_b == 1 &
   				  injury_a == 1 &
   				  injury_g == 1 ~ "multiple",
   				  injury_f == 1 &	
   				  injury_b == 1 &
   				  injury_a == 1 &
   				  injury_e == 1 ~ "multiple",
   				  injury_f == 1 &	
   				  injury_b == 1 &
   				  injury_e == 1 &
   				  injury_g == 1 ~ "multiple",
   				  injury_f == 1 &
   				  injury_a == 1 &
   				  injury_e == 1 ~ "multiple",
   				  injury_f == 1 &
   				  injury_g == 1 &
   				  injury_e == 1 ~ "multiple",
   				  injury_f == 1 &	
   				  injury_c == 1 &
   				  injury_b == 1 &
   				  injury_a == 1 ~ "multiple",
   				  injury_f == 1 &	
   				  injury_c == 1 &
   				  injury_b == 1 &
   				  injury_g == 1 ~ "multiple",
   				  injury_f == 1 &
   				  injury_c == 1 ~ "multiple",
   				  injury_f == 1 &
   				  injury_b == 1 ~ "multiple",
   				  injury_f == 1 &
   				  injury_a == 1 ~ "multiple",
   				  injury_f == 1 &
   				  injury_g == 1 ~ "multiple",
   				  injury_f == 1 &
   				  injury_e == 1 ~ "multiple",
   				  injury_c == 1 &
   				  injury_b == 1 ~ "multiple",
   				  injury_c == 1 &
   				  injury_a == 1 ~ "multiple",
   				  injury_c == 1 &
   				  injury_g == 1 ~ "multiple",
   				  injury_c == 1 &
   				  injury_e == 1 ~ "multiple",
   				  injury_b == 1 &
   				  injury_a == 1 ~ "multiple",
   				  injury_b == 1 &
   				  injury_g == 1 ~ "multiple",
   				  injury_b == 1 &
   				  injury_e == 1 ~ "multiple",
   				  injury_a == 1 &
   				  injury_g == 1 ~ "multiple",
   				  injury_a == 1 &
   				  injury_e == 1 ~ "multiple",
   				  injury_g == 1 &
   				  injury_e == 1 ~ "multiple",
   				  injury_f == 1 &
   				  injury_c == 1 &
   				  injury_b == 1 ~ "multiple",
   				  injury_f == 1 &
   				  injury_c == 1 &
   				  injury_a == 1 ~ "multiple",
   				  injury_f == 1 &
   				  injury_c == 1 &
   				  injury_g == 1 ~ "multiple",
   				  injury_f == 1 &
   				  injury_c == 1 &
   				  injury_e == 1 ~ "multiple",
   				  injury_f == 1 &
   				  injury_b == 1 &
   				  injury_a == 1 ~ "multiple",
   				  injury_f == 1 &
   				  injury_b == 1 &
   				  injury_g == 1 ~ "multiple",
   				  injury_f == 1 &
   				  injury_b == 1 &
   				  injury_e == 1 ~ "multiple",
   				  injury_f == 1 &
   				  injury_a == 1 &
   				  injury_g == 1 ~ "multiple",
   				  injury_f == 1 ~ "face",
   				  injury_c == 1 ~ "chest",
   				  injury_b == 1 ~ "back",
   				  injury_a == 1 ~ "abdomin",
   				  injury_g == 1 ~ "groin",
   				  injury_e == 1 ~ "extremities")) -> raw_data

raw_data$injurycategorical<-car::recode(raw_data$injurycategorical,"
					NA='no injury'")
raw_data$injurycategorical<-as.factor(raw_data$injurycategorical)

raw_data$injurynumerical<-as.numeric(raw_data$injurycategorical)

# raw_data %>%
# 	mutate(moi=ifelse(injurynumerical==1,
# 					typinjury_a,
# 		   ifelse(injurynumerical==2,
# 					typinjury_b,
# 		   ifelse(injurynumerical==3,
# 		   			typinjury_c,
# 		   ifelse(injurynumerical==4,
# 		   			typinjury_e,
# 		   ifelse(injurynumerical==5,
# 		   			typinjury_f,
# 		   ifelse(injurynumerical==6,
# 		   			typinjury_g,
# 		   ifelse(injurynumerical==7,
# 		   			89,
# 		   ifelse(injurynumerical==8,
# 		   			99,NA))))))))) ->raw_data
# time<-NULL

#CONSENT

consented_data<-subset(raw_data,raw_data$consent_adult==1 |
								raw_data$consent_parent==1 | 
								raw_data$assent ==1)

study_data<-with(consented_data,data.frame(age,
										   gender,
										   education_recoded,
										   occupation_recoded,
										   race,
										   year_visits,
										   monthly_income,
										   insurance,
										   record_id,
										   injury_f,
										   injury_c,
										   injury_b,
										   injury_a,
										   injury_g,
										   injury_e,
										   typinjury_f,
										   typinjury_c,
										   typinjury_b,
										   typinjury_a,
										   typinjury_g,
										   typinjury_e,
										   injuryprev
										   ))


# Building INJURY LEVEL dataset



injury_level_data<-study_data %>%
				  # as.tibble() %>%
				  gather(key, value, -c("age",
										  "gender",
										  "education_recoded",
										  "occupation_recoded",
										  "race",
										  "year_visits",
										  "monthly_income",
										  "insurance",
										  "record_id",
										  "injuryprev")) %>%
  				  extract(key, c("injury_var", "injury_area"), 
  				  			regex="([a-z]+)\\_(.)") %>%
  				  spread(injury_var, value)

head(injury_level_data)				


#

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

#######################################################
#PRE PROCESSING
#######################################################

#CALCULATING SAMPLE WEIGHTS w/ "survey package"
#creating a survey object
data.svy.unweighted <- svydesign(ids=~1, data=data)

#getting the marginal probabilities for the variables we want to
#weight from. 
# marginal probabilities are the population probabilities

gender.dist <- data.frame(gender = c("1", "2"),
                       Freq = nrow(data) * c(0.45, 0.55))

#using the rake function to weight the current data by the population values

data.svy.rake <- rake(design = data.svy.unweighted,
                   sample.margins = list(~gender),
                   population.margins = list(gender.dist))

#Check weights distribution
summary(weights(small.svy.rake))

#trim margins to reduce large or low values
data.svy.rake.trim <- trimWeights(data.svy.rake, lower=0.3, upper=3,
                                  strict=TRUE)

#creating weight object
survey_weights<-weights(small.svy.rake)

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