######################################################################
#TUTORIAL TO CREATE POST STRATUM SAMPLE WEIGHTS FOR SURVEY DESIGNS
######################################################################
#
#This example will demonstrate how to create a weighted dataset after a 
#survey has been administered. By a weighted dataset, we mean a dataset 
#that may have some nonresponse for certain demographics therefore may not 
#be representative of the population of interest. Therefore, 
#when population statistics are available, we can use them to adjust 
#our data to match those of the population.
#
#Tutorials:
# https://rstudio-pubs-static.s3.amazonaws.com/268281_cc370bbbbbfb437b8650b22d208734d1.html
# https://www.r-bloggers.com/survey-computing-your-own-post-stratification-weights-in-r/
# https://bookdown.org/jespasareig/Book_How_to_weight_a_survey/
#
# Questions: 
#
# What are the weights?
#
# Why are they important?
#
# Post vs. Pre strata
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
lapply(c("survey"), 
library, character.only=T)

######################################################################
#DATA MANAGEMENT
######################################################################

#building the sample dataset
set.seed(12345)
preYear = c(0:100)
preYear = sample(preYear, 100, replace = TRUE)

income = c(0:100000)
income = sample(income, 100, replace = TRUE)

gender = c("Male", "Female")
gender = sample(gender, 100, replace = TRUE)
gender = as.numeric(factor(gender))

ethnicity = c("White", "African_American", "Mixed_Ethnicity", "Other_Ethnicity")
ethnicity = sample(ethnicity, 100, replace = TRUE)
ethnicity = as.numeric(factor(ethnicity))

postYear = preYear + 10

data = cbind(preYear, income, gender, ethnicity, postYear)
data = as.data.frame(data)


######################################################################
#Creating a spost stratum survey weights
######################################################################

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

# example with more than 1 variable

small.svy.rake <- rake(design = small.svy.unweighted,
                   sample.margins = list(~sex, ~edu),
                   population.margins = list(sex.dist, edu.dist))

#trim margins to reduce large or low values
data.svy.rake.trim <- trimWeights(data.svy.rake, lower=0.3, upper=3,
                                  strict=TRUE)



















