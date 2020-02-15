######################################################################
#SAMPLE SIZE CALCULATION FOR PREVALENCE STUDIES
######################################################################
#
# #To calculate a sample size for prevalence estimation and adding in a correction for clustering sampling we need to:
#
#
# Calculate a sample size for power for a simple proportion sample.
# Multiple that for whatever we define that is the Design Effect. Let's assume it is 3 for now, as your colleague mentioned.
#
# Reviewing topic #1, the assumptions are:
#
# P = Prevalence
# e = Standard error (precision) = this means we are assuming a standard error of 1/2 (0.013/2=0.006) or 1/5 (0.013/5=0.003) of the estimated prevalence.
# Z = 1.96 which is to give us 95% confidence.
# r = 0.05 - response rate loss of 5%
#
# n=(Zˆ2*P*(P-1)/eˆ2)/1-r
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
# lapply(c("sem","ggplot2", "psych", "irr", "nortest", "moments",
# 	"GPArotation","nFactors","boot","psy", "car","vcd", "gridExtra",
# 	"mi","VIM","epicalc","gdata","sqldf","reshape2","mclust",
# 	"foreign","survival","memisc","foreign","mice","MissMech"), 
# library, character.only=T)

######################################################################
#FORMULA
######################################################################

P = 0.30 #30% of injury
e = 0.02 #2% error margin
Z = 1.96 #which is to give us 95% confidence.
r = 0.05 #response rate loss of 5%

n=(Z^2*P*(P-1)/e^2)/1-r
n


