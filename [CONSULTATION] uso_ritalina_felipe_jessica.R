#####################################################################################
#BASIC R STATISTICS TEMPLATE
#####################################################################################
#
#####################################################################################
#SETTING ENVIRONMENT
#####################################################################################
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
lapply(c("sem","ggplot2", "psych", "RCurl", "irr", "nortest", "moments","GPArotation","nFactors","boot","psy", "car","vcd", "gridExtra","mi","VIM","epicalc","gdata","sqldf","reshape2","mclust","foreign","survival","memisc","foreign","mice","qgraph"), library, character.only=T)

########################################################
#IMPORTING DATA
########################################################
#LOADING DATA FROM A .CSV FILE
data<-read.csv("/home/joao/Dropbox/datasets/consultation/felipe_jessica.csv",sep=",")

#Import data from Dropbox, in .csv format
#Instructions here http://goo.gl/Ofa7gQ
#data1 <- repmis::source_DropboxData("pem_parasito.csv","tkxmkg9pybmtsgh",sep = ",",header = TRUE)

########################################################
#DATA MANAGEMENT
########################################################
#Creating a data frame (group of variables)
#numeric<-with(data, data.frame(Peso,Altura,IMC,
#                          Idade))
#data <- base::merge(data1,data2,by=c("nome"))

anxiety_score<-rowSums(with(data,data.frame(Anxiety_1,Anxiety_2,Anxiety_3,Anxiety_4,Anxiety_5,Anxiety_6,Anxiety_7,Anxiety_8,Anxiety_9,Anxiety_10,Anxiety_11,Anxiety_12,Anxiety_13,Anxiety_14,Anxiety_15,Anxiety_16,Anxiety_17,Anxiety_18,Anxiety_19,Anxiety_20,Anxiety_21)))

tdah_score<-rowSums(with(data,data.frame(tdah1,tdah2,tdah3,tdah4,tdah5,tdah6,tdah7,tdah8,tdah9,tdah10,tdah11,tdah12,tdah13,tdah14,tdah15,tdah16,tdah17,tdah18)))

########################################################
#DATA IMPUTATION
########################################################
# generate imputations
# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
#imp <- mice(data, seed = 222222, m=5)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
#data_imputed<-complete(imp,1)

########################################################
#DESCRIPTIVES
########################################################
# Gender
table<-with(data,table(gender))
table
prop.table(table)
#table<-with(data_bea,table(hospitalization,risk_classification))
#table
#prop.table(table,2)
#chisq.test(table)
#fisher.test(table)
#assocstats(table) #vcd package

# Age
summary(data$age)
ad.test(data$age)
#hist(data_imputed$age)
#ci_func(data_imputed$age,.95)
by(data$age,data$rtc_involvement,describe)
wilcox.test(data$age~data$rtc_involvement)
