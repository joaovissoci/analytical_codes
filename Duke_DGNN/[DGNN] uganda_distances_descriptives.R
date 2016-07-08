#####################################################################################
#BASIC R STATISTICS TEMPLATE
#####################################################################################
#
#
#
#
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
lapply(c("sem","ggplot2", "psych", "irr", "nortest", 
	"moments","GPArotation","nFactors","boot","psy", 
	"car","vcd", "gridExtra","mi","VIM","epicalc","gdata",
	"sqldf","reshape2","mclust","foreign","survival","memisc",
	"lme4","lmerTest","dplyr","PMCMR"),library, character.only=T)

#####################################################################################
#IMPORTING DATA
#####################################################################################
#LOADING DATA FROM A .CSV FILE
#data<-read.csv("/Users/rpietro/Desktop/MDD_BIPD_Baseline.csv",sep=",")
#information between " " are the path to the directory in your computer where the data is stored

#Import data from Dropbox, in .csv format
#Instructions here http://goo.gl/Ofa7gQ
#data1 <- repmis::source_DropboxData("pem_parasito.csv",
#                                  "tkxmkg9pybmtsgh",
#                                  sep = ",",
#                                  header = TRUE)

data<-read.csv("/Users/jnv4/OneDrive - Duke University/datasets/DGNN/SOSAS/SOSAS_gis/districts_gis_uganda.csv",sep=',')

#data_hub<-read.csv("/home/joao/Desktop/hubdistance_neuro.csv",sep=',')

#############################################################################
#DATA MANAGEMENT
#############################################################################

# data <- na.omit(data)

# data_merge<-melt(with(data_hub,data.frame(tot_prop,District),id="District"))
# data_cast<-dcast(data_merge,District~variable,mean)
# write.csv(data_cast,"/home/joao/Desktop/deleteme2.csv")
#####################################################################################
#BASIC DESCRIPTIVES and EXPLORATORY ANALYSIS
#####################################################################################

summary(data)

with(data,by(POP_TOTAL,region,ad.test))

with(data,by(POP_TOTAL,region,sum))/sum(data$POP_TOTAL)

with(data,by(URBAN_POP,region,ad.test))
with(data,by(URBAN_POP,region,summary))

with(data,by(RURAL_POP,region,ad.test))
with(data,by(RURAL_POP,region,ad.test))

with(data,by(USN_district,region,summary))
with(data,by(area_district,region,summary))
with(data,by(area_district,region,ad.test))
with(data,by(area_district,region,sd))

with(data,by(distance_district/1000,region,summary))
with(data,by(distance_district,region,ad.test))
with(data,by(distance_district,region,sd))

with(data,by(access_district*100000,region,summary))
with(data,by(access_district,region,ad.test))
with(data,by(access_district,region,sd))

#####################################################################################
#ANALYSIS OF VARIANCE
#####################################################################################
# One Way Anova (Completely Randomized Design)
# Kruskal Wallis Test One Way Anova by Ranks 
kruskal.test(USN_district ~ region, data=data) # where y1 is numeric and A is a factor
#post hoc
with(data,posthoc.kruskal.nemenyi.test(x=USN_district, g=region, method="Tukey"))

# Kruskal Wallis Test One Way Anova by Ranks 
kruskal.test(distance_district ~ region, data=data) # where y1 is numeric and A is a factor
#post hoc
with(data,posthoc.kruskal.nemenyi.test(x=distance_district, g=region, method="Tukey"))

# One Way Anova (Completely Randomized Design)
# Kruskal Wallis Test One Way Anova by Ranks 
kruskal.test(area_district ~ region, data=data) # where y1 is numeric and A is a factor
#post hoc
with(data,posthoc.kruskal.nemenyi.test(x=area_district, g=region, method="Tukey"))

# One Way Anova (Completely Randomized Design)
# Kruskal Wallis Test One Way Anova by Ranks 
kruskal.test(time_district ~ region, data=data) # where y1 is numeric and A is a factor
#post hoc
with(data,posthoc.kruskal.nemenyi.test(x=time_district, g=region, method="Tukey"))

# One Way Anova (Completely Randomized Design)
# Kruskal Wallis Test One Way Anova by Ranks 
kruskal.test(access_district ~ region, data=data) # where y1 is numeric and A is a factor
#post hoc
with(data,posthoc.kruskal.nemenyi.test(x=access_district, g=region, method="Tukey"))

#####################################################################################
#CORRELATIONS
#####################################################################################
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

#####################################################################################
#LOGISTIC REGRESSION
#####################################################################################
baselineXFUP3<-glm(ATTEMPT_P ~ Anxiety_presence + AGE + SEX + MARSTAT +
                            ATTEMPT_baseline + Diagnostic
                            ,family=binomial, data=FUP3)
summary(baselineXFUP3)
#anova(reglogGEU)
#exp(coef(model1_death)) # exponentiated coefficients
#exp(confint(model1_death)) # 95% CI for exponentiated coefficients
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
#logistic.display(baselineXFUP3)