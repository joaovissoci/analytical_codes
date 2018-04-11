######################################################
#suicide_anxiety.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. see full license at the end of this file.
######################################################
#this script follows a combination of the guidelines proposed by Hadley Wickham http://goo.gl/c04kq as well as using the formatR package http://goo.gl/ri6ky
#if this is the first time you are conducting an analysis using this protocol, please watch http://goo.gl/DajIN while following step by step

#link to manuscript

 #The general plan is to compare the fibrinogen and platelet curves of RS vs Copperhead snakes.  The times points are Baseline, nadir during hospitalization, day 5, day 8, day 15.  There is some missing mess.   I am hoping we can get it done in time for an abstract deadline soon.  Let me know what is best.

######################################################
#SETTING ENVIRONMENT
######################################################
 #install.packages("VIM")
 #install.packages("VIMGUI")
 #install.packages("miP")
 #install.packages("gWidgetsRGtk2")
 #install.packages("mi")
 #install.packages("epicalc")

#Load packages neededz for the analysis
#All packages must be installes with install.packages() function
lapply(c("lavaan","psych","qgraph"),
library, character.only=T)

#Package and codes to pull data from goodle sheets
#devtools::install_github("jennybc/googlesheets")
#library(googlesheets)
#suppressMessages(library(dplyr))

######################################################
#IMPORTING DATA
######################################################

# add the path to you computer between " "
data<-read.csv("/Users/Joao/Desktop/template_factoranaysis_data.csv",sep=',')

##############################################################
#EFA
##############################################################

#ANALISE PARALELA E EIGEN VALUES
#############################################################
#MODEL 1 - Risk due to road deisgn
cor_data<-cor_auto(data)

qgraph(cor_data,layout="spring")

#Function to calculate the KMO values
kmo<-kmo(data)
kmo$overall
kmo$AIR #anti-image matrix

cortest.bartlett(cor_data,n=297,diag=TRUE)

#Scree plot
ev <- eigen(cor_data) # get eigenvalues - insert the data you want to calculate the scree plot for
scree(cor_data)

#Parallel analysis
fa.parallel(data,cor="poly")

#EXPLORATORY FACTOR ANALYSIS
#############################################################
#Functino to exctract the factor loadings. 
#Arguments are DATA, Number of factors, rotation method. 
#Look here http://goo.gl/kY3ln for different met

#holds of estimations or rotations
fa_model<-fa(cor_data,2,fm="uls",rotate="promax")

#based on a polychoric correlation matrix
fa.poly(data,1,fm="uls",rotate="oblimin")

#CONFIRMATORY FACTOR ANALYSIS
#############################################################
#1factor model ###########
audit_model <- '
Audit =~  h1 + h2 + h3 + h4 + h5 + h6 + h7 + h8 + h9 + h10
			 '
fit <- lavaan::cfa(audit_model,
				   data = data,
				   estimator="WLSMV",
				   ordered=names(data))
summary(fit,
		fit.measures=TRUE,
								  standardized = TRUE)
lavaan::fitMeasures(fit,
					fit.measures = c("chisq.scaled",
									 "df.scaled",
									 "pvalue.scaled",
									 "cfi.scaled",
									 "tli.scaled",
									 "rmsea.scaled",
									 "rmsea.ci.lower.scaled",
									 "rmsea.ci.upper.scaled"
									 ))

Est <- lavaan::parameterEstimates(fit,
								  ci = TRUE,
								  standardized = TRUE)

subset(Est, op == "=~")
subset(Est, op == "~~")


### Modification Indexes
Mod <- modificationIndices(fit)
subset(Mod, mi > 10)

#Composite Reliabilty
sum(Est$std.all[1:10])^2/(sum(Est$std.all[1:10])^2+
	sum(Est$std.all[47:56]))

#Average Extracted Variance
sum(Est$std.all[1:10]^2)/length(Est$std.all[1:10])

#Factor scores
audit_overall<-lavaan::predict(fit)


invariance_data<-data.frame(audit_data,gender=data$female)
library(semTools)

invariance_data$h2<-car::recode(invariance_data$h2,"
	2:4=1")

invariance_data$h3<-car::recode(invariance_data$h3,"
	2:4=1")

invariance_data$h5<-car::recode(invariance_data$h5,"
	0:4=1")

audit_model <- '
Audit =~  h1 + h2 + h3 + h4 + h6 + h7 + h8 + h9 + h10'


measurementInvariance(audit_model,
				   data = na.omit(invariance_data[,-5]),
				   estimator="WLSMV",
				   ordered=names(audit_data),
	group="gender")

# 2 factor model ###########
audit_model2 <- '
Audit =~  h1 + h2 + h3
Audit2 =~ h4 + h5 + h6 + h7 + h8 + h9 + h10
'

fit <- lavaan::cfa(audit_model2,
				   data = data,
				   estimator="WLSM",
				   ordered=names(data))
summary(fit,
		fit.measures=TRUE)
lavaan::fitMeasures(fit,
					fit.measures = "all")
parameterEstimates(fit)
Est <- lavaan::parameterEstimates(fit,
								  ci = TRUE,
								  standardized = TRUE)
subset(Est, op == "=~")
subset(Est, op == "~~")

# END ############################################################
