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
data<-read.csv("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/Africa/Tz/BNI/tz_bniscalevalidation.csv",sep=',')

##############################################################
#DATA MANAGEMENT
##############################################################

names(data)<-c(
				"focus_group",
				"counselot",
				"group",
				"participant",
				"action_1",
				"action_2",
				"action_3",
				"action_4",
				"action_5",
				"action_6",
				"action_7",
				"action_8",
				"action_9",
				"action_10",
				"action_11",
				"action_12",
				"action_13",
				"action_14",
				"action_15",
				"action_16",
				"action_17",
				"action_18",
				"action_19",
				"action_20",
				"action_21",
				"action_22"
				)

NAto99<-function(x){
	car::recode(x,"99=NA")
	}

data_temp<-lapply(data,NAto99)

data<-as.data.frame(data_temp)

question_data<-data[5:26]


# generate imputations
# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
question_data_imp <- mice(question_data, seed = 2222, m=5)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
question_data_imputed<-mice::complete(question_data_imp,3)

question_data_imputed$action_9<-car::recode(question_data_imputed$action_9,"0=1;1=0")
question_data_imputed$action_11<-car::recode(question_data_imputed$action_11,"0=1;1=0")
question_data_imputed$action_18<-car::recode(question_data_imputed$action_18,"0=1;1=0")

##############################################################
#FACTOR ANALYSIS
##############################################################

#ANALISE PARALELA E EIGEN VALUES
#############################################################

efa_data<-question_data_imputed[,-c(9,11,18)]

#MODEL 1 - Risk due to road deisgn
cor_data<-cor_auto(efa_data)

qgraph(cor_data,layout="spring")

#Function to calculate the KMO values
kmo<-kmo(question_data_imputed)
kmo$overall
kmo$AIR #anti-image matrix

cortest.bartlett(question_data_imputed,n=297,diag=TRUE)

#Scree plot
ev <- eigen(cor_data) # get eigenvalues - insert the data you want to calculate the scree plot for
scree(cor_data)

#Parallel analysis
fa.parallel(question_data_imputed,cor="poly")

#EXPLORATORY FACTOR ANALYSIS
#############################################################
#Functino to exctract the factor loadings. 
#Arguments are DATA, Number of factors, rotation method. 
#Look here http://goo.gl/kY3ln for different met

#holds of estimations or rotations
fa_model_1<-fa(cor_data,1,fm="uls",rotate="oblimin")
fa_model_2<-fa(cor_data,2,fm="uls",rotate="oblimin")
fa_model_3<-fa(cor_data,3,fm="uls",rotate="oblimin")
# fa_model_4<-fa(cor_data,4,fm="uls",rotate="oblimin")
# fa_model_5<-fa(cor_data,5,fm="uls",rotate="oblimin")
#
#based on a polychoric correlation matrix
# fa.poly(question_data_imputed,1,fm="uls",rotate="oblimin")

#CONFIRMATORY FACTOR ANALYSIS
#############################################################
#1 factor + method factor model ###########
bni_model <- '
BNI1 =~  action_1 + 
		 action_2 + 
		 action_3 + 
		 action_4 + 
		 action_5 + 
		 action_6 + 
		 action_7 + 
		 action_8 + 
		 action_9 + 
		 action_10 + 
		 action_11 + 
		 action_12 + 
		 action_13 + 
		 action_14 + 
		 action_15 + 
		 action_16 + 
		 action_17 + 
		 action_18 + 
		 action_19 +
		 action_20 + 
		 action_21 +
		 action_22 

# Reversed =~ action_9 + 
# 		    action_11 + 
# 		    action_18

action_9 ~~ action_18		 
			 '
			 
fit <- lavaan::cfa(bni_model,
				   data = question_data_imputed,
				   estimator="WLSMV",
				   ordered=names(question_data_imputed))
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

summary(Est$std.all[1:17])

#Composite Reliabilty
sum(Est$std.all[1:17])^2/(sum(Est$std.all[1:17])^2+
	sum(Est$std.all[35:51]))

#Average Extracted Variance
sum(Est$std.all[1:17]^2)/length(Est$std.all[1:17])

# #Factor scores
# audit_overall<-lavaan::predict(fit)

#KMO e CRONBACH's ALPHA
bni_reliability<-with(question_data_imputed,
	data.frame(action_1, 
		 action_2, 
		 action_3, 
		 action_4, 
		 action_5, 
		 action_6, 
		 action_7, 
		 action_8, 
		 action_9, 
		 action_10, 
		 action_11, 
		 action_12, 
		 action_13, 
		 action_14, 
		 action_15, 
		 action_16, 
		 action_17, 
		 action_18, 
		 action_19,
		 action_20, 
		 action_21,
		 action_22))

#CRONBACH's ALPHA
psych::alpha(bni_reliability,n.iter=1000,check.keys=TRUE)

#Function to calculate the KMO values
kmo<-kmo(bni_reliability)
kmo$overall

# invariance_data<-data.frame(audit_data,gender=data$female)
# library(semTools)

# invariance_data$h2<-car::recode(invariance_data$h2,"
# 	2:4=1")

# invariance_data$h3<-car::recode(invariance_data$h3,"
# 	2:4=1")

# invariance_data$h5<-car::recode(invariance_data$h5,"
# 	0:4=1")

# audit_model <- '
# Audit =~  h1 + h2 + h3 + h4 + h6 + h7 + h8 + h9 + h10'


# measurementInvariance(audit_model,
# 				   data = na.omit(invariance_data[,-5]),
# 				   estimator="WLSMV",
# 				   ordered=names(audit_data),
# 	group="gender")

# 2 factor model ###########

bni_model <- '
BNI1 =~  action_1 + 
		 action_2 + 
		 action_3 + 
		 action_4 + 
		 action_5 + 
		 action_6 + 
		 action_7 + 
		 action_8 + 
		 action_10 + 
		 action_12 + 
		 action_13 + 
		 action_14 + 
		 action_17 + 
		 action_19
		 # action_22

# BNI2 =~
# 		 action_8 + 
# 		 action_11 + 
# 		 action_18


BNI3 =~ 
		 action_15 + 
		 action_16 + 
		 action_20
		 # action_21

# action_9 ~~ action_18
			 '

fit <- lavaan::cfa(bni_model,
				   data = question_data_imputed,
				   estimator="WLSMV",
				   ordered=names(question_data_imputed))
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

summary(Est$std.all[1:13])

summary(Est$std.all[14:17])

#Composite Reliabilty - BNI 1
sum(Est$std.all[1:13])^2/(sum(Est$std.all[1:13])^2+
	sum(Est$std.all[35:47]))

#Average Extracted Variance - BNI 1
sum(Est$std.all[1:13]^2)/length(Est$std.all[1:13])

#Composite Reliabilty - BNI 2
sum(Est$std.all[14:17])^2/(sum(Est$std.all[14:17])^2+
	sum(Est$std.all[48:51]))

#Average Extracted Variance - BNI 2
sum(Est$std.all[14:17]^2)/length(Est$std.all[14:17])

#Factor scores
bni_overall<-lavaan::predict(fit)

data$bni_factor1<-bni_overall[,1]
data$bni_factor1<-scales::rescale(
	data$bni_factor1,to = c(0, 100))

data$bni_factor2<-bni_overall[,2]
data$bni_factor2<-scales::rescale(
	data$bni_factor2,to = c(0, 100))

#KMO e CRONBACH's ALPHA
bni_reliability_bni1<-with(question_data_imputed,
	data.frame(action_1, 
		 action_2, 
		 action_3, 
		 action_4, 
		 action_5, 
		 action_6, 
		 action_7, 
		 action_8, 
		 action_9, 
		 action_10, 
		 action_11, 
		 action_12, 
		 action_13, 
		 action_14, 
		 action_17, 
		 action_18, 
		 action_19,
		 action_22))

bni_reliability_bni2<-with(question_data_imputed,
	data.frame(
		 action_15, 
		 action_16, 
		 action_20, 
		 action_21))

#CRONBACH's ALPHA
psych::alpha(bni_reliability_bni1,n.iter=1000,check.keys=TRUE)

#Function to calculate the KMO values
kmo<-kmo(bni_reliability_bni1)
kmo$overall

#CRONBACH's ALPHA
psych::alpha(bni_reliability_bni2,n.iter=1000,check.keys=TRUE)

#Function to calculate the KMO values
kmo<-kmo(bni_reliability_bni2)
kmo$overall

# 2 factor model = to the paper

bni_model <- '
BNI1 =~  action_1 + 
		 action_3 + 
		 action_8 + 
		 action_13 + 


# BNI2 =~
# 		 action_8 + 
# 		 action_11 + 
# 		 action_18


BNI3 =~ 
		 action_7 + 
		 action_14 + 
		 action_17 + 
		 action_21

# action_9 ~~ action_18
			 '
fit <- lavaan::cfa(bni_model,
				   data = question_data_imputed,
				   estimator="WLSMV",
				   ordered=names(question_data_imputed))
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

summary(Est$std.all[1:13])

summary(Est$std.all[14:17])

#Composite Reliabilty - BNI 1
sum(Est$std.all[1:13])^2/(sum(Est$std.all[1:13])^2+
	sum(Est$std.all[35:47]))

#Average Extracted Variance - BNI 1
sum(Est$std.all[1:13]^2)/length(Est$std.all[1:13])

#Composite Reliabilty - BNI 2
sum(Est$std.all[14:17])^2/(sum(Est$std.all[14:17])^2+
	sum(Est$std.all[48:51]))

#Average Extracted Variance - BNI 2
sum(Est$std.all[14:17]^2)/length(Est$std.all[14:17])

#Factor scores
bni_overall<-lavaan::predict(fit)

data$bni_factor1<-bni_overall[,1]
data$bni_factor1<-scales::rescale(
	data$bni_factor1,to = c(0, 100))

data$bni_factor2<-bni_overall[,2]
data$bni_factor2<-scales::rescale(
	data$bni_factor2,to = c(0, 100))

#KMO e CRONBACH's ALPHA
bni_reliability_bni1<-with(question_data_imputed,
	data.frame(action_1, 
		 action_2, 
		 action_3, 
		 action_4, 
		 action_5, 
		 action_6, 
		 action_7, 
		 action_8, 
		 action_9, 
		 action_10, 
		 action_11, 
		 action_12, 
		 action_13, 
		 action_14, 
		 action_17, 
		 action_18, 
		 action_19,
		 action_22))

bni_reliability_bni2<-with(question_data_imputed,
	data.frame(
		 action_15, 
		 action_16, 
		 action_20, 
		 action_21))
##############################################################
#RELIABILITY
##############################################################

#ICC
#############################################################

icc_data<-t(data[1:7,c(2,26,27)])
icc_data<-data[8:14,c(2,26,27)]
icc_data<-data[15:22,c(2,26,27)]
icc_data<-data[23:30,c(2,26,27)]
icc_data<-data[31:39,c(2,26,27)]
icc_data<-data[40:48,c(2,26,27)]
icc_data<-data[49:57,c(2,26,27)]
icc_data<-data[58:62,c(2,26,27)]
icc_data<-data[63:67,c(2,26,27)]
icc_data<-data[68:84,c(2,26,27)]
icc_data<-data[85:96,c(2,26,27)]
icc_data<-data[97:108,c(2,26,27)]


ICC()

##############################################################
#EXTERNAL VALIDITY
##############################################################

#FREQUENCY GRAPH
#############################################################
test <-  data.frame(v=sample(1:20,1000,replace=T), g=c('M','F'))

freq_plot<-data[,c(3,5:27)]

ggplot(data=test,aes(x=as.factor(v),fill=g)) + 
  geom_bar(data=subset(test,g=="F")) + 
  geom_bar(data=subset(test,g=="M"),aes(y=..count..*(-1))) + 
  scale_y_continuous(breaks=seq(-40,40,10),labels=abs(seq(-40,40,10))) + 
  coord_flip()


#GROUP COMPARISON
#############################################################

# describeBy(audit_score2_2,audit_data$alcohol_6h_ainjury)
with(data,by(bni_factor1,group,summary))
with(data,wilcox.test(bni_factor1~group))

boxplot<-data.frame(audit_score2,grop)
boxplot<-na.omit(boxplot)

library(ggplot2)
# Use single color
p<-ggplot(boxplot, 
		  aes(x=as.factor(grop), 
		  y=audit_score2)) +
  geom_boxplot(fill='white', 
  			   color="grey20",
  			   alpha=0.5) +
  theme_bw() +
  xlab("Groups") +
  ylab("AUDIT") +
  ylim(0, 35) +
  scale_x_discrete(labels=c("Non-drinkers","Drinkers")) +
  geom_text(aes(label="*P<.05", x=1.5, y=23, label= "boat")) + 
  geom_segment(aes(x=1.2,
  				   y=20,
  				   xend=1.8,
  				   yend=20)) +
  geom_segment(aes(x=1.2,
  				   y=20,
  				   xend=1.2,
  				   yend=18)) +
  geom_segment(aes(x=1.8,
  				   y=20,
  				   xend=1.8,
  				   yend=18)) +
  geom_text(aes(label="B)", x=0.5, y=32))

p


# END ############################################################
