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
lapply(c("sem","ggplot2", "psych", "RCurl", "irr", "nortest", 
	"moments","GPArotation","nFactors","boot","psy", "car",
	"vcd", "gridExtra","mi","VIM","epicalc","gdata","sqldf",
	"reshape2","mclust","foreign","survival","memisc","lme4",
	"lmerTest","dplyr","QCA","VennDiagram","qgraph","igraph",
	"ltm","gmodels","eRm","mirt","dplyr","devtools","reshape",
  "mice","haven","pROC","Epi","OptimalCutpoints"),
library, character.only=T)


#Package and codes to pull data from goodle sheets
#devtools::install_github("jennybc/googlesheets")
#library(googlesheets)
#suppressMessages(library(dplyr))

######################################################
#IMPORTING DATA
######################################################

# add the path to you computer between " "
data<-read.csv("/Users/Joao/Box Sync/Home Folder jnv4/Data/consultation/UCB/rw_hdrs_roc_data.csv",sep=',')

######################################################
#DATA MANAGEMENT
######################################################

#re-structuring data
data$gold_standard<-as.factor(data$gold_standard)

#calculating HDRS scores
# experimental group

data$HDRS_experimental<-rowSums(with(data,data.frame(
                                it1,
                                it2,
                                it3,
                                it4,
                                it5,
                                it6,
                                it7,
                                it8,
                                it9,
                                it10,
                                it11,
                                it12,
                                it13,
                                it14,
                                it15,
                                it16,
                                it17
                                # it18a,
                                # it18b,
                                # it19,
                                # it20,
                                # it21
                                )))

data$HDRS_controle<-rowSums(with(data,data.frame(
                                it1_2,
                                it2_2,
                                it3_2,
                                it4_2,
                                it5_2,
                                it6_2,
                                it7_2,
                                it8_2,
                                it9_2,
                                it10_2,
                                it11_2,
                                it12_2,
                                it13_2,
                                it14_2,
                                it15_2,
                                it16_2,
                                it17_2
                                # it18a_2,
                                # it18b_2,
                                # it19_2,
                                # it20_2,
                                # it21_2
                                )))

######################################################################
#PRE-PROCESSING
######################################################################

######################################################################
#FLOORING,AND CEILING EFFECT
######################################################################

##############################################################
#RELIABILITY
##############################################################

########################################################
#ROC Plot with Sensitivity and Specificity
########################################################
# with(data_mcid2,by(change_score,change_cat_PGIC1_mild,summary))
# with(data_mcid2,by(data_mcid2[,4],change_cat_PGIC1_mild,summary))
# with(data_mcid2,by(change_score,change_cat_PGIC1_moderate,summary))
# with(data_mcid2,by(data_mcid2[,4],change_cat_PGIC1_moderate,summary))
# with(data_mcid2,by(change_score,change_cat_PGIC1_severe,summary))
# with(data_mcid2,by(data_mcid2[,4],change_cat_PGIC1_severe,summary))
# with(data_mcid2,by(change_score,change_cat_PGIC2,summary))
# with(data_mcid2,by(data_mcid2[,4],change_cat_PGIC2,summary))

#Roc curve for the experimental group
ROC(form=gold_standard~HDRS_experimental, data=data)

optimal.cutpoint.Youden <- optimal.cutpoints(X = "HDRS_experimental", 
                                             status = "gold_standard", 
                                             tag.healthy = "1",
                                             methods = "Youden", 
                                             data = data, 
                                             pop.prev = NULL, 
                                             categorical.cov = NULL, #"gender",
                                             control = control.cutpoints(), 
                                             ci.fit = FALSE, 
                                             conf.level = 0.95, 
                                             trace = FALSE)

# optimal.cutpoint.Youden <- optimal.cutpoints(X = "change_score", 
#                                              status = "change_cat_PGIC1_mild", 
#                                              tag.healthy = "stable",
#                                              methods = "Youden", 
#                                              data = data_mcid_control, 
#                                              pop.prev = NULL, 
#                                              categorical.cov = NULL, #"gender",
#                                              control = control.cutpoints(), 
#                                              ci.fit = FALSE, 
#                                              conf.level = 0.95, 
#                                              trace = FALSE)

summary(optimal.cutpoint.Youden)

plot(optimal.cutpoint.Youden)

#############################################################
#SRM - Standardized Response Mean - Cohen's D
#############################################################

# The SRM provided measurements of responsiveness
# and was calculated by dividing the mean difference
# in scores of participants by the SD of the change scores.

# Therefore, it was recommended to calculate responsiveness
# for only the improved patients.

# According to the criteria of Cohen,42 SRM
# values of 0.2, 0.5, and 0.8 represent small, moderate, and large
# values for responsiveness, respectively. Bootstrap 1000 samples
# with replacement were used to estimate 95% confidence
# intervals (CIs) for the SRMs.43 R software (Version 2.9.1)a was
# used for statistical computing.

CohenD<-pooled_mean/pooled_sd
CohenD
