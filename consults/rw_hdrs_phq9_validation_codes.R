######################################################
#suicide_anxiety.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. see full license at the end of this file.
######################################################
# For the study, I attach for you information the PHQ-9 protocol of validation.  We have finished collecting data for the first visit (401 cases) & second visit (80 visits).
# What we would need help with is:
 
# 1.       Demographic analysis (median, mean, min, max, SD)
# a.                   Demographic data (age, gender, marital status, employment by categories, …)
# b.                   Disease related data (duration of epilepsy since diagnosis and since first seizure, EEG, MRI, CT,…)
# c.                   Past AEDs (by number, by type (CBZ, PHT, VPA, LEV))
# d.                   Concomitant AEDs (by number of AEDs (1-2-3 or more, by type (CBZ, PHT, VPA, LEV))
# e.                   Number of seizures since last visit
# f.                    AE reports in two weeks between test and retest
# g.                   Traditional healing methods by category
# h.                   Other medications (yes/no, number of medications)
# 2.       Intra & inter radar variability on PHQ-9 pre- and post-visit
# 3.       Range Operating Curve (ROC) to determine cut-off of PHQ-9
# a.                   Separated by HDRS cut-off values (see attached reference document)
#                                                                i.      HDRS 0_6 (no depression)
#                                                              ii.      HDRS 7-17 (mild to moderate)
#                                                            iii.      HDRS 18-24 (severe)
#                                                            iv.      HDRS >24 (very severe)
# 4.       Test / re-test probability of post visit (i.e. what is the probability that Test 2 is consistent with Test 1)
# a.                   If there is a variation, must exclude patients that:
#                                                                i.      Started anti-depressants
#                                                              ii.      Experienced a life event (death, marriage, etc.)
#                                                            iii.      Changed medication
 
# Optional (TBD):
#        Split in age groups
#        Analyze factors affecting variability (age, number of seizures, marital status, employment status, …)
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
data<-read.csv("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/consultation/UCB/rw_hdrs_roc_data.csv",sep=',')

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


hdrs_exp<-with(data,data.frame(value=HDRS_experimental,gold_standard))

hdrs_ctl<-with(data,data.frame(value=HDRS_controle,gold_standard=as.factor(c(0))))

youden_data<-rbind(hdrs_exp,hdrs_ctl)


hdrs_17question1<-with(data,data.frame(
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
                                ))

# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
data_imputed <- mice(hdrs_17question1, seed = 2222, m=10)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
hdrs_17question<-mice::complete(data_imputed,4)

######################################################################
#PRE-PROCESSING
######################################################################

######################################################################
#FLOORING,AND CEILING EFFECT
######################################################################

##############################################################
#RELIABILITY
##############################################################
#psych::alpha(cor_data,n.iter=1000,check.keys=TRUE)
psych::alpha(hdrs_17question,n.iter=1000,check.keys=TRUE)

##############################################################
#CONFIRMATORY FACTOR ANALYSIS
#############################################################
# kessler_data<-lapply(kessler_data,ordered)

# 1 factor model
cfa_model <- '
HDRS =~  it1 + it2 + it3 + it4 + it5 + it6 + it7 + it8 + it9 + it10 +
         it11 + it12 + it13 + it14 + it15 + it16 + it17

#cov
it5 ~~  it6
it14 ~~  it17
it12 ~~  it16
it1 ~~  it17
it1 ~~  it14

'

fit <- lavaan::cfa(cfa_model,
                   data = hdrs_17question,
                   estimator="WLSMV",
                   ordered=colnames(hdrs_17question)
                   )
summary(fit, fit.measures=TRUE)
lavaan::fitMeasures(fit, fit.measures = c("rmsea.scaled",
                                          "rmsea.ci.lower.scaled",
                                          "rmsea.ci.upper.scaled",
                                          "cfi.scaled",
                                          "tli.scaled",
                                          "nnfi.scaled",
                                          "chisq.scaled",
                                          "pvalue.scaled"
                                          )
                    )
# AIC(fit)
Est <- lavaan::parameterEstimates(fit, ci = TRUE, standardized = TRUE)
subset(Est, op == "=~")
subset(Est, op == "~~")
lavInspect(fit,what="th")

### Modification Indexes
Mod <- lavaan::modificationIndices(fit)
subset(Mod, mi > 10)

nodeLabels<-c("Q1",
              "Q2",
              "Q3",
              "Q4",
              "Q5",
              "Q6",
              "Q7",
              "Q8",
              "Q9",
              # "Q10",
              # "Q11",
              # "Q12",
              # "Q13",
              # "Q14",
              # "Q15",
              # "Q16",
              # "Q17",
              "PHQ9")

color<-c(rep("grey",9),rep("white",1))
borders<-c(rep("FALSE",9),rep("TRUE",1))
labelcex<-c(rep(0.7,9),rep(1,1))

# tiff("/Users/jnv4/Desktop/resilience_stress_fig2.tiff", units='in', 
#   width = 15,
#  height = 10,compression = 'lzw',res=1200,bg = "white")
library(semPlot)
semPlot::semPaths(fit,
                  "model",
                  "std",
                  # layout="spring",
                  style="lisrel",
                  residuals=FALSE,
                  # cut=1,
                  # equalizeManifests=TRUE,
                  # edge.color="black",
                  exoCov=FALSE,
                  intercepts=FALSE,
                  nodeLabels=nodeLabels,
                  label.scale=FALSE,
                  edge.label.cex=0.8,
                  label.cex=labelcex,
                  color=color,
                  borders=borders)
                  # bifactor="general")
# dev.off()

#Composite Reliabilty
sum(Est$std.all[1:17])^2/(sum(Est$std.all[1:17])^2+sum(Est$std.all[80:96]))

#Average Extracted Variance
sum(Est$std.all[1:17]^2)/length(Est$std.all[1:17])

#Thresholds
by(Est$std.all[13:50],Est$lhs[13:50],mean)

#Factor scores
kessler_overall<-lavaan::predict(fit)

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

youden_data$mild<-car::recode(youden_data$gold_standard,"
    '0'=0;
    else=1")

youden_data$severe<-car::recode(youden_data$gold_standard,"
    '0'=0;
    '1'=0;
     else=1")


#Roc curve for the experimental group
ROC(form=gold_standard~value, data=youden_data)



optimal.cutpoint.Youden <- optimal.cutpoints(X = "value", 
                                             status = "gold_standard", 
                                             tag.healthy = "0",
                                             methods = "Youden", 
                                             data = youden_data, 
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
