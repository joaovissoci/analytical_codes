######################################################
#rw_hdrs_phq9_validation_codes.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License.
######################################################
# 
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

data$HDRS_experimental_17items<-rowSums(with(data,data.frame(
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

data$HDRS_experimental_16items<-rowSums(with(data,data.frame(
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
                                it16
                                # it17
                                # it18a,
                                # it18b,
                                # it19,
                                # it20,
                                # it21
                                )))

data$HDRS_controle17items<-rowSums(with(data,data.frame(
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

data$HDRS_controle16items<-rowSums(with(data,data.frame(
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
                                it16_2
                                # it17_2
                                # it18a_2,
                                # it18b_2,
                                # it19_2,
                                # it20_2,
                                # it21_2
                                )))


hdrs_exp16items<-with(data,data.frame(value=HDRS_experimental_16items,gold_standard))

hdrs_exp17items<-with(data,data.frame(value=HDRS_experimental_17items,gold_standard))

hdrs_ctl17items<-with(data,data.frame(value=HDRS_controle17items,gold_standard=as.factor(c(0))))

hdrs_ctl16items<-with(data,data.frame(value=HDRS_controle16items,gold_standard=as.factor(c(0))))

youden_data17items<-rbind(hdrs_exp17items,hdrs_ctl17items)

youden_data16items<-rbind(hdrs_exp16items,hdrs_ctl16items)


# hdrs_17question1<-with(data,data.frame(
#                                 it1,
#                                 it2,
#                                 it3,
#                                 it4,
#                                 it5,
#                                 it6,
#                                 it7,
#                                 it8,
#                                 it9,
#                                 it10,
#                                 it11,
#                                 it12,
#                                 it13,
#                                 it14,
#                                 it15,
#                                 it16,
#                                 it17
#                                 # it18a,
#                                 # it18b,
#                                 # it19,
#                                 # it20,
#                                 # it21
#                                 ))

# # argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
# data_imputed <- mice(hdrs_17question1, seed = 2222, m=10)

# # reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
# hdrs_17question<-mice::complete(data_imputed,4)

# kessler_data<-lapply(kessler_data,ordered)

hdrs_17question1<-with(data,data.frame(it1,
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
                                it17))

hdrs_17question2<-with(data,data.frame(it1_2,
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
                                it17_2))

colnames(hdrs_17question2)<-colnames(hdrs_17question1)

cfa_data<-rbind(hdrs_17question1,hdrs_17question2)

######################################################################
#PRE-PROCESSING
######################################################################

######################################################################
#DESCRIPTIVES
######################################################################

table(youden_data17items$gold_standard)
prop.table(table(youden_data17items$gold_standard))

##############################################################
#RELIABILITY
##############################################################
#psych::alpha(cor_data,n.iter=1000,check.keys=TRUE)
psych::alpha(cfa_data,n.iter=1000,check.keys=TRUE)

psych::alpha(cfa_data[,-17],n.iter=1000,check.keys=TRUE)

##############################################################
#CONFIRMATORY FACTOR ANALYSIS
#############################################################
# 17 items factor model
cfa_model <- '
HDRS =~  it1 + it2 + it3 + it4 + it5 + it6 + it7 + it8 + it9 + it10 +
         it11 + it12 + it13 + it14 + it15 + it16 + it17

#cov
# it5 ~~  it6
# it14 ~~  it17
# it12 ~~  it16
# it1 ~~  it17
# it1 ~~  it14
'

fit <- lavaan::cfa(cfa_model,
                   data = cfa_data,
                   estimator="WLSMV",
                   ordered=colnames(cfa_data)
                   )
summary(fit, fit.measures=TRUE)
lavaan::fitMeasures(fit, fit.measures = c("all"))
#                                           "rmsea.scaled",
#                                           "rmsea.ci.lower.scaled",
#                                           "rmsea.ci.upper.scaled",
#                                           "cfi.scaled",
#                                           "tli.scaled",
#                                           "nnfi.scaled",
#                                           "chisq.scaled",
#                                           "pvalue.scaled"
                                          # )
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
              "Q10",
              "Q11",
              "Q12",
              "Q13",
              "Q14",
              "Q15",
              "Q16",
              "Q17",
              "HDRS 17 items")

color<-c(rep("grey",17),rep("white",1))
borders<-c(rep("FALSE",17),rep("TRUE",1))
labelcex<-c(rep(0.7,17),rep(1,1))

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
sum(Est$std.all[1:17])^2/(sum(Est$std.all[1:17])^2+sum(Est$std.all[77:93]))

#Average Extracted Variance
sum(Est$std.all[1:17]^2)/length(Est$std.all[1:17])

#Thresholds
by(Est$std.all[13:50],Est$lhs[13:50],mean)

# 16 items factor model
cfa_model <- '
HDRS =~  it1 + it2 + it3 + it4 + it5 + it6 + it7 + it8 + it9 + it10 +
         it11 + it12 + it13 + it14 + it15 + it16

#cov
# it5 ~~  it6
# it14 ~~  it17
# it12 ~~  it16
# it1 ~~  it17
# it1 ~~  it14
'

fit <- lavaan::cfa(cfa_model,
                   data = cfa_data,
                   estimator="WLSMV",
                   ordered=colnames(cfa_data)
                   )
summary(fit, fit.measures=TRUE)
lavaan::fitMeasures(fit, fit.measures = c("all"))
#                                           "rmsea.scaled",
#                                           "rmsea.ci.lower.scaled",
#                                           "rmsea.ci.upper.scaled",
#                                           "cfi.scaled",
#                                           "tli.scaled",
#                                           "nnfi.scaled",
#                                           "chisq.scaled",
#                                           "pvalue.scaled"
                                          # )
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
              "Q10",
              "Q11",
              "Q12",
              "Q13",
              "Q14",
              "Q15",
              "Q16",
              # "Q17",
              "HDRS 16 items")

color<-c(rep("grey",16),rep("white",1))
borders<-c(rep("FALSE",16),rep("TRUE",1))
labelcex<-c(rep(0.7,16),rep(1,1))

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
sum(Est$std.all[1:16])^2/(sum(Est$std.all[1:16])^2+sum(Est$std.all[73:88]))

#Average Extracted Variance
sum(Est$std.all[1:16]^2)/length(Est$std.all[1:16])

#Thresholds
by(Est$std.all[13:50],Est$lhs[13:50],mean)

# #Factor scores
# kessler_overall<-lavaan::predict(fit)

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

# youden_data$mild<-car::recode(youden_data$gold_standard,"
    # '0'=0;
    # else=1")

youden_data16items$severe<-car::recode(youden_data16items$gold_standard,"
    '0'=0;
    '1'=0;
     else=1")

#Roc curve for the experimental group - 16 items
ROC(form=severe~value, data=youden_data16items)

optimal.cutpoint.Youden <- optimal.cutpoints(X = "value", 
                                             status = "severe", 
                                             tag.healthy = "0",
                                             methods = "Youden", 
                                             data = youden_data16items, 
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

#Roc curve for the experimental group - 17 items

# with(data_mcid2,by(data_mcid2[,4],change_cat_PGIC2,summary))

# youden_data$mild<-car::recode(youden_data$gold_standard,"
#     '0'=0;
#     else=1")

youden_data17items$severe<-car::recode(youden_data17items$gold_standard,"
    '0'=0;
    '1'=0;
     else=1")

ROC(form=severe~value, data=youden_data17items)

optimal.cutpoint.Youden <- optimal.cutpoints(X = "value", 
                                             status = "severe", 
                                             tag.healthy = "0",
                                             methods = "Youden", 
                                             data = youden_data17items, 
                                             pop.prev = NULL, 
                                             categorical.cov = NULL, #"gender",
                                             # control = control.cutpoints(valueSp=0.85), 
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
#END
#############################################################