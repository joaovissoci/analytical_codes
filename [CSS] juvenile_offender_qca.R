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
lapply(c("sem","ggplot2", "psych", "irr", "nortest", "moments","GPArotation","nFactors","boot", "car", "gridExtra","mi","VIM","epicalc","gdata","sqldf","reshape2","mclust","foreign","survival","memisc","lme4","lmerTest","dplyr","QCA","VennDiagram"),library, character.only=T)

#Package and codes to pull data from goodle sheets
#devtools::install_github("jennybc/googlesheets")
#library(googlesheets)
#suppressMessages(library(dplyr))

######################################################
#IMPORTING DATA
######################################################

#Instructions here http://goo.gl/Ofa7gQ
data <- repmis::source_DropboxData("juvenile_offenders_qca_data.csv","pgykn5tmdb4zpeq",sep = ",",header = TRUE)

#data<-read.csv("/home/joao/Dropbox/datasets/CCS/juvenile_offenders/juvenile_offenders_qca_data.csv",sep=',')

######################################################
#DATA MANAGEMENT
######################################################

#summary(data_bea)

#Excluding zero-variation variables
#data_bea<-remove.vars(data_bea,c("auxiliary_lane","rd_condition___0","rd_condition___1","rd_condition___2","rd_condition___3"))

data<-na.omit(data)


data$drug_use<-car::recode(data$drug_use,"1=0;0=1")
data$family_composition<-car::recode(data$family_composition,"1=0;0=1")
data$outcome<-car::recode(data$outcome,"1=0;0=1")
######################################################
#QUALITATIVE COMPARATIVE ANALYSIS - From http://twt.lk/bdAP
######################################################
#Pacckages Needed
#library(QCA)
#library(VennDiagram)

# Organizing dataset
#####################

# Organize dataset with dichotomous respondes for cQCA or with proportions from 0 to 1 for fQCA
qca_data<-data[2:9]#,outcome=bancocerto$Q13)

### Calibration of numeric variables to crispy sets
# Transform a set os thresholds to be calibrated from (cathegorized from)
# Using quantiles as reference
th <- quantile(qca_data$income_prop, c(0.1, 0.5, 0.9))

# Calibrate a trivalient set using thresholds derived from cluster analysis
# Calls in the findTh function with an interval cased variable, a desired number of groups, clustering method (from hclust), distance measure used.
#pred1<-calibrate(scores$scores[,1], thresholds = findTh(scores$scores[,1], groups = 2, hclustm="complete", distm="euclidean"))
#qca_data$income<-calibrate(qca_data$income, thresholds = findTh(qca_data$income, groups = 2, hclustm="complete", distm="euclidean"))
#pred3<-calibrate(scores$scores[,3], thresholds = findTh(scores$scores[,3], groups = 2, hclustm="complete", distm="euclidean"))

### Fuzzification - transform a variablie into an interval from 0 to 1
# Argument type="fuzzy" to calculate end-point or mid-point concepts
# Calibrate fuzzy set using logistic function
qca_data$income_prop<-round(calibrate(qca_data$income_prop, type = "fuzzy", thresholds = th), 2)
#plot(x, calibrate(x, type = "fuzzy", thresholds = th[c(1,2,3,3,4,5)]), ylab = "Fuzzy Set Membership")

# Analysis of Necessity
#########################
#outcome<-data_bea$risk_classification

# Evaluates necessity based on a set o conditions. Returns 3 values
nec_test<-superSubset(qca_data, outcome = "outcome", incl.cut=1)
nec_test

#Calculate Truth Table -A table with all variables coded and theis consequent outcome displaying which conditions are necessary and sufficient for the outcome to exist
TT <- truthTable(qca_data, outcome = "outcome", incl.cut1 = 0.7, show.cases = TRUE, sort.by = c("incl", "n"), complete=FALSE) 
# neg.out=TRUE -- use outcome negative value
TT

####

# solution complex
dataCS <- eqmcc(TT, details = TRUE)#, show.cases = TRUE)
dataCS
dataCS$pims

dataPS<- eqmcc(TT,  include = "?", rowdom = FALSE, details = TRUE)
dataPS$SA$M1
dataPS$IC$overall$pims

dataIS<-eqmcc(TT, include = "?", direxp = rep(1,7), details = TRUE)
dataIS$pims

# Venn Diagrams
#####################



network_data<t(dataPS$IC$overall$pims) %*% dataPS$IC$overall$pims


qsgG3<-qgraph(network_data,layout="spring",cut = 0.1, minimum = 0.2,nodeNames=rownames(network_data))#,esize=20,graph="pcor",sampleSize=nrow(pca_data),legend.cex = 0.6,cut = 0.6, maximum = 1, minimum = 0.2, esize = 20,vsize = 5, repulsion = 0.8,nodeNames=colnames(pca_data),borders = FALSE)#,gray=T,)#,nodeNames=nomesqsg, layout=Lqsg,,groups=qsggr,vsize=vSize*3,,color=c("gold","steelblue","red","grey80"),labels=rownames(pca_data)

g<-as.igraph(qsgG3)
h<-walktrap.community(g)
h<-spinglass.community(g)
plot(h,g)