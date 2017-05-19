######################################################################
#TEMPLATE_FOR _META_ANALYSIS_OF_DIAGNOSTIC_ACCURACY#
#this script follows a combination of guidelines proposed by Doebler and Holling, according to (http://cran.r-project.org/web/packages/mada/vignettes/mada.pdf)#
#
#
######################################################################
#SETTING ENVIRONMENT
######################################################################

#If this is the first time using the script, you will need to install the packages:
install.packages("metafor")
install.packages("meta")
install.packages("mada")
install.packages("RCurl")

#Load packages (after installed) with the library function
lapply(c("metafor","meta","mada","RCurl"),
library, character.only=T)
######################################################################
#IMPORTING DATA AND RECODING
######################################################################

### Diagnostic measures

#Import data from a physical spreasheet
#information between " " are the path to the directory in your computer where the data is stored
# data<-read.csv("/Users/jnv4/Box Sync/Home Folder jnv4/Data/Global EM/snakebites/snakebites_SR/snakesSR_metanalysis_data.csv")

#Another option is to import directly from an opened repository
data_github<-getURL("https://raw.githubusercontent.com/joaovissoci/Data/master/snakesSR_metanalysis_data.csv")
data<-read.csv(text = data_github,sep=",")

### Outcome measures
#from Github
data_outcome_github<-getURL("https://raw.githubusercontent.com/joaovissoci/Data/master/snakesSR_outcome.csv")
data_outcome<-read.csv(text = data_outcome_github,sep=",")

#data from .csv file
# data_outcome<-read.csv("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/snakebites_SR/snakesSR_outcome.csv")

######################################################################
#DATA MANEGEMENT
######################################################################
#DO NOT RUN
# positive<-as.integer(with(data_level3,rowSums(data.frame(TP,FN))))
# negative<-as.integer(with(data_level3,rowSums(data.frame(TN,FP))))

#subsetting the data to use only level 3 studies
data_level3<-subset(data,data$level==3)

#
data_level4<-data_level3[-c(9,10),]

#recoding outcome data
data_outcome$outcome_cat<-as.character(
	data_outcome$outcome_cat)

#subsetting only studies with level 3 data
data_outcome_level3<-data_outcome[data_outcome$level==3,]


#############################################################################
#Figure 1: DESCRIPTIVE STATISTICS
#############################################################################

#organize data.frame
metal_model<-with(data,data.frame(TP,FP,TN,FN,study_name))

# gettin descriptive diagnostic values and CIs for each study
descrip<-madad(metal_model, type = "sens",plotci=TRUE,snames=study_name)

#show object and results
descrip

#DO NOT RUN - jut used to create the dataset to export and send to co-authors
# descriptors<-data.frame(metal_model$study_name,descrip$sens$sens,descrip$sens$sens.ci,descrip$spec$spec,descrip$spec$spec.ci,descrip$negLR$negLR,descrip$negLR$negLR.ci,descrip$posLR$posLR,descrip$posLR$posLR.ci)
# colnames(descriptors)<-c("study","sens","sens_CIlower","sens_CIhigher","spec","spec_CIlower","spec_CIhigher","negLR","negLR_CIlower","negLR_CIhigher","posLR","posLR_lower","posLR_higher")
#############################################################################
#METANALYSIS - Snake Bites
#############################################################################
#attach(data)

#WRONG - Proportion metanalysis is not accurate for diagnostic testing
# #Forest plot by sensitiviy 
# data1<-with(data_level3,data.frame(study_name,positive,TP,factor))
# data_sens<-subset(data1,factor=="large snake")
# m3<-metaprop(TP,positive,study_name, sm="PLN",data=data_sens)
# forest(m3)

# #Forest plot by sepecificity
# data1<-with(data_level3,data.frame(study_name,negative,TN,factor))
# data_spec<-subset(data1,factor=="large snake")
# m3<-metaprop(TN,negative,study_name, sm="PLN",data=data_spec)
# forest(m3)

#Random effects model
#subsetting data by type of snakebite
data_sb<-subset(data_level3,factor=="large snake")

#organizing dataset
metal_model<-with(data_sb,data.frame(TP,FP,TN,FN,study_name))

#extracting descriptives
descrip<-madad(metal_model, type = "sens",plotci=TRUE,snames=study_name)

#univariate metanalysis for positive LR and negative LR
#Using DerSimonian-Laird estimator
unimeta_model<-madauni(data_sb,type="posLR",method="DSL")
unimeta_model<-madauni(data_sb,type="negLR",method="DSL")

#call for summary - reporting heterogeneity
summary(unimeta_model)

#bivariate approach by Reitsma et al. 2005
fit.reitsma <- reitsma(metal_model,correction=0.5)
summary(fit.reitsma)

#extracting summary estimated using a sampling based approach in the bivariate model
mcmc_sum <- SummaryPts(fit.reitsma, n.iter = 10^3,FUN=NULL)
summary(mcmc_sum)

#roc curve plot
plot<-plot(fit.reitsma, sroclwd = 2,
     main = "SROC curve (bivariate model) for metanalise data")
points(fpr(metanalise), sens(metanalise), pch = 2)
legend ("bottomright", c("data","summary estimate"),pch = c(2,1))
legend ("bottomleft", c("SROC", "conf.region"),lwd = c(2,1))


#############################################################################
#METANALYSIS - Time
#############################################################################

#organizing data
data_sb<-subset(data_level4,factor=="time")
metal_model<-with(data_sb,data.frame(TP,FP,TN,FN,study_name))

#fitting descriptive data
descrip<-madad(metal_model, type = "sens",plotci=TRUE,snames=study_name)

#univariate model
unimeta_model<-madauni(data_sb,type="posLR",method="DSL")
unimeta_model<-madauni(data_sb,type="negLR",method="DSL")
summary(unimeta_model)

#biriate approach
fit.reitsma <- reitsma(metal_model)
summary(fit.reitsma)

mcmc_sum <- SummaryPts(fit.reitsma, n.iter = 10^3,FUN=NULL)
summary(mcmc_sum)

#plotting roc curve
plot<-plot(fit.reitsma, sroclwd = 2,
     main = "SROC curve (bivariate model) for metanalise data")
points(fpr(metanalise), sens(metanalise), pch = 2)
legend ("bottomright", c("data","summary estimate"),pch = c(2,1))
legend ("bottomleft", c("SROC", "conf.region"),lwd = c(2,1))


#############################################################################
#METAANALYSIS model for prevalences of snakebite outcome
#############################################################################

#organizing vectors for plotting the metanalysis
data_outcome$names<-with(data_outcome,
	paste(study,year,sep=", "))
data_outcome_level3$names<-with(data_outcome_level3,
	paste(study,year,sep=", "))

#sometimes when subsetting a data for metanalysis with the meta packages
#the new subsett carries the number of levels from the full datasets#
#if by subseting you end up excluding one level, it gives you an erros
#for not having that level in the set.seed#
#to fix that, I first transformed the variable into a character so it
#would not carry the levels, then subset
#then reach back to the factor
data_outcome_level3$outcome_cat<-as.factor(data_outcome_level3$outcome_cat)

#data2<-with(data_outcome_level3,
#	data.frame(names,total,severe,outcome_cat))
#data2<-na.omit(data2)

### Metanalysis all studies
#function to export image in tiff format
# tiff("/Users/jnv4/Desktop/pointestimate_MA.tiff",
# 	width = 700, height = 200,compression = 'lzw')
#run metanalysis model for proportions
m3<-metaprop(severe,total,names, sm="PLN",
	data=data_outcome_level3,comb.fixed=FALSE)
m3
forest(m3)
# dev.off()
funnel(m3)

### Sensitivity analysis: Excluding Jorge and Nicoleti
tryout<-data_outcome[-c(3,9),]

#run model
m3<-metaprop(severe,total,names, sm="PLN",
	data=tryout,byvar=outcome_cat)
forest(m3)

### Metanalysis only level 3
m3<-metaprop(severe,total,names, sm="PLN",
	data=data_outcome_level3,byvar=outcome_cat)
forest(m3)

### Metanalysis reshaping Nicoleti = Level 4 and Janes = Level 3
data_outcome_reshaped<-data_outcome
data_outcome_reshaped$level[data_outcome_reshaped$study==
			"Nicoleti et al"]<-4
data_outcome_reshaped$level[data_outcome_reshaped$study==
			"Janes et al"]<-3

data_outcome_reshaped$outcome_cat<-as.character(
		data_outcome_level3$outcome_cat)
data_outcome_reshaped2<-subset(data_outcome_reshaped,
		data_outcome_reshaped$level==3)
data_outcome_reshaped2$outcome_cat<-as.factor(
		data_outcome_reshaped2$outcome_cat)
reshaped_data<-data_outcome_reshaped2[-3,]

m3<-metaprop(severe,total,names, sm="PLN",
	data=reshaped_data,byvar=outcome_cat)
forest(m3)
