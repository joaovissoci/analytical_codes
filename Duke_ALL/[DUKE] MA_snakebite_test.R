######################################################################
#TEMPLATE_FOR _META_ANALYSIS_OF_DIAGNOSTIC_ACCURACY#
#this script follows a combination of guidelines proposed by Doebler and Holling, according to (http://cran.r-project.org/web/packages/mada/vignettes/mada.pdf)#
#
#
######################################################################
#SETTING ENVIRONMENT
######################################################################
#Load packages (after installed) with the library function
lapply(c("metafor","ggplot2","gridExtra" ,"psych", "irr", "nortest",
	"moments","GPArotation","nFactors","gdata","meta","mada"),
library, character.only=T)
######################################################################
#IMPORTING DATA AND RECODING
######################################################################

### Diagnostic measures
#computer
data<-read.csv("/Users/joaovissoci/Dropbox/datasets/DGHI/snakebites_SR/snakesSR_metanalysis_data.csv")

#notebook
data<-read.csv("/home/joao/Dropbox/datasets/DGHI/snakebites_SR/snakesSR_metanalysis_data.csv")

data_level3<-subset(data,data$level==3)

data_level4<-data_level3[-c(9,10),]

### Outcome measures
#mobile
data_outcome<-read.csv("/home/joao/Dropbox/datasets/DGHI/snakebites_SR/snakesSR_outcome.csv")

#home
data_outcome<-read.csv("/Users/joaovissoci/Dropbox/datasets/DGHI/snakebites_SR/snakesSR_outcome.csv")

######################################################################
#DATA MANEGEMENT
######################################################################
positive<-as.integer(with(data_level3,rowSums(data.frame(TP,FN))))
negative<-as.integer(with(data_level3,rowSums(data.frame(TN,FP))))

#############################################################################
#Figure 1: DESCRIPTIVE STATISTICS
#############################################################################

# CIs
metal_model<-with(data,data.frame(TP,FP,TN,FN,study_name))
descrip<-madad(metal_model, type = "sens",plotci=TRUE,snames=study_name)
descrip

descriptors<-data.frame(descrip$sens$sens,descrip$sens$sens.ci,descrip$spec$spec,descrip$spec$spec.ci,descrip$negLR$negLR,descrip$negLR$negLR.ci,descrip$posLR$posLR,descrip$posLR$posLR.ci)
colnames(descriptors)<-c("sens","sens_CIlower","sens_CIhigher","spec","spec_CIlower","spec_CIhigher","negLR","negLR_CIlower","negLR_CIhigher","posLR","posLR_lower","posLR_higher")
#############################################################################
#METANALYSIS - Snake Bites- Sensitivie and Specificity
#############################################################################
#attach(data)

#Forest plot by sensitiviy 
data1<-with(data_level3,data.frame(study_name,positive,TP,factor))
data_sens<-subset(data1,factor=="large snake")
m3<-metaprop(TP,positive,study_name, sm="PLN",data=data_sens)
forest(m3)

#Forest plot by sepecificity
data1<-with(data_level3,data.frame(study_name,negative,TN,factor))
data_spec<-subset(data1,factor=="large snake")
m3<-metaprop(TN,negative,study_name, sm="PLN",data=data_spec)
forest(m3)

#Random effects model
data_sb<-subset(data_level3,factor=="large snake")
metal_model<-with(data_sb,data.frame(TP,FP,TN,FN,study_name))
descrip<-madad(metal_model, type = "sens",plotci=TRUE,snames=study_name)
unimeta_model<-madauni(data_sb,type="posLR",method="DSL")
unimeta_model<-madauni(data_sb,type="negLR",method="DSL")
summary(unimeta_model)
#Verdadeiro Negativo/Negativo Total
#riate approach
(fit.reitsma <- reitsma(metal_model,correction=0.5))
summary(fit.reitsma)
mcmc_sum <- SummaryPts(fit.reitsma, n.iter = 10^3,FUN=NULL)
summary(mcmc_sum)
plot<-plot(fit.reitsma, sroclwd = 2,
     main = "SROC curve (bivariate model) for metanalise data")
points(fpr(metanalise), sens(metanalise), pch = 2)
legend ("bottomright", c("data","summary estimate"),pch = c(2,1))
legend ("bottomleft", c("SROC", "conf.region"),lwd = c(2,1))


#############################################################################
#METANALYSIS - Time - LP positive and negative
#############################################################################
positive<-as.integer(with(data_level4,rowSums(data.frame(TP,FN))))
data1<-with(data_level4,data.frame(study_name,positive,TP,factor))
data_sens<-subset(data1,factor=="time")
m3<-metaprop(TP,positive,study_name, sm="PLN",data=data_sens)
forest(m3)

negative<-as.integer(with(data_level3,rowSums(data.frame(TN,FP))))
data1<-with(data_level3,data.frame(study_name,negative,TN,factor))
data_spec<-subset(data1,factor=="time")
m3<-metaprop(TN,negative,study_name, sm="PLN",data=data_spec)
metainf(m3)
forest(m3)

data_sb<-subset(data_level4,factor=="time")
metal_model<-with(data_sb,data.frame(TP,FP,TN,FN,study_name))
descrip<-madad(metal_model, type = "sens",plotci=TRUE,snames=study_name)
unimeta_model<-madauni(data_sb,type="posLR",method="DSL")
unimeta_model<-madauni(data_sb,type="negLR",method="DSL")
summary(unimeta_model)
#Verdadeiro Negativo/Negativo Total
#riate approach
(fit.reitsma <- reitsma(metal_model))
summary(fit.reitsma)
mcmc_sum <- SummaryPts(fit.reitsma, n.iter = 10^3,FUN=list(sens))
mcmc_sum <- SummaryPts(fit.reitsma, n.iter = 10^3,FUN=fpr)
mcmc_sum <- SummaryPts(fit.reitsma, n.iter = 10^3,FUN=NULL)
summary(mcmc_sum)
plot<-plot(fit.reitsma, sroclwd = 2,
     main = "SROC curve (bivariate model) for metanalise data")
points(fpr(metanalise), sens(metanalise), pch = 2)
legend ("bottomright", c("data","summary estimate"),pch = c(2,1))
legend ("bottomleft", c("SROC", "conf.region"),lwd = c(2,1))


#############################################################################
#META Model Prevalences
#############################################################################
#attach(data)

names(data)
data_outcome$names<-with(data_outcome,paste(study,year,sep=", "))

data2<-with(data_outcome,data.frame(names,total,severe,outcome_cat))
#data2<-na.omit(data2)
m3<-metaprop(severe,total,names, sm="PLN",data=data_outcome,byvar=outcome_cat)
forest(m3)

data2<-with(data,data.frame(names,Negative,TN,Tesla))
data2<-na.omit(data2)
m3<-metaprop(TN,Negative,names, sm="PLN",data=data2,byvar=Tesla)
forest(m3)

