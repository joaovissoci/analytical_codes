#################################################################
# SUICIDE PREVENTION INITIATIVES SYSTEMATIC REVIEW
#################################################################
#
#
#
#
#
#################################################################
#SETTING ENVIRONMENT
#################################################################
#All packages must be installes with install.packages() function
lapply(c("epicalc", "sem","Hmisc","ggplot2", "psych", "irr", 
	"nortest", "moments","GPArotation","nFactors","repmis",
	"gdata","qgraph","igraph","meta","metafor"), 
library, character.only=T)
#################################################################
#IMPORTING DATA
#################################################################
#LOADING DATA FROM A .CSV FILE
data<-read.csv("/Users/joaovissoci/OneDrive - Duke University/datasets/DGHI/GIC 2016/suicide_SR.csv",sep=",")
#information between " " are the path to the directory in your computer where the data is stored

#############################################################################
#DATA MANAGEMENT
#############################################################################
#gathering variables important to the model
meta_model_var<-with(data,data.frame(study,outcome,
	metaanalsysis,intervention,
	mean_FUP1_experimental_group,
	sd_FUP1_experimental_group,
	mean_FUP1_experimental_control,
	sd_FUP1_experimental_control,
	N_group_FUP1,
	N_control_FUP1,
	proportion_FUP1_experimental_group,
	proportion_FUP1_control_group))

#extracting only studies with enough information for metanalysis
meta_model<-subset(meta_model_var,
	meta_model_var$metaanalsysis=="yes")

#############################################################################
#Figure. 
#############################################################################
## Suicide ideation metanalysis model
#extracting studies with suicide ideation measures
meta_bssi<-subset(meta_model,
	meta_model$outcome=="suicide ideation")

#excluding missing information
meta_bssi<-na.omit(meta_bssi)

#Adjusting to avoind the error of a missing category in
#the byvar analysis
meta_bssi<-as.matrix(meta_bssi)
meta_bssi<-as.data.frame(meta_bssi)
meta_bssi$N_group_FUP1<-as.numeric(
	as.character(meta_bssi$N_group_FUP1)) 
meta_bssi$mean_FUP1_experimental_group<-as.numeric(
	as.character(meta_bssi$mean_FUP1_experimental_group))
meta_bssi$sd_FUP1_experimental_group<-as.numeric(
	as.character(meta_bssi$sd_FUP1_experimental_group))
meta_bssi$N_control_FUP1<-as.numeric(
	as.character(meta_bssi$N_control_FUP1))
meta_bssi$mean_FUP1_experimental_control<-as.numeric(
	as.character(meta_bssi$mean_FUP1_experimental_control))
meta_bssi$sd_FUP1_experimental_control<-as.numeric(
	as.character(meta_bssi$sd_FUP1_experimental_control))

#recoding metanalysis groups
meta_bssi$intervention<-car::recode(meta_bssi$intervention,"
	'targeted education awarenes'='TEA or BI';
	'brief intervention and contact'='TEA or BI''
	'psychotherapy'='Psychotherapy'")

#run metanalysis model for continuous data
meta1 <- metacont(N_group_FUP1, 
	mean_FUP1_experimental_group,
	sd_FUP1_experimental_group,
	N_control_FUP1,
	mean_FUP1_experimental_control,
	sd_FUP1_experimental_control, 
  data=meta_bssi, sm="SMD",
  byvar=intervention)
summary(meta1)
forest(meta1)
funnel(meta1)
metainf(meta1)
metainf(meta1, pooled="random")

#############################################################################
#Figure. 
#############################################################################
## Suicide ideation metanalysis model
#extracting studies with suicide ideation measures
meta_rate<-subset(meta_model,
	meta_model$outcome=="suicide rate")
meta_rate<-remove.vars(meta_rate,
	c("mean_FUP1_experimental_group",
		"sd_FUP1_experimental_group",
		"mean_FUP1_experimental_control",
		"sd_FUP1_experimental_control"))

#excluding missing information
meta_rate<-na.omit(meta_rate)

#Adjusting to avoind the error of a missing category in
#the byvar analysis
meta_rate<-as.matrix(meta_rate)
meta_rate<-as.data.frame(meta_rate)
meta_rate$N_group_FUP1<-as.numeric(
	as.character(meta_rate$N_group_FUP1)) 
meta_rate$proportion_FUP1_experimental_group<-as.numeric(
	as.character(meta_rate$proportion_FUP1_experimental_group))
meta_rate$proportion_FUP1_control_group<-as.numeric(
	as.character(meta_rate$proportion_FUP1_control_group))
meta_rate$N_control_FUP1<-as.numeric(
	as.character(meta_rate$N_control_FUP1)) 

#recoding metanalysis groups
#meta_bssi$intervention<-car::recode(meta_bssi$intervention,"
#	'targeted education awarenes'='TEA or BI';
#	'brief intervention and contact'='TEA or BI''
#	'psychotherapy'='Psychotherapy'")

### run metanalysis model
meta1 <- metabin(proportion_FUP1_experimental_group,
				 N_group_FUP1,
				 proportion_FUP1_control_group,
				 N_control_FUP1,
data=meta_rate, sm="OR",byvar=intervention)
summary(meta1)
funnel(meta1)
forest(meta1)
metainf(meta1)
metainf(meta1, pooled="random")

##############################################################################
#END
##############################################################################