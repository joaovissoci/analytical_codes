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
lapply(c("epicalc","sem","Hmisc","ggplot2", 
		 "psych", "irr","nortest","moments",
		 "GPArotation","nFactors","repmis",
		 "gdata","qgraph","igraph","meta",
		 "metafor"), 
library, character.only=T)
#################################################################
#IMPORTING DATA
#################################################################
#LOADING DATA FROM A .CSV FILE
data<-read.csv("/Users/jnv4/Box Sync/Home Folder jnv4/Data/Global EM/GIC 2016/suicide_SR.csv",sep=",")
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
	meandDIFF_control,
	sdDIFF_control,
	meanDIFF_exerimental,
	sdDIFF_experiment,
	N_group_FUP1,
	N_control_FUP1,
	proportion_FUP1_experimental_group,
	proportion_FUP1_control_group))

#extracting only studies with enough information for metanalysis
meta_model<-subset(meta_model_var,
	meta_model_var$metaanalsysis=="yes")

#############################################################################
#Figure. 2
#############################################################################
## Suicide ideation metanalysis model
#extracting studies with suicide ideation measures
meta_bssi<-subset(meta_model,
	meta_model$outcome=="suicide ideation")

#exclude variables for prevalences instead of scale results
meta_bssi<-remove.vars(meta_bssi,c(
	"proportion_FUP1_experimental_group",
	"proportion_FUP1_control_group"))

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
meta_bssi$meanDIFF_exerimental<-as.numeric(
	as.character(meta_bssi$meanDIFF_exerimental))
meta_bssi$sdDIFF_experiment<-as.numeric(
	as.character(meta_bssi$sdDIFF_experiment))
meta_bssi$N_control_FUP1<-as.numeric(
	as.character(meta_bssi$N_control_FUP1))
meta_bssi$mean_FUP1_experimental_control<-as.numeric(
	as.character(meta_bssi$mean_FUP1_experimental_control))
meta_bssi$sd_FUP1_experimental_control<-as.numeric(
	as.character(meta_bssi$sd_FUP1_experimental_control))
meta_bssi$meandDIFF_control<-as.numeric(
	as.character(meta_bssi$meandDIFF_control))
meta_bssi$sdDIFF_control<-as.numeric(
	as.character(meta_bssi$sdDIFF_control))

#recoding metanalysis groups
meta_bssi$intervention<-car::recode(meta_bssi$intervention,"
	'targeted education awarenes'='TEA or BI';
	'brief intervention and contact'='TEA or BI';
	'psychotherapy'='Psychotherapy'")

#run metanalysis model for continuous data
meta1 <- metacont(N_group_FUP1, 
	meanDIFF_exerimental,
	sdDIFF_experiment,
	N_control_FUP1,
	meandDIFF_control,
	sdDIFF_control, 
  data=meta_bssi, sm="MD",
  byvar=intervention,print.byvar=FALSE,
  studlab=study,comb.fixed=FALSE)
summary(meta1)

tiff("/Users/joaovissoci/Desktop/figure2.tiff",
  width = 800, height = 400,compression = 'lzw')
forest(meta1)
dev.off()

funnel(meta1)
metainf(meta1)
metainf(meta1, pooled="random")

#############################################################################
#Figure. 3
#############################################################################
## Suicide ideation metanalysis model
#extracting studies with suicide ideation measures
meta_rate<-subset(meta_model,
	meta_model$outcome=="suicide rate")
meta_rate<-remove.vars(meta_rate,
	c("mean_FUP1_experimental_group",
		"sd_FUP1_experimental_group",
		"mean_FUP1_experimental_control",
		"sd_FUP1_experimental_control",
		"meandDIFF_control",
	"sdDIFF_control",
	"meanDIFF_exerimental",
	"sdDIFF_experiment"))

#excluding missing information
meta_rate<-na.omit(meta_rate)

#recoding metanalysis groups
meta_rate$intervention<-car::recode(meta_rate$intervention,"
	'targeted education awarenes'='TEA or BI';
	'brief intervention and contact'='TEA or BI';
	'damage control'='Damage control'")

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
				 data=meta_rate, 
				 sm="OR",byvar=intervention,
				 print.byvar=FALSE,
 				 studlab=study,comb.fixed=FALSE)
summary(meta1)
funnel(meta1)

tiff("/Users/joaovissoci/Desktop/figure3.tiff",
  width = 900, height = 500,compression = 'lzw')
forest(meta1)
dev.off()

metainf(meta1)
metainf(meta1, pooled="random")

#############################################################################
#Figure. 4
#############################################################################
## Suicide ideation metanalysis model
#extracting studies with suicide ideation measures
meta_dep<-subset(meta_model,
	meta_model$outcome=="depression")

#exclude variables for prevalences instead of scale results
meta_dep<-remove.vars(meta_dep,c(
	"proportion_FUP1_experimental_group",
	"proportion_FUP1_control_group"))

#Adjusting to avoind the error of a missing category in
#the byvar analysis
meta_dep<-as.matrix(meta_dep)
meta_dep<-as.data.frame(meta_dep)
meta_dep$N_group_FUP1<-as.numeric(
	as.character(meta_dep$N_group_FUP1)) 
meta_dep$mean_FUP1_experimental_group<-as.numeric(
	as.character(meta_dep$mean_FUP1_experimental_group))
meta_dep$sd_FUP1_experimental_group<-as.numeric(
	as.character(meta_dep$sd_FUP1_experimental_group))
meta_dep$meanDIFF_exerimental<-as.numeric(
	as.character(meta_dep$meanDIFF_exerimental))
meta_dep$sdDIFF_experiment<-as.numeric(
	as.character(meta_dep$sdDIFF_experiment))
meta_dep$N_control_FUP1<-as.numeric(
	as.character(meta_dep$N_control_FUP1))
meta_dep$mean_FUP1_experimental_control<-as.numeric(
	as.character(meta_dep$mean_FUP1_experimental_control))
meta_dep$sd_FUP1_experimental_control<-as.numeric(
	as.character(meta_dep$sd_FUP1_experimental_control))
meta_dep$meandDIFF_control<-as.numeric(
	as.character(meta_dep$meandDIFF_control))
meta_dep$sdDIFF_control<-as.numeric(
	as.character(meta_dep$sdDIFF_control))

#recoding metanalysis groups
meta_dep$intervention<-car::recode(meta_dep$intervention,"
	'targeted education awarenes'='TEA or BI';
	'brief intervention and contact'='TEA or BI';
	'psychotherapy'='Psychotherapy'")

#run metanalysis model for continuous data
meta1 <- metacont(N_group_FUP1, 
	meanDIFF_exerimental,
	sdDIFF_experiment,
	N_control_FUP1,
	meandDIFF_control,
	sdDIFF_control, 
  data=meta_dep, sm="SMD",
  byvar=intervention,print.byvar=FALSE,
  studlab=study,comb.fixed=FALSE)
summary(meta1)

tiff("/Users/joaovissoci/Desktop/figure4.tiff",
  width = 800, height = 400,compression = 'lzw')
forest(meta1)
dev.off()

funnel(meta1)
metainf(meta1)
metainf(meta1, pooled="random")

#############################################################################
#Figure. 5
#############################################################################
## Suicide ideation metanalysis model
#extracting studies with suicide ideation measures
meta_hope<-subset(meta_model,
	meta_model$outcome=="hoplessness")

#exclude variables for prevalences instead of scale results
meta_hope<-remove.vars(meta_hope,c(
	"proportion_FUP1_experimental_group",
	"proportion_FUP1_control_group"))

#excluding missing information
meta_hope<-na.omit(meta_hope)

#Adjusting to avoind the error of a missing category in
#the byvar analysis
meta_hope<-as.matrix(meta_hope)
meta_hope<-as.data.frame(meta_hope)
meta_hope$N_group_FUP1<-as.numeric(
	as.character(meta_hope$N_group_FUP1)) 
meta_hope$mean_FUP1_experimental_group<-as.numeric(
	as.character(meta_hope$mean_FUP1_experimental_group))
meta_hope$sd_FUP1_experimental_group<-as.numeric(
	as.character(meta_hope$sd_FUP1_experimental_group))
meta_hope$meanDIFF_exerimental<-as.numeric(
	as.character(meta_hope$meanDIFF_exerimental))
meta_hope$sdDIFF_experiment<-as.numeric(
	as.character(meta_hope$sdDIFF_experiment))
meta_hope$N_control_FUP1<-as.numeric(
	as.character(meta_hope$N_control_FUP1))
meta_hope$mean_FUP1_experimental_control<-as.numeric(
	as.character(meta_hope$mean_FUP1_experimental_control))
meta_hope$sd_FUP1_experimental_control<-as.numeric(
	as.character(meta_hope$sd_FUP1_experimental_control))
meta_hope$meandDIFF_control<-as.numeric(
	as.character(meta_hope$meandDIFF_control))
meta_hope$sdDIFF_control<-as.numeric(
	as.character(meta_hope$sdDIFF_control))

#recoding metanalysis groups
meta_hope$intervention<-car::recode(meta_hope$intervention,"
	'targeted education awarenes'='TEA or BI';
	'brief intervention and contact'='TEA or BI';
	'psychotherapy'='Psychotherapy'")

#run metanalysis model for continuous data
meta1 <- metacont(N_group_FUP1, 
	meanDIFF_exerimental,
	sdDIFF_experiment,
	N_control_FUP1,
	meandDIFF_control,
	sdDIFF_control, 
  data=meta_hope, sm="MD",
  byvar=intervention,print.byvar=FALSE,
  studlab=study,comb.fixed=FALSE)
summary(meta1)

tiff("/Users/joaovissoci/Desktop/figure5.tiff",
  width = 800, height = 400,compression = 'lzw')
forest(meta1)
dev.off()

funnel(meta1)
metainf(meta1)
metainf(meta1, pooled="random")

##############################################################################
#END
##############################################################################