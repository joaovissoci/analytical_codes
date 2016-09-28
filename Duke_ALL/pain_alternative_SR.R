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
data<-read.csv("/Users/joaovissoci/OneDrive - Duke University/datasets/Global EM/Pain_SR/pain outcome_SR.csv")
#information between " " are the path to the directory in your computer where the data is stored

#############################################################################
#DATA MANAGEMENT
#############################################################################
#gathering variables important to the model
meta_model_var<-with(data,data.frame(
									 study,
									 intervention_cat,
									 year,
									 country,
									 sample_size,
									 intervention_1,
									 control_1,
									 mean_pre_intervention,
									 sd_pre_intervention,
									 mean_post_intervention,
									 sd_post_intervention,
									 mean_pre_control,
									 sd_pre_control,
									 mean_post_control,
									 sd_post_control,
									 Immediate))

# #extracting only studies with enough information for metanalysis
# meta_model<-subset(meta_model_var,
# 	meta_model_var$metanalysis=="Yes")

#############################################################################
#Figure. 2
#############################################################################
## Suicide ideation metanalysis model
# #extracting studies with suicide ideation measures
# meta_bssi<-subset(meta_model,
# 	meta_model$measure=="100mm VAS")

#exclude variables for prevalences instead of scale results
# meta_bssi<-remove.vars(meta_bssi,c(
# 	"proportion_FUP1_experimental_group",
# 	"proportion_FUP1_control_group"))

#excluding missing information
# meta_bssi<-na.omit(meta_bssi)

# #Adjusting to avoind the error of a missing category in
# #the byvar analysis
# meta_bssi<-as.matrix(meta_bssi)
# meta_bssi<-as.data.frame(meta_bssi)

# meta_bssi$intervention_1<-as.numeric(
# 	as.character(meta_bssi$intervention_1)) 
# meta_bssi$mean_post_1<-as.numeric(
# 	as.character(meta_bssi$mean_post_1))
# meta_bssi$sd_post_1<-as.numeric(
# 	as.character(meta_bssi$sd_post_1))
# meta_bssi$contro_1<-as.numeric(
# 	as.character(meta_bssi$contro_1))
# meta_bssi$mean_post_1_control<-as.numeric(
# 	as.character(meta_bssi$mean_post_1_control))
# meta_bssi$sd_post_1_control<-as.numeric(
# 	as.character(meta_bssi$sd_post_1_control))


# #recoding metanalysis groups
# meta_bssi$intervention<-car::recode(meta_bssi$intervention,"
# 	'targeted education awarenes'='TEA or BI';
# 	'brief intervention and contact'='TEA or BI';
# 	'psychotherapy'='Psychotherapy'")

#run metanalysis model for continuous data
meta1 <- metacont(intervention_1, 
	mean_post_intervention,
	sd_post_intervention,
	control_1,
	mean_post_control,
	sd_post_control, 
  data=meta_model_var, sm="SMD",
  byvar=intervention_cat,print.byvar=FALSE,
  comb.fixed=FALSE,studlab=study)
summary(meta1)

tiff("/Users/jnv4/Desktop/figure1.tiff",
  width = 800, height = 400,compression = 'lzw')
forest(meta1)
dev.off()

funnel(meta1)
metainf(meta1)
metainf(meta1, pooled="random")

#run metanalysis model for continuous data
meta1 <- metacont(intervention_1, 
	mean_post_intervention,
	sd_post_intervention,
	control_1,
	mean_post_control,
	sd_post_control, 
  data=meta_model_var, sm="SMD",
  byvar=Immediate,print.byvar=FALSE,
  comb.fixed=FALSE,studlab=study)
summary(meta1)

tiff("/Users/jnv4/Desktop/figure1.tiff",
  width = 800, height = 400,compression = 'lzw')
forest(meta1)
dev.off()

funnel(meta1)
metainf(meta1)
metainf(meta1, pooled="random")

#############################################################################
#Figure. 2
#############################################################################

# #extracting studies with suicide ideation measures
 meta_model_direct<-subset(meta_model_var,
 	meta_model_var$intervention_cat=="Direct")

#run metanalysis model for continuous data
meta1 <- metacont(intervention_1, 
	mean_post_intervention,
	sd_post_intervention,
	control_1,
	mean_post_control,
	sd_post_control, 
  data=meta_model_physical, sm="SMD",
  print.byvar=FALSE,
  comb.fixed=FALSE,studlab=study)
summary(meta1)

forest(meta1)
funnel(meta1)
metainf(meta1)
metainf(meta1, pooled="random")

#exclude Kober

# #extracting studies with suicide ideation measures
 meta_model_indirect<-subset(meta_model_var,
 	meta_model_var$intervention_cat=="Indirect")

#run metanalysis model for continuous data
meta1 <- metacont(intervention_1, 
	mean_post_intervention,
	sd_post_intervention,
	control_1,
	mean_post_control,
	sd_post_control, 
  data=meta_model_indirect, sm="SMD",
  print.byvar=FALSE,
  comb.fixed=FALSE,studlab=study)
summary(meta1)

forest(meta1)
funnel(meta1)
metainf(meta1)
metainf(meta1, pooled="random")

#exclude Albert 2002

# #extracting studies with suicide ideation measures
 meta_model_physical<-subset(meta_model_var,
 	meta_model_var$intervention_cat=="Physical")

#run metanalysis model for continuous data
meta1 <- metacont(intervention_1, 
	mean_post_intervention,
	sd_post_intervention,
	control_1,
	mean_post_control,
	sd_post_control, 
  data=meta_model_physical, sm="SMD",
  print.byvar=FALSE,
  comb.fixed=FALSE,studlab=study)
summary(meta1)

forest(meta1)
funnel(meta1)
metainf(meta1)
metainf(meta1, pooled="random")

#exclude Lau, Chow, and Pope 2008 


## Suicide ideation metanalysis model
#extracting studies with suicide ideation measures
meta_bssi<-subset(meta_model,
	meta_model$measure=="10cm VAS")

#exclude variables for prevalences instead of scale results
# meta_bssi<-remove.vars(meta_bssi,c(
# 	"proportion_FUP1_experimental_group",
# 	"proportion_FUP1_control_group"))

#excluding missing information
meta_bssi<-na.omit(meta_bssi)

#Adjusting to avoind the error of a missing category in
#the byvar analysis
meta_bssi<-as.matrix(meta_bssi)
meta_bssi<-as.data.frame(meta_bssi)

meta_bssi$intervention_1<-as.numeric(
	as.character(meta_bssi$intervention_1)) 
meta_bssi$mean_post_1<-as.numeric(
	as.character(meta_bssi$mean_post_1))
meta_bssi$sd_post_1<-as.numeric(
	as.character(meta_bssi$sd_post_1))
meta_bssi$contro_1<-as.numeric(
	as.character(meta_bssi$contro_1))
meta_bssi$mean_post_1_control<-as.numeric(
	as.character(meta_bssi$mean_post_1_control))
meta_bssi$sd_post_1_control<-as.numeric(
	as.character(meta_bssi$sd_post_1_control))


# #recoding metanalysis groups
# meta_bssi$intervention<-car::recode(meta_bssi$intervention,"
# 	'targeted education awarenes'='TEA or BI';
# 	'brief intervention and contact'='TEA or BI';
# 	'psychotherapy'='Psychotherapy'")

#run metanalysis model for continuous data
meta1 <- metacont(intervention_1, 
	mean_post_1,
	sd_post_1,
	contro_1,
	mean_post_1_control,
	sd_post_1_control, 
  data=meta_bssi, sm="MD",
  byvar=intervention_cat,print.byvar=FALSE,comb.fixed=FALSE)#,studlab=study
summary(meta1)

tiff("/Users/jnv4/Desktop/figure2.tiff",
  width = 800, height = 400,compression = 'lzw')
forest(meta1)
dev.off()

funnel(meta1)
metainf(meta1)
metainf(meta1, pooled="random")
#############################################################################
#Figure. 3
#############################################################################
# Suicide ideation metanalysis model
#extracting studies with suicide ideation measures
meta_bssi<-subset(meta_model,
	meta_model$measure=="1-10 Self Report")

#exclude variables for prevalences instead of scale results
# meta_bssi<-remove.vars(meta_bssi,c(
# 	"proportion_FUP1_experimental_group",
# 	"proportion_FUP1_control_group"))

#excluding missing information
meta_bssi<-na.omit(meta_bssi)

#Adjusting to avoind the error of a missing category in
#the byvar analysis
meta_bssi<-as.matrix(meta_bssi)
meta_bssi<-as.data.frame(meta_bssi)

meta_bssi$intervention_1<-as.numeric(
	as.character(meta_bssi$intervention_1)) 
meta_bssi$mean_post_1<-as.numeric(
	as.character(meta_bssi$mean_post_1))
meta_bssi$sd_post_1<-as.numeric(
	as.character(meta_bssi$sd_post_1))
meta_bssi$contro_1<-as.numeric(
	as.character(meta_bssi$contro_1))
meta_bssi$mean_post_1_control<-as.numeric(
	as.character(meta_bssi$mean_post_1_control))
meta_bssi$sd_post_1_control<-as.numeric(
	as.character(meta_bssi$sd_post_1_control))


# #recoding metanalysis groups
# meta_bssi$intervention<-car::recode(meta_bssi$intervention,"
# 	'targeted education awarenes'='TEA or BI';
# 	'brief intervention and contact'='TEA or BI';
# 	'psychotherapy'='Psychotherapy'")

#run metanalysis model for continuous data
meta1 <- metacont(intervention_1, 
	mean_post_1,
	sd_post_1,
	contro_1,
	mean_post_1_control,
	sd_post_1_control, 
  data=meta_bssi, sm="MD",
  byvar=intervention_cat,print.byvar=FALSE,comb.fixed=FALSE)#,studlab=study
summary(meta1)

tiff("/Users/jnv4/Desktop/figure3.tiff",
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