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
data<-read.csv("/Users/jnv4/Desktop/audit_data_GIC2017.csv")
#information between " " are the path to the directory in your computer where the data is stored

#############################################################################
#DATA MANAGEMENT
#############################################################################
#gathering variables important to the model
# meta_model_var<-with(data,data.frame(
# 									 study,
# 									 intervention_cat,
# 									 year,
# 									 country,
# 									 sample_size,
# 									 intervention_1,
# 									 control_1,
# 									 mean_pre_intervention,
# 									 sd_pre_intervention,
# 									 mean_post_intervention,
# 									 sd_post_intervention,
# 									 mean_pre_control,
# 									 sd_pre_control,
# 									 mean_post_control,
# 									 sd_post_control,
# 									 Immediate))

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

#run metanalysis model for type of intervention
meta1 <- metacont(N_experimental_FUP1, 
	mean_FUP1_experimental_group,
	sd_FUP1_experimental_group,
	N_control_FUP1,
	mean_FUP1_control_group,
	sd_FUP1_control_group, 
  data=data, sm="SMD",
  # byvar=Categorization,print.byvar=TRUE,
  comb.fixed=TRUE,studlab=study)
summary(meta1)

tiff("/Users/jnv4/Desktop/GIC2017_figure1.tiff",
  width = 1200, height = 800,compression = 'lzw')
forest(meta1)
dev.off()

funnel(meta1)
metabias(meta1)
metainf(meta1)
metainf(meta1, pooled="random")

#run metanalysis model by follow up time (immediate or not)
meta1 <- metacont(intervention_1, 
	mean_post_intervention,
	sd_post_intervention,
	control_1,
	mean_post_control,
	sd_post_control, 
  data=meta_model_var[-c(6,9),], sm="SMD",
  byvar=Immediate,print.byvar=FALSE,
  comb.fixed=FALSE,studlab=study)
summary(meta1)

tiff("/Users/jnv4/Desktop/GIC2017_figure1.tiff",
  width = 1200, height = 800,compression = 'lzw')
forest(meta1)
dev.off()

funnel(meta1)
metainf(meta1)
metainf(meta1, pooled="random")
metareg(meta1, ~ Categorization)
