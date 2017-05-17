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
data<-read.csv("/Users/jnv4/Box Sync/Home Folder jnv4/Data/Global EM/Pain_SR/pain outcome_SR.csv")
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
									 mean_pre_intervention_adj,
									 sd_pre_intervention_adj,
									 mean_post_intervention_adj,
									 sd_post_intervention_adj,
									 mean_pre_control_adj,
									 sd_pre_control_adj,
									 mean_post_control_adj,
									 sd_post_control_adj,
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

#run metanalysis model for type of intervention
meta1 <- metacont(intervention_1, 
	mean_post_intervention_adj,
	sd_post_intervention_adj,
	control_1,
	mean_post_control_adj,
	sd_post_control_adj, 
  data=meta_model_var[-c(7,20),], sm="SMD",
  byvar=intervention_cat,print.byvar=FALSE,
  comb.fixed=FALSE,studlab=study)
summary(meta1)

tiff("/Users/jnv4/Desktop/painSR_figure2.tiff",
  width = 1200, height = 600,compression = 'lzw')
forest(meta1)
dev.off()

funnel(meta1)
metainf(meta1)
metainf(meta1, pooled="random")

#run metanalysis model by follow up time (immediate or not)
meta1 <- metacont(intervention_1, 
	mean_post_intervention_adj,
	sd_post_intervention_adj,
	control_1,
	mean_post_control_adj,
	sd_post_control_adj, 
  data=meta_model_var[-c(7),], sm="SMD",
  byvar=Immediate,print.byvar=FALSE,
  comb.fixed=FALSE,studlab=study)
summary(meta1)

tiff("/Users/jnv4/Desktop/painSR_figure3.tiff",
  width = 1200, height = 600,compression = 'lzw')
forest(meta1)
dev.off()

# funnel(meta1)
# metainf(meta1)
# metainf(meta1, pooled="random")
# meta::metareg( ~ intervention_cat +
# 				 Immediate,
# 				 x=meta1)

#############################################################################
#Figure. 2 - Models for DIRECT interventions
#############################################################################

# modeling only DIRECT intervention results
 meta_model_direct<-subset(meta_model_var,
 	meta_model_var$intervention_cat=="Direct")

#running by=group analysis for time of FUP
#NO BYGROUP - All studies are immidiatly after the intervention

#Adjusting to avoind the error of a missing category in
#the byvar analysis
meta_model_direct<-as.matrix(meta_model_direct)
meta_model_direct<-as.data.frame(meta_model_direct)

# meta_model_direct$Immediate<-as.numeric(
	# as.character(meta_model_direct$Immediate)) 
meta_model_direct$intervention_1<-as.numeric(
	as.character(meta_model_direct$intervention_1))
meta_model_direct$mean_post_intervention_adj<-as.numeric(
	as.character(meta_model_direct$mean_post_intervention_adj))
meta_model_direct$sd_post_intervention_adj<-as.numeric(
	as.character(meta_model_direct$sd_post_intervention_adj))
meta_model_direct$control_1<-as.numeric(
	as.character(meta_model_direct$control_1))
meta_model_direct$mean_post_control_adj<-as.numeric(
	as.character(meta_model_direct$mean_post_control_adj))
meta_model_direct$sd_post_control_adj<-as.numeric(
	as.character(meta_model_direct$sd_post_control_adj))

meta1 <- metacont(intervention_1, 
	mean_post_intervention_adj,
	sd_post_intervention_adj,
	control_1,
	mean_post_control_adj,
	sd_post_control_adj, 
  data=meta_model_direct[-c(1),], sm="SMD",
  print.byvar=FALSE,byvar=Immediate,
  comb.fixed=FALSE,studlab=study)
summary(meta1)

tiff("/Users/jnv4/Desktop/painSR_figure3a.tiff",
  width = 800, height = 400,compression = 'lzw')
forest(meta1)
dev.off()

#############################################################################
#Figure. 3 - Models for INDIRECT interventions
#############################################################################
# modeling only DIRECT intervention results
 meta_model_indirect<-subset(meta_model_var,
 	meta_model_var$intervention_cat=="Indirect")

#running by=group analysis for time of FUP
#NO BYGROUP - All studies are immidiatly after the intervention

#Adjusting to avoind the error of a missing category in
#the byvar analysis
meta_model_indirect<-as.matrix(meta_model_indirect)
meta_model_indirect<-as.data.frame(meta_model_indirect)

# meta_model_indirect$Immediate<-as.numeric(
	# as.character(meta_model_indirect$Immediate)) 
meta_model_indirect$intervention_1<-as.numeric(
	as.character(meta_model_indirect$intervention_1))
meta_model_indirect$mean_post_intervention_adj<-as.numeric(
	as.character(meta_model_indirect$mean_post_intervention_adj))
meta_model_indirect$sd_post_intervention_adj<-as.numeric(
	as.character(meta_model_indirect$sd_post_intervention_adj))
meta_model_indirect$control_1<-as.numeric(
	as.character(meta_model_indirect$control_1))
meta_model_indirect$mean_post_control_adj<-as.numeric(
	as.character(meta_model_indirect$mean_post_control_adj))
meta_model_indirect$sd_post_control_adj<-as.numeric(
	as.character(meta_model_indirect$sd_post_control_adj))

#run metanalysis model for continuous data
meta3c <- metacont(intervention_1, 
	mean_post_intervention_adj,
	sd_post_intervention_adj,
	control_1,
	mean_post_control_adj,
	sd_post_control_adj, 
  data=meta_model_indirect, sm="SMD",
  byvar=Immediate,print.byvar=FALSE,
  comb.fixed=FALSE,studlab=study)
summary(meta3c)

tiff("/Users/jnv4/Desktop/painSR_figure3b.tiff",
  width = 800, height = 400,compression = 'lzw')
forest(meta3c)
dev.off()

#############################################################################
#Figure. 4 - Models for PHYSICAL interventions
#############################################################################
# modeling only DIRECT intervention results
 meta_model_physical<-subset(meta_model_var,
 	meta_model_var$intervention_cat=="Physical")

#Adjusting to avoind the error of a missing category in
#the byvar analysis
meta_model_physical<-as.matrix(meta_model_physical)
meta_model_physical<-as.data.frame(meta_model_physical)

# meta_model_indirect$Immediate<-as.numeric(
	# as.character(meta_model_indirect$Immediate)) 
meta_model_physical$intervention_1<-as.numeric(
	as.character(meta_model_physical$intervention_1))
meta_model_physical$mean_post_intervention_adj<-as.numeric(
	as.character(meta_model_physical$mean_post_intervention_adj))
meta_model_physical$sd_post_intervention_adj<-as.numeric(
	as.character(meta_model_physical$sd_post_intervention_adj))
meta_model_physical$control_1<-as.numeric(
	as.character(meta_model_physical$control_1))
meta_model_physical$mean_post_control_adj<-as.numeric(
	as.character(meta_model_physical$mean_post_control_adj))
meta_model_physical$sd_post_control_adj<-as.numeric(
	as.character(meta_model_physical$sd_post_control_adj))

#run metanalysis model for continuous data
meta3c <- metacont(intervention_1, 
	mean_post_intervention_adj,
	sd_post_intervention_adj,
	control_1,
	mean_post_control_adj,
	sd_post_control_adj, 
  data=meta_model_physical, sm="SMD",
  byvar=Immediate,print.byvar=FALSE,
  comb.fixed=FALSE,studlab=study)
summary(meta3c)

tiff("/Users/jnv4/Desktop/painSR_figure3c.tiff",
  width = 800, height = 400,compression = 'lzw')
forest(meta3c)
dev.off()



# ##############################################################################
# #END
# ##############################################################################