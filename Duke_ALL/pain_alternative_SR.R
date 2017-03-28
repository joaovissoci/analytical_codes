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

#run metanalysis model for type of intervention
meta1 <- metacont(intervention_1, 
	mean_post_intervention,
	sd_post_intervention,
	control_1,
	mean_post_control,
	sd_post_control, 
  data=meta_model_var[-c(6,9),], sm="SMD",
  byvar=intervention_cat,print.byvar=FALSE,
  comb.fixed=FALSE,studlab=study)
summary(meta1)

tiff("/Users/jnv4/Desktop/painSR_figure1a.tiff",
  width = 1200, height = 600,compression = 'lzw')
forest(meta1)
dev.off()

funnel(meta1)
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

tiff("/Users/jnv4/Desktop/painSR_figure1b.tiff",
  width = 800, height = 400,compression = 'lzw')
forest(meta1)
dev.off()

funnel(meta1)
metainf(meta1)
metainf(meta1, pooled="random")
metareg()

#############################################################################
#Figure. 2 - Models for DIRECT interventions
#############################################################################

# modeling only DIRECT intervention results
 meta_model_direct<-subset(meta_model_var,
 	meta_model_var$intervention_cat=="Direct")

#run metanalysis model for continuous data
meta2 <- metacont(intervention_1, 
	mean_post_intervention,
	sd_post_intervention,
	control_1,
	mean_post_control,
	sd_post_control, 
  data=meta_model_direct, sm="SMD",
  print.byvar=FALSE,
  comb.fixed=FALSE,studlab=study)
summary(meta2)

tiff("/Users/jnv4/Desktop/painSR_figure2a.tiff",
  width = 800, height = 400,compression = 'lzw')
forest(meta2)
dev.off()

forest(meta2)
funnel(meta2)
metainf(meta2)
metainf(meta2, pooled="random")

#running by=group analysis for time of FUP
#NO BYGROUP - All studies are immidiatly after the intervention

# #Adjusting to avoind the error of a missing category in
# #the byvar analysis
# meta_model_direct<-as.matrix(meta_model_direct)
# meta_model_direct<-as.data.frame(meta_model_direct)

# meta_model_direct$Immediate<-as.numeric(
# 	as.character(meta_model_direct$Immediate)) 
# meta_model_direct$intervention_1<-as.numeric(
# 	as.character(meta_model_direct$intervention_1))
# meta_model_direct$mean_post_intervention<-as.numeric(
# 	as.character(meta_model_direct$mean_post_intervention))
# meta_model_direct$sd_post_intervention<-as.numeric(
# 	as.character(meta_model_direct$sd_post_intervention))
# meta_model_direct$contro_1<-as.numeric(
# 	as.character(meta_model_direct$contro_1))
# meta_model_direct$mean_post_control<-as.numeric(
# 	as.character(meta_model_direct$mean_post_control))
# meta_model_direct$sd_post_control<-as.numeric(
# 	as.character(meta_model_direct$sd_post_control))

# meta1 <- metacont(intervention_1, 
# 	mean_post_intervention,
# 	sd_post_intervention,
# 	control_1,
# 	mean_post_control,
# 	sd_post_control, 
#   data=meta_model_direct, sm="SMD",
#   print.byvar=FALSE,byvar=Immediate,
#   comb.fixed=FALSE,studlab=study)
# summary(meta1)

# tiff("/Users/jnv4/Desktop/painSR_figure2a.tiff",
#   width = 800, height = 400,compression = 'lzw')
# forest(meta1)
# dev.off()

# Excluding Kobar et al. 

#run metanalysis model for continuous data
meta2b <- metacont(intervention_1, 
	mean_post_intervention,
	sd_post_intervention,
	control_1,
	mean_post_control,
	sd_post_control, 
  data=meta_model_direct[-3,], sm="SMD",
  print.byvar=FALSE,
  comb.fixed=FALSE,studlab=study)
summary(meta2b)

tiff("/Users/jnv4/Desktop/painSR_figure2c.tiff",
  width = 800, height = 400,compression = 'lzw')
forest(meta2b)
dev.off()

#############################################################################
#Figure. 3 - Models for INDIRECT interventions
#############################################################################
# modeling only DIRECT intervention results
 meta_model_indirect<-subset(meta_model_var,
 	meta_model_var$intervention_cat=="Indirect")

#run metanalysis model for continuous data
meta3 <- metacont(intervention_1, 
	mean_post_intervention,
	sd_post_intervention,
	control_1,
	mean_post_control,
	sd_post_control, 
  data=meta_model_indirect, sm="SMD",
  print.byvar=FALSE,
  comb.fixed=FALSE,studlab=study)
meta3

tiff("/Users/jnv4/Desktop/painSR_figure3a.tiff",
  width = 800, height = 400,compression = 'lzw')
forest(meta3)
dev.off()

forest(meta3)
funnel(meta3)
metainf(meta3)
metainf(meta3, pooled="random")

#running by=group analysis for time of FUP
#NO BYGROUP - All studies are immidiatly after the intervention

#Adjusting to avoind the error of a missing category in
#the byvar analysis
# meta_model_indirect<-as.matrix(meta_model_indirect)
# meta_model_indirect<-as.data.frame(meta_model_indirect)

# meta_model_indirect$Immediate<-as.numeric(
# 	as.character(meta_model_indirect$Immediate)) 
# meta_model_indirect$intervention_1<-as.numeric(
# 	as.character(meta_model_indirect$intervention_1))
# meta_model_indirect$mean_post_intervention<-as.numeric(
# 	as.character(meta_model_indirect$mean_post_intervention))
# meta_model_indirect$sd_post_intervention<-as.numeric(
# 	as.character(meta_model_indirect$sd_post_intervention))
# meta_model_indirect$contro_1<-as.numeric(
# 	as.character(meta_model_indirect$contro_1))
# meta_model_indirect$mean_post_control<-as.numeric(
# 	as.character(meta_model_indirect$mean_post_control))
# meta_model_indirect$sd_post_control<-as.numeric(
# 	as.character(meta_model_indirect$sd_post_control))

# meta3b <- metacont(intervention_1, 
# 	mean_post_intervention,
# 	sd_post_intervention,
# 	control_1,
# 	mean_post_control,
# 	sd_post_control, 
#   data=meta_model_indirect, sm="SMD",
#   print.byvar=FALSE,byvar=Immediate,
#   comb.fixed=FALSE,studlab=study)
# summary(meta3c)

# tiff("/Users/jnv4/Desktop/painSR_figure3b.tiff",
#   width = 800, height = 400,compression = 'lzw')
# forest(meta3c)
# dev.off()

#Excluding Albert, 2002
#run metanalysis model for continuous data
meta3c <- metacont(intervention_1, 
	mean_post_intervention,
	sd_post_intervention,
	control_1,
	mean_post_control,
	sd_post_control, 
  data=meta_model_indirect[-1,], sm="SMD",
  print.byvar=FALSE,
  comb.fixed=FALSE,studlab=study)
summary(meta3c)

tiff("/Users/jnv4/Desktop/painSR_figure3c.tiff",
  width = 800, height = 400,compression = 'lzw')
forest(meta3c)
dev.off()

#############################################################################
#Figure. 4 - Models for PHYSICAL interventions
#############################################################################
# modeling only DIRECT intervention results
 meta_model_physical<-subset(meta_model_var,
 	meta_model_var$intervention_cat=="Physical")

#run metanalysis model for continuous data
meta4 <- metacont(intervention_1, 
	mean_post_intervention,
	sd_post_intervention,
	control_1,
	mean_post_control,
	sd_post_control, 
  data=meta_model_physical, sm="SMD",
  print.byvar=FALSE,
  comb.fixed=FALSE,studlab=study)
meta4

tiff("/Users/jnv4/Desktop/painSR_figure3a.tiff",
  width = 800, height = 400,compression = 'lzw')
forest(meta4)
dev.off()

forest(meta4)
funnel(meta4)
metainf(meta4)
metainf(meta4, pooled="random")

#running by=group analysis for time of FUP

#Adjusting to avoind the error of a missing category in
#the byvar analysis
meta_model_physical<-as.matrix(meta_model_physical)
meta_model_physical<-as.data.frame(meta_model_physical)

#Adjusting numeric values back to numeric
meta_model_physical$intervention_1<-as.numeric(
	as.character(meta_model_physical$intervention_1))
meta_model_physical$mean_post_intervention<-as.numeric(
	as.character(meta_model_physical$mean_post_intervention))
meta_model_physical$sd_post_intervention<-as.numeric(
	as.character(meta_model_physical$sd_post_intervention))
meta_model_physical$control_1<-as.numeric(
	as.character(meta_model_physical$control_1))
meta_model_physical$mean_post_control<-as.numeric(
	as.character(meta_model_physical$mean_post_control))
meta_model_physical$sd_post_control<-as.numeric(
	as.character(meta_model_physical$sd_post_control))

meta4b <- metacont(intervention_1, 
	mean_post_intervention,
	sd_post_intervention,
	control_1,
	mean_post_control,
	sd_post_control, 
  data=meta_model_physical, sm="SMD",
  print.byvar=FALSE,byvar=Immediate,
  comb.fixed=FALSE,studlab=study)
summary(meta4b)

tiff("/Users/jnv4/Desktop/painSR_figure4b.tiff",
  width = 800, height = 400,compression = 'lzw')
forest(meta4b)
dev.off()

#Excluding Lau, Chow and Pope, 2008 and Mealy, Brennan and SHeridan, 1986
#run metanalysis model for continuous data
meta4c <- metacont(intervention_1, 
	mean_post_intervention,
	sd_post_intervention,
	control_1,
	mean_post_control,
	sd_post_control, 
  data=meta_model_physical[-c(6),], sm="SMD",
  print.byvar=FALSE,
  comb.fixed=FALSE,studlab=study)
meta4c
metainf(meta4c)

tiff("/Users/jnv4/Desktop/painSR_figure4c.tiff",
  width = 800, height = 400,compression = 'lzw')
forest(meta4c)
dev.off()



##############################################################################
#END
##############################################################################