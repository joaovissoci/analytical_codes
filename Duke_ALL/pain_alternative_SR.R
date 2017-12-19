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
data<-read.csv("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/US/Pain_SR/pain_outcome_SR_v2.csv")
#information between " " are the path to the directory in your computer where the data is stored

#############################################################################
#DATA MANAGEMENT
#############################################################################
#gathering variables important to the model
meta_model_var<-with(data,data.frame(study,
									 intervention_cat,
									 pain_outcome_time_cat,
									 first_fup,
									 metanalysis,
									 pre_intervention_samplesize,
									 mean_pre_intervention_adj,
									 sd_pre_intervention_adj,
									 post_intervention_samplesize,
									 mean_post_intervention_adj,
									 sd_post_intervention_adj,
									 post_intervention_mean_DIFF_adj,
									 post_intervention_sd_DIFF_adj,
									 pre_control_samplesize,
									 mean_pre_control_adj,
									 sd_pre_control_adj,
									 post_control_samplesize,
									 mean_post_control_adj,
									 sd_post_control_adj,
									 post_control_mean_DIFF_adj,
									 post_control_sd_DIFF_adj,
									 DIFF_ready))

# #extracting only studies with enough information for metanalysis
meta_model_temp<-subset(meta_model_var,
	meta_model_var$metanalysis=="yes")

#############################################################################
#Calculating diff scores
#############################################################################

meta_model_diff<-subset(meta_model_temp,
	meta_model_temp$DIFF_ready=="yes")

meta_model_nodiff<-subset(meta_model_temp,
	meta_model_temp$DIFF_ready=="no")

#separate studies with diff and without deff

#calculate diff control
m_control <- metacont(pre_control_samplesize, 
              mean_pre_control_adj, 
              sd_pre_control_adj,
			  post_control_samplesize, 
			  mean_post_control_adj,
			  sd_post_control_adj,
              data=meta_model_nodiff)

meta_model_nodiff$post_control_mean_DIFF_adj<-m_control$TE
meta_model_nodiff$post_control_sd_DIFF_adj<-m_control$seTE

#calculate diff intervention
m_intervention <- metacont(pre_intervention_samplesize, 
              mean_pre_intervention_adj, 
              sd_pre_intervention_adj,
			  post_intervention_samplesize, 
			  mean_post_intervention_adj,
			  sd_post_intervention_adj,
              data=meta_model_nodiff)

meta_model_nodiff$post_intervention_mean_DIFF_adj<-m_intervention$TE
meta_model_nodiff$post_intervention_sd_DIFF_adj<-m_intervention$seTE

#merge databases

meta_model<-rbind(meta_model_diff,meta_model_nodiff)

#############################################################################
#Figure. 2
#############################################################################
## Suicide ideation metanalysis model
# #extracting studies with suicide ideation measures
meta_first_fup<-subset(meta_model,
	meta_model$first_fup=="yes")

#excluding missing information
# meta_first_fup<-na.omit(meta_first_fup)

# #Adjusting to avoind the error of a missing category in
# #the byvar analysis
meta_first_fup<-as.matrix(meta_first_fup)
meta_first_fup<-as.data.frame(meta_first_fup)

meta_first_fup$pre_intervention_samplesize<-as.numeric(
	as.character(meta_first_fup$pre_intervention_samplesize)) 
meta_first_fup$post_intervention_mean_DIFF_adj<-as.numeric(
	as.character(meta_first_fup$post_intervention_mean_DIFF_adj))
meta_first_fup$post_intervention_sd_DIFF_adj<-as.numeric(
	as.character(meta_first_fup$post_intervention_sd_DIFF_adj))
meta_first_fup$pre_control_samplesize<-as.numeric(
	as.character(meta_first_fup$pre_control_samplesize))
meta_first_fup$post_control_mean_DIFF_adj<-as.numeric(
	as.character(meta_first_fup$post_control_mean_DIFF_adj))
meta_first_fup$post_control_sd_DIFF_adj<-as.numeric(
	as.character(meta_first_fup$post_control_sd_DIFF_adj))


# #recoding metanalysis groups
# meta_bssi$intervention<-car::recode(meta_bssi$intervention,"
# 	'targeted education awarenes'='TEA or BI';
# 	'brief intervention and contact'='TEA or BI';
# 	'psychotherapy'='Psychotherapy'")

meta_first_fup$intervention_cat <- relevel(
	meta_first_fup$intervention_cat, "Physical")

#run metanalysis model for type of intervention
meta1 <- metacont(n.e=pre_intervention_samplesize,
	post_intervention_mean_DIFF_adj,
	post_intervention_sd_DIFF_adj,
	n.c=pre_control_samplesize,
	post_control_mean_DIFF_adj,
	post_control_sd_DIFF_adj, 
  data=meta_first_fup, 
  sm="SMD",
  byvar=intervention_cat,
  print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=study)
summary(meta1)

tiff("/Users/jnv4/Desktop/painSR_figure2.tiff",
  width = 1200, height = 600,compression = 'lzw')
forest(meta1,bysort=FALSE)
dev.off()

funnel(meta1)
metainf(meta1)
metainf(meta1, pooled="random")

#Excluding the study by Mealy 1986 reduces Hˆ2 to 80% and the estimate to -0.44
#run metanalysis model for type of intervention
meta1 <- metacont(intervention_1, 
	mean_post_intervention_adj,
	sd_post_intervention_adj,
	control_1,
	mean_post_control_adj,
	sd_post_control_adj, 
  data=meta_first_fup[-11,], 
  sm="SMD",
  byvar=intervention_cat,
  print.byvar=FALSE,
  comb.fixed=FALSE,
  studlab=study)
summary(meta1)

tiff("/Users/jnv4/Desktop/painSR_figure2.tiff",
  width = 1200, height = 600,compression = 'lzw')
forest(meta1,bysort=FALSE)
dev.off()

funnel(meta1)
metainf(meta1)
metainf(meta1, pooled="random")

#Excluding the study by Goertz 1986 reduces Hˆ2 to 69.6%% and the estimate to -0.33 
#run metanalysis model for type of intervention
meta1 <- metacont(intervention_1, 
	mean_post_intervention_adj,
	sd_post_intervention_adj,
	control_1,
	mean_post_control_adj,
	sd_post_control_adj, 
  data=meta_first_fup[-c(7,11),], 
  sm="SMD",
  byvar=intervention_cat,
  print.byvar=FALSE,
  comb.fixed=FALSE,
  studlab=study)
summary(meta1)

tiff("/Users/jnv4/Desktop/painSR_figure2.tiff",
  width = 1200, height = 600,compression = 'lzw')
forest(meta1,bysort=FALSE)
dev.off()

funnel(meta1)
metainf(meta1)
metainf(meta1, pooled="random")

#Excluding the study by Shamloo 2015 reduces Hˆ2 to 61.4%% and the estimate to -0.29 
#run metanalysis model for type of intervention
meta1 <- metacont(intervention_1, 
	mean_post_intervention_adj,
	sd_post_intervention_adj,
	control_1,
	mean_post_control_adj,
	sd_post_control_adj, 
  data=meta_first_fup[-c(7,11,19),], 
  sm="SMD",
  byvar=intervention_cat,
  print.byvar=FALSE,
  comb.fixed=FALSE,
  studlab=study)
summary(meta1)

tiff("/Users/jnv4/Desktop/painSR_figure2.tiff",
  width = 1200, height = 600,compression = 'lzw')
forest(meta1,bysort=FALSE)
dev.off()

funnel(meta1)
metainf(meta1)
metainf(meta1, pooled="random")

#discuss Lau 2008 or Borchgrevik 1998

#############################################################################
#Figure. 2 - Models for DIRECT interventions
#############################################################################

# modeling only DIRECT intervention results
 meta_model_direct<-subset(meta_model,
 	meta_model$intervention_cat=="Direct")

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

meta_model_direct$pain_outcome_time_cat <- factor(
	meta_model_direct$pain_outcome_time_cat, 
	levels=c("Immediate",
			 "Acute",
			 "SubAcute"))

meta1 <- metacont(intervention_1, 
	mean_post_intervention_adj,
	sd_post_intervention_adj,
	control_1,
	mean_post_control_adj,
	sd_post_control_adj, 
  data=meta_model_direct, sm="SMD",
  print.byvar=FALSE,byvar=pain_outcome_time_cat,
  comb.fixed=FALSE,studlab=study)
summary(meta1)

tiff("/Users/jnv4/Desktop/painSR_figure3a.tiff",
  width = 800, height = 400,compression = 'lzw')
forest(meta1,bysort=FALSE,
	   overall=FALSE)
dev.off()

funnel(meta1)
metainf(meta1)
metainf(meta1, pooled="random")

#excluding Goertz 2006 reduces Hˆ2 to 61.4% and the estimate to -0.276
meta1 <- metacont(intervention_1, 
	mean_post_intervention_adj,
	sd_post_intervention_adj,
	control_1,
	mean_post_control_adj,
	sd_post_control_adj, 
  data=meta_model_direct[-3,], sm="SMD",
  print.byvar=FALSE,byvar=pain_outcome_time_cat,
  comb.fixed=FALSE,studlab=study)
summary(meta1)

tiff("/Users/jnv4/Desktop/painSR_figure3a.tiff",
  width = 800, height = 400,compression = 'lzw')
forest(meta1,bysort=FALSE,
	   overall=FALSE)
dev.off()

funnel(meta1)
metainf(meta1)
metainf(meta1, pooled="random")

#############################################################################
#Figure. 3 - Models for INDIRECT interventions
#############################################################################
# modeling only DIRECT intervention results
 meta_model_indirect<-subset(meta_model,
 	meta_model$intervention_cat=="Indirect")

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

meta_model_indirect$pain_outcome_time_cat <- factor(
	meta_model_indirect$pain_outcome_time_cat, 
	levels=c("Immediate",
			 "Acute",
			 "SubAcute",
			 "Prolonged"))

#run metanalysis model for continuous data
meta3c <- metacont(intervention_1, 
	mean_post_intervention_adj,
	sd_post_intervention_adj,
	control_1,
	mean_post_control_adj,
	sd_post_control_adj, 
  data=meta_model_indirect, sm="SMD",
  byvar=pain_outcome_time_cat,print.byvar=FALSE,
  comb.fixed=FALSE,studlab=study)
summary(meta3c)

tiff("/Users/jnv4/Desktop/painSR_figure3b.tiff",
  width = 800, height = 400,compression = 'lzw')
forest(meta3c,
	   bysort=FALSE,
	   overall=FALSE)
dev.off()

funnel(meta3c)
metainf(meta3c)
metainf(meta3c, pooled="random")

#############################################################################
#Figure. 4 - Models for PHYSICAL interventions
#############################################################################
# modeling only DIRECT intervention results
 meta_model_physical<-subset(meta_model,
 	meta_model$intervention_cat=="Physical")

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

meta_model_physical$pain_outcome_time_cat <- factor(
	meta_model_physical$pain_outcome_time_cat, 
	levels=c("Immediate",
			 "Acute",
			 "SubAcute",
			 "Prolonged"))

#run metanalysis model for continuous data
meta3c <- metacont(intervention_1, 
	mean_post_intervention_adj,
	sd_post_intervention_adj,
	control_1,
	mean_post_control_adj,
	sd_post_control_adj, 
  data=meta_model_physical, sm="SMD",
  byvar=pain_outcome_time_cat,print.byvar=FALSE,
  comb.fixed=FALSE,studlab=study)
summary(meta3c)

tiff("/Users/jnv4/Desktop/painSR_figure3c.tiff",
  width = 800, height = 400,compression = 'lzw')
forest(meta3c,
	   bysort=FALSE,
	   overall=FALSE)
dev.off()

funnel(meta3c)
metainf(meta3c)
metainf(meta3c, pooled="random")

#run metanalysis model for continuous data
meta3c <- metacont(intervention_1, 
	mean_post_intervention_adj,
	sd_post_intervention_adj,
	control_1,
	mean_post_control_adj,
	sd_post_control_adj, 
  data=meta_model_physical[-9,], sm="SMD",
  byvar=pain_outcome_time_cat,print.byvar=FALSE,
  comb.fixed=FALSE,studlab=study)
summary(meta3c)

tiff("/Users/jnv4/Desktop/painSR_figure3c.tiff",
  width = 800, height = 400,compression = 'lzw')
forest(meta3c,
	   bysort=FALSE,
	   overall=FALSE)
dev.off()

funnel(meta3c)
metainf(meta3c)
metainf(meta3c, pooled="random")



# ##############################################################################
# #END
# ##############################################################################