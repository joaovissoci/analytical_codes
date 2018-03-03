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
data<-read.csv("/Users/Joao/Box Sync/Home Folder jnv4/Data/Global EM/US/Pain_SR/pain_sr_data-sae.csv")
#information between " " are the path to the directory in your computer where the data is stored

#############################################################################
#DATA MANAGEMENT
#############################################################################
#gathering variables important to the model
meta_model<-with(data,data.frame(study_name,
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
# meta_model<-subset(meta_model_var,
# 	meta_model_var$metanalysis=="yes")



#############################################################################
#Calculating diff scores
#############################################################################

# meta_model_diff<-subset(meta_model,
# 	meta_model$DIFF_ready=="yes")

# meta_model_nodiff<-subset(meta_model,
# 	meta_model$DIFF_ready=="no")

# #separate studies with diff and without deff

# #calculate diff control
# m_control <- metacont(pre_control_samplesize, 
#               mean_pre_control_adj, 
#               sd_pre_control_adj,
# 			  post_control_samplesize, 
# 			  mean_post_control_adj,
# 			  sd_post_control_adj,
#               data=meta_model_nodiff)

# meta_model_nodiff$post_control_mean_DIFF_adj<-m_control$TE
# meta_model_nodiff$post_control_sd_DIFF_adj<-m_control$seTE

# #calculate diff intervention
# m_intervention <- metacont(pre_intervention_samplesize, 
#               mean_pre_intervention_adj, 
#               sd_pre_intervention_adj,
# 			  post_intervention_samplesize, 
# 			  mean_post_intervention_adj,
# 			  sd_post_intervention_adj,
#               data=meta_model_nodiff)

# meta_model_nodiff$post_intervention_mean_DIFF_adj<-m_intervention$TE
# meta_model_nodiff$post_intervention_sd_DIFF_adj<-m_intervention$seTE

# #merge databases

# meta_model_prepostdiff<-rbind(meta_model_diff,meta_model_nodiff)

#############################################################################
#
#Metanalysis Post treatment comparison
#
#############################################################################

# First fup model with post treatment data

# #extracting studies with suicide ideation measures
meta_firstFUP_post<-subset(meta_model,
	meta_model$first_fup=="yes")

meta_firstFUP_post<-with(meta_firstFUP_post,data.frame(
					post_intervention_samplesize,
					mean_post_intervention_adj,
					sd_post_intervention_adj,
					post_control_samplesize,
					mean_post_control_adj,
					sd_post_control_adj,
					study_name,
					intervention_cat))

#excluding missing information
# meta_firstFUP_post<-na.omit(meta_firstFUP_post)

# #Adjusting to avoind the error of a missing category in
# #the byvar analysis
meta_firstFUP_post<-as.matrix(meta_firstFUP_post)
meta_firstFUP_post<-as.data.frame(meta_firstFUP_post)

meta_firstFUP_post$post_intervention_samplesize<-as.numeric(
	as.character(meta_firstFUP_post$post_intervention_samplesize)) 
meta_firstFUP_post$mean_post_intervention_adj<-as.numeric(
	as.character(meta_firstFUP_post$mean_post_intervention_adj))
meta_firstFUP_post$sd_post_intervention_adj<-as.numeric(
	as.character(meta_firstFUP_post$sd_post_intervention_adj))
meta_firstFUP_post$post_control_samplesize<-as.numeric(
	as.character(meta_firstFUP_post$post_control_samplesize))
meta_firstFUP_post$mean_post_control_adj<-as.numeric(
	as.character(meta_firstFUP_post$mean_post_control_adj))
meta_firstFUP_post$sd_post_control_adj<-as.numeric(
	as.character(meta_firstFUP_post$sd_post_control_adj))


# #recoding metanalysis groups
# meta_bssi$intervention<-car::recode(meta_bssi$intervention,"
# 	'targeted education awarenes'='TEA or BI';
# 	'brief intervention and contact'='TEA or BI';
# 	'psychotherapy'='Psychotherapy'")

meta_firstFUP_post$intervention_cat <- relevel(
	meta_firstFUP_post$intervention_cat, "Physical")

#run metanalysis model for type of intervention
meta_fig1_suppl <- metacont(post_intervention_samplesize,
	mean_post_intervention_adj,
	sd_post_intervention_adj,
	post_control_samplesize,
	mean_post_control_adj,
	sd_post_control_adj, 
  data=meta_firstFUP_post, 
  sm="SMD",
  byvar=intervention_cat,
  print.byvar=FALSE,
  comb.fixed=FALSE,
  studlab=study_name)
# summary(meta1)

# tiff("/Users/Joao/Desktop/figure1_POST.tiff", width = 12, height = 9, units='in',compression = 'lzw', res = 600)
setEPS()
postscript("/Users/Joao/Desktop/figure1_POST.eps",
	width = 12, height = 9)
forest(meta_fig1_suppl,sortvar=meta_fig1_suppl$TE,
					   bysort=FALSE,
					   digits.mean=2,
					   digits.sd=2,
					   label.right="Favours control", 
					   label.left="Favours experimental")
dev.off()

setEPS()
# tiff("/Users/Joao/Desktop/Suppl_figure1_POST_funnel.tiff", width = 12, height = 9, units='in',compression = 'lzw', res = 600)
postscript("/Users/Joao/Desktop/Suppl_figure1_POST_funnel.eps",
	width = 8, height = 8)
funnel(meta_fig1_suppl)
dev.off()

metainf(meta1)
metainf(meta1, pooled="random")



#############################################################################
#Figure. 2 - Models for DIRECT interventions

# modeling only DIRECT intervention results
meta_model_direct<-subset(meta_model,
 	meta_model$intervention_cat=="Direct")

meta_model_direct<-with(meta_model_direct,data.frame(
					post_intervention_samplesize,
					mean_post_intervention_adj,
					sd_post_intervention_adj,
					post_control_samplesize,
					mean_post_control_adj,
					sd_post_control_adj,
					study_name,
					intervention_cat,
					pain_outcome_time_cat))

#excluding missing information
meta_model_direct<-na.omit(meta_model_direct)

# #Adjusting to avoind the error of a missing category in
# #the byvar analysis
meta_model_direct<-as.matrix(meta_model_direct)
meta_model_direct<-as.data.frame(meta_model_direct)

meta_model_direct$post_intervention_samplesize<-as.numeric(
	as.character(meta_model_direct$post_intervention_samplesize)) 
meta_model_direct$mean_post_intervention_adj<-as.numeric(
	as.character(meta_model_direct$mean_post_intervention_adj))
meta_model_direct$sd_post_intervention_adj<-as.numeric(
	as.character(meta_model_direct$sd_post_intervention_adj))
meta_model_direct$post_control_samplesize<-as.numeric(
	as.character(meta_model_direct$post_control_samplesize))
meta_model_direct$mean_post_control_adj<-as.numeric(
	as.character(meta_model_direct$mean_post_control_adj))
meta_model_direct$sd_post_control_adj<-as.numeric(
	as.character(meta_model_direct$sd_post_control_adj))


# #recoding metanalysis groups
# meta_bssi$intervention<-car::recode(meta_bssi$intervention,"
# 	'targeted education awarenes'='TEA or BI';
# 	'brief intervention and contact'='TEA or BI';
# 	'psychotherapy'='Psychotherapy'")

meta_model_direct$pain_outcome_time_cat <- factor(
	meta_model_direct$pain_outcome_time_cat,levels(
		meta_model_direct$pain_outcome_time_cat)[c(2,1,3)])

#run metanalysis model for type of intervention
meta_direct_post <- metacont(post_intervention_samplesize,
	mean_post_intervention_adj,
	sd_post_intervention_adj,
	post_control_samplesize,
	mean_post_control_adj,
	sd_post_control_adj, 
  data=meta_model_direct, 
  sm="SMD",
  byvar=pain_outcome_time_cat,
  print.byvar=FALSE,
  comb.fixed=FALSE,
  studlab=study_name)
summary(meta_direct_post)

setEPS()
# tiff("/Users/Joao/Desktop/figure2a_POST.tiff", width = 10, height = 7, units='in',compression = 'lzw', res = 600)
postscript("/Users/Joao/Desktop/figure2a_POST.eps",
	width = 12, height = 7)
forest(meta_direct_post,sortvar=meta_direct_post$TE,
					   bysort=FALSE,
					   digits.mean=2,
					   digits.sd=2,
					   overall=FALSE,
					   label.right="Favours control", 
					   label.left="Favours experimental")
dev.off()


# setEPS()
# # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# postscript("/Users/joaovissoci/Desktop/igure2a_POST_funnel.eps",
# 	width = 8, height = 8)
# funnel(meta_direct_post)
# dev.off()

metainf(meta_direct_post)
metainf(meta_direct_post, pooled="random")



# #excluding Goertz 2006 reduces Hˆ2 to 61.4% and the estimate to -0.276
meta_direct_post_bias <- metacont(post_intervention_samplesize,
	mean_post_intervention_adj,
	sd_post_intervention_adj,
	post_control_samplesize,
	mean_post_control_adj,
	sd_post_control_adj, 
  data=meta_model_direct, 
  sm="SMD",
  # byvar=pain_outcome_time_cat,
  print.byvar=FALSE,
  comb.fixed=FALSE,
  studlab=study_name)
# summary(meta1)

# tiff("/Users/jnv4/Desktop/painSR_figure3a.tiff",
#   width = 800, height = 400,compression = 'lzw')
# forest(meta1,bysort=FALSE,
# 	   overall=FALSE)
# dev.off()

# funnel(meta1)
# metainf(meta1)
# metainf(meta1, pooled="random")
metabias(meta_direct_post_bias)

#############################################################################
#Figure. 3 - Models for INDIRECT interventions

# modeling only DIRECT intervention results
#  meta_model_indirect<-subset(meta_model,
#  	meta_model$intervention_cat=="Indirect")

# #running by=group analysis for time of FUP
# #NO BYGROUP - All studies are immidiatly after the intervention

# meta_model_indirect<-with(meta_model_indirect,data.frame(
# 					post_intervention_samplesize,
# 					mean_post_intervention_adj,
# 					sd_post_intervention_adj,
# 					post_control_samplesize,
# 					mean_post_control_adj,
# 					sd_post_control_adj,
# 					study_name,
# 					intervention_cat,
# 					pain_outcome_time_cat))

# #excluding missing information
# meta_model_indirect<-na.omit(meta_model_indirect)

# # #Adjusting to avoind the error of a missing category in
# # #the byvar analysis
# meta_model_indirect<-as.matrix(meta_model_indirect)
# meta_model_indirect<-as.data.frame(meta_model_indirect)

# meta_model_indirect$post_intervention_samplesize<-as.numeric(
# 	as.character(meta_model_indirect$post_intervention_samplesize)) 
# meta_model_indirect$mean_post_intervention_adj<-as.numeric(
# 	as.character(meta_model_indirect$mean_post_intervention_adj))
# meta_model_indirect$sd_post_intervention_adj<-as.numeric(
# 	as.character(meta_model_indirect$sd_post_intervention_adj))
# meta_model_indirect$post_control_samplesize<-as.numeric(
# 	as.character(meta_model_indirect$post_control_samplesize))
# meta_model_indirect$mean_post_control_adj<-as.numeric(
# 	as.character(meta_model_indirect$mean_post_control_adj))
# meta_model_indirect$sd_post_control_adj<-as.numeric(
# 	as.character(meta_model_indirect$sd_post_control_adj))


# # #recoding metanalysis groups
# # meta_bssi$intervention<-car::recode(meta_bssi$intervention,"
# # 	'targeted education awarenes'='TEA or BI';
# # 	'brief intervention and contact'='TEA or BI';
# # 	'psychotherapy'='Psychotherapy'")

# meta_model_indirect$pain_outcome_time_cat <- factor(
# 	meta_model_indirect$pain_outcome_time_cat,levels(
# 		meta_model_indirect$pain_outcome_time_cat)[c(2,1,4,3)])

# #run metanalysis model for type of intervention
# meta1 <- metacont(post_intervention_samplesize,
# 	mean_post_intervention_adj,
# 	sd_post_intervention_adj,
# 	post_control_samplesize,
# 	mean_post_control_adj,
# 	sd_post_control_adj, 
#   data=meta_model_indirect, 
#   sm="SMD",
#   byvar=pain_outcome_time_cat,
#   print.byvar=FALSE,
#   # comb.fixed=FALSE,
#   studlab=study_name)
# summary(meta1)

# tiff("/Users/jnv4/Desktop/painSR_figure3b.tiff",
#   width = 800, height = 400,compression = 'lzw')
# forest(meta1,
# 	   bysort=FALSE,
# 	   overall=FALSE)
# dev.off()

# funnel(meta3c)
# metainf(meta3c)
# metainf(meta3c, pooled="random")

#############################################################################
#Figure. 4 - Models for PHYSICAL interventions

# modeling only DIRECT intervention results
 meta_model_physical<-subset(meta_model,
 	meta_model$intervention_cat=="Physical")

##running by=group analysis for time of FUP
#NO BYGROUP - All studies are immidiatly after the intervention

meta_model_physical<-with(meta_model_physical,data.frame(
					post_intervention_samplesize,
					mean_post_intervention_adj,
					sd_post_intervention_adj,
					post_control_samplesize,
					mean_post_control_adj,
					sd_post_control_adj,
					study_name,
					intervention_cat,
					pain_outcome_time_cat))

#excluding missing information
meta_model_physical<-na.omit(meta_model_physical)

# #Adjusting to avoind the error of a missing category in
# #the byvar analysis
meta_model_physical<-as.matrix(meta_model_physical)
meta_model_physical<-as.data.frame(meta_model_physical)

meta_model_physical$post_intervention_samplesize<-as.numeric(
	as.character(meta_model_physical$post_intervention_samplesize)) 
meta_model_physical$mean_post_intervention_adj<-as.numeric(
	as.character(meta_model_physical$mean_post_intervention_adj))
meta_model_physical$sd_post_intervention_adj<-as.numeric(
	as.character(meta_model_physical$sd_post_intervention_adj))
meta_model_physical$post_control_samplesize<-as.numeric(
	as.character(meta_model_physical$post_control_samplesize))
meta_model_physical$mean_post_control_adj<-as.numeric(
	as.character(meta_model_physical$mean_post_control_adj))
meta_model_physical$sd_post_control_adj<-as.numeric(
	as.character(meta_model_physical$sd_post_control_adj))


# #recoding metanalysis groups
# meta_bssi$intervention<-car::recode(meta_bssi$intervention,"
# 	'targeted education awarenes'='TEA or BI';
# 	'brief intervention and contact'='TEA or BI';
# 	'psychotherapy'='Psychotherapy'")

meta_model_physical$pain_outcome_time_cat <- factor(
	meta_model_physical$pain_outcome_time_cat,levels(
		meta_model_physical$pain_outcome_time_cat)[c(2,1,4,3)])

#run metanalysis model for type of intervention
meta_physical_post <- metacont(post_intervention_samplesize,
	mean_post_intervention_adj,
	sd_post_intervention_adj,
	post_control_samplesize,
	mean_post_control_adj,
	sd_post_control_adj, 
  data=meta_model_physical, 
  sm="SMD",
  byvar=pain_outcome_time_cat,
  print.byvar=FALSE,
  comb.fixed=FALSE,
  studlab=study_name)
summary(meta_physical_post)

setEPS()
# tiff("/Users/Joao/Desktop/figure2b_POST.tiff", width = 12, height = 9, units='in',compression = 'lzw', res = 600)
postscript("/Users/Joao/Desktop/figure2b_POST.eps",
	width = 10, height = 8)
forest(meta_physical_post,sortvar=meta_physical_post$TE,
					   bysort=FALSE,
					   digits.mean=2,
					   digits.sd=2,
					   overall=FALSE,
					   label.right="Favours control", 
					   label.left="Favours experimental")
dev.off()


# setEPS()
# # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# postscript("/Users/joaovissoci/Desktop/figure2b_POST_funnel.eps",
# 	width = 8, height = 8)
# funnel(meta_physical_post)
# dev.off()

metainf(meta3c)
metainf(meta3c, pooled="random")

# ##############################################################################
# 
# DIFF Comparisons
#
# ##############################################################################

## Suicide ideation metanalysis model
# #extracting studies with suicide ideation measures
meta_first_fup_prepostdiff<-subset(meta_model,
	meta_model$first_fup=="yes")

meta_first_fup_prepostdiff<-with(meta_first_fup_prepostdiff,data.frame(
					post_intervention_samplesize,
					post_intervention_mean_DIFF_adj,
					post_intervention_sd_DIFF_adj,
					post_control_samplesize,
					post_control_mean_DIFF_adj,
					post_control_sd_DIFF_adj,
					study_name,
					intervention_cat))

#excluding missing information
meta_first_fup_prepostdiff<-na.omit(meta_first_fup_prepostdiff)

# #Adjusting to avoind the error of a missing category in
# #the byvar analysis
meta_first_fup_prepostdiff<-as.matrix(meta_first_fup_prepostdiff)
meta_first_fup_prepostdiff<-as.data.frame(meta_first_fup_prepostdiff)

meta_first_fup_prepostdiff$post_intervention_samplesize<-as.numeric(
	as.character(meta_first_fup_prepostdiff$post_intervention_samplesize)) 
meta_first_fup_prepostdiff$post_intervention_mean_DIFF_adj<-as.numeric(
	as.character(meta_first_fup_prepostdiff$post_intervention_mean_DIFF_adj))
meta_first_fup_prepostdiff$post_intervention_sd_DIFF_adj<-as.numeric(
	as.character(meta_first_fup_prepostdiff$post_intervention_sd_DIFF_adj))
meta_first_fup_prepostdiff$post_control_samplesize<-as.numeric(
	as.character(meta_first_fup_prepostdiff$post_control_samplesize))
meta_first_fup_prepostdiff$post_control_mean_DIFF_adj<-as.numeric(
	as.character(meta_first_fup_prepostdiff$post_control_mean_DIFF_adj))
meta_first_fup_prepostdiff$post_control_sd_DIFF_adj<-as.numeric(
	as.character(meta_first_fup_prepostdiff$post_control_sd_DIFF_adj))


# #recoding metanalysis groups
# meta_bssi$intervention<-car::recode(meta_bssi$intervention,"
# 	'targeted education awarenes'='TEA or BI';
# 	'brief intervention and contact'='TEA or BI';
# 	'psychotherapy'='Psychotherapy'")

meta_first_fup_prepostdiff$intervention_cat <- relevel(
	meta_first_fup_prepostdiff$intervention_cat, "Physical")

#run metanalysis model for type of intervention
meta_firstFUP_DIFF <- metacont(post_intervention_samplesize,
	post_intervention_mean_DIFF_adj,
	post_intervention_sd_DIFF_adj,
	post_control_samplesize,
	post_control_mean_DIFF_adj,
	post_control_sd_DIFF_adj, 
  data=meta_first_fup_prepostdiff, 
  sm="SMD",
  byvar=intervention_cat,
  print.byvar=FALSE,
  comb.fixed=FALSE,
  studlab=study_name)
summary(meta_firstFUP_DIFF)

setEPS()
# tiff("/Users/Joao/Desktop/suppl_figure1_DIFF.tiff", width = 12, height = 9, units='in',compression = 'lzw', res = 600)
postscript("/Users/Joao/Desktop/suppl_figure1_DIFF.eps",
	width = 11, height = 8)
forest(meta_firstFUP_DIFF,sortvar=meta_firstFUP_DIFF$TE,
					   bysort=FALSE,
					   digits.mean=2,
					   digits.sd=2,
					   label.right="Favours control", 
					   label.left="Favours experimental")
dev.off()


# setEPS()
# # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# postscript("/Users/joaovissoci/Desktop/suppl_figure1_funnel_DIFF.eps",
# 	width = 8, height = 8)
# funnel(meta_firstFUP_DIFF)
# dev.off()

metainf(meta1)
metainf(meta1, pooled="random")

#############################################################################
#Figure. 2 - Models for DIRECT interventions
#############################################################################

# modeling only DIRECT intervention results
 meta_model_direct<-subset(meta_model,
 	meta_model$intervention_cat=="Direct")

meta_model_direct<-with(meta_model_direct,data.frame(
					post_intervention_samplesize,
					post_intervention_mean_DIFF_adj,
					post_intervention_sd_DIFF_adj,
					post_control_samplesize,
					post_control_mean_DIFF_adj,
					post_control_sd_DIFF_adj,
					study_name,
					intervention_cat,
					pain_outcome_time_cat))

#excluding missing information
meta_model_direct<-na.omit(meta_model_direct)

# #Adjusting to avoind the error of a missing category in
# #the byvar analysis
meta_model_direct<-as.matrix(meta_model_direct)
meta_model_direct<-as.data.frame(meta_model_direct)

meta_model_direct$post_intervention_samplesize<-as.numeric(
	as.character(meta_model_direct$post_intervention_samplesize)) 
meta_model_direct$post_intervention_mean_DIFF_adj<-as.numeric(
	as.character(meta_model_direct$post_intervention_mean_DIFF_adj))
meta_model_direct$post_intervention_sd_DIFF_adj<-as.numeric(
	as.character(meta_model_direct$post_intervention_sd_DIFF_adj))
meta_model_direct$post_control_samplesize<-as.numeric(
	as.character(meta_model_direct$post_control_samplesize))
meta_model_direct$post_control_mean_DIFF_adj<-as.numeric(
	as.character(meta_model_direct$post_control_mean_DIFF_adj))
meta_model_direct$post_control_sd_DIFF_adj<-as.numeric(
	as.character(meta_model_direct$post_control_sd_DIFF_adj))


# #recoding metanalysis groups
# meta_bssi$intervention<-car::recode(meta_bssi$intervention,"
# 	'targeted education awarenes'='TEA or BI';
# 	'brief intervention and contact'='TEA or BI';
# 	'psychotherapy'='Psychotherapy'")

meta_model_direct$pain_outcome_time_cat <- factor(
	meta_model_direct$pain_outcome_time_cat,levels(
		meta_model_direct$pain_outcome_time_cat)[c(2,1,3)])

#run metanalysis model for type of intervention
meta_direct_DIFF <- metacont(post_intervention_samplesize,
	post_intervention_mean_DIFF_adj,
	post_intervention_sd_DIFF_adj,
	post_control_samplesize,
	post_control_mean_DIFF_adj,
	post_control_sd_DIFF_adj, 
  data=meta_model_direct, 
  sm="SMD",
  byvar=pain_outcome_time_cat,
  print.byvar=FALSE,
  comb.fixed=FALSE,
  studlab=study_name)

# setEPS()
tiff("/Users/Joao/Desktop/suppl_figure2a_DIFF.tiff", width = 12, height = 9, units='in',compression = 'lzw', res = 600)
# postscript("/Users/joaovissoci/Desktop/suppl_figure2a_DIFF.eps",
# 	width = 14, height = 12)
forest(meta_direct_DIFF,sortvar=meta_direct_DIFF$TE,
					   bysort=FALSE,
					   digits.mean=2,
					   digits.sd=2,
					   overall=FALSE,
					   label.right="Favours control", 
					   label.left="Favours experimental")
dev.off()


# setEPS()
# # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# postscript("/Users/joaovissoci/Desktop/suppl_figure2a_DIFF_funnel.eps",
# 	width = 8, height = 8)
# funnel(meta_direct_DIFF)
# dev.off()

metainf(meta1)
metainf(meta1, pooled="random")

#excluding Goertz 2006 reduces Hˆ2 to 61.4% and the estimate to -0.276
# meta1 <- metacont(intervention_1, 
# 	mean_post_intervention_adj,
# 	sd_post_intervention_adj,
# 	control_1,
# 	mean_post_control_adj,
# 	sd_post_control_adj, 
#   data=meta_model_direct[-3,], sm="SMD",
#   print.byvar=FALSE,byvar=pain_outcome_time_cat,
#   comb.fixed=FALSE,studlab=study_name)
# summary(meta1)

# tiff("/Users/jnv4/Desktop/painSR_figure3a.tiff",
#   width = 800, height = 400,compression = 'lzw')
# forest(meta1,bysort=FALSE,
# 	   overall=FALSE)
# dev.off()

# funnel(meta1)
# metainf(meta1)
# metainf(meta1, pooled="random")

#############################################################################
#Figure. 3 - Models for INDIRECT interventions
#############################################################################
# modeling only DIRECT intervention results
#  meta_model_indirect<-subset(meta_model,
#  	meta_model$intervention_cat=="Indirect")

# #running by=group analysis for time of FUP
# #NO BYGROUP - All studies are immidiatly after the intervention

# meta_model_indirect_post<-with(meta_model_indirect,data.frame(
# 					post_intervention_samplesize,
# 					mean_post_intervention_adj,
# 					sd_post_intervention_adj,
# 					post_control_samplesize,
# 					mean_post_control_adj,
# 					sd_post_control_adj,
# 					study_name,
# 					pain_outcome_time_cat))

# #excluding missing information
# meta_model_indirect_post<-na.omit(meta_model_indirect_post)

# # #Adjusting to avoind the error of a missing category in
# # #the byvar analysis
# meta_model_indirect_post<-as.matrix(meta_model_indirect_post)
# meta_model_indirect_post<-as.data.frame(meta_model_indirect_post)

# meta_model_indirect_post$post_intervention_samplesize<-as.numeric(
# 	as.character(meta_model_indirect_post$post_intervention_samplesize)) 
# meta_model_indirect_post$mean_post_intervention_adj<-as.numeric(
# 	as.character(meta_model_indirect_post$mean_post_intervention_adj))
# meta_model_indirect_post$sd_post_intervention_adj<-as.numeric(
# 	as.character(meta_model_indirect_post$sd_post_intervention_adj))
# meta_model_indirect_post$post_control_samplesize<-as.numeric(
# 	as.character(meta_model_indirect_post$post_control_samplesize))
# meta_model_indirect_post$mean_post_control_adj<-as.numeric(
# 	as.character(meta_model_indirect_post$mean_post_control_adj))
# meta_model_indirect_post$sd_post_control_adj<-as.numeric(
# 	as.character(meta_model_indirect_post$sd_post_control_adj))


# # #recoding metanalysis groups
# # meta_bssi$intervention<-car::recode(meta_bssi$intervention,"
# # 	'targeted education awarenes'='TEA or BI';
# # 	'brief intervention and contact'='TEA or BI';
# # 	'psychotherapy'='Psychotherapy'")

# meta_model_indirect_post$intervention_cat <- relevel(
# 	meta_model_indirect_post$intervention_cat, "Physical")

# #run metanalysis model for type of intervention
# meta_indirect_DIFF <- metacont(post_intervention_samplesize,
# 	mean_post_intervention_adj,
# 	sd_post_intervention_adj,
# 	post_control_samplesize,
# 	mean_post_control_adj,
# 	sd_post_control_adj, 
#   data=meta_model_indirect_post, 
#   sm="SMD",
#   print.byvar=FALSE,
#   byvar=pain_outcome_time_cat,
#   comb.fixed=FALSE,
#   studlab=study_name)
# summary(meta3c)

# setEPS()
# # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# postscript("/Users/Joao/Desktop/figure2a.eps",
# 	width = 14, height = 12)
# forest(meta_direct_DIFF,sortvar=meta_direct_DIFF$TE,
# 					   bysort=FALSE,
# 					   digits.mean=2,
# 					   digits.sd=2)
# dev.off()


# setEPS()
# # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# postscript("/Users/Joao/Desktop/igure2a_funnel.eps",
# 	width = 8, height = 8)
# funnel(meta_direct_DIFF)
# dev.off()

# metainf(meta3c)
# metainf(meta3c, pooled="random")

#############################################################################
#Figure. 4 - Models for PHYSICAL interventions
#############################################################################
# modeling only DIRECT intervention results
 meta_model_physical<-subset(meta_model,
 	meta_model$intervention_cat=="Physical")

##running by=group analysis for time of FUP
#NO BYGROUP - All studies are immidiatly after the intervention

meta_model_physical<-with(meta_model_physical,data.frame(
					post_intervention_samplesize,
					post_intervention_mean_DIFF_adj,
					post_intervention_sd_DIFF_adj,
					post_control_samplesize,
					post_control_mean_DIFF_adj,
					post_control_sd_DIFF_adj,
					study_name,
					intervention_cat,
					pain_outcome_time_cat))

#excluding missing information
meta_model_physical<-na.omit(meta_model_physical)

# #Adjusting to avoind the error of a missing category in
# #the byvar analysis
meta_model_physical<-as.matrix(meta_model_physical)
meta_model_physical<-as.data.frame(meta_model_physical)

meta_model_physical$post_intervention_samplesize<-as.numeric(
	as.character(meta_model_physical$post_intervention_samplesize)) 
meta_model_physical$post_intervention_mean_DIFF_adj<-as.numeric(
	as.character(meta_model_physical$post_intervention_mean_DIFF_adj))
meta_model_physical$post_intervention_sd_DIFF_adj<-as.numeric(
	as.character(meta_model_physical$post_intervention_sd_DIFF_adj))
meta_model_physical$post_control_samplesize<-as.numeric(
	as.character(meta_model_physical$post_control_samplesize))
meta_model_physical$post_control_mean_DIFF_adj<-as.numeric(
	as.character(meta_model_physical$post_control_mean_DIFF_adj))
meta_model_physical$post_control_sd_DIFF_adj<-as.numeric(
	as.character(meta_model_physical$post_control_sd_DIFF_adj))


# #recoding metanalysis groups
# meta_bssi$intervention<-car::recode(meta_bssi$intervention,"
# 	'targeted education awarenes'='TEA or BI';
# 	'brief intervention and contact'='TEA or BI';
# 	'psychotherapy'='Psychotherapy'")

meta_model_physical$pain_outcome_time_cat <- factor(
	meta_model_physical$pain_outcome_time_cat,levels(
		meta_model_physical$pain_outcome_time_cat)[c(2,1,4,3)])

#run metanalysis model for type of intervention
meta_physical_DIFF <- metacont(post_intervention_samplesize,
	post_intervention_mean_DIFF_adj,
	post_intervention_sd_DIFF_adj,
	post_control_samplesize,
	post_control_mean_DIFF_adj,
	post_control_sd_DIFF_adj, 
  data=meta_model_physical, 
  sm="SMD",
  byvar=pain_outcome_time_cat,
  print.byvar=FALSE,
  comb.fixed=FALSE,
  studlab=study_name)

# setEPS()
tiff("/Users/Joao/Desktop/suppl_figure2b_DIFF.tiff", width = 12, height = 9, units='in',compression = 'lzw', res = 600)
# postscript("/Users/joaovissoci/Desktop/suppl_figure2b_DIFF.eps",
# 	width = 14, height = 12)
forest(meta_physical_DIFF,sortvar=meta_physical_DIFF$TE,
					   bysort=FALSE,
					   digits.mean=2,
					   digits.sd=2,
					   overall=FALSE,
					   label.right="Favours control", 
					   label.left="Favours experimental")
dev.off()


# setEPS()
# # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# postscript("/Users/joaovissoci/Desktop/suppl_figure2b_DIFF_funnel.eps",
# 	width = 8, height = 8)
# funnel(meta_physical_DIFF)
# dev.off()

metainf(meta3c)
metainf(meta3c, pooled="random")

#run metanalysis model for continuous data
# meta3c <- metacont(intervention_1, 
# 	mean_post_intervention_adj,
# 	sd_post_intervention_adj,
# 	control_1,
# 	mean_post_control_adj,
# 	sd_post_control_adj, 
#   data=meta_model_physical[-9,], sm="SMD",
#   byvar=pain_outcome_time_cat,print.byvar=FALSE,
#   comb.fixed=FALSE,studlab=study_name)
# summary(meta3c)

# tiff("/Users/jnv4/Desktop/painSR_figure3c.tiff",
#   width = 800, height = 400,compression = 'lzw')
# forest(meta3c,
# 	   bysort=FALSE,
# 	   overall=FALSE)
# dev.off()

# funnel(meta3c)
# metainf(meta3c)
# metainf(meta3c, pooled="random")

# ##############################################################################
# #END
# ##############################################################################