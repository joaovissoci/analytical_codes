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
data<-read.csv("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/Africa/Tz/HIV/tz_hiv_SR.csv")
#information between " " are the path to the directory in your computer where the data is stored

#############################################################################
#DATA MANAGEMENT
#############################################################################
#gathering variables important to the model
# meta_model<-with(data,data.frame(study_name,
# 									 intervention_cat,
# 									 pain_outcome_time_cat,
# 									 first_fup,
# 									 metanalysis,
# 									 pre_intervention_samplesize,
# 									 mean_pre_intervention_adj,
# 									 sd_pre_intervention_adj,
# 									 post_intervention_samplesize,
# 									 mean_post_intervention_adj,
# 									 sd_post_intervention_adj,
# 									 post_intervention_mean_DIFF_adj,
# 									 post_intervention_sd_DIFF_adj,
# 									 pre_control_samplesize,
# 									 mean_pre_control_adj,
# 									 sd_pre_control_adj,
# 									 post_control_samplesize,
# 									 mean_post_control_adj,
# 									 sd_post_control_adj,
# 									 post_control_mean_DIFF_adj,
# 									 post_control_sd_DIFF_adj,
# 									 DIFF_ready))

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
#Overall model
#
#############################################################################

#run metanalysis model for type of intervention
meta_fig1 <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data, 
  sm="SMD",
  # byvar=intervention_cat,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data[,1])
# summary(meta1)

setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
postscript("/Users/joaovissoci/Desktop/figure1_POST.eps",
	width = 14, height = 12)
forest(meta_fig1,sortvar=meta_fig1$TE,
					   bysort=FALSE,
					   digits.mean=2,
					   digits.sd=2)
dev.off()


setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
postscript("/Users/joaovissoci/Desktop/Suppl_figure1_POST_funnel.eps",
	width = 8, height = 8)
funnel(meta_fig1_suppl)
dev.off()

metainf(meta1)
metainf(meta_fig1, pooled="random")
metareg(meta_fig1, ~ age + ART_duration + gender)


#############################################################################
#Figure. 2 - Models for DIRECT interventions

#run metanalysis model for type of intervention
meta_fig2age <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data, 
  sm="SMD",
  byvar=age,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data[,1])
# summary(meta1)

setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
postscript("/Users/joaovissoci/Desktop/figure1_POST.eps",
	width = 14, height = 12)
forest(meta_fig2age,sortvar=meta_fig2age$TE,
					   bysort=FALSE,
					   digits.mean=2,
					   digits.sd=2)
dev.off()


setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
postscript("/Users/joaovissoci/Desktop/Suppl_figure1_POST_funnel.eps",
	width = 8, height = 8)
funnel(meta_fig2age)
dev.off()

# metainf(meta1)
metainf(meta_fig2age, pooled="random")
# metareg(meta_fig1, ~ age + ART_duration + gender)

#Sensitivity analysis without Zompala and Kristofaro
meta_fig2age <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data[-c(7,17),], 
  sm="SMD",
  byvar=age,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data[-c(17,7),1])
# summary(meta1)

setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
postscript("/Users/joaovissoci/Desktop/figure1_POST.eps",
	width = 14, height = 12)
forest(meta_fig2age,sortvar=meta_fig2age$TE,
					   bysort=FALSE,
					   digits.mean=2,
					   digits.sd=2)
dev.off()


setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
postscript("/Users/joaovissoci/Desktop/Suppl_figure1_POST_funnel.eps",
	width = 8, height = 8)
funnel(meta_fig2age)
dev.off()

# metainf(meta1)
metainf(meta_fig2age, pooled="random")
# metareg(meta_fig1, ~ age + ART_duration + gender)

#############################################################################
#Figure. 2ART - Models for DIRECT interventions

#run metanalysis model for type of intervention
meta_fig2art <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data, 
  sm="SMD",
  byvar=ART_duration,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data[,1])
# summary(meta1)

setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
postscript("/Users/joaovissoci/Desktop/figure1_POST.eps",
	width = 14, height = 12)
forest(meta_fig2art,sortvar=meta_fig2art$TE,
					   bysort=FALSE,
					   digits.mean=2,
					   digits.sd=2)
dev.off()


setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
postscript("/Users/joaovissoci/Desktop/Suppl_figure1_POST_funnel.eps",
	width = 8, height = 8)
funnel(meta_fig2art)
dev.off()

# metainf(meta1)
metainf(meta_fig2art, pooled="random")
# metareg(meta_fig2art, ~ age + ART_duration + gender)

#############################################################################
#Figure. 2c - Models for DIRECT interventions

#run metanalysis model for type of intervention
meta_fig2gender <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data, 
  sm="SMD",
  byvar=gender,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data[,1])
# summary(meta1)

setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
postscript("/Users/joaovissoci/Desktop/figure1_POST.eps",
	width = 14, height = 12)
forest(meta_fig2gender,sortvar=meta_fig2gender$TE,
					   bysort=FALSE,
					   digits.mean=2,
					   digits.sd=2)
dev.off()


setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
postscript("/Users/joaovissoci/Desktop/Suppl_figure1_POST_funnel.eps",
	width = 8, height = 8)
funnel(meta_fig2gender)
dev.off()

# metainf(meta1)
metainf(meta_fig2gender, pooled="random")
# metareg(meta_fig1, ~ age + ART_duration + gender)
