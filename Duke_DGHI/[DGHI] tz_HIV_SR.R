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
data<-read.csv("/Users/Joao/Box Sync/Home Folder jnv4/Data/Global EM/Africa/Tz/HIV/tz_hiv_SR.csv")
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
postscript("/Users/Joao/Desktop/figure1_overall.eps",
	width = 14, height = 8)
forest(meta_fig1,sortvar=meta_fig1$TE,
					   bysort=FALSE,
					   digits.mean=2,
					   digits.sd=2)
dev.off()

setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
postscript("/Users/Joao/Desktop/figure1_overall_v2.eps",
	width = 14, height = 8)
forest(meta_fig1,sortvar=meta_fig1$TE,
					   bysort=FALSE,
					   digits.mean=2,
					   digits.sd=2,
					   layout="JAMA")
dev.off()

setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
postscript("/Users/Joao/Desktop/Suppl_figure1_funnel.eps",
	width = 8, height = 8)
funnel(meta_fig1)
dev.off()

metainf(meta1)
metainf(meta_fig1, pooled="random")
metareg(meta_fig1, ~ age + ART_duration + gender + smoking + race)


#############################################################################
#
#Figure 2 - by Age model
#
#############################################################################

metareg(meta_fig1, ~ age)

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
postscript("/Users/Joao/Desktop/figure2_age_v2.eps",
	width = 14, height = 6)
forest(meta_fig2age,sortvar=meta_fig2age$TE,
					   bysort=FALSE,
					   digits.mean=2,
					   digits.sd=2,
					   layout="JAMA")

dev.off()

setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
postscript("/Users/Joao/Desktop/figure2_age.eps",
	width = 14, height = 8)
forest(meta_fig2age,sortvar=meta_fig2age$TE,
					   bysort=FALSE,
					   digits.mean=2,
					   digits.sd=2,
					   print.byvar=FALSE
)
dev.off()


# setEPS()
# # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# postscript("/Users/Joao/Desktop/Suppl_figure1_POST_funnel.eps",
# 	width = 8, height = 8)
# funnel(meta_fig2age)
# dev.off()

# Age above 40 sub-group
################################################################

#Age above 40 years

data_above40<-subset(data,data$age=="Age above 40 years")

data_above40$age<-as.character(data_above40$age)

#run metanalysis model for type of intervention
meta_fig2age_above40 <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data_above40, 
  sm="SMD",
  # byvar=age,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_above40[,1])

# metainf(meta1)
metainf(meta_fig2age_above40, pooled="random")
# metareg(meta_fig1, ~ age + ART_duration + gender)

#funnel
funnel(meta_fig2age_above40)

#run metanalysis model for age groups excluding Zormpala and Cristofaro
# meta_fig2age_above40_sensitivity <- metacont(n_HIV_pos,
# 	Mean_cIMT_HIVpos,
# 	SD_cIMT_HIVpos,
# 	n_HIV_neg,
# 	Mean_cIMT_HIVneg,
# 	SD_cIMT_HIVneg, 
#   data=data_above40[-c(11,13),], 
#   sm="SMD",
#   # byvar=age,
#   # print.byvar=FALSE,
#   # comb.fixed=FALSE,
#   studlab=data_above40[-c(11,13),1])

# metainf(meta1)
# metainf(meta_fig2age_above40_sensitivity, pooled="random")
# metareg(meta_fig1, ~ age + ART_duration + gender)

#funnel
# funnel(meta_fig2age_above40_sensitivity)

#Sensitivity analysis without Zompala and Kristofaro
# meta_fig2age <- metacont(n_HIV_pos,
# 	Mean_cIMT_HIVpos,
# 	SD_cIMT_HIVpos,
# 	n_HIV_neg,
# 	Mean_cIMT_HIVneg,
# 	SD_cIMT_HIVneg, 
#   data=data[-c(7,17),], 
#   sm="SMD",
#   byvar=age,
#   # print.byvar=FALSE,
#   # comb.fixed=FALSE,
#   studlab=data[-c(17,7),1])
# summary(meta1)

# setEPS()
# # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# postscript("/Users/joaovissoci/Desktop/figure1_POST.eps",
# 	width = 14, height = 12)
# forest(meta_fig2age,sortvar=meta_fig2age$TE,
# 					   bysort=FALSE,
# 					   digits.mean=2,
# 					   digits.sd=2)
# dev.off()


# setEPS()
# # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# postscript("/Users/joaovissoci/Desktop/Suppl_figure1_POST_funnel.eps",
# 	width = 8, height = 8)
# funnel(meta_fig2age)
# dev.off()

# # metainf(meta1)

# metainf(meta_fig2age, pooled="random")

# Age below 40 sub-group
################################################################

#Age below 40 years

data_below40<-subset(data,data$age=="Age less than 40 years")

data_below40$age<-as.character(data_below40$age)

#run metanalysis model for type of intervention
meta_fig2age_below40 <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data_below40, 
  sm="SMD",
  # byvar=age,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_below40[,1])

# metainf(meta1)
metainf(meta_fig2age_below40, pooled="random")
# metareg(meta_fig1, ~ age + ART_duration + gender)

#funnel
funnel(meta_fig2age_below40)

# #run metanalysis model for age groups excluding Zormpala and Cristofaro
# meta_fig2age_below40_sensitivity <- metacont(n_HIV_pos,
# 	Mean_cIMT_HIVpos,
# 	SD_cIMT_HIVpos,
# 	n_HIV_neg,
# 	Mean_cIMT_HIVneg,
# 	SD_cIMT_HIVneg, 
#   data=data_below40[-c(11,13),], 
#   sm="SMD",
#   # byvar=age,
#   # print.byvar=FALSE,
#   # comb.fixed=FALSE,
#   studlab=data_below40[-c(11,13),1])

# # metainf(meta1)
# metainf(meta_fig2age_below40_sensitivity, pooled="random")
# # metareg(meta_fig1, ~ age + ART_duration + gender)

# #funnel
# funnel(meta_fig2age_below40_sensitivity)

#############################################################################
#
#Figure 3 - by ART model
#
#############################################################################

data_ART<-subset(data,data$ART_duration!="")

#run metanalysis model for type of intervention
meta_fig1 <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data_ART, 
  sm="SMD",
  # byvar=intervention_cat,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_ART[,1])

data_ART$ART_duration<-as.character(data_ART$ART_duration)

#meta-regression
metareg(meta_fig1, ~ ART_duration)

#run metanalysis model for type of intervention
meta_fig2ART_duration <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data_ART, 
  sm="SMD",
  byvar=ART_duration,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_ART[,1])
# summary(meta1)

setEPS()
# tiff("/Users/joaovissoci/Desktop/dep3ession_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
postscript("/Users/Joao/Desktop/figure3_ART_duration_v2.eps",
	width = 14, height = 6)
forest(meta_fig2ART_duration,sortvar=meta_fig2ART_duration$TE,
					   bysort=FALSE,
					   digits.mean=2,
					   digits.sd=2,
					   layout="JAMA")

dev.off()

setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
postscript("/Users/Joao/Desktop/figure3_ART_duration.eps",
	width = 14, height = 8)
forest(meta_fig2ART_duration,sortvar=meta_fig2ART_duration$TE,
					   bysort=FALSE,
					   digits.mean=2,
					   digits.sd=2,
					   print.byvar=FALSE
)
dev.off()


# setEPS()
# # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# postscript("/Users/Joao/Desktop/Suppl_figure3_funnel.eps",
# 	width = 8, height = 8)
# funnel(meta_fig2ART_duration)
# dev.off()

# ART_duration above 5 sub-group
################################################################

#Age above 40 years

data_ARTabove5<-subset(data_ART,data_ART$ART_duration=="Duration of ART above  5 years")

data_ARTabove5$ART_duration<-as.character(data_ARTabove5$ART_duration)

#run metanalysis model for type of intervention
meta_fig2ART_duration_above5 <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data_ARTabove5, 
  sm="SMD",
  # byvar=ART_duration,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_ARTabove5[,1])

# metainf(meta1)
metainf(meta_fig2ART_duration_above5, pooled="random")
# metareg(meta_fig1, ~ ART_duration + ART_duration + gender)

#funnel
funnel(meta_fig2ART_duration_above5)

#run metanalysis model for ART_duration groups excluding Zormpala and Cristofaro

# ART_duration below 5 sub-group
################################################################

#ART_duration below 40 years

data_ART_duration_below5<-subset(data,data$ART_duration=="Duration of ART less_equal 5 years")

data_ART_duration_below5$ART_duration<-as.character(data_ART_duration_below5$ART_duration)

#run metanalysis model for type of intervention
meta_fig2ART_duration_below5 <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data_ART_duration_below5, 
  sm="SMD",
  # byvar=ART_duration,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_ART_duration_below5[,1])

# metainf(meta1)
metainf(meta_fig2ART_duration_below5, pooled="random")
# metareg(meta_fig1, ~ ART_duration + ART_duration + gender)

#funnel
funnel(meta_fig2ART_duration_below5)

# #run metanalysis model for ART_duration groups excluding Zormpala and Cristofaro
# meta_fig2ART_duration_below40_sensitivity <- metacont(n_HIV_pos,
# 	Mean_cIMT_HIVpos,
# 	SD_cIMT_HIVpos,
# 	n_HIV_neg,
# 	Mean_cIMT_HIVneg,
# 	SD_cIMT_HIVneg, 
#   data=data_below40[-c(11,13),], 
#   sm="SMD",
#   # byvar=ART_duration,
#   # print.byvar=FALSE,
#   # comb.fixed=FALSE,
#   studlab=data_below40[-c(11,13),1])

# # metainf(meta1)
# metainf(meta_fig2ART_duration_below40_sensitivity, pooled="random")
# # metareg(meta_fig1, ~ ART_duration + ART_duration + gender)

# #funnel
# funnel(meta_fig2age_below40_sensitivity)

#############################################################################
#
#Figure 4 - by GENDER model
#
#############################################################################

data_gender<-subset(data,data$gender!="")

#run metanalysis model for type of intervention
meta_fig1 <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data_gender, 
  sm="SMD",
  # byvar=intervention_cat,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_gender[,1])

#meta-regression
metareg(meta_fig1, ~ gender)

data_gender$gender<-as.character(data_gender$gender)

#run metanalysis model for type of intervention
meta_fig2gender <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data_gender, 
  sm="SMD",
  byvar=gender,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_gender[,1])
# summary(meta1)

setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
postscript("/Users/Joao/Desktop/figure4_gender.eps",
	width = 14, height = 12)
forest(meta_fig2gender,sortvar=meta_fig2gender$TE,
					   bysort=FALSE,
					   digits.mean=2,
					   digits.sd=2,
					   print.byvar=FALSE)
dev.off()

setEPS()
# tiff("/Users/Joao/Desktop/dep3ession_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
postscript("/Users/Joao/Desktop/figure4_gender_v2.eps",
	width = 14, height = 6)
forest(meta_fig2ART_duration,sortvar=meta_fig2ART_duration$TE,
					   bysort=FALSE,
					   digits.mean=2,
					   digits.sd=2,
					   layout="JAMA")

dev.off()

# gender sub-group
################################################################

#Male

data_gender_male<-subset(data_gender,data_gender$gender=="Male")

data_gender_male$gender<-as.character(data_gender_male$gender)

#run metanalysis model for type of intervention
meta_fig2gender <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data_gender_male, 
  sm="SMD",
  # byvar=gender,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_gender_male[,1])

# metainf(meta1)
metainf(meta_fig2gender, pooled="random")
# metareg(meta_fig1, ~ gender + gender + gender)

#funnel
funnel(meta_fig2gender)

#run metanalysis model for gender groups excluding Zormpala and Cristofaro

# gender below 5 sub-group
################################################################

#gender below 40 years

data_gender_female<-subset(data,data$gender=="Female")

data_gender_female$gender<-as.character(data_gender_female$gender)

#run metanalysis model for type of intervention
meta_fig2gender_female <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data_gender_female, 
  sm="SMD",
  # byvar=gender,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_gender_female[,1])

# metainf(meta1)
metainf(meta_fig2gender_female, pooled="random")
# metareg(meta_fig1, ~ gender + gender + gender)

#funnel
funnel(meta_fig2gender_female)

# #run metanalysis model for gender groups excluding Zormpala and Cristofaro
# meta_fig2gender_below40_sensitivity <- metacont(n_HIV_pos,
# 	Mean_cIMT_HIVpos,
# 	SD_cIMT_HIVpos,
# 	n_HIV_neg,
# 	Mean_cIMT_HIVneg,
# 	SD_cIMT_HIVneg, 
#   data=data_below40[-c(11,13),], 
#   sm="SMD",
#   # byvar=gender,
#   # print.byvar=FALSE,
#   # comb.fixed=FALSE,
#   studlab=data_below40[-c(11,13),1])

# # metainf(meta1)
# metainf(meta_fig2gender_below40_sensitivity, pooled="random")
# # metareg(meta_fig1, ~ gender + gender + gender)

# #funnel
# funnel(meta_fig2age_below40_sensitivity)

#############################################################################
#
#Figure 5 - by RACE model
#
#############################################################################

data_race<-subset(data,data$race!="")

#run metanalysis model for type of intervention
meta_fig1 <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data_race, 
  sm="SMD",
  # byvar=intervention_cat,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_race[,1])

#meta-regression
metareg(meta_fig1, ~ race)

data_race$race<-as.character(data_race$race)

#run metanalysis model for type of intervention
meta_fig2race <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data_race, 
  sm="SMD",
  byvar=race,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_race[,1])
# summary(meta1)

setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
postscript("/Users/Joao/Desktop/figure5_race.eps",
	width = 14, height = 12)
forest(meta_fig2race,sortvar=meta_fig2race$TE,
					   bysort=FALSE,
					   digits.mean=2,
					   digits.sd=2,
					   print.byvar=FALSE)
dev.off()

setEPS()
# tiff("/Users/Joao/Desktop/dep3ession_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
postscript("/Users/Joao/Desktop/figure5_race_v2.eps",
	width = 14, height = 6)
forest(meta_fig2ART_duration,sortvar=meta_fig2ART_duration$TE,
					   bysort=FALSE,
					   digits.mean=2,
					   digits.sd=2,
					   layout="JAMA")

dev.off()

# race sub-group
################################################################

#Male

data_race_africanamerican<-subset(data_race,data_race$race=="africanamerican")

data_race_africanamerican$race<-as.character(data_race_africanamerican$race)

#run metanalysis model for type of intervention
meta_fig2race_africanamerican <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data_race_africanamerican, 
  sm="SMD",
  # byvar=race,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_race_africanamerican[,1])

# metainf(meta1)
metainf(meta_fig2race_africanamerican, pooled="random")
# metareg(meta_fig1, ~ race + race + race)

#funnel
funnel(meta_fig2race_africanamerican)

# race below 5 sub-group
################################################################

#race below 40 years

data_race_caucasian<-subset(data,data$race=="caucasian")

data_race_caucasian$race<-as.character(data_race_caucasian$race)

#run metanalysis model for type of intervention
meta_fig2race_caucasian <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data_race_caucasian, 
  sm="SMD",
  # byvar=race,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_race_caucasian[,1])

# metainf(meta1)
metainf(meta_fig2race_caucasian, pooled="random")
# metareg(meta_fig1, ~ race + race + race)

#funnel
funnel(meta_fig2race_caucasian)

# #run metanalysis model for gender groups excluding Zormpala and Cristofaro
# meta_fig2gender_below40_sensitivity <- metacont(n_HIV_pos,
# 	Mean_cIMT_HIVpos,
# 	SD_cIMT_HIVpos,
# 	n_HIV_neg,
# 	Mean_cIMT_HIVneg,
# 	SD_cIMT_HIVneg, 
#   data=data_below40[-c(11,13),], 
#   sm="SMD",
#   # byvar=gender,
#   # print.byvar=FALSE,
#   # comb.fixed=FALSE,
#   studlab=data_below40[-c(11,13),1])

# # metainf(meta1)
# metainf(meta_fig2gender_below40_sensitivity, pooled="random")
# # metareg(meta_fig1, ~ gender + gender + gender)

# #funnel
# funnel(meta_fig2age_below40_sensitivity)

#############################################################################
#
#Figure 6 - by smoking model
#
#############################################################################

data_smoking<-subset(data,data$smoking!="")

#run metanalysis model for type of intervention
meta_fig1 <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data_smoking, 
  sm="SMD",
  # byvar=intervention_cat,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_smoking[,1])

#meta-regression
metareg(meta_fig1, ~ smoking)

data_smoking$smoking<-as.character(data_smoking$smoking)

#run metanalysis model for type of intervention
meta_fig2smoking <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data_smoking, 
  sm="SMD",
  byvar=smoking,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_smoking[,1])
# summary(meta1)

setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
postscript("/Users/Joao/Desktop/figure6_smoking.eps",
	width = 14, height = 12)
forest(meta_fig2smoking,sortvar=meta_fig2smoking$TE,
					   bysort=FALSE,
					   digits.mean=2,
					   digits.sd=2,
					   print.byvar=FALSE)
dev.off()

setEPS()
# tiff("/Users/Joao/Desktop/dep3ession_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
postscript("/Users/Joao/Desktop/figure6_smoking_v2.eps",
	width = 14, height = 6)
forest(meta_fig2ART_duration,sortvar=meta_fig2ART_duration$TE,
					   bysort=FALSE,
					   digits.mean=2,
					   digits.sd=2,
					   layout="JAMA")

dev.off()

# smoking sub-group
################################################################

#Male

data_smoking_nodiff<-subset(data_smoking,data_smoking$smoking=="No difference")

data_smoking_nodiff$smoking<-as.character(data_smoking_nodiff$smoking)

#run metanalysis model for type of intervention
meta_fig2smoking_nodiff <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data_smoking_nodiff, 
  sm="SMD",
  # byvar=smoking,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_smoking_nodiff[,1])

# metainf(meta1)
metainf(meta_fig2smoking_nodiff, pooled="random")
# metareg(meta_fig1, ~ smoking + smoking + smoking)

#funnel
funnel(meta_fig2smoking_nodiff)

# smoking below 5 sub-group
################################################################

#smoking below 40 years

data_smoking_sigdiff<-subset(data,data$smoking=="Sig. difference")

data_smoking_sigdiff$smoking<-as.character(data_smoking_sigdiff$smoking)

#run metanalysis model for type of intervention
meta_fig2smoking_sigdiff <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data_smoking_sigdiff, 
  sm="SMD",
  # byvar=smoking,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_smoking_sigdiff[,1])

# metainf(meta1)
metainf(meta_fig2smoking_sigdiff, pooled="random")
# metareg(meta_fig1, ~ smoking + smoking + smoking)

#funnel
funnel(meta_fig2smoking_sigdiff)

# #run metanalysis model for gender groups excluding Zormpala and Cristofaro
# meta_fig2gender_below40_sensitivity <- metacont(n_HIV_pos,
# 	Mean_cIMT_HIVpos,
# 	SD_cIMT_HIVpos,
# 	n_HIV_neg,
# 	Mean_cIMT_HIVneg,
# 	SD_cIMT_HIVneg, 
#   data=data_below40[-c(11,13),], 
#   sm="SMD",
#   # byvar=gender,
#   # print.byvar=FALSE,
#   # comb.fixed=FALSE,
#   studlab=data_below40[-c(11,13),1])

# # metainf(meta1)
# metainf(meta_fig2gender_below40_sensitivity, pooled="random")
# # metareg(meta_fig1, ~ gender + gender + gender)

# #funnel
# funnel(meta_fig2age_below40_sensitivity)

