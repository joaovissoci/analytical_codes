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
data<-read.csv("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/Africa/tz_hiv_SR.csv",sep=',')
#information between " " are the path to the directory in your computer where the data is stored

#############################################################################
#DATA MANAGEMENT
#############################################################################

#############################################################################
#Calculating diff scores
#############################################################################

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

# setEPS()
# # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# postscript("/Users/Joao/Desktop/figure1_overall.eps",
# 	width = 14, height = 8)
# forest(meta_fig1,sortvar=meta_fig1$TE,
# 					   bysort=FALSE,
# 					   digits.mean=2,
# 					   digits.sd=2)
# dev.off()

setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
postscript("/Users/joaovissoci/Desktop/figure1_overall_v2.eps",
	width = 14, height = 8)
forest(meta_fig1,sortvar=meta_fig1$TE,
					   bysort=FALSE,
					   digits.mean=2,
					   digits.sd=2,
					   layout="JAMA")
dev.off()

# setEPS()
# # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# postscript("/Users/Joao/Desktop/Suppl_figure1_funnel.eps",
# 	width = 8, height = 8)
# funnel(meta_fig1)
# dev.off()

# metainf(meta1)
# metainf(meta_fig1, pooled="random")

#Meta regression



#############################################################################
#
#Figure 2 - by RNA
#
#############################################################################

data_RNA<-subset(data,data$RNA!="")

data_RNA$RNA <- factor(data_RNA$RNA)

# metareg(meta_fig1, ~ RNA)

#run metanalysis model for type of intervention
meta_fig2rna <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data_RNA, 
  sm="SMD",
  byvar=RNA,
  print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_RNA[,1])
# summary(meta1)

#JAMA version option
setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
postscript("/Users/joaovissoci/Desktop/figure_RNA.eps",
	width = 8, height = 6)
forest(meta_fig2rna,sortvar=meta_fig2rna$TE,
					   bysort=FALSE,
					   digits.mean=2,
					   digits.sd=2,
					   layout="JAMA")

dev.off()

#Metaregression
levels(data_RNA$RNA)
metareg(meta_fig2rna, ~ RNA)

#Default package option
# setEPS()
# # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# postscript("/Users/Joao/Desktop/figure2_age.eps",
# 	width = 14, height = 8)
# forest(meta_fig2age,sortvar=meta_fig2age$TE,
# 					   bysort=FALSE,
# 					   digits.mean=2,
# 					   digits.sd=2,
# 					   print.byvar=FALSE
# )
# dev.off()


# setEPS()
# # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# postscript("/Users/Joao/Desktop/Suppl_figure1_POST_funnel.eps",
# 	width = 8, height = 8)
# funnel(meta_fig2age)
# dev.off()

# RNA: Mean RNA above 50 copies/ml 
################################################################

#Mean RNA above 50 copies/ml 
data_RNAabove<-subset(data_RNA,data_RNA$RNA=="Mean RNA 50 and above copies/ml")

#run metanalysis model for type of intervention
meta_fig2_RNAabove <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data_RNAabove, 
  sm="SMD",
  # byvar=age,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_RNAabove[,1])

# metainf(meta1)
metainf(meta_fig2_RNAabove, pooled="random")
# metareg(meta_fig1, ~ age + ART_duration + gender)

# setEPS()
# # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# postscript("/Users/joaovissoci/Desktop/figure_RNAfunnel.eps",
# 	width = 8, height = 6)
# #funnel
# funnel(meta_fig2_RNAabove)
# dev.off()

#############################################################################
#
#Figure 3 - by ART model
#
#############################################################################

data_ART<-subset(data,data$ART_duration!="")

data_ART$ART_duration <- factor(data_ART$ART_duration)

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

# data_ART$ART_duration<-as.character(data_ART$ART_duration)

#meta-regression
levels(data_ART$ART_duration)
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
  print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_ART[,1])
# summary(meta1)

setEPS()
# tiff("/Users/joaovissoci/Desktop/dep3ession_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
postscript("/Users/joaovissoci/Desktop/figure_ART_duration.eps",
	width = 8, height = 6)
forest(meta_fig2ART_duration,sortvar=meta_fig2ART_duration$TE,
					   bysort=FALSE,
					   digits.mean=2,
					   digits.sd=2,
					   layout="JAMA")

dev.off()

# setEPS()
# # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# postscript("/Users/Joao/Desktop/figure3_ART_duration.eps",
# 	width = 14, height = 8)
# forest(meta_fig2ART_duration,sortvar=meta_fig2ART_duration$TE,
# 					   bysort=FALSE,
# 					   digits.mean=2,
# 					   digits.sd=2,
# 					   print.byvar=FALSE
# )
# dev.off()


# setEPS()
# # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# postscript("/Users/Joao/Desktop/Suppl_figure3_funnel.eps",
# 	width = 8, height = 8)
# funnel(meta_fig2ART_duration)
# dev.off()

# Duration of HIV above 10 years
################################################################

#Age above 40 years

data_ARTabove10<-subset(data_ART,data_ART$ART_duration=="Duration of HIV above 10 years")

data_ARTabove10$ART_duration<-as.character(data_ARTabove10$ART_duration)

#run metanalysis model for type of intervention
meta_fig2ART_duration_above5 <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data_ARTabove10, 
  sm="SMD",
  # byvar=ART_duration,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_ARTabove10[,1])

# metainf(meta1)
metainf(meta_fig2ART_duration_above5, pooled="random")

#sensitivity analysis results
#Omitting Hsue et al. 2012          67.0%

#funnel
funnel(meta_fig2ART_duration_above5)

#run metanalysis model for ART_duration groups excluding Zormpala and Cristofaro

# Duration of HIV below 10 years
################################################################

#ART_duration below 40 years

data_ART_duration_below10<-subset(data,data$ART_duration=="Duration of HIV below or equal to 10 years")

data_ART_duration_below10$ART_duration<-as.character(data_ART_duration_below10$ART_duration)

#run metanalysis model for type of intervention
meta_fig2ART_duration_below5 <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data_ART_duration_below10, 
  sm="SMD",
  # byvar=ART_duration,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_ART_duration_below10[,1])

# metainf(meta1)
metainf(meta_fig2ART_duration_below5, pooled="random")

#sensitivity
#no relevant changes

#funnel
funnel(meta_fig2ART_duration_below5)

#############################################################################
#
#Figure 4 - by GENDER model
#
#############################################################################

data_gender<-subset(data,data$gender!="")

data_gender$gender <- factor(data_gender$gender)

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
  print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_gender[,1])
# summary(meta1)

# setEPS()
# # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# postscript("/Users/joaovissoci/Desktop/figure_gender.eps",
# 	width = 14, height = 12)
# forest(meta_fig2gender,sortvar=meta_fig2gender$TE,
# 					   bysort=FALSE,
# 					   digits.mean=2,
# 					   digits.sd=2,
# 					   print.byvar=FALSE)
# dev.off()

setEPS()
# tiff("/Users/Joao/Desktop/dep3ession_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
postscript("/Users/joaovissoci/Desktop/figure_gender.eps",
	width = 8, height = 10)
forest(meta_fig2gender,sortvar=meta_fig2gender$TE,
					   bysort=FALSE,
					   digits.mean=2,
					   digits.sd=2,
					   layout="JAMA")

dev.off()

# Gender: Male predominance (%male difference<0)
################################################################

#Male

data_gender_male<-subset(data_gender,data_gender$gender=="Male predominant (%male difference>0)")

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

#Male Ommiting Monroe et al. 2012

data_gender_male2<-data_gender_male[-4,]

#run metanalysis model for type of intervention
meta_fig2gender <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data_gender_male2, 
  sm="SMD",
  # byvar=gender,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_gender_male2[,1])

# metainf(meta1)
metainf(meta_fig2gender, pooled="random")
# metareg(meta_fig1, ~ gender + gender + gender)

#funnel
funnel(meta_fig2gender)

#no change was significant

# Gender: Female predominance (%male difference<0)
################################################################

#Male

data_gender_female<-subset(data_gender,data_gender$gender=="Female predominance (%male difference<0)")

data_gender_female$gender<-as.character(data_gender_female$gender)

#run metanalysis model for type of intervention
meta_fig2gender <- metacont(n_HIV_pos,
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
metainf(meta_fig2gender, pooled="random")
# metareg(meta_fig1, ~ gender + gender + gender)

#funnel
funnel(meta_fig2gender)

#no change was significant

# Gender: Equal proportion (%male difference=0)
################################################################

#gender below 40 years

data_gender_equal<-subset(data,data$gender=="Equal proportion (%male difference=0)")

data_gender_equal$gender<-as.character(data_gender_equal$gender)

#run metanalysis model for type of intervention
meta_fig2gender_female <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data_gender_equal, 
  sm="SMD",
  # byvar=gender,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_gender_equal[,1])

# metainf(meta1)
metainf(meta_fig2gender_female, pooled="random")
# metareg(meta_fig1, ~ gender + gender + gender)

#omitting Omitting Vigano et al. 2012 0.1098 [-0.0014; 0.2210]  0.0529  0.0000 0.0%

#funnel
funnel(meta_fig2gender_female)

#############################################################################
#
#Figure 5 - by RACE model
#
#############################################################################

data_race<-subset(data,data$race!="")

data_race$race <- factor(data_race$race)

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

# data_race$race<-as.character(data_race$race)

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
  print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_race[,1])
# summary(meta1)

# setEPS()
# # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# postscript("/Users/joaovissoci/Desktop/figure_race.eps",
# 	width = 14, height = 12)
# forest(meta_fig2race,sortvar=meta_fig2race$TE,
# 					   bysort=FALSE,
# 					   digits.mean=2,
# 					   digits.sd=2,
# 					   print.byvar=FALSE)
# dev.off()

setEPS()
# tiff("/Users/Joao/Desktop/dep3ession_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
postscript("/Users/joaovissoci/Desktop/figure_race.eps",
	width = 8, height = 6)
forest(meta_fig2race,sortvar=meta_fig2race$TE,
					   bysort=FALSE,
					   digits.mean=2,
					   digits.sd=2,
					   layout="JAMA")

dev.off()

#Race: African origin predominant (%African origin  difference>0)
################################################################

#Male

data_race_caucassian<-subset(data_race,data_race$race=="Caucassian/others predominance (%African origin<0) ")

data_race_caucassian$race<-as.character(data_race_caucassian$race)

#run metanalysis model for type of intervention
meta_fig2race_caucassian <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data_race_caucassian, 
  sm="SMD",
  # byvar=race,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_race_caucassian[,1])

# metainf(meta1)
metainf(meta_fig2race_caucassian, pooled="random")

#Omitting Monroe et al. 2012 0.3889 [ 0.0275; 0.7502]  0.0349  0.1097 81.9%
#Omitting Hsue et al. 2012           0.1373 [-0.1670; 0.4416]  0.3764  0.0757 82.8%

#funnel
funnel(meta_fig2race_africanamerican)

#############################################################################
#
#Figure 6 - by smoking model
#
#############################################################################

data_smoking<-subset(data,data$smoking!="")

data_smoking$smoking <- factor(data_smoking$smoking)

data_smoking$smoking2 <- ifelse(data_smoking$smoking=="Equal proportion between smokers and non-smokers (%smokers difference=0)", 
										"Non-smokers predominance (%Smokers difference<0)", 
										ifelse(data_smoking$smoking=="Non-smokers predominance (%Smokers difference<0)", 
											"Non-smokers predominance (%Smokers difference<0)",
											"Smokers predominant (%smokers  difference>0)"))



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
metareg(meta_fig1, ~ smoking2)

# data_smoking$smoking<-as.character(data_smoking$smoking)

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
  print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_smoking[,1])
# summary(meta1)

# setEPS()
# # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# postscript("/Users/Joao/Desktop/figure6_smoking.eps",
# 	width = 14, height = 12)
# forest(meta_fig2smoking,sortvar=meta_fig2smoking$TE,
# 					   bysort=FALSE,
# 					   digits.mean=2,
# 					   digits.sd=2,
# 					   print.byvar=FALSE)
# dev.off()

setEPS()
# tiff("/Users/Joao/Desktop/dep3ession_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
postscript("/Users/joaovissoci/Desktop/figure_smoking.eps",
	width = 8, height = 8)
forest(meta_fig2smoking,sortvar=meta_fig2smoking$TE,
					   bysort=FALSE,
					   digits.mean=2,
					   digits.sd=2,
					   layout="JAMA")

dev.off()

# smoking: Smokers predominant (%smokers  difference>0)
################################################################

#Male

data_smoking_smokers<-subset(data_smoking,data_smoking$smoking=="Smokers predominant (%smokers  difference>0)")

data_smoking_smokers$smoking<-as.character(data_smoking_smokers$smoking)

#run metanalysis model for type of intervention
meta_fig2smoking_nodiff <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data_smoking_smokers, 
  sm="SMD",
  # byvar=smoking,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_smoking_smokers[,1])

# metainf(meta1)
metainf(meta_fig2smoking_nodiff, pooled="random")

#Omitting Monroe et al. 2012 0.6415 [0.3403; 0.9427] < 0.0001  0.1473 82.3%

#funnel
funnel(meta_fig2smoking_nodiff)

#Smoker - Monroe

data_smoking_smokers_2<-subset(data_smoking_smokers,data_smoking_smokers[,1]!="Monroe et al. 2012")

# data_smoking_smokers$smoking<-as.character(data_smoking_smokers$smoking)

#run metanalysis model for type of intervention
meta_fig2smoking_nodiff2 <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data_smoking_smokers_2, 
  sm="SMD",
  # byvar=smoking,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_smoking_smokers_2[,1])

# metainf(meta1)
metainf(meta_fig2smoking_nodiff2, pooled="random")

#Omitting Mondy et al. 2008 0.7438 [0.4695; 1.0181] < 0.0001  0.0969 75.8%

#funnel
funnel(meta_fig2smoking_nodiff2)

#############################################################################
#
#Figure 6 - by current_CD_4
#
#############################################################################

data_current_CD_4<-subset(data,data$current_CD_4!="")

data_current_CD_4$current_CD_4 <- factor(data_current_CD_4$current_CD_4)

#run metanalysis model for type of intervention
meta_fig1 <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data_current_CD_4, 
  sm="SMD",
  # byvar=intervention_cat,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_current_CD_4[,1])

#meta-regression
metareg(meta_fig1, ~ current_CD_4)

# data_current_CD_4$current_CD_4<-as.character(data_current_CD_4$current_CD_4)

#run metanalysis model for type of intervention
meta_fig2current_CD_4 <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data_current_CD_4, 
  sm="SMD",
  byvar=current_CD_4,
  print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_current_CD_4[,1])
# summary(meta1)

# setEPS()
# # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# postscript("/Users/Joao/Desktop/figure6_current_CD_4.eps",
# 	width = 14, height = 12)
# forest(meta_fig2current_CD_4,sortvar=meta_fig2current_CD_4$TE,
# 					   bysort=FALSE,
# 					   digits.mean=2,
# 					   digits.sd=2,
# 					   print.byvar=FALSE)
# dev.off()

setEPS()
# tiff("/Users/Joao/Desktop/dep3ession_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
postscript("/Users/joaovissoci/Desktop/figure_current_CD_4.eps",
	width = 8, height = 6)
forest(meta_fig2current_CD_4,sortvar=meta_fig2current_CD_4$TE,
					   bysort=FALSE,
					   digits.mean=2,
					   digits.sd=2,
					   layout="JAMA")

dev.off()

# Current CD4: Mean current CD4 above 500 cells/mL
################################################################

data_currentCD4_above<-subset(data_current_CD_4,data_current_CD_4$current_CD_4=="Mean current CD4 500 or above")

data_currentCD4_above$current_CD_4<-as.character(data_currentCD4_above$current_CD_4)

#run metanalysis model for type of intervention
meta_fig2current_CD_4_nodiff <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data_currentCD4_above, 
  sm="SMD",
  # byvar=current_CD_4,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_currentCD4_above[,1])

# metainf(meta1)
metainf(meta_fig2current_CD_4_nodiff, pooled="random")

#Omitting Jung et al. 2009 0.9144 [ 0.6717; 1.1570] < 0.0001  0.0143 45.5%

#funnel
funnel(meta_fig2current_CD_4_nodiff)

# Current CD4: Mean current CD4 below 500 cells/uL
################################################################

data_currentCD4_below<-subset(data_current_CD_4,data_current_CD_4$current_CD_4=="Mean current CD4 below 500")

data_currentCD4_below$current_CD_4<-as.character(data_currentCD4_below$current_CD_4)

#run metanalysis model for type of intervention
meta_fig2current_CD_4_nodiff <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data_currentCD4_below, 
  sm="SMD",
  # byvar=current_CD_4,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_currentCD4_below[,1])

# metainf(meta1)
metainf(meta_fig2current_CD_4_nodiff, pooled="random")

#Omitting Zormpala et al. 2012 0.0760 [-0.1227; 0.2747]  0.4534  0.0433 68.2%

#funnel
funnel(meta_fig2current_CD_4_nodiff)

#############################################################################
#
#Figure 6 - by mean_CD_4_nadir
#
#############################################################################

data_mean_CD_4_nadir<-subset(data,data$mean_CD_4_nadir!="")

data_mean_CD_4_nadir$mean_CD_4_nadir <- factor(data_mean_CD_4_nadir$mean_CD_4_nadir)

#run metanalysis model for type of intervention
meta_fig1 <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data_mean_CD_4_nadir, 
  sm="SMD",
  # byvar=intervention_cat,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_mean_CD_4_nadir[,1])

#meta-regression
metareg(meta_fig1, ~ mean_CD_4_nadir)

# data_mean_CD_4_nadir$mean_CD_4_nadir<-as.character(data_mean_CD_4_nadir$mean_CD_4_nadir)

#run metanalysis model for type of intervention
meta_fig2mean_CD_4_nadir <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data_mean_CD_4_nadir, 
  sm="SMD",
  byvar=mean_CD_4_nadir,
  print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_mean_CD_4_nadir[,1])
# summary(meta1)

# setEPS()
# # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# postscript("/Users/Joao/Desktop/figure6_mean_CD_4_nadir.eps",
# 	width = 14, height = 12)
# forest(meta_fig2mean_CD_4_nadir,sortvar=meta_fig2mean_CD_4_nadir$TE,
# 					   bysort=FALSE,
# 					   digits.mean=2,
# 					   digits.sd=2,
# 					   print.byvar=FALSE)
# dev.off()

setEPS()
# tiff("/Users/Joao/Desktop/dep3ession_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
postscript("/Users/joaovissoci/Desktop/figure_mean_CD_4_nadir.eps",
	width = 8, height = 6)
forest(meta_fig2mean_CD_4_nadir,sortvar=meta_fig2mean_CD_4_nadir$TE,
					   bysort=FALSE,
					   digits.mean=2,
					   digits.sd=2,
					   layout="JAMA")

dev.off()

# Nadir CD4: Mean nadir CD4 below 200 cells/mL
################################################################

data_nadirCD4_above<-subset(data_mean_CD_4_nadir,data_mean_CD_4_nadir$mean_CD_4_nadir=="Mean nadir CD4 below or equal to 200")

data_nadirCD4_above$mean_CD_4_nadir<-as.character(data_nadirCD4_above$mean_CD_4_nadir)

#run metanalysis model for type of intervention
meta_fig2mean_CD_4_nadir_nodiff <- metacont(n_HIV_pos,
	Mean_cIMT_HIVpos,
	SD_cIMT_HIVpos,
	n_HIV_neg,
	Mean_cIMT_HIVneg,
	SD_cIMT_HIVneg, 
  data=data_nadirCD4_above, 
  sm="SMD",
  # byvar=mean_CD_4_nadir,
  # print.byvar=FALSE,
  # comb.fixed=FALSE,
  studlab=data_nadirCD4_above[,1])

# metainf(meta1)
metainf(meta_fig2mean_CD_4_nadir_nodiff, pooled="random")

#Omitting van Vonderen et al. 2009 0.4779 [0.0729; 0.8829]   0.0207  0.1066 0.0%

#funnel
funnel(meta_fig2mean_CD_4_nadir_nodiff)
