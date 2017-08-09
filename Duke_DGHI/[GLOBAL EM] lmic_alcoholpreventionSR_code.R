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
data<-read.csv("/Users/jnv4/Box Sync/Home Folder jnv4/Data/Global EM/US/GIC 2017/lmic_alcoholpreventionSR_data.csv")
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


by(data$category_intervention,data$outcome_cat, summary)

# % abstainers - OK
# abstinent days - OK
# binge drinking
# drinking per drinking days - OK
# drinking days - OK
# dinks per days - OK
# heavy drinking
# heavy drinking days - OK
# score - OK

#############################################################################
#Figure. 1 % abstainers
#############################################################################
## Suicide ideation metanalysis model
# #extracting studies with suicide ideation measures
meta_abstainers1<-subset(data,
	data$outcome_cat=="% abstainers")# &
	# data$fup_time=="3 monhts" | 
	# data$outcome_cat=="% abstainers" &
	# data$fup_time=="6 months")

meta_abstainers2<-subset(meta_abstainers1,
	meta_abstainers1$fup_time=="6 months" |
	meta_abstainers1$fup_time=="3 months")

meta_abstainers<-with(meta_abstainers2,
	data.frame(N_experimental_FUP1,
			   proportion_FUP1_experimental_group,
			   N_control_FUP1,
			   proportion_FUP1_control_group,
			   category_intervention,
			   type_intervention,
			   study,
			   fup_time,
			   outcome_cat))

# exclude variables for prevalences instead of scale results
# meta_bssi<-remove.vars(meta_bssi,c(
# 	"proportion_FUP1_experimental_group",
# 	"proportion_FUP1_control_group"))

# excluding missing information
meta_abstainers<-na.omit(meta_abstainers)

#Adjusting to avoind the error of a missing category in
#the byvar analysis
meta_abstainers<-as.matrix(meta_abstainers)
meta_abstainers<-as.data.frame(meta_abstainers)

meta_abstainers$N_experimental_FUP1<-as.numeric(
	as.character(meta_abstainers$N_experimental_FUP1)) 
meta_abstainers$proportion_FUP1_experimental_group<-as.numeric(
	as.character(meta_abstainers$proportion_FUP1_experimental_group))
meta_abstainers$N_control_FUP1<-as.numeric(
	as.character(meta_abstainers$N_control_FUP1))
meta_abstainers$proportion_FUP1_control_group<-as.numeric(
	as.character(meta_abstainers$proportion_FUP1_control_group))

# #recoding metanalysis groups
# meta_bssi$intervention<-car::recode(meta_bssi$intervention,"
# 	'targeted education awarenes'='TEA or BI';
# 	'brief intervention and contact'='TEA or BI';
# 	'psychotherapy'='Psychotherapy'")

# #run metanalysis model for type of intervention
# meta1 <- metabin( 
# 	proportion_FUP1_experimental_group,
# 	N_experimental_FUP1,
# 	proportion_FUP1_control_group,
# 	N_control_FUP1,
#   	data=meta_abstainers,
#   # byvar=Categorization,print.byvar=TRUE,
#   comb.fixed=TRUE,studlab=study)
# meta1

# tiff("/Users/jnv4/Desktop/GIC2017_figure1.tiff",
#   width = 1200, height = 800,compression = 'lzw')
# forest(meta1)
# dev.off()

# funnel(meta1)
# metabias(meta1)
# metainf(meta1)
# metainf(meta1, pooled="random")

#BY GROUP ANALYSIS
#run metanalysis model for type of intervention
meta1 <- metabin( 
	proportion_FUP1_experimental_group,
	N_experimental_FUP1,
	proportion_FUP1_control_group,
	N_control_FUP1,
  	data=meta_abstainers,
  byvar=category_intervention,print.byvar=TRUE,
  comb.fixed=TRUE,studlab=study)
meta1

tiff("/Users/jnv4/Desktop/GIC2017_figure1.tiff",
  width = 1200, height = 800,compression = 'lzw')
forest(meta1)
dev.off()

funnel(meta1)
metainf(meta1)
metainf(meta1, pooled="random")
# metareg(meta1, ~ Categorization)


#############################################################################
#Figure. 2 abstinent days
#############################################################################
## Suicide ideation metanalysis model
# #extracting studies with suicide ideation measures
## Suicide ideation metanalysis model
# #extracting studies with suicide ideation measures
meta_abstainers1<-subset(data,
	data$outcome_cat=="abstient days")# &
	# data$fup_time=="3 monhts" | 
	# data$outcome_cat=="% abstainers" &
	# data$fup_time=="6 months")

meta_abstainers2<-subset(meta_abstainers1,
	# meta_abstainers1$fup_time=="6 months" |
	meta_abstainers1$fup_time=="3 months")

meta_abstainers<-with(meta_abstainers2,
	data.frame(N_experimental_FUP1,
			   mean_FUP1_experimental_group,
			   sd_FUP1_experimental_group,
			   N_control_FUP1,
			   mean_FUP1_control_group,
			   sd_FUP1_control_group,
			   category_intervention,
			   type_intervention,
			   study,
			   fup_time,
			   outcome_cat))

# excluding missing information
meta_abstainers<-na.omit(meta_abstainers)

#Adjusting to avoind the error of a missing category in
#the byvar analysis
meta_abstainers<-as.matrix(meta_abstainers)
meta_abstainers<-as.data.frame(meta_abstainers)

meta_abstainers$N_experimental_FUP1<-as.numeric(
	as.character(meta_abstainers$N_experimental_FUP1)) 
meta_abstainers$mean_FUP1_experimental_group<-as.numeric(
	as.character(meta_abstainers$mean_FUP1_experimental_group))
meta_abstainers$sd_FUP1_experimental_group<-as.numeric(
	as.character(meta_abstainers$sd_FUP1_experimental_group))
meta_abstainers$N_control_FUP1<-as.numeric(
	as.character(meta_abstainers$N_control_FUP1))
meta_abstainers$mean_FUP1_control_group<-as.numeric(
	as.character(meta_abstainers$mean_FUP1_control_group))
meta_abstainers$sd_FUP1_control_group<-as.numeric(
	as.character(meta_abstainers$sd_FUP1_control_group))


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
  data=meta_abstainers, sm="SMD",
  byvar=category_intervention,print.byvar=TRUE,
  comb.fixed=TRUE,studlab=study)
summary(meta1)

tiff("/Users/jnv4/Desktop/GIC2017_figure2.tiff",
  width = 1200, height = 800,compression = 'lzw')
forest(meta1)
dev.off()

funnel(meta1)
metabias(meta1)
metainf(meta1)
metainf(meta1, pooled="random")
metareg(meta1, ~ Categorization)

#############################################################################
#Figure. 3 drinking days
#############################################################################
## Suicide ideation metanalysis model
# #extracting studies with suicide ideation measures
## Suicide ideation metanalysis model
# #extracting studies with suicide ideation measures
meta_abstainers1<-subset(data,
	data$outcome_cat=="drinking days")# &
	# data$fup_time=="3 monhts" | 
	# data$outcome_cat=="% abstainers" &
	# data$fup_time=="6 months")

meta_abstainers2<-subset(meta_abstainers1,
	meta_abstainers1$fup_time=="6 months" |
	meta_abstainers1$fup_time=="3 months")

meta_abstainers<-with(meta_abstainers2,
	data.frame(N_experimental_FUP1,
			   mean_FUP1_experimental_group,
			   sd_FUP1_experimental_group,
			   N_control_FUP1,
			   mean_FUP1_control_group,
			   sd_FUP1_control_group,
			   category_intervention,
			   type_intervention,
			   study,
			   fup_time,
			   outcome_cat))

# excluding missing information
meta_abstainers<-na.omit(meta_abstainers)

#Adjusting to avoind the error of a missing category in
#the byvar analysis
meta_abstainers<-as.matrix(meta_abstainers)
meta_abstainers<-as.data.frame(meta_abstainers)

meta_abstainers$N_experimental_FUP1<-as.numeric(
	as.character(meta_abstainers$N_experimental_FUP1)) 
meta_abstainers$mean_FUP1_experimental_group<-as.numeric(
	as.character(meta_abstainers$mean_FUP1_experimental_group))
meta_abstainers$sd_FUP1_experimental_group<-as.numeric(
	as.character(meta_abstainers$sd_FUP1_experimental_group))
meta_abstainers$N_control_FUP1<-as.numeric(
	as.character(meta_abstainers$N_control_FUP1))
meta_abstainers$mean_FUP1_control_group<-as.numeric(
	as.character(meta_abstainers$mean_FUP1_control_group))
meta_abstainers$sd_FUP1_control_group<-as.numeric(
	as.character(meta_abstainers$sd_FUP1_control_group))


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
  data=meta_abstainers, sm="SMD",
  byvar=category_intervention,print.byvar=TRUE,
  comb.fixed=TRUE,studlab=study)
summary(meta1)

tiff("/Users/jnv4/Desktop/GIC2017_figure3.tiff",
  width = 1200, height = 800,compression = 'lzw')
forest(meta1)
dev.off()

funnel(meta1)
metabias(meta1)
metainf(meta1)
metainf(meta1, pooled="random")
metareg(meta1, ~ Categorization)

#############################################################################
#Figure. 4 drinking per drinking days
#############################################################################
## Suicide ideation metanalysis model
# #extracting studies with suicide ideation measures
## Suicide ideation metanalysis model
# #extracting studies with suicide ideation measures
meta_abstainers1<-subset(data,
	data$outcome_cat=="drink per drinking days")# &
	# data$fup_time=="3 monhts" | 
	# data$outcome_cat=="% abstainers" &
	# data$fup_time=="6 months")

meta_abstainers2<-subset(meta_abstainers1,
	# meta_abstainers1$fup_time=="6 months" |
	meta_abstainers1$fup_time=="3 months")

meta_abstainers<-with(meta_abstainers2,
	data.frame(N_experimental_FUP1,
			   mean_FUP1_experimental_group,
			   sd_FUP1_experimental_group,
			   N_control_FUP1,
			   mean_FUP1_control_group,
			   sd_FUP1_control_group,
			   category_intervention,
			   type_intervention,
			   study,
			   fup_time,
			   outcome_cat))

# excluding missing information
meta_abstainers<-na.omit(meta_abstainers)

#Adjusting to avoind the error of a missing category in
#the byvar analysis
meta_abstainers<-as.matrix(meta_abstainers)
meta_abstainers<-as.data.frame(meta_abstainers)

meta_abstainers$N_experimental_FUP1<-as.numeric(
	as.character(meta_abstainers$N_experimental_FUP1)) 
meta_abstainers$mean_FUP1_experimental_group<-as.numeric(
	as.character(meta_abstainers$mean_FUP1_experimental_group))
meta_abstainers$sd_FUP1_experimental_group<-as.numeric(
	as.character(meta_abstainers$sd_FUP1_experimental_group))
meta_abstainers$N_control_FUP1<-as.numeric(
	as.character(meta_abstainers$N_control_FUP1))
meta_abstainers$mean_FUP1_control_group<-as.numeric(
	as.character(meta_abstainers$mean_FUP1_control_group))
meta_abstainers$sd_FUP1_control_group<-as.numeric(
	as.character(meta_abstainers$sd_FUP1_control_group))


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
  data=meta_abstainers, sm="SMD",
  byvar=category_intervention,print.byvar=TRUE,
  comb.fixed=TRUE,studlab=study)
summary(meta1)

tiff("/Users/jnv4/Desktop/GIC2017_figure4.tiff",
  width = 1200, height = 800,compression = 'lzw')
forest(meta1)
dev.off()

funnel(meta1)
metabias(meta1)
metainf(meta1)
metainf(meta1, pooled="random")
metareg(meta1, ~ Categorization)

#############################################################################
#Figure 5. drinks per day
#############################################################################
## Suicide ideation metanalysis model
# #extracting studies with suicide ideation measures
## Suicide ideation metanalysis model
# #extracting studies with suicide ideation measures
meta_abstainers1<-subset(data,
	data$outcome_cat=="drinks per day")# &
	# data$fup_time=="3 monhts" | 
	# data$outcome_cat=="% abstainers" &
	# data$fup_time=="6 months")

meta_abstainers2<-subset(meta_abstainers1,
	meta_abstainers1$fup_time=="12 monts" |
	meta_abstainers1$fup_time=="3 months" |
	meta_abstainers1$fup_time=="6 months")

meta_abstainers<-with(meta_abstainers2,
	data.frame(N_experimental_FUP1,
			   mean_FUP1_experimental_group,
			   sd_FUP1_experimental_group,
			   N_control_FUP1,
			   mean_FUP1_control_group,
			   sd_FUP1_control_group,
			   type_intervention,
			   study,
			   fup_time,
			   outcome_cat,
			   category_intervention))

# excluding missing information
meta_abstainers<-na.omit(meta_abstainers)

#Adjusting to avoind the error of a missing category in
#the byvar analysis
meta_abstainers<-as.matrix(meta_abstainers)
meta_abstainers<-as.data.frame(meta_abstainers)

meta_abstainers$N_experimental_FUP1<-as.numeric(
	as.character(meta_abstainers$N_experimental_FUP1)) 
meta_abstainers$mean_FUP1_experimental_group<-as.numeric(
	as.character(meta_abstainers$mean_FUP1_experimental_group))
meta_abstainers$sd_FUP1_experimental_group<-as.numeric(
	as.character(meta_abstainers$sd_FUP1_experimental_group))
meta_abstainers$N_control_FUP1<-as.numeric(
	as.character(meta_abstainers$N_control_FUP1))
meta_abstainers$mean_FUP1_control_group<-as.numeric(
	as.character(meta_abstainers$mean_FUP1_control_group))
meta_abstainers$sd_FUP1_control_group<-as.numeric(
	as.character(meta_abstainers$sd_FUP1_control_group))


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
  data=meta_abstainers[-c(4,6),], sm="SMD",
  byvar=category_intervention,print.byvar=TRUE,
  comb.fixed=TRUE,studlab=study)
summary(meta1)

tiff("/Users/jnv4/Desktop/GIC2017_figure5.tiff",
  width = 1200, height = 800,compression = 'lzw')
forest(meta1)
dev.off()

funnel(meta1)
metabias(meta1)
metainf(meta1)
metainf(meta1, pooled="random")
metareg(meta1, ~ Categorization)

#############################################################################
#Figure 6. heavy drinking days
#############################################################################
## Suicide ideation metanalysis model
# #extracting studies with suicide ideation measures
## Suicide ideation metanalysis model
# #extracting studies with suicide ideation measures
meta_abstainers1<-subset(data,
	data$outcome_cat=="heavy drinking days")# &
	# data$fup_time=="3 monhts" | 
	# data$outcome_cat=="% abstainers" &
	# data$fup_time=="6 months")

meta_abstainers2<-subset(meta_abstainers1,
	# meta_abstainers1$fup_time=="12 monts" |
	meta_abstainers1$fup_time=="3 months" |
	meta_abstainers1$fup_time=="6 months")

meta_abstainers<-with(meta_abstainers2,
	data.frame(N_experimental_FUP1,
			   mean_FUP1_experimental_group,
			   sd_FUP1_experimental_group,
			   N_control_FUP1,
			   mean_FUP1_control_group,
			   sd_FUP1_control_group,
			   type_intervention,
			   study,
			   fup_time,
			   outcome_cat,
			   category_intervention))

# excluding missing information
meta_abstainers<-na.omit(meta_abstainers)

#Adjusting to avoind the error of a missing category in
#the byvar analysis
meta_abstainers<-as.matrix(meta_abstainers)
meta_abstainers<-as.data.frame(meta_abstainers)

meta_abstainers$N_experimental_FUP1<-as.numeric(
	as.character(meta_abstainers$N_experimental_FUP1)) 
meta_abstainers$mean_FUP1_experimental_group<-as.numeric(
	as.character(meta_abstainers$mean_FUP1_experimental_group))
meta_abstainers$sd_FUP1_experimental_group<-as.numeric(
	as.character(meta_abstainers$sd_FUP1_experimental_group))
meta_abstainers$N_control_FUP1<-as.numeric(
	as.character(meta_abstainers$N_control_FUP1))
meta_abstainers$mean_FUP1_control_group<-as.numeric(
	as.character(meta_abstainers$mean_FUP1_control_group))
meta_abstainers$sd_FUP1_control_group<-as.numeric(
	as.character(meta_abstainers$sd_FUP1_control_group))


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
  data=meta_abstainers[-c(4,6),], sm="SMD",
  byvar=category_intervention,print.byvar=TRUE,
  comb.fixed=TRUE,studlab=study)
summary(meta1)

tiff("/Users/jnv4/Desktop/GIC2017_figure6.tiff",
  width = 1200, height = 800,compression = 'lzw')
forest(meta1)
dev.off()

#############################################################################
#Figure 7. score
#############################################################################
## Suicide ideation metanalysis model
# #extracting studies with suicide ideation measures
## Suicide ideation metanalysis model
# #extracting studies with suicide ideation measures
meta_abstainers1<-subset(data,
	data$outcome_cat=="score")# &
	# data$fup_time=="3 monhts" | 
	# data$outcome_cat=="% abstainers" &
	# data$fup_time=="6 months")

meta_abstainers2<-subset(meta_abstainers1,
	# meta_abstainers1$fup_time=="12 monts" |
	meta_abstainers1$fup_time=="3 months" |
	meta_abstainers1$fup_time=="6 months" |
	meta_abstainers1$fup_time=="12 months" |
	meta_abstainers1$fup_time=="24 months")

meta_abstainers2$fup_time<-car::recode(
	meta_abstainers2$fup_time,"
	'3 months'='3 to 6 months';
	'6 months'='3 to 6 months';
	'12 months' = 'more than 12 months';
	'24 months' = 'more than 12 months'")

meta_abstainers<-with(meta_abstainers2,
	data.frame(N_experimental_FUP1,
			   mean_FUP1_experimental_group,
			   sd_FUP1_experimental_group,
			   N_control_FUP1,
			   mean_FUP1_control_group,
			   sd_FUP1_control_group,
			   type_intervention,
			   study,
			   fup_time,
			   outcome_cat,
			   category_intervention))

# excluding missing information
meta_abstainers<-na.omit(meta_abstainers)

#Adjusting to avoind the error of a missing category in
#the byvar analysis
meta_abstainers<-as.matrix(meta_abstainers)
meta_abstainers<-as.data.frame(meta_abstainers)

meta_abstainers$N_experimental_FUP1<-as.numeric(
	as.character(meta_abstainers$N_experimental_FUP1)) 
meta_abstainers$mean_FUP1_experimental_group<-as.numeric(
	as.character(meta_abstainers$mean_FUP1_experimental_group))
meta_abstainers$sd_FUP1_experimental_group<-as.numeric(
	as.character(meta_abstainers$sd_FUP1_experimental_group))
meta_abstainers$N_control_FUP1<-as.numeric(
	as.character(meta_abstainers$N_control_FUP1))
meta_abstainers$mean_FUP1_control_group<-as.numeric(
	as.character(meta_abstainers$mean_FUP1_control_group))
meta_abstainers$sd_FUP1_control_group<-as.numeric(
	as.character(meta_abstainers$sd_FUP1_control_group))


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
  data=meta_abstainers[-c(3,5,11,12,13,16,17,21,23),], sm="SMD",
  byvar=fup_time,print.byvar=TRUE,
  comb.fixed=TRUE,studlab=study)
summary(meta1)

tiff("/Users/jnv4/Desktop/GIC2017_figure7.tiff",
  width = 1200, height = 800,compression = 'lzw')
forest(meta1)
dev.off()


funnel(meta1)
metabias(meta1)
metainf(meta1)
metainf(meta1, pooled="random")
metareg(meta1, ~ Categorization)
