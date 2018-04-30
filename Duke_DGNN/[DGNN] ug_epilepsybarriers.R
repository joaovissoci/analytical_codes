######################################################################
#DGNN Epilepsy Barriers 
######################################################################
#
#
#
#
#
######################################################################
#SETTING ENVIRONMENT
######################################################################
#PASCKAGES INSTALLATION CODES
#install.packages("Hmisc")
#install.packages("car")
#install.packages("psych")
#install.packages("nortest")
#install.packages("ggplot2")
#install.packages("pastecs")
#install.packages("repmis")
#install.packages("mvnormtest")
#install.packages("polycor")

#PACKAGES LOADING CODE
#Load packages neededz for the analysis
#library(Hmisc)

#All packages must be installes with install.packages() function
lapply(c("sem","ggplot2", "psych", "irr", "nortest", "moments",
	"GPArotation","nFactors","boot","psy", "car","vcd", "gridExtra",
	"mi","VIM","epicalc","gdata","sqldf","reshape2","mclust",
	"foreign","survival","memisc","foreign","mice","MissMech",
	"lavaan","psych","qgraph","tibble","semPlot"), 
library, character.only=T)

######################################################################
#IMPORTING DATA
######################################################################
#LOADING DATA FROM A .CSV FILE
data<-read.csv("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/DGNN/epilepsy/bass_epilepsy_mergeddata.csv",sep=",")
#information between " " are the path to the directory in your computer where the data is stored

# data<-data[-409,]
######################################################################
#DATA MANAGEMENT
######################################################################

#Recoding NAs
#################################################

NAto9999<-function(x){
	car::recode(x,"9999=NA")
	}

data_temp<-lapply(data,NAto9999)

data<-as.data.frame(data_temp)

#Occupation 
#################################################

data$dem_patient_ocupation_cat<-car::recode(data$dem_dm_occ___1,"
				0='employed';
				1='unemployed'")

data$dem_decisionmaker_ocupation_cat<-car::recode(data$dem_dm_occ___1,"
				0='employed';
				1='unemployed'")

#Education
#################################################

data<-as_data_frame(data)
data<-mutate(data,dm_education_combined =
			ifelse(dem_decisions==2,
					dem_patient_edu_cat,
					  dem_dm_edu_cat))


#SES score 
#################################################

# data$dem_household_weekinc_cat<-car::recode(
# 			data$dem_household_weekinc,"
# 								0:5000='0 to 5000';
# 								5001:10000='05001 to 10000';
# 								10001:15000='10001 to 15000';
# 								15001:20000='15001 to 20000';
# 								20001:25000='20001 to 25000';
# 								25001:30000='25001 to 30000';
# 								30001:35000='30001 to 35000';
# 								35001:40000='35001 to 40000';
# 								40001:45000='40001 to 45000';
# 								45001:50000='45001 to 50000';
# 								50001:55000='50001 to 55000';
# 								55001:60000='55001 to 60000';
# 								60001:65000='60001 to 65000';
# 								65001:70000='65001 to 70000';
# 								70001:75000='70001 to 75000';
# 								75001:80000='75001 to 80000';
# 								80001:85000='80001 to 85000';
# 								85001:90000='85001 to 90000';
# 								90001:95000='90001 to 95000';
# 								95001:100000='95001 to 10000';
# 								100000:7000000='99100000 ot more'		
#                              ")

data$dem_household_weekinc_cat<-car::recode(
			data$dem_household_weekinc,"
								0:5000='0 to 5000';
								5001:10000='05001 to 10000';
								10001:20000='10001 to 20000';
								20001:30000='20001 to 30000';
								30001:40000='30001 to 40000';
								40001:50000='40001 to 50000';
								50001:60000='50001 to 60000';
								60001:70000='60001 to 70000';
								70001:80000='70001 to 80000';
								80001:90000='80001 to 90000';
								90001:100000='90001 to 10000';
								100000:7000000='99100000 ot more'
                             ")

data$dem_household_weekinc_cat<-as.numeric(ordered(as.factor(data$dem_household_weekinc_cat)))

ses_data<-with(data,data.frame(dem_household_weekinc_cat,
							   # dem_household_sourceinc,
							   # dem_number_household,
							   dm_education_combined, #ordinal
							   dem_dm_occ___1, #ordinal
							   # dem_dm_occ___2, #ordinal
							   # dem_dm_occ___3, #ordinal
							   # dem_dm_occ___4, #ordinal
							   # dem_dm_occ___5, #ordinal
							   # dem_dm_occ___6, #ordinal
							   # dem_dm_occ___7, #ordinal
							   # dem_dm_occ___8, #ordinal
							   # dem_home_radio, #ordinal
							   dem_home_tv, #ordinal
							   dem_home_phone, #ordinal
							   dem_home_elec, #ordinal
							   dem_home_water, #ordinal
							   dem_patient_literacy)) #ordinal

# generate imputations
# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
ses_imp <- mice(ses_data, seed = 2222, m=5)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
ses_data_imputed<-complete(ses_imp,3)

#calculating index
#MODEL 1 - Risk due to road deisgn
cor_data<-cor_auto(ses_data_imputed)

# qgraph(cor_data,layout="spring")

#PRINCIPAL COMPONENT ANALYSIS
model <- principal(cor_data ,nfactors=1, rotate='none', scores=T, cov=T)
model
 L <- model$loadings            # Just get the loadings matrix
 S <- model$scores              # This gives an incorrect answer in the current version

 d <- ses_data_imputed              # get your data
 dc <- scale(d,scale=FALSE)     # center the data but do not standardize it
 pca1 <- dc %*% L                 # scores are the centered data times the loadings
 # lowerCor(sc)                   #These scores, being principal components
#                                # should be orthogonal 

data$ses_index<-scales::rescale(pca1,to = c(100, 0))

# fa_model<-fa(cor_data,1,fm="uls",rotate="promax")

#Seizure frequency 
#################################################

#onset seizure frequency
data<-mutate(data,seizure_onsent_freq_adjusted =
			ifelse(bb_seiz_freq_unit==1,
					bb_seiz_freq,
				ifelse(bb_seiz_freq_unit==2,
					bb_seiz_freq*4.5,
					ifelse(bb_seiz_freq_unit==3,
					bb_seiz_freq*30,
			bb_seiz_freq))))

#current seizure frequency
# levels(data$bb_seiz_freq_cont)
data$bb_seiz_freq_cont_recoded<-car::recode(
	data$bb_seiz_freq_cont,"
						'1 or 2'=1.5")
data$bb_seiz_freq_cont_recoded<-as.numeric(
			as.character(data$bb_seiz_freq_cont_recoded))

data<-mutate(data,seizure_current_freq_adjusted =
			ifelse(bb_seiz_freq_cont_unit==1,
					bb_seiz_freq_cont_recoded,
				ifelse(bb_seiz_freq_cont_unit==2,
					bb_seiz_freq_cont_recoded*4.5,
					ifelse(bb_seiz_freq_cont_unit==3,
					bb_seiz_freq_cont_recoded*30,
			bb_seiz_freq_cont_recoded))))

#Beliefs score 
#################################################

# data$dem_household_weekinc_cat<-car::recode(
# 			data$dem_household_weekinc,"
# 								0:5000='0 to 5000';
# 								5001:10000='05001 to 10000';
# 								10001:20000='10001 to 20000';
# 								20001:30000='20001 to 30000';
# 								30001:40000='30001 to 40000';
# 								40001:50000='40001 to 50000';
# 								50001:60000='50001 to 60000';
# 								60001:70000='60001 to 70000';
# 								70001:80000='70001 to 80000';
# 								80001:90000='80001 to 90000';
# 								90001:100000='90001 to 10000';
# 								100000:7000000='99100000 ot more'
#                              ")

# data$dem_household_weekinc_cat<-as.numeric(ordered(as.factor(data$dem_household_weekinc_cat)))

causes_data<-with(data,data.frame(bb_c_inh,
							bb_c_brn_inj,
							bb_c_anc,
							bb_c_mother, #ordinal
							bb_c_blood, #ordinal
							bb_c_brn_ill, #ordinal
							bb_c_pun, #ordinal
							bb_c_sun, #ordinal
							bb_c_malaria, #ordinal
							bb_c_badmeds, #ordinal
							bb_c_spirit, #ordinal
							bb_c_witch, #ordinal
							bb_c_fever, #ordinal
							bb_c_trb_home, #ordinal
							bb_c_birth_inj)) #ordinal

# generate imputations
# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
causes_imp <- mice(causes_data, seed = 2222, m=5)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
causes_data_imputed<-complete(causes_imp,3)

#calculating index
#MODEL 1 - Risk due to road deisgn
cor_data<-cor_auto(causes_data_imputed)

qgraph(cor_data,layout="spring")

#PRINCIPAL COMPONENT ANALYSIS
# model <- principal(cor_data ,nfactors=1, rotate='none', scores=T, cov=T)
# model
#  L <- model$loadings            # Just get the loadings matrix
#  S <- model$scores              # This gives an incorrect answer in the current version

#  d <- ses_data_imputed              # get your data
#  dc <- scale(d,scale=FALSE)     # center the data but do not standardize it
#  pca1 <- dc %*% L                 # scores are the centered data times the loadings
#  # lowerCor(sc)                   #These scores, being principal components
# #                                # should be orthogonal 

# data$ses_index<-scales::rescale(pca1,to = c(100, 0))

#EFA
#Function to calculate the KMO values
kmo<-kmo(causes_data_imputed)
kmo$overall
kmo$AIR #anti-image matrix

cortest.bartlett(causes_data_imputed,n=626,diag=TRUE)

#Scree plot
ev <- eigen(cor_data) # get eigenvalues - insert the data you want to calculate the scree plot for
scree(cor_data)

#Parallel analysis
fa.parallel(causes_data_imputed,cor="poly")

#running the models
fa_model_1<-fa(cor_data,1,fm="uls",rotate="promax")
fa_model_2<-fa(cor_data,2,fm="uls",rotate="promax")
fa_model_3<-fa(cor_data,3,fm="uls",rotate="promax")
fa_model_4<-fa(cor_data,4,fm="uls",rotate="promax")

#CFA
#1factor model ###########
causes_model <- '
Biological =~ bb_c_brn_inj + bb_c_brn_ill + bb_c_malaria + bb_c_fever
Behavior or Guilt =~ bb_c_mother + bb_c_blood + bb_c_pun + bb_c_sun + bb_c_trb_home + bb_c_birth_inj
Spiritual =~ bb_c_anc +  bb_c_spirit + bb_c_witch

bb_c_malaria ~~ bb_c_fever
			 '

fit <- lavaan::cfa(causes_model,
				   data = causes_data_imputed,
				   estimator="WLSMV",
				   ordered=names(causes_data_imputed))

semPlot::semPaths(fit,
		"model",
		residuals=TRUE,
		cut=1,
  		equalizeManifests=TRUE,
  		edge.color="black",
  		exoCov=FALSE,
  		intercepts=FALSE,
  		# nodeLabels=nodeLabels,
  		# label.scale=FALSE,
  		# edge.label.cex=1,
  		# label.cex=labelcex,
  		# color=color,
  		# borders=borders,
  		curvePivot = TRUE)

summary(fit,
		fit.measures=TRUE,
		standardized = TRUE)

lavaan::fitMeasures(fit,
					fit.measures = c("chisq.scaled",
									 "df.scaled",
									 "pvalue.scaled",
									 "cfi.scaled",
									 "tli.scaled",
									 "rmsea.scaled",
									 "rmsea.ci.lower.scaled",
									 "rmsea.ci.upper.scaled"
									 ))

Est <- lavaan::parameterEstimates(fit,
								  ci = TRUE,
								  standardized = TRUE)

subset(Est, op == "=~")
subset(Est, op == "~~")


### Modification Indexes
Mod <- modificationIndices(fit)
subset(Mod, mi.scaled > 50)

#Factor scores
epilepsycause_scores<-lavaan::predict(fit)

#Building the score
data$epilepsycause_scores_1_biological<-epilepsycause_scores[,1]
data$epilepsycause_scores_1_biological<-scales::rescale(
	data$epilepsycause_scores_1_biological,to = c(0, 100))

data$epilepsycause_scores_2_behavior<-epilepsycause_scores[,2]
data$epilepsycause_scores_2_behavior<-scales::rescale(
	data$epilepsycause_scores_2_behavior,to = c(0, 100))

data$epilepsycause_scores_3_spiritual<-epilepsycause_scores[,3]
data$epilepsycause_scores_3_spiritual<-scales::rescale(
	data$epilepsycause_scores_3_spiritual,to = c(0, 100))

#Health care usage categories
#################################################

#Time to first HELP (any help = TH, Pastor, BM)

data$time_first_help<-data$bb_hsh1_time

#Time to first BM Help (any one BM happened)

data<-mutate(data,time_first_BM_help =
			ifelse(bb_hsh1_bmc==1,
					bb_hsh1_time,
				ifelse(bb_hsh2_bmc==1,
						bb_hsh2_time,
					ifelse(bb_hsh3_bmc==1,
							bb_hsh3_time,
						ifelse(bb_hsh4_bmc==1,
								bb_hsh4_time,
			"no_BM_help")))))

#Time between first BM and second BM

# data<-mutate(data,time_between_firstandseconde_BM_help =
# 			ifelse(bb_hsh1_bmc==1,
# 					# bb_hsh1_time,
# 				ifelse(bb_hsh2_bmc==1,
# 						bb_hsh2_time-bb_hsh1_time,
# 						"nohelp"),"nohelp"))

# 					ifelse(bb_hsh1_bmc==1,
# 							ifelse(bb_hsh3_bmc==1,
# 								bb_hsh3_time-bb_hsh1_time,


# 						ifelse(bb_hsh4_bmc==1,
# 								bb_hsh4_time,
# 			"no_BM_help")))))

#First HELP sought (any first TH, Pastor, BM)

n_hsh_T1<-with(data,rowSums(data.frame(bb_hsh1_th,bb_hsh1_pastor,bb_hsh1_bmc)))

data<-mutate(data,first_help_sought =
			ifelse(n_hsh_T1==1,
				ifelse(bb_hsh1_bmc==1,
					"BM",
				ifelse(bb_hsh1_th==1,
						"TH",
					ifelse(bb_hsh1_pastor==1,
							"Pastoral",
							"morethan1"))),
					"morethan1"))

# data.frame(a=data$first_help_sought,
# 		   b=data$bb_hsh1_bmc,
# 		   c=data$bb_hsh1_th,
# 		   d=data$bb_hsh1_pastor,
# 		   e=n_hsh_T1)

#First BM second TH or Pastoral or BM
data<-mutate(data,help_sought_after_BM =
			ifelse(bb_hsh1_bmc==1,
				ifelse(bb_hsh2_th==1,
					"BM than TH",
				ifelse(bb_hsh2_pastor==1,
						"BM than Pastoral",
				ifelse(bb_hsh2_bmc==1,
						"BM than BM",
						"BM than no help"))),
				"BM not first"))

# head(
# data.frame(
# 		   a=data$help_sought_after_BM,
# 		   b=data$bb_hsh2_bmc,
# 		   c=data$bb_hsh2_th,
# 		   d=data$bb_hsh2_pastor,
# 		   e=data$bb_hsh1_bmc))

#First TH second TH or Pastoral or BM

data<-mutate(data,help_sought_after_TH =
			ifelse(bb_hsh1_th==1,
				ifelse(bb_hsh2_th==1,
					"TH than TH",
				ifelse(bb_hsh2_pastor==1,
						"TH than Pastoral",
				ifelse(bb_hsh2_bmc==1,
						"TH than BM",
						"TH than no help"))),
				"TH not first"))

#First Pastoral second TH or Pastoral or BM

data<-mutate(data,help_sought_after_Pastoral =
			ifelse(bb_hsh1_pastor==1,
				ifelse(bb_hsh2_th==1,
					"Pastoral than TH",
				ifelse(bb_hsh2_pastor==1,
						"Pastoral than Pastoral",
				ifelse(bb_hsh2_bmc==1,
						"Pastoral than BM",
						"Pastoral than no help"))),
				"Pastoral not first"))

#Legs until BM

data<-mutate(data,legs_until_BMC =
			ifelse(bb_hsh1_bmc==1,
					1,
				ifelse(bb_hsh2_bmc==1,
						2,
				ifelse(bb_hsh3_bmc==1,
							3,
				ifelse(bb_hsh4_bmc==1,
								4,
						"No BMC help")))))

######################################################################
#EXPORTING DATA
######################################################################

#library(foreign)
write.foreign(data, "/Users/joaovissoci/Desktop/epilepsy_data.txt", 
					  "/Users/joaovissoci/Desktop/epilepsy_data.sps", 
					  package="SPSS")

write.csv(data, "/Users/joaovissoci/Desktop/epilepsy_data.csv")

######################################################################
#END
######################################################################