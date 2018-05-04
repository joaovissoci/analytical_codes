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
	"lavaan","psych","qgraph","tibble","semPlot","tidyverse"), 
library, character.only=T)

######################################################################
#IMPORTING DATA
######################################################################
#LOADING DATA FROM A .CSV FILE
data<-read.csv("/Users/Joao/Box Sync/Home Folder jnv4/Data/DGNN/epilepsy/bass_epilepsy_mergeddata.csv",sep=",")
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

data$dem_patient_ocupation_cat<-car::recode(data$dem_patient_occ___1,"
				0=0;
				1=1")

table(data$dem_patient_ocupation_cat)
prop.table(table(data$dem_patient_ocupation_cat))

data$dem_decisionmaker_ocupation_cat<-car::recode(data$dem_dm_occ___1,"
				0=0;
				1=1")

table(data$dem_decisionmaker_ocupation_cat)
prop.table(table(data$dem_decisionmaker_ocupation_cat))

data<-mutate(data,dm_occupation_combined =
			ifelse(dem_decisions==2,
					dem_patient_ocupation_cat,
					  dem_decisionmaker_ocupation_cat))

table(data$dm_occupation_combined)
prop.table(table(data$dm_occupation_combined))

#Education
#################################################

data<-as_data_frame(data)
data<-mutate(data,dm_education_combined =
			ifelse(dem_decisions==2,
					dem_patient_edu_cat,
					  dem_dm_edu_cat))



data$dm_education_combined<-car::recode(data$dm_education_combined,"
	4:6=4")

table(data$dm_education_combined)
prop.table(table(data$dm_education_combined))

#Literacy
#################################################

data<-as_data_frame(data)
data<-mutate(data,dm_literacyn_combined =
			ifelse(dem_decisions==2,
					dem_patient_literacy,
					  dem_dm_literacy))

table(data$dm_education_combined)
prop.table(table(data$dm_education_combined))

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
								5001:30000='5001 to 30000';
								30001:60000='30001 to 60000';
								60001:90000='60001 to 90000';
								90001:7000000='90001 ot more'
                             ")


data$dem_household_weekinc_cat2<-car::recode(
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
							   dem_home_radio, #ordinal
							   dem_home_tv, #ordinal
							   dem_home_phone, #ordinal
							   dem_home_elec, #ordinal
							   dem_home_water, #ordinal
							   dem_patient_literacy)) #ordinal

# generate imputations
# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
ses_imp <- mice(ses_data, seed = 2222, m=5)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
ses_data_imputed<-mice::complete(ses_imp,3)

# ses_home<-with(ses_data_imputed,data.frame(dem_home_radio,
# dem_home_tv,
# dem_home_phone,
# dem_home_elec,
# dem_home_water))

# ses_home[,1]<-car::recode(ses_home[,1],"2=0")
# ses_home[,2]<-car::recode(ses_home[,2],"2=0")
# ses_home[,3]<-car::recode(ses_home[,3],"2=0")
# ses_home[,4]<-car::recode(ses_home[,4],"2=0")
# ses_home[,5]<-car::recode(ses_home[,5],"2=0")

# ses_home$ses_home_score<-rowSums(ses_home[,1:5])

# table(ses_home$ses_home_score)
# prop.table(table(ses_home$ses_home_score))

# # Very basic bar graph
# ggplot(data=ses_home, aes(x=ses_home_score)) +
#     geom_bar(stat="count")

# summary(ses_home$ses_home_score)
# sd(ses_home$ses_home_score)

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

graph<-melt(causes_data)
count_data_fig3<-plyr::count(graph, c("variable", "value"))
count_data_fig3<-na.omit(count_data_fig3)
count_data_fig3$value<-car::recode(count_data_fig3$value,"
	1='Strongly disagree';
	2='Disagree';
	3='Agree';
	4='Strongly agree'")
count_data_fig3$feq_2<-(count_data_fig3$freq*100)/626
count_data_fig3$feq_2<-round(count_data_fig3$feq_2,digits=1)

# #Adding value of zero to likert options not chosen
# variable_add<-c("think_less_treated_person","alcoholic_trustworthy",
# 	"recover_alcoholic_hired")
# value_add<-c("Somewhat disagree","Disagree","Somewhat agree")
# freq_2_add<-c(0.0,0.0,0.0)
# freq_add<-c(0,0,0)
# add<-data.frame(variable=variable_add,
# 	value=value_add,
# 	freq=freq_add,
# 	feq_2=freq_2_add)

plot_data<-count_data_fig3

#Adding Mean and Standard Deviations
meanttoadd_labels<-c("bb_c_inh",
					"bb_c_brn_inj",
					"bb_c_anc",
					"bb_c_mother",
					"bb_c_blood",
					"bb_c_brn_ill",
					"bb_c_pun",
					"bb_c_sun",
					"bb_c_malaria",
					"bb_c_badmeds",
					"bb_c_spirit",
					"bb_c_witch",
					"bb_c_fever",
					"bb_c_trb_home",
					"bb_c_birth_inj")

meanttoadd_variable<-rep("Mean (SD)",15)

descriptives_temp<-describeBy(graph$value,graph$variable)

meanttoadd_means<-round(c(descriptives_temp[[1]]$mean,
				    descriptives_temp[[2]]$mean,
				    descriptives_temp[[3]]$mean,
				    descriptives_temp[[4]]$mean,
				    descriptives_temp[[5]]$mean,
				    descriptives_temp[[6]]$mean,
				    descriptives_temp[[7]]$mean,
				    descriptives_temp[[8]]$mean,
				    descriptives_temp[[9]]$mean,
				    descriptives_temp[[10]]$mean,
				    descriptives_temp[[11]]$mean,
				    descriptives_temp[[12]]$mean,
				    descriptives_temp[[13]]$mean,
				    descriptives_temp[[14]]$mean,
				    descriptives_temp[[15]]$mean),2)

meanttoadd_sd<-round(c(descriptives_temp[[1]]$sd,
				    descriptives_temp[[2]]$sd,
				    descriptives_temp[[3]]$sd,
				    descriptives_temp[[4]]$sd,
				    descriptives_temp[[5]]$sd,
				    descriptives_temp[[6]]$sd,
				    descriptives_temp[[7]]$sd,
				    descriptives_temp[[8]]$sd,
				    descriptives_temp[[9]]$sd,
				    descriptives_temp[[10]]$sd,
				    descriptives_temp[[11]]$sd,
				    descriptives_temp[[12]]$sd,
				    descriptives_temp[[13]]$sd,
				    descriptives_temp[[14]]$sd,
				    descriptives_temp[[15]]$sd),2)

# meanttoadd_values<-paste0(meanttoadd_means, " ","\n", "(", meanttoadd_sd,")")
meanttoadd_values<-paste0(meanttoadd_means, " ", "(", meanttoadd_sd,")")


meantoadd_data<-data.frame(variable=meanttoadd_labels,
						   value=meanttoadd_variable,
						   freq=NA,
						   feq_2=NA)

plot_data<-rbind(count_data_fig3,meantoadd_data)

plot_data$text<-c(rep(NA,60),meanttoadd_values)
plot_data$tile<-c(rep(NA,60),rep("white",15))

# plot_data$color<-NULL
# plot_data$color[plot_data$feq_2 >= 0 & plot_data$feq_2 < 5.883]="lightcyan1"
# plot_data$color[plot_data$feq_2 >= 5.883 & plot_data$feq_2 < 11.76]="lightcyan2"
# plot_data$color[plot_data$feq_2 >= 11.76 & plot_data$feq_2 < 26.47]="lightcyan3"
# plot_data$color[plot_data$feq_2 >= 26.47]="lightcyan4"

#find colors numbers: diverge_hcl(7, c = 100, l = c(50, 90), power = 1)
#from: https://cran.r-project.org/web/packages/colorspace/vignettes/hcl-colors.pdf

# plot_data$flip<-NULL
# plot_data$flip[plot_data$variable == "recovered_alcoholic_teacher"]="notflip"
# plot_data$flip[plot_data$variable == "alcoholic_close_friend"]="notflip"
# plot_data$flip[plot_data$variable == "alc_treatment_intelligent"]="notflip"
# plot_data$flip[plot_data$variable == "alcoholic_trustworthy"]="notflip"
# plot_data$flip[plot_data$variable == "recover_alcoholic_hired"]="notflip"
# plot_data$flip[plot_data$variable == "recovered_alc_treat_same"]="notflip"

# plot_data$flip[plot_data$variable == "alc_treatment_failure"]="flip"
# plot_data$flip[plot_data$variable == "recover_alcoholic_chldrn"]="flip"
# plot_data$flip[plot_data$variable == "non_alcoholic_hired"]="flip"
# plot_data$flip[plot_data$variable == "not_date_hospital_for_alc"]="flip"
# plot_data$flip[plot_data$variable == "think_less_treated_person"]="flip"
# plot_data$flip[plot_data$variable == "less_opinion_trtd_person"]="flip"


# plot_data$color<-NULL
# plot_data$color[plot_data$flip == "notflip" & plot_data$value == "Strongly agree"]="#4A6FE3"
# plot_data$color[plot_data$flip == "notflip" & plot_data$value == "Agree"]="#8595E1"
# plot_data$color[plot_data$flip == "notflip" & plot_data$value == "Somewhat agree"]="#B5BBE3"

# plot_data$color[plot_data$flip == "notflip" & plot_data$value == "Somewhat disagree"]="#E6AFB9"
# plot_data$color[plot_data$flip == "notflip" & plot_data$value == "Disagree"]="#E07B91"
# plot_data$color[plot_data$flip == "notflip" & plot_data$value == "Strongly disagree"]="#D33F6A"

# plot_data$color[plot_data$flip == "flip" & plot_data$value == "Strongly disagree"]="#4A6FE3"
# plot_data$color[plot_data$flip == "flip" & plot_data$value == "Disagree"]="#8595E1"
# plot_data$color[plot_data$flip == "flip" & plot_data$value == "Somewhat disagree"]="#B5BBE3"

# plot_data$color[plot_data$flip == "flip" & plot_data$value == "Somewhat agree"]="#E6AFB9"
# plot_data$color[plot_data$flip == "flip" & plot_data$value == "Agree"]="#E07B91"
# plot_data$color[plot_data$flip == "flip" & plot_data$value == "Strongly agree"]="#D33F6A"

# plot_data$color[plot_data$value == "Mean (SD)"]="white"

# write.csv(plot_data,"/Users/joaovissoci/plot_data.csv")

# avseq <- ggplot(plot_data, aes(y=variable, x=value)) + 
#   geom_tile(fill=plot_data$color) + 
#   geom_text(aes(y=variable, x=value, label=feq_2),size=7) + 
#   theme_minimal() + 
#   xlab(label="") + 
#   ylab(label="Stigma Scale") + 
#   scale_x_discrete(limits = c("Strongly disagree",
#   							  "Disagree",
#   							  "Somewhat disagree",
#   							  "Somewhat agree",
#   							  "Agree",
#   							  "Strongly agree")) + 
#   scale_y_discrete(limits = rev(levels(plot_data$variable)),
#   				   labels = rev(c(
# "1. Most people would willingly accept a former alcoholic \nas a close friend.",
# "2. Most people believe that a person who has had alcohol \ntreatment is just as intelligent as the average person.",
# "3. Most people believe that a former alcoholic is just as \ntrustworthy as the average person.",
# "4. Most people would accept a fully recovered former alcoholic \nas a teacher of young children in a public school.",
# "5. Most people feel that entering alcohol treatment is a \nsign of personal failure.",
# "6. Most people would not hire a former alcoholic to take care \nof their children, even if he or she had been \nsober for some time.",
# "7. Most people think less of a person who has been in alcohol \ntreatment.",
# "8. Most employers will hire a former alcoholic if he or \nshe is qualified for the job.",
# "9. Most employers will pass over the application of a former \nalcoholic in favor of another applicant.",
# "10. Most people in my community would treat a former alcoholic \njust as they would treat anyone else.",
# "11. Most young women would be reluctant to date a man \nwho has been hospitalized for alcoholism.",
# "12. Once they know a person was in alcohol treatment, \nmost people will take his or her opinion less seriously."
# 							))) #+ 
#   # theme(text = element_text(size=35))  #+ 
#   # scale_y_discrete(limits=rev(levels))
# avseq

# library(ggplot2)
figure4<-ggplot(plot_data, aes(y=variable, x=value)) +
  geom_point(aes(size = plot_data$feq_2*2), 
  			 alpha=0.8, 
  			 color="grey", 
  			 show.legend=FALSE) +
  geom_text(aes(label = feq_2), 
  			color="black") +
  scale_size(range = c(0,25)) +
  theme_bw() +
  xlab(label="") + 
  ylab(label="Causes of epilepsy") + 
  scale_x_discrete(limits = c("Strongly disagree",
  							  "Disagree",
  							  "Agree",
  							  "Strongly agree",
  							  "Mean (SD)"),
  				   labels = c("Strongly \ndisagree",
  							  "Disagree",
  							  "Agree",
  							  "Strongly \nagree",
  							  "Mean (SD)")) + 
  scale_y_discrete(limits = rev(c("bb_c_inh",
								  "bb_c_brn_inj",
								  "bb_c_anc",
								  "bb_c_mother",
								  "bb_c_blood",
								  "bb_c_brn_ill",
								  "bb_c_pun",
								  "bb_c_sun",
								  "bb_c_malaria",
								  "bb_c_badmeds",
								  "bb_c_spirit",
								  "bb_c_witch",
								  "bb_c_fever",
								  "bb_c_trb_home",
								  "bb_c_birth_inj")),
  				   labels = rev(c(
"1. Injury at Birth.",
"2. Troubled Home.",
"3. High Fever.",
"4. Witchcraft or spells.",
"5. Spirits possessing the person.",
"6. Bad medication.",
"7. Malaria.",
"8. Sun heating the brain.",
"9. Punishment by God.",
"10. Brain illness.",
"11. The blood is abnormal.",
"12. The mother was disrespectful \nor quarreled during pregnancy.",
"13. Trouble with ancestors.",
"14. Injury to the brain.",
"15. Inherited (passed down to children)."
							))) +
  # geom_tile(fill=plot_data$tile) +
  geom_text(aes(label=text))

# figure4

# generate imputations
# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
causes_imp <- mice(causes_data, seed = 2222, m=5)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
causes_data_imputed<-mice::complete(causes_imp,3)

#calculating index
#MODEL 1 - Risk due to road deisgn
cor_data<-cor_auto(causes_data_imputed)

# qgraph(cor_data,layout="spring")

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
# kmo<-kmo(causes_data_imputed)
# kmo$overall
# kmo$AIR #anti-image matrix

# cortest.bartlett(causes_data_imputed,n=626,diag=TRUE)

# #Scree plot
# ev <- eigen(cor_data) # get eigenvalues - insert the data you want to calculate the scree plot for
# scree(cor_data)

# #Parallel analysis
# fa.parallel(causes_data_imputed,cor="poly")

#running the models
# fa_model_1<-fa(cor_data,1,fm="uls",rotate="promax")
# fa_model_2<-fa(cor_data,2,fm="uls",rotate="promax")
# fa_model_3<-fa(cor_data,3,fm="uls",rotate="promax")
# fa_model_4<-fa(cor_data,4,fm="uls",rotate="promax")

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

# semPlot::semPaths(fit,
# 		"model",
# 		residuals=TRUE,
# 		cut=1,
#   		equalizeManifests=TRUE,
#   		edge.color="black",
#   		exoCov=FALSE,
#   		intercepts=FALSE,
#   		# nodeLabels=nodeLabels,
#   		# label.scale=FALSE,
#   		# edge.label.cex=1,
#   		# label.cex=labelcex,
#   		# color=color,
#   		# borders=borders,
#   		curvePivot = TRUE)

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
					"1",
				ifelse(bb_hsh1_th==1,
						"2",
					ifelse(bb_hsh1_pastor==1,
							"3",
							"1"))),
					"1"))

table(data$first_help_sought)
with(data,describeBy(time_first_help,first_help_sought))
fit <- aov(time_first_help ~ first_help_sought, data=data)
summary(fit) # display Type I ANOVA table
TukeyHSD(fit) # where fit comes from aov()

with(data,describeBy(time_first_BM_help,first_help_sought))
fit <- aov(time_first_BM_help ~ first_help_sought, data=data)
summary(fit) # display Type I ANOVA table
TukeyHSD(fit) # where fit comes from aov()

with(data,by(time_first_help,first_help_sought,summary))
with(data,kruskal.test(time_first_help~as.factor(first_help_sought))) # where y1 is numeric and A is a factor
library(nparcomp)
tox.trial <- mctp(time_first_help ~ as.factor(first_help_sought), 
				  data = data, 
				  type = "Dunnett",
				  conf.level = 0.95,
				  asy.method = "fisher",
				  info = FALSE)
summary(tox.trial)

with(data,by(time_first_BM_help,first_help_sought,summary))
with(data,kruskal.test(time_first_BM_help~as.factor(first_help_sought))) # where y1 is numeric and A is a factor
library(nparcomp)
tox.trial <- mctp(time_first_BM_help ~ as.factor(first_help_sought), 
				  data = data, 
				  type = "Dunnett",
				  conf.level = 0.95,
				  asy.method = "fisher",
				  info = FALSE)
summary(tox.trial)

#Codes
# 1 = BH
# 2 = TH
# 3 = Pastoral
# 4 = more than 1

# data.frame(a=data$first_help_sought,
# 		   b=data$bb_hsh1_bmc,
# 		   c=data$bb_hsh1_th,
# 		   d=data$bb_hsh1_pastor,
# 		   e=n_hsh_T1)

#First BM second TH or Pastoral or BM
data<-mutate(data,help_sought_after_BM =
			ifelse(bb_hsh1_bmc==1,
				ifelse(bb_hsh2_th==1,
					"1",
				ifelse(bb_hsh2_pastor==1,
						"2",
				ifelse(bb_hsh2_bmc==1,
						"3",
						"4"))),
				"5"))

#Codes
# 1 = BM than TH
# 2 = BM than Pastoral
# 3 = BM than BM
# 4 = BM than no help
# 5 = BM not first

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
					"1",
				ifelse(bb_hsh2_pastor==1,
						"2",
				ifelse(bb_hsh2_bmc==1,
						"3",
						"4"))),
				"5"))

#Codes
# 1 = TH than TH
# 2 = TH than Pastoral
# 3 = TH than BM
# 4 = TH than no help
# 5 = TH not first

#First Pastoral second TH or Pastoral or BM

data<-mutate(data,help_sought_after_Pastoral =
			ifelse(bb_hsh1_pastor==1,
				ifelse(bb_hsh2_th==1,
					"1",
				ifelse(bb_hsh2_pastor==1,
						"2",
				ifelse(bb_hsh2_bmc==1,
						"3",
						"4"))),
				"5"))

#Codes
# 1 = Pastoral than TH
# 2 = Pastoral than Pastoral
# 3 = Pastoral than BM
# 4 = Pastoral than no help
# 5 = Pastoral not first

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
						0)))))


#Barriers score 
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

barriers_data<-with(data,data.frame(bb_b_dm, #Opinion - decision maker
							bb_b_op_fam, #Opinion - family member
							bb_b_op_cl, #Opinion - community leaders
							bb_b_op_peer, #Opinion - peers
							bb_b_damage, #Problems w/ BMC - disbelief in medical care
							bb_b_doc_time_und, #Problems w/ BMC - lack of doctor attention
							bb_b_doc_notime, #Problems w/ BMC - no time to see patients
							bb_b_doc_noexp, #Problems w/ BMC - Doctors do not explain the illness well
							bb_b_seek_tl, #Problems w/ BMC - I went to a traditional healer instead
							bb_b_work, #Social support - Having to take time away from work
							bb_b_symp, #Social support - The symptoms were not very severe
							bb_b_fam_sick, #Social support - Others in my family were sick or needed help
							bb_b_fam_assist, #FSocial support - ew family members were available to assist those who were sick or needed help
							bb_b_know, #Social support -  I did not want people to know of the seizures
							bb_b_role, #Cost and access - The doctor's role is far above our station
							bb_b_cost_doc, # Cost and access - The potential cost of seeing a doctor
							bb_b_cost_test, #Cost and access - The potential cost of doctors' tests
							bb_b_cost_med, #Cost and access - The cost of epilepsy drugs
							bb_b_meds_notwork, #Medication reactino - Epilepsy drugs may not work to treat epilepsy
							bb_b_meds_fake, #Medication reaction - Some epilepsy drugs are fake
							bb_b_meds_se, #Medication reaction - Worry that epilepsy drugs have side e􏰂ects
							bb_b_meds_worse, #Medication reaction - Worry that the medicine will increase seizures
							bb_b_meds_runout, #Medication reaction - Sometimes the amount of epilepsy drugs given at the facility is not enough and runs out
							bb_b_far, #Distance and pay - Travel distance to the health facility is far
							bb_b_cost_travel, #Distance and pay - The cost of travel to the health facility
							bb_b_cost_upfront, #Distance and pay - With the doctor you must pay at once you cannot pay slowly over time for treatment
							bb_b_payment_type, #Distance and pay - With the doctor you cannot pay with goods only money, for treatment
							bb_b_bmc_notwork #Distance and pay - I do not believe that medical doctors can treat epilepsy e􏰂ectively
							))

# generate imputations
# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
barriers_imp <- mice(barriers_data, seed = 2222, m=5)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
barriers_data_imputed<-mice::complete(barriers_imp,3)

#calculating index
#MODEL 1 - Risk due to road deisgn
cor_data<-cor_auto(barriers_data_imputed)

# qgraph(cor_data,layout="spring")

#Barriers into Yes and No

recodes_likert<-function(x){
	car::recode(x,"1:2=0;3:5=1")
	}

barriers_data_temp<-lapply(barriers_data_imputed,recodes_likert)

barriers_data_temp<-as.data.frame(barriers_data_temp)

barriers_count_data_temp<-melt(barriers_data_temp)
barriers_count_data<-plyr::count(barriers_count_data_temp, c("variable", "value"))
barriers_count_data<-na.omit(barriers_count_data)
barriers_count_data$value<-car::recode(barriers_count_data$value,"
	0='No';
	1='Yes'")
barriers_count_data$feq_2<-(barriers_count_data$freq*100)/626
barriers_count_data$feq_2<-round(barriers_count_data$feq_2,digits=1)

p <- ggplot(barriers_count_data, aes(reorder(variable,feq_2,function(x)-length(x)), feq_2))
p <- p + geom_bar(stat = "identity", 
				  position = position_dodge(),
				  aes(fill=value))
p <- p + theme_bw()
# p <- p + facet_wrap()
p


sort_vector<-barriers_count_data[barriers_count_data[,2]=="Yes",][order(barriers_count_data[barriers_count_data[,2]=="Yes",]$feq_2),]$variable

p <- ggplot(barriers_count_data, aes(variable, feq_2))
p <- p + geom_bar(stat = "identity", aes(fill=value))
p <- p + theme_minimal()
# p <- p + scale_fill_discrete(guide=FALSE)
p <- p + coord_flip()
p <- p + scale_fill_brewer(palette="Paired",guide=FALSE)
p <- p + scale_x_discrete(limits = as.character(sort_vector),
						  labels = c(
						  			 "Opinions of community leaders about treatment",
						  			 "I do not believe that medical doctors can \ntreat epilepsy e􏰂ectively",
						  			 "Worry that the medicine will increase seizures",
						  			 "Doctors do not have time to see me",
						  			 "Some epilepsy drugs are fake",
						  			 "Epilepsy drugs may not work to treat epilepsy",
						  			 "I went to a traditional healer instead",
						  			 "The belief that even if medical doctors can stop the seizures, \nthey cannot reverse the damage that is already there",
						  			 "With the doctor, you must pay at once, you \ncannot pay slowly over time for treatment",
						  			 "With the doctor, you cannot pay with goods, \nonly money, for treatment",
						  			 "Opinions of friends and peers about treatment",
						  			 "Direction by the person in my family who \nmakes decisions about health care",
						  			 "Opinions of other family members about \ntreatment",
						  			 "Doctors do not spend enough time with \npatients to understand",
						  			 "The symptoms were not very severe",
						  			 "The doctor's role is far above our station",
						  			 "Others in my family were sick or needed help",
						  			 "The potential cost of seeing a doctor",
						  			 "I did not want people to know of the seizures",
						  			 "Worry that epilepsy drugs have side e􏰂fects",
						  			 "Few family members were available to \nassist those who were sick or needed help",
						  			 "Doctors do not explain the illness well",
						  			 "The potential cost of doctors' tests",
						  			 "Having to take time away from work",
						  			 "Travel distance to the health facility is far",
						  			 "The cost of travel to the health facility",
						  			 "The cost of epilepsy drugs",
						  			 "Sometimes the amount of epilepsy drugs \ngiven at the facility is not nenough and runs out"
						  			  ))
p <- p + xlab("% or participants endorsing that item") + ylab("Barriers to epilepsy care")
# p <- p +  geom_text(aes(label=feq_2), vjust=1.6, color="white",
#             position = position_dodge(0.9), size=3.5)
p


# libraries
library(packcircles)
library(ggplot2)
 
# Create data
barriers_buuble<-subset(barriers_count_data,
	barriers_count_data$value=="Yes" &
	barriers_count_data$feq_2>45)

barriers_buuble<-barriers_buuble[order(barriers_buuble$feq_2),]

# Generate the layout. This function return a dataframe with one line per bubble. 
# It gives its center (x and y) and its radius, proportional of the value
packing <- circleProgressiveLayout(barriers_buuble$feq_2, sizetype='area')
 
# We can add these packing information to the initial data frame
barriers_buuble = cbind(barriers_buuble, packing)
 
# Check that radius is proportional to value. We don't want a linear relationship, since it is the AREA that must be proportionnal to the value
# plot(barriers_buuble$radius, barriers_buuble$feq_2)
 
# The next step is to go from one center + a radius to the coordinates of a circle that
# is drawn by a multitude of straight lines.
dat.gg <- circleLayoutVertices(packing, npoints=50)
 
# Make the plot
ggplot() + 
  
  # Make the bubbles
  geom_polygon(data = dat.gg, aes(x, y, group = id, 
  										fill=as.factor(id)), 
  			   # colour = "black",
  			   alpha = 0.7) +
  
  # Add text in the center of each bubble + control its size
  geom_text(data = barriers_buuble, 
  									aes(x, y, 
  										size=c(70,80,90,110), 
  										label = #variable
  										c(
			        "Long travel distance \n(50%)",
			        "The cost of travel \n(51%)",
			        "The cost of epilepsy drugs \n(66%)",
			        "Epilepsy drugs run out \n(68%)"
  										)
  											)) +
  scale_size_continuous(range = c(3,6)) +
  
  # General theme:
  # theme_minimal() +
  theme_void() + 
  theme(legend.position="none") +
  scale_fill_brewer() +
  coord_equal()

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

# #EFA
# #Function to calculate the KMO values
# kmo<-kmo(barriers_data_imputed)
# kmo$overall
# kmo$AIR #anti-image matrix

# cortest.bartlett(barriers_data_imputed,n=626,diag=TRUE)

# #Scree plot
# ev <- eigen(cor_data) # get eigenvalues - insert the data you want to calculate the scree plot for
# scree(cor_data)

# #Parallel analysis
# fa.parallel(barriers_data_imputed,cor="poly")

# #running the models
# fa_model_1<-fa(cor_data,3,fm="uls",rotate="oblimin")
# fa_model_2<-fa(cor_data,4,fm="uls",rotate="oblimin")
# fa_model_3<-fa(cor_data,5,fm="uls",rotate="oblimin")
# fa_model_4<-fa(cor_data,6,fm="uls",rotate="oblimin")

#CFA
#4factor model ###########
# barriers_model <- '

# Dissatisfaction with BMC =~ bb_b_dm + bb_b_op_fam + bb_b_op_cl + bb_b_doc_time_und + bb_b_doc_notime + bb_b_doc_noexp + bb_b_cost_doc + bb_b_cost_test + bb_b_cost_upfront + bb_b_payment_type
# Stigma =~ bb_b_op_peer + bb_b_work + bb_b_symp + bb_b_know + bb_b_cost_med + bb_b_meds_runout
# Lack of access =~ bb_b_role + bb_b_far + bb_b_cost_travel
# Lack of trust =~ bb_b_meds_notwork + bb_b_meds_fake + bb_b_meds_worse + bb_b_bmc_notwork

# bb_b_cost_upfront ~~ bb_b_payment_type
# bb_b_cost_med ~~  bb_b_meds_runout
# bb_b_doc_time_und ~~   bb_b_doc_notime
# bb_b_far ~~ bb_b_cost_travel

# 			 '

# fit <- lavaan::cfa(barriers_model,
# 				   data = barriers_data_imputed,
# 				   estimator="WLSMV",
# 				   ordered=names(barriers_data_imputed))

# # semPlot::semPaths(fit,
# # 		"model",
# # 		residuals=TRUE,
# # 		cut=1,
# #   		equalizeManifests=TRUE,
# #   		edge.color="black",
# #   		exoCov=FALSE,
# #   		intercepts=FALSE,
# #   		# nodeLabels=nodeLabels,
# #   		# label.scale=FALSE,
# #   		# edge.label.cex=1,
# #   		# label.cex=labelcex,
# #   		# color=color,
# #   		# borders=borders,
# #   		curvePivot = TRUE)

# summary(fit,
# 		fit.measures=TRUE,
# 		standardized = TRUE)

# lavaan::fitMeasures(fit,
# 					fit.measures = c("chisq.scaled",
# 									 "df.scaled",
# 									 "pvalue.scaled",
# 									 "cfi.scaled",
# 									 "tli.scaled",
# 									 "rmsea.scaled",
# 									 "rmsea.ci.lower.scaled",
# 									 "rmsea.ci.upper.scaled"
# 									 ))

# Est <- lavaan::parameterEstimates(fit,
# 								  ci = TRUE,
# 								  standardized = TRUE)

# subset(Est, op == "=~")
# subset(Est, op == "~~")


# ### Modification Indexes
# Mod <- modificationIndices(fit)
# subset(Mod, mi.scaled > 50)

# # #Factor scores
# epilepsycause_scores<-lavaan::predict(fit)

# #Building the score
# data$epilepsycause_scores_1_biological<-epilepsycause_scores[,1]
# data$epilepsycause_scores_1_biological<-scales::rescale(
# 	data$epilepsycause_scores_1_biological,to = c(0, 100))

# data$epilepsycause_scores_2_behavior<-epilepsycause_scores[,2]
# data$epilepsycause_scores_2_behavior<-scales::rescale(
# 	data$epilepsycause_scores_2_behavior,to = c(0, 100))

# data$epilepsycause_scores_3_spiritual<-epilepsycause_scores[,3]
# data$epilepsycause_scores_3_spiritual<-scales::rescale(
# 	data$epilepsycause_scores_3_spiritual,to = c(0, 100))

#5factor model ###########
barriers_model <- '

Dissatisfaction with BMC =~ bb_b_dm + bb_b_op_fam + bb_b_op_cl + bb_b_doc_time_und + bb_b_doc_notime + bb_b_doc_noexp + bb_b_cost_doc + bb_b_cost_test + bb_b_cost_upfront + bb_b_payment_type
Stigma =~ bb_b_op_peer + bb_b_work + bb_b_symp + bb_b_know 
Lack of access =~ bb_b_role + bb_b_far + bb_b_cost_travel
Lack of med availability =~ bb_b_cost_med + bb_b_meds_runout
Lack of trust =~ bb_b_meds_notwork + bb_b_meds_fake + bb_b_meds_worse + bb_b_bmc_notwork

bb_b_cost_upfront ~~ bb_b_payment_type
bb_b_dm ~~       bb_b_op_fam
bb_b_doc_time_und ~~   bb_b_doc_notime
# bb_b_far ~~ bb_b_cost_travel

			 '

fit <- lavaan::cfa(barriers_model,
				   data = barriers_data_imputed,
				   estimator="WLSMV",
				   ordered=names(barriers_data_imputed))

# semPlot::semPaths(fit,
# 		"model",
# 		residuals=TRUE,
# 		cut=1,
#   		equalizeManifests=TRUE,
#   		edge.color="black",
#   		exoCov=FALSE,
#   		intercepts=FALSE,
#   		# nodeLabels=nodeLabels,
#   		# label.scale=FALSE,
#   		# edge.label.cex=1,
#   		# label.cex=labelcex,
#   		# color=color,
#   		# borders=borders,
#   		curvePivot = TRUE)

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
epilepsybarriers_scores<-lavaan::predict(fit)

#Building the score
data$epilepsybarriers_scores_1_biological<-epilepsybarriers_scores[,1]
data$epilepsybarriers_scores_1_biological<-scales::rescale(
	data$epilepsybarriers_scores_1_biological,to = c(0, 100))

data$epilepsybarriers_scores_2_stigma<-epilepsybarriers_scores[,2]
data$epilepsybarriers_scores_2_stigma<-scales::rescale(
	data$epilepsybarriers_scores_2_stigma,to = c(0, 100))

data$epilepsybarriers_scores_3_access<-epilepsybarriers_scores[,3]
data$epilepsybarriers_scores_3_access<-scales::rescale(
	data$epilepsybarriers_scores_3_access,to = c(0, 100))

data$epilepsybarriers_scores_4_medication<-epilepsybarriers_scores[,4]
data$epilepsybarriers_scores_4_medication<-scales::rescale(
	data$epilepsybarriers_scores_4_medication,to = c(0, 100))

data$epilepsybarriers_scores_5_trust<-epilepsybarriers_scores[,5]
data$epilepsybarriers_scores_5_trust<-scales::rescale(
	data$epilepsybarriers_scores_5_trust,to = c(0, 100))

data<-as.data.frame(data)


#Logistis regression - Type of care sought
names(data)

data$first_help_sought_bin<-car::recode(
	data$first_help_sought,"1='BMC';2:3='Aother'")

data$first_help_sought_bin<-as.factor(data$first_help_sought_bin)

data$bb_age_seizures<-car::recode(
	data$bb_age_seizures,"'Dont know'=NA")

data$bb_age_seizures<-as.numeric(as.character(data$bb_age_seizures))

logmodel_gcs<-glm(first_help_sought_bin ~
						# as.factor(dem_patient_gender) + 
						bb_age_seizures + 
						seizure_onsent_freq_adjusted + 
						as.factor(dem_patient_urban) + 
						ses_index +
						bb_seiz_hlthctr_dist +
                        # as.factor(dem_dm_literacy) + 
                        epilepsycause_scores_1_biological +
						epilepsycause_scores_2_behavior +
						epilepsycause_scores_3_spiritual +
                        epilepsybarriers_scores_1_biological +
                        epilepsybarriers_scores_2_stigma +
                        epilepsybarriers_scores_3_access +
                        epilepsybarriers_scores_4_medication +
                        epilepsybarriers_scores_5_trust
                       ,family=binomial, 
                       	data=data)
summary(logmodel_gcs)
#anova(reglogGEU)
#exp(coef(model1_death)) # exponentiated coefficients
#exp(confint(model1_death)) # 95% CI for exponentiated coefficients
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
#logistic.display(baselineXFUP3)

logmodel_gcs<-glm(first_help_sought_bin ~
						as.factor(dem_patient_gender) + 
						bb_age_seizures + 
						seizure_onsent_freq_adjusted + 
						bb_seiz_hlthctr_dist +
						as.factor(dem_household_weekinc_cat) +
						as.factor(dm_occupation_combined) +
						as.factor(dem_patient_urban) + 
						as.factor(dm_education_combined) +
                        as.factor(dm_literacyn_combined) + 
                        epilepsycause_scores_1_biological +
						epilepsycause_scores_2_behavior +
						epilepsycause_scores_3_spiritual +
                        epilepsybarriers_scores_1_biological +
                        epilepsybarriers_scores_2_stigma +
                        epilepsybarriers_scores_3_access +
                        epilepsybarriers_scores_4_medication +
                        epilepsybarriers_scores_5_trust
                       ,family=binomial, 
                       	data=data)
summary(logmodel_gcs)
#anova(reglogGEU)
#exp(coef(model1_death)) # exponentiated coefficients
#exp(confint(model1_death)) # 95% CI for exponentiated coefficients
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
#logistic.display(baselineXFUP3)

######################################################################
#EXPORTING DATA
######################################################################

# #library(foreign)
# write.foreign(data, "/Users/joaovissoci/Desktop/epilepsy_data.txt", 
# 					  "/Users/joaovissoci/Desktop/epilepsy_data.sps", 
# 					  package="SPSS")

write.csv(data, "/Users/joaovissoci/Desktop/epilepsy_data.csv")

######################################################################
#END
######################################################################