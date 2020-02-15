######################################################################
#BASIC R STATISTICS TEMPLATE
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
lapply(c("Hmisc",
		 "car",
		 "psych",
		 "nortest",
		 "ggplot2",
		 # "pastecs",
		 "repmis",
		 "polycor",
		 "mice"), 
library, character.only=T)

######################################################################
#IMPORTING DATA
######################################################################
#LOADING DATA FROM A .CSV FILE

data_patients<-read.csv("/Users/Joao/Box/Home Folder jnv4/Data/Global EM/Africa/Tz/BNI/Tz_bnipatients_data.csv")

data_family<-read.csv("/Users/Joao/Box/Home Folder jnv4/Data/Global EM/Africa/Tz/BNI/Tz_bniKAfamily_data.csv")

######################################################################
#DATA MANAGEMENT
######################################################################

# names(data)

data_patients$group<-c("Patient")
data_family$group<-c("Family")

#RECODING STIGMA FOR BOTH GROUPS
# #STIGMA
patients_score_data<-with(data_patients, data.frame(alcoholic_close_friend,
              recovered_alcoholic_teacher,
              recover_alcoholic_chldrn,
              recover_alcoholic_hired,
              non_alcoholic_hired,
              recovered_alc_treat_same,
              no_date_hospital_for_alc,
              alc_treatment_intelligent,
              alcoholic_trustworthy,
              alc_treatment_failure,
              think_less_treated_person,
              less_opinion_trtd_person,
              group))

family_score_data<-with(data_family, data.frame(
              alcoholic_close_friend=f_alcoholic_close_friend,
              recovered_alcoholic_teacher=f_recoveralcohol_teacher,
              recover_alcoholic_chldrn=f_recover_alcoholic_chldrn,
              recover_alcoholic_hired=f_recover_alcoholic_hired,
              non_alcoholic_hired=f_non_alcoholic_hired,
              recovered_alc_treat_same=f_recovered_alc_treat_same,
              no_date_hospital_for_alc=f_no_date_hospital_for_alc,
              alc_treatment_intelligent=f_alc_treat_intel,
              alcoholic_trustworthy=f_alcoholic_trustworthy,
              alc_treatment_failure=f_alc_treatment_failure,
              think_less_treated_person=f_think_less_treat_person,
              less_opinion_trtd_person=f_less_opinion_trtd_person,
              group))

data_stigma<-rbind(patients_score_data,family_score_data)


#recoding alc_treatment_failure variable
data_stigma$alc_treatment_failure<-car::recode(data_stigma$alc_treatment_failure,"
                                     1='6';2='5';3='4';
                                     4='3';5='2';6='1'")

#recoding recover_alcoholic_chldrn variable
data_stigma$recover_alcoholic_chldrn<-car::recode(data_stigma$recover_alcoholic_chldrn,"
                                                   1='6';2='5';3='4';
                                                   4='3';5='2';6='1'")

#recoding think_less_treated_person variable
data_stigma$think_less_treated_person<-car::recode(data_stigma$think_less_treated_person,"
                                                   1='6';2='5';3='4';
                                                   4='3';5='2';6='1'")

# recoding think_less_treated_person variable
data_stigma$think_less_treated_person<-car::recode(data_stigma$think_less_treated_person,"
                                                   1='6';2='5';3='4';
                                                   4='3';5='2';6='1'")

#recoding non_alcoholic_hired variable
data_stigma$non_alcoholic_hired<-car::recode(data_stigma$non_alcoholic_hired,"
                                                       1='6';2='5';3='4';
                                                       4='3';5='2';6='1'")

#recoding no_date_hospital_for_alc variable
data_stigma$no_date_hospital_for_alc<-car::recode(data_stigma$no_date_hospital_for_alc,"
                                                 1='6';2='5';3='4';
                                                 4='3';5='2';6='1'")

#recoding less_opinion_trtd_person variable
data_stigma$less_opinion_trtd_person<-car::recode(data_stigma$less_opinion_trtd_person,"
                                                      1='6';2='5';3='4';
                                                      4='3';5='2';6='1'")

#Organize scale data_stigmasets

#BNI
# stigma_data_stigma<-with(data_stigma,data_stigma.frame(alc_treatment_intelligent,alcoholic_trustworthy,alc_treatment_failure,think_less_treated_person,
#                                            less_opinion_trtd_person,alcoholic_close_friend,recovered_alcoholic_teacher,recover_alcoholic_chldrn,
#                                            recover_alcoholic_hired,non_alcoholic_hired,recovered_alc_treat_same,no_date_hospital_for_alc))

# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
# data_stigma_imputed <- mice(BNI_data_stigma, seed = 2222, m=10)

# reports the complete data_stigmaset with missing imputated. It returns 5 options of data_stigmasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
# BNI_data_stigma<-mice::complete(data_stigma_imputed,4)

#BNI_Devaluation
# BNI_Devaluation<-with(BNI_data_stigma,data_stigma.frame(alc_treatment_intelligent,alcoholic_trustworthy,alc_treatment_failure,
#                                           think_less_treated_person,less_opinion_trtd_person))

# #BNI_Discrimination
# BNI_Discrimination<-with(BNI_data_stigma,data_stigma.frame(alcoholic_close_friend,recovered_alcoholic_teacher,recover_alcoholic_chldrn,
                                               # recover_alcoholic_hired,non_alcoholic_hired,recovered_alc_treat_same,no_date_hospital_for_alc))

# # argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
imp <- mice(data_stigma, seed = 2222, m=5)

# # reports the complete data_stigmaset with missing imputated. It returns 5 options of data_stigmasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
data_stigma<-mice::complete(imp,4)

# 1 factor model
cfa_model <- '
BNI =~ alc_treatment_intelligent + 
       alcoholic_trustworthy +
       # alc_treatment_failure +
       think_less_treated_person +
       # less_opinion_trtd_person + 
       alcoholic_close_friend +
       recovered_alcoholic_teacher +
       # recover_alcoholic_chldrn +
       recover_alcoholic_hired +
       # non_alcoholic_hired +
       recovered_alc_treat_same +
       no_date_hospital_for_alc

alc_treatment_intelligent ~~       alcoholic_trustworthy
'

# Neg =~ alc_treatment_failure +
#        # think_less_treated_person +
#        less_opinion_trtd_person + 
#        recover_alcoholic_chldrn +
#        non_alcoholic_hired +
#        no_date_hospital_for_alc
# '

# 2 factor model
# cfa_model <- '
# BNI_Devaluation =~ alc_treatment_intelligent+alcoholic_trustworthy+alc_treatment_failure+
#                                           think_less_treated_person+less_opinion_trtd_person
# BNI_Discrimination =~ alcoholic_close_friend+recovered_alcoholic_teacher+recover_alcoholic_chldrn+
#                                                recover_alcoholic_hired+non_alcoholic_hired+recovered_alc_treat_same+no_date_hospital_for_alc
# '

fit <- lavaan::cfa(cfa_model,
                   data = data_stigma[,-13],
                   estimator="WLSMV",
                   ordered=colnames(data_stigma[,-13])
)

#Predicted scores

pas_data<-with(data_stigma,data.frame(alc_treatment_intelligent, 
       alcoholic_trustworthy,
       think_less_treated_person,
       alcoholic_close_friend,
       recovered_alcoholic_teacher,
       recover_alcoholic_hired,
       recovered_alc_treat_same,
       no_date_hospital_for_alc))

pas_scores<-lavaan::lavPredict(fit,newdata=pas_data)
pas_scores_scaled<-scales::rescale(as.data.frame(pas_scores)$BNI, 
    to = c(0, 100))

data_patients$pas_scores_patients<-pas_scores_scaled[data_stigma$group=="Patient"]
data_family$pas_scores_family<-pas_scores_scaled[data_stigma$group=="Family"]

#RECODING AUDIT PATIENTS
# data<-subset(data1,data1$audit_complete==2)

audit_data<-with(data_patients,data.frame(
				consumption,
				audit_complete,
				how_often_drink,                  
				number_drinks_day,
				how_often_6_more_drinks,
				how_often_cant_stop_drinking,
				fail_expectation_bc_drinking,
				how_often_drink_morning,
				how_often_guilt_postdrinking,
				how_often_no_memory_postdrinking,
				drinking_injured_you_or_someone,
				others_concerned_your_drinking,
				age_1st_drink,drink_drive))


audit_data_questions<-audit_data[-c(1:2,13:14)]

NAto0<-function(x){
	car::recode(x,"NA=0")
	}

audit_data_NAto0<-lapply(audit_data_questions,NAto0)
audit_data_NAto0<-as.data.frame(audit_data_NAto0)
audit_data_questions$audit_score<-rowSums(audit_data_NAto0)
# audit_data_questions$audit_score_D1<-rowSums(audit_data_NAto0[,1:3])
# audit_data_questions$audit_score_D2<-rowSums(audit_data_NAto0[,4:10])
# audit_data_questions$audit_score_D3<-rowSums(audit_data_NAto0[,7:])

data_patients$audit_score<-audit_data_questions$audit_score

data_patients$audit_score_cat<-car::recode(
	audit_data_questions$audit_score,
	"0:7='No';else='Yes'")

# audit_data_cleaned<-audit_data_questions[-c(1:10)]
# audit_data_cleaned<-cbind(audit_data_cleaned,
# 	audit_data_NAto0)

#Drinc
patients_drinc_data<-with(data_patients,data.frame(
				had_hangover,
				bad_about_self,
				missed_work_school,
				ppl_worry,
				enjoy_taste,
				work_suffered,
				good_parent,
				sleep_pdrink,
				drink_drive,
				other_drugs,
				sick_vomit,
				unhappy,
				poor_eating,
				fail_expect,
				relax,
				guilt_ashamed,
				embarrassing,
				worse_persona,
				fool_risk,
				trouble,
				harsh_cruel,
				impulsive_regret,
				fight,
				harm_phys_health,
				more_pos,
				money_prob,
				harm_love,
				smoke_more,
				harm_appearance,
				hurt_fam,
				damage_friend,
				overweight,
				sex_suffers,
				lose_interest,
				enjoy_social,
				harm_spiritual,
				not_want_life,
				no_growth,
				damage_social,
				lost_money,
				dui,
				legal_trouble,
				lose_marriage,
				fired,
				drink_no_prob,
				lost_friend,
				accident,
				hurt_injury,
				injur_other,
				broken_things))

# drinc_data_questions<-subset(drinc_data,audit_data[2]==2)

# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
imp <- mice(patients_drinc_data, seed = 2222, m=5)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
patients_drinc_data_questions<-complete(imp,4)

data_patients$drinc_data_score<-rowSums(patients_drinc_data_questions)/2
data_patients$drinc_data_score_cat<-car::recode(
	data_patients$drinc_data_score,"0:5='Low';else='High'")


#RECODING FAMILY
colnames(data_family)<-c(
"id",
"date_enrollment",
"female",
"age",
"family_member_patient",
"family_member_patient2",
"study_id",
"complaint",
"improve_health_other",
"improve_health_other",
"improve_health_other",
"improve_health_other",
"improve_health_other",
"improve_health_other",
"improve_health_other",
"improve_health_other",
"improve_health_other",
"improve_health_other",
"screening_demographics_complete",
"consumption",
"how_often_drink",
"number_drinks_day",
"how_often_6_more_drinks",
"how_often_cant_stop_drinking",
"fail_expectation_bc_drinking",
"how_often_drink_morning",
"how_often_guilt_postdrinking",
"how_often_no_memory_postdrinking",
"drinking_injured_you_or_someone",
"others_concerned_your_drinking",
"audit_total",
"b11",
"b12",
"b13",
"b13_1",
"b13_2",
"b13_3",
"audit_complete",
"alc_treatment_intelligent",
"alcoholic_trustworthy",
"alc_treatment_failure",
"think_less_treated_person",
"less_opinion_trtd_person",
"alcoholic_close_friend",
"recovered_alcoholic_teacher",
"recover_alcoholic_chldrn",
"recover_alcoholic_hired",
"non_alcoholic_hired",
"recovered_alc_treat_same",
"no_date_hospital_for_alc",
"stigma_complete",
"group",
"pas_scores_family")

audit_data_family<-with(data_family,data.frame(
				consumption,
				audit_complete,
				how_often_drink,                  
				number_drinks_day,
				how_often_6_more_drinks,
				how_often_cant_stop_drinking,
				fail_expectation_bc_drinking,
				how_often_drink_morning,
				how_often_guilt_postdrinking,
				how_often_no_memory_postdrinking,
				drinking_injured_you_or_someone,
				others_concerned_your_drinking
				))


audit_data_questions_fam<-audit_data_family[-c(1:2)]

NAto0<-function(x){
	as.numeric(car::recode(x,"NA=0"))
	}

audit_data_NAto0_fam<-lapply(audit_data_questions_fam,NAto0)
audit_data_NAto0_fam<-as.data.frame(audit_data_NAto0_fam)
audit_data_questions_fam$audit_score<-rowSums(audit_data_NAto0_fam)
# audit_data_questions_fam$audit_score_D1<-rowSums(audit_data_NAto0[,1:3])
# audit_data_questions_fam$audit_score_D2<-rowSums(audit_data_NAto0[,4:10])
# audit_data_questions_fam$audit_score_D3<-rowSums(audit_data_NAto0[,7:])

data_family$audit_score<-audit_data_questions_fam$audit_score

data_family$audit_score_cat<-car::recode(
	audit_data_questions_fam$audit_score,
	"0:7='No';else='Yes'")

#MERGING DATASETS

merged_data<-merge(x = with(data_patients,data.frame(
							age,
							sex=female,
							p_audit_score=audit_score,
							p_audit_score_cat=audit_score_cat,
							p_pas_score=pas_scores_patients,
							p_drinc_data_score=drinc_data_score,
							p_drinc_data_score_cat=drinc_data_score_cat,
							study_id,
							p_consumption=consumption,
							p_groups=group
							)), 
					   y = with(data_family,data.frame(
							f_audit_score=audit_score,
							f_audit_score_cat=audit_score_cat,
							f_pas_score=pas_scores_family,
							study_id,
							f_consumption=consumption,
							f_groups=group
							)), 
					   by = "study_id", 
					   all.y = TRUE)


#M

########## Extract non-abstainners
#Non-Abstainners = everyone who responded 1 or 2 in the consumption question
data_nonabst<-subset(merged_data,							
								merged_data$p_consumption!=0)

data_abst<-subset(merged_data,							
							merged_data$p_consumption==0)

######### STACKING DATA
stacked_data<-rbind(with(data_nonabst,data.frame(
							audit_score=p_audit_score,
							audit_score_cat=p_audit_score_cat,
							pas_score=p_pas_score,
							consumption=p_consumption,
							group=p_groups	
							)),
					with(data_nonabst,data.frame(
							audit_score=f_audit_score,
							audit_score_cat=f_audit_score_cat,
							pas_score=f_pas_score,
							consumption=f_consumption,
							group=f_groups))
					)

# #Non-Abstainners = everyone who responded 1 or 2 in the consumption question
# data_nonabst<-subset(stacked_data,							
# 								stacked_data$consumption!=0)

# data_full1<-rbind(data_nonabst,data_abst)
# # argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
# imp2 <- mice(data_full1, seed = 2222, m=5)


# # reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
# data_full<-complete(imp2,4)

###########################################

with(stacked_data,by(audit_score,group,summary))
with(stacked_data,kruskal.test(audit_score~group))

table<-with(merged_data,table(p_audit_score_cat))
table
prop.table(table)

table<-with(merged_data,table(f_audit_score_cat))
table
prop.table(table)

table<-with(merged_data,table(p_audit_score_cat,f_audit_score_cat))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)


with(stacked_data,by(pas_score,group,summary))
with(stacked_data,kruskal.test(pas_score~group))

# with(stacked_data,by(discrimination,group,summary))
# with(stacked_data,kruskal.test(discrimination~group))

# with(stacked_data,by(devaluation,group,summary))
# with(stacked_data,kruskal.test(devaluation~group))


library(irr)
kappa2(with(merged_data,
		data.frame(p_audit_score_cat,f_audit_score_cat)),
		 "unweighted")



fit_glm <- glm(p_audit_score_cat ~ f_audit_score, data_nonabst, family=binomial(link="logit"))

glm_link_scores <- predict(fit_glm, data_nonabst, type="link")

glm_response_scores <- predict(fit_glm, data_nonabst, type="response")

score_data <- data.frame(link=glm_link_scores, 
                         response=glm_response_scores,
                         p_audit_score_cat=data_nonabst$p_audit_score_cat,
                         stringsAsFactors=FALSE)

library(epiR)



library(pROC)
library(Epi)

library(pROC)

auc(data_nonabst$p_audit_score_cat, glm_response_scores)

plot<-plot(roc(data_nonabst$p_audit_score_cat, 
			glm_response_scores, direction="<"))
     # col="yellow", lwd=3, main="The turtle finds its way")
print(plot)

plot

ROC(form=p_audit_score_cat~f_audit_score, data=merged_data)


merged_data<-merged_data %>% mutate(discord = case_when(
				p_audit_score_cat == f_audit_score_cat ~ 'agree',
				p_audit_score_cat != f_audit_score_cat ~ 'disagree'))

table(merged_data$discord)
prop.table(table(merged_data$discord))

with(merged_data,by(p_audit_score,discord,summary))
with(merged_data,by(f_audit_score,discord,summary))
with(merged_data,by(p_pas_score,discord,summary))
with(merged_data,by(f_pas_score,discord,summary))
with(merged_data,by(age,discord,summary))
with(merged_data,by(p_drinc_data_score,discord,summary))

table<-with(merged_data,table(p_consumption,discord))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)

table<-with(merged_data,table(sex,discord))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)


#NETWORK DATA

network_data<-with(merged_data,data.frame(
			p_audit_score,
			p_pas_score,
			p_drinc_data_score,
			f_audit_score,
			f_pas_score
			))
library(qgraph)
cor_data<-cor_auto(network_data)

qgraph(cor_data,layout="spring",graph="glasso",
	sampleSize=nrow(network_data))


#TWO NETWORKS

patients_network_data<-data.frame(alcoholic_close_friend=data_patients$alcoholic_close_friend,
              recovered_alcoholic_teacher=data_patients$recovered_alcoholic_teacher,
              recover_alcoholic_chldrn=data_patients$recover_alcoholic_chldrn,
              recover_alcoholic_hired=data_patients$recover_alcoholic_hired,
              non_alcoholic_hired=data_patients$non_alcoholic_hired,
              recovered_alc_treat_same=data_patients$recovered_alc_treat_same,
              no_date_hospital_for_alc=data_patients$no_date_hospital_for_alc,
              alc_treatment_intelligent=data_patients$alc_treatment_intelligent,
              alcoholic_trustworthy=data_patients$alcoholic_trustworthy,
              alc_treatment_failure=data_patients$alc_treatment_failure,
              think_less_treated_person=data_patients$think_less_treated_person,
              less_opinion_trtd_person=data_patients$less_opinion_trtd_person,
              audit_score=data_patients$audit_score)
              # drinc_score=data_patients$drinc_data_score)

cor_data<-cor_auto(patients_network_data)

patient_graph<-qgraph(cor_data,layout="spring",graph="glasso",
	sampleSize=nrow(patients_network_data),
	cut = 0.2,label.scale=FALSE,nodeNames=names(patients_network_data))

# library(igraph)
# #Calculating Community measures
# g<-as.igraph(patient_graph) #creating igraph object
# h<-walktrap.community(g) #creatin community object
# # h<-spinglass.community(g, weights=NA)
# plot(h,g) #plotting community network
# h$membership #extracting 


family_score_data<-with(data_family, data.frame(
              alcoholic_close_friend,
              recovered_alcoholic_teacher,
              recover_alcoholic_chldrn,
              recover_alcoholic_hired,
              non_alcoholic_hired,
              recovered_alc_treat_same,
              no_date_hospital_for_alc,
              alc_treatment_intelligent,
              alcoholic_trustworthy,
              alc_treatment_failure,
              think_less_treated_person,
              less_opinion_trtd_person,
              audit_score))

cor_data<-cor_auto(family_score_data)

family_graph<-qgraph(cor_data,layout="spring",graph="glasso",
	sampleSize=nrow(family_score_data),
	cut = 0.2,label.scale=FALSE,nodeNames=names(family_score_data))

