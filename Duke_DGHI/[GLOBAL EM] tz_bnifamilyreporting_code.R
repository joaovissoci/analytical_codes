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
		 "pastecs",
		 "repmis",
		 "polycor",
		 "mice"), 
library, character.only=T)

######################################################################
#IMPORTING DATA
######################################################################
#LOADING DATA FROM A .CSV FILE

data_patients<-read.csv("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/Africa/Tz/BNI/Tz_bnipatients_data.csv")

data_family<-read.csv("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/Africa/Tz/BNI/Tz_bniKAfamily_data.csv")

######################################################################
#DATA MANAGEMENT
######################################################################

names(data)

data_patients$group<-c("Patient")
data_family$group<-c("Family")

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
	"0:8='No';else='Yes'")

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

#STIGMA
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
							less_opinion_trtd_person))


#recoding positive oriented items to ensure a higher score indicates high stigma
patients_score_data$alcoholic_close_friend<-car::recode(data_patients$alcoholic_close_friend,
	"1=6;2=5;3=4;4=3;5=2;6=1")
patients_score_data$alc_treatment_intelligent<-car::recode(data_patients$alc_treatment_intelligent,
	"1=6;2=5;3=4;4=3;5=2;6=1")
patients_score_data$alcoholic_trustworthy<-car::recode(data_patients$alcoholic_trustworthy,
	"1=6;2=5;3=4;4=3;5=2;6=1")
patients_score_data$recovered_alcoholic_teacher<-car::recode(data_patients$recovered_alcoholic_teacher,
	"1=6;2=5;3=4;4=3;5=2;6=1")
patients_score_data$recover_alcoholic_hired<-car::recode(data_patients$recover_alcoholic_hired,
	"1=6;2=5;3=4;4=3;5=2;6=1")
patients_score_data$recovered_alc_treat_same<-car::recode(data_patients$recovered_alc_treat_same,
	"1=6;2=5;3=4;4=3;5=2;6=1")

#Calculate PAS
patients_data_PAS<-with(patients_score_data,data.frame(alcoholic_close_friend,
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
							less_opinion_trtd_person))

# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
imp <- mice(patients_data_PAS, seed = 2222, m=5)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
patients_data_PAS<-complete(imp,4)

data_patients$pas_score<-rowSums(patients_data_PAS)/ncol(patients_data_PAS)

# discrimination<-na.omit(discrimination)
# rescale <- function(x)(x-min(x))/(max(x) - min(x)) * 100
# discrimination_scaled<-lapply(pas_score,rescale)
# pas_score_scaled<-rescale(pas_score)
data_patients$pas_score_cat<-car::recode(data_patients$pas_score,"0:3='low';else='high'")

#Calculate PDiscrimination score
patients_data_PDis<-with(patients_score_data,data.frame(alcoholic_close_friend,
							recovered_alcoholic_teacher,
							recover_alcoholic_chldrn,
							recover_alcoholic_hired,
							non_alcoholic_hired,
							recovered_alc_treat_same,
							no_date_hospital_for_alc))

# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
# imp <- mice(data_PDis, seed = 2222, m=5)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
# data_PDis<-complete(imp,4)

data_patients$discrimination<-rowSums(patients_data_PDis)/ncol(patients_data_PDis)
# discrimination<-na.omit(discrimination)
# rescale <- function(x)(x-min(x))/(max(x) - min(x)) * 100
# discrimination_scaled<-lapply(discrimination,rescale)
# discrimination_scaled<-rescale(discrimination)
# summary(discrimination)
# describe(discrimination)
data_patients$discrimination_cat<-car::recode(data_patients$discrimination,"0:3='low';else='high'")


#Calculate Perceived Devaluation score
patients_data_PDev<-with(patients_score_data,data.frame(alc_treatment_intelligent,
							alcoholic_trustworthy,
							alc_treatment_failure,
							think_less_treated_person,
							less_opinion_trtd_person))

# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
imp <- mice(patients_data_PDev, seed = 2222, m=5)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
patients_data_PDev<-complete(imp,4)

data_patients$devaluation<-rowSums(patients_data_PDev)/ncol(patients_data_PDev)
# devaluation<-na.omit(devaluation)
# rescale <- function(x)(x-min(x))/(max(x) - min(x)) * 100
# devaluation_scaled<-lapply(devaluation,rescale)
# devaluation_scaled<-rescale(devaluation)
data_patients$devaluation_cat<-car::recode(data_patients$devaluation,"0:3='low';else='high'")

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
"stigma_complete")

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
	"0:8='No';else='Yes'")

#STIGMA
family_score_data<-with(data_family, data.frame(alcoholic_close_friend,
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
							less_opinion_trtd_person))


#recoding positive oriented items to ensure a higher score indicates high stigma
family_score_data$alcoholic_close_friend<-car::recode(data_family$alcoholic_close_friend,
	"1=6;2=5;3=4;4=3;5=2;6=1")
family_score_data$alc_treatment_intelligent<-car::recode(data_family$alc_treatment_intelligent,
	"1=6;2=5;3=4;4=3;5=2;6=1")
family_score_data$alcoholic_trustworthy<-car::recode(data_family$alcoholic_trustworthy,
	"1=6;2=5;3=4;4=3;5=2;6=1")
family_score_data$recovered_alcoholic_teacher<-car::recode(data_family$recovered_alcoholic_teacher,
	"1=6;2=5;3=4;4=3;5=2;6=1")
family_score_data$recover_alcoholic_hired<-car::recode(data_family$recover_alcoholic_hired,
	"1=6;2=5;3=4;4=3;5=2;6=1")
family_score_data$recovered_alc_treat_same<-car::recode(data_family$recovered_alc_treat_same,
	"1=6;2=5;3=4;4=3;5=2;6=1")

#Calculate PAS
family_data_PAS<-with(family_score_data,data.frame(alcoholic_close_friend,
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
							less_opinion_trtd_person))

# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
imp <- mice(family_data_PAS, seed = 2222, m=5)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
family_data_PAS<-complete(imp,4)

data_family$pas_score<-rowSums(family_data_PAS)/ncol(family_data_PAS)

# discrimination<-na.omit(discrimination)
# rescale <- function(x)(x-min(x))/(max(x) - min(x)) * 100
# discrimination_scaled<-lapply(pas_score,rescale)
# pas_score_scaled<-rescale(pas_score)
data_family$pas_score_cat<-car::recode(data_family$pas_score,"0:3='low';else='high'")

#Calculate PDiscrimination score
family_data_PDis<-with(family_score_data,data.frame(alcoholic_close_friend,
							recovered_alcoholic_teacher,
							recover_alcoholic_chldrn,
							recover_alcoholic_hired,
							non_alcoholic_hired,
							recovered_alc_treat_same,
							no_date_hospital_for_alc))

# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
# imp <- mice(data_PDis, seed = 2222, m=5)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
# data_PDis<-complete(imp,4)

data_family$discrimination<-rowSums(family_data_PDis)/ncol(family_data_PDis)
# discrimination<-na.omit(discrimination)
# rescale <- function(x)(x-min(x))/(max(x) - min(x)) * 100
# discrimination_scaled<-lapply(discrimination,rescale)
# discrimination_scaled<-rescale(discrimination)
# summary(discrimination)
# describe(discrimination)
data_family$discrimination_cat<-car::recode(data_family$discrimination,"0:3='low';else='high'")


#Calculate Perceived Devaluation score
family_data_PDev<-with(family_score_data,data.frame(alc_treatment_intelligent,
							alcoholic_trustworthy,
							alc_treatment_failure,
							think_less_treated_person,
							less_opinion_trtd_person))

# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
# imp <- mice(data_PDev, seed = 2222, m=5)

# # reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
# data_PDev<-complete(imp,4)

data_family$devaluation<-rowSums(family_data_PDev)/ncol(family_data_PDev)
# devaluation<-na.omit(devaluation)
# rescale <- function(x)(x-min(x))/(max(x) - min(x)) * 100
# devaluation_scaled<-lapply(devaluation,rescale)
# devaluation_scaled<-rescale(devaluation)
data_family$devaluation_cat<-car::recode(data_family$devaluation,"0:3='low';else='high'")

#Merging datasets
merged_data<-merge(x = with(data_patients,data.frame(
							p_audit_score=audit_score,
							p_audit_score_cat=audit_score_cat,
							p_pas_score=pas_score,
							p_drinc_data_score=drinc_data_score,
							p_drinc_data_score_cat=drinc_data_score_cat,
							p_pas_score_cat=pas_score_cat,
							p_discrimination=discrimination,
							p_discrimination_cat=discrimination_cat,
							p_devaluation=devaluation,
							p_devaluation_cat=devaluation_cat,
							study_id,
							p_consumption=consumption,
							p_groups=group
							)), 
					   y = with(data_family,data.frame(
							f_audit_score=audit_score,
							f_audit_score_cat=audit_score_cat,
							f_pas_score=pas_score,
							f_pas_score_cat=pas_score_cat,
							f_discrimination=discrimination,
							f_discrimination_cat=discrimination_cat,
							f_devaluation=devaluation,
							f_devaluation_cat=devaluation_cat,
							study_id,
							f_consumption=consumption,
							f_groups=group
							)), 
					   by = "study_id", 
					   all.x = TRUE)

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
							pas_score_cat=p_pas_score_cat,
							discrimination=p_discrimination,
							discrimination_cat=p_discrimination_cat,
							devaluation=p_devaluation,
							devaluation_cat=p_devaluation_cat,
							consumption=p_consumption,
							group=p_groups	
							)),
					with(data_nonabst,data.frame(
							audit_score=f_audit_score,
							audit_score_cat=f_audit_score_cat,
							pas_score=f_pas_score,
							pas_score_cat=f_pas_score_cat,
							discrimination=f_discrimination,
							discrimination_cat=f_discrimination_cat,
							devaluation=f_devaluation,
							devaluation_cat=f_devaluation_cat,
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

table<-with(stacked_data,table(audit_score_cat,group))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

with(stacked_data,by(pas_score,group,summary))
with(stacked_data,kruskal.test(pas_score~group))

with(stacked_data,by(discrimination,group,summary))
with(stacked_data,kruskal.test(discrimination~group))

with(stacked_data,by(devaluation,group,summary))
with(stacked_data,kruskal.test(devaluation~group))


