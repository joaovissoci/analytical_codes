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
		 # "mvnormtest",
		 "polycor",
		 "mice",
		 "semPlot",
		 "tidyverse"), 
library, character.only=T)

######################################################################
#IMPORTING DATA
######################################################################
#LOADING DATA FROM A .CSV FILE

data_patients<-read.csv("/Users/Joao/Box Sync/Home Folder jnv4/Data/Global EM/Africa/Tz/BNI/Tz_bnipatients_data.csv")

data_family<-read.csv("/Users/Joao/Box Sync/Home Folder jnv4/Data/Global EM/Africa/Tz/BNI/Tz_bniKAfamily_data.csv")

######################################################################
#DATA MANAGEMENT
######################################################################

#creating family and patients group variables

data_patients$group<-c("Patients")
data_family$group<-c("Family")

# names(data)

#####socioeconomic variables

data_patients$daily_drink<-car::recode(
	data_patients$daily_drink,"
	998=NA;
	999=NA;
	'2 GLAS'=2;
	'3-Feb'=3;
	'4-Mar'=4;
	'7-May'=7")
data_patients$daily_drink<-as.numeric(as.character(data_patients$daily_drink))

#### ALcohol use consequences

#drinking_interferes

data_patients$drinking_interferes<-car::recode(
	data_patients$drinking_interferes,"
	1='yes';
	5='no';
	else=NA")

#drinking_arguments

data_patients$drinking_arguments<-car::recode(
	data_patients$drinking_arguments,"
	1='yes';
	5='no';
	else=NA")

#could_get_hurt

data_patients$could_get_hurt<-car::recode(
	data_patients$could_get_hurt,"
	1='yes';
	5='no';
	else=NA")

#police_bc_drink

data_patients$police_bc_drink<-car::recode(
	data_patients$police_bc_drink,"
	1='yes';
	5='no';
	else=NA")

# AUDIT
######################################################################

# data<-subset(data1,data1$audit_complete==2)

audit_data<-with(data_patients,data.frame(
				how_often_drink,                  
				number_drinks_day,
				how_often_6_more_drinks,
				how_often_cant_stop_drinking,
				fail_expectation_bc_drinking,
				how_often_drink_morning,
				how_often_guilt_postdrinking,
				how_often_no_memory_postdrinking,
				drinking_injured_you_or_someone,
				others_concerned_your_drinking))


# audit_data_questions<-audit_data[-c(1:2,13)]

NAto0<-function(x){
	car::recode(x,"NA=0")
	}

audit_data_NAto0<-lapply(audit_data,NAto0)
audit_data_NAto0<-as.data.frame(audit_data_NAto0)
audit_data$audit_score<-rowSums(audit_data_NAto0)
audit_data$audit_score_D1<-rowSums(audit_data_NAto0[,1:3])/3
audit_data$audit_score_D2<-rowSums(audit_data_NAto0[,4:6])/3
audit_data$audit_score_D3<-rowSums(audit_data_NAto0[,7:10])/4

audit_data$audit_score_cat<-car::recode(
	audit_data$audit_score,
	"0:8='No';else='Yes'")

audit_data$audit_score_severe_cat<-car::recode(
	audit_data$audit_score,
	"0:19='No';else='Yes'")

# audit_data_cleaned<-audit_data[-c(1:10)]
# audit_data_cleaned<-cbind(audit_data_cleaned,
# 	audit_data_NAto0)

#### Care seeking
data_patients$talked_dr<-car::recode(data_patients$talked_dr,"
	1='yes';
	5='no';
	else=NA")

# DRINC
######################################################################

drinc_data<-with(data_patients,data.frame(
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

# # argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
imp <- mice(drinc_data, seed = 2222, m=5)

# # reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
drinc_data_questions<-mice::complete(imp,4)

drinc_data_score<-rowSums(drinc_data_questions)/2

# drinc_data_score_cat<-car::recode(
# 	drinc_data_score,"
# 	0:5='Low';else='High'")


# STIGMA
######################################################################
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

#inverting the scale
invertStigma<-function(x){
	car::recode(x,"1=6;2=5;3=4;4=3;5=2;6=1")
	}

data_stigma_inverted<-lapply(data_stigma,invertStigma)
data_stigma_inverted<-as.data.frame(data_stigma_inverted)



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

imp <- mice(data_stigma_inverted, seed = 2222, m=5)

data_stigma_inverted<-mice::complete(imp,4)

# Find factor scores for stigma

# Specifying model
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

#estimating model
fit <- lavaan::cfa(cfa_model,
                   data = data_stigma_inverted[,-13],
                   estimator="WLSMV",
                   ordered=colnames(data_stigma_inverted[,-13])
)

#Calculating the predicted scores

pas_data<-with(data_stigma_inverted,data.frame(alc_treatment_intelligent, 
       alcoholic_trustworthy,
       think_less_treated_person,
       alcoholic_close_friend,
       recovered_alcoholic_teacher,
       recover_alcoholic_hired,
       recovered_alc_treat_same,
       no_date_hospital_for_alc))

pas_scores<-lavaan::lavPredict(fit,newdata=pas_data)
pas_scores_scaled<-scales::rescale(as.data.frame(pas_scores)$BNI, 
    to = c(0, 100)) #rescaling

#Extracting scores for patients only

pas_scores_patients<-pas_scores_scaled[data_stigma_inverted$group=="Patients"]

# score_data<-with(data_nonabst, data.frame(alcoholic_close_friend,
# 							recovered_alcoholic_teacher,
# 							recover_alcoholic_chldrn,
# 							recover_alcoholic_hired,
# 							non_alcoholic_hired,
# 							recovered_alc_treat_same,
# 							no_date_hospital_for_alc,
# 							alc_treatment_intelligent,
# 							alcoholic_trustworthy,
# 							alc_treatment_failure,
# 							think_less_treated_person,
# 							less_opinion_trtd_person))


# #recoding positive oriented items to ensure a higher score indicates high stigma
# score_data$alcoholic_close_friend<-car::recode(data_nonabst$alcoholic_close_friend,
# 	"1=6;2=5;3=4;4=3;5=2;6=1")
# score_data$alc_treatment_intelligent<-car::recode(data_nonabst$alc_treatment_intelligent,
# 	"1=6;2=5;3=4;4=3;5=2;6=1")
# score_data$alcoholic_trustworthy<-car::recode(data_nonabst$alcoholic_trustworthy,
# 	"1=6;2=5;3=4;4=3;5=2;6=1")
# score_data$recovered_alcoholic_teacher<-car::recode(data_nonabst$recovered_alcoholic_teacher,
# 	"1=6;2=5;3=4;4=3;5=2;6=1")
# score_data$recover_alcoholic_hired<-car::recode(data_nonabst$recover_alcoholic_hired,
# 	"1=6;2=5;3=4;4=3;5=2;6=1")
# score_data$recovered_alc_treat_same<-car::recode(data_nonabst$recovered_alc_treat_same,
# 	"1=6;2=5;3=4;4=3;5=2;6=1")

# #Calculate PAS
# figure3_data_PAS<-with(score_data,data.frame(alcoholic_close_friend,
# 							recovered_alcoholic_teacher,
# 							recover_alcoholic_chldrn,
# 							recover_alcoholic_hired,
# 							non_alcoholic_hired,
# 							recovered_alc_treat_same,
# 							no_date_hospital_for_alc,
# 							alc_treatment_intelligent,
# 							alcoholic_trustworthy,
# 							alc_treatment_failure,
# 							think_less_treated_person,
# 							less_opinion_trtd_person))

# # # argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
# # imp <- mice(figure3_data_PAS, seed = 2222, m=5)

# # # reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
# # figure3_data_PAS<-mice::complete(imp,4)

# pas_score<-rowSums(figure3_data_PAS)/ncol(figure3_data_PAS)
# # summary(pas_score)
# # describe(pas_score)
# # discrimination<-na.omit(discrimination)
# # rescale <- function(x)(x-min(x))/(max(x) - min(x)) * 100
# # discrimination_scaled<-lapply(pas_score,rescale)
# # pas_score_scaled<-rescale(pas_score)
# # summary(x)
# # sd(x)
# # pas_score_cat<-car::recode(pas_score,"0:3='low';else='high'")

# #Calculate PDiscrimination score
# figure3_data_PDis<-with(score_data,data.frame(alcoholic_close_friend,
# 							recovered_alcoholic_teacher,
# 							recover_alcoholic_chldrn,
# 							recover_alcoholic_hired,
# 							non_alcoholic_hired,
# 							recovered_alc_treat_same,
# 							no_date_hospital_for_alc))

# # # argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
# # imp <- mice(figure3_data_PDis, seed = 2222, m=5)

# # # reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
# # figure3_data_PDis<-mice::complete(imp,4)

# discrimination<-rowSums(figure3_data_PDis)/ncol(figure3_data_PDis)
# # discrimination<-na.omit(discrimination)
# # rescale <- function(x)(x-min(x))/(max(x) - min(x)) * 100
# # discrimination_scaled<-lapply(discrimination,rescale)
# # discrimination_scaled<-rescale(discrimination)
# # summary(discrimination)
# # describe(discrimination)
# # discrimination_cat<-car::recode(discrimination,"0:3='low';else='high'")


# #Calculate Perceived Devaluation score
# figure3_data_PDev<-with(score_data,data.frame(alc_treatment_intelligent,
# 							alcoholic_trustworthy,
# 							alc_treatment_failure,
# 							think_less_treated_person,
# 							less_opinion_trtd_person))

# # # argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
# # imp <- mice(figure3_data_PDev, seed = 2222, m=5)

# # # reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
# # figure3_data_PDev<-mice::complete(imp,4)

# devaluation<-rowSums(figure3_data_PDev)/ncol(figure3_data_PDev)
# # devaluation<-na.omit(devaluation)
# # rescale <- function(x)(x-min(x))/(max(x) - min(x)) * 100
# # devaluation_scaled<-lapply(devaluation,rescale)
# # devaluation_scaled<-rescale(devaluation)
# # summary(devaluation)
# # describe(devaluation)
# devaluation_cat<-car::recode(devaluation,"0:3='low';else='high'")

data_full<-data.frame(age=data_patients$age,
					  female=data_patients$female,
					  pos_etoh=data_patients$pos_etoh,
					  daily_drink=data_patients$daily_drink,
					  # number_drinks_day=data_patients$number_drinks_day,
					  drinking_interferes=data_patients$drinking_interferes,
					  drinking_arguments=data_patients$drinking_arguments,
					  could_get_hurt=data_patients$could_get_hurt,
					  police_bc_drink=data_patients$police_bc_drink,
					  stigma=pas_scores_patients,
					  # devaluation,
					  # discrimination,
					  talked_dr=data_patients$talked_dr,
					  # helpful_treatment=data_patients$helpful_treatment,
					  # recent_trtmnt=data_patients$recent_trtmnt,
					  # hospital_alc=data_patients$hospital_alc,
					  # selfhelp_group=data_patients$selfhelp_group,
					  audit_total=audit_data$audit_score,
					  audit_alcoholuse=audit_data$audit_score_D1,
					  audit_alcoholdependence=audit_data$audit_score_D2,
					  audit_alcoholrisk=audit_data$audit_score_D3,
					  audit_cat=audit_data$audit_score_cat,
					  drinc_data_score,
					  age_1st_drink=data_patients$age_1st_drink,
					  age_12ormore_drinks_yr=data_patients$age_12ormore_drinks_yr,
					  consumption=data_patients$consumption)

data_full %>% mutate(test = ifelse((talked_dr == 'NA' & consumption == 0), 
									1,
									talked_dr)) -> tested


# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
imp <- mice(data_full, seed = 2222, m=5)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
data_full<-mice::complete(imp,4)

#Isolating only non_abst data
data_nonabst<-subset(data_full,data_patients$consumption!=0)



######################################################################
#POWER 
######################################################################

library(pwr)
pwr.t.test(n=NULL, d = 0.4, sig.level = .05, power = .8, type = c("two.sample"))


######################################################################
#TABLE 1
######################################################################


#### NOT RUN
describe(data_full$age_1st_drink)
describe(data_full$age_12ormore_drinks_yr)


cost_study<-with(data_full,data.frame(age_12ormore_drinks_yr,
									  age_1st_drink,
									  audit_total))

cor(cost_study,method="spearman")

#non abstainers?
#cost?
#availability

#### NOT RUN
with(data_nonabst,table(talked_dr))
with(data_nonabst,prop.table(table(talked_dr)))


#Age
with(data_nonabst,
	psych::describe(age))
with(data_nonabst,
	by(age,talked_dr,describe))
with(data_nonabst,
	t.test(age~talked_dr))
logmodel<-glm(talked_dr ~ age
			,family=binomial, data=data_nonabst)
summary(logmodel)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#gender
with(data_nonabst,table(female))
with(data_nonabst,prop.table(table(female)))
table<-with(data_nonabst,table(female,talked_dr))
table
with(data_nonabst,prop.table(table(female,talked_dr),2))
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package
logmodel<-glm(talked_dr ~ female
			,family=binomial, data=data_nonabst)
summary(logmodel)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#alcohol
with(data_nonabst,table(pos_etoh))
with(data_nonabst,prop.table(table(pos_etoh)))
table<-with(data_nonabst,table(pos_etoh,talked_dr))
table
with(data_nonabst,prop.table(table(pos_etoh,talked_dr),2))
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package
logmodel<-glm(talked_dr ~ pos_etoh
			,family=binomial, data=data_nonabst)
summary(logmodel)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#at-risk alcohol Users
with(data_nonabst,table(audit_cat))
with(data_nonabst,prop.table(table(audit_cat)))
table<-with(data_nonabst,table(audit_cat,talked_dr))
table
with(data_nonabst,prop.table(table(audit_cat,talked_dr),2))
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package
logmodel<-glm(talked_dr ~ audit_cat
			,family=binomial, data=data_nonabst)
summary(logmodel)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#daily_drink
with(data_nonabst,
	summary(daily_drink))
with(data_nonabst,
	by(daily_drink,talked_dr,summary))
with(data_nonabst,
	wilcox.test(daily_drink~talked_dr))
# logmodel<-glm(talked_dr ~ daily_drink
# 			,family=binomial, data=data_nonabst)
# summary(logmodel)
# exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#drinking_interferes
with(data_nonabst,table(drinking_interferes))
with(data_nonabst,prop.table(table(drinking_interferes)))
table<-with(data_nonabst,table(drinking_interferes,talked_dr))
table
with(data_nonabst,prop.table(table(drinking_interferes,talked_dr),2))
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package
logmodel<-glm(talked_dr ~ drinking_interferes
			,family=binomial, data=data_nonabst)
summary(logmodel)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#drinking_arguments
with(data_nonabst,table(drinking_arguments))
with(data_nonabst,prop.table(table(drinking_arguments)))
table<-with(data_nonabst,table(drinking_arguments,talked_dr))
table
with(data_nonabst,prop.table(table(drinking_arguments,talked_dr),2))
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package
logmodel<-glm(talked_dr ~ drinking_arguments
			,family=binomial, data=data_nonabst)
summary(logmodel)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#could_get_hurt
with(data_nonabst,table(could_get_hurt))
with(data_nonabst,prop.table(table(could_get_hurt)))
table<-with(data_nonabst,table(could_get_hurt,talked_dr))
table
with(data_nonabst,prop.table(table(could_get_hurt,talked_dr),2))
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package
logmodel<-glm(talked_dr ~ could_get_hurt
			,family=binomial, data=data_nonabst)
summary(logmodel)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#police_bc_drink
with(data_nonabst,table(police_bc_drink))
with(data_nonabst,prop.table(table(police_bc_drink)))
table<-with(data_nonabst,table(police_bc_drink,talked_dr))
table
with(data_nonabst,prop.table(table(police_bc_drink,talked_dr),2))
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package
logmodel<-glm(talked_dr ~ police_bc_drink
			,family=binomial, data=data_nonabst)
summary(logmodel)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#pas_score
with(data_nonabst,
	summary(stigma))
with(data_nonabst,
	by(stigma,talked_dr,summary))
with(data_nonabst,
	wilcox.test(stigma~talked_dr))
logmodel<-glm(talked_dr ~ stigma
			,family=binomial, data=data_nonabst)
summary(logmodel)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

# #devaluation
# with(data_nonabst,
# 	summary(devaluation))
# with(data_nonabst,
# 	by(devaluation,talked_dr,summary))
# with(data_nonabst,
# 	wilcox.test(devaluation~talked_dr))
# logmodel<-glm(talked_dr ~ devaluation
# 			,family=binomial, data=data_nonabst)
# summary(logmodel)
# exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

# #discrimination
# with(data_nonabst,
# 	summary(discrimination))
# with(data_nonabst,
# 	by(discrimination,talked_dr,summary))
# with(data_nonabst,
# 	wilcox.test(discrimination~talked_dr))
# logmodel<-glm(talked_dr ~ discrimination
# 			,family=binomial, data=data_nonabst)
# summary(logmodel)
# exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#audit_total
with(data_nonabst,
	summary(audit_total))
with(data_nonabst,
	by(audit_total,talked_dr,summary))
with(data_nonabst,
	wilcox.test(audit_total~talked_dr))
logmodel<-glm(talked_dr ~ audit_total
			,family=binomial, data=data_nonabst)
summary(logmodel)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#audit_alcoholuse
with(data_nonabst,
	summary(audit_alcoholuse))
with(data_nonabst,
	by(audit_alcoholuse,talked_dr,summary))
with(data_nonabst,
	wilcox.test(audit_alcoholuse~talked_dr))
logmodel<-glm(talked_dr ~ age
			,family=binomial, data=data_nonabst)
summary(logmodel)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#audit_alcoholdependence
with(data_nonabst,
	summary(audit_alcoholdependence))
with(data_nonabst,
	by(audit_alcoholdependence,talked_dr,summary))
with(data_nonabst,
	wilcox.test(audit_alcoholdependence~talked_dr))
logmodel<-glm(talked_dr ~ age
			,family=binomial, data=data_nonabst)
summary(logmodel)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#audit_alcoholrisk
with(data_nonabst,
	summary(audit_alcoholrisk))
with(data_nonabst,
	by(audit_alcoholrisk,talked_dr,summary))
with(data_nonabst,
	wilcox.test(audit_alcoholrisk~talked_dr))
logmodel<-glm(talked_dr ~ age
			,family=binomial, data=data_nonabst)
summary(logmodel)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#drinc_data_score
with(data_nonabst,
	summary(drinc_data_score))
with(data_nonabst,
	by(drinc_data_score,talked_dr,summary))
with(data_nonabst,
	wilcox.test(drinc_data_score~talked_dr))
logmodel<-glm(talked_dr ~ drinc_data_score
			,family=binomial, data=data_nonabst)
summary(logmodel)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 


logmodel<-glm(talked_dr ~ pos_etoh +
						  daily_drink +
						  drinking_interferes +
						  drinking_arguments +
						  could_get_hurt +
						  audit_total +						
						  drinc_data_score
			,family=binomial, data=data_nonabst)
summary(logmodel)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

######################################################################
#TABLE 2
######################################################################

linearmodel<-glm(pas_score ~ 
						  age +
						  pos_etoh +
						  daily_drink +
						  drinking_interferes +
						  drinking_arguments +
						  could_get_hurt +
						  audit_total +						
						  drinc_data_score
			,family=gaussian, data=data_full)
summary(linearmodel)
exp(cbind(Odds=coef(linearmodel),confint(linearmodel,level=0.95))) 


######################################################################
#FIGURE 1
######################################################################

#
network_data<-with(data_full,data.frame(#age=data_nonabst$age,
					  #female=data_nonabst$female,
					  # pos_etoh=data_nonabst$pos_etoh,
					  # daily_drink=data_nonabst$daily_drink,
					  # # number_drinks_day=data_nonabst$number_drinks_day,
					  drinking_interferes=as.numeric(as.factor(data_nonabst$drinking_interferes)),
					  drinking_arguments=as.numeric(as.factor(data_nonabst$drinking_arguments)),
					  could_get_hurt=as.numeric(as.factor(data_nonabst$could_get_hurt)),
					  # police_bc_drink=as.numeric(as.factor(data_nonabst$police_bc_drink)),
					  pas_scores_scaled,
					  # devaluation,
					  # discrimination,
					  talked_dr=as.numeric(as.factor(talked_dr)),
					  # helpful_treatment=data_nonabst$helpful_treatment,
					  # recent_trtmnt=data_nonabst$recent_trtmnt,
					  # hospital_alc=data_nonabst$hospital_alc,
					  # selfhelp_group=data_nonabst$selfhelp_group,
					  audit_total=audit_total,
					  # audit_alcoholuse=audit_data$audit_score_D1,
					  # audit_alcoholdependence=audit_data$audit_score_D2,
					  # audit_alcoholrisk=audit_data$audit_score_D3.
					  drinc_data_score
					  ))

cor<-cor_auto(network_data)

network_glasso<-qgraph(
                    cor,
                    layout="spring", 
                    # vsize=tau,
                    # esize=20,
                    graph="glasso",
                    sampleSize=nrow(network_data),
                    legend.cex = 0.5,
                    GLratio=1.5,
                    minimum=0.1,
                    cut=0.1,
                    border.width=1.5,
                    shape="square"
                    )

# Mediation analysis

model <- ' # direct effect
             talked_dr ~ a*audit_total
             talked_dr ~ b*stigma
             talked_dr ~ c*drinc_data_score
           # mediator
             stigma ~ d*audit_total
             drinc_data_score ~ e*audit_total
             stigma ~ f*drinc_data_score
           # indirect effect (a*b)
             db := d*b
             efb := e*f*b
             ec := e*c
           # total effect
             # total := c + (a*b)
         '

fit <- lavaan::sem(model, data = data_nonabst,
					estimator="WLSMV",
					ordered=colnames(data_nonabst$talked_dr))
lavaan::summary(fit,standardized=TRUE,
					fit.measures=TRUE,
					rsq=TRUE)
lavaan::fitMeasures(fit, fit.measures = "all", baseline.model = NULL)
Est <- lavaan::parameterEstimates(fit, ci = TRUE, standardized = TRUE)
subset(Est, op == "~")
subset(Est, op == ":=")

# Mediation analysis - STIGMA

model <- ' # direct effect
             talked_dr ~ dA*audit_total
             talked_dr ~ dB*drinc_data_score
             talked_dr ~ dC*pas_scores_scaled
             # talked_dr ~ dC*devaluation
             # talked_dr ~ dD*discrimination
           
           # mediator
             drinc_data_score ~ indA*audit_total
             pas_scores_scaled ~ indB*drinc_data_score
             audit_total ~ indC*pas_scores_scaled             
             # devaluation ~ indB*drinc_data_score
             # audit_total ~ indC*devaluation
             # discrimination ~ indD*drinc_data_score
             # audit_total ~ indE*discrimination
            
           # # indirect effect (a*b)
           #   ab := a*b
           # # total effect
           #   total := c + (a*b)
         '

fit <- lavaan::sem(model, data = network_data,
					estimator="WLSMV",
					ordered=colnames(data_nonabst$talked_dr))
lavaan::summary(fit,standardized=TRUE,
					fit.measures=TRUE,
					rsq=TRUE)
lavaan::fitMeasures(fit, fit.measures = "all", baseline.model = NULL)
Est <- lavaan::parameterEstimates(fit, ci = TRUE, standardized = TRUE)
subset(Est, op == "~")
subset(Est, op == ":=")


# tiff("/Users/jnv4/Desktop/resilience_stress_fig2.tiff", units='in', 
#   width = 15,
#  height = 10,compression = 'lzw',res=1200,bg = "white")
# semPaths(fit,"std",residuals=TRUE, cut=1,
#   equalizeManifests=TRUE,edge.color="black",exoCov=FALSE,
#   intercepts=FALSE, nodeLabels=nodeLabels,label.scale=FALSE,
#   edge.label.cex=1, label.cex=labelcex, color=color,borders=borders)
# dev.off()

# ### Modification Indexes
# Mod <- modificationIndices(fit)
# subset(Mod, mi > 10)


semPlot::semPaths(fit,
                  "model",
                  "std",
                  layout="tree2",
                  style="lisrel",
                  residuals=FALSE,
                  # cut=1,
                  # equalizeManifests=TRUE,
                  # edge.color="black",
                  exoCov=FALSE,
                  intercepts=FALSE,
                  # nodeLabels=colnames(network_data),
                  label.scale=FALSE,
                  edge.label.cex=0.8
                  # label.cex=labelcex,
                  # color=color,
                  # borders=borders
				  )
######################################################################
#FIGURE 2
######################################################################
