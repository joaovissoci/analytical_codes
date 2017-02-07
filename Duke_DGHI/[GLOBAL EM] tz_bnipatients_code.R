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
		 "mvnormtest",
		 "polycor",
		 "mice"), 
library, character.only=T)

######################################################################
#IMPORTING DATA
######################################################################
#LOADING DATA FROM A .CSV FILE

data<-read.csv("/Users/jnv4/Box Sync/Home Folder jnv4/Data/Global EM/Africa/Tz/tz_bnipatients_data.csv")

######################################################################
#DATA MANAGEMENT
######################################################################

names(data)

data<-subset(data,data$audit_complete[2]==2)


audit_data<-with(data,data.frame(
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


audit_data_questions<-audit_data[-c(1:2,13)]

NAto0<-function(x){
	car::recode(x,"NA=0")
	}

audit_data_NAto0<-lapply(audit_data_questions,NAto0)
audit_data_NAto0<-as.data.frame(audit_data_NAto0)
audit_data_questions$audit_score<-rowSums(audit_data_NAto0)
audit_data_questions$audit_score_cat<-car::recode(
	audit_data_questions$audit_score,
	"0:8='No';else='Yes'")

audit_data_cleaned<-audit_data_questions[-c(1:10)]
audit_data_cleaned<-cbind(audit_data_cleaned,
	audit_data_NAto0)

#Drinc
drinc_data<-with(data,data.frame(
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
imp <- mice(drinc_data, seed = 2222, m=5)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
drinc_data_questions<-complete(imp,4)

drinc_data_score<-rowSums(drinc_data_questions)/2
drinc_data_score_cat<-car::recode(
	drinc_data_score,"0:5='Low';else='High'")


#STIGMA
score_data<-with(data, data.frame(alcoholic_close_friend,
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
score_data$alcoholic_close_friend<-car::recode(data$alcoholic_close_friend,
	"1=6;2=5;3=4;4=3;5=2;6=1")
score_data$alc_treatment_intelligent<-car::recode(data$alc_treatment_intelligent,
	"1=6;2=5;3=4;4=3;5=2;6=1")
score_data$alcoholic_trustworthy<-car::recode(data$alcoholic_trustworthy,
	"1=6;2=5;3=4;4=3;5=2;6=1")
score_data$recovered_alcoholic_teacher<-car::recode(data$recovered_alcoholic_teacher,
	"1=6;2=5;3=4;4=3;5=2;6=1")
score_data$recover_alcoholic_hired<-car::recode(data$recover_alcoholic_hired,
	"1=6;2=5;3=4;4=3;5=2;6=1")
score_data$recovered_alc_treat_same<-car::recode(data$recovered_alc_treat_same,
	"1=6;2=5;3=4;4=3;5=2;6=1")

#Calculate PAS
figure3_data_PAS<-with(score_data,data.frame(alcoholic_close_friend,
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
imp <- mice(figure3_data_PAS, seed = 2222, m=5)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
figure3_data_PAS<-complete(imp,4)

pas_score<-rowSums(figure3_data_PAS)/ncol(figure3_data_PAS)
summary(pas_score)
describe(pas_score)
# discrimination<-na.omit(discrimination)
rescale <- function(x)(x-min(x))/(max(x) - min(x)) * 100
# discrimination_scaled<-lapply(pas_score,rescale)
pas_score<-rescale(pas_score)
# summary(x)
# sd(x)
pas_score_cat<-car::recode(pas_score,"0:3='low';else='high'")

#Calculate PDiscrimination score
figure3_data_PDis<-with(score_data,data.frame(alcoholic_close_friend,
							recovered_alcoholic_teacher,
							recover_alcoholic_chldrn,
							recover_alcoholic_hired,
							non_alcoholic_hired,
							recovered_alc_treat_same,
							no_date_hospital_for_alc))

# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
imp <- mice(figure3_data_PDis, seed = 2222, m=5)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
figure3_data_PDis<-complete(imp,4)

discrimination<-rowSums(figure3_data_PDis)/ncol(figure3_data_PDis)
# discrimination<-na.omit(discrimination)
# rescale <- function(x)(x-min(x))/(max(x) - min(x)) * 100
# discrimination_scaled<-lapply(discrimination,rescale)
discrimination_scaled<-rescale(discrimination)
# summary(discrimination)
# describe(discrimination)
discrimination_cat<-car::recode(discrimination,"0:3='low';else='high'")


#Calculate Perceived Devaluation score
figure3_data_PDev<-with(score_data,data.frame(alc_treatment_intelligent,
							alcoholic_trustworthy,
							alc_treatment_failure,
							think_less_treated_person,
							less_opinion_trtd_person))

# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
imp <- mice(figure3_data_PDev, seed = 2222, m=5)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
figure3_data_PDev<-complete(imp,4)

devaluation<-rowSums(figure3_data_PDev)/ncol(figure3_data_PDev)
# devaluation<-na.omit(devaluation)
# rescale <- function(x)(x-min(x))/(max(x) - min(x)) * 100
# devaluation_scaled<-lapply(devaluation,rescale)
devaluation_scaled<-rescale(devaluation)
summary(devaluation)
describe(devaluation)
devaluation_cat<-car::recode(devaluation,"0:3='low';else='high'")

data_nonabst<-subset(data.frame(audit_data_cleaned,
								pas_score,
								pas_score_cat,
								devaluation_scaled,
								devaluation_cat,
								discrimination_scaled,
								discrimination,
								drinc_data_score,
								drinc_data_score_cat),							
								audit_data_NAto0[1]!=0)
######################################################################
#TABLE 1
######################################################################
#Comparing AUdit Scores
with(data_nonabst,
	by(audit_score,drink_drive,summary))
with(data_nonabst,
	t.test(audit_score~drink_drive))
data_nonabst$audit_cat<-car::recode(data_nonabst$audit_score,"
	0.00:4.00='a';
	4.01:7.50='b';
	7.51:15.00='c';
	15.01:36.00='d'")

with(data_nonabst,
	by(pas_score,drink_drive,summary))
with(data_nonabst,
	wilcox.test(pas_score~drink_drive))

summary(data_nonabst$discrimination_scaled)
with(data_nonabst,
	by(discrimination_scaled,drink_drive,summary))
with(data_nonabst,
	wilcox.test(discrimination_scaled~drink_drive))

summary(data_nonabst$devaluation_scaled)
with(data_nonabst,
	by(devaluation_scaled,drink_drive,summary))
with(data_nonabst,
	wilcox.test(devaluation_scaled~drink_drive))


######################################################################
#TABLE 2
######################################################################

model<-glm(as.factor(drink_drive) ~ audit_score*pas_score,
			data=data_nonabst,
			family=binomial())
summary(model)

exp(coef(model)) # exponentiated coefficients
exp(confint(model)) # 95% CI for exponentiated coefficients

#Stigma and care seeking

care<-data.frame(care_seeking=data$talked_dr,pas_score)
care_seeking<-car::recode(care$care_seeking,"8=NA")
# x_clean<-subset(x,audit_data[2]==2)
with(care,
	by(devaluation,care_seeking,summary))

wilcox.test(pas_score~care_seeking)

# x_clean<-subset(x,audit_data[2]==2)
with(care,
	by(discrimination,care_seeking,summary))

with(care,
	wilcox.test(discrimination~care_seeking))

