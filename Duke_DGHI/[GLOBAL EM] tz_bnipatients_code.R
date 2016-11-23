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
lapply(c("Hmisc","car","psych","nortest","ggplot2","pastecs","repmis",
	"mvnormtest","polycor"), 
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


audit_data_questions<-subset(audit_data,audit_data[2]==2)
audit_data_questions<-audit_data[-c(1:2,13)]

NAto0<-function(x){
	car::recode(x,"NA=0")
	}

audit_data_NAto0<-lapply(audit_data_questions,NAto0)
audit_data_NAto0<-as.data.frame(audit_data_NAto0)
audit_score<-rowSums(audit_data_NAto0)
audit_score_cat<-car::recode(audit_score,"0:8='No';else='Yes'")

audit_score_cat_nonabst<-subset(data.frame(
	audit_score,audit_data$drink_drive,),
	audit_data_NAto0[1]!=0)

######################################################################
#TABLE 1
######################################################################
by(audit_score_cat_nonabst,)
with(audit_score_cat_nonabst,
	wilcox.test(audit_score~audit_data.drink_drive))

with(audit_score_cat_nonabst,
	describeBy(audit_score,audit_data.drink_drive))

wilcox.test(audit_data_questions$drink_drive~
	audit_score)

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

drinc_data_questions<-subset(drinc_data,audit_data[2]==2)

# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
imp <- mice(drinc_data_questions, seed = 2222, m=5)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
drinc_data_questions<-complete(imp,4)

drinc_data_score<-rowSums(drinc_data_questions)/2
drinc_data_score_cat<-car::recode(
	drinc_data_score,"0:5='Low';else='High'")

table(data$talked_dr)
prop.table(table(data$talked_dr))


#STIGMA
data$alc_treatment_failure<-car::recode(data$alc_treatment_failure,
	"1=5;2=4;3=3;4=2;5=1")
data$non_alcoholic_hired<-car::recode(data$non_alcoholic_hired,
	"1=5;2=4;3=3;4=2;5=1")
data$think_less_treated_person<-car::recode(data$think_less_treated_person,
	"1=5;2=4;3=3;4=2;5=1")
data$no_date_hospital_for_alc<-car::recode(data$no_date_hospital_for_alc,
	"1=5;2=4;3=3;4=2;5=1")
data$less_opinion_trtd_person<-car::recode(data$less_opinion_trtd_person,
	"1=5;2=4;3=3;4=2;5=1")

data_PDis<-with(data,data.frame(alcoholic_close_friend,
							recovered_alcoholic_teacher,
							recover_alcoholic_chldrn,
							recover_alcoholic_hired,
							non_alcoholic_hired,
							recovered_alc_treat_same,
							no_date_hospital_for_alc))

# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
imp <- mice(data_PDis, seed = 2222, m=5)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
data_PDis<-complete(imp,4)

discrimination<-rowSums(data_PDis)
discrimination<-na.omit(discrimination)
rescale <- function(x)(x-min(x))/(max(x) - min(x)) * 100
# discrimination_scaled<-lapply(discrimination,rescale)
x<-rescale(discrimination)
summary(x)
x_2<-car::recode(x,"0:50='a';else='b'")
table(x_2)
prop.table(table(x_2))

audit_score_cat_nonabst<-subset(data.frame(
	data$drink_drive,discrimination),
	audit_data_NAto0[1]!=0)

# x_clean<-subset(x,audit_data[2]==2)
with(audit_score_cat_nonabst,
	by(discrimination,data.drink_drive,summary))

with(audit_score_cat_nonabst,
	wilcox.test(discrimination~data.drink_drive))


data_PDev<-with(data,data.frame(alc_treatment_intelligent,
							alcoholic_trustworthy,
							alc_treatment_failure,
							think_less_treated_person,
							less_opinion_trtd_person))

# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
imp <- mice(data_PDev, seed = 2222, m=5)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
data_PDev<-complete(imp,4)

devaluation<-rowSums(data_PDev)
devaluation<-na.omit(devaluation)
rescale <- function(x)(x-min(x))/(max(x) - min(x)) * 100
# devaluation_scaled<-lapply(devaluation,rescale)
x<-rescale(devaluation)
summary(x)
x_2<-car::recode(x,"0:50='a';else='b'")
table(x_2)
prop.table(table(x_2))

audit_score_cat_nonabst<-subset(data.frame(
	data$drink_drive,devaluation),
	audit_data_NAto0[1]!=0)

# x_clean<-subset(x,audit_data[2]==2)
with(audit_score_cat_nonabst,
	by(devaluation,data.drink_drive,summary))

with(audit_score_cat_nonabst,
	wilcox.test(devaluation~data.drink_drive))

#Stigma and care seeking

care<-data.frame(care_seeking=data$talked_dr,devaluation,discrimination)
care$care_seeking<-car::recode(care$care_seeking,"8=NA")
# x_clean<-subset(x,audit_data[2]==2)
with(care,
	by(devaluation,care_seeking,summary))

with(care,
	wilcox.test(devaluation~care_seeking))

# x_clean<-subset(x,audit_data[2]==2)
with(care,
	by(discrimination,care_seeking,summary))

with(care,
	wilcox.test(discrimination~care_seeking))

