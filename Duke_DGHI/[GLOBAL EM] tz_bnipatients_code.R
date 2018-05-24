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
		 "mice"), 
library, character.only=T)

######################################################################
#IMPORTING DATA
######################################################################
#LOADING DATA FROM A .CSV FILE

data<-read.csv("/Users/Joao/Box Sync/Home Folder jnv4/Data/Global EM/Africa/Tz/BNI/Tz_bnipatients_data.csv")

######################################################################
#DATA MANAGEMENT
######################################################################

names(data)

# data<-subset(data1,data1$audit_complete==2)

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

data$audit_score_cat<-car::recode(
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
drinc_data_questions<-mice::complete(imp,4)

drinc_data_score<-rowSums(drinc_data_questions)/2
drinc_data_score_cat<-car::recode(
	drinc_data_score,"0:5='Low';else='High'")


#STIGMA
# score_data<-with(data, data.frame(alcoholic_close_friend,
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
# score_data$alcoholic_close_friend<-car::recode(data$alcoholic_close_friend,
# 	"1=6;2=5;3=4;4=3;5=2;6=1")
# score_data$alc_treatment_intelligent<-car::recode(data$alc_treatment_intelligent,
# 	"1=6;2=5;3=4;4=3;5=2;6=1")
# score_data$alcoholic_trustworthy<-car::recode(data$alcoholic_trustworthy,
# 	"1=6;2=5;3=4;4=3;5=2;6=1")
# score_data$recovered_alcoholic_teacher<-car::recode(data$recovered_alcoholic_teacher,
# 	"1=6;2=5;3=4;4=3;5=2;6=1")
# score_data$recover_alcoholic_hired<-car::recode(data$recover_alcoholic_hired,
# 	"1=6;2=5;3=4;4=3;5=2;6=1")
# score_data$recovered_alc_treat_same<-car::recode(data$recovered_alc_treat_same,
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

# # argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
# imp <- mice(figure3_data_PAS, seed = 2222, m=5)

# # reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
# figure3_data_PAS<-complete(imp,4)

# pas_score<-rowSums(figure3_data_PAS)/ncol(figure3_data_PAS)
# summary(pas_score)
# describe(pas_score)
# # discrimination<-na.omit(discrimination)
# # rescale <- function(x)(x-min(x))/(max(x) - min(x)) * 100
# # discrimination_scaled<-lapply(pas_score,rescale)
# # pas_score_scaled<-rescale(pas_score)
# # summary(x)
# # sd(x)
# pas_score_cat<-car::recode(pas_score,"0:3='low';else='high'")

# #Calculate PDiscrimination score
# figure3_data_PDis<-with(score_data,data.frame(alcoholic_close_friend,
# 							recovered_alcoholic_teacher,
# 							recover_alcoholic_chldrn,
# 							recover_alcoholic_hired,
# 							non_alcoholic_hired,
# 							recovered_alc_treat_same,
# 							no_date_hospital_for_alc))

# # argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
# imp <- mice(figure3_data_PDis, seed = 2222, m=5)

# # reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
# figure3_data_PDis<-complete(imp,4)

# discrimination<-rowSums(figure3_data_PDis)/ncol(figure3_data_PDis)
# # discrimination<-na.omit(discrimination)
# # rescale <- function(x)(x-min(x))/(max(x) - min(x)) * 100
# # discrimination_scaled<-lapply(discrimination,rescale)
# # discrimination_scaled<-rescale(discrimination)
# # summary(discrimination)
# # describe(discrimination)
# discrimination_cat<-car::recode(discrimination,"0:3='low';else='high'")


# #Calculate Perceived Devaluation score
# figure3_data_PDev<-with(score_data,data.frame(alc_treatment_intelligent,
# 							alcoholic_trustworthy,
# 							alc_treatment_failure,
# 							think_less_treated_person,
# 							less_opinion_trtd_person))

# # argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
# imp <- mice(figure3_data_PDev, seed = 2222, m=5)

# # reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
# figure3_data_PDev<-complete(imp,4)

# devaluation<-rowSums(figure3_data_PDev)/ncol(figure3_data_PDev)
# # devaluation<-na.omit(devaluation)
# # rescale <- function(x)(x-min(x))/(max(x) - min(x)) * 100
# # devaluation_scaled<-lapply(devaluation,rescale)
# # devaluation_scaled<-rescale(devaluation)
# summary(devaluation)
# describe(devaluation)
# devaluation_cat<-car::recode(devaluation,"0:3='low';else='high'")

# #STIGMA
#recoding alc_treatment_failure variable
data$alc_treatment_failure<-car::recode(data$alc_treatment_failure,"
                                     1='6';2='5';3='4';
                                     4='3';5='2';6='1'")

#recoding recover_alcoholic_chldrn variable
data$recover_alcoholic_chldrn<-car::recode(data$recover_alcoholic_chldrn,"
                                                   1='6';2='5';3='4';
                                                   4='3';5='2';6='1'")

#recoding think_less_treated_person variable
data$think_less_treated_person<-car::recode(data$think_less_treated_person,"
                                                   1='6';2='5';3='4';
                                                   4='3';5='2';6='1'")

#recoding think_less_treated_person variable
data$think_less_treated_person<-car::recode(data$think_less_treated_person,"
                                                   1='6';2='5';3='4';
                                                   4='3';5='2';6='1'")

#recoding non_alcoholic_hired variable
data$non_alcoholic_hired<-car::recode(data$non_alcoholic_hired,"
                                                       1='6';2='5';3='4';
                                                       4='3';5='2';6='1'")

#recoding no_date_hospital_for_alc variable
data$no_date_hospital_for_alc<-car::recode(data$no_date_hospital_for_alc,"
                                                 1='6';2='5';3='4';
                                                 4='3';5='2';6='1'")

#recoding less_opinion_trtd_person variable
data$less_opinion_trtd_person<-car::recode(data$less_opinion_trtd_person,"
                                                      1='6';2='5';3='4';
                                                      4='3';5='2';6='1'")

#Organize scale datasets

#BNI
stigma_data<-with(data,data.frame(alc_treatment_intelligent,alcoholic_trustworthy,think_less_treated_person,
                                           alcoholic_close_friend,recovered_alcoholic_teacher,
                                           recover_alcoholic_hired,recovered_alc_treat_same,no_date_hospital_for_alc))

#inverting the scale
invertStigma<-function(x){
	car::recode(x,"1=6;2=5;3=4;4=3;5=2;6=1")
	}

data_stigma_inverted<-lapply(stigma_data,invertStigma)
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

# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.

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

# pas_scores_patients<-pas_scores_scaled[data_stigma_inverted$group=="Patients"]

#Non-Abstainners = everyone who responded 1 or 2 in the consumption question
data_nonabst<-subset(data.frame(age=data$age,
							gender=data$female,
							alcohol=data$alcohol_6h_ainjury,
							positive_breath=data$pos_etoh,
							mvc=data$ibc_10,
							audit_data_cleaned,
							pas_scores_scaled,
							drinc_data_score,
							drinc_data_score_cat,
							drink_drive=data$drink_drive),							
							data$consumption!=0)

data_nonabst$groups<-data_nonabst$drink_drive

data_abst<-subset(data.frame(age=data$age,
							gender=data$female,
							alcohol=data$alcohol_6h_ainjury,
							positive_breath=data$pos_etoh,
							mvc=data$ibc_10,
							audit_data_cleaned,
							pas_scores_scaled,
							drinc_data_score,
							drinc_data_score_cat,
							drink_drive=data$drink_drive),							
							data$consumption==0)

data_abst$groups<-c(3)


data_full1<-rbind(data_nonabst,data_abst)

# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
imp2 <- mice(data_full1, seed = 2222, m=5)


# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
data_full<-mice::complete(imp2,4)

######################################################################
#DATA MANAGEMENT
######################################################################
# table(data$audit_score_cat)
# prop.table(table(data$audit_score_cat))

# summary(data$alcohol_6h_ainjury)
# table(data$alcohol_6h_ainjury)
# prop.table(table(data$alcohol_6h_ainjury))

# summary(data$pos_etoh)
# table(data$pos_etoh)
# prop.table(table(data$pos_etoh))

# test<-with(data,data.frame(audit_score_cat,alcohol_6h_ainjury,pos_etoh))

# data %>%
#   mutate(elegible = if_else(audit_score_cat == "Yes" |  #conditions
#   							alcohol_6h_ainjury == 1 | 
#   							pos_etoh == 1, 
#   							"Yes", # if then
#   							"No")) -> data# else 

# summary(data$elegible)
# table(data$elegible)
# prop.table(table(data$elegible))

######################################################################
#TABLE 1
######################################################################
str(data_full)

with(data_full,table(groups))
with(data_full,prop.table(table(groups)))

#Age
with(data_full,
	describe(age))
with(data_full,
	by(age,groups,describe))
with(data_full,
	kruskal.test(audit_score~groups))

#gender
with(data_full,table(gender))
with(data_full,prop.table(table(gender)))
with(data_full,table(gender,groups))
with(data_full,prop.table(table(gender,groups),2))

#alcohol
with(data_full,table(alcohol))
with(data_full,prop.table(table(alcohol)))
with(data_full,table(alcohol,groups))
with(data_full,prop.table(table(alcohol,groups),2))

#positive_breath
with(data_full,table(positive_breath))
with(data_full,prop.table(table(positive_breath)))
with(data_full,table(positive_breath,groups))
with(data_full,prop.table(table(positive_breath,groups),2))

#mvc
with(data_full,table(mvc))
with(data_full,prop.table(table(mvc)))
with(data_full,table(mvc,groups))
with(data_full,prop.table(table(mvc,groups),2))

#Comparing AUdit Scores
with(data_full,
	summary(audit_score))
with(data_full,
	by(audit_score,groups,summary))
with(,
	kruskal.test(audit_score~groups))
# data_nonabst$audit_cat<-car::recode(data_nonabst$audit_score,"
# 	0.00:4.00='a';
# 	4.01:7.50='b';
# 	7.51:15.00='c';
# 	15.01:36.00='d'")

#Comparing AUdit Scores - D1
with(data_full,
	summary(audit_score_D1))
with(data_full,
	by(audit_score_D1,groups,summary))
with(data_full,
	kruskal.test(audit_score_D1~groups))
# data_nonabst$audit_cat<-car::recode(data_nonabst$audit_score,"
# 	0.00:4.00='a';
# 	4.01:7.50='b';
# 	7.51:15.00='c';
# 	15.01:36.00='d'")

#Comparing AUdit Scores - D1
with(data_full,
	summary(audit_score_D2))
with(data_full,
	by(audit_score_D2,groups,summary))
with(data_full,
	kruskal.test(audit_score_D2~groups))
# data_nonabst$audit_cat<-car::recode(data_nonabst$audit_score,"
# 	0.00:4.00='a';
# 	4.01:7.50='b';
# 	7.51:15.00='c';
# 	15.01:36.00='d'")

######################################################################
#TABLE 2
######################################################################

data_nonabst$gender[2]<-1

data_nonabst$gender<-car::recode(data_nonabst$gender,"0=2")

data_nonabst$mvc<-car::recode(data_nonabst$mvc,"2=1")


model<-glm(as.factor(drink_drive) ~ age +
									gender +
									mvc +
									audit_score +
									pas_scores_scaled,
			data=data_nonabst,
			family=binomial())
summary(model)

exp(coef(model)) # exponentiated coefficients
exp(confint(model)) # 95% CI for exponentiated coefficients
1
#Stigma and care seeking

# care<-data.frame(care_seeking=data$talked_dr,pas_score)
# care_seeking<-car::recode(care$care_seeking,"8=NA")
# # x_clean<-subset(x,audit_data[2]==2)
# with(care,
# 	by(devaluation,care_seeking,summary))

# wilcox.test(pas_score~care_seeking)

# # x_clean<-subset(x,audit_data[2]==2)
# with(care,
# 	by(discrimination,care_seeking,summary))

# with(care,
# 	wilcox.test(discrimination~care_seeking))

######################################################################
#FIGURE 2
######################################################################

#
p <- ggplot(subset(data_full,data_full$groups!=3),
			aes(as.factor(groups),pas_score))
p <- p + geom_boxplot(fill="grey")
p <- p + xlab("Drink and drive") + ylab("PDD Score")
p <- p + theme_bw()
p <- p + scale_x_discrete(breaks=c("0", "2"),
                      labels=c("No", "Yes"))
p

