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

data<-read.csv("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/Africa/Tz/BNI/Tz_bnipatients_data.csv")

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
drinc_data_questions<-complete(imp,4)

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

recoding think_less_treated_person variable
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

# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
imp_stigma <- mice(stigma_data, seed = 2222, m=5)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
stigma_data<-complete(imp_stigma,4)

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
                   data = stigma_data,
                   estimator="WLSMV",
                   ordered=colnames(stigma_data)
)

# summary(fit, fit.measures=TRUE)

#Predicted scores

pas_data<-with(stigma_data,data.frame(alc_treatment_intelligent, 
       alcoholic_trustworthy,
       think_less_treated_person,
       alcoholic_close_friend,
       recovered_alcoholic_teacher,
       recover_alcoholic_hired,
       recovered_alc_treat_same,
       no_date_hospital_for_alc))

pas_scores<-lavaan::lavPredict(fit,newdata=pas_data,method="EBM")
pas_scores_scaled<-scales::rescale(as.data.frame(pas_scores)$BNI, 
    to = c(0, 100))


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
							drink_drive=data$drink_drive,
							talked_dr=data$talked_dr),							
							data$consumption!=0)

data_nonabst$outcome<-data_nonabst$drink_drive

data_abst<-subset(data.frame(age=data$age,
							gender=data$female,
							alcohol=data$alcohol_6h_ainjury,
							positive_breath=data$pos_etoh,
							mvc=data$ibc_10,
							audit_data_cleaned,
							pas_scores_scaled,
							drinc_data_score,
							drinc_data_score_cat,
							drink_drive=data$drink_drive,
							talked_dr=data$talked_dr),							
							data$consumption==0)

data_abst$outcome<-c(3)


data_full<-rbind(data_nonabst,data_abst)

# # argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
# imp2 <- mice(data_full1, seed = 2222, m=5)


# # reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
# data_full<-complete(imp2,4)

data_full$outcome<-car::recode(data_full$outcome,"2='yes';
												 0='no';
												 3='abst'")

data_full$outcome<-as.factor(data_full$outcome)

data_full$mvc<-car::recode(data_full$mvc,"2='yes';
												 0='no';
												 1='yes'")

data_full$mvc<-as.factor(data_full$mvc)
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

with(data_full,table(outcome))
with(data_full,prop.table(table(outcome)))

with(data,table(consumption))
with(data,prop.table(table(consumption)))

#Age
with(data_full,
	describe(age))
with(data_full,
	by(age,outcome,describe))
with(data_full,
	kruskal.test(age~outcome))

#gender
with(data_full,table(gender))
with(data_full,prop.table(table(gender)))
table<-with(data_full,table(gender,outcome))
table
with(data_full,prop.table(table(gender,outcome),2))
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#alcohol
with(data_full,table(alcohol))
with(data_full,prop.table(table(alcohol)))
with(data_full,table(alcohol,outcome))
with(data_full,prop.table(table(alcohol,outcome),2))
table<-with(data_full,table(alcohol,outcome))
table
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#positive_breath
with(data_full,table(positive_breath))
with(data_full,prop.table(table(positive_breath)))
with(data_full,table(positive_breath,outcome))
with(data_full,prop.table(table(positive_breath,outcome),2))
table<-with(data_full,table(positive_breath,outcome))
table
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#mvc
with(data_full,table(mvc))
with(data_full,prop.table(table(mvc)))
with(data_full,table(mvc,outcome))
with(data_full,prop.table(table(mvc,outcome),2))
table<-with(data_full,table(positive_breath,outcome))
table
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#Comparing AUdit Scores
with(data_full,
	summary(audit_score))
with(data_full,
	by(audit_score,outcome,summary))
with(data_full,
	kruskal.test(audit_score~outcome))
# data_nonabst$audit_cat<-car::recode(data_nonabst$audit_score,"
# 	0.00:4.00='a';
# 	4.01:7.50='b';
# 	7.51:15.00='c';
# 	15.01:36.00='d'")

# #Comparing AUdit Scores - D1
# with(data_full,
# 	summary(audit_score_D1))
# with(data_full,
# 	by(audit_score_D1,outcome,summary))
# with(data_full,
# 	kruskal.test(audit_score_D1~outcome))
# # data_nonabst$audit_cat<-car::recode(data_nonabst$audit_score,"
# # 	0.00:4.00='a';
# # 	4.01:7.50='b';
# # 	7.51:15.00='c';
# # 	15.01:36.00='d'")

# #Comparing AUdit Scores - D1
# with(data_full,
# 	summary(audit_score_D2))
# with(data_full,
# 	by(audit_score_D2,outcome,summary))
# with(data_full,
# 	kruskal.test(audit_score_D2~outcome))
# # data_nonabst$audit_cat<-car::recode(data_nonabst$audit_score,"
# # 	0.00:4.00='a';
# # 	4.01:7.50='b';
# # 	7.51:15.00='c';
# # 	15.01:36.00='d'")

######################################################################
#FIGURE 1
######################################################################

#
p <- ggplot(subset(data_full,data_full$outcome!=3),
			aes(as.factor(outcome),audit_score))
p <- p + geom_boxplot(fill="grey")
p <- p + xlab("Drink and drive") + ylab("AUDIT Score")
p <- p + theme_bw()
p <- p + scale_x_discrete(breaks=c("0", "2"),
                      labels=c("No", "Yes"))
p

######################################################################
#TABLE 2
######################################################################

#PDD summary
with(data_full,
	summary(pas_scores_scaled))
with(data_full,
	by(pas_scores_scaled,outcome,summary))
with(data_full,
	kruskal.test(pas_scores_scaled~outcome))

#mvc
with(data_full,table(pas_score_cat))
with(data_full,prop.table(table(pas_score_cat)))
x<-with(data_full,table(pas_score_cat,outcome))
with(data_full,prop.table(table(pas_score_cat,outcome),2))
assocstats(x)
fisher.test(x)

summary(data_full$discrimination)
with(data_full,
	by(discrimination,outcome,summary))
with(data_full,
	kruskal.test(discrimination~outcome))

summary(data_full$devaluation)
with(data_full,
	by(devaluation,outcome,summary))
with(data_full,
	kruskal.test(devaluation~outcome))

with(data_full,cor(data.frame(pas_score,audit_score)))

######################################################################
#Figure 2
######################################################################

#Building graph for each item
figure2_data1<-with(data,data.frame(alcoholic_close_friend,
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
imp <- mice(figure2_data1, seed = 2222, m=5)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
figure2_data<-complete(imp,4)

graph<-melt(figure2_data)
count_data_fig2<-plyr::count(graph, c("variable", "value"))
count_data_fig2<-na.omit(count_data_fig2)
count_data_fig2$value<-car::recode(count_data_fig2$value,"
	1='Strongly disagree';
	2='Disagree';
	3='Somewhat disagree';
	4='Somewhat agree';
	5='Agree';
	6='Strongly agree'")
count_data_fig2$feq_2<-(count_data_fig2$freq*100)/35
count_data_fig2$feq_2<-round(count_data_fig2$feq_2,digits=1)

#Adding value of zero to likert options not chosen
variable_add<-c("think_less_treated_person","alcoholic_trustworthy",
	"recover_alcoholic_hired")
value_add<-c("Somewhat disagree","Disagree","Somewhat agree")
freq_2_add<-c(0.0,0.0,0.0)
freq_add<-c(0,0,0)
add<-data.frame(variable=variable_add,
	value=value_add,
	freq=freq_add,
	feq_2=freq_2_add)

plot_data<-rbind(count_data_fig2,add)

#Adding Mean and Standard Deviations
meanttoadd_labels<-c("alcoholic_close_friend",
							"recovered_alcoholic_teacher",
							"recover_alcoholic_chldrn",
							"recover_alcoholic_hired",
							"non_alcoholic_hired",
							"recovered_alc_treat_same",
							"not_date_hospital_for_alc",
							"alc_treatment_intelligent",
							"alcoholic_trustworthy",
							"alc_treatment_failure",
							"think_less_treated_person",
							"less_opinion_trtd_person")

meanttoadd_variable<-rep("Mean (SD)",12)

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
				    descriptives_temp[[12]]$mean),2)

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
				    descriptives_temp[[12]]$sd),2)

meanttoadd_values<-paste0(meanttoadd_means, " ","\n", "(", meanttoadd_sd,")")


meantoadd_data<-data.frame(variable=meanttoadd_labels,
						   value=meanttoadd_variable,
						   freq=c(0),
						   feq_2=c(0))

plot_data<-rbind(count_data_fig2,add,meantoadd_data)

plot_data$text<-c(rep(NA,72),meanttoadd_values)
plot_data$tile<-c(rep(NA,72),rep("white",12))

# plot_data$color<-NULL
# plot_data$color[plot_data$feq_2 >= 0 & plot_data$feq_2 < 5.883]="lightcyan1"
# plot_data$color[plot_data$feq_2 >= 5.883 & plot_data$feq_2 < 11.76]="lightcyan2"
# plot_data$color[plot_data$feq_2 >= 11.76 & plot_data$feq_2 < 26.47]="lightcyan3"
# plot_data$color[plot_data$feq_2 >= 26.47]="lightcyan4"

#find colors numbers: diverge_hcl(7, c = 100, l = c(50, 90), power = 1)
#from: https://cran.r-project.org/web/packages/colorspace/vignettes/hcl-colors.pdf

plot_data$flip<-NULL
plot_data$flip[plot_data$variable == "recovered_alcoholic_teacher"]="notflip"
plot_data$flip[plot_data$variable == "alcoholic_close_friend"]="notflip"
plot_data$flip[plot_data$variable == "alc_treatment_intelligent"]="notflip"
plot_data$flip[plot_data$variable == "alcoholic_trustworthy"]="notflip"
plot_data$flip[plot_data$variable == "recover_alcoholic_hired"]="notflip"
plot_data$flip[plot_data$variable == "recovered_alc_treat_same"]="notflip"

plot_data$flip[plot_data$variable == "alc_treatment_failure"]="flip"
plot_data$flip[plot_data$variable == "recover_alcoholic_chldrn"]="flip"
plot_data$flip[plot_data$variable == "non_alcoholic_hired"]="flip"
plot_data$flip[plot_data$variable == "not_date_hospital_for_alc"]="flip"
plot_data$flip[plot_data$variable == "think_less_treated_person"]="flip"
plot_data$flip[plot_data$variable == "less_opinion_trtd_person"]="flip"


plot_data$color<-NULL
plot_data$color[plot_data$flip == "notflip" & plot_data$value == "Strongly agree"]="#4A6FE3"
plot_data$color[plot_data$flip == "notflip" & plot_data$value == "Agree"]="#8595E1"
plot_data$color[plot_data$flip == "notflip" & plot_data$value == "Somewhat agree"]="#B5BBE3"

plot_data$color[plot_data$flip == "notflip" & plot_data$value == "Somewhat disagree"]="#E6AFB9"
plot_data$color[plot_data$flip == "notflip" & plot_data$value == "Disagree"]="#E07B91"
plot_data$color[plot_data$flip == "notflip" & plot_data$value == "Strongly disagree"]="#D33F6A"

plot_data$color[plot_data$flip == "flip" & plot_data$value == "Strongly disagree"]="#4A6FE3"
plot_data$color[plot_data$flip == "flip" & plot_data$value == "Disagree"]="#8595E1"
plot_data$color[plot_data$flip == "flip" & plot_data$value == "Somewhat disagree"]="#B5BBE3"

plot_data$color[plot_data$flip == "flip" & plot_data$value == "Somewhat agree"]="#E6AFB9"
plot_data$color[plot_data$flip == "flip" & plot_data$value == "Agree"]="#E07B91"
plot_data$color[plot_data$flip == "flip" & plot_data$value == "Strongly agree"]="#D33F6A"

plot_data$color[plot_data$value == "Mean (SD)"]="white"

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
  			 color=plot_data$color, 
  			 show.legend=FALSE) +
  geom_text(aes(label = feq_2), 
  			color="white") +
  scale_size(range = c(0,25)) +
  theme_bw() +
  xlab(label="") + 
  ylab(label="Stigma Scale") + 
  scale_x_discrete(limits = c("Strongly disagree",
  							  "Disagree",
  							  "Somewhat disagree",
  							  "Somewhat agree",
  							  "Agree",
  							  "Strongly agree",
  							  "Mean (SD)"),
  				   labels = c("Strongly \ndisagree",
  							  "Disagree",
  							  "Somewhat \ndisagree",
  							  "Somewhat \nagree",
  							  "Agree",
  							  "Strongly \nagree",
  							  "Mean (SD)")) + 
  scale_y_discrete(limits = rev(c("alcoholic_close_friend",
  								  "alc_treatment_intelligent",
  								  "alcoholic_trustworthy",
  								  "recovered_alcoholic_teacher",
  								  "alc_treatment_failure",
  								  "recover_alcoholic_chldrn",
  								  "think_less_treated_person",
  								  "recover_alcoholic_hired",
  								  "non_alcoholic_hired",
  								  "recovered_alc_treat_same",
  								  "not_date_hospital_for_alc",
  								  "less_opinion_trtd_person")),
  				   labels = rev(c(
"1. Most people would willingly accept a former alcoholic \nas a close friend.",
"2. Most people believe that a person who has had alcohol \ntreatment is just as intelligent as the average person.",
"3. Most people believe that a former alcoholic is just as \ntrustworthy as the average person.",
"4. Most people would accept a fully recovered former alcoholic \nas a teacher of young children in a public school.",
"5. Most people feel that entering alcohol treatment is a \nsign of personal failure.",
"6. Most people would not hire a former alcoholic to take care \nof their children, even if he or she had been \nsober for some time.",
"7. Most people think less of a person who has been in alcohol \ntreatment.",
"8. Most employers will hire a former alcoholic if he or \nshe is qualified for the job.",
"9. Most employers will pass over the application of a former \nalcoholic in favor of another applicant.",
"10. Most people in my community would treat a former alcoholic \njust as they would treat anyone else.",
"11. Most young women would be reluctant to date a man \nwho has been hospitalized for alcoholism.",
"12. Once they know a person was in alcohol treatment, \nmost people will take his or her opinion less seriously."
							))) +
  # geom_tile(fill=plot_data$tile) +
  geom_text(aes(label=text))

 ggsave("figure4.eps", #change .eps to .pdf for different format
		figure4, #plot is the name of the fig, but the function assumes the last plot if argument is NULL
		path="/Users/joaovissoci/Desktop", #path to save the plot
		width = 8, 
		height = 8, 
		device=cairo_ps) #cairo_ps is a d
# dev.off()















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

######################################################################
#FIGURE 2
######################################################################

#
p <- ggplot(subset(data_full,data_full$outcome!=3),
			aes(as.factor(outcome),pas_score))
p <- p + geom_boxplot(fill="grey")
p <- p + xlab("Drink and drive") + ylab("PDD Score")
p <- p + theme_bw()
p <- p + scale_x_discrete(breaks=c("0", "2"),
                      labels=c("No", "Yes"))
p

