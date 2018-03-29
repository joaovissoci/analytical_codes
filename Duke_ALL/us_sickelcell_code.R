#################################################################
#IHD GEO EPI - BRAZIL
#################################################################
#
#
#
#
#
#################################################################
#SETTING ENVIRONMENT
#################################################################
# #All packages must be installes with install.packages() function
# lapply(c("ggplot2", "psych", "RCurl", "irr", "nortest", 
# 	"moments","GPArotation","nFactors","boot","psy", "car",
# 	"vcd", "gridExtra","mi","VIM","epicalc","gdata","sqldf",
# 	"reshape2","mclust","foreign","survival","memisc","lme4",
# 	"lmerTest","dplyr","meta","xlsx"), 
# library, character.only=T)

# install.packages("readxl") # CRAN version
# install.packages("writexl") # CRAN version


library("readxl")
library("writexl")
library("ggplot2")

#################################################################
#IMPORTING DATA
#################################################################
#LOADING DATA FROM A .CSV FILE
# data<-read.csv("/Users/Joao/Box Sync/Home Folder jnv4/Data/DUEM/sicke_cell/us_dukescikecell_data.csv")

data<-read_excel("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/DUEM/sicke_cell/us_EDOUsicklecell_data.xlsx")

#information between " " are the path to the directory in your computer where the data is stored

#Import data from Dropbox, in .csv format
#Instructions here http://goo.gl/Ofa7gQ
#data1 <- repmis::source_DropboxData(".csv",
#                                  "",
#                                  sep = ",",
#                                  header = TRUE)

# write.csv(data,"/Users/joaovissoci/Desktop/sickle_cell_data.csv")
#############################################################################
#DATA MANAGEMENT
#############################################################################

#changing variable naemes
colnames(data)<-c("recordID",
				  "event_name",
				  "deleteme1",
				  "deleteme2",
				  "age",
				  "gender",
				  "race",
				  "ethnicity",
				  "number_prior_ED_visits_year",
				  "number_prior_hosp_year",
				  "number_prior_day_hosp_year",
				  "pulomrary_hypertension",
				  "ox_sat",
				  "avascular_necrosis",
				  "stroke",
				  "creatinine",
				  "proteinuria",
				  "leg_ulcers",
				  "sicke_cell_severity_score",
				  "arrival_time",
				  "triage_time",
				  "pain_score_initial",
				  "pain_score_date",
				  "room_disposition_date",
				  "time_room_disposition",
				  "ceu_disposition_date",
				  "los_ed",
				  "pain_score_at_CEU_placement",
				  "pain_score_change",
				  "pain_score_change_date",
				  "ceu_dischrage_date",
				  "disposition",
				  "hospital_discharge_date",
				  "pain_score_at_discharge",
				  "pain_score_at_discharge_time",
				  "pain_score_at_discharge_2",
				  "pain_score_at_discharge_2_time",
				  "order_set",
				  "nsaids_used",
				  "drug_initial_time",
				  "time_first_dose_drug",
				  "pca_time",
				  "time_first_pca",
				  "pca_order_allow_tirations",
				  "number_pca_tirations",
				  "pca_cessation_time",
				  "readmission_7days",
				  "readmission_7days_ED",
				  "readmission_7days_dayhospital",
				  "readmission_7days_CEU",
				  "readmission_7days_hosp",
				  "number_EDvisits_7days",
				  "number_dayhospvisits_7days",
				  "number_CEUvisits_7days",
				  "number_hospvisits_7days",
				  "readmission_30days",
				  "readmission_30days_ED",
				  "readmission_30days_dayhospital",
				  "readmission_30days_CEU",
				  "readmission_30days_hosp",
				  "number_EDvisits_30days",
				  "number_dayhospvisits_30days",
				  "number_CEUvisits_30days",
				  "number_hospvisits_30days",
				  "deleteme5"
				  )

#Adjusting time based variables
data$time_first_dose_drug<-with(data,drug_initial_time-arrival_time)
data$time_first_pca<-with(data,pca_time-arrival_time)
# data$time_ed_tratment<-with(data,disposition-room_disposition_date)
data$los_ed<-with(data,ceu_dischrage_date-arrival_time)
data$los_ceu<-with(data,ceu_dischrage_date-ceu_disposition_date)

#recoding or creating new variables
data$number_pca_tirations<-car::recode(data$number_pca_tirations,"NA=0")

data$number_pca_tirations_cat<-car::recode(data$number_pca_tirations,"NA='none';
																1:3='yes';
																 else='yes'")

data$pca_order_allow_tirations<-car::recode(data$pca_order_allow_tirations,"NA='No'")

data$time_first_dose_drug_cat<-car::recode(data$time_first_dose_drug,"
							0:60='A less than 60 min';
							61:361='B more than 60 min'")


data$number_prior_ED_visits_year_cat<-car::recode(data$number_prior_ED_visits_year,"
	0:5='ALess than 5';
	6:10='BSex to Ten';
	11:60='CEleven to Twenty'")

data$number_prior_hosp_year_cat<-car::recode(data$number_prior_hosp_year,"
	0:5='ALess than 5';
	6:10='BSex to Ten';
	11:60='CEleven to Twenty'")

#creating # of acute care visits
data$number_prior_acutecare_visits_year<-with(data,rowSums(data.frame(number_prior_ED_visits_year,
														   number_prior_day_hosp_year)))

#creating the Sickle cell order set variable

# order_set_recoded<-NULL

# for(i in i:length(as.data.frame(data)[,1])){

# 	if(data$order_set[1]=="No"){ 

# 		order_set_recoded[1]<-"no use of order set"

# 	}else{ 

# 		if(data$pca_order_allow_tirations[1]=="No"){

# 			order_set_recoded[1]<-"use of order set w/o tirations"

# 		}else{

# 			order_set_recoded[1]<-"use of order set w/ tirations"

# 		}
# 	}
# }



#############################################################################
#TABLE 1
#############################################################################

# Outcome
table<-with(data,table(disposition))
table
prop.table(table)

# Gender
# current_unit
table<-with(data,table(gender))
table
prop.table(table)
table<-with(data,table(gender,disposition))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# age
with(data,describe(age))
with(data,describeBy(age,disposition))
# t-test: # independent 2-group, 2 level IV
with(data,t.test(age ~ disposition))

# sicke_cell_severity_score
with(data,describe(sicke_cell_severity_score))
with(data,describeBy(sicke_cell_severity_score,disposition))
# t-test: # independent 2-group, 2 level IV
with(data,t.test(sicke_cell_severity_score ~ disposition))

# prior ED visits
with(data,summary(number_prior_ED_visits_year))
with(data,by(number_prior_ED_visits_year,disposition,summary))
# t-test: # independent 2-group, 2 level IV
with(data,t.test(number_prior_ED_visits_year ~ disposition))


# current_unit
table<-with(data,table(number_prior_ED_visits_year_cat))
table
prop.table(table)
table<-with(data,table(number_prior_ED_visits_year_cat,disposition))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# prior ED visits
with(data,summary(number_prior_hosp_year))
with(data,by(number_prior_hosp_year,disposition,summary))
# t-test: # independent 2-group, 2 level IV
with(data,t.test(number_prior_hosp_year ~ disposition))

table<-with(data,table(number_prior_hosp_year_cat))
table
prop.table(table)
table<-with(data,table(number_prior_hosp_year_cat,disposition))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#############################################################################
#TABLE 2
#############################################################################

#MODEL 1 - Adding every variable
logmodel<-glm(as.factor(disposition) ~ 
						gender
						# age +
						# sicke_cell_severity_score +
						# number_prior_acutecare_visits_year +
						# number_prior_hosp_year +
						# time_first_dose_drug +
						# as.numeric(pain_score_change) +
						# number_pca_tirations +
						# nsaids_used +
						# time_ed_tratment +
						# # los_ed +
						# los_ceu
			,family=binomial, data=data)

summary(logmodel)
#anova(reglogGEU)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

logmodel<-glm(as.factor(disposition) ~ 
						# gender +
						age
						# sicke_cell_severity_score +
						# number_prior_acutecare_visits_year +
						# number_prior_hosp_year +
						# time_first_dose_drug +
						# as.numeric(pain_score_change) +
						# number_pca_tirations +
						# nsaids_used +
						# time_ed_tratment +
						# # los_ed +
						# los_ceu
			,family=binomial, data=data)

summary(logmodel)
#anova(reglogGEU)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95)))

logmodel<-glm(as.factor(disposition) ~ 
						# gender +
						# age +
						sicke_cell_severity_score
						# number_prior_acutecare_visits_year +
						# number_prior_hosp_year +
						# time_first_dose_drug +
						# as.numeric(pain_score_change) +
						# number_pca_tirations +
						# nsaids_used +
						# time_ed_tratment +
						# # los_ed +
						# los_ceu
			,family=binomial, data=data)

summary(logmodel)
#anova(reglogGEU)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95)))

logmodel<-glm(as.factor(disposition) ~ 
						# gender +
						# age +
						# sicke_cell_severity_score +
						number_prior_ED_visits_year
						# number_prior_hosp_year +
						# time_first_dose_drug +
						# as.numeric(pain_score_change) +
						# number_pca_tirations +
						# nsaids_used +
						# time_ed_tratment +
						# # los_ed +
						# los_ceu
			,family=binomial, data=data)

summary(logmodel)
#anova(reglogGEU)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95)))

logmodel<-glm(as.factor(disposition) ~ 
						# gender +
						# age +
						# sicke_cell_severity_score +
						# number_prior_ED_visits_year_cat +
						number_prior_hosp_year
						# time_first_dose_drug +
						# as.numeric(pain_score_change) +
						# number_pca_tirations +
						# nsaids_used +
						# time_ed_tratment +
						# # los_ed +
						# los_ceu
			,family=binomial, data=data)

summary(logmodel)
#anova(reglogGEU)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95)))

logmodel<-glm(as.factor(disposition) ~ 
						# gender +
						# age +
						# sicke_cell_severity_score +
						# number_prior_ED_visits_year_cat +
						# number_prior_hosp_year_cat +
						time_first_dose_drug_cat
						# as.numeric(pain_score_change) +
						# number_pca_tirations +
						# nsaids_used +
						# time_ed_tratment +
						# # los_ed +
						# los_ceu
			,family=binomial, data=data)

summary(logmodel)
#anova(reglogGEU)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95)))

logmodel<-glm(as.factor(disposition) ~ 
						# gender +
						# age +
						# sicke_cell_severity_score +
						# number_prior_ED_visits_year_cat +
						# number_prior_hosp_year_cat +
						# time_first_dose_drug +
						as.numeric(pain_score_change)
						# number_pca_tirations +
						# nsaids_used +
						# time_ed_tratment +
						# # los_ed +
						# los_ceu
			,family=binomial, data=data)

summary(logmodel)
#anova(reglogGEU)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95)))

logmodel<-glm(as.factor(disposition) ~ 
						# gender +
						# age +
						# sicke_cell_severity_score +
						# number_prior_ED_visits_year_cat +
						# number_prior_hosp_year_cat +
						# time_first_dose_drug +
						# as.numeric(pain_score_change) +
						number_pca_tirations
						# nsaids_used +
						# time_ed_tratment +
						# # los_ed +
						# los_ceu
			,family=binomial, data=data)

summary(logmodel)
#anova(reglogGEU)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95)))

logmodel<-glm(as.factor(disposition) ~ 
						# gender +
						# age +
						# sicke_cell_severity_score +
						# number_prior_ED_visits_year_cat +
						# number_prior_hosp_year_cat +
						# time_first_dose_drug +
						# as.numeric(pain_score_change) +
						# number_pca_tirations
						nsaids_used
						# time_ed_tratment +
						# # los_ed +
						# los_ceu
			,family=binomial, data=data)

summary(logmodel)
#anova(reglogGEU)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95)))

logmodel<-glm(as.factor(disposition) ~ 
						# gender +
						# age +
						# sicke_cell_severity_score +
						# number_prior_ED_visits_year_cat +
						# number_prior_hosp_year_cat +
						# time_first_dose_drug +
						# as.numeric(pain_score_change) +
						# number_pca_tirations
						# nsaids_used
						# time_ed_tratment
						los_ed
						# los_ceu
			,family=binomial, data=data)

summary(logmodel)
#anova(reglogGEU)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95)))

logmodel<-glm(as.factor(disposition) ~ 
						# gender +
						# age +
						# sicke_cell_severity_score +
						# number_prior_ED_visits_year_cat +
						# number_prior_hosp_year_cat +
						# time_first_dose_drug +
						# as.numeric(pain_score_change) +
						# number_pca_tirations
						# nsaids_used
						# time_ed_tratment
						# los_ed
						los_ceu
			,family=binomial, data=data)

summary(logmodel)
#anova(reglogGEU)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95)))

logmodel<-glm(as.factor(disposition) ~ 
						# gender +
						# age +
						# sicke_cell_severity_score +
						# number_prior_ED_visits_year_cat +
						# number_prior_hosp_year_cat +
						# time_first_dose_drug +
						# as.numeric(pain_score_change) +
						# number_pca_tirations
						# nsaids_used
						# time_ed_tratment
						time_first_pca
						# los_ceu
			,family=binomial, data=data)

summary(logmodel)
#anova(reglogGEU)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95)))


#traffic control was not added because had cases with 0 observations
# age and gender becaise the missing rate wsa to high

logmodel<-glm(as.factor(disposition) ~ 
						# gender + #significant for los_ed
						# age + #need tokeep
						sicke_cell_severity_score + #need to keep
						number_prior_ED_visits_year + #need to keep
						number_prior_hosp_year + #need to keep
						time_first_dose_drug_cat + #potential influence on LOS
						# as.numeric(pain_score_change) + #also improves the model
						number_pca_tirations + # small improve the model
						as.factor(nsaids_used) + #need to keep
						time_first_pca +
						# time_ed_tratment +
						los_ed
						# los_ceu
			,family=binomial, data=data)

summary(logmodel)
#anova(reglogGEU)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

logmodel<-glm(as.factor(readmission_30days) ~ 
						# gender + #significant for los_ed
						# age + #need tokeep
						sicke_cell_severity_score + #need to keep
						number_prior_ED_visits_year_cat + #need to keep
						number_prior_hosp_year_cat + #need to keep
						time_first_dose_drug_cat + #potential influence on LOS
						# as.numeric(pain_score_change) + #also improves the model
						number_pca_tirations + # small improve the model
						as.factor(nsaids_used) + #need to keep
						time_first_pca +
						# disposition +
						los_ed
						# los_ceu
			,family=binomial, data=data)

summary(logmodel)
#anova(reglogGEU)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

logmodel<-glm(as.factor(readmission_7days) ~ 
						# gender + #significant for los_ed
						# age + #need tokeep
						sicke_cell_severity_score + #need to keep
						number_prior_ED_visits_year_cat + #need to keep
						number_prior_hosp_year_cat + #need to keep
						time_first_dose_drug_cat + #potential influence on LOS
						# as.numeric(pain_score_change) + #also improves the model
						number_pca_tirations + # small improve the model
						as.factor(nsaids_used) + #need to keep
						time_first_pca +
						# disposition +
						los_ed
						# los_ceu
			,family=binomial, data=data)

summary(logmodel)
#anova(reglogGEU)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 



logmodel<-glm(as.factor(readmission_30days) ~ 
						gender +
						age +
						sicke_cell_severity_score +
						# number_prior_ED_visits_year_cat +
						number_prior_hosp_year +
						time_first_dose_drug +
						# as.numeric(pain_score_change) +
						number_pca_tirations +
						nsaids_used +
						time_ed_tratment
						# los_ed +
						# los_ceu
			,family=binomial, data=data)

summary(logmodel)
#anova(reglogGEU)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)



logmodel<-glm(as.factor(readmission_30days) ~ 
						gender +
						age +
						# sicke_cell_severity_score +
						number_prior_acutecare_visits_year +
						number_prior_hosp_year +
						# time_first_dose_drug +
						# as.numeric(pain_score_change) +
						# number_pca_tirations +
						# nsaids_used +
						time_ed_tratment +
						disposition
						# los_ed +
						# los_ceu
			,family=binomial, data=data)

summary(logmodel)
#anova(reglogGEU)
odds<-exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)

logmodel<-glm(as.factor(readmission_7days) ~ 
						gender +
						age +
						# sicke_cell_severity_score +
						number_prior_acutecare_visits_year +
						number_prior_hosp_year +
						# time_first_dose_drug +
						# as.numeric(pain_score_change) +
						# number_pca_tirations +
						# nsaids_used +
						time_ed_tratment +
						disposition
						# los_ed +
						# los_ceu
			,family=binomial, data=data)

summary(logmodel)
#anova(reglogGEU)
odds<-exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)

#############################################################################
#FIGURE 1
#############################################################################

readmission_7days_data<-with(data,c(
						readmission_7days_hosp,
						readmission_7days_dayhospital,
						readmission_7days_CEU,
						readmission_7days_ED
						))

readmission_7days_cat<-c(rep("Hospital",length(data$readmission_7days_hosp)),
						 rep("Day Hospital",length(data$readmission_7days_dayhospital)),
						 rep("CEU",length(data$readmission_7days_CEU)),
						 rep("ED",length(data$readmission_7days_ED)))

readmission_7days_label<-rep(" 7 days",length(readmission_7days_cat))

readmission_30days_data<-with(data,c(
						readmission_30days_hosp,
						readmission_30days_dayhospital,
						readmission_30days_CEU,
						readmission_30days_ED
						))

readmission_30days_cat<-c(rep("Hospital",length(data$readmission_30days_hosp)),
						 rep("Day Hospital",length(data$readmission_30days_dayhospital)),
						 rep("CEU",length(data$readmission_30days_CEU)),
						 rep("ED",length(data$readmission_30days_ED)))

readmission_30days_label<-rep("30 days",length(readmission_30days_cat))

readmission_30days_disposition<-with(data,c(disposition,disposition,disposition,disposition))

readmission_7days_disposition<-with(data,c(disposition,disposition,disposition,disposition))

readmission_data<-c(readmission_7days_data,readmission_30days_data)

readmission_cat<-c(readmission_7days_cat,readmission_30days_cat)

readmission_label<-c(readmission_7days_label,readmission_30days_label)

readmission_disposition<-c(readmission_7days_disposition,readmission_30days_disposition)

readmission_plotdata<-data.frame(readmission_data,readmission_cat,readmission_label,readmission_disposition)

readmission_plotdata_yes<-subset(readmission_plotdata,readmission_plotdata$readmission_data=='Checked')

# g <- ggplot(readmission_plotdata, aes(as.character(readmission_cat)))
# g

ggplot(readmission_plotdata_yes, aes(readmission_cat)) + 
geom_bar(aes(fill=readmission_disposition),position = "dodge",) +
facet_grid(.~readmission_label, scales="free_y")+
labs(x='Visit type', y='# of visits') + 
scale_fill_discrete(name='Initial CEU\ndisposition')


#30 days readmission
#day hospital
table<-with(data,table(disposition,readmission_30days_hosp))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#hospital
table<-with(data,table(disposition,readmission_30days_dayhospital))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#hospital
table<-with(data,table(disposition,readmission_30days_CEU))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#hospital
table<-with(data,table(disposition,readmission_30days_ED))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#7 days readmission
#day hospital
table<-with(data,table(disposition,readmission_7days_hosp))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#hospital
table<-with(data,table(disposition,readmission_7days_dayhospital))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#hospital
table<-with(data,table(disposition,readmission_7days_CEU))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#hospital
table<-with(data,table(disposition,readmission_7days_ED))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package
##############################################################################
#END
##############################################################################