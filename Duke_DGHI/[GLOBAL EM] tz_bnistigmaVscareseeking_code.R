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
		 # "mvnormtest",
		 "polycor",
		 "mice",
		 "semPlot"), 
library, character.only=T)

######################################################################
#IMPORTING DATA
######################################################################
#LOADING DATA FROM A .CSV FILE

data<-read.csv("/Users/jnv4/Box Sync/Home Folder jnv4/Data/Global EM/Africa/Tz/k award/tz_bnisurveypatients_data.csv")

######################################################################
#DATA MANAGEMENT
######################################################################

data_nonabst<-subset(data,data$consumption!=0)

# names(data)

#####socioeconomic variables
#age

#female

#alcohol positive

#daily drinking

data_nonabst$daily_drink<-car::recode(
	data_nonabst$daily_drink,"
	998=NA;
	999=NA")


#### ALcohol use consequences

#drinking_interferes

data_nonabst$drinking_interferes<-car::recode(
	data_nonabst$drinking_interferes,"
	1='yes';
	5='no';
	else=NA")

#drinking_arguments

data_nonabst$drinking_arguments<-car::recode(
	data_nonabst$drinking_arguments,"
	1='yes';
	5='no';
	else=NA")

#could_get_hurt

data_nonabst$could_get_hurt<-car::recode(
	data_nonabst$could_get_hurt,"
	1='yes';
	5='no';
	else=NA")

#police_bc_drink

data_nonabst$police_bc_drink<-car::recode(
	data_nonabst$police_bc_drink,"
	1='yes';
	5='no';
	else=NA")

#### AUDIT Score
# data<-subset(data1,data1$audit_complete==2)

audit_data<-with(data_nonabst,data.frame(
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

# audit_data_cleaned<-audit_data[-c(1:10)]
# audit_data_cleaned<-cbind(audit_data_cleaned,
# 	audit_data_NAto0)

#### Care seeking

#talekd_dr

data_nonabst$talked_dr<-car::recode(data_nonabst$talked_dr,"
	1='yes';
	5='no';
	else=NA")

#helpful_treatment

#recent_trtmnt

#hospital_alc

#Selfhelp_group

# #Drinc
drinc_data<-with(data_nonabst,data.frame(
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


#STIGMA
score_data<-with(data_nonabst, data.frame(alcoholic_close_friend,
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
score_data$alcoholic_close_friend<-car::recode(data_nonabst$alcoholic_close_friend,
	"1=6;2=5;3=4;4=3;5=2;6=1")
score_data$alc_treatment_intelligent<-car::recode(data_nonabst$alc_treatment_intelligent,
	"1=6;2=5;3=4;4=3;5=2;6=1")
score_data$alcoholic_trustworthy<-car::recode(data_nonabst$alcoholic_trustworthy,
	"1=6;2=5;3=4;4=3;5=2;6=1")
score_data$recovered_alcoholic_teacher<-car::recode(data_nonabst$recovered_alcoholic_teacher,
	"1=6;2=5;3=4;4=3;5=2;6=1")
score_data$recover_alcoholic_hired<-car::recode(data_nonabst$recover_alcoholic_hired,
	"1=6;2=5;3=4;4=3;5=2;6=1")
score_data$recovered_alc_treat_same<-car::recode(data_nonabst$recovered_alc_treat_same,
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
figure3_data_PAS<-mice::complete(imp,4)

pas_score<-rowSums(figure3_data_PAS)/ncol(figure3_data_PAS)
# summary(pas_score)
# describe(pas_score)
# discrimination<-na.omit(discrimination)
# rescale <- function(x)(x-min(x))/(max(x) - min(x)) * 100
# discrimination_scaled<-lapply(pas_score,rescale)
# pas_score_scaled<-rescale(pas_score)
# summary(x)
# sd(x)
# pas_score_cat<-car::recode(pas_score,"0:3='low';else='high'")

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
figure3_data_PDis<-mice::complete(imp,4)

discrimination<-rowSums(figure3_data_PDis)/ncol(figure3_data_PDis)
# discrimination<-na.omit(discrimination)
# rescale <- function(x)(x-min(x))/(max(x) - min(x)) * 100
# discrimination_scaled<-lapply(discrimination,rescale)
# discrimination_scaled<-rescale(discrimination)
# summary(discrimination)
# describe(discrimination)
# discrimination_cat<-car::recode(discrimination,"0:3='low';else='high'")


#Calculate Perceived Devaluation score
figure3_data_PDev<-with(score_data,data.frame(alc_treatment_intelligent,
							alcoholic_trustworthy,
							alc_treatment_failure,
							think_less_treated_person,
							less_opinion_trtd_person))

# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
imp <- mice(figure3_data_PDev, seed = 2222, m=5)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
figure3_data_PDev<-mice::complete(imp,4)

devaluation<-rowSums(figure3_data_PDev)/ncol(figure3_data_PDev)
# devaluation<-na.omit(devaluation)
# rescale <- function(x)(x-min(x))/(max(x) - min(x)) * 100
# devaluation_scaled<-lapply(devaluation,rescale)
# devaluation_scaled<-rescale(devaluation)
# summary(devaluation)
# describe(devaluation)
# devaluation_cat<-car::recode(devaluation,"0:3='low';else='high'")

data_full<-data.frame(age=data_nonabst$age,
					  female=data_nonabst$female,
					  pos_etoh=data_nonabst$pos_etoh,
					  daily_drink=data_nonabst$daily_drink,
					  # number_drinks_day=data_nonabst$number_drinks_day,
					  drinking_interferes=data_nonabst$drinking_interferes,
					  drinking_arguments=data_nonabst$drinking_arguments,
					  could_get_hurt=data_nonabst$could_get_hurt,
					  police_bc_drink=data_nonabst$police_bc_drink,
					  pas_score,
					  devaluation,
					  discrimination,
					  talked_dr=data_nonabst$talked_dr,
					  # helpful_treatment=data_nonabst$helpful_treatment,
					  # recent_trtmnt=data_nonabst$recent_trtmnt,
					  # hospital_alc=data_nonabst$hospital_alc,
					  # selfhelp_group=data_nonabst$selfhelp_group,
					  audit_total=audit_data$audit_score,
					  audit_alcoholuse=audit_data$audit_score_D1,
					  audit_alcoholdependence=audit_data$audit_score_D2,
					  audit_alcoholrisk=audit_data$audit_score_D3,
					  audit_cat=audit_data$audit_score_cat,
					  drinc_data_score)

# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
imp <- mice(data_full, seed = 2222, m=5)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
data_full<-mice::complete(imp,4)

######################################################################
#TABLE 1
######################################################################

with(data_full,table(talked_dr))


#Age
with(data_full,
	describe(age))
with(data_full,
	by(age,talked_dr,describe))
with(data_full,
	t.test(age~talked_dr))
logmodel<-glm(talked_dr ~ age
			,family=binomial, data=data_full)
summary(logmodel)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#gender
with(data_full,table(female))
with(data_full,prop.table(table(female)))
table<-with(data_full,table(female,talked_dr))
table
with(data_full,prop.table(table(female,talked_dr),2))
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package
logmodel<-glm(talked_dr ~ female
			,family=binomial, data=data_full)
summary(logmodel)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#alcohol
with(data_full,table(pos_etoh))
with(data_full,prop.table(table(pos_etoh)))
table<-with(data_full,table(pos_etoh,talked_dr))
table
with(data_full,prop.table(table(pos_etoh,talked_dr),2))
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package
logmodel<-glm(talked_dr ~ pos_etoh
			,family=binomial, data=data_full)
summary(logmodel)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#at-risk alcohol Users
with(data_full,table(audit_cat))
with(data_full,prop.table(table(audit_cat)))
table<-with(data_full,table(audit_cat,talked_dr))
table
with(data_full,prop.table(table(audit_cat,talked_dr),2))
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package
logmodel<-glm(talked_dr ~ audit_cat
			,family=binomial, data=data_full)
summary(logmodel)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#daily_drink
with(data_full,
	summary(daily_drink))
with(data_full,
	by(daily_drink,talked_dr,summary))
with(data_full,
	wilcox.test(daily_drink~talked_dr))
logmodel<-glm(talked_dr ~ daily_drink
			,family=binomial, data=data_full)
summary(logmodel)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#drinking_interferes
with(data_full,table(drinking_interferes))
with(data_full,prop.table(table(drinking_interferes)))
table<-with(data_full,table(drinking_interferes,talked_dr))
table
with(data_full,prop.table(table(drinking_interferes,talked_dr),2))
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package
logmodel<-glm(talked_dr ~ drinking_interferes
			,family=binomial, data=data_full)
summary(logmodel)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#drinking_arguments
with(data_full,table(drinking_arguments))
with(data_full,prop.table(table(drinking_arguments)))
table<-with(data_full,table(drinking_arguments,talked_dr))
table
with(data_full,prop.table(table(drinking_arguments,talked_dr),2))
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package
logmodel<-glm(talked_dr ~ drinking_arguments
			,family=binomial, data=data_full)
summary(logmodel)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#could_get_hurt
with(data_full,table(could_get_hurt))
with(data_full,prop.table(table(could_get_hurt)))
table<-with(data_full,table(could_get_hurt,talked_dr))
table
with(data_full,prop.table(table(could_get_hurt,talked_dr),2))
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package
logmodel<-glm(talked_dr ~ could_get_hurt
			,family=binomial, data=data_full)
summary(logmodel)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#police_bc_drink
with(data_full,table(police_bc_drink))
with(data_full,prop.table(table(police_bc_drink)))
table<-with(data_full,table(police_bc_drink,talked_dr))
table
with(data_full,prop.table(table(police_bc_drink,talked_dr),2))
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package
logmodel<-glm(talked_dr ~ police_bc_drink
			,family=binomial, data=data_full)
summary(logmodel)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#pas_score
with(data_full,
	summary(pas_score))
with(data_full,
	by(pas_score,talked_dr,summary))
with(data_full,
	wilcox.test(pas_score~talked_dr))
logmodel<-glm(talked_dr ~ pas_score
			,family=binomial, data=data_full)
summary(logmodel)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#devaluation
with(data_full,
	summary(devaluation))
with(data_full,
	by(devaluation,talked_dr,summary))
with(data_full,
	wilcox.test(devaluation~talked_dr))
logmodel<-glm(talked_dr ~ devaluation
			,family=binomial, data=data_full)
summary(logmodel)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#discrimination
with(data_full,
	summary(discrimination))
with(data_full,
	by(discrimination,talked_dr,summary))
with(data_full,
	wilcox.test(discrimination~talked_dr))
logmodel<-glm(talked_dr ~ discrimination
			,family=binomial, data=data_full)
summary(logmodel)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#audit_total
with(data_full,
	summary(audit_total))
with(data_full,
	by(audit_total,talked_dr,summary))
with(data_full,
	wilcox.test(audit_total~talked_dr))
logmodel<-glm(talked_dr ~ audit_total
			,family=binomial, data=data_full)
summary(logmodel)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#audit_alcoholuse
with(data_full,
	summary(audit_alcoholuse))
with(data_full,
	by(audit_alcoholuse,talked_dr,summary))
with(data_full,
	wilcox.test(audit_alcoholuse~talked_dr))
logmodel<-glm(talked_dr ~ age
			,family=binomial, data=data_full)
summary(logmodel)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#audit_alcoholdependence
with(data_full,
	summary(audit_alcoholdependence))
with(data_full,
	by(audit_alcoholdependence,talked_dr,summary))
with(data_full,
	wilcox.test(audit_alcoholdependence~talked_dr))
logmodel<-glm(talked_dr ~ age
			,family=binomial, data=data_full)
summary(logmodel)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#audit_alcoholrisk
with(data_full,
	summary(audit_alcoholrisk))
with(data_full,
	by(audit_alcoholrisk,talked_dr,summary))
with(data_full,
	wilcox.test(audit_alcoholrisk~talked_dr))
logmodel<-glm(talked_dr ~ age
			,family=binomial, data=data_full)
summary(logmodel)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#drinc_data_score
with(data_full,
	summary(drinc_data_score))
with(data_full,
	by(drinc_data_score,talked_dr,summary))
with(data_full,
	wilcox.test(drinc_data_score~talked_dr))
logmodel<-glm(talked_dr ~ drinc_data_score
			,family=binomial, data=data_full)
summary(logmodel)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 


logmodel<-glm(talked_dr ~ pos_etoh +
						  daily_drink +
						  drinking_interferes +
						  drinking_arguments +
						  could_get_hurt +
						  audit_total +						
						  drinc_data_score
			,family=binomial, data=data_full)
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
					  pas_score,
					  # devaluation,
					  # discrimination,
					  # talked_dr=as.numeric(as.factor(talked_dr)),
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
             talked_dr ~ c*audit_total
           # mediator
             drinc_data_score ~ a*audit_total
             talked_dr ~ b*drinc_data_score
           # indirect effect (a*b)
             ab := a*b
           # total effect
             total := c + (a*b)
         '

fit <- lavaan::sem(model, data = network_data,
					estimator="WLSMV",
					ordered=colnames(network_data$talked_dr))
summary(fit,standardized=TRUE,
					fit.measures=TRUE,
					rsq=TRUE)
fitMeasures(fit, fit.measures = "all", baseline.model = NULL)
Est <- parameterEstimates(fit, ci = TRUE, standardized = TRUE)
subset(Est, op == "~")
subset(Est, op == ":=")

# Mediation analysis - STIGMA

model <- ' # direct effect
             talked_dr ~ dA*audit_total
             talked_dr ~ dB*drinc_data_score
             talked_dr ~ dC*pas_score
             # talked_dr ~ dC*devaluation
             # talked_dr ~ dD*discrimination
           
           # mediator
             drinc_data_score ~ indA*audit_total
             pas_score ~ indB*drinc_data_score
             # audit_total ~ indC*pas_score             
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
					ordered=colnames(network_data$talked_dr))
summary(fit,standardized=TRUE,
					fit.measures=TRUE,
					rsq=TRUE)
fitMeasures(fit, fit.measures = "all", baseline.model = NULL)
Est <- parameterEstimates(fit, ci = TRUE, standardized = TRUE)
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
