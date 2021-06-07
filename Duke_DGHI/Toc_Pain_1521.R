##########################
# ToC_Survey of Analgesic Practices
##########################

#################################################################
#SETTING ENVIRONMENT
#################################################################
#LOADING PACKAGES
lapply(c("car",
         "psych",
         "mice",
         "gmodels",
         "dplyr",
         "Hmisc",
         "graphics",
         "lattice",
         "tidyverse",
         "ggplot2"), 
       library, character.only=T)

#loading data
tocp <-read.csv("/Users/mollyminnig/Downloads/ToCTraumaRegistry_DATA_2020-01-13_fixed.csv")

#############################################################################
#DATA MANAGEMENT
#############################################################################
#RECODING VARIABLES
tocp$female_recoded<-car::recode(tocp$female,
                                 "0='Male'; 1='Female'")
tocp$marital_recoded<-car::recode(tocp$marital,
                                  "0='Single'; 1:2='Living with partner'; 3:4='Other'")
tocp$tribe_recoded<-car::recode(tocp$tribe,
                                "1='Saamba';0='Chagga'; 2='Maasai'; 3='Pare'; 4='Other';5='Other';6='Other';7='Other';8='Other';89='Other'")
tocp$employ_recoded<-car::recode(tocp$employ,
                                 "0='Student'; 1='Unemployed'; 2='Professional'; 3='Skilled Employment'; 4='Self-Employed'; 5='Farmer'; 6='Other'")
tocp$moi_recoded<-car::recode(tocp$moi,
                              "0='Road Traffic Injury'; 1='Assault'; 2='Other'; 3='Fall'; 4='Other'; 89='Other'; 99='Other'")
tocp$insurance_recoded<-car::recode(tocp$insurance,
                                    "0='None'; 1='Cash personal payment'; 2='National Health insurance'; 3:89='Other'")
#CREATING GCS VARIABLE
#separating GOS into positive and negative categories
tocp$gos_recoded <-car::recode(tocp$gos,
                               "0:4='negative outcome'; 5:8='positive outcome'")
#creating new variable for gcs score (ultimately goes into KTS score)
tocp$gcs_tot<-rowSums(data.frame(tocp$gcs_eye+tocp$gcs_verbal
                                 +tocp$gcs_motor))
tocp$gcs_tot_recoded<-car::recode(tocp$gcs_tot,
                                  "1:12='Poor GCS';13:15='Good GCS'")
#building the dataset to be imputed
#separating data set into variables of interest to be imputed... tocp_imp serves as
#a dummy dataset for these variables that will be imputed
tocp_imp<-with(tocp,data.frame(age, female_recoded, education, marital_recoded, employ_recoded,
                               tribe_recoded, moi_recoded, gos, sf4, pain_arred,
                               pain_leaved, surg_1, avpu_arred, rr_arred, hr_arred, sbp_arred,
                               pox_arred, dispo_loc,gcs_tot_recoded, etoh, phq1, phq2))

#MISSING VARIABLES
#finding number of missing values per variable of interest before imputation
na_count<-sapply(tocp_imp, function(x) sum(length(which(is.na(x)))))
na_count
na_count<-data.frame(na_count)
na_count
na_count$percentage<-na_count$na_count/dim(tocp_imp)[1]*100
na_count
barplot(na_count$percentage,las=2,names.arg = rownames(na_count),
        col="lightgreen", main="Percent Missing per Variable",
        ylab="Percent Missing",mar=c(5,2,2,2),cex.names =0.7, ylim = c(0,100))

#IMPUTATION
imp<-mice(tocp_imp,seed=2222,m=5)
tocp_imp<-mice::complete(imp,sample(1:5,1))
#adding imputed vars back into tocp dataset
tocp$age_imp<-tocp_imp$age
tocp$female_imp<-tocp_imp$female_recoded
tocp$education_imp<-tocp_imp$education
tocp$marital_imp<-tocp_imp$marital_recoded
tocp$employ_imp<-tocp_imp$employ_recoded
tocp$tribe_imp<-tocp_imp$tribe_recoded
tocp$insurance_imp<-tocp_imp$insurance_recoded
tocp$moi_imp<-tocp_imp$moi_recoded
tocp$gos_imp<-tocp_imp$gos
tocp$sf4_imp<-tocp_imp$sf4
tocp$pain_arred_imp<-tocp_imp$pain_arred
tocp$pain_leaved_imp<-tocp_imp$pain_leaved
tocp$surg_1_imp<-tocp_imp$surg_1
tocp$avpu_arred_imp<-tocp$avpu_arred
tocp$rr_arred_imp<-tocp$rr_arred
tocp$hr_arred_imp<-tocp$hr_arred
tocp$sbp_arred_imp<-tocp$sbp_arred
tocp$pox_arred_imp<-tocp$pox_arred
tocp$dispo_loc_imp<-tocp$dispo_loc
tocp$gcs_tot_imp<-tocp$gcs_tot_recoded
tocp$etoh_imp<-tocp$etoh
tocp$phq1_imp<-tocp$phq1
tocp$phq2_imp<-tocp$phq2

#CREATING KTS VARIABLE
KTS_data<-with(tocp, data.frame(age_imp, avpu_arred_imp, rr_arred_imp,
                                hr_arred_imp,sbp_arred_imp, pox_arred_imp, surg_1_imp,
                                dispo_loc_imp, gcs_tot_imp))
#recode variables into KTS scoring
KTS_data$age_imp_cat<-as.numeric(car::recode(KTS_data$age_imp,
                                             "6:55='2'; 0:5='1'; 56:110='1'"))
table(KTS_data$age_imp_cat)
KTS_data$sbp_arred_imp_cat<-as.numeric(car::recode(KTS_data$sbp_arred_imp,
                                                   "90:300='4'; 50:89='3'; 1:49='2'"))
table(KTS_data$sbp_arred_imp_cat)
KTS_data$rr_arred_imp_cat<-as.numeric(car::recode(KTS_data$rr_arred_imp,
                                                  "10:29='3'; 30:200='2'; 0:9='1'"))
table(KTS_data$rr_arred_imp_cat)
KTS_data$avpu_arred_imp_cat<-as.numeric(car::recode(KTS_data$avpu_arred_imp,
                                                    "0='4'; 1='3'; 2='2'; 3='1'"))
table(KTS_data$avpu_arred_imp_cat)
#creating variable for 'serious injury' aspect of KTS
#recoding variables so that 1=bad, 0=good
KTS_data$dispo_loc_imp<-as.numeric(car::recode(KTS_data$dispo_loc_imp,
                                               "0='1'; 1='1'; 2:89='0'"))
KTS_data$gcs_tot_imp<-as.numeric(car::recode(KTS_data$gcs_tot_imp,
                                             "0:12='1'; 13:15='0'"))
KTS_data$pox_arred_imp<-as.numeric(car::recode(KTS_data$pox_arred_imp,
                                               "0:91='1'; 92:100='0'"))
KTS_data$sbp_arred_imp<-as.numeric(car::recode(KTS_data$sbp_arred_imp,
                                               "0:99='1'; 100:300='0'"))
KTS_data$hr_arred_imp<-as.numeric(car::recode(KTS_data$hr_arred_imp,
                                              "100:300='1'; 0:99='0'"))
KTS_data$rr_arred_imp<-as.numeric(car::recode(KTS_data$rr_arred_imp,
                                              "26:80='1'; 0:25='0'"))
table(KTS_data$rr_arred_imp)
#combining variables into 'serious injury,' if any variable is present then output is 1 (true) for serious injury, if no variable is present then output is 0 (false)
#lower combined score = worse injury
KTS_data$serious_injury<-(KTS_data$surg_1_imp==1) | (KTS_data$dispo_loc_imp==1) | (KTS_data$gcs_tot_imp==1) | (KTS_data$pox_arred_imp==1) | (KTS_data$sbp_arred_imp==1) | (KTS_data$hr_arred_imp==1) | (KTS_data$rr_arred_imp==1)
table(KTS_data$serious_injury)
#converting from logical t/f to numeric 1/0
KTS_data$serious_injury<-1*KTS_data$serious_injury
table(KTS_data$serious_injury)
#combining all variables (age, sbp, rr, avpu, serious injury)
KTS_data<-mutate(KTS_data, kts_score=(KTS_data$age_imp_cat+KTS_data$sbp_arred_imp_cat+KTS_data$rr_arred_imp_cat+KTS_data$avpu_arred_imp_cat+KTS_data$serious_injury))
table(KTS_data$kts_score)
#recoding KTS score into mild, moderate-severe (binary categorization)
KTS_data$kts_score_cat<-as.factor(car::recode(KTS_data$kts_score,
                                              "5:8='Severe KTS'; 9:16='Mild KTS'"))
#adding KTS back into main toc dataset
tocp$kts_score<-KTS_data$kts_score_cat
tocp$kts_score_cat<-KTS_data$kts_score_cat

#CREATING PHQ-2 VARIABLE
tocp<-within(tocp,phq<-tocp$phq1_imp+tocp$phq2_imp)

#CREATING DELTA PAIN VARIABLE
tocp<-within(tocp,delta<-tocp$pain_leaved_imp-tocp$pain_arred_imp)

#CREATING PERCENT CHANGE IN PAIN VARIABLE
tocp<-within(tocp,perchange<-((tocp$pain_leaved_imp-(tocp$pain_arred_imp))/(tocp$pain_arred_imp)*100))
#creating subset dataset with pain_arred=0
df_zero <- subset(tocp, pain_arred_imp==0)
#forcing NA or Inf in perchange to be 0
tocp$perchange[tocp$perchange==NaN | tocp$perchange==Inf]<-0


#propmiss <- function(toc) lapply(toc,function(x) data.frame(nmiss=sum(is.na(x)), n=length(x), propmiss=sum(is.na(x))/length(x)))

#############################################################################
#TABLE 1. COMPARING PAIN @ ED ARRIVAL VS. @ ED DISCHARGE
#THIS TABLE WAS OMITTED FROM MANUSCRIPT#
#############################################################################
summary(tocp$pain_arred_imp)
sd(tocp$pain_arred_imp)
summary(tocp$pain_leaved_imp)
sd(tocp$pain_leaved_imp)
t.test(tocp$pain_arred_imp,tocp$pain_leaved_imp, paired = TRUE, var.equal= FALSE, alternative = "two.sided")

#############################################################################
#TABLE 2. SOCIODEMOGRAPHICS STRATIFIED BY SEVERE PAIN @ ED ARRIVAL
#############################################################################
#creating new recoded pain_arrred variable, <70=mild & 70+=severe
#tocp$pain_arred_cat<-as.factor(car::recode(tocp$pain_arred_imp,
                                             #"0:69='Mild Pain arred';70:100='Severe Pain arred'")) 
#tocp$pain_leaved_cat<-as.factor(car::recode(tocp$pain_leaved_imp,
                                           #"0:69='Mild Pain leaved';70:100='Severe Pain leaved'")) 

#with(tocp,table(pain_arred_cat))
#with(tocp,prop.table(table(pain_arred_cat)))*100

#GENDER
with(tocp,table(female_imp))
with(tocp,prop.table(table(female_imp)))*100
with(tocp,describeBy(pain_arred_imp, female_imp))
with(tocp,describeBy(pain_leaved_imp, female_imp))
with(tocp,describeBy(delta,female_imp))
with(tocp,describeBy(perchange,female_imp))
with(tocp,prop.table(table(female_imp,perchange,2))
chisq.table(table)

linmodel<-lm(perchange~female_imp,data=tocp)
summary(linmodel)
confint(linmodel)

#MARITAL STATUS
with(tocp,table(marital_imp))
with(tocp,prop.table(table(marital_imp)))*100
with(tocp,describeBy(pain_arred_imp, marital_imp))
with(tocp,describeBy(pain_leaved_imp, marital_imp))
with(tocp,describeBy(delta,marital_imp))
with(tocp,describeBy(perchange,marital_imp))

linmodel<-lm(perchange~marital_imp,data=tocp)
summary(linmodel)
confint(linmodel)

#TRIBE
with(tocp,table(tribe_imp))
with(tocp,prop.table(table(tribe_imp)))*100
with(tocp,describeBy(pain_arred_imp, tribe_imp))
with(tocp,describeBy(pain_leaved_imp, tribe_imp))
with(tocp,describeBy(delta,tribe_imp))
with(tocp,describeBy(perchange,tribe_imp))

linmodel<-lm(perchange~tribe_imp,data=tocp)
summary(linmodel)
confint(linmodel)

#EMPLOYMENT
with(tocp,table(employ_imp))
with(tocp,prop.table(table(employ_imp)))*100
with(tocp,describeBy(pain_arred_imp, employ_imp))
with(tocp,describeBy(pain_leaved_imp, employ_imp))
with(tocp,describeBy(delta,employ_imp))
with(tocp,describeBy(perchange,employ_imp))

tocp$employ_imp<-relevel(tocp$employ_imp, ref="Professional")
linmodel<-lm(perchange~employ_imp,data=tocp)
summary(linmodel)
confint(linmodel)

#MOI
with(tocp,table(moi_imp))
with(tocp,prop.table(table(moi_imp)))*100
with(tocp,describeBy(pain_arred_imp, moi_imp))
with(tocp,describeBy(pain_leaved_imp, moi_imp))
with(tocp,describeBy(delta,moi_imp))
with(tocp,describeBy(perchange,moi_imp))

linmodel<-lm(perchange~moi_imp,data=tocp)
summary(linmodel)
confint(linmodel)

#KTS
with(tocp,table(kts_score_cat))

#Age
with(tocp,
     describe(age_imp))
with(tocp,
     by(age_imp,pain_arred_imp,describe))
with(tocp,
     sd(age_imp))
with(tocp,
     by(age_imp,pain_arred_cat,sd))
with(tocp, describeBy(pain_arred_imp, age_imp))
with(tocp, describeBy(pain_leaved_imp, age_imp))
with(tocp, describeBy(delta, age_imp))
#recoding age as categorical
tocp$age_imp_cat<-car::recode(tocp$age_imp,
                                  "18:29='18-29';30:49='30-49';50:64='50-64';65:100='65+'")
with(tocp,table(age_imp_cat))
with(tocp,prop.table(table(age_imp_cat)))*100
with(tocp,describeBy(pain_arred_imp, age_imp_cat))
with(tocp,describeBy(pain_leaved_imp, age_imp_cat))
with(tocp,describeBy(delta,age_imp_cat))
with(tocp,describeBy(perchange,age_imp_cat))

linmodel<-lm(perchange~age_imp_cat, data=tocp)
summary(linmodel)
confint(linmodel)

# with(tocp,
#   t.test(age_imp~pain_arred_cat))
#logmodel<-glm(pain_arred_cat ~ age
              #,family=binomial(link=logit), data=tocp)
#summary(logmodel)
#exp(cbind(Oddsratio=coef(logmodel),confint(logmodel,level=0.95))) 

#Years of Education
with(tocp,
     describe(education_imp))
with(tocp,
     by(education_imp,pain_arred_cat,describe))
with(tocp,
     sd(education_imp))
with(tocp,
     by(education_imp,pain_arred_cat,sd))
# with(tocp,
#   t.test(education_imp~pain_arred_cat))
#logmodel<-glm(pain_arred_cat ~ education_imp
              #,family=binomial(link=logit), data=tocp)
#summary(logmodel)
#exp(cbind(Oddsratio=coef(logmodel),confint(logmodel,level=0.95))) 
#Recoding edu as categorical
tocp$edu_imp_cat<-car::recode(tocp$age_imp,
                              "0:7='No Education';8:100='At least primary'")
with(tocp,table(edu_imp_cat))
with(tocp,prop.table(table(edu_imp_cat)))*100
with(tocp,describeBy(pain_arred_imp, edu_imp_cat))
with(tocp,describeBy(pain_leaved_imp, edu_imp_cat))
with(tocp,describeBy(delta,edu_imp_cat))

#KTS
with(tocp,table(kts_score_cat))
with(tocp,prop.table(table(kts_score_cat)))*100
with(tocp,describeBy(pain_arred_imp, kts_score_cat))

linmodel<-lm(perchange~kts_score_cat,data=tocp)
summary(linmodel)
confint(linmodel)

#GCS
with(tocp,table(gcs_tot_recoded))
with(tocp,prop.table(table(gcs_tot_recoded)))*100
with(tocp,describeBy(pain_arred_imp, gcs_tot_recoded))
with(tocp,describeBy(perchange, gcs_tot_recoded))

linmodel<-lm(perchange~gcs_tot_recoded,data=tocp)
summary(linmodel)
confint(linmodel)

#ALCOHOL STATUS
with(tocp,table(etoh_imp))
with(tocp,prop.table(table(etoh_imp)))*100
with(tocp,describeBy(pain_arred_imp, etoh_imp))
with(tocp,describeBy(perchange, etoh_imp))

linmodel<-lm(perchange~etoh_imp,data=tocp)
summary(linmodel)
confint(linmodel)


#PHQ
with(tocp,
     describe(phq2_imp))

#############################################################################
#REGRESSION MODELS
#############################################################################
#DELTA PAIN = OUTCOME
###################################
linmodel1<-lm(delta ~
                age_imp+
                kts_score_cat+
                gcs_tot_recoded+
                etoh_imp,
              data=tocp)
summary(linmodel1)

linmodel2<-lm(delta ~
                female_imp+
                kts_score_cat+
                gcs_tot_recoded+
                etoh_imp,
              data=tocp)
summary(linmodel2)

linmodel3<-lm(delta ~
                education_imp+
                kts_score_cat+
                gcs_tot_recoded+
                etoh_imp,
              data=tocp)
summary(linmodel3)

linmodel4<-lm(delta ~
                marital_imp+
                kts_score_cat+
                gcs_tot_recoded+
                etoh_imp,
              data=tocp)
summary(linmodel4)

linmodel5<-lm(delta ~
                tribe_imp+
                kts_score_cat+
                gcs_tot_recoded+
                etoh_imp,
              data=tocp)
summary(linmodel5)

linmodel6<-lm(delta ~
                moi_imp+
                kts_score_cat+
                gcs_tot_recoded+
                etoh_imp,
              data=tocp)
summary(linmodel6)

linmodel6<-lm(delta ~
                age_imp+
                female_imp+moi_imp+
                kts_score_cat+
                gcs_tot_recoded+
                etoh_imp,
              data=tocp)
summary(linmodel6)

tocp$employ_imp<-relevel(tocp$employ_imp, ref="Professional")
tocp$kts_score_cat<-relevel(tocp$kts_score_cat, ref="Mild KTS")
linmodel7<-lm(delta ~
                age_imp+
                female_imp+
                education_imp+
                employ_imp+
                marital_imp+
                tribe_imp+
                moi_imp+
                kts_score_cat+
                gcs_tot_recoded+
                etoh_imp+
                phq,
              data=tocp)
summary(linmodel7)
confint(linmodel7)

###################################
#PAIN LEAVING ED = OUTCOME
###################################

tocp$employ_imp<-relevel(tocp$employ_imp, ref="Professional")
tocp$kts_score_cat<-relevel(tocp$kts_score_cat, ref="Mild KTS")
linmodel8<-lm(pain_leaved_imp ~
                pain_arred_imp+
                age_imp+
                female_imp+
                education_imp+
                employ_imp+
                marital_imp+
                tribe_imp+
                moi_imp+
                kts_score_cat+
                gcs_tot_recoded+
                etoh_imp+
                phq,
              data=tocp)
summary(linmodel8)
confint(linmodel8)

###################################
#PERCENT CHANGE IN PAIN = OUTCOME
#TABLE 3 IN MANUSCRIPT
###################################
#MULTIVARIATE REGRESSION MODEL
tocp$employ_imp<-relevel(tocp$employ_imp, ref="Professional")
tocp$kts_score_cat<-relevel(tocp$kts_score_cat, ref="Mild KTS")
linmodel9<-lm(perchange ~
                pain_arred_imp+
                age_imp+
                female_imp+
                education_imp+
                employ_imp+
                marital_imp+
                tribe_imp+
                moi_imp+
                kts_score_cat+
                gcs_tot_recoded+
                etoh_imp+
                phq,
              data=tocp)
summary(linmodel9)
confint(linmodel9)

#UNIVARIATE REGRESSION  MODELS
linmodel_age<-lm(perchange ~
                   age_imp,
                 data=tocp)
summary(linmodel_age)
confint(linmodel_age)

linmodel_fem<-lm(perchange~
                   female_imp
                 data=tocp)
summary(linmodel_fem)
confint(linmodel_fem)

linmodel_edu<-lm(perchange~
                   educaition_imp
                 data=tocp)
summary(linmodel_edu)
confint(linmodel_edu)

linmodel_emp<-lm(perchange~
                   employ_imp
                 data=tocp)
summary(linmodel_emp)
confint(linmodel_emp)

linmodel_mar<-lm(perchange~
                   marital_imp
                 data=tocp)
summary(linmodel_mar)
confint(linmodel_mar)

linmodel_tribe<-lm(perchange~
                   tribe_imp
                 data=tocp)
summary(linmodel_tribe)
confint(linmodel_tribe)

linmodel_moi<-lm(perchange~
                   moi_imp
                 data=tocp)
summary(linmodel_moi)
confint(linmodel_moi)

linmodel_kts<-lm(perchange~
                   kts_score_cat
                 data=tocp)
summary(linmodel_kts)
confint(linmodel_kts)

linmodel_gcs<-lm(perchange~
                   gcs_tot_recoded
                 data=tocp)
summary(linmodel_gcs)
confint(linmodel_gcs)

linmodel_etoh<-lm(perchange~
                   etoh_imp
                 data=tocp)
summary(linmodel_etoh)
confint(linmodel_etoh)

linmodel_phq<-lm(perchange~
                   phq
                 data=tocp)
summary(linmodel_phq)
confint(linmodel_phq)

#model1<-glm(pain_arred_cat ~ 
                   # age_imp+
                   # female_imp+
                   # education_imp+
                   # marital_imp+
                   # tribe_imp+
                   # moi_imp+
                   # kts_score_cat+
                   # gcs_tot_recoded+
                   # etoh_imp, 
                 # family = binomial(link = logit),
                 # data=tocp)
#summary(model1)
#round(exp(cbind(Odds=coef(model1),confint(model1,level=0.95))), 3)
#vif(model1)

###################################
#PAIN AT ED ADMIT = OUTCOME
###################################

model1b<-glm(pain_arred_cat ~ 
              age_imp+
              female_imp+
              education_imp+
              marital_imp+
              tribe_imp+
              moi_imp+
              kts_score_cat+
              gcs_tot_recoded,
            family = binomial(link = logit),
            data=tocp)
summary(model1b)
round(exp(cbind(Odds=coef(model1b),confint(model1b,level=0.95))), 3)
vif(model1b)


model2<-glm(pain_arred_cat ~ 
              kts_score_cat+
              age_imp+
              female_imp+
              education_imp+
              marital_imp+
              tribe_imp+
              moi_imp, 
            family = binomial(link = logit),
            data=tocp)
summary(model2)
round(exp(cbind(Odds=coef(model2),confint(model2,level=0.95))), 3)
vif(model2)

###################################
#PAIN AT ED DISCHARGE = OUTCOME
###################################

model2b<-glm(pain_leaved_cat ~ 
              kts_score_cat+
              age_imp+
              female_imp+
              education_imp+
              marital_imp+
              tribe_imp+
              moi_imp, 
            family = binomial(link = logit),
            data=tocp)
summary(model2b)
round(exp(cbind(Odds=coef(model2b),confint(model2b,level=0.95))), 3)
vif(model2b)

###################################
#PAIN AT ED ADMIT = OUTCOME
###################################

model3<-glm(pain_arred_cat ~ 
              etoh_imp+
              kts_score_cat+
              age_imp+
              female_imp+
              education_imp+
              marital_imp+
              tribe_imp+
              moi_imp, 
            family = binomial(link = logit),
            data=tocp)
summary(model3)
round(exp(cbind(Odds=coef(model3),confint(model3,level=0.95))), 3)
vif(model3)

###################################
#PAIN AT ED DISCHARGE = OUTCOME
###################################

model3b<-glm(pain_leaved_cat ~ 
              etoh_imp+
              kts_score_cat+
              age_imp+
              female_imp+
              education_imp+
              marital_imp+
              tribe_imp+
              moi_imp, 
            family = binomial(link = logit),
            data=tocp)
summary(model3b)
round(exp(cbind(Odds=coef(model3b),confint(model3b,level=0.95))), 3)
vif(model3b)

###################################
#PAIN AT ED ADMIT = OUTCOME
###################################

model4<-glm(pain_arred_cat ~ 
              gcs_tot_imp+
              kts_score_cat+
              etoh_imp+
              age_imp+
              female_imp+
              education_imp+
              marital_imp+
              tribe_imp+
              moi_imp, 
            family = binomial(link = logit),
            data=tocp)
summary(model4)
round(exp(cbind(Odds=coef(model4),confint(model4,level=0.95))), 3)
vif(model4)

###################################
#PAIN AT ED DISCHARGE = OUTCOME
###################################

tocp$pain_leaved_cat<-relevel(tocp$pain_leaved_cat,ref="Mild Pain leaved")
model5<-glm(pain_leaved_cat ~ 
              gcs_tot_imp+
              kts_score_cat+
              etoh_imp+
              age_imp+
              female_imp+
              education_imp+
              marital_imp+
              tribe_imp+
              moi_imp, 
            family = binomial(link = logit),
            data=tocp)
summary(model5)
round(exp(cbind(Odds=coef(model5),confint(model5,level=0.95))), 3)
vif(model5)




#############################################################################
#VISUALIZATIONS
#############################################################################
library(ggplot2)
#spread of pain levels @ arrival vs discharge
# First distribution
hist(tocp$pain_arred_imp, breaks=30, xlim=c(0,100), col=rgb(1,0,0,0.5), xlab="Pain Level", 
     ylab="Frequency", main="Distribution of Pain Levels at ED Arrival vs Discharge" )

# Second with add=T to plot on top
hist(tocp$pain_leaved_imp, breaks=30, xlim=c(0,100),col=rgb(0,0,1,0.5), add=T)

# Add legend
legend("topright",legend=c("Pain Levels at ED Arrival","Pain Levels at ED Discharge"), col=c(rgb(1,0,0,0.5), 
                                                      rgb(0,0,1,0.5)), pt.cex=2, pch=15 )


hist(tocp$pain_arred_imp,labels=TRUE)
#labels aren't showing up ahhhh
hist(tocp$pain_arred_imp, breaks = 10,
                      xlim = c(0,100),
                      ylim =  c(0,148),
                      main ="Frequency of Pain Level Upon ED Arrival", 
                      xlab ="Pain Level", 
                      ylab = "Frequency")
qplot(tocp$pain_arred_imp, geom="histogram")
ggplot(data=tocp,aes(tocp$pain_arred))+geom_histogram() 
plot(tocp$pain_arred, freq=equidist, density())

#Pain level upon leaving the ED
as.numeric(levels(tocp$pain_leaved_imp))[tocp$pain_leaved_imp]
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

table(tocp$pain_leaved_imp)
summary(tocp$pain_leaved_imp)
mean(tocp$pain_leaved_imp)

#histogram comparing pain_arred and pain_leaved
library(plotly)
graph=plot_ly(x=)

#separating GOS into positive and negative categories
tocp$gos_imp <-car::recode(tocp$gos_imp,
    "0:4='negative outcome'; 5:8='positive outcome'")
sevpain <- subset(tocp, tocp$pain_arred_imp=='severe')
CrossTable(tocp$pain_arred_imp,tocp$gos_imp)
  
posgos <- subset(toc, toc$gos_imp=='positive outcome')

#histogram comparing pain_arred vs pain _leaved next to each other
par(mfrow=c(1,2), mar=c(4,4,1,0))
hist(tocp$pain_arred_imp, breaks=30 , xlim=c(0,100) , ylim=c(0, 350), col=rgb(1,0,0,0.5) , xlab="Pain Levels at ED Arrival" , ylab="Frequency" , main="" )
hist(tocp$pain_leaved_imp, breaks=30 , xlim=c(0,100) , ylim=c(0,350), col=rgb(0,0,1,0.5) , xlab="Pain Levels at ED Discharge" , ylab="Frequency" , main="")

##SCATTERPLOT 
data=data.frame(
  x=seq(1:100) + 0.1*seq(1:100)*sample(c(1:10), 100,replace = T),
  y=seq(1:100) + 0.2*seq(1:100)*sample(c(1:10), 100,replace = T))
plot(x=tocp$delta,y=tocp$pain_arred_imp, 
     xlab="Change in Pain", 
     ylab="Pain Level at ED Admittance",
     pch=16, 
     cex=1, 
     col="#69b3a2")

ggplot(tocp, aes(x=tocp$delta, y=tocp$pain_arred_imp), xlab="Change in Pain", ylab="Pain Level at ED Admittance") + 
  geom_point()

# the iris dataset is provided by R natively

#SCATTERPLOT w colored categories
# Create a color palette
library(paletteer)
library(ggthemes)
colors <- paletteer_c("scico::berlin", n = 7)
# Scatterplot with categoric color scale
plot(
  x = tocp$delta, 
  y = tocp$pain_arred_imp,
  bg = colors[ unclass(tocp$employ_imp) ],
  cex = 1,
  pch=21
)
colors <- paletteer_d("nord::frost", n = 4)
# Scatterplot with categoric color scale
plot(
  x = tocp$delta, 
  y = tocp$pain_arred_imp,
  bg = colors[ unclass(tocp$moi_imp) ],
  cex = 1,
  pch=21
)



