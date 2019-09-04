##########################
# ToC_Substance Use_REDO_ w/ updated KTS Code
# Molly
##########################
#
#
#
#
#
#################################################################
#SETTING ENVIRONMENT
#################################################################
#All packages must be installes with install.packages() function
lapply(c("car",
         "mice",
         "gmodels",
         "dplyr",
         "Hmisc",
         "graphics",
         "lattice",
         "tidyverse",
         "ggplot2"), 
library, character.only=T)
#################################################################
#IMPORTING DATA
#################################################################
#LOADING DATA FROM A .CSV FILE
toc_data <- read.csv("/Users/Joao/Box/Home Folder jnv4/Data/Global EM/Africa/Tz/tocr21/tz_traumaregistry_dataAug2019.csv")
#information between " " are the path to the directory in your computer where the data is stored

#############################################################################
#DATA MANAGEMENT
#############################################################################

#recoding 'no' in lifetime use to 'no' in current use
toc_data$tob_c[toc_data$tob_life==0 | toc_data$tob_c==0]<-0
toc_data$mj_c[toc_data$mj_life==0 | toc_data$mj_c==0]<-0
toc_data$khat_c[toc_data$khat_life==0 | toc_data$khat_c==0]<-0
toc_data$cocaine_c[toc_data$cocaine_c==0 | toc_data$cocaine_life==0]<-0
toc_data$heroin_c[toc_data$heroin_c==0 | toc_data$heroin_life==0]<-0
toc_data$tambu_c[toc_data$tambu_c==0 | toc_data$tambu_life==0]<-0
toc_data$kuberi_c[toc_data$kuberi_c==0 | toc_data$kuberi_life==0]<-0

# recoding 'no' in alchohl use in the past year (audit question #1)
# to 'no' in the other linked audit questions
toc_data$audit2[toc_data$audit1==0 | toc_data$audit2==0]<-0
toc_data$audit3[toc_data$audit1==0 | toc_data$audit3==0]<-0
toc_data$audit4[toc_data$audit1==0 | toc_data$audit4==0]<-0
toc_data$audit5[toc_data$audit1==0 | toc_data$audit5==0]<-0
toc_data$audit6[toc_data$audit1==0 | toc_data$audit6==0]<-0
toc_data$audit7[toc_data$audit1==0 | toc_data$audit7==0]<-0
toc_data$audit8[toc_data$audit1==0 | toc_data$audit8==0]<-0
toc_data$audit9[toc_data$audit1==0 | toc_data$audit9==0]<-0
toc_data$audit10[toc_data$audit1==0 | toc_data$audit10==0]<-0

#recoding variables
toc_data$female_recoded<-car::recode(toc_data$female,
                            "0='Male'; 1='Female'") 
toc_data$marital_recoded<-car::recode(toc_data$marital,
                             "0='Single'; 1='Married'; 2:4='Other'")
toc_data$tribe_recoded<-car::recode(toc_data$tribe,
                           "1='Saamba';0='Chagga'; 2='Maasai'; 3='Pare'; 4='Other';5='Other';6='Other';7='Other';8='Other';89='Other'")
toc_data$employ_recoded<-car::recode(toc_data$employ,
                            "0='Student'; 1='Unemployed'; 2='Professional'; 3='Skilled Employment'; 4='Self-Employed'; 5='Farmer'; 6='Other'")
toc_data$moi_recoded<-car::recode(toc_data$moi,
                         "0='Road Traffic Injury'; 1='Assault'; 2='Other'; 3='Fall'; 4='Other'; 89='Other'; 99='Other'")
toc_data$insurance_recoded<-car::recode(toc_data$insurance,
                               "0='None'; 1='Cash personal payment'; 2='National Health insurance'; 3:89='Other'")

#separating GOS into positive and negative categories
toc_data$gos_recoded <-car::recode(toc_data$gos,
                          "0:4='negative outcome'; 5:8='positive outcome'")

#creating new variable for gcs score (ultimately goes into KTS score)
toc_data$gcs_tot<-rowSums(data.frame(toc_data$gcs_eye+toc_data$gcs_verbal
                                    +toc_data$gcs_motor))

toc_data$gcs_tot_recoded<-car::recode(toc_data$gcs_tot,
                             "1:12='Poor GCS';13:15='Good GCS'")

#creating new variable for any other drugs from lifetime drugs
#excluding heorin because there was no positive response
toc_data$otherdrugs[toc_data$cocaine_life==0 & toc_data$tambu_life==0 & toc_data$kuberi_life==0]<-0
toc_data$otherdrugs[toc_data$cocaine_life==1 | toc_data$tambu_life==1 | toc_data$kuberi_life==1]<-1

#creating new variable for other drugs + mj +khat
toc_data$otherdrugs2_c[toc_data$cocaine_c==0 & toc_data$heroin_c==0 & toc_data$tambu_c==0 & toc_data$kuberi_c==0 & toc_data$mj_c==0 & toc_data$khat_c==0]<-0
toc_data$otherdrugs2_c[toc_data$cocaine_c==1 | toc_data$heroin_c==1 | toc_data$tambu_c==1 | toc_data$kuberi_c==1 | toc_data$mj_c==1 | toc_data$khat_c==1]<-1

#creating variable for ANY drug use,
#create otherdrugs2_c variable first
toc_data$anydrug_life[toc_data$tob_life==0 & toc_data$mj_life==0 & toc_data$khat_life==0 & toc_data$otherdrugs==0]<-0
toc_data$anydrug_life[toc_data$tob_life==1 | toc_data$mj_life==1 | toc_data$khat_life==1 | toc_data$otherdrugs==1]<-1
toc_data$anydrug_c[toc_data$tob_c==0 & toc_data$mj_c==0 & toc_data$khat_c==0 & toc_data$otherdrugs2_c==0]<-0
toc_data$anydrug_c[toc_data$tob_c==1 | toc_data$mj_c==1 | toc_data$khat_c==1 | toc_data$otherdrugs2_c==1]<-1

#creating variable for ANY alcohol use
toc_data$anyalcohol<-car::recode(toc_data$audit1,
                            "0='no';1:4='yes'")

#building the dataset to be imputed
#separating data set into variables of interest to be imputed... toc_data_imp serves as
#a dummy dataset for these variables that will be imputed
toc_imp<-with(toc_data,data.frame(age, female_recoded, education, marital_recoded, employ_recoded, 
                             tribe_recoded, audit1, audit2, audit3, audit4, audit5, 
                             audit6, audit7, audit8, audit9, audit10, tob_life, 
                             tob_c, mj_life, mj_c, cocaine_life, cocaine_c, 
                             heroin_life, heroin_c, khat_life, khat_c, tambu_life, 
                             tambu_c, kuberi_life, kuberi_c, moi_recoded, gos_recoded,
                             insurance, live, avpu_arred, rr_arred, hr_arred, 
                             sbp_arred, anyalcohol, pox_arred,surg_1, dispo_loc, gcs_tot_recoded))

#finding number of missing values for each var
na_count<-sapply(toc_imp, function(x) sum(length(which(is.na(x)))))
na_count
na_count<-data.frame(na_count)
na_count
na_count$percentage<-na_count$na_count/dim(toc_imp)[1]*100
na_count
barplot(na_count$percentage,las=2,names.arg = rownames(na_count), 
        col="lightgreen", main="Number of Missing Values",
        ylab="Percent Missing",mar=c(5,2,2,2))

#imputation
imp<-mice(toc_imp,seed=2222,m=5)
toc_imp<-mice::complete(imp,sample(1:5,1))

#adding imputed vars back into main toc dataset
toc_data$age_imp<-toc_imp$age
toc_data$female_imp<-toc_imp$female_recoded
toc_data$education_imp<-toc_imp$education
toc_data$marital_imp<-toc_imp$marital_recoded
toc_data$employ_imp<-toc_imp$employ_recoded
toc_data$tribe_imp<-toc_imp$tribe_recoded
toc_data$tob_life_imp<-toc_imp$tob_life
toc_data$tob_c_imp<-toc_imp$tob_c
toc_data$mj_life_imp<-toc_imp$mj_life
toc_data$mj_c_imp<-toc_imp$mj_c
toc_data$cocaine_life_imp<-toc_imp$cocaine_life
toc_data$cocaine_c_imp<-toc_imp$cocaine_c
toc_data$heroin_life_imp<-toc_imp$heroin_life
toc_data$heroin_c_imp<-toc_imp$heroin_c
toc_data$khat_life_imp<-toc_imp$khat_life
toc_data$khat_c_imp<-toc_imp$khat_c
toc_data$tambu_life_imp<-toc_imp$tambu_life
toc_data$tambu_c_imp<-toc_imp$tambu_c
toc_data$kuberi_life_imp<-toc_imp$kuberi_life
toc_data$kuberi_c_imp<-toc_imp$kuberi_c
toc_data$audit1_imp<-toc_imp$audit1
toc_data$audit2_imp<-toc_imp$audit2
toc_data$audit3_imp<-toc_imp$audit3
toc_data$audit4_imp<-toc_imp$audit4
toc_data$audit5_imp<-toc_imp$audit5
toc_data$audit6_imp<-toc_imp$audit6
toc_data$audit7_imp<-toc_imp$audit7
toc_data$audit8_imp<-toc_imp$audit8
toc_data$audit9_imp<-toc_imp$audit9
toc_data$audit10_imp<-toc_imp$audit10
toc_data$audit_score_imp<-toc_imp$audit_score
toc_data$otherdrugs_imp<-toc_imp$otherdrugs
toc_data$gos_imp<-toc_imp$gos_recoded
toc_data$insurance_imp<-toc_imp$insurance
toc_data$moi_imp<-toc_imp$moi_recoded
toc_data$live<-toc_imp$live
toc_data$avpu_arred_imp<-toc_imp$avpu_arred
toc_data$rr_arred_imp<-toc_imp$rr_arred
toc_data$hr_arred_imp<-toc_imp$hr_arred
toc_data$sbp_arred_imp<-toc_imp$sbp_arred
toc_data$pox_arred_imp<-toc_imp$pox_arred
toc_data$surg_1_imp<-toc_imp$surg_1
toc_data$dispo_loc_imp<-toc_imp$dispo_loc
toc_data$gcs_tot_imp<-toc_imp$gcs_tot_recoded
toc_data$anyalcohol_imp<-toc_imp$anyalcohol

#creating new variable for audit score with imputed audit1-audit10
toc_data$audit_score_sum <- toc_data$audit1_imp + toc_data$audit2_imp + toc_data$audit3_imp + toc_data$audit4_imp + toc_data$audit5_imp + toc_data$audit6_imp + toc_data$audit7_imp + toc_data$audit8_imp + toc_data$audit9_imp + toc_data$audit10_imp
#separating Audit Score into hazardous and nonhazardous scores
toc_data$audit_score_categorical <- car::recode(toc_data$audit_score_sum, 
                                   "0:7='Anonhazard'; 8:40='Bhazard'")

##Creating KTS Variable
KTS_data<-with(toc_data, data.frame(age_imp, avpu_arred_imp, rr_arred_imp, 
                               hr_arred_imp,sbp_arred_imp, pox_arred_imp, surg_1_imp, 
                               dispo_loc_imp, gcs_tot_imp))
#recode variables into KTS scoring
KTS_data$age_imp_cat<-as.numeric(car::recode(KTS_data$age_imp,
                                             "5:55='1'; 0:5='0'; 56:110='0'"))
table(KTS_data$age_imp_cat)

KTS_data$sbp_arred_imp_cat<-as.numeric(car::recode(KTS_data$sbp_arred_imp,
                                                   "90:300='2'; 50:89='1'; 0:49='0'"))
table(KTS_data$sbp_arred_imp_cat)

KTS_data$rr_arred_imp_cat<-as.numeric(car::recode(KTS_data$rr_arred_imp,
                                                  "10:29='2'; 30:100='1'; 0:9='0'"))
table(KTS_data$rr_arred_imp_cat)

KTS_data$avpu_arred_imp_cat<-as.numeric(car::recode(KTS_data$avpu_arred_imp,
                                                    "0='3'; 1='2'; 2='1'; 3='0'"))
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
                                              "0:8='Severe KTS'; 9:10='Mild KTS'"))
#adding KTS back into main toc dataset
toc_data$kts_score<-KTS_data$kts_score_cat
toc_data$kts_score_cat<-KTS_data$kts_score_cat

#############################################################################
#TABLE 1. SAMPLE DESCRIPTION BROKEN DOWN BY INJURY SEVERITY
#############################################################################

#INJURY SEVERITY
#Categorical data by alcohol use
####################################

#UNADJUSTED PREVALENCE 4 OUTCOMES
with(toc_data,table(kts_score_cat))
with(toc_data,prop.table(table(kts_score_cat)))

#GENDER
with(toc_data,table(female_imp))
with(toc_data,prop.table(table(female_imp)))
#by alcohol use
table<-with(toc_data,table(female_imp,kts_score_cat)) 
table
with(toc_data,prop.table(table(female_imp,kts_score_cat),2))
chisq.test(table)
fisher.test(table)
# assocstats(table) #vcd package
# logmodel<-glm(talked_dr ~ female
#       ,family=binomial, data=toc_data)
# summary(logmodel)
# exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#by tabaco use

#MARITAL STATUS
with(toc_data,table(marital_imp))
with(toc_data,prop.table(table(marital_imp)))
#by alcohol use
table<-with(toc_data,table(marital_imp,kts_score_cat)) 
table
with(toc_data,prop.table(table(marital_imp,kts_score_cat),2))
chisq.test(table)
fisher.test(table)
# assocstats(table) #vcd package
# logmodel<-glm(talked_dr ~ female
#       ,family=binomial, data=toc_data)
# summary(logmodel)
# exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#TRIBE
with(toc_data,table(tribe_imp))
with(toc_data,prop.table(table(tribe_imp)))
#by alcohol use
table<-with(toc_data,table(tribe_imp,kts_score_cat)) 
table
with(toc_data,prop.table(table(tribe_imp,kts_score_cat),2))
chisq.test(table)
fisher.test(table)
# assocstats(table) #vcd package
# logmodel<-glm(talked_dr ~ female
#       ,family=binomial, data=toc_data)
# summary(logmodel)
# exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#EMPLOYMENT
with(toc_data,table(employ_imp))
with(toc_data,prop.table(table(employ_imp)))
#by alcohol use
table<-with(toc_data,table(employ_imp,kts_score_cat)) 
table
with(toc_data,prop.table(table(employ_imp,kts_score_cat),2))
chisq.test(table)
fisher.test(table)
# assocstats(table) #vcd package
# logmodel<-glm(talked_dr ~ female
#       ,family=binomial, data=toc_data)
# summary(logmodel)
# exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#MECHANISM OF INJURY
with(toc_data,table(moi_imp))
with(toc_data,prop.table(table(moi_imp)))
#by alcohol use
table<-with(toc_data,table(moi_imp,kts_score_cat)) 
table
with(toc_data,prop.table(table(moi_imp,kts_score_cat),2))
chisq.test(table)
fisher.test(table)
# assocstats(table) #vcd package
# logmodel<-glm(talked_dr ~ female
#       ,family=binomial, data=toc_data)
# summary(logmodel)
# exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#INSURANCE TYPE
with(toc_data,table(insurance_recoded))
with(toc_data,prop.table(table(insurance_recoded)))
#by alcohol use
table<-with(toc_data,table(insurance_recoded,kts_score_cat)) 
table
with(toc_data,prop.table(table(insurance_recoded,kts_score_cat),2))
chisq.test(table)
fisher.test(table)
# assocstats(table) #vcd package
# logmodel<-glm(talked_dr ~ female
#       ,family=binomial, data=toc_data)
# summary(logmodel)
# exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#Continuous data

#Age
with(toc_data,
  describe(age_imp))
with(toc_data,
  by(age_imp,kts_score_cat,describe))
# with(toc_data,
#   t.test(age_imp~kts_score_cat))
# logmodel<-glm(kts_score_cat ~ age
#       ,family=binomial, data=toc_data)
# summary(logmodel)
# exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#Years of Education
with(toc_data,
  describe(education_imp))
with(toc_data,
  by(education_imp,kts_score_cat,describe))
# with(toc_data,
#   t.test(education_imp~kts_score_cat))

#Multivariate model

model1table1<-glm(kts_score_cat ~ 
                                  # tob_c_imp + 
                                  # otherdrugs2_c +
                                  age_imp+
                                  female_imp+
                                  education_imp+
                                  marital_imp+
                                  tribe_imp+
                                  moi_imp+
                                  insurance_imp, 
                            family = binomial(link = logit),
                            data=toc_data)
summary(model1table1)
round(exp(cbind(Odds=coef(model1table1),confint(model1table1,level=0.95))), 3)
vif(model1table1)


#############################################################################
#TABLE 2. ASSOCIATiON BERTWEEN SOCIO DEMOGRAPHIC VARIABLES AND SUBSTANCE USE
#############################################################################

#ANY ALCOHOL USE
#Categorical data by alcohol use
####################################

#UNADJUSTED PREVALENCE
with(toc_data,table(anyalcohol_imp))
with(toc_data,prop.table(table(anyalcohol_imp)))

#GENDER
with(toc_data,table(female_imp))
with(toc_data,prop.table(table(female_imp)))
#by alcohol use
table<-with(toc_data,table(female_imp,anyalcohol_imp)) 
table
with(toc_data,prop.table(table(female_imp,anyalcohol_imp),2))
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package
# logmodel<-glm(talked_dr ~ female
#       ,family=binomial, data=toc_data)
# summary(logmodel)
# exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#by tabaco use

#MARITAL STATUS
with(toc_data,table(marital_imp))
with(toc_data,prop.table(table(marital_imp)))
#by alcohol use
table<-with(toc_data,table(marital_imp,anyalcohol_imp)) 
table
with(toc_data,prop.table(table(marital_imp,anyalcohol_imp),2))
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package
# logmodel<-glm(talked_dr ~ female
#       ,family=binomial, data=toc_data)
# summary(logmodel)
# exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#TRIBE
with(toc_data,table(tribe_imp))
with(toc_data,prop.table(table(tribe_imp)))
#by alcohol use
table<-with(toc_data,table(tribe_imp,anyalcohol_imp)) 
table
with(toc_data,prop.table(table(tribe_imp,anyalcohol_imp),2))
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package
# logmodel<-glm(talked_dr ~ female
#       ,family=binomial, data=toc_data)
# summary(logmodel)
# exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#EMPLOYMENT
with(toc_data,table(employ_imp))
with(toc_data,prop.table(table(employ_imp)))
#by alcohol use
table<-with(toc_data,table(employ_imp,anyalcohol_imp)) 
table
with(toc_data,prop.table(table(employ_imp,anyalcohol_imp),2))
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package
# logmodel<-glm(talked_dr ~ female
#       ,family=binomial, data=toc_data)
# summary(logmodel)
# exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#MECHANISM OF INJURY
with(toc_data,table(moi_imp))
with(toc_data,prop.table(table(moi_imp)))
#by alcohol use
table<-with(toc_data,table(moi_imp,anyalcohol_imp)) 
table
with(toc_data,prop.table(table(moi_imp,anyalcohol_imp),2))
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package
# logmodel<-glm(talked_dr ~ female
#       ,family=binomial, data=toc_data)
# summary(logmodel)
# exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#INSURANCE TYPE
with(toc_data,table(insurance_recoded))
with(toc_data,prop.table(table(insurance_recoded)))
#by alcohol use
table<-with(toc_data,table(insurance_recoded,anyalcohol_imp)) 
table
with(toc_data,prop.table(table(insurance_recoded,anyalcohol_imp),2))
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package
# logmodel<-glm(talked_dr ~ female
#       ,family=binomial, data=toc_data)
# summary(logmodel)
# exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#Continuous data

#Age
with(toc_data,
  describe(age_imp))
with(toc_data,
  by(age_imp,anyalcohol_imp,describe))
# with(toc_data,
#   t.test(age_imp~anyalcohol_imp))
# logmodel<-glm(anyalcohol_imp ~ age
#       ,family=binomial, data=toc_data)
# summary(logmodel)
# exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#Years of Education
with(toc_data,
  describe(education_imp))
with(toc_data,
  by(education_imp,anyalcohol_imp,describe))
# with(toc_data,
#   t.test(education_imp~anyalcohol_imp))

#Multivariate model

model2able2_anyalcohol<-glm(anyalcohol_imp ~ 
                                  tob_c_imp + 
                                  otherdrugs2_c +
                                  age_imp+
                                  female_imp+
                                  education_imp+
                                  marital_imp+
                                  tribe_imp+
                                  moi_imp+
                                  insurance_imp, 
                            family = binomial(link = logit),
                            data=toc_data)
summary(model2able2_anyalcohol)
round(exp(cbind(Odds=coef(model2able2_anyalcohol),confint(model2able2_anyalcohol,level=0.95))), 3)
vif(model2able2_anyalcohol)

#CURRENT TABACCO USE
#Categorical data by alcohol use
####################################

#UNADJUSTED PREVALENCE
with(toc_data,table(tob_c_imp))
with(toc_data,prop.table(table(tob_c_imp)))

#GENDER
with(toc_data,table(female_imp))
with(toc_data,prop.table(table(female_imp)))
#by alcohol use
table<-with(toc_data,table(female_imp,tob_c_imp)) 
table
with(toc_data,prop.table(table(female_imp,tob_c_imp),2))
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package
# logmodel<-glm(talked_dr ~ female
#       ,family=binomial, data=toc_data)
# summary(logmodel)
# exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#by tabaco use

#MARITAL STATUS
with(toc_data,table(marital_imp))
with(toc_data,prop.table(table(marital_imp)))
#by alcohol use
table<-with(toc_data,table(marital_imp,tob_c_imp)) 
table
with(toc_data,prop.table(table(marital_imp,tob_c_imp),2))
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package
# logmodel<-glm(talked_dr ~ female
#       ,family=binomial, data=toc_data)
# summary(logmodel)
# exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#TRIBE
with(toc_data,table(tribe_imp))
with(toc_data,prop.table(table(tribe_imp)))
#by alcohol use
table<-with(toc_data,table(tribe_imp,tob_c_imp)) 
table
with(toc_data,prop.table(table(tribe_imp,tob_c_imp),2))
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package
# logmodel<-glm(talked_dr ~ female
#       ,family=binomial, data=toc_data)
# summary(logmodel)
# exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#EMPLOYMENT
with(toc_data,table(employ_imp))
with(toc_data,prop.table(table(employ_imp)))
#by alcohol use
table<-with(toc_data,table(employ_imp,tob_c_imp)) 
table
with(toc_data,prop.table(table(employ_imp,tob_c_imp),2))
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package
# logmodel<-glm(talked_dr ~ female
#       ,family=binomial, data=toc_data)
# summary(logmodel)
# exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#MECHANISM OF INJURY
with(toc_data,table(moi_imp))
with(toc_data,prop.table(table(moi_imp)))
#by alcohol use
table<-with(toc_data,table(moi_imp,tob_c_imp)) 
table
with(toc_data,prop.table(table(moi_imp,tob_c_imp),2))
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package
# logmodel<-glm(talked_dr ~ female
#       ,family=binomial, data=toc_data)
# summary(logmodel)
# exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#INSURANCE TYPE
with(toc_data,table(insurance_recoded))
with(toc_data,prop.table(table(insurance_recoded)))
#by alcohol use
table<-with(toc_data,table(insurance_recoded,tob_c_imp)) 
table
with(toc_data,prop.table(table(insurance_recoded,tob_c_imp),2))
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package
# logmodel<-glm(talked_dr ~ female
#       ,family=binomial, data=toc_data)
# summary(logmodel)
# exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#Continuous data

#Age
with(toc_data,
  describe(age_imp))
with(toc_data,
  by(age_imp,tob_c_imp,describe))
with(toc_data,
  t.test(age_imp~tob_c_imp))
# logmodel<-glm(tob_c_imp ~ age
#       ,family=binomial, data=toc_data)
# summary(logmodel)
# exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#Years of Education
with(toc_data,
  describe(education_imp))
with(toc_data,
  by(education_imp,tob_c_imp,describe))
with(toc_data,
  t.test(education_imp~tob_c_imp))

#Multivariate model

model2able2_tabacco<-glm(tob_c_imp ~ 
                                  anyalcohol + 
                                  otherdrugs2_c +
                                  age_imp+
                                  female_imp+
                                  education_imp+
                                  marital_imp+
                                  tribe_imp+
                                  moi_imp+
                                  insurance_imp, 
                            family = binomial(link = logit),
                            data=toc_data)
summary(model2able2_tabacco)
round(exp(cbind(Odds=coef(model2able2_tabacco),confint(model2able2_tabacco,level=0.95))), 3)
vif(model2able2_tabacco)

#FREQUENCY IS TO LOW. LET'S NOT USE IT.
# #CURRENT OTHER DRUG USE
# #Categorical data by other drugs
# ####################################

# #UNADJUSTED PREVALENCE
# with(toc_data,table(otherdrugs_imp))
# with(toc_data,prop.table(table(otherdrugs_imp)))

# #GENDER
# with(toc_data,table(female_imp))
# with(toc_data,prop.table(table(female_imp)))
# #by alcohol use
# table<-with(toc_data,table(female_imp,otherdrugs_imp)) 
# table
# with(toc_data,prop.table(table(female_imp,otherdrugs_imp),2))
# # chisq.test(table)
# # fisher.test(table)
# # assocstats(table) #vcd package
# # logmodel<-glm(talked_dr ~ female
# #       ,family=binomial, data=toc_data)
# # summary(logmodel)
# # exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

# #by tabaco use

# #MARITAL STATUS
# with(toc_data,table(marital_imp))
# with(toc_data,prop.table(table(marital_imp)))
# #by alcohol use
# table<-with(toc_data,table(marital_imp,otherdrugs_imp)) 
# table
# with(toc_data,prop.table(table(marital_imp,otherdrugs_imp),2))
# # chisq.test(table)
# # fisher.test(table)
# # assocstats(table) #vcd package
# # logmodel<-glm(talked_dr ~ female
# #       ,family=binomial, data=toc_data)
# # summary(logmodel)
# # exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

# #TRIBE
# with(toc_data,table(tribe_imp))
# with(toc_data,prop.table(table(tribe_imp)))
# #by alcohol use
# table<-with(toc_data,table(tribe_imp,otherdrugs_imp)) 
# table
# with(toc_data,prop.table(table(tribe_imp,otherdrugs_imp),2))
# # chisq.test(table)
# # fisher.test(table)
# # assocstats(table) #vcd package
# # logmodel<-glm(talked_dr ~ female
# #       ,family=binomial, data=toc_data)
# # summary(logmodel)
# # exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

# #EMPLOYMENT
# with(toc_data,table(employ_imp))
# with(toc_data,prop.table(table(employ_imp)))
# #by alcohol use
# table<-with(toc_data,table(employ_imp,otherdrugs_imp)) 
# table
# with(toc_data,prop.table(table(employ_imp,otherdrugs_imp),2))
# # chisq.test(table)
# # fisher.test(table)
# # assocstats(table) #vcd package
# # logmodel<-glm(talked_dr ~ female
# #       ,family=binomial, data=toc_data)
# # summary(logmodel)
# # exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

# #MECHANISM OF INJURY
# with(toc_data,table(moi_imp))
# with(toc_data,prop.table(table(moi_imp)))
# #by alcohol use
# table<-with(toc_data,table(moi_imp,otherdrugs_imp)) 
# table
# with(toc_data,prop.table(table(moi_imp,otherdrugs_imp),2))
# # chisq.test(table)
# # fisher.test(table)
# # assocstats(table) #vcd package
# # logmodel<-glm(talked_dr ~ female
# #       ,family=binomial, data=toc_data)
# # summary(logmodel)
# # exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

# #INSURANCE TYPE
# with(toc_data,table(insurance_recoded))
# with(toc_data,prop.table(table(insurance_recoded)))
# #by alcohol use
# table<-with(toc_data,table(insurance_recoded,otherdrugs_imp)) 
# table
# with(toc_data,prop.table(table(insurance_recoded,otherdrugs_imp),2))
# # chisq.test(table)
# # fisher.test(table)
# # assocstats(table) #vcd package
# # logmodel<-glm(talked_dr ~ female
# #       ,family=binomial, data=toc_data)
# # summary(logmodel)
# # exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

# #Continuous data

# #Age
# with(toc_data,
#   describe(age_imp))
# with(toc_data,
#   by(age_imp,otherdrugs_imp,describe))
# with(toc_data,
#   t.test(age_imp~otherdrugs_imp))
# # logmodel<-glm(otherdrugs_imp ~ age
# #       ,family=binomial, data=toc_data)
# # summary(logmodel)
# # exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

# #Years of Education
# with(toc_data,
#   describe(education_imp))
# with(toc_data,
#   by(education_imp,otherdrugs_imp,describe))
# with(toc_data,
#   t.test(education_imp~otherdrugs_imp))

# #Multivariable 

# model2able2_otherdrugs<-glm(otherdrugs2_c ~ 
#                                   # tob_c_imp + 
#                                   # otherdrugs2_c +
#                                   age_imp+
#                                   female_imp+
#                                   education_imp+
#                                   marital_imp+
#                                   tribe_imp+
#                                   moi_imp+
#                                   insurance_imp, 
#                             family = binomial(link = logit),
#                             data=toc_data)
# summary(model2able2_anyalcohol)
# round(exp(cbind(Odds=coef(model2able2_anyalcohol),confint(model2able2_anyalcohol,level=0.95))), 3)
# vif(model2able2_anyalcohol)

#############################################################################
#TABLE 3. ASSOCIATiON BERTWEEN SUBSTANCE USE AND OUTCOMES ADJUSTING 
#FOR SOCIOECONOMIC DEMOGRAPHIC VARIABLES
#############################################################################

#ALcohol by KTS
with(toc_data,table(anyalcohol_imp))
with(toc_data,prop.table(table(anyalcohol_imp)))
#by alcohol use
table<-with(toc_data,table(anyalcohol_imp,kts_score_cat)) 
table
with(toc_data,prop.table(table(anyalcohol_imp,kts_score_cat),2))
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package
# logmodel<-glm(talked_dr ~ female
#       ,family=binomial, data=toc_data)
# summary(logmodel)
# exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#ALcohol by KTS
with(toc_data,table(tob_c_imp))
with(toc_data,prop.table(table(tob_c_imp)))
#by alcohol use
table<-with(toc_data,table(tob_c_imp,kts_score_cat)) 
table
with(toc_data,prop.table(table(tob_c_imp,kts_score_cat),2))
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package
# logmodel<-glm(talked_dr ~ female
#       ,family=binomial, data=toc_data)
# summary(logmodel)
# exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#ALcohol by KTS
with(toc_data,table(anydrug_c))
with(toc_data,prop.table(table(anydrug_c)))
#by alcohol use
table<-with(toc_data,table(anydrug_c,kts_score_cat)) 
table
with(toc_data,prop.table(table(anydrug_c,kts_score_cat),2))
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package
# logmodel<-glm(talked_dr ~ female
#       ,family=binomial, data=toc_data)
# summary(logmodel)
# exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#ALcohol by KTS
with(toc_data,table(khat_c_imp))
with(toc_data,prop.table(table(khat_c_imp)))
#by alcohol use
table<-with(toc_data,table(khat_c_imp,kts_score_cat)) 
table
with(toc_data,prop.table(table(khat_c_imp,kts_score_cat),2))
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package
# logmodel<-glm(talked_dr ~ female
#       ,family=binomial, data=toc_data)
# summary(logmodel)
# exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#ALcohol by KTS
with(toc_data,table(mj_c_imp))
with(toc_data,prop.table(table(mj_c_imp)))
#by alcohol use
table<-with(toc_data,table(mj_c_imp,kts_score_cat)) 
table
with(toc_data,prop.table(table(mj_c_imp,kts_score_cat),2))
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package
# logmodel<-glm(talked_dr ~ female
#       ,family=binomial, data=toc_data)
# summary(logmodel)
# exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#ALcohol by KTS
with(toc_data,table(audit_score_categorical))
with(toc_data,prop.table(table(audit_score_categorical)))
#by alcohol use
table<-with(toc_data,table(audit_score_categorical,kts_score_cat)) 
table
with(toc_data,prop.table(table(audit_score_categorical,kts_score_cat),2))
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package
# logmodel<-glm(talked_dr ~ female
#       ,family=binomial, data=toc_data)
# summary(logmodel)
# exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 

#############################################################################
#TABLE 4. ASSOCIATiON BERTWEEN SUBSTANCE USE AND OUTCOMES ADJUSTING 
#FOR SOCIOECONOMIC DEMOGRAPHIC VARIABLES
#############################################################################

model1_KTSvsalcohol <-glm(toc_data$kts_score_cat ~ toc_data$audit_score_categorical +
                                  # toc_data$tob_c_imp + 
                                  # toc_data$otherdrugs2_c
                                  toc_data$age_imp+
                                  toc_data$female_imp+
                                  toc_data$education_imp+
                                  toc_data$marital_imp+
                                  toc_data$tribe_imp+
                                  toc_data$moi_imp+
                                  toc_data$insurance_imp, family = binomial(link = logit))
summary(model1_KTSvsalcohol)
round(exp(cbind(Odds=coef(model1_KTSvsalcohol),confint(model1_KTSvsalcohol,level=0.95))), 3)
vif(model1_KTSvsalcohol)

model1_KTSvstabacco <-glm(toc_data$kts_score_cat ~  toc_data$tob_c_imp +
                                  # toc_data$audit_score_imp + 
                                  # toc_data$otherdrugs2_c
                                  toc_data$age_imp+
                                  toc_data$female_imp+
                                  toc_data$education_imp+
                                  toc_data$marital_imp+
                                  toc_data$tribe_imp+
                                  toc_data$moi_imp+
                                  toc_data$insurance_imp, family = binomial(link = logit))
summary(model1_KTSvstabacco)
round(exp(cbind(Odds=coef(model1_KTSvstabacco),confint(model1_KTSvstabacco,level=0.95))), 3)
vif(model1_KTSvstabacco)

model1_KTSvsanydrugs <-glm(toc_data$kts_score_cat ~  toc_data$anydrug_c +
                                  # toc_data$audit_score_imp + 
                                  # toc_data$otherdrugs2_c
                                  toc_data$age_imp+
                                  toc_data$female_imp+
                                  toc_data$education_imp+
                                  toc_data$marital_imp+
                                  toc_data$tribe_imp+
                                  toc_data$moi_imp+
                                  toc_data$insurance_imp, family = binomial(link = logit))
summary(model1_KTSvsanydrugs)
round(exp(cbind(Odds=coef(model1_KTSvsanydrugs),confint(model1_KTSvsanydrugs,level=0.95))), 3)
vif(model1_KTSvsanydrugs)