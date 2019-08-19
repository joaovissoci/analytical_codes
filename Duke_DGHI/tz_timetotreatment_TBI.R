# Transitions of Care: Time to Treatment

# IMPORT DATA
data<-read.csv("~/Desktop/R/ToCTraumaRegistry_DATA_2019-02-20_1022.csv")

# SETTING THE ENVIRONMENT
library(car)
library(psych)

# CREATING A DATAFRAME WITH DESCRIPTIVE VARIABLES OF INTEREST
data_descriptive<-with(data, data.frame(live, age, female, dateinjury, timeinjury, datearrivekcmc, timearrivekcmc, moa, moi, rr_arred, hr_arred, sbp_arred, pox_arred, avpu_arred, gcs_eye, gcs_verbal, gcs_motor, dispo_loc, dispo_oloc, dc, dc_date, dc_time, death, death_date, death_time, surg_1, surg_1date, surg_1time, gos, pocus, pocus_date, pocus_time, xray, xray_date, xray_time, ct, ct_date, ct_time, fimcare, fimtoilet, fimtx, fimmotion, fimstairs, fimexpr, fimcompr, fimsocial, fimprob, fimmemory))

# EVALUATION OF DESCRIPTIVE VARIABLES
describe(data_descriptive$pocus)
summary(data_descriptive$fimtx)
table(data_descriptive$fimtx)

# SEPARATE DATASET WITH VARIABLES TO BE IMPUTED
imputed_data<-with(data,data.frame(live, age, female, moi, moa, gcs_eye, gcs_verbal, gcs_motor, rr_arred, hr_arred, sbp_arred, pox_arred, avpu_arred, gos, fimcare, fimtoilet, fimtx, fimmotion, fimstairs, fimexpr, fimcompr, fimsocial, fimprob, fimmemory, surg_1, xray, ct, pocus, dispo_loc))

# IMPUTATION
library(lattice)
library(mice)
imp<-mice(imputed_data, seed = 2222, m=5)
imputed_data<-mice::complete(imp,sample(1:5,1))

# RECODING IMPUTED LIVE
imputed_data$live<-as.factor(car::recode(imputed_data$live,"0='Moshi Urban'; 1='Moshi Rural'; 89='Other'"))
summary(imputed_data$live)

# RECODING IMPUTED FEMALE
imputed_data$female<-as.factor(car::recode(imputed_data$female, "0='Male'; 1='Female'"))
summary(imputed_data$female)

# RECODING IMPUTED MOI
imputed_data$moi<-as.factor(car::recode(imputed_data$moi,"0='Road Traffic Injury';1='Assault'; 2='Drowning'; 3='Fall'; 4='Burn'; 89='Other'; 99='Unknown'"))
summary(imputed_data$moi)

# RECODING IMPUTED MOA
imputed_data$moa<-as.factor(car::recode(imputed_data$moa,"0='Ambulance from Outside Hospital';1='Private Car'; 2='Bijaji'; 3='Police Car'; 4='Private Motorcycle'; 5='Bota Bota'; 6='Taxi'; 89='Other'; 99='Unknown'"))
summary(imputed_data$moa)

# EVALUATION OF IMPUTED DESCRIPTIVE VARIABLES
describe(imputed_data$ct)
table(imputed_data$pocus)
prop.table(table(imputed_data$pocus))*100
summary(imputed_data$pox_arred)
unique(imputed_data$live)

# INSERTING IMPUTED VARIABLES INTO DATASET
data_descriptive$live_imp<-imputed_data$live
data_descriptive$age_imp<-imputed_data$age
data_descriptive$female_imp<-imputed_data$female
data_descriptive$moi_imp<-imputed_data$moi
data_descriptive$moa_imp<-imputed_data$moa
data_descriptive$gcs_eye_imp<-imputed_data$gcs_eye
data_descriptive$gcs_verbal_imp<-imputed_data$gcs_verbal
data_descriptive$gcs_motor_imp<-imputed_data$gcs_motor
data_descriptive$gos_imp<-imputed_data$gos
data_descriptive$rr_arred_imp<-imputed_data$rr_arred
data_descriptive$hr_arred_imp<-imputed_data$hr_arred
data_descriptive$sbp_arred_imp<-imputed_data$sbp_arred
data_descriptive$pox_arred_imp<-imputed_data$pox_arred
data_descriptive$avpu_arred_imp<-imputed_data$avpu_arred
data_descriptive$dispo_loc_imp<-imputed_data$dispo_loc
data_descriptive$surg_1_imp<-imputed_data$surg_1
data_descriptive$fimcare_imp<-imputed_data$fimcare
data_descriptive$fimtoilet_imp<-imputed_data$fimtoilet
data_descriptive$fimtx_imp<-imputed_data$fimtx
data_descriptive$fimmotion_imp<-imputed_data$fimmotion
data_descriptive$fimstairs_imp<-imputed_data$fimstairs
data_descriptive$fimexpr_imp<-imputed_data$fimexpr
data_descriptive$fimcompr_imp<-imputed_data$fimcompr
data_descriptive$fimsocial_imp<-imputed_data$fimsocial
data_descriptive$fimprob_imp<-imputed_data$fimprob
data_descriptive$fimmemory_imp<-imputed_data$fimmemory
data_descriptive$xray_imp<-imputed_data$xray
data_descriptive$ct_imp<-imputed_data$ct
data_descriptive$pocus<-imputed_data$pocus

# CREATING AN IMPUTED GCS_TOT VARIABLE
library(dplyr)
library(tidyr)

data_descriptive$gcs_tot_imp<-mutate(data_descriptive$gcs_tot_imp<-(data_descriptive$gcs_eye_imp+data_descriptive$gcs_verbal_imp+data_descriptive$gcs_motor_imp))
table(data_descriptive$gcs_tot_imp)

# RECODING A GCS_TOT_CAT VARIABLE
data_descriptive$gcs_tot_imp_cat<-as.factor(car::recode(data_descriptive$gcs_tot_imp,"1:12='Poor GCS'; 13:15='Good GCS'"))
summary(data_descriptive$gcs_tot_imp_cat)

# CREATING A KTS VARIABLE
# START BY CREATING A SEPARATE DATASET WITH VARIABLES REQUIRES FOR KTS
KTS_data<-with(data_descriptive, data.frame(age_imp, avpu_arred_imp, rr_arred_imp, hr_arred_imp, sbp_arred_imp, pox_arred_imp, surg_1_imp, dispo_loc_imp, gcs_tot_imp))

# RECODE VARIABLES TO REFLECT KTS SCORING
# RECODING AGE
KTS_data$age_imp_cat<-as.numeric(car::recode(KTS_data$age_imp,"5:55='1'; 0:5='0'; 56:110='0'"))
table(KTS_data$age_imp_cat)

# RECODING SBP
KTS_data$sbp_arred_imp_cat<-as.numeric(car::recode(KTS_data$sbp_arred_imp,"90:300='2'; 50:89='1'; 0:49='0'"))
table(KTS_data$sbp_arred_imp_cat)

# RECODING RR
KTS_data$rr_arred_imp_cat<-as.numeric(car::recode(KTS_data$rr_arred_imp,"10:29='2'; 30:100='1'; 0:9='0'"))
table(KTS_data$rr_arred_imp_cat)

# RECODING AVPU/NEUROLOGIC STATUS
KTS_data$avpu_arred_imp_cat<-as.numeric(car::recode(KTS_data$avpu_arred_imp,"0='3'; 1='2'; 2='1'; 3='0'"))
table(KTS_data$avpu_arred_imp_cat)

# CREATING A NEW VARIABLE 'SERIOUS INJURY'
# FIRST NEED TO RECODE VARIBLES: DISPO ICU, GCS, POX, SBP, HR, RR --> OUTPUT OF 1=BAD, 0=GOOD 
KTS_data$dispo_loc_imp<-as.numeric(car::recode(KTS_data$dispo_loc_imp, "0='1'; 1='1'; 2:89='0'"))

KTS_data$gcs_tot_imp<-as.numeric(car::recode(KTS_data$gcs_tot_imp, "0:12='1'; 13:15='0'"))

KTS_data$pox_arred_imp<-as.numeric(car::recode(KTS_data$pox_arred_imp, "0:91='1'; 92:100='0'"))

KTS_data$sbp_arred_imp<-as.numeric(car::recode(KTS_data$sbp_arred_imp, "0:99='1'; 100:300='0'"))

KTS_data$hr_arred_imp<-as.numeric(car::recode(KTS_data$hr_arred_imp, "100:300='1'; 0:99='0'"))

KTS_data$rr_arred_imp<-as.numeric(car::recode(KTS_data$rr_arred_imp, "26:80='1'; 0:25='0'"))

table(KTS_data$rr_arred_imp)

# COMBINING VARIABLES FOR SERIOUS INJURY, IF VARIABLE=BAD=1 THEN OUTPUT IS 0, LOWER COMBINED SCORES=WORSE
KTS_data$serious_injury<-ifelse(KTS_data$surg_1_imp==1, 0, 1)+ifelse(KTS_data$dispo_loc_imp==1, 0, 1)+ifelse(KTS_data$gcs_tot_imp==1, 0, 1)+ifelse(KTS_data$pox_arred_imp==1, 0, 1)+ifelse(KTS_data$sbp_arred_imp==1, 0, 1)+ifelse(KTS_data$hr_arred_imp==1, 0, 1)+ifelse(KTS_data$rr_arred_imp==1, 0, 1)
table(KTS_data$serious_injury)

# RECODING SERIOUS INJURY SO IT CAN BE ADDED INTO KTS SCORE
KTS_data$serious_injury<-as.numeric(car::recode(KTS_data$serious_injury, "0:5='0'; 6='1'; 7='2'"))
table(KTS_data$serious_injury)

# COMBINING ALL VARIABLES (AGE, SBP, RR, AVPU, SERIOUS INJURY)
KTS_data<-mutate(KTS_data, kts_score=(KTS_data$age_imp_cat+KTS_data$sbp_arred_imp_cat+KTS_data$rr_arred_imp_cat+KTS_data$avpu_arred_imp_cat+KTS_data$serious_injury))
table(KTS_data$kts_score)

# RECODING KTS SCORE
KTS_data$kts_score_cat<-as.factor(car::recode(KTS_data$kts_score, "0:6='Severe'; 7:8='Moderate'; 9:10='Mild'"))
table(KTS_data$kts_score_cat)
prop.table(table(KTS_data$kts_score_cat))*100

# ADDING KTS INTO THE DATA_DESCRIPTIVE DATASET
data_descriptive$kts_score<-KTS_data$kts_score
data_descriptive$kts_score_cat<-KTS_data$kts_score_cat

# CODING TIME TO CARE: TIME FROM INJURY TO ARRIVAL
library(lubridate)

injuryT<-with(data,paste(dateinjury, timeinjury, sep=" "))
injury_time<-lubridate::ymd_hm(injuryT, tz="Africa/Dar_es_Salaam")

arrivalT<-with(data,paste(datearrivekcmc, timearrivekcmc, sep=" "))
arrival_time<-lubridate::ymd_hm(arrivalT, tz="Africa/Dar_es_Salaam")

diff_time<-difftime(arrival_time, injury_time, units=c("min"))

time_to_care <-NULL

for(i in 1:nrow(data)){if(is.na(diff_time) [i]==TRUE) {time_to_care[i]<-NA} else if(diff_time[i]<0) {time_to_care[i]<-NA} else{time_to_care[i]<-diff_time[i]}}

# INSERTING TIME TO CARE IN HOURS VARIABLE INTO DATASET
data_descriptive$time_to_care<-time_to_care/60
summary(data_descriptive$time_to_care)

# CODING TIME TO CARE: FROM ARRIVAL TO TIME OF XR
arrivalT<-with(data,paste(datearrivekcmc, timearrivekcmc, sep=" "))
arrival_time<-lubridate::ymd_hm(arrivalT, tz="Africa/Dar_es_Salaam")

xr_T<-with(data,paste(xray_date, xray_time, sep=" "))
xr_time<-lubridate::ymd_hm(xr_T, tz="Africa/Dar_es_Salaam")

diff_time1.1<-difftime(xr_time, arrival_time, units=c("min"))

time_to_xr <-NULL

for(i in 1:nrow(data)) {if(is.na(diff_time1.1) [i]==TRUE) {time_to_xr[i]<-NA} else if(diff_time1.1[i]<0) {time_to_xr[i]<-diff_time1.1[i]} else{time_to_xr[i]<-diff_time1.1[i]}}

# INSERTING TIME TO XR IN HOURS VARIABLE INTO DATASET
data_descriptive$time_to_xr<-time_to_xr/60
summary(data_descriptive$time_to_xr)

# CODING TIME TO CARE: FROM ARRIVAL TO TIME OF CT
arrivalT<-with(data,paste(datearrivekcmc, timearrivekcmc, sep=" "))
arrival_time<-lubridate::ymd_hm(arrivalT, tz="Africa/Dar_es_Salaam")

ct_T<-with(data,paste(ct_date, ct_time, sep=" "))
ct_time<-lubridate::ymd_hm(xr_T, tz="Africa/Dar_es_Salaam")

diff_time1.2<-difftime(ct_time, arrival_time, units=c("min"))

time_to_ct <-NULL

for(i in 1:nrow(data)) {if(is.na(diff_time1.2) [i]==TRUE) {time_to_ct[i]<-NA} else if(diff_time1.2[i]<0) {time_to_ct[i]<-diff_time1.2[i]} else{time_to_ct[i]<-diff_time1.2[i]}}

# INSERTING TIME TO CT IN HOURS VARIABLE INTO DATASET
data_descriptive$time_to_ct<-time_to_ct/60
summary(data_descriptive$time_to_ct)

# CODING TIME TO CARE: FROM ARRIVAL TO TIME OF POCUS
arrivalT<-with(data,paste(datearrivekcmc, timearrivekcmc, sep=" "))
arrival_time<-lubridate::ymd_hm(arrivalT, tz="Africa/Dar_es_Salaam")

pocus_T<-with(data,paste(pocus_date, pocus_time, sep=" "))
pocus_time<-lubridate::ymd_hm(xr_T, tz="Africa/Dar_es_Salaam")

diff_time1.3<-difftime(pocus_time, arrival_time, units=c("min"))

time_to_pocus <-NULL

for(i in 1:nrow(data)) {if(is.na(diff_time1.3) [i]==TRUE) {time_to_pocus[i]<-NA} else if(diff_time1.3[i]<0) {time_to_pocus[i]<-diff_time1.3[i]} else{time_to_pocus[i]<-diff_time1.3[i]}}

# INSERTING TIME TO POCUS IN HOURS VARIABLE INTO DATASET
data_descriptive$time_to_pocus<-time_to_pocus/60
summary(data_descriptive$time_to_pocus)

# CODING TIME TO CARE: FROM ARRIVAL TO ED DISPO ICU 
arrivalT<-with(data,paste(datearrivekcmc, timearrivekcmc, sep=" "))
arrival_time<-lubridate::ymd_hm(arrivalT, tz="Africa/Dar_es_Salaam")

icuT<-with(data,paste(date_arricu, time_arricu, sep=" "))
time_to_icu<-lubridate::ymd_hm(icuT, tz="Africa/Dar_es_Salaam")

diff_time2.1<-difftime(time_to_icu, arrival_time, units=c("min"))

time_to_icu <-NULL

for(i in 1:nrow(data)) {if(is.na(diff_time2.1) [i]==TRUE) {time_to_icu[i]<-NA} else if(diff_time2.1[i]<0) {time_to_icu[i]<-diff_time2.1[i]} else{time_to_icu[i]<-diff_time2.1[i]}}

# INSERTING TIME TO ED DISPO ICU IN HOURS VARIABLE INTO DATASET
data_descriptive$time_to_ICU_dispo<-time_to_ICU_dispo/60
summary(data_descriptive$time_to_ICU_dispo)