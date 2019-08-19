setwd("/Users/armandzimmerman/Desktop/R/Prognostic Model")

#Load packages
#install.packages("easypackages")
library(easypackages)

#download any package needed
packages("PerformanceAnalytics","caTools", "DMwR", "abind", "pROC" , "mice", "arm", "bartMachine", "obliqueR")

#load any package need
libraries("dataMaid","caret","AppliedPredictiveModeling","doParallel", "reshape2","foreign","data.table", "car", "mice",
          "corrplot", "PerformanceAnalytics", "ggplot2", "psych", "dplyr","arm","bartMachine","randomForest","caTools", "DMwR", "pROC", "plotROC", "obliqueR","pROC")

##########################################################################
########### INSERTING CODE FROM TIME TO TREATMENT PAPER ##################
##########################################################################

# importing dataset

df <- read.csv("tz_TBIregistry_data.csv")

# separating a dataset with the variables to be imputed

imputed_data<-with(df,data.frame(gcs_tot, resp_rate, sys_bp,
                                   pulse_ox, heart_rate))

# imputing dataset

library(mice)
imp<-mice(imputed_data, seed = 2222, m=5)
imputed_data<-mice::complete(imp,sample(1:5,1))

# adding imputed variables to "data"

df$gcs_tot_imp<-imputed_data$gcs_tot
df$resp_rate_imp<-imputed_data$resp_rate
df$sys_bp_imp<-imputed_data$sys_bp
df$pulse_ox_imp<-imputed_data$pulse_ox
df$heart_rate_imp<-imputed_data$heart_rate

# coding time to arrival (time of injury to time of hospital arrival)

injuryT<-with(df,paste(inj_date, inj_time, sep=" "))
injuryT
injury_time<-as.POSIXct(injuryT, format='%m/%d/%y %H:%M')
injury_time

arrivalT<-with(df,paste(date_arrival, time_arrival, sep=" "))
arrivalT
arrival_time<-as.POSIXct(arrivalT, format='%m/%d/%y %H:%M')
arrival_time

dif_time<-difftime(arrival_time, injury_time, units=c("min"))
dif_time

time_to_arrival <- NULL

for(i in 1:nrow(df))
{if(is.na(dif_time) [i]==TRUE) {time_to_arrival[i]<-NA}
  else if(dif_time[i]<0) {time_to_arrival[i]<-NA}
  else{time_to_arrival[i]<-dif_time[i]}}

time_to_arrival

# inserting time to arrival (hours) variable into dataset

df$time_to_arrival<-time_to_arrival/60
summary(df$time_to_arrival)

# coding time to cdmd (time of hospital arrival to time of cdmd arrival)

arrivalT<-with(df,paste(date_arrival, time_arrival, sep=" "))
arrivalT
arrival_time<-as.POSIXct(arrivalT, format='%m/%d/%y %H:%M')
arrival_time

cdmd_arrivalT<-with(df,paste(date_arrival, cdmd_arrival, sep=" "))
cdmd_arrivalT
cdmd_arrival_time<-as.POSIXct(cdmd_arrivalT, format='%m/%d/%y %H:%M')
cdmd_arrival_time

dif_time2<-difftime(cdmd_arrival_time, arrival_time, units=c("min"))
dif_time2

time_to_cdmd<-NULL

for(i in 1:nrow(df))
{if(is.na(dif_time2) [i]==TRUE) {time_to_cdmd[i]<-NA}
  else if(dif_time2[i]<0) {time_to_cdmd[i]<-dif_time2[i]+1440}
  else{time_to_cdmd[i]<-dif_time2[i]}}

time_to_cdmd

# inserting time to cdmd (hours) into dataset

df$time_to_cdmd<-time_to_cdmd/60
summary(df$time_to_cdmd)

# coding time to chest xray (time of cdmd arrival to time of chest radiograph)

cdmd_arrivalT<-with(df,paste(date_arrival, cdmd_arrival, sep=" "))
cdmd_arrivalT
cdmd_arrival_time<-as.POSIXct(cdmd_arrivalT, format='%m/%d/%y %H:%M')
cdmd_arrival_time

cxr_T<-with(df,paste(date_arrival, cxr_time, sep=" "))
cxr_T
cxr_time<-as.POSIXct(cxr_T, format='%m/%d/%y %H:%M')
cxr_time

dif_time3.1.1<-difftime(cxr_time, cdmd_arrival_time, units=c("min"))
dif_time3.1.1

time_to_cxr<-NULL

for(i in 1:nrow(df))
{if(is.na(dif_time3.1.1) [i]==TRUE) {time_to_cxr[i]<-NA}
  else if(dif_time3.1.1[i]<0) {time_to_cxr[i]<-dif_time3.1.1[i]+1440}
  else{time_to_cxr[i]<-dif_time3.1.1[i]}}

time_to_cxr

# inserting time to chest xray (hours) into dataset

df$time_to_cxr<-time_to_cxr/60
summary(df$time_to_cxr)

# coding time to skull xray (time of cdmd arrival to time of skull xray)

cdmd_arrivalT<-with(df,paste(date_arrival, cdmd_arrival, sep=" "))
cdmd_arrivalT
cdmd_arrival_time<-as.POSIXct(cdmd_arrivalT, format='%m/%d/%y %H:%M')
cdmd_arrival_time

sxr_T<-with(df,paste(date_arrival, skullxr_time, sep=" "))
sxr_T
sxr_time<-as.POSIXct(sxr_T, format='%m/%d/%y %H:%M')
sxr_time

dif_time3.1.2<-difftime(sxr_time, cdmd_arrival_time, units=c("min"))
dif_time3.1.2

time_to_sxr<-NULL

for(i in 1:nrow(df))
{if(is.na(dif_time3.1.2) [i]==TRUE) {time_to_sxr[i]<-NA}
  else if(dif_time3.1.2[i]<0) {time_to_sxr[i]<-dif_time3.1.2[i]+1440}
  else{time_to_sxr[i]<-dif_time3.1.2[i]}}

time_to_sxr

# inserting time to skull xray (hours) into dataset

df$time_to_sxr<-time_to_sxr/60
summary(df$time_to_sxr)

# coding time to labs sent (time of cdmd arrival to time of labs sent)

cdmd_arrivalT<-with(df,paste(date_arrival, cdmd_arrival, sep=" "))
cdmd_arrivalT
cdmd_arrival_time<-as.POSIXct(cdmd_arrivalT, format='%m/%d/%y %H:%M')
cdmd_arrival_time

lab_T<-with(df,paste(date_arrival, labs_time, sep=" "))
lab_T
lab_time<-as.POSIXct(lab_T, format='%m/%d/%y %H:%M')
lab_time

dif_time3.1.3<-difftime(lab_time, cdmd_arrival_time, units=c("min"))
dif_time3.1.3

time_to_labs_sent<-NULL

for(i in 1:nrow(df))
{if(is.na(dif_time3.1.3) [i]==TRUE) {time_to_labs_sent[i]<-NA}
  else if(dif_time3.1.3[i]<0) {time_to_labs_sent[i]<-dif_time3.1.3[i]+1440}
  else{time_to_labs_sent[i]<-dif_time3.1.3[i]}}

time_to_labs_sent

# inserting time to labs sent (hours) into dataset

df$time_to_labs_sent<-time_to_labs_sent/60
summary(df$time_to_labs_sent)

# coding time to brain CT scan (time of cdmd arrival to time of brain CT scan)

cdmd_arrivalT<-with(df,paste(date_arrival, cdmd_arrival, sep=" "))
cdmd_arrivalT
cdmd_arrival_time<-as.POSIXct(cdmd_arrivalT, format='%m/%d/%y %H:%M')
cdmd_arrival_time

brain_ct_T<-with(df,paste(ctbrain_day, ctbrain_time, sep=" "))
brain_ct_T
brain_ct_time<-as.POSIXct(brain_ct_T, format='%m/%d/%y %H:%M')
brain_ct_time

dif_time3.1.4<-difftime(brain_ct_time, cdmd_arrival_time, units=c("min"))
dif_time3.1.4

time_to_brain_ct<-NULL

for(i in 1:nrow(df))
{if(is.na(dif_time3.1.4) [i]==TRUE) {time_to_brain_ct[i]<-NA}
  else if(dif_time3.1.4[i]<0) {time_to_brain_ct[i]<-NA}
  else{time_to_brain_ct[i]<-dif_time3.1.4[i]}}

time_to_brain_ct

# inserting time to brian CT (hours) into dataset

df$time_to_brain_ct<-time_to_brain_ct/60
summary(df$time_to_brain_ct)

# coding time to fluids (time of cdmd arrival to time of fluids)

cdmd_arrivalT<-with(df,paste(date_arrival, cdmd_arrival, sep=" "))
cdmd_arrivalT
cdmd_arrival_time<-as.POSIXct(cdmd_arrivalT, format='%m/%d/%y %H:%M')
cdmd_arrival_time

fluid_T<-with(df,paste(date_arrival, fluids_time, sep=" "))
fluid_T
fluid_time<-as.POSIXct(fluid_T, format='%m/%d/%y %H:%M')
fluid_time

dif_time3.2.1<-difftime(fluid_time, cdmd_arrival_time, units=c("min"))
dif_time3.2.1

time_to_fluids<-NULL

for(i in 1:nrow(df))
{if(is.na(dif_time3.2.1) [i]==TRUE) {time_to_fluids[i]<-NA}
  else if(dif_time3.2.1[i]<0) {time_to_fluids[i]<-dif_time3.2.1[i]+1440}
  else{time_to_fluids[i]<-dif_time3.2.1[i]}}

time_to_fluids

# inserting time to fluids (hours) into dataset

df$time_to_fluids<-time_to_fluids/60
summary(df$time_to_fluids)

# coding time to oxygen (time of cdmd arrival to time of oxygen)

cdmd_arrivalT<-with(df,paste(date_arrival, cdmd_arrival, sep=" "))
cdmd_arrivalT
cdmd_arrival_time<-as.POSIXct(cdmd_arrivalT, format='%m/%d/%y %H:%M')
cdmd_arrival_time

ox_T<-with(df,paste(date_arrival, oxygen_time, sep=" "))
ox_T
ox_time<-as.POSIXct(ox_T, format='%m/%d/%y %H:%M')
ox_time

dif_time3.2.2<-difftime(ox_time, cdmd_arrival_time, units=c("min"))
dif_time3.2.2

time_to_oxygen<-NULL

for(i in 1:nrow(df))
{if(is.na(dif_time3.2.2) [i]==TRUE) {time_to_oxygen[i]<-NA}
  else if(dif_time3.2.2[i]<0) {time_to_oxygen[i]<-dif_time3.2.2[i]+1440}
  else{time_to_oxygen[i]<-dif_time3.2.2[i]}}

time_to_oxygen

# inserting time to oxygen (hours) into dataset

df$time_to_oxygen<-time_to_oxygen/60
summary(df$time_to_oxygen)

# coding time to mannitol (time of cdmd arrival to time of mannitol)

cdmd_arrivalT<-with(df,paste(date_arrival, cdmd_arrival, sep=" "))
cdmd_arrivalT
cdmd_arrival_time<-as.POSIXct(cdmd_arrivalT, format='%m/%d/%y %H:%M')
cdmd_arrival_time

mannit_T<-with(df,paste(date_arrival, mannitol_time, sep=" "))
mannit_T
mannit_time<-as.POSIXct(mannit_T, format='%m/%d/%y %H:%M')
mannit_time

dif_time3.2.3<-difftime(mannit_time, cdmd_arrival_time, units=c("min"))
dif_time3.2.3

time_to_mannitol<-NULL

for(i in 1:nrow(df))
{if(is.na(dif_time3.2.3) [i]==TRUE) {time_to_mannitol[i]<-NA}
  else if(dif_time3.2.3[i]<0) {time_to_mannitol[i]<-dif_time3.2.3[i]+1440}
  else{time_to_mannitol[i]<-dif_time3.2.3[i]}}

time_to_mannitol

# inserting time to mannitol (hours) into dataset

df$time_to_mannitol<-time_to_mannitol/60
summary(df$time_to_mannitol)

# coding time to surgery MD arrival (time of cdmd arrival to time of surgery MD arrival)

cdmd_arrivalT<-with(df,paste(date_arrival, cdmd_arrival, sep=" "))
cdmd_arrivalT
cdmd_arrival_time<-as.POSIXct(cdmd_arrivalT, format='%m/%d/%y %H:%M')
cdmd_arrival_time

surgery_md_T<-with(df, paste(date_arrival, surgmd_arrival, sep=" "))
surgery_md_T
surgery_md_time<-as.POSIXct(surgery_md_T, format='%m/%d/%y %H:%M')
surgery_md_time

dif_time4.1.1<-difftime(surgery_md_time, cdmd_arrival_time, units=c("min"))
dif_time4.1.1

time_to_surgerymd<-NULL

for(i in 1:nrow(df))
{if(is.na(dif_time4.1.1) [i]==TRUE) {time_to_surgerymd[i]<-NA}
  else if(dif_time4.1.1[i]<0) {time_to_surgerymd[i]<-dif_time4.1.1[i]+1440}
  else{time_to_surgerymd[i]<-dif_time4.1.1[i]}}

time_to_surgerymd

# inserting time to surgery md (hours) into dataset

df$time_to_surgerymd<-time_to_surgerymd/60
summary(df$time_to_surgerymd)

# coding time to TBI surgery (time of cdmd arrival to time of TBI surgery)

cdmd_arrivalT<-with(df,paste(date_arrival, cdmd_arrival, sep=" "))
cdmd_arrivalT
cdmd_arrival_time<-as.POSIXct(cdmd_arrivalT, format='%m/%d/%y %H:%M')
cdmd_arrival_time

tbi_surgery_T<-with(df, paste(date_tbisurg, time_tbisurg, sep=" "))
tbi_surgery_T
tbi_surgery_time<-as.POSIXct(tbi_surgery_T, format='%m/%d/%y %H:%M')
tbi_surgery_time

dif_time4.2.1<-difftime(tbi_surgery_time, cdmd_arrival_time, units=c("min"))
dif_time4.2.1

time_to_tbi_surgery<-NULL

for(i in 1:nrow(df))
{if(is.na(dif_time4.2.1) [i]==TRUE) {time_to_tbi_surgery[i]<-NA}
  else if(dif_time4.2.1[i]<0) {time_to_tbi_surgery[i]<-NA}
  else{time_to_tbi_surgery[i]<-dif_time4.2.1[i]}}

time_to_tbi_surgery

# inserting time to TBI surgery into dataset

df$time_to_tbi_surgery<-time_to_tbi_surgery/60
summary(df$time_to_tbi_surgery)

# coding time to other surgery (time of cdmd arrival to time of other surgery)

cdmd_arrivalT<-with(df,paste(date_arrival, cdmd_arrival, sep=" "))
cdmd_arrivalT
cdmd_arrival_time<-as.POSIXct(cdmd_arrivalT, format='%m/%d/%y %H:%M')
cdmd_arrival_time

other_surgery_T<-with(df, paste(date_othersurg, time_othersurg, sep=" "))
other_surgery_T
other_surgery_time<-as.POSIXct(other_surgery_T, format='%m/%d/%y %H:%M')
other_surgery_time

dif_time4.2.2<-difftime(other_surgery_time, cdmd_arrival_time, units=c("min"))
dif_time4.2.2

time_to_other_surgery<-NULL

for(i in 1:nrow(df))
{if(is.na(dif_time4.2.2) [i]==TRUE) {time_to_other_surgery[i]<-NA}
  else if(dif_time4.2.2[i]<0) {time_to_other_surgery[i]<-NA}
  else{time_to_other_surgery[i]<-dif_time4.2.2[i]}}

time_to_other_surgery

# inserting time to other surgery (hours) into dataset

df$time_to_other_surgery<-time_to_other_surgery/60
summary(df$time_to_other_surgery)

# coding time to ICU (time of cdmd arrival to time of surgery to ICU)

cdmd_arrivalT<-with(df,paste(date_arrival, cdmd_arrival, sep=" "))
cdmd_arrivalT
cdmd_arrival_time<-as.POSIXct(cdmd_arrivalT, format='%m/%d/%y %H:%M')
cdmd_arrival_time

icu_T<-with(df, paste(date_surgtoicu, time_surgtoicu, sep=" "))
icu_T
icu_time<-as.POSIXct(icu_T, format='%m/%d/%y %H:%M')
icu_time

dif_time4.2.3<-difftime(icu_time, cdmd_arrival_time, units=c("min"))
dif_time4.2.3

time_to_icu<-NULL

for(i in 1:nrow(df))
{if(is.na(dif_time4.2.3) [i]==TRUE) {time_to_icu[i]<-NA}
  else if(dif_time4.2.3[i]<0) {time_to_icu[i]<-NA}
  else{time_to_icu[i]<-dif_time4.2.3[i]}}

time_to_icu

# inserting time to icu (hours) into dataset

df$time_to_icu<-time_to_icu/60
summary(df$time_to_icu)

# coding time_to_arrival as categorical

time_to_arrival_cat<-rep(NA, length(df$time_to_arrival))

time_to_arrival_cat[df$time_to_arrival>0 & df$time_to_arrival<=1]<-'<=1h'
time_to_arrival_cat[df$time_to_arrival>1 & df$time_to_arrival<=4]<-'1.1-4h'
time_to_arrival_cat[df$time_to_arrival>4 & df$time_to_arrival<=12]<-'4.1-12h'
time_to_arrival_cat[df$time_to_arrival>12]<-'>12h'

time_to_arrival_cat

df$time_to_arrival_cat<-as.factor(time_to_arrival_cat)
summary(df$time_to_arrival_cat)

# coding time_to_cdmd as categorical

time_to_cdmd_cat<-rep(NA, length(df$time_to_cdmd))

time_to_cdmd_cat[df$time_to_cdmd>0 & df$time_to_cdmd<=1]<-'<=1h'
time_to_cdmd_cat[df$time_to_cdmd>1 & df$time_to_cdmd<=4]<-'1.1-4h'
time_to_cdmd_cat[df$time_to_cdmd>4]<-'>4h'

time_to_cdmd_cat

df$time_to_cdmd_cat<-as.factor(time_to_cdmd_cat)
summary(df$time_to_cdmd_cat)

# coding time_to_labs_sent at cateogrical

time_to_labs_sent_cat<-rep(NA, length(df$time_to_labs_sent))

time_to_labs_sent_cat[df$time_to_labs_sent>0 & df$time_to_labs_sent<=1]<-'<=1h'
time_to_labs_sent_cat[df$time_to_labs_sent>1 & df$time_to_labs_sent<=4]<-'1.1-4h'
time_to_labs_sent_cat[df$time_to_labs_sent>4 & df$time_to_labs_sent<=12]<-'4.1-12h'
time_to_labs_sent_cat[df$time_to_labs_sent>12]<-'>12h'

time_to_labs_sent_cat

df$time_to_labs_sent_cat<-as.factor(time_to_labs_sent_cat)
summary(df$time_to_labs_sent_cat)

# coding time_to_cxr as categorical

time_to_cxr_cat<-rep(NA, length(df$time_to_cxr))

time_to_cxr_cat[df$time_to_cxr>0 & df$time_to_cxr<=1]<-'<=1h'
time_to_cxr_cat[df$time_to_cxr>1 & df$time_to_cxr<=4]<-'1.1-4h'
time_to_cxr_cat[df$time_to_cxr>4]<-'>4h'

time_to_cxr_cat

df$time_to_cxr_cat<-as.factor(time_to_cxr_cat)
summary(df$time_to_cxr_cat)

# coding time_to_sxr as categorical

time_to_sxr_cat<-rep(NA, length(df$time_to_sxr))

time_to_sxr_cat[df$time_to_sxr>0 & df$time_to_sxr<=1]<-'<=1h'
time_to_sxr_cat[df$time_to_sxr>1 & df$time_to_sxr<=4]<-'1.1-4h'
time_to_sxr_cat[df$time_to_sxr>4]<-'>4h'

time_to_cxr_cat

df$time_to_sxr_cat<-as.factor(time_to_sxr_cat)
summary(df$time_to_sxr_cat)

# coding time_to_brain_ct as categorical

time_to_brain_ct_cat_temp<-rep(NA, length(df$time_to_brain_ct))

time_to_brain_ct_cat_temp[df$time_to_brain_ct>0 & df$time_to_brain_ct<=1]<-'<=1h'
time_to_brain_ct_cat_temp[df$time_to_brain_ct>1 & df$time_to_brain_ct<=4]<-'1.1-4h'
time_to_brain_ct_cat_temp[df$time_to_brain_ct>4]<-'>4h'

time_to_brain_ct_cat_temp

time_to_brain_ct_cat_temp<-as.factor(time_to_brain_ct_cat_temp)
summary(time_to_brain_ct_cat_temp)

df$time_to_brain_ct_cat<-NULL

for(i in 1:length(time_to_brain_ct_cat_temp)) {
  if(is.na(time_to_brain_ct_cat_temp[i])==TRUE) {
    if(df$gcs_tot_imp[i]<=13) {
      df$time_to_brain_ct_cat[i]<-"4"}}
  if(is.na(time_to_brain_ct_cat_temp[i])==TRUE) {
    if(df$gcs_tot_imp[i]>13) {
      df$time_to_brain_ct_cat[i]<-"5"}}
  else{df$time_to_brain_ct_cat[i]<-"NA"}
  if(is.na(time_to_brain_ct_cat_temp[i])==FALSE) {
    df$time_to_brain_ct_cat[i]<-time_to_brain_ct_cat_temp[i]}}

df$time_to_brain_ct_cat<-as.factor(df$time_to_brain_ct_cat)
summary(df$time_to_brain_ct_cat)

df$time_to_brain_ct_cat<-car::recode(df$time_to_brain_ct_cat, "1='1h or Less'; 2='>4h'; 3='1.1-4h'; 4='Not_Performed_Needed'; 5='Not_Performed_Not_Needed'")
summary(df$time_to_brain_ct_cat)

# coding time_to_fluids as categorical

time_to_fluids_cat_temp<-rep(NA, length(df$time_to_fluids))

time_to_fluids_cat_temp[df$time_to_fluids>0 & df$time_to_fluids<=1]<-'<=1h'
time_to_fluids_cat_temp[df$time_to_fluids>1 & df$time_to_fluids<=4]<-'1.1-4h'
time_to_fluids_cat_temp[df$time_to_fluids>4]<-'>4h'

time_to_fluids_cat_temp

time_to_fluids_cat_temp<-as.factor(time_to_fluids_cat_temp)
summary(time_to_fluids_cat_temp)

df$time_to_fluids_cat<-NULL

for(i in 1:length(time_to_fluids_cat_temp)) {
  if(is.na(time_to_fluids_cat_temp[i])==TRUE) {
    if(df$sys_bp_imp[i]<100) {
      df$time_to_fluids_cat[i]<-"4"}}
  if(is.na(time_to_fluids_cat_temp[i])==TRUE) {
    if(df$sys_bp_imp[i]>=100) {
      df$time_to_fluids_cat[i]<-"5"}}
  else{df$time_to_fluids_cat[i]<-"NA"}
  if(is.na(time_to_fluids_cat_temp[i])==FALSE) {
    df$time_to_fluids_cat[i]<-time_to_fluids_cat_temp[i]}}

df$time_to_fluids_cat<-as.factor(df$time_to_fluids_cat)
summary(df$time_to_fluids_cat)

df$time_to_fluids_cat<-car::recode(df$time_to_fluids_cat, "1='1h or Less'; 2='>4h'; 3='1.1-4h'; 4='Not_Performed_Needed'; 5='Not_Performed_Not_Needed'")
summary(df$time_to_fluids_cat)

# coding time_to_oxygen as categorical

time_to_oxygen_cat_temp<-rep(NA, length(df$time_to_oxygen))

time_to_oxygen_cat_temp[df$time_to_oxygen>0 & df$time_to_oxygen<=1]<-'<=1h'
time_to_oxygen_cat_temp[df$time_to_oxygen>1]<-'>1h'

time_to_oxygen_cat_temp

time_to_oxygen_cat_temp<-as.factor(time_to_oxygen_cat_temp)
summary(time_to_oxygen_cat_temp)

df$time_to_oxygen_cat<-NULL

for(i in 1:length(time_to_oxygen_cat_temp)) {
  if(is.na(time_to_oxygen_cat_temp[i])==TRUE) {
    if(df$gcs_tot_imp[i]<=8) {
      df$time_to_oxygen_cat[i]<-"3"}
    if(df$pulse_ox_imp[i]<92) {
      df$time_to_oxygen_cat[i]<-"4"}}
  if(is.na(time_to_oxygen_cat_temp[i])==TRUE) {
    if(df$gcs_tot_imp[i]>8 & imputed_data$pulse_ox[i]>=92) {
      df$time_to_oxygen_cat[i]<-"4"}}
  else{df$time_to_oxygen_cat[i]<-"NA"}
  if(is.na(time_to_oxygen_cat_temp[i])==FALSE) {
    df$time_to_oxygen_cat[i]<-time_to_oxygen_cat_temp[i]}}

df$time_to_oxygen_cat<-as.factor(df$time_to_oxygen_cat)
summary(df$time_to_oxygen_cat)

df$time_to_oxygen_cat<-car::recode(df$time_to_oxygen_cat, "1='1h or Less'; 2='>1h'; 3='Not_Performed_Needed'; 4='Not_Performed_Not_Needed'")
summary(df$time_to_oxygen_cat)

# coding time_to_mannitol as categorical

time_to_mannitol_cat<-rep(NA, length(df$time_to_mannitol))

time_to_mannitol_cat[df$time_to_mannitol>0 & time_to_mannitol<=1]<-'<=1h'
time_to_mannitol_cat[df$time_to_mannitol>1]<-'>1h'

time_to_mannitol_cat

df$time_to_mannitol_cat<-as.factor(time_to_mannitol_cat)
summary(df$time_to_mannitol_cat)

# coding time_to_tbi_surgery as categorical

time_to_tbi_surgery_cat<-rep(NA, length(df$time_to_tbi_surgery))

time_to_tbi_surgery_cat[df$time_to_tbi_surgery>0 & df$time_to_tbi_surgery<=4]<-'<=4h'
time_to_tbi_surgery_cat[df$time_to_tbi_surgery>4 & df$time_to_tbi_surgery<=12]<-'4.1-12h'
time_to_tbi_surgery_cat[df$time_to_tbi_surgery>12]<-'>12h'
time_to_tbi_surgery_cat[is.na(df$time_to_tbi_surgery)]<-'Not Performed, Not Needed'

time_to_tbi_surgery_cat

df$time_to_tbi_surgery_cat<-as.factor(time_to_tbi_surgery_cat)
summary(df$time_to_tbi_surgery_cat)

# coding time_to_other_surgery as categorical

time_to_other_surgery_cat<-rep(NA, length(df$time_to_other_surgery))

time_to_other_surgery_cat[df$time_to_other_surgery>0 & df$time_to_other_surgery<=4]<-'<=4h'
time_to_other_surgery_cat[df$time_to_other_surgery>4 & df$time_to_other_surgery<=12]<-'4.1-12h'
time_to_other_surgery_cat[df$time_to_other_surgery>12]<-'>12h'
time_to_other_surgery_cat[is.na(df$time_to_other_surgery)]<-'Not Performed, Not Needed'

time_to_other_surgery_cat

df$time_to_other_surgery_cat<-as.factor(time_to_other_surgery_cat)
summary(df$time_to_other_surgery_cat)

# coding time_to_icu as categorical

time_to_icu_cat<-rep(NA, length(df$time_to_icu))

time_to_icu_cat[df$time_to_icu>0 & df$time_to_icu<=1]<-'<=1h'
time_to_icu_cat[df$time_to_icu>1 & df$time_to_icu<=4]<-'1.1-4h'
time_to_icu_cat[df$time_to_icu>4]<-'>4h'
time_to_icu_cat[is.na(df$time_to_icu)]<-'Not Performed, Not Needed'

time_to_icu_cat

df$time_to_icu_cat<-as.factor(time_to_icu_cat)
summary(df$time_to_icu_cat)

# coding time_to_surgerymd as categorical

time_to_surgerymd_cat<-rep(NA, length(df$time_to_surgerymd))

time_to_surgerymd_cat[df$time_to_surgerymd>0 & df$time_to_surgerymd<=1]<-'<=1h'
time_to_surgerymd_cat[df$time_to_surgerymd>1 & df$time_to_surgerymd<=4]<-'1.1-4h'
time_to_surgerymd_cat[df$time_to_surgerymd>4]<-'>4h'
time_to_surgerymd_cat[is.na(df$time_to_surgerymd)]<-'Not Performed, Not Needed'

time_to_surgerymd_cat

df$time_to_surgerymd_cat<-as.factor(time_to_surgerymd_cat)
summary(df$time_to_surgerymd_cat)

#####################################################################
########## CYRUS PREDICTIVE MODEL CODE STARTS HERE ##################
#####################################################################

##############################################################################################################################
#Data mugging
##############################################################################################################################
#The ideia is identify how the outcome of an admission, can be antecipated from demographics, transport and vital signs information


#tool for summary
tool <- function(x){summary(as.factor(x))}

#tool for compare de summarization of two variables simultaneously
t2 <- function(x,y) {print(tool(x) )
  print(tool(y))}

#Load dataset
#df <- read.csv("E:\\Duke university\\Orientandos _ Duke\\Cyrus\\Base de dados\\tz_TBIregistry_data.csv", stringsAsFactors = FALSE)
#setwd("/Users/armandzimmerman/Desktop/R/Prognostic Model")
#df <- read.csv("tz_TBIregistry_data.csv")

# select variables to be used in this model
col_selected <- c('study_id',
                  'age',
                  'male',
                  'moi',
                  'rti',
                  'assault',
                  'fall_height',
                  'intent',
                  'alcohol',
                  'temp',
                  'resp_rate',
                  'heart_rate',
                  'sys_bp',
                  'dia_bp',
                  'pulse_ox',
                  'glucose',
                  'avpu',
                  'gcs_eye',
                  'gcs_verbal',
                  'gcs_motor',
                  'gcs_tot',
                  'pupil_right_r',
                  'pupil_left_r',
                  'perrla',
                  'cd_dispo',
                  'tbi_surgery',
                  'other_surgery',
                  'surgtoicu',
                  'death',
                  'gos',
                  'gose')

#This columm selection is for cyrus model
col_selected <- c('study_id',
                  'date_arrival',
                  'time_arrival',
                  'date_death',
                  'time_death',
                  'date_dc_home',
                  'age',
                  'male',
                  'moi',
                  'rti',
                  'assault',
                  'fall_height',
                  'intent',
                  'alcohol',
                  'temp',
                  'resp_rate',
                  'heart_rate',
                  'sys_bp',
                  'dia_bp',
                  'pulse_ox',
                  'glucose',
                  'avpu',
                  'gcs_eye',
                  'gcs_verbal',
                  'gcs_motor',
                  'gcs_tot',
                  'pupil_right_r',
                  'pupil_left_r',
                  'perrla',
                  'cd_dispo',
                  'tbi_surgery',
                  'other_surgery',
                  'surgtoicu',
                  'death',
                  'gos',
                  'gose')

# This column selection is for armand Model
col_selected <- c('study_id',
                  'date_arrival',
                  'time_arrival',
                  'date_death',
                  'time_death',
                  'date_dc_home',
                  'age',
                  'male',
                  'moi',
                  'rti',
                  'assault',
                  'fall_height',
                  'intent',
                  'alcohol',
                  'temp',
                  'resp_rate',
                  'heart_rate',
                  'sys_bp',
                  'dia_bp',
                  'pulse_ox',
                  'glucose',
                  'avpu',
                  'gcs_eye',
                  'gcs_verbal',
                  'gcs_motor',
                  'gcs_tot',
                  'pupil_right_r',
                  'pupil_left_r',
                  'perrla',
                  'cd_dispo',
                  'tbi_surgery',
                  'other_surgery',
                  'surgtoicu',
                  'death',
                  'gos',
                  'gose',
                  'time_to_arrival_cat',
                  'time_to_cdmd_cat',
                  'time_to_labs_sent_cat',
                  'time_to_cxr_cat',
                  'time_to_sxr_cat',
                  'time_to_brain_ct_cat',
                  'time_to_fluids_cat',
                  'time_to_oxygen_cat',
                  'time_to_mannitol_cat',
                  'time_to_tbi_surgery_cat',
                  'time_to_other_surgery_cat',
                  'time_to_icu_cat',
                  'time_to_surgerymd_cat')




#select the variables of interest.
#when cyrus asked for some additional variabel I preserved the original df_filtered in the object df_filtered_preserved.
df_filtered <- subset(df,select = col_selected)
rm(col_selected)

#Change the string fall height to numeric, by change each crazy value.

df_filtered$fall_height <- car::recode(df_filtered$fall_height, "'3ft'=1;'1m'=1;
                                       '2m'=2;
                                       '0m'=0;
                                       '1/2m'=0.5;
                                       '11/2m'=11.5;
                                       '40ft'=12;
                                       '5m'=5;
                                       '3M'=3;
                                       '10m'=10;
                                       '6m'=6;
                                       '2.5m'=2.5;
                                       '1.5m'=1.5;
                                       'HIT BY FALLING STONE'=NA;
                                       '0'=0;
                                       '15ft'=5;
                                       'unknown'=NA;
                                       '0.5m'=0.5;
                                       '7m'=7;
                                       'unknown height'=NA;
                                       'fell down from tree length unknown'=NA;
                                       '4.5m'=4.5;
                                       'heigh unknown'=NA;
                                       '1cm'=NA;
                                       '2cm'=NA;
                                       '9m'=9;
                                       '25m'=25;
                                       '6 m'=6;
                                       '4ft'=1.2;
                                       '10'=10;
                                       '15 m'=15;
                                       '15m'=15;
                                       '2.5'=2.5;
                                       '5'=5;
                                       '5.6'=5.6;
                                       '4'=4;
                                       '1.5'=1.5;
                                       '4.5'=4.5;
                                       '4M'=4;
                                       '3m'=3;
                                       '4m'=4;
                                       'UNKNOWN'=NA;
                                       ''=NA;") 
#as.factor.result=FALSE)
##############################################################################################################################
# Explorando os dados
head(df_filtered)
summary(df_filtered)
str(df_filtered)

#Proceed a missing analysis
na_count <-sapply(df_filtered, function(y) sum(length(which(is.na(y))))) # retorna para cada coluna o somatório de NA, muito util para identificar problemas com missing
na_count <- data.frame(na_count)
na_count$percentage <- na_count$na_count/dim(df_filtered)[1]*100
na_count

barplot(na_count$percentage, las = 2, names.arg = rownames(na_count),
        col ="lightblue", main ="Missing level",
        ylab = "missings", xlab="variables")

#Merger two different variables before decide which one to drop (for outcome measure)


#create a missing index, based on the values of multiples variables. 

df_filtered$teste_qual_moi_open <- ifelse(is.na(df_filtered$rti) & is.na(df_filtered$assault) & is.na(df_filtered$fall_height),1,0)
df_filtered$teste_qual_moi_total <- ifelse(is.na(df_filtered$moi) & is.na(df_filtered$rti) & is.na(df_filtered$assault) & is.na(df_filtered$fall_height),1,0)
df_filtered$teste_qual_gos <- ifelse(is.na(df_filtered$gos) & is.na(df_filtered$gose),1,0)
##############################################################################################################################
#Set the parmeters to merge the motive of injury expanded variables
#RTI codes 
#0 Pedestrian - to 0
#1 Motorcycle - to 1
#2 Car - to 2
#assault
#0 Fist/Foot - to 3
#1 Gun - to 4
#2 Knife - to 5
#3 Domestic - to 6
#fall
#if not na - to 7
#car necessary if you have loaded the dplyr package, because it also have a recode function

df_filtered$assault_recoded <- car::recode(df_filtered$assault, "0=3;
                                           1=4;
                                           2=5;
                                           3=6;
                                           else=NA")
df_filtered$fall_recoded <- car::recode(df_filtered$fall_height, "0:25=7;else=NA")

#moi expanded aggregate all motives of injury from the three motive variables. 
#use in the first analysis the original moi varaible, if the model perform poorly use this one. 
df_filtered$moi_complete_expanded <- ifelse(is.na(df_filtered$rti) & is.na(df_filtered$assault_recoded) & is.na(df_filtered$fall_recoded),NA,
                                            ifelse(is.na(df_filtered$assault_recoded) & is.na(df_filtered$fall_recoded),df_filtered$rti,
                                                   ifelse(is.na(df_filtered$rti) & is.na(df_filtered$assault_recoded),df_filtered$fall_recoded,
                                                          ifelse(is.na(df_filtered$rti) & is.na(df_filtered$fall_recoded),df_filtered$assault_recoded,"erro"))))

#aggregate all outcomes associated to gos and gose variables trying to reduce missings                                   
df_filtered$gose_recoded<- car::recode(df_filtered$gose, "1=1;2=2;3:4=3;5:6=4;7:8=5;else=NA")
df_filtered$gos_complete <- ifelse(is.na(df_filtered$gose_recoded) & is.na(df_filtered$gos),NA,
                                   ifelse(is.na(df_filtered$gose_recoded),df_filtered$gos,
                                          ifelse(is.na(df_filtered$gos),df_filtered$gose_recoded,df_filtered$gos)))


##############################################################################################################################
#Change the type of variables accordingly to the codebook.
df_filtered$male <- factor(df_filtered$male, labels = c("Female", "male"))
df_filtered$moi <- factor(df_filtered$moi, labels = c("Road Traffic Injury", "Assault", "Drowning", "Fall", "Other"))
df_filtered$rti <- factor(df_filtered$rti, labels = c("Pedestrian", "Motorcycle", "Car"))
df_filtered$assault <- factor(df_filtered$assault, labels = c("Fist/Foot", "Gun", "Knife", "Domestic"))
df_filtered$intent <- factor(df_filtered$intent, labels = c("Unintentional", "Self-inflicted", "inflicted by other", "Unknown"))
df_filtered$alcohol <- factor(df_filtered$alcohol, labels = c("No", "Yes", "Unknown"))
df_filtered$avpu <- factor(df_filtered$avpu, labels = c("Alert", "Responds to verbal", "Responds to pain", "Unresponsive"))
df_filtered$gcs_eye <- factor(df_filtered$gcs_eye, labels = c("Spontaneous", "To speech", "To pain", "None"))
df_filtered$gcs_verbal <- factor(df_filtered$gcs_verbal, labels = c("Oriented", "Confused", "inappropriate words", "Incomprehensible", "None"))
df_filtered$gcs_motor <- factor(df_filtered$gcs_motor, labels = c("Obeys", "Localizes", "Withdrawn from pain", "Flexion to pain", "Extension to pain", "None"))
df_filtered$pupil_right_r <- factor(df_filtered$pupil_right_r, labels = c("No", "Yes", "Unknown"))
df_filtered$pupil_left_r <- factor(df_filtered$pupil_left_r, labels = c("No", "Yes", "Unknown"))
df_filtered$perrla <- factor(df_filtered$perrla, labels = c("No", "Yes", "Unknown"))
df_filtered$cd_dispo <- factor(df_filtered$cd_dispo, labels = c("ICU", "Surgery", "OT", "Death, morgue", "Home"))
df_filtered$tbi_surgery <- factor(df_filtered$tbi_surgery, labels = c("No", "Yes", "Unknown"))
df_filtered$other_surgery <- factor(df_filtered$other_surgery, labels = c("No", "Yes", "Unknown"))
df_filtered$surgtoicu <- factor(df_filtered$surgtoicu, labels = c("Yes", "No"))
df_filtered$death <- factor(df_filtered$death, labels = c("No", "Yes"))
df_filtered$gos <- factor(df_filtered$gos, labels = c("Death", "Persistent Vegetative", "Severe Disability", "Moderate Disability", "Good recovery"))
df_filtered$gose <- factor(df_filtered$gose, labels = c("Death", "Persistent Vegetative", "Lower severe disability","Upper Severe Disability", "Lower Moderate Disability" , "Upper Moderate Disability","Lower Good recovery", "Upper Good recovery"))
df_filtered$moi_complete_expanded <- factor(df_filtered$moi_complete_expanded, labels = c("Pedestrian", "Motorcycle", "Car", "Assault", "Gun", "Knife", "Domestic"))
df_filtered$gos_complete <- factor(df_filtered$gos_complete, labels = c("Death", "Persistent Vegetative", "Severe Disability", "Moderate Disability", "Good recovery"))

##############################################################################################################################
#Proceed a missing analysis
na_count_filtered <-sapply(df_filtered, function(y) sum(length(which(is.na(y))))) # retorna para cada coluna o somatório de NA, muito util para identificar problemas com missing
na_count_filtered <- data.frame(na_count_filtered)
na_count_filtered$percentage <- na_count_filtered$na_count/dim(df_filtered)[1]*100
na_count_filtered

barplot(na_count_filtered$percentage, las = 2, names.arg = rownames(na_count_filtered),
        col ="lightblue", main ="Missing level",
        ylab = "missings", xlab="variables")

#aqui precisa ser tomada uma decisao excluir os missings ou imputar.
#codigo para excluir os missings 

#high missing are variables with more than 70% of cases missing
#col_low_miss <- row.names(subset(na_count, subset = percentage<60)) 

#retira missing elevados
#df_filtered_low_miss <- subset(df_filtered, select =col_low_miss)

#delete cases without any type of information regarding the motive of injury 
#df_filtered <- subset(df_filtered, subset = teste_qual_moi_total==0)

#remove the data create to missing analysis
#rm(na_count, na_count_filtered, low_missing, col_low_miss, missing_status)

##############################################################################################################################
#código para imputação 

###################################


###### CRIANDO UM BANCO DE DADOS APENAS COM AS VARIAVEIS QUE SERÃO IMPUTADAS #####

imputed_source<-with(df_filtered,data.frame(age,
                                            male,
                                            moi,
                                            intent,
                                            alcohol,
                                            temp,
                                            resp_rate,
                                            heart_rate,
                                            sys_bp,
                                            dia_bp,
                                            pulse_ox,
                                            avpu,
                                            gcs_eye,
                                            gcs_verbal,
                                            gcs_motor,
                                            gcs_tot,
                                            pupil_right_r,
                                            pupil_left_r,
                                            perrla,
                                            cd_dispo,
                                            tbi_surgery,
                                            other_surgery,
                                            surgtoicu,
                                            death,
                                            gos,
                                            gose,
                                            moi_complete_expanded,
                                            gos_complete,
                                            time_to_arrival_cat,
                                            time_to_cdmd_cat,
                                            time_to_labs_sent_cat,
                                            time_to_cxr_cat,
                                            time_to_sxr_cat,
                                            time_to_brain_ct_cat,
                                            time_to_fluids_cat,
                                            time_to_oxygen_cat,
                                            time_to_mannitol_cat,
                                            time_to_tbi_surgery_cat,
                                            time_to_other_surgery_cat,
                                            time_to_icu_cat,
                                            time_to_surgerymd_cat))

#imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03 and 
#http://onlinelibrary.wiley.com/doi/10.1002/sim.4067/full).
#create the method vector for input each column of the imputade data_frame
#different types of variables should be inputed using different techniques. The methods sould be choose based on the folloing guidelines
#continuous variables > linear regression - pmm
#binary data, factor with 2 levels > logistic regression - logreg
#unordered categorical data (factor >= 2 levels) > polytomous regression imputation - polyreg
#ordered categorical data (factor >= 2 levels) > proportional odds model for (ordered, >= 2 levels) - polr

method <- c("pmm", #age
            "logreg", #male
            "polyreg", #moi
            "polyreg", #intent
            "polyreg", #alcohol
            "pmm", #temp
            "pmm", #resp_rate
            "pmm", #heart_rate
            "pmm", #sys_bp
            "pmm", #dia_bp
            "pmm", #pulse_ox
            "polr", #avpu
            "polyreg", #gcs_eye
            "polyreg", #gcs_verbal
            "polyreg", #gcs_motor
            "pmm", #gcs_tot
            "polyreg", #pupil_right_r
            "polyreg", #pupil_left_r
            "polyreg", #perrla
            "polyreg", #cd_dispo
            "polyreg", #tbi_surgery
            "polyreg", #other_surgery
            "logreg", #surgtoicu
            "logreg", #death
            "polr", #gos
            "polr", #gose
            "polyreg", #moi_complete_expanded
            "polr", #gos_complete
            "polr", #time_to_arrival_cat
            "polr", #time_to_cdmd_cat
            "polr", #time_to_labs_sent_cat
            "polr", #time_to_cxr_cat
            "polr", #time_to_sxr_cat
            "polr", #time_to_brain_ct_cat
            "polr", #time_to_fluids_cat
            "polr", #time_to_oxygen_cat
            "polr", #time_to_mannitol_cat
            "polr", #time_to_tbi_surgery_cat
            "polr", #time_to_other_surgery_cat
            "polr", #time_to_icu_cat
            "polr") #time_to_surgerymd_cat


test_imputed <- mice(imputed_source, m=1, method = method, maxit=10, seed=285)
test_imputed <- mice(imputed_source, m=1, method = method, maxit=1, seed=1)

df_filtered <- complete(test_imputed, 1)

##############################################################################################################################
# Select numeric columns to perform correlation analysis
colunas_numericas <- sapply(df_filtered, is.numeric)
colunas_numericas

# Filtrando as colunas numericas para correlacao
data_cor <- subset(df_filtered, select = colunas_numericas)

#Criando um corrplot - to show problems regarding distribution, high correlation and outlierss issues.
pairs.panels(data_cor[,1:8],method = "pearson", hist.col = "#00AFBB", density = TRUE, ellipses = TRUE, lm= TRUE) 


##############################################################################################################################
#box plots for outlier analysis
#prepares de dataset to create multiple boxplots simultaneously. It`s easier to see where cut outliers and works forever. 
data_cor$id <- rep(1:nrow(data_cor))
df.m <- melt(data_cor[,], id.var = "id")

p <- ggplot(data = df.m, aes(x=variable, y=value)) + 
  geom_boxplot()
p + facet_wrap( ~ variable, scales="free")
##############################################################################################################################
#clean outliers variables

df_filtered <- subset(df_filtered, subset = age <=75)
df_filtered <- subset(df_filtered, subset = resp_rate <75)
df_filtered <- subset(df_filtered, subset = sys_bp <=220)

data_cor <- subset(df_filtered, select = colunas_numericas)
data_cor$id <- rep(1:nrow(data_cor))
df.m <- melt(data_cor[,], id.var = "id")

p <- ggplot(data = df.m, aes(x=variable, y=value)) + 
  geom_boxplot()
p + facet_wrap( ~ variable, scales="free")
############################################################################################
# Begin of preparation for predictive modelling
############################################################################################
#Teh dataset df_filtered_g2t is the clean version of the raw data, labeled and ready for the initial modelling effort.
#if something goes wrong just restart using this dataset. 

#first model using gos_Complete and moi_expanded
col_pm_modell <- c('age',
                   'male',
                   #'moi',#usar a expanded primeiro. 
                   'intent',
                   'alcohol',
                   'temp',
                   'resp_rate',
                   'heart_rate',
                   'sys_bp',
                   'dia_bp',
                   'pulse_ox',
                   'gcs_tot',
                   'pupil_right_r',
                   'pupil_left_r',
                   'perrla',
                   'cd_dispo',#possible to remove
                   'tbi_surgery',
                   'other_surgery',
                   'surgtoicu',
                   #'death',#possible to remove
                   #'gose',#possivel desfecho
                   'moi_complete_expanded', #usar no primeiro modelo como preditor
                   'gos_complete',
                   'time_to_arrival_cat',
                   'time_to_cdmd_cat',
                   'time_to_labs_sent_cat',
                   'time_to_cxr_cat',
                   'time_to_sxr_cat',
                   'time_to_brain_ct_cat',
                   'time_to_fluids_cat',
                   'time_to_oxygen_cat',
                   #'time_to_mannitol_cat',
                   'time_to_tbi_surgery_cat',
                   'time_to_other_surgery_cat',
                   'time_to_icu_cat',
                   'time_to_surgerymd_cat')

pm_modell <- subset(df_filtered,select = col_pm_modell)
rm(col_pm_modell)
##############################################################################################################################
# I did the option to use step by step approach instead of use caret preprocessing to make easier identify where may be any problems
#create dummy variables
#head(model.matrix(~., data = pm_modell))#Just exemplify what the dummy process do. 
dummies <- dummyVars(~., data = pm_modell)
#head(predict(dummies, newdata = pm_modell))
pm_modell <-predict(dummies, newdata = pm_modell)
pm_modell <- data.table(pm_modell)

#Near-Zero variance - analysis (there is 18 of 56 variables with near zero variance issue)
nzv <- nearZeroVar(pm_modell, saveMetrics= TRUE)
summary(nzv$nzv)
pm_modell[, rownames(nzv[nzv$nzv,])] <- list(NULL)

##############################################################################################################################
#If necessary can exclude automatically high correlated variables. 

col_antes <- ncol(pm_modell) #number of columns in the dataset

#create correlation matrix
descrCorr <- cor(pm_modell)

#identify highly correlated data - above 0.90
highCorr <- findCorrelation(descrCorr, .90, verbose= T)

# exclude highly correlated data from the dataset
pm_modell <- subset(pm_modell, select = -highCorr)

col_antes-ncol(pm_modell)


##############################################################################################################################
#finding liCorrelation(descrCorr, .90, verbose= T)

# exclude highly correlated data from the dataset
comboInfo <- findLinearCombos(pm_modell)
comboInfo

#remove os combos lineares dos datasets de teste
pm_modell<- subset(pm_modell, select = -comboInfo$remove)

##############################################################################################################################
#Beginning of the predictive modelling - Separate predictor from outcome and/or create data partition
##############################################################################################################################

# Funcao para gerar dados de treino e dados de teste
#splitData <- function(dataframe, seed = NULL) {
#  if (!is.null(seed)) set.seed(seed)
#  index <- 1:nrow(dataframe)
#  trainindex <- sample(index, trunc(length(index)/2))
#  trainset <- dataframe[trainindex, ]
#  testset <- dataframe[-trainindex, ]
#  list(trainset = trainset, testset = testset)
#}

# Create a sample using a randomized approach - preciso entender melhor
#amostra <- sample.split(df$age, SplitRatio = 0.70)
# Criando dados de treino - 70% dos dados
#treino = subset(df, amostra == TRUE)
# Criando dados de teste - 30% dos dados
#teste = subset(df, amostra == FALSE)

# Create an outcome vector

gos_recovery <- pm_modell$`gos_complete.Good recovery`
gos_recovery  <- car::recode(gos_recovery, "0='Bad_recovery';1='Good_recovery'") #avoid the ROC curve problem after using dummy procedures
gos_recovery <- as.factor(gos_recovery)

#create a dataset only with predictors

#predictors_gos <- subset(pm_modell, select = -c(avpu.Alert, gos_complete.Death,`gos_complete.Good recovery`,avpu.Alert, `avpu.Responds to pain`, avpu.Unresponsive))
predictors_gos <- subset(pm_modell, select = -c(gos_complete.Death,`gos_complete.Good recovery`))
#teste sem variáveis de cuidado
#predictors_gos <- subset(pm_modell, select = -c(avpu.Alert, gos_complete.Death,`gos_complete.Good recovery`,avpu.Alert, `avpu.Responds to pain`, avpu.Unresponsive, tbi_surgery.No, tbi_surgery.Yes, other_surgery.No, surgtoicu.No,cd_dispo.Surgery, other_surgery.Yes, alcohol.Unknown))
############################################################################
# Paralell Processing
############################################################################
library(doParallel)
doParallel::registerDoParallel(4) #define number of threads that will be used during analysis process

############################################################################
#definig the boot control
#creating controlling variable
bootControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, allowParallel = TRUE, search = "random",
                            returnData= TRUE,  classProb= TRUE, savePredictions=TRUE, sampling="smote")



##############################################################################################################################
#Models - Regression Algorithms
#Ordinary Least Squares Regression (OLSR)
#Linear Regression
#Logistic Regression
#Stepwise Regression
#Multivariate Adaptive Regression Splines (MARS)
#Locally Estimated Scatterplot Smoothing (LOESS)
##############################################################################################################################


##############################################################################################################################
#Models - Instance-based Algorithms
#k-Nearest Neighbor (kNN)

knngrid <- expand.grid(kmax = (1:5)*6, distance = (1:5)*4, kernel = "optimal")
knngrid 


knn_fit <- train(predictors_gos, gos_recovery,
                 preProcess = c("center", "scale"),
                 method = "kknn", tuneLength = 7,
                 metric = "Kappa",
                 trControl = bootControl,
                 tuneGrid = knngrid)


knn_fit
knn_fit$finalModel
plot(knn_fit, metric = "Kappa")
plot(knn_fit, plotType = "level")


#create the confusion matrix
resul_knn <- knn_fit$pred
cf_matrix_knn <- confusionMatrix(resul_knn$pred, resul_knn$obs, positive = "Good_recovery")
cf_matrix_knn

############################################################################
#compute the var imp
knn_fit.varimp <- varImp(knn_fit)
plot(knn_fit.varimp, main = "Importance of all Variables for 'knn' model")


#Learning Vector Quantization (LVQ)
#Self-Organizing Map (SOM)
#Locally Weighted Learning (LWL)
##############################################################################################################################

##############################################################################################################################
#Models - Regularization Algorithms
#Ridge Regression

ridgeRTgrid <- expand.grid(mtry = max(sqrt(ncol(x),2)), ntree = 100)

ridge_fit <- train(predictors_gos, gos_recovery,
                   preProcess = c("center", "scale"),
                   method = "ORFridge", tuneLength = 7,
                   metric = "Kappa",
                   trControl = bootControl)

ridge_fit
plot(ridge_fit, metric = "Kappa")
ridge_fit$finalModel

#create the confusion matrix
resul_ridge <- ridge_fit$pred
cf_matrix_ridge <- confusionMatrix(resul_ridge$pred, resul_ridge$obs, positive = "Good_recovery")
cf_matrix_ridge

############################################################################
#compute the var imp
ridge_fit.varimp <- varImp(ridge_fit)
plot(ridge_fit.varimp, main = "Importance of all Variables for Ridge Regression model")

#Least Absolute Shrinkage and Selection Operator (LASSO)
#Elastic Net
#Least-Angle Regression (LARS)
##############################################################################################################################

##############################################################################################################################
#Models - Decision Tree Algorithms
#Classification and Regression Tree (CART)
#Iterative Dichotomiser 3 (ID3)
#C4.5 and C5.0 (different versions of a powerful approach)
#Chi-squared Automatic Interaction Detection (CHAID)
#Decision Stump
#M5
#Conditional Decision Trees
#Bagged tree
##############################################################################################################################

bag_fit <- train(predictors_gos, gos_recovery,
                 preProcess = c("center", "scale"),
                 method = "treebag", tuneLength = 1,
                 metric = "Kappa",
                 trControl = bootControl)


bag_fit
bag_fit$finalModel

#create the confusion matrix
resul_bag <- bag_fit$pred
cf_matrix_bag <- confusionMatrix(resul_bag$pred, resul_bag$obs, positive = "Good_recovery")
cf_matrix_bag

############################################################################
#compute the var imp
bag_fit.varimp <- varImp(bag_fit)
plot(bag_fit.varimp, main = "Importance of all Variables for 'bag of tree' model")



##############################################################################################################################
#Models - Bayesian Algorithms
#Naive Bayes
#Gaussian Naive Bayes
#Multinomial Naive Bayes
#Averaged One-Dependence Estimators (AODE)
#Bayesian Belief Network (BBN)
#Bayesian Network (BN)
#Bayesian Generalized Linear Model
bayes_fit <- train(predictors_gos, gos_recovery,
                   preProcess = c("center", "scale"),
                   method = "bayesglm", tuneLength = 1,
                   metric = "Kappa",
                   trControl = bootControl)


bayes_fit
bayes_fit$finalModel

#create the confusion matrix
resul_bayes <- bayes_fit$pred
cf_matrix_bayes <- confusionMatrix(resul_bayes$pred, resul_bayes$obs, positive = "Good_recovery")
cf_matrix_bayes


#compute the var imp
bayes_fit.varimp <- varImp(bayes_fit)
plot(bayes_fit.varimp, main = "Importance of all Variables for Bayesian Generalized Linear Model")
############################################################################


#Bayesian Additive Regression Trees

#bayesRTgrid <- expand.grid(num_trees = (1:5)*20, k= (1:5)*50, alpha=0.95, beta= 2, q=0.9)

bayesRT_fit <- train(predictors_gos, gos_recovery,
                     preProcess = c("center", "scale"),
                     method = "bartMachine", tuneLength = 7,
                     metric = "Kappa",
                     trControl = bootControl)#,
                     #tuneGrid = bayesRTgrid)

plot(bayesRT_fit, metric = "Kappa")
plot(bayesRT_fit, plotType = "level")
bayesRT_fit
bayesRT_fit$finalModel

#create the confusion matrix
resul_bayesNT <- bayesRT_fit$pred
cf_matrix_bayesRT <- confusionMatrix(resul_bayesNT$pred, resul_bayesNT$obs, positive = "Good_recovery")
cf_matrix_bayesRT

############################################################################
#compute the var imp
bayesRT_fit.varimp <- varImp(bayesRT_fit)
plot(bayesRT_fit.varimp, main = "Importance of all Variables for Bayesian Additive Regression Trees model")



##############################################################################################################

##############################################################################################################################
#Models - Clustering Algorithms
#k-Means
#k-Medians
#Expectation Maximisation (EM)
#Hierarchical Clustering
#Single C5.0 Ruleset
c5_fit <- train(predictors_gos, gos_recovery,
                preProcess = c("center", "scale"),
                method = "C5.0Rules", tuneLength = 7,
                metric = "Kappa",
                trControl = bootControl)


c5_fit
c5_fit$finalModel

#create the confusion matrix
resul_c5 <- c5_fit$pred
cf_matrix_c5 <- confusionMatrix(resul_c5$pred, resul_c5$obs, positive = "Good_recovery")
cf_matrix_c5


#compute the var imp
c5_fit.varimp <- varImp(c5_fit)
plot(c5_fit.varimp, main = "Importance of all Variables for Single C5.0 ruleset")

#############################################################################################

##############################################################################################################################
#Models - Association Rule Learning Algorithms
#Apriori algorithm
#Eclat algorithm
#############################################################################################

##############################################################################################################################
#Models - Artificial Neural Network Algorithms
#Neural Network
# here we are controling for size of tree, iterations and learning rate
nngrid <- expand.grid(size = 10, decay=0)

nnFit <- train(predictors_gos, gos_recovery,
               preProcess = c("center", "scale"),
               method = "nnet", tuneLength = 7,
               metric = "Kappa",
               trControl = bootControl,
               tuneGrid = nngrid)
nnFit


resul_nn <- nnFit$pred
cf_matrix_nn <- confusionMatrix(resul_nn$pred, resul_nn$obs, positive = "Good_recovery")
cf_matrix_nn

nnFit.varimp <- varImp(nnFit)
plot(nnFit.varimp, main = "Importance of all Variables for 'Neural Network' model")









#Perceptron
#Back-Propagation
#Hopfield Network
#Radial Basis Function Network (RBFN)
#############################################################################################

##############################################################################################################################
#Models - Deep Learning Algorithms
#Deep Boltzmann Machine (DBM)
#Deep Belief Networks (DBN)
#Convolutional Neural Network (CNN)
#Stacked Auto-Encoders
#############################################################################################

##############################################################################################################################
#Models - Dimensionality Reduction Algorithms
#Principal Component Analysis (PCA)
#Principal Component Regression (PCR)
#Partial Least Squares Regression (PLSR)
#Sammon Mapping
#Multidimensional Scaling (MDS)
#Projection Pursuit
#Linear Discriminant Analysis (LDA)
#Mixture Discriminant Analysis (MDA)
#Quadratic Discriminant Analysis (QDA)
#Flexible Discriminant Analysis (FDA)
###########################################################################################

##############################################################################################################################
#Models - Ensemble Algorithms
#Boosting
#Bootstrapped Aggregation (Bagging)
#AdaBoost
#Stacked Generalization (blending)
#Gradient Boosting Machines (GBM)
############################################################################
# Example with gradient boosting machine
# here we are controling for size of tree, iterations and learning rate
gbmGrid <- expand.grid(.interaction.depth = (1:3) * 2,
                       .n.trees = (1:10)*20, .shrinkage = .1, .n.minobsinnode= 20)
#set.seed(2)
gbmFit <- train(predictors_gos, gos_recovery,
                method = "gbm", trControl = bootControl, verbose = FALSE,
                bag.fraction = 0.5, tuneGrid = gbmGrid)
plot(gbmFit, metric = "Kappa")
plot(gbmFit, plotType = "level")
resampleHist(gbmFit)
gbmFit

resul_gbm <- gbmFit$pred
cf_matrix_gbm <- confusionMatrix(resul_gbm$pred, resul_gbm$obs, positive = "Good_recovery")
cf_matrix_gbm

############################################################################
#compute the var imp
library(gbm)
gbmFit.varimp <- varImp(gbmFit)
plot(gbmFit.varimp, main = "Importance of all Variables for 'gbm' model")




#Gradient Boosted Regression Trees (GBRT)


#Random Forest
###########################################################################################################################
rfFit <- train(predictors_gos, gos_recovery,
               preProcess = c("center", "scale"),
               method = "parRF", tuneLength = 1,
               metric = "Kappa",
               trControl = bootControl)
rfFit
rfFit$finalModel

resul_rf <- rfFit$pred
cf_matrix_rf <- confusionMatrix(resul_rf$pred, resul_rf$obs, positive = "Good_recovery")
cf_matrix_rf

rfFit.varimp <- varImp(rfFit)
plot(rfFit.varimp, main = "Importance of all Variables for 'rf' model")

#Prediction Performance
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################

#Variable importance

#########################################################################################################################
rfFit.varimp <- varImp(rfFit)
bagTFit.varimp <- varImp(bagTFit)
gbmFit.varimp <- varImp(gbmFit)

plot(rfFit.varimp, main = "Importance of all Variables for 'rf' model")
plot(bagTFit.varimp, main = "Importance of all Variables for 'bagged tree' model")
plot(gbmFit.varimp, main = "Importance of all Variables for 'gbm' model")

#########################################################################################################################
# roc curve analysis
#########################################################################################################################

#first the response and after the predictor(probabilities), that is the order of the objects. 
#create the curves for each model.
par(pty="s",mfrow = c(3,3)) #define a frame to plot several graphics simultaneously. 
par(pty="s") #define the plot area with square shape, must be runned before the graphic. 
dev.off() #deactivate the graphical parameters of the plots display area
#knn
knn_roc <- plot.roc(resul_knn$obs, resul_knn$Good_recovery, 
                    main="K nearest neighbor", percent= TRUE, print.auc= TRUE,
                    ci=TRUE, 
                    print.auc.x=ifelse(resul_knn$Good_recovery, 70,70),
                    print.auc.y=ifelse(resul_knn$Good_recovery, 10,10),
                    print.auc.cex = 1)

#ridge
ridge_roc <- plot.roc(resul_ridge$obs, resul_ridge$Good_recovery, 
                      main="Ridge Regression", percent= TRUE, print.auc= TRUE,
                      ci=TRUE, 
                      print.auc.x=ifelse(resul_ridge$Good_recovery, 70,70),
                      print.auc.y=ifelse(resul_ridge$Good_recovery, 10,10),
                      print.auc.cex = 1)


#Bagged tree
bag_roc <- plot.roc(resul_bag$obs, resul_bag$Good_recovery, 
                    main="Bagged tree", percent= TRUE,print.auc= TRUE,
                    ci=TRUE, 
                    print.auc.x=ifelse(resul_bag$Good_recovery, 70,70),
                    print.auc.y=ifelse(resul_bag$Good_recovery, 10,10),
                    print.auc.cex = 1)


#Bayesian Generalized Linear Model 
bayes_roc <- plot.roc(resul_bayes$obs, resul_bayes$Good_recovery, 
                      main="Bayesian Generalized Linear Model", percent= TRUE, print.auc= TRUE,
                      ci=TRUE, 
                      print.auc.x=ifelse(resul_bayes$Good_recovery, 70,70),
                      print.auc.y=ifelse(resul_bayes$Good_recovery, 10,10),
                      print.auc.cex = 1)


#Bayesian Additive Regression Trees 
bayes_nt_roc <- plot.roc(resul_bayesNT$obs, resul_bayesNT$Good_recovery, 
                         main="Bayesian Additive Regression Trees", percent= TRUE,print.auc= TRUE,
                         #ci=TRUE, 
                         print.auc.x=ifelse(resul_bayesNT$Good_recovery, 70,70),
                         print.auc.y=ifelse(resul_bayesNT$Good_recovery, 10,10),
                         print.auc.cex = 1)


#gradient boosting machine 
gbm_roc <- plot.roc(resul_gbm$obs, resul_gbm$Good_recovery, 
                    main="Gradient boosting machine", percent= TRUE,print.auc= TRUE,
                    ci=TRUE, 
                    print.auc.x=ifelse(resul_gbm$Good_recovery, 70,70),
                    print.auc.y=ifelse(resul_gbm$Good_recovery, 10,10),
                    print.auc.cex = 1)


#Random forest
rf_roc <- plot.roc(resul_rf$obs, resul_rf$Good_recovery, 
                   main= "Random forest", percent= TRUE, print.auc= TRUE,
                   ci=TRUE, 
                   print.auc.x=ifelse(resul_rf$Good_recovery, 70,70),
                   print.auc.y=ifelse(resul_rf$Good_recovery, 10,10),
                   print.auc.cex = 1)

#c5
c5_roc <- plot.roc(resul_c5$obs, resul_c5$Good_recovery, 
                   main="Single C5.0 Ruleset", percent= TRUE, print.auc= TRUE,
                   ci=TRUE, 
                   print.auc.x=ifelse(resul_c5$Good_recovery, 70,70),
                   print.auc.y=ifelse(resul_c5$Good_recovery, 10,10),
                   print.auc.cex = 1)

#nn
nn_roc <- plot.roc(resul_nn$obs, resul_nn$Good_recovery, 
                   main="Neural network", percent= TRUE, print.auc= TRUE,
                   ci=TRUE, 
                   print.auc.x=ifelse(resul_nn$Good_recovery, 70,70),
                   print.auc.y=ifelse(resul_nn$Good_recovery, 10,10),
                   print.auc.cex = 1)


#Test de CI for each curve
testeci <- ci.se(teste, boot.n = 25000, parallel = TRUE, # CI of sensitivity
                 
                 specificities=seq(0, 100, 5)) # over a select set of specificities

plot(testeci, type="shape", col="#1c61b6AA") # plot as a blue shape

#### Create the matrix graphics ####

par(pty="s",mfrow = c(3,3))
#### K nearest neighbor ####
plot.roc(resul_knn$obs, resul_knn$Good_recovery, 
         main="K nearest neighbor", percent= TRUE, print.auc= TRUE,
         #ci=TRUE, 
         print.auc.x=ifelse(resul_knn$Good_recovery, 70,70),
         print.auc.y=ifelse(resul_knn$Good_recovery, 10,10),
         # max.auc.polygon=TRUE, max.auc.polygon.col="lightblue1",
         grid.v = 90, grid.h=90, grid.lty="solid", grid.lwd=1, grid.col="black",
         print.auc.cex = 1)
#### Bayesian Generalized Linear Model ####
plot.roc(resul_bayes$obs, resul_bayes$Good_recovery, 
         main="Bayesian Generalized Linear Model", percent= TRUE, print.auc= TRUE,
         #ci=TRUE, 
         print.auc.x=ifelse(resul_bayes$Good_recovery, 70,70),
         print.auc.y=ifelse(resul_bayes$Good_recovery, 10,10),
         # max.auc.polygon=TRUE, max.auc.polygon.col="lightblue1",
         grid.v = 90, grid.h=90, grid.lty="solid", grid.lwd=1, grid.col="black",
         print.auc.cex = 1)
#### Ridge Regression ####
plot.roc(resul_ridge$obs, resul_ridge$Good_recovery, 
         main="Ridge Regression", percent= TRUE, print.auc= TRUE,
         #ci=TRUE, 
         print.auc.x=ifelse(resul_ridge$Good_recovery, 70,70),
         print.auc.y=ifelse(resul_ridge$Good_recovery, 10,10),
         # max.auc.polygon=TRUE, max.auc.polygon.col="lightblue1",
         grid.v = 90, grid.h=90, grid.lty="solid", grid.lwd=1, grid.col="black",
         print.auc.cex = 1)
#### Bagged tree ####
plot.roc(resul_bag$obs, resul_bag$Good_recovery, 
         main="Bagged tree", percent= TRUE,print.auc= TRUE,
         #ci=TRUE, 
         print.auc.x=ifelse(resul_bag$Good_recovery, 70,70),
         print.auc.y=ifelse(resul_bag$Good_recovery, 10,10),
         #max.auc.polygon=TRUE, max.auc.polygon.col="lightblue1",
         grid.v = 90, grid.h=90, grid.lty="solid", grid.lwd=1, grid.col="black",
         print.auc.cex = 1)
#### Gradient boosting machine ####
plot.roc(resul_gbm$obs, resul_gbm$Good_recovery, 
         main="Gradient boosting machine", percent= TRUE,print.auc= TRUE,
         #ci=TRUE, 
         print.auc.x=ifelse(resul_gbm$Good_recovery, 70,70),
         print.auc.y=ifelse(resul_gbm$Good_recovery, 10,10),
         # max.auc.polygon=TRUE, max.auc.polygon.col="lightblue1",
         grid.v = 90, grid.h=90, grid.lty="solid", grid.lwd=1, grid.col="black",
         print.auc.cex = 1)
#### Random forest ####
plot.roc(resul_rf$obs, resul_rf$Good_recovery, 
         main= "Random forest", percent= TRUE, print.auc= TRUE,
         #ci=TRUE, 
         print.auc.x=ifelse(resul_rf$Good_recovery, 70,70),
         print.auc.y=ifelse(resul_rf$Good_recovery, 10,10),
         #max.auc.polygon=TRUE, max.auc.polygon.col="lightblue1",
         grid.v = 90, grid.h=90, grid.lty="solid", grid.lwd=1, grid.col="black",
         print.auc.cex = 1)
#### Neural network ####
plot.roc(resul_nn$obs, resul_nn$Good_recovery, 
         main="Neural network", percent= TRUE, print.auc= TRUE,
         #ci=TRUE, 
         print.auc.x=ifelse(resul_nn$Good_recovery, 70,70),
         print.auc.y=ifelse(resul_nn$Good_recovery, 10,10),
         #max.auc.polygon=TRUE, max.auc.polygon.col="lightblue1",
         grid.v = 90, grid.h=90, grid.lty="solid", grid.lwd=1, grid.col="black",
         print.auc.cex = 1)
#### Single C5.0 Ruleset ####
plot.roc(resul_c5$obs, resul_c5$Good_recovery, 
         main="Single C5.0 Ruleset", percent= TRUE, print.auc= TRUE,
         #ci=TRUE, 
         print.auc.x=ifelse(resul_c5$Good_recovery, 70,70),
         print.auc.y=ifelse(resul_c5$Good_recovery, 10,10),
         grid.v = 90, grid.h=90, grid.lty="solid", grid.lwd=1, grid.col="black",
         print.auc.cex = 1)
#### Bayesian Additive Regression Trees ####
plot.roc(resul_bayesNT$obs, resul_bayesNT$Good_recovery, 
         main="Bayesian Additive Regression Trees", percent= TRUE,print.auc= TRUE,
         #ci=TRUE, 
         print.auc.x=ifelse(resul_bayesNT$Good_recovery, 70,70),
         print.auc.y=ifelse(resul_bayesNT$Good_recovery, 10,10),
         #max.auc.polygon=TRUE, max.auc.polygon.col="lightblue1",
         grid.v = 90, grid.h=90, grid.lty="solid", grid.lwd=1, grid.col="black",
         print.auc.cex = 1)

#### Create all curves in one graphic ####
dev.off()
par(pty="s") #define the plot area with square shape, must be runned before the graphic. 
#create the curves and lines
all_resul_bayes_roc <- plot.roc(resul_bayes$obs, resul_bayes$Good_recovery, 
                                main="Comparison among models", col="black", percent = TRUE,
                                grid.v = 90, grid.h=90, grid.lty="solid", grid.lwd=1, grid.col="black")
all_resul_ridge_roc <- lines.roc(resul_ridge$obs, resul_ridge$Good_recovery,percent = TRUE, col="blue3")
all_resul_rf_roc <- lines.roc(resul_rf$obs, resul_rf$Good_recovery, percent = TRUE,col="brown4")
all_resul_gbm_roc <- lines.roc(resul_gbm$obs, resul_gbm$Good_recovery, percent = TRUE,col="darkgoldenrod2")
all_resul_bayest_roc <- lines.roc(resul_bayesNT$obs, resul_bayesNT$Good_recovery,percent = TRUE,  col="darkmagenta")
all_resul_bag_roc <- lines.roc(resul_bag$obs, resul_bag$Good_recovery, percent = TRUE,col="springgreen4")
all_resul_nn_roc <- lines.roc(resul_nn$obs, resul_nn$Good_recovery,percent = TRUE, col="cyan3")
all_resul_c5_roc <- lines.roc(resul_c5$obs, resul_c5$Good_recovery, percent = TRUE,col="coral2")
all_resul_knn_roc <- lines.roc(resul_knn$obs, resul_knn$Good_recovery, percent = TRUE,col="dimgrey")

#plot the curves and lines

plot.roc(all_resul_bayes_roc, main="Comparison among models", col="black", percent = TRUE,
         grid.v = 90, grid.h=90, grid.lty="solid", grid.lwd=1, grid.col="black")
lines.roc(all_resul_ridge_roc,percent = TRUE, col="blue3")
lines.roc(all_resul_rf_roc, percent = TRUE,col="brown4")
lines.roc(all_resul_gbm_roc, percent = TRUE,col="darkgoldenrod2")
lines.roc(all_resul_bayest_roc,percent = TRUE,  col="darkmagenta")
lines.roc(all_resul_bag_roc, percent = TRUE,col="springgreen4")
lines.roc(all_resul_nn_roc,percent = TRUE, col="cyan3")
lines.roc(all_resul_c5_roc, percent = TRUE,col="coral2")
lines.roc(all_resul_knn_roc, percent = TRUE,col="dimgrey")
legend(legend=c("AUC 87.8% - Bay. Gen. Lin. Mod", "AUC 87.2% - Ridge Regression",
                "AUC 86.9% - Random forest","AUC 86.5% - Gradient boosting machine",
                "AUC 86.1% - Bay. Add. Reg. Tr.", "AUC 85.2% - Bagged tree","AUC 80.7% - Neural network",
                "AUC 80.9% - Single C5.0 Ruleset","AUC 69.9% - K nearest neighbors"),
       col=c("black", "blue3","brown4","darkgoldenrod2","darkmagenta","springgreen4","cyan3","coral2","dimgrey"),
       lwd=2, cex = .65, x= 63, y=38, border.col = "white")

#function to smoothing not to fantastic
smth <- function(x,y){
  lines(smooth(x, method="binormal"), col=y)
}

######table do create list of CI by method ######

methods_used <- c("K nearest neighbor",
                  "Ridge Regression",
                  "Bagged tree",
                  "Bayesian Generalized Linear Model",
                  #"Bayesian Additive Regression Trees",
                  "Gradient boosting machine",
                  "Random forest",
                  "Single C5.0 Ruleset",
                  "Neural network")

methods_used_auc <- round(c(knn_roc$auc,
                      ridge_roc$auc,
                      bag_roc$auc,
                      bayes_roc$auc,
                      #bayes_nt_roc$auc,
                      gbm_roc$auc,
                      rf_roc$auc,
                      c5_roc$auc,
                      nn_roc$auc), 2)

methods_used_ci_inferior <- round(c(knn_roc$ci[1],
                              ridge_roc$ci[1],
                              bag_roc$ci[1],
                              bayes_roc$ci[1],
                              #bayes_nt_roc$ci[1],
                              gbm_roc$ci[1],
                              rf_roc$ci[1],
                              c5_roc$ci[1],
                              nn_roc$ci[1]), 2)

methods_used_ci_superior <- round(c(knn_roc$ci[3],
                              ridge_roc$ci[3],
                              bag_roc$ci[3],
                              bayes_roc$ci[3],
                              #bayes_nt_roc$ci[3],
                              gbm_roc$ci[3],
                              rf_roc$ci[3],
                              c5_roc$ci[3],
                              nn_roc$ci[3]), 2)

model_comparison <- data.frame(methods_used,methods_used_auc,methods_used_ci_inferior,methods_used_ci_superior)
write.csv(model_comparison, file = "model_comparison.csv")

#Creates a CI for a ROC object. 
#teste <- ci.auc(c5_roc, method = "bootstrap" , boot.n = 20000, reuse.auc = TRUE, parallel = 5)


##############################################################
#Partial area under the curve

data(aSAH)

plot.roc(aSAH$outcome, aSAH$s100b, # data
         
         percent=TRUE, # show all values in percent
         
         partial.auc=c(100, 90), partial.auc.correct=TRUE, # define a partial AUC (pAUC)
         
         print.auc=TRUE, #display pAUC value on the plot with following options:
         
         print.auc.pattern="Corrected pAUC (100-90%% SP):\n%.1f%%", print.auc.col="#1c61b6",
         
         auc.polygon=TRUE, auc.polygon.col="#1c61b6", # show pAUC as a polygon
         
         max.auc.polygon=TRUE, max.auc.polygon.col="lightblue1", # also show the 100% polygon
         
         main="Partial AUC (pAUC)")

plot.roc(aSAH$outcome, aSAH$s100b,
         
         percent=TRUE, add=TRUE, type="n", # add to plot, but don't re-add the ROC itself (useless)
         
         partial.auc=c(100, 90), partial.auc.correct=TRUE,
         
         partial.auc.focus="se", # focus pAUC on the sensitivity
         
         print.auc=TRUE, print.auc.pattern="Corrected pAUC (100-90%% SE):\n%.1f%%", print.auc.col="#008600",
         
         print.auc.y=40, # do not print auc over the previous one
         
         auc.polygon=TRUE, auc.polygon.col="#008600",
         
         max.auc.polygon=TRUE, max.auc.polygon.col="#00860022")

#########################################################################################################################
#confidence intervals
rocobj <- plot.roc(aSAH$outcome, aSAH$s100b,
                   
                   main="Confidence intervals", percent=TRUE,
                   
                   ci=TRUE, # compute AUC (of AUC by default)
                   
                   print.auc=TRUE,
                   print.auc.x=ifelse(aSAH$s100b, 100),
                   print.auc.y=ifelse(aSAH$s100b, 100))


ciobj <- ci.se(rocobj, # CI of sensitivity
               
               specificities=seq(0, 100, 5)) # over a select set of specificities

plot(ciobj, type="shape", col="#1c61b6AA") # plot as a blue shape

plot(ci(rocobj, of="thresholds", thresholds="best")) # add one threshold


#########################################################################################################################
#Smoothing

rocobj <- plot.roc(aSAH$outcome, aSAH$s100b, percent = TRUE, main="Smoothing")

lines(smooth(rocobj), # smoothing (default: binormal)
      
      col = "#1c61b6")

lines(smooth(rocobj, method = "density"), # density smoothing
      
      col = "#008600")

lines(smooth(rocobj, method = "fitdistr", # fit a distribution
             
             density = "lognormal"), # let the distribution be log-normal
      
      col = "#840000")

legend("bottomright", legend = c("Empirical", "Binormal", "Density", "Fitdistr\n(Log-normal)"), col = c("black", "#1c61b6", "#008600", "#840000"),lwd = 2)

#########################################################################################################################
#Confidence intervals of specificity/sensitivity
rocobj <- plot.roc(aSAH$outcome, aSAH$s100b,
                   
                   main="Confidence intervals of specificity/sensitivity", percent=TRUE,
                   
                   ci=TRUE, of="se", # ci of sensitivity
                   
                   specificities=seq(0, 100, 5), # on a select set of specificities
                   
                   ci.type="shape", ci.col="#1c61b6AA") # plot the CI as a blue shape

plot(ci.sp(rocobj, sensitivities=seq(0, 100, 5)), # ci of specificity
     
     type="bars") # print this one as bars
#########################################################################################################################
#Confidence interval of a threshold
plot.roc(aSAH$outcome, aSAH$s100b,
         
         main="Confidence interval of a threshold", percent=TRUE,
         
         ci=TRUE, of="thresholds", # compute AUC (of threshold)
         
         thresholds="best", # select the (best) threshold
         
         print.thres="best") # also highlight this threshold on the plot

#########################################################################################################################
#Statistical comparison

rocobj1 <- plot.roc(aSAH$outcome, aSAH$s100,
                    
                    main="Statistical comparison", percent=TRUE, col="#1c61b6")

rocobj2 <- lines.roc(aSAH$outcome, aSAH$ndka, percent=TRUE, col="#008600")

testobj <- roc.test(rocobj1, rocobj2)

text(50, 50, labels=paste("p-value =", format.pval(testobj$p.value)), adj=c(0, .5))

legend("bottomright", legend=c("S100B", "NDKA"), col=c("#1c61b6", "#008600"), lwd=2)

###########simulation test - surgery############### 
#creating the dataset for surgery and no surgery
#the outcome is: gos_recovery
#the orginal data source is: predictors_gos
#create the dataset considering all patients submitted to surgery or not
#the best model was bayesian generalized linear model: bayes_fit

save(predictors_gos, file = "Orginal_dataset_no_simulation.Rdata")

simulation_surgery <- predictors_gos
simulation_surgery$tbi_surgery.No <- 0
simulation_surgery$tbi_surgery.Yes <- 1

simulation_no_surgery <- predictors_gos
simulation_no_surgery$tbi_surgery.No <- 1
simulation_no_surgery$tbi_surgery.Yes <- 0

result_surgery<- predict(bayes_fit, newdata = simulation_surgery, type = "prob")
result_no_surgery<- predict(bayes_fit, newdata = simulation_no_surgery, type = "prob")

save(result_surgery, file = "outcome_prob_surgery.Rdata")
save(result_no_surgery, file = "outcome_prob_no_surgery.Rdata")

save(simulation_surgery, file = "data_surgery.Rdata")
save(simulation_no_surgery , file = "data_no_surgery.Rdata")
###########simulation test - surgery + icu ############### 
simulation_surgery_icu <- simulation_surgery
simulation_surgery_no_icu <- simulation_surgery

simulation_surgery_icu$surgtoicu.No <- 1
simulation_surgery_no_icu$surgtoicu.No <- 0


simulation_no_surgery_icu <- simulation_no_surgery
simulation_no_surgery_no_icu <- simulation_no_surgery

simulation_no_surgery_icu$surgtoicu.No <- 1
simulation_no_surgery_no_icu$surgtoicu.No <- 0


result_simulation_surgery_icu<- predict(bayes_fit, newdata = simulation_surgery_icu, type = "prob")
result_simulation_surgery_no_icu<- predict(bayes_fit, newdata = simulation_surgery_no_icu, type = "prob")
result_simulation_no_surgery_icu<- predict(bayes_fit, newdata = simulation_no_surgery_icu, type = "prob")
result_simulation_no_surgery_no_icu<- predict(bayes_fit, newdata = simulation_no_surgery_no_icu, type = "prob")


save(simulation_surgery_icu, file = "data_simulation_surgery_icu.Rdata")
save(simulation_surgery_no_icu, file = "data_simulation_surgery_no_icu.Rdata")
save(simulation_no_surgery_icu, file = "data_simulation_no_surgery_icu.Rdata")
save(simulation_no_surgery_no_icu, file = "data_simulation_no_surgery_no_icu.Rdata")



save(result_simulation_surgery_icu, file = "outcome_simulation_surgery_icu.Rdata")
save(result_simulation_surgery_no_icu, file = "outcome_simulation_surgery_no_icu.Rdata")
save(result_simulation_no_surgery_icu, file = "outcome_simulation_no_surgery_icu.Rdata")
save(result_simulation_no_surgery_no_icu, file = "outcome_simulation_no_surgery_no_icu.Rdata")


###########plotting varimp of the best model############### 
library(ggplot2)
best_model_varimp <- bayes_fit.varimp$importance
best_model_varimp$Bad_recovery <- NULL
best_model_varimp$legend <- c("Age","Female","Int. unintentional","No-alcohol","Yes-alcohol",
                              "Alcohol uknown","Temperature","Respiratory rate","Heart rate","Systolic blood pressure",
                              "Diastolic blood pressure","Pulse oximetry","GCS score","Pupils equals","Casual. Dept. disp. surg.",
                              "No-tbi surgery","Yes-tbi surgery","No-other surgery","Yes-other surgery",
                              "Surgery to ICU","Motorcycle-injury","Domestic-injury","Car-injury","Knife-injury",
                              "Fist/Foot-injury","Gun-injury")
#sorting the colums of importance.
best_model_varimp <- best_model_varimp[order(-best_model_varimp$Good_recovery),c(1,2)]
order_graph <- rev(best_model_varimp$legend)

graph<- ggplot(data=best_model_varimp, aes(x=legend, y=Good_recovery)) +
  geom_bar(stat="identity", fill="darkblue")

graph + coord_flip()
graph + coord_flip()+ scale_x_discrete(name="Predictors", limits=order_graph)+ 
  scale_y_discrete(limits=c(0,25,50,75,100), name="Variable Importance")+
  theme_classic()











