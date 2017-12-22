

######################################################
#suicide_anxiety.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. see full license at the end of this file.
######################################################
#this script follows a combination of the guidelines proposed by Hadley Wickham http://goo.gl/c04kq as well as using the formatR package http://goo.gl/ri6ky
#if this is the first time you are conducting an analysis using this protocol, please watch http://goo.gl/DajIN while following step by step

#link to manuscript 

 #The general plan is to compare the fibrinogen and platelet curves of RS vs Copperhead snakes.  The times points are Baseline, nadir during hospitalization, day 5, day 8, day 15.  There is some missing mess.   I am hoping we can get it done in time for an abstract deadline soon.  Let me know what is best.

######################################################
#SETTING ENVIRONMENT
######################################################
#install.packages("VIM")
#install.packages("VIMGUI")
#install.packages("miP") # nao foi instalada. Disse que nao estava disponivel para R 3.4.2
#install.packages("gWidgetsRGtk2")
#install.packages("mi")
#install.packages("epicalc") # nao foi instalada. Disse que nao estava disponivel para R 3.4.2
#install.packages("sem")
#install.packages("ggplot2")
#install.packages("psych")
#install.packages("RCurl")
#install.packages("irr")
#install.packages("nortest")
#install.packages("moments")
#install.packages("GPArotation")
#install.packages("nFactors")
#install.packages("boot")
#install.packages("psy")
#install.packages("car")
#install.packages("vcd")
#install.packages("gridExtra")
#install.packages("mi")
#install.packages("VIM")
#install.packages("epicalc") # nao disponivel
#install.packages("gdata")
#install.packages("sqldf")
#install.packages("reshape2")
#install.packages("mclust")
#install.packages("foreign")
#install.packages("survival")
#install.packages("memisc")
#install.packages("lme4") 
#install.packages("lmerTest")
#install.packages("dplyr")
#install.packages("QCA")
#install.packages("VennDiagram")
#install.packages("qgraph")
#install.packages("igraph")
#install.packages("ltm")
#install.packages("gmodels")
#install.packages("eRm")
#install.packages("mirt")
#install.packages("devtools")
#install.packages("reshape")
#install.packages("mice")
#install.packages("bnpa") #nao consegui instalar...
#install.packages("mice")
#install.packages("Hmisc")
#Load packages neededz for the analysis
#All packages must be installes with install.packages() function
# lapply(c("sem","ggplot2","psych","RCurl","irr","nortest","moments",
#          "GPArotation","nFactors","boot","psy","car","vcd","gridExtra",
#          "mi","VIM","gdata","sqldf","VIM","VIMGUI","gWidgetsRGtk2","mi",
#          "reshape2","mclust","foreign","survival","memisc","lme4","lmerTest",
#          "dplyr","QCA","VennDiagram","qgraph", "igraph", "ltm","gmodels",
#          "eRm","mirt","dplyr","devtools","reshape","mice","Hmisc"),library, character.only=T)

lapply(c("mice"),library, character.only=T)

#Package and codes to pull data from goodle sheets
#devtools::install_github("jennybc/googlesheets")
#library(googlesheets)
#suppressMessages(library(dplyr))

######################################################
#IMPORTING DATA
######################################################

data<-read.csv("/Users/Joao/Box Sync/Home Folder jnv4/Data/Global EM/Africa/Tz/tbi_registry/tz_TBIregistry_data.csv")

######################################################
#DATA MANAGEMENT
######################################################

#data_prehospital<-subset(data,data$prehospital_course_complete==2)

names(data)


##############################################################################
# ERIKA TENTANDO... -------------------------------------------------------

######### PERIODO 1 #########
#Time to reach hospital (da injúria até chegar no hospital)
injuryT<-with(data,paste(inj_date, inj_time, sep=" "))
injury_time <- as.POSIXct(injuryT,
                          format='%m/%d/%y %H:%M')

arrivalT<-with(data,paste(date_arrival,time_arrival, sep=" "))
arrival_time <- as.POSIXct(arrivalT,
                           format='%m/%d/%y %H:%M')

dif_time<-difftime(injury_time, arrival_time,
                   units = c("min"))*-1

time_to_care<-NULL

for (i in 1:nrow(data))
{
  if (is.na(dif_time)[i] == TRUE)
    
  { 
    
    time_to_care[i] <- NA
    
  } else if (dif_time[i] < 0) {
    
    time_to_care[i] <- NA
    
  } else {
    
    time_to_care[i] <- dif_time[i]
    
  }
} # for (i in 1:nrow(dataframe)


#time_to_care[1]<-(7*60)+40
#time_to_care[292]<-(2*60)+5
#time_to_care[952]<-40
#time_to_care[311]<-60
#time_to_care[312]<-(1*60)+30
#time_to_care[314]<-45
#time_to_care[317]<-60
#time_to_care[317]<-120
#time_to_care[318]<-(3*60)
#time_to_care[316]<-(6*60)
#time_to_care[825]<-(15*60)+30
#time_to_care[315]<-(17+24+12)*60
#time_to_care[1053]<-60

time_to_care<-time_to_care/60
time_to_care
summary(time_to_care) # EM HORAS #

# quantile(dif_time, probs=seq(0,1,0.10),na.rm=TRUE)

# errors<-car::recode(dif_time,"-524400:0='weird';
# 							 0.0001:10000='normal';
# 							 10000:1057080='weird'")

# error_log<-data.frame(errors,dif_time,id=data$study_id)

# error_log_weird<-subset(error_log,error_log$errors=='weird')

# write.csv(error_log_weird,"/Users/jnv4/Desktop/error.csv")


# boxplot(data$time_to_care~data$outcome) 
# 
######### PERIODO 2 #########
#Time to CD-MD at hospital (entre chegar no hospital e ser atendido pelo CDMD)

arrivalT<-with(data,paste(date_arrival,time_arrival, sep=" "))
arrival_time <- as.POSIXct(arrivalT,
                           format='%m/%d/%y %H:%M')

cdmd_T<-with(data,paste(date_arrival,cdmd_arrival, sep=" "))
cdmd_time<-as.POSIXct(cdmd_T,
                        format='%m/%d/%y %H:%M')

dif_time2<-difftime(arrival_time, cdmd_time,
                    units = c("min"))*-1

time_to_care_cdmd<-NULL

for (i in 1:nrow(data))
{
  if (is.na(dif_time2)[i] == TRUE)
    
  { 
    
    time_to_care_cdmd[i] <- NA
    
  } else if (dif_time2[i] < 0) {
    
    time_to_care_cdmd[i] <- dif_time2[i]+1440
    
  } else {
    
    time_to_care_cdmd[i] <- dif_time2[i]
    
  }
} # for (i in 1:nrow(dataframe)

time_to_care_cdmd<-time_to_care_cdmd/60

time_to_care_cdmd

summary(time_to_care_cdmd) # EM HORAS #
data$time_to_care_cdmd<-time_to_care_cdmd

###################################
###################################
###### CRIANDO UM BANCO DE DADOS APENAS COM AS VARIAVEIS QUE SERÃO IMPUTADAS #####
#analysis data set
summary(data$male)
summary(data$moi)
summary(data$alcohol)
summary(data$gcs_tot)
summary(data$gos)
summary(data$time_to_care_cdmd)
summary(data$resp_rate)
summary(data$sys_bp)
summary(data$pulse_ox)
summary(data$heart_rate)

imputed_data<-with(data,data.frame(age,
                                   male,
                                   moi,
                                   alcohol,
                                   gcs_tot,
                                   gos,
                                   time_to_care_cdmd,
                                   resp_rate,
                                   sys_bp,
                                   pulse_ox,
                                   heart_rate))
str(imputed_data)                                     
#recoding gos
imputed_data$gos_cat<-as.factor(car::recode(
  imputed_data$gos,"1:4='Death';
  5='Alive'"))
summary(imputed_data$gos_cat)

#recoding gcs
imputed_data$gcs_cat<-as.factor(car::recode(
  imputed_data$gcs_tot,"1:12='Severe';
  13:15='Non-severe'"))
summary(imputed_data$gcs_cat)

#recoding alcohol
imputed_data$alcohol<-as.factor(car::recode(
  imputed_data$alcohol,"0='No';
  1='Yes';
  2='No'"))
summary(imputed_data$alcohol)

#recoding moi
imputed_data$moi<-as.factor(car::recode(
  imputed_data$moi,"0='Road Traffic Injury';1='Assault';NA=NA;else='other'"))
summary(imputed_data$moi)


imputed_data$male<-as.factor(imputed_data$male)


#Imputation
#imputed_data<-na.omit(imputed_data)


# # argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03 and 
#http://onlinelibrary.wiley.com/doi/10.1002/sim.4067/full). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
imp <- mice(imputed_data, seed = 2222, m=5)

sample(1:5,1)

# # reports the complete dataset with missing imputated. It returns 5 options of datasets, 
#witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
imputed_data<-mice::complete(imp,4)

imputed_data

summary(data$age)
summary(data$male)
summary(data$moi)
summary(data$gcs_tot)
summary(data$gos)
summary(data$resp_rate)
summary(data$sys_bp)
summary(data$pulse_ox)
summary(data$heart_rate)

data$age_imp<-imputed_data$age
data$male_imp<-imputed_data$male
data$moi_imp<-imputed_data$moi
data$alcohol_imp<-imputed_data$alcohol
data$gcs_tot_imp<-imputed_data$gcs_tot
data$gos_imp<-imputed_data$gos
data$resp_rate_imp<-imputed_data$resp_rate
data$sys_bp_imp<-imputed_data$sys_bp
data$pulse_ox_imp<-imputed_data$pulse_ox
data$heart_rate_imp<-imputed_data$heart_rate


#analysis_data$time_to_care_cat<-car::recode(analysis_data$time_to_care,"
#                                            0:1='0-1hrs';
#                                            1.01:2='1-2hrs';
#                                            2.01:3='2-3hrs';
#                                            3.000001:4='3-4hrs';
#                                            else='more 4hrs'")
###################################
###################################


######### PERIODO 3.1.1 #########
#Time to CD-MD at hospital (entre ser atendido pelo CDMD e fazer Rx de tórax)

cdmd_T<-with(data,paste(date_arrival,cdmd_arrival, sep=" "))
cdmd_time <- as.POSIXct(cdmd_T,
                        format='%m/%d/%y %H:%M')

rxt_T<-with(data,paste(date_arrival,cxr_time, sep=" "))
rxt_time <- as.POSIXct(rxt_T,
                        format='%m/%d/%y %H:%M')

dif_time3.1.1<-difftime(cdmd_time, rxt_time,
                    units = c("min"))*-1

time_to_care_rxt<-NULL

for (i in 1:nrow(data))
{
  if (is.na(dif_time3.1.1)[i] == TRUE)
    
  { 
    
    time_to_care_rxt[i] <- NA
    
  } else if (dif_time3.1.1[i] < 0) {
    
    time_to_care_rxt[i] <- dif_time2[i]+1440

  } else {
    
    time_to_care_rxt[i] <- dif_time3.1.1[i]
    
  }
} # for (i in 1:nrow(dataframe)

time_to_care_rxt

time_to_care_rxt<-time_to_care_rxt/60

time_to_care_rxt
summary(time_to_care_rxt) # EM HORAS #


######### TEMPO 3.1.2 #########
#Time to CD-MD at hospital (entre ser atendido pelo CDMD e fazer Rx do cranio - rxc)

cdmd_T<-with(data,paste(date_arrival,cdmd_arrival, sep=" "))
cdmd_time <- as.POSIXct(cdmd_T,
                        format='%m/%d/%y %H:%M')

rxc_T<-with(data,paste(date_arrival,skullxr_time, sep=" "))
rxc_time <- as.POSIXct(rxc_T,
                       format='%m/%d/%y %H:%M')

dif_time3.1.2<-difftime(cdmd_time, rxc_time,
                        units = c("min"))*-1

time_to_care_rxc<-NULL

for (i in 1:nrow(data))
{
  if (is.na(dif_time3.1.2)[i] == TRUE)
    
  { 
    
    time_to_care_rxc[i] <- NA
    
  } else if (dif_time3.1.2[i] < 0) {
    
    time_to_care_rxc[i] <- dif_time3.1.2[i] + 1440
    
  } else {
    
    time_to_care_rxc[i] <- dif_time3.1.2[i]
    
  }
} # for (i in 1:nrow(dataframe)

time_to_care_rxc

time_to_care_rxc<-time_to_care_rxc/60

time_to_care_rxc

summary(time_to_care_rxc) # EM HORAS #


######### TEMPO 3.1.3 #########
#Time to CD-MD at hospital (entre ser atendido pelo CDMD e fazer exame laboratorial- labor)

cdmd_T<-with(data,paste(date_arrival,cdmd_arrival, sep=" "))
cdmd_time <- as.POSIXct(cdmd_T,
                        format='%m/%d/%y %H:%M')

labor_T<-with(data,paste(date_arrival,labs_time, sep=" "))
labor_time <- as.POSIXct(labor_T,
                       format='%m/%d/%y %H:%M')

dif_time3.1.3<-difftime(cdmd_time, labor_time,
                        units = c("min"))*-1

time_to_care_labor<-NULL

for (i in 1:nrow(data))
{
  if (is.na(dif_time3.1.3)[i] == TRUE)
    
  { 
    
    time_to_care_labor[i] <- NA
    
  } else if (dif_time3.1.3[i] < 0) {
    
    time_to_care_labor[i] <- dif_time3.1.3[i] + 1440
    
     } else {
    
    time_to_care_labor[i] <- dif_time3.1.3[i]
    
  }
} # for (i in 1:nrow(dataframe)

time_to_care_labor

time_to_care_labor<-time_to_care_labor/60

time_to_care_labor
summary(time_to_care_labor) # EM HORAS #


######### PERIODO 3.1.4 #########
#Time to CD-MD at hospital (entre ser atendido pelo CDMD e fazer CT de cerebro)

cdmd_T<-with(data,paste(date_arrival,cdmd_arrival, sep=" "))
cdmd_time <- as.POSIXct(cdmd_T,
                        format='%m/%d/%y %H:%M')

ct_T<-with(data,paste(ctbrain_day,ctbrain_time, sep=" "))
ct_time <- as.POSIXct(ct_T,
                       format='%m/%d/%y %H:%M')

dif_time3.1.4<-difftime(cdmd_time, ct_time,
                        units = c("min"))*-1

time_to_care_ct<-NULL

for (i in 1:nrow(data))
{
  if (is.na(dif_time3.1.4)[i] == TRUE)
    
  { 
    
    time_to_care_ct[i] <- NA
    
  } else if (dif_time3.1.4[i] < 0) {
    
    time_to_care_ct[i] <- NA
    
  } else {
    
    time_to_care_ct[i] <- dif_time3.1.4[i]
    
  }
} # for (i in 1:nrow(dataframe)

time_to_care_ct

#dados a serem modificados
#time_to_care_ct[]<-(*60)+0

time_to_care_ct<-time_to_care_ct/60

time_to_care_ct

summary(time_to_care_ct) # EM HORAS #
table(data$ct_brain)
length(data$ct_brain)



######### PERIODO 3.2.1 #########
#Time to CD-MD at hospital (entre ser atendido pelo CDMD e receber fluidos)

cdmd_T<-with(data,paste(date_arrival,cdmd_arrival, sep=" "))
cdmd_time <- as.POSIXct(cdmd_T,
                        format='%m/%d/%y %H:%M')

fluid_T<-with(data,paste(date_arrival,fluids_time, sep=" "))
fluid_time <- as.POSIXct(fluid_T,
                      format='%m/%d/%y %H:%M')

dif_time3.2.1<-difftime(cdmd_time, fluid_time,
                        units = c("min"))*-1

time_to_care_fluid<-NULL

for (i in 1:nrow(data))
{
  if (is.na(dif_time3.2.1)[i] == TRUE)
    
  { 
    
    time_to_care_fluid[i] <- NA
    
  } else if (dif_time3.2.1[i] < 0) {
    
    time_to_care_fluid[i] <- dif_time3.2.1[i] + 1440
    
  } else {
    
    time_to_care_fluid[i] <- dif_time3.2.1[i]
    
  }
} # for (i in 1:nrow(dataframe)

time_to_care_fluid

#dados a serem modificados
#time_to_care_ct[]<-(*60)+0

time_to_care_fluid<-time_to_care_fluid/60

time_to_care_fluid

summary(time_to_care_fluid) # EM HORAS #
#table(data$ct_brain)
#length(data$ct_brain)




######### PERIODO 3.2.2 #########
#Time to CD-MD at hospital (entre ser atendido pelo CDMD e receber oxigenio)

cdmd_T<-with(data,paste(date_arrival,cdmd_arrival, sep=" "))
cdmd_time <- as.POSIXct(cdmd_T,
                        format='%m/%d/%y %H:%M')

oxyg_T<-with(data,paste(date_arrival,oxygen_time, sep=" "))
oxyg_time <- as.POSIXct(oxyg_T,
                         format='%m/%d/%y %H:%M')

dif_time3.2.2<-difftime(cdmd_time, oxyg_time,
                        units = c("min"))*-1

time_to_care_oxyg<-NULL

for (i in 1:nrow(data))
{
  if (is.na(dif_time3.2.2)[i] == TRUE)
    
  { 
    
    time_to_care_oxyg[i] <- NA
    
  } else if (dif_time3.2.2[i] < 0) {
    
    time_to_care_oxyg[i] <- dif_time3.2.2[i] + 1440
    
  } else {
    
    time_to_care_oxyg[i] <- dif_time3.2.2[i]
    
  }
} # for (i in 1:nrow(dataframe)

time_to_care_oxyg

#dados a serem modificados
#time_to_care_ct[]<-(*60)+0

time_to_care_oxyg<-time_to_care_oxyg/60

time_to_care_oxyg

summary(time_to_care_oxyg) # EM HORAS #
#table(data$ct_brain)
#length(data$ct_brain)



######### PERIODO 3.2.3 #########
#Time to CD-MD at hospital (entre ser atendido pelo CDMD e receber MANNITOL)

cdmd_T<-with(data,paste(date_arrival,cdmd_arrival, sep=" "))
cdmd_time <- as.POSIXct(cdmd_T,
                        format='%m/%d/%y %H:%M')

mannit_T<-with(data,paste(date_arrival,mannitol_time, sep=" "))
mannit_time <- as.POSIXct(mannit_T,
                        format='%m/%d/%y %H:%M')

dif_time3.2.3<-difftime(cdmd_time, mannit_time,
                        units = c("min"))*-1

time_to_care_mannit<-NULL

for (i in 1:nrow(data))
{
  if (is.na(dif_time3.2.3)[i] == TRUE)
    
  { 
    
    time_to_care_mannit[i] <- NA
    
  } else if (dif_time3.2.3[i] < 0) {
    
    time_to_care_mannit[i] <- dif_time3.2.3[i] + 1440
    
  } else {
    
    time_to_care_mannit[i] <- dif_time3.2.3[i]
    
  }
} # for (i in 1:nrow(dataframe)

time_to_care_mannit

time_to_care_mannit<-time_to_care_mannit/60

time_to_care_mannit

summary(time_to_care_mannit) # EM HORAS #
#table(data$ct_brain)
#length(data$ct_brain)




######### PERIODO 4.1 #########
#Time to CD-MD at hospital (entre ser atendido pelo CDMD e ser atendido pelo cirurgiao)

cdmd_T<-with(data,paste(date_arrival,cdmd_arrival, sep=" "))
cdmd_time <- as.POSIXct(cdmd_T,
                        format='%m/%d/%y %H:%M')

surgmd_T<-with(data,paste(date_arrival,surgmd_arrival, sep=" "))
surgmd_time <- as.POSIXct(surgmd_T,
                          format='%m/%d/%y %H:%M')

dif_time4.1<-difftime(cdmd_time, surgmd_time,
                        units = c("min"))*-1

time_to_care_surgmd<-NULL

for (i in 1:nrow(data))
{
  if (is.na(dif_time4.1)[i] == TRUE)
    
  { 
    
    time_to_care_surgmd[i] <- NA
    
  } else if (dif_time4.1[i] < 0) {
    
    time_to_care_surgmd[i] <- dif_time4.1[i] + 1440
    
  } else {
    
    time_to_care_surgmd[i] <- dif_time4.1[i]
    
  }
} # for (i in 1:nrow(dataframe)

time_to_care_surgmd

time_to_care_surgmd<-time_to_care_surgmd/60

time_to_care_surgmd

summary(time_to_care_surgmd) # EM HORAS #
#table(data$ct_brain)
#length(data$ct_brain)



######### PERIODO 4.2.1 #########
#Time to CD-MD at hospital (entre ser atendido pelo CDMD e fazer a cirurgia- tbisurg)

cdmd_T<-with(data,paste(date_arrival,cdmd_arrival, sep=" "))
cdmd_time <- as.POSIXct(cdmd_T,
                        format='%m/%d/%y %H:%M')
tbis_T<-with(data,paste(date_tbisurg,time_tbisurg, sep=" "))
tbis_time <- as.POSIXct(tbis_T,
                          format='%m/%d/%y %H:%M')

dif_time4.2.1<-difftime(cdmd_time, tbis_time,
                      units = c("min"))*-1

time_to_care_tbis<-NULL

for (i in 1:nrow(data))
{
  if (is.na(dif_time4.2.1)[i] == TRUE)
    
  { 
    
    time_to_care_tbis[i] <- NA
    
  } else if (dif_time4.2.1[i] < 0) {
    
    time_to_care_tbis[i] <- NA
    
  } else {
    
    time_to_care_tbis[i] <- dif_time4.2.1[i]
    
  }
} # for (i in 1:nrow(dataframe)

time_to_care_tbis

#dados a serem modificados
#time_to_care_ct[]<-(*60)+0


time_to_care_tbis<-time_to_care_tbis/60

subset(data, time_to_care,
       time_to_care_cdmd,
       time_to_care_rxt,
       time_to_care_labor,
       time_to_care_ct,
       time_to_care_fluid,
       time_to_care_oxyg,
       time_to_care_tbis,
       outcome)

summary(time_to_care_tbis) # EM HORAS #
#table(data$ct_brain)
#length(data$ct_brain)



######### PERIODO 4.2.2 #########
#Time to CD-MD at hospital (entre ser atendido pelo CDMD e fazer a cirurgia- othersurg)

cdmd_T<-with(data,paste(date_arrival,cdmd_arrival, sep=" "))
cdmd_time <- as.POSIXct(cdmd_T,
                        format='%m/%d/%y %H:%M')
others_T<-with(data,paste(date_othersurg,time_othersurg, sep=" "))
others_time <- as.POSIXct(others_T,
                        format='%m/%d/%y %H:%M')

dif_time4.2.2<-difftime(cdmd_time, others_time,
                        units = c("min"))*-1

time_to_care_others<-NULL

for (i in 1:nrow(data))
{
  if (is.na(dif_time4.2.2)[i] == TRUE)
    
  { 
    
    time_to_care_others[i] <- NA
    
  } else if (dif_time4.2.2[i] < 0) {
    
    time_to_care_others[i] <- NA
    
  } else {
    
    time_to_care_others[i] <- dif_time4.2.2[i]
    
  }
} # for (i in 1:nrow(dataframe)

time_to_care_others

#dados a serem modificados
#time_to_care_ct[]<-(*60)+0

time_to_care_others<-time_to_care_others/60

time_to_care_others

summary(time_to_care_others) # EM HORAS #



######### PERIODO 4.2.3 #########
#Time to CD-MD at hospital (entre ser atendido pelo CDMD e fazer a cirurgia- surgtoicu)

cdmd_T<-with(data,paste(date_arrival,cdmd_arrival, sep=" "))
cdmd_time <- as.POSIXct(cdmd_T,
                        format='%m/%d/%y %H:%M')
surgtoicu_T<-with(data,paste(date_surgtoicu,time_surgtoicu, sep=" "))
surgtoicu_time <- as.POSIXct(surgtoicu_T,
                          format='%m/%d/%y %H:%M')

dif_time4.2.3<-difftime(cdmd_time, surgtoicu_time,
                        units = c("min"))*-1

time_to_care_surgtoicu<-NULL

for (i in 1:nrow(data))
{
  if (is.na(dif_time4.2.3)[i] == TRUE)
    
  { 
    
    time_to_care_surgtoicu[i] <- NA
    
  } else if (dif_time4.2.3[i] < 0) {
    
    time_to_care_surgtoicu[i] <- NA
    
  } else {
    
    time_to_care_surgtoicu[i] <- dif_time4.2.3[i]
    
  }
} # for (i in 1:nrow(dataframe)

time_to_care_surgtoicu

time_to_care_surgtoicu<-time_to_care_surgtoicu/60

time_to_care_surgtoicu

summary(time_to_care_surgtoicu) # EM HORAS #


#tryout<-car::recode(time_to_care,"0:24='yes';else='no'")
#table(tryout)


######### DEATH #########

death<-NULL

for (i in 1:nrow(data))
{
  if (is.na(data$death)[i] == TRUE)
    
  { 
    
    death[i] <- NA
    
  } else if (data$death[i] == 0) {
    
    death[i] <- 0
    
  } else {
    
    death[i] <- data$death[i]
    
  }
} # for (i in 1:nrow(dataframe)

death <- factor(death)


data$death2<-car::recode(data$death,"
                   0='vivo';
                   1='morto';
                   NA=NA")
data$death2 <- factor(data$death2)

names(data)

table(data$death2)
#summary(death2)
#prop.table(table(death2))

####
######### male #########

data$male2<-car::recode(data$male,"
                   0='female';
                   1='male';
                   NA=NA")
data$male2 <- factor(data$male2)
names(data)

table(data$male2)
summary(data$male2)
prop.table(table(male2))

######### moi #########
data$moi2<-car::recode(data$moi,"0='Road Traffic Injury';
                       1='Assault';2='Other';3='Fall';4='Other';NA=NA")
data$moi2 <- factor(data$moi2)

table(data$moi2)
summary(data$moi)
prop.table(table(data$moi))

######### gcs #########
gcs_tot<-rep(NA,length(data$gcs_tot))
summary(gcs_tot)
sd(gcs_tot,na.rm=T)
gcs_tot[0<time_to_care&time_to_care<=8]<-'severe'
gcs_tot[8<time_to_care&time_to_care<=12]<-'moderate'
gcs_tot[12<time_to_care&time_to_care<=15]<-'mild'
gcs_tot <- factor(gcs_tot)
gcs_tot
table(gcs_tot)
data$gcs<-car::recode(data$gcs_tot,"
                      0:8='severe'; 
                      9:12='moderate'; 
                      13:15='mild';
                      NA=NA")
data$gcs <- factor(data$gcs)
names(data)
table(data$gcs)
summary(data$gcs)

list(data$gcs)

##### GOS e GOSE - OUTCOME ####
table(data$gos)
table(data$gose)

data$gos_recoded<-car::recode(data$gos,"1:3='bad';4:5='agood'")
data$gose_recoded<-car::recode(data$gose,"1:4='bad';5:8='agood'")
data$outcome<-NULL
for(i in 1:length(data$gos)){
  if(is.na(data$gos_recoded[i])==TRUE){
    data$outcome[i]<-data$gose_recoded[i]
  } else {
    data$outcome[i]<-data$gos_recoded[i]
  }
}


str(data$gos_recoded)
str(data$gose_recoded)
summary(data$gos)
summary(as.factor(data$gos_recoded))
summary(as.factor(data$gose_recoded))
is.na(data$gos_recoded)==TRUE

data$outcome <- factor(data$outcome)
table(data$outcome)
summary(data$outcome)

names(data)
###########
time_to_care
time_to_care_cdmd
time_to_care_rxt
time_to_care_labor
time_to_care_ct
time_to_care_fluid
time_to_care_oxyg
time_to_care_tbis
#time_to_care_rxc
#time_to_care_mannit
#time_to_care_surgmd

###categorizando as variáveis###

ttc<-rep(NA,length(time_to_care))
summary(time_to_care)
sd(time_to_care,na.rm=T)
ttc[0<time_to_care&time_to_care<=1]<-'<=1h'
ttc[1<time_to_care&time_to_care<=2]<-'1.1-2h'
ttc[2<time_to_care&time_to_care<=3]<-'2.1-3h'
ttc[3<time_to_care&time_to_care<=4]<-'3.1-4h'
ttc[4<time_to_care&time_to_care<=5]<-'4.1-5h'
ttc[5<time_to_care&time_to_care<=6]<-'5.1-6h'
ttc[6<time_to_care&time_to_care<=7]<-'6.1-7h'
ttc[7<time_to_care&time_to_care<=8]<-'7.1-8h'
ttc[8<time_to_care&time_to_care<=9]<-'8.1-9h'
ttc[9<time_to_care&time_to_care<=10]<-'9.1-10h'
ttc[10<time_to_care&time_to_care<=11]<-'10.1-11h'
ttc[11<time_to_care&time_to_care<=12]<-'11.1-12h'
ttc[12<time_to_care&time_to_care<=13]<-'12.1-13h'
ttc[13<time_to_care&time_to_care<=14]<-'13.1-14h'
ttc[14<time_to_care&time_to_care<=15]<-'14.1-15h'
ttc[15<time_to_care&time_to_care<=16]<-'15.1-16h'
ttc[16<time_to_care&time_to_care<=17]<-'16.1-17h'
ttc[17<time_to_care&time_to_care<=18]<-'17.1-18h'
ttc[18<time_to_care&time_to_care<=19]<-'18.1-19h'
ttc[19<time_to_care&time_to_care<=20]<-'19.1-20h'
ttc[20<time_to_care&time_to_care<=21]<-'20.1-21h'
ttc[21<time_to_care&time_to_care<=22]<-'21.1-22h'
ttc[22<time_to_care&time_to_care<=23]<-'22.1-23h'
ttc[23<time_to_care&time_to_care<=24]<-'23.1-24h'
ttc[24<time_to_care&time_to_care<=48]<-'24.1-48h'
ttc[48<time_to_care&time_to_care<=72]<-'48.1-72h'
ttc[72<time_to_care&time_to_care<=168]<-'72.1-168h'
ttc[168<time_to_care]<-'>168h'

ttc <- factor(ttc2)

prop.table(table(ttc))
table(ttc)

ttc2<-rep(NA,length(time_to_care))
summary(time_to_care)
sd(time_to_care,na.rm=T)
ttc2[0<time_to_care&time_to_care<=6]<-'<=6h'
ttc2[6<time_to_care&time_to_care<=12]<-'6.1 - 12h'
ttc2[12<time_to_care&time_to_care<=24]<-'12.1 - 24h'
ttc2[24<time_to_care&time_to_care<=48]<-'24.1 - 48h'
ttc2[48<time_to_care]<-'>48h'

ttc2 <- factor(ttc2)

prop.table(table(ttc2))
table(ttc2)
table(ttc2,death)
prop.table(table(ttc2,death))

ttc2<-rep(NA,length(time_to_care))
summary(time_to_care)
sd(time_to_care,na.rm=T)
ttc2[0<time_to_care&time_to_care<=1]<-'<=1h'
ttc2[1<time_to_care&time_to_care<=4]<-'1.1 - 4h'
ttc2[4<time_to_care&time_to_care<=6]<-'4.1 - 6h'
ttc2[6<time_to_care&time_to_care<=12]<-'6.1 - 12h'
ttc2[12<time_to_care&time_to_care<=24]<-'12.1 - 24h'
ttc2[24<time_to_care&time_to_care<=48]<-'24.1 - 48h'
ttc2[48<time_to_care]<-'>48h'
ttc2[NA==time_to_care]<-'nao'

ttc2 <- factor(ttc2)
data$ttc2<-ttc2
names(data)

prop.table(table(ttc2))
table(ttc2)
table(ttc2,death)
prop.table(table(ttc2,death))

ttc2<-rep(NA,length(time_to_care))
summary(time_to_care)
sd(time_to_care,na.rm=T)
ttc2[0<time_to_care&time_to_care<=1]<-'<=1h'
ttc2[1<time_to_care&time_to_care<=4]<-'1.1 - 4h'
ttc2[4<time_to_care&time_to_care<=12]<-'4.1 - 12h'
ttc2[12<time_to_care]<-'>12h'
ttc2[NA==time_to_care]<-'nao'

ttc2 <- factor(ttc2)

prop.table(table(ttc2))
table(ttc2)
table(death)
table(ttc2,death)
prop.table(table(ttc2,death))

data$ttc2<-ttc2
names(data)
table(data$ttc2)
summary(data$ttc2)


logmodel_gcs<-glm(outcome ~ ttc2,family=binomial, data=data)
summary(logmodel_gcs)
#anova(reglogGEU)
#exp(coef(model1_death)) # exponentiated coefficients
#exp(confint(model1_death)) # 95% CI for exponentiated coefficients
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 


###
ttc_cdmd<-rep(NA,length(time_to_care_cdmd))
summary(time_to_care_cdmd)
sd(time_to_care_cdmd,na.rm=T)
ttc_cdmd[0<time_to_care_cdmd&time_to_care_cdmd<=1]<-'<=1h'
ttc_cdmd[1<time_to_care_cdmd&time_to_care_cdmd<=2]<-'1.1-2h'
ttc_cdmd[2<time_to_care_cdmd&time_to_care_cdmd<=3]<-'2.1-3h'
ttc_cdmd[3<time_to_care_cdmd&time_to_care_cdmd<=4]<-'3.1-4h'
ttc_cdmd[4<time_to_care_cdmd&time_to_care_cdmd<=5]<-'4.1-5h'
ttc_cdmd[5<time_to_care_cdmd&time_to_care_cdmd<=6]<-'5.1-6h'
ttc_cdmd[6<time_to_care_cdmd&time_to_care_cdmd<=7]<-'6.1-7h'
ttc_cdmd[7<time_to_care_cdmd&time_to_care_cdmd<=8]<-'7.1-8h'
ttc_cdmd[8<time_to_care_cdmd&time_to_care_cdmd<=9]<-'8.1-9h'
ttc_cdmd[9<time_to_care_cdmd&time_to_care_cdmd<=10]<-'9.1-10h'
ttc_cdmd[10<time_to_care_cdmd&time_to_care_cdmd<=11]<-'10.1-11h'
ttc_cdmd[11<time_to_care_cdmd&time_to_care_cdmd<=12]<-'11.1-12h'
ttc_cdmd[12<time_to_care_cdmd&time_to_care_cdmd<=13]<-'12.1-13h'
ttc_cdmd[13<time_to_care_cdmd&time_to_care_cdmd<=14]<-'13.1-14h'
ttc_cdmd[14<time_to_care_cdmd&time_to_care_cdmd<=15]<-'14.1-15h'
ttc_cdmd[15<time_to_care_cdmd&time_to_care_cdmd<=16]<-'15.1-16h'
ttc_cdmd[16<time_to_care_cdmd&time_to_care_cdmd<=17]<-'16.1-17h'
ttc_cdmd[17<time_to_care_cdmd&time_to_care_cdmd<=18]<-'17.1-18h'
ttc_cdmd[18<time_to_care_cdmd&time_to_care_cdmd<=19]<-'18.1-19h'
ttc_cdmd[19<time_to_care_cdmd&time_to_care_cdmd<=20]<-'19.1-20h'
ttc_cdmd[20<time_to_care_cdmd&time_to_care_cdmd<=21]<-'20.1-21h'
ttc_cdmd[21<time_to_care_cdmd&time_to_care_cdmd<=22]<-'21.1-22h'
ttc_cdmd[22<time_to_care_cdmd&time_to_care_cdmd<=23]<-'22.1-23h'
ttc_cdmd[23<time_to_care_cdmd&time_to_care_cdmd<=24]<-'23.1-24h'
ttc_cdmd[24<time_to_care_cdmd&time_to_care_cdmd<=48]<-'24.1-48h'
ttc_cdmd[48<time_to_care_cdmd&time_to_care_cdmd<=72]<-'48.1-72h'
ttc_cdmd[72<time_to_care_cdmd&time_to_care_cdmd<=168]<-'72.1-168h'
ttc_cdmd[168<time_to_care_cdmd]<-'>168h'

ttc_cdmd <- factor(ttc_cdmd2)

prop.table(table(ttc_cdmd))
table(ttc_cdmd)

ttc_cdmd2<-rep(NA,length(time_to_care_cdmd))
summary(time_to_care_cdmd)
sd(time_to_care_cdmd,na.rm=T)
ttc_cdmd2[0<time_to_care_cdmd&time_to_care_cdmd<=6]<-'<=6h'
ttc_cdmd2[6<time_to_care_cdmd&time_to_care_cdmd<=12]<-'6.1 - 12h'
ttc_cdmd2[12<time_to_care_cdmd&time_to_care_cdmd<=24]<-'12.1 - 24h'
ttc_cdmd2[24<time_to_care_cdmd&time_to_care_cdmd<=48]<-'24.1 - 48h'
ttc_cdmd2[48<time_to_care_cdmd]<-'>48h'

ttc_cdmd2 <- factor(ttc_cdmd2)

prop.table(table(ttc_cdmd2))
table(ttc_cdmd2)

ttc_cdmd2<-rep(NA,length(time_to_care_cdmd))
summary(time_to_care_cdmd)
sd(time_to_care_cdmd,na.rm=T)
ttc_cdmd2[0<time_to_care_cdmd&time_to_care_cdmd<=1]<-'<=1h'
ttc_cdmd2[1<time_to_care_cdmd&time_to_care_cdmd<=2]<-'1.1 - 2h'
ttc_cdmd2[2<time_to_care_cdmd&time_to_care_cdmd<=3]<-'2.1 - 3h'
ttc_cdmd2[3<time_to_care_cdmd&time_to_care_cdmd<=4]<-'3.1 - 4h'
ttc_cdmd2[4<time_to_care_cdmd]<-'>4h'

ttc_cdmd2 <- factor(ttc_cdmd2)

prop.table(table(ttc_cdmd2))
table(ttc_cdmd2)

ttc_cdmd2<-rep(NA,length(time_to_care_cdmd))
summary(time_to_care_cdmd)
sd(time_to_care_cdmd,na.rm=T)
ttc_cdmd2[0<time_to_care_cdmd&time_to_care_cdmd<=1]<-'<=1h'
ttc_cdmd2[1<time_to_care_cdmd&time_to_care_cdmd<=4]<-'1.1 - 4h'
ttc_cdmd2[4<time_to_care_cdmd]<-'>4h'

ttc_cdmd2 <- factor(ttc_cdmd2)
data$ttc_cdmd2<-ttc_cdmd2

prop.table(table(ttc_cdmd2))
table(data$ttc_cdmd2)
summary(data$ttc_cdmd2)

logmodel_gcs<-glm(outcome ~ ttc_cdmd2,family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 


###

time_to_care_labor
ttc_labor<-rep(NA,length(time_to_care_labor))
summary(time_to_care_labor)
sd(time_to_care_labor,na.rm=T)
ttc_labor[0<time_to_care_labor&time_to_care_labor<=1]<-'<=1h'
ttc_labor[1<time_to_care_labor&time_to_care_labor<=2]<-'1.1-2h'
ttc_labor[2<time_to_care_labor&time_to_care_labor<=3]<-'2.1-3h'
ttc_labor[3<time_to_care_labor&time_to_care_labor<=4]<-'3.1-4h'
ttc_labor[4<time_to_care_labor&time_to_care_labor<=5]<-'4.1-5h'
ttc_labor[5<time_to_care_labor&time_to_care_labor<=6]<-'5.1-6h'
ttc_labor[6<time_to_care_labor&time_to_care_labor<=7]<-'6.1-7h'
ttc_labor[7<time_to_care_labor&time_to_care_labor<=8]<-'7.1-8h'
ttc_labor[8<time_to_care_labor&time_to_care_labor<=9]<-'8.1-9h'
ttc_labor[9<time_to_care_labor&time_to_care_labor<=10]<-'9.1-10h'
ttc_labor[10<time_to_care_labor&time_to_care_labor<=11]<-'10.1-11h'
ttc_labor[11<time_to_care_labor&time_to_care_labor<=12]<-'11.1-12h'
ttc_labor[12<time_to_care_labor&time_to_care_labor<=13]<-'12.1-13h'
ttc_labor[13<time_to_care_labor&time_to_care_labor<=14]<-'13.1-14h'
ttc_labor[14<time_to_care_labor&time_to_care_labor<=15]<-'14.1-15h'
ttc_labor[15<time_to_care_labor&time_to_care_labor<=16]<-'15.1-16h'
ttc_labor[16<time_to_care_labor&time_to_care_labor<=17]<-'16.1-17h'
ttc_labor[17<time_to_care_labor&time_to_care_labor<=18]<-'17.1-18h'
ttc_labor[18<time_to_care_labor&time_to_care_labor<=19]<-'18.1-19h'
ttc_labor[19<time_to_care_labor&time_to_care_labor<=20]<-'19.1-20h'
ttc_labor[20<time_to_care_labor&time_to_care_labor<=21]<-'20.1-21h'
ttc_labor[21<time_to_care_labor&time_to_care_labor<=22]<-'21.1-22h'
ttc_labor[22<time_to_care_labor&time_to_care_labor<=23]<-'22.1-23h'
ttc_labor[23<time_to_care_labor&time_to_care_labor<=24]<-'23.1-24h'
ttc_labor[24<time_to_care_labor&time_to_care_labor<=48]<-'24.1-48h'
ttc_labor[48<time_to_care_labor&time_to_care_labor<=72]<-'48.1-72h'
ttc_labor[72<time_to_care_labor&time_to_care_labor<=168]<-'72.1-168h'
ttc_labor[168<time_to_care_labor]<-'>168h'

ttc_labor <- factor(ttc_labor2)

prop.table(table(ttc_labor))
table(ttc_labor)
summary(time_to_care_labor)

ttc_labor2<-rep(NA,length(time_to_care_labor))
summary(time_to_care_labor)
sd(time_to_care_labor,na.rm=T)
ttc_labor2[0<time_to_care_labor&time_to_care_labor<=1]<-'<=1h'
ttc_labor2[1<time_to_care_labor&time_to_care_labor<=4]<-'1.1 - 4h'
ttc_labor2[4<time_to_care_labor&time_to_care_labor<=6]<-'4.1 - 6h'
ttc_labor2[6<time_to_care_labor&time_to_care_labor<=12]<-'6.1 - 12h'
ttc_labor2[12<time_to_care_labor&time_to_care_labor<=24]<-'12.1 - 24h'
ttc_labor2[24<time_to_care_labor&time_to_care_labor<=48]<-'24.1 - 48h'
ttc_labor2[48<time_to_care_labor]<-'>48h'

ttc_labor2 <- factor(ttc_labor2)

prop.table(table(ttc_labor2))
table(ttc_labor2)

ttc_labor2<-rep(NA,length(time_to_care_labor))
summary(time_to_care_labor)
sd(time_to_care_labor,na.rm=T)
ttc_labor2[0<time_to_care_labor&time_to_care_labor<=1]<-'<=1h'
ttc_labor2[1<time_to_care_labor&time_to_care_labor<=4]<-'1.1 - 4h'
ttc_labor2[4<time_to_care_labor&time_to_care_labor<=12]<-'4.1 - 12h'
ttc_labor2[12<time_to_care_labor]<-'>12h'

ttc_labor2 <- factor(ttc_labor2)
data$ttc_labor2<-ttc_labor2

prop.table(table(ttc_labor2))
table(ttc_labor2)
table(data$ttc_labor2)
summary(data$ttc_labor2)

logmodel_gcs<-glm(outcome ~ ttc_labor2,family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 


###
ttc_rxt<-rep(NA,length(time_to_care_rxt)) #torax
summary(time_to_care_rxt)
sd(time_to_care_rxt,na.rm=T)
ttc_rxt[0<time_to_care_rxt&time_to_care_rxt<=1]<-'<=1h'
ttc_rxt[1<time_to_care_rxt&time_to_care_rxt<=2]<-'1.1-2h'
ttc_rxt[2<time_to_care_rxt&time_to_care_rxt<=3]<-'2.1-3h'
ttc_rxt[3<time_to_care_rxt&time_to_care_rxt<=4]<-'3.1-4h'
ttc_rxt[4<time_to_care_rxt&time_to_care_rxt<=5]<-'4.1-5h'
ttc_rxt[5<time_to_care_rxt&time_to_care_rxt<=6]<-'5.1-6h'
ttc_rxt[6<time_to_care_rxt&time_to_care_rxt<=7]<-'6.1-7h'
ttc_rxt[7<time_to_care_rxt&time_to_care_rxt<=8]<-'7.1-8h'
ttc_rxt[8<time_to_care_rxt&time_to_care_rxt<=9]<-'8.1-9h'
ttc_rxt[9<time_to_care_rxt&time_to_care_rxt<=10]<-'9.1-10h'
ttc_rxt[10<time_to_care_rxt&time_to_care_rxt<=11]<-'10.1-11h'
ttc_rxt[11<time_to_care_rxt&time_to_care_rxt<=12]<-'11.1-12h'
ttc_rxt[12<time_to_care_rxt&time_to_care_rxt<=13]<-'12.1-13h'
ttc_rxt[13<time_to_care_rxt&time_to_care_rxt<=14]<-'13.1-14h'
ttc_rxt[14<time_to_care_rxt&time_to_care_rxt<=15]<-'14.1-15h'
ttc_rxt[15<time_to_care_rxt&time_to_care_rxt<=16]<-'15.1-16h'
ttc_rxt[16<time_to_care_rxt&time_to_care_rxt<=17]<-'16.1-17h'
ttc_rxt[17<time_to_care_rxt&time_to_care_rxt<=18]<-'17.1-18h'
ttc_rxt[18<time_to_care_rxt&time_to_care_rxt<=19]<-'18.1-19h'
ttc_rxt[19<time_to_care_rxt&time_to_care_rxt<=20]<-'19.1-20h'
ttc_rxt[20<time_to_care_rxt&time_to_care_rxt<=21]<-'20.1-21h'
ttc_rxt[21<time_to_care_rxt&time_to_care_rxt<=22]<-'21.1-22h'
ttc_rxt[22<time_to_care_rxt&time_to_care_rxt<=23]<-'22.1-23h'
ttc_rxt[23<time_to_care_rxt&time_to_care_rxt<=24]<-'23.1-24h'
ttc_rxt[24<time_to_care_rxt&time_to_care_rxt<=48]<-'24.1-48h'
ttc_rxt[48<time_to_care_rxt&time_to_care_rxt<=72]<-'48.1-72h'
ttc_rxt[72<time_to_care_rxt&time_to_care_rxt<=168]<-'72.1-168h'
ttc_rxt[168<time_to_care_rxt]<-'>168h'

ttc_rxt <- factor(ttc_rxt2)

prop.table(table(ttc_rxt))
table(ttc_rxt)

ttc_rxt2<-rep(NA,length(time_to_care_rxt))
summary(time_to_care_rxt)
sd(time_to_care_rxt,na.rm=T)
ttc_rxt2[0<time_to_care_rxt&time_to_care_rxt<=1]<-'<=1h'
ttc_rxt2[1<time_to_care_rxt&time_to_care_rxt<=4]<-'1.1 - 4h'
ttc_rxt2[4<time_to_care_rxt&time_to_care_rxt<=6]<-'4.1 - 6h'
ttc_rxt2[6<time_to_care_rxt&time_to_care_rxt<=12]<-'6.1 - 12h'
ttc_rxt2[12<time_to_care_rxt&time_to_care_rxt<=24]<-'12.1 - 24h'
ttc_rxt2[24<time_to_care_rxt&time_to_care_rxt<=48]<-'24.1 - 48h'
ttc_rxt2[48<time_to_care_rxt]<-'>48h'

ttc_rxt2 <- factor(ttc_rxt2)

prop.table(table(ttc_rxt2))
table(ttc_rxt2)

ttc_rxt2<-rep(NA,length(time_to_care_rxt))
summary(time_to_care_rxt)
sd(time_to_care_rxt,na.rm=T)
ttc_rxt2[0<time_to_care_rxt&time_to_care_rxt<=1]<-'A-ate-1h'
ttc_rxt2[1<time_to_care_rxt&time_to_care_rxt<=4]<-'B-de1.1a4h'
ttc_rxt2[4<time_to_care_rxt]<-'C-mais-de-4h'

ttc_rxt2 <- factor(ttc_rxt2)

prop.table(table(ttc_rxt2))
table(ttc_rxt2)
table(ttc_rxt2, death)

data$ttc_rxt2<-ttc_rxt2
table(data$ttc_rxt2)
summary(data$ttc_rxt2)



logmodel_gcs<-glm(outcome ~ ttc_rxt2,family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 



#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#data$pulse_ox_m<-(data$pulse_ox+data$pulse_ox2/2)
table(ttc_rxt2)

data$ttc_rxtorax<-NULL
for(i in 1:length(data$ttc_rxt2)){
  if(is.na(data$ttc_rxt2[i])==TRUE){
    if(data$pulse_ox_imp[i]<92) {
      data$ttc_rxtorax[i]<-"nao_fez_precisava"
      
    }
    
    if(data$heart_rate_imp[i]>=100) {
      
      data$ttc_rxtorax[i]<-"nao_fez_precisava"
    }
    
    if(data$resp_rate_imp[i]>25) {
      
      data$ttc_rxtorax[i]<-"nao_fez_precisava"
    }
    
    if(data$gcs_tot_imp[i]<14) {
      
      data$ttc_rxtorax[i]<-"nao_fez_precisava"
    }}
  if(is.na(data$ttc_rxt2[i])==TRUE){
   if(data$pulse_ox_imp[i]>=92 & 
      data$heart_rate_imp[i]<100 & 
      data$resp_rate_imp[i]<=25 & 
      data$gcs_tot_imp[i]>=14){
     
     data$ttc_rxtorax[i]<-"nao_fez_e_nao_precisava"
   }
    }
  else { 
     
     data$ttc_rxtorax[i]<-NA
     
     }
  if (is.na(data$ttc_rxt2[i])==FALSE){
    data$ttc_rxtorax[i]<-data$ttc_rxt2[i]
  }
}

table(data$ttc_rxtorax)
summary(data$ttc_rxtorax)

logmodel_gcs<-glm(outcome ~ ttc_rxtorax,family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

table(data$outcome)
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


#RAIO X DO CÂNIO

str(data$gos_recoded)
str(data$gose_recoded)


###
ttc_rxc<-rep(NA,length(time_to_care_rxc)) #cranio
summary(time_to_care_rxc)
sd(time_to_care_rxc,na.rm=T)
ttc_rxc[0<time_to_care_rxc&time_to_care_rxc<=1]<-'<=1h'
ttc_rxc[1<time_to_care_rxc&time_to_care_rxc<=2]<-'1.1-2h'
ttc_rxc[2<time_to_care_rxc&time_to_care_rxc<=3]<-'2.1-3h'
ttc_rxc[3<time_to_care_rxc&time_to_care_rxc<=4]<-'3.1-4h'
ttc_rxc[4<time_to_care_rxc&time_to_care_rxc<=5]<-'4.1-5h'
ttc_rxc[5<time_to_care_rxc&time_to_care_rxc<=6]<-'5.1-6h'
ttc_rxc[6<time_to_care_rxc&time_to_care_rxc<=7]<-'6.1-7h'
ttc_rxc[7<time_to_care_rxc&time_to_care_rxc<=8]<-'7.1-8h'
ttc_rxc[8<time_to_care_rxc&time_to_care_rxc<=9]<-'8.1-9h'
ttc_rxc[9<time_to_care_rxc&time_to_care_rxc<=10]<-'9.1-10h'
ttc_rxc[10<time_to_care_rxc&time_to_care_rxc<=11]<-'10.1-11h'
ttc_rxc[11<time_to_care_rxc&time_to_care_rxc<=12]<-'11.1-12h'
ttc_rxc[12<time_to_care_rxc&time_to_care_rxc<=13]<-'12.1-13h'
ttc_rxc[13<time_to_care_rxc&time_to_care_rxc<=14]<-'13.1-14h'
ttc_rxc[14<time_to_care_rxc&time_to_care_rxc<=15]<-'14.1-15h'
ttc_rxc[15<time_to_care_rxc&time_to_care_rxc<=16]<-'15.1-16h'
ttc_rxc[16<time_to_care_rxc&time_to_care_rxc<=17]<-'16.1-17h'
ttc_rxc[17<time_to_care_rxc&time_to_care_rxc<=18]<-'17.1-18h'
ttc_rxc[18<time_to_care_rxc&time_to_care_rxc<=19]<-'18.1-19h'
ttc_rxc[19<time_to_care_rxc&time_to_care_rxc<=20]<-'19.1-20h'
ttc_rxc[20<time_to_care_rxc&time_to_care_rxc<=21]<-'20.1-21h'
ttc_rxc[21<time_to_care_rxc&time_to_care_rxc<=22]<-'21.1-22h'
ttc_rxc[22<time_to_care_rxc&time_to_care_rxc<=23]<-'22.1-23h'
ttc_rxc[23<time_to_care_rxc&time_to_care_rxc<=24]<-'23.1-24h'
ttc_rxc[24<time_to_care_rxc&time_to_care_rxc<=48]<-'24.1-48h'
ttc_rxc[48<time_to_care_rxc&time_to_care_rxc<=72]<-'48.1-72h'
ttc_rxc[72<time_to_care_rxc&time_to_care_rxc<=168]<-'72.1-168h'
ttc_rxc[168<time_to_care_rxc]<-'>168h'

ttc_rxc <- factor(ttc_rxc2)
prop.table(table(ttc_rxc))
table(ttc_rxc)

ttc_rxc2<-rep(NA,length(time_to_care_rxc))
summary(time_to_care_rxc)
sd(time_to_care_rxc,na.rm=T)
ttc_rxc2[0<time_to_care_rxc&time_to_care_rxc<=1]<-'<=1h'
ttc_rxc2[1<time_to_care_rxc&time_to_care_rxc<=4]<-'1.1 - 4h'
ttc_rxc2[4<time_to_care_rxc&time_to_care_rxc<=6]<-'4.1 - 6h'
ttc_rxc2[6<time_to_care_rxc&time_to_care_rxc<=12]<-'6.1 - 12h'
ttc_rxc2[12<time_to_care_rxc&time_to_care_rxc<=24]<-'12.1 - 24h'
ttc_rxc2[24<time_to_care_rxc&time_to_care_rxc<=48]<-'24.1 - 48h'
ttc_rxc2[48<time_to_care_rxc]<-'>48h'
ttc_rxc2 <- factor(ttc_rxc2)
prop.table(table(ttc_rxc2))
table(ttc_rxc2)

ttc_rxc2<-rep(NA,length(time_to_care_rxc))
summary(time_to_care_rxc)
sd(time_to_care_rxc,na.rm=T)
ttc_rxc2[0<time_to_care_rxc&time_to_care_rxc<=1]<-'<=1h'
ttc_rxc2[1<time_to_care_rxc&time_to_care_rxc<=4]<-'1.1 - 4h'
ttc_rxc2[4<time_to_care_rxc]<-'>4h'
ttc_rxc2 <- factor(ttc_rxc2)
prop.table(table(ttc_rxc2))
table(ttc_rxc2)
table(ttc_rxc2,death)
data$ttc_rxc2<-ttc_rxc2

logmodel_gcs<-glm(outcome ~ ttc_rxc2,family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 


###
# TOMOGRAFIA COMPUTADORIZADA

time_to_care_ct
ttc_ct<-rep(NA,length(time_to_care_ct))
summary(time_to_care_ct)
sd(time_to_care_ct,na.rm=T)
ttc_ct[0<time_to_care_ct&time_to_care_ct<=1]<-'<=1h'
ttc_ct[1<time_to_care_ct&time_to_care_ct<=2]<-'1.1-2h'
ttc_ct[2<time_to_care_ct&time_to_care_ct<=3]<-'2.1-3h'
ttc_ct[3<time_to_care_ct&time_to_care_ct<=4]<-'3.1-4h'
ttc_ct[4<time_to_care_ct&time_to_care_ct<=5]<-'4.1-5h'
ttc_ct[5<time_to_care_ct&time_to_care_ct<=6]<-'5.1-6h'
ttc_ct[6<time_to_care_ct&time_to_care_ct<=7]<-'6.1-7h'
ttc_ct[7<time_to_care_ct&time_to_care_ct<=8]<-'7.1-8h'
ttc_ct[8<time_to_care_ct&time_to_care_ct<=9]<-'8.1-9h'
ttc_ct[9<time_to_care_ct&time_to_care_ct<=10]<-'9.1-10h'
ttc_ct[10<time_to_care_ct&time_to_care_ct<=11]<-'10.1-11h'
ttc_ct[11<time_to_care_ct&time_to_care_ct<=12]<-'11.1-12h'
ttc_ct[12<time_to_care_ct&time_to_care_ct<=13]<-'12.1-13h'
ttc_ct[13<time_to_care_ct&time_to_care_ct<=14]<-'13.1-14h'
ttc_ct[14<time_to_care_ct&time_to_care_ct<=15]<-'14.1-15h'
ttc_ct[15<time_to_care_ct&time_to_care_ct<=16]<-'15.1-16h'
ttc_ct[16<time_to_care_ct&time_to_care_ct<=17]<-'16.1-17h'
ttc_ct[17<time_to_care_ct&time_to_care_ct<=18]<-'17.1-18h'
ttc_ct[18<time_to_care_ct&time_to_care_ct<=19]<-'18.1-19h'
ttc_ct[19<time_to_care_ct&time_to_care_ct<=20]<-'19.1-20h'
ttc_ct[20<time_to_care_ct&time_to_care_ct<=21]<-'20.1-21h'
ttc_ct[21<time_to_care_ct&time_to_care_ct<=22]<-'21.1-22h'
ttc_ct[22<time_to_care_ct&time_to_care_ct<=23]<-'22.1-23h'
ttc_ct[23<time_to_care_ct&time_to_care_ct<=24]<-'23.1-24h'
ttc_ct[24<time_to_care_ct&time_to_care_ct<=48]<-'24.1-48h'
ttc_ct[48<time_to_care_ct&time_to_care_ct<=72]<-'48.1-72h'
ttc_ct[72<time_to_care_ct&time_to_care_ct<=168]<-'72.1-168h'
ttc_ct[168<time_to_care_ct]<-'>168h'
ttc_ct <- factor(ttc_ct2)
prop.table(table(ttc_ct))
table(ttc_ct)
summary(time_to_care_ct)

ttc_ct2<-rep(NA,length(time_to_care_ct))
summary(time_to_care_ct)
sd(time_to_care_ct,na.rm=T)
ttc_ct2[0<time_to_care_ct&time_to_care_ct<=1]<-'<=1h'
ttc_ct2[1<time_to_care_ct&time_to_care_ct<=4]<-'1.1 - 4h'
ttc_ct2[4<time_to_care_ct&time_to_care_ct<=6]<-'4.1 - 6h'
ttc_ct2[6<time_to_care_ct&time_to_care_ct<=12]<-'6.1 - 12h'
ttc_ct2[12<time_to_care_ct&time_to_care_ct<=24]<-'12.1 - 24h'
ttc_ct2[24<time_to_care_ct&time_to_care_ct<=48]<-'24.1 - 48h'
ttc_ct2[48<time_to_care_ct]<-'>48h'
ttc_ct2 <- factor(ttc_ct2)
prop.table(table(ttc_ct2))
table(ttc_ct2)

ttc_ct2<-rep(NA,length(time_to_care_ct))
summary(time_to_care_ct)
sd(time_to_care_ct,na.rm=T)
ttc_ct2[0<time_to_care_ct&time_to_care_ct<=1]<-'<=1h'
ttc_ct2[1<time_to_care_ct&time_to_care_ct<=4]<-'1.1 - 4h'
ttc_ct2[4<time_to_care_ct]<-'>4h'
ttc_ct2 <- factor(ttc_ct2)
prop.table(table(ttc_ct2))
table(ttc_ct2)
table(ttc_ct2,death)
data$ttc_ct2<-ttc_ct2


table(gcs_tot_imp)
logmodel_gcs<-glm(outcome ~ ttc_ct2,family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 


data$ttc_tomo<-NULL
for(i in 1:length(data$ttc_ct2)){
  if(is.na(data$ttc_ct2[i])==TRUE){
    if(data$gcs_tot_imp[i]<=13) {
      data$ttc_tomo[i]<-"nao_fez_precisava"
    }
    }
  if(is.na(data$ttc_ct2[i])==TRUE){
    if(data$gcs_tot_imp[i]>13) {
      data$ttc_tomo[i]<-"nao_fez_e_nao_precisava"
    }
  }
  else {
    data$ttc_tomo[i]<-"NA"
  }
  if (is.na(data$ttc_ct2[i])==FALSE){
    data$ttc_tomo[i]<-data$ttc_ct2[i]
  }
  }
table(data$ttc_tomo)
summary(data$ttc_tomo)

logmodel_gcs<-glm(outcome ~ ttc_tomo,family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


###
time_to_care_fluid
ttc_fluid<-rep(NA,length(time_to_care_fluid))
summary(time_to_care_fluid)
sd(time_to_care_fluid,na.rm=T)
ttc_fluid[0<time_to_care_fluid&time_to_care_fluid<=1]<-'<=1h'
ttc_fluid[1<time_to_care_fluid&time_to_care_fluid<=2]<-'1.1-2h'
ttc_fluid[2<time_to_care_fluid&time_to_care_fluid<=3]<-'2.1-3h'
ttc_fluid[3<time_to_care_fluid&time_to_care_fluid<=4]<-'3.1-4h'
ttc_fluid[4<time_to_care_fluid&time_to_care_fluid<=5]<-'4.1-5h'
ttc_fluid[5<time_to_care_fluid&time_to_care_fluid<=6]<-'5.1-6h'
ttc_fluid[6<time_to_care_fluid&time_to_care_fluid<=7]<-'6.1-7h'
ttc_fluid[7<time_to_care_fluid&time_to_care_fluid<=8]<-'7.1-8h'
ttc_fluid[8<time_to_care_fluid&time_to_care_fluid<=9]<-'8.1-9h'
ttc_fluid[9<time_to_care_fluid&time_to_care_fluid<=10]<-'9.1-10h'
ttc_fluid[10<time_to_care_fluid&time_to_care_fluid<=11]<-'10.1-11h'
ttc_fluid[11<time_to_care_fluid&time_to_care_fluid<=12]<-'11.1-12h'
ttc_fluid[12<time_to_care_fluid&time_to_care_fluid<=13]<-'12.1-13h'
ttc_fluid[13<time_to_care_fluid&time_to_care_fluid<=14]<-'13.1-14h'
ttc_fluid[14<time_to_care_fluid&time_to_care_fluid<=15]<-'14.1-15h'
ttc_fluid[15<time_to_care_fluid&time_to_care_fluid<=16]<-'15.1-16h'
ttc_fluid[16<time_to_care_fluid&time_to_care_fluid<=17]<-'16.1-17h'
ttc_fluid[17<time_to_care_fluid&time_to_care_fluid<=18]<-'17.1-18h'
ttc_fluid[18<time_to_care_fluid&time_to_care_fluid<=19]<-'18.1-19h'
ttc_fluid[19<time_to_care_fluid&time_to_care_fluid<=20]<-'19.1-20h'
ttc_fluid[20<time_to_care_fluid&time_to_care_fluid<=21]<-'20.1-21h'
ttc_fluid[21<time_to_care_fluid&time_to_care_fluid<=22]<-'21.1-22h'
ttc_fluid[22<time_to_care_fluid&time_to_care_fluid<=23]<-'22.1-23h'
ttc_fluid[23<time_to_care_fluid&time_to_care_fluid<=24]<-'23.1-24h'
ttc_fluid[24<time_to_care_fluid&time_to_care_fluid<=48]<-'24.1-48h'
ttc_fluid[48<time_to_care_fluid&time_to_care_fluid<=72]<-'48.1-72h'
ttc_fluid[72<time_to_care_fluid&time_to_care_fluid<=168]<-'72.1-168h'
ttc_fluid[168<time_to_care_fluid]<-'>168h'
ttc_fluid <- factor(ttc_fluid2)
prop.table(table(ttc_fluid))
table(ttc_fluid)
summary(time_to_care_fluid)

ttc_fluid2<-rep(NA,length(time_to_care_fluid))
summary(time_to_care_fluid)
sd(time_to_care_fluid,na.rm=T)
ttc_fluid2[0<time_to_care_fluid&time_to_care_fluid<=1]<-'<=1h'
ttc_fluid2[1<time_to_care_fluid&time_to_care_fluid<=4]<-'1.1 - 4h'
ttc_fluid2[4<time_to_care_fluid&time_to_care_fluid<=6]<-'4.1 - 6h'
ttc_fluid2[6<time_to_care_fluid&time_to_care_fluid<=12]<-'6.1 - 12h'
ttc_fluid2[12<time_to_care_fluid&time_to_care_fluid<=24]<-'12.1 - 24h'
ttc_fluid2[24<time_to_care_fluid&time_to_care_fluid<=48]<-'24.1 - 48h'
ttc_fluid2[48<time_to_care_fluid]<-'>48h'
ttc_fluid2 <- factor(ttc_fluid2)
prop.table(table(ttc_fluid2))
table(ttc_fluid2)

ttc_fluid2<-rep(NA,length(time_to_care_fluid))
summary(time_to_care_fluid)
sd(time_to_care_fluid,na.rm=T)
ttc_fluid2[0<time_to_care_fluid&time_to_care_fluid<=1]<-'<=1h'
ttc_fluid2[1<time_to_care_fluid&time_to_care_fluid<=4]<-'1.1 - 4h'
ttc_fluid2[4<time_to_care_fluid]<-'>4h'
ttc_fluid2 <- factor(ttc_fluid2)
prop.table(table(ttc_fluid2))
table(ttc_fluid2)
table(ttc_fluid2,death)
data$ttc_fluid2<-ttc_fluid2


logmodel_gcs<-glm(outcome ~ ttc_fluid2,family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 


data$ttc_fluidos<-NULL
for(i in 1:length(data$ttc_fluid2)){
  if(is.na(data$ttc_fluid2[i])==TRUE){
    if(data$sys_bp_imp[i]<100) {
      data$ttc_fluidos[i]<-"nao_fez_precisava"
    }
  }
  if(is.na(data$ttc_fluid2[i])==TRUE){
    if(data$sys_bp_imp[i]>=100) {
      data$ttc_fluidos[i]<-"nao_fez_e_nao_precisava"
    }
  }
  else {
    data$ttc_fluidos[i]<-"NA"
  }
  if (is.na(data$ttc_fluid2[i])==FALSE){
    data$ttc_fluidos[i]<-data$ttc_fluid2[i]
  }
}

table(data$ttc_fluidos)
summary(data$ttc_fluidos)

logmodel_gcs<-glm(outcome ~ ttc_fluidos,family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 


###
time_to_care_oxyg
ttc_oxyg<-rep(NA,length(time_to_care_oxyg))
summary(time_to_care_oxyg)
sd(time_to_care_oxyg,na.rm=T)
ttc_oxyg[0<time_to_care_oxyg&time_to_care_oxyg<=1]<-'<=1h'
ttc_oxyg[1<time_to_care_oxyg&time_to_care_oxyg<=2]<-'1.1-2h'
ttc_oxyg[2<time_to_care_oxyg&time_to_care_oxyg<=3]<-'2.1-3h'
ttc_oxyg[3<time_to_care_oxyg&time_to_care_oxyg<=4]<-'3.1-4h'
ttc_oxyg[4<time_to_care_oxyg&time_to_care_oxyg<=5]<-'4.1-5h'
ttc_oxyg[5<time_to_care_oxyg&time_to_care_oxyg<=6]<-'5.1-6h'
ttc_oxyg[6<time_to_care_oxyg&time_to_care_oxyg<=7]<-'6.1-7h'
ttc_oxyg[7<time_to_care_oxyg&time_to_care_oxyg<=8]<-'7.1-8h'
ttc_oxyg[8<time_to_care_oxyg&time_to_care_oxyg<=9]<-'8.1-9h'
ttc_oxyg[9<time_to_care_oxyg&time_to_care_oxyg<=10]<-'9.1-10h'
ttc_oxyg[10<time_to_care_oxyg&time_to_care_oxyg<=11]<-'10.1-11h'
ttc_oxyg[11<time_to_care_oxyg&time_to_care_oxyg<=12]<-'11.1-12h'
ttc_oxyg[12<time_to_care_oxyg&time_to_care_oxyg<=13]<-'12.1-13h'
ttc_oxyg[13<time_to_care_oxyg&time_to_care_oxyg<=14]<-'13.1-14h'
ttc_oxyg[14<time_to_care_oxyg&time_to_care_oxyg<=15]<-'14.1-15h'
ttc_oxyg[15<time_to_care_oxyg&time_to_care_oxyg<=16]<-'15.1-16h'
ttc_oxyg[16<time_to_care_oxyg&time_to_care_oxyg<=17]<-'16.1-17h'
ttc_oxyg[17<time_to_care_oxyg&time_to_care_oxyg<=18]<-'17.1-18h'
ttc_oxyg[18<time_to_care_oxyg&time_to_care_oxyg<=19]<-'18.1-19h'
ttc_oxyg[19<time_to_care_oxyg&time_to_care_oxyg<=20]<-'19.1-20h'
ttc_oxyg[20<time_to_care_oxyg&time_to_care_oxyg<=21]<-'20.1-21h'
ttc_oxyg[21<time_to_care_oxyg&time_to_care_oxyg<=22]<-'21.1-22h'
ttc_oxyg[22<time_to_care_oxyg&time_to_care_oxyg<=23]<-'22.1-23h'
ttc_oxyg[23<time_to_care_oxyg&time_to_care_oxyg<=24]<-'23.1-24h'
ttc_oxyg[24<time_to_care_oxyg&time_to_care_oxyg<=48]<-'24.1-48h'
ttc_oxyg[48<time_to_care_oxyg&time_to_care_oxyg<=72]<-'48.1-72h'
ttc_oxyg[72<time_to_care_oxyg&time_to_care_oxyg<=168]<-'72.1-168h'
ttc_oxyg[168<time_to_care_oxyg]<-'>168h'
ttc_oxyg <- factor(ttc_oxyg2)
prop.table(table(ttc_oxyg))
table(ttc_oxyg)
summary(time_to_care_oxyg)

ttc_oxyg2<-rep(NA,length(time_to_care_oxyg))
summary(time_to_care_oxyg)
sd(time_to_care_oxyg,na.rm=T)
ttc_oxyg2[0<time_to_care_oxyg&time_to_care_oxyg<=1]<-'<=1h'
ttc_oxyg2[1<time_to_care_oxyg&time_to_care_oxyg<=4]<-'1.1 - 4h'
ttc_oxyg2[4<time_to_care_oxyg&time_to_care_oxyg<=6]<-'4.1 - 6h'
ttc_oxyg2[6<time_to_care_oxyg&time_to_care_oxyg<=12]<-'6.1 - 12h'
ttc_oxyg2[12<time_to_care_oxyg&time_to_care_oxyg<=24]<-'12.1 - 24h'
ttc_oxyg2[24<time_to_care_oxyg&time_to_care_oxyg<=48]<-'24.1 - 48h'
ttc_oxyg2[48<time_to_care_oxyg]<-'>48h'
ttc_oxyg2 <- factor(ttc_oxyg2)
prop.table(table(ttc_oxyg2))
table(ttc_oxyg2)

ttc_oxyg2<-rep(NA,length(time_to_care_oxyg))
summary(time_to_care_oxyg)
sd(time_to_care_oxyg,na.rm=T)
ttc_oxyg2[0<time_to_care_oxyg&time_to_care_oxyg<=1]<-'<=1h'
ttc_oxyg2[1<time_to_care_oxyg]<-'>1h'
ttc_oxyg2 <- factor(ttc_oxyg2)
prop.table(table(ttc_oxyg2))
table(ttc_oxyg2)
table(ttc_oxyg2,death)
data$ttc_oxyg2<-ttc_oxyg2

logmodel_gcs<-glm(outcome ~ ttc_oxyg2,family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

data$ttc_oxigenio<-NULL
for(i in 1:length(data$ttc_oxyg2)){
  if(is.na(data$ttc_oxyg2[i])==TRUE){
    if(data$gcs_tot_imp[i]<=8) {
      data$ttc_oxigenio[i]<-"nao_fez_precisava"
    }
    if(data$pulse_ox_imp[i]<92) {
      data$ttc_oxigenio[i]<-"nao_fez_precisava"
    }
  }
  if(is.na(data$ttc_oxyg2[i])==TRUE){
    if(data$gcs_tot_imp[i]>8 &
       data$pulse_ox_imp[i]>=92) {
      data$ttc_oxigenio[i]<-"nao_fez_e_nao_precisava"
    }
  }
  else {
    data$ttc_oxigenio[i]<-"NA"
  }
  if (is.na(data$ttc_oxyg2[i])==FALSE){
    data$ttc_oxigenio[i]<-data$ttc_oxyg2[i]
  }
}


table(data$ttc_oxigenio)
summary(data$ttc_oxigenio)

logmodel_gcs<-glm(outcome ~ ttc_oxigenio,family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

###
time_to_care_mannit
ttc_mannit<-rep(NA,length(time_to_care_mannit))
summary(time_to_care_mannit)
sd(time_to_care_mannit,na.rm=T)
ttc_mannit[0<time_to_care_mannit&time_to_care_mannit<=1]<-'<=1h'
ttc_mannit[1<time_to_care_mannit&time_to_care_mannit<=2]<-'1.1-2h'
ttc_mannit[2<time_to_care_mannit&time_to_care_mannit<=3]<-'2.1-3h'
ttc_mannit[3<time_to_care_mannit&time_to_care_mannit<=4]<-'3.1-4h'
ttc_mannit[4<time_to_care_mannit&time_to_care_mannit<=5]<-'4.1-5h'
ttc_mannit[5<time_to_care_mannit&time_to_care_mannit<=6]<-'5.1-6h'
ttc_mannit[6<time_to_care_mannit&time_to_care_mannit<=7]<-'6.1-7h'
ttc_mannit[7<time_to_care_mannit&time_to_care_mannit<=8]<-'7.1-8h'
ttc_mannit[8<time_to_care_mannit&time_to_care_mannit<=9]<-'8.1-9h'
ttc_mannit[9<time_to_care_mannit&time_to_care_mannit<=10]<-'9.1-10h'
ttc_mannit[10<time_to_care_mannit&time_to_care_mannit<=11]<-'10.1-11h'
ttc_mannit[11<time_to_care_mannit&time_to_care_mannit<=12]<-'11.1-12h'
ttc_mannit[12<time_to_care_mannit&time_to_care_mannit<=13]<-'12.1-13h'
ttc_mannit[13<time_to_care_mannit&time_to_care_mannit<=14]<-'13.1-14h'
ttc_mannit[14<time_to_care_mannit&time_to_care_mannit<=15]<-'14.1-15h'
ttc_mannit[15<time_to_care_mannit&time_to_care_mannit<=16]<-'15.1-16h'
ttc_mannit[16<time_to_care_mannit&time_to_care_mannit<=17]<-'16.1-17h'
ttc_mannit[17<time_to_care_mannit&time_to_care_mannit<=18]<-'17.1-18h'
ttc_mannit[18<time_to_care_mannit&time_to_care_mannit<=19]<-'18.1-19h'
ttc_mannit[19<time_to_care_mannit&time_to_care_mannit<=20]<-'19.1-20h'
ttc_mannit[20<time_to_care_mannit&time_to_care_mannit<=21]<-'20.1-21h'
ttc_mannit[21<time_to_care_mannit&time_to_care_mannit<=22]<-'21.1-22h'
ttc_mannit[22<time_to_care_mannit&time_to_care_mannit<=23]<-'22.1-23h'
ttc_mannit[23<time_to_care_mannit&time_to_care_mannit<=24]<-'23.1-24h'
ttc_mannit[24<time_to_care_mannit&time_to_care_mannit<=48]<-'24.1-48h'
ttc_mannit[48<time_to_care_mannit&time_to_care_mannit<=72]<-'48.1-72h'
ttc_mannit[72<time_to_care_mannit&time_to_care_mannit<=168]<-'72.1-168h'
ttc_mannit[168<time_to_care_mannit]<-'>168h'
ttc_mannit <- factor(ttc_mannit2)
prop.table(table(ttc_mannit))
table(ttc_mannit)
summary(time_to_care_mannit)

ttc_mannit2<-rep(NA,length(time_to_care_mannit))
summary(time_to_care_mannit)
sd(time_to_care_mannit,na.rm=T)
ttc_mannit2[0<time_to_care_mannit&time_to_care_mannit<=1]<-'<=1h'
ttc_mannit2[1<time_to_care_mannit&time_to_care_mannit<=4]<-'1.1 - 4h'
ttc_mannit2[4<time_to_care_mannit&time_to_care_mannit<=6]<-'4.1 - 6h'
ttc_mannit2[6<time_to_care_mannit&time_to_care_mannit<=12]<-'6.1 - 12h'
ttc_mannit2[12<time_to_care_mannit&time_to_care_mannit<=24]<-'12.1 - 24h'
ttc_mannit2[24<time_to_care_mannit&time_to_care_mannit<=48]<-'24.1 - 48h'
ttc_mannit2[48<time_to_care_mannit]<-'>48h'
ttc_mannit2 <- factor(ttc_mannit2)
prop.table(table(ttc_mannit2))
table(ttc_mannit2)

ttc_mannit2<-rep(NA,length(time_to_care_mannit))
summary(time_to_care_mannit)
sd(time_to_care_mannit,na.rm=T)
ttc_mannit2[0<time_to_care_mannit&time_to_care_mannit<=1]<-'<=1h'
ttc_mannit2[1<time_to_care_mannit]<-'>1h'
ttc_mannit2 <- factor(ttc_mannit2)
prop.table(table(ttc_mannit2))
table(ttc_mannit2)
table(ttc_mannit2,death)
data$ttc_mannit2<-ttc_mannit2

logmodel_gcs<-glm(outcome ~ ttc_mannit2,family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 


#time_to_care_surgmd
#ttc_surgmd<-rep(NA,length(time_to_care_surgmd))
#summary(time_to_care_surgmd)
#sd(time_to_care_surgmd,na.rm=T)
#ttc_surgmd[0<time_to_care_surgmd&time_to_care_surgmd<=1]<-'<=1h'
#ttc_surgmd[1<time_to_care_surgmd&time_to_care_surgmd<=2]<-'1.1-2h'
#ttc_surgmd[2<time_to_care_surgmd&time_to_care_surgmd<=3]<-'2.1-3h'
#ttc_surgmd[3<time_to_care_surgmd&time_to_care_surgmd<=4]<-'3.1-4h'
#ttc_surgmd[4<time_to_care_surgmd&time_to_care_surgmd<=5]<-'4.1-5h'
#ttc_surgmd[5<time_to_care_surgmd&time_to_care_surgmd<=6]<-'5.1-6h'
#ttc_surgmd[6<time_to_care_surgmd&time_to_care_surgmd<=7]<-'6.1-7h'
#ttc_surgmd[7<time_to_care_surgmd&time_to_care_surgmd<=8]<-'7.1-8h'
#ttc_surgmd[8<time_to_care_surgmd&time_to_care_surgmd<=9]<-'8.1-9h'
#ttc_surgmd[9<time_to_care_surgmd&time_to_care_surgmd<=10]<-'9.1-10h'
#ttc_surgmd[10<time_to_care_surgmd&time_to_care_surgmd<=11]<-'10.1-11h'
#ttc_surgmd[11<time_to_care_surgmd&time_to_care_surgmd<=12]<-'11.1-12h'
#ttc_surgmd[12<time_to_care_surgmd&time_to_care_surgmd<=13]<-'12.1-13h'
#ttc_surgmd[13<time_to_care_surgmd&time_to_care_surgmd<=14]<-'13.1-14h'
#ttc_surgmd[14<time_to_care_surgmd&time_to_care_surgmd<=15]<-'14.1-15h'
#ttc_surgmd[15<time_to_care_surgmd&time_to_care_surgmd<=16]<-'15.1-16h'
#ttc_surgmd[16<time_to_care_surgmd&time_to_care_surgmd<=17]<-'16.1-17h'
#ttc_surgmd[17<time_to_care_surgmd&time_to_care_surgmd<=18]<-'17.1-18h'
#ttc_surgmd[18<time_to_care_surgmd&time_to_care_surgmd<=19]<-'18.1-19h'
#ttc_surgmd[19<time_to_care_surgmd&time_to_care_surgmd<=20]<-'19.1-20h'
#ttc_surgmd[20<time_to_care_surgmd&time_to_care_surgmd<=21]<-'20.1-21h'
#ttc_surgmd[21<time_to_care_surgmd&time_to_care_surgmd<=22]<-'21.1-22h'
#ttc_surgmd[22<time_to_care_surgmd&time_to_care_surgmd<=23]<-'22.1-23h'
#ttc_surgmd[23<time_to_care_surgmd&time_to_care_surgmd<=24]<-'23.1-24h'
#ttc_surgmd[24<time_to_care_surgmd&time_to_care_surgmd<=48]<-'24.1-48h'
#ttc_surgmd[48<time_to_care_surgmd&time_to_care_surgmd<=72]<-'48.1-72h'
#ttc_surgmd[72<time_to_care_surgmd&time_to_care_surgmd<=168]<-'72.1-168h'
#ttc_surgmd[168<time_to_care_surgmd]<-'>168h'
#prop.table(table(ttc_surgmd))
#table(ttc_surgmd)
#summary(time_to_care_surgmd)

#ttc_surgmd2<-rep(NA,length(time_to_care_surgmd))
#summary(time_to_care_surgmd)
#sd(time_to_care_surgmd,na.rm=T)
#ttc_surgmd2[0<time_to_care_surgmd&time_to_care_surgmd<=1]<-'<=1h'
#ttc_surgmd2[1<time_to_care_surgmd&time_to_care_surgmd<=4]<-'1.1 - 4h'
#ttc_surgmd2[4<time_to_care_surgmd&time_to_care_surgmd<=6]<-'4.1 - 6h'
#ttc_surgmd2[6<time_to_care_surgmd&time_to_care_surgmd<=12]<-'6.1 - 12h'
#ttc_surgmd2[12<time_to_care_surgmd&time_to_care_surgmd<=24]<-'12.1 - 24h'
#ttc_surgmd2[24<time_to_care_surgmd&time_to_care_surgmd<=48]<-'24.1 - 48h'
#ttc_surgmd2[48<time_to_care_surgmd]<-'>48h'
#prop.table(table(ttc_surgmd2))
#table(ttc_surgmd2)

#logmodel_gcs<-glm(death ~ ttc_surgmd2,family=binomial, data=data)
#summary(logmodel_gcs)
#exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 


#logmodel_gcs<-glm(death ~ ttc2 + ttc_cdmd2 + ttc_rxt2 + ttc_rxc2 + ttc_labor2 + 
#                    ttc_ct2 + ttc_fluid2 + ttc_oxyg2 + ttc_mannit2 + 
#                    ttc_tbis2 + male + age + moi + gcs,family=binomial, data=data)
#summary(logmodel_gcs)
#exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 


###
time_to_care_tbis
ttc_tbis<-rep(NA,length(time_to_care_tbis))
summary(time_to_care_tbis)
sd(time_to_care_tbis,na.rm=T)
ttc_tbis[0<time_to_care_tbis&time_to_care_tbis<=1]<-'<=1h'
ttc_tbis[1<time_to_care_tbis&time_to_care_tbis<=2]<-'1.1-2h'
ttc_tbis[2<time_to_care_tbis&time_to_care_tbis<=3]<-'2.1-3h'
ttc_tbis[3<time_to_care_tbis&time_to_care_tbis<=4]<-'3.1-4h'
ttc_tbis[4<time_to_care_tbis&time_to_care_tbis<=5]<-'4.1-5h'
ttc_tbis[5<time_to_care_tbis&time_to_care_tbis<=6]<-'5.1-6h'
ttc_tbis[6<time_to_care_tbis&time_to_care_tbis<=7]<-'6.1-7h'
ttc_tbis[7<time_to_care_tbis&time_to_care_tbis<=8]<-'7.1-8h'
ttc_tbis[8<time_to_care_tbis&time_to_care_tbis<=9]<-'8.1-9h'
ttc_tbis[9<time_to_care_tbis&time_to_care_tbis<=10]<-'9.1-10h'
ttc_tbis[10<time_to_care_tbis&time_to_care_tbis<=11]<-'10.1-11h'
ttc_tbis[11<time_to_care_tbis&time_to_care_tbis<=12]<-'11.1-12h'
ttc_tbis[12<time_to_care_tbis&time_to_care_tbis<=13]<-'12.1-13h'
ttc_tbis[13<time_to_care_tbis&time_to_care_tbis<=14]<-'13.1-14h'
ttc_tbis[14<time_to_care_tbis&time_to_care_tbis<=15]<-'14.1-15h'
ttc_tbis[15<time_to_care_tbis&time_to_care_tbis<=16]<-'15.1-16h'
ttc_tbis[16<time_to_care_tbis&time_to_care_tbis<=17]<-'16.1-17h'
ttc_tbis[17<time_to_care_tbis&time_to_care_tbis<=18]<-'17.1-18h'
ttc_tbis[18<time_to_care_tbis&time_to_care_tbis<=19]<-'18.1-19h'
ttc_tbis[19<time_to_care_tbis&time_to_care_tbis<=20]<-'19.1-20h'
ttc_tbis[20<time_to_care_tbis&time_to_care_tbis<=21]<-'20.1-21h'
ttc_tbis[21<time_to_care_tbis&time_to_care_tbis<=22]<-'21.1-22h'
ttc_tbis[22<time_to_care_tbis&time_to_care_tbis<=23]<-'22.1-23h'
ttc_tbis[23<time_to_care_tbis&time_to_care_tbis<=24]<-'23.1-24h'
ttc_tbis[24<time_to_care_tbis&time_to_care_tbis<=48]<-'24.1-48h'
ttc_tbis[48<time_to_care_tbis&time_to_care_tbis<=72]<-'48.1-72h'
ttc_tbis[72<time_to_care_tbis&time_to_care_tbis<=168]<-'72.1-168h'
ttc_tbis[168<time_to_care_tbis]<-'>168h'
ttc_tbis <- factor(ttc_tbis2)
prop.table(table(ttc_tbis))
table(ttc_tbis)
summary(time_to_care_tbis)

ttc_tbis2<-rep(NA,length(time_to_care_tbis))
summary(time_to_care_tbis)
sd(time_to_care_tbis,na.rm=T)
ttc_tbis2[0<time_to_care_tbis&time_to_care_tbis<=1]<-'<=1h'
ttc_tbis2[1<time_to_care_tbis&time_to_care_tbis<=4]<-'1.1 - 4h'
ttc_tbis2[4<time_to_care_tbis&time_to_care_tbis<=6]<-'4.1 - 6h'
ttc_tbis2[6<time_to_care_tbis&time_to_care_tbis<=12]<-'6.1 - 12h'
ttc_tbis2[12<time_to_care_tbis&time_to_care_tbis<=24]<-'12.1 - 24h'
ttc_tbis2[24<time_to_care_tbis&time_to_care_tbis<=48]<-'24.1 - 48h'
ttc_tbis2[48<time_to_care_tbis]<-'>48h'
ttc_tbis2 <- factor(ttc_tbis2)
prop.table(table(ttc_tbis2))
table(ttc_tbis2)

ttc_tbis2<-rep(NA,length(time_to_care_tbis))
summary(time_to_care_tbis)
sd(time_to_care_tbis,na.rm=T)
ttc_tbis2[0<time_to_care_tbis&time_to_care_tbis<=4]<-'<=4h'
ttc_tbis2[4<time_to_care_tbis&time_to_care_tbis<=12]<-'4.1 - 12h'
ttc_tbis2[12<time_to_care_tbis]<-'>12h'
ttc_tbis2 <- factor(ttc_tbis2)
prop.table(table(ttc_tbis2))
table(ttc_tbis2)
table(ttc_tbis2,death)
data$ttc_tbis2<-ttc_tbis2

data$outcome <- relevel(data$outcome, ref = "bad")

logmodel_gcs<-glm(outcome ~ ttc_tbis2,family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

data$ttc_cirurgia<-car::recode(data$ttc_tbis2,"NA='sem_informacao'")
table(data$ttc_cirurgia)

logmodel_gcs<-glm(outcome ~ ttc_cirurgia,family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

table(data$gcs)
###################
logmodel_gcs<-glm(outcome ~ male_imp,family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

logmodel_gcs<-glm(outcome ~ age_imp,family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

table(data$moi_imp)
logmodel_gcs<-glm(outcome ~ moi_imp,family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

logmodel_gcs<-glm(outcome ~ gcs,family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

logmodel_gcs<-glm(outcome ~ sys_bp_imp,family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

logmodel_gcs<-glm(outcome ~ resp_rate_imp,family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

logmodel_gcs<-glm(outcome ~ pulse_ox_imp,family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

logmodel_gcs<-glm(outcome ~ heart_rate_imp,family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 



table(data$gcs)

###ADJUSTED
logmodel_gcs<-glm(outcome ~ ttc2 + ttc_cdmd2 + ttc_labor2 + ttc_cirurgia + ttc_oxigenio + ttc_fluidos +
                    ttc_tomo + ttc_rxtorax + male_imp + age_imp + moi_imp + gcs + sys_bp_imp + resp_rate_imp,
                  family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 


table(data$ttc_tomo)
logmodel_gcs<-glm(outcome ~ ttc_cirurgia + ttc_oxigenio + ttc_fluidos +
                    male_imp + age_imp + moi_imp + gcs_tot_imp + sys_bp_imp + resp_rate_imp,
                  family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 


logmodel_gcs<-glm(outcome ~ ttc_cirurgia + male_imp + age_imp + moi_imp + gcs_tot_imp + sys_bp_imp + resp_rate_imp,
                  family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 


logmodel_gcs<-glm(outcome ~ ttc_oxigenio + male_imp + age_imp + moi_imp + gcs_tot_imp + sys_bp_imp + resp_rate_imp,
                  family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

logmodel_gcs<-glm(outcome ~ ttc_fluidos + male_imp + age_imp + moi_imp + gcs_tot_imp + sys_bp_imp + resp_rate_imp,
                  family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

logmodel_gcs<-glm(outcome ~ ttc_tomo + male_imp + age_imp + moi_imp + gcs_tot_imp + sys_bp_imp + resp_rate_imp,
                  family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

logmodel_gcs<-glm(outcome ~ ttc_rxtorax + male_imp + age_imp + moi_imp + gcs_tot_imp + sys_bp_imp + resp_rate_imp,
                  family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 




###########################################
########### TABELAS E FIGURAS #############
names(data)
subsample<-with(data,data.frame(time_to_care,
                                time_to_care_cdmd,
                                time_to_care_rxt,
                                time_to_care_labor,
                                time_to_care_ct,
                                time_to_care_fluid,
                                time_to_care_oxyg,
                                time_to_care_tbis,
                                outcome))
names(subsample)
subsample<-data.frame(Arrival=subsample$time_to_care,CDMD=subsample$time_to_care_cdmd,RX=subsample$time_to_care_rxt,
                      Labor=subsample$time_to_care_labor,CT=subsample$time_to_care_ct,Fluids=subsample$time_to_care_fluid,
                      Oxygen=subsample$time_to_care_oxyg,Surgery=subsample$time_to_care_tbis,GOS=subsample$outcome)
names(subsample)

#subsample<-with(data,data.frame(Arrival,
#                                Cdmd,
#                                Xr,
#                                Lab,
#                                Ct,
#                                Fluids,
#                                Oxygen,
#                                Surgery,
#                                GOS))
#names(data)
table(data$gcs)
gcs1 <- subset(data, gcs = "mild" )
summary(gcs1)

#install.packages("tidyr")
library(tidyr)
empilhado <- subsample
empilhado <- gather(empilhado, type, time, -GOS)

empilhado2<- na.omit(empilhado)
empilhado2 <- subset(empilhado2,empilhado2$time<=30)

library(ggplot2)

# grouped boxplot
ggplot(empilhado2, aes(x=type, y=time, fill=GOS)) + 
  geom_boxplot()

#deixando apenas as três variaveis com menor escala

subsample1<-with(data,data.frame(time_to_care_cdmd,
                                time_to_care_fluid,
                                time_to_care_oxyg,
                                outcome))
names(subsample1)
subsample1<-data.frame(Arrival_CDMD=subsample1$time_to_care_cdmd,CDMD_Fluids=subsample1$time_to_care_fluid,
                      CDMD_Oxygen=subsample1$time_to_care_oxyg,GOS=subsample1$outcome)
names(subsample1)

empilhado <- subsample1
empilhado <- gather(empilhado, type, time, -GOS)

empilhado2<- na.omit(empilhado)
empilhado2 <- subset(empilhado2,empilhado2$time<=1)

# grouped boxplot
ggplot(empilhado2, aes(x=type, y=time, fill=GOS)) + 
  geom_boxplot()

#DEIXANDO SÓ AS VARIAVEIS COM MAIOR ESCALA
subsample2<-with(data,data.frame(time_to_care,
                                time_to_care_rxt,
                                time_to_care_labor,
                                time_to_care_ct,
                                time_to_care_tbis,
                                outcome))
names(subsample2)
subsample2<-data.frame(Injury_Arrival=subsample2$time_to_care,RX=subsample2$time_to_care_rxt,
                       Labor=subsample2$time_to_care_labor,Tomo=subsample2$time_to_care_ct,
                       Surgery=subsample2$time_to_care_tbis,GOS=subsample2$outcome)
names(subsample)

empilhado <- subsample2
empilhado <- gather(empilhado, type, time, -GOS)

empilhado2<- na.omit(empilhado)
empilhado2 <- subset(empilhado2,empilhado2$time<=30)

#calculando prevalências
#ttc2
#ttc_cdmd2 
#ttc_labor2
#ttc_cirurgia 
#ttc_oxigenio 
#ttc_fluidos
#ttc_tomo 
#ttc_rxtorax

mytable <- with(data,table(ttc2,outcome)) # A will be rows, B will be columns 
mytable # print table 

margin.table(mytable, 1) # A frequencies (summed over B) 
margin.table(mytable, 2) # B frequencies (summed over A)

prop.table(mytable) # cell percentages
prop.table(mytable, 1) # row percentages 
prop.table(mytable, 2) # column percentages


##
mytable <- with(data,table(ttc_cdmd2,outcome)) # A will be rows, B will be columns 
mytable # print table 
prop.table(mytable, 1) # row percentages 

mytable <- with(data,table(ttc_labor2,outcome)) # A will be rows, B will be columns 
mytable # print table 
prop.table(mytable, 1) # row percentages 

table(data$ttc_cirurgia)
mytable <- with(data,table(ttc_cirurgia,outcome)) # A will be rows, B will be columns 
mytable # print table 
prop.table(mytable, 1) # row percentages 

mytable <- with(data,table(ttc_oxigenio,outcome)) # A will be rows, B will be columns 
mytable # print table 
prop.table(mytable, 1) # row percentages 

mytable <- with(data,table(ttc_fluidos,outcome)) # A will be rows, B will be columns 
mytable # print table 
prop.table(mytable, 1) # row percentages 

mytable <- with(data,table(ttc_tomo,outcome)) # A will be rows, B will be columns 
mytable # print table 
prop.table(mytable, 1) # row percentages 

mytable <- with(data,table(ttc_rxtorax,outcome)) # A will be rows, B will be columns 
mytable # print table 
prop.table(mytable, 1) # row percentages 

table(data$male_imp)
mytable <- with(data,table(male_imp,outcome)) # A will be rows, B will be columns 
mytable # print table 
prop.table(mytable, 2) # row percentages 

table(data$moi_imp)
mytable <- with(data,table(moi_imp,outcome)) # A will be rows, B will be columns 
mytable # print table 
prop.table(mytable, 1) # row percentages 

table(data$gcs)
mytable <- with(data,table(gcs,outcome)) # A will be rows, B will be columns 
mytable # print table 
prop.table(mytable, 1) # row percentages 

table(data$gcs)
#TABELA POR CATEGORIAS DO GCS#
mytable <- with(data,table(male2,gcs)) # A will be rows, B will be columns 
mytable # print table 
prop.table(mytable, 2) # row percentages 

mytable <- with(data,table(moi_imp,gcs)) # A will be rows, B will be columns 
mytable # print table 
prop.table(mytable, 2) # row percentages 

mytable <- with(data,table(ttc2,gcs)) # A will be rows, B will be columns 
mytable # print table 
prop.table(mytable, 2) # row percentages 

mytable <- with(data,table(ttc_cdmd2,gcs)) # A will be rows, B will be columns 
mytable # print table 
prop.table(mytable, 2) # row percentages 

mytable <- with(data,table(ttc_labor2,gcs)) # A will be rows, B will be columns 
mytable # print table 
prop.table(mytable, 2) # row percentages 

mytable <- with(data,table(ttc_cirurgia,gcs)) # A will be rows, B will be columns 
mytable # print table 
prop.table(mytable, 2) # row percentages 

mytable <- with(data,table(ttc_oxigenio,gcs)) # A will be rows, B will be columns 
mytable # print table 
prop.table(mytable, 2) # row percentages 

mytable <- with(data,table(ttc_fluidos,gcs)) # A will be rows, B will be columns 
mytable # print table 
prop.table(mytable, 2) # row percentages 

mytable <- with(data,table(ttc_tomo,gcs)) # A will be rows, B will be columns 
mytable # print table 
prop.table(mytable, 2) # row percentages 

mytable <- with(data,table(ttc_rxtorax,gcs)) # A will be rows, B will be columns 
mytable # print table 
prop.table(mytable, 2) # row percentages 

mytable <- with(data,table(outcome,gcs)) # A will be rows, B will be columns 
mytable # print table 
prop.table(mytable, 2) # row percentages 
margin.table(mytable, 1) # A frequencies (summed over B) 
margin.table(mytable, 2) # B frequencies (summed over A)


summary(data$heart_rate_imp)
sd(data$heart_rate_imp)
summary(data$pulse_ox_imp)
sd(data$pulse_ox_imp)

with(data,by(heart_rate_imp,gcs,summary))
with(data,by(heart_rate_imp,gcs,sd))

with(data,by(heart_rate_imp,gcs,summary))
with(data,by(heart_rate_imp,gcs,sd))

with(data,by(pulse_ox_imp,gcs,summary))
with(data,by(pulse_ox_imp,gcs,sd))

with(data,by(age_imp,gcs,summary))
with(data,by(age_imp,gcs,sd))

with(data,by(sys_bp_imp,gcs,summary))
with(data,by(sys_bp_imp,gcs,sd))

with(data,by(resp_rate_imp,gcs,summary))
with(data,by(resp_rate_imp,gcs,sd))



with(data,by(age_imp,outcome,summary))
with(data,by(age_imp,outcome,sd))

with(data,by(pulse_ox_imp,outcome,summary))
with(data,by(pulse_ox_imp,outcome,sd))

with(data,by(sys_bp_imp,outcome,summary))
with(data,by(sys_bp_imp,outcome,sd))

with(data,by(heart_rate_imp,outcome,summary))
with(data,by(heart_rate_imp,outcome,sd))

with(data,by(resp_rate_imp,outcome,summary))
with(data,by(resp_rate_imp,outcome,sd))

summary(data$age,data$outcome)
summary(data$pulse_ox_imp,data$outcome)
summary(data$sys_bp_imp,data$outcome)
summary(data$heart_rate_imp,data$outcome)
summary(data$resp_rate_imp,data$outcome)


# algumas outras analises

names(data)
summary(data$age_imp)
sd(data$age_imp)
summary(data$sys_bp_imp)
sd(data$sys_bp_imp)
summary(data$resp_rate_imp)
sd(data$resp_rate_imp)
sd(data$age)
table(data$gcs)
summary(data$gcs)

table(data$ttc2,data$outcome)
table(data$ttc_cdmd2,data$outcome)
table(data$ttc_labor2,data$outcome)
table(data$ttc_rxt2,data$outcome)
table(data$ttc_rxc2,data$outcome)
table(data$ttc_ct2,data$outcome)
table(data$ttc_fluid2,data$outcome)
table(data$ttc_oxyg2,data$outcome)
table(data$ttc_mannit2,data$outcome)
table(data$ttc_tbis2,data$outcome)

table(data$male2,data$outcome)
table(data$moi2,data$outcome)
table(data$gcs,data$outcome)

#boxplot(data$ttc~outcome)


#ANALISE DE SENSIBILIDADE... OU SEJA, ESTRATIFICADA POR CATEGORIAS DA GCS#
table(gcs1$gcs)
gcs1 <- subset(data, data$gcs == "severe" )
summary(gcs1)

gcs1$outcome <- relevel(gcs1$outcome, ref = "bad")
gcs1$ttc2 <-relevel(gcs1$ttc2, ref = "<=1h")
gcs1$ttc_cdmd2 <-relevel(gcs1$ttc_cdmd2, ref = "<=1h")
gcs1$ttc_labor2 <-relevel(gcs1$ttc_labor2, ref = "<=1h")
gcs1$ttc_rxtorax <-relevel(as.factor(gcs1$ttc_rxtorax), ref = "nao_fez_e_nao_precisava")
gcs1$ttc_tomo <-relevel(as.factor(gcs1$ttc_tomo), ref = "nao_fez_e_nao_precisava")
gcs1$ttc_fluidos <-relevel(as.factor(gcs1$ttc_fluidos), ref = "nao_fez_e_nao_precisava")
gcs1$ttc_oxigenio <-relevel(as.factor(gcs1$ttc_oxigenio), ref = "nao_fez_e_nao_precisava")
gcs1$ttc_cirurgia <-relevel(as.factor(gcs1$ttc_cirurgia), ref = "<=4h")


logmodel_gcs<-glm(outcome ~ male_imp,family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

logmodel_gcs<-glm(outcome ~ moi_imp,family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

logmodel_gcs<-glm(outcome ~ ttc2,family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

logmodel_gcs<-glm(outcome ~ ttc_cdmd2,family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

logmodel_gcs<-glm(outcome ~ ttc_labor2,family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

logmodel_gcs<-glm(outcome ~ ttc_rxtorax,family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

logmodel_gcs<-glm(outcome ~ ttc_tomo,family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

logmodel_gcs<-glm(outcome ~ ttc_fluidos,family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

logmodel_gcs<-glm(outcome ~ ttc_oxigenio,family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

logmodel_gcs<-glm(outcome ~ ttc_cirurgia,family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

logmodel_gcs<-glm(outcome ~ age_imp,family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

logmodel_gcs<-glm(outcome ~ sys_bp_imp,family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

logmodel_gcs<-glm(outcome ~ resp_rate_imp,family=binomial, data=data)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

###ADJUSTED

logmodel_gcs<-glm(outcome ~ ttc2 + ttc_cdmd2 + ttc_labor2 + ttc_cirurgia + ttc_oxigenio + ttc_fluidos +
                    ttc_tomo + ttc_rxtorax + male_imp + age_imp + moi_imp + sys_bp_imp + resp_rate_imp,
                  family=binomial, data=gcs1)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 


table(data$ttc_tomo)
logmodel_gcs<-glm(outcome ~ ttc_cirurgia + ttc_oxigenio + ttc_fluidos +
                    male_imp + age_imp + moi_imp + gcs_tot_imp + sys_bp_imp + resp_rate_imp,
                  family=binomial, data=gcs1)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 


logmodel_gcs<-glm(outcome ~ ttc_cirurgia + male_imp + age_imp + moi_imp + gcs_tot_imp + sys_bp_imp + resp_rate_imp,
                  family=binomial, data=gcs1)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 


logmodel_gcs<-glm(outcome ~ ttc_oxigenio + male_imp + age_imp + moi_imp + gcs_tot_imp + sys_bp_imp + resp_rate_imp,
                  family=binomial, data=gcs1)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

logmodel_gcs<-glm(outcome ~ ttc_fluidos + male_imp + age_imp + moi_imp + gcs_tot_imp + sys_bp_imp + resp_rate_imp,
                  family=binomial, data=gcs1)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

logmodel_gcs<-glm(outcome ~ ttc_tomo + male_imp + age_imp + moi_imp + gcs_tot_imp + sys_bp_imp + resp_rate_imp,
                  family=binomial, data=gcs1)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

logmodel_gcs<-glm(outcome ~ ttc_rxtorax + male_imp + age_imp + moi_imp + gcs_tot_imp + sys_bp_imp + resp_rate_imp,
                  family=binomial, data=gcs1)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 


table(data$gcs)
gcs2 <- subset(data, data$gcs == "moderate" )
summary(gcs2)

###ADJUSTED
logmodel_gcs<-glm(outcome ~ ttc2 + ttc_cdmd2 + ttc_labor2 + ttc_cirurgia + ttc_oxigenio + ttc_fluidos +
                    ttc_tomo + ttc_rxtorax + male_imp + age_imp + moi_imp + gcs + sys_bp_imp + resp_rate_imp,
                  family=binomial, data=gcs2)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 


table(data$ttc_tomo)
logmodel_gcs<-glm(outcome ~ ttc_cirurgia + ttc_oxigenio + ttc_fluidos +
                    male_imp + age_imp + moi_imp + gcs_tot_imp + sys_bp_imp + resp_rate_imp,
                  family=binomial, data=gcs2)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 


logmodel_gcs<-glm(outcome ~ ttc_cirurgia + male_imp + age_imp + moi_imp + gcs_tot_imp + sys_bp_imp + resp_rate_imp,
                  family=binomial, data=gcs2)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 


logmodel_gcs<-glm(outcome ~ ttc_oxigenio + male_imp + age_imp + moi_imp + gcs_tot_imp + sys_bp_imp + resp_rate_imp,
                  family=binomial, data=gcs2)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

logmodel_gcs<-glm(outcome ~ ttc_fluidos + male_imp + age_imp + moi_imp + gcs_tot_imp + sys_bp_imp + resp_rate_imp,
                  family=binomial, data=gcs2)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

logmodel_gcs<-glm(outcome ~ ttc_tomo + male_imp + age_imp + moi_imp + gcs_tot_imp + sys_bp_imp + resp_rate_imp,
                  family=binomial, data=gcs2)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

logmodel_gcs<-glm(outcome ~ ttc_rxtorax + male_imp + age_imp + moi_imp + gcs_tot_imp + sys_bp_imp + resp_rate_imp,
                  family=binomial, data=gcs2)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 



table(data$gcs)
gcs3 <- subset(data, gcs = "mild" )
summary(gcs3)

###ADJUSTED
logmodel_gcs<-glm(outcome ~ ttc2 + ttc_cdmd2 + ttc_labor2 + ttc_cirurgia + ttc_oxigenio + ttc_fluidos +
                    ttc_tomo + ttc_rxtorax + male_imp + age_imp + moi_imp + gcs + sys_bp_imp + resp_rate_imp,
                  family=binomial, data=gcs3)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 


table(data$ttc_tomo)
logmodel_gcs<-glm(outcome ~ ttc_cirurgia + ttc_oxigenio + ttc_fluidos +
                    male_imp + age_imp + moi_imp + gcs_tot_imp + sys_bp_imp + resp_rate_imp,
                  family=binomial, data=gcs3)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 


logmodel_gcs<-glm(outcome ~ ttc_cirurgia + male_imp + age_imp + moi_imp + gcs_tot_imp + sys_bp_imp + resp_rate_imp,
                  family=binomial, data=gcs3)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 


logmodel_gcs<-glm(outcome ~ ttc_oxigenio + male_imp + age_imp + moi_imp + gcs_tot_imp + sys_bp_imp + resp_rate_imp,
                  family=binomial, data=gcs3)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

logmodel_gcs<-glm(outcome ~ ttc_fluidos + male_imp + age_imp + moi_imp + gcs_tot_imp + sys_bp_imp + resp_rate_imp,
                  family=binomial, data=gcs3)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

logmodel_gcs<-glm(outcome ~ ttc_tomo + male_imp + age_imp + moi_imp + gcs_tot_imp + sys_bp_imp + resp_rate_imp,
                  family=binomial, data=gcs3)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 

logmodel_gcs<-glm(outcome ~ ttc_rxtorax + male_imp + age_imp + moi_imp + gcs_tot_imp + sys_bp_imp + resp_rate_imp,
                  family=binomial, data=gcs3)
summary(logmodel_gcs)
exp(cbind(Odds=coef(logmodel_gcs),confint(logmodel_gcs,level=0.95))) 


####### FOREST PLOT ########
#TODOS OS 4 GRUPOS
logmodel_gos<-glm(outcome ~ ttc2 + ttc_cdmd2 + ttc_labor2 + ttc_cirurgia + ttc_oxigenio + ttc_fluidos +
                    ttc_tomo + ttc_rxtorax + male_imp + age_imp + moi_imp + gcs + sys_bp_imp + resp_rate_imp,
                  family=binomial, data=data)
summary(logmodel_gos)
exp(cbind(Odds=coef(logmodel_gos),confint(logmodel_gos,level=0.95))) 


logmodel_gos1<-glm(outcome ~ ttc2 + ttc_cdmd2 + ttc_labor2 + ttc_cirurgia + ttc_oxigenio + ttc_fluidos +
                    ttc_tomo + ttc_rxtorax + male_imp + age_imp + moi_imp + gcs + sys_bp_imp + resp_rate_imp,
                  family=binomial, data=gcs1)
summary(logmodel_gos1)
exp(cbind(Odds=coef(logmodel_gos1),confint(logmodel_gos1,level=0.95))) 


logmodel_gos2<-glm(outcome ~ ttc2 + ttc_cdmd2 + ttc_labor2 + ttc_cirurgia + ttc_oxigenio + ttc_fluidos +
                     ttc_tomo + ttc_rxtorax + male_imp + age_imp + moi_imp + gcs + sys_bp_imp + resp_rate_imp,
                   family=binomial, data=gcs2)
summary(logmodel_gos2)
exp(cbind(Odds=coef(logmodel_gos2),confint(logmodel_gos2,level=0.95))) 

logmodel_gos3<-glm(outcome ~ ttc2 + ttc_cdmd2 + ttc_labor2 + ttc_cirurgia + ttc_oxigenio + ttc_fluidos +
                     ttc_tomo + ttc_rxtorax + male_imp + age_imp + moi_imp + gcs + sys_bp_imp + resp_rate_imp,
                   family=binomial, data=gcs3)
summary(logmodel_gos3)
exp(cbind(Odds=coef(logmodel_gos3),confint(logmodel_gos3,level=0.95))) 

# plot_odds<-function(x, title = NULL){
tmp_gos<-data.frame(cbind(exp(coef(logmodel_gos)),
                          exp(confint(logmodel_gos))))

tmp_gos<-tmp_gos[-c(1,28:35),]

tmp_gos1<-data.frame(cbind(exp(coef(logmodel_gos1)),
                           exp(confint(logmodel_gos1))))

tmp_gos1<-tmp_gos1[-c(1,28:35),]

tmp_gos2<-data.frame(cbind(exp(coef(logmodel_gos2)),
                           exp(confint(logmodel_gos2))))

tmp_gos2<-tmp_gos2[-c(1,28:35),]

tmp_gos3<-data.frame(cbind(exp(coef(logmodel_gos3)),
                           exp(confint(logmodel_gos3))))

tmp_gos3<-tmp_gos3[-c(1,28:35),]

#tmp<-rbind(tmp_gos, tmp_gos1, tmp_gos2, tmp_gos3)
odds<-rbind(tmp_gos, tmp_gos1, tmp_gos2, tmp_gos3)
names(odds)<-c('OR', 'lower', 'upper')

odds$vars<-rep(c("Time to Arrival: ≤1h vs. >12h",
                 "Time to Arrival: ≤1h vs. 1.1-4h",
                 "Time to Arrival: ≤1h vs. 4.1-12h",
                 "Time to Physician: ≤1h vs. >4h",
                 "Time to Physician: ≤1h vs. 1.1-4h",
                 "Time to Laboratory: ≤1h vs. >12h",
                 "Time to Laboratory: ≤1h vs. 1.1-4h",
                 "Time to Laboratory: ≤1h vs. 4.1-12h",
                 "Time to Surgery: ≤1h vs. >12h",
                 "Time to Surgery: ≤1h vs. 4.1-12h",
                 "Time to Surgery: ≤1h vs. NA",
                 "Time to Oxygen: ≤1h vs. >1h",
                 "Time to Oxygen: ≤1h vs. no did and no need",
                 "Time to Oxygen: ≤1h vs. no did but need",
                 "Time to Fluids: ≤1h vs. 1.1-4h",
                 "Time to Fluids: ≤1h vs. >4h",
                 "Time to Fluids: ≤1h vs. no did and no need",
                 "Time to Fluids: ≤1h vs. no did but need",
                 "Time to CT: ≤1h vs. 1.1-4h",
                 "Time to CT: ≤1h vs. >4h",
                 "Time to CT: ≤1h vs. no did and no need",
                 "Time to CT: ≤1h vs. no did but need",
                 "Time to X-r: ≤1h vs. 1.1-4h",
                 "Time to X-r: ≤1h vs. >4h",
                 "Time to X-r: ≤1h vs. no did and no need",
                 "Time to X-r: ≤1h vs. no did but need"),4)
#odds$groups<-rep(c(rep("Time to care",4),
#                   "Course points",
#                   "Age", 
#                   "Gender",
#                   "MOI",
#                   "Alcohol use",
#                   rep("District",5)),2)
odds$models<-c(rep("Total sample",26),
               rep("Severe GCS sample",26), 
               rep("Moderate GCS sample",26),
               rep("Mild GCS sample",26))

#ticks<-c(seq(.1, 1, by =.1), seq(0, 10, by =1), seq(10, 100, by =10))


#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("/Users/Joao/Desktop/[GLOBAL EM] tz_prehospitalTBI_figure2.eps",
#           width = 8, height = 6)
grafico<-ggplot(odds, aes(y= OR, x = reorder(vars, OR))) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
  #scale_y_log10(breaks=ticks, labels = ticks) +
  geom_hline(yintercept = 1, linetype=2) +
  scale_x_discrete(limits=c(
    "Time to Arrival: ≤1h vs. >12h",
    "Time to Arrival: ≤1h vs. 1.1-4h",
    "Time to Arrival: ≤1h vs. 4.1-12h",
    "Time to Physician: ≤1h vs. >4h",
    "Time to Physician: ≤1h vs. 1.1-4h",
    "Time to Laboratory: ≤1h vs. >12h",
    "Time to Laboratory: ≤1h vs. 1.1-4h",
    "Time to Laboratory: ≤1h vs. 4.1-12h",
    "Time to Surgery: ≤1h vs. >12h",
    "Time to Surgery: ≤1h vs. 4.1-12h",
    "Time to Surgery: ≤1h vs. NA",
    "Time to Oxygen: ≤1h vs. >1h",
    "Time to Oxygen: ≤1h vs. no did and no need",
    "Time to Oxygen: ≤1h vs. no did but need",
    "Time to Fluids: ≤1h vs. 1.1-4h",
    "Time to Fluids: ≤1h vs. >4h",
    "Time to Fluids: ≤1h vs. no did and no need",
    "Time to Fluids: ≤1h vs. no did but need",
    "Time to CT: ≤1h vs. 1.1-4h",
    "Time to CT: ≤1h vs. >4h",
    "Time to CT: ≤1h vs. no did and no need",
    "Time to CT: ≤1h vs. no did but need",
    "Time to X-r: ≤1h vs. 1.1-4h",
    "Time to X-r: ≤1h vs. >4h",
    "Time to X-r: ≤1h vs. no did and no need",
    "Time to X-r: ≤1h vs. no did but need")) +
  facet_grid(.~models, scales="free_y") +
  coord_flip() +
  labs(x = 'Predictors of TBI Outcomes', y = 'OR (CI 95%)') +
  theme_bw()
# }
dev.off()

grafico



#APENAS UMA GRUPO (AMOSTRA COMPLETA)#
logmodel_gos<-glm(outcome ~ ttc2 + ttc_cdmd2 + ttc_labor2 + ttc_cirurgia + ttc_oxigenio + ttc_fluidos +
                    ttc_tomo + ttc_rxtorax + male_imp + age_imp + moi_imp + gcs + sys_bp_imp + resp_rate_imp,
                  family=binomial, data=data)
summary(logmodel_gos)
exp(cbind(Odds=coef(logmodel_gos),confint(logmodel_gos,level=0.95))) 


# plot_odds<-function(x, title = NULL){
tmp_gos<-data.frame(cbind(exp(coef(logmodel_gos)),
                          exp(confint(logmodel_gos))))

tmp_gos<-tmp_gos[-c(1,28:35),]


#tmp<-rbind(tmp_gos, tmp_gos1, tmp_gos2, tmp_gos3)
odds<-rbind(tmp_gos)
names(odds)<-c('OR', 'lower', 'upper')

odds$vars<-rep(c("Time to Arrival: ≤1h vs. >12h",
                 "Time to Arrival: ≤1h vs. 1.1-4h",
                 "Time to Arrival: ≤1h vs. 4.1-12h",
                 "Time to Physician: ≤1h vs. >4h",
                 "Time to Physician: ≤1h vs. 1.1-4h",
                 "Time to Laboratory: ≤1h vs. >12h",
                 "Time to Laboratory: ≤1h vs. 1.1-4h",
                 "Time to Laboratory: ≤1h vs. 4.1-12h",
                 "Time to Surgery: ≤1h vs. >12h",
                 "Time to Surgery: ≤1h vs. 4.1-12h",
                 "Time to Surgery: ≤1h vs. NA",
                 "Time to Oxygen: ≤1h vs. >1h",
                 "Time to Oxygen: ≤1h vs. no did and no need",
                 "Time to Oxygen: ≤1h vs. no did but need",
                 "Time to Fluids: ≤1h vs. 1.1-4h",
                 "Time to Fluids: ≤1h vs. >4h",
                 "Time to Fluids: ≤1h vs. no did and no need",
                 "Time to Fluids: ≤1h vs. no did but need",
                 "Time to CT: ≤1h vs. 1.1-4h",
                 "Time to CT: ≤1h vs. >4h",
                 "Time to CT: ≤1h vs. no did and no need",
                 "Time to CT: ≤1h vs. no did but need",
                 "Time to X-r: ≤1h vs. 1.1-4h",
                 "Time to X-r: ≤1h vs. >4h",
                 "Time to X-r: ≤1h vs. no did and no need",
                 "Time to X-r: ≤1h vs. no did but need"),1)
#odds$groups<-rep(c(rep("Time to care",4),
#                   "Course points",
#                   "Age", 
#                   "Gender",
#                   "MOI",
#                   "Alcohol use",
#                   rep("District",5)),2)
odds$models<-c(rep("Total sample",26))

#ticks<-c(seq(.1, 1, by =.1), seq(0, 10, by =1), seq(10, 100, by =10))


#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("/Users/Joao/Desktop/[GLOBAL EM] tz_prehospitalTBI_figure2.eps",
#           width = 8, height = 6)
grafico1<-ggplot(odds, aes(y= OR, x = reorder(vars, OR))) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
  #scale_y_log10(breaks=ticks, labels = ticks) +
  geom_hline(yintercept = 1, linetype=2) +
  scale_x_discrete(limits=c(
    "Time to Arrival: ≤1h vs. >12h",
    "Time to Arrival: ≤1h vs. 1.1-4h",
    "Time to Arrival: ≤1h vs. 4.1-12h",
    "Time to Physician: ≤1h vs. >4h",
    "Time to Physician: ≤1h vs. 1.1-4h",
    "Time to Laboratory: ≤1h vs. >12h",
    "Time to Laboratory: ≤1h vs. 1.1-4h",
    "Time to Laboratory: ≤1h vs. 4.1-12h",
    "Time to Surgery: ≤1h vs. >12h",
    "Time to Surgery: ≤1h vs. 4.1-12h",
    "Time to Surgery: ≤1h vs. NA",
    "Time to Oxygen: ≤1h vs. >1h",
    "Time to Oxygen: ≤1h vs. no did and no need",
    "Time to Oxygen: ≤1h vs. no did but need",
    "Time to Fluids: ≤1h vs. 1.1-4h",
    "Time to Fluids: ≤1h vs. >4h",
    "Time to Fluids: ≤1h vs. no did and no need",
    "Time to Fluids: ≤1h vs. no did but need",
    "Time to CT: ≤1h vs. 1.1-4h",
    "Time to CT: ≤1h vs. >4h",
    "Time to CT: ≤1h vs. no did and no need",
    "Time to CT: ≤1h vs. no did but need",
    "Time to X-r: ≤1h vs. 1.1-4h",
    "Time to X-r: ≤1h vs. >4h",
    "Time to X-r: ≤1h vs. no did and no need",
    "Time to X-r: ≤1h vs. no did but need")) +
  facet_grid(.~models, scales="free_y") +
  coord_flip() +
  labs(x = 'Predictors of TBI Outcomes', y = 'OR (CI 95%)') +
  theme_bw()
# }
dev.off()

grafico1



#TODOS OS 3 GRUPOS, according to GCS
logmodel_gos1<-glm(outcome ~ ttc2 + ttc_cdmd2 + ttc_labor2 + ttc_cirurgia + ttc_oxigenio + ttc_fluidos +
                     ttc_tomo + ttc_rxtorax + male_imp + age_imp + moi_imp + gcs + sys_bp_imp + resp_rate_imp,
                   family=binomial, data=gcs1)
summary(logmodel_gos1)
exp(cbind(Odds=coef(logmodel_gos1),confint(logmodel_gos1,level=0.95))) 


logmodel_gos2<-glm(outcome ~ ttc2 + ttc_cdmd2 + ttc_labor2 + ttc_cirurgia + ttc_oxigenio + ttc_fluidos +
                     ttc_tomo + ttc_rxtorax + male_imp + age_imp + moi_imp + gcs + sys_bp_imp + resp_rate_imp,
                   family=binomial, data=gcs2)
summary(logmodel_gos2)
exp(cbind(Odds=coef(logmodel_gos2),confint(logmodel_gos2,level=0.95))) 

logmodel_gos3<-glm(outcome ~ ttc2 + ttc_cdmd2 + ttc_labor2 + ttc_cirurgia + ttc_oxigenio + ttc_fluidos +
                     ttc_tomo + ttc_rxtorax + male_imp + age_imp + moi_imp + gcs + sys_bp_imp + resp_rate_imp,
                   family=binomial, data=gcs3)
summary(logmodel_gos3)
exp(cbind(Odds=coef(logmodel_gos3),confint(logmodel_gos3,level=0.95))) 

# plot_odds<-function(x, title = NULL){
tmp_gos1<-data.frame(cbind(exp(coef(logmodel_gos1)),
                           exp(confint(logmodel_gos1))))

tmp_gos1<-tmp_gos1[-c(1,28:35),]

tmp_gos2<-data.frame(cbind(exp(coef(logmodel_gos2)),
                           exp(confint(logmodel_gos2))))

tmp_gos2<-tmp_gos2[-c(1,28:35),]

tmp_gos3<-data.frame(cbind(exp(coef(logmodel_gos3)),
                           exp(confint(logmodel_gos3))))

tmp_gos3<-tmp_gos3[-c(1,28:35),]

#tmp<-rbind(tmp_gos, tmp_gos1, tmp_gos2, tmp_gos3)
odds<-rbind(tmp_gos1, tmp_gos2, tmp_gos3)
names(odds)<-c('OR', 'lower', 'upper')

odds$vars<-rep(c("Time to Arrival: ≤1h vs. >12h",
                 "Time to Arrival: ≤1h vs. 1.1-4h",
                 "Time to Arrival: ≤1h vs. 4.1-12h",
                 "Time to Physician: ≤1h vs. >4h",
                 "Time to Physician: ≤1h vs. 1.1-4h",
                 "Time to Laboratory: ≤1h vs. >12h",
                 "Time to Laboratory: ≤1h vs. 1.1-4h",
                 "Time to Laboratory: ≤1h vs. 4.1-12h",
                 "Time to Surgery: ≤1h vs. >12h",
                 "Time to Surgery: ≤1h vs. 4.1-12h",
                 "Time to Surgery: ≤1h vs. NA",
                 "Time to Oxygen: ≤1h vs. >1h",
                 "Time to Oxygen: ≤1h vs. no did and no need",
                 "Time to Oxygen: ≤1h vs. no did but need",
                 "Time to Fluids: ≤1h vs. 1.1-4h",
                 "Time to Fluids: ≤1h vs. >4h",
                 "Time to Fluids: ≤1h vs. no did and no need",
                 "Time to Fluids: ≤1h vs. no did but need",
                 "Time to CT: ≤1h vs. 1.1-4h",
                 "Time to CT: ≤1h vs. >4h",
                 "Time to CT: ≤1h vs. no did and no need",
                 "Time to CT: ≤1h vs. no did but need",
                 "Time to X-r: ≤1h vs. 1.1-4h",
                 "Time to X-r: ≤1h vs. >4h",
                 "Time to X-r: ≤1h vs. no did and no need",
                 "Time to X-r: ≤1h vs. no did but need"),3)
#odds$groups<-rep(c(rep("Time to care",4),
#                   "Course points",
#                   "Age", 
#                   "Gender",
#                   "MOI",
#                   "Alcohol use",
#                   rep("District",5)),2)
odds$models<-c(rep("Severe  GCS",26), 
               rep("Moderate GCS ",26),
               rep("Mild  GCS",26))

#ticks<-c(seq(.1, 1, by =.1), seq(0, 10, by =1), seq(10, 100, by =10))


#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("/Users/Joao/Desktop/[GLOBAL EM] tz_prehospitalTBI_figure2.eps",
#           width = 8, height = 6)
grafico4<-ggplot(odds, aes(y= OR, x = reorder(vars, OR))) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
  #scale_y_log10(breaks=ticks, labels = ticks) +
  geom_hline(yintercept = 1, linetype=2) +
  scale_x_discrete(limits=c(
    "Time to Arrival: ≤1h vs. >12h",
    "Time to Arrival: ≤1h vs. 1.1-4h",
    "Time to Arrival: ≤1h vs. 4.1-12h",
    "Time to Physician: ≤1h vs. >4h",
    "Time to Physician: ≤1h vs. 1.1-4h",
    "Time to Laboratory: ≤1h vs. >12h",
    "Time to Laboratory: ≤1h vs. 1.1-4h",
    "Time to Laboratory: ≤1h vs. 4.1-12h",
    "Time to Surgery: ≤1h vs. >12h",
    "Time to Surgery: ≤1h vs. 4.1-12h",
    "Time to Surgery: ≤1h vs. NA",
    "Time to Oxygen: ≤1h vs. >1h",
    "Time to Oxygen: ≤1h vs. no did and no need",
    "Time to Oxygen: ≤1h vs. no did but need",
    "Time to Fluids: ≤1h vs. 1.1-4h",
    "Time to Fluids: ≤1h vs. >4h",
    "Time to Fluids: ≤1h vs. no did and no need",
    "Time to Fluids: ≤1h vs. no did but need",
    "Time to CT: ≤1h vs. 1.1-4h",
    "Time to CT: ≤1h vs. >4h",
    "Time to CT: ≤1h vs. no did and no need",
    "Time to CT: ≤1h vs. no did but need",
    "Time to X-r: ≤1h vs. 1.1-4h",
    "Time to X-r: ≤1h vs. >4h",
    "Time to X-r: ≤1h vs. no did and no need",
    "Time to X-r: ≤1h vs. no did but need")) +
  facet_grid(.~models, scales="free_y") +
  coord_flip() +
  labs(x = 'Wait Times as Predictors of TBI Outcomes', y = 'OR (CI 95%)') +
  theme_bw()
# }
dev.off()

grafico4








#JOÃO TENTANDO... TRÊS GRUPO, ACCORDING TO GCS
logmodel_gos1<-glm(outcome ~ ttc2 + ttc_cdmd2 + ttc_labor2 + ttc_cirurgia + ttc_oxigenio + ttc_fluidos +
                     ttc_tomo + ttc_rxtorax + male_imp + age_imp + moi_imp + gcs + sys_bp_imp + resp_rate_imp,
                   family=binomial, data=gcs1)
summary(logmodel_gos1)
exp(cbind(Odds=coef(logmodel_gos1),confint(logmodel_gos1,level=0.95))) 


logmodel_gos2<-glm(outcome ~ ttc2 + ttc_cdmd2 + ttc_labor2 + ttc_cirurgia + ttc_oxigenio + ttc_fluidos +
                     ttc_tomo + ttc_rxtorax + male_imp + age_imp + moi_imp + gcs + sys_bp_imp + resp_rate_imp,
                   family=binomial, data=gcs2)
summary(logmodel_gos2)
exp(cbind(Odds=coef(logmodel_gos2),confint(logmodel_gos2,level=0.95))) 

logmodel_gos3<-glm(outcome ~ ttc2 + ttc_cdmd2 + ttc_labor2 + ttc_cirurgia + ttc_oxigenio + ttc_fluidos +
                     ttc_tomo + ttc_rxtorax + male_imp + age_imp + moi_imp + gcs + sys_bp_imp + resp_rate_imp,
                   family=binomial, data=gcs3)
summary(logmodel_gos3)
exp(cbind(Odds=coef(logmodel_gos3),confint(logmodel_gos3,level=0.95))) 

# plot_odds<-function(x, title = NULL){
tmp_gos1<-data.frame(cbind(exp(coef(logmodel_gos1)),
                           exp(confint(logmodel_gos1))))

tmp_gos1<-tmp_gos1[-c(1,28:35),]

tmp_gos2<-data.frame(cbind(exp(coef(logmodel_gos2)),
                           exp(confint(logmodel_gos2))))

tmp_gos2<-tmp_gos2[-c(1,28:35),]

tmp_gos3<-data.frame(cbind(exp(coef(logmodel_gos3)),
                           exp(confint(logmodel_gos3))))

tmp_gos3<-tmp_gos3[-c(1,28:35),]

#tmp<-rbind(tmp_gos, tmp_gos1, tmp_gos2, tmp_gos3)
odds<-rbind(tmp_gos1, tmp_gos2, tmp_gos3)
names(odds)<-c('OR', 'lower', 'upper')

odds$vars<-rep(c("Time to Arrival: ≤1h vs. >12h",
                 "Time to Arrival: ≤1h vs. 1.1-4h",
                 "Time to Arrival: ≤1h vs. 4.1-12h",
                 "Time to Physician: ≤1h vs. >4h",
                 "Time to Physician: ≤1h vs. 1.1-4h",
                 "Time to Laboratory: ≤1h vs. >12h",
                 "Time to Laboratory: ≤1h vs. 1.1-4h",
                 "Time to Laboratory: ≤1h vs. 4.1-12h",
                 "Time to Surgery: ≤1h vs. >12h",
                 "Time to Surgery: ≤1h vs. 4.1-12h",
                 "Time to Surgery: ≤1h vs. NA",
                 "Time to Oxygen: ≤1h vs. >1h",
                 "Time to Oxygen: ≤1h vs. no did and no need",
                 "Time to Oxygen: ≤1h vs. no did but need",
                 "Time to Fluids: ≤1h vs. 1.1-4h",
                 "Time to Fluids: ≤1h vs. >4h",
                 "Time to Fluids: ≤1h vs. no did and no need",
                 "Time to Fluids: ≤1h vs. no did but need",
                 "Time to CT: ≤1h vs. 1.1-4h",
                 "Time to CT: ≤1h vs. >4h",
                 "Time to CT: ≤1h vs. no did and no need",
                 "Time to CT: ≤1h vs. no did but need",
                 "Time to X-r: ≤1h vs. 1.1-4h",
                 "Time to X-r: ≤1h vs. >4h",
                 "Time to X-r: ≤1h vs. no did and no need",
                 "Time to X-r: ≤1h vs. no did but need"),3)
#odds$groups<-rep(c(rep("Time to care",4),
#                   "Course points",
#                   "Age", 
#                   "Gender",
#                   "MOI",
#                   "Alcohol use",
#                   rep("District",5)),2)
odds$models<-c(rep("Severe GCS sample",26), 
               rep("Moderate GCS sample",26),
               rep("Mild GCS sample",26))
odds$predictors<-rep(c(rep("Time to RX",4),
                       rep("Time to CT",4),
                       rep("Time to Fluids",4),
                       rep("Time to Oxygen",3),
                       rep("Time to Surgery",3),
                       rep("Time to Laboratory",3),
                       rep("Time do Physician",2),
                       rep("Time to Arrival",3)),3)

#ticks<-c(seq(.1, 1, by =.1), seq(0, 10, by =1), seq(10, 100, by =10))


#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("/Users/Joao/Desktop/[GLOBAL EM] tz_prehospitalTBI_figure2.eps",
#           width = 8, height = 6)
grafico2<-ggplot(odds, aes(y= OR, x = reorder(vars, OR))) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
  #scale_y_log10(breaks=ticks, labels = ticks) +
  geom_hline(yintercept = 1, linetype=2) +
#  scale_x_discrete(limits=c(
#    "Time to Arrival: ≤1h vs. >12h",
#    "Time to Arrival: ≤1h vs. 1.1-4h",
#    "Time to Arrival: ≤1h vs. 4.1-12h",
#    "Time to Physician: ≤1h vs. >4h",
#    "Time to Physician: ≤1h vs. 1.1-4h",
#    "Time to Laboratory: ≤1h vs. >12h",
#    "Time to Laboratory: ≤1h vs. 1.1-4h",
#    "Time to Laboratory: ≤1h vs. 4.1-12h",
#    "Time to Surgery: ≤1h vs. >12h",
#    "Time to Surgery: ≤1h vs. 4.1-12h",
#    "Time to Surgery: ≤1h vs. NA",
#    "Time to Oxygen: ≤1h vs. >1h",
#    "Time to Oxygen: ≤1h vs. no did and no need",
#    "Time to Oxygen: ≤1h vs. no did but need",
#    "Time to Fluids: ≤1h vs. 1.1-4h",
#    "Time to Fluids: ≤1h vs. >4h",
#    "Time to Fluids: ≤1h vs. no did and no need",
#    "Time to Fluids: ≤1h vs. no did but need",
#    "Time to CT: ≤1h vs. 1.1-4h",
#    "Time to CT: ≤1h vs. >4h",
#    "Time to CT: ≤1h vs. no did and no need",
#    "Time to CT: ≤1h vs. no did but need",
#    "Time to X-r: ≤1h vs. 1.1-4h",
#    "Time to X-r: ≤1h vs. >4h",
#    "Time to X-r: ≤1h vs. no did and no need",
#    "Time to X-r: ≤1h vs. no did but need")) +
  coord_flip() +
  labs(x = 'Predictors of TBI Outcomes', y = 'OR (CI 95%)') +
  facet_grid(.~models) +
  theme_bw()
# }
#dev.off()

grafico2


summary(data$pulse_ox_imp)
summary(data$heart_rate_imp)
