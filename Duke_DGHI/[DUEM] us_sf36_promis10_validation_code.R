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
 #install.packages("miP")
 #install.packages("gWidgetsRGtk2")
 #install.packages("mi")
 #install.packages("epicalc")

#Load packages neededz for the analysis
#All packages must be installes with install.packages() function
lapply(c("ggplot2","car", "vcd", "gridExtra","epicalc","gdata",
	"reshape2","dplyr","devtools","reshape","mice","haven",
  "data.table","psych"),
library, character.only=T)

#Package and codes to pull data from goodle sheets
#devtools::install_github("jennybc/googlesheets")
#library(googlesheets)
#suppressMessages(library(dplyr))

######################################################
#IMPORTING DATA
######################################################

# #unused files
# adae<-setDT(read_sas("/Users/Joao/Box Sync/Home Folder jnv4/Data/Global EM/US/snakebites/snakebites_psychometrics/BTG_20160420_Final_adamdata/adae.sas7bdat"))

# write.csv(adae,"/Users/Joao/Desktop/deleteme_adae.csv")

# adcm<-setDT(read_sas("/Users/Joao/Box Sync/Home Folder jnv4/Data/Global EM/US/snakebites/snakebites_psychometrics/BTG_20160420_Final_adamdata/adcm.sas7bdat"))

# write.csv(adcm,"/Users/Joao/Desktop/deleteme_adcm.csv")

# addv<-setDT(read_sas("/Users/Joao/Box Sync/Home Folder jnv4/Data/Global EM/US/snakebites/snakebites_psychometrics/BTG_20160420_Final_adamdata/addv.sas7bdat"))

# write.csv(addv,"/Users/Joao/Desktop/deleteme_addv.csv")

# adex<-setDT(read_sas("/Users/Joao/Box Sync/Home Folder jnv4/Data/Global EM/US/snakebites/snakebites_psychometrics/BTG_20160420_Final_adamdata/adex.sas7bdat"))

# write.csv(adex,"/Users/Joao/Desktop/deleteme_adex.csv")

# adfa<-setDT(read_sas("/Users/Joao/Box Sync/Home Folder jnv4/Data/Global EM/US/snakebites/snakebites_psychometrics/BTG_20160420_Final_adamdata/adfa.sas7bdat"))

# write.csv(adfa,"/Users/Joao/Desktop/deleteme_adfa.csv")

# adlb<-setDT(read_sas("/Users/Joao/Box Sync/Home Folder jnv4/Data/Global EM/US/snakebites/snakebites_psychometrics/BTG_20160420_Final_adamdata/adlb.sas7bdat"))

# write.csv(adlb,"/Users/Joao/Desktop/deleteme_adlb.csv")

# admh<-setDT(read_sas("/Users/Joao/Box Sync/Home Folder jnv4/Data/Global EM/US/snakebites/snakebites_psychometrics/BTG_20160420_Final_adamdata/admh.sas7bdat"))

# write.csv(admh,"/Users/Joao/Desktop/deleteme_admh.csv")

# adpe<-setDT(read_sas("/Users/Joao/Box Sync/Home Folder jnv4/Data/Global EM/US/snakebites/snakebites_psychometrics/BTG_20160420_Final_adamdata/adpe.sas7bdat"))

# write.csv(adpe,"/Users/Joao/Desktop/deleteme_adpe.csv")

# adqs<-setDT(read_sas("/Users/Joao/Box Sync/Home Folder jnv4/Data/Global EM/US/snakebites/snakebites_psychometrics/BTG_20160420_Final_adamdata/adqs.sas7bdat"))

# write.csv(adqs,"/Users/Joao/Desktop/deleteme_adqs.csv")

# adsl<-setDT(read_sas("/Users/Joao/Box Sync/Home Folder jnv4/Data/Global EM/US/snakebites/snakebites_psychometrics/BTG_20160420_Final_adamdata/adsl.sas7bdat"))

# write.csv(adsl,"/Users/Joao/Desktop/deleteme_adsl.csv")

# adtte<-setDT(read_sas("/Users/Joao/Box Sync/Home Folder jnv4/Data/Global EM/US/snakebites/snakebites_psychometrics/BTG_20160420_Final_adamdata/adtte.sas7bdat"))

# write.csv(adtte,"/Users/Joao/Desktop/deleteme_adtte.csv")

# advs<-setDT(read_sas("/Users/Joao/Box Sync/Home Folder jnv4/Data/Global EM/US/snakebites/snakebites_psychometrics/BTG_20160420_Final_adamdata/advs.sas7bdat"))

# write.csv(advs,"/Users/Joao/Desktop/deleteme_advs.csv")

# adya<-setDT(read_sas("/Users/Joao/Box Sync/Home Folder jnv4/Data/Global EM/US/snakebites/snakebites_psychometrics/BTG_20160420_Final_adamdata/adya.sas7bdat"))

# write.csv(adya,"/Users/Joao/Desktop/deleteme_adya.csv")

# adys<-setDT(read_sas("/Users/Joao/Box Sync/Home Folder jnv4/Data/Global EM/US/snakebites/snakebites_psychometrics/BTG_20160420_Final_adamdata/adys.sas7bdat"))

# write.csv(adys,"/Users/Joao/Desktop/deleteme_adys.csv")

# add the path to you computer between " "
data<-read.csv("/Users/Joao/Desktop/Add to box/US_snaekbitePSFS_data.csv",sep=',')

#DASH, PGIC and LEFS
data2<-setDT(read_sas("/Users/Joao/Box/Home Folder jnv4/Data/Global EM/US/snakebites/snakebites_psychometrics/BTG_20160420_Final_adamdata/adqs.sas7bdat"))

#PSFS
data3<-setDT(read_sas("/Users/Joao/Box/Home Folder jnv4/Data/Global EM/US/snakebites/snakebites_psychometrics/BTG_20160420_Final_adamdata/adya.sas7bdat"))

#PSFS Pilot
data4<-setDT(read_sas("/Users/Joao/Box/Home Folder jnv4/Data/Global EM/US/snakebites/snakebites_psychometrics/BTG_Copperhead_Recovery_Pilot_20150903/psfs.sas7bdat"))

#PGIC Pilot
data5<-setDT(read_sas("/Users/Joao/Box/Home Folder jnv4/Data/Global EM/US/snakebites/snakebites_psychometrics/BTG_Copperhead_Recovery_Pilot_20150903/pgic.sas7bdat"))

#SF36/PROMIS
data6<-setDT(read_sas("/Users/Joao/Box/Home Folder jnv4/Data/Global EM/US/snakebites/snakebites_psychometrics/BTG_20160420_Final_adamdata/adys.sas7bdat"))

######################################################
#DATA MANAGEMENT
######################################################

#Organizing PSFS data
######################################################
#subsetting data_3 to isolade the PSFS data only for the 4 data points
data_3_subset0<-subset(data3,data3$AVISIT=="Envenomation +14 Days" |
                             data3$AVISIT=="Envenomation +3 Days"  |
                             data3$AVISIT=="Envenomation +7 Days"  |
                             data3$AVISIT=="Envenomation +10 Days")

# subsetting the data to include only the summed PSFS score
psfs_data_subset_total_temp1<-subset(data_3_subset0,data_3_subset0$PARAM=="PSFS Total Score")

#aggregating a data frame with the total PSFS score (AVAL), time of data collection (AVISIT), subjcet ID  and treatment groups 
psfs_data_subset_total<-with(psfs_data_subset_total_temp1,data.frame(USUBJID,
                                                  TRTP,
                                                  AVAL,
                                                  AVISIT))

#Isolating the id value for dcasting data sets
id<-with(psfs_data_subset_total,
              strsplit(as.character(USUBJID),"-"))
id<-as.data.frame(t(as.data.frame(id)))

id2<-apply(id[,4:5],1,paste, collapse="-")

psfs_data_subset_total$id<-id2

#spreading the dataframe for psychometric analysis
psfs_data_subset_total_casted1 <- dcast(psfs_data_subset_total, 
                          id+USUBJID+TRTP ~ AVISIT,
                                  value.var="AVAL")

colnames(psfs_data_subset_total_casted1)[4]<-"psfs_FUP_10_total"
colnames(psfs_data_subset_total_casted1)[5]<-"psfs_FUP_14_total"
colnames(psfs_data_subset_total_casted1)[6]<-"psfs_FUP_3_total"
colnames(psfs_data_subset_total_casted1)[7]<-"psfs_FUP_7_total"

#Repeating procedures to isolate the score for response option
# getting values for score 1
psfs_data_subset_score1_temp1<-subset(data_3_subset0,data_3_subset0$PARAM=="Score 1")

psfs_data_subset_score1<-with(psfs_data_subset_score1_temp1,data.frame(USUBJID,
                                                  TRTP,
                                                  AVAL,
                                                  AVISIT))

id_score1<-with(psfs_data_subset_score1,
              strsplit(as.character(USUBJID),"-"))

id_score1<-as.data.frame(t(as.data.frame(id_score1)))

id_score12<-apply(id_score1[,4:5],1,paste, collapse="-")

psfs_data_subset_score1$id<-id_score12

psfs_data_subset_score1_casted1 <- dcast(psfs_data_subset_score1, 
                          id+USUBJID+TRTP ~ AVISIT,
                                  value.var="AVAL")

colnames(psfs_data_subset_score1_casted1)[4]<-"psfs_FUP_10_score1"
colnames(psfs_data_subset_score1_casted1)[5]<-"psfs_FUP_14_score1"
colnames(psfs_data_subset_score1_casted1)[6]<-"psfs_FUP_3_score1"
colnames(psfs_data_subset_score1_casted1)[7]<-"psfs_FUP_7_score1"

# getting values for score 2
psfs_data_subset_score2_temp1<-subset(data_3_subset0,data_3_subset0$PARAM=="Score 2")

psfs_data_subset_score2<-with(psfs_data_subset_score2_temp1,data.frame(USUBJID,
                                                  TRTP,
                                                  AVAL,
                                                  AVISIT))

id_score2<-with(psfs_data_subset_score2,
              strsplit(as.character(USUBJID),"-"))
id_score2<-as.data.frame(t(as.data.frame(id_score2)))

id_score22<-apply(id_score2[,4:5],1,paste, collapse="-")

psfs_data_subset_score2$id<-id_score22

psfs_data_subset_score2_casted1 <- dcast(psfs_data_subset_score2, 
                          id+USUBJID+TRTP ~ AVISIT,
                                  value.var="AVAL")

colnames(psfs_data_subset_score2_casted1)[4]<-"psfs_FUP_10_score2"
colnames(psfs_data_subset_score2_casted1)[5]<-"psfs_FUP_14_score2"
colnames(psfs_data_subset_score2_casted1)[6]<-"psfs_FUP_3_score2"
colnames(psfs_data_subset_score2_casted1)[7]<-"psfs_FUP_7_score2"

# getting values for score 3
psfs_data_subset_score3_temp1<-subset(data_3_subset0,data_3_subset0$PARAM=="Score 3")

psfs_data_subset_score3<-with(psfs_data_subset_score3_temp1,data.frame(USUBJID,
                                                  TRTP,
                                                  AVAL,
                                                  AVISIT))

id_score3<-with(psfs_data_subset_score3,
              strsplit(as.character(USUBJID),"-"))
id_score3<-as.data.frame(t(as.data.frame(id_score3)))

id_score32<-apply(id_score3[,4:5],1,paste, collapse="-")

psfs_data_subset_score3$id<-id_score32

psfs_data_subset_score3_casted1 <- dcast(psfs_data_subset_score3, 
                          id+USUBJID+TRTP ~ AVISIT,
                                  value.var="AVAL")

colnames(psfs_data_subset_score3_casted1)[4]<-"psfs_FUP_10_score3"
colnames(psfs_data_subset_score3_casted1)[5]<-"psfs_FUP_14_score3"
colnames(psfs_data_subset_score3_casted1)[6]<-"psfs_FUP_3_score3"
colnames(psfs_data_subset_score3_casted1)[7]<-"psfs_FUP_7_score3"

#Repeating procedures to isolate activity #1
# getting values for activity 1 chosen in the PSFS
data_3_subset_activity_1_temp1<-subset(data3, data3$AVISIT=="Envenomation +3 Days")

psfs_data_subset_data_3_subset_activity_1_temp1<-subset(data_3_subset_activity_1_temp1,data_3_subset_activity_1_temp1$PARAM=="Activity 1")

psfs_data_subset_data_3_subset_activity_1<-with(psfs_data_subset_data_3_subset_activity_1_temp1,data.frame(USUBJID,
                                                  TRTP,
                                                  AVALC,
                                                  AVISIT))

id_data_3_subset_activity_1<-with(psfs_data_subset_data_3_subset_activity_1,
              strsplit(as.character(USUBJID),"-"))
id_data_3_subset_activity_1<-as.data.frame(t(as.data.frame(id_data_3_subset_activity_1)))

id_data_3_subset_activity_12<-apply(id_data_3_subset_activity_1[,4:5],1,paste, collapse="-")

psfs_data_subset_data_3_subset_activity_1$id<-id_data_3_subset_activity_12

psfs_data_subset_data_3_subset_activity_1_casted1 <- dcast(psfs_data_subset_data_3_subset_activity_1, 
                          id+USUBJID+TRTP ~ AVISIT,
                                  value.var="AVALC")

colnames(psfs_data_subset_data_3_subset_activity_1_casted1)[4]<-"psfs_FUP_3_data_3_subset_activity_1"

# getting values for activity 2 chosen in the PSFS
data_3_subset_activity_2_temp1<-subset(data3, data3$AVISIT=="Envenomation +3 Days")

psfs_data_subset_data_3_subset_activity_2_temp1<-subset(data_3_subset_activity_2_temp1,data_3_subset_activity_2_temp1$PARAM=="Activity 2")

psfs_data_subset_data_3_subset_activity_2<-with(psfs_data_subset_data_3_subset_activity_2_temp1,data.frame(USUBJID,
                                                  TRTP,
                                                  AVALC,
                                                  AVISIT))

id_data_3_subset_activity_2<-with(psfs_data_subset_data_3_subset_activity_2,
              strsplit(as.character(USUBJID),"-"))
id_data_3_subset_activity_2<-as.data.frame(t(as.data.frame(id_data_3_subset_activity_2)))

id_data_3_subset_activity_22<-apply(id_data_3_subset_activity_2[,4:5],1,paste, collapse="-")

psfs_data_subset_data_3_subset_activity_2$id<-id_data_3_subset_activity_22

psfs_data_subset_data_3_subset_activity_2_casted1 <- dcast(psfs_data_subset_data_3_subset_activity_2, 
                          id+USUBJID+TRTP ~ AVISIT,
                                  value.var="AVALC")

colnames(psfs_data_subset_data_3_subset_activity_2_casted1)[4]<-"psfs_FUP_3_data_3_subset_activity_2"

# getting values for activity 3 chosen in the PSFS
data_3_subset_activity_3_temp1<-subset(data3, data3$AVISIT=="Envenomation +3 Days")

psfs_data_subset_data_3_subset_activity_3_temp1<-subset(data_3_subset_activity_3_temp1,data_3_subset_activity_3_temp1$PARAM=="Activity 3")

psfs_data_subset_data_3_subset_activity_3<-with(psfs_data_subset_data_3_subset_activity_3_temp1,data.frame(USUBJID,
                                                  TRTP,
                                                  AVALC,
                                                  AVISIT))

id_data_3_subset_activity_3<-with(psfs_data_subset_data_3_subset_activity_3,
              strsplit(as.character(USUBJID),"-"))
id_data_3_subset_activity_3<-as.data.frame(t(as.data.frame(id_data_3_subset_activity_3)))

id_data_3_subset_activity_32<-apply(id_data_3_subset_activity_3[,4:5],1,paste, collapse="-")

psfs_data_subset_data_3_subset_activity_3$id<-id_data_3_subset_activity_32

psfs_data_subset_data_3_subset_activity_3_casted1 <- dcast(psfs_data_subset_data_3_subset_activity_3, 
                          id+USUBJID+TRTP ~ AVISIT,
                                  value.var="AVALC")

colnames(psfs_data_subset_data_3_subset_activity_3_casted1)[4]<-"psfs_FUP_3_data_3_subset_activity_3"

#Combining PSFS data

psfs_data_1<-data.frame(psfs_data_subset_total_casted1,
                        psfs_data_subset_score1_casted1[,-1],
                      psfs_data_subset_data_3_subset_activity_2_casted1[,-1],
                      psfs_data_subset_data_3_subset_activity_3_casted1[,-1],
                      psfs_data_subset_data_3_subset_activity_1_casted1[,-1])

psfs_data_2<-data.frame(psfs_data_subset_score2_casted1,
                      psfs_data_subset_score3_casted1[,-1])

#FINAL DATA FOR PSFS
psfs_data<-merge(psfs_data_1,psfs_data_2, by="id",all.x = TRUE)

#organizing data from the pilos study
#Organizing PSFS data
data4$redcap_event_name<-as.factor(data4$redcap_event_name)
data_4_subset<-subset(data4,data4$redcap_event_name=="Env + 14 Days" |
                             data4$redcap_event_name=="Env + 3 Days" |
                             data4$redcap_event_name=="Env + 7 Days")

data_psfs_pilot<-with(data_4_subset,data.frame(id=subjid,
                                               AVISIT=redcap_event_name,
                                               AVAL=scorres_psfs_total,
                                               USUBJID=subjid))

data_psfs_pilot_casted <- dcast(data_psfs_pilot, 
                          id+USUBJID ~ AVISIT,
                                  value.var="AVAL")

colnames(data_psfs_pilot_casted)[3]<-"psfs_FUP_14"
colnames(data_psfs_pilot_casted)[4]<-"psfs_FUP_3"
colnames(data_psfs_pilot_casted)[5]<-"psfs_FUP_7"

data_psfs_pilot_casted$TRTP<-"pilot"

#merging pilot and rct data

psfs_data_tocombine<-with(psfs_data,data.frame(id,
                                               USUBJID=USUBJID.x,
                                               psfs_FUP_14=psfs_FUP_14_total,
                                               psfs_FUP_3=psfs_FUP_3_total,
                                               psfs_FUP_7=psfs_FUP_7_total,
                                               TRTP=TRTP.x))

data_psfs_combined<-rbind(psfs_data_tocombine,data_psfs_pilot_casted)

# recoding method os administration variable
data$type<-car::recode(data$time,"
            '7days'='In-person PSFS Score';
            '10days'='Telephone PSFS Score';
            '14days'='In-person PSFS Score';
            '17days'='Telephone PSFS Score';
            '21days'='In-person PSFS Score';
            '24days'='Telephone PSFS Score';
            else=NA")




data$time_2measures<-car::recode(data$time,"
            '7days'='T1';
            '10days'='T1';
            '14days'='T2';
            '17days'='T2';
            '21days'='T3';
            '24days'='T3';
            else=NA")

data$time_2measures_2<-car::recode(data$time,"
            '7days'='T1 In person';
            '10days'='T1 Telephone';
            '14days'='T2 In person';
            '17days'='T2 Telephone';
            '21days'='T3 In person';
            '24days'='T3 Telephone';
            else=NA")

# #Kessler
psfs_questions<-with(data,data.frame(q1,q2,q3))

data$score<-rowMeans(psfs_questions)

# #subsetting data set to keep only baseline data
data_validation_paperT0<-data[data$time=="3days",]
data_validation_phoneT0<-data[data$time=="10days",]

data_validationT0<-data[data$time=="3days",]
data_validationT1<-data[data$time=="7days",]
data_validationT2<-data[data$time=="10days",]

#Organize scale datasets

### Temporal stability data

temporal_data_paper<-subset(data,data$time=="21days" | data$time=="28days")

icc_temporal_paper1<-with(temporal_data_paper,data.frame(id,time,score))
icc_temporal_paper<-cast(icc_temporal_paper1,
                          id~time)

temporal_data_phone<-subset(data,data$time=="17days" | data$time=="24days")

icc_temporal_phone1<-with(temporal_data_phone,data.frame(id,time,score))
icc_temporal_phone<-cast(icc_temporal_phone1,
                          id~time)

# ### Consistency between methods of application

icc<-subset(data,data$time=="14days" | data$time=="17days")

icc_data1<-with(icc,data.frame(id,time,score))
icc_data<-cast(data,
                          id~time)

descriptive_data<-subset(data,data$time_2measures=="T1paper" | 
                              data$time_2measures=="T1phone" |
                              data$time_2measures=="T2paper" |
                              data$time_2measures=="T2phone" |
                              data$time_2measures=="T3paper" |
                              data$time_2measures=="T3phone"
                              )

#Organizing PGIC data
data_2_subset1<-subset(data2,data2$PARAMCD=="PGIC1")

data_2_subset2<-subset(data_2_subset1,
                             data_2_subset1$AVISIT=="Envenomation +14 Days")

data_globalchange<-with(data_2_subset2,data.frame(USUBJID,
                                                  AVISIT,
                                                  AVAL))

id<-with(data_globalchange,
              strsplit(as.character(USUBJID),"-"))
id<-as.data.frame(t(as.data.frame(id)))

id2<-apply(id[,4:5],1,paste, collapse="-")

data_globalchange$id<-id2

data_globalchange_casted1 <- dcast(data_globalchange, 
                                  id+USUBJID ~ AVISIT,
                                  value.var="AVAL")

colnames(data_globalchange_casted1)[3]<-"pgic_FUP_14"

#Organizing PGIC data from the pilot study
# data5$redcap_event_name<-as.factor(data5$redcap_event_name)
data_5_subset<-subset(data5,data5$redcap_event_name=="Env + 14 Days")

data_pgic_pilot<-with(data_5_subset,data.frame(id=subjid,
                                               pgic_FUP_14=scorres_pgic_1,
                                               # PGIC2=scorres_pgic_2,
                                               USUBJID=subjid))


#merging pilot and rct data

data_globalchange_casted<-rbind(data_globalchange_casted1,data_pgic_pilot)



#merging PSFS and PGIC data for the clinimetrics assessment
data_mcid<-merge(x = psfs_data_tocombine, 
             y = data_globalchange_casted1, 
             by = "id", 
             all.x = TRUE)

#Calculating changes over time T3-T7
data_mcid$change_score_t7t3<-data_mcid[,5]-data_mcid[,4]

#Calculating changes over time T3-T14
data_mcid$change_score_t14t3<-data_mcid[,3]-data_mcid[,4]

#recoding globa impression of change
data_mcid$change_cat_PGIC1_small<-car::recode(data_mcid$pgic_FUP_14,"1:4='stable';
                                                    5:7='improved'")

data_mcid$change_cat_PGIC1_medium<-car::recode(data_mcid$pgic_FUP_14,"1:5='stable';
                                                    6:7='improved'")

data_mcid$change_cat_PGIC1_large<-car::recode(data_mcid$pgic_FUP_14,"1:6='stable';
                                                    7='improved'")

# data_mcid$change_cat_PGIC2<-car::recode(data_mcid$PGIC2,"0:3='improved';
#                                                     4:6='stable';
#                                                     7:10='stable'")

# data_mcid2<-na.omit(data_mcid[-c(22,79),])

# data_mcid_stable<-subset(data_mcid2,data_mcid2$change_cat_PGIC1_severe=='stable')
# data_mcid_improved<-subset(data_mcid2,data_mcid2$change_cat_PGIC1_severe=='improved')

##Organizing DASH data
####################################

data_2_subset_DASH<-subset(data2,data2$AVISIT=="Envenomation +3 Days" |
                                 data2$AVISIT=="Envenomation +7 Days" |
                                 data2$AVISIT=="Envenomation +14 Days")

# getting total scores
dash_data_subset_total_temp1<-subset(data_2_subset_DASH,data_2_subset_DASH$PARAMCD=="DASHSCOR")

dash_data_subset_total<-with(dash_data_subset_total_temp1,data.frame(USUBJID,
                                                  AVISIT,
                                                  AVAL,
                                                  PARAM))

id<-with(dash_data_subset_total,
              strsplit(as.character(USUBJID),"-"))
id<-as.data.frame(t(as.data.frame(id)))

id2<-apply(id[,4:5],1,paste, collapse="-")

dash_data_subset_total$id<-id2

dash_data_subset_total_casted1 <- dcast(dash_data_subset_total, 
                          id+USUBJID ~ AVISIT,
                                  value.var="AVAL")

colnames(dash_data_subset_total_casted1)[3]<-"dash_FUP_14"
colnames(dash_data_subset_total_casted1)[4]<-"dash_FUP_3"
colnames(dash_data_subset_total_casted1)[5]<-"dash_FUP_7"

#merging PSFS and DASH data for the clinimetrics assessment
data_dash<-merge(x = psfs_data_tocombine, 
             y = dash_data_subset_total_casted1, 
             by = "id", 
             all.y = TRUE)

##Organizing LEFS data
####################################

data_2_subset_LEFS<-subset(data2,data2$AVISIT=="Envenomation +3 Days" |
                                 data2$AVISIT=="Envenomation +7 Days" |
                                 data2$AVISIT=="Envenomation +14 Days")

# getting total scores
lefs_data_subset_total_temp1<-subset(data_2_subset_LEFS,data_2_subset_DASH$PARAMCD=="LEFSSCOR")

lefs_data_subset_total<-with(lefs_data_subset_total_temp1,data.frame(USUBJID,
                                                  AVISIT,
                                                  AVAL,
                                                  PARAM))

id<-with(lefs_data_subset_total,
              strsplit(as.character(USUBJID),"-"))
id<-as.data.frame(t(as.data.frame(id)))

id2<-apply(id[,4:5],1,paste, collapse="-")

lefs_data_subset_total$id<-id2

lefs_data_subset_total_casted1 <- dcast(lefs_data_subset_total, 
                          id+USUBJID ~ AVISIT,
                                  value.var="AVAL")

colnames(lefs_data_subset_total_casted1)[3]<-"lefs_FUP_14"
colnames(lefs_data_subset_total_casted1)[4]<-"lefs_FUP_3"
colnames(lefs_data_subset_total_casted1)[5]<-"lefs_FUP_7"

#merging PSFS and DASH data for the clinimetrics assessment
data_lefs<-merge(x = psfs_data_tocombine, 
             y = lefs_data_subset_total_casted1, 
             by = "id", 
             all.y = TRUE)

##Organizing SF36 data
####################################

#Bodily Pain Score
data_6_subset_BPScore<-subset(data6,data6$AVISIT=="Envenomation +7 Days" |
                                    data6$AVISIT=="Envenomation +14 Days")

# getting total scores
BPScore_data_subset_total_temp1<-subset(data_6_subset_BPScore,data_6_subset_BPScore$PARAMCD=="BPSCO")

BPScore_data_subset_total<-with(BPScore_data_subset_total_temp1,data.frame(USUBJID,
                                                  AVISIT,
                                                  AVAL,
                                                  PARAMCD))

id<-with(BPScore_data_subset_total,
              strsplit(as.character(USUBJID),"-"))
id<-as.data.frame(t(as.data.frame(id)))

id2<-apply(id[,4:5],1,paste, collapse="-")

BPScore_data_subset_total$id<-id2

BPScore_data_subset_total_casted1 <- dcast(BPScore_data_subset_total, 
                          id+USUBJID ~ AVISIT,
                                  value.var="AVAL")

colnames(BPScore_data_subset_total_casted1)[3]<-"BPScore_FUP_14"
colnames(BPScore_data_subset_total_casted1)[4]<-"BPScore_FUP_7"

#merging PSFS and DASH data for the clinimetrics assessment
data_BPScore<-merge(x = psfs_data_tocombine, 
             y = BPScore_data_subset_total_casted1, 
             by = "id", 
             all.y = TRUE)


#General health Score
data_6_subset_GHScore<-subset(data6,data6$AVISIT=="Envenomation +7 Days" |
                                    data6$AVISIT=="Envenomation +14 Days")

# getting total scores
GHScore_data_subset_total_temp1<-subset(data_6_subset_GHScore,data_6_subset_GHScore$PARAMCD=="GHSCO")

GHScore_data_subset_total<-with(GHScore_data_subset_total_temp1,data.frame(USUBJID,
                                                  AVISIT,
                                                  AVAL,
                                                  PARAMCD))

id<-with(GHScore_data_subset_total,
              strsplit(as.character(USUBJID),"-"))
id<-as.data.frame(t(as.data.frame(id)))

id2<-apply(id[,4:5],1,paste, collapse="-")

GHScore_data_subset_total$id<-id2

GHScore_data_subset_total_casted1 <- dcast(GHScore_data_subset_total, 
                          id+USUBJID ~ AVISIT,
                                  value.var="AVAL")

colnames(GHScore_data_subset_total_casted1)[3]<-"GHScore_FUP_14"
colnames(GHScore_data_subset_total_casted1)[4]<-"GHScore_FUP_7"

#merging PSFS and DASH data for the clinimetrics assessment
data_GHScore<-merge(x = psfs_data_tocombine, 
             y = GHScore_data_subset_total_casted1, 
             by = "id", 
             all.y = TRUE)

#Health transition Score
data_6_subset_HTScore<-subset(data6,data6$AVISIT=="Envenomation +7 Days" |
                                    data6$AVISIT=="Envenomation +14 Days")

# getting total scores
HTScore_data_subset_total_temp1<-subset(data_6_subset_HTScore,data_6_subset_HTScore$PARAMCD=="HTSCO")

HTScore_data_subset_total<-with(HTScore_data_subset_total_temp1,data.frame(USUBJID,
                                                  AVISIT,
                                                  AVAL,
                                                  PARAMCD))

id<-with(HTScore_data_subset_total,
              strsplit(as.character(USUBJID),"-"))
id<-as.data.frame(t(as.data.frame(id)))

id2<-apply(id[,4:5],1,paste, collapse="-")

HTScore_data_subset_total$id<-id2

HTScore_data_subset_total_casted1 <- dcast(HTScore_data_subset_total, 
                          id+USUBJID ~ AVISIT,
                                  value.var="AVAL")

colnames(HTScore_data_subset_total_casted1)[3]<-"HTScore_FUP_14"
colnames(HTScore_data_subset_total_casted1)[4]<-"HTScore_FUP_7"

#merging PSFS and DASH data for the clinimetrics assessment
data_HTScore<-merge(x = psfs_data_tocombine, 
             y = HTScore_data_subset_total_casted1, 
             by = "id", 
             all.y = TRUE)

#Mental Helath Score
data_6_subset_MHScore<-subset(data6,data6$AVISIT=="Envenomation +7 Days" |
                                    data6$AVISIT=="Envenomation +14 Days")

# getting total scores
MHScore_data_subset_total_temp1<-subset(data_6_subset_MHScore,data_6_subset_MHScore$PARAMCD=="MHSCO")

MHScore_data_subset_total<-with(MHScore_data_subset_total_temp1,data.frame(USUBJID,
                                                  AVISIT,
                                                  AVAL,
                                                  PARAMCD))

id<-with(MHScore_data_subset_total,
              strsplit(as.character(USUBJID),"-"))
id<-as.data.frame(t(as.data.frame(id)))

id2<-apply(id[,4:5],1,paste, collapse="-")

MHScore_data_subset_total$id<-id2

MHScore_data_subset_total_casted1 <- dcast(MHScore_data_subset_total, 
                          id+USUBJID ~ AVISIT,
                                  value.var="AVAL")

colnames(MHScore_data_subset_total_casted1)[3]<-"MHScore_FUP_14"
colnames(MHScore_data_subset_total_casted1)[4]<-"MHScore_FUP_7"

#merging PSFS and DASH data for the clinimetrics assessment
data_MHScore<-merge(x = psfs_data_tocombine, 
             y = MHScore_data_subset_total_casted1, 
             by = "id", 
             all.y = TRUE)

#Physical Functioning Score
data_6_subset_PFScore<-subset(data6,data6$AVISIT=="Envenomation +7 Days" |
                                    data6$AVISIT=="Envenomation +14 Days")

# getting total scores
PFScore_data_subset_total_temp1<-subset(data_6_subset_PFScore,data_6_subset_PFScore$PARAMCD=="PFSCO")

PFScore_data_subset_total<-with(PFScore_data_subset_total_temp1,data.frame(USUBJID,
                                                  AVISIT,
                                                  AVAL,
                                                  PARAMCD))

id<-with(PFScore_data_subset_total,
              strsplit(as.character(USUBJID),"-"))
id<-as.data.frame(t(as.data.frame(id)))

id2<-apply(id[,4:5],1,paste, collapse="-")

PFScore_data_subset_total$id<-id2

PFScore_data_subset_total_casted1 <- dcast(PFScore_data_subset_total, 
                          id+USUBJID ~ AVISIT,
                                  value.var="AVAL")

colnames(PFScore_data_subset_total_casted1)[3]<-"PFScore_FUP_14"
colnames(PFScore_data_subset_total_casted1)[4]<-"PFScore_FUP_7"

#merging PSFS and DASH data for the clinimetrics assessment
data_PFScore<-merge(x = psfs_data_tocombine, 
             y = PFScore_data_subset_total_casted1, 
             by = "id", 
             all.y = TRUE)

#Role Emotional Score
data_6_subset_REScore<-subset(data6,data6$AVISIT=="Envenomation +7 Days" |
                                    data6$AVISIT=="Envenomation +14 Days")

# getting total scores
REScore_data_subset_total_temp1<-subset(data_6_subset_REScore,data_6_subset_REScore$PARAMCD=="RESCO")

REScore_data_subset_total<-with(REScore_data_subset_total_temp1,data.frame(USUBJID,
                                                  AVISIT,
                                                  AVAL,
                                                  PARAMCD))

id<-with(REScore_data_subset_total,
              strsplit(as.character(USUBJID),"-"))
id<-as.data.frame(t(as.data.frame(id)))

id2<-apply(id[,4:5],1,paste, collapse="-")

REScore_data_subset_total$id<-id2

REScore_data_subset_total_casted1 <- dcast(REScore_data_subset_total, 
                          id+USUBJID ~ AVISIT,
                                  value.var="AVAL")

colnames(REScore_data_subset_total_casted1)[3]<-"REScore_FUP_14"
colnames(REScore_data_subset_total_casted1)[4]<-"REScore_FUP_7"

#merging PSFS and DASH data for the clinimetrics assessment
data_RPScore<-merge(x = psfs_data_tocombine, 
             y = RPScore_data_subset_total_casted1, 
             by = "id", 
             all.y = TRUE)

#Role physical Score
data_6_subset_RPScore<-subset(data6,data6$AVISIT=="Envenomation +7 Days" |
                                    data6$AVISIT=="Envenomation +14 Days")

# getting total scoRPS
RPScore_data_subset_total_temp1<-subset(data_6_subset_RPScore,data_6_subset_RPScore$PARAMCD=="RPSCO")

RPScore_data_subset_total<-with(RPScore_data_subset_total_temp1,data.frame(USUBJID,
                                                  AVISIT,
                                                  AVAL,
                                                  PARAMCD))

id<-with(RPScore_data_subset_total,
              strsplit(as.character(USUBJID),"-"))
id<-as.data.frame(t(as.data.frame(id)))

id2<-apply(id[,4:5],1,paste, collapse="-")

RPScore_data_subset_total$id<-id2

RPScore_data_subset_total_casted1 <- dcast(RPScore_data_subset_total, 
                          id+USUBJID ~ AVISIT,
                                  value.var="AVAL")

colnames(RPScore_data_subset_total_casted1)[3]<-"RPScore_FUP_14"
colnames(RPScore_data_subset_total_casted1)[4]<-"RPScore_FUP_7"

#merging PSFS and DASH data for the clinimetrics assessment
data_RPScore<-merge(x = psfs_data_tocombine, 
             y = RPScore_data_subset_total_casted1, 
             by = "id", 
             all.y = TRUE)

#Social functioning Score
data_6_subset_SFScore<-subset(data6,data6$AVISIT=="Envenomation +7 Days" |
                                    data6$AVISIT=="Envenomation +14 Days")

# getting total scoSFS
SFScore_data_subset_total_temp1<-subset(data_6_subset_SFScore,data_6_subset_SFScore$PARAMCD=="SFSCO")

SFScore_data_subset_total<-with(SFScore_data_subset_total_temp1,data.frame(USUBJID,
                                                  AVISIT,
                                                  AVAL,
                                                  PARAMCD))

id<-with(SFScore_data_subset_total,
              strsplit(as.character(USUBJID),"-"))
id<-as.data.frame(t(as.data.frame(id)))

id2<-apply(id[,4:5],1,paste, collapse="-")

SFScore_data_subset_total$id<-id2

SFScore_data_subset_total_casted1 <- dcast(SFScore_data_subset_total, 
                          id+USUBJID ~ AVISIT,
                                  value.var="AVAL")

colnames(SFScore_data_subset_total_casted1)[3]<-"SFScore_FUP_14"
colnames(SFScore_data_subset_total_casted1)[4]<-"SFScore_FUP_7"

#merging PSFS and DASH data for the clinimetrics assessment
data_SFScore<-merge(x = psfs_data_tocombine, 
             y = SFScore_data_subset_total_casted1, 
             by = "id", 
             all.y = TRUE)

#Vitality Score
data_6_subset_VTScore<-subset(data6,data6$AVISIT=="Envenomation +7 Days" |
                                    data6$AVISIT=="Envenomation +14 Days")

# getting total scoVTS
VTScore_data_subset_total_temp1<-subset(data_6_subset_VTScore,data_6_subset_VTScore$PARAMCD=="VTSCO")

VTScore_data_subset_total<-with(VTScore_data_subset_total_temp1,data.frame(USUBJID,
                                                  AVISIT,
                                                  AVAL,
                                                  PARAMCD))

id<-with(VTScore_data_subset_total,
              strsplit(as.character(USUBJID),"-"))
id<-as.data.frame(t(as.data.frame(id)))

id2<-apply(id[,4:5],1,paste, collapse="-")

VTScore_data_subset_total$id<-id2

VTScore_data_subset_total_casted1 <- dcast(VTScore_data_subset_total, 
                          id+USUBJID ~ AVISIT,
                                  value.var="AVAL")

colnames(VTScore_data_subset_total_casted1)[3]<-"VTScore_FUP_14"
colnames(VTScore_data_subset_total_casted1)[4]<-"VTScore_FUP_7"

#merging PVTS and DASH data for the clinimetrics assessment
data_VTScore<-merge(x = psfs_data_tocombine, 
             y = VTScore_data_subset_total_casted1, 
             by = "id", 
             all.y = TRUE)

##Organizing PROMIS data
####################################

#PROMIS Score
data_6_subset_COMBTSCO<-subset(data6,data6$AVISIT=="Envenomation +7 Days" |
                                    data6$AVISIT=="Envenomation +14 Days")
# getting total scoVTS
COMBTSCO_data_subset_total_temp1<-subset(data_6_subset_COMBTSCO,data_6_subset_COMBTSCO$PARAMCD=="COMBTSCO")

COMBTSCO_data_subset_total<-with(COMBTSCO_data_subset_total_temp1,data.frame(USUBJID,
                                                  AVISIT,
                                                  AVAL,
                                                  PARAMCD))

id<-with(COMBTSCO_data_subset_total,
              strsplit(as.character(USUBJID),"-"))
id<-as.data.frame(t(as.data.frame(id)))

id2<-apply(id[,4:5],1,paste, collapse="-")

COMBTSCO_data_subset_total$id<-id2

COMBTSCO_data_subset_total_casted1 <- dcast(COMBTSCO_data_subset_total, 
                          id+USUBJID ~ AVISIT,
                                  value.var="AVAL")

colnames(COMBTSCO_data_subset_total_casted1)[3]<-"COMBTSCO_FUP_14"
colnames(COMBTSCO_data_subset_total_casted1)[4]<-"COMBTSCO_FUP_7"

#merging PVTS and DASH data for the clinimetrics assessment
data_COMBTSCO<-merge(x = psfs_data_tocombine, 
             y = COMBTSCO_data_subset_total_casted1, 
             by = "id", 
             all.y = TRUE)


# full dataset


data_merged_1 <- merge(data_mcid,data_psfs_combined,all.y=TRUE,by.x="USUBJID.x",by.y="USUBJID")
data_merged_2 <- merge(data_dash,data_merged_1,all.y=TRUE,by.x="USUBJID.x",by.y="USUBJID.x")
data_merged_3 <- merge(data_lefs,data_merged_2,all.y=TRUE,by.x="USUBJID.x",by.y="USUBJID.x")
data_merged_4 <- merge(data_COMBTSCO,data_merged_3,all.y=TRUE,by.x="USUBJID.x",by.y="USUBJID.x")
data_merged_5 <- merge(data_PFScore,data_merged_4,all.y=TRUE,by.x="USUBJID.x",by.y="USUBJID.x")


data_merged_all<-cbind(data_merged_1,data_merged_2,data_merged_3,data_merged_4,data_merged_5)

write.csv(data_merged_all,"/Users/Joao/Desktop/psfs_all_data.csv")

######################################################################
#TABLE 1
######################################################################
###Section wih several exploratory data analysis functions
###### Exploratory Data Anlysis
###### UNIVARIATE

# Numerical descriptives
#summary(data)#This comand will provide a whole set of descriptive #results for each variables
# describe(data_validation$age)
# describe(data_validation$home_people)

# Categorical Descriptives
table<-with(data_mcid,table(change_cat_PGIC1_severe))
table
prop.table(table)

######################################################################
#TABLE 2
######################################################################

#FLOORING AND CEILING EFFECT
######################################################################
# Categorical Descriptives
table<-with(data_psfs_combined,table(psfs_FUP_14))
table
prop.table(table)

# Categorical Descriptives
with(psfs_data_tocombine,psych::describe(psfs_FUP_3))
with(psfs_data_tocombine,psych::describe(psfs_FUP_7))
with(psfs_data_tocombine,psych::describe(psfs_FUP_14))

# Comparison by time

descriptive_data_T1<-subset(data,data$time_2measures=="T1paper" | 
                              data$time_2measures=="T1phone" )

with(descriptive_data_T1,t.test(score~time_2measures))

descriptive_data_T1<-subset(data,data$time_2measures=="T2paper" | 
                              data$time_2measures=="T2phone" )

with(descriptive_data_T1,t.test(score~time_2measures))

descriptive_data_T1<-subset(data,data$time_2measures=="T3paper" | 
                              data$time_2measures=="T3phone" )

with(descriptive_data_T1,t.test(score~time_2measures))


#Reliability
##############################################################

### INTERNAL CONSISTENCY
#RELIABILITY
#psych::alpha(cor_data,n.iter=1000,check.keys=TRUE)
psych::alpha(psfs_data_phone,n.iter=1000,check.keys=TRUE)

psych::alpha(psfs_data_paper,n.iter=1000,check.keys=TRUE)

#### INTER-RATER Agreement
# data_agreement<-with(data,data.frame( ))

# data_sl_agree_model1<-melt(data_sl_temp_model1,id=c("rater","id"))

#TEMPORAL stability

x<-ICC(icc_temporal_paper[,-1])
plot(x)

ICC(na.omit(icc_temporal_phone[,-1]))


#Correlation between both measures

ICC(na.omit(icc_data[,-1]))

library(BlandAltmanLeh)
bland.altman.plot(icc_data[,2], icc_data[,3], main="This is a Bland Altman Plot", 
                        xlab="Means", 
                        ylab="Differences")

pl <- bland.altman.plot(icc_data[,2], icc_data[,3], graph.sys = "ggplot2")
print(pl)

setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
postscript("/Users/Joao/Desktop/figure1_psfstelephone.eps",
  width = 8, height = 6)
bland.altman.plot(icc_data[,2], icc_data[,3], conf.int=.95, pch=19,
                  xlab="Average PSFS for telephone and in-person", ylab="Difference in PSFS between telephone and in-person")
#Add plot
dev.off()

# SEM<-()

cor(na.omit(icc_data[,c(2,3)]))

plot<-ggplot(icc_data, aes(icc_data[,2],icc_data[,3])) +
    geom_point() +    # Use hollow circles
    geom_smooth(method=lm,color="grey50") +
    xlab("In-person PSFS Score") +
    ylab("Telephone PSFS Score") +
    theme_bw()
plot

ggsave("figure2_psfstelephone.eps", #change .eps to .pdf for different format
    plot, #plot is the name of the fig, but the function assumes the last plot if argument is NULL
    path="/Users/Joao/Desktop", #path to save the plot
    width = 6, 
    height = 4, 
    device=cairo_ps) #cairo

#Scores by time boxplot

data_plot<-na.omit(data)

plot<-ggplot(data_plot, aes(time_2measures_2,score,fill=type)) +
    geom_boxplot() + #color=c("grey50","white")) +    # Use hollow circles
    # geom_smooth(method=lm,color="grey50") +
    scale_fill_manual(values=c("grey50", "white"), 
                      # name="Experimental\nCondition",
                       breaks=c("In-person PSFS Score", "Telephone PSFS Score")) +
                      # labels=c("Control", "Treatment 1", "Treatment 2")) +
    xlab("PSFS method of administration") +
    ylab("PSFS Score") +
    # facet_wrap(~type) +
    theme_bw() +
    guides(fill=guide_legend(title=NULL)) +
    theme(legend.position="none")


plot

              # Add a loess smoothed fit curve with confidence region
#> `geom_smooth()` using method = 'loess'

#save figure
ggsave("figure3_psfstelephone.eps", #change .eps to .pdf for different format
    plot, #plot is the name of the fig, but the function assumes the last plot if argument is NULL
    path="/Users/Joao/Desktop", #path to save the plot
    width = 6, 
    height = 4, 
    device=cairo_ps) #cairo_ps is a device to save eps with transparecy

########################################################
#TABLE 3
########################################################

#correlation between PSFS and SF36 Measures
# library(Hmisc)
rcorr(as.matrix(data_BPScore[,c(3,4,5,8,9)]), type="spearman")
rcorr(as.matrix(data_GHScore[,c(3,4,5,8,9)]), type="spearman")
rcorr(as.matrix(data_HTScore[,c(3,4,5,8,9)]), type="spearman")
rcorr(as.matrix(data_MHScore[,c(3,4,5,8,9)]), type="spearman")
rcorr(as.matrix(data_PFScore[,c(3,4,5,8,9)]), type="spearman")
rcorr(as.matrix(data_REScore[,c(3,4,5,8,9)]), type="spearman")
rcorr(as.matrix(data_RPScore[,c(3,4,5,8,9)]), type="spearman")
rcorr(as.matrix(data_SFScore[,c(3,4,5,8,9)]), type="spearman")
rcorr(as.matrix(data_VTScore[,c(3,4,5,8,9)]), type="spearman")

#correlation between PSFS and PROMIS-10
rcorr(as.matrix(data_COMBTSCO[,c(3,4,5,8,9)]), type="spearman")

#correlation between PSFS and LEFS
rcorr(as.matrix(data_lefs[,c(3,4,5,8,9,10)]), type="spearman")

#correlation between PSFS and DASH
rcorr(as.matrix(data_dash[,c(3,4,5,8,9,10)]), type="spearman")

#correlation between PSFS and PGIC
cor_auto(data_mcid2[,c(3,8)])

########################################################
#TABLE 4
########################################################

# MDC - Minimal Detectable Change
#############################################################

clinimetric_data<-subset(data,data$time=="3days" | data$time=="7days")

clinimetric_data_cast1<-with(clinimetric_data,data.frame(X...id,time,score))
icc_clinimetric_data<-cast(clinimetric_data_cast1,
                          X...id~time)

colnames(icc_clinimetric_data)<-c("id","t1","t2")

# Value of one unit above the standard error of a measurement
# if the SE is 2, a 3 indicates the MDC
# Can be calculated based on the standard error of measurement

# SDD – minimum amount of an observed change in a single measure 
# that represents a real change, as distinct from noise. 
# (1.96 × SD of observation difference).
# (http://www.sciencedirect.com/science/article/pii/S1063458406000677)

# The MDC is calculated by multiplying
# the standard error of measurement by the z score associated
# with the desired confidence level and the square root of 2,
# adjusting for sampling from 2 different measures.

# The standard error of measurement is estimated as the pooled standard
# deviation (SDpooled) of pre- and posttreatment assessments multiplied
# by the square root of (1r), where r is the intraclass
# correlation coefficient. 

# The MDC is estimated based on the
# 90% confidence interval (CI) (z1.65). The MDC90 is the most
# common standard used in the literature47 and means that one
# can be 90% confident that a change score at this threshold or
# higher is true and reliable rather than measurement error.

#SEM = SD * sqrt(1-ICC) ~ SD=standard deviation; ICC=intraclass correlation
#MDC = 1.96 * sqrt(2) * SEM ~ 1.96=zscore for 5% sig;
#sqrt2=diff for 2 dependent samples


# Calculate SEM
# install.packages("effsize")
library(effsize)
treatment = rnorm(100,mean=10)
control = rnorm(100,mean=12)
d = (c(treatment,control))
f = rep(c("Treatment","Control"),each=100)
## compute Cohen's d
## treatment and control
cohen.d(treatment,control)
## data and factor
cohen.d(d,f)
## formula interface
cohen.d(d ~ f)
## compute Hedges' g
cohen.d(d,f,hedges.correction=TRUE)

# Pooled SD Pre
# sqrt(sum(var/(sum(N-1)-nrow(dd))))
clinimetric_data2<-icc_clinimetric_data[-23,]
mean_diff_numeric<-clinimetric_data2$t2-clinimetric_data2$t1

# mean_diff_numeric<-c(clinimetric_data2$t2,clinimetric_data2$t1)
# mean_diff_cat<-c(rep("T1",70),
#                  rep("T2",70))

D<-cohen.d(clinimetric_data2$t2,clinimetric_data2$t1,paired=TRUE)

sd_baseline<-sd(clinimetric_data2$t1)
sd_pooled<-sd(mean_diff_numeric)

# mean_diff_pos<-mean_diff[which(mean_diff > -0.1)]

# pooled_sd<-sd(mean_diff_pos)
# pooled_mean<-mean(mean_diff_pos)

# SD difference
# sd1<-sd(icc_clinimetric_data$t1)
# sd2<-sd(icc_clinimetric_data$t2)
# sd3<-sqrt((sd1^2+sd2^2) - 2*cov(icc_clinimetric_data$t1,icc_clinimetric_data$t2))

# ICC
icc<-ICC(cbind(clinimetric_data2$t1,clinimetric_data2$t2))$results$ICC[3]

# Formula for the SEM
SEM<-sd_pooled*sqrt(1-icc)
SEM*1.96

# MDC
MDC<-1.68*sqrt(2)*SEM
MDC

#ANCHOR Based
############################################################

# The anchor-based approach applying a relevant external
# criterion provides meaningful estimates of the measure’s
# MCID18; that is, relating change scores in an instrument to an
# external standard of clinical change (eg, patients’ global ratings
# of change in health) to establish the MCID

# Therefore,
# the anchor-based MCID estimate was calculated as mean
# change score on the NEADL, corresponding to patients defined
# as having MCID; that is, those with a perceived overall change
# score of 5 to 7.5 points (10%–15% of the total scale score
# range) on the ADL/IADL domain of the SIS.

# https://cran.r-project.org/web/packages/MRQoL/MRQoL.pdf
install.packages("MRQoL")
library(MRQoL)
data(dataghs)

# The amount of change that is enough to detect a change in the 
#patient health status
#calculated based on anchors

#Example 1:
#Example to calculate the MCID without effect of Response Sift:
MCID(dataghs$GHS1, dataghs$GHS0, dataghs$anchor1)


#Example 2:
#Example to calculate the MCID with effect of Response Sift:
MCID(data_mcid[4], data_mcid[5], data_mcid$change_cat_PGIC1_mild)

fit_glm <- glm(as.factor(change_cat_PGIC1_severe) ~ data_mcid[,4], 
              data_mcid, family=binomial())

test_data<-with(data_mcid,data.frame(change_cat_PGIC1_severe,change_score))

glm_response_scores <- predict(fit_glm, test_data, type="response")

library(pROC)
roc_curve<-roc(data_mcid$change_cat_PGIC1_severe, 
                                 glm_response_scores, 
  direction="<")#,
     # col="yellow", lwd=3, main="The turtle finds its way")

plot(roc_curve)

coords(roc_curve, "best", ret = "threshold")

roc_curve$thresholds[which.max(roc_curve$sensitivities + roc_curve$specificities)]

library(ROCR)
pred <- prediction(glm_response_scores,as.factor(data_mcid$change_cat_PGIC1_severe))

perf <- performance(pred, "sens", "spec")

plot(perf, avg="threshold",spread.estimate="boxplot")

cutoffs <- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]], 
                      tpr=perf@y.values[[1]])

head(cutoffs)

cutoffs[findInterval(0.5, cutoffs$tpr), 'cut']

cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]
head(subset(cutoffs, fpr < 0.2))

auc = performance(pred, "auc")

library(pROC)
library(Epi)

with(data_mcid,table(change_cat_PGIC1_large))
with(data_mcid,prop.table(table(change_cat_PGIC1_large)))


with(data_mcid,by(data_mcid[,4],change_cat_PGIC1_mild,summary))
with(data_mcid,by(change_score,change_cat_PGIC1_moderate,summary))
with(data_mcid,by(data_mcid[,4],change_cat_PGIC1_moderate,summary))
with(data_mcid,by(change_score,change_cat_PGIC1_severe,summary))
with(data_mcid,by(data_mcid[,4],change_cat_PGIC1_severe,summary))
with(data_mcid,by(change_score,change_cat_PGIC2,summary))
with(data_mcid,by(data_mcid[,4],change_cat_PGIC2,summary))

data_mcid$change_cat_PGIC1_small<-as.factor(data_mcid$change_cat_PGIC1_small)
data_mcid$change_cat_PGIC1_medium<-as.factor(data_mcid$change_cat_PGIC1_medium)
data_mcid$change_cat_PGIC1_large<-as.factor(data_mcid$change_cat_PGIC1_large)

#change from 0 to T3
ROC(form=change_cat_PGIC1_small~psfs_FUP_3, data=data_mcid)
ROC(form=change_cat_PGIC1_medium~psfs_FUP_3, data=data_mcid)
ROC(form=change_cat_PGIC1_large~psfs_FUP_3, data=data_mcid)

#change from T3 to T7
ROC(form=change_cat_PGIC1_small~change_score_t7t3, data=data_mcid[-c(36,37),])
ROC(form=change_cat_PGIC1_medium~change_score_t7t3, data=data_mcid[-c(36,37),])
ROC(form=change_cat_PGIC1_large~change_score_t7t3, data=data_mcid[-c(36,37),])

#change from T3 to t14
ROC(form=change_cat_PGIC1_small~change_score_t14t3, data=data_mcid)
ROC(form=change_cat_PGIC1_medium~change_score_t14t3, data=data_mcid)
ROC(form=change_cat_PGIC1_large~change_score_t14t3, data=data_mcid)

# install.packages("OptimalCutpoints")

library(OptimalCutpoints)

optimal_psfs_FUP3 <- optimal.cutpoints(X = "psfs_FUP_3", 
                                             status = "change_cat_PGIC1_large", 
                                             tag.healthy = "stable",
                                             methods = "Youden", 
                                             data = data_mcid, 
                                             pop.prev = NULL, 
                                             categorical.cov = NULL, #"gender",
                                             control = control.cutpoints(), 
                                             ci.fit = FALSE, 
                                             conf.level = 0.95, 
                                             trace = FALSE)

summary(optimal_psfs_FUP3)


optimal_psfs_FUP3to7 <- optimal.cutpoints(X = "change_score_t7t3", 
                                             status = "change_cat_PGIC1_large", 
                                             tag.healthy = "stable",
                                             methods = "Youden", 
                                             data = data_mcid, 
                                             pop.prev = NULL, 
                                             categorical.cov = NULL, #"gender",
                                             control = control.cutpoints(), 
                                             ci.fit = FALSE, 
                                             conf.level = 0.95, 
                                             trace = FALSE)

# optimal.cutpoint.Youden <- optimal.cutpoints(X = "change_score", 
#                                              status = "change_cat_PGIC1_mild", 
#                                              tag.healthy = "stable",
#                                              methods = "Youden", 
#                                              data = data_mcid_control, 
#                                              pop.prev = NULL, 
#                                              categorical.cov = NULL, #"gender",
#                                              control = control.cutpoints(), 
#                                              ci.fit = FALSE, 
#                                              conf.level = 0.95, 
#                                              trace = FALSE)

summary(optimal_psfs_FUP3to7)

plot(optimal_psfs_FUP3to7)

mcidofchange3to7<-data_mcid$psfs_FUP_7-data_mcid$psfs_FUP_3

mcidof1<-car::recode(mcidofchange3to7,"0:1='below';
                                 else='above'")

table(mcidof1)

externalvaliditymcid<-data.frame(mcidof1,
                                 # data_BPScore,
                                 # data_GHScore,
                                 # data_HTScore,
                                 # data_MHScore,
                                 # data_PFScore,
                                 # data_REScore,
                                 # data_RPScore,
                                 # data_SFScore,
                                 # data_VTScore,
                                 data_COMBTSCO)
                                 # data_lefs,
                                 # data_dash)

#External validity - PROMIS
by(data_COMBTSCO$COMBTSCO_FUP_7,mcidof1,summary)

by(data_COMBTSCO$COMBTSCO_FUP_14,mcidof1,summary)

wilcox.test(data_COMBTSCO$COMBTSCO_FUP_7 ~ mcidof1)
wilcox.test(data_COMBTSCO$COMBTSCO_FUP_14 ~ mcidof1)


boxplot<-data.frame(promis=data_COMBTSCO$COMBTSCO_FUP_7,mcidof1)



library(ggplot2)
# Use single color



ggplot(boxplot, 
      aes(x=mcidof1, 
      y=promis)) +
  geom_boxplot(fill='white', 
           color="grey20",
           alpha=0.5) +
  theme_bw() +
  xlab("PSFS MCID") +
  ylab("PROMIS at 7 Days") +
  # ylim(0, 35) +
  scale_x_discrete(labels=c("Above 1.0","Below 1.0")) #+
  geom_text(aes(label="*P<.05", x=1.5, y=23, label= "boat")) + 
  geom_segment(aes(x=1.2,
             y=20,
             xend=1.8,
             yend=20)) +
  geom_segment(aes(x=1.2,
             y=20,
             xend=1.2,
             yend=18)) +
  geom_segment(aes(x=1.8,
             y=20,
             xend=1.8,
             yend=18)) +
  geom_text(aes(label="B)", x=0.5, y=32))


#External validity - SF36 PF

table(data_PFScore$mcidof1)


data_PFScore$mcidofchange3to7<-data_PFScore$psfs_FUP_7-data_PFScore$psfs_FUP_3

data_PFScore$mcidof1<-car::recode(data_PFScore$mcidofchange3to7,
                                "0:1='below';
                                 else='above'")

by(data_PFScore$PFScore_FUP_7,data_PFScore$mcidof1,summary)
by(data_PFScore$PFScore_FUP_14,data_PFScore$mcidof1,summary)

wilcox.test(data_PFScore$PFScore_FUP_7 ~ data_PFScore$mcidof1)
wilcox.test(data_PFScore$PFScore_FUP_14 ~ data_PFScore$mcidof1)

boxplot<-data.frame(promis=data_PFScore$PFScore_FUP_7,mcidof1=data_PFScore$mcidof1)

library(ggplot2)
# Use single color

ggplot(boxplot, 
      aes(x=mcidof1, 
      y=promis)) +
  geom_boxplot(fill='white', 
           color="grey20",
           alpha=0.5) +
  theme_bw() +
  xlab("PSFS MCID") +
  ylab("SF 36 - Physical Function at 7 days") +
  # ylim(0, 35) +
  scale_x_discrete(labels=c("Above 1.0","Below 1.0")) #+
  geom_text(aes(label="*P<.05", x=1.5, y=23, label= "boat")) #+ 
  # geom_segment(aes(x=1.2,
  #            y=20,
  #            xend=1.8,
  #            yend=20)) +
  # geom_segment(aes(x=1.2,
  #            y=20,
  #            xend=1.2,
  #            yend=18)) +
  # geom_segment(aes(x=1.8,
  #            y=20,
  #            xend=1.8,
  #            yend=18)) +
  # geom_text(aes(label="B)", x=0.5, y=32))

#External validity - LEFS

data_lefs$mcidofchange3to7<-data_lefs$psfs_FUP_7-data_lefs$psfs_FUP_3

data_lefs$mcidof1<-car::recode(data_lefs$mcidofchange3to7,
                                "0:1='below';
                                 else='above'")

by(data_lefs$lefs_FUP_3,data_lefs$mcidof1,summary)
by(data_lefs$lefs_FUP_7,data_lefs$mcidof1,summary)
by(data_lefs$lefs_FUP_14,data_lefs$mcidof1,summary)

wilcox.test(data_lefs$lefs_FUP_3 ~ data_lefs$mcidof1)
wilcox.test(data_lefs$lefs_FUP_7 ~ data_lefs$mcidof1)
wilcox.test(data_lefs$lefs_FUP_14 ~ data_lefs$mcidof1)

boxplot<-data.frame(promis=data_lefs$lefs_FUP_14,mcidof1=data_lefs$mcidof1)

library(ggplot2)
# Use single color

ggplot(boxplot, 
      aes(x=mcidof1, 
      y=promis)) +
  geom_boxplot(fill='white', 
           color="grey20",
           alpha=0.5) +
  theme_bw() +
  xlab("PSFS MCID") +
  ylab("LEFS at 14 days") +
  # ylim(0, 35) +
  scale_x_discrete(labels=c("Above 1.0","Below 1.0")) #+
  # geom_text(aes(label="*P<.05", x=1.5, y=23, label= "boat")) #+ 
  # geom_segment(aes(x=1.2,
  #            y=20,
  #            xend=1.8,
  #            yend=20)) +
  # geom_segment(aes(x=1.2,
  #            y=20,
  #            xend=1.2,
  #            yend=18)) +
  # geom_segment(aes(x=1.8,
  #            y=20,
  #            xend=1.8,
  #            yend=18)) +
  # geom_text(aes(label="B)", x=0.5, y=32))

#External validity - DASH

data_dash$mcidofchange3to7<-data_dash$psfs_FUP_7-data_dash$psfs_FUP_3

data_dash$mcidof1<-car::recode(data_dash$mcidofchange3to7,
                                "0:1='below';
                                 else='above'")

by(data_dash$dash_FUP_3,data_dash$mcidof1,summary)
by(data_dash$dash_FUP_7,data_dash$mcidof1,summary)
by(data_dash$dash_FUP_14,data_dash$mcidof1,summary)

wilcox.test(data_dash$dash_FUP_3 ~ data_dash$mcidof1)
wilcox.test(data_dash$dash_FUP_7 ~ data_dash$mcidof1)
wilcox.test(data_dash$dash_FUP_14 ~ data_dash$mcidof1)

boxplot<-data.frame(promis=data_dash$dash_FUP_14,mcidof1=data_dash$mcidof1)

library(ggplot2)
# Use single color

ggplot(boxplot, 
      aes(x=mcidof1, 
      y=promis)) +
  geom_boxplot(fill='white', 
           color="grey20",
           alpha=0.5) +
  theme_bw() +
  xlab("PSFS MCID") +
  ylab("DASH at 14 days") +
  # ylim(0, 35) +
  scale_x_discrete(labels=c("Above 1.0","Below 1.0")) #+
  # geom_text(aes(label="*P<.05", x=1.5, y=23, label= "boat")) #+ 
  # geom_segment(aes(x=1.2,
  #            y=20,
  #            xend=1.8,
  #            yend=20)) +
  # geom_segment(aes(x=1.2,
  #            y=20,
  #            xend=1.2,
  #            yend=18)) +
  # geom_segment(aes(x=1.8,
  #            y=20,
  #            xend=1.8,
  #            yend=18)) +
  # geom_text(aes(label="B)", x=0.5, y=32))










# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3068975/
# https://www.ncbi.nlm.nih.gov/pubmed/2509379/
# http://rpsychologist.com/d3/cohend/
# https://cran.r-project.org/web/packages/OptimalCutpoints/OptimalCutpoints.pdf
# https://stats.stackexchange.com/questions/29719/how-to-determine-best-cutoff-point-and-its-confidence-interval-using-roc-curve-i
# https://ccrma.stanford.edu/workshops/mir2009/references/ROCintro.pdf

# Distribution based

CohenD<-pooled_mean/pooled_sd

MICD<-0.5*sd_pooled
MICD
MICD<-0.5*sd_baseline
MICD

RCI<-sd_pooled*(sqrt(2*(1-icc)))
RCI

SD_based<-0.5*sd_polled

D_based<-0.5*sd_baseline


t.test(clinimetric_data2$t2,clinimetric_data2$t1,paired=TRUE)
2.609/SEM


mean(data_mcid_improved$change_score)-mean(data_mcid_stable$change_score)

# Cohen suggested
# that score differences of 0.2SD units correspond to
# small but important changes in treatment-effectiveness research

#SRM - Standardized Response Mean - Cohen's D
#############################################################

# The SRM provided measurements of responsiveness
# and was calculated by dividing the mean difference
# in scores of participants by the SD of the change scores.

# Therefore, it was recommended to calculate responsiveness
# for only the improved patients.

# According to the criteria of Cohen,42 SRM
# values of 0.2, 0.5, and 0.8 represent small, moderate, and large
# values for responsiveness, respectively. Bootstrap 1000 samples
# with replacement were used to estimate 95% confidence
# intervals (CIs) for the SRMs.43 R software (Version 2.9.1)a was
# used for statistical computing.

CohenD<-pooled_mean/pooled_sd
CohenD

########################################################
#CONTENT TEXT ANALYSIS
########################################################

library(tm)
library(SnowballC)
library(wordcloud)
library(ggplot2)
library(textreg)

#recodeing activities item 1
psfs_act1<-Corpus(VectorSource(psfs_data$psfs_FUP_3_data_3_subset_activity_1))
#primeiras manipulações

#removing punctuations
psfs_act1<-tm_map(psfs_act1,removePunctuation)

#transforminf upper to lowercase
psfs_act1 <- tm_map(psfs_act1,content_transformer(tolower))

#removing numbers
psfs_act1 <- tm_map(psfs_act1, removeNumbers)

#removing "stopwords" (https://en.wikipedia.org/wiki/Stop_words) 
psfs_act1 <- tm_map(psfs_act1, removeWords, stopwords("en"))

#reducing words to stems
psfs_act1 <- tm_map(psfs_act1, stemDocument, language = "english")

# #casos específicos pt-br
# psfs_act1 <- tm_map(psfs_act1, content_transformer(gsub),
#                pattern = " walk", replacement = "walk")
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "é", replacement = "e")
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "í", replacement = "i")
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "ó", replacement = "o")
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "ú", replacement = "u")
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "ã", replacement = "a")
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "õ", replacement = "o")
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "ç", replacement = "c")
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "ê", replacement = "e")
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "â", replacement = "a")
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "à", replacement = "a")
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "ô", replacement = "o")
# writeLines(as.character(psfs_act1))

psfs_act1_vector <- data.frame(text=sapply(psfs_act1, identity), 
    stringsAsFactors=F)

table(psfs_act1_vector)



psfs_data$psfs_FUP_3_data_3_subset_activity_1_RECODED<-car::recode(psfs_act1_vector$text,"
'put pant' = 'Put on clothes';
'climb stair' = 'Climb stairs';
'shower' = 'Self-care';
'make fist' = 'Higher extremitiy function';
'buckl belt' = 'Put on clothes';
'walk' = 'Walk/run';
'open jar' = 'Eat/Cook';
'spread finger' = 'Higher extremitiy function';
'run' = 'Walk/run';
'footbal' = 'Play/sports';
'tie knot' = 'Put on clothes';
'go step' = 'Climb stairs';
'exercisewalkrun' = 'Walk/run';
'work paint' = 'Work';
'play farm anim' = 'Play/sports';
'walk dog' = 'Walk/run';
'drive manual car' = 'Drive';
'morn exercis' = 'Play/sports';
'walk upstair' = 'Climb stairs';
'work construct' = 'Work';
'work painter' = 'Work';
'jog' = 'Play/sports';
'drive' = 'Drive';
'exercis' = 'Play/sports';
'work nurseri' = 'Work';
'carri heavi item' = 'Carry items';
'take shower abl wash' = 'Self-care';
'stand wash dish' = 'Carry items';
'tenni' = 'Play/sports';
'write' = 'Higher extremitiy function';
'drive car' = 'Drive';
'go stair' = 'Climb stairs';
'use mous work' = 'Work';
'type' = 'Higher extremitiy function';
'use bathroom groom' = 'Self-care';
'put sock shoe' = 'Put on clothes';
'ski' = 'Play/sports';
'light cigarett' = 'Light cigarett';
'get dress' = 'Put on clothes';
'wash hair' = 'Self-care';
'laundri' = 'House care';
'bike ride' = 'Drive';
'put shoe' = 'Put on clothes';
'get bed' = 'Bend down';
'cut veget' = 'Eat/Cook';
'eat' = 'Eat/Cook';
'cook' = 'Eat/Cook';
'grip small object' = 'Higher extremitiy function';
'chang tire' = 'Carry items';
'play basketbal' = 'Play/sports';
'play video game' = 'Play/sports';
'carri object' = 'Carry items'
")

#recodeing activities item 2
psfs_act2<-Corpus(VectorSource(psfs_data$psfs_FUP_3_data_3_subset_activity_2))
#primeiras manipulações

#removing punctuations
psfs_act2<-tm_map(psfs_act2,removePunctuation)

#transforminf upper to lowercase
psfs_act2 <- tm_map(psfs_act2,content_transformer(tolower))

#removing numbers
psfs_act2 <- tm_map(psfs_act2, removeNumbers)

#removing "stopwords" (https://en.wikipedia.org/wiki/Stop_words) 
psfs_act2 <- tm_map(psfs_act2, removeWords, stopwords("en"))

#reducing words to stems
psfs_act2 <- tm_map(psfs_act2, stemDocument, language = "english")

# #casos específicos pt-br
# psfs_act1 <- tm_map(psfs_act1, content_transformer(gsub),
#                pattern = " walk", replacement = "walk")
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "é", replacement = "e")
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "í", replacement = "i")
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "ó", replacement = "o")
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "ú", replacement = "u")
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "ã", replacement = "a")
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "õ", replacement = "o")
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "ç", replacement = "c")
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "ê", replacement = "e")
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "â", replacement = "a")
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "à", replacement = "a")
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "ô", replacement = "o")
# writeLines(as.character(psfs_act1))

psfs_act2_vector <- data.frame(text=sapply(psfs_act2, identity), 
    stringsAsFactors=F)

table(psfs_act2_vector)
unique(psfs_act2_vector)

psfs_data$psfs_FUP_3_data_3_subset_activity_2_RECODED<-car::recode(psfs_act2_vector$text,"
        'brush teeth' = 'Self-care';
        'squat' = 'Lower extremity function';
        'exercis' = 'Play/sports';
        'scratch back' = 'Higher extremitiy function';
        'button shirt' = 'Put on clothes';
        'shower' = 'Self-care';
        'write' = 'Higher extremitiy function';
        'go stair' = 'Climb stairs';
        'sweep floor' = 'House care';
        'swim' = 'Play/sports';
        'dig yard' = 'Play/sports';
        'stand fix hair' = 'Self-care';
        'food prep' = 'Eat/Cook';
        'cookingfood prep' = 'Eat/Cook';
        'yard work' = 'House care';
        'workout' = 'Play/sports';
        'load unload equip' = 'Carry items';
        'prepar breakfast' = 'Eat/Cook';
        'exerciserun' = 'Play/sports';
        'play ball son' = 'Play/sports';
        'walk dog' = 'Walk/run';
        'wash dish' = 'House care';
        'walk' = 'Walk/run';
        'cook' = 'Eat/Cook';
        'construct task' = 'Work';
        'play basketbal' = 'Play/sports';
        'open medicin bottl' = 'Self-care';
        'take trash' = 'House care';
        'golf' = 'Play/sports';
        'drive' = 'Drive';
        'type' = 'Higher extremity function';
        'clean floor' = 'House care';
        'play dog' = 'Play/sports';
        'move foot' = 'Lower extremity function';
        'run' = 'Walk/run';
        'garden' = 'House care';
        'work weight' = 'Work';
        'weightsworkout' = 'Play/sports';
        'pray' = 'Pray';
        'shoe' = 'Put on clothes';
        'drive tractor' = 'Drive';
        'open pill bottl' = 'Self-care';
        'hang cloth line' = 'Put on clothes';
        'swim lap' = 'Play/sports';
        'mow lawn' = 'House care';
        'tie shoe' = 'Put on clothes';
        'use arm drink' = 'Eat/Cook';
        'put boot' = 'Put on clothes';
        'put pant' = 'Put on clothes';
        'collect' = 'House care';
        'pick object' = 'Carry item';
        'tie shoelac' = 'Put on clothes';
        'grip cup' = 'Eat/Coord';
        'move hand freeli play' = 'Higher extremity function'
    ")


#recodeing activities item 3
psfs_act3<-Corpus(VectorSource(psfs_data$psfs_FUP_3_data_3_subset_activity_3))
#primeiras manipulações

#removing punctuations
psfs_act3<-tm_map(psfs_act3,removePunctuation)

#transforminf upper to lowercase
psfs_act3 <- tm_map(psfs_act3,content_transformer(tolower))

#removing numbers
psfs_act3 <- tm_map(psfs_act3, removeNumbers)

#removing "stopwords" (https://en.wikipedia.org/wiki/Stop_words) 
psfs_act3 <- tm_map(psfs_act3, removeWords, stopwords("en"))

#reducing words to stems
psfs_act3 <- tm_map(psfs_act3, stemDocument, language = "english")

# #casos específicos pt-br
# psfs_act1 <- tm_map(psfs_act1, content_transformer(gsub),
#                pattern = " walk", replacement = "walk")
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "é", replacement = "e")
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "í", replacement = "i")
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "ó", replacement = "o")
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "ú", replacement = "u")
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "ã", replacement = "a")
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "õ", replacement = "o")
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "ç", replacement = "c")
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "ê", replacement = "e")
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "â", replacement = "a")
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "à", replacement = "a")
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "ô", replacement = "o")
# writeLines(as.character(psfs_act1))

psfs_act3_vector <- data.frame(text=sapply(psfs_act3, identity), 
    stringsAsFactors=F)

table(psfs_act3_vector)
unique(psfs_act3_vector)

psfs_data$psfs_FUP_3_data_3_subset_activity_3_RECODED<-car::recode(psfs_act3_vector$text,"
              'shower'='Self-care';
              'walk'='Walk/run';
              'go toilet'='Self-care';
              'hold tool'='Work';
              'put pant'='Work';
              'turn key'='Higher extremity function';
              'get'='Carry item';
              'get bathtub'='Self-care';
              'open lid'='Higher extremity function';
              'pe class'='Play/sports';
              'drive'='Drive';
              'drive hand wheel'='Drive';
              'walk unassist'='Walk/run';
              'drive car'='Drive';
              'yard work'='House care';
              'food prep'='Eat/Cook';
              'garden'='House care';
              'run'='Walk/run';
              'walk stair'='Climb stairs';
              'hous chore'='House care';
              'cook'='Eat/Cook';
              'walk school'='Walk/run';
              'hike'='Play/sports';
              'shop'='Shop';
              'draw'='Higher extremity function';
              'go stair'='Climb stairs';
              'run walk behind lawn mower'='Walk/run';
              'type keyboard'='Higher extremity function';
              'tub'='Self-care';
              'walk dog'='Walk/run';
              'swim'='Play/sports';
              'door handl turn'='Higher extremity function';
              'move'='Walk/run';
              'make dinner'='Eat/Cook';
              'work'='Work';
              'button shirt'='Put on clothes';
              'get bed'='Self-care';
              'pullup'='Play/sports';
              'stair'='Climb stairs';
              'teethbrushingcook'='Self-care';
              'hold heavi object'='Carry items';
              'jet ski'='Play/sports';
              'workout'='Play/sports';
              'work nurs'='Work';
              'write'='Higher extremity function';
              'text'='Higher extremity function';
              'fish'='Play/sports';
              'walk updown stair'='';
              'go bathroom'='Self-care';
              'put shirt'='Put on clothes';
              'use cell phone'='Higher extremity function';
              'exercis'='Play/sports';
              'get dress'='Put on clothes';
              'collect'='Carry items';
              'small motor function'='Higher extremity function';
              'lift car door handl'='Higher extremity function';
              'walkingget around normal'='Walk/run';
              'pick box'='Carry items';
              'throw object'='Higher extremity function'
    ")


list<-rbind(unique(psfs_act1_vector),unique(psfs_act2_vector),unique(psfs_act3_vector))

# list<-as.data.frame(list)

#Graph data
content_graph_data<-NULL
content_graph_data$content<-with(psfs_data,c(
    psfs_FUP_3_data_3_subset_activity_1_RECODED,
    psfs_FUP_3_data_3_subset_activity_2_RECODED,
    psfs_FUP_3_data_3_subset_activity_3_RECODED))

content_graph_data$item<-c(rep("Item 1",length(psfs_data[,1])),
                           rep("Item 2",length(psfs_data[,1])),
                           rep("Item 3",length(psfs_data[,1]))
                          )

content_graph_data<-as.data.frame(content_graph_data)
 

plot_data<-plyr::count(content_graph_data, vars=c("content","item"))

plot_data$prop<-(plot_data$freq/73)*100

unique(plot_data$content)

limits0<-plot_data[plot_data$item=="Item 1",]
limits<-unique(limits0$content)[order(limits0$prop)]

# Library
library(tidyverse)
 
# Create data
# data=data.frame(x=LETTERS[1:26], y=abs(rnorm(26)))
 
# 1 - Custom markers (left)
# note: shape = integer between 0 and 25
# note: stroke exists only for shapes between 1 and 24
ggplot(data, aes(x=x, y=y)) +
  geom_segment( aes(x=x, xend=x, y=0, yend=y)) +
  geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2)
 
# 2 - Custom stems (right)
# note: size is the width in mm
# note: style can be in: "blank", "solid", "dashed", "dotted", "dotdash", "longdash","twodash"
ggplot(data, aes(x=x, y=y)) +
  geom_segment( aes(x=x, xend=x, y=0, yend=y) , size=1, color="blue", linetype="dotted" ) +
  geom_point()


ggplot(plot_data, aes(x=content, y=prop)) +
  geom_segment( aes(x=content, xend=content, y=0, yend=prop), color="grey") +
  geom_point( color="orange", size=4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()) +
  xlab("") +
  ylab("Value of Y") +
  facet_grid(~item) +
  coord_flip() +
  scale_y_discrete(breaks=limits(plot_data$content),
                      labels=c("Control", "Treat 1", "Treat 2"))



content<-with(psfs_data,c(
    psfs_FUP_3_data_3_subset_activity_1_RECODED,
    psfs_FUP_3_data_3_subset_activity_2_RECODED,
    psfs_FUP_3_data_3_subset_activity_3_RECODED))

content_2<-car::recode(content,"
          'Bend down' = 'Daily activities';
          'Carry item' = 'Daily activities';
          'Carry items' = 'Daily activities';
          'Climb stairs' = 'Daily activities';
          'Drive' = 'Drive';
          'Eat/Cook' = 'Eat/Cook';
          'Eat/Coord' = 'Eat/Cook';
          'Higher extremitiy function' = 'Body movement';
          'Higher extremity function' = 'Body movement';
          'House care' = 'Daily activities';
          'Light cigarett' = 'Body movement';
          'Lower extremity function' = 'Body movement';
          'Play/sports' = 'Play/sports';
          'Pray' = 'Play/sports';
          'Put on clothes' = 'Daily activities';
          'Self-care' = 'Daily activities';
          'Shop' = 'Daily activities';
          'Walk/run' = 'Play/sports';
          'Work' = 'Work';
          else='Daily activities'"
          )

table(content_2)
prop.table(table(content_2))
