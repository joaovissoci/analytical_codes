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
 # install.packages("tidyverse")

#Load packages neededz for the analysis
#All packages must be installes with install.packages() function
lapply(c("sem","ggplot2", "psych", "RCurl", "irr", "nortest", 
  "moments","GPArotation","nFactors","boot","psy", "car",
  "vcd", "gridExtra","mi","VIM","epicalc","gdata","sqldf",
  "reshape2","mclust","foreign","survival","memisc","lme4",
  "lmerTest","dplyr","QCA","VennDiagram","qgraph","igraph",
  "ltm","gmodels","eRm","mirt","dplyr","devtools","reshape",
  "mice"),
library, character.only=T)

#Package and codes to pull data from goodle sheets
#devtools::install_github("jennybc/googlesheets")
#library(googlesheets)
#suppressMessages(library(dplyr))

######################################################
#IMPORTING DATA
######################################################

# install.packages("jsonlite")
library(jsonlite)
file <- file("/Users/Joao/Downloads/export_data.txt",
             open="r",
             encoding="UTF-8-BOM")

df <- read.delim(file=file,
                 header=FALSE,
                 fill=FALSE,
                 stringsAsFactors=FALSE,
                 na.strings=c("NULL", ""),
                 quote="",
                 colClasses=c("integer", "character", "factor", "POSIXct", "POSIXct", "integer", "integer", "character"))
close(file)
rm(file)

dataTransform <- function(df) {
  # Parses JSON string into data frames
  json_data <- lapply(df$V8, fromJSON)
  
  # Transpose data frames, dropping all columns except label and value
  json_data <- lapply(json_data, function(df) {
    l <- lapply(df$value, function(x) if (length(x) != 1) I(list(x)) else x)
    names(l) <- trimws(df$label)
    data.frame(l, stringsAsFactors = FALSE)
  })

  # All one-line data frames must have the same columns
  labels <- unique(c(sapply(json_data, colnames)))
  for (i in seq_along(json_data)) {
    json_data[[i]][setdiff(labels, colnames(json_data[[i]]))] <- NA
  }
  
  # Binds rows from all one-line data frames
  json_data <- do.call(rbind, json_data)
  
  # Binds columns back to original data frame
  df_complete <- cbind(df, json_data)
}

df_hdrs <- dataTransform(df[df$V3 == "HDRS",])
df_phq9 <- dataTransform(df[df$V3 == "PHQ9",])
df_questionnaire <- dataTransform(df[df$V3 == "Questionnaire",])
df_short_questionnaire <- dataTransform(df[df$V3 == "ShortQuestionnaire",])
rm(dataTransform)

######################################################
#DATA MANAGEMENT
######################################################

str(df_hdrs)
write.csv(df_short_questionnaire[,1],"/Users/joaovissoci/Desktop/deleteme1.csv")
dim(df_short_questionnaire)
View(df_short_questionnaire)
names(df_phq9)

#Organizing PHQ9 data
 colnames(df_phq9)<-c("id",
                      "deleteme1",
                      "scale",
                      "T1",
                      "T2",
                      "deleteme2",
                      "phq9_score",
                      "deleteme3",
                      "deleteme4",
                      "phq1",
                      "phq2",
                      "phq3",
                      "phq4",
                      "phq5",
                      "phq6",
                      "phq7",
                      "phq8",
                      "phq9",
                      "deleteme5")


phq9_data<-remove.vars(df_phq9,c(
                                "deleteme1",
                                "deleteme2",
                                "deleteme3",
                                "deleteme4",
                                "deleteme5"))

str(phq9_data)

phq9_data$phq1_num<-car::recode(phq9_data$phq1,"
                    'Nta na rimwe'=0;
                    'Rimwe na rimwe'=1;
                    ''=NA;
                    NA=NA;
                    'Hafi ya buri munsi'=3;
                    else=2
                    ")

phq9_data$phq2_num<-car::recode(phq9_data$phq2,"
                    'Nta na rimwe'=0;
                    'Rimwe na rimwe'=1;
                    ''=NA;
                    NA=NA;
                    'Hafi ya buri munsi'=3;
                    else=2
                    ")

phq9_data$phq3_num<-car::recode(phq9_data$phq3,"
                    'Nta na rimwe'=0;
                    'Rimwe na rimwe'=1;
                    ''=NA;
                    NA=NA;
                    'Hafi ya buri munsi'=3;
                    else=2
                    ")

phq9_data$phq4_num<-car::recode(phq9_data$phq4,"
                    'Nta na rimwe'=0;
                    'Rimwe na rimwe'=1;
                    ''=NA;
                    NA=NA;
                    'Hafi ya buri munsi'=3;
                    else=2
                    ")

phq9_data$phq5_num<-car::recode(phq9_data$phq5,"
                    'Nta na rimwe'=0;
                    'Rimwe na rimwe'=1;
                    ''=NA;
                    NA=NA;
                    'Hafi ya buri munsi'=3;
                    else=2
                    ")

phq9_data$phq6_num<-car::recode(phq9_data$phq6,"
                    'Nta na rimwe'=0;
                    'Rimwe na rimwe'=1;
                    ''=NA;
                    NA=NA;
                    'Hafi ya buri munsi'=3;
                    else=2
                    ")

phq9_data$phq7_num<-car::recode(phq9_data$phq7,"
                    'Nta na rimwe'=0;
                    'Rimwe na rimwe'=1;
                    ''=NA;
                    NA=NA;
                    'Hafi ya buri munsi'=3;
                    else=2
                    ")

phq9_data$phq8_num<-car::recode(phq9_data$phq8,"
                    'Nta na rimwe'=0;
                    'Rimwe na rimwe'=1;
                    ''=NA;
                    NA=NA;
                    'Hafi ya buri munsi'=3;
                    else=2
                    ")

phq9_data$phq9_num<-car::recode(phq9_data$phq9,"
                    'Nta na rimwe'=0;
                    'Rimwe na rimwe'=1;
                    ''=NA;
                    NA=NA;
                    'Hafi ya buri munsi'=3;
                    else=2
                    ")

phq9_data$phq9_sum<-with(phq9_data,rowSums(data.frame(phq1_num,
                            phq2_num,
                            phq3_num,
                            phq4_num,
                            phq5_num,
                            phq6_num,
                            phq7_num,
                            phq8_num,
                            phq9_num)))

data.frame(phq9_data$phq9_sum,phq9_data$phq9_score)

phq9_data2 <- phq9_data[!duplicated(phq9_data$id), ]

phq9_data3 <- phq9_data[duplicated(phq9_data$id), ]

icc_phq9<-merge(phq9_data3, 
     phq9_data2,
     all.x = TRUE,by=c("id"))

#Organizing HDRS data

df_hdrs<-df_hdrs[,1:26]

 colnames(df_hdrs)<-c("id",
                      "deleteme1",
                      "scale",
                      "T1",
                      "T2",
                      "deleteme2",
                      "hrds_score",
                      "deleteme3",
                      "deleteme4",
                      "hdrs1",
                      "hdrs2",
                      "hdrs3",
                      "hdrs4",
                      "hdrs5",
                      "hdrs6",
                      "hdrs7",
                      "hdrs8",
                      "hdrs9",
                      "hdrs10",
                      "hdrs11",
                      "hdrs12",
                      "hdrs13",
                      "hdrs14",
                      "hdrs15",
                      "hdrs16",
                      "hdrs17")


hdrs_data<-remove.vars(df_hdrs,c(
                                "deleteme1",
                                "deleteme2",
                                "deleteme3",
                                "deleteme4"))

str(hdrs_data)

hdrs_data$hdrs1_num<-car::recode(hdrs_data$hdrs1,"
                    'Ntabyo'=0;
                    'Ayo marangamutima agaragarira gusa mu ibazwa'=1;
                    'Ayo marangamutima aboneka gusa mu magambo uvuga utiriwe ubitekerezaho'=2;
                    'Ayo marangamutima uyagaragaza utavuga, urugero, ku maso, ku mubiri mu ijwi cyangwa se ushaka kurira'=3;
                    else=4
                    ")

hdrs_data$hdrs2_num<-car::recode(hdrs_data$hdrs2,"
                    'Ntayo'=0;
                    'Kwishinja wumva ko wahemukiye abandi'=1;
                    'Ibitekerezo byo kwishinja, gutinda ku makosa ya kera cyangwa ku bikorwa bigayitse'=2;
                    'Uburwayi bwawe ni igihano, ibitekerezo bitari ukuri byo kwishinja'=3;
                    else=4
                    ")

hdrs_data$hdrs3_num<-car::recode(hdrs_data$hdrs3,"
                    'Nta byo'=0;
                    'Kumva ko kubaho nta cyo bimaze'=1;
                    'Kwifuza gupfa cyangwa ibisa na byo. Igitekerezo cyose kiganisha ku rupfu rwawe'=2;
                    'Ibitekerezo cyangwa, imyiteguro yo kwiyahura'=3;
                    'Kugerageza kwiyahura. (Kubigerageza uko ari ko kose ni amanota ane)'=4
                    ")

hdrs_data$hdrs4_num<-car::recode(hdrs_data$hdrs4,"
                    'Nta kibazo cyo gusinzira'=0;
                    'Kubura ibitotsi rimwe na rimwe nko hejuru y’igice cy’isaha'=1;
                    'Kubura ibitotsi buri mugoroba'=2
                    ")

hdrs_data$hdrs5_num<-car::recode(hdrs_data$hdrs5,"
                    'Nta kibazo cyo gusinzira'=0;
                    'Gushikagurika kandi ukabura amahoro nijoro'=1;
                    'Kubyuka mu ijoro (Kuva mu gitanda kose ni amanota keretse kujya kwituma)'=2
                    ")

hdrs_data$hdrs6_num<-car::recode(hdrs_data$hdrs6,"
                    'Nta kibazo gihari'=0;
                    'Gukanguka bwenda gucya ariko ukongera ugasinzira'=1;
                    'Ntibishoboka kongera gusinzira iyo uvuye mu buriri'=2
                    ")

hdrs_data$hdrs7_num<-car::recode(as.numeric(as.factor(hdrs_data$hdrs7)),"
                    4=0;
                    1=1;
                    3=2;
                    2=3;
                    5=4
                    ")

hdrs_data$hdrs8_num<-car::recode(hdrs_data$hdrs8,"
                    'Imivugire n’imitekerereze bisanzwe'=0;
                    'Umwete muke ugaragarira mu biganiro'=1;
                    'Kuzarira gukabije mu biganiro'=2;
                    'Ikiganiro kigoye'=3;
                    'Kugwa mu kantu'=4
                    ")

hdrs_data$hdrs9_num<-car::recode(as.numeric(as.factor(hdrs_data$hdrs9)),"
                    1=0;
                    4=1;
                    3=2;
                    2=3;
                    5=4
                    ")

hdrs_data$hdrs10_num<-car::recode(hdrs_data$hdrs10,"
                    'Nta bwo'=0;
                    'Kudatuza kudasobanutse no kugira umunabi'=1;
                    'Uhangayikishwa n’utubazo duto'=2;
                    'Imyitwarire iteye impungenge (mu mvugo no mu ngiro) '=3;
                    'Ubwoba bugaragara utaniriwe ubaza'=4
                    ")

hdrs_data$hdrs11_num<-car::recode(as.numeric(as.factor(hdrs_data$hdrs11)),"
                    6=0;
                    5=1;
                    2=2;
                    4=3;
                    3=4;
                    1=NA
                    ")

hdrs_data$hdrs12_num<-car::recode(as.numeric(as.factor(hdrs_data$hdrs12)),"
                    3=0;
                    2=1;
                    4=2;
                    1=4;
                    ")

hdrs_data$hdrs13_num<-car::recode(as.numeric(as.factor(hdrs_data$hdrs13)),"
                    2=0;
                    1=1;
                    3=2
                    ")

hdrs_data$hdrs14_num<-car::recode(hdrs_data$hdrs14,"
                    'Nta byo Ibimenyetso bishoboka'=0;
                    'Byoroheje'=1;
                    'Birakabije'=2
                    ")

hdrs_data$hdrs15_num<-car::recode(as.numeric(as.factor(hdrs_data$hdrs15)),"
                    5=0;
                    3=1;
                    1=2;
                    2=3;
                    4=4
                    ")

hdrs_data$hdrs16_num<-car::recode(as.numeric(as.factor(hdrs_data$hdrs16)),"
                    3=0;
                    1=1;
                    2=2
                                        ")

hdrs_data$hdrs17_num<-car::recode(hdrs_data$hdrs17,"
                    'Wemera ko wihebye kandi urwaye'=0;
                    'Wiyiziho uburwayi ariko ushakira impamvu mu mirire mibi, imihindagurikire y’ikirere, akazi kenshi agakoko gatera uburwayi mu mubiri, gukenera ikiruhuko n’ibindi'=1;
                    'Ntiwemera na gato ko urwaye bwe'=2
                                        ")

hdrs_data$hdrs_sum<-with(hdrs_data,rowSums(data.frame(hdrs1_num,
                            hdrs2_num,
                            hdrs3_num,
                            hdrs4_num,
                            hdrs5_num,
                            hdrs6_num,
                            hdrs7_num,
                            hdrs8_num,
                            hdrs9_num,
                            hdrs10_num,
                            hdrs11_num,
                            hdrs12_num,
                            hdrs13_num,
                            hdrs14_num,
                            hdrs15_num,
                            hdrs16_num,
                            hdrs17_num)))

data.frame(hdrs_data$hdrs_sum,hdrs_data$hrds_score)

hdrs_data2 <- hdrs_data[!duplicated(hdrs_data$id), ]

#Organizing SES questionnaire data

socio_data<-df_questionnaire[,c(9,14,15,16,17,30,32,33,34,36,39,40:47,50,51)]

colnames(# Categorical Descriptives
table<-with(data_validation,table(occupation_cat))
table
prop.table(table))<-c("date",
                        "birth",
                        "sex",
                        "weight",
                        "height",
                        "health_insurance",
                        "marital_status",
                        "occupation",
                        "household_size",
                        "education",
                        "mental_health_issue",
                        "mental_handicap",
                        "diabetes",
                        "ihd",
                        "infectious_disease",
                        "renal",
                        "malnutrition",
                        "tbi",
                        "perinatal_suffering",
                        "date_epilepsy_diag",
                        "age_first_crisis")

str(socio_data)

socio_data$birth_recoded<-as.Date(socio_data$birth,'%d/%m/%Y')
# Mergind datasets

# data_merged <- merge(
#   x=phq9_data2,
#   y=hdrs_data2, by="id",all.x = TRUE)

data_merged<-join(phq9_data2, 
     hdrs_data2,
     type = "inner",by=c("id"))

######################################################################
#Table 1
######################################################################

#Numeric

with(socio_data,describe())

# Categorical Descriptives
table<-with(data_validation,table(occupation_cat))
table
prop.table(table)



######################################################################
#FLOORING,AND CEILING EFFECT
######################################################################

##############################################################
#RELIABILITY
##############################################################
#psych::alpha(cor_data,n.iter=1000,check.keys=TRUE)
psych::alpha(data_merged[,15:23],n.iter=1000,check.keys=TRUE)
psych::alpha(data_merged[,46:62],n.iter=1000,check.keys=TRUE)

#TEMPORAL stability

x<-ICC(icc_phq9[,c(24,47)])
plot(x)

ICC(na.omit(icc_temporal_phone[,-1]))


#Correlation between both measures

ICC(na.omit(icc_data[,-1]))

library(BlandAltmanLeh)
bland.altman.plot(icc_phq9[,24], icc_phq9[,47], 
                        conf.int=.95, pch=19,
                  xlab="Average measurements over time", ylab="Difference in measurement over time")

plot<-ggplot(icc_phq9, aes(icc_phq9[,24],icc_phq9[,47])) +
    geom_point() +    # Use hollow circles
    geom_smooth() +
    xlab("PHQ9 Score at T2") +
    ylab("PHQ9 Score at T1")

    plot

plot<-ggplot(data_merged, aes(data_merged[,24],data_merged[,64])) +
    geom_point() +    # Use hollow circles
    geom_smooth() +
    xlab("PHQ9 Score") +
    ylab("HDRS Score")

    plot

cor(na.omit(data.frame(data_merged[,24],data_merged[,64])))
##############################################################
#CONFIRMATORY FACTOR ANALYSIS
#############################################################
# PHQ9

# 1 factor model
cfa_model <- '
PHQ9 =~  phq1_num + phq2_num + phq3_num + phq4_num + phq5_num + phq6_num + phq7_num + phq8_num + phq9_num

#cov
phq6_num ~~ phq9_num
'

fit <- lavaan::cfa(cfa_model,
                   data = data_merged[,15:23],
                   estimator="WLSMV",
                   ordered=colnames(data_merged[,15:23])
                   )
summary(fit, fit.measures=TRUE)
lavaan::fitMeasures(fit, fit.measures = c("rmsea.scaled",
                                          "rmsea.ci.lower.scaled",
                                          "rmsea.ci.upper.scaled",
                                          "cfi.scaled",
                                          "tli.scaled",
                                          "nnfi.scaled",
                                          "chisq.scaled",
                                          "pvalue.scaled"
                                          )
                    )
# AIC(fit)
Est <- lavaan::parameterEstimates(fit, ci = TRUE, standardized = TRUE)
subset(Est, op == "=~")
subset(Est, op == "~~")
lavInspect(fit,what="th")

### Modification Indexes
Mod <- lavaan::modificationIndices(fit)
subset(Mod, mi > 10)

library()
nodeLabels<-c("Q1",
              "Q2",
              "Q3",
              "Q4",
              "Q5",
              "Q6",
              "Q7",
              "Q8",
              "Q9",
              "Q10",
              "Q11",
              "Q12",
              "Q13",
              "Q14",
              "Q15",
              "Q16",
              "Q17",
              "HDRS")

color<-c(rep("grey",17),rep("white",1))
borders<-c(rep("FALSE",17),rep("TRUE",1))
labelcex<-c(rep(0.7,17),rep(1,1))

# tiff("/Users/jnv4/Desktop/resilience_stress_fig2.tiff", units='in', 
#   width = 15,
#  height = 10,compression = 'lzw',res=1200,bg = "white")
library(semPlot)
semPlot::semPaths(fit,
                  "model",
                  "std",
                  # layout="spring",
                  style="lisrel",
                  residuals=FALSE,
                  # cut=1,
                  # equalizeManifests=TRUE,
                  # edge.color="black",
                  exoCov=FALSE,
                  intercepts=FALSE,
                  nodeLabels=nodeLabels,
                  label.scale=FALSE,
                  edge.label.cex=0.8,
                  label.cex=labelcex,
                  color=color,
                  borders=borders)
                  # bifactor="general")
# dev.off()

#Composite Reliabilty
sum(Est$std.all[1:9])^2/(sum(Est$std.all[1:9])^2+sum(Est$std.all[71:87]))

#Average Extracted Variance
sum(Est$std.all[1:17]^2)/length(Est$std.all[1:17])

#Thresholds
by(Est$std.all[13:50],Est$lhs[13:50],mean)

#Factor scores
# kessler_overall<-lavaan::predict(fit)

# HDRS

# 1 factor model
cfa_model <- '
HDRS =~  hdrs1_num + hdrs2_num + hdrs3_num + hdrs4_num + hdrs5_num + hdrs6_num + hdrs7_num + hdrs8_num + hdrs9_num + hdrs10_num +
         hdrs11_num + hdrs12_num + hdrs13_num + hdrs14_num + hdrs15_num + hdrs16_num + hdrs17_num

#cov
hdrs4_num ~~  hdrs6_num
hdrs5_num ~~  hdrs6_num
'

fit <- lavaan::cfa(cfa_model,
                   data = data_merged[,46:62],
                   estimator="WLSMV",
                   ordered=colnames(data_merged[,46:62])
                   )
summary(fit, fit.measures=TRUE)
lavaan::fitMeasures(fit, fit.measures = c("rmsea.scaled",
                                          "rmsea.ci.lower.scaled",
                                          "rmsea.ci.upper.scaled",
                                          "cfi.scaled",
                                          "tli.scaled",
                                          "nnfi.scaled",
                                          "chisq.scaled",
                                          "pvalue.scaled"
                                          )
                    )
# AIC(fit)
Est <- lavaan::parameterEstimates(fit, ci = TRUE, standardized = TRUE)
subset(Est, op == "=~")
subset(Est, op == "~~")
lavInspect(fit,what="th")

### Modification Indexes
Mod <- lavaan::modificationIndices(fit)
subset(Mod, mi > 10)

library()
nodeLabels<-c("Q1",
              "Q2",
              "Q3",
              "Q4",
              "Q5",
              "Q6",
              "Q7",
              "Q8",
              "Q9",
              "Q10",
              "Q11",
              "Q12",
              "Q13",
              "Q14",
              "Q15",
              "Q16",
              "Q17",
              "HDRS")

color<-c(rep("grey",17),rep("white",1))
borders<-c(rep("FALSE",17),rep("TRUE",1))
labelcex<-c(rep(0.7,17),rep(1,1))

# tiff("/Users/jnv4/Desktop/resilience_stress_fig2.tiff", units='in', 
#   width = 15,
#  height = 10,compression = 'lzw',res=1200,bg = "white")
library(semPlot)
semPlot::semPaths(fit,
                  "model",
                  "std",
                  # layout="spring",
                  style="lisrel",
                  residuals=FALSE,
                  # cut=1,
                  # equalizeManifests=TRUE,
                  # edge.color="black",
                  exoCov=FALSE,
                  intercepts=FALSE,
                  nodeLabels=nodeLabels,
                  label.scale=FALSE,
                  edge.label.cex=0.8,
                  label.cex=labelcex,
                  color=color,
                  borders=borders)
                  # bifactor="general")
# dev.off()

#Composite Reliabilty
sum(Est$std.all[1:17])^2/(sum(Est$std.all[1:17])^2+sum(Est$std.all[38:46]))

#Average Extracted Variance
sum(Est$std.all[1:17]^2)/length(Est$std.all[1:17])

#Thresholds
by(Est$std.all[13:50],Est$lhs[13:50],mean)

#Factor scores
# kessler_overall<-lavaan::predict(fit)



########################################################
#ROC Plot with Sensitivity and Specificity
########################################################
# with(data_mcid2,by(change_score,change_cat_PGIC1_mild,summary))
# with(data_mcid2,by(data_mcid2[,4],change_cat_PGIC1_mild,summary))
# with(data_mcid2,by(change_score,change_cat_PGIC1_moderate,summary))
# with(data_mcid2,by(data_mcid2[,4],change_cat_PGIC1_moderate,summary))
# with(data_mcid2,by(change_score,change_cat_PGIC1_severe,summary))
# with(data_mcid2,by(data_mcid2[,4],change_cat_PGIC1_severe,summary))
# with(data_mcid2,by(change_score,change_cat_PGIC2,summary))
# with(data_mcid2,by(data_mcid2[,4],change_cat_PGIC2,summary))

#creating intervals based on the HDRS

data_merged$hdrs_international_cat<-car::recode(data_merged$hdrs_sum,"
                                0:6='ab';
                                7:17='mild';
                                18:23='severe';
                                NA=NA;
                                else='verysevere'")

prop.table(table(data_merged$hdrs_international_cat))

data_merged$hdrs_international_abscence<-car::recode(data_merged$hdrs_sum,"
                                0:6=0;
                                NA=NA;
                                else=1")
data_merged$hdrs_international_mild<-car::recode(data_merged$hdrs_sum,"
                                0:17=0;
                                NA=NA;
                                else=1")
data_merged$hdrs_international_severe<-car::recode(data_merged$hdrs_sum,"
                                0:23=0;
                                NA=NA;
                                else=1")
data_merged$hdrs_international_verysevere<-car::recode(data_merged$hdrs_sum,"
                                0:24=0;
                                NA=NA;
                                else=1")

data_merged$hdrs_international_verysevere<-car::recode(data_merged$hdrs_sum,"
                                0:15=0;
                                NA=NA;
                                else=1")

#Roc curve for the experimental group
library(Epi)
library(pROC)

ROC(form=hdrs_international_verysevere~phq9_sum, data=data_merged)

library(OptimalCutpoints)
optimal.cutpoint.Youden <- optimal.cutpoints(X = "phq9_sum", 
                                             status = "hdrs_international_verysevere", 
                                             tag.healthy = "0",
                                             methods = "Youden", 
                                             data = data_merged, 
                                             pop.prev = NULL, 
                                             categorical.cov = NULL, #"gender",
                                             control = control.cutpoints("generalized.Youden"), 
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

summary(optimal.cutpoint.Youden)

plot(optimal.cutpoint.Youden)


