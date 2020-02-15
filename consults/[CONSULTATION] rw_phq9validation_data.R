######################################################
#rw_phq9validation_data.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. see full license at the end of this file.
######################################################
#this script follows a combination of the guidelines proposed by Hadley Wickham http://goo.gl/c04kq as well as using the formatR package http://goo.gl/ri6ky
#if this is the first time you are conducting an analysis using this protocol, please watch http://goo.gl/DajIN while following step by step
#
######################################################
#SETTING ENVIRONMENT
######################################################
 #install.packages("VIM")
 #install.packages("VIMGUI")
 #install.packages("miP")
 #install.packages("gWidgetsRGtk2")
 #install.packages("mi")
 # install.packages("eeptools")

#Load packages neededz for the analysis
#All packages must be installes with install.packages() function
lapply(c("ggplot2", "psych", "RCurl", "irr", "nortest", 
  "moments","GPArotation","nFactors","boot","psy", "car",
  "vcd", "gridExtra","mi","VIM","gdata",
  "reshape2","mclust","foreign","survival","memisc","lme4",
  "lmerTest","dplyr","eRm","mirt","dplyr","devtools","reshape",
  "mice","jsonlite","tidyverse","pROC","Epi"),
library, character.only=T)

#Package and codes to pull data from goodle sheets
#devtools::install_github("jennybc/googlesheets")
#library(googlesheets)
#suppressMessages(library(dplyr))

######################################################
#IMPORTING DATA
######################################################

# library(jsonlite)

file <- file("/Users/Joao/Box/Home Folder jnv4/Data/consultation/UCB/exported_data.txt",
             open="r",
             encoding="UTF-8-BOM")

df <- read.delim(file=file,
                 header=FALSE,
                 fill=FALSE,
                 stringsAsFactors=FALSE,
                 fileEncoding="UTF-8-BOM",
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
#Sociodemographics
######################################################

#age
library(lubridate)

df_questionnaire$data_birth<-
    dmy(car::recode(df_questionnaire$Date.de.naissance,
           "'00-00-1970'='01-01-1970';
           '00-00-1978'='01-01-1978';
           '00-00-1960'='01-01-1960';
           '00-00-1982'='01-01-1982';
           '00-00-1983'='01-01-1983';
           '00-00-1984'='01-01-1984';
           '00-00-1985'='01-01-1985';
           '00-00-1986'='01-01-1986';
           '00-00-1987'='01-01-1987';
           '00-00-1988'='01-01-1988';
           '00-00-1989'='01-01-1989';
           '00-00-1990'='01-01-1990';
           '00-00-1991'='01-01-1991';
           '00-00-1992'='01-01-1992';
           '00-00-1993'='01-01-1993';
           '00-00-1994'='01-01-1994';
           '00-00-1995'='01-01-1995';
           '00-00-1996'='01-01-1996';
           '00-00-1997'='01-01-1997';
           '00-00-1998'='01-01-1998';
           '00-00-1999'='01-01-1999';
           '00-00-1979'='01-01-1979';
           '00-00-1980'='01-01-1980';
           '00-00-1959'='01-01-1959';
           '00-00-1969'='01-01-1969';
           '00-00-1974'='01-01-1974';
           '00-00-1964'='01-01-1964';
           '00-00-1951'='01-01-1951';
           '00-00-1966'='01-01-1966';
           '00-00-1944'='01-01-1944';
           '00-00-1973'='01-01-1973';
           '00-00-1952'='01-01-1952';
           '00-00-1955'='01-01-1955';
           '00-00-1946'='01-01-1946';
           '01-01-197'='01-01-1970';
           '8-10-202'=' 8-10-2002';
           '00-00-2002'='01-01-2002';
           '11-11-1111'=NA"))
    # dmy() -> data_birth

library(eeptools)
current_date<-rep(mdy("01-01-2018"),length(df_questionnaire$Date.de.naissance))
df_questionnaire$age<-with(df_questionnaire,age_calc(na.omit(data_birth),current_date,units = "years"))

describe(df_questionnaire$age)

table(df_questionnaire$Statut.marital)

#gender
table(df_questionnaire$Sexe)
prop.table(table(df_questionnaire$Sexe))

#employment
unique(df_questionnaire$Fonction.professionnelle)

level_key <- list(
                  "Cuisinier"= "employed",
                  "En chômage"= "unemployed",
                  "enfant"= "unemployed",
                  "cultivateur"= "employed",
                  "fonctionnaire de l’état"= "employed",
                  "etudiant"= "unemployed",
                  "commerçant/entrepreneur indépendant"= "employed",
                  "Pas d'emploi"= "unemployed",
                  "Chaumeur"= "employed",
                  "Ménage"= "unemployed",
                  "Menage"= "unemployed",
                  "Aucune"= "unemployed",
                  "PAS D'emploi"= "unemployed",
                  "Chômage"= "unemployed",
                  "Passionnée"= "unemployed",
                  "Pasteur pensionné"= "unemployed",
                  "Chomeur"= "unemployed",
                  "Incapable"= "unemployed",
                  "Travaux ménager"= "unemployed",
                  "Démobilisé"= "unemployed",
                  "Ménagère"= "unemployed",
                  "Business"= "employed",
                  "Chauffeur"= "employed",
                  "Sans emploi"= "unemployed",
                  "Pas d,emploi"= "unemployed",
                  "Housekeeper"= "unemployed",
                  "Secteur privé"= "employed",
                  "Aide maçon"= "employed",
                  "Pas emploi"= "unemployed",
                  "Such events"= "unemployed",
                  "Militaire"= "employed",
                  "Atelier de couture"= "employed",
                  "Passionné"= "unemployed")

employment<-recode_factor(df_questionnaire$Fonction.professionnelle, !!!level_key)

table(employment)
prop.table(table(employment))

#education

test<-data.frame(matrix(unlist(df_questionnaire$Lévétiracétam), 
                    nrow=length(df_questionnaire$Lévétiracétam), 
                    byrow=T))

test<-data.frame(matrix(unlist(df_questionnaire$Valproate), 
                    nrow=length(df_questionnaire$Valproate), 
                    byrow=T))


test<-data.frame(matrix(unlist(df_questionnaire$Diphantoine), 
                    nrow=length(df_questionnaire$Diphantoine), 
                    byrow=T))

test<-data.frame(matrix(unlist(df_questionnaire$Phénobarbital), 
                    nrow=length(df_questionnaire$Phénobarbital), 
                    byrow=T))

test<-data.frame(matrix(unlist(df_questionnaire$Lorazépam), 
                    nrow=length(df_questionnaire$Lorazépam), 
                    byrow=T))

test<-data.frame(matrix(unlist(df_questionnaire$Clonazépam), 
                    nrow=length(df_questionnaire$Clonazépam), 
                    byrow=T))

test<-data.frame(matrix(unlist(df_questionnaire$Diazepam), 
                    nrow=length(df_questionnaire$Diazepam), 
                    byrow=T))

table(as_tibble())
prop.table(table(df_questionnaire$Lévétiracétam))

#age of first onset

# df_questionnaire<-
# df_questionnaire %>%
#     as.tibble() %>%
#       mutate(age_onset_numerictemp = gsub("[^0-9\\.]", "",
#             Age.du.patient.au.moment.de.la.premiere.crise)) %>%
#       mutate(age_onset_numerictemp = as.numeric(age_onset_numerictemp)) %>% 
#       mutate(age_onset_numerictemp2 = ifelse(age_onset_numerictemp < 1000, age_onset_numerictemp,
#                                           (2017-age_onset_numerictemp)))# %>%
#       # mutate(age_onset_numerictemp3 = age_calc(data_birth,mdy(as.character(age_onset_numerictemp2)))) %>%
#       # # mutate(ade_at_onset = )
#       # pull(age_onset_numerictemp2)

# describe(df_questionnaire$age_onset_numerictemp2)

#type of epilepsy
df <- data.frame(matrix(unlist(df_questionnaire$Type.de.crise.épileptiques), nrow=434, byrow=T))

table(df$X1)
table(df$X2)
table(df$X3)
table(df$X4)
table(df$X5)
table(df$X6)


partial_epilepsy <- 71 + 118
partial_epilepsy
partial_epilepsy/434
general_epilepsy <- 55 + 177 + 116
general_epilepsy
general_epilepsy/434
unknown <- 11
unknown
unknown/434

unique(df_questionnaire$Veuillez.preciser.les.traitements.non.anti.epileptiques..actuels..veuillez.mentionner.le.nom.generique.du.produit..par.example.amitryptilline..)

epilepsy_treatment<-car::recode(df_questionnaire$Veuillez.preciser.les.traitements.non.anti.epileptiques..actuels..veuillez.mentionner.le.nom.generique.du.produit..par.example.amitryptilline..,"
                                                                         "Clomipramine "                                                                              "Pipemperon"                                                                                 "Fluonxetine et chlorazepate dipotasique"                                                    "Pas mentionnes"                                                                             "Pas autre traitement pour le moment "                                                       "Pas"                                                                                        "Anti hypertension (amilodepine5mg)"                                                         "Risperdone"                                                                                 "Halloperidol 5mg/j"                                                                         "Topamax et omeprazole "                                                                     "Inconnu"                                                                                    "Pas d'autre traitement"                                                                     "Insuline lente et rapide"                                                                   "Cinarzine 75mg 2cp/jour"                                                                    "Stugeron 2*75mg/Jrs et cafergot 1 cp par jour"                                              "ARVS"                                                                                       "Pas d' autres medicaments utilise à long terme"                                             "Contaceptifs pilure q'elle ne connaît pas les noms"                                         "Cinarzine "                                                                                 "Halloperidol "                                                                              "Amitryptilline,Xanax,cinnarizine,citalopram"                                                "Tryptizol 50mg/jr et Akineton 2mg "                                                         "Amitriptylline et cinarzine "                                                               "Tryptisol 50mg/jrs  et stagneron 75x2/Jrs"                                                  "Pas autre traitement mentionne"                                                             "Amitriptylline 50mg/j"                                                                      "Amitriptilline 25mg/j, cinarzine 75mgx2/j"                                                  "AMITRYPTILLINE,CINNARIZINE "                                                                "Cinnarizine "                                                                               "Halperdol"                                                                                  "Pas mentionne"                                                                              "Spas mentionne "                                                                            "Aucun traitement antiepileptiques utilise "                                                 "Orap 1mg /Jrs/30jrs"                                                                        "Risperidone"                                                                                "Gut"                                                                                        "Risperdal 1mg/ j/30jrs"                                                                     "Pas autre medicaments actuels"                                                              "Amoxicilline "                                                                              "Tryptizol 50mg/ mg "                                                                        "Risperidone et biperidene(akineton)"                                                        "Pas autres medicaments "                                                                    "Propanolol"                                                                                 "Bipe perron"                                                                                "Propanolol "                                                                                "Cotrimoxazole forte 960mg/j comme prophylaxie et nevirapine et TDF+3TC+TEnofovir comme ARV" "Pas autre medicament actuellement"                                                          "TEnofovir, lamividine, nevirapine, cotrimoxazole, insuline, dipiperon et vitamine B complex""Pas d'autres medicaments utilises "                                                         "Pas des autres medicaments a prendre "                                                      "Pas de medicaments mentionne "                                                              "Pas autres traitement a actuels"                                                            "Risperidone 2mg/j"                                                                          "Antibiotiques pas precis"                                                                   "Diclofenac"                                                                                 "Halloperidol et chlorpromazine "                                                            adone 100mg"                                                                            "Amitryptilline,cinnarizine"                                                                 "Courtemanche"                                                                               "Aucune traitement traditionnel "                                                            "Nifedipine 40mg/j et propanolol 40mg/j"                                                     "Dogmatil 200mg/jour"                                                                        "Amitriptylline et ci narine"                                                                "Pas d'autre medicament"                                                                     "Pas d'autres traitement reçu "                                                              "Amitryptilline,cinnarizine,Propanolol"                                                      "Solo idem"                                                                                  "Coartem,paracetamol"                                                                        "Dipiperon"                                                                                  "Halloperidol deconanoas"                                                                    "Pas autre medicament actuel"                                                                "Amoxycilline gels 500mgx2/j et ibuprofene 400mgx2/j"                                        "Antiretroviraux et cotrimoxazole " 

df_questionnaire$Traitement.antiepileptique



prop.table(table(df_questionnaire$Traitement.antiepileptique))

table(df_questionnaire$Probleme.de.sante.mentale.)
prop.table(table(df_questionnaire$Traitement.antiepileptique))

prop.table(table(df_questionnaire$V1))

#Treatment for depression



######################################################
#PHQ9
######################################################

str(df_hdrs)
# write.csv(df_short_questionnaire[,1],"/Users/joaovissoci/Desktop/deleteme1.csv")
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

# str(phq9_data)

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

######################LIKERT SCALE GRAPH
#######################################

# phq9_data[,c(15:23)][phq9_data[,c(15:23)]==0]<-"Nta na rimwe"
# phq9_data[,c(15:23)][phq9_data[,c(15:23)]==1]<-"Rimwe na rimwe"
# phq9_data[,c(15:23)][phq9_data[,c(15:23)]==2]<-"Birenze igice cy'umunsi"
# phq9_data[,c(15:23)][phq9_data[,c(15:23)]==3]<-"Hafi ya buri muns"

phq9_likertplot<-with(phq9_data,data.frame(phq1_num,
                                           phq2_num,
                                           phq3_num,
                                           phq4_num,
                                           phq5_num,
                                           phq6_num,
                                           phq7_num,
                                           phq8_num,
                                           phq9_num
                                           ))

phq9_likertplot_transformed<-likert(na.omit(phq9_likertplot))

library(likert)
phq9_plot<-plot(phq9_likertplot_transformed,
           colors=c(
                "#D33F6A",
                "#E07B91",
                "lightgrey",
                "#8595E1"))

phq9_plot<- phq9_plot + scale_x_discrete(breaks=c(
                "phq1_num",
                "phq2_num",
                "phq3_num",
                "phq4_num",
                "phq5_num",
                "phq6_num",
                "phq7_num",
                "phq8_num",
                "phq9_num"),
                                        limits=c(
                "phq1_num",
                "phq2_num",
                "phq3_num",
                "phq4_num",
                "phq5_num",
                "phq6_num",
                "phq7_num",
                "phq8_num",
                "phq9_num"),
                                        labels=c(
                "Question #1",
                "Question #2",
                "Question #3",
                "Question #4",
                "Question #5",
                "Question #6",
                "Question #7",
                "Question #8",
                "Question #9"
                )) +
        theme_bw() +
        theme(legend.position="bottom")

# summary_data<-summary(knowledge_data)

# mean_plot<-ggplot(summary_data, aes(y=mean,x=Item)) + 
#     geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) +
#     geom_line() +
#     geom_point() +
#     coord_flip() +
#     xlab("") +
#     ylab("Mean/Standard Deviation") +
#     scale_x_discrete(limits=rev(c("talking_can_be_successful",
#                     "discuss_risky_alc",
#                     "discuss_counsel_pts",
#                     "not_my_role",
#                     "called_harmful_drinkers"))) +
#     expand_limits(y=c(1:5)) +
#     theme_bw() +
#     theme(panel.background = element_rect(colour = 'grey'))

phq9_network<-with(phq9_data,data.frame(phq1_num,
                                        phq2_num,
                                        phq3_num,
                                        phq4_num,
                                        phq5_num,
                                        phq6_num,
                                        phq7_num,
                                        phq8_num,
                                        phq9_num
                                           ))

#organizing datasets
library(qgraph)
# phq9_network_data<-data.frame(Under,Respo)#,reading_scores$scores)
phq9_network_data<-na.omit(phq9_network) #omitting NAs

#creating correlation matrix
phq9_cor_data<-cor_auto(phq9_network_data)
#qsgc<-qsgc$rho

#listing grouping variables in the network resulting from the 
#community analysis
# phq9_node_groups<-list(Under1=c(1,2,3,4,5,6,7,15),
#   Under2=c(8,9,10,11,12,13,14))

# creating vectors for labels
phq9_node_labels<-c("Question #1",
                "Question #2",
                "Question #3",
                "Question #4",
                "Question #5",
                "Question #6",
                "Question #7",
                "Question #8",
                "Question #9")

# creating nodes labels vector
phq9_node_names<-c("Q1","Q2","Q3","Q4","Q5","Q6","Q7",
  "Q8","Q9")

# creating vector with mean values for each node
mean_data<-sapply(phq9_network_data,mean)

#creating vector with mean values adjusted to proportional 
#sizes to be plotted
phq9_vSize<-c(mean_data/min(mean_data))

#building network figures 
# 3 types are created to get an avarege position and layout

#GLASSO NETWORK
# tiff("/Users/joaovissoci/Desktop/importance_network.tiff", width = 1200,
#  height = 700,compression = 'lzw')
network<-qgraph(phq9_cor_data,
  layout="spring",
  vsize=phq9_vSize*6,
  graph="glasso",
  sampleSize=nrow(phq9_network_data),
  legend.cex = 0.6,
  cut = 0.1, 
  maximum = 1, 
  minimum = 0, 
  # esize = 20,
  repulsion = 0.8,
  # groups=importance_network_groups,
  # nodeNames=phq9_node_names,
  #color=c("gold","steelblue","red","grey80",
  # layoutScale=c(2,2),
  # borders = FALSE,
  labels=phq9_node_labels,
  threshold=TRUE)#,gray=T,)#,nodeNames=nomesqsg
# dev.off()

#layout2<-averageLayout(network_glasso,network_pcor,network_cor)

# library(igraph)
# #Calculating Community measures
# g<-as.igraph(network) #creating igraph object
# #h<-walktrap.community(g) #creatin community object
# h<-spinglass.community(g, weights=NA)
# plot(h,g) #plotting community network
# h$membership #extracting community membership for each node on the network

#Identify SPLs within the graph and extract direct paths to WP
predictors<-centrality(network)
# predictors<-centrality(network)$ShortestPaths[,15]
predictors

# require(ggpubr)
# library(gridExtra)

# vp <- viewport(height = unit(1,"npc"), width=unit(0.5, "npc"), 
#               just = c("left","top"),
#               y = 1, x = 0)

# print(phq9_plot, vp = vp)

####################################
# ICC
####################################

# data.frame(phq9_data$phq9_sum,phq9_data$phq9_score)

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

# str(hdrs_data)

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

# data.frame(hdrs_data$hdrs_sum,hdrs_data$hrds_score)

hdrs_data2 <- hdrs_data[!duplicated(hdrs_data$id), ]

#Organizing SES questionnaire data

socio_data<-df_questionnaire[,c(9,14,15,16,17,30,32,33,34,36,39,40:47,50,51)]

colnames(socio_data)<-c("date",
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


# str(socio_data)

socio_data$birth_recoded<-as.POSIXct(socio_data$birth,
                      format='%d-%m-%Y')

socio_data$date_epilepsy_diag_recoded<-as.POSIXct(socio_data$date_epilepsy_diag,
                      format='%d-%m-%Y')
# Mergind datasets

# data_merged <- merge(
#   x=phq9_data2,
#   y=hdrs_data2, by="id",all.x = TRUE)

library("plyr")
data_merged0<-join(phq9_data2, 
     hdrs_data2,
     type = "inner",by=c("id"))

socio_data$id<-df_questionnaire$V1

data_merged<-join(socio_data, 
     data_merged0,
     type = "right",by=c("id"))
dim

#creating intervals based on the HDRS

data_merged$hdrs_international_cat<-car::recode(data_merged$hdrs_sum,"
                                0:7='ab';
                                8:16='mild';
                                17:23='severe';
                                NA=NA;
                                else='verysevere'")

data_merged$hdrs_international_mild<-car::recode(data_merged$hdrs_sum,"
                                0:7=0;
                                NA=NA;
                                else=1")
data_merged$hdrs_international_mod<-car::recode(data_merged$hdrs_sum,"
                                0:16=0;
                                NA=NA;
                                else=1")
data_merged$hdrs_international_severe<-car::recode(data_merged$hdrs_sum,"
                                0:23=0;
                                NA=NA;
                                else=1")
# data_merged$hdrs_international_severe<-car::recode(data_merged$hdrs_sum,"
#                                 0:24=0;
#                                 NA=NA;
#                                 else=1")

data_merged$hdrs_international_rwandan<-car::recode(data_merged$hdrs_sum,"
                                0:17=0;
                                NA=NA;
                                else=1")

######################################################################
#Table 1
######################################################################

# a.                   Demographic data (age, gender, marital status, employment by categories, …)
# b.                   Disease related data (duration of epilepsy since diagnosis and since first seizure, EEG, MRI, CT,…)
# c.                   Past AEDs (by number, by type (CBZ, PHT, VPA, LEV))
# d.                   Concomitant AEDs (by number of AEDs (1-2-3 or more, by type (CBZ, PHT, VPA, LEV))
# e.                   Number of seizures since last visit
# f.                    AE reports in two weeks between test and retest
# g.                   Traditional healing methods by category
# h.                   Other medications (yes/no, number of medications)

#Age

library(eeptools)

socio_data$today_data<-Sys.Date()

socio_data$age<-floor(difftime(socio_data$birth_recoded, socio_data$today_data,
         units = c("days"))*-1)

socio_data$age<-as.numeric(round(socio_data$age/365,digits=0))

socio_data$age<-car::recode(socio_data$age,"100:2000=NA")

with(socio_data,describe(age))

#Gender

table(socio_data$sex)
prop.table(table(socio_data$sex))

#Marital Status

socio_data$marital_status<-car::recode(socio_data$marital_status,"
                      'Choose'='autre'")

table(socio_data$marital_status)
prop.table(table(socio_data$marital_status))

#Empolyment

table(socio_data$occupation)
prop.table(table(socio_data$occupation))

#health_insurance

table(socio_data$health_insurance)
prop.table(table(socio_data$health_insurance))

#Household size

table(socio_data$household_size)

#education

#mental_health_issue

#mental_handicap

#diabetes

#ihd

#infectious_disease

#renal

#malnutrition

#tbi

#perinatal_suffering

#date_epilepsy_diag_recoded

#age_first_crisis



#Duration of epilepsy since first diagnosis

#Duration of epilepsy since first seizures

#EEG

#MRI

#CT

#AED by number

#AED by type (CBZ, PHT, VPA, LEV)

# number of seizures since last visit

#AE reports in the two weeks between test and retest

#Traditional healing methods by category

#Other medication (yes/no)

#number of medication

# Depression severity

table(data_merged$hdrs_international_cat)
prop.table(table(data_merged$hdrs_international_cat))

summary(as.factor(data_merged$hdrs_international_cat))

describe(data_merged$phq9_sum)
describe(data_merged$hdrs_sum)


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
                   data = phq9_network,
                   estimator="WLSMV",
                   ordered=colnames(phq9_network)
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

# library()
nodeLabels<-c("Q1",
              "Q2",
              "Q3",
              "Q4",
              "Q5",
              "Q6",
              "Q7",
              "Q8",
              "Q9",
              # "Q10",
              # "Q11",
              # "Q12",
              # "Q13",
              # "Q14",
              # "Q15",
              # "Q16",
              # "Q17",
              "PHQ9")

color<-c(rep("grey",9),rep("white",1))
borders<-c(rep("FALSE",9),rep("TRUE",1))
labelcex<-c(rep(0.7,9),rep(1,1))

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

require(pROC)
require(ztable)
require(moonBook)

roc_mild<-ROC(form=hdrs_international_mild~phq9_sum+education_recoded, 
    data=data_merged,
    PV=TRUE,
    MX=TRUE,
    MI=FALSE,
    AUC=TRUE,
    plot="sp")

plot(roc_mild)
# text("A")

roc_mod<-ROC(form=hdrs_international_mod~phq9_sum, 
    data=data_merged,
    PV=TRUE,
    MX=TRUE,
    MI=FALSE,
    AUC=TRUE,
    plot="ROC")

plot_ROC(roc_mod)

roc_sever<-ROC(form=hdrs_international_severe~phq9_sum, 
    data=data_merged,
    PV=TRUE,
    MX=TRUE,
    MI=FALSE,
    AUC=TRUE,
    plot="ROC")

plot_ROC(roc_sever)


roc_rw<-ROC(form=hdrs_international_rwandan~phq9_sum, 
    data=data_merged,
    PV=TRUE,
    MX=TRUE,
    MI=FALSE,
    AUC=TRUE,
    plot="ROC")

plot_ROC(roc_rw)

plot(roc_mild)



library(OptimalCutpoints)
optimal.cutpoint.Youden <- optimal.cutpoints(X = "phq9_sum", 
                                             status = "hdrs_international_rwandan", 
                                             tag.healthy = "0",
                                             methods = "Youden", 
                                             data = data_merged, 
                                             pop.prev = NULL, 
                                             categorical.cov = NULL, #"gender",
                                             control = control.cutpoints("generalized.Youden = TRUE"), 
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

data_above30<-subset(data_merged,data_merged$age>=30)

roc_mild<-ROC(form=hdrs_international_mild~phq9_sum, 
    data=data_above30,
    PV=TRUE,
    MX=TRUE,
    MI=FALSE,
    AUC=TRUE,
    plot="")

plot_ROC(roc_mild)
# text("A")

roc_mod<-ROC(form=hdrs_international_mod~phq9_sum, 
    data=data_above30,
    PV=TRUE,
    MX=TRUE,
    MI=FALSE,
    AUC=TRUE,
    plot="ROC")

plot_ROC(roc_mod)

roc_sever<-ROC(form=hdrs_international_severe~phq9_sum, 
    data=data_above30,
    PV=TRUE,
    MX=TRUE,
    MI=FALSE,
    AUC=TRUE,
    plot="ROC")

plot_ROC(roc_sever)


roc_rw<-ROC(form=hdrs_international_rwandan~phq9_sum, 
    data=data_above30,
    PV=TRUE,
    MX=TRUE,
    MI=FALSE,
    AUC=TRUE,
    plot="ROC")

plot_ROC(roc_rw)

data_below30<-subset(data_merged,data_merged$age<30)

roc_mild<-ROC(form=hdrs_international_mild~phq9_sum, 
    data=data_below30,
    PV=TRUE,
    MX=TRUE,
    MI=FALSE,
    AUC=TRUE,
    plot="")

plot_ROC(roc_mild)
# text("A")

roc_mod<-ROC(form=hdrs_international_mod~phq9_sum, 
    data=data_below30,
    PV=TRUE,
    MX=TRUE,
    MI=FALSE,
    AUC=TRUE,
    plot="ROC")

plot_ROC(roc_mod)

roc_sever<-ROC(form=hdrs_international_severe~phq9_sum, 
    data=data_below30,
    PV=TRUE,
    MX=TRUE,
    MI=FALSE,
    AUC=TRUE,
    plot="ROC")

plot_ROC(roc_sever)


roc_rw<-ROC(form=hdrs_international_rwandan~phq9_sum, 
    data=data_below30,
    PV=TRUE,
    MX=TRUE,
    MI=FALSE,
    AUC=TRUE,
    plot="ROC")

plot_ROC(roc_rw)

#Education

data_merged$education_recoded<-car::recode(data_merged$education,"
              'primaire'='educ1';
              'sans scolarité'='educ1';
              else='educ2'")

data_educ1<-subset(data_merged,data_merged$education_recoded=="educ1")

roc_mild<-ROC(form=hdrs_international_mild~phq9_sum, 
    data=data_educ1,
    PV=TRUE,
    MX=TRUE,
    MI=FALSE,
    AUC=TRUE,
    plot="")

plot_ROC(roc_mild)
# text("A")

roc_mod<-ROC(form=hdrs_international_mod~phq9_sum, 
    data=data_educ1,
    PV=TRUE,
    MX=TRUE,
    MI=FALSE,
    AUC=TRUE,
    plot="ROC")

plot_ROC(roc_mod)

roc_sever<-ROC(form=hdrs_international_severe~phq9_sum, 
    data=data_educ1,
    PV=TRUE,
    MX=TRUE,
    MI=FALSE,
    AUC=TRUE,
    plot="ROC")

plot_ROC(roc_sever)


roc_rw<-ROC(form=hdrs_international_rwandan~phq9_sum, 
    data=data_educ1,
    PV=TRUE,
    MX=TRUE,
    MI=FALSE,
    AUC=TRUE,
    plot="ROC")

plot_ROC(roc_rw)

data_educ2<-subset(data_merged,data_merged$education_recoded=="educ2")

roc_mild<-ROC(form=hdrs_international_mild~phq9_sum, 
    data=data_educ2,
    PV=TRUE,
    MX=TRUE,
    MI=FALSE,
    AUC=TRUE,
    plot="")

plot_ROC(roc_mild)
# text("A")

roc_mod<-ROC(form=hdrs_international_mod~phq9_sum, 
    data=data_educ2,
    PV=TRUE,
    MX=TRUE,
    MI=FALSE,
    AUC=TRUE,
    plot="ROC")

plot_ROC(roc_mod)

roc_sever<-ROC(form=hdrs_international_severe~phq9_sum, 
    data=data_educ2,
    PV=TRUE,
    MX=TRUE,
    MI=FALSE,
    AUC=TRUE,
    plot="ROC")

plot_ROC(roc_sever)


roc_rw<-ROC(form=hdrs_international_rwandan~phq9_sum, 
    data=data_educ2,
    PV=TRUE,
    MX=TRUE,
    MI=FALSE,
    AUC=TRUE,
    plot="ROC")

plot_ROC(roc_rw)
