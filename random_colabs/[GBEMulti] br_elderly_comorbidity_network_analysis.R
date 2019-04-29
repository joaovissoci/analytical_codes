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
#
#Instalar os pacotes importantes/Install important packages
# install.packages ("IsingFit")
# install.packages ("igraph")
# install.packages ("qgraph")
# # install.packages ("ggplot2")
# # install.packages("car")
# # install.packages("mgm")
# # install.packages ("dplyr")
# install.packages ("networktools")
# install.packages("NetworkComparisonTest")
# install.packages("plyr")
library(networktools)
library (plyr)
library (dplyr)
library(qgraph)
library(IsingFit)
library(ggplot2)
library(car)
library(tidyverse)
library(haven)
library(igraph)
library(mgm)
library(NetworkComparisonTest)
library(mice)
# remove.packages("qgraph")

######################################################################
#IMPORTING DATA
######################################################################

Banco_PNS_2013_elderly <- read_dta("/Users/Joao/Downloads/Banco idosos FINAL 20181119 PARA TESE SANDRO.dta")
#View(Banco_PNS_2013_diseases_depression_alcohol_accidents_20180117)
# names(Banco_PNS_2013_diseases_depression_alcohol_accidents_20180117)

#subset for only individuals who responded to the individual questionnaire

#list of morbidities
data_sub_mmonly<-with(Banco_PNS_2013_elderly,data.frame(has2,
                                                 diab2,
                                                 coles2,
                                                 iaic,
                                                 derr,
                                                 asma,
                                                 artreu,
                                                 probcol,
                                                 dort,
                                                 outramental,
                                                 dpoc,
                                                 ca,
                                                 insre,
                                                 outradc,
                                                 depre_dx_algo,
                                                 obeoms))

data_sub_all<-with(Banco_PNS_2013_elderly,data.frame(has2,
                                                 diab2,
                                                 coles2,
                                                 iaic,
                                                 derr,
                                                 asma,
                                                 artreu,
                                                 probcol,
                                                 dort,
                                                 outramental,
                                                 dpoc,
                                                 ca,
                                                 insre,
                                                 outradc,
                                                 depre_dx_algo,
                                                 obeoms,
                                                 sex))
                                                 # age=C008))
                                                 # cor,
                                                 # escbianca,
                                                 # vc,
                                                 # plano,
                                                 # regiao))
                                                 # zona))

#list of morbidities
data_sub_southregion_north<-subset(data_sub_mmonly,Banco_PNS_2013_elderly$regiao==1)
data_sub_southregion_northeast<-subset(data_sub_mmonly,Banco_PNS_2013_elderly$regiao==2)
data_sub_southregion_mid_west<-subset(data_sub_mmonly,Banco_PNS_2013_elderly$regiao==3)
data_sub_southregion_southeast<-subset(data_sub_mmonly,Banco_PNS_2013_elderly$regiao==4)
data_sub_southregion_south<-subset(data_sub_mmonly,Banco_PNS_2013_elderly$regiao==5)

data_sub_sex_man<-subset(data_sub_mmonly,Banco_PNS_2013_elderly$sex==1)
data_sub_sex_women<-subset(data_sub_mmonly,Banco_PNS_2013_elderly$sex==0)

######################################################################
#DATA MANAGEMENT
######################################################################

#rename diseases in english

# data_sub$heart_failure <- data_sub$inscard
# data_sub$HTN <- data_sub$has2
# data_sub$heart_attack <- data_sub$infarto
# data_sub$stroke <- data_sub$derr
# data_sub$asthma <- data_sub$asma
# data_sub$arth_rheu <- data_sub$artreu
# data_sub$work_osteomusc <- data_sub$dort
# data_sub$depression <- data_sub$dxdepre
# data_sub$schizo <- data_sub$esquiz
# data_sub$ocd <- data_sub$toc
# data_sub$copd <- data_sub$dpoc
# data_sub$emphysema <- data_sub$enfis
# data_sub$chron_bronch <- data_sub$bronq
# data_sub$cancer <- data_sub$ca
# data_sub$chron_kidfailure <- data_sub$insre
# data_sub$spinalprob <- data_sub$probcol

#age
# data_sub$agecat <- car::recode(data_sub$C008, "18:24 = '0'; 25:44 = '1'; 45:64 = '2'; 65:84 = '3'; 85:101 = '4'" )
# data_sub$agecat <- factor(data_sub$agecat, labels = c("18-24", "25-44", "45-64", "65-84", "85 and older"))

#education
# data_sub$educ <- car::recode(data_sub$esc, " 1 = '0'; 2:3 = '1'; 4:5 = '2'; 6:7 = '3' ")
# data_sub_all$educ <- factor(data_sub_all$educ, labels = c("no schooling", "elementary/equivalent", "secondary/equivalent", "post-secondary/equivalent"))
# no need to recode following: sex, cor, regiao 

#recode RTI
# str(data_sub_all$O009)
# data_sub_all$RTI<- car::recode (data_sub_all$O009, "2=0; 1=1") 

#recode alcohol
# data_sub_all %>%
  # mutate(Alcohol=case_when(P027 == "1"|
  #                            P027== "2"|
  #                            P032==  "0" ~ 0,
  #                          P032== "1" ~ 1)) -> data_sub_all 



#creat smaller dataset with just diseases
# newdata4 <- with (data_sub_all, data.frame(HTN, diab2, arth_rheu, chron_kidfailure, copd, ocd, depression, heart_attack, stroke, spinalprob, angina, asthma, work_osteomusc, emphysema, cancer, bipolar, schizo, chron_bronch, RTI, heart_failure,  Alcohol))

#creat smaller dataset with diseases + demographic variables
# newdata5 <- with (data_sub_all, data.frame(HTN, diab2, arth_rheu, chron_kidfailure, copd, heart_failure, ocd, depression, heart_attack, stroke, spinalprob, angina, asthma, work_osteomusc, emphysema, cancer, bipolar, schizo, chron_bronch, RTI, Alcohol, sex, cor, educ, agecat, regiao))

#######################################################
#ANALYZING MISSING DATA
#######################################################

#Studying missing data
#Calculating frequency of missing data per variable
propmiss <- function(dataframe) lapply(dataframe,function(x) data.frame(nmiss=sum(is.na(x)), n=length(x), propmiss=sum(is.na(x))/length(x)))

propmiss(data_sub_mmonly)

#remove incomplete cases from newdata4

# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
# imp <- mice(data_sub_mmonly, seed = 2222, m=10)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
# data_imputed<-complete(imp,4)

# newdata4[complete.cases(newdata4), ] #Keep only complete rows
# data4_complete <- data_imputed

# data4_complete$sex<-as.numeric(data4_complete$sex)
# # data4_complete$cor<-as.numeric(data4_complete$cor)
# data4_complete$educ<-as.numeric(data4_complete$educ)
# data4_complete$agecat<-as.numeric(data4_complete$agecat)
# # data4_complete$regiao<-as.numeric(as.character(data4_complete$regiao))

######################################################################
#OVERALL GRAPH
######################################################################
#Prevalences (proportion with disease/total)

table(data_sub_mmonly$has2)
prev_hyp<-prop.table(table(data_sub_mmonly$has2))

table(data_sub_mmonly$diab2)
prev_diab2<-prop.table(table(data_sub_mmonly$diab2))

table(data_sub_mmonly$coles2)
prev_coles2<-prop.table(table(data_sub_mmonly$coles2))

table(data_sub_mmonly$iaic)
prev_iaic<-prop.table(table(data_sub_mmonly$iaic))

table(data_sub_mmonly$derr)
prev_derr<-prop.table(table(data_sub_mmonly$derr))

table(data_sub_mmonly$asma)
prev_asma<-prop.table(table(data_sub_mmonly$asma))

table(data_sub_mmonly$artreu)
prev_artreu<-prop.table(table(data_sub_mmonly$artreu))

table(data_sub_mmonly$probcol)
prev_probcol<-prop.table(table(data_sub_mmonly$probcol))

table(data_sub_mmonly$dort)
prev_dort <-prop.table(table(data_sub_mmonly$dort))

table(data_sub_mmonly$outramental)
prev_outramental<-prop.table(table(data_sub_mmonly$outramental))

table(data_sub_mmonly$dpoc)
prev_dpoc<-prop.table(table(data_sub_mmonly$dpoc))

table(data_sub_mmonly$ca)
prev_ca<-prop.table(table(data_sub_mmonly$ca))

table(data_sub_mmonly$insre)
prev_insre<-prop.table(table(data_sub_mmonly$insre))

table(data_sub_mmonly$outradc)
prev_outradc <- prop.table(table(data_sub_mmonly$outradc))

table(data_sub_mmonly$depre_dx_algo)
prev_depre_dx_algo <- prop.table(table(data_sub_mmonly$depre_dx_algo))

table(data_sub_mmonly$obeoms)
prev_obeoms <-prop.table(table(data_sub_mmonly$obeoms))

prev_all<-c(prev_hyp[2]+20,
            prev_diab2[2]+12,
            prev_coles2[2]+15,
            prev_iaic[2]+10,
            prev_derr[2]+5,
            prev_asma[2]+5,
            prev_artreu[2]+12,
            prev_probcol[2] + 16, 
            prev_dort[2]+4,
            prev_outramental[2]+4,
            prev_dpoc[2]+4,
            prev_ca[2]+5, 
            prev_insre[2] + 5, 
            prev_outradc[2] + 5 ,
            prev_depre_dx_algo[2] + 10,
            prev_obeoms[2] + 15)

#Fitting the network
fit_obj <- mgm(data = data_sub_all, 
               type = rep('c', ncol(data_sub_all)),
               level = rep(2,ncol(data_sub_all)),
               lambdaSel = 'EBIC',
               lambdaGam=0.25,
               ruleReg = 'AND',
               binarySign=TRUE)


#Computing predictability nodes
pred_obj <- predict(object = fit_obj, 
                    data = data_sub_all, 
                    errorCat = 'CC')

#Centrality measures
centRes <- centrality(fit_obj$pairwise$wadj) #tells us about node strength
centRes$OutDegree
centRes$Closeness
centRes$Betweenness
centRes$ShortestPathLength
centralityPlot(fit_obj$pairwise$wadj)

#Centrality
centralityPlot(fit_obj$pairwise$wadj, include = c("ExpectedInfluence")) 

#Flow
# flow(fit_obj$pairwise$wadj, "RTI", theme = "colorblind")#, vsize = prev_all)

Graph_Ising_predictability<-qgraph(fit_obj$pairwise$wadj, # weighted adjacency matrix as input
                                   layout = 'spring', 
                                   vsize=c(prev_all,3),
                                   pie = pred_obj$error[,2], # provide errors as input
                                   pieColor = rep('#377EB8',ncol(data_sub_all)),
                                   edge.color = fit_obj$pairwise$edgecolor,
                                   labels = colnames(data_sub_all))#,
                                  # groups=as.factor(g2_sg$membership))

# análise de comunidades 
# https://www.nature.com/articles/srep30750
g2<-as.igraph(Graph_Ising_predictability)
# se não tiver arestas negativas:
# g2_cl<-cluster_louvain(g2)

# se houver arestas negativas
g2_sg<-spinglass.community(g2,implementation = "neg")

Graph_Ising_predictability<-qgraph(fit_obj$pairwise$wadj, # weighted adjacency matrix as input
                                   layout = 'spring', 
                                   vsize=prev_all,
                                   pie = pred_obj$error[,2], # provide errors as input
                                   pieColor = rep('#377EB8',ncol(data_sub_all)),
                                   edge.color = fit_obj$pairwise$edgecolor,
                                   labels = colnames(data_sub_all),
                                   groups=as.factor(g2_sg$membership))

#Flow
# qgraph::flow(Graph_Ising_predictability, "RTI", theme = "colorblind")#, vsize = prev_all2)

######################################################################
#NORTH REGION GRAPH
######################################################################
#Prevalences (proportion with disease/total)

table(data_sub_southregion_north$has2)
north_prev_hyp<-prop.table(table(data_sub_southregion_north$has2))

table(data_sub_southregion_north$diab2)
north_prev_diab2<-prop.table(table(data_sub_southregion_north$diab2))

table(data_sub_southregion_north$coles2)
north_prev_coles2<-prop.table(table(data_sub_southregion_north$coles2))

table(data_sub_southregion_north$iaic)
north_prev_iaic<-prop.table(table(data_sub_southregion_north$iaic))

table(data_sub_southregion_north$derr)
north_prev_derr<-prop.table(table(data_sub_southregion_north$derr))

table(data_sub_southregion_north$asma)
north_prev_asma<-prop.table(table(data_sub_southregion_north$asma))

table(data_sub_southregion_north$artreu)
north_prev_artreu<-prop.table(table(data_sub_southregion_north$artreu))

table(data_sub_southregion_north$probcol)
north_prev_probcol<-prop.table(table(data_sub_southregion_north$probcol))

table(data_sub_southregion_north$dort)
north_prev_dort <-prop.table(table(data_sub_southregion_north$dort))

table(data_sub_southregion_north$outramental)
north_prev_outramental<-prop.table(table(data_sub_southregion_north$outramental))

table(data_sub_southregion_north$dpoc)
north_prev_dpoc<-prop.table(table(data_sub_southregion_north$dpoc))

table(data_sub_southregion_north$ca)
north_prev_ca<-prop.table(table(data_sub_southregion_north$ca))

table(data_sub_southregion_north$insre)
north_prev_insre<-prop.table(table(data_sub_southregion_north$insre))

table(data_sub_southregion_north$outradc)
north_prev_outradc <- prop.table(table(data_sub_southregion_north$outradc))

table(data_sub_southregion_north$depre_dx_algo)
north_prev_depre_dx_algo <- prop.table(table(data_sub_southregion_north$depre_dx_algo))

table(data_sub_southregion_north$obeoms)
north_prev_obeoms <-prop.table(table(data_sub_southregion_north$obeoms))

prev_all_north<-c(north_prev_hyp[2]+20,
            north_prev_diab2[2]+12,
            north_prev_coles2[2]+15,
            north_prev_iaic[2]+10,
            north_prev_derr[2]+5,
            north_prev_asma[2]+5,
            north_prev_artreu[2]+12,
            north_prev_probcol[2] + 16, 
            north_prev_dort[2]+4,
            north_prev_outramental[2]+4,
            north_prev_dpoc[2]+4,
            north_prev_ca[2]+5, 
            north_prev_insre[2] + 5, 
            north_prev_outradc[2] + 5 ,
            north_prev_depre_dx_algo[2] + 10,
            north_prev_obeoms[2] + 15)

#Fitting the network
fit_obj <- mgm(data = data_sub_southregion_north, 
               type = rep('c', ncol(data_sub_southregion_north)),
               level = rep(2,ncol(data_sub_southregion_north)),
               lambdaSel = 'EBIC',
               lambdaGam=0.25,
               ruleReg = 'AND',
               binarySign=TRUE)


#Computing predictability nodes
pred_obj <- predict(object = fit_obj, 
                    data = data_sub_southregion_north, 
                    errorCat = 'CC')

#Centrality measures
centRes <- centrality(fit_obj$pairwise$wadj) #tells us about node strength
centRes$OutDegree
centRes$Closeness
centRes$Betweenness
centRes$ShortestPathLength
centralityPlot(fit_obj$pairwise$wadj)

#Centrality
centralityPlot(fit_obj$pairwise$wadj, include = c("ExpectedInfluence")) 

#Flow
# flow(fit_obj$pairwise$wadj, "RTI", theme = "colorblind")#, vsize = prev_all)

Graph_Ising_predictability<-qgraph(fit_obj$pairwise$wadj, # weighted adjacency matrix as input
                                   layout = 'spring', 
                                   vsize=c(prev_all_north,3),
                                   pie = pred_obj$error[,2], # provide errors as input
                                   pieColor = rep('#377EB8',ncol(data_sub_all)),
                                   edge.color = fit_obj$pairwise$edgecolor,
                                   labels = colnames(data_sub_all))#,
                                  # groups=as.factor(g2_sg$membership))

# análise de comunidades 
# https://www.nature.com/articles/srep30750
g2<-as.igraph(Graph_Ising_predictability)
# se não tiver arestas negativas:
# g2_cl<-cluster_louvain(g2)

# se houver arestas negativas
g2_sg<-spinglass.community(g2,implementation = "neg")

Graph_Ising_predictability<-qgraph(fit_obj$pairwise$wadj, # weighted adjacency matrix as input
                                   layout = 'spring', 
                                   vsize=prev_all_north,
                                   pie = pred_obj$error[,2], # provide errors as input
                                   pieColor = rep('#377EB8',ncol(data_sub_all)),
                                   edge.color = fit_obj$pairwise$edgecolor,
                                   labels = colnames(data_sub_all),
                                   groups=as.factor(g2_sg$membership))

#Flow
# qgraph::flow(Graph_Ising_predictability, "RTI", theme = "colorblind")#, vsize = prev_all2)

#network comparison analysis
NCTObaRom <- NCT(data_complete2, 
                 data_complete3, 
                 gamma=0.5,
                 it = 1000, 
                 AND=TRUE,
                 binary.data = TRUE, 
                 paired = FALSE, 
                 test.edges = TRUE, 
                 edges = 'all')
NCTObaRom$glstrinv.real
NCTObaRom$glstrinv.pval
NCTObaRom$nwinv.real
NCTObaRom$nwinv.pval
NCTObaRom$einv.pvals




