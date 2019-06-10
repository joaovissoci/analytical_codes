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
data_sub$educ <- factor(data_sub$educ, labels = c("no schooling", "elementary/equivalent", "secondary/equivalent", "post-secondary/equivalent"))
# no need to recode following: sex, cor, regiao 

#recode RTI
# str(data_sub$O009)
data_sub$RTI<- car::recode (data_sub$O009, "2=0; 1=1") 

#recode alcohol
data_sub %>%
  mutate(Alcohol=case_when(P027 == "1"|
                             P027== "2"|
                             P032==  "0" ~ 0,
                           P032== "1" ~ 1)) -> data_sub 



#creat smaller dataset with just diseases
newdata4 <- with (data_sub, data.frame(HTN, diab2, arth_rheu, chron_kidfailure, copd, ocd, depression, heart_attack, stroke, spinalprob, angina, asthma, work_osteomusc, emphysema, cancer, bipolar, schizo, chron_bronch, RTI, heart_failure,  Alcohol))

#creat smaller dataset with diseases + demographic variables
newdata5 <- with (data_sub, data.frame(HTN, diab2, arth_rheu, chron_kidfailure, copd, heart_failure, ocd, depression, heart_attack, stroke, spinalprob, angina, asthma, work_osteomusc, emphysema, cancer, bipolar, schizo, chron_bronch, RTI, Alcohol, sex, cor, educ, agecat, regiao))

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
#PRE-PROCESSING
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


#Part 1: Association between MM and RTI in entire population
# #model 1:

# model_1 <- IsingFit(as.matrix(data4_complete), gamma = 0.25,plot=FALSE)
# Graph_Ising1 <- qgraph(model_1$weiadj, layout = "spring", vsize=prev_all)
   
#modify data4_complete
#data4_complete_mod <- with (data4_complete, data.frame(HTN, diab2, arth_rheu, chron_kidfailure, copd, depression, heart_attack, stroke, spinalprob, angina, asthma, work_osteomusc, emphysema, cancer,chron_bronch, RTI, heart_failure, Alcohol, sex, educ, agecat))

#Model 1:
# model_1b <- IsingFit(as.matrix(data4_complete_mod), gamma = 0.25,plot=FALSE)
# Graph_Ising1b <- qgraph(model_1b$weiadj, 
#                        layout = "spring",
#                        vsize=prev_all2)

#Centrality measures

# centRes <- centrality(Graph_Ising1b) #tells us about node strength
# centRes$OutDegree
# centRes$Closeness
# centRes$Betweenness
# centRes$ShortestPathLength
# centralityPlot(Graph_Ising1b)

# #Centrality
# centralityPlot(Graph_Ising1b, include = c("ExpectedInfluence")) 

#Flow
# flow(Graph_Ising1, "RTI", theme = "colorblind")#, vsize = prev_all)

# análise de comunidades 
# https://www.nature.com/articles/srep30750
# g2<-as.igraph(Graph_Ising1b)
# # se não tiver arestas negativas:
# g2_cl<-cluster_louvain(g2)

# # se houver arestas negativas
# g2_sg<-spinglass.community(g2,implementation = "neg")

# # qgraph(Graph_Ising1,groups=as.factor(g2_cl$membership))

# Graph_Ising2b <- qgraph(model_1b$weiadj, 
#                         layout = "spring",
#                         vsize=prev_all2,
#                         groups=as.factor(g2_sg$membership))


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
qgraph::flow(Graph_Ising_predictability, "RTI", theme = "colorblind")#, vsize = prev_all2)


#Part 2--People with RTI only
north_subset <-subset(data_sub_all, Banco_PNS_2013_elderly$regiao == 5)

#prevalences

prev_allb<-c(prev_hyp[2]+20,
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


#model 2
fit_obj <- mgm(data = north_subset, 
               type = rep('c', ncol(north_subset)),
               level = rep(2,ncol(north_subset)),
               lambdaSel = 'EBIC',
               lambdaGam=0.25,
               ruleReg = 'AND',
               binarySign=TRUE)


#Computing predictability nodes

pred_obj <- predict(object = fit_obj, 
                    data = north_subset, 
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
                                   pieColor = rep('#377EB8',ncol(north_subset)),
                                   edge.color = fit_obj$pairwise$edgecolor,
                                   labels = colnames(north_subset))#,
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


# #section with model 3 (people without RTI)


# NoRTI_subset <-subset(data_sub, RTI == 0)

# data_part3 <- with(NoRTI_subset, data.frame(has2, diab2, artreu, insre, dpoc, dxdepre, infarto, derr, probcol, angina, asma, dort, enfis, ca, bronq, inscard,  Alcohol) )
# data_part3[complete.cases(data_part3), ] 
# data_part3_complete <- data_part3[complete.cases(data_part3), ] 

# #prevalences

# prev_all_3 <-c(prev_hyp2[2]+19,
#                 prev_diab[2]+9,
#                 prev_angina2[2]+6,
#                 prev_asma2[2]+7,
#                 prev_artreu2[2]+9,
#                 prev_dort2[2]+7,
#                 prev_depre2[2]+12,
#                 prev_alcohol2[2 ] + 15, 
#                 prev_enfis2[2]+4,
#                 prev_ca2[2]+5,
#                 prev_insre2[2]+8,
#                 prev_probcol2[2]+10, 
#                 prev_bronq2 [2] + 5 ,
#                 prev_derr2 [2] + 6,
#                 prev_dpoc2 [2] + 6 ,
#                 prev_infarto2 [2] + 7,
#                 prev_inscard2 [2] + 8)


# #model 3
# model_3 <- IsingFit(as.matrix(data_part3_complete), gamma = 0.25,plot=FALSE)
# Graph_Ising3 <- qgraph(model_3$weiadj, 
#                        layout = "spring",
#                        vsize=prev_all_3)

# fit_obj_norti <- mgm(data = data_part3_complete, 
#                    type = rep('c', ncol(data_part3_complete)),
#                    level = rep(2, ncol(data_part3_complete)),
#                    lambdaSel = 'EBIC',
#                    lambdaGam=0.5,
#                    ruleReg = 'AND',
#                    binarySign=TRUE)

# Graph_Ising_norti <- qgraph(fit_obj_norti$pairwise$wadj,layout = "spring", vsize=prev_all_3,
#                           labels = colnames(data_part3_complete))


# #community analysis part 2
# g2_norti <-as.igraph(Graph_Ising_norti)

# # se não tiver arestas negativas:
# g2_cl_norti<-cluster_louvain(g2_norti)

# # se houver arestas negativas
# g2_sg_norti<-spinglass.community(g2,implementation = "neg")


# #Computing predictability nodes

# pred_obj_norti <- predict(object = fit_obj_norti, 
#                         data = data_part3_complete, 
#                         errorCat = 'CC')

# Graph_Ising_predictability_norti <-qgraph(fit_obj_norti$pairwise$wadj, # weighted adjacency matrix as input
#                                         layout = 'spring', 
#                                         vsize=prev_all_3,
#                                         pie = pred_obj_norti$error[,2], # provide errors as input
#                                         pieColor = rep('#377EB8',ncol(data_part3_complete)),
#                                         edge.color = fit_obj_norti$pairwise$edgecolor,
#                                         labels = colnames(data_part3_complete),
#                                         groups=as.factor(g2_cl_norti$membership))

# centralityPlot(Graph_Ising_norti, include = c("ExpectedInfluence"))


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




