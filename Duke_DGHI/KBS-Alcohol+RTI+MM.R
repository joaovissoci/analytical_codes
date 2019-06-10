##Uso do alcool dentro da rede do RTI

##Instalar os pacotes importantes/Install important packages
# install.packages ("IsingFit")
# install.packages ("qgraph")
# install.packages ("ggplot2")
# install.packages("car")
# install.packages("mgm")
# install.packages("NetworkComparisonTest")

library(qgraph)
library(IsingFit)
library(ggplot2)
library(car)
library(tidyverse)
library(haven)
library(igraph)
library(mgm)
library(NetworkComparisonTest)

#Abaixar bancos de dados/download dataset 
Banco_PNS_Original <- read_dta("/Users/Joao/Downloads/Banco_PNS_Original.dta 4") 

#Recodificar alguns variaveis/Recoding some variables
Banco_PNS_Original$RTI <- car::recode(Banco_PNS_Original$O009, " 1=1; 2=0 ")
Banco_PNS_Original$HAS <- car::recode(Banco_PNS_Original$Q002, " 1=1; 2=0; 3=0 ")
Banco_PNS_Original$Diab <- car::recode(Banco_PNS_Original$Q030, " 1=1; 2=0; 3 =0 ")
Banco_PNS_Original$Coles<- car::recode(Banco_PNS_Original$Q060, "'1' = '1'; '2' = '0'")
Banco_PNS_Original$Cardiaco<- car::recode(Banco_PNS_Original$Q063, "'1' = '1'; '2' = '0'")
Banco_PNS_Original$AVC<- car::recode(Banco_PNS_Original$Q068, "'1' = '1'; '2' = '0'")
Banco_PNS_Original$Asma<- car::recode(Banco_PNS_Original$Q074, "'1' = '1'; '2' = '0'")
Banco_PNS_Original$Artri_reuma<- car::recode(Banco_PNS_Original$Q079, "'1' = '1'; '2' = '0'")
Banco_PNS_Original$DORT<- car::recode(Banco_PNS_Original$Q088, "'1' = '1'; '2' = '0'")
Banco_PNS_Original$Depres<- car::recode(Banco_PNS_Original$Q092, "'1' = '1'; '2' = '0'")
Banco_PNS_Original$Mental <- car::recode(Banco_PNS_Original$Q110, "'1' = '1'; '2' = '0'")
Banco_PNS_Original$Pulmo<- car::recode(Banco_PNS_Original$Q116, "'1' = '1'; '2' = '0'")
Banco_PNS_Original$Cancer<- car::recode(Banco_PNS_Original$Q120, "'1' = '1'; '2' = '0'")
Banco_PNS_Original$Renal<- car::recode(Banco_PNS_Original$Q124, "'1' = '1'; '2' = '0'")
Banco_PNS_Original$Coluna <- car::recode(Banco_PNS_Original$Q084, "'1' = '1'; '2' = '0'")
Banco_PNS_Original$Alcohol <- car::recode(Banco_PNS_Original$P032, "'1' = '1'; '2' = '0'")


#Criar um subjunto(do banco de dado) com variaveis importantes/create subset(of the dataset) with important variables
data_B <- with(Banco_PNS_Original, data.frame(RTI, HAS,Diab, Coles,Cardiaco, AVC, Asma, Artri_reuma, DORT, Depres, Mental, Pulmo, Cancer, Renal, Coluna, Alcohol))

# remover casos incompletos/ removing incomplete cases
# data_B[complete.cases(data_B), ] 
data_complete1 <- data_B[complete.cases(data_B), ] 

# Agora, tem 11,675 pessoas com casos completos (significa que eu removi pessoas com observacoes que sao missing/perdidos)

#Prevalencias
#Prevalences (proportion with disease/total)

table (data_complete1$RTI)
prev_rti <-prop.table(table(data_complete1$RTI))

table(data_complete1$HAS)
prev_has<-prop.table(table(data_complete1$HAS))

table(data_complete1$Diab)
prev_diab<-prop.table(table(data_complete1$Diab))

table(data_complete1$Coles)
prev_coles<-prop.table(table(data_complete1$Coles))

table(data_complete1$Cardiaco)
prev_cardiaco<-prop.table(table(data_complete1$Cardiaco))

table(data_complete1$AVC)
prev_avc<-prop.table(table(data_complete1$AVC))

table(data_complete1$Asma)
prev_asma<-prop.table(table(data_complete1$Asma))

table(data_complete1$Artri_reuma)
prev_artrireuma<-prop.table(table(data_complete1$Artri_reuma))

table(data_complete1$DORT)
prev_dort<-prop.table(table(data_complete1$DORT))

table(data_complete1$Depres)
prev_depre<-prop.table(table(data_complete1$Depres))

table(data_complete1$Mental)
prev_mental<-prop.table(table(data_complete1$Mental))

table(data_complete1$Pulmo)
prev_pulmo<-prop.table(table(data_complete1$Pulmo))

table(data_complete1$Cancer)
prev_cancer<-prop.table(table(data_complete1$Cancer))

table(data_complete1$Insufi_renal)
prev_renal<-prop.table(table(data_complete1$Renal))

table(data_complete1$Prob_Coluna)
prev_coluna<-prop.table(table(data_complete1$Coluna))

table(data_complete1$Alcohol)
prev_alcohol<-prop.table(table(data_complete1$Alcohol))

prev_all<-c(prev_has[2]+17,
            prev_alcohol[2]+23,
            prev_diab[2]+7,
            prev_coles[2]+12,
            prev_cardiaco[2]+6,
            prev_avc[2]+4,
            prev_rti[2]+7,
            prev_asma[2]+7,
            prev_artrireuma[2]+7,
            prev_dort[2]+5,
            prev_depre[2]+8,
            prev_mental[2]+4,
            prev_pulmo[2]+3,
            prev_cancer[2]+5,
            prev_renal[2]+5,
            prev_coluna[2]+13)

#Parte 1: associacao entre MM, alcohol e rti na amostra da populacao do PNS 
#modelo de rede 1

model_1 <- IsingFit(as.matrix(data_complete1), gamma = 0.25,plot=FALSE)
Graph_Ising1 <- qgraph(model_1$weiadj, 
                       layout = "spring",
                       vsize=prev_all)

                       
#Medindo centralidade (measuring centrality)
centRes <- centrality(Graph_Ising1) #tells us about node strength
centRes$OutDegree
centRes$Closeness
centRes$Betweenness
centRes$ShortestPathsLength
# centralityPlot(Graph_Ising1a)
#Centrality
centralityPlot(Graph_Ising1, include = c("ExpectedInfluence")) 

flow(Graph_Ising1, "RTI", theme = "colorblind")#, vsize = prev_all)

# análise de comunidades 
# https://www.nature.com/articles/srep30750
g2<-as.igraph(Graph_Ising1)

# se não tiver arestas negativas:
g2_cl<-spinglass.community(g2)
# g2_cl<-walktrap.community(g2)
g2_cl$membership


#Outras figuras (can't have this for some reason)
Graph_Ising1 <- qgraph(model_1$weiadj, 
                        layout = "spring",
                        vsize=prev_all)


fit_obj <- mgm(data = data_complete1, 
               type = rep('c', ncol(data_complete1)),
               level = rep(2, ncol(data_complete1)),
               lambdaSel = 'EBIC',
               lambdaGam=0.5,
               ruleReg = 'AND',
               binarySign=TRUE)


#Computing predictability nodes

pred_obj <- predict(object = fit_obj, 
                    data = data_complete1, 
                    errorCat = 'CC')

Graph_Ising1_predictability<-qgraph(fit_obj$pairwise$wadj, # weighted adjacency matrix as input
                                     layout = 'spring', 
                                     vsize=prev_all,
                                     pie = pred_obj$error[,2], # provide errors as input
                                     pieColor = rep('#377EB8',ncol(data_complete1)),
                                     edge.color = fit_obj$pairwise$edgecolor,
                                     labels = colnames(data_complete1),
                                     groups=as.factor(g2_cl$membership))

flow(Graph_Ising1_predictability, "RTI", theme = "colorblind")#, vsize = prev_all)

# So com pessos que tem suicidio

#Parte 2: padroes de multimorbidade e alcohol stratificado 
#A: pessoas que tem grande possibilidade de ter RTI

#Crie um subjunto de dados so com pessoas que tem cancer
rti_subset <-subset(Banco_PNS_Original, RTI == 1)

#novo banco de dados com variaveis importantes (eu nao inclui cancer como variavel porque estas pessoas ja tem cancer)
data_B2 <- with(rti_subset, data.frame(HAS, Diab, Coles, Cardiaco, AVC, Asma, Artri_reuma, DORT, Depres, Mental, Pulmo, Renal, Coluna, Cancer, Alcohol))

#casos completos/completed cases
data_B2[complete.cases(data_B2), ] 
data_complete2 <- data_B2[complete.cases(data_B2), ] 

#modelo de rede 2
model_2 <- IsingFit(as.matrix(data_complete2), gamma = 0.25, plot=FALSE) 
Graph_Ising2 <- qgraph(model_2$weiadj, layout = "spring")

fit_obj_rti <- mgm(data = data_complete2, 
                        type = rep('c', ncol(data_complete2)),
                        level = rep(2, ncol(data_complete2)),
                        lambdaSel = 'EBIC',
                        lambdaGam=0.5,
                        ruleReg = 'AND',
                        binarySign=TRUE)


#network model
prev_all<-c(prev_has[2]+17,
            prev_alcohol[2]+23,
            prev_diab[2]+7,
            prev_coles[2]+12,
            prev_cardiaco[2]+6,
            prev_avc[2]+4,
            prev_asma[2]+7,
            prev_artrireuma[2]+7,
            prev_dort[2]+5,
            prev_depre[2]+8,
            prev_mental[2]+4,
            prev_pulmo[2]+3,
            prev_cancer[2]+5,
            prev_renal[2]+5,
            prev_coluna[2]+13)



Graph_Ising_rti<- qgraph(fit_obj_rti$pairwise$wadj,layout = "spring", vsize=prev_allrti,
                              labels = colnames(data_complete2))



# análise de comunidades 
# https://www.nature.com/articles/srep30750
g2_suicidio<-as.igraph(Graph_Ising_suicidio)

# se não tiver arestas negativas:
g2_cl_cancer<-cluster_louvain(g2_cancer)

#Computing predictability nodes

pred_obj_rti <- predict(object = fit_obj_rti, 
                             data = data_complete2, 
                             errorCat = 'CC')


Graph_Ising_predictability_rti<-qgraph(fit_obj_cancer$pairwise$wadj, # weighted adjacency matrix as input
                                          layout = 'spring', 
                                          vsize=prev_allcancer,
                                          pie = pred_obj_cancer$error[,2], # provide errors as input
                                          pieColor = rep('#377EB8',ncol(data_complete2)),
                                          edge.color = fit_obj_rti$pairwise$edgecolor,
                                          labels = colnames(data_complete2),
                                          groups=as.factor(g2_cl_cancer$membership))

centralityPlot(Graph_Ising2, include = c("ExpectedInfluence"))

