#######################################################################################
#example_metanalysis.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. see full license at the end of this file.
#######################################################################################
#this script follows a combination of the guidelines proposed by Hadley Wickham http://goo.gl/c04kq as well as using the formatR package http://goo.gl/ri6ky
#if this is the first time you are conducting an analysis using this protocol, please watch http://goo.gl/DajIN while following step by step

#link to manuscript

#####################################################################################
#SETTING ENVIRONMENT
#####################################################################################
#Load packages (after installed) with the library function
library(metafor)
library(meta)

##############################################################################
#IMPORTING DATA AND RECODING
##############################################################################
#Importing data set from the Spredsheet in google docs (Insert link)

##Call continuous data dataset
dados <- read.csv("/home/joao/Dropbox/datasets/pro esporte/hope_SR/hope_correlation_sr.csv", header = TRUE)

network_data <- read.csv("/home/joao/Dropbox/datasets/pro esporte/hope_SR/hope_network_SR.csv", header = TRUE)

mean_data <- read.csv("/home/joao/Dropbox/datasets/pro esporte/hope_SR/hope_mean_sr.csv", header = TRUE)

####################################################################
#Example from Meta Package: Correlation data
####################################################################
## Example 1
data_cor<-na.omit(with(dados,data.frame(author,n=N,cor=Tarefa)))

meta1 <- metacor(cor, n, data=data_cor,sm="COR")
summary(meta1)
forest(meta1)
funnel(meta1)
metainf(meta1)
metainf(meta1, pooled="random")

## Example 1
data_cor<-na.omit(with(dados,data.frame(author,n=N,cor=Ego)))

meta1 <- metacor(cor, n, data=data_cor,sm="COR")
summary(meta1)
forest(meta1)
funnel(meta1)
metainf(meta1)
metainf(meta1, pooled="random")

## Example 1
data_cor<-na.omit(with(dados,data.frame(author,n=N,cor=Autoeficacia1)))

meta1 <- metacor(cor, n, data=data_cor,sm="COR")
summary(meta1)
forest(meta1)
funnel(meta1)
metainf(meta1)
metainf(meta1, pooled="random")

## Example 1
data_cor<-na.omit(with(dados,data.frame(author,n=N,cor=Autoeficacia2)))

meta1 <- metacor(cor, n, data=data_cor,sm="COR")
summary(meta1)
forest(meta1)
funnel(meta1)
metainf(meta1)
metainf(meta1, pooled="random")

## Example 1
data_cor<-na.omit(with(dados,data.frame(author,n=N,cor=Afetopositivo1)))

meta1 <- metacor(cor, n, data=data_cor,sm="COR")
summary(meta1)
forest(meta1)
funnel(meta1)
metainf(meta1)
metainf(meta1, pooled="random")

## Example 1
data_cor<-na.omit(with(dados,data.frame(author,n=N,cor=Afetopositivo2)))

meta1 <- metacor(cor, n, data=data_cor,sm="COR")
summary(meta1)
forest(meta1)
funnel(meta1)
metainf(meta1)
metainf(meta1, pooled="random")

#########################################################
#METASSUMARIZATION
#########################################################

NAto0<-function(x){
	car::recode(x,"NA=0")
}

network_data<-lapply(network_data,NAto0)

effect_size<-colSums(as.data.frame(network_data[-1]))

effect_sizes<-as.matrix(effect_size/dim(as.data.frame(network_data))[1])

intensity<-length(dim(effect_sizes>=0.25))
intensity_data<-rowSums(as.data.frame(data_ef4))

intensity_sizes<-intensity_data/intensity

#########################################################
#NETWORK ANALYSIS
#########################################################
#data_network<-remove.vars(data,c("Study"))
data_network<-as.matrix(as.data.frame(network_data[-1]))
network_data <- t(as.matrix(data_network)) %*% as.matrix(data_network)
#study_data<-as.matrix(data_network)
#rownames(study_data)<-network_data$author
#network_data<-rbind(variable_data,study_data)
#network_data <- (as.matrix(network_data)) %*% t(as.matrix(network_data))
diag(network_data) <- 0
names<-c(c("Desempenho","Maestria","Coping","Motivação","Controle emocional","Orgulho","Entusiasmo","Auto-eficácia","Treinamento","Medo","Esforço mental","Estresse e Burnout","Otimismo","Resiliência","Perseverança","Afeto positivo","Afeto negativo","Reabilitação","Bem-estar","Suporte social","Perfeccionismo","Auto-estima","Educação","Confiança"))

size_edges<-c(effect_sizes[,1]*30)
color<-c("gold","gold","gold","gold","gold","gold","gold","gold","gold","red","gold","red","gold","gold","gold","gold","red","gold","gold","gold","gold","gold","gold","gold")
#shape<-c(rep("circle",7),rep("square",26)) 
#label.cex<- c(rep(1.5,7),rep(1.0,26))
#groups<-c("Ensaio Clínico","Medicamentos","Outras Razões")

tiff("/home/joao/Desktop/hope_network_SR.tiff", width = 1000, height = 700,compression = 'lzw')
network_meta <- qgraph(network_data,layout = "spring",minimum=0,cut=1,labels=names,label.scale=FALSE,vsize=size_edges,color=color,borders = FALSE,label.cex = 1,posCol = "grey")#,posCol = "grey"),shape=shape
legend(0.8,-0.8, bty=".",c("Ensaio Clínico","Medicamentos","Outras Razões"),cex=1.2,fill=c("lightblue","red","yellow"))
#legend(-1.32,-0.5	, bty="n",c("EA: Efeitos Adversos","OT: Outro Tratamento","ECR: Questões com o ECR","FR: Falha no Retorno","MD: Problemas com medicamentos","ST: Melhora nos Sintomas","QF: Questões Familiares","OU: Outras Razões"),cex=1.2)
dev.off()



<-100*((RolandMorrisSUM - 0)/(24-0))


### load BCG vaccine data
mean_meta<-with(mean_data,data.frame(N,Esperança.M,Esperanca.SD))
### calculate the pooled raw mean in a metanalysis model where mi = mean, sdi = sd and ni = sample size
dat <- escalc(measure="MN", mi=Esperança.M, sdi=Esperanca.SD, ni=N, data=mean_meta)
### random-effects model (method="REML" is default, so technically not needed)
res<-rma(yi, vi, data=dat, method="REML")
rma(yi, sei=sqrt(vi), data=dat, method="REML")

forest.rma(res)