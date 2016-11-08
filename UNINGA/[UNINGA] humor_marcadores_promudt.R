#####################################################################################
#BASIC R STATISTICS TEMPLATE
#####################################################################################
#
#
#
#
#
#####################################################################################
#SETTING ENVIRONMENT
#####################################################################################
#PASCKAGES INSTALLATION CODES
#install.packages("Hmisc")
#install.packages("car")
#install.packages("psych")
#install.packages("nortest")
#install.packages("ggplot2")
#install.packages("pastecs")
#install.packages("repmis")
#install.packages("mvnormtest")
#install.packages("polycor")

#PACKAGES LOADING CODE
#Load packages neededz for the analysis
#library(Hmisc)

#All packages must be installes with install.packages() function
lapply(c("sem","ggplot2", "psych", "RCurl", "irr", "nortest", "moments","GPArotation","nFactors","boot","psy", "car","vcd", "gridExtra","mi","VIM","epicalc","gdata","sqldf","reshape2","mclust","foreign","survival","memisc","foreign","qgraph"), library, character.only=T)

########################################################
#IMPORTING DATA
########################################################
#LOADING DATA FROM A .CSV FILE
data<-read.csv("/Users/jnv4/Desktop/mood_bio.csv",sep=",")

#information between " " are the path to the directory in your computer where the data is stored

#Import data from Dropbox, in .csv format
#Instructions here http://goo.gl/Ofa7gQ
#data1 <- repmis::source_DropboxData("pem_parasito.csv","tkxmkg9pybmtsgh",sep = ",",header = TRUE)

########################################################
#DATA MANAGEMENT
########################################################
#Creating a data frame (group of variables)
#numeric<-with(data, data.frame(Peso,Altura,IMC,
#                          Idade))
#
##Change variables properties
##Change variable to factor
#data$Classificacao<-as.factor(data$Classificacao)
#
##Change variable to character
#data$Classificacao<-as.character(data$Classificacao)
#
##Change variable to numeric
#data$Classificacao<-as.numeric(data$Classificacao)
#
##Recoding variables
#data$Classificacao<-car::recode(data$Classificacao,"#1='baixo';2='medio';
#	3='alto'")

#data <- base::merge(data1,data2,by=c("nome"))

########################################################
#DATA IMPUTATION
########################################################
# # generate imputations
# # argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
# imp <- mice(data, seed = 222222, m=5)

# # reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
# data_imputed<-complete(imp,1)

########################################################
#DESCRIPTIVES
########################################################
# Age
summary(data_imputed$anger)
ad.test(data_imputed$anger)
#hist(data_imputed$anger)
#ci_func(data_imputed$anger,.95)
by(data_imputed$anger,outcomes$rtc_involvement,describe)
wilcox.test(data_imputed$anger~outcomes$rtc_involvement)


########################################################
#NETWORK ANALYSIS
########################################################
###### Preparation #######################################################
network_data<-subset(data,data$moment=='prep')
network_data<-remove.vars(network_data,c("moment"))

cor_matrix<-cor(network_data,method="spearman")
#qsgc<-qsgc$rho
# cor_matrix<-Hmisc::rcorr(as.matrix(network_data),type=c("spearman"))

groups<-list(Humor=c(1,2,3,4,5,6),Biomarkers=c(7,8,9,10))
varLabels<-c("Raiva", "Tensao", "Depressao","Vigor","Fadiga","Confusao","TBARS","TIOIS","CAT","SOD")
# varNames<-c("H1","H2","H3","H4","H5","H6","Bio1","Bio2","Bio3","Bio4","Bio5","Bio6")
normalize<-function(x){(x-min(x))/(max(x)-min(x))}
mean_data<-sapply(as.data.frame(sapply(network_data,normalize)),mean)
vSize<-mean_data*15

#graph_glasso<-qgraph(cor_matrix,layout="spring",vsize=6,esize=20,graph="glasso",sampleSize=nrow(network_data),legend.cex = 0.5,GLratio=1.5)
#graph_pcor<-qgraph(cor_matrix,layout="spring",vsize=6,esize=20,graph="pcor",sampleSize=nrow(network_data),legend.cex = 0.5,GLratio=1.5)#r=threshold='holm'
#graph_cor<-qgraph(cor_matrix,layout="spring",vsize=6,esize=20,legend.cex = 0.5,GLratio=1.5)
#graph_layout<-averageLayout(graph_glasso,graph_pcor,graph_cor)

PcorGRAPH<-qgraph(cor_matrix,
	layout="spring",
	vsize=vSize,
	esize=20,
	graph="pcor",
	legend.cex = 0.3,
	cut = 0.4, 
	maximum = 1, 
	minimum = 0.2, 
	repulsion = 0.8,
	groups=groups,
	gray=FALSE,
	color=c("steelblue","gold"),
	legend=TRUE,
	labels=varLabels,
	label.scale=FALSE)
	# nodeNames=varLabels)
	# labels=varNames)#,layout=graph_layout
#Findal PCOR representation
#qsgG2<-qgraph(cor_matrix,layout=Lqsg,vsize=6,esize=20,graph="pcor",legend.cex = 0.3,cut = 0.1, maximum = 1, minimum = 0, esize = 20,vsize = 5, repulsion = 0.8,groups=qsggr,gray=TRUE,color=c("gray80","gray50"),legend=F)#,nodeNames=nomesqsg
#Final GLASSO representation
#qsgG3<-qgraph(cor_matrix,layout=Lqsg,vsize=vSize*3,esize=20,graph="glasso",sampleSize=nrow(network_data),legend.cex = 0.6,cut = 0.1, maximum = 1, minimum = 0, esize = 20,vsize = 5, repulsion = 0.8,groups=qsggr,nodeNames=nomesqsg,color=c("gold","steelblue","red","grey80"),borders = FALSE,labels=varNames)#,gray=T,)#,nodeNames=nomesqsg
#tiff("/home/joao/Desktop/importance_network.tiff", width = 1200, height = 700,compression = 'lzw')
#dev.off()
#legend(0.8,-0.8, bty=".",c("Ensaio Clínico","Medicamentos","Outras Razões"),cex=1.2,fill=c("lightblue","red","yellow"))
#centralityPlot(qsgG3)
#clusteringPlot(qsgG3)
#g<-as.igraph(PcorGRAPH)
#h<-walktrap.community(g)
#plot(h,g)

# Para identificar no qgraph o resultado do algortimo de comunidade, criar objeto de "groups"
# com o resultado de wcG1
predictors<-centrality(PcorGRAPH)
#centralityPlot(qsgG3)

#as.data.frame(predictors[[1]])[2,]
#dim(as.data.frame(predictors[[1]]))[1]

#qsgG3$Edgelist$from
#qsgG3$Edgelist$to
#qsgG3$Edgelist$weight

#subset(qsgG3$Edgelist$weight,qsgG3$Edgelist$from==1 & qsgG3$Edgelist$to==15)
#subset(qsgG3$Edgelist$weight,qsgG3$Edgelist$from==2 & qsgG3$Edgelist$to==15)
#subset(qsgG3$Edgelist$weight,qsgG3$Edgelist$from==3 & qsgG3$Edgelist$to==15)
#subset(qsgG3$Edgelist$weight,qsgG3$Edgelist$from==10 & qsgG3$Edgelist$to==15)
#subset(qsgG3$Edgelist$weight,qsgG3$Edgelist$from==13 & qsgG3$Edgelist$to==15)

#### PreCompetition #######################################################network_data<-subset(data,data$moment=='prep')
network_data<-subset(data,data$moment=='precomp')
network_data<-remove.vars(network_data,c("moment"))

cor_matrix<-cor(network_data,method="spearman")
#qsgc<-qsgc$rho

groups<-list(Humor=c(1,2,3,4,5,6),Biomarkers=c(7,8,9,10))
varLabels<-c("Raiva", "Tensao", "Depressao","Vigor","Fadiga","Confusao","TBARS","TIOIS","CAT","SOD")
# varNames<-c("H1","H2","H3","H4","H5","H6","Bio1","Bio2","Bio3","Bio4","Bio5","Bio6")
normalize<-function(x){(x-min(x))/(max(x)-min(x))}
mean_data<-sapply(as.data.frame(sapply(network_data,normalize)),mean)
vSize<-mean_data*15

#graph_glasso<-qgraph(cor_matrix,layout="spring",vsize=6,esize=20,graph="glasso",sampleSize=nrow(network_data),legend.cex = 0.5,GLratio=1.5)
#graph_pcor<-qgraph(cor_matrix,layout="spring",vsize=6,esize=20,graph="pcor",sampleSize=nrow(network_data),legend.cex = 0.5,GLratio=1.5)#r=threshold='holm'
#graph_cor<-qgraph(cor_matrix,layout="spring",vsize=6,esize=20,legend.cex = 0.5,GLratio=1.5)
#graph_layout<-averageLayout(graph_glasso,graph_pcor,graph_cor)

PcorGRAPH<-qgraph(cor_matrix,
	layout="spring",
	vsize=vSize,
	esize=20,
	graph="pcor",
	legend.cex = 0.3,
	cut = 0.4, 
	maximum = 1, 
	minimum = 0.2, 
	repulsion = 0.8,
	groups=groups,
	gray=FALSE,
	color=c("steelblue","gold"),
	legend=TRUE,
	labels=varLabels,
	label.scale=TRUE)
#Findal PCOR representation
#qsgG2<-qgraph(cor_matrix,layout=Lqsg,vsize=6,esize=20,graph="pcor",legend.cex = 0.3,cut = 0.1, maximum = 1, minimum = 0, esize = 20,vsize = 5, repulsion = 0.8,groups=qsggr,gray=TRUE,color=c("gray80","gray50"),legend=F)#,nodeNames=nomesqsg
#Final GLASSO representation
#qsgG3<-qgraph(cor_matrix,layout=Lqsg,vsize=vSize*3,esize=20,graph="glasso",sampleSize=nrow(network_data),legend.cex = 0.6,cut = 0.1, maximum = 1, minimum = 0, esize = 20,vsize = 5, repulsion = 0.8,groups=qsggr,nodeNames=nomesqsg,color=c("gold","steelblue","red","grey80"),borders = FALSE,labels=varNames)#,gray=T,)#,nodeNames=nomesqsg
#tiff("/home/joao/Desktop/importance_network.tiff", width = 1200, height = 700,compression = 'lzw')
#dev.off()
#legend(0.8,-0.8, bty=".",c("Ensaio Clínico","Medicamentos","Outras Razões"),cex=1.2,fill=c("lightblue","red","yellow"))
#centralityPlot(qsgG3)
#clusteringPlot(qsgG3)
#g<-as.igraph(PcorGRAPH)
#h<-walktrap.community(g)
#plot(h,g)

# Para identificar no qgraph o resultado do algortimo de comunidade, criar objeto de "groups"
# com o resultado de wcG1
predictors<-centrality(PcorGRAPH)
#centralityPlot(qsgG3)

PcorGRAPH$Edgelist

data.frame(PcorGRAPH$Edgelist$from,PcorGRAPH$Edgelist$to,PcorGRAPH$Edgelist$weight)

#as.data.frame(predictors[[1]])[2,]
#dim(as.data.frame(predictors[[1]]))[1]

#qsgG3$Edgelist$from
#qsgG3$Edgelist$to
#qsgG3$Edgelist$weight

#subset(qsgG3$Edgelist$weight,qsgG3$Edgelist$from==1 & qsgG3$Edgelist$to==15)
#subset(qsgG3$Edgelist$weight,qsgG3$Edgelist$from==2 & qsgG3$Edgelist$to==15)
#subset(qsgG3$Edgelist$weight,qsgG3$Edgelist$from==3 & qsgG3$Edgelist$to==15)
#subset(qsgG3$Edgelist$weight,qsgG3$Edgelist$from==10 & qsgG3$Edgelist$to==15)
#subset(qsgG3$Edgelist$weight,qsgG3$Edgelist$from==13 & qsgG3$Edgelist$to==15)

#### Regeneration #######################################################
network_data<-subset(data,data$moment=='comp')
network_data<-remove.vars(network_data,c("moment"))

cor_matrix<-cor(network_data,method="spearman")
#qsgc<-qsgc$rho

groups<-list(Humor=c(1,2,3,4,5,6),Biomarkers=c(7,8,9,10))
varLabels<-c("Raiva", "Tensao", "Depressao","Vigor","Fadiga","Confusao","TBARS","TIOIS","CAT","SOD")
# varNames<-c("H1","H2","H3","H4","H5","H6","Bio1","Bio2","Bio3","Bio4","Bio5","Bio6")
normalize<-function(x){(x-min(x))/(max(x)-min(x))}
mean_data<-sapply(as.data.frame(sapply(network_data,normalize)),mean)
vSize<-mean_data*15

#graph_glasso<-qgraph(cor_matrix,layout="spring",vsize=6,esize=20,graph="glasso",sampleSize=nrow(network_data),legend.cex = 0.5,GLratio=1.5)
#graph_pcor<-qgraph(cor_matrix,layout="spring",vsize=6,esize=20,graph="pcor",sampleSize=nrow(network_data),legend.cex = 0.5,GLratio=1.5)#r=threshold='holm'
#graph_cor<-qgraph(cor_matrix,layout="spring",vsize=6,esize=20,legend.cex = 0.5,GLratio=1.5)
#graph_layout<-averageLayout(graph_glasso,graph_pcor,graph_cor)

PcorGRAPH<-qgraph(cor_matrix,
	layout="spring",
	vsize=vSize,
	esize=20,
	graph="pcor",
	legend.cex = 0.3,
	cut = 0.4, 
	maximum = 1, 
	minimum = 0.2, 
	repulsion = 0.8,
	groups=groups,
	gray=FALSE,
	color=c("steelblue","gold"),
	legend=TRUE,
	labels=varLabels,
	label.scale=TRUE)
#Findal PCOR representation
#qsgG2<-qgraph(cor_matrix,layout=Lqsg,vsize=6,esize=20,graph="pcor",legend.cex = 0.3,cut = 0.1, maximum = 1, minimum = 0, esize = 20,vsize = 5, repulsion = 0.8,groups=qsggr,gray=TRUE,color=c("gray80","gray50"),legend=F)#,nodeNames=nomesqsg
#Final GLASSO representation
#qsgG3<-qgraph(cor_matrix,layout=Lqsg,vsize=vSize*3,esize=20,graph="glasso",sampleSize=nrow(network_data),legend.cex = 0.6,cut = 0.1, maximum = 1, minimum = 0, esize = 20,vsize = 5, repulsion = 0.8,groups=qsggr,nodeNames=nomesqsg,color=c("gold","steelblue","red","grey80"),borders = FALSE,labels=varNames)#,gray=T,)#,nodeNames=nomesqsg
#tiff("/home/joao/Desktop/importance_network.tiff", width = 1200, height = 700,compression = 'lzw')
#dev.off()
#legend(0.8,-0.8, bty=".",c("Ensaio Clínico","Medicamentos","Outras Razões"),cex=1.2,fill=c("lightblue","red","yellow"))
#centralityPlot(qsgG3)
#clusteringPlot(qsgG3)
#g<-as.igraph(PcorGRAPH)
#h<-walktrap.community(g)
#plot(h,g)

# Para identificar no qgraph o resultado do algortimo de comunidade, criar objeto de "groups"
# com o resultado de wcG1
#predictors<-centrality(qsgG3)$ShortestPaths[,15]
#centralityPlot(qsgG3)

#as.data.frame(predictors[[1]])[2,]
#dim(as.data.frame(predictors[[1]]))[1]

#qsgG3$Edgelist$from
#qsgG3$Edgelist$to
#qsgG3$Edgelist$weight

#subset(qsgG3$Edgelist$weight,qsgG3$Edgelist$from==1 & qsgG3$Edgelist$to==15)
#subset(qsgG3$Edgelist$weight,qsgG3$Edgelist$from==2 & qsgG3$Edgelist$to==15)
#subset(qsgG3$Edgelist$weight,qsgG3$Edgelist$from==3 & qsgG3$Edgelist$to==15)
#subset(qsgG3$Edgelist$weight,qsgG3$Edgelist$from==10 & qsgG3$Edgelist$to==15)
#subset(qsgG3$Edgelist$weight,qsgG3$Edgelist$from==13 & qsgG3$Edgelist$to==15)

#### Regeneration #######################################################
network_data<-subset(data,data$moment=='reg')
network_data<-remove.vars(network_data,c("moment"))

cor_matrix<-cor(network_data,method="spearman")
#qsgc<-qsgc$rho

groups<-list(Humor=c(1,2,3,4,5,6),Biomarkers=c(7,8,9,10))
varLabels<-c("Raiva", "Tensao", "Depressao","Vigor","Fadiga","Confusao","TBARS","TIOIS","CAT","SOD")
# varNames<-c("H1","H2","H3","H4","H5","H6","Bio1","Bio2","Bio3","Bio4","Bio5","Bio6")
normalize<-function(x){(x-min(x))/(max(x)-min(x))}
mean_data<-sapply(as.data.frame(sapply(network_data,normalize)),mean)
vSize<-mean_data*15

#graph_glasso<-qgraph(cor_matrix,layout="spring",vsize=6,esize=20,graph="glasso",sampleSize=nrow(network_data),legend.cex = 0.5,GLratio=1.5)
#graph_pcor<-qgraph(cor_matrix,layout="spring",vsize=6,esize=20,graph="pcor",sampleSize=nrow(network_data),legend.cex = 0.5,GLratio=1.5)#r=threshold='holm'
#graph_cor<-qgraph(cor_matrix,layout="spring",vsize=6,esize=20,legend.cex = 0.5,GLratio=1.5)
#graph_layout<-averageLayout(graph_glasso,graph_pcor,graph_cor)

PcorGRAPH<-qgraph(cor_matrix,
	layout="spring",
	vsize=vSize,
	esize=20,
	graph="pcor",
	legend.cex = 0.3,
	cut = 0.4, 
	maximum = 1, 
	minimum = 0.2, 
	repulsion = 0.8,
	groups=groups,
	gray=FALSE,
	color=c("steelblue","gold"),
	legend=TRUE,
	labels=varLabels,
	label.scale=TRUE)
#Findal PCOR representation
#qsgG2<-qgraph(cor_matrix,layout=Lqsg,vsize=6,esize=20,graph="pcor",legend.cex = 0.3,cut = 0.1, maximum = 1, minimum = 0, esize = 20,vsize = 5, repulsion = 0.8,groups=qsggr,gray=TRUE,color=c("gray80","gray50"),legend=F)#,nodeNames=nomesqsg
#Final GLASSO representation
#qsgG3<-qgraph(cor_matrix,layout=Lqsg,vsize=vSize*3,esize=20,graph="glasso",sampleSize=nrow(network_data),legend.cex = 0.6,cut = 0.1, maximum = 1, minimum = 0, esize = 20,vsize = 5, repulsion = 0.8,groups=qsggr,nodeNames=nomesqsg,color=c("gold","steelblue","red","grey80"),borders = FALSE,labels=varNames)#,gray=T,)#,nodeNames=nomesqsg
#tiff("/home/joao/Desktop/importance_network.tiff", width = 1200, height = 700,compression = 'lzw')
#dev.off()
#legend(0.8,-0.8, bty=".",c("Ensaio Clínico","Medicamentos","Outras Razões"),cex=1.2,fill=c("lightblue","red","yellow"))
#centralityPlot(qsgG3)
#clusteringPlot(qsgG3)
#g<-as.igraph(PcorGRAPH)
#h<-walktrap.community(g)
#plot(h,g)

# Para identificar no qgraph o resultado do algortimo de comunidade, criar objeto de "groups"
# com o resultado de wcG1
#predictors<-centrality(qsgG3)$ShortestPaths[,15]
#centralityPlot(qsgG3)

#as.data.frame(predictors[[1]])[2,]
#dim(as.data.frame(predictors[[1]]))[1]

#qsgG3$Edgelist$from
#qsgG3$Edgelist$to
#qsgG3$Edgelist$weight

#subset(qsgG3$Edgelist$weight,qsgG3$Edgelist$from==1 & qsgG3$Edgelist$to==15)
#subset(qsgG3$Edgelist$weight,qsgG3$Edgelist$from==2 & qsgG3$Edgelist$to==15)
#subset(qsgG3$Edgelist$weight,qsgG3$Edgelist$from==3 & qsgG3$Edgelist$to==15)
#subset(qsgG3$Edgelist$weight,qsgG3$Edgelist$from==10 & qsgG3$Edgelist$to==15)
#subset(qsgG3$Edgelist$weight,qsgG3$Edgelist$from==13 & qsgG3$Edgelist$to==15)