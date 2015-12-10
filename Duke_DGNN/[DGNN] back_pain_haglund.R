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
lapply(c("Hmisc","car","psych","nortest","ggplot2","pastecs","repmis","mvnormtest","polycor"), library, character.only=T)
#####################################################################################
#IMPORTING DATA
#####################################################################################
#LOADING DATA FROM A .CSV FILE
data<-read.csv("/home/joao/Dropbox/datasets/DGNN/back_pain/backPainHaglund_3.csv",sep=",")
#information between " " are the path to the directory in your computer where the data is storedtest

data_year2_test<-read.csv("/home/joao/Dropbox/datasets/DGNN/back_pain/backPainHaglund_2_year2.csv")

#Import data from Dropbox, in .csv format
#Instructions here http://goo.gl/Ofa7gQ
#data1 <- repmis::source_DropboxData("pem_parasito.csv",
#                                  "tkxmkg9pybmtsgh",
#                                  sep = ",",
#                                  header = TRUE)

data_longitudinal_test<-read.csv("/Users/joaovissoci/Dropbox/datasets/DGNN/back_pain/back_pain_longitudinal_data.csv")

###########################################################################################
#DATA MANAGEMENT
###########################################################################################
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

data$group<-"Year 1"

data_year2<-t(data_year2_test[,-1])
colnames(data_year2)<-data_year2_test[,1]
data_year2$group<-"Year 2"
data_year2<-as.data.frame(data_year2)

data_longitudinal<-t(data_longitudinal_test[,-1])
colnames(data_longitudinal)<-data_longitudinal_test[,1]
data_longitudinal<-as.data.frame(data_longitudinal)
data_longitudinal[,1]<-as.numeric(as.character(data_longitudinal[,1]))
data_longitudinal[,2]<-as.numeric(as.character(data_longitudinal[,2]))
#####################################################################################
#BASIC DESCRIPTIVES and EXPLORATORY ANALYSIS
#####################################################################################
###Section wih several exploratory data analysis functions
#Exploratory Data Anlysis
#dim(data)
#str (data)
#head(data)
#names(data)
#summary(data)#This comand will provide a whole set of descriptive #results for each variables
describe(data)
with(data,by(data,outcome,describe))
with(data,by(data,outcome,summary))
#stat.desc(data)
with(data,by(data,outcome,ad.test)) # Anderson-Darling test for normality
#skewness(data$Idade) #Will provide skweness analysis
#kurtosis(data$Idade) - 3 #Will provide kurtosis analysis
#qplot(data$Idade) # histogram plot
#boxplot(data$Idade~data$Classificacao) #will provide a boxplot for the #variables to analysis potential outliers
## Bartlett Test of Homogeneity of Variances
#bartlett.test(data$Idade~data$Classificacao, data=data)
## Figner-Killeen Test of Homogeneity of Variances
#fligner.test(data$Idade~data$Classificacao, data=data)
#leveneTest(data$Idade~data$Classificacao, data=data)

#########################################################
#ANALYSIS OF VARIANCE
#########################################################
#YEAR 1
with(data_cleande,by(introduction,FUP,describe))
with(data_cleande,by(introduction,FUP,ad.test)) # Anderson-Darling test for normality
# One Way Anova (Completely Randomized Design)
fit <- aov(introduction ~ FUP, data=data)
summary(fit)

#data_plot<-reshape()

ggplot(data=data, aes(x=as.factor(year), y=general)) + geom_boxplot(fill="grey") + ylab("Resident Performance") + xlab("Time") + theme_bw()

#YEAR 2
ggplot(data=data_year2, aes(x=as.factor(Year_2), y=General_2)) + geom_boxplot(fill="grey") + ylab("Resident Performance") + xlab("Time") + theme_bw()


#Comparison YEAR 1 Vs. YEAR 2
comparison_data<-c(data$general,data_year2$General_2)
comparison_data2<-c(data$group,data_year2$group)
comparison_data3<-c(data$year,data_year2$Year_2)
comparison<-data.frame(value=comparison_data,group=comparison_data2)

ggplot(data=comparison, aes(x=group, y=value)) + geom_boxplot(fill="grey") + ylab("Resident Performance") + xlab("Year") + theme_bw()

with(comparison,wilcox.test(value~group))

comparison<-data.frame(value=comparison_data,group=comparison_data2,year=comparison_data3)
ggplot(data=comparison, aes(x=as.factor(year), y=value, fill=group)) + geom_boxplot() + ylab("Resident Performance") + xlab("Year") + theme_bw()

#########################################################
#Momentos comparison
#########################################################
#Comparison YEAR 1 Vs. YEAR 2
with(data_longitudinal,describeBy(overall_performance,moment))
with(data_longitudinal,wilcox.test(overall_performance~moment,paired=TRUE))

ggplot(data=data_longitudinal, aes(x=moment, y=overall_performance)) + geom_boxplot(fill="grey") + ylab("Resident Performance") + xlab("Year") + theme_bw()

with(data_longitudinal,describeBy(general_score,moment))
with(data_longitudinal,wilcox.test(general_score~moment,paired=TRUE))

ggplot(data=data_longitudinal, aes(x=moment, y=general_score)) + geom_boxplot(fill="grey") + ylab("Resident General Score") + xlab("Year") + theme_bw()

#########################################################
#Network
#########################################################
#Correlation dataset
cor_data<-data[,-c(1,25,26)]
cor_data<-na.omit(cor_data)
# generating correlation matrix
network_data<-cor(cor_data,method="spearman")
network_data<-cor_auto(cor_data)

library(reshape2)
melted_cormat <- melt(network_data)
head(melted_cormat)

ggplot(melted_cormat, aes(y=X1, x=X2, fill=value)) + geom_tile() + scale_fill_gradient2(low="darkblue",high="darkred", limits=c(-1,1))# + facet_grid(regions ~ .,scales="free_y",space="free") + geom_text(aes(y=X1, x=X2, label=value)) 

#visualizing correlation matrix with a network
#qgraph(cor_data,layout="spring",min=0.40)

varNames<-c("Interacted in a professional manner","Allowed patient to tell own story and raise questions","About the history of present illness","About quality and location of back pain","Whether the pain is continuous or intermittent","What makes pain better and worse","About radicular symptoms","Loss of motor function and bowel/bladder control","About significant past medical history","About systemic symptoms","What medications currently taking","Washed hands before OR after encounter","Considered patient comfort during exam","General appearance","Bend forward, backward and to both sides","Palpate spine","Deep tendon reflexes of knee","Deep tendon reflexes of ankle","Strength in leg","Sensation in both legs","Problems and plan communicated effectivel","Problem list","General impression of resident performance")


qsgg3<-qgraph(network_data,layout="spring",vsize=6,esize=20,graph="glasso",sampleSize=nrow(cor_data),legend.cex = 0.5,GLratio=1.5)
qsgg2<-qgraph(cor_data,layout="spring",vsize=6,esize=20,graph="pcor",threshold="holm",sampleSize=nrow(cor_data),legend.cex = 0.5,GLratio=1.5)
qsgg1<-qgraph(cor_data,layout="spring",vsize=6,esize=20,legend.cex = 0.5,GLratio=1.5)
Lqsg<-averageLayout(qsgg1,qsgg2,qsgg3)

qsgG1<-qgraph(cor_data,layout=Lqsg,vsize=6,esize=20,legend.cex = 0.3,cut = 0.3, maximum = 1, minimum = 0, esize = 20,vsize = 5, repulsion = 0.8,groups=qsggr,gray=TRUE,color=c("gray80","gray50"),legend=F)#nodeNames=nomesqsg,
qsgG2<-qgraph(cor_data,layout=Lqsg,vsize=6,esize=20,graph="pcor",legend.cex = 0.3,cut = 0.1, maximum = 1, minimum = 0, esize = 20,vsize = 5, repulsion = 0.8,groups=qsggr,gray=TRUE,color=c("gray80","gray50"),legend=F)#,nodeNames=nomesqsg

tiff("/home/joao/Desktop/back_pain_network.tiff", width = 1300, height = 700,compression = 'lzw')
qsgG3<-qgraph(network_data,layout="spring",esize=20,graph="glasso",sampleSize=nrow(cor_data),legend.cex = 0.6,cut = 0.2, maximum = 1, minimum = 0, esize = 20,vsize = 5, repulsion = 0.8,borders = TRUE,nodeNames=varNames)#,gray=T,)#,nodeNames=nomesqsg,groups=qsggr,layout=Lqsg,nodeNames=nomesqsg,vsize=vSize*3,color=c("gold","steelblue","red","grey80")
dev.off()
#legend(0.8,-0.8, bty=".",c("Ensaio Clínico","Medicamentos","Outras Razões"),cex=1.2,fill=c("lightblue","red","yellow"))
centralityPlot(qsgG3)
clusteringPlot(qsgG3)
g<-as.igraph(qsgG3)
h<-walktrap.community(g)
plot(h,g)


predictors<-centrality(qsgG3)$ShortestPaths[,23]

3,5,7,10,12,14,16,18,19,20,21








# Define the amout of factor to retain
#Group of functinos to determine the number os items to be extracted
par(mfrow=c(2,2)) #Command to configure the plot area for the scree plot graph
ev <- eigen(cor_data) # get eigenvalues - insert the data you want to calculate the scree plot for
ev # Show eigend values
ap <- parallel(subject=nrow(cor_data),var=ncol(cor_data),rep=100,cent=.05) #Calculate the acceleration factor
summary(ap)
nS <- nScree(ev$values) #Set up the Scree Plot 
plotnScree(nS) # Plot the ScreePlot Graph
my.vss <- VSS(cor_data,title="VSS of BEA data")
print(my.vss[,1:12],digits =2)
VSS.plot(my.vss, title="VSS of 24 mental tests")
scree(cor_data)
VSS.scree(cor_data)
fa.parallel(cor_data,n.obs=36)

# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 
fit <- principal(cor_data,4,rotate="varimax",scores=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
fit$scores
predict(fit,cor_data)
scores<-scoreItems(fit$weights,pca_data)
describe(scores$scores)
by(scores$scores,data_bea$risk_classification,summary)
wilcox.test(scores$scores[,1]~data_bea$risk_classification)
wilcox.test(scores$scores[,2]~data_bea$risk_classification)
wilcox.test(scores$scores[,3]~data_bea$risk_classification)
#wilcox.test(scores$scores[,4]~data_bea$risk_classification)


