##############################################################
#template_secondary_data_analysis.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. see full license at the end of this file.
##############################################################
#this script follows a combination of the guidelines proposed by Hadley Wickham http://goo.gl/c04kq as well as using the formatR package http://goo.gl/ri6ky
#if this is the first time you are conducting an analysis using this protocol, please watch http://goo.gl/DajIN while following step by step

#link to manuscript

##############################################################
#SETTING ENVIRONMENT
##############################################################

#command below will install individual and is only run once. remove the hash tag if this is the first time you are running the code on RStudio, and then you can add the hash tag again
#install.packages("car", repos="http://cran.r-project.org")
#install.packages("ggplot2", repos="http://cran.r-project.org")

#command below will install each package. if you run this script from the beginning you need to run every single one again
lapply(c("epicalc", "sem","Hmisc","ggplot2", "psych", "irr", "nortest", "moments","GPArotation","nFactors","repmis","gdata","qgraph"), library, character.only=T)

##############################################################
#IMPORTING DATA AND RECODING
##############################################################

#if you are using a file that is local to your computer, then replace path below by path to the data file. command will throw all the data into the templateData object. replace the word template.data by a name that might easier for you to remember and that represents your data
infoconsent <- repmis::source_DropboxData("infoconsent2.csv","xucrht4l08z1kiw",sep = ",",header = TRUE)

#Data for Demographics Graph
#options(RCurlOptions = list(capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE))
#data <- getURL("https://docs.google.com/spreadsheet/pub?key=0AjVEotAQPzQsdGtFRXR3QXY5NXFDTEFxLVk3eFZhb2c&single=true&gid=0&output=csv")
#feasability<-read.csv(textConnection(data))
bancocerto <-subset(infoconsent, V10==1)
#below will list variable names, classes (integer, factor, etc), alternative responses
str(bancocerto)
#list variable names so that they can be used later
names(bancocerto)
#below will attach your data so that when you execute a command you don't have to write the name of your data object over and over again
#attach(bancocerto)
View(bancocerto)

#Creating a dataset with only Importance questions
Import<-with(bancocerto,data.frame(Q36_1,Q36_2,Q36_3,Q36_4,Q36_5,Q36_6,Q36_7,Q36_8,Q36_9,Q36_10,Q36_11,Q36_12,Q36_13,Q36_14))
import_score<-rowMeans(Import)

#Creating a dataset with only Understanding questions
Under<-with(bancocerto,data.frame(Q46_1,Q46_2,Q46_3,Q46_4,Q46_5,Q46_6,Q46_7,Q46_8,Q46_9,Q46_10,Q46_11,Q46_12,Q46_13,Q46_14))
under_score<-rowMeans(Under)

#Creating a dataset with only read/noread data
read<-with(bancocerto,data.frame(Q8,Q9,Q10,Q14,Q16,Q18,Q20,Q22,Q24,Q26,Q28,Q30,Q32,Q34))

sem_data<-data.frame(Import,Under,outcome=bancocerto$Q13)

#Recoding Gender variable
Gender<-as.factor(bancocerto$Q37)

#Reconding Response (Willignness TO participate) variable
Respo<-as.factor(bancocerto$Q13)

#Recoding Race variable
Race<-as.factor(bancocerto$Q39)

##################################################################
#DESCRITIVOS E COMPARAÇÕES
##################################################################
#Categorical data example
summary(Gender) #Choose one variable

#General descriptives
Hmisc::describe(bancocerto)

#General descriptives with a different code
psych::describe(bancocerto)

#Specific descriptives by Gender
x<-psych::describeBy(Q1,Gender)

#Descriptives
Hmisc::describe(Gender)
Hmisc::describe(Race)
Hmisc::describe(Q38)
Hmisc::describe(Import,subset=Respo,na.action=na.omit)
Hmisc::describe(Under)

#Obtain CI for the Means of the descriptive analysis
ci(bancocerto) # CI for all the data set
ci(Import) # CI for the import data frame - Import=Questions regarding level of importane
ci(Under) # CI for the Under data frame - Under=Questions regarding level of Understanding
describeBy(Import,Q13) # Descriptives divided by groups of respondants and non-respondants
by(Import,bancocerto$Q13,summary)
x<-by(Under,bancocerto$Q13,describe)

#Calculating descriptives for Importance questions by Willingness to participate (Q13)
x<-by(Import,bancocerto$Q13,psych::describe)
import_yes<-as.data.frame(x[1])$X1.median #create vector with median values for yes
import_no<-as.data.frame(x[2])$X2.median #create vector with median values for no
y<-by(Under,bancocerto$Q13,psych::describe)
under_yes<-as.data.frame(y[1])$X1.median
under_no<-as.data.frame(y[2])$X2.median
scores<-c(import_yes,import_no,under_yes,under_no)
will<-as.factor(c(rep(c('yes'),14),rep(c('no'),14),rep(c('yes'),14),rep(c('no'),14)))
will2<-c(rep(c('Import_yes'),14),rep(c('Import_no'),14),
	rep(c('Under_yes'),14),rep(c('Under_no'),14))
questions<-rep(c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9",
			"Q10","Q11","Q12","Q13","Q14"),4)
groups<-as.factor(c(rep("Importance",28),rep("Comprehension",28)))
plot<-data.frame(scores,will,questions,groups,will2)

under_1quantiles_yes<-sapply(subset(Under,bancocerto$Q13==1),summary)[2,]
under_1quantiles_no<-sapply(subset(Under,bancocerto$Q13==2),summary)[2,]
import_1quantiles_yes<-sapply(subset(Under,bancocerto$Q13==1),summary)[2,]
import_1quantiles_no<-sapply(subset(Under,bancocerto$Q13==2),summary)[2,]
plot$quantiles_1<-c(import_1quantiles_yes,import_1quantiles_no,under_1quantiles_yes,under_1quantiles_no)

under_3quantiles_yes<-sapply(subset(Under,bancocerto$Q13==1),summary)[5,]
under_3quantiles_no<-sapply(subset(Under,bancocerto$Q13==2),summary)[5,]
import_3quantiles_yes<-sapply(subset(Under,bancocerto$Q13==1),summary)[5,]
import_3quantiles_no<-sapply(subset(Under,bancocerto$Q13==2),summary)[5,]
plot$quantiles_3<-c(import_3quantiles_yes,import_3quantiles_no,under_3quantiles_yes,under_3quantiles_no)

#setEPS()
#postscript("general_emadults.eps")
ggplot(data=plot, aes(x=questions, y=scores,group=will, color=will)) + geom_line(size=1.5) + facet_grid(groups ~.) + geom_point(size=3,fill="white") + theme_bw()+ ylab("Median") + xlab("Questions") + scale_colour_manual(values=c("#999999","darkred"), name="Willing to Participate", breaks=c("yes","no"),labels=c("Yes", "No"))+ theme(legend.position=c(0.1,0.1)) + geom_segment(aes(x = questions, y = quantiles_1, xend = questions, yend = quantiles_3)) + scale_x_discrete(limits=c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14"))

#dev.off()

##############################################################
#NETWORK APPROACH
#############################################################
################# Importance ###################

sem_data<-data.frame(Import,outcome=bancocerto$Q13)#,outcome=bancocerto$Q13)
sem_data<-na.omit(sem_data)

qsgc<-cor_auto(sem_data)
#qsgc<-qsgc$rho

qsggr<-list(Import1=c(1,2,3,6,8,15),Import2=c(4,12),Import3=c(7,13),Other=c(5,9,10,11,14))
nomesqsg<-c("Why is this study being done?", "What is involved in this study?", "Who is going to be my doctor in this study?","How many people will take part in this study?","How long will I be in this study?","What are the benefits of being in this study?","What about compensation?","What are the risks of being in this study?","What are the costs?","Will my information be kept confidential?","What about research related injuries?","What are the alternatives to being in this study?","What if I want decline participation or withdraw?","Whom do I call if I have questions or trouble?","Willingness to participate")
varNames<-c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","WP")
mean_data<-sapply(sem_data,mean)
vSize<-c(mean_data[1:14]/min(mean_data[1:14]),1.81)

qsgg3<-qgraph(qsgc,layout="spring",vsize=6,esize=20,graph="glasso",sampleSize=nrow(sem_data),legend.cex = 0.5,GLratio=1.5)
qsgg2<-qgraph(qsgc,layout="spring",vsize=6,esize=20,graph="pcor",threshold="holm",sampleSize=nrow(sem_data),legend.cex = 0.5,GLratio=1.5)
qsgg1<-qgraph(qsgc,layout="spring",vsize=6,esize=20,legend.cex = 0.5,GLratio=1.5)
Lqsg<-averageLayout(qsgg1,qsgg2,qsgg3)

qsgG1<-qgraph(qsgc,layout=Lqsg,vsize=6,esize=20,legend.cex = 0.3,cut = 0.3, maximum = 1, minimum = 0, esize = 20,vsize = 5, repulsion = 0.8,groups=qsggr,gray=TRUE,color=c("gray80","gray50"),legend=F)#nodeNames=nomesqsg,
qsgG2<-qgraph(qsgc,layout=Lqsg,vsize=6,esize=20,graph="pcor",legend.cex = 0.3,cut = 0.1, maximum = 1, minimum = 0, esize = 20,vsize = 5, repulsion = 0.8,groups=qsggr,gray=TRUE,color=c("gray80","gray50"),legend=F)#,nodeNames=nomesqsg

tiff("/home/joao/Desktop/importance_network.tiff", width = 1200, height = 700,compression = 'lzw')
qsgG3<-qgraph(qsgc,layout=Lqsg,vsize=vSize*3,esize=20,graph="glasso",sampleSize=nrow(sem_data),legend.cex = 0.6,cut = 0.1, maximum = 1, minimum = 0, esize = 20,vsize = 5, repulsion = 0.8,groups=qsggr,nodeNames=nomesqsg,color=c("gold","steelblue","red","grey80"),borders = FALSE,labels=varNames)#,gray=T,)#,nodeNames=nomesqsg
dev.off()
#legend(0.8,-0.8, bty=".",c("Ensaio Clínico","Medicamentos","Outras Razões"),cex=1.2,fill=c("lightblue","red","yellow"))
centralityPlot(qsgG3)
clusteringPlot(qsgG3)
g<-as.igraph(qsgG3)
h<-walktrap.community(g)
plot(h,g)

# Para identificar no qgraph o resultado do algortimo de comunidade, criar objeto de "groups"
# com o resultado de wcG1
predictors<-centrality(qsgG3)$ShortestPaths[,15]
centralityPlot(qsgG3)

as.data.frame(predictors[[1]])[2,]
dim(as.data.frame(predictors[[1]]))[1]

qsgG3$Edgelist$from
qsgG3$Edgelist$to
qsgG3$Edgelist$weight

subset(qsgG3$Edgelist$weight,qsgG3$Edgelist$from==1 & qsgG3$Edgelist$to==15)
subset(qsgG3$Edgelist$weight,qsgG3$Edgelist$from==2 & qsgG3$Edgelist$to==15)
subset(qsgG3$Edgelist$weight,qsgG3$Edgelist$from==3 & qsgG3$Edgelist$to==15)
subset(qsgG3$Edgelist$weight,qsgG3$Edgelist$from==10 & qsgG3$Edgelist$to==15)
subset(qsgG3$Edgelist$weight,qsgG3$Edgelist$from==13 & qsgG3$Edgelist$to==15)

################# Comprehension ###################

sem_data<-data.frame(Under,outcome=bancocerto$Q13)
sem_data<-na.omit(sem_data)

qsgc<-cor_auto(sem_data)
#qsgc<-qsgc$rho

qsggr<-list(Under1=c(1,2,3),Under2=c(4,5,6,7,8,9),Under3=c(10,11,12,13,14),Other=c(15))
nomesqsg<-c("Why is this study being done?", "What is involved in this study?", "Who is going to be my doctor in this study?","How many people will take part in this study?","How long will I be in this study?","What are the benefits of being in this study?","What about compensation?","What are the risks of being in this study?","What are the costs?","Will my information be kept confidential?","What about research related injuries?","What are the alternatives to being in this study?","What if I want decline participation or withdraw?","Whom do I call if I have questions or trouble?","Willingness to participate")
varNames<-c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","WP")
mean_data<-sapply(sem_data,mean)
vSize<-c(mean_data[1:14]/min(mean_data[1:14]),1.81)

qsgg3<-qgraph(qsgc,layout="spring",vsize=6,esize=20,graph="glasso",sampleSize=nrow(sem_data),legend.cex = 0.5,GLratio=1.5)
qsgg2<-qgraph(qsgc,layout="spring",vsize=6,esize=20,graph="pcor",threshold="holm",sampleSize=nrow(sem_data),legend.cex = 0.5,GLratio=1.5)
qsgg1<-qgraph(qsgc,layout="spring",vsize=6,esize=20,legend.cex = 0.5,GLratio=1.5)
Lqsg<-averageLayout(qsgg1,qsgg2,qsgg3)

qsgG1<-qgraph(qsgc,layout=Lqsg,vsize=6,esize=20,legend.cex = 0.3,cut = 0.3, maximum = 1, minimum = 0, esize = 20,vsize = 5, repulsion = 0.8,groups=qsggr,gray=TRUE,color=c("gray80","gray50"),legend=F)#nodeNames=nomesqsg,
qsgG2<-qgraph(qsgc,layout=Lqsg,vsize=6,esize=20,graph="pcor",legend.cex = 0.3,cut = 0.1, maximum = 1, minimum = 0, esize = 20,vsize = 5, repulsion = 0.8,groups=qsggr,gray=TRUE,color=c("gray80","gray50"),legend=F)#,nodeNames=nomesqsg

tiff("/home/joao/Desktop/comprehension_network.tiff", width = 1200, height = 700,compression = 'lzw')
Lqsg<-averageLayout(qsgg1,qsgg2,qsgg3)
qsgG3<-qgraph(qsgc,layout=Lqsg,vsize=vSize*3,esize=20,graph="glasso",sampleSize=nrow(sem_data),legend.cex = 0.6,cut = 0.1, maximum = 1, minimum = 0, esize = 20,vsize = 5, repulsion = 0.8,groups=qsggr,nodeNames=nomesqsg,color=c("gold","steelblue","red","grey80"),borders = FALSE,labels=varNames)#,gray=T,)#,nodeNames=nomesqsg
dev.off()
centralityPlot(qsgG3)
clusteringPlot(qsgG3)
g<-as.igraph(qsgG3)
h<-walktrap.community(g)
plot(h,g)

# Para identificar no qgraph o resultado do algortimo de comunidade, criar objeto de "groups"
# com o resultado de wcG1
predictors<-centrality(qsgG3)$ShortestPaths[,15]
centralityPlot(qsgG3)

subset(qsgG3$Edgelist$weight,qsgG3$Edgelist$from==1 & qsgG3$Edgelist$to==15)
subset(qsgG3$Edgelist$weight,qsgG3$Edgelist$from==3 & qsgG3$Edgelist$to==15)
subset(qsgG3$Edgelist$weight,qsgG3$Edgelist$from==14 & qsgG3$Edgelist$to==15)
##############################################################
#SEM Model
##############################################################
sem_data<-data.frame(Import,outcome=bancocerto$Q13)
sem_data<-na.omit(sem_data)


fit <- glm(as.factor(outcome)~Q36_1+Q36_2+Q36_3+Q36_10+Q36_13,data=sem_data,family=binomial)

summary(fit)
exp(coef(fit)) # exponentiated coefficients
exp(confint(fit)) # 95% CI for exponentiated coefficients



under_data<-data.frame(Under,outcome=bancocerto$Q13)
under_data<-na.omit(under_data)


fit <- glm(as.factor(outcome)~Q46_1+Q46_3+Q46_14,data=under_data,family=binomial)

summary(fit)
exp(coef(fit)) # exponentiated coefficients
exp(confint(fit)) # 95% CI for exponentiated coefficients
























################### FACTOR ANALYSIS ##########################
par(mfrow=c(2,2))
ev <- eigen(cor(na.omit(Import))) # get eigenvalues
ev
ap <- parallel(subject=nrow(Import),var=ncol(Import),rep=100,cent=.05)
nS <- nScree(ev$values)
plotnScree(nS)

#KMO
kmo(na.omit(Import))

#library(mvnmle)
#mlest(Import)
#library(mvnormtest)
#mshapiro.test(t(Import))

#center <- colMeans(t(Import)) # centroid
#n <- nrow(t(Import)); p <- ncol(t(Import)); cov <- cov(t(Import)); 
#d <- mahalanobis(t(Import),center,cov) # distances 
#qqplot(qchisq(ppoints(n),df=p),d,main="QQ Plot Assessing Multivariate Normality",ylab="Mahalanobis D2")
#abline(a=0,b=1)

#FACTOR EXTRACTION

#fa.poly(PCA,3,fm="pca",rotate="promax")
Import<-remove.vars(Import,c("Q36_1","Q36_4"))
fa(Import,4,n.obs = 216,rotate="Promax")
fa(Import,3,n.obs = 216,rotate="Promax")
x<-fa(Import,1,n.obs = 216,rotate="Promax")
fa(Import,4,n.obs = 216,rotate="Varimax",fm="pa")
fa(Import,3,n.obs = 216,rotate="Varimax",fm="pa")
fa(Import,2,n.obs = 216,rotate="Varimax")
fa(Import,4,n.obs = 216,rotate="Oblimin",fm="pa")
fa(Import,3,n.obs = 216,rotate="Oblimin",fm="pa")
fa(Import,2,n.obs = 216,rotate="Oblimin")

################### MEASUREMENT MODEL ###########################

Import<-na.omit(Import)

#CFA Model
cfa <- specifyModel()
F1->Q36_1,var1,NA
F1->Q36_2,var3,NA
F1->Q36_3,var4,NA
F2->Q36_4,var5,NA
F2->Q36_5,var6,NA
F2->Q36_6,var7,NA
F2->Q36_7,var8,NA
F2->Q36_8,var9,NA
F3->Q36_9,var10,NA
F3->Q36_10,var11,NA
F3->Q36_11,var12,NA
F3->Q36_12,var13,NA
F3->Q36_13,var14,NA
F3->Q36_14,var15,NA        
F1<->F1,NA,1
F2<->F2,NA,1
F3<->F3,NA,1
F1<->F2,latcor1,NA
F1<->F3,latcor2,NA
F2<->F3,latcor3,NA
#Endofmodel

# Insert de covariance matrix - CFA (or SEM) is always calculated in relation to a covariance or correlation matrix, here we will create the covariance matrix
#cov1frm <- cov(RMorrisFactorModel, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))
cov<-cov(Import)

# Estimate the model
cfamodel<- sem(cfa, cov, N=216)
summary(cfamodel,fit.indices=c("GFI", "AGFI", "RMSEA", "NFI", "NNFI", "CFI","RNI", "IFI", "SRMR", "AIC", "AICc","TLI"))
modIndices(cfamodel)

#Under<-data.frame(Q46_1,Q46_2,Q46_3,Q46_4,Q46_5,Q46_6,Q46_7,Q46_8,Q46_9,Q46_10,Q46_11,Q46_12,Q46_13,Q46_14)
Under<-na.omit(Under)
par(mfrow=c(2,2))
ev <- eigen(cor(Under)) # get eigenvalues
ev
ap <- parallel(subject=nrow(Under),var=ncol(Under),rep=100,cent=.05)
nS <- nScree(ev$values)
plotnScree(nS)

#KMO
kmo(Under)

library(mvnmle)
mlest(Under)
library(mvnormtest)
mshapiro.test(t(Under))

center <- colMeans(t(Under)) # centroid
n <- nrow(t(Under)); p <- ncol(t(Under)); cov <- cov(t(Under)); 
d <- mahalanobis(t(Under),center,cov) # distances 
qqplot(qchisq(ppoints(n),df=p),d,
			 main="QQ Plot Assessing Multivariate Normality",
			 ylab="Mahalanobis D2")
abline(a=0,b=1)

#Import<-remove.vars(Import,c("Q36_1","Q36_4"))
fa(Under,4,n.obs = 216,rotate="Promax")
fa(Under,3,n.obs = 216,rotate="Promax",fm="pa")
fa(Under,2,n.obs = 216,rotate="Promax")
fa(Import,4,n.obs = 216,rotate="Varimax",fm="pa")
fa(Import,3,n.obs = 216,rotate="Varimax",fm="pa")
fa(Import,2,n.obs = 216,rotate="Varimax")
fa(Import,4,n.obs = 216,rotate="Oblimin",fm="pa")
fa(Import,3,n.obs = 216,rotate="Oblimin",fm="pa")
fa(Import,2,n.obs = 216,rotate="Oblimin")

#CFA Model
cfa <- specifyModel()
F1->Q46_1,var1,NA
F1->Q46_2,var3,NA
F1->Q46_3,var4,NA
F2->Q46_4,var11,NA
F2->Q46_5,var12,NA
F2->Q46_6,var13,NA
F2->Q46_7,var14,NA
F2->Q46_8,var15,NA
F3->Q46_9,var21,NA
F3->Q46_10,var22,NA
F3->Q46_11,var23,NA
F3->Q46_12,var24,NA
F3->Q46_13,var25,NA
F3->Q46_14,var26,NA        
F1<->F1,NA,1
F2<->F2,NA,1
F3<->F3,NA,1
F1<->F2,latcor1,NA
F1<->F3,latcor2,NA
F2<->F3,latcor3,NA
Q46_13<->Q46_12,erro1,NA
Q46_4<->Q46_3,erro2,NA
Q46_2<->Q46_1,erro3,NA
#Endofmodel

# Insert de covariance matrix - CFA (or SEM) is always calculated in relation to a covariance or correlation matrix, here we will create the covariance matrix
#cov1frm <- cov(RMorrisFactorModel, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))
Under<-na.omit(Under)
cov<-cov(Under)

# Estimate the model
cfamodel<- sem(cfa, cov, N=216)
summary(cfamodel,fit.indices=c("GFI", "AGFI", "RMSEA", "NFI", "NNFI", "CFI", "RNI", "IFI", "SRMR", "AIC", "AICc","TLI"))
modIndices(cfamodel)

################### SEM read->Import+Under->Will ###################

sem_model <- specifyModel()

#Model 1
Import1->Q46_1,var1,NA
Import1->Q46_2,var3,NA
Import1->Q46_3,var4,NA
Import2->Q46_4,var11,NA
Import2->Q46_5,var12,NA
Import2->Q46_6,var13,NA
Import2->Q46_7,var14,NA
Import2->Q46_8,var15,NA
Import3->Q46_9,var21,NA
Import3->Q46_10,var22,NA
Import3->Q46_11,var23,NA
Import3->Q46_12,var24,NA
Import3->Q46_13,var25,NA
Import3->Q46_14,var26,NA        
Import1<->Import1,NA,1
Import2<->Import2,NA,1
Import3<->Import3,NA,1
#Import1<->Import2,latcor1,NA
#Import1<->Import3,latcor2,NA
#Import2<->Import3,latcor3,NA

#Model 2
Under1->Q36_1,var1,NA
Under1->Q36_2,var3,NA
Under1->Q36_3,var4,NA
Under2->Q36_4,var5,NA
Under2->Q36_5,var6,NA
Under2->Q36_6,var7,NA
Under2->Q36_7,var8,NA
Under2->Q36_8,var9,NA
Under3->Q36_9,var10,NA
Under3->Q36_10,var11,NA
Under3->Q36_11,var12,NA
Under3->Q36_12,var13,NA
Under3->Q36_13,var14,NA
Under3->Q36_14,var15,NA        
Under1<->Under1,NA,1
Under2<->Under2,NA,1
Under3<->Under3,NA,1
#Under1<->Under2,latcor1,NA
#Under1<->Under3,latcor2,NA
#Under2<->Under3,latcor3,NA

#Prediction Paths
Import1->outcome,import_pred1,NA
Import2->outcome,import_pred2,NA
Import3->outcome,import_pred3,NA
Under1->outcome,under_pred1,NA
Under2->outcome,under_pred2,NA
Under3->outcome,under_pred3,NA

#Errors
outcome<->outcome,erro1,NA
#Q46_4<->Q46_3,erro2,NA
#Q46_2<->Q46_1,erro3,NA
#Endofmodel

sem_data<-na.omit(sem_data)
cov<-cov(sem_data)

cfamodel<- sem(sem_model, cov, N=216)
summary(cfamodel,fit.indices=c("GFI", "AGFI", "RMSEA", "NFI", "NNFI", "CFI", "RNI", "IFI", "SRMR", "AIC", "AICc","TLI"))
modIndices(cfamodel)
qgraph(cfamodel)