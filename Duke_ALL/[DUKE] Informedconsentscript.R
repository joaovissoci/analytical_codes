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
infoconsent <- read.csv("/home/joao/Dropbox/datasets/infoconsent2.csv")

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
#PCA Score comparisons
#############################################################
# # Define the amout of factor to retain
#Group of functinos to determine the number os items to be extracted
#par(mfrow=c(2,2)) #Command to configure the plot area for the scree plot graph
#ev <- eigen(cor_data) # get eigenvalues - insert the data you want to calculate the scree plot for
#ev # Show eigend values
#ap <- parallel(subject=nrow(cor_data),var=ncol(cor_data),rep=100,cent=.05) #Calculate the acceleration factor
#summary(ap)
#nS <- nScree(ev$values) #Set up the Scree Plot 
#plotnScree(nS) # Plot the ScreePlot Graph
#my.vss <- VSS(cor_data,title="VSS of BEA data")
#print(my.vss[,1:12],digits =2)
#VSS.plot(my.vss, title="VSS of 24 mental tests")
#scree(cor_data)
#VSS.scree(cor_data)
#fa.parallel(cor_data,n.obs=36)

# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 

summary(rowMeans(Import))
importance_scores <- principal(Import,1,rotate="varimax",scores=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
importance_scores$scores
#predict(fit,Import)
#importance_scores<-scoreItems(fit$weights,Import,totals=TRUE)

summary(rowMeans(Under))
understanding_scores <- principal(Under,1,rotate="varimax",scores=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
understanding_scores$scores
#predict(fit,Import)
#importance_scores<-scoreItems(fit$weights,Import,totals=TRUE)

reading_scores <- principal(read,1,rotate="varimax",scores=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
reading_scores$scores
#predict(fit,Import)
#importance_scores<-scoreItems(fit$weights,Import,totals=TRUE)

log_model<-data.frame(bancocerto$Q13,importance_scores$scores,understanding_scores$scores)#,reading_scores$scores)
log_model<-na.omit(log_model)

fit <- glm(as.factor(bancocerto$Q13)~importance_scores$scores+understanding_scores$scores,data=log_model,family=binomial)

summary(fit)
exp(coef(fit)) # exponentiated coefficients
exp(confint(fit)) # 95% CI for exponentiated coefficients


##############################################################
#NETWORK APPROACH
#############################################################

################# Importance ###################

importance_network_data<-data.frame(Import,outcome=bancocerto$Q13)#,outcome=bancocerto$Q13)
importance_network_data<-na.omit(importance_network_data)

importance_cor_data<-cor_auto(importance_network_data)
#qsgc<-qsgc$rho

importance_network_groups<-list(Import1=c(1,2,3,6,8,15),Import2=c(4,12),Import3=c(7,13),Other=c(5,9,10,11,14))
importance_node_labels<-c("Why is this study being done?", "What is involved in this study?", "Who is going to be my doctor in this study?","How many people will take part in this study?","How long will I be in this study?","What are the benefits of being in this study?","What about compensation?","What are the risks of being in this study?","What are the costs?","Will my information be kept confidential?","What about research related injuries?","What are the alternatives to being in this study?","What if I want decline participation or withdraw?","Whom do I call if I have questions or trouble?","Willingness to participate")
importance_node_names<-c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","WP")
mean_data<-sapply(importance_network_data,mean)
importance_vSize<-c(mean_data[1:14]/min(mean_data[1:14]),1.81)

importance_network_glasso<-qgraph(importance_cor_data,layout="spring",vsize=6,esize=20,graph="glasso",sampleSize=nrow(importance_network_data),legend.cex = 0.5,GLratio=1.5)
importance_network_pcor<-qgraph(importance_cor_data,layout="spring",vsize=6,esize=20,graph="pcor",threshold="holm",sampleSize=nrow(importance_network_data),legend.cex = 0.5,GLratio=1.5)
importance_network_cor<-qgraph(importance_cor_data,layout="spring",vsize=6,esize=20,legend.cex = 0.5,GLratio=1.5)
#layout1<-averageLayout(network_glasso,network_pcor,network_cor)

#qsgG1<-qgraph(qsgc,layout=Lqsg,vsize=6,esize=20,legend.cex = 0.3,cut = 0.3, maximum = 1, minimum = 0, esize = 20,vsize = 5, repulsion = 0.8,groups=qsggr,gray=TRUE,color=c("gray80","gray50"),legend=F)#nodeNames=nomesqsg,
#qsgG2<-qgraph(qsgc,layout=Lqsg,vsize=6,esize=20,graph="pcor",legend.cex = 0.3,cut = 0.1, maximum = 1, minimum = 0, esize = 20,vsize = 5, repulsion = 0.8,groups=qsggr,gray=TRUE,color=c("gray80","gray50"),legend=F)#,nodeNames=nomesqsg

#centralityPlot(qsgG3)
#clusteringPlot(qsgG3)
g<-as.igraph(importance_network_glasso)
h<-walktrap.community(g)
#h<-spinglass.community(g)
plot(h,g)
h$membership

# Para identificar no qgraph o resultado do algortimo de comunidade, criar objeto de "groups"
# com o resultado de wcG1
predictors<-centrality(importance_network_glasso)$ShortestPaths[,15]
predictors
#centralityPlot(importance_network_glasso)

as.data.frame(predictors[[1]])[2,]
dim(as.data.frame(predictors[[1]]))[1]

importance_network_glasso$Edgelist$from
importance_network_glasso$Edgelist$to
importance_network_glasso$Edgelist$weight

subset(importance_network_glasso$Edgelist$weight,importance_network_glasso$Edgelist$from==1 & importance_network_glasso$Edgelist$to==15)
subset(importance_network_glasso$Edgelist$weight,importance_network_glasso$Edgelist$from==2 & importance_network_glasso$Edgelist$to==15)
subset(importance_network_glasso$Edgelist$weight,importance_network_glasso$Edgelist$from==3 & importance_network_glasso$Edgelist$to==15)
subset(importance_network_glasso$Edgelist$weight,importance_network_glasso$Edgelist$from==10 & importance_network_glasso$Edgelist$to==15)
subset(importance_network_glasso$Edgelist$weight,importance_network_glasso$Edgelist$from==13 & importance_network_glasso$Edgelist$to==15)


log_model<-data.frame(bancocerto$Q13,Import$Q36_1,Import$Q36_2,Import$Q36_3,Import$Q36_10,Import$Q36_13)#,reading_scores$scores)
#log_model<-na.omit(log_model)

fit <- glm(as.factor(bancocerto$Q13)~Import$Q36_1+Import$Q36_2+Import$Q36_3+Import$Q36_10+Import$Q36_13,data=log_model,family=binomial)

summary(fit)
exp(coef(fit)) # exponentiated coefficients
exp(confint(fit)) # 95% CI for exponentiated coefficients

################# Comprehension ###################

comprehension_networlog_model<-data.frame(bancocerto$Q13,Import$Q36_1,Import$Q36_2,Import$Q36_3,Import$Q36_10,Import$Q36_13)#,reading_scores$scores)
#log_model<-na.omit(log_model)

fit <- glm(as.factor(bancocerto$Q13)~Import$Q36_1+Import$Q36_2+Import$Q36_3+Import$Q36_10+Import$Q36_13,data=log_model,family=binomial)

summary(fit)
exp(coef(fit)) # exponentiated coefficients
exp(confint(fit)) # 95% CI for exponentiated coefficientsk_data<-data.frame(Under,outcome=bancocerto$Q13)
comprehension_network_data<-na.omit(comprehension_network_data)

comprehension_cor_data<-cor_auto(comprehension_network_data)
#qsgc<-qsgc$rho

comprehension_node_groups<-list(Under1=c(1,2,3),Under2=c(4,5,6,7,8,9),Under3=c(10,11,12,13,14),Other=c(15))
comprehension_node_labels<-c("Why is this study being done?", "What is involved in this study?", "Who is going to be my doctor in this study?","How many people will take part in this study?","How long will I be in this study?","What are the benefits of being in this study?","What about compensation?","What are the risks of being in this study?","What are the costs?","Will my information be kept confidential?","What about research related injuries?","What are the alternatives to being in this study?","What if I want decline participation or withdraw?","Whom do I call if I have questions or trouble?","Willingness to participate")
comprehension_node_names<-c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","WP")
mean_data<-sapply(comprehension_network_data,mean)
comprehension_vSize<-c(mean_data[1:14]/min(mean_data[1:14]),1.81)

comprehension_network_glasso<-qgraph(comprehension_cor_data,layout="spring",vsize=6,esize=20,graph="glasso",sampleSize=nrow(comprehension_network_data),legend.cex = 0.5,GLratio=1.5)
comprehension_network_pcor<-qgraph(comprehension_cor_data,layout="spring",vsize=6,esize=20,graph="pcor",threshold="holm",sampleSize=nrow(comprehension_network_data),legend.cex = 0.5,GLratio=1.5)
comprehension_network_cor<-qgraph(comprehension_cor_data,layout="spring",vsize=6,esize=20,legend.cex = 0.5,GLratio=1.5)
#layout2<-averageLayout(network_glasso,network_pcor,network_cor)

#qsgG1<-qgraph(cor_data,layout=Lqsg,vsize=6,esize=20,legend.cex = 0.3,cut = 0.3, maximum = 1, minimum = 0, esize = 20,vsize = 5, repulsion = 0.8,groups=qsggr,gray=TRUE,color=c("gray80","gray50"),legend=F)#nodeNames=nomesqsg,
#qsgG2<-qgraph(cor_data,layout=Lqsg,vsize=6,esize=20,graph="pcor",legend.cex = 0.3,cut = 0.1, maximum = 1, minimum = 0, esize = 20,vsize = 5, repulsion = 0.8,groups=qsggr,gray=TRUE,color=c("gray80","gray50"),legend=F)#,nodeNames=nomesqsg

#centralityPlot(qsgG3)
#clusteringPlot(qsgG3)
g<-as.igraph(comprehension_network_glasso)
h<-walktrap.community(g)
plot(h,g)
wdensity(g,h)


# Para identificar no qgraph o resultado do algortimo de comunidade, criar objeto de "groups"
# com o resultado de wcG1
predictors<-centrality(comprehension_network_glasso)$ShortestPaths[,15]
predictors
#centralityPlot(qsgG3)

subset(comprehension_network_glasso$Edgelist$weight,comprehension_network_glasso$Edgelist$from==1 & comprehension_network_glasso$Edgelist$to==15)
subset(comprehension_network_glasso$Edgelist$weight,comprehension_network_glasso$Edgelist$from==3 & comprehension_network_glasso$Edgelist$to==15)
subset(comprehension_network_glasso$Edgelist$weight,comprehension_network_glasso$Edgelist$from==14 & comprehension_network_glasso$Edgelist$to==15)

log_model<-data.frame(bancocerto$Q13,Import$Q36_1,Import$Q36_3,Import$Q36_14)#,reading_scores$scores)
#log_model<-na.omit(log_model)

fit <- glm(as.factor(bancocerto$Q13)~Import$Q36_1+Import$Q36_2+Import$Q36_3+Import$Q36_10+Import$Q36_13,data=log_model,family=binomial)

summary(fit)
exp(coef(fit)) # exponentiated coefficients
exp(confint(fit)) # 95% CI for exponentiated coefficients

############## FINAL FIGURES ############################
# Organizing both figures to be with the same layout
layout_final<-averageLayout(comprehension_network_glasso,comprehension_network_pcor,comprehension_network_cor,importance_network_glasso,importance_network_glasso,importance_network_glasso)


tiff("/home/joao/Desktop/importance_network.tiff", width = 1200, height = 700,compression = 'lzw')
final_importance_network<-qgraph(importance_cor_data,layout=layout_final,vsize=importance_vSize*3,esize=20,graph="glasso",sampleSize=nrow(importance_network_data),legend.cex = 0.6,cut = 0.1, maximum = 1, minimum = 0, esize = 20,vsize = 5, repulsion = 0.8,groups=importance_network_groups,nodeNames=importance_node_labels,color=c("gold","steelblue","red","grey80"),borders = FALSE,labels=importance_node_names)#,gray=T,)#,nodeNames=nomesqsg
dev.off()
#legend(0.8,-0.8, bty=".",c("Ensaio Clínico","Medicamentos","Outras Razões"),cex=1.2,fill=c("lightblue","red","yellow"))

tiff("/home/joao/Desktop/comprehension_network.tiff", width = 1200, height = 700,compression = 'lzw')
final_comprehension_network<-qgraph(comprehension_cor_data,layout=layout_final,vsize=comprehension_vSize*3,esize=20,graph="glasso",sampleSize=nrow(comprehension_network_data),legend.cex = 0.6,cut = 0.1, maximum = 1, minimum = 0, esize = 20,vsize = 5, repulsion = 0.8,groups=comprehension_node_groups,nodeNames=comprehension_node_labels,color=c("gold","steelblue","red","grey80"),borders = FALSE,labels=comprehension_node_names)#,gray=T,)#,nodeNames=nomesqsg
dev.off()
