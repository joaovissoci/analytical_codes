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
#lapply(c("Hmisc","car","psych","nortest","ggplot2","pastecs","repmis","mvnormtest","polycor","lavaan","nFactors","qgraph","semTools"), library, character.only=T)

library(reshape)
library(ggplot2)
library(gdata)
library(repmis)
library(qgraph)

#####################################################################################
#IMPORTING DATA
#####################################################################################
#LOADING DATA FROM A .CSV FILE
#data<-read.csv("/Users/rpietro/Desktop/MDD_BIPD_Baseline.csv",sep=",")
#information between " " are the path to the directory in your computer where the data is stored

#Import data from Dropbox, in .csv format
#Instructions here http://goo.gl/Ofa7gQ
#data <- repmis::source_DropboxData("moral_competence_leo_thesis.csv","fdilgafkauxzpz2",sep = ",",header = TRUE)
data<-read.csv("/home/joao/Dropbox/datasets/luciano papers/suicide_gis.csv",sep=',')

#data <- repmis::source_DropboxData("suicide_gis.csv","i99ndw3so6ur7m6",sep = ",",header = TRUE)

#############################################################################
#DATA MANAGEMENT
#############################################################################

str(data)

data_1<-remove.vars(data,c("CD_GEOCODM"))

data_2000<-with(data,data.frame(school2000,Inform2000,Agric2000,Income00,Unemploy00,Chil_lab00,IDHM00,SMR151908,SMR202408,SMR252908))

data_2010<-with(data,data.frame(school2010,Inform2010,Agric2010,Income10,Unemploy10,Chil_Lab10,IDHM10,SMR151908,SMR202408,SMR252908))

#creating data set to melt
#age_data<-with(data,data.frame(LAG151998,LAG151908,LAG202498,LAG202408,LAG252998,LAG252908,CD_GEOCODM))
#melting data to interpolate columns and rows
#test_data<-remove.vars(data,c("NM_MUNICIP"))
#age_data<-melt(test_data,id=c("CD_GEOCODM"))
#age_data$value<-as.numeric(as.character(age_data$value))

#creating factor variable to cathegorize time series (2000 and 2012)
#age_data$time_serie<-rep(c(rep("2000",length(data$LAG151998)),rep("2000",length(data$LAG151998)),rep("2000",length(data$LAG151998)),rep("2012",length(data$LAG151908)),rep("2012",length(data$LAG151908)),rep("2012",length(data$LAG151908))),6)

#creating factor variable to cathegorize group ages
#age_data$age_group<-rep(c(rep("15-19",length(data$LAG151998)),rep("20-24",length(data$LAG151998)),rep("25-29",length(data$LAG151998)),rep("15-19",length(data$LAG151908)),rep("20-24",length(data$LAG151908)),rep("25-29",length(data$LAG151908))),6)

#age_data$age_group<-as.factor(age_data$age_group)
#age_data$time_serie<-as.factor(age_data$time_serie)
#age_data$socio_variable<-as.factor(c(rep(c("LAG"),399*6),rep(c("LINF"),399*6),rep(c("LSCH"),399*6),rep(c("LINC"),399*6),rep(c("LUNE"),399*6),rep(c("LCHI"),399*6)))

#############################################################################
#CLUSTERING ANALYSIS
##############################################################
 
wssplot <- function(data, nc=15, seed=1234){
               wss <- (nrow(data)-1)*sum(apply(data,2,var))
               for (i in 2:nc){
                    set.seed(seed)
                    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
                plot(1:nc, wss, type="b", xlab="Number of Clusters",
                     ylab="Within groups sum of squares")}

#1 standardize data
cluster_2000<-scale(data_2000[,1:7])

#2 determine number of clusters
wssplot(cluster_2000) 
library(NbClust)
set.seed(1234)
nc <- NbClust(cluster_2000, min.nc=2, max.nc=15, method="kmeans")
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]),
          xlab="Numer of Clusters", ylab="Number of Criteria",
          main="Number of Clusters Chosen by 26 Criteria")

#3 K-means cluster analysis
set.seed(1234)
fit.km <- kmeans(cluster_2000, 3, nstart=25)
fit.km$size
fit.km$centers
aggregate(data_2000[,1:7], by=list(cluster=fit.km$cluster), mean)
#ct.km <- table(wine$Type, fit.km$cluster)
#library(flexclust)
#randIndex(ct.km)
data_2000$groups<-fit.km$cluster
data_2000$groups<-car::recode(data_2000$groups, "1='Pattern1';2='Pattern2';3='Pattern3'")


d <- dist(scale(data_2000[,1:7]), method = "euclidean") # distance matrix
                                               #
fit <- hclust(d, method="ward") 
plot(fit) # display dendogram
data_2000$groups <- cutree(fit, k=4) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=4, border="red")
data_2000$groups<-car::recode(data_2000$groups, "1='Pattern1';2='Pattern2';3='Pattern3'")


data_graph<-remove.vars(data_2000,c("SMR151908","SMR202408","SMR252908"))

descriptive_graph<-melt(data_graph,id=c("groups"))
timemeans <- cast(descriptive_graph, groups~variable, mean)
timesd <- cast(descriptive_graph, groups~variable, sd)


d <- dist(scale(data_2010[,1:7]), method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit) # display dendogram
data_2010$groups <- cutree(fit, k=3) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=3, border="red")
data_2010$groups<-car::recode(data_2010$groups, "1='Pattern2';2='Pattern1';3='Pattern3'")

data_graph<-remove.vars(data_2010,c("SMR151908","SMR202408","SMR252908"))

descriptive_graph<-melt(data_graph,id=c("groups"))
timemeans <- cast(descriptive_graph, groups~variable, mean)
timesd <- cast(descriptive_graph, groups~variable, sd)


#descriptive_graph_2<-melt(timemeans,id=c("groups"))

#############################################################################
#ENVIRONMENT RISK
#############################################################################
logmodel2000_1<-glm(Group1519_9802 ~ as.factor(data_2000$groups),family=binomial, data=data)
summary(logmodel2000_1)
logistic.display(logmodel2000_1)
#anova(reglogGEU)
#exp(coef(model1_death)) # exponentiated coefficients
#exp(confint(model1_death)) # 95% CI for exponentiated coefficients
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logmodel2000_2<-glm(Group2024_9802 ~ as.factor(data_2000$groups),family=binomial, data=data)
summary(logmodel2000_2)
logistic.display(logmodel2000_2)
#anova(reglogGEU)
#exp(coef(model1_death)) # exponentiated coefficients
#exp(confint(model1_death)) # 95% CI for exponentiated coefficients
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logmodel2000_3<-glm(Group2529_9802 ~ as.factor(data_2000$groups),family=binomial, data=data)
summary(logmodel2000_3)
logistic.display(logmodel2000_3)
#anova(reglogGEU)
#exp(coef(model1_death)) # exponentiated coefficients
#exp(confint(model1_death)) # 95% CI for exponentiated coefficients
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals

logmodel2010_1<-glm(Group1519_0812 ~ as.factor(data_2010$groups),family=binomial, data=data)
summary(logmodel2010_1)
logistic.display(logmodel2010_1)
#anova(reglogGEU)
#exp(coef(model1_death)) # exponentiated coefficients
#exp(confint(model1_death)) # 95% CI for exponentiated coefficients
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logmodel2010_2<-glm(Group2024_0812 ~ as.factor(data_2010$groups),family=binomial, data=data)
summary(logmodel2010_2)
logistic.display(logmodel2010_2)
#anova(reglogGEU)
#exp(coef(model1_death)) # exponentiated coefficients
#exp(confint(model1_death)) # 95% CI for exponentiated coefficients
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logmodel2010_3<-glm(Group2529__0812 ~ as.factor(data_2010$groups),family
	=binomial, data=data)
summary(logmodel2010_3)
logistic.display(logmodel2010_3)
#anova(reglogGEU)
#exp(coef(model1_death)) # exponentiated coefficients
#exp(confint(model1_death)) # 95% CI for exponentiated coefficients
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
#########################################################
#PLOTTING
#########################################################

### PLOTING MODEL
#plot_odds<-function(x, title = NULL){
tmp_1<-data.frame(rbind(exp(coef(logmodel2000_1)),exp(coef(logmodel2000_2)),exp(coef(logmodel2000_3))),rbind(exp(confint(logmodel2000_1)),exp(confint(logmodel2000_2)),exp(confint(logmodel2000_3))))
#odds<-tmp[-1,]
names(tmp_1)<-c('OR', 'lower', 'upper')
tmp_1$vars<-c('15-19','20-24','25-29')
tmp_1$facet<-c('2000 Time Series')
#ticks<-c(seq(.1, 1, by =.1), seq(0, 10, by =1), seq(10, 100, by =10))

#plot_odds<-function(x, title = NULL){
tmp_2<-data.frame(rbind(exp(coef(logmodel2010_1)),exp(coef(logmodel2010_2)),exp(coef(logmodel2010_3))),rbind(exp(confint(logmodel2010_1)),exp(confint(logmodel2010_2)),exp(confint(logmodel2010_3))))
#odds<-tmp[-1,]
names(tmp_2)<-c('OR', 'lower', 'upper')
tmp_2$vars<-c('15-19','20-24','25-29')
tmp_2$facet<-c('2010 Time Series')
#ticks<-c(seq(.1, 1, by =.1), seq(0, 10, by =1), seq(10, 100, by =10))




#plot_odds<-function(x, title = NULL){
tmp_3<-data.frame(rbind(exp(coef(x_1C)),exp(coef(x_2C)),exp(coef(x_3C))),rbind(exp(confint(x_1C)),exp(confint(x_2C)),exp(confint(x_3C))))
#odds<-tmp[-1,]
names(tmp_3)<-c('OR', 'lower', 'upper')
tmp_3$vars<-c('24 hours','1 week','Overall')
tmp_3$facet<-c('Violence')
#ticks<-c(seq(.1, 1, by =.1), seq(0, 10, by =1), seq(10, 100, by =10))

odds<-rbind(tmp_1,tmp_2)

ggplot(odds, aes(y= OR, x = reorder(vars, OR))) + geom_point() + geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
#scale_y_log10(breaks=ticks, labels = ticks) + 
geom_hline(yintercept = 1, linetype=2) +
coord_flip() + labs(title = "", x = 'Variables', y = 'OR') + theme_bw() + facet_wrap(~ facet,ncol=1) 































#############################################################################
#NETWORK APPROACH
#############################################################################
str(data_2000)

network_data<-cor(data_2000)
qsggr<-list(Outcome=c(8,9,10),Predictors=c(1,2,3,4,5,6,7))
nodeNames=c("Education","Informality","Agriculture","Income","Unemployment","Child Labor","IDH","Suicidality 15-19","Suicadality 20-24","Suicidality 25-29")


qsgg3<-qgraph(network_data,layout="spring",vsize=6,esize=20,graph="glasso",sampleSize=nrow(data_2000),legend.cex = 0.5,GLratio=1.5)
qsgg2<-qgraph(network_data,layout="spring",vsize=6,esize=20,graph="pcor",threshold="holm",sampleSize=nrow(data_2000),legend.cex = 0.5,GLratio=1.5)
qsgg1<-qgraph(network_data,layout="spring",vsize=6,esize=20,legend.cex = 0.5,GLratio=1.5)
Lqsg<-averageLayout(qsgg1,qsgg2,qsgg3)

qsgG1<-qgraph(network_data,layout=Lqsg,nodeNames=nodeNames,vsize=6,esize=20,legend.cex = 0.3,cut = 0.3, maximum = 1, minimum = 0, esize = 20,vsize = 5, repulsion = 0.8,groups=qsggr,gray=TRUE,color=c("gray80","gray50"),legend=T)
qsgG2<-qgraph(network_data,layout=Lqsg,nodeNames=nodeNames,vsize=6,esize=20,graph="pcor",legend.cex = 0.3,cut = 0.1, maximum = 1, minimum = 0, esize = 20,vsize = 5, repulsion = 0.8,groups=qsggr,gray=TRUE,color=c("gray80","gray50"),legend=F)
qsgG3<-qgraph(network_data,layout=Lqsg,nodeNames=nodeNames,vsize=6,esize=20,graph="glasso",sampleSize=nrow(data_2000),legend.cex = 0.3,cut = 0.1, maximum = 1, minimum = 0, esize = 20,vsize = 5, repulsion = 0.8,groups=qsggr,gray=F,color=c("gray80","white"))

x<-centrality(qsgG3)

data_line_plot<-data.frame(x$ShortestPathLengths[1:7,8:10])

names_line_plot<-c("Education","Informality","Agriculture","Income","Unemployment","Child Labor","IDH")#,"Suicidality 15-19","Suicadality 20-24","Suicidality 25-29")

#Some organiztion needed for the codes
value<-with(data_line_plot,c(X1,X2,X3))
Profile<-c(rep(c("Suicidality 15-19"),7),rep(c("Suicadality 20-24"),7),rep(c("Suicadality 25-29"),7))
Themes<-rep(names_line_plot,3)
data_plot<-data.frame(value,Profile,Themes)

#using GGPLOT TO PLOT THE LINES (Still fixing variable names)
ggplot(data=data_plot, aes(y=value, x=Themes, group=Profile,color=Profile)) + geom_line(size=1.5) + geom_point(size=3,fill="white") + ylab("") + xlab("") + theme_bw()+ scale_colour_manual(values=c("#999999","darkred","black"))+theme(axis.text.x = element_text(angle= 270, hjust = 0, colour = "black",size=14))

centralityPlot(qsgG3)
clusteringPlot(qsgG3)
g<-as.igraph(qsgG3)
walktrap.community()

data_2000$suicide<-rowMeans(data_2000[8:10])
directed_network<-with(data_2000,data.frame(school2000,Inform2000,Agric2000,Income00,Unemploy00,Chil_lab00,IDHM00,suicide))

library("pcalg")
#data(gmI)
suffStat <- list(C = cor(directed_network), n = nrow(directed_network))
pc.fit <- pc(suffStat, indepTest=gaussCItest,
p = ncol(directed_network), alpha = 0.01)
qgraph(pc.fit)
#############################################################################
#MULTIPLE REGRESSION OR PREDICTIVE MODEL APPROACH
#############################################################################
library(bnlearn)
library(Rgraphviz)

data_2000$suicide<-rowMeans(data_2000[8:10])
directed_network<-with(data_2000,data.frame(school2000,Inform2000,Agric2000,Income00,Unemploy00,Chil_lab00,IDHM00,suicide))

## Manual graph construction
varnames=c("Education","Informality","Agriculture","Income","Unemployment","Child Labor","IDH","Suicide")
ag=empty.graph(varnames)

## Automated graph contruction
#data(asia) 
names(directed_network) = varnames

###########################
# Learning RB Structure
###########################
# Growth Shrink  Algorithmm <<<<<<<<<<<<<<<<
rb_gs = gs(directed_network)
graphviz.plot(rb_gs)

# IAMB  Algorithm  <<<<<<<<<<<<<<<<
rb_ia = iamb(directed_network)
graphviz.plot(rb_ia)

# Hill Climbing Algorithm  <<<<<<<<<<<<<<<<
rb_hc = hc(directed_network)
graphviz.plot(rb_hc)

# Learning CPT for each node in the graph
fitted = bn.fit(rb_hc, directed_network)
print(fitted$SoB)
print(fitted)
fitted

gs(directed_network, debug = TRUE)
score(rb_hc, data = directed_network)
nparams(rb_hc)

# plot the network learned by gs().
#res = set.arc(rb_hc)
strength = arc.strength(rb_hc, directed_network)
strength.plot(rb_hc, strength)
# add another (non-significant) arc and plot the network again.
res = set.arc(res, "A", "C")
strength = arc.strength(res, learning.test, criterion = "x2")
strength.plot(rb_hc)
averaged.network(strength, nodes, threshold)



#############################################################################
#ANCOVA
#############################################################################
#merging both time series points education variables
#age_data$education<-with(data,rep(c(M_SCH00,M_SCH10),3))

#ANOVA 1
#fitting anova model
anova1<-subset(age_data,age_data$socio_variable=="LAG")
fit <- aov(value ~ time_serie*age_group, data=anova1)
summary(fit)

#fiding predictions patterns
pred1 <- predict(fit)

#ANOVA 2
#fitting anova model
anova2<-subset(age_data,age_data$socio_variable=="LINF")
fit <- aov(value ~ time_serie*age_group, data=anova2)
summary(fit)

#fiding predictions patterns
pred2 <- predict(fit)

#ANOVA 3
#fitting anova model
anova3<-subset(age_data,age_data$socio_variable=="LCHI")
fit <- aov(value ~ time_serie*age_group, data=anova3)
summary(fit)

#fiding predictions patterns
pred3 <- predict(fit)

#ANOVA 4
#fitting anova model
anova4<-subset(age_data,age_data$socio_variable=="LINC")
fit <- aov(value ~ time_serie*age_group, data=anova4)
summary(fit)

#fiding predictions patterns
pred4 <- predict(fit)

#ANOVA 5
#fitting anova model
anova5<-subset(age_data,age_data$socio_variable=="LSCH")
fit <- aov(value ~ time_serie*age_group, data=anova5)
summary(fit)

#fiding predictions patterns
pred5 <- predict(fit)

#ANOVA 6
#fitting anova model
anova6<-subset(age_data,age_data$socio_variable=="LUNE")
fit <- aov(value ~ time_serie*age_group, data=anova6)
summary(fit)

#fiding predictions patterns
pred6 <- predict(fit)

pred<-c(pred1,pred2,pred3,pred4,pred5,pred6)

#plotting prediction values
ggplot(data = cbind(age_data, pred),
    aes(age_group, value, color=time_serie, group=time_serie)) + geom_line(aes(y=pred)) +     facet_grid(. ~ socio_variable)