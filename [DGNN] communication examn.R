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
lapply(c("Hmisc","car","psych","nortest","ggplot2","pastecs","repmis","mvnormtest","polycor","QCA","VennDiagram"),library, character.only=T)
#####################################################################################
#IMPORTING DATA
#####################################################################################
#LOADING DATA FROM A .CSV FILE
#data<-read.csv("/Users/rpietro/Desktop/MDD_BIPD_Baseline.csv",sep=",")
#information between " " are the path to the directory in your computer where the data is stored

#Import data from Dropbox, in .csv format
#Instructions here http://goo.gl/Ofa7gQ

data<-read.csv("/home/joao/Dropbox/datasets/DGNN/communication/data_communication.csv")

data_comm<-with(data,data.frame(avg_score,country,com_result=com_result,outcome=overall_result,primary_len=X.1))
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

#data <- base::merge(data1,data2,by=c("nome"))

#############################################################################
#BASIC DESCRIPTIVES and EXPLORATORY ANALYSIS
#############################################################################

table<-with(data_comm,table(country))
prop.table(table)

table<-with(data_comm,table(country,outcome))
prop.table(table,1)

table<-with(data_comm,table(outcome,primary_len))
prop.table(table,2)
chisq.test(table)

table<-with(data_comm,table(outcome,primary_len))
prop.table(table,2)
chisq.test(table)


with(data_comm,by(avg_score,outcome,describe))
t.test(data_comm$avg_score~data_comm$outcome)

with(data_comm,by(avg_score,primary_len,describe))
t.test(data_comm$avg_score~data_comm$primary_len)

with(data_comm,describe(avg_score))
#####################################################################################
#LOGISTIC REGRESSION
##############################################################
peer<-with(data_comm,table(com_result,outcome)) # A will be rows, B will be columns Classificacao
spineplot(peer, main="Communication Score Vs. Final Examn Score", xlab="Communication Score", ylab="Final Examn Score")
assocstats(peer)

peer<-with(data_comm,table(primary_len,outcome)) # A will be rows, B will be columns Classificacao
spineplot(peer, main="Primary Language Vs. Final Examn Score", xlab="Primary  Language", ylab="Final Examn Score")
assocstats(peer)

new_score<-car::recode(data_comm$avg_score,"4.5=5;5.5=6;6.5=7;7.5=8")
peer<-with(data_comm,table(as.factor(avg_score),primary_len)) # A will be rows, B will be columns Classificacao
spineplot(peer, main="Primary Language Vs. Final Examn Score", xlab="Primary  Language", ylab="Final Examn Score")
assocstats(peer)

########
data_comm$avg_score2<-car::recode(data_comm$avg_score,"0:5='No';else='yes'")
data_comm$primary_len2<-car::recode(data_comm$primary_len,"'English'=1;else=0")
baselineXFUP3<-glm(outcome ~ avg_score2+primary_len2,family=binomial, data=data_comm)
summary(baselineXFUP3)
logistic.display(baselineXFUP3)

baselineXFUP3<-glm(outcome~as.factor(avg_score2),family=binomial, data=data_comm)
summary(baselineXFUP3)
logistic.display(baselineXFUP3)
########
with(data_comm,t.test(avg_score~primary_len))

##############################################################
#BY COUNTRY ANALYSIS
##############################################################
#eixo X - grade (categorica)
#eixo Y - proporcao de Ingles

with(data_comm,table(outcome,country))
with(data_comm,table(com_result,country))
with(data_comm,by(avg_score,country,summary))

##############################################################
#QCA
##############################################################
qca_data<-with(data_comm,data.frame(avg_score,primary_len,outcome))

#qca_data$country<-as.numeric(qca_data$country)
qca_data$primary_len<-as.numeric(qca_data$primary_len)
qca_data$outcome<-as.numeric(qca_data$outcome)

qca_data$avg_score<-qca_data$avg_score/10
qca_data$primary_len<-car::recode(qca_data$primary_len,"2=0;1=1")
qca_data$outcome<-car::recode(qca_data$outcome,"2=1;1=0")

qca_data$avg_score<-calibrate(qca_data$avg_score, thresholds = findTh(qca_data$avg_score, groups = 2, hclustm="complete", distm="euclidean"))


# Evaluates necessity based on a set o conditions. Returns 3 values
nec_test<-superSubset(qca_data, outcome = "outcome", incl.cut = 0.7, cov.cut = 0.20)
nec_test

#qca_data$country<-car::recode(qca_data$country,"2=1;1=0")

TT <- truthTable(qca_data, outcome = "outcome", incl.cut1 = 0.7,show.cases = TRUE, sort.by = c("incl", "n"), complete=TRUE) 
# neg.out=TRUE -- use outcome negative value

# solution complex
dataSC <- eqmcc(TT, details = TRUE, show.cases = TRUE)
dataSC

# parcimonious solution
dataSP <- eqmcc(TT, include = "?", rowdom = FALSE, details = TRUE)
dataSP

# solution intermediate
dataSI <- eqmcc(TT, incl.cut1 = 0.9,  include = "?", direxp = rep(1, 2), details = TRUE)
dataSI

vennPI <- venn.diagram(x = list("outcome" = which(qca_data$outcome == 1),"Comm Score" = which (dataSC$pims>= 0.5)),filename = NULL,cex= 2.5, cat.cex=2, fill = c("#9d9969", "#8B4513"))#, "#8B4513", "#9d9969", "#E69F00", "#0072B2"))  0.12, 0.12, 0.30, 0.15


vennPI <- venn.diagram(x = list("outcome" = which(qca_data$outcome == 1),"Comm Score" = which (qca_data$avg_score >= 0.5),"Primary Eng" = which (qca_data$primary_len == 1)),filename = NULL,cex= 2.5, cat.cex=2, fill = c("#E69F00", "#8B4513", "#0072B2"))#, "#8B4513", "#9d9969", "#E69F00", "#0072B2"))  0.12, 0.12, 0.30, 0.15
#fill = gray (c(0.3, 0.5, 0.7, 0.9))0,30, 300, 180 cat.pos= c(350,190,cat.dist= c(0.22, 0.40)
grid.newpage()
grid.draw(vennPI)

vennPI <- venn.diagram(x = list("outcome" = which(qca_data$outcome == 0),"Comm Score" = which (qca_data$avg_score >= 0.5),"Primary Eng" = which (qca_data$primary_len == 1)),filename = NULL,cex= 2.5, cat.cex=2, fill = c("#E69F00", "#8B4513", "#0072B2"))





#data <- read.csv("/Users/joaovissoci/Desktop/data_qca_luc.csv", sep=";")
#View(data)
#data
#data<-na.omit(data)
#head(data)

dataNR <- superSubset(data_comm[-1,], outcome = "outcome", incl.cut = 0.9, cov.cut = 0.52)
dataNR
COms <- dataNR$coms[ ,1:4]

vennKrookNec <- venn.diagram(
  x=list(
    "DEATH" = which(data$DEATH == 1),
    "  GCS+ox_apl+crx+SURGERY" = which(COms[, 4] == 1),
    " GCS+ox_apl+LAB+SURGERY" = which(COms[, 3] == 1),
    "GCS+crx+lab" = which(COms[, 2] == 1),
    "GCS+surgery " = which(COms[, 1] == 1)),
  
  filename = NULL,
  cex= 2.5, cat.cex=2, cat.pos= c(340,185,-10,10,30),
  cat.dist= c(0.22, 0.40, 0.12, 0.12,0.24), 
  
fill = c("#BC8F8F", "#D2B48C", "#8B4513", "#9d9969", "#70b6c8ff"))                       
#fill = gray (c(0.3, 0.5, 0.7, 0.9, 0.11, 0.13)))
grid.draw(vennKrookNec)
grid.newpage()

# trthTable
TT<-truthTable(data_comm[-1,], outcome = "outcome", show.cases = TRUE, sort.by = c("incl", "n"))
TT

#TT <- truthTable(data, outcome = c("res"), neg.out = FALSE, conditions = c("MOT", "GCS", "OXI", "CXR","DISP", "SURG"),
                # n.cut = 1, incl.cut1 = 1, incl.cut0 = 1, complete = FALSE,
                # show.cases = FALSE, sort.by = c(""), decreasing = TRUE,
                 #use.letters = FALSE)

#TT

# solution complex
dataSC <- eqmcc(TT, details = TRUE, show.cases = TRUE)
dataSC

# parcimonious solution
dataSP <- eqmcc(TT, include = "?", rowdom = FALSE, details = TRUE)
dataSP

# solution intermediate
dataSI <- eqmcc(TT, incl.cut1 = 0.9,  include = "?", direxp = rep(1, 6), details = TRUE)
dataSI
 
#factorize(dataSI)

#dataSI$pims$i.sol$C1P1

PIms <- dataSI$i.sol$C1P1$pims
PIms[ , 1:5]






############################
#data1<-PIms
#data1$outcome<-data$OUTCOME
#install.packages("qgraph")
library(qgraph)

network_data <- t(as.matrix(qca_data)) %*% as.matrix(qca_data)
qca <- c(4,1,4,4,1,5)
#names <- rownames(network_data)
names <-c("GCS*CT_BRAIN*SURGERY","GCS*LAB*CT_BRAIN","GCS*ox_apl*CT_BRAIN","ox_apl*CXR*CT_BRAIN","OX_APL*cxr*LAB*CT_BRAIN","Global")
color<-c("#009900", "#D2B48C", "#8B4513", "#9d9969", "#E69F00", "#0072B2")
diag(network_data) <- 0

tiff("/Users/joaovissoci/Desktop/qca_luciano.tiff", width = 1500, height = 1200,compression = 'lzw')
network <- qgraph(network_data,layout = "spring",labels=names,label.scale=FALSE,label.cex = 2, vsize=qca*2.5,edge.labels=TRUE,edge.label.bg=TRUE,label.prop=0.9,gray=TRUE,edge.label.cex=2,esize=4, color=c("#CCCC99","#CCCCCC","#99CC99","#CCCC66","#FFFFCC","#CCFFFF"),borders=FALSE)
dev.off()

##############################################################
#NETWORK
##############################################################



network <- qgraph(network_data,layout = "spring", cut=3,labels=names,label.scale=FALSE,label.cex = 1.5, vsize=qca)