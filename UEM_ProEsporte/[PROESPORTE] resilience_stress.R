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
lapply(c("Hmisc","car","psych","nortest","ggplot2",
	"pastecs","repmis","mvnormtest","polycor","lavaan",
	"nFactors","qgraph"), library, character.only=T)
#####################################################################################
#IMPORTING DATA
#####################################################################################
#LOADING DATA FROM A .CSV FILE
#data<-read.csv("/Users/rpietro/Desktop/MDD_BIPD_Baseline.csv",sep=",")
#information between " " are the path to the directory in your computer where the data is stored

#Import data from Dropbox, in .csv format
#Instructions here http://goo.gl/Ofa7gQ
#data <- repmis::source_DropboxData("resilience_stress_data.csv","1ak5t8oramn8lu9",sep = ",", header = TRUE)

data<-read.csv("/Users/jnv4/OneDrive - Duke University/datasets/pro esporte/resiliencia_stress/resilience_stress_data.csv")
##############################################################
#DATA MANAGEMENT
##############################################################
#Creating a data frame (group of variables)
stress<-with(data, data.frame(estressegeral,estresseemocional,estressesocial,conflitospressao,fadiga,faltaenergia,queixasomaticas,pertubacoesintervalos,exaustaoemocional,lesoes))

rec<-with(data,data.frame(sucesso,recuperacaosocial,recuperacaofisica,bemestar,qualidadesono,estarforma,aceitacaopessoal,autoeficacia,autorregulacao))

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
#data$Classificacao<-car::recode(data$Classificacao,#1='baixo';2='medio';
#	3='alto'")
#data <- base::merge(data1,data2,by=c("nome"))


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

####################################################################
#CRONBACH'S ALPHA
####################################################################
stress1<-with(data,data.frame(Q22,Q24,Q30,Q45))
psych::alpha(stress1)

general_stress<-with(data,data.frame(Q22,Q24,Q30,Q45))
emotional_stress<-with(data,data.frame(Q5,Q8,Q28,Q37))
social_stress<-with(data,data.frame(Q21,Q26,Q39,Q48))
conflicts_pressure<-with(data,data.frame(Q12,Q18,Q32,Q44))
fatigue<-with(data,data.frame(Q2,Q16,Q25,Q35))
energy_lack<-with(data,data.frame(Q4,Q11,Q31,Q40))
somatic_complaints<-with(data,data.frame(Q7,Q15,Q20,Q42))
success<-with(data,data.frame(Q3,Q17,Q41,Q49))
social_recovery<-with(data,data.frame(Q6,Q14,Q23,Q33))
physical_recovery<-with(data,data.frame(Q9,Q13,Q29,Q38))
general_wellbeing<-with(data,data.frame(Q10,Q34,Q43,Q47))
sleep_quality<-with(data,data.frame(Q19,Q27,Q36,Q46))
disturbed_breaks<-with(data,data.frame(Q51,Q58,Q66,Q72))
emotional_exhaustion<-with(data,data.frame(Q54,Q63,Q68,Q76))
injuries<-with(data,data.frame(Q50,Q57,Q64,Q73))
being_inshape<-with(data,data.frame(Q53,Q61,Q69,Q75))
personal_acceptance<-with(data,data.frame(Q55,Q60,Q70,Q77))
self_efficacy<-with(data,data.frame(Q52,Q59,Q65,Q71))
self_regulation<-with(data,data.frame(Q56,Q62,Q67,Q74))

####################################################################
#Descritivos
####################################################################
network_data<-cor(data,method="spearman")
#network_data<-Hmisc::rcorr(as.matrix(data),type=c("spearman"))

qsggr<-list(Outcome=c(1),Predictors1=c(2,3,4,5,6,7,8,9,10),Predictors2=c(11,12,13,14,15,16,17,18,19,20))
labels<-c("RES","REC1","REC2","REC3","REC4","REC5","REC6","REC7","REC8","REC9","STR1","STR2","STR3","STR4","STR5","STR6","STR7","STR8","STR9","STR10")
nodeNames=c("Resilience","Success","Social Recovery","Physical Recovery","Well Being","Sleep Quality","Good Shape","Personal Acceptance","Self-Efficacy","Self-Regulation","General Stress","Emotional Stress","Social Stress","Conflicts and Pressure","Fatigue","Lack of Energy","Somatic Complaints","Interval perturbation","Emotional Exhaustion","Lesions")
nodeSizes=c()


qsgg3<-qgraph(network_data,layout="spring",vsize=6,esize=20,graph="glasso",sampleSize=nrow(data),legend.cex = 0.5,GLratio=1.5)
qsgg2<-qgraph(network_data,layout="spring",vsize=6,esize=20,graph="pcor",threshold="holm",sampleSize=nrow(data),legend.cex = 0.5,GLratio=1.5)
qsgg1<-qgraph(network_data,layout="spring",vsize=6,esize=20,legend.cex = 0.5,GLratio=1.5)
Lqsg<-averageLayout(qsgg1,qsgg2,qsgg3)

qsgG1<-qgraph(network_data,layout=Lqsg,nodeNames=nodeNames,vsize=6,esize=20,legend.cex = 0.3,cut = 0.3, maximum = 1, minimum = 0, esize = 20,vsize = 5, repulsion = 0.8,gray=TRUE,color=c("gray80","gray50"),legend=T)#,groups=qsggr
qsgG2<-qgraph(network_data,layout=Lqsg,nodeNames=nodeNames,vsize=6,esize=20,graph="pcor",legend.cex = 0.3,cut = 0.1, maximum = 1, minimum = 0, esize = 20,vsize = 5, repulsion = 0.8,gray=TRUE,color=c("gray80","gray50"),legend=F)#,groups=qsggr
qsgG3<-qgraph(network_data,layout=Lqsg,nodeNames=nodeNames,vsize=6,esize=20,graph="glasso",sampleSize=nrow(data),legend.cex = 0.3,cut = 0.1, maximum = 1, minimum = 0, esize = 20,vsize = 5, repulsion = 0.8,gray=F,color=c("gray80","white"))#,groups=qsggr

x<-centrality(qsgG3)

####################################################################
#EFA and CFA
####################################################################
#MOtivation######################################################
data_stress_reco<-data.frame(rec,stress)

par(mfrow=c(2,2)) #Command to configure the plot area for the scree plot graph
ev <- eigen(polychoric(data_stress_reco)$rho) # get eigenvalues - insert the data you want to calculate the scree plot for
ev # Show eigend values
ap <- parallel(subject=nrow(data_stress_reco),var=ncol(data_stress_reco),rep=100,cent=.05) #Calculate the acceleration factor
nS <- nScree(ev$values) #Set up the Scree Plot 
plotnScree(nS) # Plot the ScreePlot Graph

#Function to calculate the KMO values - colocar link par ao gist
kmo<-kmo(data_stress_reco) #Run the Kmo function for the data you want to calculate
kmo$overall
kmo$AIR
write.csv(kmo$AIR,"/Users/rpietro/Desktop/AIR_mot.csv")
cortest.bartlett(polychoric(data_stress_reco)$rho, n = 145)

fa.poly(motivation,1,fm="uls",rotate="oblimin")
fa.poly(data_stress_reco,2,fm="uls",rotate="oblimin")
fa.poly(data_stress_reco,3,fm="uls",rotate="oblimin")

#efa_LOD <- efa(motivation, method="cor.polycor")
#efa.plotCorr (efa_LOD)
#efa_LOD <- efa.compute(efa_LOD,factors =3,method="extract.uls", rotate="promax", horn=T)
#efa.plotScree(efa_LOD)
#efa_LOD<-efa.setMinLoad(efa_LOD, minload=0.40, col="black")
#efa.plotFactor(efa_LOD)
#qgraph(efa_LOD)

reco_stress_model <- 'Recovery =~  sucesso + recuperacaosocial + recuperacaofisica + bemestar + qualidadesono + estarforma +  				 aceitacaopessoal + autoeficacia + autorregulacao
			 Stress =~ estressegeral + estresseemocional + estressesocial + fadiga + faltaenergia + queixasomaticas +   conflitospressao + pertubacoesintervalos + exaustaoemocional + lesoes
			 '

fit <- lavaan::cfa(reco_stress_model, data = data_stress_reco,
	estimator="ulsm")
summary(fit, fit.measures=TRUE)
fitMeasures(fit, fit.measures = "all", baseline.model = NULL)
parameterEstimates(fit)
Est <- parameterEstimates(fit, ci = TRUE, standardized = TRUE)
subset(Est, op == "=~")

qgraph(fit)

### Modification Indexes
Mod <- modificationIndices(fit)
subset(Mod, mi > 10)

####################################################################
#SEM
####################################################################
data$genero<-car::recode(data$genero,"1=1;2=0")
data$esporte<-car::recode(data$genero,"1=1;2=0")

#Measurement Model
#model <- '
# measurement model
#MI =~  SMSI_23 + SMSI_27 + SMSI_08 + SMSI_12 + SMSI_15 + SMSI_20 + SMSI_13 + SMSI_18 + SMSI_25
#ME =~ SMSI_09 + SMSI_14 + SMSI_26 + SMSI_06 + SMSI_10 + SMSI_16 +   SMSI_07 + SMSI_11 + SMSI_24
#PS =~  EMP_06 + EMP_12 + EMP_16 + EMP_19 + EMP_30 + EMP_02  + EMP_08 + EMP_31
#PC =~ EMP_01 1*EMP_11 + EMP_15 + EMP_20 + EMP_26 + EMP_03 + EMP_05 + EMP_22 + EMP_09 + EMP_10 + EMP_13 + EMP_14 + EMP_18 + EMP_21 + EMP_23 + EMP_25 + EMP_28
#Cop1 =~ ACSI_08 + ACSI_13 + ACSI_20 + ACSI_14 + ACSI_15 + ACSI_27 + ACSI_04 + ACSI_16'

#fit <- lavaan::sem(model, data = data,estimator="ulsm")
#summary(fit, fit.measures=TRUE)
#fitMeasures(fit, fit.measures = "all", baseline.model = NULL)
#parameterEstimates(fit)
#Est <- parameterEstimates(fit, ci = TRUE, standardized = TRUE)
#subset(Est, op == "=~")
#subset(Est, op == "~")

#qgraph(fit,gray=T)

### Modification Indexes
#Mod <- modificationIndices(fit)
#subset(Mod, mi > 10)

#Modelo 1. Fully mediated
model <- '
# measurement model
Recovery =~  sucesso + recuperacaosocial + recuperacaofisica + bemestar + qualidadesono + estarforma +  				 aceitacaopessoal + autorregulacao
Stress =~ estressegeral + estresseemocional + estressesocial + fadiga + faltaenergia + queixasomaticas +   conflitospressao + pertubacoesintervalos + exaustaoemocional + lesoes
#regressions
resiliencia ~ idade
Recovery ~ resiliencia
Stress ~ resiliencia
#Cop1 ~ v*ME
##indirect effects
#cop_PC := c*z
#cop_PS := b*z
# covariances
#Recovery ~~ Recovery
#Stress ~~ Stress
#resiliencia ~~ resiliencia
#SMSI_12 ~~ SMSI_20
#EMP_18 ~~  EMP_28
#SMSI_06 ~~ SMSI_07
#EMP_02 ~~  EMP_20
'
##regressions
#ME ~ a*PS
#MI ~ b*PS
#ME ~ c*PC
#Cop1 ~ z*MI
#Cop1 ~ v*ME
## covariances
#MI ~~ ME
#SMSI_12 ~~ SMSI_20
#EMP_18 ~~  EMP_28
#SMSI_06 ~~ SMSI_07
#EMP_02 ~~  EMP_20
##indirect effects
#cop_PC := c*v
#cop_PS := a*b*v*z
#model<-lavaanify(model)
fit <- lavaan::sem(model, data = data, estimator="WLSM")
summary(fit, fit.measures=TRUE)
fitMeasures(fit, fit.measures = "all", baseline.model = NULL)
parameterEstimates(fit)
inspect(fit,"rsquare")
Est <- standardizedSolution(fit)
subset(Est, op == "=~")
subset(Est, op == "~")
subset(Est, op == ":=")

vcov(fit)

library(semPlot)
nodeLabels<-c("Sucesso",
             "Rec. SCL",
             "Rec. Fisica",
             "Bem estar",
             "Qualidade de sono",
             "Estar em forma",
             "Aceitacao pessoal",
             "Autorregulacao",
             "Estresse geral",
             "Estresse emocional", 
             "Estresse social",
             "Fadiga",
             "Falta de energia",
             "Queixas somáticas",
             "Conflitos e pressao",
             "Perturbacao",
             "Exaustao emocional",
             "Lesoes",
             "Resiliencia",
             "Recuperacao",
             "Estresse",
             "Idade")
color<-c(rep("grey",18),rep("white",4))
borders<-c(rep("FALSE",18),rep("TRUE",4))
semPaths(fit,"std",layout="spring",residuals=FALSE,
  # edge.color="black",
  # nodeLabels=nodeLabels,
  exoCov=FALSE,
  edge.label.cex=1.0,
  equalizeManifests=TRUE,
  label.scale=FALSE,
  label.cex=1)
  # color=color,
  # borders=borders)


### Modification Indexes
Mod <- modificationIndices(fit)
subset(Mod, mi > 10)

T.orig <- fitMeasures(fit, "chisq")

T.boot <- bootstrapLavaan(fit, R=1000, type="bollen.stine",
FUN=fitMeasures, fit.measures="chisq")

pvalue.boot <- length(which(T.boot > T.orig))/length(T.boot)

quantile(T.boot[,1],.025)
quantile(T.boot[,1],.975)

#MODEL BY GENDER
fit <- lavaan::sem(model, data = data, estimator="WLSM",group = "genero")
summary(fit, fit.measures=TRUE)
fitMeasures(fit, fit.measures = "all", baseline.model = NULL)
parameterEstimates(fit)
inspect(fit,"rsquare")
Est <- standardizedSolution(fit)
subset(Est, op == "=~")
subset(Est, op == "~")
subset(Est, op == ":=")

vcov(fit)

qgraph(fit,gray=FALSE,layout="spring")

### Modification Indexes
Mod <- modificationIndices(fit)
subset(Mod, mi > 10)

T.orig <- fitMeasures(fit, "chisq")

T.boot <- bootstrapLavaan(fit, R=1000, type="bollen.stine",
FUN=fitMeasures, fit.measures="chisq")

pvalue.boot <- length(which(T.boot > T.orig))/length(T.boot)

quantile(T.boot[,1],.025)
quantile(T.boot[,1],.975)

library(semTools)
measurementInvariance(model, data = data, group = "genero")

#MODEL BY SPORT
fit <- lavaan::sem(model, data = data, estimator="WLSM",group = "esporte")
summary(fit, fit.measures=TRUE)
fitMeasures(fit, fit.measures = "all", baseline.model = NULL)
parameterEstimates(fit)
inspect(fit,"rsquare")
Est <- standardizedSolution(fit)
subset(Est, op == "=~")
subset(Est, op == "~")
subset(Est, op == ":=")

vcov(fit)

qgraph(fit,gray=FALSE,layout="spring")

measurementInvariance(model, data = data, group = "esporte")

### Modification Indexes
Mod <- modificationIndices(fit)
subset(Mod, mi > 10)

T.orig <- fitMeasures(fit, "chisq")

T.boot <- bootstrapLavaan(fit, R=1000, type="bollen.stine",
FUN=fitMeasures, fit.measures="chisq")

pvalue.boot <- length(which(T.boot > T.orig))/length(T.boot)

quantile(T.boot[,1],.025)
quantile(T.boot[,1],.975)