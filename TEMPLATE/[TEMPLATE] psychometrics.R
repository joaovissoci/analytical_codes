######################################################
#suicide_anxiety.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. see full license at the end of this file.
######################################################
#this script follows a combination of the guidelines proposed by Hadley Wickham http://goo.gl/c04kq as well as using the formatR package http://goo.gl/ri6ky
#if this is the first time you are conducting an analysis using this protocol, please watch http://goo.gl/DajIN while following step by step

#link to manuscript

 #The general plan is to compare the fibrinogen and platelet curves of RS vs Copperhead snakes.  The times points are Baseline, nadir during hospitalization, day 5, day 8, day 15.  There is some missing mess.   I am hoping we can get it done in time for an abstract deadline soon.  Let me know what is best.

######################################################
#SETTING ENVIRONMENT
######################################################
 #install.packages("VIM")
 #install.packages("VIMGUI")
 #install.packages("miP")
 #install.packages("gWidgetsRGtk2")
 #install.packages("mi")
 #install.packages("epicalc")

#Load packages neededz for the analysis
#All packages must be installes with install.packages() function
lapply(c("sem","ggplot2", "psych", "RCurl", "irr", "nortest", 
	"moments","GPArotation","nFactors","boot","psy", "car",
	"vcd", "gridExtra","mi","VIM","epicalc","gdata","sqldf",
	"reshape2","mclust","foreign","survival","memisc","lme4",
	"lmerTest","dplyr","QCA","VennDiagram","qgraph","igraph",
	"ltm","gmodels","eRm","mirt","dplyr","devtools","reshape"),
library, character.only=T)

#Package and codes to pull data from goodle sheets
#devtools::install_github("jennybc/googlesheets")
#library(googlesheets)
#suppressMessages(library(dplyr))

######################################################
#IMPORTING DATA
######################################################

# add the path to you computer between " "
data_tz<-read.csv("",sep=',')

######################################################
#DATA MANAGEMENT
######################################################
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

data <- base::merge(data1,data2,by=c("nome"))

##############################################################
#Descriptives
#############################################################
######################################################################
#BASIC DESCRIPTIVES and EXPLORATORY ANALYSIS
######################################################################
###Section wih several exploratory data analysis functions
###### Exploratory Data Anlysis
###### UNIVARIATE

#dim(data)
#str (data)
#head(data)
#names(data)
#stat.desc(data)
with(data,by(data,outcome,ad.test)) # Anderson-Darling test for normality
#skewness(data$Idade) #Will provide skweness analysis
#kurtosis(data$Idade) - 3 #Will provide kurtosis analysis
#qplot(data$Idade) # histogram plot

# Numerical descriptives
#summary(data)#This comand will provide a whole set of descriptive #results for each variables
describe(data)
with(data,by(data,outcome,describe))
with(data,by(data,outcome,summary))

# Categorical Descriptives
table<-with(bea_data,table(road_area))
table
prop.table(table)

##### BIVARIATE

#Graphing and homogeneity
boxplot(data$Idade~data$Classificacao) #will provide a boxplot for the #variables to analysis potential outliers
## Bartlett Test of Homogeneity of Variances
#bartlett.test(data$Idade~data$Classificacao, data=data)
## Figner-Killeen Test of Homogeneity of Variances
#fligner.test(data$Idade~data$Classificacao, data=data)
#leveneTest(data$Idade~data$Classificacao, data=data)

# Categorical Descriptives 2x2, 2x3 ...
table<-with(bea_data,table(road_area,country)) #create cross-tabs with 2 caegorical variables
table #display cross-tabs
prop.table(table,2) #find proportions. Argument #2 means  proportion by columns. Change to 1 for rows.
chisq.test(table) #chi-square test
fisher.test(table) #fisher's exact correction for cases with less then 5 observations
assocstats(table) #vcd package, gives all range of associations

# Numerical descriptives
# function by allow to appy a function to a vector conditional on a factor vector
by(bea_data$density_car,bea_data$country,summary)
#wilcox comparisons, change for t.test for parametric data
wilcox.test(bea_data$density_car~bea_data$country)

##############################################################
#TAXONOMETRIC ANALAYSIS
##############################################################

#Taxonometric Scale
MAMBAC(scale(NeckDisabilityIndexNA)[,1:3], Comp.Data = T)

##############################################################
#RELIABILITY
##############################################################
### INTERNAL CONSISTENCY
#RELIABILITY
#psych::alpha(cor_data,n.iter=1000,check.keys=TRUE)
psych::alpha(model2_bea,n.iter=1000,check.keys=TRUE)

#### INTER-RATER Agreement
data_sl_temp_model1<-with(data_sl,data.frame(
	road_area,
	road_design,
	intersections,
	lane_type,
	auxiliary_lane,
	rd_condition___0,
	roadside,
	curves_type___0,
	visibility___0,
	bridges,
	rater,
	id))

data_sl_agree_model1<-melt(data_sl_temp_model1,id=c("rater","id"))

#data_sl_agree2<-cast(data_sl_agree_model1,id~rater+variable)

data_sl_agree_model1_e<-subset(data_sl_agree_model1,
	data_sl_agree_model1=='e')
data_sl_agree_model1_s<-subset(data_sl_agree_model1,
	data_sl_agree_model1=='s')
data_sl_agree_model1_t<-subset(data_sl_agree_model1,
	data_sl_agree_model1=='t')

agree_data_sl_model1<-data.frame(
	data_sl_agree_model1_e$value,
	data_sl_agree_model1_t$value,
	data_sl_agree_model1_s$value)
#agree_data_sl<-na.omit(agree_data_sl)
#agree_data_sl$data_cluster_police_sl.RISK<-car::recode(agree_data_sl$data_cluster_police_sl.RISK,"1=0;2=1;3=1")
#agree_data_sl$data_cluster_survey_sl.RISK<-car::recode(agree_data_sl$data_cluster_survey_sl.RISK,"1=0;2=1;3=1")
#Executing agreement nalysis
agree<-agree(na.omit(agree_data_sl_model1), tolerance=0) #% of Agreement
kappa<-cohen.kappa(na.omit(agree_data_sl_model1)) #Kappa-value
#AC1(kappa$agree)
#cor<-cor(agree_data_sl,method=c("kendall"))
#kendall<-Kendall(agree_data_sl$data_cluster_police_sl.RISK,agree_data_sl$data_cluster_survey_sl.RISK)
#poly<-hetcor(agree_data_sl)

##############################################################
#NETWORK 
##############################################################
# # Define the amout of factor to retain
#Group of functinos to determine the number os items to be extracted
cor_data<-cor_auto(bea_data[,-c(1,7,9)])

# #Community analysis
# comprehension_network_glasso<-qgraph(cor_data,
# 	layout="spring",
# 	vsize=6,esize=20,graph="glasso",
# 	sampleSize=nrow(bea_data),
# 	legend.cex = 0.5,GLratio=1.5,minimum=0.1)
# #Calculating Community measures
# g<-as.igraph(comprehension_network_glasso) #creating igraph object
# h<-walktrap.community(g) #creatin community object
# h<-spinglass.community(g, weights=NA)
# plot(h,g) #plotting community network
# h$membership #extracting community membership for each node on the network
# community<-data.frame(h$membership,rownames(cor_data))

#listing grouping variables in the network resulting from the community analysis
# network_groups<-list(
# Component1=as.numeric(rownames(community)[community[,1]==1]),
# Component2=as.numeric(rownames(community)[community[,1]==2]),
# Component3=as.numeric(rownames(community)[community[,1]==3])
# )

network_groups<-list(
Component1=c(1,3,4,5,15,14),
Component2=c(2,16,6,7),
Component3=c(11,12,13,10),
Component4=c(19,20,21,23),
Component5=c(9,17,18,22,8)
)

# creating vectors for labels
node_labels<-c(
"What is the area of the roadway?",
"What type of roadway?",
"Is this point at an intersection/junction?",
"How many lanes in the roadway?",
"Is there an auxiliary/other lane?",
"How is the road surface conditions?",
"Is there space on the side of the road 
for any reason or use?",
"Are there pedestrian pathways?",
"Is there a Bus Stop?",
"Is there a Speed bump?",
"Is there a traffic light at this location?",
"Are there road traffic signs at this hotspot?",
"Is there a sign for speed limit of road?",
"Road visibility is influenced by curves?",
"Is the visibility influenced by 
environmental factors?",
"Are there bridges on the road?",
"Is there a safe area for pedestrians 
to cross the road?",
"Is there a safe area for pedestrians
to in the center of the road?",
"Count the number of cars",
"Count the number of moto",
"Count the number of bike",
"Count the number of pedestrians",
"Count the number of bus/trucks"
)

# creating nodes labels vector
node_names<-c("RD","RT","INT","TLA","AR",
	"RC","RS",
	"WALK","BS","SB","TLI","TS","SL","CUR",
	"VIS","BRI","PED","PEDc","CARd","MOTOd","BIKEd","PEDd","TRUCKd")

# creating vector with mean values for each node
#mean_data<-sapply(network_data,mean)

#creating vector with mean values adjusted to proportional sizes to be plotted
#importance_vSize<-c(mean_data[1:14]/min(mean_data[1:14]),1.81)

#building network figures 
# 3 types are created to get an avarege position and layout
#GLASSO NETWORK
# network_glasso<-qgraph(cor_data,layout="spring",
# 	vsize=6,esize=20,graph="glasso",
# 	sampleSize=nrow(bea_data),
# 	legend.cex = 0.5,GLratio=1.5)

# #PARTIAL CORRELATION NETWORK
# network_pcor<-qgraph(cor_data,layout="spring",
# 	vsize=6,esize=20,graph="pcor",threshold="holm",
# 	sampleSize=nrow(bea_data),
# 	legend.cex = 0.5,GLratio=1.5)

# #CORRELATION NETWORK
# network_cor<-qgraph(cor_data,layout="spring",
# 	vsize=6,esize=20,legend.cex = 0.5,GLratio=1.5)
# #layout1<-averageLayout(network_glasso,network_pcor,network_cor)

# # Organizing both figures to be with the same layout
# layout_final<-averageLayout(network_glasso,
# 	network_pcor,
# 	network_cor)

#postscript("/home/joao/Desktop/info_consent_figure2.eps",
#	width = 1500, height = 1200,horizontal = FALSE, 
#	onefile = FALSE)
#postscript("/Users/joaovissoci/Desktop/info_consent_figure2.eps",
#	width = 1500, height = 1200,horizontal = FALSE, 
#	onefile = FALSE)
tiff("/Users/jnv4/Desktop/bea_pca_network.tiff", width = 1200,
 height = 700,compression = 'lzw')
final_importance_network<-qgraph(cor_data,
	esize=20,graph="glasso",
	sampleSize=nrow(bea_data),
	legend.cex = 0.6,cut = 0.3, maximum = 1, 
	minimum = 0.1, esize = 20,vsize = 5, 
	repulsion = 0.8,groups=network_groups,
	nodeNames=node_labels,
	color=c("gold","steelblue","red","grey80","green"),borders = FALSE,
	labels=node_names)#,gray=T,)#,nodeNames=nomesqsg,layoutScale=c(2,2)
dev.off()
#legend(0.8,-0.8, bty=".",c("Ensaio Clínico","Medicamentos","Outras Razões"),cex=1.2,fill=c("lightblue","red","yellow"))

#############################################################
#ANALISE PARALELA E EIGEN VALUES
#############################################################

#MODEL 1 - Risk due to road deisgn
model1_bea<-with(bea_data,data.frame(
			road_area,
			road_design,
			intersections,
			lane_type,
			auxiliary_lane,
			bridges,
			road_visibility,
			curves_type,
			road_condition,
			roadside))

cor_data<-cor_auto(model1_bea)


#Function to calculate the KMO values - colocar link par ao gist
kmo<-kmo(NeckDisabilityIndex) #Run the Kmo function for the data you want to calculate
kmo$overall
kmo$AIR #anti-image matrix

# par(mfrow=c(2,2)) #Command to configure the plot area for the scree plot graph
# ev <- eigen(cor_data) # get eigenvalues - insert the data you want to calculate the scree plot for
# ev # Show eigend values
# ap <- parallel(subject=nrow(cor_data),var=ncol(cor_data),rep=100,cent=.05) #Calculate the acceleration factor
# summary(ap)
# nS <- nScree(ev$values) #Set up the Scree Plot 
# plotnScree(nS) # Plot the ScreePlot Graph
my.vss <- VSS(cor_data,title="VSS of BEA data")
#print(my.vss[,1:12],digits =2)
VSS.plot(my.vss, title="VSS of 24 mental tests")
scree(cor_data)
VSS.scree(cor_data)
fa.parallel(cor_data,n.obs=229)


##############################################################
#PCA
#############################################################
# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 
fit <- psych::principal(cor_data,nfactors=2,rotate="none",scores=TRUE)
fit
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
#fit$scores
pca1<-predict(fit,model1_bea)
# scores<-scoreItems(fit$weights,bea_data[,-1],totals=TRUE)
# summary(scores)
# describe(scores$scores)
# by(scores$scores,data_bea$risk_classification,summary)
# wilcox.test(scores$scores[,1]~data_bea$risk_classification)
# wilcox.test(scores$scores[,2]~data_bea$risk_classification)
# wilcox.test(scores$scores[,3]~data_bea$risk_classification)
# #wilcox.test(scores$scores[,4]~data_bea$risk_classification)

 model <- principal(cor_data ,nfactors=2, rotate='none', scores=T, cov=T)
 L <- model$loadings            # Just get the loadings matrix
 S <- model$scores              # This gives an incorrect answer in the current version

 d <- model1_bea              # get your data
 dc <- scale(d,scale=FALSE)     # center the data but do not standardize it
 pca1 <- dc %*% L                 # scores are the centered data times the loadings
 # lowerCor(sc)                   #These scores, being principal components
#                                # should be orthogonal 

#Extracting normalized data
bea_data$ID<-c(1:227)
pca_scores_data<-data.frame(PCA1a=pca1[,1],PCA1b=pca1[,2],
							PCA2=pca2[,1],PCA3=pca3[,1],
							PCA4=pca4[,1],id=bea_data$ID,
							country=bea_data[,1])
pca_scores_data<-na.omit(pca_scores_data)

rescale <- function(x)(x-min(x))/(max(x) - min(x)) * 100
pca_score_data_rescaled<-lapply(pca_scores_data[,1:5],rescale)
pca_score_data_rescaled<-data.frame(pca_score_data_rescaled,
	pca_scores_data$id,pca_scores_data$country)

# extract a .csv of the components
# write.csv(pca_score_data_rescaled,"/Users/joaovissoci/Desktop/bea_PCAscores.csv")

##############################################################
#EXPLORATORY FACTOR ANALYSIS
#############################################################
#Functino to exctract the factor loadings. 
#Arguments are DATA, Number of factors, rotation method. 
#Look here http://goo.gl/kY3ln for different met

#holds of estimations or rotations
fa(NeckDisabilityIndex,2,fm="pa",rotate="promax")
fa(NeckDisabilityIndex,1,fm="pa",rotate="oblimin")

#based on a polychoric correlation matrix
fa.poly(data_stress_reco,3,fm="uls",rotate="oblimin")

#efa_LOD <- efa(motivation, method="cor.polycor")
#efa.plotCorr (efa_LOD)
#efa_LOD <- efa.compute(efa_LOD,factors =3,method="extract.uls", rotate="promax", horn=T)
#efa.plotScree(efa_LOD)
#efa_LOD<-efa.setMinLoad(efa_LOD, minload=0.40, col="black")
#efa.plotFactor(efa_LOD)
#qgraph(efa_LOD)

##############################################################
#CONFIRMATORY FACTOR ANALYSIS
#############################################################
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

#Composite Reliabilty
# sum(x[1:24,2])^2/(sum(x[1:24,2])^2+sum(x[28:51,2]))

##############################################################
#ITEM RESPONSE THEORY
##############################################################

#### USING eRM Package
IRTRolandMorris <- PCM(NeckDisabilityIndex)
diff_index<-thresholds(IRTRolandMorris)
summary(diff_index$threshtable[[1]][,1])
sd(diff_index$threshtable[[1]][,1])/sqrt(length(diff_index$threshtable[[1]][,1]))
plotICC(IRTRolandMorris,item.subset=3,ask=F,empICC=list("raw"),empCI=list(lty="solid"))
plotPImap(IRTRolandMorris, sorted=FALSE)
plotPWmap(IRTRolandMorris)
pp<-person.parameter(IRTRolandMorris)
#lrt<-LRtest(IRTRolandMorris,se=TRUE)
#Waldtest(IRTRolandMorris)
eRm::itemfit(pp)
summary(eRm::itemfit(pp)$i.outfitMSQ)
sd(eRm::itemfit(pp)$i.outfitMSQ)
summary(eRm::itemfit(pp)$i.infitMSQ)
sd(eRm::itemfit(pp)$i.infitMSQ)
#NPtest(IRTRolandMorris,method="T11")
#plotGOF(lrt,conf=list())
#fscores(NeckDisabilityIndex, rotate = "oblimin", Target = NULL, full.scores = FALSE,method = "EAP", quadpts = NULL, response.pattern = NULL,plausible.draws = 0, returnER = FALSE, return.acov = FALSE,mean = NULL, cov = NULL, verbose = TRUE, full.scores.SE = FALSE,theta_lim = c(-6, 6), MI = 0, QMC = FALSE, custom_den = NULL, custom_theta = NULL, min_expected = 1)


IRTRolandMorris <- RM(neckdisability2)
plotICC(IRTRolandMorris,item.subset=1:4,ask=F,empICC=list("raw"),empCI=list(lty="solid"))
plotPImap(IRTRolandMorris)
pp<-person.parameter(IRTRolandMorris)
lrt<-LRtest(IRTRolandMorris,se=TRUE)
Waldtest(IRTRolandMorris)
eRm::itemfit(pp)
summary(itemfit(pp)$i.outfitMSQ)
sd(itemfit(pp)$i.outfitMSQ)
summary(itemfit(pp)$i.infitMSQ)
sd(itemfit(pp)$i.infitMSQ)
NPtest(IRTRolandMorris,method="T11")
plotGOF(lrt,conf=list())

##############################################################
#EXTERNAL VALIDITY
##############################################################


#############################################################################
#CAT
#############################################################################
install.packages("catR")
require(catR)
c<-coef(irt_model)
itemBank <- cbind(c[,2], c[,1], 0, 1)
catBank<-createItemBank(irt_model, model="2pl")
catBank
catBank$itemPar
plot(catBank$infoTab[,1])
plot(my2pl, type = "IIC", items=1)

items_administered<-c(4)
responses<-c(1)
it<-itemBank[items_administered, 1:4,drop=F ]
theta<-thetaEst(it, responses)
q<-nextItem(catBank, theta,out=items_administered)
q$item
