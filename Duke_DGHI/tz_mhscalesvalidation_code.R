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
	"ltm","gmodels","eRm","mirt","dplyr","devtools","reshape","mice"),
library, character.only=T)

#Package and codes to pull data from goodle sheets
#devtools::install_github("jennybc/googlesheets")
#library(googlesheets)
#suppressMessages(library(dplyr))

######################################################
#IMPORTING DATA
######################################################

# add the path to you computer between " "
data<-read.csv("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/Africa/Tz/MH post TBI in Tz/Tz_MHpostTBI_data.csv",sep=',')

data2<-read.csv("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/Africa/Tz/tz_bnipatients_data.csv")

######################################################
#DATA MANAGEMENT
######################################################

#Data WAVE 1
######################################################

#subsetting data set to keep only baseline data
data_validation<-data[data$redcap_event_name=="enrollment_arm_1",]

#recoding gender variable
data_validation$female<-car::recode(data_validation$female,"
	0='male';1='female'")

#recoding marital status variable
data_validation$married<-car::recode(data_validation$married,"
	0='married';1='married';2='not married';
	3='not married';4='married';5='not married'")

#recoding education varibLE
data_validation$education_cat<-car::recode(data_validation$education,"
     0:7='Some primary';8:13='Some secondary';
     14:16='Some university';89=NA")

# #recoding education varibLE
data_validation$occupation_cat<-car::recode(
	data_validation$occupation,"
     0='Business';1='Farming';
     3='Paid worker';4='Skilled worker';
     5='Paid worker';6='Other';8='Other';89=NA")

#recoding education varibLE
data_validation$age_cat<-car::recode(
	data_validation$age,"
     0:35='<35';36:100='>35'")

#Organize scale datasets
#AUDIT
audit_data<-with(data,data.frame(
	h1,h2,h3,h4,h5,h6,h7,h8,h9,h10))
# audit_data1<-with(data_validation,data.frame(
# 	h1,h2,h3))
# audit_data2<-with(data_validation,data.frame(
# 	h4,h5,h6,h7,h8,h9,h10))
# audit_data2_3<-with(data_validation,data.frame(
# 	h4,h5,h6))
# audit_data3_3<-with(data_validation,data.frame(
# 	h7,h8,h9,h10))

#CAGE
cage_data<-with(data,data.frame(
	h11,h12,h13,h14))
# summary(cage_data)
# cage_data<-na.omit(cage_data)

# #SF8
# sf8_PCS<-with(data,data.frame(sf8_b1,sf8_b2,sf8_b3,sf8_b4,sf8_b5))
# # data$sf8_mcs<-rowSums(sf8_PCS)

# sf8_MCS<-with(data,data.frame(sf8_b6,sf8_b7,sf8_b8))
# # data$sf8_pcs<-rowSums(sf8_MCS)

# #CES-D
# ces_data<-wiht(data,data.frame(e1,e2,e3,e4,e5,e6,e7,e8,e9,
# 	e10,e11,e12,e13,e14,e15,e16,e17,e18,e19,e20))

# #Kessler
# kessler_data<-wiht(data,data.frame(d1,d2,d3,d4,d5,d6,d7,d8,d9,
# 	d10))

# #PHQ9
# phq9<-with(data,data.frame(phq9_b11,phq9_b12,phq9_b13,phq9_b14,
# 	phq9_b15,phq9_b16,phq9_b17,phq9_b17,phq9_b18,phq9_b19))
# # data$phq9score<-rowSums(phq9)

# #FIM
# fim_physical<-with(data,data.frame(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,
# 	g11,g12,g13,g14,g15,g16))
# # data$fim_physical<-rowSums(fim_physical)/16

# fim_mental<-with(data,data.frame(g17,g18,g19,g20,g21,g22,g23,g24,g25,g26,
# 	g27,g28,g29,g30))
# # data$fim_mental<-rowSums(fim_mental)/14

# #Mini-Mental
# mental<-with(data,data.frame(f1a,f1b,f1c,f1d,f1e,f2a,f2b,f2c,f2d,f2e,f3,
# 	f4,f5,f6,f7,f8,f9,f10,f11,f12___0,f12___1,f12___2))
# # data$mental<-rowSums(mental)

# #MOCA
# moca<-with(data,data.frame(f17,f18,f19,f20,f21,f21b,f22,f23))
# # data$moca<-rowSums(moca)

# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
data_imputed <- mice(audit_data, seed = 2222, m=10)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
audit_data<-mice::complete(data_imputed,4)

# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
data_imputed <- mice(cage_data, seed = 2222, m=10)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
cage_data<-mice::complete(data_imputed,4)


#Data WAVE 2
######################################################
#getting second dataset for external validity
audit_data2<-with(data2,data.frame(
				how_often_drink,                  
				number_drinks_day,
				how_often_6_more_drinks,
				how_often_cant_stop_drinking,
				fail_expectation_bc_drinking,
				how_often_drink_morning,
				how_often_guilt_postdrinking,
				how_often_no_memory_postdrinking,
				drinking_injured_you_or_someone,
				others_concerned_your_drinking,
				consumption,
				alcohol_6h_ainjury,
				pos_etoh))

#Getting only participants that responded about alcohol consumption
audit_data2<-audit_data2[which(is.na(audit_data2$consumption)==FALSE),]

#imputing 0 for the answers from abstainers
NAto0<-function(x){
	car::recode(x,"NA=0")
	}

audit_data_cleaned<-lapply(audit_data2[1:10],NAto0)

######################################################################
#BASIC DESCRIPTIVES and EXPLORATORY ANALYSIS
######################################################################
###Section wih several exploratory data analysis functions
###### Exploratory Data Anlysis
###### UNIVARIATE

# Numerical descriptives
#summary(data)#This comand will provide a whole set of descriptive #results for each variables
describe(data_validation$age)
describe(data_validation$home_people)

# Categorical Descriptives
table<-with(data_validation,table(married))
table
prop.table(table)

# Categorical Descriptives
table<-with(data_validation,table(female))
table
prop.table(table)

# Categorical Descriptives
table<-with(data_validation,table(occupation_cat))
table
prop.table(table)

# Categorical Descriptives
table<-with(data_validation,table(education_cat))
table
prop.table(table)

##### BIVARIATE

# #Graphing and homogeneity
# boxplot(data$Idade~data$Classificacao) #will provide a boxplot for the #variables to analysis potential outliers
# ## Bartlett Test of Homogeneity of Variances
# #bartlett.test(data$Idade~data$Classificacao, data=data)
# ## Figner-Killeen Test of Homogeneity of Variances
# #fligner.test(data$Idade~data$Classificacao, data=data)
# #leveneTest(data$Idade~data$Classificacao, data=data)

# # Categorical Descriptives 2x2, 2x3 ...
# table<-with(bea_data,table(road_area,country)) #create cross-tabs with 2 caegorical variables
# table #display cross-tabs
# prop.table(table,2) #find proportions. Argument #2 means  proportion by columns. Change to 1 for rows.
# chisq.test(table) #chi-square test
# fisher.test(table) #fisher's exact correction for cases with less then 5 observations
# assocstats(table) #vcd package, gives all range of associations

# # Numerical descriptives
# # function by allow to appy a function to a vector conditional on a factor vector
# by(bea_data$density_car,bea_data$country,summary)
# #wilcox comparisons, change for t.test for parametric data
# wilcox.test(bea_data$density_car~bea_data$country)

summary(audit_data)

##############################################################
#AUDIT
##############################################################

#TAXONOMETRIC ANALAYSIS

#Taxonometric Scale
# MAMBAC(scale(NeckDisabilityIndexNA)[,1:3], Comp.Data = T)


#RELIABILITY
##############################################################
### INTERNAL CONSISTENCY
#RELIABILITY
#psych::alpha(cor_data,n.iter=1000,check.keys=TRUE)
psych::alpha(audit_data,n.iter=1000,check.keys=TRUE)
psych::alpha(audit_data1,n.iter=1000,check.keys=TRUE)
psych::alpha(audit_data2,n.iter=1000,check.keys=TRUE)
psych::alpha(audit_data2_3,n.iter=1000,check.keys=TRUE)
psych::alpha(audit_data3_3,n.iter=1000,check.keys=TRUE)

#### INTER-RATER Agreement
# data_agreement<-with(data,data.frame( ))

# data_sl_agree_model1<-melt(data_sl_temp_model1,id=c("rater","id"))

#Executing agreement nalysis
# agree<-agree(na.omit(agree_data_sl_model1), tolerance=0) #% of Agreement
# kappa<-cohen.kappa(na.omit(agree_data_sl_model1)) #Kappa-value
#AC1(kappa$agree)
#cor<-cor(agree_data_sl,method=c("kendall"))
#kendall<-Kendall(agree_data_sl$data_cluster_police_sl.RISK,agree_data_sl$data_cluster_survey_sl.RISK)
#poly<-hetcor(agree_data_sl)

#NETWORK 
##############################################################
# # Define the amout of factor to retain
# #Group of functinos to determine the number os items to be extracted
# cor_data<-cor_auto(audit_data)

# #Community analysis
# comprehension_network_glasso<-qgraph(cor_data,
# 	layout="spring",
# 	vsize=6,esize=20,graph="glasso",
# 	sampleSize=nrow(audit_data),
# 	legend.cex = 0.5,GLratio=1.5,minimum=0.1)

# #Calculating Community measures
# g<-as.igraph(comprehension_network_glasso) #creating igraph object
# # h<-walktrap.community(g) #creatin community object
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

# network_groups<-list(
# Component1=c(1,3,4,5,15,14),
# Component2=c(2,16,6,7),
# Component3=c(11,12,13,10),
# Component4=c(19,20,21,23),
# Component5=c(9,17,18,22,8)
# )

# creating vectors for labels
# node_labels<-c(
# "What is the area of the roadway?",
# "What type of roadway?",
# "Is this point at an intersection/junction?",
# "How many lanes in the roadway?",
# "Is there an auxiliary/other lane?",
# "How is the road surface conditions?",
# "Is there space on the side of the road 
# for any reason or use?",
# "Are there pedestrian pathways?",
# "Is there a Bus Stop?",
# "Is there a Speed bump?",
# "Is there a traffic light at this location?",
# "Are there road traffic signs at this hotspot?",
# "Is there a sign for speed limit of road?",
# "Road visibility is influenced by curves?",
# "Is the visibility influenced by 
# environmental factors?",
# "Are there bridges on the road?",
# "Is there a safe area for pedestrians 
# to cross the road?",
# "Is there a safe area for pedestrians
# to in the center of the road?",
# "Count the number of cars",
# "Count the number of moto",
# "Count the number of bike",
# "Count the number of pedestrians",
# "Count the number of bus/trucks"
# )

# creating nodes labels vector
# node_names<-c("RD","RT","INT","TLA","AR",
# 	"RC","RS",
# 	"WALK","BS","SB","TLI","TS","SL","CUR",
# 	"VIS","BRI","PED","PEDc","CARd","MOTOd","BIKEd","PEDd","TRUCKd")

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
# tiff("/Users/jnv4/Desktop/bea_pca_network.tiff", width = 1200,
#  # height = 700,compression = 'lzw')
# final_importance_network<-qgraph(cor_data,
# 	layout='spring',
# 	esize=20,
# 	graph="glasso",
# 	sampleSize=nrow(audit_data),
# 	legend.cex = 0.6,
# 	cut = 0.3,
# 	maximum = 1, 
# 	minimum = 0.1,
# 	esize = 20,
# 	vsize = 5, 
# 	repulsion = 0.8)
# 	# groups=network_groups,
# 	# nodeNames=node_labels,
# 	# color=c("gold","steelblue","red","grey80","green"),borders = FALSE,
# 	# labels=node_names)#,gray=T,)#,nodeNames=nomesqsg,layoutScale=c(2,2)
# # dev.off()
# #legend(0.8,-0.8, bty=".",c("Ensaio Clínico","Medicamentos","Outras Razões"),cex=1.2,fill=c("lightblue","red","yellow"))

#ANALISE PARALELA E EIGEN VALUES
#############################################################
#MODEL 1 - Risk due to road deisgn
# cor_data<-cor_auto(model1_bea)

#Function to calculate the KMO values - colocar link par ao gist
kmo<-kmo(audit_data) #Run the Kmo function for the data you want to calculate
kmo$overall
kmo$AIR #anti-image matrix

# par(mfrow=c(2,2)) #Command to configure the plot area for the scree plot graph
# ev <- eigen(cor_data) # get eigenvalues - insert the data you want to calculate the scree plot for
# ev # Show eigend values
# ap <- parallel(subject=nrow(cor_data),var=ncol(cor_data),rep=100,cent=.05) #Calculate the acceleration factor
# summary(ap)
# nS <- nScree(ev$values) #Set up the Scree Plot 
# # plotnScree(nS) # Plot the ScreePlot Graph
# my.vss <- VSS(cor_data,title="VSS of BEA data")
# #print(my.vss[,1:12],digits =2)
# VSS.plot(my.vss, title="VSS of 24 mental tests")
# scree(cor_data)
# VSS.scree(cor_data)
fa.parallel(audit_data,cor="poly")

fa.parallel(cage_data,cor="poly")

#EXPLORATORY FACTOR ANALYSIS
#############################################################
#Functino to exctract the factor loadings. 
#Arguments are DATA, Number of factors, rotation method. 
#Look here http://goo.gl/kY3ln for different met

#holds of estimations or rotations
fa(cor_data,2,rotate="promax")
# fa(NeckDisabilityIndex,1,fm="pa",rotate="oblimin")

#based on a polychoric correlation matrix
# fa.poly(data_stress_reco,3,fm="uls",rotate="oblimin")

#efa_LOD <- efa(motivation, method="cor.polycor")
#efa.plotCorr (efa_LOD)
#efa_LOD <- efa.compute(efa_LOD,factors =3,method="extract.uls", rotate="promax", horn=T)
#efa.plotScree(efa_LOD)
#efa_LOD<-efa.setMinLoad(efa_LOD, minload=0.40, col="black")
#efa.plotFactor(efa_LOD)
#qgraph(efa_LOD)

#CONFIRMATORY FACTOR ANALYSIS
#############################################################
#1factor model ###########
audit_model <- '
Audit =~  h1 + h2 + h3 + h4 + h5 + h6 + h7 + h8 + h9 + h10
			 '
fit <- lavaan::cfa(audit_model,
				   data = audit_data,
				   estimator="WLSMV",
				   ordered=names(audit_data))
summary(fit,
		fit.measures=TRUE)
lavaan::fitMeasures(fit,
					fit.measures = "all")
parameterEstimates(fit)
Est <- lavaan::parameterEstimates(fit,
								  ci = TRUE,
								  standardized = TRUE)
subset(Est, op == "=~")
subset(Est, op == "~~")


nodeLabels<-c("Q1",
              "Q2",
              "Q3",
              "Q4",
              "Q5",
              "Q6",
              "Q7",
              "Q8",
              "Q9",
              "Q10",
              "AUDIT")

color<-c(rep("grey30",10),rep("white",1))
borders<-c(rep("FALSE",10),rep("TRUE",1))
labelcex<-c(rep(1.5,10),rep(1.5,1))

tiff("/Users/jnv4/Desktop/resilience_stress_fig2.tiff", units='in', 
  width = 15,
 height = 10,compression = 'lzw',res=1200,bg = "white")
semPlot::semPaths(fit,
		"model")#,
		# residuals=TRUE,
		cut=1,
  		equalizeManifests=TRUE,
  		edge.color="black",
  		exoCov=FALSE,
  		intercepts=FALSE,
  		# nodeLabels=nodeLabels,
  		label.scale=FALSE,
  		edge.label.cex=1,
  		# label.cex=labelcex,
  		# color=color,
  		# borders=borders,
  		curvePivot = TRUE)
dev.off()

### Modification Indexes
Mod <- modificationIndices(fit)
subset(Mod, mi > 10)

#Composite Reliabilty
sum(Est$std.all[1:10])^2/(sum(Est$std.all[1:10])^2+
	sum(Est$std.all[47:56]))

#Average Extracted Variance
sum(Est$std.all[1:10]^2)/length(Est$std.all[1:10])

#Factor scores
audit_overall<-lavaan::predict(fit)

#Thresholds
with(subset(Est, op == "|"),
  by(std.all,lhs,mean))
tau<-as.data.frame(with(subset(Est, op == "|"),
  by(std.all,lhs,mean))[1:9])

# 2 factor model ###########

audit_model2 <- '
Audit =~  h1 + h2 + h3
Audit2 =~ h4 + h5 + h6 + h7 + h8 + h9 + h10
'

fit <- lavaan::cfa(audit_model2,
				   data = audit_data,
				   estimator="WLSM",
				   ordered=names(audit_data))
summary(fit,
		fit.measures=TRUE)
lavaan::fitMeasures(fit,
					fit.measures = "all")
parameterEstimates(fit)
Est <- lavaan::parameterEstimates(fit,
								  ci = TRUE,
								  standardized = TRUE)
subset(Est, op == "=~")
subset(Est, op == "~~")

nodeLabels<-c("Q1",
              "Q2",
              "Q3",
              "Q4",
              "Q5",
              "Q6",
              "Q7",
              "Q8",
              "Q9",
              "Q10",
              "AUDIT1",
              "AUDIT2")

color<-c(rep("grey",10),rep("white",1))
borders<-c(rep("FALSE",10),rep("TRUE",1))
labelcex<-c(rep(0.7,10),rep(1,1))

tiff("/Users/jnv4/Desktop/resilience_stress_fig2.tiff", units='in', 
  width = 15,
 height = 10,compression = 'lzw',res=1200,bg = "white")
semPlot::semPaths(fit,"std")#,residuals=TRUE, cut=1,
  equalizeManifests=TRUE,edge.color="black",exoCov=FALSE,
  intercepts=FALSE, nodeLabels=nodeLabels,label.scale=FALSE,
  edge.label.cex=1, label.cex=labelcex)
sdev.off()
### Modification Indexes
Mod <- modificationIndices(fit)
subset(Mod, mi > 10)

#Composite Reliabilty
sum(Est$std.all[1:3])^2/(sum(Est$std.all[1:3])^2+
	sum(Est$std.all[47:49]))

sum(Est$std.all[4:10])^2/(sum(Est$std.all[4:10])^2+
	sum(Est$std.all[50:56]))

#Average Extracted Variance
sum(Est$std.all[1:3]^2)/length(Est$std.all[1:3])

sum(Est$std.all[4:10]^2)/length(Est$std.all[4:10])

#Factor scores
audit_2dimension<-lavaan::predict(fit)

# 3-factor model ###########

audit_model3 <- '
Audit =~  h1 + h2 + h3
Audit2 =~ h4 + h5 + h6
Audit3 =~ h7 + h8 + h9 + h10
# Audit4 =~ h1 + h2 + h3 + h4 + h5 + h6 + h7 + h8 + h9 + h10
			 '

fit <- lavaan::cfa(audit_model3,
				   data = audit_data,
				   estimator="WLSM")
summary(fit,
		fit.measures=TRUE)
lavaan::fitMeasures(fit,
					fit.measures = "all")
parameterEstimates(fit)
Est <- lavaan::parameterEstimates(fit,
								  ci = TRUE,
								  standardized = TRUE)
subset(Est, op == "=~")
subset(Est, op == "~~")

nodeLabels<-c("Q1",
              "Q2",
              "Q3",
              "Q4",
              "Q5",
              "Q6",
              "Q7",
              "Q8",
              "Q9",
              "Q10",
              "AUDIT")

color<-c(rep("grey",10),rep("white",1))
borders<-c(rep("FALSE",10),rep("TRUE",1))
labelcex<-c(rep(0.7,10),rep(1,1))

tiff("/Users/jnv4/Desktop/resilience_stress_fig2.tiff", units='in', 
  width = 15,
 height = 10,compression = 'lzw',res=1200,bg = "white")
semPlot::semPaths(fit,
		"std",
		# residuals=TRUE,
		cut=1,
  		equalizeManifests=TRUE,
  		edge.color="black",
  		exoCov=FALSE,
  		intercepts=FALSE,
  		# nodeLabels=nodeLabels,
  		label.scale=FALSE,
  		edge.label.cex=1,
  		# label.cex=labelcex,
  		# color=color,
  		# borders=borders,
  		curvePivot = TRUE)
dev.off()
### Modification Indexes
Mod <- modificationIndices(fit)
subset(Mod, mi > 10)

#Composite Reliabilty
sum(Est$std.all[1:3])^2/(sum(Est$std.all[1:3])^2+
	sum(Est$std.all[47:49]))

sum(Est$std.all[4:6])^2/(sum(Est$std.all[4:6])^2+
	sum(Est$std.all[50:52]))

sum(Est$std.all[7:10])^2/(sum(Est$std.all[7:10])^2+
	sum(Est$std.all[53:56]))

#Average Extracted Variance
sum(Est$std.all[1:3]^2)/length(Est$std.all[1:3])

sum(Est$std.all[4:6]^2)/length(Est$std.all[4:6])

sum(Est$std.all[7:10]^2)/length(Est$std.all[7:10])

#Factor scores
audit_3dimension<-lavaan::predict(fit)

##############################################################
#CAGE
##############################################################
#TAXONOMETRIC ANALAYSIS

#Taxonometric Scale
# MAMBAC(scale(NeckDisabilityIndexNA)[,1:3], Comp.Data = T)


#RELIABILITY
##############################################################
### INTERNAL CONSISTENCY
#RELIABILITY
#psych::alpha(cor_data,n.iter=1000,check.keys=TRUE)
psych::alpha(audit_data,n.iter=1000,check.keys=TRUE)
psych::alpha(audit_data1,n.iter=1000,check.keys=TRUE)
psych::alpha(audit_data2,n.iter=1000,check.keys=TRUE)
psych::alpha(audit_data2_3,n.iter=1000,check.keys=TRUE)
psych::alpha(audit_data3_3,n.iter=1000,check.keys=TRUE)

psych::alpha(cage_data,n.iter=1000,check.keys=TRUE)

#### INTER-RATER Agreement
data_agreement<-with(data,data.frame( ))

# data_sl_agree_model1<-melt(data_sl_temp_model1,id=c("rater","id"))

#Executing agreement nalysis
# agree<-agree(na.omit(agree_data_sl_model1), tolerance=0) #% of Agreement
# kappa<-cohen.kappa(na.omit(agree_data_sl_model1)) #Kappa-value
#AC1(kappa$agree)
#cor<-cor(agree_data_sl,method=c("kendall"))
#kendall<-Kendall(agree_data_sl$data_cluster_police_sl.RISK,agree_data_sl$data_cluster_survey_sl.RISK)
#poly<-hetcor(agree_data_sl)

#NETWORK 
##############################################################
# # Define the amout of factor to retain
# #Group of functinos to determine the number os items to be extracted
# cor_data<-cor_auto(audit_data)

# #Community analysis
# comprehension_network_glasso<-qgraph(cor_data,
# 	layout="spring",
# 	vsize=6,esize=20,graph="glasso",
# 	sampleSize=nrow(audit_data),
# 	legend.cex = 0.5,GLratio=1.5,minimum=0.1)

# #Calculating Community measures
# g<-as.igraph(comprehension_network_glasso) #creating igraph object
# # h<-walktrap.community(g) #creatin community object
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

# network_groups<-list(
# Component1=c(1,3,4,5,15,14),
# Component2=c(2,16,6,7),
# Component3=c(11,12,13,10),
# Component4=c(19,20,21,23),
# Component5=c(9,17,18,22,8)
# )

# creating vectors for labels
# node_labels<-c(
# "What is the area of the roadway?",
# "What type of roadway?",
# "Is this point at an intersection/junction?",
# "How many lanes in the roadway?",
# "Is there an auxiliary/other lane?",
# "How is the road surface conditions?",
# "Is there space on the side of the road 
# for any reason or use?",
# "Are there pedestrian pathways?",
# "Is there a Bus Stop?",
# "Is there a Speed bump?",
# "Is there a traffic light at this location?",
# "Are there road traffic signs at this hotspot?",
# "Is there a sign for speed limit of road?",
# "Road visibility is influenced by curves?",
# "Is the visibility influenced by 
# environmental factors?",
# "Are there bridges on the road?",
# "Is there a safe area for pedestrians 
# to cross the road?",
# "Is there a safe area for pedestrians
# to in the center of the road?",
# "Count the number of cars",
# "Count the number of moto",
# "Count the number of bike",
# "Count the number of pedestrians",
# "Count the number of bus/trucks"
# )

# creating nodes labels vector
# node_names<-c("RD","RT","INT","TLA","AR",
# 	"RC","RS",
# 	"WALK","BS","SB","TLI","TS","SL","CUR",
# 	"VIS","BRI","PED","PEDc","CARd","MOTOd","BIKEd","PEDd","TRUCKd")

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
# tiff("/Users/jnv4/Desktop/bea_pca_network.tiff", width = 1200,
#  # height = 700,compression = 'lzw')
# final_importance_network<-qgraph(cor_data,
# 	layout='spring',
# 	esize=20,
# 	graph="glasso",
# 	sampleSize=nrow(audit_data),
# 	legend.cex = 0.6,
# 	cut = 0.3,
# 	maximum = 1, 
# 	minimum = 0.1,
# 	esize = 20,
# 	vsize = 5, 
# 	repulsion = 0.8)
# 	# groups=network_groups,
# 	# nodeNames=node_labels,
# 	# color=c("gold","steelblue","red","grey80","green"),borders = FALSE,
# 	# labels=node_names)#,gray=T,)#,nodeNames=nomesqsg,layoutScale=c(2,2)
# # dev.off()
# #legend(0.8,-0.8, bty=".",c("Ensaio Clínico","Medicamentos","Outras Razões"),cex=1.2,fill=c("lightblue","red","yellow"))

#ANALISE PARALELA E EIGEN VALUES
#############################################################
#MODEL 1 - Risk due to road deisgn
# cor_data<-cor_auto(model1_bea)

#Function to calculate the KMO values - colocar link par ao gist
kmo<-kmo(audit_data) #Run the Kmo function for the data you want to calculate
kmo$overall
kmo$AIR #anti-image matrix

# par(mfrow=c(2,2)) #Command to configure the plot area for the scree plot graph
# ev <- eigen(cor_data) # get eigenvalues - insert the data you want to calculate the scree plot for
# ev # Show eigend values
# ap <- parallel(subject=nrow(cor_data),var=ncol(cor_data),rep=100,cent=.05) #Calculate the acceleration factor
# summary(ap)
# nS <- nScree(ev$values) #Set up the Scree Plot 
# # plotnScree(nS) # Plot the ScreePlot Graph
# my.vss <- VSS(cor_data,title="VSS of BEA data")
# #print(my.vss[,1:12],digits =2)
# VSS.plot(my.vss, title="VSS of 24 mental tests")
# scree(cor_data)
# VSS.scree(cor_data)
fa.parallel(audit_data,cor="poly")

fa.parallel(cage_data,cor="poly")

#EXPLORATORY FACTOR ANALYSIS
#############################################################
#Functino to exctract the factor loadings. 
#Arguments are DATA, Number of factors, rotation method. 
#Look here http://goo.gl/kY3ln for different met

#holds of estimations or rotations
fa(cor_data,2,rotate="promax")
# fa(NeckDisabilityIndex,1,fm="pa",rotate="oblimin")

#based on a polychoric correlation matrix
# fa.poly(data_stress_reco,3,fm="uls",rotate="oblimin")

#efa_LOD <- efa(motivation, method="cor.polycor")
#efa.plotCorr (efa_LOD)
#efa_LOD <- efa.compute(efa_LOD,factors =3,method="extract.uls", rotate="promax", horn=T)
#efa.plotScree(efa_LOD)
#efa_LOD<-efa.setMinLoad(efa_LOD, minload=0.40, col="black")
#efa.plotFactor(efa_LOD)
#qgraph(efa_LOD)

#CONFIRMATORY FACTOR ANALYSIS
#############################################################
#1 factor model
cage_model <- '
Cage =~  h11 + h12 + h13 + h14
			 '

fit <- lavaan::cfa(cage_model,
				   data = cage_data,
				   estimator="WLSM")
summary(fit,
		fit.measures=TRUE)
lavaan::fitMeasures(fit,
					fit.measures = "all")
parameterEstimates(fit)
Est <- lavaan::parameterEstimates(fit,
								  ci = TRUE,
								  standardized = TRUE)
subset(Est, op == "=~")
subset(Est, op == "~~")

nodeLabels<-c("Q1",
              "Q2",
              "Q3",
              "Q4",
              "CAGE")

color<-c(rep("grey",4),rep("white",1))
borders<-c(rep("FALSE",4),rep("TRUE",1))
labelcex<-c(rep(0.7,19),rep(1,4))

tiff("/Users/jnv4/Desktop/resilience_stress_fig2.tiff", units='in', 
  width = 15,
 height = 10,compression = 'lzw',res=1200,bg = "white")
semPaths(fit,"std",residuals=TRUE, cut=1,
  equalizeManifests=TRUE,edge.color="black",exoCov=FALSE,
  intercepts=FALSE, nodeLabels=nodeLabels,label.scale=FALSE,
  edge.label.cex=1, label.cex=labelcex, color=color,borders=borders)
dev.off()

### Modification Indexes
Mod <- modificationIndices(fit)
subset(Mod, mi > 10)

#Composite Reliabilty
sum(Est$std.all[1:4])^2/(sum(Est$std.all[1:4])^2+
	sum(Est$std.all[5:8]))

#Average Extracted Variance
sum(Est$std.all[1:4]^2)/length(Est$std.all[1:4])

#Factor scores
cage_overall<-lavaan::predict(fit)

#ITEM RESPONSE THEORY
##############################################################

#### USING eRM Package
IRTRolandMorris <- PCM(audit_data)
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

#############################################################################
#CAT
#############################################################################
# install.packages("catR")
# require(catR)
# c<-coef(irt_model)
# itemBank <- cbind(c[,2], c[,1], 0, 1)
# catBank<-createItemBank(irt_model, model="2pl")
# catBank
# catBank$itemPar
# plot(catBank$infoTab[,1])
# plot(my2pl, type = "IIC", items=1)

# items_administered<-c(4)
# responses<-c(1)
# it<-itemBank[items_administered, 1:4,drop=F ]
# theta<-thetaEst(it, responses)
# q<-nextItem(catBank, theta,out=items_administered)
# q$item

#############################################################################
#GENERATING SCORES
#############################################################################

alcohol_scores<-data.frame(audit_overall,
						   audit_2dimension,
						   audit_3dimension,
						   cage_overall)
colnames(alcohol_scores)<-c("audit_overall",
                            "audit_2dim_d1",
                            "audit_2dim_d2",
                            "audit_3dim_d1",
                            "audit_3dim_d2",
                            "audit_3dim_d3",
                            "cage_overall")

rescale <- function(x)(x-min(x))/(max(x) - min(x)) * 100
alcohol_scores_scaled<-lapply(alcohol_scores,rescale)
alcohol_scores_scaled<-as.data.frame(alcohol_scores_scaled)

# write.csv(alcohol_scores_scaled,"/Users/jnv4/Desktop/alcohol_scores.csv")

Hmisc::rcorr(as.matrix(alcohol_scores),type="spearman")

cor(alcohol_scores,method="spearman")


#############################################################################
#VALIDITY MEASURE
#############################################################################
# sf8_scores_scaled<-read.csv("/Users/jnv4/Desktop/sf8_scores.csv")
# depression_scores_scaled<-read.csv("/Users/jnv4/Desktop/depression_scores.csv")
# kessler_scores_scaled<-read.csv("/Users/jnv4/Desktop/kessler_scores.csv")

# cor_data<-data.frame(alcohol_scores_scaled,
# 					 sf8_scores_scaled[2:4],
# 					 depression_scores_scaled[2:3],
# 					 kessler_scores_scaled[2:5])

# cor_data<-cor_data[data$redcap_event_name=="enrollment_arm_1",]


# Hmisc::rcorr(as.matrix(cor_data))

audit2<-as.data.frame(audit_data_cleaned)
audit_score2<-rowSums(audit2)
audit_score2_1<-rowSums(audit2[1:3])
audit_score2_2<-rowSums(audit2[4:10])
audit_score3_1<-rowSums(audit2[1:3])
audit_score3_2<-rowSums(audit2[4:6])
audit_score3_3<-rowSums(audit2[7:10])



rescale <- function(x)(x-min(x))/(max(x) - min(x)) * 100
# audit_score2_scaled<-lapply(alcohol_scores,rescale)
# alcohol_scores_scaled<-as.data.frame(alcohol_scores_scaled)
audit_score2_scaled<-rescale(scale(rowSums(audit2)))
audit_score2_1_scaled<-rescale(scale(rowSums(audit2[1:3])))
audit_score2_2_scaled<-rescale(scale(rowSums(audit2[4:10])))
audit_score3_1_scaled<-rescale(scale(rowSums(audit2[1:3])))
audit_score3_2_scaled<-rescale(scale(rowSums(audit2[4:6])))
audit_score3_3_scaled<-rescale(scale(rowSums(audit2[7:10])))

describeBy(audit_score2,audit_data$alcohol_6h_ainjury)
by(audit_score2_scaled,audit_data2$alcohol_6h_ainjury,summary)
wilcox.test(audit_score2_scaled~audit_data2$alcohol_6h_ainjury)

# describeBy(audit_score2_1_scaled,audit_data$alcohol_6h_ainjury)
by(audit_score2_1_scaled,audit_data2$alcohol_6h_ainjury,summary)
wilcox.test(audit_score2_1_scaled~audit_data2$alcohol_6h_ainjury)

# describeBy(audit_score2_2,audit_data$alcohol_6h_ainjury)
by(audit_score2_2_scaled,audit_data2$alcohol_6h_ainjury,summary)
wilcox.test(audit_score2_2_scaled~audit_data2$alcohol_6h_ainjury)

# describeBy(audit_score2_2,audit_data$alcohol_6h_ainjury)
by(audit_score3_1_scaled,audit_data2$alcohol_6h_ainjury,summary)
wilcox.test(audit_score2_2_scaled~audit_data2$alcohol_6h_ainjury)

# describeBy(audit_score2_2,audit_data$alcohol_6h_ainjury)
by(audit_score3_2_scaled,audit_data2$alcohol_6h_ainjury,summary)
wilcox.test(audit_score3_2_scaled~audit_data2$alcohol_6h_ainjury)

# describeBy(audit_score2_2,audit_data$alcohol_6h_ainjury)
by(audit_score3_3_scaled,audit_data2$alcohol_6h_ainjury,summary)
wilcox.test(audit_score3_2_scaled~audit_data2$alcohol_6h_ainjury)






describeBy(audit_score2,audit_data$pos_etoh)

wilcox.test(audit_score2~audit_data$pos_etoh)

boxplot<-data.frame(audit_score2,grop=audit_data$alcohol_6h_ainjury)
boxplot<-na.omit(boxplot)

library(ggplot2)
# Use single color
p<-ggplot(boxplot, aes(x=as.factor(grop), y=audit_score2)) +
  geom_boxplot(fill='#A4A4A4', color="black")
p + theme(legend.position="left")
# p + scale_fill_grey() + theme_classic()

# Change box plot colors by groups
p<-ggplot(ToothGrowth, aes(x=dose, y=len, fill=dose)) +
  geom_boxplot()
p



