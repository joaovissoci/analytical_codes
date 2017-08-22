#####################################################################################
#BASIC R STATISTICS TEMPLATE
#####################################################################################
#
#
#
#
#
#############################################################################
#SETTING ENVIRONMENT
#############################################################################
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
lapply(c("Hmisc","car","psych","nortest","ggplot2","pastecs","repmis",
	"polycor","nortest"), 
library, character.only=T)
#############################################################################
#IMPORTING DATA
#############################################################################
#LOADING DATA FROM A .CSV FILE
#data<-read.csv("/Users/rpietro/Desktop/MDD_BIPD_Baseline.csv",sep=",")
#information between " " are the path to the directory in your computer where the data is stored

#Import data from Dropbox, in .csv format
#Instructions here http://goo.gl/Ofa7gQ
#data1 <- repmis::source_DropboxData("pem_parasito.csv",
#                                  "tkxmkg9pybmtsgh",
#                                  sep = ",",
#                                  header = TRUE)

data <- read.csv("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/Africa/Tz/MH post TBI in Tz/Tz_MHpostTBI_data.csv", header = TRUE)

data_registry<-read.csv("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/Africa/Tz/tbi_registry/tz_TBIregistry_data.csv",header=TRUE)

# data_tbi <- read.csv("/Users/jnv4/Desktop/tz_tbiregistry_data.csv", header = TRUE)

#############################################################################
#MERGING REGISTRY AND MH data
#############################################################################

data_registry$tbi_reg<-data_registry$study_id

data_mhregistry1<-merge(x = data, 
					   y = data_registry, 
					   by = "tbi_reg", 
					   all.x = TRUE)

# write.csv(data_mhregistry,"/Users/jnv4/Box Sync/Home Folder jnv4/Data/Global EM/Africa/tbi_registry/tz_TBIregistryANDmh_data.csv")

#subsetting data set to keep only baseline data
data_mhregistry<-data_mhregistry1[
			data_mhregistry1$redcap_event_name=="enrollment_arm_1",]

#############################################################################
#DATA MANAGEMENT
#############################################################################

data_mhregistry$married_recoded<-car::recode(data_mhregistry$married,
	"0=1;1=1;4=1;2=0;3=0;5=0")

data_mhregistry$occupation_recoded<-car::recode(data_mhregistry$occupation,
	"89='Other'")

data_mhregistry$education_recoded<-car::recode(data_mhregistry$education,"
	1:7='Primary';8:13='Form';14:16='University';")

#
sf8_PCS<-with(data_mhregistry,data.frame(sf8_b1,sf8_b2,sf8_b3,sf8_b4,sf8_b5))
data_mhregistry$sf8_mcs<-rowSums(sf8_PCS)

sf8_MCS<-with(data_mhregistry,data.frame(sf8_b6,sf8_b7,sf8_b8))
data_mhregistry$sf8_pcs<-rowSums(sf8_MCS)

phq9<-with(data_mhregistry,data.frame(phq9_b11,phq9_b12,phq9_b13,phq9_b14,
	phq9_b15,phq9_b16,phq9_b17,phq9_b18,phq9_b19))
data_mhregistry$phq9score<-rowSums(phq9)

audit<-with(data_mhregistry,data.frame(h1,h2,h3,h4,h5,h6,h7,h8,h9,h10))
data_mhregistry$auditscore<-rowSums(audit)

cage<-with(data_mhregistry,data.frame(h11,h12,h13,h14))
data_mhregistry$cagescore<-rowSums(cage)

fim_physical<-with(data_mhregistry,data.frame(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,
	g11,g12,g13,g14,g15,g16))
data_mhregistry$fim_physical_score<-rowSums(fim_physical)/16

fim_mental<-with(data_mhregistry,data.frame(g17,g18,g19,g20,g21,g22,g23,g24,g25,g26,
	g27,g28,g29,g30))
data_mhregistry$fim_mental_score<-rowSums(fim_mental)/14

# mental<-with(data_mhregistry,data.frame(f1a,f1b,f1c,f1d,f1e,f2a,f2b,f2c,f2d,f2e,f3,
	# f4,f5,f6,f7,f8,f9,f10,f11,f12___0,f12___1,f12___2))
# data_mhregistry$mental<-rowSums(mental)

moca<-with(data_mhregistry,data.frame(f17,f18,f19,f20,f21,f21b,f22,f23))
data_mhregistry$moca_score<-rowSums(moca)

kessler<-with(data_mhregistry,data.frame(d1,d2,d3,d4,d5,d6,d7,d8,d9,
	d10))
data_mhregistry$kessler_scpre<-rowSums(kessler)

#classifying variables
data_mhregistry$phq9_cat<-car::recode(data_mhregistry$phq9score,"0:4.9='no';5:21='yes'")
# ces_score_cat<-car::recode(data$ces_score,"0:15.9='no';16:45='yes'")
data_mhregistry$kes_score_cat<-car::recode(data_mhregistry$kes_score,"0:19.9='no';20:50='yes'")
data_mhregistry$auditscore_cat<-car::recode(data_mhregistry$auditscore,"0:7.9='no';8:32='yes'")
# fimphysical_cat<-car::recode(data$fim_physical,"0:5.99='yes';else='no'")
# fim_mental_cat<-car::recode(data$fim_mental,"0:5.99='yes';else='no'")
# mental_cat<-car::recode(data$mental,"0:24='yes';25:30='no'")
# data_mhregistry$cage_cat<-car::recode(data$cagescore,"0:1='no';2:4='yes'")

#recoding gos
data_mhregistry$gos<-as.factor(car::recode(
	data_mhregistry$gos,"1:4='Death';
					   5='Alive'"))

#recoding gcs
data_mhregistry$gcs<-as.factor(car::recode(
	data_mhregistry$gcs_tot,"1:12='Severe';
					   13:15='Non-severe'"))

mhealth_scales<-cbind(sf8_PCS,
					  sf8_MCS,
					  phq9,
					  audit,
					  cage,
					  fim_physical,
					  fim_mental,
					  moca,
					  kessler,
					  gcs=data_mhregistry$gcs_tot,
					  moi=data_mhregistry$moi,
					  gos=data_mhregistry$gos)

#Recoding clinical conditionMessage
# data_tbi$registry_year<-as.Date(as.character(data_tbi$date_arrival),
#   format = "%m/%d/%y")

#############################################################################
#BASIC DESCRIPTIVES and EXPLORATORY ANALYSIS
#############################################################################
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
#############################################################################
#DESCRIPTIVES
#############################################################################
# Gender 0=male 1=female
table<-with(data_mhregistry,table(female))
table
prop.table(table)
# table<-with(data_mhregistry,table(female,gcs))
# table
# prop.table(table,2)
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package

# Marital 0=not_married 1=married
table<-with(data_mhregistry,table(married_recoded))
table
prop.table(table)
# table<-with(data_mhregistry,table(married_recoded,gcs))
# table
# prop.table(table,2)
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package

# Ocupation
table<-with(data_mhregistry,table(occupation_recoded))
table
prop.table(table)
#table<-with(data,table(female,))
#table
#prop.table(table,2)
#chisq.test(table)
#fisher.test(table)
#assocstats(table) #vcd package

# Education
table<-with(data_mhregistry,table(education_recoded))
table
prop.table(table)
#table<-with(data,table(female,))
#table
#prop.table(table,2)
#chisq.test(table)
#fisher.test(table)
#assocstats(table) #vcd package

# Home people residing in the house
with(data_mhregistry,summary(age.x))
# ad.test(data$age)
#hist(data$home_people)
#ci_func(data$home_people,.95)
#by(data$home_people,data$risk_classification,summary)
#wilcox.test(data$home_people~data$risk_classification)

# Home people residing in the house
with(data,by(home_people,redcap_event_name,summary))
# summary(data$home_people)
ad.test(data$home_people)
#hist(data$home_people)
#ci_func(data$home_people,.95)
#by(data$home_people,data$risk_classification,describe)
#wilcox.test(data$home_people~data$risk_classification)

# Minors residing in the house
summary(data$minors_home)
ad.test(data$minors_home)
#hist(data$home_people)
#ci_func(data$home_people,.95)
#by(data$home_people,data$risk_classification,describe)
#wilcox.test(data$home_people~data$risk_classification)

# Minors supporting financially in the house
summary(data$minors_finan)
ad.test(data$minors_finan)
#hist(data$home_people)
#ci_func(data$home_people,.95)
#by(data$home_people,data$risk_classification,describe)
#wilcox.test(data$home_people~data$risk_classification)

# Adult supporting financially
summary(data$adult_finan)
ad.test(data$adult_finan)
#hist(data$home_people)
#ci_func(data$home_people,.95)
#by(data$home_people,data$risk_classification,describe)
#wilcox.test(data$home_people~data$risk_classification)

# Personal income
with(data_mhregistry,summary(personal_income))
# summary(data$personal_income)
ad.test(data$personal_income)
#hist(data$home_people)
#ci_func(data$home_people,.95)
#by(data$home_people,data$risk_classification,describe)
#wilcox.test(data$home_people~data$risk_classification)

# Family income
with(data,by(fam_income,redcap_event_name,summary))
ad.test(data$fam_income)
#hist(data$home_people)
#ci_func(data$home_people,.95)
#by(data$home_people,data$risk_classification,describe)
#wilcox.test(data$home_people~data$risk_classification)

######## CLINICAL INDICATORS
# GCS
with(data_mhregistry,summary(gcs_tot))
ad.test(data$fam_income)
#hist(data$home_people)
#ci_func(data$home_people,.95)
#by(data$home_people,data$risk_classification,describe)
#wilcox.test(data$home_people~data$risk_classification)

# GOS
with(data_mhregistry,summary(gose))
ad.test(data$fam_income)
#hist(data$home_people)
#ci_func(data$home_people,.95)
#by(data$home_people,data$risk_classification,describe)
#wilcox.test(data$home_people~data$risk_classification)

# MOI
table<-with(data_mhregistry,table(moi))
table
prop.table(table)

######## MENTAL HEALTH SCALES

#SF8

# PHQ9
with(data_mhregistry,by(phq9score,redcap_event_name,summary))
summary(phq9score)
ad.test(phq9score)
# One Way Anova (Completely Randomized Design)
kruskal.test(data$phq9score ~ data$redcap_event_name, data=data)
posthoc.kruskal.nemenyi.test(x=data$phq9score, g=data$redcap_event_name,
 method="Chisq")
# summary(fit)
#hist(data$home_people)
#ci_func(data$home_people,.95)
#by(data$home_people,data$risk_clas0sification,describe)
#wilcox.test(data$home_people~data$risk_classification)
table<-with(data_mhregistry,table(phq9_cat))
table
prop.table(table)

# KESSLER
with(data,by(kes_score,redcap_event_name,summary))
# 1summary(kes_score)
ad.test(kes_score)
# One Way Anova (Completely Randomized Design)
kruskal.test(data$kes_score ~ data$redcap_event_name, data=data)
posthoc.kruskal.nemenyi.test(x=data$kes_score, g=data$redcap_event_name,
 method="Chisq")
#hist(data$home_people)
#ci_func(data$home_people,.95)
#by(data$home_people,data$risk_classification,describe)
#wilcox.test(data$home_people~data$risk_classification)
table<-with(data_mhregistry,table(kes_score_cat))
table
prop.table(table)

# CES
# with(data,by(ces_score,redcap_event_name,summary))
# # 1summary(ces_score)
# ad.test(ces_score)
# # One Way Anova (Completely Randomized Design)
# kruskal.test(data$ces_score ~ data$redcap_event_name, data=data)
# posthoc.kruskal.nemenyi.test(x=data$ces_score, g=data$redcap_event_name,
#  method="Chisq")
# #hist(data$home_people)
# #ci_func(data$home_people,.95)
# #by(data$home_people,data$risk_classification,describe)
# #wilcox.test(data$home_people~data$risk_classification)
# table<-with(data,table(ces_score_cat))
# table
# prop.table(table)

# AUDIT
summary(auditscore)
ad.test(auditscore)
#hist(data$home_people)
#ci_func(data$home_people,.95)
#by(data$home_people,data$risk_classification,describe)
#wilcox.test(data$home_people~data$risk_classification)
table<-with(data_mhregistry,table(auditscore_cat))
table
prop.table(table)

#############################################################################
#ANALYSIS OF VARIANCE
#############################################################################
# One Way Anova (Completely Randomized Design)
fit <- aov(Idade ~ Classificacao, data=data)
summary(fit)

# Randomized Block Design (B is the blocking factor) 
fit <- aov(Idade ~ Classificacao+Sexo, data=data)
summary(fit)

# Two Way Factorial Design 
fit <- aov(Idade ~ Classificacao*Sexo, data=data)
summary(fit)

# Tukey Honestly Significant Differences
TukeyHSD(fit) # where fit comes from aov()

# Analysis of Covariance 
fit <- aov(Idade ~ Classificacao + IMC, data=data)
summary(fit)

# Kruskal Wallis Test One Way Anova by Ranks 
kruskal.test(Idade ~ Classificacao, data=data) # where y1 is numeric and A is a factor

#####################################################################################
#CORRELATIONS
#####################################################################################
#Pearson
cor(numeric, use="complete.obs", method="pearson") 
#Spearman
cor(numeric, use="complete.obs", method="spearman") 
#Kendall
cor(numeric, use="complete.obs", method="kendall")

#Significance testing
rcorr(as.matrix(numeric), type="pearson") # type can be pearson or spearman

cor.test(numeric$Peso,numeric$Altura) #Used for a single test of significance

# heterogeneous correlations in one matrix 
# pearson (numeric-numeric), 
# polyserial (numeric-ordinal), 
# and polychoric (ordinal-ordinal)
# x is a data frame with ordered factors 
# and numeric variables
hetcor(data) 

# polychoric correlation
# x is a contingency table of counts
polychor(data) 

#############################################################################
#GRAPH
#############################################################################

library(qgraph)
cor<-cor_auto(mhealth_scales)

#qsgc<-qsgc$rho

#listing grouping variables in the network resulting from the community analysis
network_groups<-list(QOL_Mental=c(1:5),
								QOL_Physical=c(6:8),
								Depression=c(9:17),
								Alcohol_use=c(18:31),
								Funct_Physical=c(32:46),
								Funct_Cog=c(47:60),
								Cognition=c(61:69),
								Distress=c(70:79),
								Clinicals=c(80:81))

# # creating vectors for labels
# importance_node_labels<-c("Why is this study being done?", 
# 	"What is involved in this study?", 
# 	"Who is going to be my doctor in this study?",
# 	"How many people will take part in this study?",
# 	"How long will I be in this study?",
# 	"What are the benefits of being in this study?",
# 	"What about compensation?",
# 	"What are the risks of being in this study?",
# 	"What are the costs?",
# 	"Will my information be kept confidential?",
# 	"What about research related injuries?",
# 	"What are the alternatives to being in this study?",
# 	"What if I want decline participation or withdraw?",
# 	"Whom do I call if I have questions or trouble?",
# 	"Willingness to participate")

# creating nodes labels vector
node_names<-paste("Q ",c(1:81),sep="")

# creating vector with mean values for each node
# mean_data<-sapply(importance_network_data,mean)

#creating vector with mean values adjusted to proportional sizes to be plotted
# importance_vSize<-c(mean_data[1:14]/min(mean_data[1:14]),1.81)

#Calculating Community measures
# g<-as.igraph(importance_network_glasso) #creating igraph object
# #h<-walktrap.community(g) #creatin community object
# h<-spinglass.community(g, weights=NA)
# plot(h,g) #plotting community network
# h$membership #extracting community membership for each node on the network

#Identify SPLs within the graph and extract direct paths to WP
# predictors<-centrality(importance_network_glasso)$ShortestPaths[,15]
# predictors

# #getting edge list with edges originating, receiveing and weights
# importance_network_glasso$Edgelist$from
# importance_network_glasso$Edgelist$to
# importance_network_glasso$Edgelist$weight

# #Extracting edges weights for each direct path
# subset(importance_network_glasso$Edgelist$weight,
# 	importance_network_glasso$Edgelist$from==1 & 
# 	importance_network_glasso$Edgelist$to==15)
# subset(importance_network_glasso$Edgelist$weight,
# 	importance_network_glasso$Edgelist$from==2 & 
# 	importance_network_glasso$Edgelist$to==15)
# subset(importance_network_glasso$Edgelist$weight,
# 	importance_network_glasso$Edgelist$from==3 & 
# 	importance_network_glasso$Edgelist$to==15)
# subset(importance_network_glasso$Edgelist$weight,
# 	importance_network_glasso$Edgelist$from==10 & 
# 	importance_network_glasso$Edgelist$to==15)
# subset(importance_network_glasso$Edgelist$weight,
# 	importance_network_glasso$Edgelist$from==13 & 
# 	importance_network_glasso$Edgelist$to==15)

 network_glasso<-qgraph(cor,
	layout='spring',
	# esize=20,
	graph="glasso",
	sampleSize=nrow(mhealth_scales),
	legend.cex = 0.5,
	cut = 0.3,
	# maximum = 1, 
	minimum = 0.1,
	# esize = 20,
	# vsize = tau, 
	# repulsion = 0.8,
  # nodeNames=node_labels,
  # shape="square",
  border.width=5,
	groups=network_groups,
	# color=c("gold","steelblue","red","grey80","green"),borders = FALSE,
	labels=node_names
  #gray=T,
  )

#conducting logitic regression with variables showing direct path in the network
log_model<-data.frame(Respo,Import$Q36_1,Import$Q36_2,
	Import$Q36_3,Import$Q36_10,Import$Q36_13)#,reading_scores$scores)

#fitting the model
fit <- glm(as.factor(Respo)~Import$Q36_1+Import$Q36_2
	+Import$Q36_3+Import$Q36_10+Import$Q36_13,
	data=log_model,family=binomial)










library(RColorBrewer)
library(ggplot2)

#
with(data,table(phq9_cat,redcap_event_name))
x<-with(data,prop.table(table(phq9_cat,redcap_event_name),2))
t(x)

with(data,table(ces_score_cat,redcap_event_name))
y<-with(data,prop.table(table(ces_score_cat,redcap_event_name),2))
t(y)

with(data,table(kes_score_cat,redcap_event_name))
z<-with(data,prop.table(table(kes_score_cat,redcap_event_name),2))
t(z)

with(data,table(auditscore_cat,redcap_event_name))
a<-with(data,prop.table(table(auditscore_cat,redcap_event_name),2))
t(a)

with(data,table(fimphysical_cat,redcap_event_name))
b<-with(data,prop.table(table(fimphysical_cat,redcap_event_name),2))
t(b)

with(data,table(fim_mental_cat,redcap_event_name))
c<-with(data,prop.table(table(fim_mental_cat,redcap_event_name),2))
t(c)

with(data,table(mental_cat,redcap_event_name))
c<-with(data,prop.table(table(mental_cat,redcap_event_name),2))
t(c)

with(data,table(cage_cat,redcap_event_name))
c<-with(data,prop.table(table(cage_cat,redcap_event_name),2))
t(c)

scales <- rep(c("Depression","Depression",
				"Depression2","Depression2",
				"Stress","Stress",
				"Alcohol Use","Alcohol Use",
				"FIM Physical","FIM Physical",
				"FIM Cognitive","FIM Cognitive"),3)

times <- c(rep("Admission",12),rep("FUP 3",12),rep("FUP 6",12))

Prevalence <- rep(c("no","yes","no","yes","no","yes","no","yes",
	"no","yes","no","yes"),3)

df <- data.frame(scales,times,Prevalence)
df

df$value<-c(x[,1],y[,1],z[,1],a[,1],b[,1],c[,1],x[,2],y[,2],z[,2],a[,2],
	b[,2],c[,2],x[,3],y[,3],z[,3],a[,3],b[,3],c[,3])

#plot the stacked bar plot
tiff("/Users/jnv4/Desktop/menta_health1.tiff",
	width = 800, height = 300,compression = 'lzw')
ggplot(cor, aes(x = times)) + geom_bar(aes(weight=value, fill = Prevalence),
		 position = 'fill') + scale_y_continuous("", breaks=NULL) +
	scale_fill_manual(values=c("lightblue","darkblue")) +
	facet_grid(.~scales)+
	xlab("Follow up Times") +
	# geom_text(aes(y=,x=,label=df$value))
dev.off()

#plot the stacked bar plot with polar coordinates
ggplot(df, aes(x = project)) + geom_bar(aes(weight=numbers, fill = component), position = 'fill') + scale_y_continuous("", breaks=NA) + scale_fill_manual(values = rev(brewer.pal(6, "Purples"))) + 
coord_polar()










#### TIME STUFF
time_series<-with(data_tbi,data.frame(death,
	registry_year,gcs_tot))
# time_series<-na.omit(time_series)

time_series_severe<-subset(time_series,time_series$gcs_tot<9)
#BY month
#recoding data to decompose time series into month based time series
time_series_severe$date_month <- floor_date(time_series_severe$registry_year, 
	"year")

# # summarise crash data by month
# time_series_severe_month<-ddply(time_series_severe, "date_year", summarise, 
# 	deaths_month = table(time_series_severe$death))

table(time_series_severe$date_month,time_series_severe$death)
prop.table(table(time_series_severe$date_month,time_series_severe$death),1)
# table<-as.data.frame(prop.table(table(time_series_severe$date_month,
	# time_series_severe$death),1))
#get descriptives
# psych::describe(time_series_severe_month)

table(time_series_severe$date_month,time_series_severe$death)
prop.table(table(time_series_severe$date_month,time_series_severe$death),1)
# table<-as.data.frame(prop.table(table(time_series$date_month,
	# time_series$death),1))
#get descriptives
# psych::describe(time_series_month)

table_2<-subset(table,table$Var2==1)

#### ICU
time_series<-with(data_tbi,data.frame(death,
	registry_year,gcs_tot,surgtoicu))
# time_series<-na.omit(time_series)

time_series_severe<-subset(time_series,time_series$surgtoicu==1)
#BY month
#recoding data to decompose time series into month based time series
time_series_severe$date_month <- floor_date(time_series_severe$registry_year, 
	"year")

# # summarise crash data by month
# time_series_severe_month<-ddply(time_series_severe, "date_year", summarise, 
# 	deaths_month = table(time_series_severe$death))

table(time_series_severe$date_month,time_series_severe$death)
prop.table(table(time_series_severe$date_month,time_series_severe$death),1)
# table<-as.data.frame(prop.table(table(time_series_severe$date_month,
	# time_series_severe$death),1))
#get descriptives
# psych::describe(time_series_severe_month)

table(time_series_severe$date_month,time_series_severe$death)
prop.table(table(time_series_severe$date_month,time_series_severe$death),1)
# table<-as.data.frame(prop.table(table(time_series$date_month,
	# time_series$death),1))
#get descriptives
# psych::describe(time_series_month)

table_2<-subset(table,table$Var2==1)


