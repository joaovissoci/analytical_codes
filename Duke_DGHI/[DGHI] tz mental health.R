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
	"mvnormtest","polycor","nortest"), 
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

data <- read.csv("/Users/jnv4/Box Sync/Home Folder jnv4/Data/Global EM/Africa/Tz/MH post TBI in Tz/Tz_MHpostTBI_data.csv", header = TRUE)

data_registry<-read.csv("/Users/jnv4/Box Sync/Home Folder jnv4/Data/Global EM/Africa/tbi_registry/tz_TBIregistry_data.csv",header=TRUE)

# data_tbi <- read.csv("/Users/jnv4/Desktop/tz_tbiregistry_data.csv", header = TRUE)

#############################################################################
#MERGING REGISTRY AND MH data
#############################################################################

data_registry$tbi_reg<-data_registry$study_id

data_mhregistry<-merge(x = data, 
					   y = data_registry, 
					   by = "tbi_reg", 
					   all.x = TRUE)

write.csv(data_mhregistry,"/Users/jnv4/Box Sync/Home Folder jnv4/Data/Global EM/Africa/tbi_registry/tz_TBIregistryANDmh_data.csv")

#############################################################################
#DATA MANAGEMENT
#############################################################################

data$married<-car::recode(data$married,"0=1;1=1;4=1;2=0;3=0;5=0")
data$occupation<-car::recode(data$occupation,"89='Other'")
data$education<-car::recode(data$education,"
	1:7='Primary';8:13='Form';14:16='University';")

#
sf8_PCS<-with(data,data.frame(sf8_b1,sf8_b2,sf8_b3,sf8_b4,sf8_b5))
data$sf8_mcs<-rowSums(sf8_PCS)

sf8_MCS<-with(data,data.frame(sf8_b6,sf8_b7,sf8_b8))
data$sf8_pcs<-rowSums(sf8_MCS)

phq9<-with(data,data.frame(phq9_b11,phq9_b12,phq9_b13,phq9_b14,
	phq9_b15,phq9_b16,phq9_b17,phq9_b17,phq9_b18,phq9_b19))
data$phq9score<-rowSums(phq9)

audit<-with(data,data.frame(h1,h2,h3,h4,h5,h6,h7,h8,h9,h10))
data$auditscore<-rowSums(audit)

cage<-with(data,data.frame(h11,h12,h13,h14))
data$cagescore<-rowSums(cage)

fim_physical<-with(data,data.frame(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,
	g11,g12,g13,g14,g15,g16))

data$fim_physical<-rowSums(fim_physical)/16
fim_mental<-with(data,data.frame(g17,g18,g19,g20,g21,g22,g23,g24,g25,g26,
	g27,g28,g29,g30))

data$fim_mental<-rowSums(fim_mental)/14

mental<-with(data,data.frame(f1a,f1b,f1c,f1d,f1e,f2a,f2b,f2c,f2d,f2e,f3,
	f4,f5,f6,f7,f8,f9,f10,f11,f12___0,f12___1,f12___2))
data$mental<-rowSums(mental)

moca<-with(data,data.frame(f17,f18,f19,f20,f21,f21b,f22,f23))
data$moca<-rowSums(moca)

#classifying variables
phq9_cat<-car::recode(data$phq9score,"0:4.9='no';5:21='yes'")
ces_score_cat<-car::recode(data$ces_score,"0:15.9='no';16:45='yes'")
kes_score_cat<-car::recode(data$kes_score,"0:19.9='no';20:50='yes'")
auditscore_cat<-car::recode(data$auditscore,"0:7.9='no';8:32='yes'")
fimphysical_cat<-car::recode(data$fim_physical,"0:5.99='yes';else='no'")
fim_mental_cat<-car::recode(data$fim_mental,"0:5.99='yes';else='no'")
mental_cat<-car::recode(data$mental,"0:24='yes';25:30='no'")
cage_cat<-car::recode(data$cagescore,"0:1='no';2:4='yes'")


#Recoding clinical conditionMessage
data_tbi$registry_year<-as.Date(as.character(data_tbi$date_arrival),
  format = "%m/%d/%y")

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
table<-with(data,table(female,redcap_event_name))
table
prop.table(table,2)
#table<-with(data,table(female,))
#table
#prop.table(table,2)
#chisq.test(table)
#fisher.test(table)
#assocstats(table) #vcd package

# Marital 0=not_married 1=married
table<-with(data,table(married,redcap_event_name))
table
prop.table(table,2)
#table<-with(data,table(female,))
#table
#prop.table(table,2)
#chisq.test(table)
#fisher.test(table)
#assocstats(table) #vcd package

# Ocupation
table<-with(data,table(occupation,redcap_event_name))
table
prop.table(table,2)
#table<-with(data,table(female,))
#table
#prop.table(table,2)
#chisq.test(table)
#fisher.test(table)
#assocstats(table) #vcd package

# Education
table<-with(data,table(education,redcap_event_name))
table
prop.table(table,2)
#table<-with(data,table(female,))
#table
#prop.table(table,2)
#chisq.test(table)
#fisher.test(table)
#assocstats(table) #vcd package

# Home people residing in the house
with(data,by(age,redcap_event_name,summary))
ad.test(data$age)
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
with(data,by(personal_income,redcap_event_name,summary))
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

######## MENTAL HEALTH SCALES

#SF8

# PHQ9
with(data,by(phq9score,redcap_event_name,summary))
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
table<-with(data,table(phq9_cat))
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
table<-with(data,table(kes_score_cat))
table
prop.table(table)

# CES
with(data,by(ces_score,redcap_event_name,summary))
# 1summary(ces_score)
ad.test(ces_score)
# One Way Anova (Completely Randomized Design)
kruskal.test(data$ces_score ~ data$redcap_event_name, data=data)
posthoc.kruskal.nemenyi.test(x=data$ces_score, g=data$redcap_event_name,
 method="Chisq")
#hist(data$home_people)
#ci_func(data$home_people,.95)
#by(data$home_people,data$risk_classification,describe)
#wilcox.test(data$home_people~data$risk_classification)
table<-with(data,table(ces_score_cat))
table
prop.table(table)

# AUDIT
summary(auditscore)
ad.test(auditscore)
#hist(data$home_people)
#ci_func(data$home_people,.95)
#by(data$home_people,data$risk_classification,describe)
#wilcox.test(data$home_people~data$risk_classification)
table<-with(data,table(auditscore_cat))
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
ggplot(df, aes(x = times)) + geom_bar(aes(weight=value, fill = Prevalence),
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


