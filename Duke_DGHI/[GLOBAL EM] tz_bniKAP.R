######################################################################
#BASIC R STATISTICS TEMPLATE
######################################################################
#
#
#
#
#
######################################################################
#SETTING ENVIRONMENT
######################################################################
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
lapply(c("sem","ggplot2", "psych", "irr", "nortest", "moments",
	"GPArotation","nFactors","boot","psy", "car","vcd", "gridExtra",
	"mi","VIM","epicalc","gdata","sqldf","reshape2","mclust","foreign",
	"survival","memisc","lme4","lmerTest","dplyr","qgraph",
	"grid","lattice","latticeExtra","HH"),library, 
character.only=T)

######################################################################
#IMPORTING DATA
######################################################################
#LOADING DATA FROM A .CSV FILE
data<-read.csv("/Users/jnv4/OneDrive - Duke University/datasets/Global EM/Africa/BNI/Tz_bniKAP_data.csv",sep=",")
#information between " " are the path to the directory in your computer where the data is stored

######################################################################
#DATA MANAGEMENT
######################################################################
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

# data <- base::merge(data1,data2,by=c("nome"))


######################################################################
#BASIC DESCRIPTIVES and EXPLORATORY ANALYSIS
######################################################################
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

######################################################
#TABLE 1.
######################################################
# Gender
table<-with(data,table(female))
table
prop.table(table)
#table<-with(data_bea,table(hospitalization,risk_classification))
#table
#prop.table(table,2)
#chisq.test(table)
#fisher.test(table)
#assocstats(table) #vcd package

# Age
describe(data$age)
ad.test(data$age)
#hist(data$age)
#ci_func(data$age,.95)
# by(data$age,outcomes$rtc_involvement,describe)
# wilcox.test(data$age~outcomes$rtc_involvement)

# Occupation
table<-with(data,table(occupation))
table
prop.table(table)
#table<-with(data_bea,table(hospitalization,risk_classification))
#table
#prop.table(table,2)
#chisq.test(table)
#fisher.test(table)
#assocstats(table) #vcd package

# specialty
table<-with(data,table(specialty))
table
prop.table(table)
#table<-with(data_bea,table(hospitalization,risk_classification))
#table
#prop.table(table,2)
#chisq.test(table)
#fisher.test(table)
#assocstats(table) #vcd package

# location_grew_up
table<-with(data,table(location_grew_up))
table
prop.table(table)
#table<-with(data_bea,table(hospitalization,risk_classification))
#table
#prop.table(table,2)
#chisq.test(table)
#fisher.test(table)
#assocstats(table) #vcd package

# location_grew_up
table<-with(data,table(tanzanian))
table
prop.table(table)
#table<-with(data_bea,table(hospitalization,risk_classification))
#table
#prop.table(table,2)
#chisq.test(table)
#fisher.test(table)
#assocstats(table) #vcd package

# religious
table<-with(data,table(religious))
table
prop.table(table)
#table<-with(data_bea,table(hospitalization,risk_classification))
#table
#prop.table(table,2)
#chisq.test(table)
#fisher.test(table)
#assocstats(table) #vcd package

# religion
table<-with(data,table(religion))
table
prop.table(table)
#table<-with(data_bea,table(hospitalization,risk_classification))
#table
#prop.table(table,2)
#chisq.test(table)
#fisher.test(table)
#assocstats(table) #vcd package

# consume_alcohol
table<-with(data,table(consume_alcohol))
table
prop.table(table)
#table<-with(data_bea,table(hospitalization,risk_classification))
#table
#prop.table(table,2)
#chisq.test(table)
#fisher.test(table)
#assocstats(table) #vcd package

# worked_casualty
table<-with(data,table(worked_casualty))
table
prop.table(table)
#table<-with(data_bea,table(hospitalization,risk_classification))
#table
#prop.table(table,2)
#chisq.test(table)
#fisher.test(table)
#assocstats(table) #vcd package

######################################################################
#Figure 1.
######################################################################

# A
likert_data1<-with(data,data.frame(
	discuss_risky_alc,
	discuss_counsel_pts,
	called_harmful_drinkers,
	not_my_role,
	talking_can_be_successful))

table(likert_data1$discuss_risky_alc)
table(likert_data1$discuss_counsel_pts)
table(likert_data1$called_harmful_drinkers)
table(likert_data1$not_my_role)
table(likert_data1$talking_can_be_successful)

likert1<-c(0,1,1,16,0)
likert2<-c(1,2,3,12,1)
likert3<-c(0,0,5,2,0)
likert4<-c(12,18,13,2,13)
likert5<-c(21,12,12,2,19)

likert<-data.frame(likert1,likert2,likert3,likert4,likert5)
rownames(likert)<-c("In my schooling, we discussed at­risk\n alcohol behavior and alcohol abuse.",
					"In my schooling, we discussed counseling\n patients with at­risk drinking behaviors.",
					"Once patients suffer an injury from\n drinking they are called 'harmful drinkers",
					"It is not my role to ask\n about alcohol use",
					"Talking to patients about decreasing\n their alcohol ingestion can be successful.")

colnames(likert)<-c("Strongly Disagree",
					"Disagree",
					"I don't know",
					"Agree",
					"Strongly Agree")

likert(likert,
auto.key=list(between=1, between.columns=2),
xlab="Percentage",
# main="Knowledge about alcohol use in ED patients",
BrewerPaletteName="Blues")
# sub="Likert Scale")

#B

data$risky_drinks_sitting_men_NEW<-car::recode(
	data$risky_drinks_sitting_men,"
	0:3='Wrong';4='Right';5='Wrong'")
data$risky_drinks_sitting_women_NEW<-car::recode(
	data$risky_drinks_sitting_women,"
	0:2='Wrong';3='Right';4:5='Wrong'")
data$risky_drinks_week_men_NEW<-car::recode(
	data$risky_drinks_week_men,"
	1='Wrong';2='Right';3:5='Wrong'")
data$risky_drinks_week_women_NEW<-car::recode(
	data$risky_drinks_week_women,"
	1='Right';2:5='Wrong'")

table(data$risky_drinks_sitting_men_NEW)
table(data$risky_drinks_sitting_women_NEW)
table(data$risky_drinks_week_men_NEW)
table(data$risky_drinks_week_women_NEW)

likert1<-c(8,18,5,9)
likert2<-c(26,16,29,24)

likert<-data.frame(likert1,likert2)
rownames(likert)<-c("How many drinks per sitting is\n at­risk drinking for men?",
					"How many drinks per sitting is\n at­risk drinking for women?",
					"How many drinks per week is\n at­risk drinking for men?",
					"How many drinks per week is\n at­risk drinking for women?")

colnames(likert)<-c("Right","Wrong")

likert(likert,
auto.key=list(between=1, between.columns=2),
xlab="Percentage",
# main="Knowledge about alcohol use in ED patients",
BrewerPaletteName="Blues")
# sub="Likert Scale")

######################################################################
#Figure 2.
######################################################################

figure2_data<-with(data,data.frame(
						 pts_drink,
						 alcohol_not_problem_kcmc,
						 likely_drunk_while_injured,
						 intox_pts_frustrating,
						 few_alc_related_injuries,
						 knowledge_not_improve_care,
						 comfortable_asking_alcohol,
						 comfortable_counseling,
						 motivated_alc_screening,
						 learn_reducing_alc,
						 willing_alc_screening))


ggplot(data, aes(carat, ..count.., fill = cut)) +
  geom_density(position = "fill")


ggplot(diamonds, aes(carat, ..count.., fill = cut)) +
  geom_density(position = "fill")

######################################################################
#Figure 3.
######################################################################

qplot(subclass,data, data=feasability, fill=data, geom="bar") + 
  facet_grid(set~ . ~ class, scales="free_x", space="free") + 
xlab("") + ylab ("% of Sample Loss")+ theme(legend.position = "none") + theme_bw() +
theme(legend.position = "none",
axis.text.x  = element_text(angle=45, hjust=1.2))


######################################################################
#END
######################################################################