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
	"grid","lattice","latticeExtra","mice","likert",
	"ggpubr"),library, 
character.only=T)

######################################################################
#IMPORTING DATA
######################################################################
#LOADING DATA FROM A .CSV FILE
data<-read.csv("/Users/Joao/Box Sync/Home Folder jnv4/Data/Global EM/Africa/Tz/BNI/Tz_bniKAPprofessionals_data.csv",sep=",")
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
#Table 2.
######################################################################

# # drinking per day / men
# describe(data$risky_drinks_sitting_men)
# ad.test(data$risky_drinks_sitting_men)
# #hist(data$age)
# #ci_func(data$age,.95)
# # by(data$age,outcomes$rtc_involvement,describe)
# # wilcox.test(data$age~outcomes$rtc_involvement)

# # drinking per day / women
# describe(data$risky_drinks_sitting_women)
# ad.test(data$risky_drinks_sitting_women)
# #hist(data$age)
# #ci_func(data$age,.95)
# # by(data$age,outcomes$rtc_involvement,describe)
# # wilcox.test(data$age~outcomes$rtc_involvement)


# drinking per week / women
table<-with(data,table(risky_drinks_sitting_men))
table
prop.table(table)
#table<-with(data_bea,table(hospitalization,risk_classification))
#table
#prop.table(table,2)
#chisq.test(table)
#fisher.test(table)
#assocstats(table) #vcd package


# drinking per week / women
table<-with(data,table(risky_drinks_sitting_women))
table
prop.table(table)
#table<-with(data_bea,table(hospitalization,risk_classification))
#table
#prop.table(table,2)
#chisq.test(table)
#fisher.test(table)
#assocstats(table) #vcd package

# drinking per week / women
table<-with(data,table(risky_drinks_week_men))
table
prop.table(table)
#table<-with(data_bea,table(hospitalization,risk_classification))
#table
#prop.table(table,2)
#chisq.test(table)
#fisher.test(table)
#assocstats(table) #vcd package

# drinking per week / women
table<-with(data,table(risky_drinks_week_women))
table
prop.table(table)
2#table<-with(data_bea,table(hospitalization,risk_classification))
#table
#prop.table(table,2)
#chisq.test(table)
#fisher.test(table)
#assocstats(table) #vcd package

######################################################################
#Figure 1.
######################################################################

#FIRST SUBMISSION FIGURE
# likert_data1<-with(data,data.frame(
# 	discuss_risky_alc,
# 	discuss_counsel_pts,
# 	called_harmful_drinkers,
# 	not_my_role,
# 	talking_can_be_successful))

# likert_data1$talking_can_be_successful

# table(likert_data1$discuss_risky_alc)
# table(likert_data1$discuss_counsel_pts)
# table(likert_data1$called_harmful_drinkers)
# table(likert_data1$not_my_role)
# table(likert_data1$talking_can_be_successful)

# likert1<-c(0,1,1,2,0)
# likert2<-c(1,2,3,2,1)
# likert3<-c(0,0,5,2,0)
# likert4<-c(12,18,13,12,13)
# likert5<-c(21,12,12,16,19)

# likert<-data.frame(likert1,likert2,likert3,likert4,likert5)
# likert$var<-c("q1","q2","q3","q4","q5")
# rownames(likert)<-c("In my schooling, we discussed at­risk\n alcohol behavior and alcohol abuse.",
# 					"In my schooling, we discussed counseling\n patients with at­risk drinking behaviors.",
# 					"Once patients suffer an injury from\n drinking they are called 'harmful drinkers",
# 					"It is my role to ask\n about alcohol use",
# 					"Talking to patients about decreasing\n their alcohol ingestion can be successful.")

# colnames(likert)<-c("Strongly Disagree",
# 					"Disagree",
# 					"I don't know",
# 					"Agree",
# 					"Strongly Agree")

# # likert(likert,
# # auto.key=list(between=1, between.columns=2),
# # xlab="Percentage",
# # # main="Knowledge about alcohol use in ED patients",
# # BrewerPaletteName="Blues")
# # # sub="Likert Scale")

# HH::likert(likert, main="",
# 			as.percent=TRUE, rightAxisLabels=NULL, 
# 			# ylab.right="Perceptions",
#             positive.order=TRUE,
#             scales=list(x=list(limits=c(-100,100),
#             	at=c(-100,-50,0,50,100))))
#             # scales=list(x=list(at=seq(0,50,100))))
#                           # labels=as.vector(rbind("",seq(0,50,100))))))

# require(vcd)
# data(ProfChal)
# likertMosaic(Question ~ . | Subtable, ProfChal,
# main="Is your job professionally challenging?")

# likertMosaic(Question ~ . | Subtable, ProfChal,
# main="Is your job professionally challenging?", as.percent=TRUE)

# likertMosaic(Question ~ . | Subtable, ProfChal,
# main="Is your job professionally challenging?", as.percent=TRUE,
# variable.width=TRUE)
# #B

# data$risky_drinks_sitting_men_NEW<-car::recode(
# 	data$risky_drinks_sitting_men,"
# 	0:3='Wrong';4='Right';5='Wrong'")
# data$risky_drinks_sitting_women_NEW<-car::recode(
# 	data$risky_drinks_sitting_women,"
# 	0:2='Wrong';3='Right';4:5='Wrong'")
# data$risky_drinks_week_men_NEW<-car::recode(
# 	data$risky_drinks_week_men,"
# 	1='Wrong';2='Right';3:5='Wrong'")
# data$risky_drinks_week_women_NEW<-car::recode(
# 	data$risky_drinks_week_women,"
# 	1='Right';2:5='Wrong'")

# table(data$risky_drinks_sitting_men_NEW)
# table(data$risky_drinks_sitting_women_NEW)
# table(data$risky_drinks_week_men_NEW)
# table(data$risky_drinks_week_women_NEW)

# likert1<-c(8,18,5,9)
# likert2<-c(26,16,29,24)

# likert<-data.frame(likert2,likert1)
# rownames(likert)<-c("How many drinks per sitting is\n at­risk drinking for men?",
# 					"How many drinks per sitting is\n at­risk drinking for women?",
# 					"How many drinks per week is\n at­risk drinking for men?",
# 					"How many drinks per week is\n at­risk drinking for women?")

# colnames(likert)<-c("Wrong","Right")

# likert(likert,
# auto.key=list(between=1, between.columns=2),
# xlab="Percentage",
# # main="Knowledge about alcohol use in ED patients",
# BrewerPaletteName="Blues")
# # sub="Likert Scale")

# HH::likert(likert, main="",
# 			as.percent=TRUE, 
# 			rightAxisLabels=NULL, 
# 			# ylab.right="Perceptions",
#             positive.order=TRUE,
#             scales=list(x=list(limits=c(-100,100),
#             at=c(-100,-50,0,50,100))),
#             col=c("#D33F6A","#4A6FE3"))

# #SECOND SUBMISSION FIGURE

#KNOWLEDGE
likert_data1<-with(data,data.frame(
	discuss_risky_alc,
	discuss_counsel_pts,
	called_harmful_drinkers,
	not_my_role,
	talking_can_be_successful))

likert_data1[,c(1:3,5)][likert_data1[,c(1:3,5)]==1]<-"Strongly Disagree"
likert_data1[,c(1:3,5)][likert_data1[,c(1:3,5)]==2]<-"Disagree"
likert_data1[,c(1:3,5)][likert_data1[,c(1:3,5)]==3]<-"I don't know"
likert_data1[,c(1:3,5)][likert_data1[,c(1:3,5)]==4]<-"Agree"
likert_data1[,c(1:3,5)][likert_data1[,c(1:3,5)]==5]<-"Strongly Agree"

likert_data1[,4][likert_data1[,4]==1]<-"Strongly Agree"
likert_data1[,4][likert_data1[,4]==2]<-"Agree"
likert_data1[,4][likert_data1[,4]==3]<-"I don't know"
likert_data1[,4][likert_data1[,4]==4]<-"Disagree"
likert_data1[,4][likert_data1[,4]==5]<-"Strongly Disagree"

mylevels <- c("Strongly Disagree", "Disagree", "I don't know", "Agree", "Strongly Agree")

for(i in seq_along(likert_data1)) {
	likert_data1[,i] <- factor(likert_data1[,i], levels=mylevels)
}

knowledge_data<-likert(na.omit(likert_data1))

# knowledge_data$Group<-c("Knowledge")
summary(knowledge_data)
knowledge_plot<-plot(knowledge_data,
					 colors=c("#D33F6A",
					 		  "#E07B91",
					 		  "lightgrey",
					 		  "#8595E1",
					 		  "#4A6FE3"))

knowledge_plot<- knowledge_plot + scale_x_discrete(breaks=c(
							  "talking_can_be_successful",
    						  "discuss_risky_alc",
    						  "discuss_counsel_pts",
    						  "not_my_role",
    					      "called_harmful_drinkers"),
													labels=c(
							  "Talking to patients about decreasing\n their alcohol ingestion can be successful.",
							  "In my schooling, we discussed at­-risk\n alcohol behavior and alcohol abuse.",
							  "In my schooling, we discussed counseling\n patients with at­risk drinking behaviors.",
							  "It is my role to ask\n about alcohol use",
							  "Once patients suffer an injury from\n drinking they are called 'harmful drinkers"  							
							  )) +
				theme_bw() +
				theme(legend.position="bottom")

summary_data<-summary(knowledge_data)

mean_plot<-ggplot(summary_data, aes(y=mean,x=Item)) + 
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) +
    geom_line() +
    geom_point() +
    coord_flip() +
    xlab("") +
    ylab("Mean/Standard Deviation") +
    scale_x_discrete(limits=rev(c("talking_can_be_successful",
    							  "discuss_risky_alc",
    							  "discuss_counsel_pts",
    							  "not_my_role",
    							  "called_harmful_drinkers"))) +
    expand_limits(y=c(1:5)) +
    theme_bw() +
    theme(panel.background = element_rect(colour = 'grey'))

# require(ggpubr)

knowledge_grid<-ggarrange(knowledge_plot, #+ rremove("y.text"), 
		  mean_plot + rremove("y.text"), 
          labels = c("A", "B"),
          # heights = c(2, 0.7),
          ncol = 2,
          widths = c(1, 0.3),
          align = c("h"),
          # common.legend = TRUE,
          legend = "none")

ggsave("figure1.eps", #change .eps to .pdf for different format
		knowledge_grid, #plot is the name of the fig, but the function assumes the last plot if argument is NULL
		path="/Users/joaovissoci/Desktop", #path to save the plot
		width = 10, 
		height = 2.5, 
		device=cairo_ps) #cairo_ps is a d
######################################################################
#Figure 2.
######################################################################

# figure2_data<-with(data,data.frame(
# 						 pts_drink,
# 						 alcohol_not_problem_kcmc,
# 						 likely_drunk_while_injured,
# 						 intox_pts_frustrating,
# 						 few_alc_related_injuries,
# 						 knowledge_not_improve_care,
# 						 comfortable_asking_alcohol,
# 						 comfortable_counseling,
# 						 motivated_alc_screening,
# 						 learn_reducing_alc,
# 						 willing_alc_screening))

# x2<-na.omit(melt(figure2_data))

# count_data<-plyr::count(x2, c("variable", "value"))

# # write.csv(count_data,"/Users/joaovissoci/blah.csv")

# # dat<-read.csv("/Users/joaovissoci/blah.csv")
# colnames(count_data)<-c("var","likert","value")
# dat2<-cast(count_data,var~likert)
# colnames(dat2)<-c("var","likert1","likert2","likert3",
# 	"likert4","likert5")
# # dat3<-rbind(dat2,likert)
# # rownames(dat3)<-dat3$var
# # dat3$gr<-as.factor(c(rep("test1",11),rep("test2",5)))

# NAto0<-function(x){
# 	car::recode(x,"NA=0")
# 	}

# dat_2_2<-sapply(dat2,NAto0)

# rownames(dat_2_2)<-c("A large number of patients drink alcohol.",
# 				"Alcohol use and abuse is not a problem \n amongs our patient population at KCMC.",
# 				"Injury pateints at KCMC were likely drinking\n when they were injured.",
# 			  	"Caring for patients who are intoxicated is\n frustrating as they caused themselves\n to be ill/injured.",
# 			  	"Few injury patients at KCMC suffer from\n alcohol related injuries.",
# 			  	"Knowing if patients have atrisk drinking\n does NOT improve care I can provide.",
# 			  	"I feel comfortable asking patients about\n their alcohol use behavior.",
# 			  	"I feel comfortable counseling patients about\n their atrisk drinking.",
# 			  	"How motivated are you and your colleagues at\n implementing alcohol screening\n and testing?",
# 			  	"How willing are you and your colleagues to\n learn about reducing harmful alcohol\n use among injury patients?",
# 			  	"How willing are you and your colleagues to\n implement alcohol screening among\n injury patients?")

# colnames(dat_2_2)<-c("var","Strongly Disagree",
# 					"Disagree",
# 					"I don't know",
# 					"Agree",
# 					"Strongly Agree")



# HH::likert(dat_2_2[,-1], main="",
# 			as.percent=TRUE, rightAxisLabels=NULL, 
# 			# ylab.right="Perceptions",
#             positive.order=TRUE,
#             scales=list(x=list(limits=c(-100,100),
#             at=c(-100,-50,0,50,100))))

#SECOND SUBMISSION
#PERCEPTIONS
likert_data2<-with(data,data.frame(
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


likert_data2[likert_data2==1]<-"Strongly Disagree"
likert_data2[likert_data2==2]<-"Disagree"
likert_data2[likert_data2==3]<-"I don't know"
likert_data2[likert_data2==4]<-"Agree"
likert_data2[likert_data2==5]<-"Strongly Agree"

# likert_data2[,4][likert_data2[,4]==1]<-"Strongly Agree"
# likert_data2[,4][likert_data2[,4]==2]<-"Agree"
# likert_data2[,4][likert_data2[,4]==3]<-"I don't know"
# likert_data2[,4][likert_data2[,4]==4]<-"Disagree"
# likert_data2[,4][likert_data2[,4]==5]<-"Strongly Disagree"

# colnames(likert_data2)<-c("A large number of patients drink alcohol.",
# 				"Alcohol use and abuse is not a problem \n amongs our patient population at KCMC.",
# 				"Injury pateints at KCMC were likely drinking\n when they were injured.",
# 			  	"Caring for patients who are intoxicated is\n frustrating as they caused themselves\n to be ill/injured.",
# 			  	"Few injury patients at KCMC suffer from\n alcohol related injuries.",
# 			  	"Knowing if patients have atrisk drinking\n does NOT improve care I can provide.",
# 			  	"I feel comfortable asking patients about\n their alcohol use behavior.",
# 			  	"I feel comfortable counseling patients about\n their atrisk drinking.",
# 			  	"How motivated are you and your colleagues at\n implementing alcohol screening\n and testing?",
# 			  	"How willing are you and your colleagues to\n learn about reducing harmful alcohol\n use among injury patients?",
# 			  	"How willing are you and your colleagues to\n implement alcohol screening among\n injury patients?")


mylevels <- c("Strongly Disagree", "Disagree", "I don't know", "Agree", "Strongly Agree")

for(i in seq_along(likert_data2)) {
	likert_data2[,i] <- factor(likert_data2[,i], levels=mylevels)
}

perception_data<-likert(na.omit(likert_data2))
# perception_data$results$Group<-as.factor(c("Perception"))
# perception_data$grouping<-as.factor(rep("Perception",34))
perception_plot<-plot(perception_data,
					 colors=c("#D33F6A",
					 		  "#E07B91",
					 		  "lightgrey",
					 		  "#8595E1",
					 		  "#4A6FE3"))

perception_plot<- perception_plot + scale_x_discrete(breaks=c(
							  "comfortable_counseling",
    						  "likely_drunk_while_injured",
    						  "willing_alc_screening",
    						  "comfortable_asking_alcohol",
    						  "learn_reducing_alc",
    						  "motivated_alc_screening",
    						  "pts_drink",
    						  "intox_pts_frustrating",
    						  "few_alc_related_injuries",
    						  "knowledge_not_improve_care",
    						  "alcohol_not_problem_kcmc"),
													labels=c(
							  "I feel comfortable counseling patients about\n their atrisk drinking.",
							  "Injury pateints at KCMC were likely drinking\n when they were injured.",
							  "How willing are you and your colleagues to\n implement alcohol screening among\n injury patients?",
							  "I feel comfortable asking patients about\n their alcohol use behavior.",
							  "How willing are you and your colleagues to\n learn about reducing harmful alcohol\n use among injury patients?",
							  "How motivated are you and your colleagues at\n implementing alcohol screening\n and testing?",
							  "A large number of patients drink alcohol.",
							  "Caring for patients who are intoxicated is\n frustrating as they caused themselves\n to be ill/injured.",
							  "Few injury patients at KCMC suffer from\n alcohol related injuries.",
							  "Knowing if patients have atrisk drinking\n does NOT improve care I can provide.",
							  "Alcohol use and abuse is not a problem \n amongs our patient population at KCMC."))+
				theme_bw() +
				theme(legend.position="bottom")

summary_data<-summary(perception_data)

mean_plot<-ggplot(summary_data, aes(y=mean,x=Item)) + 
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) +
    geom_line() +
    geom_point() +
    coord_flip() +
    xlab("") +
    ylab("Mean/Standard Deviation") +
    scale_x_discrete(limits=rev(c("comfortable_counseling",
    						  "likely_drunk_while_injured",
    						  "willing_alc_screening",
    						  "comfortable_asking_alcohol",
    						  "learn_reducing_alc",
    						  "motivated_alc_screening",
    						  "pts_drink",
    						  "intox_pts_frustrating",
    						  "few_alc_related_injuries",
    						  "knowledge_not_improve_care",
    						  "alcohol_not_problem_kcmc"))) +

    theme_bw()

# mean_table_data<-with(summary_data,data.frame(Item=c(
# 							  "I feel comfortable counseling patients about\n their atrisk drinking.",
# 							  "Injury pateints at KCMC were likely drinking\n when they were injured.",
# 							  "How willing are you and your colleagues to\n implement alcohol screening among\n injury patients?",
# 							  "I feel comfortable asking patients about\n their alcohol use behavior.",
# 							  "How willing are you and your colleagues to\n learn about reducing harmful alcohol\n use among injury patients?",
# 							  "How motivated are you and your colleagues at\n implementing alcohol screening\n and testing?",
# 							  "A large number of patients drink alcohol.",
# 							  "Caring for patients who are intoxicated is\n frustrating as they caused themselves\n to be ill/injured.",
# 							  "Few injury patients at KCMC suffer from\n alcohol related injuries.",
# 							  "Knowing if patients have atrisk drinking\n does NOT improve care I can provide.",
# 							  "Alcohol use and abuse is not a problem \n amongs our patient population at KCMC."),
# 											  Mean=round(mean,1),
# 											  SD=round(sd,1)))
# mean_table<-ggtexttable(mean_table_data, rows = NULL)

# require(ggpubr)
perception_grid<-ggarrange(perception_plot, #+ rremove("y.text"), 
		  mean_plot + rremove("y.text"), 
		  # mean_table,
          labels = c("A", "B"),
          # heights = c(2, 4),
          ncol = 2,
          widths = c(1, 0.3),
          align = c("h"),
          common.legend = TRUE,
          legend = "bottom")

ggsave("figure2.eps", #change .eps to .pdf for different format
		perception_grid, #plot is the name of the fig, but the function assumes the last plot if argument is NULL
		path="/Users/joaovissoci/Desktop", #path to save the plot
		width = 10, 
		height = 5.3, 
		device=cairo_ps) #cairo_ps is a d

######################################################################
#Figure 3.
######################################################################

#FIRST SUBMISSION
# figure4_data<-with(data,data.frame(
# 						 common_ask_pts_drink,
# 						 common_test_pts_alc,
# 						 common_ask_pts_tobacco,
# 						 resources_refer_pts,
# 						 ask_pts_alc,
# 						 test_alc_breath_or_serum,
# 						 counsel_patients
# 						 ))

# x2<-na.omit(melt(figure4_data))

# count_data<-plyr::count(x2, c("variable", "value"))

# # write.csv(count_data,"/Users/joaovissoci/blah2.csv")

# # dat<-read.csv("/Users/joaovissoci/blah2.csv")
# colnames(count_data)<-c("var","likert","value")
# dat2<-cast(count_data,var~likert)

# NAto0<-function(x){
# 	car::recode(x,"NA=0")
# 	}

# dat_2_2<-sapply(dat2,NAto0)


# colnames(dat_2_2)<-c("var","likert1","likert2","likert3",
# 	"likert4","likert5")
# # dat3<-rbind(dat2,likert)
# # rownames(dat3)<-dat3$var
# # dat3$gr<-as.factor(c(rep("test1",11),rep("test2",5)))
# rownames(dat_2_2)<-c(
# "It is common to ask patients about their\n drinking behavior.",
# "It is common to test patients for alcohol.",
# "It is comon to ask patients about their\n tobacco use behavior.",
# "There are resources to refer patients to\n when I determine they have high risk drinking.",
# "I ask my patients about their alcohol use",
# "I test injured patients for alcohol in their\n breath or serum",
# "I counsel patients to reduce their drinking\n if I think they have harmful drinking behavior.")

# colnames(dat_2_2)<-c("var","Strongly Disagree",
# 					"Disagree",
# 					"I don't know",
# 					"Agree",
# 					"Strongly Agree")

# HH::likert(dat_2_2[,-1], main="",
# 			as.percent=TRUE, rightAxisLabels=NULL, 
# 			# ylab.right="Perceptions",
#             positive.order=TRUE,
#             scales=list(x=list(limits=c(-100,100),
#             at=c(-100,-50,0,50,100))))

#SECOND SUBMISSION
#ATTITUDES
likert_data3<-with(data,data.frame(
						 common_ask_pts_drink,
						 common_test_pts_alc,
						 common_ask_pts_tobacco,
						 resources_refer_pts,
						 ask_pts_alc,
						 test_alc_breath_or_serum,
						 counsel_patients
						 ))

likert_data3[likert_data3==1]<-"Strongly Disagree"
likert_data3[likert_data3==2]<-"Disagree"
likert_data3[likert_data3==3]<-"I don't know"
likert_data3[likert_data3==4]<-"Agree"
likert_data3[likert_data3==5]<-"Strongly Agree"

# likert_data3[,4][likert_data3[,4]==1]<-"Strongly Agree"
# likert_data3[,4][likert_data3[,4]==2]<-"Agree"
# likert_data3[,4][likert_data3[,4]==3]<-"I don't know"
# likert_data3[,4][likert_data3[,4]==4]<-"Disagree"
# likert_data3[,4][likert_data3[,4]==5]<-"Strongly Disagree"

mylevels <- c("Strongly Disagree", "Disagree", "I don't know", "Agree", "Strongly Agree")

for(i in seq_along(likert_data3)) {
	likert_data3[,i] <- factor(likert_data3[,i], levels=mylevels)
}

attitude_data<-likert(na.omit(likert_data3))
# attitude_data$results$Group<-as.factor(c("attitude"))
# attitude_data$grouping<-as.factor(rep("attitude",34))
attitude_plot<-plot(attitude_data,
					 colors=c("#D33F6A",
					 		  "#E07B91",
					 		  "lightgrey",
					 		  "#8595E1",
					 		  "#4A6FE3"))

attitude_plot<- attitude_plot + scale_x_discrete(breaks=c(
					     "common_ask_pts_drink",
					     "counsel_patients",
					     "ask_pts_alc",
					     "common_ask_pts_tobacco",
					     "test_alc_breath_or_serum",
						 "common_test_pts_alc",
						 "resources_refer_pts"),
												labels=c(
						 "It is common to ask patients about their\n drinking behavior.",
						 "I counsel patients to reduce their drinking\n if I think they have harmful drinking behavior.",
						 "I ask my patients about their alcohol use",
						 "It is comon to ask patients about their\n tobacco use behavior.",
						 "I test injured patients for alcohol in their\n breath or serum",
						 "It is common to test patients for alcohol.",
						 "There are resources to refer patients to\n when I determine they have high risk drinking.")
						  )+
				theme_bw() +
				theme(legend.position="bottom")

summary_data<-summary(attitude_data)

mean_plot<-ggplot(summary_data, aes(y=mean,x=Item)) + 
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) +
    geom_line() +
    geom_point() +
    coord_flip() +
    xlab("") +
    ylab("Mean/Standard Deviation") +
    scale_x_discrete(limits=rev(c("common_ask_pts_drink",
					     "counsel_patients",
					     "ask_pts_alc",
					     "common_ask_pts_tobacco",
					     "test_alc_breath_or_serum",
						 "common_test_pts_alc",
						 "resources_refer_pts"))) +

    theme_bw()

# require(ggpubr)
attitude_grid<-ggarrange(attitude_plot, #+ rremove("y.text"), 
		  mean_plot + rremove("y.text"), 
		  # mean_table,
          labels = c("A", "B"),
          # heights = c(2, 4),
          ncol = 2,
          widths = c(1, 0.3),
          align = c("h"),
          common.legend = TRUE,
          legend = "bottom")

ggsave("figure3.eps", #change .eps to .pdf for different format
		attitude_grid, #plot is the name of the fig, but the function assumes the last plot if argument is NULL
		path="/Users/joaovissoci/Desktop", #path to save the plot
		width = 10, 
		height = 3.8, 
		device=cairo_ps) #cairo_ps is a d
######################################################################
#Figure 4.
######################################################################
score_data<-with(data, data.frame(alcoholic_close_friend,
							recovered_alcoholic_teacher,
							recover_alcoholic_chldrn,
							recover_alcoholic_hired,
							non_alcoholic_hired,
							recovered_alc_treat_same,
							not_date_hospital_for_alc,
							alc_treatment_intelligent,
							alcoholic_trustworthy,
							alc_treatment_failure,
							think_less_treated_person,
							less_opinion_trtd_person))


#recoding positive oriented items to ensure a higher score indicates high stigma
score_data$alcoholic_close_friend<-car::recode(data$alcoholic_close_friend,
	"1=6;2=5;3=4;4=3;5=2;6=1")
score_data$alc_treatment_intelligent<-car::recode(data$alc_treatment_intelligent,
	"1=6;2=5;3=4;4=3;5=2;6=1")
score_data$alcoholic_trustworthy<-car::recode(data$alcoholic_trustworthy,
	"1=6;2=5;3=4;4=3;5=2;6=1")
score_data$recovered_alcoholic_teacher<-car::recode(data$recovered_alcoholic_teacher,
	"1=6;2=5;3=4;4=3;5=2;6=1")
score_data$recover_alcoholic_hired<-car::recode(data$recover_alcoholic_hired,
	"1=6;2=5;3=4;4=3;5=2;6=1")
score_data$recovered_alc_treat_same<-car::recode(data$recovered_alc_treat_same,
	"1=6;2=5;3=4;4=3;5=2;6=1")

#Calculate PAS
figure3_data_PAS<-with(score_data,data.frame(alcoholic_close_friend,
							recovered_alcoholic_teacher,
							recover_alcoholic_chldrn,
							recover_alcoholic_hired,
							non_alcoholic_hired,
							recovered_alc_treat_same,
							not_date_hospital_for_alc,
							alc_treatment_intelligent,
							alcoholic_trustworthy,
							alc_treatment_failure,
							think_less_treated_person,
							less_opinion_trtd_person))

# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
imp <- mice(figure3_data_PAS, seed = 2222, m=5)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
figure3_data_PAS<-complete(imp,4)

pas_score<-rowSums(figure3_data_PAS)/ncol(figure3_data_PAS)
summary(pas_score)
describe(pas_score)
# discrimination<-na.omit(discrimination)
# rescale <- function(x)(x-min(x))/(max(x) - min(x)) * 100
# # discrimination_scaled<-lapply(discrimination,rescale)
# x<-rescale(pas_score)
# summary(x)
pas_score_cat<-car::recode(pas_score,"0:3='low';else='high'")
table(pas_score_cat)
prop.table(table(pas_score_cat))

#Calculate PDiscrimination score
figure3_data_PDis<-with(score_data,data.frame(alcoholic_close_friend,
							recovered_alcoholic_teacher,
							recover_alcoholic_chldrn,
							recover_alcoholic_hired,
							non_alcoholic_hired,
							recovered_alc_treat_same,
							not_date_hospital_for_alc))

# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
imp <- mice(figure3_data_PDis, seed = 2222, m=5)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
figure3_data_PDis<-complete(imp,4)

discrimination<-rowSums(figure3_data_PDis)/ncol(figure3_data_PDis)
# discrimination<-na.omit(discrimination)
# rescale <- function(x)(x-min(x))/(max(x) - min(x)) * 100
# discrimination_scaled<-lapply(discrimination,rescale)
# x<-rescale(discrimination)
summary(discrimination)
describe(discrimination)
discrimination_cat<-car::recode(discrimination,"0:3='low';else='high'")
table(discrimination_cat)
prop.table(table(discrimination_cat))

#Calculate Perceived Devaluation score
figure3_data_PDev<-with(score_data,data.frame(alc_treatment_intelligent,
							alcoholic_trustworthy,
							alc_treatment_failure,
							think_less_treated_person,
							less_opinion_trtd_person))

# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
imp <- mice(figure3_data_PDev, seed = 2222, m=5)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
figure3_data_PDev<-complete(imp,4)

devaluation<-rowSums(figure3_data_PDev)/ncol(figure3_data_PDev)
# devaluation<-na.omit(devaluation)
# rescale <- function(x)(x-min(x))/(max(x) - min(x)) * 100
# devaluation_scaled<-lapply(devaluation,rescale)
# z<-rescale(devaluation)
summary(devaluation)
describe(devaluation)
devaluation_cat<-car::recode(devaluation,"0:3='low';else='high'")
table(devaluation_cat)
prop.table(table(devaluation_cat))

#Building graph for each item
figure3_data1<-with(data, data.frame(alcoholic_close_friend,
							recovered_alcoholic_teacher,
							recover_alcoholic_chldrn,
							recover_alcoholic_hired,
							non_alcoholic_hired,
							recovered_alc_treat_same,
							not_date_hospital_for_alc,
							alc_treatment_intelligent,
							alcoholic_trustworthy,
							alc_treatment_failure,
							think_less_treated_person,
							less_opinion_trtd_person))

# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
imp <- mice(figure3_data1, seed = 2222, m=5)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
figure3_data<-complete(imp,4)

graph<-melt(figure3_data)
count_data_fig3<-plyr::count(graph, c("variable", "value"))
count_data_fig3<-na.omit(count_data_fig3)
count_data_fig3$value<-car::recode(count_data_fig3$value,"
	1='Strongly disagree';
	2='Disagree';
	3='Somewhat disagree';
	4='Somewhat agree';
	5='Agree';
	6='Strongly agree'")
count_data_fig3$feq_2<-(count_data_fig3$freq*100)/35
count_data_fig3$feq_2<-round(count_data_fig3$feq_2,digits=1)

#Adding value of zero to likert options not chosen
variable_add<-c("think_less_treated_person","alcoholic_trustworthy",
	"recover_alcoholic_hired")
value_add<-c("Somewhat disagree","Disagree","Somewhat agree")
freq_2_add<-c(0.0,0.0,0.0)
freq_add<-c(0,0,0)
add<-data.frame(variable=variable_add,
	value=value_add,
	freq=freq_add,
	feq_2=freq_2_add)

plot_data<-rbind(count_data_fig3,add)

#Adding Mean and Standard Deviations
meanttoadd_labels<-c("alcoholic_close_friend",
							"recovered_alcoholic_teacher",
							"recover_alcoholic_chldrn",
							"recover_alcoholic_hired",
							"non_alcoholic_hired",
							"recovered_alc_treat_same",
							"not_date_hospital_for_alc",
							"alc_treatment_intelligent",
							"alcoholic_trustworthy",
							"alc_treatment_failure",
							"think_less_treated_person",
							"less_opinion_trtd_person")

meanttoadd_variable<-rep("Mean (SD)",12)

descriptives_temp<-describeBy(graph$value,graph$variable)

meanttoadd_means<-round(c(descriptives_temp[[1]]$mean,
				    descriptives_temp[[2]]$mean,
				    descriptives_temp[[3]]$mean,
				    descriptives_temp[[4]]$mean,
				    descriptives_temp[[5]]$mean,
				    descriptives_temp[[6]]$mean,
				    descriptives_temp[[7]]$mean,
				    descriptives_temp[[8]]$mean,
				    descriptives_temp[[9]]$mean,
				    descriptives_temp[[10]]$mean,
				    descriptives_temp[[11]]$mean,
				    descriptives_temp[[12]]$mean),2)

meanttoadd_sd<-round(c(descriptives_temp[[1]]$sd,
				    descriptives_temp[[2]]$sd,
				    descriptives_temp[[3]]$sd,
				    descriptives_temp[[4]]$sd,
				    descriptives_temp[[5]]$sd,
				    descriptives_temp[[6]]$sd,
				    descriptives_temp[[7]]$sd,
				    descriptives_temp[[8]]$sd,
				    descriptives_temp[[9]]$sd,
				    descriptives_temp[[10]]$sd,
				    descriptives_temp[[11]]$sd,
				    descriptives_temp[[12]]$sd),2)

meanttoadd_values<-paste0(meanttoadd_means, " ","\n", "(", meanttoadd_sd,")")


meantoadd_data<-data.frame(variable=meanttoadd_labels,
						   value=meanttoadd_variable,
						   freq=c(0),
						   feq_2=c(0))

plot_data<-rbind(count_data_fig3,add,meantoadd_data)

plot_data$text<-c(rep(NA,72),meanttoadd_values)
plot_data$tile<-c(rep(NA,72),rep("white",12))

# plot_data$color<-NULL
# plot_data$color[plot_data$feq_2 >= 0 & plot_data$feq_2 < 5.883]="lightcyan1"
# plot_data$color[plot_data$feq_2 >= 5.883 & plot_data$feq_2 < 11.76]="lightcyan2"
# plot_data$color[plot_data$feq_2 >= 11.76 & plot_data$feq_2 < 26.47]="lightcyan3"
# plot_data$color[plot_data$feq_2 >= 26.47]="lightcyan4"

#find colors numbers: diverge_hcl(7, c = 100, l = c(50, 90), power = 1)
#from: https://cran.r-project.org/web/packages/colorspace/vignettes/hcl-colors.pdf

plot_data$flip<-NULL
plot_data$flip[plot_data$variable == "recovered_alcoholic_teacher"]="notflip"
plot_data$flip[plot_data$variable == "alcoholic_close_friend"]="notflip"
plot_data$flip[plot_data$variable == "alc_treatment_intelligent"]="notflip"
plot_data$flip[plot_data$variable == "alcoholic_trustworthy"]="notflip"
plot_data$flip[plot_data$variable == "recover_alcoholic_hired"]="notflip"
plot_data$flip[plot_data$variable == "recovered_alc_treat_same"]="notflip"

plot_data$flip[plot_data$variable == "alc_treatment_failure"]="flip"
plot_data$flip[plot_data$variable == "recover_alcoholic_chldrn"]="flip"
plot_data$flip[plot_data$variable == "non_alcoholic_hired"]="flip"
plot_data$flip[plot_data$variable == "not_date_hospital_for_alc"]="flip"
plot_data$flip[plot_data$variable == "think_less_treated_person"]="flip"
plot_data$flip[plot_data$variable == "less_opinion_trtd_person"]="flip"


plot_data$color<-NULL
plot_data$color[plot_data$flip == "notflip" & plot_data$value == "Strongly agree"]="#4A6FE3"
plot_data$color[plot_data$flip == "notflip" & plot_data$value == "Agree"]="#8595E1"
plot_data$color[plot_data$flip == "notflip" & plot_data$value == "Somewhat agree"]="#B5BBE3"

plot_data$color[plot_data$flip == "notflip" & plot_data$value == "Somewhat disagree"]="#E6AFB9"
plot_data$color[plot_data$flip == "notflip" & plot_data$value == "Disagree"]="#E07B91"
plot_data$color[plot_data$flip == "notflip" & plot_data$value == "Strongly disagree"]="#D33F6A"

plot_data$color[plot_data$flip == "flip" & plot_data$value == "Strongly disagree"]="#4A6FE3"
plot_data$color[plot_data$flip == "flip" & plot_data$value == "Disagree"]="#8595E1"
plot_data$color[plot_data$flip == "flip" & plot_data$value == "Somewhat disagree"]="#B5BBE3"

plot_data$color[plot_data$flip == "flip" & plot_data$value == "Somewhat agree"]="#E6AFB9"
plot_data$color[plot_data$flip == "flip" & plot_data$value == "Agree"]="#E07B91"
plot_data$color[plot_data$flip == "flip" & plot_data$value == "Strongly agree"]="#D33F6A"

plot_data$color[plot_data$value == "Mean (SD)"]="white"

# write.csv(plot_data,"/Users/joaovissoci/plot_data.csv")

# avseq <- ggplot(plot_data, aes(y=variable, x=value)) + 
#   geom_tile(fill=plot_data$color) + 
#   geom_text(aes(y=variable, x=value, label=feq_2),size=7) + 
#   theme_minimal() + 
#   xlab(label="") + 
#   ylab(label="Stigma Scale") + 
#   scale_x_discrete(limits = c("Strongly disagree",
#   							  "Disagree",
#   							  "Somewhat disagree",
#   							  "Somewhat agree",
#   							  "Agree",
#   							  "Strongly agree")) + 
#   scale_y_discrete(limits = rev(levels(plot_data$variable)),
#   				   labels = rev(c(
# "1. Most people would willingly accept a former alcoholic \nas a close friend.",
# "2. Most people believe that a person who has had alcohol \ntreatment is just as intelligent as the average person.",
# "3. Most people believe that a former alcoholic is just as \ntrustworthy as the average person.",
# "4. Most people would accept a fully recovered former alcoholic \nas a teacher of young children in a public school.",
# "5. Most people feel that entering alcohol treatment is a \nsign of personal failure.",
# "6. Most people would not hire a former alcoholic to take care \nof their children, even if he or she had been \nsober for some time.",
# "7. Most people think less of a person who has been in alcohol \ntreatment.",
# "8. Most employers will hire a former alcoholic if he or \nshe is qualified for the job.",
# "9. Most employers will pass over the application of a former \nalcoholic in favor of another applicant.",
# "10. Most people in my community would treat a former alcoholic \njust as they would treat anyone else.",
# "11. Most young women would be reluctant to date a man \nwho has been hospitalized for alcoholism.",
# "12. Once they know a person was in alcohol treatment, \nmost people will take his or her opinion less seriously."
# 							))) #+ 
#   # theme(text = element_text(size=35))  #+ 
#   # scale_y_discrete(limits=rev(levels))
# avseq

# library(ggplot2)
figure4<-ggplot(plot_data, aes(y=variable, x=value)) +
  geom_point(aes(size = plot_data$feq_2*2), 
  			 alpha=0.8, 
  			 color=plot_data$color, 
  			 show.legend=FALSE) +
  geom_text(aes(label = feq_2), 
  			color="white") +
  scale_size(range = c(0,25)) +
  theme_bw() +
  xlab(label="") + 
  ylab(label="Stigma Scale") + 
  scale_x_discrete(limits = c("Strongly disagree",
  							  "Disagree",
  							  "Somewhat disagree",
  							  "Somewhat agree",
  							  "Agree",
  							  "Strongly agree",
  							  "Mean (SD)"),
  				   labels = c("Strongly \ndisagree",
  							  "Disagree",
  							  "Somewhat \ndisagree",
  							  "Somewhat \nagree",
  							  "Agree",
  							  "Strongly \nagree",
  							  "Mean (SD)")) + 
  scale_y_discrete(limits = rev(c("alcoholic_close_friend",
  								  "alc_treatment_intelligent",
  								  "alcoholic_trustworthy",
  								  "recovered_alcoholic_teacher",
  								  "alc_treatment_failure",
  								  "recover_alcoholic_chldrn",
  								  "think_less_treated_person",
  								  "recover_alcoholic_hired",
  								  "non_alcoholic_hired",
  								  "recovered_alc_treat_same",
  								  "not_date_hospital_for_alc",
  								  "less_opinion_trtd_person")),
  				   labels = rev(c(
"1. Most people would willingly accept a former alcoholic \nas a close friend.",
"2. Most people believe that a person who has had alcohol \ntreatment is just as intelligent as the average person.",
"3. Most people believe that a former alcoholic is just as \ntrustworthy as the average person.",
"4. Most people would accept a fully recovered former alcoholic \nas a teacher of young children in a public school.",
"5. Most people feel that entering alcohol treatment is a \nsign of personal failure.",
"6. Most people would not hire a former alcoholic to take care \nof their children, even if he or she had been \nsober for some time.",
"7. Most people think less of a person who has been in alcohol \ntreatment.",
"8. Most employers will hire a former alcoholic if he or \nshe is qualified for the job.",
"9. Most employers will pass over the application of a former \nalcoholic in favor of another applicant.",
"10. Most people in my community would treat a former alcoholic \njust as they would treat anyone else.",
"11. Most young women would be reluctant to date a man \nwho has been hospitalized for alcoholism.",
"12. Once they know a person was in alcohol treatment, \nmost people will take his or her opinion less seriously."
							))) +
  # geom_tile(fill=plot_data$tile) +
  geom_text(aes(label=text))

 ggsave("figure4.eps", #change .eps to .pdf for different format
		figure4, #plot is the name of the fig, but the function assumes the last plot if argument is NULL
		path="/Users/Joao/Desktop", #path to save the plot
		width = 8, 
		height = 8, 
		device=cairo_ps) #cairo_ps is a d
# dev.off()
######################################################################
#Figure 5.
######################################################################

factor_to_numeric<-function(x){ 

as.numeric(x)

}



network_data<-data.frame(likert_data1,
	likert_data2,likert_data3,pas_score)

network_data<-lapply(network_data,factor_to_numeric)


colnames(network_data)<-c("Q1","Q2","Q3","Q4","Q5",
	"Q6","Q7","Q8","Q9","Q10",
	"Q11","Q12","Q13","Q14","Q15",
	"Q16","Q17","Q18","Q19","Q20",
	"Q21","Q22","Q23","PAS")

cor<-cor(na.omit(as.data.frame(network_data)),method="spearman")
# cor<-cor_auto(na.omit(network_data))

Hmisc::rcorr(na.omit(as.matrix(as.data.frame(network_data))),type="spearman")

test<-FDRnetwork(cor, cutoff=0.06,method="pval")

#listing grouping variables in the network resulting from the 
#community analysis
node_groups<-list(first_path=c(8,11,22,24),
	non_sig_PAS=c(6,7,10,18,21),
	second_path=c(1,2,3,4,5,9,12,13,14,15,16,17,19,20),
	outcome=c(22,23))

# creating vectors for labels
node_labels<-c(
"1.In my schooling, we discussed at­ risk alcohol behavior and alcohol abuse.",
"2.In my schooling, we discussed counseling patients with at­ risk drinking behaviors.",
"3.Once patients suffer an injury from drinking they are called 'harmful drinkers",
"4.It is not my role to ask about alcohol use.",
"5.Talking to patients about decreasing their alcohol ingestion can be successful.",

"6.A large number of patients drink alcohol.",
"7.Alcohol use and abuse is not a problem amongs our patient population at KCMC.",
"8.Injury pateints at KCMC were likely drinking when they were injured.",
"9.Caring for patients who are intoxicated is\n frustrating as they caused themselves\n to be ill/injured.",
"10.Few injury patients at KCMC suffer from alcohol related injuries.",
"11.Knowing if patients have atrisk drinking does NOT improve care I can provide.",
"12.I feel comfortable asking patients about their alcohol use behavior.",
"13.I feel comfortable counseling patients about their atrisk drinking.",
"14.How motivated are you and your colleagues at implementing alcohol screening\n and testing?",
"15.How willing are you and your colleagues to learn about reducing\n harmful alcohol\n use among injury patients?",
"16.How willing are you and your colleagues to\n implement alcohol screening among\n injury patients?",

"17.It is common to ask patients about their drinking behavior.",
"18.It is common to test patients for alcohol.",
"19.It is comon to ask patients about their tobacco use behavior.",
"20.There are resources to refer patients to when I determine\n they have high risk drinking.",
"21.I ask my patients about their alcohol use",
"22. Testing breath and serum"
"23.I counsel patients to reduce their drinking\n if I think they have harmful drinking behavior.",
"24.Perceived alcohol stigma")#,
# "Personal Devaluation")

# creating nodes labels vector
# node_names<-c("Know1","Know2","Know3","Know4","Know5",
# 	"Percept1","Percept2","Percept3","Percept4","Percept5",
# 	"Percept6","Percept7","Percept8","Percept9","Percept10",
# 	"Percept11","Pract1","Pract2","Pract3","Pract4","Pract5",
# 	"Pract6","PAS")#,"PDeval")

node_names<-c("Q1","Q2","Q3","Q4","Q5",
	"Q6","Q7","Q8","Q9","Q10",
	"Q11","Q12","Q13","Q14","Q15",
	"Q16","Q17","Q18","Q19","Q20",
	"Q21","Q22","Q23","PAS")#,"PDeval")

# findGraph(cor, 34, type = "cor")

network<-qgraph(cor,
	layout="spring",
	# vsize=importance_vSize*3,
	esize=20,
	# graph="pcor",
	sampleSize=nrow(network_data),
	# cut = 0.6,
	maximum = 1, 
	minimum = 0.35,
	repulsion = 0.8,
	groups=node_groups,
	# nodeNames=node_labels,
	color=c("steelblue","white","grey","gold",layoutScale=c(2,2)),
	borders = FALSE,
	labels=node_names,
	legend.cex=0.35,
	legend.mode="names")#,gray=T,)#,nodeNames=nomesqsg

#Identify SPLs within the graph and extract direct paths to WP
predictors<-centrality(network,all.shortest.paths=TRUE)$ShortestPaths[,22]
predictors

predictors<-centrality(network,all.shortest.paths=TRUE)$ShortestPaths[,23]
predictors

# predictors<-centrality(network)$ShortestPaths[,24]
# predictors

library(corrplot)
MDS_Corr <- read.csv("JoshBrmMdsCORR_9.4.2014.csv", header=T, row.names=1, na.rm=TRUE)
round(MDS_Corr, digits=2)
MDS_Corr$FAM<- NULL
MDS_Corr$Freq<- NULL

joshcor<-cor(MDS_Corr, use="complete.obs")



rownames(cor)<-c("Q1",
"Q2",
"Q3",
"Q4",
"Q5",
"Q6",
"Q7",
"Q8",
"Q9",
"Q10",
"Q11",
"Q12",
"Q13",
"Q14",
"Q15",
"Q16",
"Q17",
"Q18",
"Q19",
"Q20",
"Q21",
"Q22",
"Q23",
"Q24")

colnames(cor)<-c("Q1",
"Q2",
"Q3",
"Q4",
"Q5",
"Q6",
"Q7",
"Q8",
"Q9",
"Q10",
"Q11",
"Q12",
"Q13",
"Q14",
"Q15",
"Q16",
"Q17",
"Q18",
"Q19",
"Q20",
"Q21",
"Q22",
"Q23",
"Q24")

setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
postscript("/Users/Joao/Desktop/appendix2.eps",
	width = 8, height = 8)
#Add plot
corrplot(cor, method="shade", shade.col=NA, tl.col="black")
dev.off()
######################################################################
#END
######################################################################