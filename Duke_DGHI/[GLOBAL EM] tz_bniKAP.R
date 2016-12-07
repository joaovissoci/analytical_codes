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
	"grid","lattice","latticeExtra","HH","mice"),library, 
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

likert_data1$talking_can_be_successful

table(likert_data1$discuss_risky_alc)
table(likert_data1$discuss_counsel_pts)
table(likert_data1$called_harmful_drinkers)
table(likert_data1$not_my_role)
table(likert_data1$talking_can_be_successful)

likert1<-c(0,1,1,2,0)
likert2<-c(1,2,3,2,1)
likert3<-c(0,0,5,2,0)
likert4<-c(12,18,13,12,13)
likert5<-c(21,12,12,16,19)

likert<-data.frame(likert1,likert2,likert3,likert4,likert5)
likert$var<-c("q1","q2","q3","q4","q5")
rownames(likert)<-c("In my schooling, we discussed at­risk\n alcohol behavior and alcohol abuse.",
					"In my schooling, we discussed counseling\n patients with at­risk drinking behaviors.",
					"Once patients suffer an injury from\n drinking they are called 'harmful drinkers",
					"It is my role to ask\n about alcohol use",
					"Talking to patients about decreasing\n their alcohol ingestion can be successful.")

colnames(likert)<-c("Strongly Disagree",
					"Disagree",
					"I don't know",
					"Agree",
					"Strongly Agree")

# likert(likert,
# auto.key=list(between=1, between.columns=2),
# xlab="Percentage",
# # main="Knowledge about alcohol use in ED patients",
# BrewerPaletteName="Blues")
# # sub="Likert Scale")

HH::likert(likert, main="",
			as.percent=TRUE, rightAxisLabels=NULL, 
			# ylab.right="Perceptions",
            positive.order=TRUE,
            scales=list(x=list(limits=c(-100,100),
            	at=c(-100,-50,0,50,100))))
            # scales=list(x=list(at=seq(0,50,100))))
                          # labels=as.vector(rbind("",seq(0,50,100))))))

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

likert<-data.frame(likert2,likert1)
rownames(likert)<-c("How many drinks per sitting is\n at­risk drinking for men?",
					"How many drinks per sitting is\n at­risk drinking for women?",
					"How many drinks per week is\n at­risk drinking for men?",
					"How many drinks per week is\n at­risk drinking for women?")

colnames(likert)<-c("Wrong","Right")

# likert(likert,
# auto.key=list(between=1, between.columns=2),
# xlab="Percentage",
# # main="Knowledge about alcohol use in ED patients",
# BrewerPaletteName="Blues")
# # sub="Likert Scale")

HH::likert(likert, main="",
			as.percent=TRUE, 
			rightAxisLabels=NULL, 
			# ylab.right="Perceptions",
            positive.order=TRUE,
            scales=list(x=list(limits=c(-100,100),
            at=c(-100,-50,0,50,100))),
            col=c("#D33F6A","#4A6FE3"))

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

x2<-na.omit(melt(figure2_data))

count_data<-plyr::count(x2, c("variable", "value"))

write.csv(count_data,"/Users/joaovissoci/blah.csv")

dat<-read.csv("/Users/joaovissoci/blah.csv")
colnames(count_data)<-c("var","likert","value")
dat2<-cast(count_data,var~likert)
colnames(dat2)<-c("var","likert1","likert2","likert3",
	"likert4","likert5")
# dat3<-rbind(dat2,likert)
# rownames(dat3)<-dat3$var
# dat3$gr<-as.factor(c(rep("test1",11),rep("test2",5)))
rownames(dat2)<-c("A large number of patients drink alcohol.",
				"Alcohol use and abuse is not a problem \n amongs our patient population at KCMC.",
				"Injury pateints at KCMC were likely drinking\n when they were injured.",
			  	"Caring for patients who are intoxicated is\n frustrating as they caused themselves\n to be ill/injured.",
			  	"Few injury patients at KCMC suffer from\n alcohol related injuries.",
			  	"Knowing if patients have atrisk drinking\n does NOT improve care I can provide.",
			  	"I feel comfortable asking patients about\n their alcohol use behavior.",
			  	"I feel comfortable counseling patients about\n their atrisk drinking.",
			  	"How motivated are you and your colleagues at\n implementing alcohol screening\n and testing?",
			  	"How willing are you and your colleagues to\n learn about reducing harmful alcohol\n use among injury patients?",
			  	"How willing are you and your colleagues to\n implement alcohol screening among\n injury patients?")

colnames(dat2)<-c("var","Strongly Disagree",
					"Disagree",
					"I don't know",
					"Agree",
					"Strongly Agree")

NAto0<-function(x){
	car::recode(x,"NA=0")
	}

dat_2_2<-sapply(dat2,NAto0)

HH::likert(dat_2_2[,-1], main="",
			as.percent=TRUE, rightAxisLabels=NULL, 
			# ylab.right="Perceptions",
            positive.order=TRUE,
            scales=list(x=list(limits=c(-100,100),
            at=c(-100,-50,0,50,100))))


# likert(dat2,
# auto.key=list(between=1, between.columns=2),
# xlab="Percentage",
# # main="Knowledge about alcohol use in ED patients",
# BrewerPaletteName="Blues")
# # sub="Likert Scale")

# # if(!require(devtools)) install.packages("devtools")
# # devtools::install_github('jbryer/likert')

# # # additional requirements
# # library(ggplot2)
# # library(reshape2)
# # library(RColorBrewer)
# library(likert)

# # create summary table
# table_summary = likert::likert(na.omit(figure2_data))

# # reshape results
# results = melt(table_summary$results, id.vars='Item')

# # reorder results
# # results$Item = factor(results$Item, levels=c("LT", "ST", "SemTag", "SemTagContext"))

# # some defaults
# ymin = 0
# text.size = 3

# ggplot(dat, aes(y=value, x=var, group=likert)) + 
#   geom_bar(stat='identity', aes(fill=var)) + 
#   ylim(c(-5,105)) + 
#   coord_flip() +
#   scale_fill_manual('Response', values=brewer.pal(11, "RdYlGn"), 
#               breaks=levels(dat$var), 
#               labels=levels(dat$var)) +
#   geom_text(data=table_summary$summary, y=ymin, aes(x=Item, 
#               label=paste(round(low), '%', sep='')), 
#               size=text.size, hjust=1) +
#   geom_text(data=table_summary$summary, y=100, aes(x=Item,
#               label=paste(round(high), '%', sep='')), 
#               size=text.size, hjust=-.2) +
#   ylab('Percentage') + xlab('')

######################################################################
#Figure 3.
######################################################################

figure4_data<-with(data,data.frame(
						 common_ask_pts_drink,
						 common_test_pts_alc,
						 common_ask_pts_tobacco,
						 resources_refer_pts,
						 ask_pts_alc,
						 test_alc_breath_or_serum
						 ))

x2<-na.omit(melt(figure4_data))

count_data<-plyr::count(x2, c("variable", "value"))

# write.csv(count_data,"/Users/joaovissoci/blah2.csv")

# dat<-read.csv("/Users/joaovissoci/blah2.csv")
colnames(count_data)<-c("var","likert","value")
dat2<-cast(count_data,var~likert)

NAto0<-function(x){
	car::recode(x,"NA=0")
	}

dat_2_2<-sapply(dat2,NAto0)


colnames(dat_2_2)<-c("var","likert1","likert2","likert3",
	"likert4","likert5")
# dat3<-rbind(dat2,likert)
# rownames(dat3)<-dat3$var
# dat3$gr<-as.factor(c(rep("test1",11),rep("test2",5)))
rownames(dat_2_2)<-c(
"It is common to ask patients about their\n drinking behavior.",
"It is common to test patients for alcohol.",
"It is comon to ask patients about their\n tobacco use behavior.",
"There are resources to refer patients to\n when I determine they have high risk drinking.",
"I ask my patients about their alcohol use",
"I counsel patients to reduce their drinking\n if I think they have harmful drinking behavior.")

colnames(dat_2_2)<-c("var","Strongly Disagree",
					"Disagree",
					"I don't know",
					"Agree",
					"Strongly Agree")

HH::likert(dat_2_2[,-1], main="",
			as.percent=TRUE, rightAxisLabels=NULL, 
			# ylab.right="Perceptions",
            positive.order=TRUE,
            scales=list(x=list(limits=c(-100,100),
            at=c(-100,-50,0,50,100))))

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

# plot_data$color<-NULL
# plot_data$color[plot_data$feq_2 >= 0 & plot_data$feq_2 < 5.883]="lightcyan1"
# plot_data$color[plot_data$feq_2 >= 5.883 & plot_data$feq_2 < 11.76]="lightcyan2"
# plot_data$color[plot_data$feq_2 >= 11.76 & plot_data$feq_2 < 26.47]="lightcyan3"
# plot_data$color[plot_data$feq_2 >= 26.47]="lightcyan4"

#find colors numbers: diverge_hcl(7, c = 100, l = c(50, 90), power = 1)
#from: https://cran.r-project.org/web/packages/colorspace/vignettes/hcl-colors.pdf
plot_data$color<-NULL
plot_data$color[plot_data$value == "Strongly agree"]="#4A6FE3"
plot_data$color[plot_data$value == "Agree"]="#8595E1"
plot_data$color[plot_data$value == "Somewhat agree"]="#B5BBE3"

plot_data$color[plot_data$value == "Somewhat disagree"]="#E6AFB9"
plot_data$color[plot_data$value == "Disagree"]="#E07B91"
plot_data$color[plot_data$value == "Strongly disagree"]="#D33F6A"


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

library(ggplot2)
ggplot(plot_data, aes(y=variable, x=value)) +
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
  							  "Strongly agree"),
  				   labels = c("Strongly \ndisagree",
  							  "Disagree",
  							  "Somewhat \ndisagree",
  							  "Somewhat \nagree",
  							  "Agree",
  							  "Strongly \nagree")) + 
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
							)))

######################################################################
#Figure 5.
######################################################################
network_data<-data.frame(likert_data1,
	figure2_data,figure4_data,pas_score)
colnames(network_data)<-c("Q1","Q2","Q3","Q4","Q5",
	"Q6","Q7","Q8","Q9","Q10",
	"Q11","Q12","Q13","Q14","Q15",
	"Q16","Q17","Q18","Q19","Q20",
	"Q21","Q22","PAS")

cor<-cor(na.omit(network_data),method="spearman")
# cor<-cor_auto(na.omit(network_data))

Hmisc::rcorr(as.matrix(network_data),type="spearman")

#listing grouping variables in the network resulting from the 
#community analysis
node_groups<-list(first_path=c(8,11,22),
	non_sig_PAS=c(6,7,10,18,21),
	second_path=c(1,2,3,4,5,9,12,13,14,15,16,17,19,20),
	stigma=c(23))

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
"22.I counsel patients to reduce their drinking\n if I think they have harmful drinking behavior.",
"23.Perceived alcohol stigma")#,
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
	"Q21","Q22","PAS")#,"PDeval")

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

# predictors<-centrality(network)$ShortestPaths[,24]
# predictors


######################################################################
#END
######################################################################