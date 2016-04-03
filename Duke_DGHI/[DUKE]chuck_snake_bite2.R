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
lapply(c("ggplot2", "psych", "RCurl", "irr", "nortest", 
	"moments","GPArotation","nFactors","boot","psy", "car",
	"vcd", "gridExtra","mi","VIM","epicalc","gdata","sqldf",
	"reshape2","mclust","foreign","survival","memisc","lme4",
	"lmerTest","dplyr"),library, character.only=T)

#Package and codes to pull data from goodle sheets
#devtools::install_github("jennybc/googlesheets")
#library(googlesheets)
#suppressMessages(library(dplyr))

######################################################
#IMPORTING DATA
######################################################

#uploading data ---------------------------------------------------------------------
#Load the data set.
#All data are stored in  http://figshare.com/articles/The_reliability_of_AO_classification_on_femur_fractures_among_orthopedic_residents/103664
#Note that the data set has been reorganized to be applied to some functions

#baseline_lena<-read.csv("/Users/rpietro/Google Drive/research_groups/RoR/IPq/Suicide_Anxiety/baseline.csv",sep=",")
 
#Pulling data from dropbox
#data_hamilton <- repmis::source_DropboxData("lena_hamilton.csv","r31zt5zeiygsc23",sep = ",",header = TRUE)

#pulling data from Google Spreadsheet - using http://goo.gl/VV4o1g
#authorize(new_user = TRUE)
#my_sheets <- list_sheets()
	#sheet_rs <- register_ss("Copy of Rattlesnake data")
#data_rs  <- get_via_lf(sheet_rs, ws = "Coagulation Data")
#demographics_rs<-get_via_lf(sheet_rs, ws = "Demographics") 

data_rs <- read.csv("/Users/joaovissoci/OneDrive - Duke University/datasets/DGHI/snakebite_longitudinal/data_rs.csv")
demographics_rs <- read.csv("/Users/joaovissoci/OneDrive - Duke University/datasets/DGHI/snakebite_longitudinal/demographics_rs.csv")
data_cp <- read.csv("/Users/joaovissoci/OneDrive - Duke University/datasets/DGHI/snakebite_longitudinal/data_cp.csv")
demographics_cp <- read.csv("/Users/joaovissoci/OneDrive - Duke University/datasets/DGHI/snakebite_longitudinal/demographics_cp.csv")

#sheet_cp <- register_ss("Copy of Copperhead data")#
#data_cp <- get_via_lf(sheet_cp, ws = "Coagulation Data") 
#demographics_cp<-get_via_lf(sheet_cp, ws = "Demographics") 

#data_cp<-read.csv("/home/joao/Desktop/data_cp.csv",sep=',')
#data_rs<-read.csv("/home/joao/Desktop/data_rs.csv",sep=',')
#demographics_cp<-read.csv("/home/joao/Desktop/demographics2.csv",sep=',')
#demographics_rs<-read.csv("/home/joao/Desktop/demographics2.csv",sep=',')

######################################################
#DATA MANAGEMENT
######################################################

names(data_rs)
names(data_cp)

## Adjusting RS nadir data platelets
nadir1_platelets_rs<-with(data_rs,data.frame(platelets2hoursafterstartof1ststudydruginfusionkmm3,platelets2hoursafterstartof2ndstudydruginfusionkmm3,platelets2hoursafterstartof3rdstudydruginfusionkmm3,platelets2hoursafterstartof3rdmaintenancestudydruginfusionkmm3))

nadir2_platelets_rs<-sapply(nadir1_platelets_rs,function(x) recode(x,"NA=9999"))

nadir_platelets_rs<-NULL

for (i in 1:93) {
   nadir_platelets_rs[i]=min(nadir2_platelets_rs[i,])
} 

nadir_platelets_rs<-car::recode(nadir_platelets_rs,"9999=NA")

## Adjusting RS nadir data fibrinogen
nadir1_fibrinogen_rs<-with(data_rs,data.frame(fibrinogen2hoursafterstartof1ststudydruginfusionmgdl,fibrinogen2hoursafterstartof2ndstudydruginfusionmgdl,fibrinogen2hoursafterstartof3rdstudydruginfusionmgdl,fibrinogen2hoursafterstartof3rdmaintenancestudydruginfusionmgdl))

nadir2_fibrinogen_rs<-sapply(nadir1_fibrinogen_rs,function(x) recode(x,"NA=9999"))

nadir_fibrinogen_rs<-NULL

for (i in 1:93) {
   nadir_fibrinogen_rs[i]=min(nadir2_fibrinogen_rs[i,])
} 

nadir_fibrinogen_rs<-car::recode(nadir_fibrinogen_rs,"9999=NA")

## Adjusting CP nadir data platelets
nadir1_platelets_cp<-with(data_cp,data.frame(platelets2hoursafterstartof1ststudydruginfusionkmm3,platelets2hoursafterstartofunscheduledstudydruginfusionkmm3,platelets2hoursafterstartof3rdstudydruginfusionkmm3,platelets2hoursafterstartof2ndstudydruginfusionkmm3,platelets2hoursafterstartof3rdmaintenancestudydruginfusionkmm3))

nadir2_platelets_cp<-sapply(nadir1_platelets_cp,function(x) recode(x,"NA=9999"))

nadir_platelets_cp<-NULL

for (i in 1:22) {
   nadir_platelets_cp[i]=min(nadir2_platelets_cp[i,])
} 

nadir_platelets_cp<-car::recode(nadir_platelets_cp,"9999=NA")

## Adjusting CP nadir data fibrinogen
nadir1_fibrinogen_cp<-with(data_cp,data.frame(fibrinogen2hoursafterstartof1ststudydruginfusionmgdl,fibrinogen2hoursafterstartofunscheduledstudydruginfusionmgdl,fibrinogen2hoursafterstartof2ndstudydruginfusionmgdl,fibrinogen2hoursafterstartof3rdstudydruginfusionmgdl,fibrinogen2hoursafterstartof3rdmaintenancestudydruginfusionmgdl,fibrinogen2hoursafterstartofunscheduledstudydruginfusionduringunscheduledvisitmgdl))

nadir2_fibrinogen_cp<-sapply(nadir1_fibrinogen_cp,function(x) recode(x,"NA=9999"))

nadir_fibrinogen_cp<-NULL

for (i in 1:22) {
   nadir_fibrinogen_cp[i]=min(nadir2_fibrinogen_cp[i,])
} 

nadir_fibrinogen_cp<-car::recode(nadir_fibrinogen_cp,"9999=NA")

#ID AND GRUOPING Variables
data_rs$group<-c("Rattlesnake")
data_rs$id<-rep(1:93)
data_cp$group<-c("Copperhead")
data_cp$id<-rep(1:22)

#Joining datasets RS and CP for platelets outcome
platelets<-NULL
platelets$baseline = c(data_rs$baselineplateletcountkmm3,data_cp$baselineplateletcountkmm3)
platelets$T1 = c(nadir_platelets_rs,nadir_platelets_cp)
platelets$T2 = c(data_rs$day5followupplateletskmm3,data_cp$day5followupplateletskmm3)
platelets$T3 = c(data_rs$day8followupplateletskmm3,data_cp$day8followupplateletskmm3)
platelets$T4 = c(data_rs$day15followupplateletskmm3,data_cp$day15followupplateletskmm3)
platelets$id<-c(data_rs$id,data_cp$id)
platelets$group<-c(data_rs$group,data_cp$group)

#Joining datasets RS and CP for fibrinogen outcome
fibrinogen<-NULL
fibrinogen$baseline = c(data_rs$baselinefibrinogencountmgdl,data_cp$baselinefibrinogencountmgdl)
fibrinogen$T1 = c(nadir_fibrinogen_rs,nadir_fibrinogen_cp)
fibrinogen$T2 = c(data_rs$day5followupfibrinogenmgdl,data_cp$day5followupfibrinogenmgdl)
fibrinogen$T3 = c(data_rs$day8followupfibrinogenmgdl,data_cp$day8followupfibrinogenmgdl)
fibrinogen$T4 = c(data_rs$day15followupfibrinogenmgdl,data_cp$day15followupfibrinogenmgdl)
fibrinogen$id<-c(data_rs$id,data_cp$id)
fibrinogen$group<-c(data_rs$group,data_cp$group)

######################################################
#POWER AND SAMPLE SIZE
######################################################
library(pwr)
pwr.t.test(n=22,sig.level=0.05,d=0.60,type=c("paired"))

#https://cran.r-project.org/web/packages/longpower/longpower.pdf
#Liu, G., & Liang, K. Y. (1997). Sample size calculations for studies with correlated observations. Biometrics, 53(3), 937-47.
#Diggle PJ, Heagerty PJ, Liang K, Zeger SL. Analysis of longitudinal data. Second Edition. Oxford. Statistical Science Serires. 2002

require(longpower)
require(lme4)
fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy) 
#lmmpower(fm1, pct.change = 0.30, t = seq(0,9,1), power = 0.80)
# code run after the model

######################################################
#DEMOGRAPHICS
######################################################
## OVERALL

demographics_data<-rbind(demographics_rs,demographics_cp)
psych::describe(demographics_data$Age)
ad.test(demographics_data$Age)
hist(demographics_data$Age)
ci_func(demographics_data$Age,.95)
describeBy(demographics_data$Age,fibrinogen$group)
wilcox.test(demographics_data$Age~fibrinogen$group)

demographics_data$Gender<-car::recode(demographics_data$Gender,"'F'='Female';'M'='Male'")
table_overall<-table(demographics_data$Gender)
table_overall
prop.table(table_overall)
table<-table(demographics_data$Gender,fibrinogen$group)
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

demographics_data$Ethnicity<-car::recode(demographics_data$Ethnicity,"'American Indian'='Other';'Amiercan Indian'='Other';'Native Hawaiian'='Other';'Asian'='Other';'Caucasian'='White'")
table_overall<-table(demographics_data$Ethnicity)
table_overall
prop.table(table_overall)
table<-table(demographics_data$Ethnicity,fibrinogen$group)
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

demographics_data$extra_dose<-car::recode(demographics_data$extra_dose,"'Yes'='Yes';else='No'")
table_overall<-table(demographics_data$extra_dose)
table_overall
prop.table(table_overall)
table<-table(demographics_data$extra_dose,fibrinogen$group)
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

summary(demographics_data$severity_score)
ad.test(demographics_data$severity_score)
hist(demographics_data$severity_score)
ci_func(demographics_data$severity_score,.95)
by(demographics_data$severity_score,fibrinogen$group,summary)
wilcox.test(demographics_data$severity_score~fibrinogen$group)

## TREATMENT Descriptives
table_overall<-table(demographics_data$Treatment)
table_overall
prop.table(table_overall)
table<-table(demographics_data$Treatment,fibrinogen$group)
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

########## PLATELETS #####################
data_rs_table2<-with(data_rs,data.frame(baselineplateletcountkmm3,nadir_platelets_rs,day5followupplateletskmm3,day8followupplateletskmm3,day15followupplateletskmm3))	
data_rs_table2<-na.omit(data_rs_table2)

data_cp_table2<-with(data_cp,data.frame(baselineplateletcountkmm3,nadir_platelets_cp,day5followupplateletskmm3,day8followupplateletskmm3,day15followupplateletskmm3))	
data_cp_table2<-na.omit(data_cp_table2)

#### BASELINE PLATELETS
#Baseline Platelets - RS
ad.test(data_rs_table2$baselineplateletcountkmm3)
describe(data_rs_table2$baselineplateletcountkmm3)
x_bar = mean(data_rs_table2$baselineplateletcountkmm3)
n = length(data_rs_table2$baselineplateletcountkmm3)
s = sd(data_rs_table2$baselineplateletcountkmm3)
t_star = qt(0.95, n-1)
ci_lower = x_bar - t_star*s/sqrt(n)
ci_upper = x_bar + t_star*s/sqrt(n)
c(ci_lower, ci_upper)

#Platelets <10ˆ9/L
platelets_less150<-car::recode(data_rs_table2$baselineplateletcountkmm3,
	"0:150='less';else='high'")
table<-table(platelets_less150)
table
prop.table(table)

#Platelets 50, 100, 150
platelets_lesscats<-car::recode(data_rs_table2$baselineplateletcountkmm3,
	"0:50='50less';50.01:100='100to50';100.01:150='150to100';else='high'")
table<-table(platelets_lesscats)
table
prop.table(table)

#Baseline Platelets - CP
ad.test(data_cp_table2$baselineplateletcountkmm3)
describe(data_cp_table2$baselineplateletcountkmm3)
x_bar = mean(data_cp_table2$baselineplateletcountkmm3)
n = length(data_cp_table2$baselineplateletcountkmm3)
s = sd(data_cp_table2$baselineplateletcountkmm3)
t_star = qt(0.95, n-1)
ci_lower = x_bar - t_star*s/sqrt(n)
ci_upper = x_bar + t_star*s/sqrt(n)
c(ci_lower, ci_upper)

#Platelets <10ˆ9/L
platelets_less150<-car::recode(data_cp_table2$baselineplateletcountkmm3,
	"0:150='less';else='high'")
table<-table(platelets_less150)
table
prop.table(table)

#Platelets 50, 100, 150
platelets_lesscats<-car::recode(data_cp_table2$baselineplateletcountkmm3,
	"0:50='50less';50.01:100='100to50';100.01:150='150to100';else='high'")
table<-table(platelets_lesscats)
table
prop.table(table)

#Comparing both
t.test(data_rs_table2$baselineplateletcountkmm3,data_cp_table2$baselineplateletcountkmm3,conf.level=.95)

#### NADIR PLATELETS

#NADIR Platelets - RS
ad.test(data_rs_table2$nadir_platelets_rs)
describe(data_rs_table2$nadir_platelets_rs)
x_bar = mean(na.omit(data_rs_table2$nadir_platelets_rs))
n = length(data_rs_table2$nadir_platelets_rs)
s = sd(na.omit(data_rs_table2$nadir_platelets_rs))
t_star = qt(0.95, n-1)
ci_lower = x_bar - t_star*s/sqrt(n)
ci_upper = x_bar + t_star*s/sqrt(n)
c(ci_lower, ci_upper)

#NADIR Platelets - CP
ad.test(data_cp_table2$nadir_platelets_cp)
describe(data_cp_table2$nadir_platelets_cp)
x_bar = mean(data_cp_table2$nadir_platelets_cp)
n = length(data_cp_table2$nadir_platelets_cp)
s = sd(data_cp_table2$nadir_platelets_cp)
t_star = qt(0.95, n-1)
ci_lower = x_bar - t_star*s/sqrt(n)
ci_upper = x_bar + t_star*s/sqrt(n)
c(ci_lower, ci_upper)

#Comparing both
t.test(data_rs_table2$nadir_platelets_rs,nadir_platelets_cp,conf.level=.95)

####  PLATELETS 5DAYS

#PLATELETS 5DAYS - RS
ad.test(data_rs_table2$day5followupplateletskmm3)
describe(data_rs_table2$day5followupplateletskmm3)
x_bar = mean(na.omit(data_rs_table2$day5followupplateletskmm3))
n = length(data_rs_table2$day5followupplateletskmm3)
s = sd(na.omit(data_rs_table2$day5followupplateletskmm3))
t_star = qt(0.95, n-1)
ci_lower = x_bar - t_star*s/sqrt(n)
ci_upper = x_bar + t_star*s/sqrt(n)
c(ci_lower, ci_upper)

#PLATELETS 5DAYS - CP
ad.test(data_cp_table2$day5followupplateletskmm3)
describe(data_cp_table2$day5followupplateletskmm3)
x_bar = mean(na.omit(data_cp_table2$day5followupplateletskmm3))
n = length(na.omit(data_cp_table2$day5followupplateletskmm3))
s = sd(na.omit(data_cp_table2$day5followupplateletskmm3))
t_star = qt(0.95, n-1)
ci_lower = x_bar - t_star*s/sqrt(n)
ci_upper = x_bar + t_star*s/sqrt(n)
c(ci_lower, ci_upper)

#Comparing both
t.test(data_rs_table2$day5followupplateletskmm3,data_cp_table2$day5followupplateletskmm3,conf.level=.95)

####  PLATELETS 5DAYS

#PLATELETS 8DAYS - RS
ad.test(data_rs_table2$day8followupplateletskmm3)
describe(data_rs_table2$day8followupplateletskmm3)
x_bar = mean(na.omit(data_rs_table2$day8followupplateletskmm3))
n = length(data_rs_table2$day8followupplateletskmm3)
s = sd(na.omit(data_rs_table2$day8followupplateletskmm3))
t_star = qt(0.95, n-1)
ci_lower = x_bar - t_star*s/sqrt(n)
ci_upper = x_bar + t_star*s/sqrt(n)
c(ci_lower, ci_upper)

#PLATELETS 8DAYS - CP
ad.test(data_cp_table2$day8followupplateletskmm3)
describe(data_cp_table2$day8followupplateletskmm3)
x_bar = mean(na.omit(data_cp_table2$day8followupplateletskmm3))
n = length(na.omit(data_cp_table2$day8followupplateletskmm3))
s = sd(na.omit(data_cp_table2$day8followupplateletskmm3))
t_star = qt(0.95, n-1)
ci_lower = x_bar - t_star*s/sqrt(n)
ci_upper = x_bar + t_star*s/sqrt(n)
c(ci_lower, ci_upper)

#Comparing both
t.test(data_rs_table2$day8followupplateletskmm3,data_cp_table2$day8followupplateletskmm3,conf.level=.95)

####  PLATELETS 5DAYS

#PLATELETS 15DAYS - RS
ad.test(data_rs_table2$day15followupplateletskmm3)
describe(data_rs_table2$day15followupplateletskmm3)
x_bar = mean(na.omit(data_rs_table2$day15followupplateletskmm3))
n = length(data_rs_table2$day15followupplateletskmm3)
s = sd(na.omit(data_rs_table2$day15followupplateletskmm3))
t_star = qt(0.95, n-1)
ci_lower = x_bar - t_star*s/sqrt(n)
ci_upper = x_bar + t_star*s/sqrt(n)
c(ci_lower, ci_upper)

#PLATELETS 15DAYS - CP
ad.test(data_cp_table2$day15followupplateletskmm3)
describe(data_cp_table2$day15followupplateletskmm3)
x_bar = mean(na.omit(data_cp_table2$day15followupplateletskmm3))
n = length(na.omit(data_cp_table2$day15followupplateletskmm3))
s = sd(na.omit(data_cp_table2$day15followupplateletskmm3))
t_star = qt(0.95, n-1)
ci_lower = x_bar - t_star*s/sqrt(n)
ci_upper = x_bar + t_star*s/sqrt(n)
c(ci_lower, ci_upper)

t.test(data_rs_table2$day15followupplateletskmm3,data_cp_table2$day15followupplateletskmm3,conf.level=.95)

######## FIBRINOGEN #######################
data_rs_table3<-with(data_rs,data.frame(baselinefibrinogencountmgdl,nadir_fibrinogen_rs,day5followupfibrinogenmgdl,day8followupfibrinogenmgdl,day15followupfibrinogenmgdl))	
data_rs_table3<-na.omit(data_rs_table3)

data_cp_table3<-with(data_cp,data.frame(baselinefibrinogencountmgdl,nadir_fibrinogen_cp,day5followupfibrinogenmgdl,day8followupfibrinogenmgdl,day15followupfibrinogenmgdl))	
data_cp_table3<-na.omit(data_cp_table3)

#### BASELINE FIBRINOGEN
#Baseline FIBRINOGEN - RS
ad.test(data_rs_table3$baselinefibrinogencountmgdl)
describe(data_rs_table3$baselinefibrinogencountmgdl)
x_bar = mean(data_rs_table3$baselinefibrinogencountmgdl)
n = length(data_rs_table3$baselinefibrinogencountmgdl)
s = sd(data_rs_table3$baselinefibrinogencountmgdl)
t_star = qt(0.95, n-1)
ci_lower = x_bar - t_star*s/sqrt(n)
ci_upper = x_bar + t_star*s/sqrt(n)
c(ci_lower, ci_upper)

#Fibrogen <10ˆ9/L
fibrogen_less150<-car::recode(data_rs_table3$baselinefibrinogencountmgdl,
	"0:150='less';else='high'")
table<-table(fibrogen_less150)
table
prop.table(table)

#Fibrogen 50, 100, 150
fibrogen_lesscats<-car::recode(data_rs_table3$baselinefibrinogencountmgdl,
	"0:50='50less';50.01:100='100to50';100.01:150='150to100';else='high'")
table<-table(fibrogen_lesscats)
table
prop.table(table)

#Baseline FIBRINOGEN - CP
ad.test(data_cp_table3$baselinefibrinogencountmgdl)
describe(data_cp_table3$baselinefibrinogencountmgdl)
x_bar = mean(data_cp_table3$baselinefibrinogencountmgdl)
n = length(data_cp_table3$baselinefibrinogencountmgdl)
s = sd(data_cp_table3$baselinefibrinogencountmgdl)
t_star = qt(0.95, n-1)
ci_lower = x_bar - t_star*s/sqrt(n)
ci_upper = x_bar + t_star*s/sqrt(n)
c(ci_lower, ci_upper)

#Fibrogen <10ˆ9/L
fibrogen_less150<-car::recode(data_cp_table3$baselinefibrinogencountmgdl,
	"0:150='less';else='high'")
table<-table(fibrogen_less150)
table
prop.table(table)

#Fibrogen 50, 100, 150
fibrogen_lesscats<-car::recode(data_cp_table3$baselinefibrinogencountmgdl,
	"0:50='50less';50.01:100='100to50';100.01:150='150to100';else='high'")
table<-table(fibrogen_lesscats)
table
prop.table(table)

#Comparing both
wilcox.test(data_rs_table3$baselinefibrinogencountmgdl,data_cp_table3$baselinefibrinogencountmgdl,conf.level=.95)

#### NADIR FIBRINOGEN

#NADIR FIBRINOGEN - RS
ad.test(data_rs_table3$nadir_fibrinogen_rs)
describe(data_rs_table3$nadir_fibrinogen_rs)
x_bar = mean(na.omit(data_rs_table3$nadir_fibrinogen_rs))
n = length(data_rs_table3$nadir_fibrinogen_rs)
s = sd(na.omit(data_rs_table3$nadir_fibrinogen_rs))
t_star = qt(0.95, n-1)
ci_lower = x_bar - t_star*s/sqrt(n)
ci_upper = x_bar + t_star*s/sqrt(n)
c(ci_lower, ci_upper)

#NADIR FIBRINOGEN - CP
ad.test(data_cp_table3$nadir_fibrinogen_cp)
describe(data_cp_table3$nadir_fibrinogen_cp)
x_bar = mean(data_cp_table3$nadir_fibrinogen_cp)
n = length(data_cp_table3$nadir_fibrinogen_cp)
s = sd(data_cp_table3$nadir_fibrinogen_cp)
t_star = qt(0.95, n-1)
ci_lower = x_bar - t_star*s/sqrt(n)
ci_upper = x_bar + t_star*s/sqrt(n)
c(ci_lower, ci_upper)

#Comparing both
t.test(data_rs_table3$nadir_fibrinogen_rs,nadir_fibrinogen_cp,conf.level=.95)

####  FIBRINOGEN 5DAYS

#FIBRINOGEN 5DAYS - RS
ad.test(data_rs_table3$day5followupfibrinogenmgdl)
describe(data_rs_table3$day5followupfibrinogenmgdl)
x_bar = mean(na.omit(data_rs_table3$day5followupfibrinogenmgdl))
n = length(data_rs_table3$day5followupfibrinogenmgdl)
s = sd(na.omit(data_rs_table3$day5followupfibrinogenmgdl))
t_star = qt(0.95, n-1)
ci_lower = x_bar - t_star*s/sqrt(n)
ci_upper = x_bar + t_star*s/sqrt(n)
c(ci_lower, ci_upper)

#FIBRINOGEN 5DAYS - CP
ad.test(data_cp_table3$day5followupfibrinogenmgdl)
describe(data_cp_table3$day5followupfibrinogenmgdl)
x_bar = mean(na.omit(data_cp_table3$day5followupfibrinogenmgdl))
n = length(na.omit(data_cp_table3$day5followupfibrinogenmgdl))
s = sd(na.omit(data_cp_table3$day5followupfibrinogenmgdl))
t_star = qt(0.95, n-1)
ci_lower = x_bar - t_star*s/sqrt(n)
ci_upper = x_bar + t_star*s/sqrt(n)
c(ci_lower, ci_upper)

#Comparing both
t.test(data_rs_table3$day5followupfibrinogenmgdl,data_cp_table3$day5followupfibrinogenmgdl,conf.level=.95)

####  FIBRINOGEN 5DAYS

#FIBRINOGEN 8DAYS - RS
ad.test(data_rs_table3$day8followupfibrinogenmgdl)
describe(data_rs_table3$day8followupfibrinogenmgdl)
x_bar = mean(na.omit(data_rs_table3$day8followupfibrinogenmgdl))
n = length(data_rs_table3$day8followupfibrinogenmgdl)
s = sd(na.omit(data_rs_table3$day8followupfibrinogenmgdl))
t_star = qt(0.95, n-1)
ci_lower = x_bar - t_star*s/sqrt(n)
ci_upper = x_bar + t_star*s/sqrt(n)
c(ci_lower, ci_upper)

#FIBRINOGEN 8DAYS - CP
ad.test(data_cp_table3$day8followupfibrinogenmgdl)
describe(data_cp_table3$day8followupfibrinogenmgdl)
x_bar = mean(na.omit(data_cp_table3$day8followupfibrinogenmgdl))
n = length(na.omit(data_cp_table3$day8followupfibrinogenmgdl))
s = sd(na.omit(data_cp_table3$day8followupfibrinogenmgdl))
t_star = qt(0.95, n-1)
ci_lower = x_bar - t_star*s/sqrt(n)
ci_upper = x_bar + t_star*s/sqrt(n)
c(ci_lower, ci_upper)

#Comparing both
t.test(data_rs_table3$day8followupfibrinogenmgdl,data_cp_table3$day8followupfibrinogenmgdl,conf.level=.95)

####  FIBRINOGEN 5DAYS

#FIBRINOGEN 15DAYS - RS
ad.test(data_rs_table3$day15followupfibrinogenmgdl)
describe(data_rs_table3$day15followupfibrinogenmgdl)
x_bar = mean(na.omit(data_rs_table3$day15followupfibrinogenmgdl))
n = length(data_rs_table3$day15followupfibrinogenmgdl)
s = sd(na.omit(data_rs_table3$day15followupfibrinogenmgdl))
t_star = qt(0.95, n-1)
ci_lower = x_bar - t_star*s/sqrt(n)
ci_upper = x_bar + t_star*s/sqrt(n)
c(ci_lower, ci_upper)

#FIBRINOGEN 15DAYS - CP
ad.test(data_cp_table3$day15followupfibrinogenmgdl)
describe(data_cp_table3$day15followupfibrinogenmgdl)
x_bar = mean(na.omit(data_cp_table3$day15followupfibrinogenmgdl))
n = length(na.omit(data_cp_table3$day15followupfibrinogenmgdl))
s = sd(na.omit(data_cp_table3$day15followupfibrinogenmgdl))
t_star = qt(0.95, n-1)
ci_lower = x_bar - t_star*s/sqrt(n)
ci_upper = x_bar + t_star*s/sqrt(n)
c(ci_lower, ci_upper)

t.test(data_rs_table3$day15followupfibrinogenmgdl,data_cp_table3$day15followupfibrinogenmgdl,conf.level=.95)

######################################################
#MIXED MODELS ANALYSIS
######################################################
# PLATELETS
platelets_analysis_data <- melt(as.data.frame(platelets), id=c("id","group"))

##GroupsXGroups comparison
#baselinecomparison<-subset(nlmedata,moments=="Baseline#")
#summary(baselinecomparison)
#library(nortest)
#bartlett.test(outcome, groups)
#fit <- aov(outcome ~ groups, data=baselinecomparison)
#summary(fit)


##NLME PAckage
nlmedata<-NULL
nlmedata$subject<-as.factor(platelets_analysis_data$id)
nlmedata$outcome<-platelets_analysis_data$value
nlmedata$moments<-as.factor(platelets_analysis_data$variable)
nlmedata$groups<-as.factor(platelets_analysis_data$group)

#REPEATED MEASURES
#aov.out = aov(outcome ~ groups + Error(subject/moments), data=nlmedata)
#summary(aov.out)
#nlmedata<-na.omit(nlmedata)
#with(nlmedata, t.test(outcome, groups,p.adjust.method="holm", paired=F))

nlmedata<-na.omit(nlmedata)
m1.lme4 = lmer(outcome ~ groups*moments + (1|subject),data = nlmedata,REML=FALSE)
#m1.nlme = lme(outcome ~ moments*groups,
#              random = ~ 1|subject/moments/groups,
#              data = nlmedata)
#intervals(m1.lme4)
summary(m1.lme4)
anova(m1.lme4)
#generating coefficients of whatever
coefs <- data.frame(coef(summary(m1.lme4)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
# get Satterthwaite-approximated degrees of freedom
coefs$df.Satt <- coef(summary(m1.lme4))[, 3]
# get approximate p-values
coefs$p.Satt <- coef(summary(m1.lme4))[, 5]
coefs
difflsmeans(m1.lme4)
plot(difflsmeans(m1.lme4, test.effs="groups:moments"))
lsmeans(m1.lme4)
confint(m1.lme4,level=0.95)
lmmpower(m1.lme4, pct.change = 0.30, t = seq(0,5,1), power = 0.80)

m1.lme4_2 = lmer(outcome ~ groups + moments + (1|subject),data = nlmedata,REML=FALSE)

m1.lme4_3 = lmer(outcome ~ groups + (1|subject),data = nlmedata,REML=FALSE)

anova(m1.lme4,m1.lme4_2,m1.lme4_3)

lmmpower(m1.lme4, pct.change = 0.40, t = seq(0,3,1), power = 0.80)


# FIBRINOGEN
fibrinogn_analysis_data <- melt(as.data.frame(fibrinogen), id=c("id","group"))

##GroupsXGroups comparison
#baselinecomparison<-subset(nlmedata,moments=="Baseline#")
#summary(baselinecomparison)
#library(nortest)
#bartlett.test(outcome, groups)
#fit <- aov(outcome ~ groups, data=baselinecomparison)
#summary(fit)
#kruskal.test(outcome ~ groups, data = baselinecomparison) 

##NLME PAckage
nlmedata<-NULL
nlmedata$subject<-as.factor(fibrinogn_analysis_data$id)
nlmedata$outcome<-fibrinogn_analysis_data$value
nlmedata$moments<-as.factor(fibrinogn_analysis_data$variable)
nlmedata$groups<-as.factor(fibrinogn_analysis_data$group)

m1.lme4 = lmer(outcome ~ moments*groups + (1|subject), data = nlmedata,REML=FALSE)
#m1.nlme = lme(outcome ~ moments*groups,
#              random = ~ 1|subject/moments/groups,
#              data = nlmedata)
#intervals(m1.lme4)
summary(m1.lme4)
anova(m1.lme4)
#generating coefficients of whatever
coefs <- data.frame(coef(summary(m1.lme4)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
# get Satterthwaite-approximated degrees of freedom
coefs$df.Satt <- coef(summary(m1.lme4))[, 3]
# get approximate p-values
coefs$p.Satt <- coef(summary(m1.lme4))[, 5]
coefs
difflsmeans(m1.lme4)
plot(difflsmeans(m1.lme4, test.effs="groups:moments"))
lsmeans(m1.lme4)
lmmpower(m1.lme4, pct.change = 0.30, t = seq(0,5,1), power = 0.80)


######################################################
#GRAPHING
######################################################
#PLATELETS
platelets<-as.data.frame(platelets)
#platelets<-na.omit(platelets)
platelets_rs<- c(
	with(subset(platelets,platelets$group=="Rattlesnake"),
		mean(baseline)),
    with(subset(platelets,platelets$group=="Rattlesnake"),
    	mean(na.omit(T1))),
	with(subset(platelets,platelets$group=="Rattlesnake"),
		mean(na.omit(T2))),
	with(subset(platelets,platelets$group=="Rattlesnake"),
		mean(na.omit(T3))),
	with(subset(platelets,platelets$group=="Rattlesnake"),
		mean(na.omit(T4))))

platelets_cp<- c(
	with(subset(platelets,platelets$group=="Copperhead"),
		mean(baseline)),
    with(subset(platelets,platelets$group=="Copperhead"),
    	mean(na.omit(T1))),
	with(subset(platelets,platelets$group=="Copperhead"),
		mean(na.omit(T2))),
	with(subset(platelets,platelets$group=="Copperhead"),
		mean(na.omit(T3))),
	with(subset(platelets,platelets$group=="Copperhead"),
		mean(na.omit(T4))))

tiff("/Users/jnv4/Desktop/platelets.tiff", width = 700,
 height = 500,compression = 'lzw')
	plot(platelets_cp, type="o", col="grey50", ylim=c(80,400),
		xlim=c(0.7,5.5),axes=FALSE, ann=FALSE)
	# Make x axis using tests labels
	axis(1, at=1:5, lab=c("Baseline","Nadir","5 days" ,
		"8 days","15 days"))
	axis(2, at=seq(from = 100, to = 400, by = 30))
	# Create box around plot
	#box()
	title(xlab="Follow Up")
	title(ylab=expression("Platelets" ~ 10^{9} ~ "/L")) #, col.lab=rgb(0,0.5,0)
	points(c(rep(seq(0.8,1.2,0.05),10),0.8,1.0,1.2),
		subset(platelets$baseline,platelets$group=="Rattlesnake"),
		col='grey50')
	points(c(rep(seq(0.8,1.2,0.05),2),0.8,1.0,1.2,1.0),
		subset(platelets$baseline,platelets$group=="Copperhead"),
		col='black')
	points(c(rep(seq(1.8,2.2,0.05),10),1.8,2.2,2.0),
		subset(platelets$T1,platelets$group=="Rattlesnake"),
		col='grey50')
	points(c(rep(seq(1.8,2.2,0.05),2),1.8,2.2,2.0,1.8),
		subset(platelets$T1,platelets$group=="Copperhead"),
		col='black')
	points(c(rep(seq(2.8,3.2,0.05),10),2.8,3.2,3.0),
		subset(platelets$T2,platelets$group=="Rattlesnake"),
		col='grey50')
	points(c(rep(seq(2.8,3.2,0.05),2),2.8,3.2,3.0,2.8),
		subset(platelets$T2,platelets$group=="Copperhead"),
		col='black')
	points(c(rep(seq(3.8,4.2,0.05),10),3.8,4.2,4.0),
		subset(platelets$T3,platelets$group=="Rattlesnake"),
		col='grey50')
	points(c(rep(seq(3.8,4.2,0.05),2),3.8,4.2,4.0,3.8),
		subset(platelets$T3,platelets$group=="Copperhead"),
		col='black')
	points(c(rep(seq(4.8,5.2,0.05),10),4.8,5.2,5.0),
		subset(platelets$T4,platelets$group=="Rattlesnake"),
		col='grey50')
	points(c(rep(seq(4.8,5.2,0.05),2),4.8,5.2,5.0,4.8),
		subset(platelets$T4,platelets$group=="Copperhead"),
		col='black')
	lines(platelets_rs, type="o", pch=22, lty=4,col="black")
	legend(4.5, 120,c("Other Crotaline","Copperhead"), cex=0.8,
		col=c("grey50","black"), pch=21:24, lty=1:4,bg="white")
dev.off()

#FIBRINOGEN
fibrinogen<-as.data.frame(fibrinogen)
#fibrinogen<-na.omit(fibrinogen)
fibrinogen_rs<- c(
	with(subset(fibrinogen,fibrinogen$group=="Rattlesnake"),
		mean(na.omit(baseline))),
    with(subset(fibrinogen,fibrinogen$group=="Rattlesnake"),
    	mean(na.omit(T1))),
	with(subset(fibrinogen,fibrinogen$group=="Rattlesnake"),
		mean(na.omit(T2))),
	with(subset(fibrinogen,fibrinogen$group=="Rattlesnake"),
		mean(na.omit(T3))),
	with(subset(fibrinogen,fibrinogen$group=="Rattlesnake"),
		mean(na.omit(T4))))

fibrinogen_cp<- c(
	with(subset(fibrinogen,fibrinogen$group=="Copperhead"),
		mean(na.omit(baseline))),
    with(subset(fibrinogen,fibrinogen$group=="Copperhead"),
    	mean(na.omit(T1))),
	with(subset(fibrinogen,fibrinogen$group=="Copperhead"),
		mean(na.omit(T2))),
	with(subset(fibrinogen,fibrinogen$group=="Copperhead"),
		mean(na.omit(T3))),
	with(subset(fibrinogen,fibrinogen$group=="Copperhead"),
		mean(na.omit(T4))))

tiff("/Users/jnv4/Desktop/fibrinogen.tiff", width = 700,
 height = 500,compression = 'lzw')
plot(fibrinogen_rs, type="o", col="grey50", ylim=c(100,600),
	xlim=c(0.8,5.3),axes=FALSE, ann=FALSE)
# Make x axis using tests labels
axis(1, at=1:5, lab=c("Baseline","Nadir","5 days" ,"8 days","15 days"))
axis(2, at=seq(from = 150, to = 570, by = 20))
# Create box around plot
#box()
title(xlab="Follow Up")
title(ylab="Fibrinogen mg/dL") #, col.lab=rgb(0,0.5,0)
points(c(rep(seq(0.8,1.2,0.05),10),0.8,1.0,1.2),
	subset(fibrinogen$baseline,fibrinogen$group=="Rattlesnake"),
	col='grey50')
points(c(rep(seq(0.8,1.2,0.05),2),0.8,1.0,1.2,1.0),
	subset(fibrinogen$baseline,fibrinogen$group=="Copperhead"),
	col='black')
points(c(rep(seq(1.8,2.2,0.05),10),1.8,2.2,2.0),
	subset(fibrinogen$T1,fibrinogen$group=="Rattlesnake"),
	col='grey50')
points(c(rep(seq(1.8,2.2,0.05),2),1.8,2.2,2.0,1.8),
	subset(fibrinogen$T1,fibrinogen$group=="Copperhead"),
	col='black')
points(c(rep(seq(2.8,3.2,0.05),10),2.8,3.2,3.0),
	subset(fibrinogen$T2,fibrinogen$group=="Rattlesnake"),
	col='grey50')
points(c(rep(seq(2.8,3.2,0.05),2),2.8,3.2,3.0,2.8),
	subset(fibrinogen$T2,fibrinogen$group=="Copperhead"),
	col='black')
points(c(rep(seq(3.8,4.2,0.05),10),3.8,4.2,4.0),
	subset(fibrinogen$T3,fibrinogen$group=="Rattlesnake"),
	col='grey50')
points(c(rep(seq(3.8,4.2,0.05),2),3.8,4.2,4.0,3.8),
	subset(fibrinogen$T3,fibrinogen$group=="Copperhead"),
	col='black')
points(c(rep(seq(4.8,5.2,0.05),10),4.8,5.2,5.0),
	subset(fibrinogen$T4,fibrinogen$group=="Rattlesnake"),
	col='grey50')
points(c(rep(seq(4.8,5.2,0.05),2),4.8,5.2,5.0,4.8),
	subset(fibrinogen$T4,fibrinogen$group=="Copperhead"),
	col='black')
lines(fibrinogen_cp, type="o", pch=22, col="black",lty=4)
legend(4, 180, c("Other Crotaline","Copperhead"), cex=0.8,
	col=c("grey50","black"), pch=21:24, lty=1:4,bg="white")
dev.off()

###### DID NOT USE FOR THE PAPER ##################
######### GRAPH WITH SD VALUES
#PLATELETS
# plot(platelets_cp, type="o", col="red", ylim=c(100,350),axes=FALSE, ann=FALSE)
# lines(platelets_rs, type="o", pch=22, lty=2, col="black")
# Make x axis using tests labels
# axis(1, at=1:5, lab=c("Baseline","Nadir","5 days" ,"8 days","15 days"))
# axis(2, at=seq(from = 100, to = 350, by = 20))
# Create box around plot
#box()
# title(xlab="Follow Up")
# title(ylab="Platelets") #, col.lab=rgb(0,0.5,0)
# legend(4, 130, c("Rattlesnake","Copperhead"), cex=0.8,col=c("black","red"), pch=21:24, lty=1:4)
# segments(1,mean(platelets$baseline[platelets$group=="Rattlesnake"]),1,(mean(platelets$baseline[platelets$group=="Rattlesnake"])-sd(platelets$baseline[platelets$group=="Rattlesnake"])),col='black')
# segments(0.95,(mean(platelets$baseline[platelets$group=="Rattlesnake"])-sd(platelets$baseline[platelets$group=="Rattlesnake"])),1.05,(mean(platelets$baseline[platelets$group=="Rattlesnake"])-sd(platelets$baseline[platelets$group=="Rattlesnake"])),col='black')
# segments(1,mean(platelets$baseline[platelets$group=="Copperhead"]),1,(mean(platelets$baseline[platelets$group=="Copperhead"])+sd(platelets$baseline[platelets$group=="Copperhead"])),col='red')
# segments(0.95,(mean(platelets$baseline[platelets$group=="Copperhead"])+sd(platelets$baseline[platelets$group=="Copperhead"])),1.05,(mean(platelets$baseline[platelets$group=="Copperhead"])+sd(platelets$baseline[platelets$group=="Copperhead"])),col='red')

# segments(2,mean(platelets$T1[platelets$group=="Rattlesnake"]),2,(mean(platelets$T1[platelets$group=="Rattlesnake"])-sd(platelets$T1[platelets$group=="Rattlesnake"])),col='black')
# segments(1.95,(mean(platelets$T1[platelets$group=="Rattlesnake"])-sd(platelets$T1[platelets$group=="Rattlesnake"])),2.05,(mean(platelets$T1[platelets$group=="Rattlesnake"])-sd(platelets$T1[platelets$group=="Rattlesnake"])),col='black')
# segments(2,mean(platelets$T1[platelets$group=="Copperhead"]),2,(mean(platelets$T1[platelets$group=="Copperhead"])+sd(platelets$T1[platelets$group=="Copperhead"])),col='red')
# segments(1.95,(mean(platelets$T1[platelets$group=="Copperhead"])+sd(platelets$T1[platelets$group=="Copperhead"])),2.05,(mean(platelets$T1[platelets$group=="Copperhead"])+sd(platelets$T1[platelets$group=="Copperhead"])),col='red')

# segments(3,mean(platelets$T2[platelets$group=="Rattlesnake"]),3,(mean(platelets$T2[platelets$group=="Rattlesnake"])-sd(platelets$T2[platelets$group=="Rattlesnake"])),col='black')
# segments(2.95,(mean(platelets$T2[platelets$group=="Rattlesnake"])-sd(platelets$T2[platelets$group=="Rattlesnake"])),3.05,(mean(platelets$T2[platelets$group=="Rattlesnake"])-sd(platelets$T2[platelets$group=="Rattlesnake"])),col='black')
# segments(3,mean(platelets$T2[platelets$group=="Copperhead"]),3,(mean(platelets$T2[platelets$group=="Copperhead"])+sd(platelets$T2[platelets$group=="Copperhead"])),col='red')
# segments(2.95,(mean(platelets$T2[platelets$group=="Copperhead"])+sd(platelets$T2[platelets$group=="Copperhead"])),3.05,(mean(platelets$T2[platelets$group=="Copperhead"])+sd(platelets$T2[platelets$group=="Copperhead"])),col='red')

# segments(4,mean(platelets$T3[platelets$group=="Rattlesnake"]),4,(mean(platelets$T3[platelets$group=="Rattlesnake"])-sd(platelets$T3[platelets$group=="Rattlesnake"])),col='black')
# segments(3.95,(mean(platelets$T3[platelets$group=="Rattlesnake"])-sd(platelets$T3[platelets$group=="Rattlesnake"])),4.05,(mean(platelets$T3[platelets$group=="Rattlesnake"])-sd(platelets$T3[platelets$group=="Rattlesnake"])),col='black')
# segments(4,mean(platelets$T3[platelets$group=="Copperhead"]),4,(mean(platelets$T3[platelets$group=="Copperhead"])+sd(platelets$T3[platelets$group=="Copperhead"])),col='red')
# segments(3.95,(mean(platelets$T3[platelets$group=="Copperhead"])+sd(platelets$T3[platelets$group=="Copperhead"])),4.05,(mean(platelets$T3[platelets$group=="Copperhead"])+sd(platelets$T3[platelets$group=="Copperhead"])),col='red')

# segments(5,mean(platelets$T4[platelets$group=="Rattlesnake"]),5,(mean(platelets$T4[platelets$group=="Rattlesnake"])-sd(platelets$T4[platelets$group=="Rattlesnake"])),col='black')
# segments(4.95,(mean(platelets$T4[platelets$group=="Rattlesnake"])-sd(platelets$T4[platelets$group=="Rattlesnake"])),5.05,(mean(platelets$T4[platelets$group=="Rattlesnake"])-sd(platelets$T4[platelets$group=="Rattlesnake"])),col='black')
# segments(5,mean(platelets$T4[platelets$group=="Copperhead"]),5,(mean(platelets$T4[platelets$group=="Copperhead"])+sd(platelets$T4[platelets$group=="Copperhead"])),col='red')
# segments(4.95,(mean(platelets$T4[platelets$group=="Copperhead"])+sd(platelets$T4[platelets$group=="Copperhead"])),5.05,(mean(platelets$T4[platelets$group=="Copperhead"])+sd(platelets$T4[platelets$group=="Copperhead"])),col='red')

######### GRAPH WITH SD VALUES
plot(fibrinogen_rs, type="o", col="black", ylim=c(100,500),axes=FALSE, ann=FALSE)
lines(fibrinogen_cp, type="o", pch=22, lty=2, col="red")
# Make x axis using tests labels
axis(1, at=1:5, lab=c("Baseline","Nadir","5 days" ,"8 days","15 days"))
axis(2, at=seq(from = 100, to = 500, by = 30))
# Create box around plot
#box()
title(xlab="Follow Up")
title(ylab="Fibrinogen") #, col.lab=rgb(0,0.5,0)
legend(4, 180, c("Rattlesnake","Copperhead"), cex=0.8,col=c("black","red"), pch=21:24, lty=1:4)
segments(1,mean(fibrinogen$baseline[fibrinogen$group=="Rattlesnake"]),1,(mean(fibrinogen$baseline[fibrinogen$group=="Rattlesnake"])-sd(fibrinogen$baseline[fibrinogen$group=="Rattlesnake"])),col='black')
segments(0.95,(mean(fibrinogen$baseline[fibrinogen$group=="Rattlesnake"])-sd(fibrinogen$baseline[fibrinogen$group=="Rattlesnake"])),1.05,(mean(fibrinogen$baseline[fibrinogen$group=="Rattlesnake"])-sd(fibrinogen$baseline[fibrinogen$group=="Rattlesnake"])),col='black')
segments(1,mean(fibrinogen$baseline[fibrinogen$group=="Copperhead"]),1,(mean(fibrinogen$baseline[fibrinogen$group=="Copperhead"])+sd(fibrinogen$baseline[fibrinogen$group=="Copperhead"])),col='red')
segments(0.95,(mean(fibrinogen$baseline[fibrinogen$group=="Copperhead"])+sd(fibrinogen$baseline[fibrinogen$group=="Copperhead"])),1.05,(mean(fibrinogen$baseline[fibrinogen$group=="Copperhead"])+sd(fibrinogen$baseline[fibrinogen$group=="Copperhead"])),col='red')

segments(2,mean(fibrinogen$T1[fibrinogen$group=="Rattlesnake"]),2,(mean(fibrinogen$T1[fibrinogen$group=="Rattlesnake"])-sd(fibrinogen$T1[fibrinogen$group=="Rattlesnake"])),col='black')
segments(1.95,(mean(fibrinogen$T1[fibrinogen$group=="Rattlesnake"])-sd(fibrinogen$T1[fibrinogen$group=="Rattlesnake"])),2.05,(mean(fibrinogen$T1[fibrinogen$group=="Rattlesnake"])-sd(fibrinogen$T1[fibrinogen$group=="Rattlesnake"])),col='black')
segments(2,mean(fibrinogen$T1[fibrinogen$group=="Copperhead"]),2,(mean(fibrinogen$T1[fibrinogen$group=="Copperhead"])+sd(fibrinogen$T1[fibrinogen$group=="Copperhead"])),col='red')
segments(1.95,(mean(fibrinogen$T1[fibrinogen$group=="Copperhead"])+sd(fibrinogen$T1[fibrinogen$group=="Copperhead"])),2.05,(mean(fibrinogen$T1[fibrinogen$group=="Copperhead"])+sd(fibrinogen$T1[fibrinogen$group=="Copperhead"])),col='red')

segments(3,mean(fibrinogen$T2[fibrinogen$group=="Rattlesnake"]),3,(mean(fibrinogen$T2[fibrinogen$group=="Rattlesnake"])-sd(fibrinogen$T2[fibrinogen$group=="Rattlesnake"])),col='black')
segments(2.95,(mean(fibrinogen$T2[fibrinogen$group=="Rattlesnake"])-sd(fibrinogen$T2[fibrinogen$group=="Rattlesnake"])),3.05,(mean(fibrinogen$T2[fibrinogen$group=="Rattlesnake"])-sd(fibrinogen$T2[fibrinogen$group=="Rattlesnake"])),col='black')
segments(3,mean(fibrinogen$T2[fibrinogen$group=="Copperhead"]),3,(mean(fibrinogen$T2[fibrinogen$group=="Copperhead"])+sd(fibrinogen$T2[fibrinogen$group=="Copperhead"])),col='red')
segments(2.95,(mean(fibrinogen$T2[fibrinogen$group=="Copperhead"])+sd(fibrinogen$T2[fibrinogen$group=="Copperhead"])),3.05,(mean(fibrinogen$T2[fibrinogen$group=="Copperhead"])+sd(fibrinogen$T2[fibrinogen$group=="Copperhead"])),col='red')

segments(4,mean(fibrinogen$T3[fibrinogen$group=="Rattlesnake"]),4,(mean(fibrinogen$T3[fibrinogen$group=="Rattlesnake"])-sd(fibrinogen$T3[fibrinogen$group=="Rattlesnake"])),col='black')
segments(3.95,(mean(fibrinogen$T3[fibrinogen$group=="Rattlesnake"])-sd(fibrinogen$T3[fibrinogen$group=="Rattlesnake"])),4.05,(mean(fibrinogen$T3[fibrinogen$group=="Rattlesnake"])-sd(fibrinogen$T3[fibrinogen$group=="Rattlesnake"])),col='black')
segments(4,mean(fibrinogen$T3[fibrinogen$group=="Copperhead"]),4,(mean(fibrinogen$T3[fibrinogen$group=="Copperhead"])+sd(fibrinogen$T3[fibrinogen$group=="Copperhead"])),col='red')
segments(3.95,(mean(fibrinogen$T3[fibrinogen$group=="Copperhead"])+sd(fibrinogen$T3[fibrinogen$group=="Copperhead"])),4.05,(mean(fibrinogen$T3[fibrinogen$group=="Copperhead"])+sd(fibrinogen$T3[fibrinogen$group=="Copperhead"])),col='red')

segments(5,mean(fibrinogen$T4[fibrinogen$group=="Rattlesnake"]),5,(mean(fibrinogen$T4[fibrinogen$group=="Rattlesnake"])-sd(fibrinogen$T4[fibrinogen$group=="Rattlesnake"])),col='black')
segments(4.95,(mean(fibrinogen$T4[fibrinogen$group=="Rattlesnake"])-sd(fibrinogen$T4[fibrinogen$group=="Rattlesnake"])),5.05,(mean(fibrinogen$T4[fibrinogen$group=="Rattlesnake"])-sd(fibrinogen$T4[fibrinogen$group=="Rattlesnake"])),col='black')
segments(5,mean(fibrinogen$T4[fibrinogen$group=="Copperhead"]),5,(mean(fibrinogen$T4[fibrinogen$group=="Copperhead"])+sd(fibrinogen$T4[fibrinogen$group=="Copperhead"])),col='red')
segments(4.95,(mean(fibrinogen$T4[fibrinogen$group=="Copperhead"])+sd(fibrinogen$T4[fibrinogen$group=="Copperhead"])),5.05,(mean(fibrinogen$T4[fibrinogen$group=="Copperhead"])+sd(fibrinogen$T4[fibrinogen$group=="Copperhead"])),col='red')


######### GRAPH WITH IC VALUES
#PLATELETS
plot(platelets_cp, type="o", col="red", ylim=c(150,350),axes=FALSE, ann=FALSE)
lines(platelets_rs, type="o", pch=22, lty=2, col="black")
# Make x axis using tests labels
axis(1, at=1:5, lab=c("Baseline","Nadir","5 days" ,"8 days","15 days"))
axis(2, at=seq(from = 150, to = 350, by = 30))
# Create box around plot
#box()
title(xlab="Follow Up")
title(ylab="Platelets") #, col.lab=rgb(0,0.5,0)
legend(4, 130, c("Rattlesnake","Copperhead"), cex=0.8,col=c("black","red"), pch=21:24, lty=1:4)
#Baseline Lines
segments(1,167.25,1,201.74,col='black')
segments(0.95,167.25,1.05,167.25,col='black')
segments(0.95,201.74,1.05,201.74,col='black')

segments(1,215.02,1,277.45,col='red')
segments(0.95,215.02,1.05,215.02,col='red')
segments(0.95,277.45,1.05,277.45,col='red')

#Nadir Lines
segments(2,187.49,2,207.99,col='black')
segments(1.95,187.49,2.05,187.49,col='black')
segments(1.95,207.99,2.05,207.99,col='black')

segments(2,190.82,2,237.77,col='red')
segments(1.95,190.82,2.05,190.82,col='red')
segments(1.95,237.77,2.05,237.77,col='red')

#T1
segments(3,232.88,3,261.82,col='black')
segments(2.95,232.88,3.05,232.88,col='black')
segments(2.95,261.82,3.05,261.82,col='black')

segments(3,232.90,3,291.45,col='red')
segments(2.95,232.90,3.05,232.90,col='red')
segments(2.95,291.45,3.05,291.45,col='red')

#T2
segments(4,235.65,4,267.78,col='black')
segments(3.95,235.65,4.05,235.65,col='black')
segments(3.95,267.78,4.05,267.78,col='black')

segments(4,246.21,4,304.26,col='red')
segments(3.95,246.21,4.05,246.21,col='red')
segments(3.95,304.26,4.05,304.26,col='red')

#T3
segments(5,289.17,5,330.25,col='black')
segments(4.95,289.17,5.05,289.17,col='black')
segments(4.95,330.25,5.05,330.25,col='black')

segments(5,255.33,5,311.49,col='red')
segments(4.95,255.33,5.05,255.33,col='red')
segments(4.95,311.49,5.05,311.49,col='red')

###### FIBROGEN ##########
plot(fibrinogen_rs, type="o", col="black", ylim=c(150,500),axes=FALSE, ann=FALSE)
lines(fibrinogen_cp, type="o", pch=22, lty=2, col="red")
# Make x axis using tests labels
axis(1, at=1:5, lab=c("Baseline","Nadir","5 days" ,"8 days","15 days"))
axis(2, at=seq(from = 150, to = 500, by = 30))
# Create box around plot
#box()
title(xlab="Follow Up")
title(ylab="Fibrinogen") #, col.lab=rgb(0,0.5,0)
legend(4, 200, c("Rattlesnake","Copperhead"), cex=0.8,col=c("black","red"), pch=21:24, lty=1:4)
#Baseline Lines
segments(1,241.37,1,281.19,col='black')
segments(0.95,241.37,1.05,241.37,col='black')
segments(0.95,281.19,1.05,281.19,col='black')

segments(1,276.59,1,414.64,col='red')
segments(0.95,276.59,1.05,276.59,col='red')
segments(0.95,414.64,1.05,414.64,col='red')

#Nadir Lines
segments(2,195.35,2,234.009,col='black')
segments(1.95,195.35,2.05,195.35,col='black')
segments(1.95,234.009,2.05,234.009,col='black')

segments(2,267.93,2,359.15,col='red')
segments(1.95,267.93,2.05,267.93,col='red')
segments(1.95,359.15,2.05,359.15,col='red')

#T1
segments(3,359.98,3,397.06,col='black')
segments(2.95,359.98,3.05,359.98,col='black')
segments(2.95,397.06,3.05,397.06,col='black')

segments(3,342.92,3,445.24,col='red')
segments(2.95,342.92,3.05,342.92,col='red')
segments(2.95,445.24,3.05,445.24,col='red')

#T2
segments(4,341.04,4,377.77,col='black')
segments(3.95,341.04,4.05,341.04,col='black')
segments(3.95,377.77,4.05,377.77,col='black')

segments(4,316.19,4,403.50,col='red')
segments(3.95,316.19,4.05,316.19,col='red')
segments(3.95,403.50,4.05,403.50,col='red')

#T3
segments(5,301.27,5,329.02,col='black')
segments(4.95,301.27,5.05,301.27,col='black')
segments(4.95,329.02,5.05,329.02,col='black')

segments(5,290.91,5,347.55,col='red')
segments(4.95,290.91,5.05,290.91,col='red')
segments(4.95,347.55,5.05,347.55,col='red')