

library(haven) # haven package now available on cran

data_stata <- read_dta("/Users/Joao/Downloads/EPS_Combined_24Mar2019.dta")

data <- read.csv("/Users/Joao/Downloads/EPS_Individual_Questionnaire_v17_ksd_hseek_rpt.csv")

district_data<-read.csv("/Users/Joao/Box/Home Folder jnv4/Data/DGNN/SOSAS/SOSAS_gis/districts_gis_uganda.csv")

data_stata<-subset(data_stata,data_stata$consent_obtained_prelim==1)

data_stata<-subset(data_stata,data_stata$EIQeligible==1)

library(dplyr)
data_subset<-subset(data,data$PARENT_KEY %in% data_stata$EIQKEY)


data$age_cat<-car::recode(data$age,"0:5='Under 5';
								6:18='Under 15';
								19:35='Under 35';
								36:60='Under 60';
								60:108='Above 60'")

data_age1<-subset(data,data$age_cat=="Under 5")
data_age2<-subset(data,data$age_cat=="Under 15")
data_age3<-subset(data,data$age_cat=="Under 35")
data_age4<-subset(data,data$age_cat=="Under 60")
data_age5<-subset(data,data$age_cat=="Above 60")

prop.table(table(data$EIQeligible))
names(data)

prevalence1<-prop.table(table(data$elig,data$level2),2)[2,]*100
prevalence2<-prop.table(table(data$EIQeligible,data$level2),2)[2,]*100
district<-names(prevalence1)

lvl1_prev<-data.frame(prevalence1,prevalence2,district)

lvl1_prev$prevalenceage1<-prop.table(table(data_age1$EIQeligible,data_age1$level2),2)[2,]*100
lvl1_prev$prevalenceage2<-prop.table(table(data_age2$EIQeligible,data_age2$level2),2)[2,]*100
lvl1_prev$prevalenceage3<-prop.table(table(data_age3$EIQeligible,data_age3$level2),2)[2,]*100
lvl1_prev$prevalenceage4<-prop.table(table(data_age4$EIQeligible,data_age4$level2),2)[2,]*100
lvl1_prev$prevalenceage5<-prop.table(table(data_age5$EIQeligible,data_age5$level2),2)[2,]*100


write.csv(lvl1_prev,"/Users/Joao/Desktop/epilepsyprevalence.csv")

data_NA<-data[-is.na(data$EIQweight)==FALSE,]


library(survey)
data_weighted <- svydesign(ids = ~1, 
                     data = data_NA,
                     weights = data_NA$EIQweight)

table(data_weighted$EIQeligible)
svytable(~EIQeligible, design = data_weighted)
with(data_weighted,table(method_injury_recoded,country))
svytable(~method_injury_recoded + country, design = data_weighted_full_weighted)
prop.table(svytable(~EIQeligible, design = data_weighted))
#metanalysis




fit.logistic=glm(survived~ sex + pclass + embarked, family=binomial,
data = titanic)
prLogisticBootCond(fit.logistic, data = titanic)


data_epilepsy<-subset(data,data$EIQeligible==1)

#Sought care 1 = yes; 2 = no 3?
table(data_subset$hspos)

#what kind of care; biom, th, pastoral
table(data_subset$hsk_grp.consult)

#legs to bc?
table(data$bc_type)

table(data_subset$bc_seen)

#time to bc
#need to subset bu bc only

data_subset_bmc<-subset(data_subset,data_subset$hsk_grp.consult=="biomedical_care")


summary(data_subset_bmc$sought_time)
sd(data_subset_bmc$sought_time)

describe(age_victims)
with(data_subset,describeBy(sought_time,hsk_grp.consult))
# t-test: # independent 2-group, 2 level IV
testName <- with(data_subset,aov(sought_time ~ hsk_grp.consult))
summary(testName)



table(data_epilepsy$ths)
table(data_epilepsy$biomedical_sought)
table(data_epilepsy$biomedical_sought_yes_b)
table(data_epilepsy$biomedical_sought_yes_c)
table(data_epilepsy$biomedical_sought_yes_d)
table(data_epilepsy$biomedical_sought_yes_e)

th_error
ths
pastors
pastor_error
biomedical_sought





