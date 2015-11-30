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
lapply(c("sem","ggplot2", "psych", "RCurl", "irr", "nortest", "moments","GPArotation","nFactors","boot","psy", "car","vcd", "gridExtra","mi","VIM","epicalc","gdata","sqldf","reshape2","mclust","foreign","survival","memisc","lme4","lmerTest","dplyr","qgraph"),library, character.only=T)

#Package and codes to pull data from goodle sheets
#devtools::install_github("jennybc/googlesheets")
#library(googlesheets)
#suppressMessages(library(dplyr))

######################################################
#IMPORTING DATA
######################################################
#Pulling data from dropbox
#data_hamilton <- repmis::source_DropboxData("lena_hamilton.csv","r31zt5zeiygsc23",sep = ",",header = TRUE)

data<-read.csv("/home/joao/Dropbox/datasets/DGHI/safe_habits_tz/tz_safet_habits_data.csv",sep=',')

######################################################
#DATA MANAGEMENT
######################################################
safe_habits<-with(data,data.frame(safety_helmet_use,safety_helmet_use_colleague,safety_risk_driving,safety_headlight_use_day,safety_headlight_use_night,safety_purchase_helmet_after_use,safety_buckle_helmet,safety_belief_helmet_reduce_risk,safety_belief_helmetstraps_reduce_risk,helmet_cracks,helmet_scratch,helmet_broken_chin,helmet_face_shield,helmet_obscure_face_shield,helmet_fit))
safe_habits$safety_helmet_use<-car::recode(safe_habits$safety_helmet_use,"'Always'=1;else=0")
safe_habits$safety_helmet_use_colleague<-car::recode(safe_habits$safety_helmet_use_colleague,"'Always'=1;'Sometimes'=1;'Often'=1;else=0")
safe_habits$safety_risk_driving<-car::recode(data$safety_risk_driving,"'Always'=1;'Sometimes'=1;'Often'=1;else=0")
safe_habits$safety_headlight_use_day<-car::recode(safe_habits$safety_headlight_use_day,"'Always'=1;else=0")
safe_habits$safety_headlight_use_night<-car::recode(safe_habits$safety_headlight_use_night,"'Always'=1;else=0")
safe_habits$safety_purchase_helmet_after_use<-car::recode(safe_habits$safety_purchase_helmet_after_use,"'Always'=1;else=0")
safe_habits$safety_buckle_helmet<-car::recode(safe_habits$safety_buckle_helmet,"'Always'=1;else=0")
safe_habits$safety_belief_helmet_reduce_risk<-car::recode(safe_habits$safety_belief_helmet_reduce_risk,"'Agree'=1;'Strongly Agree'=1;else=0")
safe_habits$safety_belief_helmetstraps_reduce_risk<-car::recode(safe_habits$safety_belief_helmetstraps_reduce_risk,"'Agree'=1;'Strongly Agree'=1;else=0")
safe_habits$helmet_cracks<-car::recode(safe_habits$helmet_cracks,"'Yes'=1;'No'=0;else=NA")
safe_habits$helmet_scratch<-car::recode(safe_habits$helmet_scratch,"'Yes'=1;'No'=0;else=NA")
safe_habits$helmet_broken_chin<-car::recode(safe_habits$helmet_broken_chin,"'Yes'=1;'No'=0;else=NA")
safe_habits$helmet_face_shield<-car::recode(safe_habits$helmet_face_shield,"'Yes'=1;'No'=0;else=NA")
safe_habits$helmet_obscure_face_shield<-car::recode(safe_habits$helmet_obscure_face_shield,"'Yes'=1;'No'=0;else=NA")
safe_habits$helmet_fit<-car::recode(safe_habits$helmet_fit,"'Yes'=1;'No'=0;else=NA")

safe_habits_numeric <-lapply(safe_habits,function(x) as.numeric(as.character(x)))

reasons_danger<-with(data,data.frame(reason_trafficlane,reason_roadcond,reason_less_density,reason_regulation,reason_others_awareness,reason_walkways,reason_trainning,reason_lighting,reason_reflectorvests,reason_helmet,reason_education,reason_roadrules,reason_carefulness,reason_alcoholuse,reason_respectforBB,reason_reducespeed,	reason_roadsigns,	reason_widerroads,	reason_confidentdriving,	reason_inspectlicenses,	reason_properuseindicators,	reason_agelimit,	reason_policeaccountability,	reason_distraction,	reason_headlights,	reason_punishment,	reason_riskbehavior,	reason_vehiclecondition))

demographics<-with(data,data.frame(age,gender,alternative_transportation,hours_alternative_transportation))

work_experience<-with(data,data.frame(hours_work_onbodaboda,days_work_bodaboda,years_work_onbodaboda))

outcomes<-with(data,data.frame(rtc_involvement,injury,hospitalization,los,disability,out_of_work,nearmissmonth))
outcomes$nearmissmonth<-car::recode(outcomes$nearmissmonth,"0=0;1:4=1;else=1")
outcomes$rtc_involvement<-car::recode(outcomes$rtc_involvement,"'No'=0;'Yes'=1;else=0")
outcomes$rtc_involvement<-as.numeric(as.character(outcomes$rtc_involvement))
outcomes$hospitalization<-car::recode(data$hospitalization,"'No'='No';'Yes'='Yes';else=NA")
######################################################
#DESCRIPTIVE ANALYSIS
######################################################

##### DEMOGRAPHICS ###########

# Age
summary(demographics$age)
ad.test(demographics$age)
#hist(demographics$age)
#ci_func(demographics$age,.95)
by(demographics$age,outcomes$rtc_involvement,describe)
wilcox.test(demographics$age~outcomes$rtc_involvement)

# Experience (Years)
summary(work_experience$years_work_onbodaboda)
ad.test(work_experience$years_work_onbodaboda)
#hist(work_experience$years_work_onbodaboda)
#ci_func(work_experience$years_work_onbodaboda,.95)
by(work_experience$years_work_onbodaboda,outcomes$rtc_involvement,describe)
wilcox.test(work_experience$years_work_onbodaboda~outcomes$rtc_involvement)

# Hours of Work
summary(work_experience$hours_work_onbodaboda)
ad.test(work_experience$hours_work_onbodaboda)
#hist(work_experience$hours_work_onbodaboda)
#ci_func(work_experience$hours_work_onbodaboda,.95)
by(work_experience$hours_work_onbodaboda,outcomes$rtc_involvement,describe)
wilcox.test(work_experience$hours_work_onbodaboda~outcomes$rtc_involvement)

# Days of Work
summary(work_experience$days_work_bodaboda)
ad.test(work_experience$days_work_bodaboda)
#hist(work_experience$days_work_bodaboda)
#ci_func(work_experience$days_work_bodaboda,.95)
by(work_experience$days_work_bodaboda,outcomes$rtc_involvement,describe)
wilcox.test(work_experience$days_work_bodaboda~outcomes$rtc_involvement)

##### OUTCOME MEASURES ###########

# Injury
table<-with(outcomes,table(injury))
table
prop.table(table)
#table<-with(data_bea,table(hospitalization,risk_classification))
#table
#prop.table(table,2)
#chisq.test(table)
#fisher.test(table)
#assocstats(table) #vcd package

# Hospitalization
table<-with(outcomes,table(hospitalization))
table
prop.table(table)
#table<-with(data_bea,table(hospitalization,risk_classification))
#table
#prop.table(table,2)
#chisq.test(table)
#fisher.test(table)
#assocstats(table) #vcd package

# Near Miss
table<-with(outcomes,table(nearmissmonth))
table
prop.table(table)
#table<-with(data_bea,table(hospitalization,risk_classification))
#table
#prop.table(table,2)
#chisq.test(table)
#fisher.test(table)
#assocstats(table) #vcd package

# Days out of work
table<-with(outcomes,table(out_of_work))
table
prop.table(table)
#table<-with(data_bea,table(hospitalization,risk_classification))
#table
#prop.table(table,2)
#chisq.test(table)
#fisher.test(table)
#assocstats(table) #vcd package

# Days out of work
summary(outcomes$out_of_work)
ad.test(outcomes$out_of_work)
#table<-with(data_bea,table(hospitalization,risk_classification))
#table
#prop.table(table,2)
#chisq.test(table)
#fisher.test(table)
#assocstats(table) #vcd package

# LOS
summary(outcomes$los)
ad.test(outcomes$los)
#hist(work_experience$hospitalization)
#ci_func(work_experience$hospitalization,.95)
#by(work_experience$hospitalization,outcomes$rtc_involvement,describe)
#wilcox.test(work_experience$hospitalization~outcomes$rtc_involvement)

##### OUTCOME MEASURES ###########

######################################################
# Logistic Regression
######################################################

logistic_data<-data.frame(safe_habits,outcomes,age=data$age,hours=data$hours_work_onbodaboda,years=data$years_work_onbodaboda)

logmodel<-glm(rtc_involvement ~ safety_helmet_use + safety_helmet_use_colleague + safety_risk_driving + safety_headlight_use_day + safety_headlight_use_night + safety_headlight_use_night + safety_purchase_helmet_after_use + safety_buckle_helmet + safety_belief_helmet_reduce_risk + safety_belief_helmetstraps_reduce_risk + helmet_cracks + helmet_scratch + helmet_broken_chin + helmet_face_shield + helmet_obscure_face_shield + helmet_fit + age +hours + years,family=binomial, data=logistic_data)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)

logmodel<-glm(injury ~ safety_helmet_use + safety_helmet_use_colleague + safety_risk_driving + safety_headlight_use_day + safety_headlight_use_night + safety_headlight_use_night + safety_purchase_helmet_after_use + safety_buckle_helmet + safety_belief_helmet_reduce_risk + helmet_cracks + helmet_scratch + helmet_face_shield + helmet_obscure_face_shield + age +hours + years,family=binomial, data=logistic_data)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)

logmodel<-glm(hospitalization ~  safety_helmet_use + safety_helmet_use_colleague + safety_risk_driving + safety_headlight_use_day + safety_headlight_use_night + safety_headlight_use_night + safety_purchase_helmet_after_use + safety_buckle_helmet + safety_belief_helmet_reduce_risk + helmet_cracks + helmet_scratch + helmet_face_shield + helmet_obscure_face_shield + age +hours + years,family=binomial, data=logistic_data)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)

logmodel<-glm(nearmissmonth ~  safety_helmet_use + safety_helmet_use_colleague + safety_risk_driving + safety_headlight_use_day + safety_headlight_use_night + safety_headlight_use_night + safety_purchase_helmet_after_use + safety_buckle_helmet + safety_belief_helmet_reduce_risk + helmet_cracks + helmet_scratch + helmet_face_shield + helmet_obscure_face_shield + age +hours + years,family=binomial, data=logistic_data)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)









######################################################
#PRINCIPAL COMPONENT ANALYSIS - From psych package - http://twt.lk/bdAQ or http://twt.lk/bdAR or http://twt.lk/bdAS
######################################################
# Define the amout of factor to retain
#Group of functinos to determine the number os items to be extracted
par(mfrow=c(2,2)) #Command to configure the plot area for the scree plot graph
ev <- eigen(cor_auto(safe_habits_numeric)) # get eigenvalues - insert the data you want to calculate the scree plot for
ev # Show eigend values
ap <- parallel(subject=nrow(safe_habits_numeric),var=ncol(safe_habits_numeric),rep=100,cent=.05) #Calculate the acceleration factor
summary(ap)
nS <- nScree(ev$values) #Set up the Scree Plot 
plotnScree(nS) # Plot the ScreePlot Graph
my.vss <- VSS(cor_auto(safe_habits_numeric),title="VSS of BEA data")
print(my.vss[,1:12],digits =2)
VSS.plot(my.vss, title="VSS of 24 mental tests")
scree(safe_habits_numeric)
VSS.scree(cor_auto(safe_habits_numeric))
fa.parallel(cor_auto(safe_habits_numeric),n.obs=36)

# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 
fit <- principal(as.data.frame(safe_habits_numeric),4,rotate="varimax",scores=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
fit$scores
predict(fit,safe_habits_numeric)
safety_scores<-scoreItems(fit$weights,as.data.frame(safe_habits_numeric))$scores
describe(scores$scores)
#by(scores$scores,data_bea$risk_classification,summary)
#wilcox.test(scores$scores[,1]~data_bea$risk_classification)

# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix work
fit <- principal(as.data.frame(work_experience),1,rotate="varimax",scores=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
fit$scores
predict(fit,safe_habits_numeric)
experience_scores<-scoreItems(fit$weights,as.data.frame(work_experience))$scores
describe(scores$scores)
#by(scores$scores,data_bea$risk_classification,summary)
#wilcox.test(scores$scores[,1]~data_bea$risk_classification)

######################################################
#PATH ANALYSIS - From lavaan package - https://gist.github.com/joaovissoci/7838f4808885e506527e
######################################################
logistic_data<-data.frame(outcome=outcomes$injury,rtc=outcomes$rtc_involvement,nm=outcomes$nearmissmonth,safety_use,helmet_condition,peer_safety,safety_belief,age=demographics$age,hours=work_experience$hours,years=work_experience$years,hosp=outcomes$hospitalization)

logmodel<-glm(outcome ~ safety_use + helmet_condition + peer_safety + safety_belief + age + hours + years,family=binomial, data=logistic_data)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)

logmodel<-glm(nm ~ safety_use + helmet_condition + peer_safety + safety_belief + age + hours + years,family=binomial, data=logistic_data)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)

logmodel<-glm(outcome ~ safety_use + helmet_condition + peer_safety + safety_belief + age + hours + years,family=binomial, data=logistic_data)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)

logmodel<-glm(hosp ~ safety_use + helmet_condition + peer_safety + safety_belief + age + hours + years,family=binomial, data=logistic_data)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)

safety_use<-safety_scores[,1]
helmet_condition<-safety_scores[,2]
peer_safety<-safety_scores[,3]
safety_belief<-safety_scores[,4]
outcome1<-outcomes$rtc_involvement
outcome2<-outcomes$nearmissmonth
exp<-experience_scores[,1]#work_experience$years_work_onbodaboda#car::recode(work_experience$years_work_onbodaboda,"0:3=0;else=1")
#hours<-work_experience$hours_work_onbodaboda#car::recode(work_experience$hours_work_onbodaboda,"0:8=0;else=1")


network_data<-data.frame(safety_use,helmet_condition,peer_safety,safety_belief,outcome1,outcome2,exp)#,hours)

cor_data<-cor_auto(network_data)

qsgG3<-qgraph(cor_data,layout="spring",esize=20,graph="cor",sampleSize=nrow(network_data),legend.cex = 0.6,cut = 0.1, maximum = 1, minimum = 0.1, esize = 20,vsize = 5, repulsion = 0.8,nodeNames=colnames(network_data),borders = TRUE)#,gray=T,)#,nodeNames=nomesqsg, layout=Lqsg,,groups=qsggr,vsize=vSize*3,,color=c("gold","steelblue","red","grey80"),labels=rownames(pca_data)


inj<-car::recode(outcomes$injury,"'Yes'=1;'No'=0;else=NA")
inj<-as.numeric(as.character(inj))
hosp<-car::recode(outcomes$hospitalization,"'Yes'=1;'No'=0;else=NA")
hosp<-as.numeric(as.character(hosp))
#exp=car::recode(work_experience$years_work_onbodaboda,"0:3=0;else=1")

network_data<-data.frame(safety_use,helmet_condition,peer_safety,safety_belief,inj,exp,hosp)#,exp)
network_data_2<-subset(network_data,outcome1==1)
cor_data<-cor_auto(network_data_2)
qsgG3<-qgraph(cor_data,layout="spring",esize=20,graph="cor",sampleSize=nrow(network_data_2),legend.cex = 0.6,cut = 0.2, maximum = 1, minimum = 0.1, esize = 20,vsize = 5,repulsion = 0.8,nodeNames=colnames(network_data_2),borders = TRUE)#,gray=T,)#,nodeNames=nomesqsg, layout=Lqsg,,groups=qsggr,vsize=vSize*3,,color=c("gold","steelblue","red","grey80"),labels=rownames(pca_data)

groups<-list(Infrastructure=c(1,2,3,6,8,17,18,25),AttitudesBehaviors=c(5,9,10,12,13,14,15,16,19,21,24,27),Education=c(7,11),Rules=c(4,20,22,23,26,28))
varLabels<-c("Traffic Lane","Road Condition","Less Density","Traffic Regulation","Others Awareness","Walkways","Improve Training","Road Lighting","Reflector Vests","Helmet Usage","Education in Traffic","Respect of Rules","Carefulness","Alcohol Use","Respect for drivers","Reduce Speed","Road Signs","Wider Roads","Confident Driving","Inspect Licenses","Proper use of vehicle","Age limit for driving","Corruption","Dristracted Driving","Fines/Punishment","Risk Behavior","Vehicle condition","RR4")
varNames<-c("IS1","IS2","IS3","RR1","AB1","IS4","ED1","IS5","AB2","AB3","ED2","AB4","AB5","AB6","AB7","AB8","IS7","IS8","AB9","RR2","AB10","RR3","RR4","AB11","IS8","RR5","AB12","RR6")
normalize<-function(x){(x-min(x))/(max(x)-min(x))}
#mean_data<-sapply(as.data.frame(sapply(network_data,normalize)),mean)
vSize<-normalize(colSums(reasons_danger))*7

cor_data<-cor_auto(reasons_danger)
qsgG3<-qgraph(cor_data,layout="spring",esize=20,graph="glasso",sampleSize=nrow(network_data_2),legend.cex = 0.6,cut = 0.2, maximum = 1, minimum = 0.1, esize = 20,vsize = 5,repulsion = 0.8,nodeNames=colnames(network_data_2),borders = TRUE)#,gray=T,)#,nodeNames=nomesqsg, layout=Lqsg,,groups=qsggr,vsize=vSize*3,,color=c("gold","steelblue","red","grey80"),labels=rownames(pca_data)
PcorGRAPH<-qgraph(cor_data,layout="spring",vsize=vSize,esize=20,graph="glasso",sampleSize=nrow(network_data_2),legend.cex = 0.3,cut = 0.2, maximum = 1, minimum = 0.1, repulsion = 0.8,groups=groups,gray=FALSE,color=c("steelblue","gold"),legend=TRUE, nodeNames=varLabels,labels=varNames)#,layout=graph_layout
