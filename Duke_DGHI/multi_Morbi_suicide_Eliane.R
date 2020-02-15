#####################################################################
#BASIC R STATISTICS TEMPLATE
######################################################################
#
#https://link.springer.com/content/pdf/10.3758%2Fs13428-017-0910-x.pdf
#https://arxiv.org/pdf/1510.06871v2.pdf
#https://www.r-bloggers.com/predictability-in-network-models/
#
#
######################################################################
#SETTING ENVIRONMENT
######################################################################
#
#Instalar os pacotes importantes/Install important packages
#install.packages ("IsingFit")
#install.packages ("igraph")
# install.packages ("qgraph")
#install.packages ("ggplot2")
#install.packages("car")
#install.packages("mgm")
#install.packages ("dplyr")
#install.packages ("networktools")
#install.packages("NetworkComparisonTest")
#install.packages("plyr")

library(networktools)
library (plyr)
library (dplyr)
library(qgraph)
library(IsingFit)
library(ggplot2)
library(car)
library(tidyverse)
library(haven)
library(igraph)
library(mgm)
library(NetworkComparisonTest)
library(mice)
library(survey)
library(psych)
library (MASS)
library(foreign)
     
#remove.packages("qgraph")

######################################################################
#IMPORTING DATA
######################################################################

Banco_PNS_Original <- read_dta("/Users/Joao/Downloads/Banco_PNS_Original.dta")

#Use this command to remove columns that are entirely NA values, it will elave columns where only some vlaues are NA
#Banco_PNS_Original %>% select_if(~!all(is.na(.))) 


colnames(Banco_PNS_Original)

#setting up latin language
cat('\14')

######################################################################
#DATA WRANGLING
######################################################################

######################################################################
#subset for only individuals who responded to the individual questionnaire
#list of morbidities
# códigos de acordo com o dicionário
summary(Banco_PNS_Original$Q001)

table(Banco_PNS_Original$Q001)

data_sub <-subset(Banco_PNS_Original, Q001 == 1 | Q001 == 2 | Q001 ==3 | Q001 ==4 | Q001 ==5 | Q001 ==6)

colnames(data_sub)

#data_sub$peso<-case_when(data_sub$P001==1 & data_sub$P00101==1 ~ 1, data_sub$P001==2 ~ 2, TRUE ~ NA_real_)( Variávies que não trazem um missing alto)
#summary (data_sub$peso)
#table(data_sub$P001)
#table(data_sub$P004)
#table(data_sub$P027)
#table(data_sub$P028)

######################################################################
#adjusting linked questions

data_sub$esquizofrenia<-case_when(data_sub$Q110==1 & data_sub$Q11001==1 ~ 1, data_sub$Q110==2 ~ 2, TRUE ~ NA_real_)
summary (data_sub$esquizofrenia)

data_sub$bipolar<-case_when(data_sub$Q110==1 & data_sub$Q11002==1 ~ 1, data_sub$Q110==2 ~ 2, TRUE ~ NA_real_)
summary (data_sub$bipolar)

data_sub$TOC<-case_when(data_sub$Q110==1 & data_sub$Q11003==1 ~ 1, data_sub$Q110==2 ~ 2, TRUE ~ NA_real_)
summary (data_sub$TOC)

#combining mental health indicators due to collinearity
data_sub$mental_illness<-case_when(data_sub$Q110==1 & data_sub$Q11001==1 ~ 1,
                                   data_sub$Q110==1 & data_sub$Q11002==1 ~ 1,
                                   data_sub$Q110==1 & data_sub$Q11003==1 ~ 1,
                                   data_sub$Q110==2 ~ 2, TRUE ~ NA_real_)
summary (data_sub$mental_illness)

data_sub$infarto<-case_when(data_sub$Q063==1 & data_sub$Q06301==1 ~ 1, data_sub$Q063==2 ~ 2, TRUE ~ NA_real_)
summary (data_sub$infarto)

data_sub$angina<-case_when(data_sub$Q063==1 & data_sub$Q06302==1 ~ 1, data_sub$Q063==2 ~ 2, TRUE ~ NA_real_)
summary (data_sub$angina)

data_sub$ins_cardiaca<-case_when(data_sub$Q063==1 & data_sub$Q06303==1 ~ 1, data_sub$Q063==2 ~ 2, TRUE ~ NA_real_)
summary (data_sub$ins_cardiaca)

#combining heart condition indicators due to collinearity
data_sub$heart_condition<-case_when(data_sub$Q063==1 & data_sub$Q06301==1 ~ 1,
                                   data_sub$Q063==1 & data_sub$Q06302==1 ~ 1,
                                   data_sub$Q063==1 & data_sub$Q06303==1 ~ 1,
                                   data_sub$Q063==2 ~ 2, TRUE ~ NA_real_)
summary (data_sub$heart_condition)

data_sub$Enfisema<-case_when(data_sub$Q116==1 & data_sub$Q11601==1 ~ 1, data_sub$Q116==2 ~ 2, TRUE ~ NA_real_)
summary (data_sub$Enfisema)

data_sub$Bronquite<-case_when(data_sub$Q116==1 & data_sub$Q11602==1 ~ 1, data_sub$Q116==2 ~ 2, TRUE ~ NA_real_)
summary (data_sub$Bronquite)

#changing variables names
data_sub$insonia<-data_sub$Q132
data_sub$hipertensao <- data_sub$Q002
data_sub$diabetes <- data_sub$Q030
data_sub$colesterol <- data_sub$Q060
data_sub$Derrame <- data_sub$Q068
data_sub$artreu <- data_sub$Q079
data_sub$probcol <- data_sub$Q084
data_sub$dort <- data_sub$Q088
data_sub$cancer <- data_sub$Q120
data_sub$ins_renal <- data_sub$Q124
#data_sub$outradc <- data_sub$Q128
data_sub$depressao <- data_sub$Q092
data_sub$Acidente <- data_sub$O009
#data_sub$drink <- data_sub$P032
#data_sub$altura <- data_sub$P00401
#data_sub$peso <- data_sub$P00101
data_sub$sexo <- data_sub$C006
data_sub$raca <- data_sub$C009
data_sub$escol <- data_sub$D009
data_sub$vc <- data_sub$C010
#data_sub$plano <- data_sub$I001
data_sub$UF <- data_sub$V0001
data_sub$zona <- data_sub$V0026 
data_sub$idade <- data_sub$C008
data_sub$id_suic <- data_sub$N018

#creating new subset with analysis data
data_sub_all<-with(data_sub,data.frame(insonia,infarto, angina, ins_cardiaca,
                                       esquizofrenia, bipolar, TOC,
                                       Bronquite, Enfisema,
                                       hipertensao,
                                      diabetes,
                                      colesterol,
                                      Derrame,
                                      artreu,
                                      probcol,
                                      dort,
                                      cancer,
                                      ins_renal,
                                      depressao,
                                      vc,
                                      id_suic,
                                      sexo,
                                      idade,
                                      raca,
                                      escol,
                                      UF,
                                      zona,
                                      mental_illness,
                                      heart_condition))
                                      
#######################################################################################################
#Recoding value 2 to value 0
#recode angina
data_sub_all$angina <- car::recode (data_sub_all$angina, "2=0; 1=1")

#recode infarto
data_sub_all$infarto <- car::recode (data_sub_all$infarto, "2=0; 1=1")

#recode Ins. Cardíaca
data_sub_all$ins_cardiaca <- car::recode (data_sub_all$ins_cardiaca, "2=0; 1=1")

#recode angina
data_sub_all$heart_condition <- car::recode (data_sub_all$heart_condition, "2=0; 1=1")

#recode Enfisema
data_sub_all$Enfisema<- car::recode (data_sub_all$Enfisema, "2=0; 1=1")

#recode Bronquite
data_sub_all$Bronquite<- car::recode (data_sub_all$Bronquite, "2=0; 1=1")

#recode esquizofrenia
data_sub_all$esquizofrenia<- car::recode (data_sub_all$esquizofrenia, "2=0; 1=1")

#recode bipolar
data_sub_all$bipolar<- car::recode (data_sub_all$bipolar, "2=0; 1=1")

#recode TOC
data_sub_all$TOC<- car::recode (data_sub_all$TOC, "2=0; 1=1")

#recode mental illness
data_sub_all$mental_illness<- car::recode (data_sub_all$mental_illness, "2=0; 1=1")

#recode hypertension
data_sub_all$hipertensao<- car::recode (data_sub_all$hipertensao, "2:3=0; 1=1")

#recode diabetes
data_sub_all$diabetes<- car::recode (data_sub_all$diabetes, "2:3=0; 1=1")

#recode cholesterol
data_sub_all$colesterol<- car::recode (data_sub_all$colesterol, "2=0; 1=1")

#recode Derrame
data_sub_all$Derrame<- car::recode (data_sub_all$Derrame, "2=0; 1=1")

#recode artreu
data_sub_all$artreu<- car::recode (data_sub_all$artreu, "2=0; 1=1")

#recode probcol
data_sub_all$probcol<- car::recode (data_sub_all$probcol, "2=0; 1=1")

#recode dort
data_sub_all$dort<- car::recode (data_sub_all$dort, "2=0; 1=1")

#recode cancer
data_sub_all$cancer<- car::recode (data_sub_all$cancer, "2=0; 1=1")

#recode insre
data_sub_all$ins_renal<- car::recode (data_sub_all$ins_renal, "2=0; 1=1")

#recode dxdepre
data_sub_all$depressao<- car::recode (data_sub_all$depressao, "2=0; 1=1")

#recode suicidal ideation
data_sub_all$id_suic<- car::recode (data_sub_all$id_suic, "1 = 0; 2:4= 1") # 1= 0 (no ideation) 2:3:4=1 (ideation)

#recode sleep with medicine
data_sub_all$insonia<- car::recode (data_sub_all$insonia, "2:3=0; 1=1")

#recode live_someone
data_sub_all$vc<- car::recode (data_sub_all$vc, "1=0; 2=1") # 1= 0 (vive parceiro) 2=1 (vive sozinho)

# data_sub_all %>% 
#   dplyr::rename( 
  
#     infarction = infarto, 
#    angina = angina,
#    heart_failure = ins_cardiaca,
#    hypertension = hipertensao, 
#    sleep_medicine = insonia,
#    diabetes = diabetes, 
#    cholesterol = colesterol,
#    ODC = TOC, 
#    stroke = Derrame,
#    bipolar_disorder = bipolar,
#    arth_rheu = artreu ,
#    osteomusc = dort,
#    depression = depressao,
#    schizophrenia = esquizofrenia,
#    emphysema = Enfisema, 
#    cancer = cancer,
#    ckd = ins_renal,
#    spinalprob = probcol,
#    id_suic = id_suic,
#    bronchitis = Bronquite) -> data_sub_all 
   
#######################################################################################################
#Recoding categorical variables

#age
data_sub_all$idade_group <- car::recode(data_sub_all$idade, "18:24 = '0'; 25:44 = '1'; 45:64 = '2'; 65:84 = '3'; 85:109 = '4'" )
#unique (data_sub_all$agecat)
data_sub_all$idade_group <- factor(data_sub_all$idade_group, labels = c("18-24", "25-44", "45-64", "65-84", "85 ou mais"))


#escolaridade

#unique (data_sub_all$school) ( Não deu certo)
data_sub_all$schooling <- car::recode(data_sub_all$escol, "1 = '1'; 2:4 = '2'; 5:8 = '3'; 9:11 = '4'; 12 = '5' ")
data_sub_all$sch_group<- factor(data_sub_all$schooling, labels = c("no schooling", "elementary", "middle", " high ", "university"))

#race(data_sub_all$race)

data_sub_all$raca_group <- car::recode(data_sub_all$raca, " 1= 'ABranca'; 2= 'Preta'; 3= 'ZAmarela'; 4= 'Parda'; 5 = 'Indigena'; else = NA")
# data_sub_all$raca_group<- factor(data_sub_all$raca, labels = c("Branca ", "preta", "amarela", " Parda ", "indigena"))

#creating region data 
data_sub_all$region <- car::recode(data_sub_all$UF, "11:17 = 'North'; 21:29 = 'Nordeste'; 31:35 = 'Sudoeste'; 41:43 = 'ASul'; 50:53 = 'Centrooeste'" )

#######################################################################################################
#Dealing with missing data

#Calculating frequency of missing data per variable
propmiss <- function(dataframe) lapply(dataframe,function(x) data.frame(nmiss=sum(is.na(x)), n=length(x), propmiss=sum(is.na(x))/length(x)))
propmiss(data_sub_all)

#creat smaller dataset with just diseases ( only  binaries variables)

head(data_sub_all)
newdata4 <- with (data_sub_all, data.frame (insonia,
                                            heart_condition,
                                            # bipolar,
                                            Bronquite,
                                            hipertensao,
                                            diabetes,
                                            colesterol,
                                            Derrame,
                                            artreu,
                                            probcol,
                                            dort,
                                            cancer,
                                            ins_renal,
                                            depressao,
                                            id_suic,
                                            mental_illness
                                            # esquizofrenia, collinear with bipolar
                                            # TOC, collinear with bipolar
                                            # Enfisema / Collinear with Bronchitis
                                            ))  

#imputing data
imp <- mice(newdata4, seed = 2222, ridge = 0.01,  m=10)
# view(imp)
#imp <- mice(newdata4, seed = 2222, method = 'cart',  m=21)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
data_imputed<-mice::complete(imp)

# newdata4[complete.cases(newdata4), ] #Keep only complete rows
data4_complete <- data_imputed

#######################################################################################################
#calculating mm variables

data4_complete$mm<-rowSums(data4_complete)
summary(data4_complete)

#calculating mm variables
data4_complete$mm1<- car::recode (data4_complete$mm, "0='no';1:11='yes'") # 1:2= 0 (  < = que 2 morbidades) 1 ( > maior que 2 morbidades)
data4_complete$mm1<-as.factor(data4_complete$mm1)

#calculating mm variables
data4_complete$mm2<- car::recode (data4_complete$mm, "0:1='1no mm';4:11='5more than 5'") # 1:2= 0 (  < = que 2 morbidades) 1 ( > maior que 2 morbidades)
data4_complete$mm2<-as.factor(data4_complete$mm2)

#calculating mm variables
data4_complete$mm3<- car::recode (data4_complete$mm, "0:1='0';else='1'") # 1:2= 0 (  < = que 2 morbidades) 1 ( > maior que 2 morbidades)
data4_complete$mm3<-as.factor(data4_complete$mm3)

#######################################################################################################
#Combining predictors with the imputed mm data

data4_complete$region<-data_sub_all$region
data4_complete$vc<-data_sub_all$vc
data4_complete$sexo<-data_sub_all$sexo
data4_complete$idade<-data_sub_all$idade_group
data4_complete$raca<-data_sub_all$raca_group
data4_complete$escol<-data_sub_all$sch_group
data4_complete$zona<-data_sub_all$zona

data4_complete_network <- with (data4_complete, data.frame (insonia,
                                            heart_condition,
                                            # bipolar,
                                            Bronquite,
                                            hipertensao,
                                            diabetes,
                                            colesterol,
                                            Derrame,
                                            artreu,
                                            probcol,
                                            dort,
                                            cancer,
                                            ins_renal,
                                            depressao,
                                            id_suic,
                                            mental_illness
                                            # esquizofrenia, collinear with bipolar
                                            # TOC, collinear with bipolar
                                            # Enfisema / Collinear with Bronchitis
                                            ))  

data4_complete <- lapply(data4_complete, factor)  ## as.factor() could also be used

data4_complete$V0029<-data_sub$V0029
data4_complete$V0024<-data_sub$V0024

data4_complete<-as.data.frame(data4_complete)
##########################################################################
#Subsetting data by region

data_sub_region_north<-subset(data4_complete,data4_complete$region==1)
data_sub_region_northeast<-subset(data4_complete,data4_complete$region==2)
data_sub_region_southeast<-subset(data4_complete,data4_complete$region==3)
data_sub_region_south<-subset(data4_complete,data4_complete$region==4)
data_sub_region_mid_west<-subset(data4_complete,data4_complete$region==5)

##########################################################################
#Subsetting data by sex

data_sub_sex_man<-subset(data4_complete,data4_complete$sex==1)
data_sub_sex_women<-subset(data4_complete,data4_complete$sex==2)

##########################################################################
#Creating design weights
data.w1 <-svydesign (ids = ~1, weights = data4_complete$V0029, 
                               strata = data4_complete$V0024, 
                               data = data4_complete)

######################################################################
#TABLE 1
######################################################################
#idade with svytable
svytable <-with(data4_complete, table(sexo, id_suic), design = data.w1)
svytable
prop.table(svytable,1)

fit_syv1 <- svyglm(id_suic~ sexo, design=data.w1,family=quasibinomial())
exp(cbind(OR = coef(fit_syv1), confint(fit_syv1)))

#idade with svytable
svytable <-with(data4_complete, table(idade, id_suic), design = data.w1)
svytable
prop.table(svytable,2)

fit_syv1 <- svyglm(id_suic~ idade, design=data.w1,family=quasibinomial())
exp(cbind(OR = coef(fit_syv1), confint(fit_syv1)))

#idade with svytable
svytable <-with(data4_complete, table(region, id_suic), design = data.w1)
svytable
prop.table(svytable,2)

fit_syv1 <- svyglm(id_suic~ region, design=data.w1,family=quasibinomial())
exp(cbind(OR = coef(fit_syv1), confint(fit_syv1)))

#idade with svytable
svytable <-with(data4_complete, table(escol, id_suic), design = data.w1)
svytable
prop.table(svytable,2)

fit_syv1 <- svyglm(id_suic~ escol, design=data.w1,family=quasibinomial())
exp(cbind(OR = coef(fit_syv1), confint(fit_syv1)))

#idade with svytable
svytable <-with(data4_complete, table(raca, id_suic), design = data.w1)
svytable
prop.table(svytable,2)

fit_syv1 <- svyglm(id_suic~ raca, design=data.w1,family=quasibinomial())
exp(cbind(OR = coef(fit_syv1), confint(fit_syv1)))

#idade with svytable
svytable <-with(data4_complete, table(vc, id_suic), design = data.w1)
svytable
prop.table(svytable,2)

fit_syv1 <- svyglm(id_suic~ vc, design=data.w1,family=quasibinomial())
exp(cbind(OR = coef(fit_syv1), confint(fit_syv1)))

#idade with svytable
svytable <-with(data4_complete, table(mm1), design = data.w1)
svytable
prop.table(svytable,2)

svytable <-with(data4_complete, table(mm1,id_suic), design = data.w1)
svytable
prop.table(svytable,2)

fit_syv1 <- svyglm(id_suic~ vc, design=data.w1,family=quasibinomial())
exp(cbind(OR = coef(fit_syv1), confint(fit_syv1)))

######################################################################
#TABLE 2
######################################################################
#idade with svytable
svytable <-with(data4_complete, table(hipertensao, id_suic), design = data.w1)
svytable
prop.table(svytable,1)

fit_syv1 <- svyglm(id_suic~ hipertensao, design=data.w1,family=quasibinomial())
exp(cbind(OR = coef(fit_syv1), confint(fit_syv1)))

#idade with svytable
svytable <-with(data4_complete, table(diabetes, id_suic), design = data.w1)
svytable
prop.table(svytable,2)

fit_syv1 <- svyglm(id_suic~ diabetes, design=data.w1,family=quasibinomial())
exp(cbind(OR = coef(fit_syv1), confint(fit_syv1)))

#idade with svytable
svytable <-with(data4_complete, table(colesterol, id_suic), design = data.w1)
svytable
prop.table(svytable,2)

fit_syv1 <- svyglm(id_suic~ colesterol, design=data.w1,family=quasibinomial())
exp(cbind(OR = coef(fit_syv1), confint(fit_syv1)))

#idade with svytable
svytable <-with(data4_complete, table(heart_condition, id_suic), design = data.w1)
svytable
prop.table(svytable,2)

fit_syv1 <- svyglm(id_suic~ heart_condition, design=data.w1,family=quasibinomial())
exp(cbind(OR = coef(fit_syv1), confint(fit_syv1)))

#idade with svytable
svytable <-with(data4_complete, table(Derrame, id_suic), design = data.w1)
svytable
prop.table(svytable,2)

fit_syv1 <- svyglm(id_suic~ Derrame, design=data.w1,family=quasibinomial())
exp(cbind(OR = coef(fit_syv1), confint(fit_syv1)))

#idade with svytable
svytable <-with(data4_complete, table(artreu, id_suic), design = data.w1)
svytable
prop.table(svytable,2)

fit_syv1 <- svyglm(id_suic~ artreu, design=data.w1,family=quasibinomial())
exp(cbind(OR = coef(fit_syv1), confint(fit_syv1)))

#idade with svytable
svytable <-with(data4_complete, table(probcol, id_suic), design = data.w1)
svytable
prop.table(svytable,2)

fit_syv1 <- svyglm(id_suic~ probcol, design=data.w1,family=quasibinomial())
exp(cbind(OR = coef(fit_syv1), confint(fit_syv1)))

#idade with svytable
svytable <-with(data4_complete, table(dort, id_suic), design = data.w1)
svytable
prop.table(svytable,2)

fit_syv1 <- svyglm(id_suic~ dort, design=data.w1,family=quasibinomial())
exp(cbind(OR = coef(fit_syv1), confint(fit_syv1)))

#idade with svytable
svytable <-with(data4_complete, table(ins_renal, id_suic), design = data.w1)
svytable
prop.table(svytable,2)

fit_syv1 <- svyglm(id_suic~ ins_renal, design=data.w1,family=quasibinomial())
exp(cbind(OR = coef(fit_syv1), confint(fit_syv1)))

#idade with svytable
svytable <-with(data4_complete, table(cancer, id_suic), design = data.w1)
svytable
prop.table(svytable,2)

fit_syv1 <- svyglm(id_suic~ cancer, design=data.w1,family=quasibinomial())
exp(cbind(OR = coef(fit_syv1), confint(fit_syv1)))

#idade with svytable
svytable <-with(data4_complete, table(Bronquite, id_suic), design = data.w1)
svytable
prop.table(svytable,2)

fit_syv1 <- svyglm(id_suic~ Bronquite, design=data.w1,family=quasibinomial())
exp(cbind(OR = coef(fit_syv1), confint(fit_syv1)))

#idade with svytable
svytable <-with(data4_complete, table(depressao, id_suic), design = data.w1)
svytable
prop.table(svytable,2)

fit_syv1 <- svyglm(id_suic~ depressao, design=data.w1,family=quasibinomial())
exp(cbind(OR = coef(fit_syv1), confint(fit_syv1)))

#idade with svytable
svytable <-with(data4_complete, table(mental_illness, id_suic), design = data.w1)
svytable
prop.table(svytable,2)

fit_syv1 <- svyglm(id_suic~ mental_illness, design=data.w1,family=quasibinomial())
exp(cbind(OR = coef(fit_syv1), confint(fit_syv1)))

#idade with svytable
svytable <-with(data4_complete, table(insonia, id_suic), design = data.w1)
svytable
prop.table(svytable,2)

fit_syv1 <- svyglm(id_suic~ insonia, design=data.w1,family=quasibinomial())
exp(cbind(OR = coef(fit_syv1), confint(fit_syv1)))

######################################################################
#FIGURE 1
######################################################################

#logistic regression with all exposure variables (demographic variables + diseases)
#outcome is binary

# data4_complete$region<-as.factor(data4_complete$region)

mod1 <- svyglm(id_suic ~ region +
                   vc +
                   sexo +
                   idade +
                   raca +
                   escol +
                   zona +
                   heart_condition +
                   Bronquite +
                   hipertensao +
                   diabetes +
                   colesterol +
                   Derrame +
                   artreu +
                   probcol +
                   dort +
                   cancer +
                   ins_renal +
                   insonia +
                   depressao +
                   mental_illness, 
                   design=data.w1,
                   data = data4_complete, 
                   family = quasibinomial())

dope<-summary(mod1) #will display results
exp(cbind(OR = coef(mod1), confint(mod1)))
car::vif(mod1)

tmp<-data.frame(cbind(exp(coef(mod1)), exp(confint(mod1))))
odds<-tmp[-1,]
names(odds)<-c("OR", "lower", "upper")
rownames(odds) <- c("Região (Ref=Sul): Centro-Oeste",
                    "Região (Ref=Sul): Nordeste",
                    "Região (Ref=Sul): Norte",
                    "Região (Ref=Sul): Sudoeste",
                    "Estado civil (Ref=Vive c/ comp.): Vive s/ companheiro",
                    "Sexo (Ref=Homem): Mulher",
                    "Idade (Ref=>25 anos): 25-44",
                    "Idade (Ref=>25 anos): 45-64",
                    "Idade (Ref=>25 anos): 65-84",
                    "Idade (Ref=>25 anos): 85 ou mais",
                    "Raça (Ref=Branca): Indígena",
                    "Raça (Ref=Branca): Parda",
                    "Raça (Ref=Branca): Preta",
                    "Raça (Ref=Branca): Amarela",
                    "Escol. (Ref=Sem Instr.): Fundamental I",
                    "Escol. (Ref=Sem Instr.): Fundamental II",
                    "Escol. (Ref=Sem Instr.): Médio",
                    "Escol. (Ref=Sem Instr.): Superior",
                    "Zona de moradia (Ref=Urbana): Rural",
                    "Morbidade: IAC",
                    "Morbidade: Bronquite",
                    "Morbidade: hipertensao",
                    "Morbidade: Diabetes",
                    "Morbidade: Colesterol",
                    "Morbidade: Derrame",
                    "Morbidade: Artrite Reum.",
                    "Morbidade: Prob. Coluna",
                    "Morbidade: DORT",
                    "Morbidade: Câncer",
                    "Morbidade: Insf. Renal",
                    "Morbidade: Insônia",
                    "Morbidade: Depressão",
                    "Morbidade: Outra doença mental")

odds$groups <- c("Socio-demográficos",
                    "Socio-demográficos",
                    "Socio-demográficos",
                    "Socio-demográficos",
                    "Socio-demográficos",
                    "Socio-demográficos",
                    "Socio-demográficos",
                    "Socio-demográficos",
                    "Socio-demográficos",
                    "Socio-demográficos",
                    "Socio-demográficos",
                    "Socio-demográficos",
                    "Socio-demográficos",
                    "Socio-demográficos",
                    "Socio-demográficos",
                    "Socio-demográficos",
                    "Socio-demográficos",
                    "Socio-demográficos",
                    "Socio-demográficos",
                    "Morbidade",
                    "Morbidade",
                    "Morbidade",
                    "Morbidade",
                    "Morbidade",
                    "Morbidade",
                    "Morbidade",
                    "Morbidade",
                    "Morbidade",
                    "Morbidade",
                    "Morbidade",
                    "Morbidade",
                    "Morbidade",
                    "Morbidade")
odds$vars<-row.names(odds)
odds$pval<-dope$coefficients[-1,4]
# odds$pval<-dope2$`Pr(>|z|)`
ticks<-c(seq(.1, 1, by =.1), seq(0, 10, by =1), seq(10, 100, by =10))

# ggplot(odds, aes(y=OR , x = reorder(vars, OR))) +
ggplot(odds, aes(y=OR , x = vars)) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), 
                color=ifelse(odds$pval <.05, "orange", "grey"), 
                size=ifelse (odds$pval <.05, 1.2, .9), 
                width=.2) +
  #scale_y_log10(breaks=ticks, labels = ticks) +
  geom_hline(yintercept = 1, linetype=2) +
   coord_flip() +
  facet_grid(groups ~ .,scales="free_y",space="free") +
  labs(x = "Preditores", y = "Razão de Chance") +
  theme_bw()

######################################################################
#TABLE 3
######################################################################
#logistic regression with all exposure variables (demographic variables + mm)
#outcome is binary

svytable <-with(data4_complete, table(mm2), design = data.w1)
svytable
prop.table(svytable)

#idade with svytable
svytable <-with(data4_complete, table(mm2, id_suic), design = data.w1)
svytable
prop.table(svytable,1)

fit_syv1 <- svyglm(id_suic~ mm2, design=data.w1,family=quasibinomial())
exp(cbind(OR = coef(fit_syv1), confint(fit_syv1)))

mod1 <- svyglm(id_suic ~ region +
                   vc +
                   sexo +
                   idade +
                   raca +
                   escol +
                   mm2, 
                   design=data.w1,
                   data = data4_complete, 
                   family = quasibinomial())

summary(mod1) #will display results
exp(cbind(OR = coef(mod1), confint(mod1)))
car::vif(mod1)

#
svytable <-with(data4_complete, table(mm3), design = data.w1)
svytable
prop.table(svytable)

#idade with svytable
svytable <-with(data4_complete, table(mm3, id_suic), design = data.w1)
svytable
prop.table(svytable,1)

fit_syv1 <- svyglm(id_suic~ mm3, design=data.w1,family=quasibinomial())
exp(cbind(OR = coef(fit_syv1), confint(fit_syv1)))

mod1 <- svyglm(id_suic ~ region +
                   vc +
                   sexo +
                   idade +
                   raca +
                   escol +
                   mm2, 
                   design=data.w1,
                   data = data4_complete, 
                   family = quasibinomial())

summary(mod1) #will display results
exp(cbind(OR = coef(mod1), confint(mod1)))
car::vif(mod1)

######################################################################
#OVERALL GRAPH
######################################################################
# #Prevalences (proportion with disease/total)
# # data4_complete$colesterol<-as.numeric(data4_complete$colesterol)
# prev_coles<-prop.table(table(data4_complete$colesterol))

# # data4_complete$hipertensao<-as.numeric(data4_complete$hipertensao)
# prev_hipertensao<-prop.table(table(data4_complete$hipertensao))

# # data4_complete$Derrame<-as.numeric(data4_complete$Derrame)
# prev_Derrame<-prop.table(table(data4_complete$Derrame))

# # data4_complete$artreu<-as.numeric(data4_complete$artreu)
# prev_artreu<-prop.table(table(data4_complete$artreu))

# # data4_complete$probcol<-as.numeric(data4_complete$probcol)
# prev_probcol<-prop.table(table(data4_complete$probcol))

# # data4_complete$ins_renal<-as.numeric(data4_complete$ins_renal)
# prev_ins_renal <- prop.table(table(data4_complete$ins_renal))

# # data4_complete$depressao<-as.numeric(data4_complete$depressao)
# prev_depressao<-prop.table(table(data4_complete$depressao))

# # data4_complete$dort<-as.numeric(data4_complete$dort)
# prev_dort<-prop.table(table(data4_complete$dort))

# # data4_complete$insonia<-as.numeric(data4_complete$insonia)
# prev_insonia<-prop.table(table(data4_complete$insonia))

# # data4_complete$cancer<-as.numeric(data4_complete$cancer)
# prev_cancer <-  prop.table(table(data4_complete$cancer))

# # data4_complete$diabetes<-as.numeric(data4_complete$diabetes)
# prev_diabetes<-prop.table(table(data4_complete$diabetes))

# # data4_complete$heart_condition<-as.numeric(data4_complete$heart_condition)
# prev_heart_condition<-prop.table(table(data4_complete$heart_condition))

# # data4_complete$mental_illness<-as.numeric(data4_complete$mental_illness)
# prev_mental_illness<-prop.table(table(data4_complete$mental_illness))

# # data4_complete$Bronquite<-as.numeric(data4_complete$Bronquite)
# prev_Bronquite <-prop.table(table(data4_complete$Bronquite))

# # data4_complete$id_suic<-as.numeric(data4_complete$id_suic)
# prev_id_suic<-prop.table(table(data4_complete$id_suic))

# prev_all<-c(prev_hipertensao[2]+20,
#             prev_coles[2]+15,
#             prev_Derrame[2]+5,
#             prev_artreu[2]+10,
#             prev_probcol[2]+17,
#             prev_ins_renal[2]+7,
#             prev_depressao[2]+10,
#             prev_dort [2]+7,
#             prev_insonia[2]+10,
#             prev_cancer[2]+7,
#             # prev_Acidente[2],
#             prev_diabetes[2]+9,
#             prev_heart_condition[2]+9, 
#             prev_mental_illness[2]+7,
#             prev_Bronquite[2]+7,
#             prev_id_suic[2]+7)
            
#Fitting the network
#https://jmbh.github.io/Predictability-in-network-models/

fit_obj <- mgm(data = data4_complete_network,
               type =  rep('c', ncol(data4_complete_network)),
               level = rep(2,ncol(data4_complete_network)),
               lambdaSel = 'EBIC',
               lambdaGam=0.25,
               ruleReg = 'AND',
               moderators = 1:15,
               binarySign=TRUE)

# #network info
# fit_obj$interactions$indicator #(explicar)

# #Show moderators
# showInteraction(object = fit_obj, int = c(3,9,12))

# #building the network
# fit_g<-qgraph(fit_obj$pairwise$wadj, 
#                 labels=colnames(data4_complete_network))

# flow(fit_g,"id_suic")

# FactorGraph(object = fit_obj ,
#             edge.labels = TRUE,
#             labels = colnames(data4_complete_network))

# FactorGraph(object = fit_obj,
#             edge.labels = TRUE,
#             Nodewise = TRUE,
#            labels = colnames(data4_complete_network))

# FactorGraph(object = fit_obj,
#            edge.labels = TRUE,
#           PairwiseAsEdge = TRUE,
#             labels = colnames(data4_complete_network))

#Computing predictability nodes
pred_obj <- predict(object = fit_obj, data = data4_complete_network,
                    errorCat = c("CC"))

pred_obj$errors

#Table 2. Weight Adjacency Matrix - Ver se dá para rodar uma tabela dessa

# REVISAR ESSA PARTE

# error_list <- list() # List for ring-segments
# for(i in 1:14) error_list[[i]] <- pred_obj$errors[i,2]
# beyondmarg <- pred_obj$errors[14,2]-pred_obj$errors[14,5]
# error_list[[14]] <- c(pred_obj$errors[14,5],beyondmarg)
# color_list <- list() # List for Colors
# for(i in 1:13) color_list[[i]] <- "#90B4D4"
# color_list[[14]] <- c("#ffa500", "#ff4300")

#Centrality measures
centRes <- centrality(fit_obj$pairwise$wadj) #tells us about node strength
centRes$OutDegree
centRes$Closeness
centRes$Betweenness
centRes$ShortestPathLength
centralityPlot(fit_obj$pairwise$wadj)

#Centrality
centralityPlot(fit_obj$pairwise$wadj, include = c("ExpectedInfluence","Strength"), 
                                      labels = colnames(data4_complete_network), 
                                      orderBy = "ExpectedInfluence")

#Flow
flow(fit_g, 16, theme = "colorblind")#, vsize = prev_all)
flow

#Building the graph image
Graph_Ising_predictability <- qgraph(fit_obj$pairwise$wadj, # weighted adjacency matrix as input
                                   layout = "spring",
                                  pie = pred_obj$errors[,2], # provide errors as input
                                   pieColor = rep('#377EB8',ncol(data4_complete_network)),
                                   edge.color = fit_obj$pairwise$edgecolor,
                                  edge.labels = TRUE,
                                   curveAll = FALSE, curveDefault = .3, cut = 0,
                                   labels = colnames(data4_complete_network))#

# análise de comunidades
# https://www.nature.com/articles/srep30750

g2<-as.igraph(Graph_Ising_predictability)

#clusters    = g.clusters()
#giant       = clusters.giant() ## using the biggest component as an example, you can use the others here.
#communities = giant.community_spinglass()

# se não tiver arestas negativas:
g2_cl<-cluster_louvain(g2)

#g2 <- as.igraph(Graph_Ising_predictability)
#clu <- components(g2)
#groups(clu)

#se houver arestas negativas
# g2_sg<-spinglass.community(g2,implementation = "neg")

cluster_labels<-car::recode(g2_cl$membership,"
                    1='Doenças Doenças físicas e respiratórias';
                    2='Doenças cardiovasculates, endócrinas e neoplasias';
                    3='Doenças Psicossoacis'
                    ")

# tiff("E:/Documentos/script R/first_netowrk1.tiff",
#      width = 3000, height = 3000,compression = 'lzw', res=300)
#Add plot
Graph_Ising_predictability<-qgraph(fit_obj$pairwise$wadj, # weighted adjacency matrix as input
                                   layout = 'spring',
                                   # vsize=prev_all,
                                   pie = pred_obj$errors[,2], # provide errors as input
                                   pieColor = rep('#377EB8',ncol(data4_complete_network)),
                                   edge.color = fit_obj$pairwise$edgecolor,
                                   curveAll = TRUE, curveDefault = .3, cut = 0,
                                   labels = c("insonia",
                                              "IAC",
                                              "Bronqute",
                                              "hipertensao",
                                              "Diabetes",
                                              "Colesterol",
                                              "Derrame",
                                              "Artrite \nreumatóide",
                                              "Problemas \nna coluna",
                                              "DORT",
                                              "Câncer",
                                              "Insuf. renal",
                                              "depressao",
                                              "Ideação \nsuicída",
                                              "Outra doença \nmental"),
                                   groups=cluster_labels,
                                   palette="ggplot2",
                                   label.scale=TRUE)

# title("Overall multimorbidity pattern",line=2)

dev.off()
#Flow
qgraph::flow(Graph_Ising_predictability, "Ideação \nsuicída", theme="colorblind")

####################################################################
#NETWORK WITH CONFOUNDER

network_confounders<-data.frame(data4_complete_network,
                                data4_complete[,c(20:25)])

network_confounders$raca<-car::recode(network_confounders$raca,"
                          'ABranca'=1;
                          else=0")
network_confounders$raca<-as.numeric(as.character(network_confounders$raca))

network_confounders$vc<-as.numeric(as.character(network_confounders$vc))

network_confounders$sexo<-car::recode(network_confounders$sexo,"
                          '1'=0;
                          '2'=1")
network_confounders$sexo<-as.numeric(as.character(network_confounders$sexo))

network_confounders$idade<-as.numeric(network_confounders$idade)

network_confounders$escol<-as.numeric(network_confounders$escol)

network_confounders$zona<-car::recode(network_confounders$zona,"
                          '1'=0;
                          '2'=1")
network_confounders$zona<-as.numeric(as.character(network_confounders$zona))


network_confounders<-na.omit(network_confounders)

fit_obj <- mgm(data = network_confounders,
               type =  rep('c', ncol(network_confounders)),
               level = c(rep(2,17),5,2,5,2),
               lambdaSel = 'EBIC',
               lambdaGam=0.25,
               ruleReg = 'AND',
               # moderators = 1:15,
               binarySign=TRUE)

 fit_obj$pairwise$edgecolor

 fit_obj$interactions$indicator

#Computing predictability nodes
pred_obj <- predict(object = fit_obj, data = network_confounders,
                    errorCat = c("CC"))

pred_obj$errors

#Table 2. Weight Adjacency Matrix - Ver se dá para rodar uma tabela dessa

# REVISAR ESSA PARTE

# error_list <- list() # List for ring-segments
# for(i in 1:14) error_list[[i]] <- pred_obj$errors[i,2]
# beyondmarg <- pred_obj$errors[14,2]-pred_obj$errors[14,5]
# error_list[[14]] <- c(pred_obj$errors[14,5],beyondmarg)
# color_list <- list() # List for Colors
# for(i in 1:13) color_list[[i]] <- "#90B4D4"
# color_list[[14]] <- c("#ffa500", "#ff4300")

#Centrality measures
centRes <- centrality(fit_obj$pairwise$wadj) #tells us about node strength
centRes$OutDegree
centRes$Closeness
centRes$Betweenness
centRes$ShortestPathLength
centralityPlot(fit_obj$pairwise$wadj)

#Centrality
centralityPlot(fit_obj$pairwise$wadj, include = c("ExpectedInfluence","Strength","Closeness"), 
                                      # labels = colnames(data4_complete_network), 
                                      orderBy = "ExpectedInfluence")
#Building the graph image
Graph_Ising_predictability <- qgraph(fit_obj$pairwise$wadj, # weighted adjacency matrix as input
                                   layout = "spring",
                                  pie = pred_obj$errors[,2], # provide errors as input
                                   pieColor = rep('#377EB8',ncol(network_confounders)),
                                   edge.color = fit_obj$pairwise$edgecolor,
                                  edge.labels = TRUE,
                                   curveAll = FALSE, curveDefault = .3, cut = 0,
                                   labels = colnames(network_confounders))#

# análise de comunidades
# https://www.nature.com/articles/srep30750

g2<-as.igraph(Graph_Ising_predictability)

#clusters    = g.clusters()
#giant       = clusters.giant() ## using the biggest component as an example, you can use the others here.
#communities = giant.community_spinglass()

# se não tiver arestas negativas:
# g2_cl<-cluster_louvain(g2)

#g2 <- as.igraph(Graph_Ising_predictability)
#clu <- components(g2)
#groups(clu)

#se houver arestas negativas
g2_sg<-spinglass.community(g2,implementation = "neg")

community_names<-car::recode(as.factor(g2_sg$membership),"1='Doenças mentais';
                                                          2='Cardiovascular';
                                                          3='Funcionais \ne respiratórias';
                                                          4='Neoplasias e DORT'")
# tiff("E:/Documentos/script R/first_netowrk1.tiff",
#      width = 3000, height = 3000,compression = 'lzw', res=300)
#Add plot
Graph_Ising_predictability<-qgraph(fit_obj$pairwise$wadj, # weighted adjacency matrix as input
                                   layout = 'spring',
                                   # vsize=prev_all,
                                   pie = pred_obj$errors[,2], # provide errors as input
                                   pieColor = rep('#377EB8',ncol(network_confounders)),
                                   edge.color = fit_obj$pairwise$edgecolor,
                                   curveAll = TRUE, curveDefault = .3, cut = 0,
                                   labels = c("Insônia",
                                              "IAC",
                                              "Bronquite",
                                              "Hipertensao",
                                              "Diabetes",
                                              "Colesterol",
                                              "Derrame",
                                              "Artrite \nreumatóide",
                                              "Problemas \nna coluna",
                                              "DORT",
                                              "Câncer",
                                              "Insuf. renal",
                                              "Depressão",
                                              "Ideação \nsuicída",
                                              "Outra doença \nmental",
                                              "Estado civil",
                                              "Sexo",
                                              "Idade",
                                              "Raça",
                                              "Escolaridade",
                                              "Zona"),
                                   groups=community_names,
                                   palette="ggplot2",
                                   label.scale=TRUE)

# title("Overall multimorbidity pattern",line=2)

dev.off()
#Flow
qgraph::flow(Graph_Ising_predictability, "Ideação \nsuicída")



# ######################################################################
# #NORTH REGION GRAPH
# ######################################################################
# #Prevalences (proportion with disease/total)


# table(data_sub_region_north$hypertension)
# north_prev_hypertension<-prop.table(table(data_sub_region_north$hypertension))

# table(data_sub_region_north$diabetes)
# north_prev_diabetes<-prop.table(table(data_sub_region_north$diabetes))

# table(data_sub_region_north$cholesterol)
# north_prev_cholesterol<-prop.table(table(data_sub_region_north$cholesterol))
                                
# table(data_sub_region_north$Derrame)
# north_prev_Derrame<-prop.table(table(data_sub_region_north$Derrame))

                                
# table(data_sub_southregion_north$diab2)
# north_prev_diabetes<-prop.table(table(data_sub_southregion_north$diab2))

# table(data_sub_region_north$coles2)
# north_prev_coles2<-prop.table(table(data_sub_southregion_north$coles2))

# table(data_sub_southregion_north$iaic)
# north_prev_iaic<-prop.table(table(data_sub_southregion_north$iaic))

# table(data_sub_southregion_north$derr)
# north_prev_derr<-prop.table(table(data_sub_southregion_north$derr))

# table(data_sub_southregion_north$asma)
# north_prev_asma<-prop.table(table(data_sub_southregion_north$asma))

# table(data_sub_southregion_north$artreu)
# north_prev_artreu<-prop.table(table(data_sub_southregion_north$artreu))

# table(data_sub_southregion_north$probcol)
# north_prev_probcol<-prop.table(table(data_sub_southregion_north$probcol))

# table(data_sub_southregion_north$dort)
# north_prev_dort <-prop.table(table(data_sub_southregion_north$dort))

# table(data_sub_southregion_north$outramental)
# north_prev_outramental<-prop.table(table(data_sub_southregion_north$outramental))

# table(data_sub_southregion_north$dpoc)
# north_prev_dpoc<-prop.table(table(data_sub_southregion_north$dpoc))

# table(data_sub_southregion_north$ca)
# north_prev_ca<-prop.table(table(data_sub_southregion_north$ca))

# table(data_sub_southregion_north$insre)
# north_prev_insre<-prop.table(table(data_sub_southregion_north$insre))

# table(data_sub_southregion_north$outradc)
# north_prev_outradc <- prop.table(table(data_sub_southregion_north$outradc))

# table(data_sub_southregion_north$depre_dx_algo)
# north_prev_depre_dx_algo <- prop.table(table(data_sub_southregion_north$depre_dx_algo))

# table(data_sub_southregion_north$obeoms)
# north_prev_obeoms <-prop.table(table(data_sub_southregion_north$obeoms))

# prev_all_north<-c(north_prev_hyp[2]+20,
#                   north_prev_diab2[2]+12,
#                   north_prev_coles2[2]+15,
#                   north_prev_iaic[2]+10,
#                   north_prev_derr[2]+7,
#                   north_prev_asma[2]+6,
#                   north_prev_artreu[2]+12,
#                   north_prev_probcol[2] + 16,
#                   north_prev_dort[2]+4,
#                   north_prev_outramental[2]+4,
#                   north_prev_dpoc[2]+4,
#                   north_prev_ca[2]+5,
#                   north_prev_insre[2] + 5,
#                   north_prev_outradc[2] + 6 ,
#                   north_prev_depre_dx_algo[2] + 10,
#                   north_prev_obeoms[2] + 15)

# #Fitting the network
# fit_obj <- mgm(data = data4_complete,
#                type = rep('c', ncol(data4_complete)),
#                level = rep(2,ncol(data4_complete)),
#                lambdaSel = 'EBIC',
#                lambdaGam=0.25,
#                ruleReg = 'AND',
#                binarySign=TRUE)


# #Computing predictability nodes
# pred_obj <- predict(object = fit_obj,
#                     data = data4_complete,
#                     errorCat = 'CC')

# #Centrality measures
# centRes <- centrality(fit_obj$pairwise$wadj) #tells us about node strength
# centRes$OutDegree
# centRes$Closeness

# centRes$Betweenness
# centRes$ShortestPathLength
# centralityPlot(fit_obj$pairwise$wadj)

# #Centrality
# centralityPlot(fit_obj$pairwise$wadj, include = c("ExpectedInfluence"))

# #Flow
# # flow(fit_obj$pairwise$wadj, "RTI", theme = "colorblind")#, vsize = prev_all)

# Graph_Ising_predictability<-qgraph(fit_obj$pairwise$wadj, # weighted adjacency matrix as input
#                                    layout = 'spring',
#                                    vsize=prev_all,
#                                    pie = pred_obj$error[,2], # provide errors as input
#                                    pieColor = rep('#377EB8',ncol(data4_complete)),
#                                    edge.color = fit_obj$pairwise$edgecolor,
#                                    labels = colnames(data4_complete))#,
# # groups=as.factor(g2_sg$membership))




# #modelo Yolande

# model_1b <- IsingFit(as.matrix(data4_complete), gamma = 0.25,plot=FALSE)
# Graph_Ising1b <- qgraph(model_1b$weiadj, 
#                         layout = "spring",
#                         vsize=prev_all)



# #Centrality measures

# centRes <- centrality(Graph_Ising1b) #tells us about node strength
# centRes$OutDegree
# centRes$Closeness
# centRes$Betweenness
# centRes$ShortestPathLength
# centralityPlot(Graph_Ising1b)

# #Centrality
# centralityPlot(Graph_Ising1b, include = c("ExpectedInfluence")) 

# #Flow
# flow(Graph_Ising1b, "id_suic", theme = "colorblind")#, vsize = prev_all)

# # anÃ¡lise de comunidades 
# # https://www.nature.com/articles/srep30750
# g2<-as.igraph(Graph_Ising1b)
# # se nÃ£o tiver arestas negativas:
# g2_cl<-cluster_louvain(g2)

# # se houver arestas negativas
# g2_sg<-spinglass.community(g2,implementation = "neg")

# # qgraph(Graph_Ising1,groups=as.factor(g2_cl$membership))

# Graph_Ising2b <- qgraph(model_1b$weiadj, 
#                         layout = "spring",
#                         vsize=prev_all,
#                         groups=as.factor(g2_sg$membership))


# fit_obj <- mgm(data = data4_complete, 
#                type = rep('c', ncol(data4_complete)),
#                level = rep(2, ncol(data4_complete)),
#                lambdaSel = 'EBIC',
#                lambdaGam=0.5,
#                ruleReg = 'AND',
#                binarySign=TRUE)


# #Computing predictability nodes

# pred_obj <- predict(object = fit_obj, 
#                     data = data4_complete, 
#                     errorCat = 'CC')


# Graph_Ising_predictability<-qgraph(fit_obj$pairwise$wadj, # weighted adjacency matrix as input
#                                    layout = 'spring', 
#                                    vsize=prev_all,
#                                    pie = pred_obj$error[,2], # provide errors as input
#                                    pieColor = rep('#377EB8',ncol(data4_complete)),
#                                    edge.color = fit_obj$pairwise$edgecolor,
#                                    labels = colnames(data4_complete),
#                                    groups=as.factor(g2_sg$membership))


# flow(Graph_Ising_predictability, "id_suic", theme = "colorblind")#, vsize = data4_complete)


# # análise de comunidades
# # https://www.nature.com/articles/srep30750
# g2<-as.igraph(Graph_Ising_predictability)
# # se não tiver arestas negativas:
# g2_cl<-cluster_louvain(g2)

# # se houver arestas negativas
# # g2_sg<-spinglass.community(g2,implementation = "neg")

# rede1<-qgraph(fit_obj$pairwise$wadj, # weighted adjacency matrix as input
#                     layout = 'spring',
#                     vsize=prev_all,
#                     pie = pred_obj$error[,2], # provide errors as input
#                     pieColor = rep('#377EB8',ncol(data4_complete)),
#                     edge.color = fit_obj$pairwise$edgecolor,
#                     labels = colnames(data4_complete),
#                     groups=as.factor(g2_cl$membership))

# ######################################################################
# #NORTHEAST REGION GRAPH
# ######################################################################
# #Prevalences (proportion with disease/total)

# table(data_sub_southregion_northeast$has2)
# northeast_prev_hyp<-prop.table(table(data_sub_southregion_northeast$has2))

# table(data_sub_southregion_northeast$diab2)
# northeast_prev_diab2<-prop.table(table(data_sub_southregion_northeast$diab2))

# table(data_sub_southregion_northeast$coles2)
# northeast_prev_coles2<-prop.table(table(data_sub_southregion_northeast$coles2))

# table(data_sub_southregion_northeast$iaic)
# northeast_prev_iaic<-prop.table(table(data_sub_southregion_northeast$iaic))

# table(data_sub_southregion_northeast$derr)
# northeast_prev_derr<-prop.table(table(data_sub_southregion_northeast$derr))

# table(data_sub_southregion_northeast$asma)
# northeast_prev_asma<-prop.table(table(data_sub_southregion_northeast$asma))

# table(data_sub_southregion_northeast$artreu)
# northeast_prev_artreu<-prop.table(table(data_sub_southregion_northeast$artreu))

# table(data_sub_southregion_northeast$probcol)
# northeast_prev_probcol<-prop.table(table(data_sub_southregion_northeast$probcol))

# table(data_sub_southregion_northeast$dort)
# northeast_prev_dort <-prop.table(table(data_sub_southregion_northeast$dort))

# table(data_sub_southregion_northeast$outramental)
# northeast_prev_outramental<-prop.table(table(data_sub_southregion_northeast$outramental))

# table(data_sub_southregion_northeast$dpoc)
# northeast_prev_dpoc<-prop.table(table(data_sub_southregion_northeast$dpoc))

# table(data_sub_southregion_northeast$ca)
# northeast_prev_ca<-prop.table(table(data_sub_southregion_northeast$ca))

# table(data_sub_southregion_northeast$insre)
# northeast_prev_insre<-prop.table(table(data_sub_southregion_northeast$insre))

# table(data_sub_southregion_northeast$outradc)
# northeast_prev_outradc <- prop.table(table(data_sub_southregion_northeast$outradc))

# table(data_sub_southregion_northeast$depre_dx_algo)
# northeast_prev_depre_dx_algo <- prop.table(table(data_sub_southregion_northeast$depre_dx_algo))

# table(data_sub_southregion_northeast$obeoms)
# northeast_prev_obeoms <-prop.table(table(data_sub_southregion_northeast$obeoms))

# prev_all_northeast<-c(northeast_prev_hyp[2]+20,
#                       northeast_prev_diab2[2]+12,
#                       northeast_prev_coles2[2]+16,
#                       northeast_prev_iaic[2]+10,
#                       northeast_prev_derr[2]+6,
#                       northeast_prev_asma[2]+5,
#                       northeast_prev_artreu[2]+12,
#                       northeast_prev_probcol[2] + 16,
#                       northeast_prev_dort[2]+4,
#                       northeast_prev_outramental[2]+4,
#                       northeast_prev_dpoc[2]+4,
#                       northeast_prev_ca[2]+5,
#                       northeast_prev_insre[2] + 4,
#                       northeast_prev_outradc[2] + 6 ,
#                       northeast_prev_depre_dx_algo[2] + 10,
#                       northeast_prev_obeoms[2] + 15)

# #Fitting the network
# fit_obj <- mgm(data = data_sub_southregion_northeast,
#                type = rep('c', ncol(data_sub_southregion_northeast)),
#                level = rep(2,ncol(data_sub_southregion_northeast)),
#                lambdaSel = 'EBIC',
#                lambdaGam=0.25,
#                ruleReg = 'AND',
#                binarySign=TRUE)


# #Computing predictability nodes
# pred_obj <- predict(object = fit_obj,
#                     data = data_sub_southregion_northeast,
#                     errorCat = 'CC')

# #Centrality measures
# centRes <- centrality(fit_obj$pairwise$wadj) #tells us about node strength
# centRes$OutDegree
# centRes$Closeness
# centRes$Betweenness
# centRes$ShortestPathLength
# centralityPlot(fit_obj$pairwise$wadj)

# #Centrality
# centralityPlot(fit_obj$pairwise$wadj, include = c("ExpectedInfluence"))

# #Flow
# # flow(fit_obj$pairwise$wadj, "RTI", theme = "colorblind")#, vsize = prev_all)

# Graph_Ising_predictability<-qgraph(fit_obj$pairwise$wadj, # weighted adjacency matrix as input
#                                    layout = 'spring',
#                                    vsize=prev_all_northeast,
#                                    pie = pred_obj$error[,2], # provide errors as input
#                                    pieColor = rep('#377EB8',ncol(data_sub_southregion_northeast)),
#                                    edge.color = fit_obj$pairwise$edgecolor,
#                                    labels = colnames(data_sub_southregion_northeast))#,
# # groups=as.factor(g2_sg$membership))

# # análise de comunidades
# # https://www.nature.com/articles/srep30750
# g2<-as.igraph(Graph_Ising_predictability)
# # se não tiver arestas negativas:
# g2_cl<-cluster_louvain(g2)

# # se houver arestas negativas
# # g2_sg<-spinglass.community(g2,implementation = "neg")

# rede2_northeast<-qgraph(fit_obj$pairwise$wadj, # weighted adjacency matrix as input
#                         layout = 'spring',
#                         vsize=prev_all_northeast,
#                         pie = pred_obj$error[,2], # provide errors as input
#                         pieColor = rep('#377EB8',ncol(data_sub_southregion_northeast)),
#                         edge.color = fit_obj$pairwise$edgecolor,
#                         labels = colnames(data_sub_southregion_northeast),
#                         groups=as.factor(g2_cl$membership))

# ######################################################################
# #Midwest REGION GRAPH
# ######################################################################
# #Prevalences (proportion with disease/total)

# table(data_sub_southregion_mid_west$has2)
# midwest_prev_hyp<-prop.table(table(data_sub_southregion_mid_west$has2))

# table(data_sub_southregion_mid_west$diab2)
# midwest_prev_diab2<-prop.table(table(data_sub_southregion_mid_west$diab2))

# table(data_sub_southregion_mid_west$coles2)
# midwest_prev_coles2<-prop.table(table(data_sub_southregion_mid_west$coles2))

# table(data_sub_southregion_mid_west$iaic)
# midwest_prev_iaic<-prop.table(table(data_sub_southregion_mid_west$iaic))

# table(data_sub_southregion_mid_west$derr)
# midwest_prev_derr<-prop.table(table(data_sub_southregion_mid_west$derr))

# table(data_sub_southregion_mid_west$asma)
# midwest_prev_asma<-prop.table(table(data_sub_southregion_mid_west$asma))

# table(data_sub_southregion_mid_west$artreu)
# midwest_prev_artreu<-prop.table(table(data_sub_southregion_mid_west$artreu))

# table(data_sub_southregion_mid_west$probcol)
# midwest_prev_probcol<-prop.table(table(data_sub_southregion_mid_west$probcol))

# table(data_sub_southregion_mid_west$dort)
# midwest_prev_dort <-prop.table(table(data_sub_southregion_mid_west$dort))

# table(data_sub_southregion_mid_west$outramental)
# midwest_prev_outramental<-prop.table(table(data_sub_southregion_mid_west$outramental))

# table(data_sub_southregion_mid_west$dpoc)
# midwest_prev_dpoc<-prop.table(table(data_sub_southregion_mid_west$dpoc))

# table(data_sub_southregion_mid_west$ca)
# midwest_prev_ca<-prop.table(table(data_sub_southregion_mid_west$ca))

# table(data_sub_southregion_mid_west$insre)
# midwest_prev_insre<-prop.table(table(data_sub_southregion_mid_west$insre))

# table(data_sub_southregion_mid_west$outradc)
# midwest_prev_outradc <- prop.table(table(data_sub_southregion_mid_west$outradc))

# table(data_sub_southregion_mid_west$depre_dx_algo)
# midwest_prev_depre_dx_algo <- prop.table(table(data_sub_southregion_mid_west$depre_dx_algo))

# table(data_sub_southregion_mid_west$obeoms)
# midwest_prev_obeoms <-prop.table(table(data_sub_southregion_mid_west$obeoms))

# prev_all_midwest<-c(midwest_prev_hyp[2]+20,
#                     midwest_prev_diab2[2]+13,
#                     midwest_prev_coles2[2]+16,
#                     midwest_prev_iaic[2]+10,
#                     midwest_prev_derr[2]+5,
#                     midwest_prev_asma[2]+5,
#                     midwest_prev_artreu[2]+13,
#                     midwest_prev_probcol[2] + 16,
#                     midwest_prev_dort[2]+4,
#                     midwest_prev_outramental[2]+4,
#                     midwest_prev_dpoc[2]+4,
#                     midwest_prev_ca[2]+5,
#                     midwest_prev_insre[2] + 4,
#                     midwest_prev_outradc[2] + 6 ,
#                     midwest_prev_depre_dx_algo[2] + 10,
#                     midwest_prev_obeoms[2] + 16)

# #Fitting the network
# fit_obj <- mgm(data = data_sub_southregion_mid_west,
#                type = rep('c', ncol(data_sub_southregion_mid_west)),
#                level = rep(2,ncol(data_sub_southregion_mid_west)),
#                lambdaSel = 'EBIC',
#                lambdaGam=0.25,
#                ruleReg = 'AND',
#                binarySign=TRUE)


# #Computing predictability nodes
# pred_obj <- predict(object = fit_obj,
#                     data = data_sub_southregion_mid_west,
#                     errorCat = 'CC')

# #Centrality measures
# centRes <- centrality(fit_obj$pairwise$wadj) #tells us about node strength
# centRes$OutDegree
# centRes$Closeness
# centRes$Betweenness
# centRes$ShortestPathLength
# centralityPlot(fit_obj$pairwise$wadj)

# #Centrality
# centralityPlot(fit_obj$pairwise$wadj, include = c("ExpectedInfluence"))

# #Flow
# # flow(fit_obj$pairwise$wadj, "RTI", theme = "colorblind")#, vsize = prev_all)

# Graph_Ising_predictability<-qgraph(fit_obj$pairwise$wadj, # weighted adjacency matrix as input
#                                    layout = 'spring',
#                                    vsize=prev_all_midwest,
#                                    pie = pred_obj$error[,2], # provide errors as input
#                                    pieColor = rep('#377EB8',ncol(data_sub_southregion_mid_west)),
#                                    edge.color = fit_obj$pairwise$edgecolor,
#                                    labels = colnames(data_sub_southregion_mid_west))#,
# # groups=as.factor(g2_sg$membership))

# # análise de comunidades
# # https://www.nature.com/articles/srep30750
# g2<-as.igraph(Graph_Ising_predictability)
# # se não tiver arestas negativas:
# g2_cl<-cluster_louvain(g2)

# # se houver arestas negativas
# # g2_sg<-spinglass.community(g2,implementation = "neg")

# rede3_midwest<-qgraph(fit_obj$pairwise$wadj, # weighted adjacency matrix as input
#                       layout = 'spring',
#                       vsize=prev_all_midwest,
#                       pie = pred_obj$error[,2], # provide errors as input
#                       pieColor = rep('#377EB8',ncol(data_sub_southregion_mid_west)),
#                       edge.color = fit_obj$pairwise$edgecolor,
#                       labels = colnames(data_sub_southregion_mid_west),
#                       groups=as.factor(g2_cl$membership))

# ######################################################################
# #Southeast REGION GRAPH
# ######################################################################
# #Prevalences (proportion with disease/total)

# table(data_sub_southregion_southeast$has2)
# southeast_prev_hyp<-prop.table(table(data_sub_southregion_southeast$has2))

# table(data_sub_southregion_southeast$diab2)
# southeast_prev_diab2<-prop.table(table(data_sub_southregion_southeast$diab2))

# table(data_sub_southregion_southeast$coles2)
# southeast_prev_coles2<-prop.table(table(data_sub_southregion_southeast$coles2))

# table(data_sub_southregion_southeast$iaic)
# southeast_prev_iaic<-prop.table(table(data_sub_southregion_southeast$iaic))

# table(data_sub_southregion_southeast$derr)
# southeast_prev_derr<-prop.table(table(data_sub_southregion_southeast$derr))

# table(data_sub_southregion_southeast$asma)
# southeast_prev_asma<-prop.table(table(data_sub_southregion_southeast$asma))

# table(data_sub_southregion_southeast$artreu)
# southeast_prev_artreu<-prop.table(table(data_sub_southregion_southeast$artreu))

# table(data_sub_southregion_southeast$probcol)
# southeast_prev_probcol<-prop.table(table(data_sub_southregion_southeast$probcol))

# table(data_sub_southregion_southeast$dort)
# southeast_prev_dort <-prop.table(table(data_sub_southregion_southeast$dort))

# table(data_sub_southregion_southeast$outramental)
# southeast_prev_outramental<-prop.table(table(data_sub_southregion_southeast$outramental))

# table(data_sub_southregion_southeast$dpoc)
# southeast_prev_dpoc<-prop.table(table(data_sub_southregion_southeast$dpoc))

# table(data_sub_southregion_southeast$ca)
# southeast_prev_ca<-prop.table(table(data_sub_southregion_southeast$ca))

# table(data_sub_southregion_southeast$insre)
# southeast_prev_insre<-prop.table(table(data_sub_southregion_southeast$insre))

# table(data_sub_southregion_southeast$outradc)
# southeast_prev_outradc <- prop.table(table(data_sub_southregion_southeast$outradc))

# table(data_sub_southregion_southeast$depre_dx_algo)
# southeast_prev_depre_dx_algo <- prop.table(table(data_sub_southregion_southeast$depre_dx_algo))

# table(data_sub_southregion_southeast$obeoms)
# southeast_prev_obeoms <-prop.table(table(data_sub_southregion_southeast$obeoms))

# prev_all_southeast<-c(southeast_prev_hyp[2]+20,
#                       southeast_prev_diab2[2]+13,
#                       southeast_prev_coles2[2]+16,
#                       southeast_prev_iaic[2]+10,
#                       southeast_prev_derr[2]+5,
#                       southeast_prev_asma[2]+5,
#                       southeast_prev_artreu[2]+12,
#                       southeast_prev_probcol[2] + 16,
#                       southeast_prev_dort[2]+4,
#                       southeast_prev_outramental[2]+4,
#                       southeast_prev_dpoc[2]+4,
#                       southeast_prev_ca[2]+5,
#                       southeast_prev_insre[2] + 4,
#                       southeast_prev_outradc[2] + 6 ,
#                       southeast_prev_depre_dx_algo[2] + 10,
#                       southeast_prev_obeoms[2] + 16)

# #Fitting the network
# fit_obj <- mgm(data = data_sub_southregion_southeast,
#                type = rep('c', ncol(data_sub_southregion_southeast)),
#                level = rep(2,ncol(data_sub_southregion_southeast)),
#                lambdaSel = 'EBIC',
#                lambdaGam=0.25,
#                ruleReg = 'AND',
#                binarySign=TRUE)


# #Computing predictability nodes
# pred_obj <- predict(object = fit_obj,
#                     data = data_sub_southregion_southeast,
#                     errorCat = 'CC')

# #Centrality measures
# centRes <- centrality(fit_obj$pairwise$wadj) #tells us about node strength
# centRes$OutDegree
# centRes$Closeness
# centRes$Betweenness
# centRes$ShortestPathLength
# centralityPlot(fit_obj$pairwise$wadj)

# #Centrality
# centralityPlot(fit_obj$pairwise$wadj, include = c("ExpectedInfluence"))

# #Flow
# # flow(fit_obj$pairwise$wadj, "RTI", theme = "colorblind")#, vsize = prev_all)

# Graph_Ising_predictability<-qgraph(fit_obj$pairwise$wadj, # weighted adjacency matrix as input
#                                    layout = 'spring',
#                                    vsize=prev_all_southeast,
#                                    pie = pred_obj$error[,2], # provide errors as input
#                                    pieColor = rep('#377EB8',ncol(data_sub_southregion_southeast)),
#                                    edge.color = fit_obj$pairwise$edgecolor,
#                                    labels = colnames(data_sub_southregion_southeast))#,
# # groups=as.factor(g2_sg$membership))

# # análise de comunidades
# # https://www.nature.com/articles/srep30750
# g2<-as.igraph(Graph_Ising_predictability)
# # se não tiver arestas negativas:
# g2_cl<-cluster_louvain(g2)

# # se houver arestas negativas
# # g2_sg<-spinglass.community(g2,implementation = "neg")

# rede4_southeast<-qgraph(fit_obj$pairwise$wadj, # weighted adjacency matrix as input
#                         layout = 'spring',
#                         vsize=prev_all_southeast,
#                         pie = pred_obj$error[,2], # provide errors as input
#                         pieColor = rep('#377EB8',ncol(data_sub_southregion_southeast)),
#                         edge.color = fit_obj$pairwise$edgecolor,
#                         labels = colnames(data_sub_southregion_southeast),
#                         groups=as.factor(g2_cl$membership))

# ######################################################################
# #South REGION GRAPH
# ######################################################################
# #Prevalences (proportion with disease/total)

# table(data_sub_southregion_south$has2)
# south_prev_hyp<-prop.table(table(data_sub_southregion_south$has2))

# table(data_sub_southregion_south$diab2)
# south_prev_diab2<-prop.table(table(data_sub_southregion_south$diab2))

# table(data_sub_southregion_south$coles2)
# south_prev_coles2<-prop.table(table(data_sub_southregion_south$coles2))

# table(data_sub_southregion_south$iaic)
# south_prev_iaic<-prop.table(table(data_sub_southregion_south$iaic))

# table(data_sub_southregion_south$derr)
# south_prev_derr<-prop.table(table(data_sub_southregion_south$derr))

# table(data_sub_southregion_south$asma)
# south_prev_asma<-prop.table(table(data_sub_southregion_south$asma))

# table(data_sub_southregion_south$artreu)
# south_prev_artreu<-prop.table(table(data_sub_southregion_south$artreu))

# table(data_sub_southregion_south$probcol)
# south_prev_probcol<-prop.table(table(data_sub_southregion_south$probcol))

# table(data_sub_southregion_south$dort)
# south_prev_dort <-prop.table(table(data_sub_southregion_south$dort))

# table(data_sub_southregion_south$outramental)
# south_prev_outramental<-prop.table(table(data_sub_southregion_south$outramental))

# table(data_sub_southregion_south$dpoc)
# south_prev_dpoc<-prop.table(table(data_sub_southregion_south$dpoc))

# table(data_sub_southregion_south$ca)
# south_prev_ca<-prop.table(table(data_sub_southregion_south$ca))

# table(data_sub_southregion_south$insre)
# south_prev_insre<-prop.table(table(data_sub_southregion_south$insre))

# table(data_sub_southregion_south$outradc)
# south_prev_outradc <- prop.table(table(data_sub_southregion_south$outradc))

# table(data_sub_southregion_south$depre_dx_algo)
# south_prev_depre_dx_algo <- prop.table(table(data_sub_southregion_south$depre_dx_algo))

# table(data_sub_southregion_south$obeoms)
# south_prev_obeoms <-prop.table(table(data_sub_southregion_south$obeoms))

# prev_all_south<-c(south_prev_hyp[2]+20,
#                   south_prev_diab2[2]+12,
#                   south_prev_coles2[2]+16,
#                   south_prev_iaic[2]+10,
#                   south_prev_derr[2]+6,
#                   south_prev_asma[2]+6,
#                   south_prev_artreu[2]+16,
#                   south_prev_probcol[2] + 18,
#                   south_prev_dort[2]+4,
#                   south_prev_outramental[2]+4,
#                   south_prev_dpoc[2]+5,
#                   south_prev_ca[2]+7,
#                   south_prev_insre[2] + 4,
#                   south_prev_outradc[2] + 6 ,
#                   south_prev_depre_dx_algo[2] + 12,
#                   south_prev_obeoms[2] + 16)

# #Fitting the network
# fit_obj <- mgm(data = data_sub_southregion_south,
#                type = rep('c', ncol(data_sub_southregion_south)),
#                level = rep(2,ncol(data_sub_southregion_south)),
#                lambdaSel = 'EBIC',
#                lambdaGam=0.25,
#                ruleReg = 'AND',
#                binarySign=TRUE)


# #Computing predictability nodes
# pred_obj <- predict(object = fit_obj,
#                     data = data_sub_southregion_south,
#                     errorCat = 'CC')

# #Centrality measures
# centRes <- centrality(fit_obj$pairwise$wadj) #tells us about node strength
# centRes$OutDegree
# centRes$Closeness
# centRes$Betweenness
# centRes$ShortestPathLength
# centralityPlot(fit_obj$pairwise$wadj)

# #Centrality
# centralityPlot(fit_obj$pairwise$wadj, include = c("ExpectedInfluence"))

# #Flow
# # flow(fit_obj$pairwise$wadj, "RTI", theme = "colorblind")#, vsize = prev_all)

# Graph_Ising_predictability<-qgraph(fit_obj$pairwise$wadj, # weighted adjacency matrix as input
#                                    layout = 'spring',
#                                    vsize=prev_all_south,
#                                    pie = pred_obj$error[,2], # provide errors as input
#                                    pieColor = rep('#377EB8',ncol(data_sub_southregion_south)),
#                                    edge.color = fit_obj$pairwise$edgecolor,
#                                    labels = colnames(data_sub_southregion_south))#,
# # groups=as.factor(g2_sg$membership))

# # análise de comunidades
# # https://www.nature.com/articles/srep30750
# g2<-as.igraph(Graph_Ising_predictability)
# # se não tiver arestas negativas:
# g2_cl<-cluster_louvain(g2)

# # se houver arestas negativas
# # g2_sg<-spinglass.community(g2,implementation = "neg")

# rede5_south<-qgraph(fit_obj$pairwise$wadj, # weighted adjacency matrix as input
#                     layout = 'spring',
#                     vsize=prev_all_south,
#                     pie = pred_obj$error[,2], # provide errors as input
#                     pieColor = rep('#377EB8',ncol(data_sub_southregion_south)),
#                     edge.color = fit_obj$pairwise$edgecolor,
#                     labels = colnames(data_sub_southregion_south),
#                     groups=as.factor(g2_cl$membership))

# ######################################################################
# #male GRAPH
# ######################################################################
# #Prevalences (proportion with disease/total)

# table(data_sub_sex_man$has2)
# male_prev_hyp<-prop.table(table(data_sub_sex_man$has2))

# table(data_sub_sex_man$diab2)
# male_prev_diab2<-prop.table(table(data_sub_sex_man$diab2))

# table(data_sub_sex_man$coles2)
# male_prev_coles2<-prop.table(table(data_sub_sex_man$coles2))

# table(data_sub_sex_man$iaic)
# male_prev_iaic<-prop.table(table(data_sub_sex_man$iaic))

# table(data_sub_sex_man$derr)
# male_prev_derr<-prop.table(table(data_sub_sex_man$derr))

# table(data_sub_sex_man$asma)
# male_prev_asma<-prop.table(table(data_sub_sex_man$asma))

# table(data_sub_sex_man$artreu)
# male_prev_artreu<-prop.table(table(data_sub_sex_man$artreu))

# table(data_sub_sex_man$probcol)
# male_prev_probcol<-prop.table(table(data_sub_sex_man$probcol))

# table(data_sub_sex_man$dort)
# male_prev_dort <-prop.table(table(data_sub_sex_man$dort))

# table(data_sub_sex_man$outramental)
# male_prev_outramental<-prop.table(table(data_sub_sex_man$outramental))

# table(data_sub_sex_man$dpoc)
# male_prev_dpoc<-prop.table(table(data_sub_sex_man$dpoc))

# table(data_sub_sex_man$ca)
# male_prev_ca<-prop.table(table(data_sub_sex_man$ca))

# table(data_sub_sex_man$insre)
# male_prev_insre<-prop.table(table(data_sub_sex_man$insre))

# table(data_sub_sex_man$outradc)
# male_prev_outradc <- prop.table(table(data_sub_sex_man$outradc))

# table(data_sub_sex_man$depre_dx_algo)
# male_prev_depre_dx_algo <- prop.table(table(data_sub_sex_man$depre_dx_algo))

# table(data_sub_sex_man$obeoms)
# male_prev_obeoms <-prop.table(table(data_sub_sex_man$obeoms))

# prev_all_male<-c(male_prev_hyp[2]+20,
#                  male_prev_diab2[2]+14,
#                  male_prev_coles2[2]+16,
#                  male_prev_iaic[2]+10,
#                  male_prev_derr[2]+5,
#                  male_prev_asma[2]+5,
#                  male_prev_artreu[2]+15,
#                  male_prev_probcol[2] + 16,
#                  male_prev_dort[2]+4,
#                  male_prev_outramental[2]+4,
#                  male_prev_dpoc[2]+4,
#                  male_prev_ca[2]+4 ,
#                  male_prev_insre[2] + 4,
#                  male_prev_outradc[2] + 6 ,
#                  male_prev_depre_dx_algo[2] + 10,
#                  male_prev_obeoms[2] + 16)

# #Fitting the network
# fit_obj <- mgm(data = data_sub_sex_man,
#                type = rep('c', ncol(data_sub_sex_man)),
#                level = rep(2,ncol(data_sub_sex_man)),
#                lambdaSel = 'EBIC',
#                lambdaGam=0.25,
#                ruleReg = 'AND',
#                binarySign=TRUE)


# #Computing predictability nodes
# pred_obj <- predict(object = fit_obj,
#                     data = data_sub_sex_man,
#                     errorCat = 'CC')

# #Centrality measures
# centRes <- centrality(fit_obj$pairwise$wadj) #tells us about node strength
# centRes$OutDegree
# centRes$Closeness
# centRes$Betweenness
# centRes$ShortestPathLength
# centralityPlot(fit_obj$pairwise$wadj)

# #Centrality
# centralityPlot(fit_obj$pairwise$wadj, include = c("ExpectedInfluence"))

# #Flow
# # flow(fit_obj$pairwise$wadj, "RTI", theme = "colorblind")#, vsize = prev_all)

# Graph_Ising_predictability<-qgraph(fit_obj$pairwise$wadj, # weighted adjacency matrix as input
#                                    layout = 'spring',
#                                    vsize=prev_all_male,
#                                    pie = pred_obj$error[,2], # provide errors as input
#                                    pieColor = rep('#377EB8',ncol(data_sub_sex_man)),
#                                    edge.color = fit_obj$pairwise$edgecolor,
#                                    labels = colnames(data_sub_sex_man))#,
# # groups=as.factor(g2_sg$membership))

# # análise de comunidades
# # https://www.nature.com/articles/srep30750
# g2<-as.igraph(Graph_Ising_predictability)
# # se não tiver arestas negativas:
# g2_cl<-cluster_louvain(g2)

# # se houver arestas negativas
# # g2_sg<-spinglass.community(g2,implementation = "neg")

# rede1_men<-qgraph(fit_obj$pairwise$wadj, # weighted adjacency matrix as input
#                   layout = 'spring',
#                   vsize=prev_all_male,
#                   pie = pred_obj$error[,2], # provide errors as input
#                   pieColor = rep('#377EB8',ncol(data_sub_sex_man)),
#                   edge.color = fit_obj$pairwise$edgecolor,
#                   labels = colnames(data_sub_sex_man),
#                   groups=as.factor(g2_cl$membership))

# ######################################################################
# #Women REGION GRAPH
# ######################################################################
# #Prevalences (proportion with disease/total)

# table(data_sub_sex_women$has2)
# women_prev_hyp<-prop.table(table(data_sub_sex_women$has2))

# table(data_sub_sex_women$diab2)
# women_prev_diab2<-prop.table(table(data_sub_sex_women$diab2))

# table(data_sub_sex_women$coles2)
# women_prev_coles2<-prop.table(table(data_sub_sex_women$coles2))

# table(data_sub_sex_women$iaic)
# women_prev_iaic<-prop.table(table(data_sub_sex_women$iaic))

# table(data_sub_sex_women$derr)
# women_prev_derr<-prop.table(table(data_sub_sex_women$derr))

# table(data_sub_sex_women$asma)
# women_prev_asma<-prop.table(table(data_sub_sex_women$asma))

# table(data_sub_sex_women$artreu)
# women_prev_artreu<-prop.table(table(data_sub_sex_women$artreu))

# table(data_sub_sex_women$probcol)
# women_prev_probcol<-prop.table(table(data_sub_sex_women$probcol))

# table(data_sub_sex_women$dort)
# women_prev_dort <-prop.table(table(data_sub_sex_women$dort))

# table(data_sub_sex_women$outramental)
# women_prev_outramental<-prop.table(table(data_sub_sex_women$outramental))

# table(data_sub_sex_women$dpoc)
# women_prev_dpoc<-prop.table(table(data_sub_sex_women$dpoc))

# table(data_sub_sex_women$ca)
# women_prev_ca<-prop.table(table(data_sub_sex_women$ca))

# table(data_sub_sex_women$insre)
# women_prev_insre<-prop.table(table(data_sub_sex_women$insre))

# table(data_sub_sex_women$outradc)
# women_prev_outradc <- prop.table(table(data_sub_sex_women$outradc))

# table(data_sub_sex_women$depre_dx_algo)
# women_prev_depre_dx_algo <- prop.table(table(data_sub_sex_women$depre_dx_algo))

# table(data_sub_sex_women$obeoms)
# women_prev_obeoms <-prop.table(table(data_sub_sex_women$obeoms))

# prev_all_women<-c(women_prev_hyp[2]+19,
#                   women_prev_diab2[2]+10,
#                   women_prev_coles2[2]+12,
#                   women_prev_iaic[2]+10,
#                   women_prev_derr[2]+6,
#                   women_prev_asma[2]+5,
#                   women_prev_artreu[2]+8,
#                   women_prev_probcol[2] + 16,
#                   women_prev_dort[2]+4,
#                   women_prev_outramental[2]+4,
#                   women_prev_dpoc[2]+4,
#                   women_prev_ca[2]+5,
#                   women_prev_insre[2] + 4,
#                   women_prev_outradc[2] + 5 ,
#                   women_prev_depre_dx_algo[2] + 6,
#                   women_prev_obeoms[2] + 14)

# #Fitting the network
# fit_obj <- mgm(data = data_sub_sex_women,
#                type = rep('c', ncol(data_sub_sex_women)),
#                level = rep(2,ncol(data_sub_sex_women)),
#                lambdaSel = 'EBIC',
#                lambdaGam=0.25,
#                ruleReg = 'AND',
#                binarySign=TRUE)


# #Computing predictability nodes
# pred_obj <- predict(object = fit_obj,
#                     data = data_sub_sex_women,
#                     errorCat = 'CC')

# #Centrality measures
# centRes <- centrality(fit_obj$pairwise$wadj) #tells us about node strength
# centRes$OutDegree
# centRes$Closeness
# centRes$Betweenness
# centRes$ShortestPathLength
# centralityPlot(fit_obj$pairwise$wadj)

# #Centrality
# centralityPlot(fit_obj$pairwise$wadj, include = c("ExpectedInfluence"))

# #Flow
# # flow(fit_obj$pairwise$wadj, "RTI", theme = "colorblind")#, vsize = prev_all)

# Graph_Ising_predictability<-qgraph(fit_obj$pairwise$wadj, # weighted adjacency matrix as input
#                                    layout = 'spring',
#                                    vsize=prev_all_women,
#                                    pie = pred_obj$error[,2], # provide errors as input
#                                    pieColor = rep('#377EB8',ncol(data_sub_sex_women)),
#                                    edge.color = fit_obj$pairwise$edgecolor,
#                                    labels = colnames(data_sub_sex_women))#,
# # groups=as.factor(g2_sg$membership))

# # análise de comunidades
# # https://www.nature.com/articles/srep30750
# g2<-as.igraph(Graph_Ising_predictability)
# # se não tiver arestas negativas:
# g2_cl<-cluster_louvain(g2)

# # se houver arestas negativas
# # g2_sg<-spinglass.community(g2,implementation = "neg")

# rede2_women<-qgraph(fit_obj$pairwise$wadj, # weighted adjacency matrix as input
#                     layout = 'spring',
#                     vsize=prev_all_women,
#                     pie = pred_obj$error[,2], # provide errors as input
#                     pieColor = rep('#377EB8',ncol(data_sub_sex_women)),
#                     edge.color = fit_obj$pairwise$edgecolor,
#                     labels = colnames(data_sub_sex_women),
#                     groups=as.factor(g2_cl$membership))

# #Flow
# # qgraph::flow(Graph_Ising_predictability, "RTI", theme = "colorblind")#, vsize = prev_all2)

# #network comparison analysis
# # NCTObaRom <- NCT(data_complete2,
# #                  data_complete3,
# #                  gamma=0.5,
# #                  it = 1000,
# #                  AND=TRUE,
# #                  binary.data = TRUE,
# #                  paired = FALSE,
# #                  test.edges = TRUE,
# #                  edges = 'all')
# # NCTObaRom$glstrinv.real
# # NCTObaRom$glstrinv.pval
# # NCTObaRom$nwinv.real
# # NCTObaRom$nwinv.pval
# # NCTObaRom$einv.pvals



# # dev.off()
# tiff("/Users/Joao/Desktop/sandro_networks_byregion.tiff",
#      width = 3000, height = 3000,compression = 'lzw', res=300)
# #Add plot
# par(mfrow = c(2, 3))  # 3 rows and 2 columns
# plot(rede1_north)
# title("North region",line=2)
# plot(rede2_northeast)
# title("Northeast region",line=2)
# plot(rede3_midwest)
# title("Mid-west region",line=2)
# plot(rede4_southeast)
# title("Southeast region",line=2)
# plot(rede5_south)
# title("South region",line=2)
# dev.off()

# # dev.off()
# tiff("/Users/Joao/Desktop/sandro_networks_bysex.tiff",
#      width = 3000, height = 3000,compression = 'lzw', res=300)
# #Add plot
# par(mfrow = c(1, 2))  # 3 rows and 2 columns
# plot(rede1_men)
# title("Men",line=2)
# plot(rede2_women)
# title("Women",line=2)
# # plot(rede3_midwest)
# # title("Elderly in the Mid-west region",line=2)
# # plot(rede4_southeast)
# # title("Elderly in the Southeast region",line=2)
# # plot(rede5_south)
# # title("Elderly in the South region",line=2)
# dev.off()