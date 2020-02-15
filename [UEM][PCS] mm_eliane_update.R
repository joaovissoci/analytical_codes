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
library(lattice)
library(MASS)
library(memisc)
        
#remove.packages("qgraph")

library(readstata13)


######################################################################
#IMPORTING DATA
######################################################################

Banco_PNS_Original <- read.dta13("/Users/Joao/Downloads/data4_complete_03092019.dta")


x<-as.data.frame(as.data.set(Stata.file("filename.dta")))


Banco_PNS_Original <- load("/Users/Joao/Downloads/data4_complete_03092019.RData")

#Use this command to remove columns that are entirely NA values, it will elave columns where only some vlaues are NA
#Banco_PNS_Original %>% select_if(~!all(is.na(.))) 

colnames(Banco_PNS_Original)

####################################################################################################
#subset for only individuals who responded to the individual questionnaire
#list of morbidities
# códigos de acordo com o dicionário
summary(Banco_PNS_Original$Q001)

table(Banco_PNS_Original$Q001)

#Subset para as perguntas individuais e nao as perguntas da casa
data_sub <-subset(Banco_PNS_Original, Q001 == 1 | Q001 == 2 | Q001 ==3 | Q001 ==4 | Q001 ==5 | Q001 ==6)

colnames(data_sub)

#data_sub ["iam"] <- NA
#data_sub$iam <- rowSums(data_sub[,536:537])
# criar uma nova variável com duas colunas, mantendo a caracteristica binária
data_sub$ic <- case_when(data_sub$Q06301==1 & data_sub$Q06302==1 & data_sub$Q06303 ==1 ~ 1, 
                          data_sub$Q06301==1 & data_sub$Q06302==2 & data_sub$Q06303 ==1 ~ 1,
                          data_sub$Q06301==1 & data_sub$Q06302==2 & data_sub$Q06303 ==2 ~ 1,
                          data_sub$Q06301==2 & data_sub$Q06302==1 & data_sub$Q06303 ==1 ~ 1,
                          data_sub$Q06301==2 & data_sub$Q06302==2 & data_sub$Q06303 ==1 ~ 1,
                          data_sub$Q06301==2 & data_sub$Q06302==2 & data_sub$Q06303 ==2 ~ 0)
                       

 #data_sub$dpoc <- data_sub$Q11603 +data_sub$Q11601
data_sub$dpoc <- case_when(data_sub$Q11603==1 & data_sub$Q11601==1 ~ 1, 
                           data_sub$Q11603==1 & data_sub$Q11601==2 ~ 1,
                           data_sub$Q11603==2 & data_sub$Q11601==1 ~ 1,
                           data_sub$Q11603==2 & data_sub$Q11601==2 ~ 2)

colnames(data_sub)
unique(data_sub$dpoc) 

#data_sub$asma <- data_sub$Q074 +data_sub$Q11602

data_sub$asma <- case_when(data_sub$Q074==1 & data_sub$Q11602==1 ~ 1, 
                           data_sub$Q074==1 & data_sub$Q11602==2 ~ 1,
                           data_sub$Q074==2 & data_sub$Q11602==1 ~ 1,
                           data_sub$Q074==2 & data_sub$Q11602==2 ~ 2)
colnames(data_sub)
unique(data_sub$asma) 

data_sub$peso <- data_sub$P00101
data_sub$imc <- data_sub$peso/(data_sub$P00401/100)^2   #  note correct height to m from cm
summary(data_sub$imc)
data_sub$bmi <- car::recode(data_sub$imc, "10.56:29.99999 = '0'; 30.00000:97.00000= '1'" )
unique(data_sub$bmi)


#data_sub$esquizo <- data_sub$Q11001
#data_sub$bipolar <- data_sub$Q11002
#data_sub$toc <- data_sub$Q11003

#Doença mental
data_sub$EZBIPOCD <- case_when(data_sub$Q11001==1 & data_sub$Q11002==1 & data_sub$Q11003 ==1 ~ 1, 
                               data_sub$Q11001==1 & data_sub$Q11002==2 & data_sub$Q11003 ==2 ~ 1,
                               data_sub$Q11001==2 & data_sub$Q11002==1 & data_sub$Q11003 ==1 ~ 1,
                               data_sub$Q11001==2 & data_sub$Q11002==2 & data_sub$Q11003 ==1 ~ 1,
                               data_sub$Q11001==2 & data_sub$Q11002==2 & data_sub$Q11003 ==2 ~ 0)







data_sub$hypertension <- data_sub$Q002
data_sub$diabetes <- data_sub$Q030
data_sub$cholesterol <- data_sub$Q060
data_sub$Derrame <- data_sub$Q068
data_sub$artreu <- data_sub$Q079
data_sub$probcol <- data_sub$Q084
data_sub$dort <- data_sub$Q088
data_sub$cancer <- data_sub$Q120
data_sub$ins_renal <- data_sub$Q124
data_sub$outradc <- data_sub$Q128
data_sub$dxdepre <- data_sub$Q092
data_sub$RTI <- data_sub$O009
data_sub$drink <- data_sub$P032
data_sub$altura <- data_sub$P00401
data_sub$sex <- data_sub$C006
data_sub$race <- data_sub$C009
data_sub$escol <- data_sub$D009
data_sub$vc <- data_sub$C010
data_sub$plano <- data_sub$I001
data_sub$UF <- data_sub$V0001
data_sub$zone <- data_sub$V0026 
data_sub$age <- data_sub$C008
data_sub$id_suic <- data_sub$N018



save (data_sub, file = "data_sub_Suicide_16092019.dta")
getwd()




#setwd("E:/Documentos")
#data_sub_all <- read_dta ("E:\\Documentos\\data_sub_Suicide_16092019.dta")

data_sub_all<-with(data_sub,data.frame(ic,
                                      dpoc,
                                      asma,
                                      EZBIPOCD,
                                      hypertension,
                                      diabetes,
                                      cholesterol,
                                      Derrame,
                                      artreu,
                                      probcol,
                                      dort,
                                      cancer,
                                      bmi,
                                      ins_renal,
                                      outradc,
                                      dxdepre,
                                      RTI,
                                      drink,
                                      vc,
                                      id_suic,
                                      altura,
                                      sex,
                                      age,
                                      race,
                                      escol,
                                      plano,
                                      UF,
                                      zone))
                                      
                                     
colnames(data_sub_all)






#data_sub_all <- read_dta ("E:\\Documentos\\data_sub_Suicide_03082019.dta")


data_sub_all$region <- car::recode(data_sub_all$UF, "11:17 = '1'; 21:29 = '2'; 31:35 = '3'; 41:43 = '4'; 50:53 = '5'" )

#######################################################################################################
#recode ami
str(data_sub_all$ic)
unique(data_sub_all$ic)
#data_sub_all$iam<- car::recode (data_sub_all$iam, "2=0; 1=1") # 2= 0 (no) 1=1 ( yes)


#recode dpoc
str(data_sub_all$dpoc)
unique(data_sub_all$dpoc)
data_sub_all$dpoc<- car::recode (data_sub_all$dpoc, "2=0; 1=1")

#recode asma
str(data_sub_all$asma)
unique(data_sub_all$asma)
data_sub_all$asma<- car::recode (data_sub_all$asma, "2=0; 1=1")

#recode esquizo, bipolar, toc
str(data_sub_all$EZBIPOCD)
unique(data_sub_all$EZBIPOCD)
#data_sub_all$EZBIPOCD<- car::recode (data_sub_all$EZBIPOCD, "2=0; 1=1")

#recode bipolar
#str(data_sub_all$bipolar)
#unique(data_sub_all$bipolar)
#data_sub_all$bipolar<- car::recode (data_sub_all$bipolar, "2=0; 1=1")

#recode toc
#str(data_sub_all$toc)
#unique(data_sub_all$toc)
#data_sub_all$toc<- car::recode (data_sub_all$toc, "2=0; 1=1")

#recode hypertension
str(data_sub_all$hypertension)
unique(data_sub_all$hypertension)
data_sub_all$hypertension<- car::recode (data_sub_all$hypertension, "2:3=0; 1=1")
unique(data_sub_all$hypertension)


#recode diabetes
str(data_sub_all$diabetes)
unique(data_sub_all$diabetes)
data_sub_all$diabetes<- car::recode (data_sub_all$diabetes, "2:3=0; 1=1")
unique(data_sub_all$diabetes)

#recode cholesterol
str(data_sub_all$cholesterol)
unique(data_sub_all$cholesterol)
data_sub_all$cholesterol<- car::recode (data_sub_all$cholesterol, "2=0; 1=1")

#recode hemodialisis
#str(data_sub_all$hemodialisis)
#unique(data_sub_all$hemodialisis)
#data_sub_all$hemodialisis<- car::recode (data_sub_all$hemodialisis, "2=0; 1=1")

#recode Derrame
str(data_sub_all$Derrame)
unique(data_sub_all$Derrame)
data_sub_all$Derrame<- car::recode (data_sub_all$Derrame, "2=0; 1=1")

#recode artreu
str(data_sub_all$artreu)
unique(data_sub_all$artreu)
data_sub_all$artreu<- car::recode (data_sub_all$artreu, "2=0; 1=1")

#recode probcol
str(data_sub_all$probcol)
unique(data_sub_all$probcol)
data_sub_all$probcol<- car::recode (data_sub_all$probcol, "2=0; 1=1")

#recode dort
str(data_sub_all$dort)
unique(data_sub_all$dort)
data_sub_all$dort<- car::recode (data_sub_all$dort, "2=0; 1=1")

#recode outramental
#str(data_sub_all$outramental)
#unique(data_sub_all$outramental)
#data_sub_all$outramental<- car::recode (data_sub_all$outramental, "2=0; 1=1")

#recode cancer
str(data_sub_all$cancer)
unique(data_sub_all$cancer)
data_sub_all$cancer<- car::recode (data_sub_all$cancer, "2=0; 1=1")

#recode insre
str(data_sub_all$ins_renal)
unique(data_sub_all$ins_renal)
data_sub_all$ins_renal<- car::recode (data_sub_all$ins_renal, "2=0; 1=1")

#recode outradc
str(data_sub_all$outradc)
unique(data_sub_all$outradc)
data_sub_all$outradc<- car::recode (data_sub_all$outradc, "2=0; 1=1")

#recode dxdepre
str(data_sub_all$dxdepre)
unique(data_sub_all$dxdepre)
data_sub_all$dxdepre<- car::recode (data_sub_all$dxdepre, "2=0; 1=1")

#recode RTI
str(data_sub_all$RTI)
unique(data_sub_all$RTI)
data_sub_all$RTI<- car::recode (data_sub_all$RTI, "2=0; 1=1")

#recode drink
str(data_sub_all$drink)
unique(data_sub_all$drink)
data_sub_all$drink<- car::recode (data_sub_all$drink, "2=0; 1=1") # 2= 0 (no drink) 1=1 ( yes drink)

#recode suicidal ideation
str(data_sub_all$id_suic)
unique(data_sub_all$id_suic)
data_sub_all$id_suic<- car::recode (data_sub_all$id_suic, "1:2=0; 3:4=1") # 1:2= 0 (no ideation) 3:4=1 (ideation)

#recode live_someone
str(data_sub_all$vc)
unique(data_sub_all$vc)
data_sub_all$vc<- car::recode (data_sub_all$vc, "1=0; 2=1") # 1= 0 (vive parceiro) 2=1 (vive sozinho)


data_sub_all$sum <- apply(data_sub_all[, 1:20], 1, sum, na.rm = T)

str(data_sub_all$sum)
unique(data_sub_all$sum)

data_sub_all$mm2<- car::recode (data_sub_all$sum, "2='1';else='0'") # 2= 1 ( 2 morbidades) 0 ( todos os outros valores)

data_sub_all$mm3<- car::recode (data_sub_all$sum, "0:2='0';else='1'") # 1:2= 0 (  < = que 2 morbidades) 1 ( > maior que 2 morbidades)

view(data_sub_all)

data_sub_region_north<-subset(data_sub_all,data_sub_all$region==1)
data_sub_region_northeast<-subset(data_sub_all,data_sub_all$region==2)
data_sub_region_southeast<-subset(data_sub_all,data_sub_all$region==3)
data_sub_region_south<-subset(data_sub_all,data_sub_all$region==4)
data_sub_region_mid_west<-subset(data_sub_all,data_sub_all$region==5)

#data_sub_sex_man<-subset(data_sub_all,data_sub_all$sex==1)
#data_sub_sex_women<-subset(data_sub_all,data_sub_all$sex==2)

######################################################################
#DATA MANAGEMENT
######################################################################
# Rename in English

names(data_sub_all)[names(data_sub_all) == "ic"] <- "ci"
names(data_sub_all)[names(data_sub_all) == "hypertension"] <- "hyp"
names(data_sub_all)[names(data_sub_all) == "bmi"] <- "obesity"
names(data_sub_all)[names(data_sub_all) == "diabetes"] <- "DM"
names(data_sub_all)[names(data_sub_all) == "cholesterol"] <- "cholesterol"
names(data_sub_all)[names(data_sub_all) == "outradc"] <- "cd_other"
names(data_sub_all)[names(data_sub_all) == "Derrame"] <- "stroke"
names(data_sub_all)[names(data_sub_all) == "asma"] <- "asthma"
names(data_sub_all)[names(data_sub_all) == "artreu"] <- "arth_rheu"
names(data_sub_all)[names(data_sub_all) == "dort"] <- "osteomusc"
names(data_sub_all)[names(data_sub_all) == "dxdepre"] <- "depression"
names(data_sub_all)[names(data_sub_all) == "EZBIPOCD"] <- "ezbipocd"
names(data_sub_all)[names(data_sub_all) == "dpoc"] <- "cpod"
names(data_sub_all)[names(data_sub_all) == "ca"] <- "cancer"
names(data_sub_all)[names(data_sub_all) == "ins_renal"] <- "ckd"
names(data_sub_all)[names(data_sub_all) == "probcol"] <- "spinalprob"
names(data_sub_all)[names(data_sub_all) == "id_suic"] <- "id_suic"
names(data_sub_all)[names(data_sub_all) == "vc"] <- "partner"
names(data_sub_all)[names(data_sub_all) == "plano"] <- "Plan"
names(data_sub_all)[names(data_sub_all) == "region"] <- "region"
names(data_sub_all)[names(data_sub_all) == "zone"] <- "zone"
names(data_sub_all)[names(data_sub_all) == "RTI"] <- "rti"
names(data_sub_all)[names(data_sub_all) == "drink"] <- "drink"
names(data_sub_all)[names(data_sub_all) == "Bipolar"] <- "Bipolar"
names(data_sub_all)[names(data_sub_all) == "sex"] <- "sex"
names(data_sub_all)[names(data_sub_all) == "age"] <- "age"
names(data_sub_all)[names(data_sub_all) == "race"] <- "race"
names(data_sub_all)[names(data_sub_all) == "escol"] <- "school"

colnames(data_sub_all)

#age
data_sub_all$agecat <- car::recode(data_sub_all$age, "18:24 = '0'; 25:44 = '1'; 45:64 = '2'; 65:84 = '3'; 85:109 = '4'" )
#unique (data_sub_all$agecat)
data_sub_all$agegroups <- factor(data_sub_all$agecat, labels = c("18-24", "25-44", "45-64", "65-84", "85 ou mais"))

#escolaridade

#unique (data_sub_all$school)
data_sub_all$schooling <- car::recode(data_sub_all$school, "1 = '1'; 2:4 = '2'; 5:8 = '3'; 9:11 = '4'; 12 = '5'  ")
data_sub_all$sch_group<- factor(data_sub_all$schooling, labels = c("no schooling", "elementary", "middle", " high ", "university"))



#data_sub_all %>%
 # mutate(Alcohol=case_when(Q06301 == "1"|
                             #Q06301== "2"|
                             #Q06302==  "0" ~ 0,
                          # Q06302== "1" ~ 1)) -> data_sub_all 


head(data_sub_all)
tail(data_sub_all)

colnames(data_sub_all)
save (data_sub, file = "data_sub_all.dta")


#Calculating frequency of missing data per variable
propmiss <- function(dataframe) lapply(dataframe,function(x) data.frame(nmiss=sum(is.na(x)), n=length(x), propmiss=sum(is.na(x))/length(x)))
propmiss(data_sub_all)


#creat smaller dataset with just diseases ( only  binaries variables)
newdata4 <- with (data_sub_all, data.frame(hyp, DM,cholesterol, stroke, arth_rheu, spinalprob, ckd, depression, osteomusc, cancer, rti, id_suic))

#creat smaller dataset with diseases + demographic variables
newdata5 <- with (data_sub_all, data.frame(hyp, DM,cholesterol, stroke, arth_rheu, spinalprob, ckd, depression, osteomusc, cancer, rti, cd_other, id_suic, sex, age, race, schooling, agecat, region, mm2,mm3 ))



###############################################################################################################

#######################################################
#ANALYZING MISSING DATA
#######################################################

#Studying missing data ( posso ter essa opção, como também posso omitir os casos não completos)
#data <- na.omit(data)
##remove incomplete cases from newdata4
#newdata4[complete.cases(newdata4), ] #Keep only complete rows
#data4_complete <- newdata4[complete.cases(newdata4), ] #store complete cases subset


#remove incomplete cases from newdata4
newdata4[complete.cases(newdata4), ] #Keep only complete rows
data4_complete <- newdata4[complete.cases(newdata4), ] #store complete cases subset


# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
imp <- mice(newdata4, seed = 2222, m=10)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
data_imputed<-mice::complete(imp,2)

# newdata4[complete.cases(newdata4), ] #Keep only complete rows
data4_complete <- data_imputed

str(data4_complete)

save (data_sub, file = "data4_complete_03092019.dta")
getwd()


##########################################################################################################
head(data4_complete)
tail(data4_complete)


#data4_complete$sex<-as.numeric(data4_complete$sex)
#data4_complete$cor<-as.numeric(data4_complete$cor)
#data4_complete$educ<-as.numeric(data4_complete$educ)
#data4_complete$agecat<-as.integer(data4_complete$agecat)
#data4_complete$drinked<-as.numeric(data4_complete$drinked)
#data4_complete$RTINJUR<-as.numeric(data4_complete$RTINJUR)
#data4_complete$regiao<-as.numeric(as.character(data4_complete$regiao))

######################################################################
#OVERALL GRAPH
######################################################################
#Prevalences (proportion with disease/total)

#table(data4_complete$ami)
#prev_ami<-prop.table(table(data4_complete$ami))

#table(data4_complete$cpod)
#prev_diab<-prop.table(table(data4_complete$cpod))
str(data4_complete$cholesterol)
data4_complete$cholesterol<-as.numeric(data4_complete$cholesterol)
table(data4_complete$cholesterol)
prop.table(table(data4_complete$cholesterol))
prev_choles<-prop.table(table(data4_complete$cholesterol))
table(data4_complete$cholesterol, data4_complete$id_suic)
prop.table(table(data4_complete$cholesterol, data4_complete$id_suic))
prop.table(table(data4_complete$cholesterol, data4_complete$id_suic))*100
chisq.test(data4_complete$cholesterol, data4_complete$id_suic)

#dado.p <-svydesign (ids = ~1, weights = newdata2$V0029 , strata = newdata2$V0024, data = newdata2)

str(data4_complete$hyp)
table(data4_complete$hyp)
prop.table(table(data4_complete$hyp))
prev_hyp<-prop.table(table(data4_complete$hyp))
table(data4_complete$hyp, data4_complete$id_suic)
prop.table(table(data4_complete$hyp, data4_complete$id_suic))
prop.table(table(data4_complete$hyp, data4_complete$id_suic))*100
chisq.test(data4_complete$hyp, data4_complete$id_suic)


table(data4_complete$stroke)
prop.table(table(data4_complete$stroke))
prev_stroke<-prop.table(table(data4_complete$stroke))
table(data4_complete$stroke, data4_complete$id_suic)
prop.table(table(data4_complete$stroke, data4_complete$id_suic))
prop.table(table(data4_complete$stroke, data4_complete$id_suic))*100
chisq.test(data4_complete$stroke, data4_complete$id_suic)


table(data4_complete$arth_rheu)
prop.table(table(data4_complete$arth_rheu))
prev_arth_rheu<-prop.table(table(data4_complete$arth_rheu))
table(data4_complete$arth_rheu, data4_complete$id_suic)
prop.table(table(data4_complete$arth_rheu, data4_complete$id_suic))
prop.table(table(data4_complete$arth_rheu, data4_complete$id_suic))*100
chisq.test(data4_complete$arth_rheu, data4_complete$id_suic)

table(data4_complete$spinalprob)
prev_spinalprob<-prop.table(table(data4_complete$spinalprob))
prop.table(table(data4_complete$spinalprob))
table(data4_complete$spinalprob, data4_complete$id_suic)
prop.table(table(data4_complete$spinalprob, data4_complete$id_suic))
prop.table(table(data4_complete$spinalprob, data4_complete$id_suic))*100
chisq.test(data4_complete$spinalprob, data4_complete$id_suic)

table(data4_complete$ckd)
prev_ckd <-prop.table(table(data4_complete$ckd))
prop.table(table(data4_complete$ckd))
table(data4_complete$ckd, data4_complete$id_suic)
prop.table(table(data4_complete$ckd, data4_complete$id_suic))
prop.table(table(data4_complete$ckd, data4_complete$id_suic))*100
chisq.test(data4_complete$ckd, data4_complete$id_suic)

table(data4_complete$depression)
prev_depression<-prop.table(table(data4_complete$depression))
prop.table(table(data4_complete$depression))
table(table (data4_complete$depression, data4_complete$id_suic))
prop.table(table(data4_complete$depression, data4_complete$id_suic))
prop.table(table(data4_complete$depression, data4_complete$id_suic))*100
chisq.test(data4_complete$depression, data4_complete$id_suic)


table(data4_complete$osteomusc)
prev_osteomusc<-prop.table(table(data4_complete$work_osteomusc))
prop.table(table(data4_complete$osteomusc))
table(data4_complete$osteomusc, data4_complete$id_suic)
prop.table(table(data4_complete$osteomusc, data4_complete$id_suic))
prop.table(table(data4_complete$osteomusc, data4_complete$id_suic))*100
chisq.test(data4_complete$osteomusc, data4_complete$id_suic)

#table(data4_complete$cd_other)
#prev_dcother<-prop.table(table(data4_complete$cd_other))
#prop.table(data4_complete$cd_other)
#table(data4_complete$cd_other, data4_complete$id_suic)
#prop.table(table(data4_complete$cd_other, data4_complete$id_suic))
#prop.table(table(data4_complete$cd_other, data4_complete$id_suic))*100
#fisher.test(data4_complete$cd_other, data4_complete$id_suic)

#table(data4_complete$schooling)
#prev_schooling<-prop.table(table(data4_complete$schooling))

table(data4_complete$cancer)
prev_cancer <-  prop.table(table(data4_complete$cancer))
prop.table(table(data4_complete$cancer))
table(data4_complete$cancer, data4_complete$id_suic)
prop.table(table(data4_complete$cancer, data4_complete$id_suic))
prop.table(table(data4_complete$cancer, data4_complete$id_suic))*100
chisq.test(data4_complete$cancer, data4_complete$id_suic)

#table(data4_complete$mentalother)
#prev_mentalother <- prop.table(table(data4_complete$mentalother))

table(data4_complete$rti)
prev_rti <-prop.table(table(data4_complete$rti))
prop.table(table(data4_complete$rti))
table(data4_complete$rti, data4_complete$id_suic)
prop.table(table(data4_complete$rti, data4_complete$id_suic))
prop.table(table(data4_complete$rti, data4_complete$id_suic))*100
chisq.test(data4_complete$rti, data4_complete$id_suic)

#table(data4_complete$obesity)
#prev_obesity <-prop.table(table(data4_complete$obesity))

#table(data4_complete$ drink)
#prev_drink <-prop.table(table(data4_complete$drink))

#table(data4_complete$ schizo)
#prev_schizo <-prop.table(table(data4_complete$schizo))

#table(data4_complete$bipolar)
#prev_schizo <-prop.table(table(data4_complete$bipolar))

#table(data4_complete$ocd)
#prev_ocd <-prop.table(table(data4_complete$ocd))

#table(data4_complete$agecat)
#prev_agecat <-prop.table(table(data4_complete$agecat))

#table(data4_complete$health_insur)
#prev_health_insur <-prop.table(table(data4_complete$health_insur))

table(data4_complete$DM)
prev_dm<-prop.table(table(data4_complete$DM))
prop.table(table(data4_complete$DM))
table(data4_complete$DM, data4_complete$id_suic)
prop.table(table(data4_complete$DM, data4_complete$id_suic))
prop.table(table(data4_complete$DM, data4_complete$id_suic))*100
chisq.test(data4_complete$DM, data4_complete$id_suic)

#table(data4_complete$partner)
#prev_partner<-prop.table(table(data4_complete$live_someone))

#data4_complete$id_suic<-as.numeric(data4_complete$id_suic)
table(data4_complete$id_suic)
#data4_complete$cholesterol<-factor (data4_complete$cholesterol, labels = c("0","1"))
prev_id_suic<-prop.table(table(data4_complete$id_suic))
prop.table(table(data4_complete$id_suic))
prop.table(table(data4_complete$id_suic))*100




prev_all<-c(prev_hyp[3]+20,
            prev_dm[2]+20,
            prev_choles[2]+10,
            prev_stroke[2]+20,
            prev_arth_rheu[2]+12,
            prev_spinalprob[2] + 15,
            prev_ckd[2]+15,
            prev_depression[2]+15,
            prev_osteomusc[2]+12,
            prev_cancer[2] + 15,
            prev_rti [2] + 5,
            prev_id_suic[2] + 15 )
           


#Fitting the network
#https://jmbh.github.io/Predictability-in-network-models/


fit_obj <- mgm(data = data4_complete,
               type =  rep('c', ncol(data4_complete)),
               level = rep(2,ncol(data4_complete)),
               lambdaSel = 'EBIC',
               lambdaGam=0.25,
               ruleReg = 'AND',
               moderators = 1:12,
               binarySign=TRUE)

fit_obj$interactions$indicator #(explicar)

showInteraction(object = fit_obj, int = c(3,9,12))

colnames(data4_complete)

fit_g<-qgraph(fit_obj$pairwise$wadj, labels=colnames(data4_complete))

flow(fit_g,"id_suic")

FactorGraph(object = fit_obj ,
            edge.labels = TRUE,
            labels = colnames(data4_complete))

FactorGraph(object = fit_obj,
            edge.labels = TRUE,
            Nodewise = TRUE,
           labels = colnames(data4_complete))


FactorGraph(object = fit_obj,
           edge.labels = TRUE,
          PairwiseAsEdge = TRUE,
            labels = colnames(data4_complete))




##########################################################################################

#Computing predictability nodes
pred_obj <- predict(object = fit_obj, data = data4_complete,
                    errorCat = c("CC"))

pred_obj$errors


#Table 2. Weight Adjacency Matrix - Ver se dá para rodar uma tabela dessa

# REVISAR ESSA PARTE

error_list <- list() # List for ring-segments
for(i in 1:14) error_list[[i]] <- pred_obj$errors[i,2]
beyondmarg <- pred_obj$errors[14,2]-pred_obj$errors[14,5]
error_list[[14]] <- c(pred_obj$errors[14,5],beyondmarg)
color_list <- list() # List for Colors
for(i in 1:13) color_list[[i]] <- "#90B4D4"
color_list[[14]] <- c("#ffa500", "#ff4300")


#Centrality measures

centRes <- centrality(fit_obj$pairwise$wadj) #tells us about node strength
centRes$OutDegree
centRes$Closeness
centRes$Betweenness
centRes$ShortestPathLength
centralityPlot(fit_obj$pairwise$wadj)

#Centrality
centralityPlot(fit_obj$pairwise$wadj, include = c("ExpectedInfluence"), labels = colnames(data4_complete), orderBy = "ExpectedInfluence" )

#Flow
flow(fit_g, 12, theme = "colorblind")#, vsize = prev_all)
flow


Graph_Ising_predictability <- qgraph(fit_obj$pairwise$wadj, # weighted adjacency matrix as input
                                   layout = "spring",
                                  
                                  pie = pred_obj$errors[,2], # provide errors as input
                                   pieColor = rep('#377EB8',ncol(data4_complete)),
                                   edge.color = fit_obj$pairwise$edgecolor,
                                  edge.labels = TRUE,
                                   curveAll = FALSE, curveDefault = .3, cut = 0,
                                   labels = colnames(data4_complete))#



Graph_Ising_predictability

# groups=as.factor(g2_sg$membership))

# análise de comunidades
# https://www.nature.com/articles/srep30750

g2<-as.igraph(Graph_Ising_predictability)

#clusters    = g.clusters()
#giant       = clusters.giant() ## using the biggest component as an example, you can use the others here.
#communities = giant.community_spinglass()

# se não tiver arestas negativas:
#g2_cl<-cluster_louvain(g2)


#g2 <- as.igraph(Graph_Ising_predictability)
#clu <- components(g2)
#groups(clu)


#se houver arestas negativas
g2_sg<-spinglass.community(g2,implementation = "neg")



tiff("E:/Documentos/script R/first_netowrk1.tiff",
     width = 3000, height = 3000,compression = 'lzw', res=300)
#Add plot
Graph_Ising_predictability<-qgraph(fit_obj$pairwise$wadj, # weighted adjacency matrix as input
                                   layout = 'spring',
                                   vsize=prev_all,
                                   pie = pred_obj$errors[,2], # provide errors as input
                                   pieColor = rep('#377EB8',ncol(data4_complete)),
                                   edge.color = fit_obj$pairwise$edgecolor,
                                   curveAll = TRUE, curveDefault = .3, cut = 0,
                                   labels = colnames(data4_complete),
                                   groups=as.factor(g2_sg$membership))

title("Overall multimorbidity pattern",line=2)

dev.off()
