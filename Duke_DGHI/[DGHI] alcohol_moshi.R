###################################################
#TEMPLATE_Alcohol_Moshi_dataanalysis#
#this script follows a combination of guidelines proposed by Doebler and Holling, according to (http://cran.r-project.org/web/packages/mada/vignettes/mada.pdf)#
#
#
###################################################
#SETTING ENVIRONMENT
###################################################
#install.packages("mada")
#library("mada")
#install.packages("car", repos="http://cran.r-project.org")
#install.packages("ggplot2", repos="http://cran.r-project.org")

#devtools::install_github("cran/epicalc")
#devtools::install_github("BioStatMatt/sas7bdat")

#Load packages (after installed) with the library function
lapply(c("metafor","ggplot2","gridExtra" ,"psych", 
   "RCurl", "irr", "nortest", "moments","GPArotation",
   "nFactors","gdata","meta","metafor","ggplot2",
   "gridExtra" ,"psych", "RCurl", "irr", "nortest", 
   "moments","GPArotation","nFactors","gdata",
   "repmis","sqldf","VIM","survival",
   "sas7bdat","epicalc","vcd","mice"), library, character.only=T)

#Dosage specific ODSS function
#source: http://goo.gl/ETWdZd
# doseSpecificOddsRatios <- function(mymatrix,referencerow=1)
# {
#    numstrata <- nrow(mymatrix)
#    # calculate the stratum-specific odds ratios, and odds of disease:
#    doses <- as.numeric(rownames(mymatrix))
#    for (i in 1:numstrata)
#    {
#       dose <- doses[i]
#       # calculate the odds ratio:
#       DiseaseExposed <- mymatrix[i,1]
#       DiseaseUnexposed <- mymatrix[i,2]
#       ControlExposed <- mymatrix[referencerow,1]
#       ControlUnexposed <- mymatrix[referencerow,2]
#       totExposed <- DiseaseExposed + ControlExposed
#       totUnexposed <- DiseaseUnexposed + ControlUnexposed
#       probDiseaseGivenExposed <- DiseaseExposed/totExposed
#       probDiseaseGivenUnexposed <- DiseaseUnexposed/totUnexposed
#       probControlGivenExposed <- ControlExposed/totExposed
#       probControlGivenUnexposed <- ControlUnexposed/totUnexposed
#       oddsRatio <- (probDiseaseGivenExposed*probControlGivenUnexposed)/
#                    (probControlGivenExposed*probDiseaseGivenUnexposed)
#       print(paste("dose =", dose, ", odds ratio = ",oddsRatio))
#    }
# }
###################################################
#IMPORTING DATA AND RECODING
###################################################
#Instructions here http://goo.gl/Ofa7gQ
#data <- repmis::source_DropboxData("alcohol_moshi.csv","wkcecvvrvert5h9",sep = ",",header = TRUE)
#data<-
#data<-read.sas7bdat("C:\\Users\\Joao\\Desktop\\tanzclean.sas7bdat")
#data<-read.sas7bdat("/Users/rpietro/Dropbox/datasets/Africa_DGHI/tanzclean.sas7bdat")
data<-read.sas7bdat("/Users/jnv4/OneDrive - Duke University/datasets/Global EM/Africa/tanzclean.sas7bdat")
data<-as.data.frame(data)
###################################################
#DATA MANAGEMENT
###################################################
#### CREATING ETHANOL AMOUT / ml data

c9999toNA<-function(x){
   car::recode(x,"9999=NA")
}
data<-lapply(data,c9999toNA)

#Recoding QB01a from hours to minutes
#data$QB01a<-data$QB01a*60

#Creating QB01 (summing up QB01A and QB01b)
#data$QB01<-NULL
#data$QB01<-with(data,rowSums(data.frame(QB01a,QB01b)))

#Creating GCS score
#gcs<-with(data,rowSums(data.frame(QL02,QL02a,QL02b)))

#Alcohol Dosage - Time 1
# alcohol_amount_1_rowB<-with(data,(as.numeric(QF07B1B)*as.numeric(QF07B2))*as.numeric(QF07B3))
# alcohol_amount_1_rowC<-with(data,(as.numeric(QF07C1B)*as.numeric(QF07C2))*as.numeric(QF07C3))
# alcohol_amount_1_rowD<-with(data,(as.numeric(QF07D1B)*as.numeric(QF07D2))*as.numeric(QF07D3))
# alcohol_amount_1_rowE<-with(data,(as.numeric(QF07E1B)*as.numeric(QF07E2))*as.numeric(QF07E3))
# alcohol_amount_1_rowF<-with(data,(as.numeric(QF07F1B)*as.numeric(QF07F2))*as.numeric(QF07F3))
# alcohol_amount_1_rowG<-with(data,(as.numeric(QF07G1B)*as.numeric(QF07G2))*as.numeric(QF07G3))
# alcohol_amount_1_rowH<-with(data,(as.numeric(QF07H1B)*as.numeric(QF07H2))*as.numeric(QF07H3))
# alcohol_amount_1_rowL1<-with(data,(as.numeric(QF07L11B)*as.numeric(QF07L12))*as.numeric(QF07L13))
# alcohol_amount_1_rowL2<-with(data,(as.numeric(QF07L21B)*as.numeric(QF07L22))*as.numeric(QF07L23))
# alcohol_amount_1_rowL3<-with(data,(as.numeric(QF07L31B)*as.numeric(QF07L32))*as.numeric(QF07L33))
alcohol_amount_1<-with(data,data.frame(
                     QF07B5,
                     QF07C5,
                     QF07D5,
                     QF07E5,
                     QF07F5,
                     QF07G5,
                     QF07H5,
                     QF07L15,
                     QF07L25,
                     QF07L35))

NaNtoNA<-function(x){
   car::recode(x,"NaN=0")
}

alcohol_amount_1<-lapply(alcohol_amount_1,NaNtoNA)
alcohol_amount_1<-rowSums(as.data.frame(alcohol_amount_1))

#Alcohol Dosage - Time 2
# alcohol_amount_2_rowB<-with(data,(as.numeric(QNH05B1B)*as.numeric(QNH05B2))*as.numeric(QNH05B3))
# alcohol_amount_2_rowC<-with(data,(as.numeric(QNH05C1B)*as.numeric(QNH05C2))*as.numeric(QNH05C3))
# alcohol_amount_2_rowD<-with(data,(as.numeric(QNH05D1B)*as.numeric(QNH05D2))*as.numeric(QNH05D3))
# alcohol_amount_2_rowE<-with(data,(as.numeric(QNH05E1B)*as.numeric(QNH05E2))*as.numeric(QNH05E3))
# alcohol_amount_2_rowF<-with(data,(as.numeric(QNH05F1B)*as.numeric(QNH05F2))*as.numeric(QNH05F3))
# alcohol_amount_2_rowG<-with(data,(as.numeric(QNH05G1B)*as.numeric(QNH05G2))*as.numeric(QNH05G3))
# alcohol_amount_2_rowH<-with(data,(as.numeric(QNH05H1B)*as.numeric(QNH05H2))*as.numeric(QNH05H3))
# alcohol_amount_2_rowL1<-with(data,(as.numeric(QNH05L11B)*as.numeric(QNH05L12))*as.numeric(QNH05L13))
# alcohol_amount_2_rowL2<-with(data,(as.numeric(QNH05L21B)*as.numeric(QNH05L22))*as.numeric(QNH05L23))
# alcohol_amount_2_rowL3<-with(data,(as.numeric(QNH05L31B)*as.numeric(QNH05L32))*as.numeric(QNH05L33))
alcohol_amount_2<-with(data,data.frame(
            QNH05B5,
            QNH05C5,
            QNH05D5,
            QNH05E5,
            QNH05F5,
            QNH05G5,
            QNH05H5,
            QNH05L15,
            QNH05L25,
            QNH05L35))

alcohol_amount_2<-lapply(alcohol_amount_2,NaNtoNA)
alcohol_amount_2<-rowSums(as.data.frame(alcohol_amount_2))

#Alcohol Dosage - Time 3

# alcohol_amount_3_rowB<-with(data,(as.numeric(QG02B1B)*as.numeric(QG02B2))*as.numeric(QG02B3))
# alcohol_amount_3_rowC<-with(data,(as.numeric(QG02C1B)*as.numeric(QG02C2))*as.numeric(QG02C3))
# alcohol_amount_3_rowD<-with(data,(as.numeric(QG02D1B)*as.numeric(QG02D2))*as.numeric(QG02D3))
# alcohol_amount_3_rowE<-with(data,(as.numeric(QG02E1B)*as.numeric(QG02E2))*as.numeric(QG02E3))
# alcohol_amount_3_rowF<-with(data,(as.numeric(QG02F1B)*as.numeric(QG02F2))*as.numeric(QG02F3))
# alcohol_amount_3_rowG<-with(data,(as.numeric(QG02G1B)*as.numeric(QG02G2))*as.numeric(QG02G3))
# alcohol_amount_3_rowH<-with(data,(as.numeric(QG02H1B)*as.numeric(QG02H2))*as.numeric(QG02H3))
# alcohol_amount_3_rowL1<-with(data,(as.numeric(QG02L11B)*as.numeric(QG02L12))*as.numeric(QG02L13))
# alcohol_amount_3_rowL2<-with(data,(as.numeric(QG02L21B)*as.numeric(QG02L22))*as.numeric(QG02L23))
alcohol_amount_3<-with(data,data.frame(
            QH03B5,
            QH03C5,
            QH03D5,
            QH03E5,
            QH03F5,
            QH03G5,
            QH03H5,
            QH03L15,
            QH03L25,
            QH03L35))

alcohol_amount_3<-lapply(alcohol_amount_3,NaNtoNA)
alcohol_amount_3<-rowSums(as.data.frame(alcohol_amount_3))

bottles_drank_1<-alcohol_amount_1/16.5
bottles_drank_2<-alcohol_amount_2/16.5
bottles_drank_3<-alcohol_amount_3/16.5

#### CREATING FINAL DATASET
data_moshi<-NULL
data_moshi<-with(data,data.frame(breath_level=QD04,
                                 age=AGE,
                                 gender=SEX,
                                 years_education=QI01,
                                 work=QI02,
                                 # sys_bp=QL01,
                                 gcs=QL02C,
                                 # resp_rate=QL03,
                                 # pulse=QL04,
                                 # avpu=QL05,
                                 freq_injuries=QL06,
                                 time_to_injury=QB01,
                                 fracture=QE011,
                                 dislocation=QE012,
                                 open_wound=QE013,
                                 bruise=QE014,
                                 burn=QE015,
                                 concussion=QE016,
                                 organ_injury=QE017,
                                 method_injury=QE02,
                                 type_vehicle=QE02B,
                                 motive_injury=QE03,
                                 location=QE06,
                                 activity_prior=QE071,
                                 past_alcohol_use=QG01,
                                 # high_alcohol_use=QG03,
                                 # past_alchol_use=QG04,
                                 # low_alcohol_use=QG04,
                                 # emergency_need=QG07,
                                 predictor_FUP1=QNH04,
                                 predictor_FUP2=QH02,
                                 alcohol_amount_1,
                                 alcohol_amount_2,
                                 alcohol_amount_3,
                                 bottles_drank_1,
                                 bottles_drank_2,
                                 bottles_drank_3))

#Creating KTS variable
kts<-NULL
kts$age<-car::recode(data$AGE,"6:55=2;else=1")
kts$number_injuries<-car::recode(data$QL06,"2=3;3=2;4=1;else=NA")
kts$sbp<-car::recode(data$QL01,"89.1:221=4;50:89=3;1:49=2;else=NA")
kts$resp_rate<-car::recode(data$QL03,"10:29=3;30:200=2;0:9=1;else=NA")
kts$avpu<-car::recode(data$QL05,"2=4;3=3;4=2;5=1;else=NA")
kts$total<-with(kts,rowSums(data.frame(age,number_injuries,sbp,resp_rate,avpu)))
data_moshi$kts<-NULL
data_moshi$kts<-kts$total

#Creating RTS variable
rts<-NULL
rts$sbp<-car::recode(data$QL01,"89.1:221=4;76:89=3;50:75=2;1:49=1;0=0;else=NA")
rts$sbp<-0.7326*rts$sbp
rts$resp_rate<-car::recode(data$QL03,"10:29=4;30:200=3;6:9=2;1:5=1;0=0;else=NA")
rts$resp_rate<-0.2908*rts$resp_rate
rts$gcs<-car::recode(data$QL02C,"13:15=4;9:12=3;6:8=2;5:4=1;3=0;else=NA")
rts$gcs<-0.9368*rts$gcs
rts$total<-with(rts,rowSums(data.frame(gcs,sbp,resp_rate)))
data_moshi$rts<-NULL
data_moshi$rts<-rts$total
#recode Missing from 99 to NA

#recode_moshi<-function(x){ 
#car::recode(x,"99=NA")
#}

## RECODING MISSING AND UNKNOWN DATA

data_moshi$method_injury<-car::recode(data_moshi$method_injury,"89=NA;99=NA")
data_moshi$type_vehicle<-car::recode(data_moshi$type_vehicle,"
               1='car';
               2='avru';
               3='avru';
               4='truck';
               9=NA;
               else='non-RTI'")
data_moshi$type_vehicle<-as.factor(data_moshi$type_vehicle)
data_moshi$work<-car::recode(data_moshi$work,"9=NA")
data_moshi$activity_prior<-car::recode(data_moshi$activity_prior,"99=NA")
# data_moshi$high_alcohol_use<-car::recode(data_moshi$high_alcohol_use,"99=NA")
data_moshi$past_alcohol_use<-car::recode(data_moshi$past_alcohol_use,"1:8='yes';9='no';99=NA")
data_moshi$past_alcohol_use<-as.factor(data_moshi$past_alcohol_use)
# data_moshi$low_alcohol_use<-car::recode(data_moshi$low_alcohol_use,"99=NA")
data_moshi$predictor_FUP1<-car::recode(data_moshi$predictor_FUP1,"
            1='yes';2='no';8=NA;9=NA;99=NA;NaN='no'")
data_moshi$predictor_FUP1<-as.factor(data_moshi$predictor_FUP1)
data_moshi$predictor_FUP2<-car::recode(data_moshi$predictor_FUP2,"
            1='yes';2='no';8=NA;9=NA;99=NA;90=NA;NaN='no'")
data_moshi$predictor_FUP2<-as.factor(data_moshi$predictor_FUP2)
data_moshi$breath_level<-car::recode(data_moshi$breath_level,"0='no';9=NA;else='yes'")
data_moshi$breath_level<-as.factor(data_moshi$breath_level)
data_moshi$id<-c(1:516)
data_moshi$breath_level_limit<-car::recode(data$QD04,"0:0.08='no';NA=NA;else='yes'")
data_moshi$gcs<-car::recode(data_moshi$gcs,"99=NA")
data_moshi$method_injury<-car::recode(data_moshi$method_injury,"1='RTI';2='RTI';3='RTI';5='Violence';6='Violence';7='Violence';9='Outros';10='Violence';13='Outros';14='Outros'")
data_moshi$method_injury<-as.factor(data_moshi$method_injury)
# data_moshi$sys_bp<-car::recode(data_moshi$sys_bp,"999=NA")
# data_moshi$pulse<-car::recode(data_moshi$pulse,"999=NA")
# data_moshi$avpu<-car::recode(data_moshi$avpu,"9=NA")
data_moshi$fracture<-car::recode(data_moshi$fracture,"9=NA")
data_moshi$dislocation<-car::recode(data_moshi$dislocation,"9=NA")
data_moshi$open_wound<-car::recode(data_moshi$open_wound,"9=NA")
data_moshi$bruise<-car::recode(data_moshi$bruise,"9=NA")
data_moshi$concussion<-car::recode(data_moshi$concussion,"9=NA")
data_moshi$organ_injury<-car::recode(data_moshi$organ_injury,"9=NA")
data_moshi$type_vehicle<-car::recode(data_moshi$type_vehicle,"9=NA")
data_moshi$motive_injury<-car::recode(data_moshi$motive_injury,"9=NA")
data_moshi$location<-car::recode(data_moshi$location,"
               1='home';
               2='home';
               3='drinking place';
               4='drinking place';
               5='drinking place';
               6='drinking place';
               8='work place';
               9='Other';
               10='outdoor public place';
               11='outdoor public place';
               12='Other';
               99=NA")
data_moshi$location<-as.factor(data_moshi$location)
# data_moshi$emergency_need<-car::recode(data_moshi$emergency_need,"9=NA")
data_moshi$bottle<-car::recode(data_moshi$bottles_drank_1,"0=0;0.001:2.5='1 to 2';2.5001:4.5='3 to 4';4.50001:72.73='5 or more'")
data_moshi$bottle24<-car::recode(data_moshi$bottles_drank_2,"0=0;0.001:2.5='1 to 2';2.5001:4.5='3 to 4';4.50001:63.64='5 or more'")
data_moshi$bottle1week<-car::recode(data_moshi$bottles_drank_3,"0=0;0.001:2.5='1 to 2';2.5001:4.5='3 to 4';4.50001:79.55='5 or more'")
# data_moshi$bottle_alcohol_positive<-car::recode(data_moshi$bottles_drank_1,"0=NA;0.001:2.5='1 to 2';2.5001:4.5='3 to 4';4.50001:63.64='5 or more'")
# data_moshi$bottle24_alcohol_positive<-car::recode(data_moshi$bottles_drank_2,"0=NA;0.001:2.5='1 to 2';2.5001:4.5='3 to 4';4.50001:63.64='5 or more'")
# data_moshi$bottle1week_alcohol_positive<-car::recode(data_moshi$bottles_drank_3,"0=NA;0.001:2.5='1 to 2';2.5001:4.5='3 to 4';4.50001:159.1000='5 or more'")

# data_moshi <- within(data_moshi, predictor_FUP1[QG01==9] <- 2)
# data_moshi <- within(data_moshi, predictor_FUP2[QG01==9] <- 2)

###################################################
#IMPUTING MISSING DATA
###################################################
#Studying missing data
#Calculating frequency of missing data per variable
propmiss <- function(dataframe) lapply(dataframe,function(x) data.frame(nmiss=sum(is.na(x)), n=length(x), propmiss=sum(is.na(x))/length(x)))

propmiss(data_moshi)

#inspecting measure random of missing data
#Inspectif Weather Conditions
#weather_missing<-car::recode(data_epi$weather_condition,"NA=0;else=1")
#logmodel<-glm(weather_missing ~  data_epi$day_crash,family=binomial)
#summary(logmodel)
#logmodel<-glm(weather_missing ~  data_epi$hour_crash,family=binomial)
#summary(logmodel)
#logmodel<-glm(weather_missing ~  data_epi$road_type,family=binomial)
#summary(logmodel)
#logmodel<-glm(weather_missing ~  data_epi$road_condition,family=binomial)
#summary(logmodel)
#logmodel<-glm(weather_missing ~  data_epi$visibility,family=binomial)
#summary(logmodel)
#logmodel<-glm(weather_missing ~  data_epi$type_vehicle,family=binomial)
#summary(logmodel)
#logmodel<-glm(weather_missing ~  data_epi$type_vehicle2,family=binomial)
#summary(logmodel)
#logmodel<-glm(weather_missing ~  data_epi$gender,family=binomial)
#summary(logmodel)
#logmodel<-glm(weather_missing ~  data_epi$crash_type,family=binomial)
#summary(logmodel)

#out <- TestMCARNormality(data_epi)
#missing.pattern.plot(data_epi)
# #MICE framework for imputation
# # describing the pattern of missingnesss
# md.pattern(data_epi)

# # showing pairs of missingines
# md.pairs(data_epi)

# # plots impact of missing data for a set of pairs - works better for numerical data
# marginplot(data.frame(data_epi$outcome,data_epi$visibility), col = mdc(1:2), cex = 1.2, cex.lab = 1.2, cex.numbers = 1.3, pch = 19)

# generate imputations
# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
imp <- mice(data_moshi, seed = 2222, m=10)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
data_moshi_imp<-complete(imp,1)

#Plost the distrbution of each of the 5 possibilities of imputations
# stripplot(imp,pch=20,cex=1.2)

# #plots a scatter plot of pairs of variables
# xyplot(imp, outcome ~ visibility | .imp, pch = 20, cex = 1.4)

# #returns the matrix specifying each variable used to -predict imputation - columns 1=predictor 0=not predictor. rows are the variables of interest
# imp$predictorMatrix
# pred <- imp$predictorMatrix #if you want to exclude  variable from the prediction model for imputation then assign an obect to pred
# pred[, "bmi"] <- 0 #transform the column values into 0's for not predictiong
# imp <- mice(nhanes, pred = pred, pri = FALSE) # rerun the model specifying pred argumento witht eh matriz recoded.

###################################################
#SAMPLE SIZE CALCULATION
###################################################
# Sample size for SCCS
#extracted from -  http://statistics.open.ac.uk/sccs/sccs_ssize.pdf
ssize<-function(za,zb,r,rho,p){
za<-1.96
zb<-0.8416
r<-2.0
rho<-0.15
p<-0.30
beta<-log(rho)
factor<-(1+p*r*(rho-1))/(p*(rho*r+1-r))
form6num<-(za+zb)^2
form6den<-4*(asin(sqrt(rho*r/(rho*r+1-r)))-asin(sqrt(r)))^2
form6<-factor*form6num/form6den
A1<-2*((rho*r/(rho*r+1-r))*beta-log(rho*r+1-r))
B1<-(beta^2)*rho*r*(1-r)/(A1*(rho*r+1-r)^2)
form7<-factor*((za+zb*sqrt(B1))^2)/A1
c(form6,form7)
}

ssize(1.96,0.8416,2.0,15,30)

###################################################
#TABLE 1. DESCRIPTIVES
###################################################
summary(data_moshi_imp)

#alcohol usage
mytable <- with(data_moshi_imp,table(breath_level))
mytable
prop.table(mytable)
prop.test(156, 516)

#alcohol legal limit
mytable <- with(data_moshi_imp,table(breath_level_limit))
mytable
prop.table(mytable)
mytable <- with(data_moshi_imp,table(breath_level_limit,breath_level))
mytable
prop.table(mytable,2)
assocstats(mytable)

#TABLE 1
#Age
with(data_moshi_imp,describe(age))
with(data_moshi_imp,by(age,breath_level,ad.test))
with(data_moshi_imp,describeBy(age,breath_level))
with(data_moshi_imp,t.test(age~breath_level))

#Gender
mytable <- with(data_moshi_imp,table(gender))
mytable
prop.table(mytable)
mytable <- with(data_moshi_imp,table(gender,breath_level))
mytable
prop.table(mytable,2)
assocstats(mytable)

#Education
with(data_moshi_imp,describe(years_education))
with(data_moshi_imp,by(years_education,breath_level,ad.test))
with(data_moshi_imp,describeBy(years_education,breath_level))
with(data_moshi_imp,t.test(years_education~breath_level))

#Work
mytable <- with(data_moshi_imp,table(work))
mytable
prop.table(mytable)
mytable <- with(data_moshi_imp,table(work,breath_level))
mytable
prop.table(mytable,2)
assocstats(mytable)

#Income
# quantile(data_moshi_imp$income, probs = seq(0, 1, 0.20))
# data_moshi_imp$income_cat<-car::recode(data_moshi_imp$income,"12000:60000='low';60001:108000='mediumlow';108001:200000='medium';200001:376000='mediumhigh';376001:80000000='high'")
# with(data_moshi_imp,describe(income))
# with(data_moshi_imp,by(income,breath_level,ad.test))
# with(data_moshi_imp,by(income,breath_level,summary))
# with(data_moshi_imp,t.test(income~breath_level))

#past alcohol usage
mytable <- with(data_moshi_imp,table(past_alcohol_use))
mytable
prop.table(mytable)
mytable <- with(data_moshi_imp,table(past_alcohol_use,breath_level))
mytable
prop.table(mytable,2)
assocstats(mytable)
fisher.test(mytable)

#predictor_FUP1=QNH04 - 24 hrs to injury
mytable <- with(data_moshi_imp,table(predictor_FUP1))
mytable
prop.table(mytable)
mytable <- with(data_moshi_imp,table(predictor_FUP1,breath_level))
mytable
prop.table(mytable,2)
assocstats(mytable)

#predictor_FUP2=QH02 - 1 week prior to injury
mytable <- with(data_moshi_imp,table(predictor_FUP2))
mytable
prop.table(mytable)
mytable <- with(data_moshi_imp,table(predictor_FUP2,breath_level))
mytable
prop.table(mytable,2)
assocstats(mytable)

# #alcohol usage
# mytable <- with(data_moshi_imp,table(breath_level_limit))
# mytable
# prop.table(mytable)
# mytable <- with(data_moshi_imp,table(breath_level,breath_level_limit))
# mytable
# prop.table(mytable,1)
# #assocstats(mytable)

# #sys_bp
# with(data_moshi_imp,describe(sys_bp))
# with(data_moshi_imp,by(sys_bp,breath_level,ad.test))
# with(data_moshi_imp,by(sys_bp,breath_level,describe))
# with(data_moshi_imp,t.test(sys_bp~breath_level))

# #gcs
# with(data_moshi_imp,describe(gcs))
# with(data_moshi_imp,by(gcs,breath_level,ad.test))
# with(data_moshi_imp,by(gcs,breath_level,describe))
# with(data_moshi_imp,t.test(gcs~breath_level))

# #resp_rate
# with(data_moshi_imp,describe(resp_rate))
# with(data_moshi_imp,by(resp_rate,breath_level,ad.test))
# with(data_moshi_imp,by(resp_rate,breath_level,describe))
# with(data_moshi_imp,t.test(resp_rate~breath_level))

# #avpu
# with(data_moshi_imp,describe(avpu))
# with(data_moshi_imp,by(avpu,breath_level,ad.test))
# with(data_moshi_imp,by(avpu,breath_level,describe))
# with(data_moshi_imp,t.test(avpu~breath_level))

#time_to_injury
with(data_moshi_imp,describe(time_to_injury))
with(data_moshi_imp,by(time_to_injury,breath_level,ad.test))
with(data_moshi_imp,by(time_to_injury,breath_level,describe))
with(data_moshi_imp,t.test (time_to_injury~breath_level))

#kts
with(data_moshi,describe(kts))
with(data_moshi_imp,by(kts,breath_level,ad.test))
with(data_moshi_imp,by(kts,breath_level,describe))
with(data_moshi_imp,t.test(kts~breath_level))

# #pulse
# with(data_moshi_imp,describe(pulse))
# with(data_moshi_imp,by(pulse,breath_level,ad.test))
# with(data_moshi_imp,by(pulse,breath_level,describe))
# with(data_moshi_imp,t.test(pulse~breath_level))

#rts
with(data_moshi_imp,describe(rts))
with(data_moshi_imp,by(rts,breath_level,ad.test))
with(data_moshi_imp,by(rts,breath_level,describe))
with(data_moshi_imp,t.test (rts~breath_level))

#fracture
mytable <- with(data_moshi_imp,table(fracture))
mytable
prop.table(mytable)
mytable <- with(data_moshi_imp,table(fracture,breath_level))
mytable
prop.table(mytable,2)
assocstats(mytable)

#dislocation
mytable <- with(data_moshi_imp,table(dislocation))
mytable
prop.table(mytable)
mytable <- with(data_moshi_imp,table(dislocation,breath_level))
mytable
prop.table(mytable,2)
assocstats(mytable)

#open_wound
mytable <- with(data_moshi_imp,table(open_wound))
mytable
prop.table(mytable)
mytable <- with(data_moshi_imp,table(open_wound,breath_level))
mytable
prop.table(mytable,2)
assocstats(mytable)

#bruise
mytable <- with(data_moshi_imp,table(bruise))
mytable
prop.table(mytable)
mytable <- with(data_moshi_imp,table(bruise,breath_level))
mytable
prop.table(mytable,2)
assocstats(mytable)

#burn
mytable <- with(data_moshi_imp,table(burn))
mytable
prop.table(mytable)
mytable <- with(data_moshi_imp,table(burn,breath_level))
mytable
prop.table(mytable,2)
assocstats(mytable)

#concussion
mytable <- with(data_moshi_imp,table(concussion))
mytable
prop.table(mytable)
mytable <- with(data_moshi_imp,table(concussion,breath_level))
mytable
prop.table(mytable,2)
assocstats(mytable)

#organ_injury
mytable <- with(data_moshi_imp,table(organ_injury))
mytable
prop.table(mytable)
mytable <- with(data_moshi_imp,table(organ_injury,breath_level))
mytable
prop.table(mytable,2)
assocstats(mytable)

#method_injury
mytable <- with(data_moshi_imp,table(method_injury))
mytable
prop.table(mytable)
mytable <- with(data_moshi_imp,table(method_injury,breath_level))
mytable
prop.table(mytable,2)
assocstats(mytable)

#type_vehicle
data_moshi_imp$type_vehicle<-as.factor(data_moshi_imp$type_vehicle)
mytable <- with(data_moshi_imp,table(type_vehicle))
mytable
prop.table(mytable)
mytable <- with(data_moshi_imp,table(type_vehicle,breath_level))
mytable
prop.table(mytable,2)
assocstats(mytable)

#motive_injury
data_moshi_imp$motive_injury<-as.factor(data_moshi_imp$motive_injury)
mytable <- with(data_moshi_imp,table(motive_injury))
mytable
prop.table(mytable)
mytable <- with(data_moshi_imp,table(motive_injury,breath_level))
mytable
prop.table(mytable,2)
assocstats(mytable)

#location
data_moshi_imp$location<-as.factor(data_moshi_imp$location)
mytable <- with(data_moshi_imp,table(location))
mytable
prop.table(mytable)
mytable <- with(data_moshi_imp,table(location,breath_level))
mytable
prop.table(mytable,2)
assocstats(mytable)
fisher.test(mytable)

# #emergency_need
# data_moshi_imp$emergency_need<-as.factor(data_moshi_imp$emergency_need)
# mytable <- with(data_moshi_imp,table(emergency_need))
# mytable
# prop.table(mytable)
# mytable <- with(data_moshi_imp,table(emergency_need,breath_level))
# mytable
# prop.table(mytable,2)
# assocstats(mytable)

# #time_to_injury
# with(data_moshi_imp,describe(alcohol_amount_1))
# with(data_moshi_imp,by(alcohol_amount_1,breath_level,ad.test))
# with(data_moshi_imp,by(alcohol_amount_1,breath_level,describe))
# with(data_moshi_imp,t.test (alcohol_amount_1~breath_level))

# ### DUMMY Variables
# dummy_data<-with(data_moshi_imp,data.frame(breath_level,
#  method_injury,as.factor(type_vehicle),location))
# dummy<-as.data.frame(model.matrix(breath_level~ ., data = dummy_data))
# names(dummy)<-c("Intercept",
#           "injury_fall_trip",
#           "injury_other",
#           "injury_RIT",
#           "injury_violence",
#           "vehicle2",
#           "vehicle3",
#           "vehicle4",
#           "location_drinking",
#           "home",
#           "location_outdoor",
#           "location_vehicle",
#           "location_workplace")

# #Fall/TRIP
# mytable <- table(dummy$injury_fall_trip,data_moshi_imp$breath_level)
# mytable
# prop.table(mytable,2)
# assocstats(mytable)

# #RTI
# mytable <- table(dummy$injury_RIT,data_moshi_imp$breath_level)
# mytable
# prop.table(mytable,2)
# assocstats(mytable)

# #violence
# mytable <- table(dummy$injury_violence,data_moshi_imp$breath_level)
# mytable
# prop.table(mytable,2)
# assocstats(mytable)

# #vehicle1
# mytable <- table(dummy$vehicle1,data_moshi_imp$breath_level)
# mytable
# prop.table(mytable,2)
# assocstats(mytable)

# #vehicle2
# mytable <- table(dummy$vehicle2,data_moshi_imp$breath_level)
# mytable
# prop.table(mytable,2)
# assocstats(mytable)

# #vehicle3
# mytable <- table(dummy$vehicle3,data_moshi_imp$breath_level)
# mytable
# prop.table(mytable,2)
# assocstats(mytable)

# #vehicle4
# mytable <- table(dummy$Intercept,data_moshi_imp$breath_level)
# mytable
# prop.table(mytable,2)
# assocstats(mytable)

#REGRESSION MODEL
###################################################

regression<-with(data_moshi_imp,data.frame(
                  age,
                  gender=as.factor(gender),
                  years_education,
                  work=as.factor(work),
                  # gcs,
                  time_to_injury,
                  rts,
                  kts,
                  fracture=as.factor(fracture),
                  dislocation=as.factor(dislocation),
                  open_wound=as.factor(open_wound),
                  bruise=as.factor(bruise),
                  concussion=as.factor(concussion),
                  organ_injury=as.factor(organ_injury),
                  method_injury,
                  type_vehicle=type_vehicle,
                  motive_injury=as.factor(motive_injury),
                  location,
                  outcome=as.factor(breath_level),
                  past_alcohol_use,
                  predictor_FUP1,
                  predictor_FUP2))

regression$method_injury<-car::recode(regression$method_injury,
   "'RTI'='ARTI'")
# regression$type_vehicle<-car::recode(regression$type_vehicle,
#  "'3'='2'")
regression$location<-car::recode(regression$location,
   "'outdoor public place'='Aoutdoor public place'")
regression$gender<-car::recode(regression$gender,
   "1='male';
    2='female'")
# regression$outcome<-car::recode(regression$outcome,
#  "'2'='0';'1'='1'")
regression$fracture<-car::recode(regression$fracture,
   "'2'='0';'1'='1'")
regression$dislocation<-car::recode(regression$dislocation,
   "'2'='0';'1'='1'")
regression$open_wound<-car::recode(regression$open_wound,
   "'2'='0';'1'='1'")
regression$bruise<-car::recode(regression$bruise,
   "'2'='0';'1'='1'")
regression$concussion<-car::recode(regression$concussion,
   "'2'='0';'1'='1'")
regression$organ_injury<-car::recode(regression$organ_injury,
   "'2'='0';'1'='1'")

#Method of injury
# mytable <- with(regression,table(method_injury))
# mytable
# prop.table(mytable)
# mytable <- with(regression,table(method_injury,outcome))
# mytable
# prop.table(mytable,2)
# assocstats(mytable)

# #Type of RTI
# mytable <- with(regression,table(type_vehicle))
# mytable
# prop.table(mytable)
# mytable <- with(regression,table(type_vehicle,outcome))
# mytable
# prop.table(mytable,2)
# assocstats(mytable)

# #Location
# mytable <- with(regression,table(location))
# mytable
# prop.table(mytable)
# mytable <- with(regression,table(location,outcome))
# mytable
# prop.table(mytable,2)
# assocstats(mytable)

fit <- glm(outcome~age+
                   gender+
                   years_education+
                   work+
                   # gcs+
                   time_to_injury+
                   rts+
                   kts+
                   fracture+
                   dislocation+
                   open_wound+
                   bruise+
                   concussion+
                   organ_injury+
                   method_injury+
                   # type_vehicle+
                   motive_injury+
                   location,
                   # past_alcohol_use+
                   # predictor_FUP1+
                   # predictor_FUP2,
                  data=regression,family=binomial())
summary(fit) # display results
cbind(exp(cbind(Odds=coef(fit),confint(fit,level=0.95))))
# Pvalue=x$coefficients[,4])
#predict(fit, type="response") # predicted values
#residuals(fit, type="deviance") # residuals
logistic.display(fit)

###########################################################
#Table 2. Alchohl dosage usage
###########################################################
#ALCOHOL POSITIVE
# cleaned_data$bottle_alcohol_positive<-car::recode(cleaned_data$bottle,"0=NA")
# cleaned_data$bottle24_alcohol_positive<-car::recode(cleaned_data$bottle24,"0=NA")
# cleaned_data$bottle1week_alcohol_positive<-car::recode(cleaned_data$bottle1week,"0=NA")

#alcohol usage - time of injury
mytable <- with(data_moshi_imp,table(breath_level))
mytable
prop.table(mytable)
mytable <- with(data_moshi_imp,table(breath_level,bottle))
mytable
prop.table(mytable,1)
assocstats(mytable)

#alcohol usage - 24 hours
mytable <- with(data_moshi_imp,table(bottle24))
mytable
prop.table(mytable)
mytable <- with(data_moshi_imp,table(breath_level,bottle24))
mytable
prop.table(mytable,1)
assocstats(mytable)

#alcohol usage - 1 week
mytable <- with(data_moshi_imp,table(bottle1week))
mytable
prop.table(mytable)
mytable <- with(data_moshi_imp,table(breath_level,bottle1week))
mytable
prop.table(mytable,1)
assocstats(mytable)

#CREATING ABSTAINERS COLUMN
# cleaned_data$abstainers<-car::recode(cleaned_data$QG01,"99=NA;9='yes';else='no'")
#alcohol usage - time of injury
mytable <- with(data_moshi_imp,table(past_alcohol_use))
mytable
prop.table(mytable)

#Abstrainers Vs. Alchool Use
mytable <- with(data_moshi_imp,table(past_alcohol_use,breath_level))
mytable
prop.table(mytable,2)
fisher.test(mytable)

mytable <- with(data_moshi_imp,table(past_alcohol_use,bottle))
mytable
prop.table(mytable,1)
59assocstats(mytable)

#alcohol usage - 24 hours
mytable <- with(data_moshi_imp,table(bottle24))
mytable
prop.table(mytable)
mytable <- with(data_moshi_imp,table(past_alcohol_use,bottle24))
mytable
prop.table(mytable,1)
assocstats(mytable)

#alcohol usage - 1 week
mytable <- with(data_moshi_imp,table(bottle1week))
mytable
prop.table(mytable)
mytable <- with(data_moshi_imp,table(past_alcohol_use,bottle1week))
mytable
prop.table(mytable,1)
assocstats(mytable)

###########################################################
##TABLE 4
###########################################################

#ALL INJURIES
#CLOGIT MODEL 1:1 -Control 24 hours
id_1<-c(1)
id_2<-c(2)
#id_3<-c(3)
strata<-c(1:516)
outcome1<-c(1)
outcome2<-c(0)
#outcome3<-c(0)
fup1<-with(data_moshi_imp,data.frame(
         predictor=breath_level,
         # predictor2=bottle,
         strata,
         id=id_1,
         outcome=outcome1,
         age,
         years_education,
         gcs,
         gender,
         bottle=bottle))
fup2<-with(data_moshi_imp,data.frame(
         predictor=predictor_FUP1,
         # predictor2=bottle24,
         strata,
         id=id_2,
         outcome=outcome2,
         age,
         years_education,
         gcs,
         gender,
         bottle=bottle24))
#fup3<-with(cleaned_data,data.frame(predictor=predictor_FUP2,strata,id=id_3,outcome=outcome3))
#matched_data<-with(cleaned_data,data.frame(breath_level,id))
#fup1$predictor<-car::recode(fup1$predictor,"'yes'=1;'no'=2")
#fup1$predictor<-as.numeric(as.character(fup1$predictor))
clogit_data<-rbind(fup1,fup2)#,fup3)
#clogit_data<-with(cleaned_data,data.frame(predictor1=predictor_FUP1,predictor2=predictor_FUP2,strata,outcome=breath_level))
#matched_data<-with(cleaned_data,data.frame(breath_level,id))
# clogit_data$predictor<-car::recode(clogit_data$predictor,"2=1;1=2")
# clogit_data$predictor<-as.factor(clogit_data$predictor)
x_1<-clogit(outcome ~ predictor + strata(strata),clogit_data, 
   method="exact")
summary(x_1) 
clogistic.display(x_1)

x_1<-clogit(outcome ~ bottle + strata(strata),clogit_data, 
   method="exact")
summary(x_1) 
clogistic.display(x_1)

#CLOGIT MODEL 1:1 - 1 week control
id_1<-c(1)
#id_2<-c(2)
id_3<-c(3)
strata<-c(1:516)
outcome1<-c(1)
#outcome2<-c(0)
outcome3<-c(0)
fup1<-with(data_moshi_imp,data.frame(predictor=breath_level,strata,id=id_1,outcome=outcome1,bottle=bottle))
#fup2<-with(cleaned_data,data.frame(predictor=predictor_FUP1,strata,id=id_2,outcome=outcome2))
fup3<-with(data_moshi_imp,data.frame(predictor=predictor_FUP2,strata,id=id_3,outcome=outcome3,bottle=bottle1week))
#matched_data<-with(cleaned_data,data.frame(breath_level,id))
#fup1$predictor<-car::recode(fup1$predictor,"'yes'=1;'no'=2")
#fup1$predictor<-as.numeric(as.character(fup1$predictor))
clogit_data<-rbind(fup1,fup3)#,fup3)
#clogit_data<-with(cleaned_data,data.frame(predictor1=predictor_FUP1,predictor2=predictor_FUP2,strata,outcome=breath_level))
#matched_data<-with(cleaned_data,data.frame(breath_level,id))
# clogit_data$predictor<-car::recode(clogit_data$predictor,"2=1;1=2")
# clogit_data$predictor<-as.factor(clogit_data$predictor)
x_2<-clogit(outcome ~ predictor + strata(strata),clogit_data, method="exact")
summary(x_2) 
clogistic.display(x_2)

x_22<-clogit(outcome ~ bottle + strata(strata),clogit_data, 
   method="exact")
summary(x_22) 
clogistic.display(x_1)

#CLOGIT MODEL 2:1
id_1<-c(1)
id_2<-c(2)
id_3<-c(3)
strata<-c(1:516)
outcome1<-c(1)
outcome2<-c(0)
outcome3<-c(0)
fup1<-with(data_moshi_imp,data.frame(predictor=breath_level,
   strata,id=id_1,outcome=outcome1,confouder=bottle))
fup2<-with(data_moshi_imp,data.frame(predictor=predictor_FUP1,
   strata,id=id_2,outcome=outcome2,confouder=bottle24))
fup3<-with(data_moshi_imp,data.frame(predictor=predictor_FUP2,
   strata,id=id_3,outcome=outcome3,confouder=bottle1week))
#matched_data<-with(cleaned_data,data.frame(breath_level,id))
#fup1$predictor<-car::recode(fup1$predictor,"'yes'=1;'no'=2")
#fup1$predictor<-as.numeric(as.character(fup1$predictor))
clogit_data<-rbind(fup1,fup2,fup3)
#clogit_data<-with(cleaned_data,data.frame(predictor1=predictor_FUP1,predictor2=predictor_FUP2,strata,outcome=breath_level))
#matched_data<-with(cleaned_data,data.frame(breath_level,id))
# clogit_data$predictor<-car::recode(clogit_data$predictor,"2=1;1=2")
# clogit_data$predictor<-as.factor(clogit_data$predictor)
x_3<-clogit(outcome ~ predictor + strata(strata),clogit_data, method="exact")
summary(x_3) 
clogistic.display(x_3)

x_3<-clogit(outcome ~ confouder + strata(strata),clogit_data, method="exact")
summary(x_3) 
clogistic.display(x_3)

# #DOSAGE LEVEL
# mytable<-with(clogit_data,table(confouder,outcome))
# dose_matrix<-as.matrix(mytable)
# doseSpecificOddsRatios(dose_matrix)

#############################################################
#CLOGIT MODEL - RTI outcome
#############################################################
### MODEL 1:1 - 24 hours
rti_model<-subset(cleaned_data,cleaned_data$method_injury=='RTI')
id_1<-c(1)
id_2<-c(2)
#id_3<-c(3)
strata<-c(1:length(rti_model$breath_level))
outcome1<-c(1)
outcome2<-c(0)
#outcome3<-c(0)
fup1<-with(rti_model,data.frame(predictor=breath_level,strata,id=id_1,outcome=outcome1,confouder=alcohol_amount_1))
fup2<-with(rti_model,data.frame(predictor=predictor_FUP1,strata,id=id_2,outcome=outcome2,confouder=alcohol_amount_2))
#fup3<-with(rti_model,data.frame(predictor=predictor_FUP2,strata,id=id_3,outcome=outcome3,confouder=alcohol_amount_3))
#matched_data<-with(rti_model,data.frame(breath_level,id))
#fup1$predictor<-car::recode(fup1$predictor,"'yes'=2;'no'=1")
#fup1$predictor<-as.numeric(as.character(fup1$predictor))
clogit_data<-rbind(fup1,fup2)#,fup3)
#clogit_data<-with(rti_model,data.frame(predictor1=predictor_FUP1,predictor2=predictor_FUP2,strata,outcome=breath_level))
#matched_data<-with(rti_model,data.frame(breath_level,id))
clogit_data$predictor<-car::recode(clogit_data$predictor,"2=1;1=2")
clogit_data$predictor<-as.factor(clogit_data$predictor)
x_1B<-clogit(outcome ~ predictor +strata(strata),clogit_data, method="exact")
summary(x_1B) 
clogistic.display(x_1B)

#create dummy variables to exposure within the clogit stuff

### MODEL 1:1 - 1 week
rti_model<-subset(cleaned_data,cleaned_data$method_injury=='RTI')
id_1<-c(1)
#id_2<-c(2)
id_3<-c(3)
strata<-c(1:length(rti_model$breath_level))
outcome1<-c(1)
#outcome2<-c(0)
outcome3<-c(0)
fup1<-with(rti_model,data.frame(predictor=breath_level,strata,id=id_1,outcome=outcome1,confouder=alcohol_amount_1))
#fup2<-with(rti_model,data.frame(predictor=predictor_FUP1,strata,id=id_2,outcome=outcome2,confouder=alcohol_amount_2))
fup3<-with(rti_model,data.frame(predictor=predictor_FUP2,strata,id=id_3,outcome=outcome3,confouder=alcohol_amount_3))
#matched_data<-with(rti_model,data.frame(breath_level,id))
#fup1$predictor<-car::recode(fup1$predictor,"'yes'=2;'no'=1")
#fup1$predictor<-as.numeric(as.character(fup1$predictor))
clogit_data<-rbind(fup1,fup3)#,fup2)
#clogit_data<-with(rti_model,data.frame(predictor1=predictor_FUP1,predictor2=predictor_FUP2,strata,outcome=breath_level))
#matched_data<-with(rti_model,data.frame(breath_level,id))
clogit_data$predictor<-car::recode(clogit_data$predictor,"2=1;1=2")
clogit_data$predictor<-as.factor(clogit_data$predictor)
x_2B<-clogit(outcome ~ predictor +strata(strata),clogit_data, method="exact")
summary(x_2B) 
clogistic.display(x_2B)

#create dummy variables to exposure within the clogit stuff

### MODEL 1:2
rti_model<-subset(data_moshi_imp,data_moshi_imp$method_injury=='RTI')
id_1<-c(1)
id_2<-c(2)
id_3<-c(3)
strata<-c(1:length(rti_model$breath_level))
outcome1<-c(1)
outcome2<-c(0)
outcome3<-c(0)
fup1<-with(rti_model,data.frame(predictor=breath_level,strata,id=id_1,outcome=outcome1,confouder=bottle))
fup2<-with(rti_model,data.frame(predictor=predictor_FUP1,strata,id=id_2,outcome=outcome2,confouder=bottle24))
fup3<-with(rti_model,data.frame(predictor=predictor_FUP2,strata,id=id_3,outcome=outcome3,confouder=bottle1week))
#matched_data<-with(rti_model,data.frame(breath_level,id))
#fup1$predictor<-car::recode(fup1$predictor,"'yes'=2;'no'=1")
#fup1$predictor<-as.numeric(as.character(fup1$predictor))
clogit_data<-rbind(fup1,fup2,fup3)
#clogit_data<-with(rti_model,data.frame(predictor1=predictor_FUP1,predictor2=predictor_FUP2,strata,outcome=breath_level))
#matched_data<-with(rti_model,data.frame(breath_level,id))
# clogit_data$predictor<-car::recode(clogit_data$predictor,"2=1;1=2")
# clogit_data$predictor<-as.factor(clogit_data$predictor)
x_3B<-clogit(outcome ~ predictor +strata(strata),clogit_data, method="exact")
summary(x_3B) 
clogistic.display(x_3B)

x_3B<-clogit(outcome ~ confouder + strata(strata),clogit_data, method="exact")
summary(x_3B) 
clogistic.display(x_3B)

#DOSAGE LEVEL
# mytable<-with(clogit_data,table(confouder,outcome))
# dose_matrix<-as.matrix(mytable)
# doseSpecificOddsRatios(dose_matrix)
#############################################################
#CLOGIT MODEL - Violence outcome
#############################################################
### MODEL 1:1 - 24 hours
violence_model<-subset(cleaned_data,cleaned_data$method_injury=='Violence')
id_1<-c(1)
id_2<-c(2)
#id_3<-c(3)
strata<-c(1:length(violence_model$breath_level))
outcome1<-c(1)
outcome2<-c(0)
#outcome3<-c(0)
fup1<-with(violence_model,data.frame(predictor=breath_level,strata,id=id_1,outcome=outcome1,confouder=alcohol_amount_1))
fup2<-with(violence_model,data.frame(predictor=predictor_FUP1,strata,id=id_2,outcome=outcome2,confouder=alcohol_amount_2))
#fup3<-with(violence_model,data.frame(predictor=predictor_FUP2,strata,id=id_3,outcome=outcome3,confouder=alcohol_amount_3))
#matched_data<-with(violence_model,data.frame(breath_level,id))
#fup1$predictor<-car::recode(fup1$predictor,"'yes'=2;'no'=1")
#fup1$predictor<-as.numeric(as.character(fup1$predictor))
clogit_data<-rbind(fup1,fup2)#,fup3)
#clogit_data<-with(violence_model,data.frame(predictor1=predictor_FUP1,predictor2=predictor_FUP2,strata,outcome=breath_level))
#matched_data<-with(violence_model,data.frame(breath_level,id))
clogit_data$predictor<-car::recode(clogit_data$predictor,"2=1;1=2")
clogit_data$predictor<-as.factor(clogit_data$predictor)
x_1C<-clogit(outcome ~ predictor +strata(strata),clogit_data, method="exact")
summary(x_1C) 
clogistic.display(x_1C)

#create dummy variables to exposure within the clogit stuff

### MODEL 1:1 - 1 week
violence_model<-subset(cleaned_data,cleaned_data$method_injury=='Violence')
id_1<-c(1)
#id_2<-c(2)
id_3<-c(3)
strata<-c(1:length(violence_model$breath_level))
outcome1<-c(1)
#outcome2<-c(0)
outcome3<-c(0)
fup1<-with(violence_model,data.frame(predictor=breath_level,strata,id=id_1,outcome=outcome1,confouder=alcohol_amount_1))
#fup2<-with(violence_model,data.frame(predictor=predictor_FUP1,strata,id=id_2,outcome=outcome2,confouder=alcohol_amount_2))
fup3<-with(violence_model,data.frame(predictor=predictor_FUP2,strata,id=id_3,outcome=outcome3,confouder=alcohol_amount_3))
#matched_data<-with(violence_model,data.frame(breath_level,id))
#fup1$predictor<-car::recode(fup1$predictor,"'yes'=2;'no'=1")
#fup1$predictor<-as.numeric(as.character(fup1$predictor))
clogit_data<-rbind(fup1,fup3)#,fup2)
#clogit_data<-with(violence_model,data.frame(predictor1=predictor_FUP1,predictor2=predictor_FUP2,strata,outcome=breath_level))
#matched_data<-with(violence_model,data.frame(breath_level,id))
clogit_data$predictor<-car::recode(clogit_data$predictor,"2=1;1=2")
clogit_data$predictor<-as.factor(clogit_data$predictor)
x_2C<-clogit(outcome ~ predictor +strata(strata),clogit_data, method="exact")
summary(x_2C) 
clogistic.display(x_2C)

#create dummy variables to exposure within the clogit stuff

### MODEL 1:2
violence_model<-subset(data_moshi_imp,data_moshi_imp$method_injury=='Violence')
id_1<-c(1)
id_2<-c(2)
id_3<-c(3)
strata<-c(1:length(violence_model$breath_level))
outcome1<-c(1)
outcome2<-c(0)
outcome3<-c(0)
fup1<-with(violence_model,data.frame(predictor=breath_level,strata,id=id_1,outcome=outcome1,confouder=bottle))
fup2<-with(violence_model,data.frame(predictor=predictor_FUP1,strata,id=id_2,outcome=outcome2,confouder=bottle24))
fup3<-with(violence_model,data.frame(predictor=predictor_FUP2,strata,id=id_3,outcome=outcome3,confouder=bottle1week))
#matched_data<-with(violence_model,data.frame(breath_level,id))
#fup1$predictor<-car::recode(fup1$predictor,"'yes'=2;'no'=1")
#fup1$predictor<-as.numeric(as.character(fup1$predictor))
clogit_data<-rbind(fup1,fup2,fup3)
#clogit_data<-with(violence_model,data.frame(predictor1=predictor_FUP1,predictor2=predictor_FUP2,strata,outcome=breath_level))
#matched_data<-with(violence_model,data.frame(breath_level,id))
# clogit_data$predictor<-car::recode(clogit_data$predictor,"2=1;1=2")
# clogit_data$predictor<-as.factor(clogit_data$predictor)
x_3C<-clogit(outcome ~ predictor +strata(strata),clogit_data, method="exact")
summary(x_3C) 
#clogistic.display(x)

x_3C<-clogit(outcome ~ confouder + strata(strata),clogit_data, method="exact")
summary(x_3C) 
clogistic.display(x_3C)

#DOSAGE LEVEL
# clogit_data$confounder_dosage<-car::recode(clogit_data$confouder,"'5 to 11'='high';'12 or more'='high'")
# mytable<-with(clogit_data,table(confounder_dosage,outcome))
# dose_matrix<-as.matrix(mytable)
# doseSpecificOddsRatios(dose_matrix)
#############################################################
#Non-violence related
#############################################################
violence_model<-subset(cleaned_data,cleaned_data$method_injury!='Violence')
id_1<-c(1)
id_2<-c(2)
#id_3<-c(3)
strata<-c(1:length(violence_model$breath_level))
outcome1<-c(1)
outcome2<-c(0)
#outcome3<-c(0)
fup1<-with(violence_model,data.frame(predictor=breath_level,strata,id=id_1,outcome=outcome1,confouder=alcohol_amount_1))
fup2<-with(violence_model,data.frame(predictor=predictor_FUP1,strata,id=id_2,outcome=outcome2,confouder=alcohol_amount_2))
#fup3<-with(violence_model,data.frame(predictor=predictor_FUP2,strata,id=id_3,outcome=outcome3,confouder=alcohol_amount_3))
#matched_data<-with(violence_model,data.frame(breath_level,id))
#fup1$predictor<-car::recode(fup1$predictor,"'yes'=2;'no'=1")
#fup1$predictor<-as.numeric(as.character(fup1$predictor))
clogit_data<-rbind(fup1,fup2)#,fup3)
#clogit_data<-with(violence_model,data.frame(predictor1=predictor_FUP1,predictor2=predictor_FUP2,strata,outcome=breath_level))
#matched_data<-with(violence_model,data.frame(breath_level,id))
clogit_data$predictor<-car::recode(clogit_data$predictor,"2=1;1=2")
clogit_data$predictor<-as.factor(clogit_data$predictor)
x_1C<-clogit(outcome ~ predictor +strata(strata),clogit_data, method="exact")
summary(x_1C) 
clogistic.display(x_1C)

#create dummy variables to exposure within the clogit stuff

### MODEL 1:1 - 1 week
violence_model<-subset(cleaned_data,cleaned_data$method_injury!='Violence')
id_1<-c(1)
#id_2<-c(2)
id_3<-c(3)
strata<-c(1:length(violence_model$breath_level))
outcome1<-c(1)
#outcome2<-c(0)
outcome3<-c(0)
fup1<-with(violence_model,data.frame(predictor=breath_level,strata,id=id_1,outcome=outcome1,confouder=alcohol_amount_1))
#fup2<-with(violence_model,data.frame(predictor=predictor_FUP1,strata,id=id_2,outcome=outcome2,confouder=alcohol_amount_2))
fup3<-with(violence_model,data.frame(predictor=predictor_FUP2,strata,id=id_3,outcome=outcome3,confouder=alcohol_amount_3))
#matched_data<-with(violence_model,data.frame(breath_level,id))
#fup1$predictor<-car::recode(fup1$predictor,"'yes'=2;'no'=1")
#fup1$predictor<-as.numeric(as.character(fup1$predictor))
clogit_data<-rbind(fup1,fup3)#,fup2)
#clogit_data<-with(violence_model,data.frame(predictor1=predictor_FUP1,predictor2=predictor_FUP2,strata,outcome=breath_level))
#matched_data<-with(violence_model,data.frame(breath_level,id))
clogit_data$predictor<-car::recode(clogit_data$predictor,"2=1;1=2")
clogit_data$predictor<-as.factor(clogit_data$predictor)
x_2C<-clogit(outcome ~ predictor +strata(strata),clogit_data, method="exact")
summary(x_2C) 
clogistic.display(x_2C)

#create dummy variables to exposure within the clogit stuff

### MODEL 1:2
violence_model<-subset(data_moshi_imp,data_moshi_imp$method_injury!='Violence')
id_1<-c(1)
id_2<-c(2)
id_3<-c(3)
strata<-c(1:length(violence_model$breath_level))
outcome1<-c(1)
outcome2<-c(0)
outcome3<-c(0)
fup1<-with(violence_model,data.frame(predictor=breath_level,strata,id=id_1,outcome=outcome1,confouder=bottle,years_education,age,gender))
fup2<-with(violence_model,data.frame(predictor=predictor_FUP1,strata,id=id_2,outcome=outcome2,confouder=bottle24,years_education,age,gender))
fup3<-with(violence_model,data.frame(predictor=predictor_FUP2,strata,id=id_3,outcome=outcome3,confouder=bottle1week,years_education,age,gender))
#matched_data<-with(violence_model,data.frame(breath_level,id))
#fup1$predictor<-car::recode(fup1$predictor,"'yes'=2;'no'=1")
#fup1$predictor<-as.numeric(as.character(fup1$predictor))
clogit_data<-rbind(fup1,fup2,fup3)
#clogit_data<-with(violence_model,data.frame(predictor1=predictor_FUP1,predictor2=predictor_FUP2,strata,outcome=breath_level))
#matched_data<-with(violence_model,data.frame(breath_level,id))
# clogit_data$predictor<-car::recode(clogit_data$predictor,"2=1;1=2")
# clogit_data$predictor<-as.factor(clogit_data$predictor)
x_3C<-clogit(outcome ~ predictor + strata(strata),clogit_data, method="exact")
summary(x_3C) 
clogistic.display(x)


x_3C<-clogit(outcome ~ confouder + strata(strata),clogit_data, method="exact")
summary(x_3C) 
clogistic.display(x_3C)

#DOSAGE LEVEL
clogit_data$confounder_dosage<-car::recode(clogit_data$confouder,"'5 to 11'='high';'12 or more'='high'")
mytable<-with(clogit_data,table(confounder_dosage,outcome))
dose_matrix<-as.matrix(mytable)
doseSpecificOddsRatios(dose_matrix)
#########################################################
#PLOTTING
#########################################################

### PLOTING MODEL
#plot_odds<-function(x, title = NULL){
tmp_1<-data.frame(rbind(exp(coef(x_1)),exp(coef(x_2)),exp(coef(x_3))),rbind(exp(confint(x_1)),exp(confint(x_2)),exp(confint(x_3))))
#odds<-tmp[-1,]
names(tmp_1)<-c('OR', 'lower', 'upper')
tmp_1$vars<-c('24 hours','1 week','Overall')
tmp_1$facet<-c('Alcohol Use')
#ticks<-c(seq(.1, 1, by =.1), seq(0, 10, by =1), seq(10, 100, by =10))

#plot_odds<-function(x, title = NULL){
tmp_2<-data.frame(rbind(exp(coef(x_1B)),exp(coef(x_2B)),exp(coef(x_3B))),rbind(exp(confint(x_1B)),exp(confint(x_2B)),exp(confint(x_3B))))
#odds<-tmp[-1,]
names(tmp_2)<-c('OR', 'lower', 'upper')
tmp_2$vars<-c('24 hours','1 week','Overall')
tmp_2$facet<-c('RTI')
#ticks<-c(seq(.1, 1, by =.1), seq(0, 10, by =1), seq(10, 100, by =10))

#plot_odds<-function(x, title = NULL){
tmp_3<-data.frame(rbind(exp(coef(x_1C)),exp(coef(x_2C)),exp(coef(x_3C))),rbind(exp(confint(x_1C)),exp(confint(x_2C)),exp(confint(x_3C))))
#odds<-tmp[-1,]
names(tmp_3)<-c('OR', 'lower', 'upper')
tmp_3$vars<-c('24 hours','1 week','Overall')
tmp_3$facet<-c('Violence')
#ticks<-c(seq(.1, 1, by =.1), seq(0, 10, by =1), seq(10, 100, by =10))

odds<-rbind(tmp_1,tmp_2,tmp_3)

ggplot(odds, aes(y= OR, x = reorder(vars, OR))) +
geom_point() +
geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
#scale_y_log10(breaks=ticks, labels = ticks) +
geom_hline(yintercept = 1, linetype=2) +
coord_flip() +
labs(title = "", x = 'Variables', y = 'OR') + 
theme_bw() +
facet_wrap(~ facet,ncol=1) 




