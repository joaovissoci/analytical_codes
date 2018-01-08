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
lapply(c("ggplot2","gridExtra" ,"psych", 
   "RCurl", "irr", "nortest", "moments","GPArotation",
   "nFactors","gdata","meta","ggplot2",
   "gridExtra" ,"psych", "RCurl", "irr", "nortest", 
   "moments","GPArotation","nFactors","gdata",
   "repmis","sqldf","VIM","survival",
   "sas7bdat","epicalc","vcd","mice",
   "tidyverse"), library, character.only=T)

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
data_tz<-read.sas7bdat("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/Africa/tanzclean.sas7bdat")
data_tz<-as.data.frame(data_tz)

data_mzsa <- read.spss("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/Africa/Multi_country/Tz, SA, Mz alcohol use in ED/whoer_mozambique_southafrica.sav",
                       to.data.frame=TRUE)
data_mzsa<-as.data.frame(data_mzsa)
###################################################
#DATA MANAGEMENT
###################################################

#Organizing TZ data

# #Recoding QB01a from hours to minutes
# #data$QB01a<-data$QB01a*60

# #Creating QB01 (summing up QB01A and QB01b)
# #data$QB01<-NULL
# #data$QB01<-with(data,rowSums(data.frame(QB01a,QB01b)))

# #Creating GCS score
# #gcs<-with(data,rowSums(data.frame(QL02,QL02a,QL02b)))


NaNto0<-function(x){
  car::recode(x,"NaN=0")
  }

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
alcohol_amount_1<-with(data_tz,data.frame(
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


alcohol_amount_1<-lapply(alcohol_amount_1,NaNto0)
alcohol_amount_1<-rowSums(as.data.frame(alcohol_amount_1))

#transforming into alcohol amount
data_tz$bottles_drank_tz<-alcohol_amount_1/16.5

#Merging datasets

data_tz$country<-c("ATz")
data_tz$weights<-c(1)

data_tz2<-with(data_tz,data.frame(current_alcohol_use=as.character(QF04),
                                 breath_level=QD04,
                                 age=AGE,
                                 gender=as.character(SEX),
                                 years_education=QI01,
                                 work=as.character(QI02),
                                 # sys_bp=QL01,
                                 # gcs=QL02C,
                                 # resp_rate=QL03,
                                 # pulse=QL04,
                                 # avpu=QL05,
                                 # freq_injuries=QL06,
                                 # time_to_injury=QB01,
                                 # fracture=QE011,
                                 # dislocation=QE012,
                                 # open_wound=QE013,
                                 # bruise=QE014,
                                 # burn=QE015,
                                 # concussion=QE016,
                                 # organ_injury=QE017,
                                 method_injury=as.character(QE02),
                                 # type_vehicle=QE02B,
                                 # motive_injury=QE03,
                                 # location=QE06,
                                 # activity_prior=QE071,
                                 past_alcohol_use=as.character(QG01),
                                 current_alcohol_use=as.character(QF04),
                                 # high_alcohol_use=QG03,
                                 # past_alchol_use=QG04,
                                 # low_alcohol_use=QG04,
                                 # emergency_need=QG07,
                                 # predictor_FUP1=QNH04,
                                 # predictor=QH02,
                                 # alcohol_amount_1,
                                 # alcohol_amount_2,
                                 # alcohol_amount_3,
                                 country=country,
                                 alcohol_amount=bottles_drank_tz,
                                 weight=weights))

##############################################
#Organize data for Mozambique and South Africa

#Alcohol Dosage - Time 1
# alcohol_amount_1_rowB<-with(data,(as.numeric(QF07B1B)*as.numeric(QF07B2))*as.numeric(QF07B3))
# alcohol_amount_1_rowC<-with(data,(as.numeric(QF07C1B)*as.numeric(QF07C2))*as.numeric(QF07C3))
# alcohol_amount_1_rowD<-with(data,(as.numeric(QF07D1B)*as.numeric(QF07D2))*as.numeric(QF07D3))
# alcohol_amount_1_rowE<-with(data,(as.numeric(QF07E1B)*as.numeric(QF07E2))*as.numeric(QF07E3))
# alcohol_amount_1_rowF<-with(data,(as.numeric(QF07F1B)*as.numeric(QF07F2))*as.numeric(QF07F3))
# # alcohol_amount_1_rowG<-with(data,(as.numeric(QF07G1B)*as.numeric(QF07G2))*as.numeric(QF07G3))
# # alcohol_amount_1_rowH<-with(data,(as.numeric(QF07H1B)*as.numeric(QF07H2))*as.numeric(QF07H3))
# # alcohol_amount_1_rowL1<-with(data,(as.numeric(QF07L11B)*as.numeric(QF07L12))*as.numeric(QF07L13))
# # alcohol_amount_1_rowL2<-with(data,(as.numeric(QF07L21B)*as.numeric(QF07L22))*as.numeric(QF07L23))
# # alcohol_amount_1_rowL3<-with(data,(as.numeric(QF07L31B)*as.numeric(QF07L32))*as.numeric(QF07L33))
# alcohol_amount_1<-with(data_tz,data.frame(
#                      QF07B5,
#                      QF07C5,
#                      QF07D5,
#                      QF07E5,
#                      QF07F5,
#                      QF07G5,
#                      QF07H5,
#                      QF07L15,
#                      QF07L25,
#                      QF07L35))


# alcohol_amount_1<-lapply(alcohol_amount_1,NaNtoNA)
# alcohol_amount_1<-rowSums(as.data.frame(alcohol_amount_1))

#transforming into alcohol amount
data_mzsa$bottles_drank_mzsa<-data_mzsa$qf07k/16.5

#aggregate data
data_mzsa2<-with(data_mzsa,data.frame(current_alcohol_use=qf04,
                                 breath_level=qd04,
                                 age=qa07,
                                 gender=qa08,
                                 years_education=qi01,
                                 work=qi02,
                                 # sys_bp=QL01,
                                 # gcs=QL02C,
                                 # resp_rate=QL03,
                                 # pulse=QL04,
                                 # avpu=QL05,
                                 # freq_injuries=QL06,
                                 # time_to_injury=QB01,
                                 # fracture=QE011,
                                 # dislocation=QE012,
                                 # open_wound=QE013,
                                 # bruise=QE014,
                                 # burn=QE015,
                                 # concussion=QE016,
                                 # organ_injury=QE017,
                                 method_injury=qe02,
                                 # type_vehicle=qe02B,
                                 # motive_injury=QE03,
                                 # location=QE06,
                                 # activity_prior=QE071,
                                 past_alcohol_use=qg01,
                                 current_alcohol_use=qg01,
                                 # high_alcohol_use=QG03,
                                 # past_alchol_use=QG04,
                                 # low_alcohol_use=QG04,
                                 # emergency_need=QG07,
                                 # predictor_FUP1=qnh04,
                                 # predictor=qh02,
                                 # alcohol_amount_1=,
                                 # alcohol_amount_2=,
                                 # alcohol_amount_3=
                                 country=country,
                                 alcohol_amount=bottles_drank_mzsa,
                                 weight=finalwgt))

# #separating countries
# data_mz<-data_mzsa2 %>% subset(country=="Mozambique")

# data_sa<-data_mzsa2 %>% subset(country=="South Africa")

# #Applying sampling weights to South Africa
# library(survey)

#### CREATING ETHANOL AMOUT / ml data

data_who<-rbind(data_mzsa2,data_tz2)

c9999toNA<-function(x){
   car::recode(x,"9999=NA")
}
data_who<-lapply(data_who,c9999toNA)
data_who<-as.data.frame(data_who)

NaNtoNA<-function(x){
   car::recode(x,"NaN=0")
}

data_who<-lapply(data_who,NaNtoNA)
data_who<-as.data.frame(data_who)

## RECODING DATA

#Current alcohol use
data_who$current_alcohol_use<-car::recode(data_who$current_alcohol_use,
                  "0=NA;
                   1='Yes';
                   2= 'No';
                   9=NA")
data_who$current_alcohol_use<-as.factor(data_who$current_alcohol_use)

#Breathlyzer level Positive
data_who$breath_level<-car::recode(data_who$breath_level,"
                   0='no';
                   9=NA;
                   else='yes'")
data_who$breath_level<-as.factor(data_who$breath_level)

#Age
data_who$age<-car::recode(data_who$age,"
                   18:30='18 to 30';
                   31:90='31 or more';
                   else=NA")
data_who$age<-as.factor(data_who$age)

#Gender
data_who$gender<-car::recode(data_who$gender,
                  "0=NA;
                   1='Male';
                   2='Female'")
data_who$gender<-as.factor(data_who$gender)

#Education years = OK

#Work
data_who$work<-car::recode(data_who$work,"0=NA;
                                          9=NA;
                                          1='yes';
                                          2='no'")
data_who$work<-as.factor(data_who$work)

#Method of injury
data_who$method_injury<-car::recode(data_who$method_injury,"
                        0=NA;
                        89=NA;
                        99=NA;
                        1='RTI';
                        2='RTI';
                        3='RTI';
                        5='Violence';
                        6='Violence';
                        7='Violence';
                        9='Outros';
                        10='Violence';
                        13='Outros';
                        14='Outros';
                        'Being hit by vehicle'='RTI';
                        'in vehicle collision as driver'='RTI';
                        'in vehicle collision as passenger'='RTI';
                        'Sexual assault'='Violence';
                        'Blunt force inj'='Violence';
                        'Gunshot'='Violence';
                        'Stab, cut, bite'='Violence';
                        'Choking, hanging'='Violence';
                        'Fall, trip'='Outros';
                        'Struck against /caught between'='Violence';
                        'Poisoning'='Violence';
                        'Burn with fire,hot liquid'='Outros';
                        'Other'='Outros';
                        'Unknown'=NA")
data_who$method_injury<-as.factor(
                 data_who$method_injury)

#Method of injury
data_who$method_injury_recoded<-car::recode(data_who$method_injury,"
                        'Violence'='Outros'")
data_who$method_injury_recoded<-as.factor(
                 data_who$method_injury_recoded)

#Past alcohol use (last 12 months)
data_who$past_alcohol_use<-car::recode(data_who$past_alcohol_use,
                  "1:8='yes';
                   9='no';
                   99=NA;
                   '1-5 a year'='yes';
                   '1/2 a week'='yes';
                   '2/3 a month'='yes';
                   '3/4 a week'='yes';
                   '6-11 a year'='yes';
                   'about 1 a month'='yes';
                   'every day'='yes';
                   'nearly every day'='yes';
                   'unknown'=NA;
                   'Not last 12 months'='no';
                   0=NA")
data_who$past_alcohol_use<-as.factor(data_who$past_alcohol_use)

data_who$alcohol_amount<-car::recode(
          data_who$alcohol_amount,"
                   0='0 use';
                   0.001:
                   2.5='1 to 2';
                   2.5001:4.5='3 to 4';
                   4.50001:72.73='5 or more'")
data_who$alcohol_amount<-as.factor(data_who$alcohol_amount)

# data_who$type_vehicle<-car::recode(data_who$type_vehicle,"
#                1='car';
#                2='avru';
#                3='avru';
#                4='truck';
#                9=NA;
#                else='non-RTI'")
# data_who$type_vehicle<-as.factor(data_who$type_vehicle)

#Separating RTI only data
rti_data<-subset(data_who,data_who$method_injury_recoded=="RTI")

# data_moshi$activity_prior<-car::recode(data_moshi$activity_prior,"99=NA")
# # data_moshi$high_alcohol_use<-car::recode(data_moshi$high_alcohol_use,"99=NA")
# data_moshi$past_alcohol_use<-car::recode(data_moshi$past_alcohol_use,"1:8='yes';9='no';99=NA")
# data_moshi$past_alcohol_use<-as.factor(data_moshi$past_alcohol_use)
# # data_moshi$low_alcohol_use<-car::recode(data_moshi$low_alcohol_use,"99=NA")
# data_moshi$predictor_FUP1<-car::recode(data_moshi$predictor_FUP1,"
#             1='yes';2='no';8=NA;9=NA;99=NA;NaN='no'")
# data_moshi$predictor_FUP1<-as.factor(data_moshi$predictor_FUP1)
# data_moshi$predictor_FUP2<-car::recode(data_moshi$predictor_FUP2,"
#             1='yes';2='no';8=NA;9=NA;99=NA;90=NA;NaN='no'")
# data_moshi$predictor_FUP2<-as.factor(data_moshi$predictor_FUP2)
# data_moshi$breath_level<-car::recode(data_moshi$breath_level,"0='no';9=NA;else='yes'")
# data_moshi$breath_level<-as.factor(data_moshi$breath_level)
# data_moshi$id<-c(1:516)
# data_moshi$breath_level_limit<-car::recode(data$QD04,"0:0.08='no';NA=NA;else='yes'")
# data_moshi$gcs<-car::recode(data_moshi$gcs,"99=NA")
# # data_moshi$sys_bp<-car::recode(data_moshi$sys_bp,"999=NA")
# # data_moshi$pulse<-car::recode(data_moshi$pulse,"999=NA")
# # data_moshi$avpu<-car::recode(data_moshi$avpu,"9=NA")
# data_moshi$fracture<-car::recode(data_moshi$fracture,"9=NA")
# data_moshi$dislocation<-car::recode(data_moshi$dislocation,"9=NA")
# data_moshi$open_wound<-car::recode(data_moshi$open_wound,"9=NA")
# data_moshi$bruise<-car::recode(data_moshi$bruise,"9=NA")
# data_moshi$concussion<-car::recode(data_moshi$concussion,"9=NA")
# data_moshi$organ_injury<-car::recode(data_moshi$organ_injury,"9=NA")
# data_moshi$type_vehicle<-car::recode(data_moshi$type_vehicle,"9=NA")
# data_moshi$motive_injury<-car::recode(data_moshi$motive_injury,"9=NA")
# data_moshi$location<-car::recode(data_moshi$location,"
#                1='home';
#                2='home';
#                3='drinking place';
#                4='drinking place';
#                5='drinking place';
#                6='drinking place';
#                8='work place';
#                9='Other';
#                10='outdoor public place';
#                11='outdoor public place';
#                12='Other';
#                99=NA")
# data_moshi$location<-as.factor(data_moshi$location)
# # data_moshi$emergency_need<-car::recode(data_moshi$emergency_need,"9=NA")
# data_moshi$bottle<-car::recode(data_moshi$bottles_drank_1,"0=0;0.001:2.5='1 to 2';2.5001:4.5='3 to 4';4.50001:72.73='5 or more'")
# data_moshi$bottle24<-car::recode(data_moshi$bottles_drank_2,"0=0;0.001:2.5='1 to 2';2.5001:4.5='3 to 4';4.50001:63.64='5 or more'")
# data_moshi$bottle1week<-car::recode(data_moshi$bottles_drank_3,"0=0;0.001:2.5='1 to 2';2.5001:4.5='3 to 4';4.50001:79.55='5 or more'")
# # data_moshi$bottle24_alcohol_positive<-car::recode(data_moshi$bottles_drank_2,"0=NA;0.001:2.5='1 to 2';2.5001:4.5='3 to 4';4.50001:63.64='5 or more'")
# # data_moshi$bottle1week_alcohol_positive<-car::recode(data_moshi$bottles_drank_3,"0=NA;0.001:2.5='1 to 2';2.5001:4.5='3 to 4';4.50001:159.1000='5 or more'")

# # data_moshi <- within(data_moshi, predictor_FUP1[QG01==9] <- 2)
# # data_moshi <- within(data_moshi, predictor_FUP2[QG01==9] <- 2)

#MANAGING MZ and SA data

#applying sampling weights

# data_who<-rbind(data_tz2,data_mzsa2)

#applying weights using the "survey" package
#see:http://faculty.washington.edu/tlumley/old-survey/index.html
#see:https://rpubs.com/corey_sparks/53683
library(survey)
data_who_full_weighted <- svydesign(ids = ~1, 
                     data = data_who,
                     weights = data_who$weight)

data_who_rti_weighted <- svydesign(ids = ~1, 
                     data = rti_data,
                     weights = rti_data$weight)

#data Tz
data_tz$method_injury<-car::recode(data_tz$QE02,"
            1='RTI';
            2='RTI';
            3='RTI';
            5='Violence';
            6='Violence';
            7='Violence';
            9='Outros';
            10='Violence';
            13='Outros';
            14='Outros'")
data_tz$type_vehicle<-car::recode(data_tz$QE02B,"
               1='car';
               2='avru';
               3='avru';
               4='car';
               9=NA;
               else='non-RTI'")

data_tz$past_alcohol_use<-car::recode(data_tz$QG01,"
               1:8='yes';
               9='no';
               99=NA")
data_tz$past_alcohol_use<-as.factor(data_tz$past_alcohol_use)

data_tz$breath_level<-car::recode(data_tz$QD04,"
               0='no';
               9=NA;
               else='yes'")
data_tz$breath_level<-as.factor(data_tz$breath_level)

rti_data_tz<-subset(data_tz,data_tz$method_injury=="RTI")


dim(data_tz)

###################################################
#IMPUTING MISSING DATA
###################################################
#Studying missing data
# #Calculating frequency of missing data per variable
# propmiss <- function(dataframe) lapply(dataframe,function(x) data.frame(nmiss=sum(is.na(x)), n=length(x), propmiss=sum(is.na(x))/length(x)))

# propmiss(data_moshi)

# #inspecting measure random of missing data
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

# # generate imputations
# # argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
# imp <- mice(data_tz, seed = 2222, m=10)

# # reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
# data_moshi_imp<-complete(imp,1)

#Plost the distrbution of each of the 5 possibilities of imputations
# stripplot(imp,pch=20,cex=1.2)

# #plots a scatter plot of pairs of variables
# xyplot(imp, outcome ~ visibility | .imp, pch = 20, cex = 1.4)

# #returns the matrix specifying each variable used to -predict imputation - columns 1=predictor 0=not predictor. rows are the variables of interest
# imp$predictorMatrix
# pred <- imp$predictorMatrix #if you want to exclude  variable from the prediction model for imputation then assign an obect to pred
# pred[, "bmi"] <- 0 #transform the column values into 0's for not predictiong
# imp <- mice(nhanes, pred = pred, pri = FALSE) # rerun the model specifying pred argumento witht eh matriz recoded.

#########################################################
#TABLE 1
#########################################################
#All injury DATA
#RTI prevalence with weighted sampling for all injury
with(data_who,table(country))
svytable(~country, design = data_who_full_weighted)
with(data_who,table(method_injury_recoded,country))
svytable(~method_injury_recoded + country, design = data_who_full_weighted)
prop.table(svytable(~method_injury_recoded + country, design = data_who_full_weighted),2)
#metanalysis
m3 <- metaprop(c(375,96,113),c(514,464,459), sm="PLOGIT")
m3

#RTI DATA Only
#gender use prevalences by country
with(rti_data,table(gender))
with(rti_data,table(gender,country))
svytable(~gender + country, design = data_who_rti_weighted)
prop.table(svytable(~gender + country, design = data_who_rti_weighted),2)
svychisq(~gender + country, design = data_who_rti_weighted,statistic = c("F"))
#metanalysis
m3 <- metaprop(c(279,80,67),c(375,113,96), sm="PLOGIT")
m3

#age use prevalences by country
with(rti_data,table(age))
with(rti_data,table(age,country))
svytable(~age + country, design = data_who_rti_weighted)
prop.table(svytable(~age + country, design = data_who_rti_weighted),2)
svychisq(~age + country, design = data_who_rti_weighted,statistic = c("F"))
#metanalysis
m3 <- metaprop(c(188,68,40),c(375,113,96), sm="PLOGIT")
m3

#education years use prevalences by country
svyby(~years_education, ~country, design = data_who_rti_weighted, svymean)
svychisq(~age + country, design = data_who_rti_weighted,statistic = c("F"))
0.39*sqrt(375)
0.32*sqrt(113)
0.44*sqrt(96)

#metanalysis
m3 <- metamean(n=c(375,113,96),
               mean=c(9.4,6.5,8.7),
               sd=c(7.6,3.4,4.3))
m3

#occupation use prevalences by country
with(rti_data,table(work))
with(rti_data,table(work,country))
svytable(~work + country, design = data_who_rti_weighted)
prop.table(svytable(~work + country, design = data_who_rti_weighted),2)
svychisq(~work + country, design = data_who_rti_weighted,statistic = c("F"))

#metanalysis
m3 <- metaprop(c(298,71,57),c(375,113,96), sm="PLOGIT")
m3

#Self-reported Alcohol use prevalences by country
with(rti_data,table(current_alcohol_use))
with(rti_data,table(current_alcohol_use,country))
svytable(~current_alcohol_use + country, design = data_who_rti_weighted)
prop.table(svytable(~current_alcohol_use + country, design = data_who_rti_weighted),2)
svychisq(~current_alcohol_use + country, design = data_who_rti_weighted,statistic = c("F"))

#metanalysis
m3 <- metaprop(c(96,26,26),c(375,113,96), sm="PLOGIT")
m3

#BAC Alcohol use prevalences by country
with(rti_data,table(breath_level))
with(rti_data,table(breath_level,country))
svytable(~breath_level + country, design = data_who_rti_weighted)
prop.table(svytable(~breath_level + country, design = data_who_rti_weighted),2)
svychisq(~breath_level + country, design = data_who_rti_weighted,statistic = c("F"))

#metanalysis
m3 <- metaprop(c(107,24,33),c(375,113,96), sm="PLOGIT")
m3

#Alcohol amount prevalences by country
with(rti_data,table(alcohol_amount))
with(rti_data,table(alcohol_amount,country))
svytable(~alcohol_amount + country, design = data_who_rti_weighted)
prop.table(svytable(~alcohol_amount + country, design = data_who_rti_weighted),2)
svychisq(~alcohol_amount + country, design = data_who_rti_weighted,statistic = c("F"))

#metanalysis
m3 <- metaprop(c(289,91,56),c(375,113,96), sm="PLOGIT")
m3
#metanalysis
m3 <- metaprop(c(18,4,1),c(375,113,96), sm="PLOGIT")
m3
#metanalysis
m3 <- metaprop(c(33,5,5),c(375,113,96), sm="PLOGIT")
m3
#metanalysis
m3 <- metaprop(c(35,13,31),c(375,113,96), sm="PLOGIT")
m3

#########################################################
#TABLE 2
#########################################################

#All injury DATA
#RTI prevalence with weighted sampling for all injury
with(data_who,table(method_injury_recoded))
with(data_who,table(method_injury_recoded,country))
svytable(~method_injury_recoded + country, design = data_who_full_weighted)
prop.table(svytable(~method_injury_recoded + country, design = data_who_full_weighted),2)
#metanalysis
m3 <- metaprop(c(375,113,93),c(516,459,464), sm="PLOGIT")
m3

#Self-Reported Alcohol use prevalences by country
with(data_who,table(current_alcohol_use))
with(data_who,table(current_alcohol_use,country))
svytable(~current_alcohol_use + country, design = data_who_full_weighted)
prop.table(svytable(~current_alcohol_use + country, design = data_who_full_weighted),2)
#metanalysis
m3 <- metaprop(c(143,77,238),c(516,459,464), sm="PLOGIT")
m3

#BAC Alcohol use prevalences by country
with(data_who,table(breath_level))
with(data_who,table(breath_level,country))
svytable(~breath_level + country, design = data_who_full_weighted)
prop.table(svytable(~breath_level + country, design = data_who_full_weighted),2)
#metanalysis
m3 <- metaprop(c(154,81,244),c(516,459,464), sm="PLOGIT")
m3

#RTI DATA Only
#Self-Reported Alcohol use prevalences by country
svytable(~past_alcohol_use + country, design = data_who_rti_weighted)
prop.table(svytable(~past_alcohol_use + country, design = data_who_rti_weighted),2)

#BAC Alcohol use prevalences by country
svytable(~breath_level + country, design = data_who_rti_weighted)
prop.table(svytable(~breath_level + country, design = data_who_rti_weighted),2)

# TZ data
# Type of rti
table<-with(rti_data_tz,table(type_vehicle))
table
prop.table(table)

#by self-reported alcohol use
table<-with(rti_data_tz,table(type_vehicle,QF04))
table
prop.table(table,2)

#by BAC level
table<-with(rti_data_tz,table(type_vehicle,breath_level))
table
prop.table(table,2)

###########################################################
##TABLE 2
###########################################################

#BAC Alcohol use prevalences by country
svytable(~breath_level + country, design = data_who_rti_weighted)
prop.table(svytable(~breath_level + country, design = data_who_rti_weighted),2)

## higher efficiency by modelling variance better
table2_model <- svyglm(past_alcohol_use~
                        age +
                        gender +
                        years_education +
                        work, 
              design=data_who_full_weighted,
              family=quasibinomial())






logmodel<-glm(current_alcohol_use ~ 
            age +
            gender +
            # years_education +
            # work +
            #rd_condition +
            method_injury_recoded*country
      ,family=binomial, data=data_who)

summary(logmodel)
#anova(reglogGEU)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)




#########################################################
#FIGURE 1
#########################################################

event_alcohol<-c(375,96,113,
                279,67,80,
                188,40,68,
                9.4,8.7,6.5,
                298,57,71,
                96,31,26,
                107,33,24,
                152,65,91,
                25,1,4,
                43,9,5,
                59,22,13)

n_alcohol<-c(375,96,113,
                279,67,80,
                188,40,68,
                9.4,8.7,6.5,
                298,57,71,
                96,31,26,
                107,33,24,
                25,1,4,
                43,9,5,
                59,22,13)

event_nonalcohol<-c(152,65,91,
                    152,65,91,
                    152,65,91)

n_nonalcohol<-c()

label<-c(rep("RTI",3),
         rep("Gender",3),
         rep("Age",3),
         rep("Education",3),
         rep("Employment",3),
         rep("Self-reported alcohol use",3),
         rep("Positive BAC",3),
         rep("1 to 2 containners",3),
         rep("3 to 4 containners",3),
         rep("5 or more containners",3))

country<-c(rep("Tanzania","South Africa","Mozambique",10))



