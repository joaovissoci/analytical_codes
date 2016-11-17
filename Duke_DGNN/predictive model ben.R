############################################################################################
# First part of R script is dedicated to import data and adjust variables to make ir processable
############################################################################################
###########################################################################################
#Carrega os pacotes a serem utilizados
#PASCKAGES INSTALLATION CODES
# install.packages("caret")
# install.packages("AppliedPredictiveModeling")
# install.packages("mlbench")
# install.packages("QSARdata")
# install.packages("ipred")
# install.packages("parallelRandomForest")
# install.packages("e1071")
# install.packages("pROC")
# install.packages("gbm")
# install.packages("doParallel")
# install.packages("reshape")
# install.packages("foreign")
# install.packages("readstata13")
# install.packages("data.table")

#Load packages needed for the analysis
lapply(c("caret",
         "AppliedPredictiveModeling",
         "mlbench",
         "QSARdata",
         "ipred",
         # "parallelRandomForest",
         "e1071",
         "pROC",
         "gbm",
         "doParallel",
         "reshape",
         # "foreign",
         "readstata13",
         "haven",
         "mice",
         "VIM"),
         library, character.only=T)

############################################################################################
# Import data from STATA
############################################################################################
#Abdominal
abdominal <- haven::read_dta("/Users/jnv4/Desktop/Predictive model ben/Combined 2012 and 2013 NSQIP-P - G1 Abdominal.dta")
abdominal <- lapply(abdominal, unclass)

#Abdominal wall repair
abdominal_wall_repair <- haven::read_dta("/Users/jnv4/Desktop/Predictive model ben/Combined 2012 and 2013 NSQIP-P - G2 Abdominal wall repair.dta")
abdominal_wall_repair <- lapply(abdominal_wall_repair, unclass)

#Appendectomy
apendectomy <- haven::read_dta("/Users/jnv4/Desktop/Predictive model ben/Combined 2012 and 2013 NSQIP-P - G3 Appendectomy.dta")
apendectomy <- lapply(apendectomy, unclass)

#biliary
biliary <- haven::read_dta("/Users/jnv4/Desktop/Predictive model ben/Combined 2012 and 2013 NSQIP-P - G4 Biliary.dta")
biliary <- lapply(biliary, unclass)

#cholecystectomy
cholecystectomy <- haven::read_dta("/Users/jnv4/Desktop/Predictive model ben/Combined 2012 and 2013 NSQIP-P - G5 Cholecystectomy.dta")
cholecystectomy <- lapply(cholecystectomy, unclass)

#colorectal
colorectal <- haven::read_dta("/Users/jnv4/Desktop/Predictive model ben/Combined 2012 and 2013 NSQIP-P - G6 Colorectal.dta")
colorectal <- lapply(colorectal, unclass)

#diaphragmatic
diaphragmatic <- haven::read_dta("/Users/jnv4/Desktop/Predictive model ben/Combined 2012 and 2013 NSQIP-P - G7 Diaphragmatic.dta")
diaphragmatic <- lapply(diaphragmatic, unclass)

#esophageal
esophageal <- haven::read_dta("/Users/jnv4/Desktop/Predictive model ben/Combined 2012 and 2013 NSQIP-P - G8 Esophageal.dta")
esophageal <- lapply(esophageal, unclass)

#genital_resection
genital_resection <- haven::read_dta("/Users/jnv4/Desktop/Predictive model ben/Combined 2012 and 2013 NSQIP-P - G9 Genital Resection _ Reconstruction.dta")
genital_resection <- lapply(genital_resection, unclass)

#liver_resection
liver_resection <- haven::read_dta("/Users/jnv4/Desktop/Predictive model ben/Combined 2012 and 2013 NSQIP-P - G10 Liver Resection.dta")
liver_resection <- lapply(liver_resection, unclass)

#pancreatic
pancreatic <- haven::read_dta("/Users/jnv4/Desktop/Predictive model ben/Combined 2012 and 2013 NSQIP-P - G11 Pancreatic.dta")
pancreatic <- lapply(pancreatic, unclass)

#splenectomy
splenectomy <- haven::read_dta("/Users/jnv4/Desktop/Predictive model ben/Combined 2012 and 2013 NSQIP-P - G12 Splenectomy.dta")
splenectomy <- lapply(splenectomy, unclass)

#thoracic
thoracic <- haven::read_dta("/Users/jnv4/Desktop/Predictive model ben/Combined 2012 and 2013 NSQIP-P - G13 Thoracic.dta")
thoracic <- lapply(thoracic, unclass)

############################################################################################
# Data management
############################################################################################
#Addinng diagnostic variable
abdominal$diagnostic_code <- c("abdominal")
abdominal_wall_repair$diagnostic_code <- c("abdominal_wall_repair")
apendectomy$diagnostic_code <- c("apendectomy")
biliary$diagnostic_code <- c("biliary")
cholecystectomy$diagnostic_code <- c("cholecystectomy")
colorectal$diagnostic_code <- c("colorectal")
diaphragmatic$diagnostic_code <- c("diaphragmatic")
esophageal$diagnostic_code <- c("esophageal")
genital_resection$diagnostic_code <- c("genital_resection")
liver_resection$diagnostic_code <- c("liver_resection")
pancreatic$diagnostic_code <- c("pancreatic")
splenectomy$diagnostic_code <- c("splenectomy")
thoracic$diagnostic_code <- c("thoracic")

#Aggregating datasets
data_all<-rbind(
as.data.frame(as.data.frame(abdominal)),
as.data.frame(abdominal_wall_repair),
as.data.frame(apendectomy),
as.data.frame(biliary),
as.data.frame(cholecystectomy),
as.data.frame(colorectal),
as.data.frame(diaphragmatic),
as.data.frame(esophageal),
as.data.frame(genital_resection),
as.data.frame(liver_resection),
as.data.frame(pancreatic),
as.data.frame(splenectomy),
as.data.frame(thoracic))

# select variables to be used in this model
model_data<-with(data_all,data.frame(
  male_01,
  ethnicity_01,
  race_01,
  agec_01,
  prem_01,
  height_to_age_01,
  weight_to_age_01,
  height_to_weight_01,
  crf_01,
  prvpcs_01,
  diabetes_0,
  wtloss_01,
  nutr_support_01,
  asthma_01,
  struct_pulm_ab_01,
  oxygen_sup_01,
  ventilat_01,
  tracheostomy_01,
  hxcld_01,
  cystic_fib_01,
  cpneumon_01,
  bleeddis_01,
  hemodisorder_01,
  transfus_01,
  immune_dis_01,
  steroid_01,
  cancer_01,
  chemo_01,
  radio_01,
  bone_marrow_trans_01,
  organ_trans_01,
  acq_abnormality_01,
  cerebral_palsy_01,
  tumorcns_01,
  coma_01,
  neuromuscdis_01,
  impcogstat_01,
  seizure_01,
  ivh_01,
  cva_01,
  esovar_01,
  lbp_disease_01,
  renafail_01,
  dialysis_01,
  wndinf_01,
  proper30_01,
  preop_sepsis_1,
  inotr_support_01,
  cpr_prior_surg_01,
  dnr_01,
  htooday,
  tothlos_01,
  workrvu,
  optime_01,
  asaclas_01,
  wndclas_01,
  transt_01,
  inpatient_01,
  casetype_01,
  adverse_any_01,
  diagnostic_code
  ))

############################################################################################
# Pre-Processing: Dummy
############################################################################################
#create dummy variables
head(model.matrix(~., data = model_data))
dummies <- dummyVars(~., data = model_data)
head(predict(dummies, newdata = model_data))
model_data_dummy <-predict(dummies, newdata = model_data)
model_data_dummy <- as.data.frame(model_data_dummy)
# rm(dummies)

############################################################################################
# Pre-Processing: Missing data check and Imputation
############################################################################################
#vingnette: [http://www.stefvanbuuren.nl/publications/MICE%20in%20R%20-%20Draft.pdf]
#loading packages
lapply(c("mice",
         "VIM",
         "lattice"),
         library, character.only=T)

#Inspect missing patterns / MICE package needed
missing_pattern<-md.pattern(model_data_dummy)
missing_pattern #shows missing cases in the rows. combinations showing missing in columns

#Inspect missing patterns on pairs of variables
missing_pairs<-md.pairs(model_data_dummy)
missing_pairs$rr #pairs with cases where both variables are observed
missing_pairs$rm #pairs with cases where the first variables ir observed and the second is a missing
missing_pairs$mr #pairs with cases where the first variables ir missing and the second is a observed
missing_pairs$mm #pairs with cases where the both variables are missing

#Graphically inspect missing pattern over pairs of variables / VIM package needed
#scatter plot with bar plost of distribution
marginplot(model_data_dummy[,c("weight_to_age_01","tothlos_01")],
                             col=c("blue","red","orange"),
                             cex=1.5,
                             cex.lab=1.5,
                             cex.numbers=1.3,
                             pch=19)

#distribution of mising and non-missing data
pbox(model_data_dummy,pos=1,int=FALSE,cex=1.2)

#Creating imputations: Step by setPS
#Check for MAR or MNAR

#Define predictors for multiple imputations
#Use 15 to 25 predictors if possible
#Define predictors that best explain the variance of the targeted variable
#Check for correlation between variables
cor(model_data_dummy, use="pairwise.complete.obs")

#Find variables with good proportion of complete vs. missing with targeted variables
#Function quickpred finds that and returns a predictorMatrix object to set the predictors
quickpred(model_data_dummy)

#methodd for numerical data is PMM = Predictive mean matching
#

imp <- mice(nhanes, 
            pred=quickpred(nhanes,
                           minpuc=0.25, #minimum % of complete cases in pairs of variables
                           include="age"), #defines variables to always be included as predictors
            seed=333333)

data_imputed<-complete(imp)

#post Processing
#use argument post that receives a string to restraing imputation to valid arguments
# post["chl"] <- "imp[[j]][,i] <- squeeze(imp[[j]][,i],c(100,300))"
# imp <- mice(nhanes2.ext, meth=meth, pred=pred, post=post, seed=1, maxit=10)
# imp$imp$chl

#Inspect diagnostic measures / Lettice package needed
com <- complete(imp, "long", inc=T)
col <- rep(c("blue","red")[1+as.numeric(is.na(imp$data$chl))],6)
stripplot(chl˜.imp, data=com, jit=TRUE, fac=0.8, col=col, pch=20,
cex=1.4, xlab="Imputation number")

############################################################################################
# Begin of preparation for predictive modelling
############################################################################################
# I did the option to use step by step approach instead of use caret preprocessing to make easier identify where may be any problems
#create dummy variables
head(model.matrix(~., data = abdominal_wall_repair_filtered))
dummies <- dummyVars(~., data = abdominal_wall_repair_filtered)
head(predict(dummies, newdata = abdominal_wall_repair_filtered))
abdominal_wall_repair_filtered <-predict(dummies, newdata = abdominal_wall_repair_filtered)
abdominal_wall_repair_filtered <- data.table(abdominal_wall_repair_filtered)
rm(dummies)
#missing check - (there is some variables with large number of missings)
sapply(abdominal_wall_repair_filtered, function(x) sum(is.na(x)))
abdominal_wall_repair_filtered <- na.omit(abdominal_wall_repair_filtered)

# Create outcome
outcome_abdominal_wall_repair_filtered <- abdominal_wall_repair_filtered$adverse_any_01.yes


#create a dataset only with predictors
predictors_abdominal_wall_repair_filtered <- subset(abdominal_wall_repair_filtered, select = -c(adverse_any_01.yes,adverse_any_01.no))

#Near-Zero variance - analysis (there is 78 of 125 variables with near zero variance issue)
nzv <- nearZeroVar(predictors_abdominal_wall_repair_filtered, saveMetrics= TRUE)
summary(nzv$nzv)
predictors_abdominal_wall_repair_filtered[, rownames(nzv[nzv$nzv,])] <- list(NULL)
rm(nzv)

#### Create partitioning datasets
# Training set  will be used to performed all modeling
# Test set will be used to test model's performance - accuracy, model comparison
# set a random number to start the randomization
set.seed(107)
# arguments are: dataset = nome do dataset, proportion = 3/4
inTrain_abdominal <- createDataPartition(outcome_abdominal_wall_repair_filtered, p = 3/4, list = FALSE)
trainDescr_abdominal <- predictors_abdominal_wall_repair_filtered[inTrain_abdominal,] # isolate descriptors for the train data para turnover medico
testDescr_abdominal <- predictors_abdominal_wall_repair_filtered[-inTrain_abdominal,] # isolate descriptors for the test data para turnover medico

trainClass_abdominal <- outcome_abdominal_wall_repair_filtered[inTrain_abdominal] # isolate outcome for the train data
testClass_abdominal <- outcome_abdominal_wall_repair_filtered[-inTrain_abdominal] # isolate outcome for the train data

rm(inTrain_abdominal)

############################################################################
# Paralell Processing
############################################################################

doParallel::registerDoParallel(4) #define number of threads that will be used during analysis process

############################################################################
# Pre-Processing
############################################################################
##### Multicolinearity assessment
col_antes_abdominal <- ncol(trainDescr_abdominal) #number of columns in the dataset

#create correlation matrix
descrCorr_abdominal <- cor(trainDescr_abdominal)

#identify highly correlated data - above 0.90
highCorr_abdominal <- findCorrelation(descrCorr_abdominal, .90, verbose= T)

# exclude highly correlated data from the dataset
#turnover
trainDescr_abdominal<- subset(trainDescr_abdominal, select = -highCorr_abdominal)
testDescr_abdominal <- subset(testDescr_abdominal, select = -highCorr_abdominal)
#show the mumber of removed columns
ncol(trainDescr_abdominal)- col_antes_abdominal

rm(descrCorr_abdominal, col_antes_abdominal, highCorr_abdominal)
##############################################################################################
#### Pre-process data to find center, re-scaling, dimensionality issues
# The function has an argument, method, that can have possible values of 
#"center", "scale", "pca" and "spatialSign". The first two options provide 
#simple location and scale transformations of each predictor (and are the 
# default values of method).

preprocess_abdominal<- preProcess(trainDescr_abdominal) #define pre-processing methods for each variable

#Apply pre-processing to dataset
trainDescr_abdominal <- predict(preprocess_abdominal, trainDescr_abdominal)
testDescr_abdominal <- predict(preprocess_abdominal, testDescr_abdominal)

#finding linear combos
#turnover
comboInfo_abdominal <- findLinearCombos(trainDescr_abdominal)
comboInfo_abdominal

#remove os combos lineares dos datasets de teste
trainDescr_abdominal<-trainDescr_abdominal[, -comboInfo_abdominal$remove]
testDescr_abdominal<-testDescr_abdominal[, -comboInfo_abdominal$remove]

############################################################################
# Tuning training data set
############################################################################

#For the train function, the possible resampling methods are: bootstrapping, 
#k-fold cross- validation, leave-one-out cross-validation, and leave-group-
#out cross-validation (i.e., repeated splits without replacement).

#Arguments are:
#a matrix or data.frame of predictors - must be numeric
#a vector for outcomes - might be numeric or factors
#method used to train the dataset. To see options, check pg 9 - https://drive.google.com/open?id=0B4TReYGK49h_UU5wR2IwU3BMSzg
#a vector discussing the metrics to be returned. Options are "Accuracy", "Kappa", "RMSE" or "Rsquared"
#trControl: a list of control parameters such as number of resamples

#creating controlling variable
bootControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, allowParallel = TRUE, search = "random")
bootControl_rf <- trainControl(method = "oob", number = 5, allowParallel = TRUE, search = "random")
set.seed(2)

# building the tuning model with the random forest method
#Joao confere a metrica a ser usada - fiquei um pouco indeciso sobre qual usar: Accuracy, Kappa, RMSE ou ROC
rfFit_abdominal <- train(trainDescr_abdominal, trainClass_abdominal,
                        method = "parRF", tuneLength = 5,
                        metric = "Accuracy",
                        trControl = bootControl_rf)
rfFit_abdominal
rfFit_abdominal$finalModel

# building the tuning model with the bagged trees method
bagTFit_abdominal <- train(trainDescr_abdominal, trainClass_abdominal,
                          method = "treebag", metric = "Accuracy", trControl = bootControl, tuneLength = 20)
bagTFit_abdominal
bagTFit_abdominal$finalModel

# Example with the boosted tree method
# here we are controling for size of tree, iterations and learning rate
gbmGrid <- expand.grid(.interaction.depth = (1:5) * 2,
                       .n.trees = (1:10)*25, .shrinkage = .1, .n.minobsinnode= 5)
set.seed(2)
gbmFit_abdominal <- train(trainDescr_abdominal, trainClass_abdominal,
                         method = "gbm", trControl = bootControl, verbose = FALSE,
                         bag.fraction = 0.5, tuneGrid = gbmGrid)
plot(gbmGrid)
plot(gbmFit_abdominal, metric = "Kappa")
plot(gbmFit_abdominal, plotType = "level")
resampleHist(gbmFit_abdominal)
gbmFit_abdominal
# todo meu parco conhecimento sobre machine learning acaba aqui......kkkkkkkk

############################################################################
# Prediction de novas amostras após o modelo ter sido rodado
############################################################################


library(bnlearn)

res<-hc(data_imputed)

plot(res)

qgraph(res)




############################################################################
# Prediction Performance
############################################################################

























