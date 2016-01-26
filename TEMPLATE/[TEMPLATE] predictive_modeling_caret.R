#############################################################################
#CARET PACKEGE FOR PREDICTIVE MODELING TEMPLATE
#############################################################################
#
#https://cran.r-project.org/web/packages/caret/vignettes/caret.pdf
#https://www.r-project.org/nosvn/conferences/useR-2013/Tutorials/kuhn/user_caret_2up.pdf
#http://machinelearningmastery.com/caret-r-package-for-applied-predictive-modeling/
#List of models supported by caret http://topepo.github.io/caret/modelList.html
#http://topepo.github.io/caret/index.html
#
#############################################################################
#SETTING ENVIRONMENT
#############################################################################
#PASCKAGES INSTALLATION CODES
install.packages("caret")
install.packages("caretNWS")
install.packages("mlbench")
install.packages("QSARdata")

#PACKAGES LOADING CODE
#Load packages neededz for the analysis
#library(Hmisc)

#All packages must be installes with install.packages() function
lapply(c("caret","caretNWS","mlbench","QSARdata"), 
library, character.only=T)

#############################################################################
#IMPORTING DATA
#############################################################################

data(Mutagen)
#object with the vector for the outcome
mutagen=Mutagen_Outcome

#object with the set of vector with predictors
descr=Mutagen_Dragon

############################################################################
# Exploratory data analysis
############################################################################
library(AppliedPredictiveModeling)
transparentTheme(trans = .4)

# Frequency Plots

#Correlation plots
featurePlot(x = iris[, 1:4],
            y = iris$Species,
            plot = "ellipse",
            ## Add a key at the top
            auto.key = list(columns = 3))

#Density plots
transparentTheme(trans = .9)
featurePlot(x = iris[, 1:4],
                  y = iris$Species,
                  plot = "density",
                  ## Pass in options to xyplot() to 
                  ## make it prettier
                  scales = list(x = list(relation="free"),
                                y = list(relation="free")),
                  adjust = 1.5,
                  pch = "|",
                  layout = c(4, 1),
                  auto.key = list(columns = 3))

#boxplots
featurePlot(x = iris[, 1:4],
                  y = iris$Species,
                  plot = "box",
                  ## Pass in options to bwplot() 
                  scales = list(y = list(relation="free"),
                                x = list(rot = 90)),
                  layout = c(4,1 ),
                  auto.key = list(columns = 2))

#create dummy variables
library(earth)
data(etitanic)
head(model.matrix(survived ~ ., data = etitanic))
dummies <- dummyVars(survived ~ ., data = etitanic)
head(predict(dummies, newdata = etitanic))

#Near-Zero variance
data(mdrr)
data.frame(table(mdrrDescr$nR11))
nzv <- nearZeroVar(mdrrDescr, saveMetrics= TRUE)
nzv[nzv$nzv,][1:10,]
filteredDescr <- mdrrDescr[, -nzv]

############################################################################
# Data preparation
############################################################################

#### Create partitioning datasets
# Training set  will be used to performed all modeling
# Test set will be used to test model's performance - accuracy, model comparison

# set a random number to start the randomization
set.seed(107)

# Creating partitions through
# arguments are: dataset = mutagen, proportion = 3/4
inTrain <- createDataPartition(mutagen, p = 3/4, list = FALSE)

#In cases where the outcome is numeric, the samples are split into quartiles
#and the sampling is done within each quartile. Although not discussed in 
#this paper, the package also contains method for selecting samples using 
#maximum dissimilarity sampling (Willett 1999). This approach to sampling 
#can be used to partition the samples into training and test sets on the 
#basis of their predictor values.

trainDescr <- descr[inTrain,] # isolate descriptors for the train data
testDescr <- descr[-inTrain,] # isolate descriptors for the test data
trainClass <- mutagen[inTrain] # isolate outcome for the train data
testClass <- mutagen[-inTrain] # isolate outcome for the train data

# Fixing an error with the test data
zv = apply(trainDescr, 2, function(x) length(unique(x))==1)
sum(zv) # there are 3 zero-variance columns; remove them
which(zv)
trainDescr = trainDescr[ ,!zv]
testDescr = testDescr[ ,!zv]

#For time series, see http://topepo.github.io/caret/splitting.html

############################################################################
# Pre-Processing
############################################################################
##### Multicolinearity assessment

ncol(trainDescr) #number of columns in the dataset

#create correlation matrix
descrCorr <- cor(trainDescr)

#identify highly correlated data - above 0.90
highCorr <- findCorrelation(descrCorr, 0.90)

# exclude highly correlated data from the dataset
trainDescr <- trainDescr[, -highCorr]
testDescr <- testDescr[, -highCorr]

ncol(trainDescr) #validate comparing number of columns
# in this example, we excluded 933 columns

#### Pre-process data to find center, re-scaling, dimensionality issues
# The function has an argument, method, that can have possible values of 
#"center", "scale", "pca" and "spatialSign". The first two options provide 
#simple location and scale transformations of each predictor (and are the 
# default values of method).

xTrans <- preProcess(trainDescr) #dfine pre-processing methods for each variable

#Apply pre-processing method to both sets
trainDescr <- predict(xTrans, trainDescr)
testDescr <- predict(xTrans, testDescr)

#finding linear combos
comboInfo <- findLinearCombos(ltfrDesign)
comboInfo
ltfrDesign[, -comboInfo$remove]

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
bootControl <- trainControl(number = 200)
set.seed(2)

# controlling through repeatedcv method
#ctrl <- trainControl(method = "repeatedcv",
#repeats = 3)

# building the tuning model with the SVM method
svmFit <- train(trainDescr, trainClass,
method = "rf", tuneLength = 5,
trControl = bootControl, scaled = FALSE)
svmFit
svmFit$finalModel

# You can also pre-process within the training function
## Center and scale the predictors for the training
## set and all future samples.
#preProc = c("center", "scale"))

#plsFit <- train(Class ~ .,
#data = training,
#method = "pls",
#tuneLength = 15,
#trControl = ctrl,
#preProc = c("center", "scale"))

# Example with the bagged tree method
# here we are controling for size of tree, iterations and learning rate
gbmGrid <- expand.grid(.interaction.depth = (1:5) * 2,
	.n.trees = (1:10)*25, .shrinkage = .1)
set.seed(2)
bmFit <- train(trainDescr, trainClass,
method = "gbm", trControl = bootControl, verbose = FALSE,
bag.fraction = 0.5, tuneGrid = gbmGrid)
plot(gbmGrid)
plot(gbmFit, metric = "Kappa")
plot(gbmFit, plotType = "level")
resampleHist(gbmFit)

#For the train function, the possible resampling methods are: bootstrapping, 
#k-fold cross- validation, leave-one-out cross-validation, and leave-group-
#out cross-validation (i.e., repeated splits without replacement). By 
#default, 25 iterations of the bootstrap are used as the resampling scheme. #In this case, the number of iterations was increased to 200 due to the large
#number of samples in the training set.

############################################################################
# Prediction of new samples
############################################################################

#predict function will predict the outcome based on the set of descriptors
# the arguments are initially the final tunned model (svmFit) and the data to be predicted (e.g. test data)
#code below will provide the predicted outcome for each obsevation in the test set. Here we are returning only the 5 initial observations
predict(svmFit, newdata = testDescr)[1:5]
predict.train(svmFit, newdata = testDescr, type = "prob")[1:5]


#to predict by a set of different models, create a list with the objects for each model fitted
models <- list(svm = svmFit, gbm = gbmFit)
testPred <- predict(models, newdata = testDescr) #predict data by each model
lapply(testPred, function(x) x[1:5] #apply prediction all together and return as a list

#obtain predictions for training, test and/or unknown samples at once and 
#will return the data in a data frame.
#arguments are: list of models to find prediction performance outputs, textX = dataset with predictors to be predicted (test set), testY = data set with outcomes to be predicted 
predValues <- extractPrediction(models,
testX = testDescr, testY = testClass)
testValues <- subset(predValues, dataType == "Test")
#works with the following object classes: lm, mars, earth,
#randomForest, gbm, mvr (in the pls package), rpart, RandomForest (from the 
#party package),pamrtrained, bagEarth, bagFDA, classbagg and regbagg

probValues <- extractProb(models,
testX = testDescr, testY = testClass)
testProbs <- subset(probValues, dataType == "Test")

plotClassProbs(testProbs) # for classification models

############################################################################
# Prediction Performance
############################################################################

#### Performance measures
#describe the performance of classification models
svmPred <- subset(testValues, model == "svmRadial")

#compute various summaries for classification models
confusionMatrix(svmPred$pred, svmPred$obs) 

#ROC curves baes on probailities
svmProb <- subset(testProbs, model == "svmRadial")
svmROC <- roc(svmProb$mutagen, svmProb$obs)
aucRoc(vmROC)

#summmary of all performance metrics
multiClassSummary(test_results, lev = levels(test_results$obs))


#### Variable Importance
# works with the following object classes: lm, mars, earth, randomForest, 
#gbm, mvr (in the pls package), rpart, RandomForest (from the party package),
#pamrtrained, bagEarth, bagFDA, classbagg and regbagg.
# see definition of importante by model in pg. 17 - 
gbmImp <- varImp(gbmFit, scale = FALSE)
gbmImp
plot(varImp(gbmFit), top = 20)

############################################################################
# Paralell Processing exmaple
############################################################################
#caretNWS method

svmFit <- trainNWS(trainDescr, trainClass,
method = "svmRadial", tuneLength = 5, scaled = FALSE)

############################################################################
# END
############################################################################