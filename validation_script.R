#############################################################
#SETTING ENVIRONMENT
#############################################################
#remove all objects and then check

#Instal packages needes for the analysis
lapply(c("ggplot2", "psych", "RCurl", "irr", "nortest", "moments","nFactors","psych","ltm", "GPArotation", "gmodels","sem","eRm","mirt","googlesheets","dplyr","devtools"), library, character.only=T)
############################################################
#IMPORTING DATA
############################################################

#uploading data ---------------------------------------------

data<-read.csv("/home/joao/Dropbox/datasets/neck_pain_dataset.csv")


#data <- repmis::source_DropboxData("parasito_pem.csv",
#                                  "wmajdtrllak6y5d",
#                                  sep = ",",
#                                  header = TRUE)

#detach(data)
#attach(data)


#############################################################
#EXPLORATORY DATA ANALYSIS
#############################################################
#This is a bunch of functions to understand data and find problems
#No need to run this now

#dim(data)
#str (data)
#head(data)
#names(data)
summary(data)#This comand will provide a whole set of descriptive results for each variables
#ad.test() # Anderson-Darling test for normality
#skewness() #Will provide skweness analysis
#kurtosis() - 3 #Will provide kurtosis analysis
#qplot() # histogram plot
#pwr.anova.test(k = , n = , f = , sig.level = , power = )#Power analysis for variance analysis
#boxplot() #will provide a boxplot for the variables to analysis potential outliers

##########################################################################
#OBJECTIVE 2 - Individual validation for each scale
##########################################################################
#
#
######## NECK DISABILLITY INDEX SCALE ####################################

#neckdisability Index dataset
NeckDisabilityIndexOriginal<-with(data,data.frame(Pain,PersonalCare,Lifting,Reading,Headaches,Concentration,Work,Driving,Sleeping,Recreation))

#Excluding NAs
NeckDisabilityIndex<-na.omit(NeckDisabilityIndexOriginal)

#Taxonometric Scale
#MAMBAC(scale(NeckDisabilityIndexNA)[,1:3], Comp.Data = T)
#EFA
#Group of functinos to determine the number os items to be extracted
#Group of functinos to determine the number os items to be extracted
par(mfrow=c(2,2)) #Command to configure the plot area for the scree plot graph
ev <- eigen(cor(NeckDisabilityIndex)) # get eigenvalues - insert the data you want to calculate the scree plot for
ev # Show eigend values
ap <- parallel(subject=nrow(NeckDisabilityIndex),var=ncol(NeckDisabilityIndex),rep=100,cent=.05) #Calculate the acceleration factor
summary(ap)
nS <- nScree(ev$values) #Set up the Scree Plot 
plotnScree(nS) # Plot the ScreePlot Graph

#Function to calculate the KMO values - colocar link par ao gist
kmo<-kmo(NeckDisabilityIndex) #Run the Kmo function for the data you want to calculate
kmo$overall

#Functino to exctract the factor loadings. 
#Arguments are DATA, Number of factors, rotation method. 
#Look here http://goo.gl/kY3ln for different met

#holds of estimations or rotations
fa(NeckDisabilityIndex,2,fm="pa",rotate="promax")
fa(NeckDisabilityIndex,1,fm="pa",rotate="oblimin")

#CFA
# Insert de covariance matrix - CFA (or SEM) is always calculated in relation to a covariance or correlation matrix, here we will create the covariance matrix

#Tryout 1
cfa1fneckdisability <- specifyModel()
F1->Pain,var1,NA
F1->PersonalCare,var2,NA
F1->Lifting,var3,NA
F1->Reading,var4,NA
F1->Headaches,var5,NA
F1->Concentration,var6,NA
F1->Work,var7,NA
F1->Driving,var8,NA
F1->Sleeping,var9,NA
F1->Recreation,var10,NA
F1<->F1,NA,1

#Endofmodel

# Insert de covariance matrix - CFA (or SEM) is always calculated in relation to a covariance or correlation matrix, here we will create the covariance matrix
#cov1frm <- cov(RMorrisFactorModel, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))
cor1frm<-cov(NeckDisabilityIndex)
# Estimate the model
cfa1factorrm<- sem(cfa1fneckdisability, cor1frm, N=394)
summary(cfa1factorrm,fit.indices=c("GFI", "AGFI", "RMSEA", "NFI","NNFI", "CFI", "RNI", "IFI", "SRMR", "AIC", "AICc"))

# Reporta os efeitos das trjetórias do modelo
effects(cfa1factorrm)

# Reporta os coeficientes estandardizados das trajetórias (linhas)
x<-standardizedCoefficients(cfa1factorrm)

#Modification index
modIndices(cfa1factorrm)


#ITEM RESPONSE THEORY
#### USING eRM Package
IRTRolandMorris <- PCM(NeckDisabilityIndex)
diff_index<-thresholds(IRTRolandMorris)
summary(diff_index$threshtable[[1]][,1])
sd(diff_index$threshtable[[1]][,1])/sqrt(length(diff_index$threshtable[[1]][,1]))
plotICC(IRTRolandMorris,item.subset=3,ask=F,empICC=list("raw"),empCI=list(lty="solid"))
plotPImap(IRTRolandMorris, sorted=FALSE)
plotPWmap(IRTRolandMorris)
pp<-person.parameter(IRTRolandMorris)
#lrt<-LRtest(IRTRolandMorris,se=TRUE)
#Waldtest(IRTRolandMorris)
eRm::itemfit(pp)
summary(eRm::itemfit(pp)$i.outfitMSQ)
sd(eRm::itemfit(pp)$i.outfitMSQ)
summary(eRm::itemfit(pp)$i.infitMSQ)
sd(eRm::itemfit(pp)$i.infitMSQ)
#NPtest(IRTRolandMorris,method="T11")
#plotGOF(lrt,conf=list())
#fscores(NeckDisabilityIndex, rotate = "oblimin", Target = NULL, full.scores = FALSE,method = "EAP", quadpts = NULL, response.pattern = NULL,plausible.draws = 0, returnER = FALSE, return.acov = FALSE,mean = NULL, cov = NULL, verbose = TRUE, full.scores.SE = FALSE,theta_lim = c(-6, 6), MI = 0, QMC = FALSE, custom_den = NULL, custom_theta = NULL, min_expected = 1)

#Recoding variables#
neckdisability2<-NULL
neckdisability2$Pain<-car::recode(NeckDisabilityIndex$Pain,"1=0;2=1;3=1;4=1;5=1;6=1")
neckdisability2$PersonalCare<-car::recode(NeckDisabilityIndex$PersonalCare,"1=0;2=1;3=1;4=1;5=1;6=1")
neckdisability2$Lifting<-car::recode(NeckDisabilityIndex$Lifting,"1=0;2=1;3=1;4=1;5=1;6=1")
neckdisability2$Reading<-car::recode(NeckDisabilityIndex$Reading,"1=0;2=1;3=1;4=1;5=1;6=1")
neckdisability2$Headaches<-car::recode(NeckDisabilityIndex$Headaches,"1=0;2=1;3=1;4=1;5=1;6=1")
neckdisability2$Concentration<-car::recode(NeckDisabilityIndex$Concentration,"1=0;2=1;3=1;4=1;5=1;6=1")
neckdisability2$Work<-car::recode(NeckDisabilityIndex$Work,"1=0;2=1;3=1;4=1;5=1;6=1")
neckdisability2$Driving<-car::recode(NeckDisabilityIndex$Driving,"1=0;2=1;3=1;4=1;5=1;6=1")
neckdisability2$Sleeping<-car::recode(NeckDisabilityIndex$Sleeping,"1=0;2=1;3=1;4=1;5=1;6=1")
neckdisability2$Recreation<-car::recode(NeckDisabilityIndex$Recreation,"1=0;2=1;3=1;4=1;5=1;6=1")
neckdisability2<-as.data.frame(neckdisability2)

IRTRolandMorris <- RM(neckdisability2)
plotICC(IRTRolandMorris,item.subset=1:4,ask=F,empICC=list("raw"),empCI=list(lty="solid"))
plotPImap(IRTRolandMorris)
pp<-person.parameter(IRTRolandMorris)
lrt<-LRtest(IRTRolandMorris,se=TRUE)
Waldtest(IRTRolandMorris)
eRm::itemfit(pp)
summary(itemfit(pp)$i.outfitMSQ)
sd(itemfit(pp)$i.outfitMSQ)
summary(itemfit(pp)$i.infitMSQ)
sd(itemfit(pp)$i.infitMSQ)
NPtest(IRTRolandMorris,method="T11")
plotGOF(lrt,conf=list())

#Alpha de Cronbach by ltm package - GIves CI
cronbach.alpha(NeckDisabilityIndex, standardized = TRUE, CI = TRUE, probs = c(0.025, 0.975), B = 1000, na.rm = FALSE)

alpha(NeckDisabilityIndex)

#Composite Reliabilty
# sum(x[1:24,2])^2/(sum(x[1:24,2])^2+sum(x[28:51,2]))
#
#
######## NORTHWICK SCALE ############
#NORTHWICK dataset
NorthwickParkNeckPainQuestionnaireOriginal<-with(data,data.frame(Intensity,Sleep,Pin,Symptons,Carry,Wachting,Housework,Social,Drive))

#Recoding variables to adjust item's values
#Positive direction
#Interfering<-car::recode(Interfering,"1=0;2=1;3=2")

#Excluding NAs
NorthwickParkNeckPainQuestionnaire<-na.omit(NorthwickParkNeckPainQuestionnaireOriginal)
#Taxonometric Scale
#MAMBAC(scale(RolandMorrisOriginal)[,1:3], Comp.Data = T)
#EFA
par(mfrow=c(2,2)) #Command to configure the plot area for the scree plot graph
ev <- eigen(cor(NorthwickParkNeckPainQuestionnaire)) # get eigenvalues - insert the data you want to calculate the scree plot for
ev # Show eigend values
ap <- parallel(subject=nrow(NorthwickParkNeckPainQuestionnaire),var=ncol(NorthwickParkNeckPainQuestionnaire),rep=100,cent=.05) #Calculate the acceleration factor
summary(ap)
nS <- nScree(ev$values) #Set up the Scree Plot 
plotnScree(nS) # Plot the ScreePlot Graph


#Function to calculate the KMO values - colocar link par ao gist
kmo<-kmo(NorthwickParkNeckPainQuestionnaire) #Run the Kmo function for the data you want to calculate
kmo$overall

#Functino to exctract the factor loadings. 
#Arguments are DATA, Number of factors, rotation method. 
#Look here http://goo.gl/kY3ln for different methods of estimations or rotations
fa(NorthwickParkNeckPainQuestionnaire,2,fm="pa",rotate="promax")
fa(NorthwickParkNeckPainQuestionnaire,1,fm="pa",rotate="promax")
#CFA
# Insert de covariance matrix - CFA (or SEM) is always calculated in relation to a covariance or correlation matrix, here we will create the covariance matrix

#Tryout 1
cfa1fNorthwick <- specifyModel()
F1->Intensity,var1,NA
F1->Sleep,var2,NA
F1->Pin,var3,NA
F1->Symptons,var4,NA
F1->Carry,var5,NA
F1->Wachting,var6,NA
F1->Housework,var7,NA
F1->Social,var8,NA
F1->Drive,var9,NA
F1<->F1,NA,1

# Insert de covariance matrix - CFA (or SEM) is always calculated in relation to a covariance or correlation matrix, here we will create the covariance matrix
cov1fNorthwick <- cov(NorthwickParkNeckPainQuestionnaire, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))

# Estimate the model
cfa1factorrm<- sem(cfa1fNorthwick, cov1fNorthwick, N=394)
summary(cfa1factorrm,fit.indices=c("GFI", "AGFI", "RMSEA", "NFI", "NNFI", "CFI", "RNI", "IFI", "SRMR", "AIC", "AICc"))

# Reporta os efeitos das trjetórias do modelo
effects(cfa1factorrm)

# Reporta os coeficientes estandardizados das trajetórias (linhas)
x<-standardizedCoefficients(cfa1factorrm)

#Modification index
modIndices(cfa1factorrm)

#ITEM RESPONSE THEORY
#### USING eRM Package
IRTRolandMorris <- PCM(NorthwickParkNeckPainQuestionnaire)
diff_index<-thresholds(IRTRolandMorris)
summary(diff_index$threshtable[[1]][,1])
sd(diff_index$threshtable[[1]][,1])/sqrt(length(diff_index$threshtable[[1]][,1]))
plotICC(IRTRolandMorris,item.subset=6,ask=F,empICC=list("raw"),empCI=list(lty="solid"))
plotPImap(IRTRolandMorris, sorted=FALSE)
plotPWmap(IRTRolandMorris)
pp<-person.parameter(IRTRolandMorris)
#lrt<-LRtest(IRTRolandMorris,se=TRUE)
#Waldtest(IRTRolandMorris)
eRm::itemfit(pp)
summary(eRm::itemfit(pp)$i.outfitMSQ)
sd(eRm::itemfit(pp)$i.outfitMSQ)
summary(eRm::itemfit(pp)$i.infitMSQ)
sd(eRm::itemfit(pp)$i.infitMSQ)
#NPtest(IRTRolandMorris,method="T11")
#plotGOF(lrt,conf=list())
#fscores(NeckDisabilityIndex, rotate = "oblimin", Target = NULL, full.scores = FALSE,method = "EAP", quadpts = NULL, response.pattern = NULL,plausible.draws = 0, returnER = FALSE, return.acov = FALSE,mean = NULL, cov = NULL, verbose = TRUE, full.scores.SE = FALSE,theta_lim = c(-6, 6), MI = 0, QMC = FALSE, custom_den = NULL, custom_theta = NULL, min_expected = 1)

#Alpha de Cronbach by ltm package - GIves CI
cronbach.alpha(NorthwickParkNeckPainQuestionnaire, standardized = TRUE, CI = TRUE, probs = c(0.025, 0.975), B = 1000, na.rm = FALSE)

alpha(NorthwickParkNeckPainQuestionnaire)

#Composite Reliabilty
# sum(x[1:24,2])^2/(sum(x[1:24,2])^2+sum(x[28:51,2]))

#
#
######## COPENHAGEN SCALE ############
#Copenhagen dataset
CopenhagenNeckFunctionalDisabilityScaleOriginal<-with(data,data.frame(Interfering,DailyActivities,ManageHelp,ClothesMorning,BrushTeeth,SpendTime,LiftingObjects,ReadingActivity,BotheredHeadaches,Ability,Leisure,Bed,Emotional,Shoes,GiveUp))

CopenhagenNeckFunctionalDisabilityScale<-na.omit(CopenhagenNeckFunctionalDisabilityScaleOriginal)
#Taxonometric Scale
#MAMBAC(scale(RolandMorrisOriginal)[,1:3], Comp.Data = T)

#EFA
#Group of functinos to determine the number os items to be extracted
par(mfrow=c(2,2)) #Command to configure the plot area for the scree plot graph
ev <- eigen(cor(CopenhagenNeckFunctionalDisabilityScale)) # get eigenvalues - insert the data you want to calculate the scree plot for
ev # Show eigend values
ap <- parallel(subject=nrow(CopenhagenNeckFunctionalDisabilityScale),var=ncol(CopenhagenNeckFunctionalDisabilityScale),rep=100,cent=.05) #Calculate the acceleration factor
summary(ap)
nS <- nScree(ev$values) #Set up the Scree Plot 
plotnScree(nS) # Plot the ScreePlot Graph

#Function to calculate the KMO values - colocar link par ao gist
kmo<-kmo(CopenhagenNeckFunctionalDisabilityScale) #Run the Kmo function for the data you want to calculate
kmo$overall

#Functino to exctract the factor loadings. 
#Arguments are DATA, Number of factors, rotation method. 
#Look here http://goo.gl/kY3ln for different methods of estimations or rotations
fa(CopenhagenNeckFunctionalDisabilityScale,4,fm="pa",rotate="promax")
fa(CopenhagenNeckFunctionalDisabilityScale,3,fm="pa",rotate="promax")
fa(CopenhagenNeckFunctionalDisabilityScale,2,fm="pa",rotate="promax")
fa(CopenhagenNeckFunctionalDisabilityScale,1,fm="pa",rotate="promax")


#CFA
# Insert de covariance matrix - CFA (or SEM) is always calculated in relation to a covariance or correlation matrix, here we will create the covariance matrix

#Tryout 1
CopenhagenFactorModel<-remove.vars(CopenhagenNeckFunctionalDisabilityScale, names=c("BotheredHeadaches"), info=TRUE)


cfa1fCopenhagen <- specifyModel()
F1->Interfering,var1,NA
F1->DailyActivities,var2,NA
F1->ManageHelp,var3,NA
F1->ClothesMorning,var4,NA
F1->BrushTeeth,var5,NA
F1->SpendTime,var6,NA
F1->LiftingObjects,var7,NA
F1->ReadingActivity,var8,NA
F1->Ability,var10,NA
F1->Leisure,var11,NA
F1->Bed,var12,NA
F1->Emotional,var13,NA
F1->Shoes,var14,NA
F1->GiveUp,var15,NA      
F1<->F1,NA,1
DailyActivities<->Interfering,erro1,NA
# Insert de covariance matrix - CFA (or SEM) is always calculated in relation to a covariance or correlation matrix, here we will create the covariance matrix
cov1fCopenhagen <- cov(CopenhagenFactorModel, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))

# Estimate the model
cfa1factorrm<- sem(cfa1fCopenhagen, cov1fCopenhagen, N=394)
summary(cfa1factorrm,fit.indices=c("GFI", "AGFI", "RMSEA", "NFI", "NNFI", "CFI", "RNI", "IFI", "SRMR", "AIC", "AICc"))

# Reporta os efeitos das trjetórias do modelo
effects(cfa1factorrm)

# Reporta os coeficientes estandardizados das trajetórias (linhas)
x<-standardizedCoefficients(cfa1factorrm)

#Modification index
modIndices(cfa1factorrm)

#Tryout 2 - 2 factor model
CopenhagenFactorModel<-remove.vars(CopenhagenNeckFunctionalDisabilityScale, names=c("BotheredHeadaches"), info=TRUE)


cfa1fCopenhagen <- specifyModel()
F1->Interfering,var1,NA
F1->DailyActivities,var2,NA
F1->ManageHelp,var3,NA
F1->ClothesMorning,var4,NA
F1->BrushTeeth,var5,NA
F2->SpendTime,var6,NA
F2->LiftingObjects,var7,NA
F2->ReadingActivity,var8,NA
F2->Ability,var10,NA
F2->Leisure,var11,NA
F2->Bed,var12,NA
F2->Emotional,var13,NA
F2->Shoes,var14,NA
F2->GiveUp,var15,NA      
F1<->F1,NA,1
F2<->F2,NA,1

# Insert de covariance matrix - CFA (or SEM) is always calculated in relation to a covariance or correlation matrix, here we will create the covariance matrix
cov1fCopenhagen <- cov(CopenhagenFactorModel, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))

# Estimate the model
cfa1factorrm<- sem(cfa1fCopenhagen, cov1fCopenhagen, N=394)
summary(cfa1factorrm,fit.indices=c("GFI", "AGFI", "RMSEA", "NFI", "NNFI", "CFI", "RNI", "IFI", "SRMR", "AIC", "AICc"))

# Reporta os efeitos das trjetórias do modelo
effects(cfa1factorrm)

# Reporta os coeficientes estandardizados das trajetórias (linhas)
x<-standardizedCoefficients(cfa1factorrm)

#Modification index
modIndices(cfa1factorrm)


#ITEM RESPONSE THEORY
#### USING eRM Package
IRTRolandMorris <- PCM(CopenhagenNeckFunctionalDisabilityScale)
diff_index<-thresholds(IRTRolandMorris)
summary(diff_index$threshtable[[1]][,1])
sd(diff_index$threshtable[[1]][,1])/sqrt(length(diff_index$threshtable[[1]][,1]))
plotICC(IRTRolandMorris,item.subset=1,ask=F,empICC=list("raw"),empCI=list(lty="solid"))
plotPImap(IRTRolandMorris, sorted=FALSE)
plotPWmap(IRTRolandMorris)
pp<-person.parameter(IRTRolandMorris)
#lrt<-LRtest(IRTRolandMorris,se=TRUE)
#Waldtest(IRTRolandMorris)
eRm::itemfit(pp)
summary(eRm::itemfit(pp)$i.outfitMSQ)
sd(eRm::itemfit(pp)$i.outfitMSQ)
summary(eRm::itemfit(pp)$i.infitMSQ)
sd(eRm::itemfit(pp)$i.infitMSQ)
#NPtest(IRTRolandMorris,method="T11")
#plotGOF(lrt,conf=list())
#fscores(NeckDisabilityIndex, rotate = "oblimin", Target = NULL, full.scores = FALSE,method = "EAP", quadpts = NULL, response.pattern = NULL,plausible.draws = 0, returnER = FALSE, return.acov = FALSE,mean = NULL, cov = NULL, verbose = TRUE, full.scores.SE = FALSE,theta_lim = c(-6, 6), MI = 0, QMC = FALSE, custom_den = NULL, custom_theta = NULL, min_expected = 1)

#Alpha de Cronbach by ltm package - GIves CI
cronbach.alpha(CopenhagenNeckFunctionalDisabilityScale[,-5], standardized = TRUE, CI = TRUE, probs = c(0.025, 0.975), B = 1000, na.rm = FALSE)

alpha(CopenhagenNeckFunctionalDisabilityScale[,-5])

#Composite Reliabilty
# sum(x[1:24,2])^2/(sum(x[1:24,2])^2+sum(x[28:51,2]))

#
#
######## NECK BOURNEMOUTH SCALE ############
#NeckBoutnemouth dataset
NeckBournemouthQuestionnaireOriginal<-with(data,data.frame(RateNeckPain,InterferedDailyActivities,Recreational,Anxious,Depressed,YourWork,Control))

#Excluding NAs
NeckBournemouthQuestionnaire<-na.omit(NeckBournemouthQuestionnaireOriginal)

#Taxonometric Scale
#MAMBAC(scale(RolandMorrisOriginal)[,1:3], Comp.Data = T)

#EFA
#Group of functinos to determine the number os items to be extracted
par(mfrow=c(2,2)) #Command to configure the plot area for the scree plot graph
ev <- eigen(cor(NeckBournemouthQuestionnaire)) # get eigenvalues - insert the data you want to calculate the scree plot for
ev # Show eigend values
ap <- parallel(subject=nrow(NeckBournemouthQuestionnaire),var=ncol(NeckBournemouthQuestionnaire),rep=100,cent=.05) #Calculate the acceleration factor
summary(ap)
nS <- nScree(ev$values) #Set up the Scree Plot 
plotnScree(nS) # Plot the ScreePlot Graph

#Function to calculate the KMO values - colocar link par ao gist
kmo<-kmo(NeckBournemouthQuestionnaire) #Run the Kmo function for the data you want to calculate
kmo$overall

#Functino to exctract the factor loadings. 
#Arguments are DATA, Number of factors, rotation method. 
#Look here http://goo.gl/kY3ln for different methods of estimations or rotations
fa(NeckBournemouthQuestionnaire,4,fm="pa",rotate="promax")
fa(NeckBournemouthQuestionnaire,3,fm="pa",rotate="promax")
fa(NeckBournemouthQuestionnaire,2,fm="pa",rotate="promax")
fa(NeckBournemouthQuestionnaire,1,fm="pa",rotate="promax")

#CFA
# Insert de covariance matrix - CFA (or SEM) is always calculated in relation to a covariance or correlation matrix, here we will create the covariance matrix

#Tryout 1
cfa1fNeckBournemouth <- specifyModel()
F1->RateNeckPain,var1,NA
F1->InterferedDailyActivities,var2,NA
F1->Recreational,var3,NA
F1->Anxious,var4,NA
F1->Depressed,var5,NA
F1->YourWork,var6,NA
F1->Control,var7,NA
F1<->F1,NA,1
Depressed<->Anxious,erro1,NA

# Insert de covariance matrix - CFA (or SEM) is always calculated in relation to a covariance or correlation matrix, here we will create the covariance matrix
cov1fNeckBournemouth <- cov(NeckBournemouthQuestionnaire, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))

# Estimate the model
cfa1factorrm<- sem(cfa1fNeckBournemouth, cov1fNeckBournemouth, N=394)
summary(cfa1factorrm,fit.indices=c("GFI", "AGFI", "RMSEA", "NFI","NNFI", "CFI", "RNI", "IFI", "SRMR", "AIC", "AICc"))

# Reporta os efeitos das trjetórias do modelo
effects(cfa1factorrm)

# Reporta os coeficientes estandardizados das trajetórias (linhas)
x<-standardizedCoefficients(cfa1factorrm)

#Modification index
modIndices(cfa1factorrm)

#ITEM RESPONSE THEORY
#### USING eRM Package
IRTRolandMorris <- PCM(NeckBournemouthQuestionnaire)
diff_index<-thresholds(IRTRolandMorris)
summary(diff_index$threshtable[[1]][,1])
sd(diff_index$threshtable[[1]][,1])/sqrt(length(diff_index$threshtable[[1]][,1]))
plotICC(IRTRolandMorris,item.subset=1,ask=F,empICC=list("raw"),empCI=list(lty="solid"))
plotPImap(IRTRolandMorris, sorted=FALSE)
plotPWmap(IRTRolandMorris)
pp<-person.parameter(IRTRolandMorris)
#lrt<-LRtest(IRTRolandMorris,se=TRUE)
#Waldtest(IRTRolandMorris)
eRm::itemfit(pp)
summary(eRm::itemfit(pp)$i.outfitMSQ)
sd(eRm::itemfit(pp)$i.outfitMSQ)
summary(eRm::itemfit(pp)$i.infitMSQ)
sd(eRm::itemfit(pp)$i.infitMSQ)
#NPtest(IRTRolandMorris,method="T11")
#plotGOF(lrt,conf=list())
#fscores(NeckDisabilityIndex, rotate = "oblimin", Target = NULL, full.scores = FALSE,method = "EAP", quadpts = NULL, response.pattern = NULL,plausible.draws = 0, returnER = FALSE, return.acov = FALSE,mean = NULL, cov = NULL, verbose = TRUE, full.scores.SE = FALSE,theta_lim = c(-6, 6), MI = 0, QMC = FALSE, custom_den = NULL, custom_theta = NULL, min_expected = 1)

#Alpha de Cronbach by ltm package - GIves CI
cronbach.alpha(NeckBournemouthQuestionnaire, standardized = TRUE, CI = TRUE, probs = c(0.025, 0.975), B = 1000, na.rm = FALSE)

alpha(NeckBournemouthQuestionnaire)

#Composite Reliabilty
# sum(x[1:24,2])^2/(sum(x[1:24,2])^2+sum(x[28:51,2]))


##########################################################################
#OBJECTIVE 3 - EXTERNAL VALIDITY
##########################################################################
#COMORBIDITY SCALE
  HeartDisease<-car::recode(HeartDisease,"1=1;else=0"),
  TreatHeart<-car::recode(TreatHeart,"1=1;else=0"),
  ActHeart<-car::recode(ActHeart,"1=1;else=0"),
  HBPressure<-car::recode(HBPressure,"1=1;else=0"),
  TreatHB<-car::recode(TreatHB,"1=1;else=0"),
  ActHB<-car::recode(ActHB,"1=1;else=0"),
  LungDisease<-car::recode(LungDisease,"1=1;else=0"),
  TreatLung<-car::recode(TreatLung,"1=1;else=0"),
  ActLung<-car::recode(ActLung,"1=1;else=0"),
  Diabetes<-car::recode(Diabetes,"1=1;else=0"),
  TreatDiabetes<-car::recode(TreatDiabetes,"1=1;else=0"),
  ActDiabetes<-car::recode(ActDiabetes,"1=1;else=0"),
  Ulcer<-car::recode(Ulcer,"1=1;else=0"),
  TreatUlcer<-car::recode(TreatUlcer,"1=1;else=0"),
  ActUlcer<-car::recode(ActUlcer,"1=1;else=0"),
  Kidney<-car::recode(Kidney,"1=1;else=0"),
  TreatKidney<-car::recode(TreatKidney,"1=1;else=0"),
  ActKidney<-car::recode(ActKidney,"1=1;else=0"),
  LiverDisease<-car::recode(LiverDisease,"1=1;else=0"),
  TreatLiver<-car::recode(TreatLiver,"1=1;else=0"),
  ActLiver<-car::recode(ActLiver,"1=1;else=0"),
  Anemia<-car::recode(Anemia,"1=1;else=0"),
  TreatAnemia<-car::recode(TreatAnemia,"1=1;else=0"),
  ActAnemia<-car::recode(ActAnemia,"1=1;else=0"),
  Cancer<-car::recode(Cancer,"1=1;else=0"),
  TreatCancer<-car::recode(TreatCancer,"1=1;else=0"),
  ActCancer<-car::recode(ActCancer,"1=1;else=0"),
  Depression<-car::recode(Depression,"1=1;else=0"),
  TreatDepression<-car::recode(TreatDepression,"1=1;else=0"),
  ActDepression<-car::recode(ActDepression,"1=1;else=0"),
  Osteoarthritis<-car::recode(Osteoarthritis,"1=1;else=0"),
  TreatOsteoarthritis<-car::recode(TreatOsteoarthritis,"1=1;else=0"),
  ActOsteoarthritis<-car::recode(ActOsteoarthritis,"1=1;else=0"),
  BackPain<-car::recode(BackPain,"1=1;else=0"),
  TreatBack<-car::recode(TreatBack,"1=1;else=0"),
  ActBack<-car::recode(ActBack,"1=1;else=0"),
  NeckPain<-car::recode(NeckPain,"1=1;else=0"),
  TreatNeck<-car::recode(TreatNeck,"1=1;else=0"),
  ActNeck<-car::recode(ActNeck,"1=1;else=0"),
  Rheumatoid<-car::recode(Rheumatoid,"1=1;else=0"),
  TreatRheumatoid<-car::recode(TreatRheumatoid,"1=1;else=0"),
  ActRheumatoid<-car::recode(ActRheumatoid,"1=1;else=0")
  
SCQ<-data.frame(HeartDisease,TreatHeart,ActHeart,HBPressure,TreatHB,ActHB,LungDisease,
                TreatLung,ActLung,Diabetes,TreatDiabetes,ActDiabetes,Ulcer,TreatUlcer,
                ActUlcer,Kidney,TreatKidney,ActKidney,LiverDisease,TreatLiver,ActLiver,
                Anemia,ActAnemia,Cancer,TreatCancer,ActCancer,Depression,TreatDepression,
                ActDepression,Osteoarthritis,TreatOsteoarthritis,ActOsteoarthritis,BackPain,
                TreatBack,ActBack,NeckPain,TreatNeck, ActNeck,  Rheumatoid,TreatRheumatoid,
                ActRheumatoid)

#SEM
#Defining dataframe with the data for the SEM model
PainSEM<-na.omit(data.frame(NeckDisabilityIndexOriginal,NorthwickParkNeckPainQuestionnaireOriginal,
                            CopenhagenNeckFunctionalDisabilityScaleOriginal,NeckBournemouthQuestionnaireOriginal))
PainSEM<-na.omit(PainSEM)

#Specifying SEM model
#Padrão:
#1. Copiar todos as linhas dos modelos criadas dna seção da CFA
#2. Modificar F1 pelo nome da escala
#3. Criar linhas de covariância (correlação entre as escalas)
#4. Criar variavel latente PAIN e definir modelo (cada escala predizendo "->" PAIN)
#5. Mudar nome das variáveis de cada escala (todas estão com a mesma sequencia var1,var2,var3)
#6. Mudar linha de comando da primeira variavel de cada escala de "var1,NA" para "NA,1"
#7. Mudar linha de comando do erro de cada escala (F1<->F1, ou neckdisability<->neckdisability)
# de "NA,1" para "err1,NA". O numero apos o "err" segue uma sequencia ordinal.
semmodel <- specifyModel()# Type these values that specify the model's relations (just use de Ctrl+R over each relation).

neckdisability->Pain,NA,1
neckdisability->PersonalCare,var2,NA
neckdisability->Lifting,var3,NA
neckdisability->Reading,var4,NA
neckdisability->Headaches,var5,NA
neckdisability->Concentration,var6,NA
neckdisability->Work,var7,NA
neckdisability->Driving,var8,NA
neckdisability->Sleeping,var9,NA
neckdisability->Recreation,var10,NA
neckdisability<->neckdisability,err1,NA
Northwick->Intensity,NA,1
Northwick->Sleep,var11,NA
Northwick->Pin,var12,NA
Northwick->Symptons,var13,NA
Northwick->Carry,var14,NA
Northwick->Wachting,var15,NA
Northwick->Housework,var17,NA
Northwick->Social,var18,NA
Northwick->Drive,var19,NA
Northwick<->Northwick,err2,NA
Copenhagen->Interfering,NA,1
Copenhagen->DailyActivities,var22,NA
Copenhagen->ManageHelp,var23,NA
Copenhagen->ClothesMorning,var24,NA
Copenhagen->BrushTeeth,var25,NA
Copenhagen->SpendTime,var26,NA
Copenhagen->LiftingObjects,var27,NA
Copenhagen->ReadingActivity,var28,NA
Copenhagen->Ability,var20,NA
Copenhagen->Leisure,var21,NA
Copenhagen->Bed,var32,NA
Copenhagen->Emotional,var33,NA
Copenhagen->Shoes,var34,NA
Copenhagen->GiveUp,var35,NA      
Copenhagen<->Copenhagen,err3,NA
NeckBournemouth->RateNeckPain,NA,1
NeckBournemouth->InterferedDailyActivities,var42,NA
NeckBournemouth->Recreational,var43,NA
NeckBournemouth->Anxious,var44,NA
NeckBournemouth->Depressed,var45,NA
NeckBournemouth->YourWork,var46,NA
NeckBournemouth->Control,var47,NA
NeckBournemouth<->NeckBournemouth,err4,NA

NeckBournemouth<->Copenhagen,NA,1
NeckBournemouth<->Northwick,NA,1
NeckBournemouth<->neckdisability,NA,1
Copenhagen<->Northwick,NA,1
Copenhagen<->neckdisability,NA,1
Northwick<->neckdisability,NA,1

NeckBournemouth->Pain,2order1,NA
Copenhagen->Pain,2order2,NA
Northwick->Pain,2order3,NA
neckdisability->Pain,NA,1

Pain<->Pain,err5,NA

#ANA, replicar até aqui apenas, os comandos abaixo eu fiz para ajustar o modelo
LiftingObjects<->SpendTime,erro1,NA
ReadingActivity<->LiftingObjects,erro2,NA
ReadingActivity<->SpendTime,erro3,NA
Drive<->Driving,erro4,NA
Copenhagen<->Interfering,laterro1,NA
Carry<->Lifting,erro5,NA
Intensity<->Pain,erro6,NA
#end of the model

# Insert de covariance matrix
cov <- cov(PainSEM, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))
cor <- cor(PainSEM)

# Estimate de model (Here is where we have been finding difficulties)
sem <- sem(semmodel, cov, N=615 )
sem2 <- sem(semmodel, cor, N=615 )
summary(sem) # Copiar e colar no texto
effects(semPMC)
standardizedCoefficients(sem) #Copiar e colar texto
# calcula ?ndices de modifica??o (testes de pontua??o) e altera??es de par?metros
# estimados para os par?metros fixos e limitados em um modelo de equa??es estruturais
modIndices(sem)

#QGRAPH
x<- cor(PainSEM,method="spearman")
names<-c(c(1:length(NeckDisabilityIndex)),c(1:length(NorthwickParkNeckPainQuestionnaireOriginal)),c(1:length(CopenhagenNeckFunctionalDisabilityScaleOriginal)),c(1:length(NeckBournemouthQuestionnaireOriginal)))
color<-c(rep("steelblue",length(NeckDisabilityIndex)),rep("gold",length(NorthwickParkNeckPainQuestionnaireOriginal)),rep("lightgreen",length(CopenhagenNeckFunctionalDisabilityScaleOriginal)),rep("red2",length(NeckBournemouthQuestionnaireOriginal)))

qgraph(x,minimum=0.20,cut=0.60,layout="spring",color=color,labels=names)

#IRT with all the data
#ITEM RESPONSE THEORY
#ploytomous
irt_model<-mirt(PainSEM,1,itemtype="Rasch")
#RSM(data)
thresholds(irt_model)
plotPImap(irt_model)
pp<-person.parameter(irt_model)
summary(itemfit(pp)$i.outfitMSQ)
sd(itemfit(pp)$i.outfitMSQ)
summary(itemfit(pp)$i.infitMSQ)
sd(itemfit(pp)$i.infitMSQ)
eRm::personfit(pp)
###########################################################################
#OBJECTIVE 5 - External validity comparison of normalized and non-normalized scales
#############################################################################
#Calculating total scores for each test

NeckDisabilitySUM<-rowSums(NeckDisabilityIndexOriginal) #Sum the number in a row for a data frame
summary(NeckDisabilitySUM)
NorthwickParkSUM<-rowSums(NorthwickParkNeckPainQuestionnaireOriginal)
summary(NorthwickParkSUM)
CopenhagenSUM<-rowSums(CopenhagenNeckFunctionalDisabilityScaleOriginal)
summary(CopenhagenSUM)
BournemouthSUM<-rowSums(NeckBournemouthQuestionnaireOriginal)
summary(BournemouthSUM)
#Normalization to a scale from 0 to 100
#Formula 100*((RESULTADO - Valor minimo possivel)/(Valor maximo possivel - Valor minimo possivel))
NeckDisabilityIndexScore<-100*((NeckDisabilitySUM - 10)/(60-10))
summary(NeckDisabilityIndexScore)
NorthwickParkNeckPainQuestionnaireScore<-100*((NorthwickParkSUM - 9)/(40-9))
summary(NorthwickParkNeckPainQuestionnaireScore)
CopenhagenNeckFunctionalDisabilityScaleScore<-100*((CopenhagenSUM - 0)/(30-0))
summary(CopenhagenNeckFunctionalDisabilityScaleScore)
NeckBournemouthQuestionnaireScore<-100*((BournemouthSUM - 0)/(70-0))
summary(NeckBournemouthQuestionnaireScore)

TestsScores<-c(NeckDisabilityIndexScore,NorthwickParkNeckPainQuestionnaireScore,
               CopenhagenNeckFunctionalDisabilityScaleScore,
               NeckBournemouthQuestionnaireScore)


##Transforming to Rasch Measures
ndi_rasch<-car::recode(NeckDisabilitySUM,"9=1;10=3;11=5;12=9;13=20;14=20;15=26;16=32;17=39;18=46;19=49;20=57;21=63;22=63;23=63;24=63;25=80;26=84;27=87;28=90;29=90;30=93;31-94;32=94;33=96;34=96;35=96;36=99;37=99;38=99;39=99;40=100")
npq_rasch<-car::recode(NorthwickParkSUM,"0=1;1=1;2=2;3=4;4=5;5=7;6=9;7=11;8=14;9=17;10=19;11=21;12=23;13=25;14=27;15=30;16=32;17=35;18=37;19=39;20=43;21=46;22=50;23=54;24=60;25=65;26=69;27=73;28=77;29=81;30=83;31=86;32=87;33=89;34=92;35=93;36=94;37=96;38=97;39=97;40=99")
wd_rasch<-car::recode(WaddellSUM,"0=3;1=10;2=19;3=26;4=36;5=51;6=64;7=75;8=84;9=94")
bp_rasch<-car::recode(BackPainSUM,"0=1;1=1;2=2;3=3;4=4;5=5;6=6;7=9;8=12;9=16;10=20;11=25;12=32;13=39;14=45;15=50;16=55;17=59;18=63;19=67;20=71;21=75;22=80;23=85;24=94")


#The idea here is to create a data frame with the final results of al the tests to be plotted together with the bpxplot.
NeckDisabilityname<-c(rep("NeckDisabilityIndex",615))
NeckDisability<-data.frame(NeckDisabilitySUM,NeckDisabilityname)
#summary(NeckDisability)
NorthwickParkname<-c(rep("NorthwickParkNeckPain",615))
NorthwickPark<-data.frame(NorthwickParkSUM,NorthwickParkname)
#summary(NorthwickPark)
Copenhagenname<-c(rep("CopenhagenNeckFunctionalDisabilityScale",615))
Copenhagen<-data.frame(CopenhagenSUM,Copenhagenname)
#summary(Copenhagen)
Bournemouthname<-c(rep("NeckBournemouth",615))
Bournemouth<-data.frame(BournemouthSUM,Bournemouthname)
#summary(Bournemouth)

TestsNames<-c(NeckDisabilityname,NorthwickParkname,
              Copenhagenname,Bournemouthname)


#Organizing data to plot 
boxplot<-data.frame(TestsScores,TestsNames)        
#Create a boxplotfor all scales's the total scores
ggplot(boxplot, aes(TestsNames,TestsScores)) + geom_boxplot()
        
#rowMeans() #Extract the mean of the numbers in a row for a data frame
#colSums()  #Sum the number in a column for a data frame
#colMeans() #Extract the mean of the numbers in a column for a data frame
   


#############################################################################
#FIGURE 6. Correlation with SCQ
#############################################################################
Scatterplot<-data.frame(TestsScores,TestsNames,SCQSUM) 

qplot(TestsScores, SCQSUM, data=Scatterplot, colour=TestsNames) + theme_bw() + 
  geom_jitter() 

#Comparison between Comorbidity groups
TestsScores<-c(NeckDisabilityIndexScore,NorthwickParkNeckPainQuestionnaireScore,CopenhagenNeckFunctionalDisabilityScaleScore,
               NeckBournemouthQuestionnaireScore)
DepressionIndex<-c(Depression,Depression,Depression,Depression)
OsteoarthritisIndex<-c(Osteoarthritis,Osteoarthritis,Osteoarthritis,Osteoarthritis)
RheumatoidIndex<-c(Rheumatoid,Rheumatoid,Rheumatoid,Rheumatoid)
TestsNames<-c(NeckDisabilityname,NorthwickParkname,
              Copenhagenname,Bournemouthname)
Scores<-c(TestsScores,TestsScores,TestsScores)
Groups<-c(DepressionIndex,OsteoarthritisIndex,RheumatoidIndex)
Groups<-car::recode(Groups,"1='sim';0='não'")
Tests<-c(TestsNames,TestsNames,TestsNames) 
Comorbidities<-c(rep("Depression",2460),rep("Osteoarthritis",2460),rep("Rheumatoid",2460))
scqcomparison<-data.frame(Scores,Groups,Tests,Comorbidities)

qplot(Groups, Scores, data=scqcomparison, geom = "boxplot", fill=Groups) + 
  facet_grid(Comorbidities~ . ~ Tests) +
  xlab("") + ylab ("")+ theme(legend.position = "none")

#Test for comparison between comorbidity groups
ad.test(NeckDisabilityIndexScore)
ad.test(NorthwickParkNeckPainQuestionnaireScore)
ad.test(CopenhagenNeckFunctionalDisabilityScaleScore)
ad.test(NeckBournemouthQuestionnaireScore)

wilcox.test(NeckDisabilityIndexScore~data$Depression,paired=FALSE)
wilcox.test(NeckDisabilityIndexScore~data$Osteoarthritis,paired=FALSE)
wilcox.test(NeckDisabilityIndexScore~data$Rheumatoid,paired=FALSE)
wilcox.test(NorthwickParkNeckPainQuestionnaireScore~data$Depression,paired=FALSE)
wilcox.test(NorthwickParkNeckPainQuestionnaireScore~data$Osteoarthritis,paired=FALSE)
wilcox.test(NorthwickParkNeckPainQuestionnaireScore~data$Rheumatoid,paired=FALSE)
wilcox.test(CopenhagenNeckFunctionalDisabilityScaleScore~data$Depression,paired=FALSE)
wilcox.test(CopenhagenNeckFunctionalDisabilityScaleScore~data$Osteoarthritis,paired=FALSE)
wilcox.test(CopenhagenNeckFunctionalDisabilityScaleScore~data$Rheumatoid,paired=FALSE)
wilcox.test(NeckBournemouthQuestionnaireScore~data$Depression,paired=FALSE)
wilcox.test(NeckBournemouthQuestionnaireScore~data$Osteoarthritis,paired=FALSE)
wilcox.test(NeckBournemouthQuestionnaireScore~data$Rheumatoid,paired=FALSE)

#
#############################################################################
#CAT
#############################################################################
install.packages("catR")
require(catR)
c<-coef(irt_model)
itemBank <- cbind(c[,2], c[,1], 0, 1)
catBank<-createItemBank(irt_model, model="2pl")
catBank
catBank$itemPar
plot(catBank$infoTab[,1])
plot(my2pl, type = "IIC", items=1)

items_administered<-c(4)
responses<-c(1)
it<-itemBank[items_administered, 1:4,drop=F ]
theta<-thetaEst(it, responses)
q<-nextItem(catBank, theta,out=items_administered)
q$item