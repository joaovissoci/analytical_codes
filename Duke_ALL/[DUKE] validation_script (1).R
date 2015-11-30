##########################################################################
#SETTING ENVIRONMENT
##########################################################################
#remove all objects and then check

#Instal packages needes for the analysis
lapply(c("ggplot2", "psych", "RCurl", "irr", "nortest", "moments","nFactors","psych","ltm", "GPArotation", "gmodels","sem","eRm","mirt","googlesheets","dplyr","devtools"), library, character.only=T)
############################################################
#IMPORTING DATA
############################################################

#uploading data ------------------------------------------------------------------------
sheet <- gs_title("back_pain_dataset")
#gap_url <- "https://docs.google.com/spreadsheets/d/1NWiYzkW2KxQIjF0eNlkMcLAj8cDsFVWxS6wrMcZwA5A/edit?usp=sharing"
#gap <- gap_url %>% gs_url()
data<-gs_read(sheet,ws = "Lumbar_Pain")
data<-as.data.frame(data)
#data <- read.csv("/Users/apbonilauri/Google Drive/IOT/MTurk Low Back Pain/BackPain_mturk/Data Set MTurk - Back Pain - Lumbar_Pain.csv", header=T)

#detach(data)
attach(data)

##########################################################################
#EXPLORATORY DATA ANALYSIS
##########################################################################

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
#DESCRIPTIVES
##########################################################################

##########################################################################
#OBJECTIVE 2 - Individual validation for each scale
##########################################################################
#
#
######## ROLAND MORRIS PAIN SCALE ############

#Recoding Variables
Home<-car::recode(Home,"2=0;1=1")
Position<-car::recode(Position,"2=0;1=1")
Walk<-car::recode(Walk,"2=0;1=1")
Jobs<-car::recode(Jobs,"2=0;1=1")
Handrail<-car::recode(Handrail,"2=0;1=1")
Liedown<-car::recode(Liedown,"2=0;1=1")
Holdon<-car::recode(Holdon,"2=0;1=1")
Otherpeople<-car::recode(Otherpeople,"2=0;1=1")
Dressed<-car::recode(Dressed,"2=0;1=1")
Periods<-car::recode(Periods,"2=0;1=1")
Bend<-car::recode(Bend,"2=0;1=1")
Getout<-car::recode(Getout,"2=0;1=1")
Painful<-car::recode(Painful,"2=0;1=1")
Turnover<-car::recode(Turnover,"2=0;1=1")
Appetite<-car::recode(Appetite,"2=0;1=1")
Socks<-car::recode(Socks,"2=0;1=1")
Distances<-car::recode(Distances,"2=0;1=1")
Lesswell<-car::recode(Lesswell,"2=0;1=1")
Someone<-car::recode(Someone,"2=0;1=1")
Sitdown<-car::recode(Sitdown,"2=0;1=1")
Heavy<-car::recode(Heavy,"2=0;1=1")
Irritable<-car::recode(Irritable,"2=0;1=1")
Upstairs<-car::recode(Upstairs,"2=0;1=1")
Bed<-car::recode(Bed,"2=0;1=1")
RolandMorrisOriginal<-data.frame(Home,Position,Walk,Jobs,Handrail,Liedown,Holdon,Otherpeople,
                                 Dressed,Periods,Bend,Getout,Painful,Turnover,Appetite,Socks,
                                 Distances,Lesswell,Someone,Sitdown,Heavy,Irritable,Upstairs,Bed)

#creating dataset
#write.csv(RolandMorrisOriginal,"/Users/joaovissoci/Desktop/roland_morris.csv")

#Excluding NAs
RolandMorrisNA<-na.omit(RolandMorrisOriginal)


#### Taxonometric Scale
MAMBAC(scale(RolandMorrisOriginal)[,1:3], Comp.Data = T)


#### FACTOR ANALYSIS APPROACH
#EFA
#Group of functinos to determine the number os items to be extracted
par(mfrow=c(2,2)) #Command to configure the plot area for the scree plot graph
ev <- eigen(cor(RolandMorrisOriginal)) # get eigenvalues - insert the data you want to calculate the scree plot for
ev # Show eigend values
ap <- parallel(subject=nrow(RolandMorrisOriginal),var=ncol(RolandMorrisOriginal),rep=100,cent=.05) #Calculate the acceleration factor
summary(ap)
nS <- nScree(ev$values) #Set up the Scree Plot 
plotnScree(nS) # Plot the ScreePlot Graph

#Function to calculate the KMO values - colocar link par ao gist
kmo<-kmo(RolandMorrisOriginal) #Run the Kmo function for the data you want to calculate
kmo$overall

#Functino to exctract the factor loadings. 
#Arguments are DATA, Number of factors, rotation method. 
#Look here http://goo.gl/kY3ln for different methods of estimations or rotations
RM1f<-mirt(RolandMorrisOriginal,1)
RM1f
summary(RM1f,rotate="promax",suppress=0.25)
RM2f<-mirt(RolandMorrisOriginal,2)
RM2f
summary(RM2f,rotate="promax",suppress=0.25)
RM3f<-mirt(RolandMorrisOriginal,3)
RM3f
summary(RM3f,rotate="promax",suppress=0.25)
RM4f<-mirt(RolandMorrisOriginal,4)
RM4f
summary(RM4f,rotate="promax",suppress=0.25)

#CFA
# Insert de covariance matrix - CFA (or SEM) is always calculated in relation to a covariance or correlation matrix, here we will create the covariance matrix

#Tryout 1
RMorrisFactorModel<-remove.vars(RolandMorrisOriginal, names=c("Position"), info=TRUE)

cfa1frm <- specifyModel()
F1->Home,var1,NA
F1->Position,var2,NA
F1->Walk,var3,NA
F1->Jobs,var4,NA
F1->Handrail,var5,NA
F1->Liedown,var6,NA
F1->Holdon,var7,NA
F1->Otherpeople,var8,NA
F1->Dressed,var9,NA
F1->Periods,var10,NA
F1->Bend,var11,NA
F1->Getout,var12,NA
F1->Painful,var13,NA
F1->Turnover,var14,NA
F1->Appetite,var15,NA    
F1->Socks,var16,NA
F1->Distances,var17,NA
F1->Lesswell,var18,NA
F1->Someone,var19,NA
F1->Sitdown,var20,NA
F1->Heavy,var21,NA     
F1->Irritable,var22,NA
F1->Upstairs,var23,NA
F1->Bed,var24,NA       
F1<->F1,NA,1
Getout<->Holdon,erro1,NA
Someone<->Appetite,erro2,NA
#Endofmodel

# Insert de covariance matrix - CFA (or SEM) is always calculated in relation to a covariance or correlation matrix, here we will create the covariance matrix
#cov1frm <- cov(RMorrisFactorModel, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))
cor1frm<-hetcor(RolandMorrisOriginal, maxcor=.9999)$correlations

# Estimate the model
cfa1factorrm<- sem(cfa1frm, cor1frm, N=394)
summary(cfa1factorrm,fit.indices=c("GFI", "AGFI", "RMSEA", "NFI", 
                          "NNFI", "CFI", "RNI", "IFI", "SRMR", "AIC", "AICc"))

# Reporta os efeitos das trjetórias do modelo
effects(cfa1factorrm)

# Reporta os coeficientes estandardizados das trajetórias (linhas)
x<-standardizedCoefficients(cfa1factorrm)

#Modification index
modIndices(cfa1factorrm)

#ITEM RESPONSE THEORY
#### USING eRM Package
IRTRolandMorris <- RM(RolandMorrisOriginal)
summary(IRTRolandMorris)
  summary(IRTRolandMorris$etapar)
  summary(IRTRolandMorris$se.eta)
coef(IRTRolandMorris)
confint(IRTRolandMorris,"beta")
plotI(IRTRolandMorris,i=3)
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

#########Alpha de Cronbach by ltm package - GIves CI
cronbach.alpha(RolandMorrisOriginal, standardized = TRUE, CI = TRUE, 
               probs = c(0.025, 0.975), B = 1000, na.rm = FALSE)

alpha(RolandMorrisOriginal)

#Composite Reliabilty
# sum(x[1:24,2])^2/(sum(x[1:24,2])^2+sum(x[28:51,2]))
#
#
######## QUEBEC PAIN SCALE ############
#### Taxonometric Scale
MAMBAC(QuebecOriginal[,1:3], Comp.Data = T)

####TRYOUT with all the original 6 categories of responses ###############

QuebecOriginal<-data.frame(Getoutbed,Sleep,Turn.over,Ride,Standup,Chair,Climb,Fewblocks,
                           Severalmiles,Shelves,Ball,Run,Food,Makebed,Pantyhose,Bathtub,
                           Movechair,Heavydoors,Carrybags,Suitcase)
#Excluding NAs
QuebecNA<-na.omit(QuebecOriginal)

#### FACTOR ANALYSIS APPROACH

par(mfrow=c(2,2)) #Command to configure the plot area for the scree plot graph
ev <- eigen(cor(QuebecOriginal)) # get eigenvalues - insert the data you want to calculate the scree plot for
ev # Show eigend values
ap <- parallel(subject=nrow(QuebecOriginal),var=ncol(QuebecOriginal),rep=100,cent=.05) #Calculate the acceleration factor
summary(ap)
nS <- nScree(ev$values) #Set up the Scree Plot 
plotnScree(nS) # Plot the ScreePlot Graph

#Function to calculate the KMO values - colocar link par ao gist
kmo<-kmo(QuebecOriginal) #Run the Kmo function for the data you want to calculate
kmo$overall

#Functino to exctract the factor loadings. 
fa(QuebecOriginal,6,fm="pa",rotate="promax")
fa(QuebecOriginal,5,fm="pa",rotate="promax")
fa(QuebecOriginal,4,fm="pa",rotate="promax")
fa(QuebecOriginal,3,fm="pa",rotate="promax")
fa(QuebecOriginal,2,fm="pa",rotate="promax")
fa(QuebecOriginal,1,fm="pa",rotate="promax")

#Confirmatory Factor Analysis
#Using SEM package to rund CFA Models
#CFA 1 Factor Quebec
cfa1fq <- specifyModel()
F1->Getoutbed,var1,NA
F1->Sleep,var2,NA
F1->Turn.over,var3,NA
F1->Ride,var4,NA
F1->Standup,var5,NA
F1->Chair,var6,NA
F1->Climb,var7,NA
F1->Fewblocks,var8,NA
F1->Severalmiles,var9,NA
F1->Shelves,var10,NA
F1->Ball,var11,NA
F1->Run,var12,NA
F1->Food,var13,NA
F1->Makebed,var14,NA    
F1->Pantyhose,var15,NA
F1->Bathtub,var16,NA
F1->Movechair,var17,NA
F1->Heavydoors,var18,NA
F1->Carrybags,var19,NA
F1->Suitcase,var20,NA     
F1<->F1,NA,1
Movechair<->Food,erro1,NA
Turn.over<->Sleep,erro2,NA
#end of the model

# Insert de covariance matrix - CFA (or SEM) is always calculated in relation to a covariance or correlation matrix, here we will create the covariance matrix
cov1fq <- cov(QuebecOriginal, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))

# Estimate the model
cfa1factorq<- sem(cfa1fq, cov1fq, N=394)
summary(cfa1factorq,fit.indices=c("GFI", "AGFI", "RMSEA", "NFI","NNFI", "CFI", "RNI", "IFI", "SRMR", "AIC", "AICc"))

# Reporta os efeitos das trjetórias do modelo
effects(cfa1factorq)

# Reporta os coeficientes estandardizados das trajetórias (linhas)
standardizedCoefficients(cfa1factorq)

#Modification index
modIndices(cfa1factorq)


#ITEM RESPONSE THEORY
#### USING eRM Package

#ploytomous
irt_model<-PCM(QuebecOriginal)
#RSM(data)
thresholds(irt_model)
plotI(irt_model,i=1)
plotICC(irt_model,item.subset=1:4,ask=F,empICC=list("raw"),empCI=list(lty="solid"))
plotPImap(irt_model, sorted=FALSE)
plotPWmap(irt_model)
pp<-person.parameter(irt_model)
eRm::itemfit(pp)
summary(itemfit(pp)$i.outfitMSQ)
sd(itemfit(pp)$i.outfitMSQ)
summary(itemfit(pp)$i.infitMSQ)
sd(itemfit(pp)$i.infitMSQ)

####TRYOUT with 3 categories of responses ###############
#irt modeling with 3 categories of response
  ## Trying to recode the data
  quebec_irt2<-NULL
  quebec_irt2$Getoutbed<-car::recode(Getoutbed,"1=0;2=1;3=1;4=2;5=2;6=2")
  quebec_irt2$Sleep<-car::recode(Sleep,"1=0;2=1;3=1;4=2;5=2;6=2")
  quebec_irt2$Turn.over<-car::recode(Turn.over,"1=0;2=1;3=1;4=2;5=2;6=2")
  quebec_irt2$Ride<-car::recode(Ride,"1=0;2=1;3=1;4=2;5=2;6=2")
  quebec_irt2$Standup<-car::recode(Standup,"1=0;2=1;3=1;4=2;5=2;6=2")
  quebec_irt2$Chair<-car::recode(Chair,"1=0;2=1;3=1;4=2;5=2;6=2")
  quebec_irt2$Climb<-car::recode(Climb,"1=0;2=1;3=1;4=2;5=2;6=2")
  quebec_irt2$Fewblocks<-car::recode(Fewblocks,"1=0;2=1;3=1;4=2;5=2;6=2")
  quebec_irt2$Severalmiles<-car::recode(Severalmiles,"1=0;2=1;3=1;4=2;5=2;6=2")
  quebec_irt2$Shelves<-car::recode(Shelves,"1=0;2=1;3=1;4=2;5=2;6=2")
  quebec_irt2$Ball<-car::recode(Ball,"1=0;2=1;3=1;4=2;5=2;6=2")
  quebec_irt2$Run<-car::recode(Run,"1=0;2=1;3=1;4=2;5=2;6=2")
  quebec_irt2$Food<-car::recode(Food,"1=0;2=1;3=1;4=2;5=2;6=2")
  quebec_irt2$Makebed<-car::recode(Makebed,"1=0;2=1;3=1;4=2;5=2;6=2")
  quebec_irt2$Pantyhose<-car::recode(Pantyhose,"1=0;2=1;3=1;4=2;5=2;6=2")
  quebec_irt2$Bathtub<-car::recode(Bathtub,"1=0;2=1;3=1;4=2;5=2;6=2")
  quebec_irt2$Movechair<-car::recode(Movechair,"1=0;2=1;3=1;4=2;5=2;6=2")
  quebec_irt2$Heavydoors<-car::recode(Heavydoors,"1=0;2=1;3=1;4=2;5=2;6=2")
  quebec_irt2$Carrybags<-car::recode(Carrybags,"1=0;2=1;3=1;4=2;5=2;6=2")
  quebec_irt2$Suitcase<-car::recode(Suitcase,"1=0;2=1;3=1;4=2;5=2;6=2")
  QuebecOriginal<-with(quebec_irt2,data.frame(Getoutbed,Sleep,Turn.over,Ride,Standup,Chair,Climb,Fewblocks,
                             Severalmiles,Shelves,Ball,Run,Food,Makebed,Pantyhose,Bathtub,
                             Movechair,Heavydoors,Carrybags,Suitcase))

#creating dataset
write.csv(QuebecOriginal,"/Users/joaovissoci/Desktop/quebec.csv")

#EFA
#### FACTOR ANALYSIS APPROACH

  par(mfrow=c(2,2)) #Command to configure the plot area for the scree plot graph
  ev <- eigen(cor(QuebecOriginal)) # get eigenvalues - insert the data you want to calculate the scree plot for
  ev # Show eigend values
  ap <- parallel(subject=nrow(QuebecOriginal),var=ncol(QuebecOriginal),rep=100,cent=.05) #Calculate the acceleration factor
  summary(ap)
  nS <- nScree(ev$values) #Set up the Scree Plot 
  plotnScree(nS) # Plot the ScreePlot Graph

  #Function to calculate the KMO values - colocar link par ao gist
  kmo<-kmo(QuebecOriginal) #Run the Kmo function for the data you want to calculate
  kmo$overall

  #Functino to exctract the factor loadings. 
  fa(QuebecOriginal,6,fm="pa",rotate="promax")
  fa(QuebecOriginal,5,fm="pa",rotate="promax")
  fa(QuebecOriginal,4,fm="pa",rotate="promax")
  fa(QuebecOriginal,3,fm="pa",rotate="promax")
  fa(QuebecOriginal,2,fm="pa",rotate="promax")
  fa(QuebecOriginal,1,fm="pa",rotate="oblimin")

#Confirmatory Factor Analysis
  #Using SEM package to rund CFA Models
  #CFA 1 Factor Quebec
  cfa1fq<- specifyModel()
  F1->Getoutbed,var1,NA
  F1->Sleep,var2,NA
  F1->Turn.over,var3,NA
  F1->Ride,var4,NA
  F1->Standup,var5,NA
  F1->Chair,var6,NA
  F1->Climb,var7,NA
  F1->Fewblocks,var8,NA
  F1->Severalmiles,var9,NA
  F1->Shelves,var10,NA
  F1->Ball,var11,NA
  F1->Run,var12,NA
  F1->Food,var13,NA
  F1->Makebed,var14,NA    
  F1->Pantyhose,var15,NA
  F1->Bathtub,var16,NA
  F1->Movechair,var17,NA
  F1->Heavydoors,var18,NA
  F1->Carrybags,var19,NA
  F1->Suitcase,var20,NA     
  F1<->F1,NA,1
  Movechair<->Food,erro1,NA
  Turn.over<->Sleep,erro2,NA
  #end of the model

  # Insert de covariance matrix - CFA (or SEM) is always calculated in relation to a covariance or correlation matrix, here we will create the covariance matrix
  cov1fq <- cov(QuebecOriginal, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))

  # Estimate the model
  cfa1factorq<- sem(cfa1fq, cov1fq, N=394)
  summary(cfa1factorq,fit.indices=c("GFI", "AGFI", "RMSEA", "NFI","NNFI", "CFI", "RNI", "IFI", "SRMR", "AIC", "AICc"))

  # Reporta os efeitos das trjetórias do modelo
  effects(cfa1factorq)

  # Reporta os coeficientes estandardizados das trajetórias (linhas)
x<-standardizedCoefficients(cfa1factorq)

  #Modification index
  modIndices(cfa1factorq)

#ITEM RESPONSE THEORY
#### USING eRM Package

  #ploytomous
  irt_model<-PCM(QuebecOriginal)
  #RSM(data)
  thresholds(irt_model)
  summary(irt_model$etapar)
  summary(irt_model$se.eta)
  plotI(irt_model,i=1)
  plotICC(irt_model,item.subset=6,ask=F,empICC=list("raw"),empCI=list(lty="solid"))
  plotPImap(irt_model, sorted=FALSE)
  plotPWmap(irt_model)
  pp<-eRm::person.parameter(irt_model)
 summary(eRm::itemfit(pp)$i.outfitMSQ)
 sd(eRm::itemfit(pp)$i.outfitMSQ)
 summary(eRm::itemfit(pp)$i.infitMSQ)
 sd(eRm::itemfit(pp)$i.infitMSQ)

####TRYOUT with one categories of responses ###############
#irt modeling with dichotomous response
#Recoding Data
quebec_irt3<-NULL
quebec_irt3$Getoutbed<-car::recode(Getoutbed,"1=0;2=0;3=0;4=1;5=1;6=1")
quebec_irt3$Sleep<-car::recode(Sleep,"1=0;2=0;3=0;4=1;5=1;6=1")
quebec_irt3$Turn.over<-car::recode(Turn.over,"1=0;2=0;3=0;4=1;5=1;6=1")
quebec_irt3$Ride<-car::recode(Ride,"1=0;2=0;3=0;4=1;5=1;6=1")
quebec_irt3$Standup<-car::recode(Standup,"1=0;2=0;3=0;4=1;5=1;6=1")
quebec_irt3$Chair<-car::recode(Chair,"1=0;2=0;3=0;4=1;5=1;6=1")
quebec_irt3$Climb<-car::recode(Climb,"1=0;2=0;3=0;4=1;5=1;6=1")
quebec_irt3$Fewblocks<-car::recode(Fewblocks,"1=0;2=0;3=0;4=1;5=1;6=1")
quebec_irt3$Severalmiles<-car::recode(Severalmiles,"1=0;2=0;3=0;4=1;5=1;6=1")
quebec_irt3$Shelves<-car::recode(Shelves,"1=0;2=0;3=0;4=1;5=1;6=1")
quebec_irt3$Ball<-car::recode(Ball,"1=0;2=0;3=0;4=1;5=1;6=1")
quebec_irt3$Run<-car::recode(Run,"1=0;2=0;3=0;4=1;5=1;6=1")
quebec_irt3$Food<-car::recode(Food,"1=0;2=0;3=0;4=1;5=1;6=1")
quebec_irt3$Makebed<-car::recode(Makebed,"1=0;2=0;3=0;4=1;5=1;6=1")
quebec_irt3$Pantyhose<-car::recode(Pantyhose,"1=0;2=0;3=0;4=1;5=1;6=1")
quebec_irt3$Bathtub<-car::recode(Bathtub,"1=0;2=0;3=0;4=1;5=1;6=1")
quebec_irt3$Movechair<-car::recode(Movechair,"1=0;2=0;3=0;4=1;5=1;6=1")
quebec_irt3$Heavydoors<-car::recode(Heavydoors,"1=0;2=0;3=0;4=1;5=1;6=1")
quebec_irt3$Carrybags<-car::recode(Carrybags,"1=0;2=0;3=0;4=1;5=1;6=1")
quebec_irt3$Suitcase<-car::recode(Suitcase,"1=0;2=0;3=0;4=1;5=1;6=1")
QuebecOriginal<-with(quebec_irt3,data.frame(Getoutbed,Sleep,Turn.over,Ride,Standup,Chair,Climb,Fewblocks,
                           Severalmiles,Shelves,Ball,Run,Food,Makebed,Pantyhose,Bathtub,
                           Movechair,Heavydoors,Carrybags,Suitcase))


#### FACTOR ANALYSIS APPROACH

par(mfrow=c(2,2)) #Command to configure the plot area for the scree plot graph
ev <- eigen(cor(QuebecOriginal)) # get eigenvalues - insert the data you want to calculate the scree plot for
ev # Show eigend values
ap <- parallel(subject=nrow(QuebecOriginal),var=ncol(QuebecOriginal),rep=100,cent=.05) #Calculate the acceleration factor
summary(ap)
nS <- nScree(ev$values) #Set up the Scree Plot 
plotnScree(nS) # Plot the ScreePlot Graph

#Function to calculate the KMO values - colocar link par ao gist
kmo<-kmo(QuebecOriginal) #Run the Kmo function for the data you want to calculate
kmo$overall

#Functino to exctract the factor loadings. 
fa(QuebecOriginal,6,fm="pa",rotate="promax")
fa(QuebecOriginal,5,fm="pa",rotate="promax")
fa(QuebecOriginal,4,fm="pa",rotate="promax")
fa(QuebecOriginal,3,fm="pa",rotate="promax")
fa(QuebecOriginal,2,fm="pa",rotate="promax")
fa(QuebecOriginal,1,fm="pa",rotate="promax")

#Confirmatory Factor Analysis
#Using SEM package to rund CFA Models
#CFA 1 Factor Quebec
cfa1fq <- specifyModel()
F1->Getoutbed,var1,NA
F1->Sleep,var2,NA
F1->Turn.over,var3,NA
F1->Ride,var4,NA
F1->Standup,var5,NA
F1->Chair,var6,NA
F1->Climb,var7,NA
F1->Fewblocks,var8,NA
F1->Severalmiles,var9,NA
F1->Shelves,var10,NA
F1->Ball,var11,NA
F1->Run,var12,NA
F1->Food,var13,NA
F1->Makebed,var14,NA    
F1->Pantyhose,var15,NA
F1->Bathtub,var16,NA
F1->Movechair,var17,NA
F1->Heavydoors,var18,NA
F1->Carrybags,var19,NA
F1->Suitcase,var20,NA     
F1<->F1,NA,1
Movechair<->Food,erro1,NA
Turn.over<->Sleep,erro2,NA
#end of the model

# Insert de covariance matrix - CFA (or SEM) is always calculated in relation to a covariance or correlation matrix, here we will create the covariance matrix
cov1fq <- cov(QuebecOriginal, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))

# Estimate the model
cfa1factorq<- sem(cfa1fq, cov1fq, N=394)
summary(cfa1factorq,fit.indices=c("GFI", "AGFI", "RMSEA", "NFI","NNFI", "CFI", "RNI", "IFI", "SRMR", "AIC", "AICc"))

# Reporta os efeitos das trjetórias do modelo
effects(cfa1factorq)

# Reporta os coeficientes estandardizados das trajetórias (linhas)
standardizedCoefficients(cfa1factorq)

#Modification index
modIndices(cfa1factorq)

#ITEM RESPONSE THEORY
#### USING eRM Package

#ploytomous
irt_model<-PCM(QuebecOriginal)
#RSM(data)
thresholds(irt_model)
plotI(irt_model,i=1)
plotICC(irt_model,item.subset=1:4,ask=F,empICC=list("raw"),empCI=list(lty="solid"))
plotPImap(irt_model, sorted=FALSE)
plotPWmap(irt_model)
pp<-person.parameter(irt_model)
summary(itemfit(pp)$i.outfitMSQ)
sd(itemfit(pp)$i.outfitMSQ)
summary(itemfit(pp)$i.infitMSQ)
sd(itemfit(pp)$i.infitMSQ)

#########Alpha de Cronbach by ltm package - GIves CI
cronbach.alpha(QuebecOriginal, standardized = TRUE, CI = TRUE, 
               probs = c(0.025, 0.975), B = 1000, na.rm = FALSE)

alpha(QuebecOriginal)

#Composite Reliabilty
sum(x[1:20,2])^2/(sum(x[1:20,2])^2+sum(x[24:43,2]))

#
#
######## WADDEL PAIN SCALE ############
#Recoding Variables
Lifting<-car::recode(Lifting,"2=0;1=1")
Sitting<-car::recode(Sitting,"2=0;1=1")
Travelling<-car::recode(Travelling,"2=0;1=1")
Standing<-car::recode(Standing,"2=0;1=1")
Walking<-car::recode(Walking,"2=0;1=1")
Disturbed<-car::recode(Disturbed,"2=0;1=1")
Social<-car::recode(Social,"2=0;1=1")
Sexual<-car::recode(Sexual,"2=0;1=1")
Footwear<-car::recode(Footwear,"2=0;1=1")
WaddellOriginal<-data.frame(Lifting,Sitting,Travelling,Standing,Walking,Disturbed,Social,
                            Sexual,Footwear)

#Excluding NAs
WaddellNA<-na.omit(WaddellOriginal)

write.csv(WaddellOriginal,"/Users/joaovissoci/Desktop/waddel.csv")

#### Taxonometric Scale
MAMBAC(scale(WaddellOriginal)[,1:3], Comp.Data = T)


#EFA
#Group of functinos to determine the number os items to be extracted
par(mfrow=c(2,2)) #Command to configure the plot area for the scree plot graph
ev <- eigen(cor(WaddellOriginal)) # get eigenvalues - insert the data you want to calculate the scree plot for
ev # Show eigend values
ap <- parallel(subject=nrow(WaddellOriginal),var=ncol(WaddellOriginal),rep=100,cent=.05) #Calculate the acceleration factor
summary(ap)
nS <- nScree(ev$values) #Set up the Scree Plot 
plotnScree(nS) # Plot the ScreePlot Graph

#Function to calculate the KMO values - colocar link par ao gist
kmo<-kmo(WaddellOriginal) #Run the Kmo function for the data you want to calculate
kmo$overall

#Functino to exctract the factor loadings. 
waddel1f<-mirt(WaddellOriginal,1)
waddel1f
summary(waddel1f,rotate="oblimin",suppress=0.25)
waddel2f<-mirt(WaddellOriginal,2)
waddel2f
summary(waddel2f,rotate="promax",suppress=0.25)
waddel3f<-mirt(WaddellOriginal,3)
waddel3f
summary(waddel3f,rotate="promax",suppress=0.25)


#Confirmatory Factor Analysis
#Using SEM package to rund CFA Models
#CFA 1 Factor Waddell

cfa1fw <- specifyModel()
F1->Lifting,var1,NA
F1->Sitting,var2,NA
F1->Travelling,var3,NA
F1->Standing,var4,NA
F1->Walking,var5,NA
F1->Disturbed,var6,NA
F1->Social,var7,NA
F1->Sexual,var8,NA
F1->Footwear,var9,NA
F1<->F1,NA,1
Walking<->Standing,erro1,NA
#end of the model

# Insert de covariance matrix - CFA (or SEM) is always calculated in relation to a covariance or correlation matrix, here we will create the covariance matrix
#cov1fw <- cov(Waddell, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))
cor1frm<-hetcor(WaddellOriginal, maxcor=.9999)$correlations

# Estimate the model
cfa1factorw<- sem(cfa1fw, cor1frm, N=394)
summary(cfa1factorw,fit.indices=c("GFI", "AGFI", "RMSEA", "NFI","NNFI", "CFI", "RNI", "IFI", "SRMR", "AIC", "AICc"))


# Reporta os efeitos das trjetórias do modelo
effects(cfa1factorw)

# Reporta os coeficientes estandardizados das trajetórias (linhas)
x<-standardizedCoefficients(cfa1factorw)

#Modification index
modIndices(cfa1factorw)

#ITEM RESPONSE THEORY

#### USING eRM Package
IRTRolandMorris <- RM(WaddellOriginal)
summary(IRTRolandMorris)
summary(IRTRolandMorris$etapar)
summary(IRTRolandMorris$se.eta)
coef(IRTRolandMorris)
confint(IRTRolandMorris,"beta")
plotI(IRTRolandMorris,i=3)
plotICC(IRTRolandMorris,item.subset=1:4,ask=F,empICC=list("raw"),empCI=list(lty="solid"))
plotPImap(IRTRolandMorris)
pp<-person.parameter(IRTRolandMorris)
lrt<-LRtest(IRTRolandMorris,se=TRUE)
Waldtest(IRTRolandMorris)
itemfit(pp)
summary(itemfit(pp)$i.outfitMSQ)
sd(itemfit(pp)$i.outfitMSQ)
summary(itemfit(pp)$i.infitMSQ)
sd(itemfit(pp)$i.infitMSQ)
NPtest(IRTRolandMorris,method="T11")
plotGOF(lrt,conf=list())

#########Alpha de Cronbach by ltm package - GIves CI
cronbach.alpha(RolandMorrisOriginal, standardized = TRUE, CI = TRUE, 
               probs = c(0.025, 0.975), B = 1000, na.rm = FALSE)

alpha(WaddellOriginal)

#Composite Reliabilty
sum(x[1:9,2])^2/(sum(x[1:9,2])^2+sum(x[12:20,2]))

#
#
######## BACKPAIN SCALE ############

### TRYOUT WITH ALL ORIGINAL 6 CATEGORIES OF RESPONSES
BackPainOriginal<-data.frame(Housework,Hobbies,Performing,Bending,Shoes,Groceries,Sleeping,Standing1h,Walkingmile,Goingup,Sitting1h,Driving)

#Excluding NAs
BackPainNA<-na.omit(BackPainOriginal)

#### Taxonometric Scale
MAMBAC(scale(BackPainOriginal)[,1:3], Comp.Data = T)

#### FACTOR ANALYSIS APPROACH
#Group of functinos to determine the number os items to be extracted
par(mfrow=c(2,2)) #Command to configure the plot area for the scree plot graph
ev <- eigen(cor(BackPainOriginal)) # get eigenvalues - insert the data you want to calculate the scree plot for
ev # Show eigend values
ap <- parallel(subject=nrow(BackPainOriginal),var=ncol(BackPainOriginal),rep=100,cent=.05) #Calculate the acceleration factor
summary(ap)
nS <- nScree(ev$values) #Set up the Scree Plot 
plotnScree(nS) # Plot the ScreePlot Graph

#Function to calculate the KMO values - colocar link par ao gist
kmo<-kmo(BackPainOriginal) #Run the Kmo function for the data you want to calculate
kmo$overall

#Functino to exctract the factor loadings. 
fa(BackPainOriginal,2,fm="pa",rotate="promax")
fa(BackPainOriginal,1,fm="pa",rotate="promax")

#Confirmatory Factor Analysis
#Using SEM package to rund CFA Models
#CFA 1 Factor Back Pain

cfa1fbp <- specifyModel()
F1->Housework,var1,NA
F1->Hobbies,var2,NA
F1->Performing,var3,NA
F1->Bending,var4,NA
F1->Shoes,var5,NA
F1->Groceries,var6,NA
F1->Sleeping,var7,NA
F1->Standing1h,var8,NA
F1->Walkingmile,var9,NA
F1->Goingup,var10,NA
F1->Sitting1h,var11,NA
F1->Driving,var12,NA
F1<->F1,NA,1
Walkingmile<->Standing1h,erro1,NA
Hobbies<->Housework,erro2,NA
Goingup<->Walkingmile,erro3,
Performing<->Hobbies,erro4,NA
#end of the model

# Insert de covariance matrix - CFA (or SEM) is always calculated in relation to a covariance or correlation matrix, here we will create the covariance matrix
cov1fbp <- cov(BackPainOriginal, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))

# Estimate the model
cfa1factorbp<- sem(cfa1fbp, cov1fbp, N=394)
summary(cfa1factorbp,fit.indices=c("GFI", "AGFI", "RMSEA", "NFI","NNFI", "CFI", "RNI", "IFI", "SRMR", "AIC", "AICc"))

# Reporta os efeitos das trjetórias do modelo
effects(cfa1factorbp)

# Reporta os coeficientes estandardizados das trajetórias (linhas)
standardizedCoefficients(cfa1factorbp)

#Modification index
modIndices(cfa1factorbp)

#ITEM RESPONSE THEORY
#ploytomous
irt_model<-PCM(BackPainOriginal)
#RSM(data)
thresholds(irt_model)
plotPImap(irt_model)
pp<-person.parameter(irt_model)
eRm::itemfit(pp)
summary(itemfit(pp)$i.outfitMSQ)
sd(itemfit(pp)$i.outfitMSQ)
summary(itemfit(pp)$i.infitMSQ)
sd(itemfit(pp)$i.infitMSQ)
 plotICC(irt_model,item.subset=2,ask=F,empICC=list("raw"),empCI=list(lty="solid"))

### TRYOUT WITH 3 CATEGORIES OF RESPONSES
#Recoding Variables to match scales original categories of responses
bp2<-NULL
bp2$Housework<-car::recode(Housework,"1=0;2=0;3=1;4=1;5=2;6=2")
bp2$Hobbies<-car::recode(Hobbies,"1=0;2=0;3=1;4=1;5=2;6=2")
bp2$Performing<-car::recode(Performing,"1=0;2=0;3=1;4=1;5=2;6=2")
bp2$Bending<-car::recode(Bending,"1=0;2=0;3=1;4=1;5=2;6=2")
bp2$Shoes<-car::recode(Shoes,"1=0;2=0;3=1;4=1;5=2;6=2")
bp2$Groceries<-car::recode(Groceries,"1=0;2=0;3=1;4=1;5=2;6=2")
bp2$Sleeping<-car::recode(Sleeping,"1=0;2=0;3=1;4=1;5=2;6=2")
bp2$Standing1h<-car::recode(Standing1h,"1=0;2=0;3=1;4=1;5=2;6=2")
bp2$Walkingmile<-car::recode(Walkingmile,"1=0;2=0;3=1;4=1;5=2;6=2")
bp2$Goingup<-car::recode(Goingup,"1=0;2=0;3=1;4=1;5=2;6=2")
bp2$Sitting1h<-car::recode(Sitting1h,"1=0;2=0;3=1;4=1;5=2;6=2")
bp2$Driving<-car::recode(Driving,"1=0;2=0;3=1;4=1;5=2;6=2")

BackPainOriginal<-with(bp2,data.frame(Housework,Hobbies,Performing,Bending,Shoes,Groceries,Sleeping,
                             Standing1h,Walkingmile,Goingup,Sitting1h,Driving))

#Excluding NAs
BackPainNA<-na.omit(BackPainOriginal)

write.csv(BackPainOriginal,"/Users/joaovissoci/Desktop/backpain.csv")

#### Taxonometric Scale
MAMBAC(scale(BackPainOriginal)[,1:3], Comp.Data = T)


#### FACTOR ANALYSIS APPROACH
#Group of functinos to determine the number os items to be extracted
par(mfrow=c(2,2)) #Command to configure the plot area for the scree plot graph
ev <- eigen(cor(BackPainOriginal)) # get eigenvalues - insert the data you want to calculate the scree plot for
ev # Show eigend values
ap <- parallel(subject=nrow(BackPainOriginal),var=ncol(BackPainOriginal),rep=100,cent=.05) #Calculate the acceleration factor
summary(ap)
nS <- nScree(ev$values) #Set up the Scree Plot 
plotnScree(nS) # Plot the ScreePlot Graph

#Function to calculate the KMO values - colocar link par ao gist
kmo<-kmo(BackPainOriginal) #Run the Kmo function for the data you want to calculate
kmo$overall

#Functino to exctract the factor loadings. 
fa(BackPainOriginal,2,fm="pa",rotate="promax")
fa(BackPainOriginal,1,fm="pa",rotate="oblimin")

#Confirmatory Factor Analysis
#Using SEM package to rund CFA Models
#CFA 1 Factor Back Pain

cfa1fbp <- specifyModel()
F1->Housework,var1,NA
F1->Hobbies,var2,NA
F1->Performing,var3,NA
F1->Bending,var4,NA
F1->Shoes,var5,NA
F1->Groceries,var6,NA
F1->Sleeping,var7,NA
F1->Standing1h,var8,NA
F1->Walkingmile,var9,NA
F1->Goingup,var10,NA
F1->Sitting1h,var11,NA
F1->Driving,var12,NA
F1<->F1,NA,1
Walkingmile<->Standing1h,erro1,NA
Goingup<->Walkingmile,erro2,NA
#end of the model

# Insert de covariance matrix - CFA (or SEM) is always calculated in relation to a covariance or correlation matrix, here we will create the covariance matrix
cov1fbp <- cov(BackPainOriginal, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))

# Estimate the model
cfa1factorbp<- sem(cfa1fbp, cov1fbp, N=394)
summary(cfa1factorbp,fit.indices=c("GFI", "AGFI", "RMSEA", "NFI","NNFI", "CFI", "RNI", "IFI", "SRMR", "AIC", "AICc"))

# Reporta os efeitos das trjetórias do modelo
effects(cfa1factorbp)

# Reporta os coeficientes estandardizados das trajetórias (linhas)
x<-standardizedCoefficients(cfa1factorbp)

#Modification index
modIndices(cfa1factorbp)


#ITEM RESPONSE THEORY
#ploytomous
irt_model<-PCM(BackPainOriginal)
#RSM(data)
thresholds(irt_model)
summary(irt_model$etapar)
summary(irt_model$se.eta)
plotI(irt_model,i=1)
plotICC(irt_model,item.subset=1:4,ask=F,empICC=list("raw"),empCI=list(lty="solid"))
plotPImap(irt_model, sorted=FALSE)
plotPWmap(irt_model)
pp<-person.parameter(irt_model)
summary(eRm::itemfit(pp)$i.outfitMSQ)
sd(eRm::itemfit(pp)$i.outfitMSQ)
summary(eRm::itemfit(pp)$i.infitMSQ)
sd(eRm::itemfit(pp)$i.infitMSQ)

#########Alpha de Cronbach by ltm package - GIves CI
cronbach.alpha(RolandMorrisOriginal, standardized = TRUE, CI = TRUE, 
               probs = c(0.025, 0.975), B = 1000, na.rm = FALSE)

alpha(BackPainOriginal)

#Composite Reliabilty
sum(x[1:12,2])^2/(sum(x[1:12,2])^2+sum(x[16:27,2]))
#
#
##########################################################################
#OBJECTIVE 4 - Normalization of all four scales
##########################################################################
#Defining dataframe with the data for the SEM model
PainSEM<-na.omit(data.frame(RolandMorrisOriginal,QuebecOriginal,WaddellOriginal,BackPainOriginal))

# SEM with all scales (?) attesting multidimensionality
#Specifying SEM model
#Padrão:
#1. Copiar todos as linhas dos modelos criadas na seção da CFA
#2. Modificar F1 pelo nome da escala
#3. Criar linhas de covariância (correlação entre as escalas)
#4. Criar variavel latente PAIN e definir modelo (cada escala predizendo "->" PAIN)
#5. Mudar nome das variáveis de cada escala (todas estão com a mesma sequencia var1,var2,var3)
#6. Mudar linha de comando da primeira variavel de cada escala de "var1,NA" para "NA,1"
#7. Mudar linha de comando do erro de cada escala (F1<->F1, ou neckdisability<->neckdisability)
# de "NA,1" para "err1,NA". O numero apos o "err" segue uma sequencia ordinal.
semmodel <- specifyModel()# Type these values that specify the model's relations (just use de Ctrl+R over each relation).

RolandMorris->Home,NA,1
RolandMorris->Walk,var113,NA
RolandMorris->Jobs,var114,NA
RolandMorris->Handrail,var115,NA
RolandMorris->Liedown,var116,NA
RolandMorris->Holdon,var117,NA
RolandMorris->Otherpeople,var118,NA
RolandMorris->Dressed,var119,NA
RolandMorris->Periods,var110,NA
RolandMorris->Bend,var111,NA
RolandMorris->Getout,var112,NA
RolandMorris->Painful,var113,NA
RolandMorris->Turnover,var114,NA
RolandMorris->Appetite,var115,NA    
RolandMorris->Socks,var116,NA
RolandMorris->Distances,var117,NA
RolandMorris->Lesswell,var118,NA
RolandMorris->Someone,var119,NA
RolandMorris->Sitdown,var120,NA
RolandMorris->Heavy,var121,NA     
RolandMorris->Irritable,var122,NA
RolandMorris->Upstairs,var123,NA
RolandMorris->Bed,var124,NA       
RolandMorris<->RolandMorris,cov1,NA
Quebec->Getoutbed,NA,1
Quebec->Sleep,var232,NA
Quebec->Turn.over,var233,NA
Quebec->Ride,var234,NA
Quebec->Standup,var235,NA
Quebec->Chair,var236,NA
Quebec->Climb,var237,NA
Quebec->Fewblocks,var238,NA
Quebec->Severalmiles,var239,NA
Quebec->Shelves,var240,NA
Quebec->Ball,var241,NA
Quebec->Run,var242,NA
Quebec->Food,var243,NA
Quebec->Makebed,var244,NA    
Quebec->Pantyhose,var245,NA
Quebec->Bathtub,var246,NA
Quebec->Movechair,var247,NA
Quebec->Heavydoors,var248,NA
Quebec->Carrybags,var249,NA
Quebec->Suitcase,var250,NA     
Quebec<->Quebec,cov2,NA
Waddell->Lifting,NA,1
Waddell->Sitting,var352,NA
Waddell->Travelling,var353,NA
Waddell->Standing,var354,NA
Waddell->Walking,var355,NA
Waddell->Disturbed,var356,NA
Waddell->Social,var357,NA
Waddell->Sexual,var358,NA
Waddell->Footwear,var359,NA
Waddell<->Waddell,cov3,NA
Backpain->Housework,NA,1
Backpain->Hobbies,var462,NA
Backpain->Performing,var463,NA
Backpain->Bending,var464,NA
Backpain->Shoes,var465,NA
Backpain->Groceries,var466,NA
Backpain->Sleeping,var467,NA
Backpain->Standing1h,var468,NA
Backpain->Walkingmile,var469,NA
Backpain->Goingup,var470,NA
Backpain->Sitting1h,var471,NA
Backpain->Driving,var472,NA
Backpain<->Backpain,cov4,NA
Backpain->Pain,2order1,NA
Waddell->Pain,2order2,NA
Quebec->Pain,2order3,NA
RolandMorris->Pain,2order4,NA
Pain<->Pain,err5,NA
Backpain<->Waddell,coverro1,NA
Backpain<->Quebec,coverro2,NA
Backpain<->RolandMorris,coverro3,NA
Waddell<->Quebec,coverro4,NA
Waddell<->RolandMorris,coverro5,NA
Quebec<->RolandMorris,coverro6,NA

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
cor <- hetcor(PainSEM)$correlation

# Estimate de model (Here is where we have been finding difficulties)
sem <- sem(semmodel, cov, N=394)
sem2 <- sem(semmodel, cor, N=394)
summary(sem2) # Copiar e colar no texto
effects(semPMC)
standardizedCoefficients(sem) #Copiar e colar texto
# calcula ?ndices de modifica??o (testes de pontua??o) e altera??es de par?metros
# estimados para os par?metros fixos e limitados em um modelo de equa??es estruturais
modIndices(sem)

x<- psych::polychoric(PainSEM)
names<-c(c(1:24),c(1:20),c(1:9),c(1:12))
color<-c(rep("steelblue",24),rep("gold",20),rep("lightgreen",9),rep("red2",12))

tiff("/Users/rpietro/Desktop/Figure1.tiff", width = 500, height = 500,compression = 'lzw')
#tiff("C:\\Users\\Joao\\Desktop\\IRT_map.tiff", width = 500, height = 700,compression = 'lzw')
qgraph(x$rho,minimum=0.40,cut=0.60,layout="spring",color=color,labels=names)
dev.off()

#IRT with all the data
#ITEM RESPONSE THEORY
#ploytomous
irt_model<-PCM(PainSEM)
#RSM(data)
thresholds(irt_model)
tiff("/Users/rpietro/Desktop/Figure2.tiff", width = 500, height = 700,compression = 'lzw')
#tiff("C:\\Users\\Joao\\Desktop\\IRT_map.tiff", width = 500, height = 700,compression = 'lzw')
plotPImap(irt_model)
dev.off()
pp<-person.parameter(irt_model)
summary(itemfit(pp)$i.outfitMSQ)
sd(itemfit(pp)$i.outfitMSQ)
summary(itemfit(pp)$i.infitMSQ)
sd(itemfit(pp)$i.infitMSQ)

##########################################################################
#OBJECTIVE 5 - External validity comparison of normalized and non-normalized scales
##########################################################################
### Total score for each test and each subscale
#Calculating total scores for each test
RolandMorrisSUM<-rowSums(RolandMorrisOriginal) #Sum the number in a row for a data frame
summary(RolandMorrisSUM)
QuebecSUM<-rowSums(QuebecOriginal)
summary(QuebecSUM)
WaddellSUM<-rowSums(WaddellOriginal)
summary(WaddellSUM)
BackPainSUM<-rowSums(BackPainOriginal)
summary(BackPainSUM)

#Normalization to a scale from 0 to 100
#Formula 100*(RESULTADO - Valor minimo possivel do teste)/(Valor maximo do teste - valor minimo possivel do teste)
RolandMorrisScore<-100*((RolandMorrisSUM - 0)/(24-0))
QuebecScore<-100*((QuebecSUM - 0)/(40-0))
WaddellScore<-100*((WaddellSUM - 0)/(9-0))
BackPainScore<-100*((BackPainSUM - 0)/(24-0))

TestsScores_raw<-c(RolandMorrisScore,QuebecScore,WaddellScore,BackPainScore)

##Transforming to Rasch Measures
rm_rasch<-car::recode(RolandMorrisSUM,"0=1;1=2;2=6;3=10;4=14;5=20;6=25;7=31;8=36;9=41;10=46;11=51;12=57;13=62;14=66;15=70;16=75;17=78;18=81;20=86;21=88;22=90;23=91;24=96")
qb_rasch<-car::recode(QuebecSUM,"0=1;1=1;2=2;3=4;4=5;5=7;6=9;7=11;8=14;9=17;10=19;11=21;12=23;13=25;14=27;15=30;16=32;17=35;18=37;19=39;20=43;21=46;22=50;23=54;24=60;25=65;26=69;27=73;28=77;29=81;30=83;31=86;32=87;33=89;34=92;35=93;36=94;37=96;38=97;39=97;40=99")
wd_rasch<-car::recode(WaddellSUM,"0=3;1=10;2=19;3=26;4=36;5=51;6=64;7=75;8=84;9=94")
bp_rasch<-car::recode(BackPainSUM,"0=1;1=1;2=2;3=3;4=4;5=5;6=6;7=9;8=12;9=16;10=20;11=25;12=32;13=39;14=45;15=50;16=55;17=59;18=63;19=67;20=71;21=75;22=80;23=85;24=94")

TestsScores_rasch<-c(rm_rasch,qb_rasch,wd_rasch,bp_rasch)
#The idea here is to create a data frame with the final results of al the tests to be plotted together with the bpxplot.
RolandMorrisname<-c(rep("RolandMorris",394))
#summary(RolandMorris)
Quebecname<-c(rep("Quebec",394))
#summary(Quebec)
Waddellname<-c(rep("Waddell",394))
#summary(Waddell)
BackPainname<-c(rep("BackPain",394))
#summary(Backpain)

TestsNames<-rep(c(RolandMorrisname,Quebecname,
              Waddellname,BackPainname),2)
TestsScores<-c(TestsScores_raw,TestsScores_rasch)
Versions<-c(rep("Raw Scores",1576),rep("Rasch Percentiles",1576))
#Organizing data to plot 
boxplot<-data.frame(TestsScores,TestsNames,Versions)        
#Create a boxplotfor all scales's the total scores

tiff("/Users/rpietro/Desktop/Figure3.tiff", width = 500, height = 300,compression = 'lzw')
#tiff("C:\\Users\\Joao\\Desktop\\IRT_map.tiff", width = 500, height = 700,compression = 'lzw')
ggplot(boxplot, aes(TestsNames,TestsScores)) + geom_boxplot(aes(fill = Versions)) + scale_fill_brewer() + theme_bw() 
dev.off()

#rowMeans() #Extract the mean of the numbers in a row for a data frame
#colSums()  #Sum the number in a column for a data frame
#colMeans() #Extract the mean of the numbers in a column for a data frame


#COMORBIDITY SCALE - Recoding
HeartDisease<-car::recode(HeartDisease,"1=1;else=0")
TreatHeart<-car::recode(TreatHeart,"1=1;else=0")
ActHeart<-car::recode(ActHeart,"1=1;else=0")
HBPressure<-car::recode(HBPressure,"1=1;else=0")
TreatHB<-car::recode(TreatHB,"1=1;else=0")
ActHB<-car::recode(ActHB,"1=1;else=0")
LungDisease<-car::recode(LungDisease,"1=1;else=0")
TreatLung<-car::recode(TreatLung,"1=1;else=0")
ActLung<-car::recode(ActLung,"1=1;else=0")
Diabetes<-car::recode(Diabetes,"1=1;else=0")
TreatDiabetes<-car::recode(TreatDiabetes,"1=1;else=0")
ActDiabetes<-car::recode(ActDiabetes,"1=1;else=0")
Ulcer<-car::recode(Ulcer,"1=1;else=0")
TreatUlcer<-car::recode(TreatUlcer,"1=1;else=0")
ActUlcer<-car::recode(ActUlcer,"1=1;else=0")
Kidney<-car::recode(Kidney,"1=1;else=0")
TreatKidney<-car::recode(TreatKidney,"1=1;else=0")
ActKidney<-car::recode(ActKidney,"1=1;else=0")
LiverDisease<-car::recode(LiverDisease,"1=1;else=0")
TreatLiver<-car::recode(TreatLiver,"1=1;else=0")
ActLiver<-car::recode(ActLiver,"1=1;else=0")
Anemia<-car::recode(Anemia,"1=1;else=0")
TreatAnemia<-car::recode(TreatAnemia,"1=1;else=0")
ActAnemia<-car::recode(ActAnemia,"1=1;else=0")
Cancer<-car::recode(Cancer,"1=1;else=0")
TreatCancer<-car::recode(TreatCancer,"1=1;else=0")
ActCancer<-car::recode(ActCancer,"1=1;else=0")
Depression<-car::recode(Depression,"1=1;else=0")
TreatDepression<-car::recode(TreatDepression,"1=1;else=0")
ActDepression<-car::recode(ActDepression,"1=1;else=0")
Osteoarthritis<-car::recode(Osteoarthritis,"1=1;else=0")
TreatOsteoarthritis<-car::recode(TreatOsteoarthritis,"1=1;else=0")
ActOsteoarthritis<-car::recode(ActOsteoarthritis,"1=1;else=0")
BackPain<-car::recode(BackPain,"1=1;else=0")
TreatBack<-car::recode(TreatBack,"1=1;else=0")
ActBack<-car::recode(ActBack,"1=1;else=0")
Rheumatoid<-car::recode(Rheumatoid,"1=1;else=0")
TreatRheumatoid<-car::recode(TreatRheumatoid,"1=1;else=0")
ActRheumatoid<-car::recode(ActRheumatoid,"1=1;else=0")

SCQ<-data.frame(HeartDisease,TreatHeart,ActHeart,HBPressure,TreatHB,ActHB,LungDisease,
                TreatLung,ActLung,Diabetes,TreatDiabetes,ActDiabetes,Ulcer,TreatUlcer,
                ActUlcer,Kidney,TreatKidney,ActKidney,LiverDisease,TreatLiver,ActLiver,
                Anemia,ActAnemia,Cancer,TreatCancer,ActCancer,Depression,TreatDepression,
                ActDepression,Osteoarthritis,TreatOsteoarthritis,ActOsteoarthritis,BackPain,
                TreatBack,ActBack,Rheumatoid,TreatRheumatoid,
                ActRheumatoid)


#ASSOCIATION WITH COMORBIDITY
Scatterplot<-data.frame(TestsScores,TestsNames,SCQSUM) 

qplot(TestsScores, SCQSUM, data=Scatterplot, colour=TestsNames) + theme_bw() + 
  geom_jitter() 

#Comparison between Comorbidity groups
TestsScores<-c(RolandMorrisScore,QuebecScore,WaddellScore,
               BackPainScore)
DepressionIndex<-c(Depression,Depression,Depression,Depression)
OsteoarthritisIndex<-c(Osteoarthritis,Osteoarthritis,Osteoarthritis,Osteoarthritis)
RheumatoidIndex<-c(Rheumatoid,Rheumatoid,Rheumatoid,Rheumatoid)
TestsNames<-c(RolandMorrisname,Quebecname,
              Waddellname,BackPainname)
Scores<-c(TestsScores,TestsScores,TestsScores)
Groups<-c(DepressionIndex,OsteoarthritisIndex,RheumatoidIndex)
Groups<-car::recode(Groups,"1='sim';0='não'")
Tests<-c(TestsNames,TestsNames,TestsNames) 
Comorbidities<-c(rep("Depression",1576),rep("Osteoarthritis",1576),rep("Rheumatoid",1576))
scqcomparison<-data.frame(Scores,Groups,Tests,Comorbidities)

qplot(Groups, Scores, data=scqcomparison, geom = "boxplot", fill=Groups) + 
  facet_grid(Comorbidities~ . ~ Tests) +
  xlab("") + ylab ("")+ theme(legend.position = "none")

#Test for comparison between comorbidity groups
ad.test(RolandMorrisScore)
ad.test(QuebecScore)
ad.test(WaddellScore)
ad.test(BackPainScore)

wilcox.test(RolandMorrisScore~data$Depression,paired=FALSE)
wilcox.test(RolandMorrisScore~data$Osteoarthritis,paired=FALSE)
wilcox.test(RolandMorrisScore~data$Rheumatoid,paired=FALSE)
wilcox.test(QuebecScore~data$Depression,paired=FALSE)
wilcox.test(QuebecScore~data$Osteoarthritis,paired=FALSE)
wilcox.test(QuebecScore~data$Rheumatoid,paired=FALSE)
wilcox.test(WaddellScore~data$Depression,paired=FALSE)
wilcox.test(WaddellScore~data$Osteoarthritis,paired=FALSE)
wilcox.test(WaddellScore~data$Rheumatoid,paired=FALSE)
wilcox.test(BackPainScore~data$Depression,paired=FALSE)
wilcox.test(BackPainScore~data$Osteoarthritis,paired=FALSE)
wilcox.test(BackPainScore~data$Rheumatoid,paired=FALSE)

#CORRELATION BETWEEN PAIN SCALES
#Evaluating normality of the data
#ANA ISSO É SÖ UMA AVALIA"CÃO DE NORMALIDADE. NADA DEMAIS.
ad.test(RolandMorrisScore)
ad.test(QuebecScore)
ad.test(WaddellScore)
ad.test(BackPainScore)

#COrrelations throuhg Spearman Correlation test
cor(data.frame(RolandMorrisScore,QuebecScore,WaddellScore,BackPainScore),method=c("spearman")) #Give correlation significance test

Pain<-data.frame(jitter(RolandMorrisScore),jitter(QuebecScore),
                 jitter(WaddellScore),jitter(BackPainScore))
library(TeachingDemos)
par(fig=c(0.65,1,0,0.8),new=TRUE)
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex * abs(r))
}

panel=function(x,y,...) {
  points(x,y,...)
  abline(lm(y~x), col='red')
}

pairs(Pain, labels=c("Roland Morris","Quebec", "Waddell", "Back Pain"), 
      upper.panel=panel, lower.panel=panel.cor,row1attop = TRUE, gap = 1)


# SEM with all scales (?)
#semdata<-remove.vars(efadata,names=c("MERE","MEIn", "MEId", "MIAO", "MIEE" ,"MIC",
#                                     "SPP", "SPM"))
#str(semdata)

#Defining dataframe with the data for the SEM model
PainSEM<-na.omit(data.frame(RolandMorrisOriginal,QuebecOriginal,
                          WaddellOriginal,BackPainOriginal))

#Specifying SEM model
#Padrão:
#1. Copiar todos as linhas dos modelos criadas na seção da CFA
#2. Modificar F1 pelo nome da escala
#3. Criar linhas de covariância (correlação entre as escalas)
#4. Criar variavel latente PAIN e definir modelo (cada escala predizendo "->" PAIN)
#5. Mudar nome das variáveis de cada escala (todas estão com a mesma sequencia var1,var2,var3)
#6. Mudar linha de comando da primeira variavel de cada escala de "var1,NA" para "NA,1"
#7. Mudar linha de comando do erro de cada escala (F1<->F1, ou neckdisability<->neckdisability)
# de "NA,1" para "err1,NA". O numero apos o "err" segue uma sequencia ordinal.
semmodel <- specifyModel()# Type these values that specify the model's relations (just use de Ctrl+R over each relation).

RolandMorris->Home,var11,NA
RolandMorris->Position,var12,NA
RolandMorris->Walk,var13,NA
RolandMorris->Jobs,var14,NA
RolandMorris->Handrail,var15,NA
RolandMorris->Liedown,var16,NA
RolandMorris->Holdon,var17,NA
RolandMorris->Otherpeople,var18,NA
RolandMorris->Dressed,var19,NA
RolandMorris->Periods,var110,NA
RolandMorris->Bend,var111,NA
RolandMorris->Getout,var112,NA
RolandMorris->Painful,var113,NA
RolandMorris->Turnover,var114,NA
RolandMorris->Appetite,var115,NA    
RolandMorris->Socks,var116,NA
RolandMorris->Distances,var117,NA
RolandMorris->Lesswell,var118,NA
RolandMorris->Someone,var119,NA
RolandMorris->Sitdown,var120,NA
RolandMorris->Heavy,var121,NA     
RolandMorris->Irritable,var122,NA
RolandMorris->Upstairs,var123,NA
RolandMorris->Bed,var124,NA       
RolandMorris<->RolandMorris,NA,1
Quebec->Getoutbed,var231,NA
Quebec->Sleep,var232,NA
Quebec->Turn.over,var233,NA
Quebec->Ride,var234,NA
Quebec->Standup,var235,NA
Quebec->Chair,var236,NA
Quebec->Climb,var237,NA
Quebec->Fewblocks,var238,NA
Quebec->Severalmiles,var239,NA
Quebec->Shelves,var240,NA
Quebec->Ball,var241,NA
Quebec->Run,var242,NA
Quebec->Food,var243,NA
Quebec->Makebed,var244,NA    
Quebec->Pantyhose,var245,NA
Quebec->Bathtub,var246,NA
Quebec->Movechair,var247,NA
Quebec->Heavydoors,var248,NA
Quebec->Carrybags,var249,NA
Quebec->Suitcase,var250,NA     
Quebec<->Quebec,NA,1
Waddell->Lifting,var351,NA
Waddell->Sitting,var352,NA
Waddell->Travelling,var353,NA
Waddell->Standing,var354,NA
Waddell->Walking,var355,NA
Waddell->Disturbed,var356,NA
Waddell->Social,var357,NA
Waddell->Sexual,var358,NA
Waddell->Footwear,var359,NA
Waddell<->Waddell,NA,1
Backpain->Housework,var461,NA
Backpain->Hobbies,var462,NA
Backpain->Performing,var463,NA
Backpain->Bending,var464,NA
Backpain->Shoes,var465,NA
Backpain->Groceries,var466,NA
Backpain->Sleeping,var467,NA
Backpain->Standing1h,var468,NA
Backpain->Walkingmile,var469,NA
Backpain->Goingup,var470,NA
Backpain->Sitting1h,var471,NA
Backpain->Driving,var472,NA
Backpain<->Backpain,NA,1
Backpain->Pain,2order1,NA
Waddell->Pain,2order2,NA
Quebec->Pain,2order3,NA
RolandMorris->Pain,2order4,NA
Pain<->Pain,err5,NA
Backpain<->Waddell,NA,1
Backpain<->Quebec,NA,1
Backpain<->RolandMorris,NA,1
Waddell<->Quebec,NA,1
Waddell<->RolandMorris,NA,1
Quebec<->RolandMorris,NA,1

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
cor <- hetcor(PainSEM)$correlations

# Estimate de model (Here is where we have been finding difficulties)
sem <- sem(semmodel, cov, N=394)
sem2 <- sem(semmodel, cor, N=394)
summary(sem2) # Copiar e colar no texto
effects(semPMC)
standardizedCoefficients(sem) #Copiar e colar texto
# calcula ?ndices de modifica??o (testes de pontua??o) e altera??es de par?metros
# estimados para os par?metros fixos e limitados em um modelo de equa??es estruturais
modIndices(sem)

##########################################################################
