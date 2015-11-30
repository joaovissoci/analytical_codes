##########################################################################
#SETTING ENVIRONMENT
##########################################################################
#remove all objects and then check

#Instal packages needes for the analysis
lapply(c("ggplot2", "psych", "RCurl", "irr", "nortest", "moments","nFactors","psych","ltm", "GPArotation", "gmodels","sem","eRm","mirt","googlesheets","dplyr","devtools","mirtCAT","catR","catIrt"), library, character.only=T)
############################################################
#IMPORTING DATA
############################################################

#uploading data ------------------------------------------------------------------------
data<-read.csv("/home/joao/Dropbox/datasets/neck_pain_dataset.csv")

#detach(data)
#attach(data)

##########################################################################
#RECODING VARIABLES
##########################################################################
######## NECK DISABILLITY INDEX SCALE ############
# 0 to 6 
data$Pain<-car::recode(data$Pain,"0=0;1=0;2=0;3:6=1")
data$PersonalCare<-car::recode(data$PersonalCare,"0=0;1=0;2=0;3:6=1")
data$Lifting<-car::recode(data$Lifting,"0=0;1=0;2=0;3:6=1")
data$Reading<-car::recode(data$Reading,"0=0;1=0;2=0;3:6=1")
data$Headaches<-car::recode(data$Headaches,"0=0;1=0;2=0;3:6=1")
data$Concentration<-car::recode(data$Concentration,"0=0;1=0;2=0;3:6=1")
data$Work<-car::recode(data$Work,"0=0;1=0;2=0;3:6=1")
data$Driving<-car::recode(data$Driving,"0=0;1=0;2=0;3:6=1")
data$Sleeping<-car::recode(data$Sleeping,"0=0;1=0;2=0;3:6=1")
data$Recreation<-car::recode(data$Recreation,"0=0;1=0;2=0;3:6=1")

NeckDisabilityIndexOriginal<-with(data,data.frame(Pain,PersonalCare,Lifting,Reading,Headaches,Concentration,Work,Driving,Sleeping,Recreation))

######## NORTHWICK SCALE ############
# 0 to 4
data$Intensity<-car::recode(data$Intensity,"1:2=0;3:5=1")
data$Sleep<-car::recode(data$Sleep,"1:2=0;3:5=1")
data$Pin<-car::recode(data$Pin,"1:2=0;3:5=1")
data$Symptons<-car::recode(data$Symptons,"1:2=0;3:5=1")
data$Carry<-car::recode(data$Carry,"1:2=0;3:5=1")
data$Wachting<-car::recode(data$Wachting,"1:2=0;3:5=1")
data$Housework<-car::recode(data$Housework,"1:2=0;3:5=1")
data$Social<-car::recode(data$Social,"1:2=0;3:5=1")
data$Drive<-car::recode(data$Drive,"1:2=0;3:5=1")

NorthwickParkNeckPainQuestionnaireOriginal<-with(data,data.frame(Intensity,Sleep,Pin,Symptons,Carry,Wachting,Housework,Social,Drive))

######## COPENHAGEN SCALE ############
#need to be inverted 0 = Yes
data$Interfering<-car::recode(data$Interfering,"3=0;1:2=1")
data$DailyActivities<-car::recode(data$DailyActivities,"3=0;1:2=1")
data$ManageHelp<-car::recode(data$ManageHelp,"3=0;1:2=1")
data$ClothesMorning<-car::recode(data$ClothesMorning,"3=0;1:2=1")
data$BrushTeeth<-car::recode(data$BrushTeeth,"3=0;1:2=1")
data$SpendTime<-car::recode(data$SpendTime,"3=0;1:2=1")
data$LiftingObjects<-car::recode(data$LiftingObjects,"3=0;1:2=1")
data$ReadingActivity<-car::recode(data$ReadingActivity,"3=0;1:2=1")
data$BotheredHeadaches<-car::recode(data$BotheredHeadaches,"3=0;1:2=1")
data$Ability<-car::recode(data$Ability,"3=0;1:2=1")
data$Leisure<-car::recode(data$Leisure,"3=0;1:2=1")
data$Bed<-car::recode(data$Bed,"3=0;1:2=1")
data$Emotional<-car::recode(data$Emotional,"3=0;1:2=1")
data$Shoes<-car::recode(data$Shoes,"3=0;1:2=1")
data$GiveUp<-car::recode(data$GiveUp,"3=0;1:2=1")

CopenhagenNeckFunctionalDisabilityScaleOriginal<-with(data,data.frame(Interfering,DailyActivities,ManageHelp,ClothesMorning,BrushTeeth,SpendTime,LiftingObjects,ReadingActivity,BotheredHeadaches,Ability,Leisure,Bed,Emotional,Shoes,GiveUp))

######## NECK BOURNEMOUTH SCALE ############
# 0 to 10
data$RateNeckPain<-car::recode(data$RateNeckPain,"0:5=0;6:10=1")
data$InterferedDailyActivities<-car::recode(data$InterferedDailyActivities,"0:5=0;6:10=1")
data$Recreational<-car::recode(data$Recreational,"0:5=0;6:10=1")
data$Anxious<-car::recode(data$Anxious,"0:5=0;6:10=1")
data$Depressed<-car::recode(data$Depressed,"0:5=0;6:10=1")
data$YourWork<-car::recode(data$YourWork,"0:5=0;6:10=1")
data$Control<-car::recode(data$Control,"0:5=0;6:10=1")

NeckBournemouthQuestionnaireOriginal<-with(data,data.frame(RateNeckPain,InterferedDailyActivities,Recreational,Anxious,Depressed,YourWork,Control))

##########################################################################
#CFA
##########################################################################
#Defining dataframe with the data for the SEM model
PainSEM<-na.omit(data.frame(NeckDisabilityIndexOriginal,NorthwickParkNeckPainQuestionnaireOriginal,CopenhagenNeckFunctionalDisabilityScaleOriginal,NeckBournemouthQuestionnaireOriginal))
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

neckpain->BotheredHeadaches,var111,NA
neckpain->Pain,var1,NA
neckpain->PersonalCare,var2,NA
neckpain->Lifting,var3,NA
neckpain->Reading,var4,NA
neckpain->Headaches,var5,NA
neckpain->Concentration,var6,NA
neckpain->Work,var7,NA
neckpain->Driving,var8,NA
neckpain->Sleeping,var9,NA
neckpain->Recreation,var10,NA
neckpain->Intensity,NA,1
neckpain->Sleep,var11,NA
neckpain->Pin,var12,NA
neckpain->Symptons,var13,NA
neckpain->Carry,var14,NA
neckpain->Wachting,var15,NA
neckpain->Housework,var17,NA
neckpain->Social,var18,NA
neckpain->Drive,var19,NA
neckpain->Interfering,var21,NA
neckpain->DailyActivities,var22,NA
neckpain->ManageHelp,var23,NA
neckpain->ClothesMorning,var24,NA
neckpain->BrushTeeth,var25,NA
neckpain->SpendTime,var26,NA
neckpain->LiftingObjects,var27,NA
neckpain->ReadingActivity,var28,NA
neckpain->Ability,var20,NA
neckpain->Leisure,var21,NA
neckpain->Bed,var32,NA
neckpain->Emotional,var33,NA
neckpain->Shoes,var34,NA
neckpain->GiveUp,var35,NA      
neckpain->RateNeckPain,var36,NA
neckpain->InterferedDailyActivities,var42,NA
neckpain->Recreational,var43,NA
neckpain->Anxious,var44,NA
neckpain->Depressed,var45,NA
neckpain->YourWork,var46,NA
neckpain->Control,var47,NA
neckpain<->neckpain,NA,1
#end of the model

# Insert de covariance matrix
#cov <- cov(PainSEM, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))
cor <- polychoric(PainSEM)$rho

# Estimate de model (Here is where we have been finding difficulties)
sem <- sem(semmodel, cov, N=536)
sem2 <- sem(semmodel, cor, N=536)
summary(sem2) # Copiar e colar no texto
effects(semPMC)
standardizedCoefficients(sem) #Copiar e colar texto
# calcula ?ndices de modifica??o (testes de pontua??o) e altera??es de par?metros
# estimados para os par?metros fixos e limitados em um modelo de equa??es estruturais
modIndices(sem2)
residuals(sem2)
##########################################################################
#IRT Calibration
##########################################################################
#ITEM RESPONSE THEORY
colnames(PainSEM)<-c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10",
  "Q11","Q12","Q13","Q14","Q15","Q16","Q17","Q18","Q19","Q20",
  "Q21","Q22","Q23","Q24","Q25","Q26","Q27","Q28","Q29","Q30",
  "Q31","Q32","Q33","Q34","Q35","Q36","Q37","Q38","Q39","Q40","Q41")
#ploytomous
irt_model<-mirt(PainSEM,1,itemtype="Rasch")
irt_model<-RM(PainSEM)
summary(irt_model)
summary(irt_model$betapar)*-1
summary(irt_model$se.eta)
coef(irt_model)
confint(irt_model,"beta")
itemplot(irt_model,2)
plotI(irt_model,i=3)
plotICC(irt_model,item.subset=1:4,ask=F,empICC=list("raw"),empCI=list(lty="solid"))
plotPImap(irt_model,main = "Mapa da Itens",latdim = "Dimensão Latente",pplabel = "Parametros\nPearson")
pp<-person.parameter(irt_model)
lrt<-LRtest(irt_model,se=TRUE)
Waldtest(irt_model)
eRm::itemfit(pp)
summary(eRm::itemfit(pp)$i.outfitMSQ)
sd(eRm::itemfit(pp)$i.outfitMSQ)
summary(eRm::itemfit(pp)$i.infitMSQ)
sd(eRm::itemfit(pp)$i.infitMSQ)
NPtest(irt_model,method="T11")
plotGOF(lrt,conf=list())

iteminfo(item_plot,)
##########################################################################
#Reliability
##########################################################################
#########Alpha de Cronbach by ltm package - GIves CI
cronbach.alpha(PainSEM, standardized = TRUE, CI = TRUE, 
               probs = c(0.025, 0.975), B = 1000, na.rm = FALSE)

alpha(BackPainOriginal)

#Composite Reliabilty
sum(x[1:12,2])^2/(sum(x[1:12,2])^2+sum(x[16:27,2]))

##########################################################################
#CAT and Simulation
##########################################################################
library('mirtCAT')
options(stringsAsFactors = FALSE)

# define population IRT parameters
set.seed(1234)
nitems <- 100
itemnames <- paste0('Item.', 1:nitems)
a <- matrix(rlnorm(nitems, .2, .3))
d <- matrix(rnorm(nitems))
pars <- data.frame(a1=a, d=d, g=0.2)
mod <- generate.mirt_object(pars, 'Rasch')

# math items definitions
# addition for one factor and multiplication for the other
questions <- answers <- character(nitems)
choices <- matrix('a', nitems, 5)
spacing <- floor(d - min(d)) + 1 #easier items have more variation

for(i in 1:nitems){
    n1 <- sample(1:100, 1)
    n2 <- sample(101:200, 1)
    ans <- n1 + n2
    questions[i] <- paste0(n1, ' + ', n2, ' = ?')
    answers[i] <- as.character(ans)
    ch <- ans + sample(c(-5:-1, 1:5) * spacing[i,], 5)
    ch[sample(1:5, 1)] <- ans
    choices[i,] <- as.character(ch)
}

df <- data.frame(Questions=questions, Answer=answers, Option=choices, Type='radio')
head(df)

plot(irt_model)

plot(irt_model, type = 'infoSE', theta_lim=c(-3,3))

result <- mirtCAT(df2, irt_model, method = 'EAP', criteria = 'MI', start_item = 'random',design = list(min_SEM = 0.5))
print(result)
plot(result)
summary(result)


df2<-read.csv("/home/joao/Desktop/cat_questions.csv")
df2$Type <- rep('radio',length(df2[1]))

## generate random response pattern
set.seed(1)
pattern <- generate_pattern(irt_model, Theta = 1)
head(pattern[1L,])

# use adaptive method using Kullback-Leibler item selection with root-N adjustment
#   with MAP estimation of latent traits
result <- mirtCAT(mo=irt_model, local_pattern=pattern, method = 'EAP', criteria = 'MI', start_item = 'random',design = list(min_SEM = 0.5))

result <- mirtCAT(df2, irt_model, method = 'EAP', criteria = 'MI', start_item = 'random',
                  design = list(min_SEM = 0.5))

print(result)
plot(result)
summary(result)

pat <- generate_pattern(irt_model, Theta = 0)
x<-mirtCAT(mo=irt_model, local_pattern = pat)

pat3 <- generate_pattern(irt_model, Theta = matrix(c(0, 2, -2), 3))
x<-mirtCAT(mo=irt_model, local_pattern = pat3)




library('mirtCAT')

set.seed(1)
bank <- 250 #bank size
N <- 500 #calibration sample size

a <- matrix(rlnorm(bank*2, .2,.3), bank)
a[1:75, 1] <- a[250:175, 2] <- 0
d <- matrix(seq(1.25, -1.25, length.out=4), bank, 4, byrow=TRUE) + rnorm(bank)
pars <- data.frame(a, d)
colnames(pars) <- c('a1', 'a2', paste0('d', 1:4))
mod <- generate.mirt_object(pars, itemtype = 'graded')
head(coef(irt_model, simplify=TRUE)$items)

## generate random response pattern
set.seed(1)
pattern <- generate_pattern(irt_model, Theta = 1)
head(pattern[1L,])

# use adaptive method using Kullback-Leibler item selection with root-N adjustment
#   with MAP estimation of latent traits
result <- mirtCAT(mo=irt_model, local_pattern=pattern, criteria='KLn',
                  design = list(min_SEM=.2), method = 'MAP')

print(result)
plot(result, scales = list(x = list(at = NULL)), SE = 1.96) #95% confidence interval


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


## Dichotomous IRT model ##
# Loading the 'tcals' parameters
data(tcals)
bank <- as.matrix(tcals[,1:4])
# Creation of a starting list: 3 items, initial theta 0, bw 2
start <- list(nrItems = 3, theta = 0, halfRange = 2)
# Creation of 'test' list: maximum likelihood estimation and
# progressive method
test <- list(method = "ML", itemSelect = "progressive")
# Creation of a stopping rule: precision criterion, standard
# error to be reached 0.3
stop <- list(rule = "precision", thr = 0.3)
# Creation of 'final' list: ML estimation of final ability
final <- list(method = "ML")
# Generation of ten respondents
set.seed(1)
thetas <- rnorm(10)
# Default CAT generations, output not saved
res <- simulateRespondents(thetas, bank, start = start, test = test, stop = stop, final = final)
# Maximum exposure restricted to 0.8
res2 <- simulateRespondents(thetas, bank, start = start, test = test, stop = stop, final = final, rmax = 0.8)
# Output saved
#res3 <- simulateRespondents(thetas, bank, start = start, test = test, stop = stop, final = final, save.output = TRUE, output = c("C:/Program Files/", "out", "txt"))
# With content balancing #
# Creation of an appropriate list for content balancing
# Equal proportions across subgroups of items
cbList <- list(names = c("Audio1", "Audio2", "Written1", "Written2", "Written3"),props = c(0.1, 0.2, 0.2, 0.2, 0.3))
# CAT test (same options as above)
res4 <- simulateRespondents(thetas, tcals, start = start, test = test, stop = stop, final = final, cbControl = cbList)
# Plotting and saving output #
# Plotting all possible panels
plot(res)
plot(res, deciles = "deciles")
# Saving the plot in the "fig" pdf file in "c:/Program Files/"
plot(res, save.plot = TRUE, save.options = c("c:/Program Files/", "fig", "pdf"))
# Plotting the 'trueEst' type of plot
plot(res, type = "trueEst")


#########################
# Binary Response Model #
#########################
library(catIrt)
set.seed(888)
# generating random theta:
theta <- rnorm(1000)
# generating an item bank under a 2-parameter binary response model:
b.params <- cbind(a = 1, b = irt_model$betapar*-1, c = 0)
# simulating responses:
b.resp <- simIrt(theta = theta, params = b.params, mod = "brm")$resp

## CAT 1 ##
# the typical, classic post-hoc CAT:
catStart1 <- list(init.theta = 0, n.start = 5,
select = "UW-FI", at = "theta",
n.select = 4, it.range = c(-1, 1),
score = "step", range = c(-1, 1),
step.size = 3, leave.after.MLE = FALSE)
catMiddle1 <- list(select = "UW-FI", at = "theta",
n.select = 1, it.range = NULL,
score = "MLE", range = c(-6, 6),
expos = "none")
catTerm1 <- list(term = "fixed", n.min = 10, n.max = 20)
cat1 <- catIrt(params = b.params, mod = "brm",
resp = b.resp,
catStart = catStart1,
catMiddle = catMiddle1,
catTerm = catTerm1)
# we can print, summarize, and plot:
cat1 # prints theta because
# we have fewer than
# 200 simulees
summary(cat1, group = TRUE, ids = "none") # nice summary!
summary(cat1, group = FALSE, ids = 1:4) # summarizing people too! :)
par(mfrow = c(2, 2))
plot(cat1, ask = FALSE) # 2-parameter model, so expected FI
# and observed FI are the same
par(mfrow = c(1, 1))
# we can also plot particular simulees:
par(mfrow = c(2, 1))
plot(cat1, which = "none", ids = c(1, 30), ask = FALSE)
par(mfrow = c(1, 1))

## CAT 2 ##
# using Fixed Point KL info rather than Unweighted FI to select items:
catStart2 <- catStart1
catMiddle2 <- catMiddle1
catTerm2 <- catTerm1
catStart2$leave.after.MLE <- TRUE # leave after mixed response pattern
catMiddle2$select <- "FP-KL"
catMiddle2$at <- "bounds"
catMiddle2$delta <- .2
catTerm2$c.term <- list(bounds = 0)
cat2 <- catIrt(params = b.params, mod = "brm",
resp = b.resp,
catStart = catStart2,
catMiddle = catMiddle2,
catTerm = catTerm2)
cor(cat1$cat_theta, cat2$cat_theta) # very close!
summary(cat2, group = FALSE, ids = 1:4) # rarely 5 starting items!
summary(cat2, group = TRUE, ids = "none") # nice summary!

## CAT 3/4 ##
# using "precision" rather than "fixed" to terminate:
catTerm1 <- list(term ="precision", n.min = 5, n.max = 50,p.term=list(method="threshold",crit=.40))
#catTerm1$term <- catTerm2$term <- "precision"
#catTerm1$p.term <- list()
cat3 <- catIrt(params = b.params, mod = "brm",
resp = b.resp,
catStart = catStart1,
catMiddle = catMiddle1,
catTerm = catTerm1)

cat4 <- catIrt(params = b.params, mod = "brm",
resp = b.resp,
catStart = catStart2,
catMiddle = catMiddle2,
catTerm = catTerm2)
mean(cat3$cat_length - cat4$cat_length) # KL info results in slightly more items
summary(cat3, group = TRUE, ids = "none") # nice summary!
summary(cat1, group = FALSE, ids = 1:4) # summarizing people too! :)
par(mfrow = c(2, 2))
plot(cat3, ask = FALSE) # 2-parameter model, so expected FI
# and observed FI are the same
par(mfrow = c(1, 1))
# we can also plot particular simulees:
par(mfrow = c(2, 1))
plot(cat3, which = "none", ids = "all", ask = FALSE)
par(mfrow = c(1, 1))