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
#RECODING VARIABLES
##########################################################################

######## ROLAND MORRIS PAIN SCALE ############
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

######## QUEBEC PAIN SCALE ############
#irt modeling with 3 categories of response
  ## Trying to recode the data
Getoutbed<-car::recode(Getoutbed,"1=0;2=0;3=0;4=1;5=1;6=1")
Sleep<-car::recode(Sleep,"1=0;2=0;3=0;4=1;5=1;6=1")
Turn.over<-car::recode(Turn.over,"1=0;2=0;3=0;4=1;5=1;6=1")
Ride<-car::recode(Ride,"1=0;2=0;3=0;4=1;5=1;6=1")
Standup<-car::recode(Standup,"1=0;2=0;3=0;4=1;5=1;6=1")
Chair<-car::recode(Chair,"1=0;2=0;3=0;4=1;5=1;6=1")
Climb<-car::recode(Climb,"1=0;2=0;3=0;4=1;5=1;6=1")
Fewblocks<-car::recode(Fewblocks,"1=0;2=0;3=0;4=1;5=1;6=1")
Severalmiles<-car::recode(Severalmiles,"1=0;2=0;3=0;4=1;5=1;6=1")
Shelves<-car::recode(Shelves,"1=0;2=0;3=0;4=1;5=1;6=1")
Ball<-car::recode(Ball,"1=0;2=0;3=0;4=1;5=1;6=1")
Run<-car::recode(Run,"1=0;2=0;3=0;4=1;5=1;6=1")
Food<-car::recode(Food,"1=0;2=0;3=0;4=1;5=1;6=1")
Makebed<-car::recode(Makebed,"1=0;2=0;3=0;4=1;5=1;6=1")
Pantyhose<-car::recode(Pantyhose,"1=0;2=0;3=0;4=1;5=1;6=1")
Bathtub<-car::recode(Bathtub,"1=0;2=0;3=0;4=1;5=1;6=1")
Movechair<-car::recode(Movechair,"1=0;2=0;3=0;4=1;5=1;6=1")
Heavydoors<-car::recode(Heavydoors,"1=0;2=0;3=0;4=1;5=1;6=1")
Carrybags<-car::recode(Carrybags,"1=0;2=0;3=0;4=1;5=1;6=1")
Suitcase<-car::recode(Suitcase,"1=0;2=0;3=0;4=1;5=1;6=1")
  QuebecOriginal<-data.frame(Getoutbed,Sleep,Turn.over,Ride,Standup,Chair,Climb,Fewblocks,
                             Severalmiles,Shelves,Ball,Run,Food,Makebed,Pantyhose,Bathtub,
                             Movechair,Heavydoors,Carrybags,Suitcase)


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

######## BACKPAIN SCALE ############

### TRYOUT WITH ALL ORIGINAL 6 CATEGORIES OF RESPONSES
#Recoding Variables to match scales original categories of responses
Housework<-car::recode(Housework,"1=0;2=0;3=0;4=1;5=1;6=1")
Hobbies<-car::recode(Hobbies,"1=0;2=0;3=0;4=1;5=1;6=1")
Performing<-car::recode(Performing,"1=0;2=0;3=0;4=1;5=1;6=1")
Bending<-car::recode(Bending,"1=0;2=0;3=0;4=1;5=1;6=1")
Shoes<-car::recode(Shoes,"1=0;2=0;3=0;4=1;5=1;6=1")
Groceries<-car::recode(Groceries,"1=0;2=0;3=0;4=1;5=1;6=1")
Sleeping<-car::recode(Sleeping,"1=0;2=0;3=0;4=1;5=1;6=1")
Standing1h<-car::recode(Standing1h,"1=0;2=0;3=0;4=1;5=1;6=1")
Walkingmile<-car::recode(Walkingmile,"1=0;2=0;3=0;4=1;5=1;6=1")
Goingup<-car::recode(Goingup,"1=0;2=0;3=0;4=1;5=1;6=1")
Sitting1h<-car::recode(Sitting1h,"1=0;2=0;3=0;4=1;5=1;6=1")
Driving<-car::recode(Driving,"1=0;2=0;3=0;4=1;5=1;6=1")
BackPainOriginal<-data.frame(Housework,Hobbies,Performing,Bending,Shoes,Groceries,Sleeping,
                             Standing1h,Walkingmile,Goingup,Sitting1h,Driving)

##########################################################################
#CFA
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

Pain->Home,var112,NA
Pain->Position,var125,NA
Pain->Walk,var113,NA
Pain->Jobs,var114,NA
Pain->Handrail,var115,NA
Pain->Liedown,var116,NA
Pain->Holdon,var117,NA
Pain->Otherpeople,var118,NA
Pain->Dressed,var119,NA
Pain->Periods,var110,NA
Pain->Bend,var111,NA
Pain->Getout,var112,NA
Pain->Painful,var113,NA
Pain->Turnover,var114,NA
Pain->Appetite,var115,NA    
Pain->Socks,var116,NA
Pain->Distances,var117,NA
Pain->Lesswell,var118,NA
Pain->Someone,var119,NA
Pain->Sitdown,var120,NA
Pain->Heavy,var121,NA     
Pain->Irritable,var122,NA
Pain->Upstairs,var123,NA
Pain->Bed,var124,NA       
Pain->Getoutbed,var231,NA
Pain->Sleep,var232,NA
Pain->Turn.over,var233,NA
Pain->Ride,var234,NA
Pain->Standup,var235,NA
Pain->Chair,var236,NA
Pain->Climb,var237,NA
Pain->Fewblocks,var238,NA
Pain->Severalmiles,var239,NA
Pain->Shelves,var240,NA
Pain->Ball,var241,NA
Pain->Run,var242,NA
Pain->Food,var243,NA
Pain->Makebed,var244,NA    
Pain->Pantyhose,var245,NA
Pain->Bathtub,var246,NA
Pain->Movechair,var247,NA
Pain->Heavydoors,var248,NA
Pain->Carrybags,var249,NA
Pain->Suitcase,var250,NA     
Pain->Lifting,var351,NA
Pain->Sitting,var352,NA
Pain->Travelling,var353,NA
Pain->Standing,var354,NA
Pain->Walking,var355,NA
Pain->Disturbed,var356,NA
Pain->Social,var357,NA
Pain->Sexual,var358,NA
Pain->Footwear,var359,NA
Pain->Housework,var461,NA
Pain->Hobbies,var462,NA
Pain->Performing,var463,NA
Pain->Bending,var464,NA
Pain->Shoes,var465,NA
Pain->Groceries,var466,NA
Pain->Sleeping,var467,NA
Pain->Standing1h,var468,NA
Pain->Walkingmile,var469,NA
Pain->Goingup,var470,NA
Pain->Sitting1h,var471,NA
Pain->Driving,var472,NA
Pain<->Pain,NA,1
#end of the model

cor <- polychoric(PainSEM)$rho

# Estimate de model (Here is where we have been finding difficulties)
#sem <- sem(semmodel, cov, N=394)
sem2 <- sem(semmodel, cor, N=394)
summary(sem2) # Copiar e colar no texto
effects(semPMC)
standardizedCoefficients(sem) #Copiar e colar texto
# calcula ?ndices de modifica??o (testes de pontua??o) e altera??es de par?metros
# estimados para os par?metros fixos e limitados em um modelo de equa??es estruturais
modIndices(sem)
residuals(sem2)
##########################################################################
#IRT Calibration
##########################################################################
#ITEM RESPONSE THEORY
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
plotPImap(irt_model)
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