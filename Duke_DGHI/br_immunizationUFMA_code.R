###################################################
#TEMPLATE_FOR _META_ANALYSIS_OF_DIAGNOSTIC_ACCURACY#
#this script follows a combination of guidelines proposed by Doebler and Holling, according to (http://cran.r-project.org/web/packages/mada/vignettes/mada.pdf)#
#
#
###################################################
#SETTING ENVIRONMENT
###################################################

# install.packages("readstata13")
#All packages must be installes with install.packages() function
lapply(c("poLCA","ggplot2","devtools","dplyr",
		 "scales","tibble","fmsb","scales","reshape","mice"),
library, character.only=T)
# library("haven")
###################################################
#IMPORTING DATA AND RECODING
###################################################
data <- read.csv("/Users/joaovissoci/Downloads/br_immunizationLCA_data.csv")
# data<-as.data.frame(data)

# data[] <- lapply(data, unclass)

#recode missing and other random problems
# data$Gender<-car::recode(data$Gender,"'male'=0;'female'=1")

# dicotomize<-function(x){
# 	car::recode(x,"0=0;NA=NA;else=1")
# }

# barriers_dic<-sapply(barriers,
# 	function(x) dicotomize(x))

lca_data<-with(data,data.frame(
	periodo_func2,
	qt_enf,
	s_vacina,
	ar_vacina,
	geladeira_vacina,
	equip_vaci_adeq,
	cx_termica,
	termometro_max,
	rec_perfuro,
	seringa,
	agulha,
	cartao_vacina,
	BCG,
	DUPLA,
	HEPATITE_B,
	ROTAVIRUS,
	POLIO,
	TETRA,
	PNEUMO10,
	MENINGO_C,
	PENUMO23,
	TRIPLICE_B,
	TRIPLICE_V,
	febre_amarela_mod,
	INFLUENZA_mod
	))

str(lca_data)

#Studying missing data
#Calculating frequency of missing data per variable
propmiss <- function(dataframe) lapply(dataframe,function(x) data.frame(nmiss=sum(is.na(x)), n=length(x), propmiss=sum(is.na(x))/length(x)))

propmiss(lca_data)

# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
imp <- mice(lca_data, seed = 2222, m=15)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
lca_data_imputed<-mice::complete(imp,4)

###################################################
#Table 1
###################################################

# Age
summary(data_ses$Age)
# describe(data_ses$Age)
by(data_ses$Age,data_ses$Untreated,summary)
# t-test: # independent 2-group, 2 level IV
wilcox.test(data_ses$Age ~ data_ses$Untreated)

# Gender
table<-table(data_ses$Gender)
table
prop.table(table)
#
table<-table(data_ses$Gender,data_ses$Untreated)
table
prop.table(table,2)
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package
# logmodel<-glm(victimis_outcome_driversonly ~   as.factor(victms_gender),family=binomial)
# summary(logmodel)
# #anova(reglogGEU)
# exp(coef(logmodel)) # exponentiated coefficients
# exp(confint(logmodel)) # 95% CI for exponentiated coefficients
# #predict(model1_death, type="response") # predicted values
# #residuals(model1_death, type="deviance") # residuals
# logistic.display(logmodel)

# location
table<-table(data_ses$E15_rural)
table
prop.table(table)
#
table<-table(data_ses$E15_rural,data_ses$Untreated)
table
prop.table(table,2)
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package
# logmodel<-glm(victimis_outcome_driversonly ~   as.factor(victms_gender),family=binomial)
# summary(logmodel)
# #anova(reglogGEU)
# exp(coef(logmodel)) # exponentiated coefficients
# exp(confint(logmodel)) # 95% CI for exponentiated coefficients
# #predict(model1_death, type="response") # predicted values
# #residuals(model1_death, type="deviance") # residuals
# logistic.display(logmodel)

# Household size
summary(data_ses$Household)
# describe(data_ses$Age)
by(data_ses$Household,data_ses$Untreated,summary)
# t-test: # independent 2-group, 2 level IV
testName <- t.test(age_victims ~ victimis_outcome_all)

# Education
table<-table(data_ses$Education)
table
prop.table(table)
#
table<-table(data_ses$Education,data_ses$Untreated)
table
prop.table(table,2)
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package
# logmodel<-glm(victimis_outcome_driversonly ~   as.factor(victms_gender),family=binomial)
# summary(logmodel)
# #anova(reglogGEU)
# exp(coef(logmodel)) # exponentiated coefficients
# exp(confint(logmodel)) # 95% CI for exponentiated coefficients
# #predict(model1_death, type="response") # predicted values
# #residuals(model1_death, type="deviance") # residuals
# logistic.display(logmodel)

# Literacy
table<-table(data_ses$Literacy)
table
prop.table(table)
#
table<-table(data_ses$Literacy,data_ses$Untreated)
table
prop.table(table,2)
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package
# logmodel<-glm(victimis_outcome_driversonly ~   as.factor(victms_gender),family=binomial)
# summary(logmodel)
# #anova(reglogGEU)
# exp(coef(logmodel)) # exponentiated coefficients
# exp(confint(logmodel)) # 95% CI for exponentiated coefficients
# #predict(model1_death, type="response") # predicted values
# #residuals(model1_death, type="deviance") # residuals
# logistic.display(logmodel)

# Occupation
table<-table(data_ses$Occupation)
table
prop.table(table)
#
table<-table(data_ses$Occupation,data_ses$Untreated)
table
prop.table(table,2)
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package
# logmodel<-glm(victimis_outcome_driversonly ~   as.factor(victms_gender),family=binomial)
# summary(logmodel)
# #anova(reglogGEU)
# exp(coef(logmodel)) # exponentiated coefficients
# exp(confint(logmodel)) # 95% CI for exponentiated coefficients
# #predict(model1_death, type="response") # predicted values
# #residuals(model1_death, type="deviance") # residuals
# logistic.display(logmodel)

# Time ill
table<-table(data_ses$Time_ill)
table
prop.table(table)
#
table<-table(data_ses$Time_ill,data_ses$Untreated)
table
prop.table(table,2)
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package
# logmodel<-glm(victimis_outcome_driversonly ~   as.factor(victms_gender),family=binomial)
# summary(logmodel)
# #anova(reglogGEU)
# exp(coef(logmodel)) # exponentiated coefficients
# exp(confint(logmodel)) # 95% CI for exponentiated coefficients
# #predict(model1_death, type="response") # predicted values
# #residuals(model1_death, type="deviance") # residuals
# logistic.display(logmodel)

# Health status
table<-table(data_ses$Health_status)
table
prop.table(table)
#
table<-table(data_ses$Health_status,data_ses$Untreated)
table
prop.table(table,2)
# chisq.test(table)
# fisher.test(table)
# assocstats(table) #vcd package
# logmodel<-glm(victimis_outcome_driversonly ~   as.factor(victms_gender),family=binomial)
# summary(logmodel)
# #anova(reglogGEU)
# exp(coef(logmodel)) # exponentiated coefficients
# exp(confint(logmodel)) # 95% CI for exponentiated coefficients
# #predict(model1_death, type="response") # predicted values
# #residuals(model1_death, type="deviance") # residuals
# logistic.display(logmodel)

###################################################
#Latent class analysis
###################################################

## Questions
# http://rfunction.com/archives/1499
# https://drive.google.com/open?id=0B4TReYGK49h_X09ZYno1OG5aUVk

# Correlation

# Building the formula
# To specify a latent class model, poLCA uses the standard, symbolic R model formula expres- sion. The response variables are the manifest variables of the model. Because latent class models have multiple manifest variables, these variables must be “bound” as cbind(Y1, Y2, Y3, ...) in the model formula. For the basic latent class model with no covariates, the formula definition takes the form

lca_data_cat<-sapply(lca_data_imputed,function(x) as.factor(x))
lca_data_cat<-as.data.frame(lca_data_cat)
lca_data_cat<-data.frame(lca_data_cat)

f <- cbind(	periodo_func2,
			qt_enf,
			s_vacina,
			ar_vacina,
			geladeira_vacina,
			# equip_vaci_adeq,
			cx_termica,
			termometro_max,
			rec_perfuro,
			seringa,
			agulha,
			cartao_vacina,
			BCG,
			DUPLA,
			HEPATITE_B,
			ROTAVIRUS,
			POLIO,
			TETRA,
			PNEUMO10,
			MENINGO_C,
			PENUMO23,
			TRIPLICE_B,
			TRIPLICE_V) ~ 1
			# febre_amarela_mod,
			# INFLUENZA_mod

# The ~ 1 instructs poLCA to estimate the basic latent class model. For the latent class regres- sion model, replace the ~ 1 with the desired function of covariates, as, for example:
# f <- cbind(Y1, Y2, Y3) ~ X1 + X2 * X3

# To estimate the specified latent class model, the default poLCA command is:
# poLCA(formula, data, ncl pass = 2, maxiter = 1000, graphs = FALSE,
#     tol = 1e-10, na.rm = TRUE, probs.start = NULL, nrep = 1,
#     verbose = TRUE, calc.se = TRUE)

set.seed(1002)
#========================================================= 
# Fit for 2 latent classes: 
#========================================================= 
# ses_data<-na.omit(ses_data)
lcamodel_2 <- poLCA(f, lca_data_imputed, nclass = 2,
					maxiter=50000,
					nrep=10)

# Entropy
entropy<-function (p) sum(-p*log(p))
error_prior <- entropy(lcamodel_2$P) # Class proportions
error_post <- mean(apply(lcamodel_2$posterior, 1, entropy))
R2_entropy <- (error_prior - error_post) / error_prior
R2_entropy

### results

#Coherence of interpretation
#Predicted class memberships (by modal posterior prob.) 
 # 0.6642 0.3358 

#Statistical fit
# number of observations: 38812 
# number of estimated parameters: 49 
# residual degrees of freedom: 38763 
# maximum log-likelihood: -303510.9 
 
# AIC(2): 607119.7
# BIC(2): 607539.5
# G^2(2): 92638.43 (Likelihood ratio/deviance statistic) 
# X^2(2): 6.088068e+12 (Chi-square goodness of fit) 

#Entropy
# Entropy = [1] 0.9990477

# ========================================================= 
# Fit for 3 latent classes: 
# ========================================================= 
# set.seed(1978)
lcamodel <- poLCA(f, lca_data_imputed, nclass = 3,
					maxiter=50000,
					nrep=10)
# Entropy
entropy<-function (p) sum(-p*log(p))
error_prior <- entropy(lcamodel$P) # Class proportions
error_post <- mean(apply(lcamodel$posterior, 1, entropy),na.rm=TRUE)
R2_entropy <- (error_prior - error_post) / error_prior
R2_entropy

#results

#Coherence of interpretation
# Predicted class memberships (by modal posterior prob.) 
#  0.2905 0.3354 0.3741 

#Statistical fit
# number of observations: 38812 
# number of estimated parameters: 77 
# residual degrees of freedom: 38735 
# maximum log-likelihood: -303669.9 
 
# AIC(3): 607493.9
# BIC(3): 608153.5
# G^2(3): 92956.59 (Likelihood ratio/deviance statistic) 
# X^2(3): 4.971469e+12 (Chi-square goodness of fit) 

#Entropy
# Entropy = [1] 0.9992585

data$lca_classe<-lcamodel$predclass
data$lca_posterior<-lcamodel$posterior

write.csv(data,"/Users/joaovissoci/Desktop/lca_rejane.csv")
# ========================================================= 
# Fit for 4 latent classes: 
# ========================================================= 
# set.seed(1988)

lcamodel_4 <- poLCA(f, lca_data_cat, nclass = 4,
					maxiter=50000,
					nrep=10)
# Entropy
entropy<-function (p) sum(-p*log(p))
error_prior <- entropy(lcamodel_4$P) # Class proportions
error_post <- mean(apply(lcamodel_4$posterior, 1, entropy),na.rm=TRUE)
R2_entropy <- (error_prior - error_post) / error_prior
R2_entropy

# results
# Coherence of interpretation
# Predicted class memberships (by modal posterior prob.) 
#  0.2593 0.3739 0.0765 0.2903 

#Statistical fit
# number of observations: 38812 
# number of estimated parameters: 103 
# residual degrees of freedom: 38709 
# maximum log-likelihood: -294454.2 
 
# AIC(4): 589114.4
# BIC(4): 589996.7
# G^2(4): 74525.11 (Likelihood ratio/deviance statistic) 
# X^2(4): 3.523021e+13 (Chi-square goodness of fit) 

#Entropy
# Entropy = [1] 0.9997825

# ========================================================= 
# Fit for 5 latent classes: 
# ========================================================= 

lcamodel <- poLCA(f, lca_data_cat, nclass = 5,
					maxiter=50000,
					nrep=10)
# Entropy
entropy<-function (p) sum(-p*log(p))
error_prior <- entropy(lcamodel$P) # Class proportions
error_post <- mean(apply(lcamodel$posterior, 1, entropy),na.rm=TRUE)
R2_entropy <- (error_prior - error_post) / error_prior
R2_entropy

# results
# Coherence of interpretation
# Predicted class memberships (by modal posterior prob.) 
#  0.3715 0.0496 0.2188 0.0707 0.2894 

#Statistical fit
# number of observations: 38812 
# number of estimated parameters: 129 
# residual degrees of freedom: 38683 
# maximum log-likelihood: -289055.6 
 
# AIC(5): 578369.2
# BIC(5): 579474.2
# G^2(5): 63727.9 (Likelihood ratio/deviance statistic) 
# X^2(5): 750796574352 (Chi-square goodness of fit) 

#Entropy
# Entropy = [1] 0.9999999

# ========================================================= 
# Fit for 6 latent classes: 
# ========================================================= 

lcamodel <- poLCA(f, lca_data_cat, nclass = 6,
					maxiter=50000,
					nrep=10)
# Entropy
entropy<-function (p) sum(-p*log(p))
error_prior <- entropy(lcamodel$P) # Class proportions
error_post <- mean(apply(lcamodel$posterior, 1, entropy),na.rm=TRUE)
R2_entropy <- (error_prior - error_post) / error_prior
R2_entropy

# results
# Coherence of interpretation
# Predicted class memberships (by modal posterior prob.) 
#  0.3715 0.0496 0.2188 0.0707 0.2894 

#Statistical fit
# number of observations: 38812 
# number of estimated parameters: 129 
# residual degrees of freedom: 38683 
# maximum log-likelihood: -289055.6 
 
# AIC(5): 578369.2
# BIC(5): 579474.2
# G^2(5): 63727.9 (Likelihood ratio/deviance statistic) 
# X^2(5): 750796574352 (Chi-square goodness of fit) 

#Entropy
# Entropy = [1] 0.9999999

#nclass = number of latent classes to assume
#maxiter = maximum of iterations
#graph = logical weather poLCA should graphcially display the parameters
#tol=tolerance for judging convergence
#na.rm=exclude missing data
#probs.start=starter conditions for estimation
#verbose = display the results
#calc.se=calculate standard errors

#### GRAPHING SOLUTION
library(reshape)
library(ggplot2)
#Isolating classes to be plotted
classes_prob<-as.data.frame(lcamodel$probs)[,c(2,4,6,8,10,12)]
classes_prob$class<-c("class1","class2","class3","Class4")
class_prob_melt<-melt(classes_prob)

p <- ggplot(data = class_prob_melt, aes(class, variable, fill = value))+
 	 geom_tile(color = "white")+
     scale_fill_gradient2(low = "white", high = "steelblue", 
     	 mid = "lightblue",
   midpoint = 0.5, limit = c(0,1), space = "Lab", 
   name="% of Sample") + #
     geom_label(aes(y=variable, x=class, 
     	label=round(class_prob_melt$value,digits=2)*100),
     fill="white",alpha=0.70) +
     xlab(label="Latent class") +
     ylab(label="Variables") +
     scale_y_discrete(
     	labels=c(
  "Educated","Literate","Elderly",
  "Paid employment",
  "Long term illness","Positive health")) +
     scale_x_discrete(
     	labels=c("Class 1","Class 2","Class 3","Class 4"))


     # labels=c("No","Yes"),name="Evaluation")
p


#LOGICTI REGRESSION MODELS
analytical_data_dic<-na.omit(analytical_data_dic)
# analytical_data_dic<-na.omit(analytical_data_dic)
analytical_data_dic$class<-as.factor(lcamodel$predclass)
analytical_data_dic$class_recoded<-car::recode(
	analytical_data_dic$class,"1='class1';
							   2='class2';
							   3='class3';
							   4='class4';
							   5='class5'")

logmodel<-glm(barrier_data_nomoney ~ class_recoded
			,family=binomial, data=analytical_data_dic)
summary(logmodel)
#anova(reglogGEU)
odds_model_1<-exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)

logmodel<-glm(barrier_data_notransport ~ class_recoded
			,family=binomial, data=analytical_data_dic)
summary(logmodel)
#anova(reglogGEU)
odds_model_2<-exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)

logmodel<-glm(barrier_data_fear ~ class_recoded
			,family=binomial, data=analytical_data_dic)
summary(logmodel)
#anova(reglogGEU)
odds_model_3<-exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)

logmodel<-glm(barrier_data_socialsupport ~ class_recoded
			,family=binomial, data=analytical_data_dic)
summary(logmodel)
#anova(reglogGEU)
odds_model_4<-exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.90))) 
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)

logmodel<-glm(barrier_data_notavailable1 ~ class_recoded
			,family=binomial, data=analytical_data_dic)
summary(logmodel)
#anova(reglogGEU)
odds_model_5<-exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.90))) 
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)

logmodel<-glm(barrier_data_no_need ~ class_recoded
			,family=binomial, data=analytical_data_dic)
summary(logmodel)
#anova(reglogGEU)
odds_model_6<-exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.90))) 
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)

#LOGISTIC REGRESSION MODELS
colnames(odds_model_1)<-c("OR","LowCI","HighCI")
colnames(odds_model_2)<-c("OR","LowCI","HighCI")
colnames(odds_model_3)<-c("OR","LowCI","HighCI")
colnames(odds_model_4)<-c("OR","LowCI","HighCI")
colnames(odds_model_5)<-c("OR","LowCI","HighCI")
colnames(odds_model_6)<-c("OR","LowCI","HighCI")

odds_all<-rbind(odds_model_1,
					 odds_model_2,
					 odds_model_3,
					 odds_model_4,
					 odds_model_5,
					 odds_model_6)
odds_all_OR<-as.data.frame(odds_all[1:24])
odds_all_LIC<-as.data.frame(odds_all[25:48])
odds_all_HIC<-as.data.frame(odds_all[49:72])
odds_data<-data.frame(OR=odds_all_OR,lowCI=odds_all_LIC,
	highCI=odds_all_HIC)
colnames(odds_data)<-c("OR","LowCI","HighCI")
odds_data$barrier<-c(rep("No money",4),
					rep("No transport",4),
					rep("Fear",4),
					rep("Social support",4),
					rep("Not available",4),
					rep("No need",4)
					)
odds_data$class<-rep(c("class1","class2","class3","class4"),6)

# odds_model_melt<-melt(odds_data)

object1<- ggplot(odds_data, aes(y= reorder(class,OR), 
  x = round(OR,2))) +
facet_grid(barrier ~ .,space="free") +
geom_point() +
geom_errorbarh(aes(xmin=LowCI, xmax=HighCI), height=.2) +
geom_vline(xintercept = 1, linetype=2) +
geom_text(aes(label=format(round(OR,2),nsmall=2)), 
  vjust=-0.5, hjust=0, size=3) +
#coord_flip() +
#facet_grid(measure ~ ., scales="free_x", space="free") +
labs(y = 'Latent classes', x = 'Odds Ration and Confidence Interval') +
# scale_x_continuous(breaks=seq(0, 2000, 200)) +
# annotate("segment", x = 2000, xend=2020, y = 13, yend=13,
#   colour = "black",
#   arrow=arrow(length=unit(0.2,"cm"),type = "closed")) +
# annotate("segment", x = 2000, xend=2020, y = 10, yend=10,
#   colour = "black",
#   arrow=arrow(length=unit(0.2,"cm"),type = "closed")) +
# annotate("text", x = 1990, y = 12.7,
#   colour = "black",label="2940.52",size=3) +
# annotate("text", x = 1990, y = 9.7,
#   colour = "black",label="2014.76",size=3) +
theme_bw()

object1





