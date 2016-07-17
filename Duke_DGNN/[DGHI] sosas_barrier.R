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
lapply(c("sem","ggplot2", "psych", "RCurl", "irr", "nortest", 
	"moments","GPArotation","nFactors","boot","psy", "car",
	"vcd", "gridExtra","mi","VIM","epicalc","gdata","sqldf",
	"reshape2","mclust","foreign","survival","memisc","lme4",
	"lmerTest","dplyr","QCA","VennDiagram","qgraph","igraph",
	"ltm","gmodels","eRm","mirt","dplyr","devtools","reshape",
	"poLCA","readstata13"),#,"venneuler"),
library, character.only=T)

###################################################
#IMPORTING DATA AND RECODING
###################################################
data <- read.dta13("/Users/jnv4/OneDrive - Duke University/datasets/DGNN/SOSAS/sosas_data.dta")


#recode missing and other random problems
data$Gender<-car::recode(data$Gender,"'male'=0;'female'=1")
data$Education<-car::recode(
	data$Education,"'edu_none'=0;
						'primary_school'=1;
						'secondary_school1'=1;
						'secondary_school2'=1;x
						'tertiary_school'=1;
						'university'=1;
						else=NA")

data$Literacy<-car::recode(
	data$Literacy,"'no'=0;
					   'yes'=1;
					   else=NA")

data$Occupation<-car::recode(
	data$Occupation,"'domestic_helpers'=0;
						'farmer'=1;
						'government_employees'=1;
						'homemaker'=1;
						'nongov_employees'=1;
						'self_employed'=1;
						'student'=0;
						'unemployed'=0;
						else=NA")

#paind and unpaid

# data$Ethnicity<-car::recode(
# 	data$Ethnicity,"''
# 	")

# data$Religion<-car::recode(
# 	data$Religion,"''
# 	")

data$Household_stay_length<-car::recode(
	data$Household_stay_length,"
	'days'='daysweeks';
	'weeks'='daysweeks';
	'-77'=NA;
	'-99'=NA")

data$Time_ill<-car::recode(
	data$Time_ill,"
	'-77'=NA;
	'-99'=NA;
	'days'=0;
	'weeks'=0;
	'months'=1;
	'years'=1;
	else=NA")

# data$Age<-car::recode(
# 	data$Age,"
# 	0:15='children';
# 	16:34='young adults';
# 	35:64='adults';
# 	65:102='elderly'")
# data$Age<-as.factor(data$Age)

data_barriers_temp<-subset(data,data$Untreated==1)
data_barriers<-subset(data_barriers_temp,
	data_barriers_temp$Age>=18)

barrier_data_nomoney<-with(data_barriers,rowSums(data.frame(
	Prob1Reason_no_money,
	Prob2Reason_no_money,
	Prob3Reason_no_money,
	Prob4Reason_no_money,
	Prob5Reason_no_money)))

barrier_data_notransport<-with(data_barriers,rowSums(data.frame(
	Prob1Reason_no_transport,
	Prob2Reason_no_transport,
	Prob3Reason_no_transport,
	Prob4Reason_no_transport,
	Prob5Reason_no_transport)))

barrier_data_notime<-with(data_barriers,rowSums(data.frame(
	Prob1Reason_no_time,
	Prob2Reason_no_time,
	Prob3Reason_no_time,
	Prob4Reason_no_time,
	Prob5Reason_no_time)))

barrier_data_fear<-with(data_barriers,rowSums(data.frame(
	Prob1Reason_fear,
	Prob2Reason_fear,
	Prob3Reason_fear,
	Prob4Reason_fear,
	Prob5Reason_fear)))

barrier_data_socialsupport<-with(data_barriers,rowSums(data.frame(
	Prob1Reason_socialsupport,
	Prob2Reason_socialsupport,
	Prob3Reason_socialsupport,
	Prob4Reason_socialsupport,
	Prob5Reason_socialsupport)))

barrier_data_notavailable1<-with(data_barriers,rowSums(data.frame(
	Prob1Reason_notavailable1,
	Prob2Reason_notavailable1,
	Prob3Reason_notavailable1,
	Prob4Reason_notavailable1,
	Prob5Reason_notavailable1,#)))

# barrier_data_notavailable2<-with(data_barriers,rowSums(data.frame(
	Prob1Reason_notavailable2,
	Prob2Reason_notavailable2,
	Prob3Reason_notavailable2,
	Prob4Reason_notavailable2,
	Prob5Reason_notavailable2,#)))

# barrier_data_notavailable3<-with(data_barriers,rowSums(data.frame(
	Prob1Reason_notavailable3,
	Prob2Reason_notavailable3,
	Prob3Reason_notavailable3,
	Prob4Reason_notavailable3,
	Prob5Reason_notavailable3)))

barrier_data_no_need<-with(data_barriers,rowSums(data.frame(
	Prob1Reason_no_need,
	Prob2Reason_no_need,
	Prob3Reason_no_need,
	Prob4Reason_no_need,
	Prob5Reason_no_need)))

barrier_dataProb1Long12<-with(data_barriers,rowSums(data.frame(
	Prob1Long12,
	Prob1Long12,
	Prob1Long12,
	Prob1Long12,
	Prob1Long12)))


barriers<-data.frame(barrier_data_nomoney,
	barrier_data_notransport,
	barrier_data_notime,
	barrier_data_fear,
	barrier_data_socialsupport,
	barrier_data_notavailable1,
	# barrier_data_notavailable2,
	# barrier_data_notavailable3,
	barrier_data_no_need)
	# barrier_dataProb1Long12)

dicotomize<-function(x){
	car::recode(x,"0=0;NA=NA;else=1")
}

barriers_dic<-sapply(barriers,
	function(x) dicotomize(x))

ses_data<-with(data_barriers,data.frame(
	Gender,
	Age,
	Education,
	Literacy,
	Occupation,
	# Ethnicity,
	# Religion,
	# Household_stay_length,
	Time_ill,
	Health_status,
	Household,
	E15_rural
	))


analytical_data_dic<-data.frame(ses_data,barriers_dic)
analytical_data<-data.frame(ses_data,barriers)
data_ses<-subset(data,
	data$Age>=18)

###################################################
#Table 1
###################################################

# Age
# describe(data_ses$Age)
by(data_ses$Age,data_ses$Untreated,summary)
# t-test: # independent 2-group, 2 level IV
wilcox.test(data_ses$Age ~ data_ses$Untreated)

# Gender
# table<-table(victms_gender)
# table
# prop.table(table)
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
# table<-table(victms_gender)
# table
# prop.table(table)
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
# describe(data_ses$Age)
by(data_ses$Household,data_ses$Untreated,summary)
# t-test: # independent 2-group, 2 level IV
testName <- t.test(age_victims ~ victimis_outcome_all)

# Education
# table<-table(victms_gender)
# table
# prop.table(table)
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
# table<-table(victms_gender)
# table
# prop.table(table)
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
# table<-table(victms_gender)
# table
# prop.table(table)
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
# table<-table(victms_gender)
# table
# prop.table(table)
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
# table<-table(victms_gender)
# table
# prop.table(table)
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

## NO MONEY
logmodel<-glm(Untreated ~ 
				# Age + 
				as.factor(Gender) +
				as.factor(Education) +
				as.factor(Literacy) +
				as.factor(Occupation) +
				Household+
				as.factor(Time_ill) +
				as.factor(E15_rural) +
				Health_status,
			family=binomial, data=data_ses)
summary(logmodel)
#anova(reglogGEU)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.95))) 
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)

###################################################
#Figure 1
###################################################

vd <- venneuler(c(A=0.3, B=0.3, C=1.1, "A&B"=0.1, 
	"A&C"=0.2, "B&C"=0.1 ,"A&B&C"=0.1))
plot(vd)
# same as c(A=1, `A&B&C`=1, C=1)
m <- data.frame(elements=c("1","2","2","2","3"), 
	sets=c("A","A","B","C","C"))
v <- venneuler(m)
plot(v)
m <- as.matrix(data.frame(A=c(1.5, 0.2, 0.4, 0, 0),B=c(0 , 0.2, 0 , 1, 0),
C=c(0 , 0 , 0.3, 0, 1)))
# without weights
v <- venneuler(m > 0)
plot(v)
# with weights
v <- venneuler(m)
plot(v)


barriers_matrix<-as.matrix(barriers_dic)
colnames(barriers_matrix)<-c("No money",
						  "No transport",
						  "No time",
						  "Fear",
						  "Lack of social support",
						  "Not available",
						  "No need")
v <- venneuler(barriers_matrix)
plot(v)

cor<-cor_auto(barriers_matrix)
qgraph(cor,layout="spring",vsize=log(colSums(barriers_matrix))*10)

###################################################
#Table 2
###################################################
str(data_barriers_temp)

# table(data$Untreated)

## NO MONEY
logmodel<-glm(barrier_data_nomoney ~ 
				Age + 
				Gender +
				Education +
				Literacy +
				Occupation +
				Household+
				Time_ill +
				E15_rural +
				Health_status,
			family=binomial, data=analytical_data_dic)
summary(logmodel)
#anova(reglogGEU)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.90))) 
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)

## NO TRANSPORT
logmodel<-glm(barrier_data_notransport ~ 
				Age + 
				Gender +
				Education +
				Literacy +
				Occupation +
				Household+
				Time_ill +
				E15_rural +
				Health_status,
			family=binomial, data=analytical_data_dic)
summary(logmodel)
#anova(reglogGEU)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.90))) 
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)

# NO TIME
logmodel<-glm(barrier_data_notime ~ 
				Age + 
				Gender +
				Education +
				Literacy +
				Occupation +
				Household+
				Time_ill +
				E15_rural +
				Health_status,
			family=binomial, data=analytical_data_dic)
summary(logmodel)
#anova(reglogGEU)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.90))) 
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)

# FEAR
logmodel<-glm(barrier_data_fear ~ 
				Age + 
				Gender +
				Education +
				Literacy +
				Occupation +
				Household+
				Time_ill +
				E15_rural +
				Health_status,
			family=binomial, data=analytical_data_dic)
summary(logmodel)
#anova(reglogGEU)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.90))) 
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)

# SOCIAL SUPPORT
logmodel<-glm(barrier_data_socialsupport ~ 
				Age + 
				Gender +
				Education +
				Literacy +
				Occupation +
				Household+
				Time_ill +
				E15_rural +
				Health_status,
			family=binomial, data=analytical_data_dic)
summary(logmodel)
#anova(reglogGEU)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.90))) 
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)

# NOT AVAILABLE
logmodel<-glm(barrier_data_notavailable1 ~ 
				Age + 
				Gender +
				Education +
				Literacy +
				Occupation +
				Household+
				Time_ill +
				E15_rural +
				Health_status,
			family=binomial, data=analytical_data_dic)
summary(logmodel)
#anova(reglogGEU)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.90))) 
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)

###################################################
#Network approach
###################################################


#network
dassIsing<-IsingFit(dass3,plot=TRUE,
	labels=namesdass,
	groups=dassgr,
	color=c("gray90","gray70","gray50"),
	edge.color="black")

###################################################
#Latent class analysis
###################################################

## Questions
# http://rfunction.com/archives/1499
# https://drive.google.com/open?id=0B4TReYGK49h_X09ZYno1OG5aUVk

# Correlation

# Building the formula
# To specify a latent class model, poLCA uses the standard, symbolic R model formula expres- sion. The response variables are the manifest variables of the model. Because latent class models have multiple manifest variables, these variables must be “bound” as cbind(Y1, Y2, Y3, ...) in the model formula. For the basic latent class model with no covariates, the formula definition takes the form

ses_data_cat<-sapply(ses_data,function(x) as.factor(x))
ses_data_cat<-as.data.frame(ses_data_cat)

ses_data_cat$Household_cat<-car::recode(ses_data_cat$Household,"0:5='average';else='high'")
ses_data_cat$age_cat<-car::recode(ses_data_cat$Age,"
	18:50='adult';else='elderly'")

f <- cbind(Gender, Education, Literacy,
		   Occupation,Household_cat,
		   Time_ill, Health_status) ~ 1

# The ~ 1 instructs poLCA to estimate the basic latent class model. For the latent class regres- sion model, replace the ~ 1 with the desired function of covariates, as, for example:
# f <- cbind(Y1, Y2, Y3) ~ X1 + X2 * X3

# To estimate the specified latent class model, the default poLCA command is:
# poLCA(formula, data, ncl pass = 2, maxiter = 1000, graphs = FALSE,
#     tol = 1e-10, na.rm = TRUE, probs.start = NULL, nrep = 1,
#     verbose = TRUE, calc.se = TRUE)

#========================================================= 
# Fit for 2 latent classes: 
#========================================================= 
ses_data<-na.omit(ses_data)
lcamodel <- poLCA(f, ses_data_cat, nclass = 2)

# Entropy
entropy<-function (p) sum(-p*log(p))
error_prior <- entropy(lcamodel$P) # Class proportions
error_post <- mean(apply(lcamodel$posterior, 1, entropy))
R2_entropy <- (error_prior - error_post) / error_prior
R2_entropy

### results
# number of observations: 245 
# number of estimated parameters: 27 
# residual degrees of freedom: 218 
# maximum log-likelihood: -1243.071 
 
# AIC(2): 2540.142
# BIC(2): 2634.676
# G^2(2): 284.7812 (Likelihood ratio/deviance statistic) 
# X^2(2): 908.1992 (Chi-square goodness of fit)  

# Entropy = [1] 0.7747763

# ========================================================= 
# Fit for 3 latent classes: 
# ========================================================= 
set.seed(1988)
lcamodel <- poLCA(f, ses_data_cat, nclass = 3)

# Entropy
entropy<-function (p) sum(-p*log(p))
error_prior <- entropy(lcamodel$P) # Class proportions
error_post <- mean(apply(lcamodel$posterior, 1, entropy),na.rm=TRUE)
R2_entropy <- (error_prior - error_post) / error_prior
R2_entropy

#results

# number of observations: 245 
# number of estimated parameters: 41 
# residual degrees of freedom: 204 
# maximum log-likelihood: -1231.49 
 
# AIC(3): 2544.98
# BIC(3): 2688.532
# G^2(3): 261.6194 (Likelihood ratio/deviance statistic) 
# X^2(3): 761.6353 (Chi-square goodness of fit) 

# Entropy = [1] 0.7727836

# ========================================================= 
# Fit for 4 latent classes: 
# ========================================================= 
set.seed(1988)

lcamodel <- poLCA(f, ses_data_cat, nclass = 4)

# Entropy
entropy<-function (p) sum(-p*log(p))
error_prior <- entropy(lcamodel$P) # Class proportions
error_post <- mean(apply(lcamodel$posterior, 1, entropy),na.rm=TRUE)
R2_entropy <- (error_prior - error_post) / error_prior
R2_entropy

# results
# number of observations: 245 
# number of estimated parameters: 55 
# residual degrees of freedom: 190 
# maximum log-likelihood: -1216.756 
 
# AIC(4): 2543.513
# BIC(4): 2736.082
# G^2(4): 232.1518 (Likelihood ratio/deviance statistic) 
# X^2(4): 443.2317 (Chi-square goodness of fit) 

# Entropy = [1] 0.6794298

# ========================================================= 
# Fit for 5 latent classes: 
# ========================================================= 
set.seed(1988)

lcamodel <- poLCA(f, ses_data_cat, nclass = 5)
summary(lcamodel)

# Entropy
entropy<-function (p) sum(-p*log(p))
error_prior <- entropy(lcamodel$P) # Class proportions
error_post <- mean(apply(lcamodel$posterior, 1, entropy),na.rm=TRUE)
R2_entropy <- (error_prior - error_post) / error_prior
R2_entropy

# number of observations: 245 
# number of estimated parameters: 69 
# residual degrees of freedom: 176 
# maximum log-likelihood: -1213.042 
 
# AIC(5): 2564.083
# BIC(5): 2805.67
# G^2(5): 224.7225 (Likelihood ratio/deviance statistic) 
# X^2(5): 513.0323 (Chi-square goodness of fit) 

# Entropy = [1] 0.7103862

#nclass = number of latent classes to assume
#maxiter = maximum of iterations
#graph = logical weather poLCA should graphcially display the parameters
#tol=tolerance for judging convergence
#na.rm=exclude missing data
#probs.start=starter conditions for estimation
#verbose = display the results
#calc.se=calculate standard errors

#### GRAPHING SOLUTION
#Isolating classes to be plotted
classes_prob<-as.data.frame(lcamodel$probs)[,c(1,4,6,8,9,12,13)]
classes_prob$class<-c("class1","class2","class3","class4")
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
     	labels=c("Female",
  "Educated","Literate",
  "Paid employment","Large household",
  "Long term illness","Positive health")) +
     scale_x_discrete(
     	labels=c("Class 1","Class 2","Class 3","Class 4"))


     # labels=c("No","Yes"),name="Evaluation")
p


#LOGICTI REGRESSION MODELS

analytical_data_dic<-na.omit(analytical_data_dic)
analytical_data_dic$class<-as.factor(lcamodel$predclass)
analytical_data_dic$class_recoded<-car::recode(
	analytical_data_dic$class,"1='class2';
							   2='class3';
							   3='class1'#;
							   #4='class4'")#;5='class1'")

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
odds_model_2<-exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.90))) 
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)

logmodel<-glm(barrier_data_fear ~ class_recoded
			,family=binomial, data=analytical_data_dic)
summary(logmodel)
#anova(reglogGEU)
odds_model_3<-exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.90))) 
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
					rep("Lack of social support",4),
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
labs(y = '', x = '') +
scale_x_continuous(breaks=seq(0, 2000, 200)) +
annotate("segment", x = 2000, xend=2020, y = 13, yend=13,
  colour = "black",
  arrow=arrow(length=unit(0.2,"cm"),type = "closed")) +
annotate("segment", x = 2000, xend=2020, y = 10, yend=10,
  colour = "black",
  arrow=arrow(length=unit(0.2,"cm"),type = "closed")) +
annotate("text", x = 1990, y = 12.7,
  colour = "black",label="2940.52",size=3) +
annotate("text", x = 1990, y = 9.7,
  colour = "black",label="2014.76",size=3) +
theme_bw()

object1





