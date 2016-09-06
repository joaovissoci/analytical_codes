######################################################################
#baltiore_outlets_crashes
######################################################################
#
#
#
#
#
######################################################################
#SETTING ENVIRONMENT
######################################################################
#PASCKAGES INSTALLATION CODES
#install.packages("Hmisc")
#install.packages("car")
#install.packages("psych")
#install.packages("nortest")
#install.packages("ggplot2")
#install.packages("pastecs")
#install.packages("repmis")
#install.packages("mvnormtest")
#install.packages("polycor")

#PACKAGES LOADING CODE
#Load packages neededz for the analysis
#library(Hmisc)

#All packages must be installes with install.packages() function
lapply(c("Hmisc","car","psych","nortest","ggplot2","pastecs","repmis",
	"mvnormtest","polycor","MASS"), 
library, character.only=T)

######################################################################
#IMPORTING DATA
######################################################################
#LOADING DATA FROM A .CSV FILE
data<-read.csv("/Users/joaovissoci/OneDrive - Duke University/datasets/Global EM/baltimore_gis/paper 2/balticsv.csv",sep=",")
#information between " " are the path to the directory in your computer where the data is stored

######################################################################
#DATA MANAGEMENT
######################################################################
#Creating a data frame (group of variables)
#numeric<-with(data, data.frame(Peso,Altura,IMC,
#                          Idade))
data$outlet_density<-data$outlet/data$Area_km

data$crash_density<-data$crash_imp/data$Area_km

data$agglomeration<-car::recode(data$LISA_CL,"0=3;
	1=2; 2=1; 3=1;4=2")
data$agglomeration<-as.factor(data$agglomeration)
######################################################################
#DESCRIPTIVES
######################################################################

sum(data$crash_imp)
sum(data$outlet)/238.5

table(data$agglomeration)

psych::describe(data$outlet_density)
describe(data$outlet)

with(data,by(outlet,agglomeration,describe))
with(data,by(outlet_density,agglomeration,describe))
with(data,by(crash_imp,agglomeration,describe))
with(data,by(off_premis,agglomeration,summary))

######################################################################
#EXPLORATORY ANALYSIS
######################################################################
ggplot(data, aes(outlet,crash_imp)) +
  geom_point() + geom_smooth(se = TRUE)
#,color=agglomeration

#See
# http://stats.stackexchange.com/questions/17006/interpretation-of-incidence-rate-ratios
# http://stats.stackexchange.com/questions/11096/how-to-interpret-coefficients-in-a-poisson-regression
#http://datavoreconsulting.com/programming-tips/count-data-glms-choosing-poisson-negative-binomial-zero-inflated-poisson/

fm_nbin <- glm.nb(crash_imp ~ outlet,
	data = data)
summary(fm_nbin)
exp(coef(fm_nbin))
exp(confint(fm_nbin,level=0.95))

1 - pchisq(summary(fm_nbin)$deviance,
           summary(fm_nbin)$df.residual
           )


fm_nbin <- glm.nb(crash_imp ~ outlet + agglomeration + 
		lenght_km + population, 
	data = data)
summary(fm_nbin)
exp(coef(fm_nbin))
exp(confint(fm_nbin,level=0.95))

1 - pchisq(summary(fm_nbin)$deviance,
           summary(fm_nbin)$df.residual
           )

fm_nbin <- glm.nb(crash_imp ~ off_premis + agglomeration + 
    Area_km + population, 
  data = data)
summary(fm_nbin)
exp(coef(fm_nbin))
exp(confint(fm_nbin,level=0.95))

1 - pchisq(summary(fm_nbin)$deviance,
           summary(fm_nbin)$df.residual
           )

fm_nbin <- glm.nb(crash_imp ~ onboth + agglomeration + 
    Area_km + population, 
  data = data)
summary(fm_nbin)
exp(coef(fm_nbin))
exp(confint(fm_nbin,level=0.95))

1 - pchisq(summary(fm_nbin)$deviance,
           summary(fm_nbin)$df.residual
           )

# Regression Tree Example
library(rpart)

# grow tree 
fit <- rpart(crash_imp ~ outlet, 
   method="poisson", data=data)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(fit) # visualize cross-validation results  	

# plot tree 
plot(fit, uniform=TRUE, 
  	main="Regression Tree for Mileage ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postcript plot of tree 
post(fit, file = "c:/tree2.ps", 
  	title = "Regression Tree for Mileage ")

# prune the tree 
pfit<- prune(fit, cp=0.01160389) # from cptable   

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
  	main="Pruned Regression Tree for Mileage")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
post(pfit, file = "c:/ptree2.ps", 
  	title = "Pruned Regression Tree for Mileage")


# Conditional Inference Tree for Kyphosis
tree_data<-subset(data,data$agglomeration!="Low")

library(party)
fit <- ctree(crash_imp ~ outlet + as.factor(agglomeration) + 
	population, 
   data=data)
plot(fit, main="Conditional Inference Tree for Kyphosis")



# cbind(nd, 
#       Mean = predict(fm_nbin, newdata=nd, type="response"), 
#       SE = predict(fm_nbin, newdata=nd, type="response", se.fit=T)$se.fit
#       )

# Multiple Linear Regression Example 
fit <- lm(crash_density ~ outlet_density + agglomeration, 
	data=data)
summary(fit) # show results
# Other useful functions 
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics
plot(fit)





######################################################################
#TABLE 1
######################################################################
# 2-Way Frequency Table 
mytable <- with(data,table(Sexo,Classificacao)) # A will be rows, B will be columns 
mytable # print table 

margin.table(mytable, 1) # A frequencies (summed over B) 
margin.table(mytable, 2) # B frequencies (summed over A)

prop.table(mytable) # cell percentages
prop.table(mytable, 1) # row percentages 
prop.table(mytable, 2) # column percentages

#Teste de associação/Fisher
assocstats(mytable)
fisher.test(mytable)

# 3-Way Frequency Table 
mytable <- with(data,table(Sexo,Classificacao,Faixa_etaria)) # A will be rows, B will be columns 
ftable(mytable)

#OUTCOME ASSOCIATION AND BIVARIATE ANALYSIS
###########################################
#PARAMETRIC
# one sample t-test
t.test(data$IMC,mu=25) # Ho: mu=3

# independent 2-group t-test
t.test(data$IMC~data$Sexo,paired=FALSE) # where y is numeric and x is a binary factor

# paired t-test
IMC2<-data$IMC*2
t.test(data$IMC,IMC2,paired=TRUE) # where y is numeric and x is a binary factor

#NONPARAMETRIC
wilcox.test(data$IMC~data$Sexo,paired=FALSE) # where y is numeric and x is a binary factor

# paired t-test
wilcox.test(data$IMC,IMC2,paired=TRUE) 

######################################################################
#MULTIVARIATE ANALYSIS
######################################################################

# ANALYSIS OF VARIANCE
##################################
# One Way Anova (Completely Randomized Design)
fit <- aov(Idade ~ Classificacao, data=data)
summary(fit)

# Randomized Block Design (B is the blocking factor) 
fit <- aov(Idade ~ Classificacao+Sexo, data=data)
summary(fit)

# Two Way Factorial Design 
fit <- aov(Idade ~ Classificacao*Sexo, data=data)
summary(fit)

# Tukey Honestly Significant Differences
TukeyHSD(fit) # where fit comes from aov()

# Analysis of Covariance 
fit <- aov(Idade ~ Classificacao + IMC, data=data)
summary(fit)

# Kruskal Wallis Test One Way Anova by Ranks 
kruskal.test(Idade ~ Classificacao, data=data) # where y1 is numeric and A is a factor


#CORRELATIONS
##############################
#Pearson
cor(numeric, use="complete.obs", method="pearson") 
#Spearman
cor(numeric, use="complete.obs", method="spearman") 
#Kendall
cor(numeric, use="complete.obs", method="kendall")

#Significance testing
rcorr(as.matrix(numeric), type="pearson") # type can be pearson or spearman

cor.test(numeric$Peso,numeric$Altura) #Used for a single test of significance

# heterogeneous correlations in one matrix 
# pearson (numeric-numeric), 
# polyserial (numeric-ordinal), 
# and polychoric (ordinal-ordinal)
# x is a data frame with ordered factors 
# and numeric variables
hetcor(data) 

# polychoric correlation
# x is a contingency table of counts
polychor(data) 

#GLM
############################################
baselineXFUP3<-glm(ATTEMPT_P ~ Anxiety_presence + AGE + SEX + MARSTAT +
                            ATTEMPT_baseline + Diagnostic
                            ,family=binomial, data=FUP3)
summary(baselineXFUP3)
#anova(reglogGEU)
#exp(coef(model1_death)) # exponentiated coefficients
#exp(confint(model1_death)) # 95% CI for exponentiated coefficients
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
#logistic.display(baselineXFUP3)

######################################################################
#COMPLEX ANALYSIS AND OTHER FIGURES
######################################################################


######################################################################
#END
######################################################################