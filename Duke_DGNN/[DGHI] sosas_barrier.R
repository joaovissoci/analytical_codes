###################################################
#TEMPLATE_FOR _META_ANALYSIS_OF_DIAGNOSTIC_ACCURACY#
#this script follows a combination of guidelines proposed by Doebler and Holling, according to (http://cran.r-project.org/web/packages/mada/vignettes/mada.pdf)#
#
#
###################################################
#SETTING ENVIRONMENT
###################################################

# install.packages("readstata13")
library(readstata13)
library(poLCA)
#All packages must be installes with install.packages() function
lapply(c("sem","ggplot2", "psych", "RCurl", "irr", "nortest", 
	"moments","GPArotation","nFactors","boot","psy", "car",
	"vcd", "gridExtra","mi","VIM","epicalc","gdata","sqldf",
	"reshape2","mclust","foreign","survival","memisc","lme4",
	"lmerTest","dplyr","QCA","VennDiagram","qgraph","igraph",
	"ltm","gmodels","eRm","mirt","dplyr","devtools","reshape"),
library, character.only=T)

###################################################
#IMPORTING DATA AND RECODING
###################################################
data <- read.dta13("/Users/joaovissoci/Desktop/sosas_data.dta")

data_barries<-subset(data,data$Untreated==1)

barrier_data<-with(data_barries,data.frame(
	Prob1Reason_no_money,
	Prob1Reason_no_transport,
	# Prob1Reason_no_time,
	Prob1Reason_fear,
	Prob1Reason_socialsupport,
	Prob1Reason_notavailable1,
	Prob1Reason_notavailable2,
	Prob1Reason_notavailable3,
	Prob1Reason_no_need
	# Prob1Long12,
	# Prob2Reason_no_money,
	# Prob2Reason_no_transport,
	# Prob2Reason_no_time,
	# Prob2Reason_fear,
	# Prob2Reason_socialsupport,
	# Prob2Reason_notavailable1,
	# Prob2Reason_notavailable2,
	# Prob2Reason_notavailable3,
	# Prob2Reason_no_need,
	# Prob2Long12,
	# Prob3Reason_no_money,
	# Prob3Reason_no_transport,
	# Prob3Reason_no_time,
	# Prob3Reason_fear,
	# Prob3Reason_socialsupport,
	# Prob3Reason_notavailable1,
	# Prob3Reason_notavailable2,
	# Prob3Reason_notavailable3,
	# Prob3Reason_no_need,
	# Prob3Long12,
	# Prob4Reason_no_money,
	# Prob4Reason_no_transport,
	# Prob4Reason_no_time,
	# Prob4Reason_fear,
	# Prob4Reason_socialsupport,
	# Prob4Reason_notavailable1,
	# Prob4Reason_notavailable2,
	# Prob4Reason_notavailable3,
	# Prob4Reason_no_need,
	# Prob4Long12,
	# Prob5Reason_no_money,
	# Prob5Reason_no_transport,
	# Prob5Reason_no_time,
	# Prob5Reason_fear,
	# Prob5Reason_socialsupport,
	# Prob5Reason_notavailable1,
	# Prob5Reason_notavailable2,
	# Prob5Reason_notavailable3,
	# Prob5Reason_no_need,
	# Prob5Long12
	))

ses_data<-with(data_barries,data.frame(
	Gender,
	Age,
	Education,
	Literacy,
	Occupation,
	# Ethnicity,
	# Religion,
	Household_stay_length,
	Time_ill,
	Health_status
	))

#recode missing and other random problems
ses_data$Education<-car::recode(
	ses_data$Education,"'edu_none'=0;
						'primary_school'=1;
						'secondary_school1'=3;
						'secondary_school2'=3;
						'tertiary_school'=4;
						'university'=4;
						else=NA")

ses_data$Literacy<-car::recode(
	ses_data$Literacy,"'no'=0;
					   'yes'=1;
					   else=NA")

ses_data$Occupation<-car::recode(
	ses_data$Occupation,"'domestic_helpers'=1;
						'farmer'=1;
						'government_employees'=1;
						'homemaker'=1;
						'nongov_employees'=1;
						'self_employed'=1;
						'student'=0;
						'unemployed'=0;
						else=NA")

# ses_data$Ethnicity<-car::recode(
# 	ses_data$Ethnicity,"''
# 	")

# ses_data$Religion<-car::recode(
# 	ses_data$Religion,"''
# 	")

ses_data$Household_stay_length<-car::recode(
	ses_data$Household_stay_length,"
	'-77'=NA;
	'-99'=NA")

ses_data$Time_ill<-car::recode(
	ses_data$Time_ill,"
	'-77'=NA;
	'-99'=NA")

# ses_data$Age<-car::recode(
# 	ses_data$Age,"
# 	0:15='children';
# 	16:34='young adults';
# 	35:64='adults';
# 	65:102='elderly'")
# ses_data$Age<-as.factor(ses_data$Age)


analytical_data<-data.frame(ses_data,barrier_data)
analytical_data<-na.omit(analytical_data)
###################################################
#DESCRIPTIVES
###################################################
## Questions
# http://rfunction.com/archives/1499
# https://drive.google.com/open?id=0B4TReYGK49h_X09ZYno1OG5aUVk

# Correlation

# Building the formula
# To specify a latent class model, poLCA uses the standard, symbolic R model formula expres- sion. The response variables are the manifest variables of the model. Because latent class models have multiple manifest variables, these variables must be “bound” as cbind(Y1, Y2, Y3, ...) in the model formula. For the basic latent class model with no covariates, the formula definition takes the form
f <- cbind(Gender, Age, Education, Literacy,
		   Occupation, Household_stay_length,
		   Time_ill, Health_status) ~ 1

# The ~ 1 instructs poLCA to estimate the basic latent class model. For the latent class regres- sion model, replace the ~ 1 with the desired function of covariates, as, for example:
# f <- cbind(Y1, Y2, Y3) ~ X1 + X2 * X3

# To estimate the specified latent class model, the default poLCA command is:
# poLCA(formula, data, ncl pass = 2, maxiter = 1000, graphs = FALSE,
#     tol = 1e-10, na.rm = TRUE, probs.start = NULL, nrep = 1,
#     verbose = TRUE, calc.se = TRUE)

# 2 classes
lcamodel <- poLCA(f, ses_data, nclass = 2)

# Entropy
entropy<-function (p) sum(-p*log(p))
error_prior <- entropy(lcamodel$P) # Class proportions
error_post <- mean(apply(lcamodel$posterior, 1, entropy))
R2_entropy <- (error_prior - error_post) / error_prior
R2_entropy

# ========================================================= 
# Fit for 2 latent classes: 
# ========================================================= 
# number of observations: 332 
# number of estimated parameters: 39 
# residual degrees of freedom: 293 
# maximum log-likelihood: -2426.673 
 
# AIC(2): 4931.346
# BIC(2): 5079.746
# G^2(2): 1283.111 (Likelihood ratio/deviance statistic) 
# X^2(2): 7185.324 (Chi-square goodness of fit) 

# Entropy = [1] 0.9024248

#3 classes
lcamodel <- poLCA(f, ses_data, nclass = 3)

# Entropy
entropy<-function (p) sum(-p*log(p))
error_prior <- entropy(lcamodel$P) # Class proportions
error_post <- mean(apply(lcamodel$posterior, 1, entropy),na.rm=TRUE)
R2_entropy <- (error_prior - error_post) / error_prior
R2_entropy

# ========================================================= 
# Fit for 3 latent classes: 
# ========================================================= 
# number of observations: 332 
# number of estimated parameters: 59 
# residual degrees of freedom: 273 
# maximum log-likelihood: -2329.039 
 
# AIC(3): 4776.077
# BIC(3): 5000.58
# G^2(3): 1087.842 (Likelihood ratio/deviance statistic) 
# X^2(3): 5147.289 (Chi-square goodness of fit) 

# Entropy = [1] 0.8971588

#4 classes
lcamodel <- poLCA(f, ses_data, nclass = 4)

# Entropy
entropy<-function (p) sum(-p*log(p))
error_prior <- entropy(lcamodel$P) # Class proportions
error_post <- mean(apply(lcamodel$posterior, 1, entropy),na.rm=TRUE)
R2_entropy <- (error_prior - error_post) / error_prior
R2_entropy

# ========================================================= 
# Fit for 4 latent classes: 
# ========================================================= 
# number of observations: 332 
# number of estimated parameters: 79 
# residual degrees of freedom: 253 
# maximum log-likelihood: -2302.42 
 
# AIC(4): 4762.839
# BIC(4): 5063.445
# G^2(4): 1034.604 (Likelihood ratio/deviance statistic) 
# X^2(4): 4628.984 (Chi-square goodness of fit) 

# Entropy = [1] 0.7920469

#5 classes
lcamodel <- poLCA(f, ses_data, nclass = 5)

# Entropy
entropy<-function (p) sum(-p*log(p))
error_prior <- entropy(lcamodel$P) # Class proportions
error_post <- mean(apply(lcamodel$posterior, 1, entropy),na.rm=TRUE)
R2_entropy <- (error_prior - error_post) / error_prior
R2_entropy

# ========================================================= 
# Fit for 5 latent classes: 
# ========================================================= 
# number of observations: 332 
# number of estimated parameters: 99 
# residual degrees of freedom: 233 
# maximum log-likelihood: -2282.563 
 
# AIC(5): 4763.126
# BIC(5): 5139.835
# G^2(5): 994.891 (Likelihood ratio/deviance statistic) 
# X^2(5): 4367.721 (Chi-square goodness of fit) 

# Entropy = [1] 0.7675588

#nclass = number of latent classes to assume
#maxiter = maximum of iterations
#graph = logical weather poLCA should graphcially display the parameters
#tol=tolerance for judging convergence
#na.rm=exclude missing data
#probs.start=starter conditions for estimation
#verbose = display the results
#calc.se=calculate standard errors


#### GRAPHING SOLUTION

lcmodel <- reshape2::melt(lcamodel$probs)
zp1 <- ggplot(lcmodel,aes(x = X1, y = value, fill = X2))
zp1 <- zp1 + geom_bar(stat = "identity", position = "stack")
zp1 <- zp1 + facet_grid(L1 ~ .) 
zp1 <- zp1 + scale_fill_brewer(type="seq", palette="Greys") +theme_bw()
zp1 <- zp1 + labs(x = "Fragebogenitems",
	y="Anteil der Item-\nAntwortkategorien", 
	fill ="Antwortkategorien")
zp1 <- zp1 + theme( axis.text.y=element_blank(),
                    axis.ticks.y=element_blank(),                    
                    panel.grid.major.y=element_blank())
zp1 <- zp1 + guides(fill = guide_legend(reverse=TRUE))
zp1




#MODEL 1 - Adding every variable
#traffic control was not added because had cases with 0 observations
# age and gender becaise the missing rate wsa to high

analytical_data$class<-as.factor(lcamodel$predclass)

logmodel<-glm(Prob1Reason_no_money ~ class
			,family=binomial, data=analytical_data)
summary(logmodel)
#anova(reglogGEU)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.90))) 
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
# logistic.display(logmodel)

logmodel<-glm(Prob1Reason_no_transport ~ class
			,family=binomial, data=analytical_data)
summary(logmodel)
#anova(reglogGEU)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.90))) 
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
# logistic.display(logmodel)

logmodel<-glm(Prob1Reason_no_time ~ class
			,family=binomial, data=analytical_data)
summary(logmodel)
#anova(reglogGEU)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.90))) 
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
# logistic.display(logmodel)

logmodel<-glm(Prob1Reason_fear ~ class
			,family=binomial, data=analytical_data)
summary(logmodel)
#anova(reglogGEU)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.90))) 
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
# logistic.display(logmodel)

logmodel<-glm(Prob1Reason_socialsupport ~ class
			,family=binomial, data=analytical_data)
summary(logmodel)
#anova(reglogGEU)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.90))) 
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
# logistic.display(logmodel)


logmodel<-glm(Prob1Reason_notavailable1 ~ class
			,family=binomial, data=analytical_data)
summary(logmodel)
#anova(reglogGEU)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.90))) 
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
# logistic.display(logmodel)

logmodel<-glm(Prob1Reason_notavailable2 ~ class
			,family=binomial, data=analytical_data)
summary(logmodel)
#anova(reglogGEU)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.90))) 
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
# logistic.display(logmodel)

logmodel<-glm(Prob1Reason_notavailable3 ~ class
			,family=binomial, data=analytical_data)
summary(logmodel)
#anova(reglogGEU)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.90))) 
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
# logistic.display(logmodel)

logmodel<-glm(Prob1Reason_no_need ~ class
			,family=binomial, data=analytical_data)
summary(logmodel)
#anova(reglogGEU)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.90))) 
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
# logistic.display(logmodel)

logmodel<-glm(Prob1Long12 ~ class
			,family=binomial, data=analytical_data)
summary(logmodel)
#anova(reglogGEU)
exp(cbind(Odds=coef(logmodel),confint(logmodel,level=0.90))) 
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
# logistic.display(logmodel)



poLCA.table(formula = COOPERAT ~ 1,
    condition = list(PURPOSE = 3, ACCURACY = 1, UNDERSTA = 2),
    lc = gss.lc2)



analytical_data$Gender<-as.numeric(analytical_data$Gender)
analytical_data$Education<-as.numeric(analytical_data$Education)
analytical_data$Literacy<-as.numeric(analytical_data$Literacy)
analytical_data$Occupation<-as.numeric(analytical_data$Occupation)
analytical_data$Household_stay_length<-as.numeric(analytical_data$Household_stay_length)
analytical_data$Time_ill<-as.numeric(analytical_data$Time_ill)
analytical_data$Health_status<-as.numeric(analytical_data$Health_status)

# # Define the amout of factor to retain
#Group of functinos to determine the number os items to be extracted
cor_data<-cor_auto(analytical_data)

# #Community analysis
# comprehension_network_glasso<-qgraph(cor_data,
# 	layout="spring",
# 	vsize=6,esize=20,graph="glasso",
# 	sampleSize=nrow(bea_data),
# 	legend.cex = 0.5,GLratio=1.5,minimum=0.1)
# #Calculating Community measures
# g<-as.igraph(comprehension_network_glasso) #creating igraph object
# h<-walktrap.community(g) #creatin community object
# h<-spinglass.community(g, weights=NA)
# plot(h,g) #plotting community network
# h$membership #extracting community membership for each node on the network
# community<-data.frame(h$membership,rownames(cor_data))

#listing grouping variables in the network resulting from the community analysis
# network_groups<-list(
# Component1=as.numeric(rownames(community)[community[,1]==1]),
# Component2=as.numeric(rownames(community)[community[,1]==2]),
# Component3=as.numeric(rownames(community)[community[,1]==3])
# )

network_groups<-list(
Component1=c(1,3,4,5,15,14),
Component2=c(2,16,6,7),
Component3=c(11,12,13,10),
Component4=c(19,20,21,23),
Component5=c(9,17,18,22,8)
)

# creating vectors for labels
node_labels<-c(
"What is the area of the roadway?",
"What type of roadway?",
"Is this point at an intersection/junction?",
"How many lanes in the roadway?",
"Is there an auxiliary/other lane?",
"How is the road surface conditions?",
"Is there space on the side of the road 
for any reason or use?",
"Are there pedestrian pathways?",
"Is there a Bus Stop?",
"Is there a Speed bump?",
"Is there a traffic light at this location?",
"Are there road traffic signs at this hotspot?",
"Is there a sign for speed limit of road?",
"Road visibility is influenced by curves?",
"Is the visibility influenced by 
environmental factors?",
"Are there bridges on the road?",
"Is there a safe area for pedestrians 
to cross the road?",
"Is there a safe area for pedestrians
to in the center of the road?",
"Count the number of cars",
"Count the number of moto",
"Count the number of bike",
"Count the number of pedestrians",
"Count the number of bus/trucks"
)

# creating nodes labels vector
node_names<-c("RD","RT","INT","TLA","AR",
	"RC","RS",
	"WALK","BS","SB","TLI","TS","SL","CUR",
	"VIS","BRI","PED","PEDc","CARd","MOTOd","BIKEd","PEDd","TRUCKd")

# creating vector with mean values for each node
#mean_data<-sapply(network_data,mean)

#creating vector with mean values adjusted to proportional sizes to be plotted
#importance_vSize<-c(mean_data[1:14]/min(mean_data[1:14]),1.81)

#building network figures 
# 3 types are created to get an avarege position and layout
#GLASSO NETWORK
# network_glasso<-qgraph(cor_data,layout="spring",
# 	vsize=6,esize=20,graph="glasso",
# 	sampleSize=nrow(bea_data),
# 	legend.cex = 0.5,GLratio=1.5)

# #PARTIAL CORRELATION NETWORK
# network_pcor<-qgraph(cor_data,layout="spring",
# 	vsize=6,esize=20,graph="pcor",threshold="holm",
# 	sampleSize=nrow(bea_data),
# 	legend.cex = 0.5,GLratio=1.5)

# #CORRELATION NETWORK
# network_cor<-qgraph(cor_data,layout="spring",
# 	vsize=6,esize=20,legend.cex = 0.5,GLratio=1.5)
# #layout1<-averageLayout(network_glasso,network_pcor,network_cor)

# # Organizing both figures to be with the same layout
# layout_final<-averageLayout(network_glasso,
# 	network_pcor,
# 	network_cor)

#postscript("/home/joao/Desktop/info_consent_figure2.eps",
#	width = 1500, height = 1200,horizontal = FALSE, 
#	onefile = FALSE)
#postscript("/Users/joaovissoci/Desktop/info_consent_figure2.eps",
#	width = 1500, height = 1200,horizontal = FALSE, 
#	onefile = FALSE)
tiff("/Users/jnv4/Desktop/bea_pca_network.tiff", width = 1200,
 height = 700,compression = 'lzw')
final_importance_network<-qgraph(cor_data,
	esize=20,layout="spring",graph="glasso",
	sampleSize=nrow(analytical_data),
	legend.cex = 0.6,cut = 0.3, maximum = 1, 
	minimum = 0.1, esize = 20,vsize = 5, 
	repulsion = 0.8,nodeNames=rownames(cor_data),
	borders = TRUE)#,groups=network_groups,
	#labels=node_labels,
	#color=c("gold","steelblue","red","grey80","green"),borders = FALSE,
	#)#,gray=T,)#,nodeNames=nomesqsg,layoutScale=c(2,2)
dev.off()
#legend(0.8,-0.8, bty=".",c("Ensaio Clínico","Medicamentos","Outras Razões"),cex=1.2,fill=c("lightblue","red","yellow"))



