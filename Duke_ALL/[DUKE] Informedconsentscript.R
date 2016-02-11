##############################################################
#informed_consent_tradeoff.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. see full license at the end of this file.
##############################################################
#this script follows a combination of the guidelines proposed by Hadley Wickham http://goo.gl/c04kq as well as using the formatR package http://goo.gl/ri6ky
#if this is the first time you are conducting an analysis using this protocol, please watch http://goo.gl/DajIN while following step by step

#link to manuscript

##############################################################
#SETTING ENVIRONMENT
##############################################################

#command below will install individual and is only run once. remove the hash tag if this is the first time you are running the code on RStudio, and then you can add the hash tag again
#install.packages("car", repos="http://cran.r-project.org")
#install.packages("ggplot2", repos="http://cran.r-project.org")

#command below will install each package. if you run this script from the beginning you need to run every single one again
lapply(c("epicalc", "sem","Hmisc","ggplot2", "psych", "irr", "nortest", 
	"moments","GPArotation","nFactors","repmis","gdata","qgraph",
	"igraph"), 
library, character.only=T)

##############################################################
#IMPORTING DATA AND RECODING
##############################################################

#if you are using a file that is local to your computer, then replace path below by path to the data file. command will throw all the data into the templateData object. replace the word template.data by a name that might easier for you to remember and that represents your data
infoconsent <- read.csv("/home/joao/Dropbox/datasets/infoconsent2.csv")

infoconsent <- read.csv("/Users/joaovissoci/Dropbox/datasets/infoconsent2.csv")

#subsetting data only for consenting participants
bancocerto <-subset(infoconsent, V10==1)

#below will list variable names, classes (integer, factor, etc), alternative responses
str(bancocerto)
#list variable names so that they can be used later
names(bancocerto)
#below will attach your data so that when you execute a command you don't have to write the name of your data object over and over again
#attach(bancocerto)
View(bancocerto)

#Creating a dataset with only Importance questions
Import<-with(bancocerto,data.frame(Q36_1,Q36_2,Q36_3,Q36_4,Q36_5,Q36_6,
	Q36_7,Q36_8,Q36_9,Q36_10,Q36_11,Q36_12,Q36_13,Q36_14))
#import_score<-rowMeans(Import)

#Creating a dataset with only Understanding questions
Under<-with(bancocerto,data.frame(Q46_1,Q46_2,Q46_3,Q46_4,Q46_5,Q46_6,
	Q46_7,Q46_8,Q46_9,Q46_10,Q46_11,Q46_12,Q46_13,Q46_14))
#under_score<-rowMeans(Under)

#Creating a dataset with only read/noread data
#read<-with(bancocerto,data.frame(Q8,Q9,Q10,Q14,Q16,Q18,Q20,Q22,Q24,Q26,Q28,Q30,Q32,Q34))

#sem_data<-data.frame(Import,Under,outcome=Respo)

#Recoding Gender variable
Gender<-as.factor(bancocerto$Q37)

#Reconding Response (Willignness TO participate) variable
Respo<-car::recode(bancocerto$Q13,"1=1;2=0")

#Recoding Race variable
Race<-as.factor(bancocerto$Q39)

##################################################################
#DESCRIPTIVES AND ITEM COMPARISONS
##################################################################
#Categorical data example
summary(Gender) #Choose one variable

#General descriptives
Hmisc::describe(bancocerto)

#General descriptives with a different code
psych::describe(bancocerto)

#Descriptives
Hmisc::describe(Gender)
Hmisc::describe(Race)
Hmisc::describe(Import)
Hmisc::describe(Under)

# Importance and Comprehension descriptives by WP
by(Import,Respo,psych::describe)
by(Under,Respo,psych::describe)

### PLOTING DESCRIPTIVES FOR EACH QUESTION BY WP
#Calculating descriptives for Importance questions by Willingness to participate (Respo)
x<-by(Import,Respo,psych::describe)
import_yes<-x[[1]]$median #create vector with median values for yes
import_no<-x[[2]]$median #create vector with median values for no

#Calculating descriptives for Importance questions by Willingness to participate (Respo)
y<-by(Under,Respo,psych::describe)
under_yes<-y[[1]]$median  #create vector with median values for yes
under_no<-y[[2]]$median #create vector with median values for no

scores<-c(import_yes,import_no,under_yes,under_no)  #building a vector aggregating median values for each item for importance and comprehension by WP

will<-as.factor(c(rep(c('yes'),14),rep(c('no'),14),rep(c('yes'),14),rep(c('no'),14))) #building a grouping variable for yes/no WP

will2<-c(rep(c('Import_yes'),14),rep(c('Import_no'),14),
	rep(c('Under_yes'),14),rep(c('Under_no'),14)) ##building a grouping variable for yes/no WP responses for importance and comprehension


questions<-rep(c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9",
			"Q10","Q11","Q12","Q13","Q14"),4) #building a grouping variable for lables of each item. This is repeated 4 times becuase of the grouping Ye/No, for importance and comprehension


groups<-as.factor(c(rep("Importance",28),rep("Comprehension",28))) #building a grouping variable items separating by imortance and comprehension

plot<-data.frame(scores,will,questions,groups,will2) #aggregating all vectors into a data frame to be plotted

# Isolating first (Q1) and third (Q3) quantiles for comprehension by Yes/No WP
under_1quantiles_yes<-sapply(subset(Under,Respo==1),summary)[2,]
under_1quantiles_no<-sapply(subset(Under,Respo==2),summary)[2,]
import_1quantiles_yes<-sapply(subset(Import,Respo==1),summary)[2,]
import_1quantiles_no<-sapply(subset(Import,Respo==2),summary)[2,]
plot$quantiles_1<-c(import_1quantiles_yes,import_1quantiles_no,under_1quantiles_yes,under_1quantiles_no) #adding to the plot data.frame object

# Isolating first (Q1) and third (Q3) quantiles for importance by Yes/No WP
under_3quantiles_yes<-sapply(subset(Under,Respo==1),summary)[5,]
under_3quantiles_no<-sapply(subset(Under,Respo==2),summary)[5,]
import_3quantiles_yes<-sapply(subset(Import,Respo==1),summary)[5,]
import_3quantiles_no<-sapply(subset(Import,Respo==2),summary)[5,]
plot$quantiles_3<-c(import_3quantiles_yes,import_3quantiles_no,under_3quantiles_yes,under_3quantiles_no)  #adding to the plot data.frame object

# plot figure
#creating EPS vector type file for Figure 1
#postscript("/home/joao/Desktop/info_consent_figure1.eps",
#	width = 1500, height = 1200,horizontal = FALSE, 
#	onefile = FALSE)
tiff("/Users/joaovissoci/Desktop/descriptives.tiff",
	width = 600, height = 400,compression = 'lzw')
ggplot(data=plot, aes(x=questions, y=scores,group=will, color=will)) +
	 geom_line(size=1.5) + facet_grid(groups ~.) + 
	 geom_point(size=3,fill="white") + 
	 theme_bw()+ 
	 ylab("Median") + 
	 xlab("Questions") + 
	 scale_colour_manual(values=c("#999999","darkred"), 
	 	name="Willing to Participate", breaks=c("yes","no"),
	 	labels=c("Yes", "No"))+ 
	 theme(legend.position=c(0.45,0.1)) + 
	 geom_segment(aes(x = questions, y = quantiles_1, xend = questions,
	 	yend = quantiles_3)) + 
	 scale_x_discrete(limits=c("Q1","Q2","Q3","Q4","Q5","Q6",
	 	"Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14"))
dev.off()

# pairwise non-parametric comparison for each item in comprehension by WP
wilcox.test(Under[,1]~Respo)
wilcox.test(Under[,2]~Respo)
wilcox.test(Under[,3]~Respo)
wilcox.test(Under[,4]~Respo)
wilcox.test(Under[,5]~Respo)
wilcox.test(Under[,6]~Respo)
wilcox.test(Under[,7]~Respo)
wilcox.test(Under[,8]~Respo)
wilcox.test(Under[,9]~Respo)
wilcox.test(Under[,10]~Respo)
wilcox.test(Under[,11]~Respo)
wilcox.test(Under[,12]~Respo)
wilcox.test(Under[,13]~Respo)
wilcox.test(Under[,14]~Respo)

# pairwise non-parametric comparison for each item in importance by WP
wilcox.test(Import[,1]~Respo)
wilcox.test(Import[,2]~Respo)
wilcox.test(Import[,3]~Respo)
wilcox.test(Import[,4]~Respo)
wilcox.test(Import[,5]~Respo)
wilcox.test(Import[,6]~Respo)
wilcox.test(Import[,7]~Respo)
wilcox.test(Import[,8]~Respo)
wilcox.test(Import[,9]~Respo)
wilcox.test(Import[,10]~Respo)
wilcox.test(Import[,11]~Respo)
wilcox.test(Import[,12]~Respo)
wilcox.test(Import[,13]~Respo)
wilcox.test(Import[,14]~Respo)


##############################################################
#PCA Score comparisons
#############################################################
# Comparing each question

# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 

summary(rowMeans(Import))
importance_scores <- principal(Import,1,rotate="varimax",scores=TRUE)
summary(importance_scores) # print variance accounted for 
loadings(importance_scores) # pc loadings 
importance_scores$scores
#predict(fit,Import)
#importance_scores<-scoreItems(fit$weights,Import,totals=TRUE)

summary(rowMeans(Under))
understanding_scores <- principal(Under,1,rotate="varimax",scores=TRUE)
summary(understanding_scores) # print variance accounted for 
loadings(understanding_scores) # pc loadings 
understanding_scores$scores
#predict(fit,Import)
#importance_scores<-scoreItems(fit$weights,Import,totals=TRUE)

#reading_scores <- principal(read,1,rotate="varimax",scores=TRUE)
#summary(fit) # print variance accounted for 
#loadings(fit) # pc loadings 
#reading_scores$scores
#predict(fit,Import)
#importance_scores<-scoreItems(fit$weights,Import,totals=TRUE)

#calculating ODDS ratio for WP by importance and comprehension Scores
log_model<-data.frame(Respo,imppimportance_scores$scores,
	understanding_scores$scores)#,reading_scores$scores)
log_model<-na.omit(log_model)

# fitting the model
fit <- glm(as.factor(Respo)~importance_scores$scores+
	understanding_scores$scores,family=binomial)

#getting summary and coefficients 
summary(fit)
exp(coef(fit)) # exponentiated coefficients
exp(confint(fit)) # 95% CI for exponentiated coefficients


##############################################################
#NETWORK APPROACH
#############################################################

################# Importance ###################

#organizing dataset
importance_network_data<-data.frame(Import,outcome=Respo)#,outcome=Respo)
importance_network_data<-na.omit(importance_network_data) #ommiting NA

#creating correlation matrix
importance_cor_data<-cor_auto(importance_network_data)
#qsgc<-qsgc$rho

#listing grouping variables in the network resulting from the community analysis
importance_network_groups<-list(Import1=c(1,2,3,4,5,15),
	Import2=c(6,8,12,14),
	Import3=c(7,9,10,11,13))

# creating vectors for labels
importance_node_labels<-c("Why is this study being done?", 
	"What is involved in this study?", 
	"Who is going to be my doctor in this study?",
	"How many people will take part in this study?",
	"How long will I be in this study?",
	"What are the benefits of being in this study?",
	"What about compensation?",
	"What are the risks of being in this study?",
	"What are the costs?",
	"Will my information be kept confidential?",
	"What about research related injuries?",
	"What are the alternatives to being in this study?",
	"What if I want decline participation or withdraw?",
	"Whom do I call if I have questions or trouble?",
	"Willingness to participate")

# creating nodes labels vector
importance_node_names<-c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9",
	"Q10","Q11","Q12","Q13","Q14","WP")

# creating vector with mean values for each node
mean_data<-sapply(importance_network_data,mean)

#creating vector with mean values adjusted to proportional sizes to be plotted
importance_vSize<-c(mean_data[1:14]/min(mean_data[1:14]),1.81)

#building network figures 
# 3 types are created to get an avarege position and layout
#GLASSO NETWORK
importance_network_glasso<-qgraph(importance_cor_data,layout="spring",
	vsize=6,esize=20,graph="glasso",
	sampleSize=nrow(importance_network_data),
	legend.cex = 0.5,GLratio=1.5)

#PARTIAL CORRELATION NETWORK
importance_network_pcor<-qgraph(importance_cor_data,layout="spring",
	vsize=6,esize=20,graph="pcor",threshold="holm",
	sampleSize=nrow(importance_network_data),
	legend.cex = 0.5,GLratio=1.5)

#CORRELATION NETWORK
importance_network_cor<-qgraph(importance_cor_data,layout="spring",
	vsize=6,esize=20,legend.cex = 0.5,GLratio=1.5)
#layout1<-averageLayout(network_glasso,network_pcor,network_cor)

#Calculating Community measures
g<-as.igraph(importance_network_glasso) #creating igraph object
#h<-walktrap.community(g) #creatin community object
h<-spinglass.community(g, weights=NA)
plot(h,g) #plotting community network
h$membership #extracting community membership for each node on the network

#Identify SPLs within the graph and extract direct paths to WP
predictors<-centrality(importance_network_glasso)$ShortestPaths[,15]
predictors

#getting edge list with edges originating, receiveing and weights
importance_network_glasso$Edgelist$from
importance_network_glasso$Edgelist$to
importance_network_glasso$Edgelist$weight

#Extracting edges weights for each direct path
subset(importance_network_glasso$Edgelist$weight,
	importance_network_glasso$Edgelist$from==1 & 
	importance_network_glasso$Edgelist$to==15)
subset(importance_network_glasso$Edgelist$weight,
	importance_network_glasso$Edgelist$from==2 & 
	importance_network_glasso$Edgelist$to==15)
subset(importance_network_glasso$Edgelist$weight,
	importance_network_glasso$Edgelist$from==3 & 
	importance_network_glasso$Edgelist$to==15)
subset(importance_network_glasso$Edgelist$weight,
	importance_network_glasso$Edgelist$from==10 & 
	importance_network_glasso$Edgelist$to==15)
subset(importance_network_glasso$Edgelist$weight,
	importance_network_glasso$Edgelist$from==13 & 
	importance_network_glasso$Edgelist$to==15)

#conducting logitic regression with variables showing direct path in the network
log_model<-data.frame(Respo,Import$Q36_1,Import$Q36_2,
	Import$Q36_3,Import$Q36_10,Import$Q36_13)#,reading_scores$scores)

#fitting the model
fit <- glm(as.factor(Respo)~Import$Q36_1+Import$Q36_2
	+Import$Q36_3+Import$Q36_10+Import$Q36_13,
	data=log_model,family=binomial)

#summary and coefficients
summary(fit)
exp(coef(fit)) # exponentiated coefficients
exp(confint(fit)) # 95% CI for exponentiated coefficients

################# Comprehension ###################

#organizing datasets
comprehension_network_data<-data.frame(Under,Respo)#,reading_scores$scores)
comprehension_network_data<-na.omit(comprehension_network_data) #omitting NAs

#creating correlation matrix
comprehension_cor_data<-cor_auto(comprehension_network_data)
#qsgc<-qsgc$rho

#listing grouping variables in the network resulting from the 
#community analysis
comprehension_node_groups<-list(Under1=c(1,2,3,4,5,6,7,15),
	Under2=c(8,9,10,11,12,13,14))

# creating vectors for labels
comprehension_node_labels<-c("Why is this study being done?", 
	"What is involved in this study?", 
	"Who is going to be my doctor in this study?",
	"How many people will take part in this study?",
	"How long will I be in this study?",
	"What are the benefits of being in this study?",
	"What about compensation?",
	"What are the risks of being in this study?",
	"What are the costs?",
	"Will my information be kept confidential?",
	"What about research related injuries?",
	"What are the alternatives to being in this study?",
	"What if I want decline participation or withdraw?",
	"Whom do I call if I have questions or trouble?",
	"Willingness to participate")

# creating nodes labels vector
comprehension_node_names<-c("Q1","Q2","Q3","Q4","Q5","Q6","Q7",
	"Q8","Q9","Q10","Q11","Q12","Q13","Q14","WP")

# creating vector with mean values for each node
mean_data<-sapply(comprehension_network_data,mean)

#creating vector with mean values adjusted to proportional 
#sizes to be plotted
comprehension_vSize<-c(mean_data[1:14]/min(mean_data[1:14]),1.81)

#building network figures 
# 3 types are created to get an avarege position and layout
#GLASSO NETWORK
comprehension_network_glasso<-qgraph(comprehension_cor_data,layout="spring",
	vsize=6,esize=20,graph="glasso",
	sampleSize=nrow(comprehension_network_data),
	legend.cex = 0.5,GLratio=1.5)

#PARTIAL CORRELATION NETWORK
comprehension_network_pcor<-qgraph(comprehension_cor_data,layout="spring",
	vsize=6,esize=20,graph="pcor",threshold="holm",
	sampleSize=nrow(comprehension_network_data),
	legend.cex = 0.5,GLratio=1.5)

#CORRELATION NETWORK
comprehension_network_cor<-qgraph(comprehension_cor_data,layout="spring",
	vsize=6,esize=20,legend.cex = 0.5,GLratio=1.5)
#layout2<-averageLayout(network_glasso,network_pcor,network_cor)

#Calculating Community measures
g<-as.igraph(comprehension_network_glasso) #creating igraph object
#h<-walktrap.community(g) #creatin community object
h<-spinglass.community(g, weights=NA)
plot(h,g) #plotting community network
h$membership #extracting community membership for each node on the network

#Identify SPLs within the graph and extract direct paths to WP
predictors<-centrality(comprehension_network_glasso)$ShortestPaths[,15]
predictors

#Extracting edges weights for each direct path
subset(comprehension_network_glasso$Edgelist$weight,
	comprehension_network_glasso$Edgelist$from==1 & 
	comprehension_network_glasso$Edgelist$to==15)
subset(comprehension_network_glasso$Edgelist$weight,
	comprehension_network_glasso$Edgelist$from==3 & 
	comprehension_network_glasso$Edgelist$to==15)
subset(comprehension_network_glasso$Edgelist$weight,
	comprehension_network_glasso$Edgelist$from==14 & 
	comprehension_network_glasso$Edgelist$to==15)

#conducting logitic regression with variables
#showing direct path in the network
log_model<-data.frame(Respo,Under$Q46_1,Under$Q46_3,
	Under$Q46_14)#,reading_scores$scores)
#log_model<-na.omit(log_model)

#fitting the model
fit <- glm(as.factor(Respo)~Under$Q46_1+
	Under$Q46_3+Under$Q46_14,
	data=log_model,family=binomial)

#summary and coefficients
summary(fit)
exp(coef(fit)) # exponentiated coefficients
exp(confint(fit)) # 95% CI for exponentiated coefficients

############## FINAL FIGURES ############################
# Organizing both figures to be with the same layout
layout_final<-averageLayout(comprehension_network_glasso,
	comprehension_network_pcor,
	comprehension_network_cor,
	importance_network_glasso,
	importance_network_glasso,
	importance_network_glasso)

#postscript("/home/joao/Desktop/info_consent_figure2.eps",
#	width = 1500, height = 1200,horizontal = FALSE, 
#	onefile = FALSE)
#postscript("/Users/joaovissoci/Desktop/info_consent_figure2.eps",
#	width = 1500, height = 1200,horizontal = FALSE, 
#	onefile = FALSE)
tiff("/Users/joaovissoci/Desktop/importance_network.tiff", width = 1200,
 height = 700,compression = 'lzw')
final_importance_network<-qgraph(importance_cor_data,
	layout=layout_final,vsize=importance_vSize*3,
	esize=20,graph="glasso",
	sampleSize=nrow(importance_network_data),
	legend.cex = 0.6,cut = 0.1, maximum = 1, 
	minimum = 0, esize = 20,vsize = 5, 
	repulsion = 0.8,groups=importance_network_groups,
	nodeNames=importance_node_labels,
	color=c("gold","steelblue","red","grey80",
	layoutScale=c(2,2)),borders = FALSE,
	labels=importance_node_names)#,gray=T,)#,nodeNames=nomesqsg
dev.off()
#legend(0.8,-0.8, bty=".",c("Ensaio Clínico","Medicamentos","Outras Razões"),cex=1.2,fill=c("lightblue","red","yellow"))

#postscript("/home/joao/Desktop/info_consent_figure3.eps",
#	width = 20, height = 5,horizontal = FALSE, 
#	onefile = FALSE)
tiff("/Users/joaovissoci/Desktop/comprehension_network.tiff",
	width = 1200, height = 700,compression = 'lzw')
final_comprehension_network<-qgraph(comprehension_cor_data,
	layout=layout_final,vsize=comprehension_vSize*3,
	esize=20,graph="glasso",
	sampleSize=nrow(comprehension_network_data),
	legend.cex = 0.6,cut = 0.1, maximum = 1,
	minimum = 0, esize = 20,vsize = 5,
	repulsion = 0.8,groups=comprehension_node_groups,
	nodeNames=comprehension_node_labels,
	color=c("gold","steelblue","red","grey80"),
	borders = FALSE,labels=comprehension_node_names)
	#,gray=T,)#,nodeNames=nomesqsg,layoutScale=c(2,2)
dev.off()