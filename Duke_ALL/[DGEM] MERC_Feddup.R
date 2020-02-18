######################################################################
#BASIC R STATISTICS TEMPLATE
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
lapply(c("sem","ggplot2", "psych", "irr", "nortest", "moments",
	"GPArotation","nFactors","boot","psy", "car","vcd", "gridExtra",
	"mi","VIM","epicalc","gdata","sqldf","reshape2","mclust",
	"foreign","survival","memisc","foreign","mice","MissMech",
	"haven","qgraph","semPlot","readr"), 
library, character.only=T)

######################################################################
#IMPORTING DATA
######################################################################
#LOADING DATA FROM A .CSV FILE

library(readr)

data_residents<-read_csv("/Users/joaovissoci/Downloads/MERCResidentSurvey_DATA_2019-09-11_0745.csv")
#information between " " are the path to the directory in your computer where the data is stored

data_faculty<-read_csv("/Users/joaovissoci/Downloads/MERCFacultySurvey_DATA_2019-09-11_0744.csv")

######################################################################
#DATA MANAGEMENT
######################################################################

# data <- base::merge(data1,data2,by=c("nome"))

#SCALE DATA FRAME

data_residents$group<-c("residents")
data_faculty$group<-c("faculty")

questions_data_residents<-with(data_residents,data.frame(question_1,
									 question_2,
									 question_3,
									 question_4,
									 question_5,
									 question_6,
									 question_7,
									 question_8,
									 question_9,
									 question_10,
									 question_11,
									 question_12,
									 question_13,
									 question_14,
									 question_15,
									 question_16,
									 question_17,
									 question_18,
									 question_19,
									 question_20,
									 question_21,
									 question_22,
									 question_23,
									 question_24,
									 question_25))

questions_data_faculty<-with(data_faculty,data.frame(question_1,
									 question_2,
									 question_3,
									 question_4,
									 question_5,
									 question_6,
									 question_7,
									 question_8,
									 question_9,
									 question_10,
									 question_11,
									 question_12,
									 question_13,
									 question_14,
									 question_15,
									 question_16,
									 question_17,
									 question_18,
									 question_19,
									 question_20,
									 question_21,
									 question_22,
									 question_23,
									 question_24,
									 question_25))

data_question_all<-rbind(questions_data_residents,
							questions_data_faculty)


# data$hiv_related_stigma_sumscore<-rowSums(hiv_related_stigma)

# recode_likert<-function(x){
#     car::recode(x,"0=1;
#     			   1=2;
#     			   2=3;
#     			   3=4")
#     }

# hiv_related_stigma_recoded<-lapply(hiv_related_stigma,recode_likert)
# hiv_related_stigma_recoded<-as.data.frame(hiv_related_stigma_recoded)



#######################################################
#ANALYZING MISSING DATA
#######################################################
#Studying missing data
#Calculating frequency of missing data per variable
propmiss <- function(dataframe) lapply(dataframe,function(x) data.frame(nmiss=sum(is.na(x)), n=length(x), propmiss=sum(is.na(x))/length(x)))

propmiss(data_question_all)

#inspecting measure random of missing data
#Inspectif Weather Conditions
#weather_missing<-car::recode(data_epi$weather_condition,"NA=0;else=1")
#logmodel<-glm(weather_missing ~  data_epi$day_crash,family=binomial)
#summary(logmodel)
#logmodel<-glm(weather_missing ~  data_epi$hour_crash,family=binomial)
#summary(logmodel)
#logmodel<-glm(weather_missing ~  data_epi$road_type,family=binomial)
#summary(logmodel)
#logmodel<-glm(weather_missing ~  data_epi$road_condition,family=binomial)
#summary(logmodel)
#logmodel<-glm(weather_missing ~  data_epi$visibility,family=binomial)
#summary(logmodel)
#logmodel<-glm(weather_missing ~  data_epi$type_vehicle,family=binomial)
#summary(logmodel)
#logmodel<-glm(weather_missing ~  data_epi$type_vehicle2,family=binomial)
#summary(logmodel)
#logmodel<-glm(weather_missing ~  data_epi$gender,family=binomial)
#summary(logmodel)
#logmodel<-glm(weather_missing ~  data_epi$crash_type,family=binomial)
#summary(logmodel)

# #out <- TestMCARNormality(data_epi)
# #missing.pattern.plot(data_epi)
# #MICE framework for imputation
# # describing the pattern of missingnesss
# md.pattern(data_epi)

# # showing pairs of missingines
# md.pairs(data_epi)

# # plots impact of missing data for a set of pairs - works better for numerical data
# marginplot(data.frame(data_epi$outcome,data_epi$visibility), col = mdc(1:2), cex = 1.2, cex.lab = 1.2, cex.numbers = 1.3, pch = 19)

# # generate imputations

# # organize data set to be imputed
# data_tobeimp<-with(data_epi,data.frame(hour_crash,urban_location,outcome,
# 	holiday,day_week,crash_type,rd_condition,weather,light_condition,
# 	type_location,traffic_control,speed_limit_sign,type_vehicle,
# 	human_crash_factor,ped_precrash,alcohol_tested,victim_classification,
# 	rd_size))

# argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
imp <- mice(data_question_all, seed = 2222, m=10)

# reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
data_question_all_imp<-mice::complete(imp,4)

#Plost the distrbution of each of the 5 possibilities of imputations
#stripplot(imp,pch=20,cex=1.2)

#plots a scatter plot of pairs of variables
#xyplot(imp, outcome ~ visibility | .imp, pch = 20, cex = 1.4)

#returns the matrix specifying each variable used to -predict imputation - columns 1=predictor 0=not predictor. rows are the variables of interest
#imp$predictorMatrix
#pred <- imp$predictorMatrix #if you want to exclude  variable from the prediction model for imputation then assign an obect to pred
#pred[, "bmi"] <- 0 #transform the column values into 0's for not predictiong
#imp <- mice(nhanes, pred = pred, pri = FALSE) # rerun the model specifying pred argumento witht eh matriz recoded.

######################################################################
#TABLE 1 - DESCRIPTIVEs
######################################################################

######################################################################
#FIGURE 1 - ITEMs Descriptions
######################################################################

describe(data$hiv_related_stigma_sumscore)
table(data$hiv_related_stigma_sumscore)


#KNOWLEDGE

questions_data_categorical<-data_question_all_imp

questions_data_categorical[questions_data_categorical==1]<-"Strongly Disagree"
questions_data_categorical[questions_data_categorical==2]<-"Disagree"
questions_data_categorical[questions_data_categorical==3]<-"I don't know"
questions_data_categorical[questions_data_categorical==4]<-"Agree"
questions_data_categorical[questions_data_categorical==5]<-"Strongly Agree"

# hiv_related_stigma_likert[,4][hiv_related_stigma_likert[,4]==1]<-"Strongly Agree"
# hiv_related_stigma_likert[,4][hiv_related_stigma_likert[,4]==2]<-"Agree"
# hiv_related_stigma_likert[,4][hiv_related_stigma_likert[,4]==3]<-"I don't know"
# hiv_related_stigma_likert[,4][hiv_related_stigma_likert[,4]==4]<-"Disagree"
# hiv_related_stigma_likert[,4][hiv_related_stigma_likert[,4]==5]<-"Strongly Disagree"

mylevels <- c("Strongly Disagree", "Disagree","I don't know","Agree", "Strongly Agree")

for(i in seq_along(questions_data_categorical)) {
	questions_data_categorical[,i] <- factor(questions_data_categorical[,i], levels=mylevels)
}

questions_data_categorical<-likert::likert(na.omit(questions_data_categorical))

# knowledge_data$Group<-c("Knowledge")
# summary(knowledge_data)
questions_data_categorical_plot<-plot(questions_data_categorical,
					 colors=c("#D33F6A",
					 		  "#E07B91",
					 		  "lightgrey",
					 		  "#8595E1",
					 		  "#4A6FE3"))

questions_data_categorical_plot<- questions_data_categorical_plot + 
								# scale_x_discrete(labels=c(
							 #  "Getting HIV is a punishment for bad behavior.",
    				# 		  "I would think less of someone if I found out the person has HIV.",
    				# 		  "I would be upset if someone with HIV moved in next door to me.",
    				# 		  "I feel uncomfortable around someone with HIV.",
    				# 	      "People with HIV have only themselves to blame for getting HIV.",
    				# 	      "People with HIV must have done something wrong to get it.",
    				# 	      "People with HIV should feel ashamed about having HIV.",
    				# 	      "I would be ashamed if someone in my family has HIV.",
    				# 	      "If I was in public or private transport, \n I would not like to sit next to someone with HIV.",
    				# 	      "I would not like to be friends with someone with HIV.",
    				# 	      "I would not employ someone with HIV.",
    				# 	      "I would not eat together with someone I knew had HIV.",
    				# 	      "If a relative of mine became ill with HIV, \n I would not want to care for that \n person in my home.",
    				# 	      "I would not want to buy food from someone I know has HIV.",
    				# 	      "If a teacher has HIV but is not sick, she should not \n be allowed to continue teaching in the school.",
    				# 	      "I would not want someone with HIV to look after my child.",
    				# 	      "I do not want to get too close to someone with HIV \n because I am afraid I might get infected with HIV.",
    				# 	      "I would not want my child to play with a \n child who has HIV or whose parents have HIV."
    				# 	      ),
								# 					breaks=c(
								# "HSS1",
								# "HSS2",
								# "HSS3",
								# "HSS4",
								# "HSS5",
								# "HSS6",
								# "HSS7",
								# "HSS8",
								# "HSS9",
								# "HSS10",
								# "HSS11",
								# "HSS12",
								# "HSS13",
								# "HSS14",
								# "HSS15",
								# "HSS16",
								# "HSS17",
								# "HSS18"
							    							
							  # )) +
				theme_bw() +
				theme(legend.position="bottom")

# summary_data<-summary(knowledge_data)

# mean_plot<-ggplot(summary_data, aes(y=mean,x=Item)) + 
#     geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) +
#     geom_line() +
#     geom_point() +
#     coord_flip() +
#     xlab("") +
#     ylab("Mean/Standard Deviation") +
#     scale_x_discrete(limits=rev(c("talking_can_be_successful",
#     							  "discuss_risky_alc",
#     							  "discuss_counsel_pts",
#     							  "not_my_role",
#     							  "called_harmful_drinkers"))) +
#     expand_limits(y=c(1:5)) +
#     theme_bw() +
#     theme(panel.background = element_rect(colour = 'grey'))


######################################################################
#Figure 1 - Network
######################################################################


# # Define the amout of factor to retain
#Group of functinos to determine the number os items to be extracted
cor_data<-cor_auto(data_question_all_imp)

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

# network_groups<-list(
# Component1=c(1,3,4,5,15,14),
# Component2=c(2,16,6,7),
# Component3=c(11,12,13,10),
# Component4=c(19,20,21,23),
# Component5=c(9,17,18,22,8)
# )

# creating vectors for labels
# node_labels<-c(
# "What is the area of the roadway?",
# "What type of roadway?",
# "Is this point at an intersection/junction?",
# "How many lanes in the roadway?",
# "Is there an auxiliary/other lane?",
# "How is the road surface conditions?",
# "Is there space on the side of the road 
# for any reason or use?",
# "Are there pedestrian pathways?",
# "Is there a Bus Stop?",
# "Is there a Speed bump?",
# "Is there a traffic light at this location?",
# "Are there road traffic signs at this hotspot?",
# "Is there a sign for speed limit of road?",
# "Road visibility is influenced by curves?",
# "Is the visibility influenced by 
# environmental factors?",
# "Are there bridges on the road?",
# "Is there a safe area for pedestrians 
# to cross the road?",
# "Is there a safe area for pedestrians
# to in the center of the road?",
# "Count the number of cars",
# "Count the number of moto",
# "Count the number of bike",
# "Count the number of pedestrians",
# "Count the number of bus/trucks"
# )

# creating nodes labels vector
# node_names<-c("RD","RT","INT","TLA","AR",
# 	"RC","RS",
# 	"WALK","BS","SB","TLI","TS","SL","CUR",
# 	"VIS","BRI","PED","PEDc","CARd","MOTOd","BIKEd","PEDd","TRUCKd")

# creating vector with mean values for each node
#mean_data<-sapply(network_data,mean)

#creating vector with mean values adjusted to proportional sizes to be plotted
#importance_vSize<-c(mean_data[1:14]/min(mean_data[1:14]),1.81)

#postscript("/home/joao/Desktop/info_consent_figure2.eps",
#	width = 1500, height = 1200,horizontal = FALSE, 
#	onefile = FALSE)
#postscript("/Users/joaovissoci/Desktop/info_consent_figure2.eps",
#	width = 1500, height = 1200,horizontal = FALSE, 
#	onefile = FALSE)
# tiff("/Users/jnv4/Desktop/bea_pca_network.tiff", width = 1200,
 # height = 700,compression = 'lzw')
network <-qgraph(cor_data,
	graph="glasso",
	layout="spring",
	sampleSize=nrow(data_question_all_imp),
	legend.cex = 0.6,
	cut = 0.3, 
	maximum = 1, 
	minimum = 0.1, 
	esize = 20,
	vsize = 5, 
	repulsion = 0.8,
	threshold=TRUE)#,
	# groups=network_groups,
	# nodeNames=node_labels,
	# color=c("gold","steelblue","red","grey80","green"),
	# borders = FALSE,
	# labels=node_names,
	# gray=T,
	# nodeNames=nomesqsg,
	# layoutScale=c(2,2))
# dev.off()
#legend(0.8,-0.8, bty=

library(igraph)
# análise de comunidades 
# https://www.nature.com/articles/srep30750
g2<-as.igraph(network)
# se não tiver arestas negativas:
# g2_cl<-walktrap.community(g2)

# se houver arestas negativas
g2_sg<-cluster_louvain(g2)#,implementation = "neg")

community_network<-qgraph(cor_data,
	graph="glasso",
	layout="spring",
	sampleSize=nrow(data_question_all_imp),
	legend.cex = 0.6,
	cut = 0.3, 
	maximum = 1, 
	minimum = 0.1, 
	esize = 20,
	vsize = 5, 
	repulsion = 0.8,
	threshold=TRUE,
	groups=as.factor(g2_sg$membership))#,

######################################################################
#TABLE 2 - RELIABILITY and FACTOR ANALYSIS RESULTS
######################################################################

#ONE FACTOR
#############################################################
#psych::alpha(cor_data,n.iter=1000,check.keys=TRUE)
psych::alpha(data_question_all_imp,n.iter=1000,check.keys=TRUE)

#CONFIRMATORY FACTOR ANALYSIS

#identifying the model
onefactor_model <- 'FEEDUP =~  question_1 +
							   question_2 +
							   question_3 +
							   question_4 +
							   question_5 +
							   question_6 +
							   question_7 +
							   question_8 +
							   question_9 +
							   question_10 +
							   # question_11 +
							   question_12 +
							   # question_13 +
							   question_14 +
							   question_15 +
							   question_16 +
							   question_17 +
							   question_18 +
							   question_19 +
							   # question_20 +
							   # question_21 +
							   question_22 +
							   # question_23 +
							   question_24 +
							   question_25
									 
#Errors correlation
 			  # question_2 ~~  question_9
 			  # question_7 ~~  question_3
 			  # question_20 ~~  question_3				
'

#estimating the model
fit <- lavaan::cfa(onefactor_model,
                   data = data_question_all_imp,
                   estimator="WLSMV",
                   ordered=colnames(data_question_all_imp)
                   )

#summary statistics of h mofrl
summary(fit, fit.measures=TRUE)

#fitness indicators
lavaan::fitMeasures(fit, fit.measures = c("rmsea.scaled",
                                          "rmsea.ci.lower.scaled",
                                          "rmsea.ci.upper.scaled",
                                          "cfi.scaled",
                                          "tli.scaled",
                                          "nnfi.scaled",
                                          "chisq.scaled",
                                          "pvalue.scaled",
                                          "df"
)
)
# AIC(fit)

#Estimated
Est <- lavaan::parameterEstimates(fit, ci = TRUE, standardized = TRUE)
subset(Est, op == "=~")
subset(Est, op == "~~")
#lavInspect(fit,what="th") 

# Plot with semPlot:
# library(semPlot)

#node Names
# nodeNames <- c(
# 				"Getting HIV is a punishment for bad behavior.",
#     			"I would think less of someone if I found out the person has HIV.",
#     			"I would be upset if someone with HIV moved in next door to me.",
#     			"I feel uncomfortable around someone with HIV.",
#     			"People with HIV have only themselves to blame for getting HIV.",
#     			"People with HIV must have done something wrong to get it.",
#     			"People with HIV should feel ashamed about having HIV.",
#     			"I would be ashamed if someone in my family has HIV.",
#     			"If I was in public or private transport, \n I would not like to sit next to someone with HIV.",
#     			"I would not like to be friends with someone with HIV.",
#     			"I would not employ someone with HIV.",
#     			"I would not eat together with someone I knew had HIV.",
#     			"If a relative of mine became ill with HIV, \n I would not want to care for that \n person in my home.",
#     			"I would not want to buy food from someone I know has HIV.",
#     			"If a teacher has HIV but is not sick, she should not \n be allowed to continue teaching in the school.",
#     			"I would not want someone with HIV to look after my child.",
#     			"I do not want to get too close to someone with HIV \n because I am afraid I might get infected with HIV.",
#     		    "I would not want my child to play with a \n child who has HIV or whose parents have HIV."
#     					      )


# Now we can plot:
semPaths(fit,
    # style="lisrel",
    what = "std", # this argument controls what the color of edges represent. In this case, standardized parameters. Options are "paths"
    whatLabels = "std.all", # This argument controls what the edge labels represent. In this case, parameter estimates
    # style = "lisrel", # This will plot residuals as arrows, closer to what we use in class
    # residScale = 8, # This makes the residuals larger
    theme = "colorblind", # qgraph colorblind friendly theme
    nCharNodes = 0, # Setting this to 0 disables abbreviation of nodes
    # nCharEdges = 0, # Setting this to 0 disables abbreviation of edges
    # manifests = paste0("Q",1:19), # Names of manifests, to order them appropriatly.
    reorder = FALSE, # This disables the default reordering
    # nodeNames = paste0("Q",1:19), # Add a legend with node names
    # legend.cex = 0.5, # Makes the legend smaller
    rotation = 2, # Rotates the plot
    layout = "tree2", # tree layout options are "tree", "tree2", and "tree3"
    # cardinal = "lat cov", # This makes the latent covariances connet at a cardinal center point
    curvePivot = TRUE,                         # Changes curve into rounded straight lines
    sizeMan = 4, # Size of manifest variables
    residuals=TRUE,
    intercepts=FALSE,
    sizeLat = 10, # Size of latent variables
    mar = c(2,8,2,8.5), # Figure margins 1st=bottom,2nd=left,3rd=top,4th=right
    esize=TRUE,
    # structural="TRUE"
    # intStyle="multi",
    edge.label.cex = 0.95,
    node.label.cex = 1,
    equalizeManifests = FALSE,
    optimizeLatRes = TRUE, 
    shapeMan = "rectangle",
    node.width = 2,
    exoCov = FALSE,
    thresholds = FALSE,
    nodeLabels=c(paste0("Q",1:20),"FEED-UP"),
    edge.color="black"
    # shapeLat = "ellipse"
    # filetype = "pdf", width = 8, height = 6, filename = "StarWars" #  Save to PDF
    )

### Modification Indexes
Mod <- lavaan::modificationIndices(fit)
subset(Mod, mi > 10)

# Composite reliability
sum(Est$std.all[1:19])^2/(sum(Est$std.all[1:19])^2+
  sum(Est$std.all[95:113]))

#Average Extracted Variance
sum(Est$std.all[1:18]^2)/length(Est$std.all[1:18])

summary(Est$std.all[1:19])

#FIVE FACTORS
#############################################################
#psych::alpha(cor_data,n.iter=1000,check.keys=TRUE)
# psych::alpha(data_question_all_imp,n.iter=1000,check.keys=TRUE)

#CONFIRMATORY FACTOR ANALYSIS

#identifying the model
fivefactor_model <- 'FEEDUP_timeliness =~  question_1 +
							   question_5 +
							   question_7 +
							   question_12 +
							   question_24

			        FEEDUP_feedback_culture =~	question_2 +
							   question_3 +
							   question_10 +
							   # question_13 +
							   question_18
							   # question_21

					FEEDUP_specificity =~  question_4 +
							   question_14 +
							   question_17 +
							   question_22

					FEEDUP_actionplan =~ question_6 +
							   question_9 +
							   # question_11 +
							   question_15 +
							   question_16

					FEEDUP_respect =~ question_8 +
							   question_19 +
							   # question_20 +
							   # question_23 +
							   question_25

#Errors correlation
 			  # question_3 ~~  question_20
 			  # question_7 ~~  question_20
 			  # question_2 ~~ question_9
'

#estimating the model
fit <- lavaan::cfa(fivefactor_model,
                   data = data_question_all_imp,
                   estimator="WLSMV",
                   ordered=colnames(data_question_all_imp)
                   )

#summary statistics of h mofrl
summary(fit, fit.measures=TRUE)

#fitness indicators
lavaan::fitMeasures(fit, fit.measures = c("rmsea.scaled",
                                          "rmsea.ci.lower.scaled",
                                          "rmsea.ci.upper.scaled",
                                          "cfi.scaled",
                                          "tli.scaled",
                                          "nnfi.scaled",
                                          "chisq.scaled",
                                          "pvalue.scaled",
                                          "df"
)
)
# AIC(fit)

#Estimated
Est <- lavaan::parameterEstimates(fit, ci = TRUE, standardized = TRUE)
subset(Est, op == "=~")
subset(Est, op == "~~")
#lavInspect(fit,what="th") 

# Plot with semPlot:
# library(semPlot)

#node Names
# nodeNames <- c(
# 				"Getting HIV is a punishment for bad behavior.",
#     			"I would think less of someone if I found out the person has HIV.",
#     			"I would be upset if someone with HIV moved in next door to me.",
#     			"I feel uncomfortable around someone with HIV.",
#     			"People with HIV have only themselves to blame for getting HIV.",
#     			"People with HIV must have done something wrong to get it.",
#     			"People with HIV should feel ashamed about having HIV.",
#     			"I would be ashamed if someone in my family has HIV.",
#     			"If I was in public or private transport, \n I would not like to sit next to someone with HIV.",
#     			"I would not like to be friends with someone with HIV.",
#     			"I would not employ someone with HIV.",
#     			"I would not eat together with someone I knew had HIV.",
#     			"If a relative of mine became ill with HIV, \n I would not want to care for that \n person in my home.",
#     			"I would not want to buy food from someone I know has HIV.",
#     			"If a teacher has HIV but is not sick, she should not \n be allowed to continue teaching in the school.",
#     			"I would not want someone with HIV to look after my child.",
#     			"I do not want to get too close to someone with HIV \n because I am afraid I might get infected with HIV.",
#     		    "I would not want my child to play with a \n child who has HIV or whose parents have HIV."
#     					      )


# Now we can plot:
semPaths(fit,
    # style="lisrel",
    what = "std", # this argument controls what the color of edges represent. In this case, standardized parameters. Options are "paths"
    whatLabels = "std.all", # This argument controls what the edge labels represent. In this case, parameter estimates
    # style = "lisrel", # This will plot residuals as arrows, closer to what we use in class
    # residScale = 8, # This makes the residuals larger
    theme = "colorblind", # qgraph colorblind friendly theme
    nCharNodes = 0, # Setting this to 0 disables abbreviation of nodes
    # nCharEdges = 0, # Setting this to 0 disables abbreviation of edges
    # manifests = paste0("Q",1:19), # Names of manifests, to order them appropriatly.
    reorder = FALSE, # This disables the default reordering
    # nodeNames = paste0("Q",1:19), # Add a legend with node names
    # legend.cex = 0.5, # Makes the legend smaller
    rotation = 2, # Rotates the plot
    layout = "tree2", # tree layout options are "tree", "tree2", and "tree3"
    # cardinal = "lat cov", # This makes the latent covariances connet at a cardinal center point
    curvePivot = TRUE,                         # Changes curve into rounded straight lines
    sizeMan = 4, # Size of manifest variables
    # residuals=TRUE,
    intercepts=FALSE,
    sizeLat = 7, # Size of latent variables
    mar = c(2,8,2,8.5), # Figure margins 1st=bottom,2nd=left,3rd=top,4th=right
    esize=TRUE,
    # structural="TRUE"
    # intStyle="multi",
    edge.label.cex = 0.95,
    node.label.cex = 0.9,
    equalizeManifests = FALSE,
    # optimizeLatRes = TRUE, 
    shapeMan = "rectangle",
    node.width = 2,
    exoCov = FALSE,
    thresholds = FALSE,
    nodeLabels=c(paste0("Q",1:20),"Timeliness",
    							  "Feedback \nCulture",
    							  "Specificity",
    							  "Action/\nPlan",
    							  "Respect\nCommunication"),
    edge.color="black",
    # label.cex=1
    # shapeLat = "ellipse"
    # filetype = "pdf", width = 8, height = 6, filename = "StarWars" #  Save to PDF
    )

# qgraph(fit)

### Modification Indexes
Mod <- lavaan::modificationIndices(fit)
subset(Mod, mi > 10)

# Composite reliability
sum(Est$std.all[1:18])^2/(sum(Est$std.all[1:18])^2+
  sum(Est$std.all[109:131]))

#Average Extracted Variance
sum(Est$std.all[1:18]^2)/length(Est$std.all[1:18])

#Reliability
################################

#Timeliness
data_timeliness<-with(data_question_all_imp,data.frame(
			question_1,
			question_5,
			question_7,
			question_12,
			question_24))

psych::alpha(data_timeliness,n.iter=1000,check.keys=TRUE)

# Composite reliability
sum(Est$std.all[15,])^2/(sum(Est$std.all[1:18])^2+
  sum(Est$std.all[109:131]))

#Feedback Culture
data_feedback<-with(data_question_all_imp,data.frame(
			question_2,
			question_3,
			question_10,
			# question_13,
			question_18))
			# question_21))

psych::alpha(data_feedback,n.iter=1000,check.keys=TRUE)


#Specificity
data_specificity<-with(data_question_all_imp,data.frame(
			question_4,
			question_14,
			question_17,
			# question_13,
			question_22))
			# question_21))

psych::alpha(data_specificity,n.iter=1000,check.keys=TRUE)

#Action/Plan
data_actionplan<-with(data_question_all_imp,data.frame(
			question_6,
			question_9,
			# question_11,
			question_15,
			question_16))

psych::alpha(data_actionplan,n.iter=1000,check.keys=TRUE)

#Respect
data_respect<-with(data_question_all_imp,data.frame(
			question_8,
			question_19,
			question_20,
			question_23,
			question_25))

psych::alpha(data_respect,n.iter=1000,check.keys=TRUE)

######################################################################
#FIGURE 1 - EFA PATH DIAGRAM CFA
######################################################################

fa.poly(data_question_all_imp,5)
 
######################################################################
#TABLE 3 - REGRESSION ADJUSTED STANDARDIZED SCORES
######################################################################

#GLM
############################################
require(ggplot2)
require(pscl)
require(boot)

summary(m1 <- zeroinfl(count ~ child + camper | persons, data = zinb))


logmodel<-zeroinfl(hiv_related_stigma_sumscore ~ 
						DEM1 + 
						as.numeric(DEM2) + 
						DEM3
                       , data=data)
summary(logmodel)
exp(coef(logmodel))
exp(confint(logmodel,level=0.95))
predicted_responses<-predict(logmodel) # predicted values
#residuals(model1_death, type="deviance") # residuals
#logistic.display(baselineXFUP3)

######################################################################
#FIGURE 2 - ITEM RESPONSE THEORY INFORMATION CURVES and PERSON MAP
######################################################################
library(eRm)

#### USING eRM Package
IRTRolandMorris <- PCM(hiv_related_stigma_imputed)
diff_index<-thresholds(IRTRolandMorris)
summary(diff_index$threshtable[[1]][,1])
sd(diff_index$threshtable[[1]][,1])/sqrt(length(diff_index$threshtable[[1]][,1]))
plotICC(IRTRolandMorris,item.subset=18,ask=F,empICC=list("raw"),empCI=list(lty="solid"))
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


# summary(pp$theta.table[,1])
# pp
######################################################################
#TABLE 4 - ITEM RESPONSE THEORY FIT INDICATORS
######################################################################

######################################################################
#TABLE 5 - STANDAEDIZAtiON NORMS TABLE
######################################################################

#raw score
quantile(data$hiv_related_stigma_sumscore, 
				probs = seq(0, 1, by= 0.1),
				na.rm=TRUE) # decile


#regression adjusted score
quantile(predicted_responses, 
				probs = seq(0, 1, by= 0.1),
				na.rm=TRUE) # decile

#IRT adjusted score
quantile(pp$theta.table[,1], 
				probs = seq(0, 1, by= 0.1),
				na.rm=TRUE) # decile
pp
3