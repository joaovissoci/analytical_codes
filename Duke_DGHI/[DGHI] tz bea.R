######################################################
#suicide_anxiety.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. see full license at the end of this file.
######################################################
#this script follows a combination of the guidelines proposed by Hadley Wickham http://goo.gl/c04kq as well as using the formatR package http://goo.gl/ri6ky
#if this is the first time you are conducting an analysis using this protocol, please watch http://goo.gl/DajIN while following step by step

#link to manuscript

 #The general plan is to compare the fibrinogen and platelet curves of RS vs Copperhead snakes.  The times points are Baseline, nadir during hospitalization, day 5, day 8, day 15.  There is some missing mess.   I am hoping we can get it done in time for an abstract deadline soon.  Let me know what is best.

######################################################
#SETTING ENVIRONMENT
######################################################
 #install.packages("VIM")
 #install.packages("VIMGUI")
 #install.packages("miP")
 #install.packages("gWidgetsRGtk2")
 #install.packages("mi")
 #install.packages("epicalc")

#Load packages neededz for the analysis
#All packages must be installes with install.packages() function
lapply(c("sem","ggplot2", "psych", "RCurl", "irr", "nortest", 
	"moments","GPArotation","nFactors","boot","psy", "car","vcd", 
	"gridExtra","mi","VIM","epicalc","gdata","sqldf","reshape2",
	"mclust","foreign","survival","memisc","lme4","lmerTest",
	"dplyr","QCA","VennDiagram"),library, character.only=T)

#Package and codes to pull data from goodle sheets
#devtools::install_github("jennybc/googlesheets")
#library(googlesheets)
#suppressMessages(library(dplyr))

######################################################
#IMPORTING DATA
######################################################

data_bea<-read.csv("/Users/jnv4/Box Sync/Home Folder jnv4/Data/Global EM/Africa/Tz/bea_indicators.csv",sep=',')

data_bea_pcascores<-read.csv("/Users/jnv4/Desktop/bea_PCAscores.csv",sep=',')
######################################################
#DATA MANAGEMENT
######################################################

summary(data_bea)

#Excluding zero-variation variables
data_bea<-remove.vars(data_bea,c("auxiliary_lane","rd_condition___0","rd_condition___1","rd_condition___2","rd_condition___3"))

#Recoding

data_bea$road_design<-car::recode(data_bea$road_design,"0:1=1;2:3=0;else=NA")
data_bea$intersections<-car::recode(data_bea$intersections,"0=0;1:2=1;else=NA")
data_bea$pavement<-car::recode(data_bea$pavement,"0=0;1=1;2=0;3=0;else=NA")
data_bea$road_narrow___0<-car::recode(data_bea$road_narrow___0,"1=0;0=1")
data_bea$road_narrow<-with(data_bea,rowSums(data.frame(road_narrow___0,road_narrow___1,road_narrow___2,road_narrow___3)))
data_bea$road_narrow<-car::recode(data_bea$road_narrow,"0=0;else=1")
data_bea$walkways<-car::recode(data_bea$walkways,"0=0;1:2=1;else=NA")
data_bea$road_traffic_signs___0<-car::recode(data_bea$road_traffic_signs___0,"1=0;0=1")
data_bea$road_traffic_signs<-with(data_bea,rowSums(data.frame(road_traffic_signs___0,road_traffic_signs___1,road_traffic_signs___2)))
data_bea$road_traffic_signs<-car::recode(data_bea$road_traffic_signs,"0=0;else=1")
data_bea$speed_limit<-car::recode(data_bea$speed_limit,"1=1;else=0")
data_bea$curves_type___0<-car::recode(data_bea$curves_type___0,"1=0;0=1")
data_bea$curve_type<-with(data_bea,rowSums(data.frame(curves_type___0,curves_type___1,curves_type___2)))
data_bea$curve_type<-car::recode(data_bea$curve_type,"0=0;else=1")
data_bea$night_lights_1<-with(data_bea,rowSums(data.frame(night_lights___0,night_lights___3)))
data_bea$night_lights_1<-car::recode(data_bea$night_lights_1,"0=1;else=0")
data_bea$night_lights_2<-witMENTORING PLANh(data_bea,rowSums(data.frame(night_lights___1,night_lights___2)))
data_bea$night_lights<-with(data_bea,rowSums(data.frame(night_lights_1,night_lights_2)))
data_bea$night_lights<-car::recode(data_bea$night_lights,"0=0;else=1")
data_bea$night_lighting<-car::recode(data_bea$night_lights,"0=0;1=1;2=0;else=NA")
data_bea$visibility<-with(data_bea,rowSums(data.frame(visibility_1,visibility_2,visibility_3,visibility_4)))/4
data_bea$visibility<-car::recode(data_bea$visibility,"NA=11.64")
data_bea$risk_classification<-car::recode(data_bea$Risk.Classification, "'High'=1;'Low'=0")

data_bea$risk_classification<-as.numeric(as.character(data_bea$risk_classification))

data_bea_clusters<-subset(data_bea_pcascores,
	data_bea_pcascores$pca_scores_data.country=="tz")

data_bea_clusters$outcome<-data_bea$risk_classification
######################################################
#DESCRIPTIVE ANALYSIS
######################################################
# Road Area
table<-with(data_bea,table(road_area))
table
prop.table(table)
table<-with(data_bea,table(road_area,risk_classification))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Road Area
table<-with(data_bea,table(road_design))
table
prop.table(table)
table<-with(data_bea,table(road_design,risk_classification))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# intersection
table<-with(data_bea,table(intersections))
table
prop.table(table)	
table<-with(data_bea,table(intersections,risk_classification))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# intersectionwith cars in the same direction
table<-with(data_bea,table(conflict_intersections___0))
table
prop.table(table)	
table<-with(data_bea,table(conflict_intersections___0,risk_classification))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# intersectionwith cars in the same direction
table<-with(data_bea,table(conflict_intersections___1))
table
prop.table(table)	
table<-with(data_bea,table(conflict_intersections___1,risk_classification))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# intersectionwith cars in the same direction
table<-with(data_bea,table(conflict_intersections___2))
table
prop.table(table)	
table<-with(data_bea,table(conflict_intersections___2,risk_classification))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# lane type - one vs two
table<-with(data_bea,table(lane_type))
table
prop.table(table)	
table<-with(data_bea,table(lane_type,risk_classification))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Road narrowness
table<-with(data_bea,table(road_narrow))
table
prop.table(table)	
table<-with(data_bea,table(road_narrow,risk_classification))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Roadside danger
table<-with(data_bea,table(roadside_danger))
table
prop.table(table)	
table<-with(data_bea,table(roadside_danger,risk_classification))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# roadside Uneveness
table<-with(data_bea,table(unevenness_roadside))
table
prop.table(table)	
table<-with(data_bea,table(unevenness_roadside,risk_classification))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Roadsie Median
table<-with(data_bea,table(median))
table
prop.table(table)	
table<-with(data_bea,table(median,risk_classification))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Walkways
table<-with(data_bea,table(walkways))
table
prop.table(table)	
table<-with(data_bea,table(walkways,risk_classification))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Walkways
table<-with(data_bea,table(bus_stop))
table
prop.table(table)	
table<-with(data_bea,table(bus_stop,risk_classification))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Speed Bumps
table<-with(data_bea,table(bump))
table
prop.table(table)	
table<-with(data_bea,table(bump,risk_classification))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Traffic Signs
table<-with(data_bea,table(road_traffic_signs))
table
prop.table(table)	
table<-with(data_bea,table(road_traffic_signs,risk_classification))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Speed Limit
table<-with(data_bea,table(speed_limit))
table
prop.table(table)	
table<-with(data_bea,table(speed_limit,risk_classification))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Speed Limit
table<-with(data_bea,table(curve_type))
table
prop.table(table)	
table<-with(data_bea,table(curve_type,risk_classification))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Night Lights
table<-with(data_bea,table(night_lights))
table
prop.table(table)	
table<-with(data_bea,table(night_lights,risk_classification))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd pacMENTORING PLANkage

# Density
table<-with(data_bea,table(ddensity_level))
table
prop.table(table)	
table<-with(data_bea,table(ddensity_level,risk_classification))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Car density
summary(data_bea$ddensity_cars)
ad.test(data_bea$ddensity_cars)
#hist(data_bea$ddensity_cars)
#ci_func(data_bea$ddensity_cars,.95)
by(data_bea$ddensity_cars,data_bea$risk_classification,summary)
wilcox.test(data_bea$ddensity_cars~data_bea$risk_classification)

# Truck density
summary(data_bea$ddensity_trucks)
ad.test(data_bea$ddensity_trucks)
#hist(data_bea$ddensity_trucks)
#ci_func(data_bea$ddensity_trucks,.95)
by(data_bea$ddensity_trucks,data_bea$risk_classification,summary)
wilcox.test(data_bea$ddensity_trucks~data_bea$risk_classification)

# Bike density
summary(data_bea$ddensity_bikes)
ad.test(data_bea$ddensity_bikes)
#hist(data_bea$ddensity_bikes)
#ci_func(data_bea$ddensity_bikes,.95)
by(data_bea$ddensity_bikes,data_bea$risk_classification,summary)
wilcox.test(data_bea$ddensity_bikes~data_bea$risk_classification)

# Moto density
summary(data_bea$ddensity_motos)
ad.test(data_bea$ddensity_motos)
#hist(data_bea$ddensity_motos)
#ci_func(data_bea$ddensity_motos,.95)
by(data_bea$ddensity_motos,data_bea$risk_classification,summary)
wilcox.test(data_bea$ddensity_motos~data_bea$risk_classification)

# Bus density
summary(data_bea$ddensity_big_bus)
ad.test(data_bea$ddensity_big_bus)
#hist(data_bea$ddensity_big_bus)
#ci_func(data_bea$ddensity_big_bus,.95)
by(data_bea$ddensity_big_bus,data_bea$risk_classification,summary)
wilcox.test(data_bea$ddensity_big_bus~data_bea$risk_classification)

# DalaDala density
summary(data_bea$ddensity_daladala)
ad.test(data_bea$ddensity_daladala)
#hist(data_bea$ddensity_daladala)
#ci_func(data_bea$ddensity_daladala,.95)
by(data_bea$ddensity_daladala,data_bea$risk_classification,summary)
wilcox.test(data_bea$ddensity_daladala~data_bea$risk_classification)

# Pedestrian Crossings
summary(data_bea$ddensity_peda_crossing)
ad.test(data_bea$ddensity_peda_crossing)
#hist(data_bea$ddensity_peda_crossing)
#ci_func(data_bea$ddensity_peda_crossing,.95)
by(data_bea$ddensity_peda_crossing,data_bea$risk_classification,summary)
wilcox.test(data_bea$ddensity_peda_crossing~data_bea$risk_classification)

# Pedestrian alongside the road
summary(data_bea$ddensity_peds_road)
ad.test(data_bea$ddensity_peds_road)
#hist(data_bea$ddensity_peds_road)
#ci_func(data_bea$ddensity_peds_road,.95)
by(data_bea$ddensity_peds_road,data_bea$risk_classification,summary)
wilcox.test(data_bea$ddensity_peds_road~data_bea$risk_classification)

# Motorcycle Helmet
summary(data_bea$ddensity_motos_helmet)
ad.test(data_bea$ddensity_motos_helmet)
#hist(data_bea$ddensity_motos_helmet)
#ci_func(data_bea$ddensity_motos_helmet,.95)
by(data_bea$ddensity_motos_helmet,data_bea$risk_classification,summary)
wilcox.test(data_bea$ddensity_motos_helmet~data_bea$risk_classification)

# Helmet Strap
summary(data_bea$ddensity_motos_strap_pass)
ad.test(data_bea$ddensity_motos_strap_pass)
#hist(data_bea$ddensity_motos_strap_pass)
#ci_func(data_bea$ddensity_motos_strap_pass,.95)
by(data_bea$ddensity_motos_strap_pass,data_bea$risk_classification,summary)
wilcox.test(data_bea$ddensity_motos_strap_pass~data_bea$risk_classification)

# White Helmet
summary(data_bea$ddensity_motos_white)
ad.test(data_bea$ddensity_motos_white)
#hist(data_bea$ddensity_motos_white)
#ci_func(data_bea$ddensity_motos_white,.95)
by(data_bea$ddensity_motos_white,data_bea$risk_classification,summary)
wilcox.test(data_bea$ddensity_motos_white~data_bea$risk_classification)

# Helmet Strap
summary(data_bea$ddensity_motos_lights)
ad.test(data_bea$ddensity_motos_lights)
#hist(data_bea$ddensity_motos_lights)
#ci_func(data_bea$ddensity_motos_lights,.95)
by(data_bea$ddensity_motos_lights,data_bea$risk_classification,summary)
wilcox.test(data_bea$ddensity_motos_lights~data_bea$risk_classification)

# Helmet Strap
summary(data_bea$ndensity_motos)
ad.test(data_bea$ndensity_motos)
#hist(data_bea$ndensity_motos)
#ci_func(data_bea$ndensity_motos,.95)
by(data_bea$ndensity_motos,data_bea$risk_classification,summary)
wilcox.test(data_bea$ndensity_motos~data_bea$risk_classification)


######################################################
#PRINCIPAL COMPONENT ANALYSIS - From psych package - http://twt.lk/bdAQ or http://twt.lk/bdAR or http://twt.lk/bdAS
######################################################
# Define the amout of factor to retain
#Group of functinos to determine the number os items to be extracted
par(mfrow=c(2,2)) #Command to configure the plot area for the scree plot graph
ev <- eigen(cor_data) # get eigenvalues - insert the data you want to calculate the scree plot for
ev # Show eigend values
ap <- parallel(subject=nrow(pca_data),var=ncol(pca_data),rep=100,cent=.05) #Calculate the acceleration factor
summary(ap)
nS <- nScree(ev$values) #Set up the Scree Plot 
plotnScree(nS) # Plot the ScreePlot Graph
my.vss <- VSS(cor_data,title="VSS of BEA data")
print(my.vss[,1:12],digits =2)
VSS.plot(my.vss, title="VSS of 24 mental tests")
scree(pca_data)
VSS.scree(cor_data)
fa.parallel(cor_data,n.obs=36)

# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 
fit <- principal(cor_data,6,rotate="varimax",scores=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
fit$scores
predict(fit,cor_data)
scores<-scoreItems(fit$weights,pca_data)
describe(scores$scores)
by(scores$scores,data_bea$risk_classification,summary)
wilcox.test(scores$scores[,1]~data_bea$risk_classification)
wilcox.test(scores$scores[,2]~data_bea$risk_classification)
wilcox.test(scores$scores[,3]~data_bea$risk_classification)
#wilcox.test(scores$scores[,4]~data_bea$risk_classification)

######################################################
#QUALITATIVE COMPARATIVE ANALYSIS
######################################################

# # Organizing dataset
# #####################
# beaPCAscores <- read.csv("/Users/joaovissoci//Desktop/bea_PCAscores.csv")
# #Excluding variables
# #night_lights - no variance
# #walkways - no variance
# # 

# pca_data<-with(data_bea,data.frame(road_area,road_design,
# 	intersections,conflict_intersections___0,
# 	conflict_intersections___1,conflict_intersections___2,
# 	lane_type,pavement,road_narrow,roadside,roadside_danger,
# 	unevenness_roadside,bus_stop,bump,road_traffic_signs,
# 	speed_limit,curve_type,visibility,ddensity_level,
# 	ddensity_motos,ddensity_cars,
# 	ddensity_bikes,ddensity_daladala,ddensity_peda_crossing,
# 	ddensity_peds_road,ndensity_motos_lights,
# 	outcome=risk_classification))

# summary(pca_data)

# # generating correlation matrix
# cor_data<-cor_auto(pca_data)

# #visualizing correlation matrix with a network
# qgraph(cor_data,layout="spring")

# # Organize dataset with dichotomous respondes for cQCA or with proportions from 0 to 1 for fQCA
# qca_data<-with(data_bea,data.frame(road_area,road_design,
# 	pavement,road_narrow,unevenness_roadside,
# 	speed_limit,test=risk_classification))#,outcome=bancocerto$Q13)

# summary(qca_data)

# #write.csv(qca_data,"/Users/jnv4/Desktop/qca_data.csv")

# ### Calibration of numeric variables to crispy sets
# # Transform a set os thresholds to be calibrated from (cathegorized from)
# # Using quantiles as reference
# #th <- quantile(scores$scores, c(0.1, 0.5, 0.9))

# # Calibrate a trivalient set using thresholds derived from cluster analysis
# # Calls in the findTh function with an interval cased variable, a desired number of groups, clustering method (from hclust), distance measure used.
# #pred1<-calibrate(scores$scores[,1], thresholds = findTh(scores$scores[,1], groups = 2, hclustm="complete", distm="euclidean"))
# #pred2<-calibrate(scores$scores[,2], thresholds = findTh(scores$scores[,2], groups = 2, hclustm="complete", distm="euclidean"))
# #pred3<-calibrate(scores$scores[,3], thresholds = findTh(scores$scores[,3], groups = 2, hclustm="complete", distm="euclidean"))

# ### Fuzzification - transform a variablie into an interval from 0 to 1
# # Argument type="fuzzy" to calculate end-point or mid-point concepts
# # Calibrate fuzzy set using logistic function
# #round(calibrate(scores$scores, type = "fuzzy", thresholds = th), 2)
# #plot(x, calibrate(x, type = "fuzzy", thresholds = th[c(1,2,3,3,4,5)]), ylab = "Fuzzy Set Membership")

# # Analysis of Necessity
# #########################
# #outcome<-data_bea$risk_classification
# #qca_data<-data.frame(pred1,pred2,pred3,outcome)

# tt <- truthTable(data, outcome = "test", neg.out = TRUE, conditions =
# + "road_area, road_design, pavement, road_narrow, unevenness_roadside,
# + speed_limit", sort.by = "out=TRUE")
# tt

# ####

# # Minimization
# # Sufficient and necessary solution relation="sufnec"
# eqmcc(data, outcome = "test", neg.out = TRUE, conditions = "road_area,
# road_design, pavement, road_narrow, unevenness_roadside, speed_limit",
# relation = "sufnec", all.sol = TRUE, details = TRUE, show.cases = TRUE)


# # solution complex
# eqmcc(data, outcome = "test", neg.out = TRUE, conditions = "road_area,
# road_design, pavement, road_narrow, unevenness_roadside, speed_limit",
# all.sol = TRUE, details = TRUE, show.cases = TRUE)

# #Heatmap for the Thruth Table
# #############################

# heat_data<-TT$tt[TT$tt$OUT==1,]

# data_plot<-melt(heat_data)
# data_plot$value[43:49]<-0
# data_plot$value2<-c(rep(NA,42),
# 	paste(round((heat_data$n/12)*100,0),"%"))

# avseq <- ggplot(data_plot, aes(y=cases, x=variable)) + 
#   geom_raster(aes(fill=as.factor(value))) + 
#   scale_fill_manual(values=c("white","steelblue"),
#   	guide = guide_legend(title = " "),labels=c("Abscence","Presence")) +
#   geom_text(aes(y=cases, x=variable, label=value2)) + 
#   xlab(label="BEA indicators") + 
#   ylab(label="Conditions") + 
#   scale_x_discrete(labels=c("Road area", "Road design", "Pavement",
#   	"Road narrow","Uneveness", "Speed limit", "Coverage")) +
#   scale_y_discrete(labels=c("Condition 1","Condition 2",
#   	"Condition 3","Condition 4","Condition 5","Condition 6",
#   	"Condition 7")) +
#     theme(axis.text.x = element_text(angle=60,vjust=0.5))
# avseq


########################################################
#BEA analysis
########################################################

#PCA1a
with(data_bea_clusters,
	by(PCA1a,clusters,summary))
with(data_bea_clusters,
	kruskal.test(PCA1a,clusters))

#PCA1b
with(data_bea_clusters,
	by(PCA1b,clusters,summary))
with(data_bea_clusters,
	kruskal.test(PCA1b~clusters))

#PCA2
with(data_bea_clusters,
	by(PCA2,clusters,summary))
with(data_bea_clusters,
	kruskal.test(PCA2~clusters))

#PCA3
with(data_bea_clusters,
	by(PCA3,clusters,summary))
with(data_bea_clusters,
	kruskal.test(PCA3~clusters))

#PCA4
with(data_bea_clusters,
	by(PCA4,clusters,summary))
with(data_bea_clusters,
	kruskal.test(PCA4~clusters))

#Outcome

# Road Area
table<-with(data_bea_clusters,
	table(outcome))
table
prop.table(table)
table<-with(data_bea_clusters,
	table(outcome,clusters))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#data_epi$type_vehicle<-car::recode(data_epi$type_vehicle,"'pedestrian'=NA;'bicycle'=NA")
logmodel<-glm(outcome ~   clusters,
						  family=binomial, 
						  data=data_bea_clusters)
summary(logmodel)
#anova(reglogGEU)
exp(coef(logmodel)) # exponentiated coefficients
exp(confint(logmodel)) # 95% CI for exponentiated coefficients
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)

