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
	"dplyr","QCA","VennDiagram","qgraph"),library, character.only=T)

#Package and codes to pull data from goodle sheets
#devtools::install_github("jennybc/googlesheets")
#library(googlesheets)
#suppressMessages(library(dplyr))

######################################################
#IMPORTING DATA
######################################################

data_tz<-read.csv("/Users/jnv4/OneDrive - Duke University/datasets/DGHI/Africa/bea validation/bea_tz.csv",sep=',')
data_sl<-read.csv("/Users/jnv4/OneDrive - Duke University/datasets/DGHI/Africa/bea validation/bea_sl.csv",sep=',')
data_rw<-read.csv("/Users/jnv4/OneDrive - Duke University/datasets/DGHI/Africa/bea validation/bea_rw.csv",sep=',')

######################################################
#DATA MANAGEMENT
######################################################
#clean data from Sri Lanka
data_sl2<-subset(data_sl,data_sl$rater=="e")

# building merged dataset
bea_data<-NULL

bea_data$country<-c(rep("tz",length(data_tz[,1])),
					rep("rw",length(data_rw[,1])),
					rep("sl2",length(data_sl2[,1])))

#Recode variables

#ROAD AREA
data_tz$road_area
data_rw$road_area<-data_rw$road_urban
data_sl2$road_area

bea_data$road_area<-c(data_tz$road_area,
					  data_rw$road_area,
					  data_sl2$road_area)


#ROAD DESIGN
data_tz$road_design
data_rw$aroad_design
data_sl2$road_design

bea_data$road_design<-c(data_tz$road_design,
					  data_rw$aroad_design,
					  data_sl2$road_design)

#Intersections
data_tz$intersections
data_rw$intersections
data_sl2$intersections

bea_data$intersections<-c(data_tz$intersections,
					  data_rw$intersections,
					  data_sl2$intersections)

# lane_type

data_tz$lane_type_recoded<-car::recode(data_tz$lane_type,"
	0=0; 1=1; 2=1; 3=0;4=0")
data_rw$alane_type_recoded<-car::recode(data_rw$alane_type,"
	0=0;1=1;2=1;3=1")
data_sl2$lane_type_recoded<-car::recode(data_sl2$lane_type,"
	0=0; 1=1; 2=1; 3=0;4=0")

bea_data$lane_type<-c(data_tz$lane_type_recoded,
					  data_rw$alane_type_recoded,
					  data_sl2$lane_type_recoded)

# auxiliary_lane
data_tz$auxiliary_lane
data_rw$aauxiliary_lane
data_sl2$auxiliary_lane

bea_data$auxiliary_lane<-c(data_tz$auxiliary_lane,
					  data_rw$aauxiliary_lane,
					  data_sl2$auxiliary_lane)

# pavement
data_tz$pavement_recoded<-car::recode(data_tz$pavement,"
	0=0; 1=1; 2=1; 3=0")
data_rw$pavement_recoded<-car::recode(data_rw$apavement,"
	0=0;1=0;2=1")
data_sl2$pavement_recoded<-car::recode(data_sl2$pavement,"
	0=0; 1=0; 2=1")

bea_data$pavement<-c(data_tz$pavement_recoded,
					  data_rw$pavement_recoded,
					  data_sl2$pavement_recoded)

# road_condition - dry = 1
data_tz$rd_condition___0
data_rw$ard_condition___0
data_sl2$rd_condition___0

bea_data$road_condition<-c(data_tz$rd_condition___0,
					       data_rw$ard_condition___0,
					       data_sl2$rd_condition___0)

#road_narrow - No norrowing - 1
data_tz$road_narrow___0
data_rw$aroad_narrow___0
data_sl2$road_narrow___0

bea_data$road_narrow<-c(data_tz$road_narrow___0,
					       data_rw$aroad_narrow___0,
					       data_sl2$road_narrow___0)

# roadside
data_tz$roadside
data_rw$aroadside
data_sl2$roadside

bea_data$roadside<-c(data_tz$roadside,
					       data_rw$aroadside,
					       data_sl2$roadside)

# walkways
data_tz$walkways<-car::recode(data_tz$walkways,"0=0;1=1;2=1")
data_rw$awalkways
data_sl2$walkways<-car::recode(data_sl2$walkways,"0=0;1=1;2=1")

bea_data$walkways<-c(data_tz$walkways,
					       data_rw$awalkways,
					       data_sl2$walkways)

# bus_stop
data_tz$bus_stop
data_rw$abus_stop
data_sl2$bus_stop

bea_data$bus_stop<-c(data_tz$bus_stop,
					       data_rw$abus_stop,
					       data_sl2$bus_stop)

# bump
data_tz$bump
data_rw$abump
data_sl2$bump

bea_data$bump<-c(data_tz$bump,
					       data_rw$abump,
					       data_sl2$bump)

# traffic_light
data_tz$traffic_light<-c(0)
data_rw$atraffic_light
data_sl2$traffic_light

bea_data$traffic_light<-c(data_tz$traffic_light,
					       data_rw$atraffic_light,
					       data_sl2$traffic_light)

# road_traffic_signs - 1 = no signs
data_tz$road_traffic_signs___0
data_rw$aroad_signs_traffic___0
data_sl2$road_traffic_signs___0

bea_data$road_traffic_signs<-c(data_tz$road_traffic_signs___0,
					       data_rw$aroad_signs_traffic___0,
					       data_sl2$road_traffic_signs___0)

# speed_limit
data_tz$speed_limit
data_rw$aspeed_limit
data_sl2$speed_limit<-car::recode(data_sl2$speed_limit,"
	0=1")

bea_data$speed_limit<-c(data_tz$speed_limit,
					       data_rw$aspeed_limit,
					       data_sl2$speed_limit)

# curves_type

data_tz$curves_type___0
data_rw$acurves_type___0
data_sl2$curves_type___0

bea_data$curves_type<-c(data_tz$curves_type___0,
					       data_rw$acurves_type___0,
					       data_sl2$curves_type___0)
# road_visibility
data_tz$visibility___0
data_rw$avisibility___0
data_sl2$visibility___0

bea_data$road_visibility<-c(data_tz$visibility___0,
					       data_rw$avisibility___0,
					       data_sl2$visibility___0)

# bridges
data_tz$bridges
data_rw$abridges
data_sl2$bridges

bea_data$bridges<-c(data_tz$bridges,
					       data_rw$abridges,
					       data_sl2$bridges)


#pedestrians_crossing
data_tz$pedestrians___0
data_rw$apedestrians___0
data_sl2$pedestrians___0

bea_data$pedestrians_crossing<-c(data_tz$pedestrians___0,
					      		 data_rw$apedestrians___0,
					       		 data_sl2$pedestrians___0)

#pedestrian_centralrestarea
data_tz$pedestrians___2<-c(0)
data_rw$apedestrians___2
data_sl2$pedestrians___2

bea_data$pedestrians_center<-c(data_tz$pedestrians___2,
					      		 data_rw$apedestrians___2,
					       		 data_sl2$pedestrians___2)

#pedestrian_walkways
data_tz$pedestrians___3
data_rw$pedestrians___3<-data_rw$apedestrians___3+
						 data_rw$apedestrians___4+
						 data_rw$apedestrians___5
data_rw$pedestrians___3<-car::recode(data_rw$pedestrians___3,"
	0=0;else=1")
data_sl2$pedestrians___3

bea_data$pedestrians_walkways<-c(data_tz$pedestrians___3,
					      		 data_rw$pedestrians___3,
					       		 data_sl2$pedestrians___3)

#car_density
data_tz$ddensity_cars
data_rw$density_cars
data_sl2$car_density

bea_data$density_car<-c(data_tz$ddensity_cars,
					      		 data_rw$density_cars,
					       		 data_sl2$car_density)

#moto_density
data_tz$ddensity_motos
data_rw$density_motos
data_sl2$motorcycle_density

bea_data$density_moto<-c(data_tz$ddensity_motos,
					      		 data_rw$density_motos,
					       		 data_sl2$motorcycle_density)

#bike_density
data_tz$ddensity_bikes
data_rw$density_bikes
data_sl2$bike_density

bea_data$density_bike<-c(data_tz$ddensity_bikes,
					      		 data_rw$density_bikes,
					       		 data_sl2$bike_density)

#pedestrian_density
data_tz$ddensity_peda_crossing
data_rw$density_peda_crossing
data_sl2$ddensity_peda_crossing

bea_data$density_pedestrian<-c(data_tz$ddensity_peda_crossing,
					      		 data_rw$density_peda_crossing,
					       		 data_sl2$ddensity_peda_crossing)

#bus_truck_density
data_tz$ddensity_peda_crossing
data_rw$density_peda_crossing
data_sl2$pedestrian_density

bea_data$density_pedestrian<-c(data_tz$ddensity_peda_crossing,
					      		 data_rw$density_peda_crossing,
					       		 data_sl2$pedestrian_density)

# Organizing data set

bea_data<-as.data.frame(bea_data)
summary(bea_data)

bea_data$road_design<-car::recode(bea_data$road_design,"
	0=1;1=0;2=0;3=0;99=NA")
bea_data$intersections<-car::recode(bea_data$intersections,"
	0=0;1=1;2=1;99=NA")
bea_data$auxiliary_lane<-car::recode(bea_data$auxiliary_lane,"
	99=NA")
bea_data$pavement<-car::recode(bea_data$pavement,"
	99=NA")
bea_data$bus_stop<-car::recode(bea_data$bus_stop,"
	99=NA")
bea_data$speed_limit<-car::recode(bea_data$speed_limit,"
	99=0")

##############################################################
#PCA Score comparisons
#############################################################
# # Define the amout of factor to retain
#Group of functinos to determine the number os items to be extracted
cor_data<-cor_auto(bea_data[,-1])

comprehension_network_glasso<-qgraph(cor_data,
	layout="spring",
	vsize=6,esize=20,graph="glasso",
	sampleSize=nrow(bea_data),
	legend.cex = 0.5,GLratio=1.5,minimum=0.1)
#Calculating Community measures
g<-as.igraph(comprehension_network_glasso) #creating igraph object
#h<-walktrap.community(g) #creatin community object
h<-spinglass.community(g, weights=NA)
plot(h,g) #plotting community network
h$membership #extracting community membership for each node on the network

#

par(mfrow=c(2,2)) #Command to configure the plot area for the scree plot graph
ev <- eigen(cor_data) # get eigenvalues - insert the data you want to calculate the scree plot for
ev # Show eigend values
ap <- parallel(subject=nrow(cor_data),var=ncol(cor_data),rep=100,cent=.05) #Calculate the acceleration factor
summary(ap)
nS <- nScree(ev$values) #Set up the Scree Plot 
plotnScree(nS) # Plot the ScreePlot Graph
my.vss <- VSS(cor_data,title="VSS of BEA data")
print(my.vss[,1:12],digits =2)
VSS.plot(my.vss, title="VSS of 24 mental tests")
scree(cor_data)
VSS.scree(cor_data)
fa.parallel(cor_data,n.obs=36)

# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 
fit <- principal(cor_data,4,rotate="varimax",scores=TRUE)
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




