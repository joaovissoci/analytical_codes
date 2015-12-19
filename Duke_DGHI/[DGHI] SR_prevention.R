################################################################################
#Template for the Road Traffic Injury Prevention Initiatives; a systematic review and meta-summary of effectiveness in low and middle income countries.
##
##############################################################################
#SETTING ENVIRONMENT
##############################################################################
#install.packages("", repos="http://cran.r-project.org")

#Load packages (after installed) with the library function
lapply(c("metafor","ggplot2","gridExtra" ,"psych", "RCurl", "irr", "nortest", "moments","GPArotation","nFactors","gdata","meta","qgraph","googlesheets"), library, character.only=T)

###########################################################################
#IMPORTING DATA
###########################################################################
#Instructions here http://goo.gl/Ofa7gQ
#data <- repmis::source_DropboxData("","",sep = ",",header = TRUE)

data<-read.csv("/Users/joaovissoci/Dropbox/datasets/DGHI/prevention_SR/prevention_SR.csv",header=T)

#sheet <- gs_title("[DGHI] SR prevention data")
#data<-gs_read_csv(sheet,ws = "Intervention based summary")
#data<-as.data.frame(data)

###########################################################################
#DATA MANAGEMENT
###########################################################################
code_na<-function(x){car::recode(x,"NA=0")}

network_data<-as.data.frame(lapply(data,FUN=code_na))

###########################################################################
#EFFECT SIZES
###########################################################################
data_ef<-remove.vars(network_data,c("author","country","intervention_type"))

effect_size<-colSums(data_ef)

effect_sizes<-as.matrix(effect_size/dim(data_ef)[[1]])

intensity<-length(dim(effect_sizes>=0.25))
intensity_data<-rowSums(data_ef)

intensity_sizes<-intensity_data/2
#########################################################
#NETWORK ANALYSIS
#########################################################
data_network<-remove.vars(network_data,c("country","author","intervention_type"))
variable_data <- t(as.matrix(data_network)) %*% as.matrix(data_network)
#study_data<-as.matrix(data_network)
#rownames(study_data)<-network_data$author
#network_data<-rbind(variable_data,study_data)
#network_data <- (as.matrix(network_data)) %*% t(as.matrix(network_data))
diag(variable_data) <- 0
names<-c("Legislation","Speed Control","Public Awareness","Enforcement","Road Improvement","Pedestrians","Bycyclists","Motorcyclists","Overall Crashes","Overall Drivers","Improved Laws","Higher Penalties","Death Reduction","Crashes Reduction","Time Series Improvement","Enforcement Training","Helmet Use Law","RTI Reduction","ER Admission/LOS Reduction","Severe Disability Reduction","Non significant Death Reduction","Non Significant RTI Reduction","Non Significant Crash Reduction","Speed Limit","Rumble Strips","Education","Community Comparison","Non significant Alcohol Use Reduction","Road Pavement","RTI Increased","Non Significant Time Series Reduction","BAC level Reduction","Signs and Media")#,rownames(study_data))
size_edges<-c(effect_sizes[,1]*10)
color<-c(rep("steelblue",5),rep("lightgreen",7),rep("red",3),rep("lightgreen",2),rep("red",6),rep("lightgreen",4),rep("red",1),rep("lightgreen",1),rep("red",3),rep("lightgreen",1))#,rep("white",15))
shape<-c(rep("circle",5),rep("circle",28))#,rep("square",15)) 
label.cex<- c(rep(2.0,5),rep(1.0,28))#,rep(0.8,15))
groups<-c("Methodological Issues","Outcomes","Interventions")

tiff("/Users/joaovissoci/Desktop/prevention_sr_network.tiff", width = 1500, height = 1000,compression = 'lzw')
network_meta <- qgraph(variable_data,layout = "spring",minimum=0.5,cut=3,labels=names,label.scale=FALSE,label.cex = label.cex,vsize=size_edges,color=color,posCol = "grey",borders = FALSE)#,shape=shape
legend(0.6,-0.8, bty="n",c("Methodological Issues","Outcomes","Interventions"),cex=1.2,fill=c("lightgreen","red","steelblue"))
dev.off()

#label.cex = label.cex,vsize=size_edges,color=color,shape=shape,greyscale=T

##############################################################################
#End of the script
##############################################################################

