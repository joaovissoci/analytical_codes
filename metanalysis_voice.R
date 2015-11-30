################################################################################
#TEMPLATE_FOR _META_ANALYSIS_OF_DIAGNOSTIC_ACCURACY#
#this script follows a combination of guidelines proposed by Doebler and Holling, according to (http://cran.r-project.org/web/packages/mada/vignettes/mada.pdf)#
#
#
#####################################################################################
#SETTING ENVIRONMENT
#####################################################################################
#install.packages("mada")
#library("mada")
#install.packages("car", repos="http://cran.r-project.org")
#install.packages("ggplot2", repos="http://cran.r-project.org")

#Load packages (after installed) with the library function
lapply(c("metafor","ggplot2","gridExtra" ,"psych", "RCurl", "irr", "nortest", "moments","GPArotation","nFactors","gdata","meta","qgraph"), library, character.only=T)
########################################################################################################
#IMPORTING DATA AND RECODING
######################################################################################################
#Instructions here http://goo.gl/Ofa7gQ
metadata <- repmis::source_DropboxData("voice_SR_metanalysis.csv","nswh6wtpit5b2h8",sep = ",",header = TRUE)

data <- repmis::source_DropboxData("SR_voice_network.csv",
                                  "xvd3hfm26b319i7",
                                  sep = ",",
                                  header = TRUE)

qualitydata <- repmis::source_DropboxData("SR_voice_network.csv","xvd3hfm26b319i7", sep = ",", header = TRUE)

##### RECODING
attrition_raw<-with(metadata,attrition_intervention+ attrition_control)
total<-metadata$total_sample
cov1<-metadata$cov_1_treatment_group
cov1<-as.factor(car::recode(cov1,"1='Medication';2='Non-Medication';3='Both'"))
cov2<-metadata$cov_2_treatment_kind
cov2<-as.factor(car::recode(cov2,"1='Only vocal training';2='Vocal training and other';3='Vocal training and no training';4='Invasive treatment'"))
cov3<-metadata$cov3_healthy_patients
cov3<-as.factor(car::recode(cov3,"1='Healthy patients';2='Unhealthy patients'"))
cov4<-metadata$cov_4_qol
cov4<-as.factor(car::recode(cov4,"1='Yes';2='No'"))
cov5<-metadata$cov_5_fup
cov5<-as.factor(car::recode(cov5,"1:='Yes';2='No'"))

study<-metadata$author_year

###########################################################
#Figure 1. Attrition rate prevalence
###########################################################
meta1<-data.frame(attrition_raw,total,study)
meta1<-na.omit(meta1)
m3<-metaprop(attrition_raw,total,sm="PLN",data=meta1,studlab=study)

tiff("C:\\Users\\Joao\\Desktop\\voice_overall.tiff", width = 700, height = 500,compression = 'lzw')
forest(m3)
dev.off()
metainf(m3)
metainf(m3, pooled="random")

### BY GROUP ANALYSIS - TYPE OF TREATMENT
meta1<-data.frame(attrition_raw,total,study,cov1)
meta1<-na.omit(meta1)
meta1$cov1<-as.character(meta1$cov1)
m3<-metaprop(attrition_raw,total,sm="PLN",data=meta1,studlab=study,byvar=meta1$cov1)

tiff("C:\\Users\\Joao\\Desktop\\voice_overall_bytreatment.tiff", width = 700, height = 700,compression = 'lzw')
forest(m3)
dev.off()
metainf(m3)
metainf(m3, pooled="random")
funnel(m3)

meta2<-meta1[-c(13,14,19),]
m3<-metaprop(attrition_raw,total,sm="PLN",data=meta2,studlab=study,byvar=meta2$cov1,comb.fixed=FALSE)

### BY GROUP ANALYSIS - KIND OF TREATMENT
meta1<-data.frame(attrition_raw,total,study,cov2)
meta1<-na.omit(meta1)
meta1$cov2<-as.character(meta1$cov2)
m3<-metaprop(attrition_raw,total,sm="PLN",data=meta1,studlab=study,byvar=meta1$cov2)

tiff("C:\\Users\\Joao\\Desktop\\voice_overall_bykind.tiff", width = 900, height = 700,compression = 'lzw')
forest(m3)
dev.off()
metainf(m3)
metainf(m3, pooled="random")


### BY GROUP ANALYSIS - HEALTH STATUS
meta1<-data.frame(attrition_raw,total,study,cov3)
meta1<-na.omit(meta1)
meta1$cov3<-as.character(meta1$cov3)
m3<-metaprop(attrition_raw,total,sm="PLN",data=meta1,studlab=study,byvar=meta1$cov3)

tiff("C:\\Users\\Joao\\Desktop\\voice_overall_byhealth.tiff", width = 700, height = 700,compression = 'lzw')
forest(m3)
dev.off()
metainf(m3)
metainf(m3, pooled="random")

### BY GROUP ANALYSIS - HEALTH STATUS
meta1<-data.frame(attrition_raw,total,study,cov4)
meta1<-na.omit(meta1)
meta1$cov4<-as.character(meta1$cov4)
m3<-metaprop(attrition_raw,total,sm="PLN",data=meta1,studlab=study,byvar=meta1$cov4)

tiff("C:\\Users\\Joao\\Desktop\\voice_overall_byQV.tiff", width = 700, height = 700,compression = 'lzw')
forest(m3)
dev.off()
metainf(m3)
metainf(m3, pooled="random")

###########################################################
#Figure 2. Attrition rate prevalence INTERVENTION GROUP
###########################################################
attrition_intervention<-metadata$attrition_intervention
total_intervention<-metadata$total_intervention

meta1<-data.frame(attrition_intervention,total_intervention,study)
meta1<-na.omit(meta1)
m3<-metaprop(attrition_intervention,total_intervention,sm="PLN",data=meta1,studlab=study)
meta2<-meta1[-c(13,14,19),]
m3<-metaprop(attrition_intervention,total_intervention,sm="PLN",data=meta2,studlab=study,byvar=meta2$cov1,comb.fixed=FALSE)

tiff("C:\\Users\\Joao\\Desktop\\voice_overall_intervention.tiff", width = 700, height = 700,compression = 'lzw')
forest(m3)
dev.off()
metainf(m3)
metainf(m3, pooled="random")

### BY GROUP ANALYSIS - TYPE OF TREATMENT
meta1<-data.frame(attrition_intervention,total_intervention,study,cov1)
meta1<-na.omit(meta1)
meta1$cov1<-as.character(meta1$cov1)
m3<-metaprop(attrition_intervention,total_intervention,sm="PLN",data=meta1,studlab=study,byvar=meta1$cov1)
meta2<-meta1[-c(13,14,19),]
m3<-metaprop(attrition_intervention,total_intervention,sm="PLN",data=meta2,studlab=study,byvar=meta2$cov1,comb.fixed=FALSE)

tiff("C:\\Users\\Joao\\Desktop\\voice_overall_intervention_bytreatment.tiff", width = 700, height = 700,compression = 'lzw')
forest(m3)
dev.off()
metainf(m3)
metainf(m3, pooled="random")
funnel(m3)

### BY GROUP ANALYSIS - KIND OF TREATMENT
meta1<-data.frame(attrition_intervention,total_intervention,study,cov2)
meta1<-na.omit(meta1)
meta1$cov2<-as.character(meta1$cov2)
m3<-metaprop(attrition_intervention,total_intervention,sm="PLN",data=meta1,studlab=study,byvar=meta1$cov2)
meta2<-meta1[-c(11,12,17),]
m3<-metaprop(attrition_intervention,total_intervention,sm="PLN",data=meta2,studlab=study,byvar=meta2$cov2,comb.fixed=FALSE)

tiff("C:\\Users\\Joao\\Desktop\\voice_overall_intervention_bykind.tiff", width = 900, height = 700,compression = 'lzw')
forest(m3)
dev.off()
metainf(m3)
metainf(m3, pooled="random")


### BY GROUP ANALYSIS - HEALTH STATUS
meta1<-data.frame(attrition_intervention,total_intervention,study,cov3)
meta1<-na.omit(meta1)
meta1$cov3<-as.character(meta1$cov3)
m3<-metaprop(attrition_intervention,total_intervention,sm="PLN",data=meta1,studlab=study,byvar=meta1$cov3)
meta2<-meta1[-c(17),]
m3<-metaprop(attrition_intervention,total_intervention,sm="PLN",data=meta2,studlab=study,byvar=meta2$cov3,comb.fixed=FALSE)

tiff("C:\\Users\\Joao\\Desktop\\voice_overall_intervention_byhealth.tiff", width = 700, height = 700,compression = 'lzw')
forest(m3)
dev.off()
metainf(m3)
metainf(m3, pooled="random")

### BY GROUP ANALYSIS - HEALTH STATUS
meta1<-data.frame(attrition_intervention,total_intervention,study,cov4)
meta1<-na.omit(meta1)
meta1$cov4<-as.character(meta1$cov4)
m3<-metaprop(attrition_intervention,total_intervention,sm="PLN",data=meta1,studlab=study,byvar=meta1$cov4)
meta2<-meta1[-c(11,12,17),]
m3<-metaprop(attrition_intervention,total_intervention,sm="PLN",data=meta2,studlab=study,byvar=meta2$cov4,comb.fixed=FALSE)
tiff("C:\\Users\\Joao\\Desktop\\voice_overall_intervention_byQV.tiff", width = 700, height = 700,compression = 'lzw')
forest(m3)
dev.off()
metainf(m3)
metainf(m3, pooled="random")
funnel(m3)

########################################################################################################
#Figure 2. Metaregression
######################################################################################################

metareg(m3, ~cov1 + cov2 + cov3)


########################################################################################################
#Sensitivity Analysis
######################################################################################################

meta1_sensitivity1<-meta1[-c(6,9,10),]

m3<-metaprop(attrition_raw,total,sm="PLN",data=meta1_sensitivity1)
forest(m3)

#####################################################################################
#EFFECT SIZES
#####################################################################################
data_ef<-remove.vars(data,c("Study"))

effect_size<-colSums(data_ef)

effect_sizes<-as.matrix(effect_size/dim(data_ef)[[1]])

data_ef$intolerance<-car::recode(data_ef$intolerance,"1=0")
data_ef$tinnitus<-car::recode(data_ef$intolerance,"1=0")
data_ef$second_diagnostic<-car::recode(data_ef$intolerance,"1=0")
data_ef$voice_therapy<-car::recode(data_ef$intolerance,"1=0")
data_ef$pain<-car::recode(data_ef$intolerance,"1=0")
data_ef$paralysis<-car::recode(data_ef$intolerance,"1=0")
data_ef$satisfied_result<-car::recode(data_ef$intolerance,"1=0")
data_ef$cancer<-car::recode(data_ef$intolerance,"1=0")
data_ef$unknown<-car::recode(data_ef$intolerance,"1=0")
data_ef$technical_errors<-car::recode(data_ef$intolerance,"1=0")
data_ef$discontinued<-car::recode(data_ef$intolerance,"1=0")
data_ef$dissatisfied<-car::recode(data_ef$intolerance,"1=0")

intensity<-length(dim(effect_sizes>=0.25))
intensity_data<-rowSums(data_ef)

intensity_sizes<-intensity_data/2
#########################################################
#NETWORK ANALYSIS
#########################################################
data_network<-remove.vars(data,c("Study"))
variable_data <- t(as.matrix(data_network)) %*% as.matrix(data_network)
study_data<-as.matrix(data_network)
rownames(study_data)<-data$Study
network_data<-rbind(variable_data,study_data)
network_data <- (as.matrix(network_data)) %*% t(as.matrix(network_data))
diag(network_data) <- 0
names<-c(c("Intolerance","Incompletion","Refusal","Tinnitus","Second Diagnostic",
	"Voice Therapy","Pain","Paralysis","Deceased","Satisfied Result","Cancer","Unknown","Technical Errors","Discontinued","Dissatisfied"),rownames(study_data))

size_edges<-c(effect_sizes[,1]*10,intensity_sizes*3)
color<-c(c("red","steelblue","steelblue","lightgreen","steelblue","steelblue","lightgreen","lightgreen","red","red","red","steelblue","steelblue","red","red"),rep("white",16))
shape<-c(rep("circle",15),rep("square",16)) 
label.cex<- c(rep(2.0,15),rep(1.2,16))
groups<-c("Methodological Issues","Clinical Issues","Side-Effects")

tiff("/Users/rpietro/Desktop/voice_sr_network.tiff", width = 1300, height = 1300,compression = 'lzw')
network_meta <- qgraph(network_data,layout = "spring",minimum=0.5,cut=10,labels=names,label.scale=FALSE,label.cex = label.cex,vsize=size_edges,color=color,shape=shape,greyscale=T)
legend(0.8,-0.8, bty="n",c("Methodological Issues","Clinical Issues","Side-Effects"),cex=1.2,fill=c("steelblue","red","lightgreen"))
dev.off()

###########################################################
#Quality Assessment
###########################################################

summary(qualitydata)
qualitydata$Item<-as.factor(qualitydata$Item)
#attach(qualitydata)
#Generate ggplot graph fro quality data information
ggplot(qualitydata, aes(Item, Author)) + geom_tile(aes(fill = Value),
colour = "white") + scale_fill_gradient(low = "white",
high = "steelblue", name="", breaks=c(0,5,10), labels=c("No","Not Clear","Yes")) +
 theme(axis.text.x = element_text(hjust = 0, colour = "black",size=14),
        axis.text.y = element_text(colour = "black",size=14),
        axis.title.x = element_text(face="bold",size=14),
        axis.title.y = element_text(face="bold", size=14))
