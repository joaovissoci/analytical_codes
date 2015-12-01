#######################################################################################
#AOAnalysisscript.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. see full license at the end of this file.
#######################################################################################
#this script follows a combination of the guidelines proposed by Hadley Wickham http://goo.gl/c04kq as well as using the formatR package http://goo.gl/ri6ky
#if this is the first time you are conducting an analysis using this protocol, please watch http://goo.gl/DajIN while following step by step

#link to manuscript

#####################################################################################
#SETTING ENVIRONMENT
#####################################################################################
 #install.packages("VIM")
 #install.packages("VIMGUI")
 #install.packages("miP")
 #install.packages("gWidgetsRGtk2")
 #install.packages("mi")
 #install.packages("epicalc")

#Load packages neededz for the analysis
#All packages must be installes with install.packages() function
lapply(c("sem","ggplot2", "psych", "RCurl", "irr", "nortest", "moments","GPArotation","nFactors",
         "boot","psy", "car","vcd", "gridExtra","mi",
         "VIM","miP","epicalc","gdata","mclust","reshape","repmis"), 
library, character.only=T)
#####################################################################################
#IMPORTING DATA
#####################################################################################

#uploading data ---------------------------------------------------------------------
#Load the data set.
#All data are stored in  http://figshare.com/articles/The_reliability_of_AO_classification_on_femur_fractures_among_orthopedic_residents/103664
#Note that the data set has been reorganized to be applied to some functions

#Functions to pull the dara from the internet file 
#see http://goo.gl/mQwxO on how to get this link
#webdata <- getURL("https://docs.google.com/spreadsheet/pub?key=0ArVW3PoO2euydHFkeHVMd3FyeVlkZE1ySlc2bWEwZFE&single=true&gid=1&output=csv"
#,ssl.verifypeer = FALSE)
#data<-read.csv(textConnection(webdata))

data <- repmis::source_DropboxData("personality_wow.csv",
                                  "vioqqkb3ihbqp9d",
                                  sep = ",",
                                  header = TRUE)

#data <- read.csv("/Users/joaovissoci/Google Drive/Research Groups/NeuroGames/personality_wow.csv")
data<-na.omit(data)
data<-subset(data, data$AGE <= 25)
data$lineage[data$AGE <=18]<-c("adolescents")
data$lineage[data$AGE >=18.01]<-c("emadults")
data$lineage<-as.factor(data$lineage)

###########################################################################################
#DATA MANAGEMENT
###########################################################################################
#Section dedicated to any data management necessary
#Reconding variables to a numerical/discrete format. This format of data was used 
#to create some of the graphical displays.
#Recoding variables
#data$T1 <-as.numeric(car::recode(data$R1, "'33A1' = 1; '33A2' = 2; '33A3' = 3;
#                         '33B1' = 4; '33B2' = 5; '33B3' = 6;
#                         '33C1' = 7; '33C2' = 8; '33C3' = 9"))

### Imputation method
#data_time <- proc.time()
#data_imputed <- kNN(data)
#proc.time() - data_time
#summary(data)

#Vars
#[1] "X"                      "Age"                    "Gender"                
#[4] "MechanismofInjury"      "AlcoholInvolved."       "Temperature"           
#[7] "RespiratoryRate"        "Pulse"                  "SystolicBloodPressure" 
#[10] "DiastolicBloodPressure" "PulseOxygen"            "Vital_Sign"            
#[13] "AVPU"                   "GlasgowComaScore"       "AirwayAssessment"      
#[16] "AirwayManagement"       "BreathingAssessment"    "OxygenApplied"         
#[19] "ChestRadiograph"        "PulsesEvaluated"        "FluidsStarted"         
#[22] "LabsSent."              "Movedallextremities"    "SkullRadiograph"       
#[25] "gos"   

#### Organizing dataset to be imputed
#data_total<-remove.vars(datatotal,c("RightPupilReactive",
#                        "LeftPupilReactive","ChestTubePlaced",
#                        "FBPSent","Seizure.likeactivity",
#                        "CTbrain","Mannitol",
#                        "SeizureMedAdministration","HadSurgeryforTBI"))

# [1] "Age"                    "Gender"                 "MechanismofInjury"     
# [4] "AlcoholInvolved."       "Temperature"            "RespiratoryRate"       
# [7] "Pulse"                  "SystolicBloodPressure"  "DiastolicBloodPressure"
#[10] "PulseOxygen"            "Vital_Sign"             "AVPU"                  
#[13] "GlasgowComaScore"       "AirwayAssessment"       "AirwayManagement"      
#[16] "BreathingAssessment"    "OxygenApplied"          "ChestRadiograph"       
#[19] "PulsesEvaluated"        "FluidsStarted"          "LabsSent."             
#[22] "Movedallextremities"    "SkullRadiograph"        "gos"                          
###########################################################################################
#EXPLORATORY DATA ANALYSIS
###########################################################################################
###Section wih several exploratory data analysis functions
#Exploratory Data Anlysis
#dim(data)
#str (data)
#head(data)
names(data)
#summary(data)#This comand will provide a whole set of descriptive results for each variables
#Hmisc::describe(data$R1)
#Hmisc::describe(data$R2)
#Hmisc::describe(data$R3)
#ad.test() # Anderson-Darling test for normality
#skewness() #Will provide skweness analysis
#kurtosis() - 3 #Will provide kurtosis analysis
#qplot() # histogram plot
#pwr.anova.test(k = , n = , f = , sig.level = , power = )#Power analysis for variance analysis
#boxplot() #will provide a boxplot for the variables to analysis potential outliers
#detach(data)
#attach(data)

with(data,by(PLAYINGYEARS,lineage,describe))
with(data,by(DAYSPLAYING,lineage,describe))

###########################################################################################
#OBJECTIVE 1: Latent Profile Analysis - General Sample
###########################################################################################
###Adolescents
model_adolescents<-subset(data,data$lineage=="adolescents")
test1<-with(model_adolescents,data.frame(BFIE,BFIA,BFIC,BFIN,BFIO))
test1<-na.omit(test1)
fit <- mclustBIC(test1,2)
cl<-mclustModel(test1,fit)
summary(cl) # display the best model
cl
lat_adol<-cl$parameters$mean
plot(cl) # plot results 
#y<-NULL
#y[cl$z[,1]<=0.50]<-c("Lat1")
#y[cl$z[,1]>=0.51]<-c("Lat2")
x<-car::recode(cl$z[,1],"0.51:1.00='Lat1';else='Lat2'")
by(model_adolescents$BFIE,x,describe)
by(model_adolescents$BFIA,x,describe)
by(model_adolescents$BFIC,x,describe)
by(model_adolescents$BFIN,x,describe)
by(model_adolescents$BFIO,x,describe)
wilcox.test(model_adolescents$BFIE~x)
wilcox.test(model_adolescents$BFIA~x)
wilcox.test(model_adolescents$BFIC~x)
wilcox.test(model_adolescents$BFIN~x)
wilcox.test(model_adolescents$BFIO~x)

###Emerging Adults
model_emadults<-subset(data,data$lineage=="emadults")
test1<-with(model_emadults,data.frame(BFIE,BFIA,BFIC,BFIN,BFIO))
#test1<-na.omit(test1)
fit <- mclustBIC(test1,3)
cl<-mclustModel(test1,fit)
summary(cl) # display the best model
lat_emadults<-cl$parameters$mean
#x<-car::recode(cl$z[,1],"0.51:1.00='Lat1';else='Lat2'")
x<-NULL
x[cl$z[,1]>=0.51]<-c("Lat1")
x[cl$z[,2]>=0.51]<-c("Lat2")
x[cl$z[,3]>=0.51]<-c("Lat3")
x<-as.factor(x)
by(model_emadults$BFIE,x,mean)
by(model_emadults$BFIE,x,sd)
anova<-aov(model_emadults$BFIE~x,data=model_emadults)
TukeyHSD(anova)
by(model_emadults$BFIA,x,mean)
by(model_emadults$BFIA,x,sd)
anova<-aov(model_emadults$BFIA~x,data=model_emadults)
TukeyHSD(anova)
anova<-aov(model_emadults$BFIC~x,data=model_emadults)
TukeyHSD(anova)
anova<-aov(model_emadults$BFIN~x,data=model_emadults)
TukeyHSD(anova)
anova<-aov(model_emadults$BFIO~x,data=model_emadults)
TukeyHSD(anova)

#### GRAPH 1 - Lines for each profile
X1<-c("BFIE","BFIA","BFIC","BFIN","BFIO")
X2<-c(3)
value<-c(25.06,
34.38,
33.57,
25.04,
39.00)
overall<-data.frame(X1,X2,value)
lat_adol<-melt(lat_adol)
lat_adol<-rbind(lat_adol,overall)
lat_adol$X2<-as.factor(lat_adol$X2)
lat_adol<-rename.vars(lat_adol,from="X2", to="Profile")
setEPS()
postscript("general_adolescents.eps")
ggplot(data=lat_adol, aes(x=X1, y=value, group=Profile,
	color=Profile)) + 
geom_line(size=1.5) + geom_point(size=3,fill="white") +
ylab("BFI Dimensions") + xlab("BFI Scores") +
scale_colour_manual(values=c("#999999", "#E69F00","darkred"),
					name="Latent Profiles",
                    breaks=c(1,2,3),
                    labels=c("Introversive", "Extroversive","BFI Norm"))
dev.off()

lat_emadults
lat_emadults<-melt(lat_emadults)
X1<-c("BFIE","BFIA","BFIC","BFIN","BFIO")
X2<-c(4)
value<-c(25.06,
34.38,
33.57,
25.04,
39.00)
overall<-data.frame(X1,X2,value)
lat_emadults<-rbind(lat_emadults,overall)
lat_emadults$X2<-as.factor(lat_emadults$X2)
lat_emadults<-rename.vars(lat_emadults,from="X2", to="Profile")
#lat_emadults[,2]<-car::recode(lat_emadults[,2],"1='Balanced';2='Tony Style'")
setEPS()
postscript("general_emadults.eps")
ggplot(data=lat_emadults, aes(x=X1, y=value, group=Profile,
	color=Profile)) + 
geom_line(size=1.5) + geom_point(size=3,fill="white") +
ylab("BFI Dimensions") + xlab("BFI Scores") +
scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9","darkred"),
					name="Latent Profiles",
                    breaks=c("1", "2","3","4"),
                    labels=c("Introversive", "Extroversive","Ambiversive","BFI Norm"))
dev.off()

###########################################################################################
#OBJECTIVE 2: Latent Profile Analysis - SPECIALIZATION 
###########################################################################################
###Tank
#model_tank<-subset(data,data$SPECIALIZATION=="Tank")
#test1<-with(model_tank,data.frame(BFIE,BFIA,BFIC,BFIN,BFIO))
#test1<-na.omit(test1)
#fit <- mclustBIC(test1,1)
#cl<-mclustModel(test1,fit)
#summary(cl) # display the best model
#plot(fit) # plot results 
#cl$parameters$mean
model_tank<-subset(model_adolescents,model_adolescents$SPECIALIZATION=="Tank")
test1<-with(model_tank,data.frame(BFIE,BFIA,BFIC,BFIN,BFIO))
test1<-na.omit(test1)
fit <- mclustBIC(test1,1)
cl<-mclustModel(test1,fit)
summary(cl) # display the best model
plot(fit) # plot results 
lat_tank_adol<-cl$parameters$mean
x<-car::recode(cl$z[,1],"0.51:1.00='Lat1';else='Lat2'")
#x[cl$z[,1]>=0.50]<-c("Lat1")
#x[cl$z[,1]<=0.51]<-c("Lat2")
x<-as.factor(x)
describe(model_tank$BFIE)
describe(model_tank$BFIA)
describe(model_tank$BFIC)
describe(model_tank$BFIN)
describe(model_tank$BFIO)
wilcox.test(model_emadults$BFIE~x)
wilcox.test(model_emadults$BFIA~x)
wilcox.test(model_emadults$BFIC~x)
wilcox.test(model_emadults$BFIN~x)
wilcox.test(model_emadults$BFIO~x)

model_tank<-subset(model_emadults,model_emadults$SPECIALIZATION=="Tank")
test1<-with(model_tank,data.frame(BFIE,BFIA,BFIC,BFIN,BFIO))
test1<-na.omit(test1)
fit <- mclustBIC(test1)
cl<-mclustModel(test1,fit)
summary(cl) # display the best model
plot(fit) # plot results 
lat_tank_emadult<-cl$parameters$mean
x<-car::recode(cl$z[,1],"0.51:1.00='Lat1';else='Lat2'")
#x[cl$z[,1]>=0.50]<-c("Lat1")
#x[cl$z[,1]<=0.51]<-c("Lat2")
x<-as.factor(x)
describe(model_tank$BFIE)
describe(model_tank$BFIA)
describe(model_tank$BFIC)
describe(model_tank$BFIN)
describe(model_tank$BFIO)
wilcox.test(model_emadult5s$BFIE~x)
wilcox.test(model_emadults$BFIA~x)
wilcox.test(model_emadults$BFIC~x)
wilcox.test(model_emadults$BFIN~x)
wilcox.test(model_emadults$BFIO~x)

###Healer
#model_healer<-subset(data,data$SPECIALIZATION=="Healer")
#test1<-with(model_healer,data.frame(BFIE,BFIA,BFIC,BFIN,BFIO))
#test1<-na.omit(test1)
#fit <- mclustBIC(test1,1)
#cl<-mclustModel(test1,fit)
#summary(cl) # display the best model
#cl$parameters$mean
#print(cl) # display the best model
#plot(fit) # plot results 

model_healer<-subset(model_adolescents,model_adolescents$SPECIALIZATION=="Healer")
test1<-with(model_healer,data.frame(BFIE,BFIA,BFIC,BFIN,BFIO))
test1<-na.omit(test1)
fit <- mclustBIC(test1,1)
cl<-mclustModel(test1,fit)
summary(cl) # display the best model
lat_healer_adole<-cl$parameters$mean
print(cl) # display the best model
plot(fit) # plot results 

describe(model_tank$BFIE)
describe(model_tank$BFIA)
describe(model_tank$BFIC)
describe(model_tank$BFIN)
describe(model_tank$BFIO)

model_healer<-subset(model_emadults,model_emadults$SPECIALIZATION=="Healer")
test1<-with(model_healer,data.frame(BFIE,BFIA,BFIC,BFIN,BFIO))
test1<-na.omit(test1)
fit <- mclustBIC(test1)
cl<-mclustModel(test1,fit)
summary(cl) # display the best model
lat_healer_emadults<-cl$parameters$mean
print(cl) # display the best model
plot(fit) # plot results 

describe(model_healer$BFIE)
describe(model_healer$BFIA)
describe(model_healer$BFIC)
describe(model_healer$BFIN)
describe(model_healer$BFIO)

###Demage
model<-subset(data,data$SPECIALIZATION=="Damage")
test1<-with(model,data.frame(BFIE,BFIA,BFIC,BFIN,BFIO))
test1<-na.omit(test1)
fit <- mclustBIC(test1)
cl<-mclustModel(test1,fit)
lat_damage_adol<-cl$parameters$mean
#summary(cl) # display the best model
#print(cl) # display the best model
#plot(fit) # plot results 

x<-car::recode(cl$z[,1],"0.51:1.00='Lat1';else='Lat2'")
#x[cl$z[,1]>=0.50]<-c("Lat1")
#x[cl$z[,1]<=0.51]<-c("Lat2")
x<-as.factor(x)
by(model$BFIE,x,describe)
by(model$BFIA,x,describe)
by(model$BFIC,x,describe)
by(model$BFIN,x,describe)
by(model$BFIO,x,describe)

model<-subset(model_adolescents,model_adolescents$SPECIALIZATION=="Damage")
test1<-with(model,data.frame(BFIE,BFIA,BFIC,BFIN,BFIO))
test1<-na.omit(test1)
fit <- mclustBIC(test1)
cl<-mclustModel(test1,fit)
lat_damage_adol<-cl$parameters$mean
summary(cl) # display the best model
print(cl) # display the best model
plot(fit) # plot results 
x<-car::recode(cl$z[,1],"0.51:1.00='Lat1';else='Lat2'")
#x[cl$z[,1]>=0.50]<-c("Lat1")
#x[cl$z[,1]<=0.51]<-c("Lat2")
x<-as.factor(x)
by(model$BFIE,x,describe)
by(model$BFIA,x,describe)
by(model$BFIC,x,describe)
by(model$BFIN,x,describe)
by(model$BFIO,x,describe)


model<-subset(model_emadults,model_emadults$SPECIALIZATION=="Damage")
test1<-with(model,data.frame(BFIE,BFIA,BFIC,BFIN,BFIO))
test1<-na.omit(test1)
fit <- mclustBIC(test1)
cl<-mclustModel(test1,fit)
lat_damage_emadults<-cl$parameters$mean
summary(cl) # display the best model
print(cl) # display the best model
plot(fit) # plot results 
x<-car::recode(cl$z[,1],"0.51:1.00='Lat1';else='Lat2'")
#x[cl$z[,1]>=0.50]<-c("Lat1")
#x[cl$z[,1]<=0.51]<-c("Lat2")
x<-as.factor(x)
by(model$BFIE,x,describe)
by(model$BFIA,x,describe)
by(model$BFIC,x,describe)
by(model$BFIN,x,describe)
by(model$BFIO,x,describe)

anova<-aov(BFIE~SPECIALIZATION,data=data)
TukeyHSD(anova)
anova<-aov(BFIA~SPECIALIZATION,data=data)
TukeyHSD(anova)
anova<-aov(BFIC~SPECIALIZATION,data=data)
TukeyHSD(anova)
anova<-aov(BFIN~SPECIALIZATION,data=data)
TukeyHSD(anova)
anova<-aov(BFIO~SPECIALIZATION,data=data)
TukeyHSD(anova)

#### GRAPH 1 - Lines for each profile
lat_tank_adol<-melt(lat_tank_adol)
lat_tank_adol$Specialization<-c("Tank")
lat_tank_adol$Group<-c("Adolescents")
lat_tank_emadult<-melt(lat_tank_emadult)
lat_tank_emadult$Specialization<-c("Tank")
lat_tank_emadult$Group<-c("Em. Adults")
lat_healer_adole<-melt(lat_healer_adole)
lat_healer_adole$Specialization<-c("Healer")
lat_healer_adole$Group<-c("Adolescents")
lat_healer_emadults<-melt(lat_healer_emadults)
lat_healer_emadults$Specialization<-c("Healer")
lat_healer_emadults$Group<-c("Em. Adults")
lat_damage_adol<-melt(lat_damage_adol)
lat_damage_adol$Specialization<-c(rep("Damage Ext.",5),rep("Damage Int.",5))
lat_damage_adol$Group<-c("Adolescents")
lat_damage_emadults<-melt(lat_damage_emadults)
lat_damage_emadults$Specialization<-c(rep("Damage Ext.",5),rep("Damage Int.",5))
lat_damage_emadults$Group<-c("Em. Adults")
lat_spec<-rbind(lat_tank_adol,lat_tank_emadult,lat_healer_adole,
				lat_healer_emadults,lat_damage_adol,lat_damage_emadults)


lat_spec$X2<-as.factor(lat_spec$X2)
lat_spec$Specialization<-as.factor(lat_spec$Specialization)
lat_spec$Group<-as.factor(lat_spec$Group)
#lat_emadults[,2]<-car::recode(lat_emadults[,2],"1='Balanced';2='Tony Style'")
setEPS()
postscript("specialization.eps")
ggplot(data=lat_spec, aes(x=X1, y=value,
	fill=Group)) + 
geom_bar(position="dodge") + facet_grid(Specialization ~ .) +
ylab("BFI Dimensions") + xlab("BFI Scores") +
geom_point(aes(x="BFIE",y=25.06)) +
geom_point(aes(x="BFIA",y=34.38))+
geom_point(aes(x="BFIC",y=33.57))+
geom_point(aes(x="BFIN",y=25.04))+
geom_point(aes(x="BFIO",y=39.00))+
geom_point(aes(x=0.5,y=44))+
geom_segment(aes(x=0.5,xend=0.5,y=41,yend=47))+
geom_segment(aes(x="BFIE",xend="BFIE",y=(25.06-7.20),yend=(25.06+7.20)))+
geom_segment(aes(x="BFIA",xend="BFIA",y=(34.38-6.12),yend=(34.38+6.12)))+
geom_segment(aes(x="BFIC",xend="BFIC",y=(33.57-6.39),yend=(33.57+6.39)))+
geom_segment(aes(x="BFIN",xend="BFIN",y=(25.04-6.88),yend=(25.04+6.88)))+
geom_segment(aes(x="BFIO",xend="BFIO",y=(39.00-6.90),yend=(39.00+6.90)))+
annotate("text", x = 0.9, y = 44, label = "BFI Norm",size=3)
dev.off()
#scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9"),
#					name="Latent Profiles",
#                    breaks=c("1", "2","3"),
#                    labels=c("Tony Style", "Balanced","Thiago Style"))
#BFIE		25.06 (7.20)  
#BFIA		34.38 (6.12)  
#BFIC		33.57 (6.39)  
#BFIN		25.04 (6.88)  
#BFIO	39.00 (6.90)
###########################################################################################
#OBJECTIVE 5: Latent Profile Analysis - Styles of play
###########################################################################################
###PvP
#model_pvp<-subset(data,data$STYLEOFPLAY=="PvP (Player VS Player)")
#test1<-with(model_pvp,data.frame(BFIE,BFIA,BFIC,BFIN,BFIO))
#test1<-na.omit(test1)
#fit <- mclustBIC(test1)
#cl<-mclustModel(test1,fit)
#cl
#summary(cl) # display the best model
#plot(fit) # plot results 
#cl$parameters$mean
#y[cl$z[,1]>=0.51]<-c("Lat1")
#y[cl$z[,2]>=0.51]<-c("Lat2")
#x[cl$z[,3]>=0.51]<-c("Lat3")
#x<-as.factor(y)

#wilcox.test(model_pvp$BFIE~x)
#wilcox.test(model_pvp$BFIA~x)
#wilcox.test(model_pvp$BFIC~x)
#wilcox.test(model_pvp$BFIN~x)
#wilcox.test(model_pvp$BFIO~x)

model_pvp<-subset(model_adolescents,model_adolescents$STYLEOFPLAY=="PvP (Player VS Player)")
test1<-with(model_pvp,data.frame(BFIE,BFIA,BFIC,BFIN,BFIO))
test1<-na.omit(test1)
fit <- mclustBIC(test1,2)
cl<-mclustModel(test1,fit)
cl
summary(cl) # display the best model
plot(fit) # plot results 
model_pvp_adole<-cl$parameters$mean
x<-car::recode(cl$z[,1],"0.51:1.00='Lat1';else='Lat2'")
#x[cl$z[,1]>=0.50]<-c("Lat1")
#x[cl$z[,1]<=0.51]<-c("Lat2")
x<-as.factor(x)
by(model_pvp$BFIE,x,describe)
by(model_pvp$BFIA,x,describe)
by(model_pvp$BFIC,x,describe)
by(model_pvp$BFIN,x,describe)
by(model_pvp$BFIO,x,describe)

model_pvp<-subset(model_emadults,model_emadults$STYLEOFPLAY=="PvP (Player VS Player)")
test1<-with(model_pvp,data.frame(BFIE,BFIA,BFIC,BFIN,BFIO))
test1<-na.omit(test1)
fit <- mclustBIC(test1)
cl<-mclustModel(test1,fit)
cl
summary(cl) # display the best model
plot(fit) # plot results 
model_pvp_emadult<-cl$parameters$mean
x<-car::recode(cl$z[,1],"0.51:1.00='Lat1';else='Lat2'")
x<-as.factor(x)
by(model_pvp$BFIE,x,describe)
by(model_pvp$BFIA,x,describe)
by(model_pvp$BFIC,x,describe)
by(model_pvp$BFIN,x,describe)
by(model_pvp$BFIO,x,describe)

### PvE
#model_pve<-subset(data,data$STYLEOFPLAY=="PvE (Player VS Environment)")
#test1<-with(model_pve,data.frame(BFIE,BFIA,BFIC,BFIN,BFIO))
#test1<-na.omit(test1)
#fit <- mclustBIC(test1)
#cl<-mclustModel(test1,fit)
#cl
#summary(cl) # display the best model
#plot(fit) # plot results 
#cl$parameters$mean
#latpve<-NULL
#latpve[cl$z[,1]>=0.51]<-c("Lat1")
#latpve[cl$z[,2]>=0.51]<-c("Lat2")
#latpve[cl$z[,3]>=0.51]<-c("Lat3")
#<-as.factor(latpve)

model_pve<-subset(model_adolescents,model_adolescents$STYLEOFPLAY=="PvE (Player VS Environment)")
test1<-with(model_pve,data.frame(BFIE,BFIA,BFIC,BFIN,BFIO))
test1<-na.omit(test1)
fit <- mclustBIC(test1)
cl<-mclustModel(test1,fit)
cl
summary(cl) # display the best model
plot(fit) # plot results 
model_pve_adol<-cl$parameters$mean
x<-car::recode(cl$z[,1],"0.51:1.00='Lat1';else='Lat2'")
x<-as.factor(x)
by(model_pve$BFIE,x,describe)
by(model_pve$BFIA,x,describe)
by(model_pve$BFIC,x,describe)
by(model_pve$BFIN,x,describe)
by(model_pve$BFIO,x,describe)

model_pve<-subset(model_emadults,model_emadults$STYLEOFPLAY=="PvE (Player VS Environment)")
test1<-with(model_pve,data.frame(BFIE,BFIA,BFIC,BFIN,BFIO))
test1<-na.omit(test1)
fit <- mclustBIC(test1)
cl<-mclustModel(test1,fit)
cl
summary(cl) # display the best model
plot(fit) # plot results 
model_pve_emadult<-cl$parameters$mean
y<-NULL
y[cl$z[,1]>=0.51]<-c("Lat1")
y[cl$z[,2]>=0.51]<-c("Lat2")
y[cl$z[,3]>=0.51]<-c("Lat3")
x<-as.factor(y)
by(model_pve$BFIE,y,describe)
by(model_pve$BFIA,y,describe)
by(model_pve$BFIC,y,describe)
by(model_pve$BFIN,y,describe)
by(model_pve$BFIO,y,describe)


anova<-aov(BFIE~x,data=model_pve)
TukeyHSD(anova)
anova<-aov(BFIA~x,data=model_pve)
TukeyHSD(anova)
anova<-aov(BFIC~x,data=model_pve)
TukeyHSD(anova)
anova<-aov(BFIN~x,data=model_pve)
TukeyHSD(anova)
anova<-aov(BFIO~x,data=model_pve)
TukeyHSD(anova)

model_rp<-subset(model_adolescents,model_adolescents$STYLEOFPLAY=="RP (Role Playing)")
test1<-with(model_rp,data.frame(BFIE,BFIA,BFIC,BFIN,BFIO))
test1<-na.omit(test1)
fit <- mclustBIC(test1,1)
cl<-mclustModel(test1,fit)
summary(cl) # display the best model
plot(fit) # plot results 
cl
model_rp_adole<-cl$parameters$mean
describe(test1)

model_rp<-subset(model_emadults,model_emadults$STYLEOFPLAY=="RP (Role Playing)")
test1<-with(model_rp,data.frame(BFIE,BFIA,BFIC,BFIN,BFIO))
test1<-na.omit(test1)
fit <- mclustBIC(test1,1)
cl<-mclustModel(test1,fit)
summary(cl) # display the best model
plot(fit) # plot results 
cl
model_rp_adults<-cl$parameters$mean
describe(test1)

#### GRAPH 1 - Lines for each profile
#write.csv(lat_spec,"WoW_lat_style.csv")

X1<-c("BFIE","BFIA","BFIC","BFIN","BFIO")
X2<-c(3)
value<-c(25.06,
34.38,
33.57,
25.04,
39.00)
overall<-data.frame(X1,X2,value)

model_pvp_adole<-melt(model_pvp_adole)
model_pvp_adole<-rbind(model_pvp_adole,overall)
model_pvp_adole$X2<-as.factor(model_pvp_adole$X2)
model_pvp_adole<-rename.vars(model_pvp_adole,from="X2", to="Profile")
model_pvp_adole$Specialization<-c(rep("PvP Ext.",5),rep("PvP Int.",5),rep("BFI Norm",5))
model_pvp_adole$Group<-c("PvP Adolescents")
graph1<-ggplot(data=model_pvp_adole, aes(x=X1, y=value, group=Profile,
	color=Profile)) + 
geom_line(size=1.5) + geom_point(size=3,fill="white") +
ylab("BFI Dimensions") + xlab("BFI Scores") + facet_grid(Group ~ .) +
scale_colour_manual(values=c("black","lightblue","darkred"),
					name="Latent Profiles",
                    breaks=c(2,1,3),
                    labels=c("Extroversive","Introversive", "BFI Norm"))


model_pvp_emadult<-melt(model_pvp_emadult)
model_pvp_emadult<-rbind(model_pvp_emadult,overall)
model_pvp_emadult$X2<-as.factor(model_pvp_emadult$X2)
model_pvp_emadult<-rename.vars(model_pvp_emadult,from="X2", to="Profile")
model_pvp_emadult$Specialization<-c(rep("PvP Ext.",5),rep("PvP Int.",5),rep("BFI Norm",5))
model_pvp_emadult$Group<-c("PvP Em. Adults")
graph2<-ggplot(data=model_pvp_emadult, aes(x=X1, y=value, group=Profile,
	color=Profile)) + 
geom_line(size=1.5) + geom_point(size=3,fill="white") +
ylab("BFI Dimensions") + xlab("BFI Scores") + facet_grid(Group ~ .)+
scale_colour_manual(values=c("lightblue", "black","darkred"),
					name="Latent Profiles",
                    breaks=c(1,2,3),
                    labels=c("Extroversive", "Introversive","BFI Norm"))
setEPS()
postscript("style_pvp.eps")
grid.arrange(graph1,graph2)
dev.off()

model_pve_adol<-melt(model_pve_adol)
model_pve_adol<-rbind(model_pve_adol,overall)
model_pve_adol$X2<-as.factor(model_pve_adol$X2)
model_pve_adol<-rename.vars(model_pve_adol,from="X2", to="Profile")
model_pve_adol$Specialization<-c(rep("PvE Ext.",5),rep("PvE Int.",5),rep("BFI Norm",5))
model_pve_adol$Group<-c("PvE Adolescents")
graph3<-ggplot(data=model_pve_adol, aes(x=X1, y=value, group=Profile,
	color=Profile)) + 
geom_line(size=1.5) + geom_point(size=3,fill="white") +
ylab("BFI Dimensions") + xlab("BFI Scores") + facet_grid(Group ~ .)+
scale_colour_manual(values=c("lightblue", "black","darkred"),
					name="Latent Profiles",
                    breaks=c(1,2,3),
                    labels=c("Extroversive", "Introversive","BFI Norm"))

model_pve_emadult<-melt(model_pve_emadult)
X1<-c("BFIE","BFIA","BFIC","BFIN","BFIO")
X2<-c(4)
value<-c(25.06,
34.38,
33.57,
25.04,
39.00)
overall2<-data.frame(X1,X2,value)
model_pve_emadult<-rbind(model_pve_emadult,overall2)
model_pve_emadult$X2<-as.factor(model_pve_emadult$X2)
model_pve_emadult<-rename.vars(model_pve_emadult,from="X2", to="Profile")
model_pve_emadult$Specialization<-c(rep("PvE Ext.",5),rep("PvE Int.",5),rep("PvE Amb.",5),rep("BFI Norm",5))
model_pve_emadult$Group<-c("PvE Em. Adults")
graph4<-ggplot(data=model_pve_emadult, aes(x=X1, y=value, group=Profile,
	color=Profile)) + 
geom_line(size=1.5) + geom_point(size=3,fill="white") +
ylab("BFI Dimensions") + xlab("BFI Scores") + facet_grid(Group ~ .)+
scale_colour_manual(values=c("darkgoldenrod","lightblue","black","darkred"),
					name="Latent Profiles",
                    breaks=c(2,3,1,4),
                    labels=c("Extroversive", "Introversive","Ambiversive","BFI Norm"))
setEPS()
postscript("style_pve.eps")
grid.arrange(graph3,graph4)
dev.off()


X1<-c("BFIE","BFIA","BFIC","BFIN","BFIO")
X2<-c(2)
value<-c(25.06,
34.38,
33.57,
25.04,
39.00)
overall3<-data.frame(X1,X2,value)
model_rp_adole<-melt(model_rp_adole)
model_rp_adole<-rbind(model_rp_adole,overall3)
model_rp_adole$X2<-as.factor(model_rp_adole$X2)
model_rp_adole<-rename.vars(model_rp_adole,from="X2", to="Profile")
model_rp_adole$Specialization<-c("RP")
model_rp_adole$Group<-c("RP Adolescents")
graph5<-ggplot(data=model_rp_adole, aes(x=X1, y=value, group=Profile,
	color=Profile)) + 
geom_line(size=1.5) + geom_point(size=3,fill="white") +
ylab("BFI Dimensions") + xlab("BFI Scores") + facet_grid(Group ~ .)+
scale_colour_manual(values=c("lightblue","darkred"),
					name="Latent Profiles",
                    breaks=c(1,2),
                    labels=c("BFI RP","BFI Norm"))

model_rp_adults<-melt(model_rp_adults)
model_rp_adults<-rbind(model_rp_adults,overall3)
model_rp_adults$Specialization<-c("RP")
model_rp_adults$Group<-c("RP Em. Adults")
#lat_spec<-rbind(model_pvp_adole,model_pvp_emadult,model_pve_adol,
#				model_pve_emadult,model_rp_adole,model_rp_adults)
model_rp_adults$X2<-as.factor(model_rp_adults$X2)
model_rp_adults<-rename.vars(model_rp_adults,from="X2", to="Profile")
graph6<-ggplot(data=model_rp_adults, aes(x=X1, y=value, group=Profile,
	color=Profile)) + 
geom_line(size=1.5) + geom_point(size=3,fill="white") +
ylab("BFI Dimensions") + xlab("BFI Scores") + facet_grid(Group ~ .)+
scale_colour_manual(values=c("lightblue","darkred"),
					name="Latent Profiles",
                    breaks=c(1,2),
                    labels=c("BFI RP","BFI Norm"))
setEPS()
postscript("style_rp.eps")
grid.arrange(graph5,graph6)
dev.off()

###########################################################################################
#ALPHAS
###########################################################################################

alpha(data, keys=NULL,title=NULL,na.rm = TRUE)
alpha(data, keys=NULL,title=NULL,na.rm = TRUE)

bfie<-with(data,data.frame(BFI1,bfirecoded6,BFI11,bfirecoded21,BFI26,bfirecoded31,BFI36))
bfia<-with(data,data.frame(bfirecoded2,BFI7,bfirecoded12,BFI17,BFI22,bfirecoded27,BFI32,bfirecoded37,BFI42))
bfic<-with(data,data.frame(BFI3, bfirecoded8, BFI13, bfirecoded18, bfirecoded23, BFI28, BFI33, BFI38, bfirecoded43))
bfin<-with(data,data.frame(BFI4, bfirecoded9, BFI14, BFI19, bfirecoded24, BFI29, bfirecoded34, BFI39))
bfio<-with(data,data.frame(BFI5, BFI10, BFI15, BFI20, BFI25, BFI30, bfirecoded35, BFI40, bfirecoded41, BFI44))

#Alpha de Cronbach by ltm package
install.packages("ltm")
library(ltm)
data<-na.omit(data)
cronbach.alpha(bfio, standardized = TRUE, CI = TRUE, 
               probs = c(0.025, 0.975), B = 1000, na.rm = FALSE)

model_pve$BFIE,y,describe)
by(model_pve$BFIA,y,describe)
by(model_pve$BFIC,y,describe)
by(model_pve$BFIN,y,describe)
by(model_pve$BFIO

###########################################################################################
#Correlations
###########################################################################################

str(model_adolescents)
cor<-with(model_adolescents,data.frame(BFIE,BFIA,BFIC,BFIN,BFIO,
  HOURSPLAYING,DAYSPLAYING,PLAYINGYEARS))
correlations<-rcorr(as.matrix(cor),typ="spearman")
write.csv(correlations$r,file="/Users/joaovissoci/Desktop/R.csv")
write.csv(correlations$P,file="/Users/joaovissoci/Desktop/P.csv")

###Emerging Adults
model_emadults<-subset(data,data$lineage=="emadults")
test1<-with(model_emadults,data.frame(BFIE,BFIA,BFIC,BFIN,BFIO))
#test1<-na.omit(test1)
fit <- mclustBIC(test1)
cl<-mclustModel(test1,fit)
summary(cl) # display the best model
lat_emadults<-cl$parameters$mean
#x<-car::recode(cl$z[,1],"0.51:1.00='Lat1';else='Lat2'")
x<-NULL
x[cl$z[,1]>=0.51]<-c("Intro")
x[cl$z[,2]>=0.51]<-c("Extro")
x[cl$z[,3]>=0.51]<-c("Zmbi")
x<-as.factor(x)

model_emadults$STYLEOFPLAY<-car::recode(model_emadults$STYLEOFPLAY,"'RP (Role Playing)'=NA")
#Zone
mytable <- with(data,table(x,model_emadults$STYLEOFPLAY))
mytable
prop.table(mytable, 2)
assocstats(mytable)
model_emadults$STYLEOFPLAY<-as.factor(model_emadults$STYLEOFPLAY)
logmodel<-glm(STYLEOFPLAY ~   x,family=binomial, data=model_emadults)
summary(logmodel)
#anova(reglogGEU)
#exp(coef(model1_death)) # exponentiated coefficients
#exp(confint(model1_death)) # 95% CI for exponentiated coefficients
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
logistic.display(logmodel)


