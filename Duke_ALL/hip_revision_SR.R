###########################THR_SRMA_2014_SCRIPT#################################
################################################################################
#template_secondary_data_analysis.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. see full license at the end of this file.
#######################################################################################
#this script follows a combination of the guidelines proposed by Hadley Wickham http://goo.gl/c04kq as well as using the formatR package http://goo.gl/ri6ky
#if this is the first time you are conducting an analysis using this protocol, please watch http://goo.gl/DajIN while following step by step

#link to manuscript http://goo.gl/shQckc 

#####################################################################################
#SETTING ENVIRONMENT
#####################################################################################

#command below will install individual and is only run once. remove the hash tag if this is the first time you are running the code on RStudio, and then you can add the hash tag again
#install.packages("car", repos="http://cran.r-project.org")
#install.packages("ggplot2", repos="http://cran.r-project.org")
#install.packages("mada",repos="http://cran.r-project.org")
#install.packages("RCurl", repos="http://cran.r-project.org")
#install.packages("gdata", repos="http://cran.r-project.org")
#install.packages("meta", repos="http://cran.r-project.org")

#Load packages (after installed) with the library function
#Loading packages
lapply(c("metafor","ggplot2","car","RCurl","gdata","meta"), library, character.only=T)

#############################################################################
#IMPORTING DATA AND RECODING
#############################################################################
#Importing data set from the Spredsheet in google docs (Insert link)
#options(RCurlOptions = list(capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE))

#Data for prevalence metanalisys revision   
#webdata <- getURL("https://docs.google.com/spreadsheet/pub?key=0AjVEotAQPzQsdFBXWHN6Yk5lWGtrOFhNQ3dQd043MVE&single=true&gid=16&output=csv")
#data<-read.csv(textConnection(webdata)) #Data for revision all 

data<-read.csv("/Users/joaovissoci/Dropbox/datasets/RoR/hip_revision_SR/hip_SR.csv")

str(data)

data$Type<-car::recode(data$Type,"'cemented'='Cemented';'uncemented'='Uncemented';'mixed'='Mixed';'NO'='Not available'")

#data$DeathLost<-car::recode(data$DeathLost,"'NS'=NA")
data$DeathLost<-as.numeric(as.character(data$DeathLost))

#Organizing dataset for overall revision analysis
metanalise<-with(data,data.frame(Revisions,Hips,Names))
#Organizing dataset for cemented revision analysis
metaCement<-subset(data, data$Type=="Cemented")
#Organizing dataset for uncemented revision analysis
metaUNCement<-subset(data, data$Type=="Uncemented")

#############################################################################
#Figure 1: METANALISYS POLL PREVALENCE REVISION ALL CAUSES OVER 15 YEARS Follow up 
#############################################################################
#Forest Plot Pooled Prevalence
metanalysis<-metaprop(Revisions, Hips, Names, sm="PLN",data=data)
tiff("/home/joao/Desktop/Overall.tiff", width = 800, height = 1100,compression = 'lzw')
meta::forest(metanalysis)
dev.off()
funnel(metanalysis)

#Forest Plot for Pooled Prevalence subgrouped by Type
metaPPType<-metaprop(Revisions,Hips,Names, sm="PLN",data=data,byvar=Type,print.byvar=FALSE,comb.fixed=FALSE)
tiff("/Users/joaovissoci/Desktop/Type.tiff", width = 800, height = 1250,compression = 'lzw')
meta::forest(metaPPType)
dev.off()
#######################################################################################
#Figure 2: META POLL PREVALENCE CEMENTED THR REVISION SUBANALYSIS OVER 15 YEARS Follow up 
#######################################################################################

#Forest Plot for Pooled Prevalence subgrouped by Follow Up
metaCEFUP<-metaprop(Revisions,Hips,Names, sm="PLN",data=metaCement,byvar=FUP)
tiff("/home/joao/Desktop/CemtFUP.tiff", width = 800, height = 800,compression = 'lzw')
meta::forest(metaCEFUP)
dev.off()
funnel(metaCEFUP)
prop<-metaCEFUP$event/metaCEFUP$n
with(metaCement,by(prop,FUP,summary))

#Forest Plot for Pooled Prevalence subgrouped by Cohort Mean Age
metaCEAge<-metaprop(Revisions,Hips,Names, sm="PLN",data=metaCement,byvar=AgeGroup)
tiff("/home/joao/Desktop/CemtAge.tiff", width = 800, height = 850,compression = 'lzw')
meta::forest(metaCEAge)
dev.off()
prop<-metaCEFUP$event/metaCEFUP$n
with(metaCement,by(prop,FUP,summary))

#Forest Plot for Pooled Prevalence subgrouped by Decade
metaCEDecade<-metaprop(Revisions,Hips,Names, sm="PLN",data=metaCement,byvar=Decade)
tiff("/home/joao/Desktop/CemtDecade.tiff", width = 800, height = 800,compression = 'lzw')
meta::forest(metaCEDecade)
dev.off()
#Forest Plot for Pooled Prevalence subgrouped by %DeathLost
metaCEDeath<-metaprop(Revisions,Hips,Names, sm="PLN",data=metaCement,byvar=DeathLost)
tiff("/home/joao/Desktop/CemtDeath.tiff", width = 800, height = 700,compression = 'lzw')
meta::forest(metaCEDeath)
dev.off()

###########################################################################################
#Figure 3: META POLL PREVALENCE UNCEMENTED THR REVISION SUBANALYSIS OVER 15 YEARS Follow up 
###########################################################################################

#Forest Plot for Pooled Prevalence subgrouped by Follow Up
metaUNFUP<-metaprop(Revisions,Hips,Names, sm="PLN",data=metaUNCement,byvar=FUP)
tiff("/Users/Talitha/Desktop/MetaHip/UncemFUP.tiff", width = 800, height = 650,compression = 'lzw')
meta::forest(metaUNFUP)
dev.off()
#Forest Plot for Pooled Prevalence subgrouped by Cohort Mean Age
metaUNAge<-metaprop(Revisions,Hips,Names, sm="PLN",data=metaUNCement,byvar=AgeGroup)
tiff("/Users/Talitha/Desktop/MetaHip/UncemAge.tiff", width = 800, height = 700,compression = 'lzw')
meta::forest(metaUNAge)
dev.off()
#Forest Plot for Pooled Prevalence subgrouped by Decade
metaUNDecade<-metaprop(Revisions,Hips,Names, sm="PLN",data=metaUNCement,byvar=Decade)
tiff("/Users/Talitha/Desktop/MetaHip/UncemDecade.tiff", width = 800, height = 750,compression = 'lzw')
meta::forest(metaUNDecade)
dev.off()
#Forest Plot for Pooled Prevalence subgrouped by %DeathLost
metaUNDeath<-metaprop(Revisions,Hips,Names, sm="PLN",data=metaUNCement,byvar=DeathLost)
tiff("/Users/Talitha/Desktop/MetaHip/UncemDeath.tiff", width = 800, height = 650,compression = 'lzw')
meta::forest(metaUNDeath)
dev.off()

###########################################################################################
#Figure 4: META POLL PREVALENCE CAUSES FOR REVISION OVER 15 YEARS Follow up 
###########################################################################################

#Forest Plot for Pooled Prevalence Revision Caused by Infection
metaInfect<-metaprop(Infection,Revisions,Names, sm="PLN",data=data)
tiff("/Users/Talitha/Desktop/MetaHip/Infection.tiff", width = 800, height = 1000,compression = 'lzw')
meta::forest(metaInfect)
dev.off()
#Forest Plot for Pooled Prevalence Revision Caused by Infection subgrouped by THR Type
metaInfect<-metaprop(Infection,Revisions,Names, sm="PLN",data=data, byvar=Type)
tiff("/Users/Talitha/Desktop/MetaHip/InfecType.tiff", width = 800, height = 1200,compression = 'lzw')
meta::forest(metaInfect)
dev.off()
#Forest Plot for Pooled Prevalence Revision Caused By Aseptic Loosening
metaAL<-metaprop(AsepticLoosening,Revisions,Names, sm="PLN",data=data)
tiff("/Users/Talitha/Desktop/MetaHip/AsepLoos.tiff", width = 800, height = 1000,compression = 'lzw')
meta::forest(metaAL)
dev.off()
#Forest Plot for Pooled Prevalence Revision Caused By Aseptic Loosening subgrouped by type
metaAL<-metaprop(AsepticLoosening,Revisions,Names, sm="PLN",data=data, byvar=Type)
tiff("/Users/Talitha/Desktop/MetaHip/AsepLoosType.tiff", width = 800, height = 1200,compression = 'lzw')
meta::forest(metaAL)
dev.off()
#Forest Plot for Pooled Prevalence Revision Caused by Fracture
metaFract<-metaprop(Fracture,Revisions,Names, sm="PLN",data=data)
tiff("/Users/Talitha/Desktop/MetaHip/Fracture.tiff", width = 800, height = 1000,compression = 'lzw')
meta::forest(metaFract)
dev.off()
#Forest Plot for Pooled Prevalence Revision Caused by Fracture subgrouped by type 
metaFract<-metaprop(Fracture,Revisions,Names, sm="PLN",data=data, byvar=Type)
tiff("/Users/Talitha/Desktop/MetaHip/FractureType.tiff", width = 800, height = 1200,compression = 'lzw')
meta::forest(metaFract)
dev.off()
#Forest Plot for Pooled Prevalence Revision Caused by Dislocation
metaDislo<-metaprop(Dislocation,Revisions,Names, sm="PLN",data=data)
tiff("/Users/Talitha/Desktop/MetaHip/Dislocation.tiff", width = 800, height = 1000,compression = 'lzw')
meta::forest(metaDislo)
dev.off()
#Forest Plot for Pooled Prevalence Revision Caused by Dislocation subgrouped by type
metaDislo<-metaprop(Dislocation,Revisions,Names, sm="PLN",data=data, byvar=Type)
tiff("/Users/Talitha/Desktop/MetaHip/DislocationType.tiff", width = 800, height = 1200,compression = 'lzw')
meta::forest(metaDislo)
dev.off()
#Forest Plot for Pooled Prevalence Revision Caused by POlywear/Osleolysis
metaPoly<-metaprop(OsteolysisWear,Revisions,Names, sm="PLN",data=data)
tiff("/Users/Talitha/Desktop/MetaHip/Poly.tiff", width = 800, height = 1000,compression = 'lzw')
meta::forest(metaPoly)
dev.off()
#Forest Plot for Pooled Prevalence Revision Caused by POlywear/Osleolysis subgrouped by type
metaPoly<-metaprop(OsteolysisWear,Revisions,Names, sm="PLN",data=data, byvar=Type)
tiff("/Users/Talitha/Desktop/MetaHip/PolyType.tiff", width = 800, height = 1200,compression = 'lzw')
meta::forest(metaPoly)
dev.off()

###########################################################################################
#Figure 5: META POLL PREVALENCE CAUSES FOR REVISION CEMENTED OVER 15 YEARS Follow up 
###########################################################################################

#Forest Plot for Pooled Prevalence Revision Caused by Infection
metaCEInfect<-metaprop(Infection,Revisions,Names, sm="PLN",data=metaCement)
tiff("/Users/Talitha/Desktop/Figure12.tiff", width = 800, height = 650,compression = 'lzw')
meta::forest(metaCEInfect)
dev.off()
#Forest Plot for Pooled Prevalence Revision Caused By Aseptic Loosening
metaCEAL<-metaprop(AsepticLoosening,Revisions,Names, sm="PLN",data=metaCement)
tiff("/Users/Talitha/Desktop/Figure13.tiff", width = 800, height = 700,compression = 'lzw')
meta::forest(metaCEAL)
dev.off()
#Forest Plot for Pooled Prevalence Revision Caused by Fracture
metaCEFract<-metaprop(Fracture,Revisions,Names, sm="PLN",data=metaCement)
tiff("/Users/Talitha/Desktop/Figure14.tiff", width = 800, height = 700,compression = 'lzw')
meta::forest(metaCEFract)
dev.off()
#Forest Plot for Pooled Prevalence Revision Caused by Dislocation
metaCEDislo<-metaprop(Dislocation,Revisions,Names, sm="PLN",data=metaCement)
tiff("/Users/Talitha/Desktop/Figure15.tiff", width = 800, height = 650,compression = 'lzw')
meta::forest(metaCEDislo)
dev.off()
#Forest Plot for Pooled Prevalence Revision Caused by POlywear/Osleolysis
metaCEPoly<-metaprop(Polywear/Osleolysis,Revisions,Names, sm="PLN",data=metaCement)
tiff("/Users/Talitha/Desktop/Figure16.tiff", width = 800, height = 650,compression = 'lzw')
meta::forest(metaCEPoly)
dev.off()

###########################################################################################
#Figure 6: META POLL PREVALENCE CAUSES FOR REVISION UNCEMENTED OVER 15 YEARS Follow up 
###########################################################################################

#Forest Plot for Pooled Prevalence Revision Caused by Infection
metaUNInfect<-metaprop(Infection,Revisions,Names, sm="PLN",data=metaUNCement)
tiff("/Users/Talitha/Desktop/Figure17.tiff", width = 800, height = 650,compression = 'lzw')
meta::forest(metaUNInfect)
dev.off()
#Forest Plot for Pooled Prevalence Revision Caused By Aseptic Loosening
metaUNAL<-metaprop(AsepticLoosening,Revisions,Names, sm="PLN",data=metaUNCement)
tiff("/Users/Talitha/Desktop/Figure18.tiff", width = 800, height = 700,compression = 'lzw')
meta::forest(metaUNAL)
dev.off()
#Forest Plot for Pooled Prevalence Revision Caused by Fracture
metaUNFract<-metaprop(Fracture,Revisions,Names, sm="PLN",data=metaUNCement)
tiff("/Users/Talitha/Desktop/Figure19.tiff", width = 800, height = 700,compression = 'lzw')
meta::forest(metaUNFract)
dev.off()
#Forest Plot for Pooled Prevalence Revision Caused by Dislocation
metaUNDislo<-metaprop(Dislocation,Revisions,Names, sm="PLN",data=metaUNCement)
tiff("/Users/Talitha/Desktop/Figure20.tiff", width = 800, height = 650,compression = 'lzw')
meta::forest(metaUNDislo)
dev.off()
#Forest Plot for Pooled Prevalence Revision Caused by POlywear/Osleolysis
metaUNPoly<-metaprop(Polywear/Osteolysis,Revisions,Names, sm="PLN",data=metaUNCement)
tiff("/Users/Talitha/Desktop/Figure21.tiff", width = 800, height = 650,compression = 'lzw')
meta::forest(metaUNPoly)
dev.off()