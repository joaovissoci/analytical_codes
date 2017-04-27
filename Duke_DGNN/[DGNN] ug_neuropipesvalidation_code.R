
library(irr)
library(psych)

icc_raw_data<-read.csv("/Users/jnv4/Box Sync/Home Folder jnv4/Data/DGNN/surgical_capacity/ug_iccrawdataneuropipes_data.csv")

AR1<-c(29,13,48,26,13)
AR2<-c(21,11,47,23,9)
AR3<-c(31,12,48,27,9)

#OVERALL ASSESSMENT
ICC(with(data_raters,data.frame(AR1,AR2,AR3)))

#Personnel
icc_personnel<-subset(icc_raw_data,
	icc_raw_data$domain=="Personnel")
icc_personnel<-icc_personnel[,-c(1,5)]

ICC(icc_personnel)

#Infra-structure
icc_personnel<-subset(icc_raw_data,
	icc_raw_data$domain=="Infrastructure")
icc_personnel<-icc_personnel[,-c(1,5)]

agree(na.omit(icc_personnel)) #% of Agreement
kappam.fleiss(icc_personnel)
cohen.kappa(icc_personnel)

#Procedures
icc_Procedures<-subset(icc_raw_data,
	icc_raw_data$domain=="Procedures")
icc_Procedures<-icc_Procedures[,-c(1,5)]

agree(na.omit(icc_Procedures)) #% of Agreement
kappam.fleiss(icc_Procedures)
cohen.kappa(icc_Procedures)

#Equipment
icc_Equipment<-subset(icc_raw_data,
	icc_raw_data$domain=="Equipment")
icc_Equipment<-icc_Equipment[,-c(1,5)]

agree(na.omit(icc_Equipment)) #% of Agreement
kappam.fleiss(icc_Equipment)
cohen.kappa(icc_Equipment)

#Supplies
icc_Supplies<-subset(icc_raw_data,
	icc_raw_data$domain=="Supplies")
icc_Supplies<-icc_Supplies[,-c(1,5)]

agree(na.omit(icc_Supplies)) #% of Agreement
kappam.fleiss(icc_Supplies)
cohen.kappa(icc_Supplies)