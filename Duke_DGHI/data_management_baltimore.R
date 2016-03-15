#############################################################################
#BASIC R STATISTICS TEMPLATE
#############################################################################
#
#
#
#
#
#############################################################################
#SETTING ENVIRONMENT
#############################################################################
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
lapply(c("Hmisc","car","psych","nortest","ggplot2","pastecs","repmis",
	"mvnormtest","polycor"), 
library, character.only=T)
#####################################################################################
#IMPORTING DATA
#####################################################################################
#LOADING DATA FROM A .CSV FILE

#loading data
crash_data_09<-read.csv("/home/joao/Dropbox/datasets/DGHI/baltimore_gis/crashdata2009.csv",sep=",")
point_data_09<-read.csv("/home/joao/Dropbox/datasets/DGHI/baltimore_gis/geocoded2009.csv",sep=",")
crash_data_10<-read.csv("/home/joao/Dropbox/datasets/DGHI/baltimore_gis/crashdata2010.csv",sep=",")
point_data_10<-read.csv("/home/joao/Dropbox/datasets/DGHI/baltimore_gis/geocoded2010.csv",sep=",")
crash_data_11<-read.csv("/home/joao/Dropbox/datasets/DGHI/baltimore_gis/crashdata2011.csv",sep=",")
point_data_11<-read.csv("/home/joao/Dropbox/datasets/DGHI/baltimore_gis/geocoded2011.csv",sep=",")
crash_data_12<-read.csv("/home/joao/Dropbox/datasets/DGHI/baltimore_gis/crashdata2012.csv",sep=",")
point_data_12<-read.csv("/home/joao/Dropbox/datasets/DGHI/baltimore_gis/geocoded2012.csv",sep=",")
crash_data_13<-read.csv("/home/joao/Dropbox/datasets/DGHI/baltimore_gis/crashdata2013.csv",sep=",")
point_data_13<-read.csv("/home/joao/Dropbox/datasets/DGHI/baltimore_gis/geocoded2013.csv",sep=",")

#############################################################################
#DATA MANAGEMENT
#############################################################################
# calculating Vunerable Road Users
crash_data_09$vru<-with(crash_data_09,rowSums(data.frame(PEDS,BIKES,MC))) 
crash_data_10$vru<-with(crash_data_10,rowSums(data.frame(PEDS,BIKES,MC))) 
crash_data_11$vru<-with(crash_data_11,rowSums(data.frame(PEDS,BIKES,MC))) 
crash_data_12$vru<-with(crash_data_12,rowSums(data.frame(PEDS,BIKES,MC))) 
crash_data_13$vru<-with(crash_data_13,rowSums(data.frame(PEDS,BIKES,MC))) 

# calculating Distracted driving
crash_data_09$distracted_driving<-with(crash_data_09,rowSums(
	data.frame(SLEEPY,INATT,AGG))) 
crash_data_10$distracted_driving<-with(crash_data_10,rowSums(
	data.frame(SLEEPY,INATT,AGG))) 
crash_data_11$distracted_driving<-with(crash_data_11,rowSums(
	data.frame(SLEEPY,INATT,AGG))) 
crash_data_12$distracted_driving<-with(crash_data_12,rowSums(
	data.frame(SLEEPY,INATT,AGG))) 
crash_data_13$distracted_driving<-with(crash_data_13,rowSums(
	data.frame(SLEEPY,INATT,AGG))) 

# calculating extreme extreme_age
crash_data_09$extreme_age<-with(crash_data_09,rowSums(
	data.frame(YOUNGER,OLDER)))
crash_data_10$extreme_age<-with(crash_data_10,rowSums(
	data.frame(YOUNGER,OLDER))) 
crash_data_11$extreme_age<-with(crash_data_11,rowSums(
	data.frame(YOUNGER,OLDER))) 
crash_data_12$extreme_age<-with(crash_data_12,rowSums(
	data.frame(YOUNGER,OLDER))) 
crash_data_13$extreme_age<-with(crash_data_13,rowSums(
	data.frame(YOUNGER,OLDER))) 

# calculating extreme extreme_age
crash_data_09$extreme_age<-with(crash_data_09,rowSums(
	data.frame(YOUNGER,OLDER)))
crash_data_10$extreme_age<-with(crash_data_10,rowSums(
	data.frame(YOUNGER,OLDER))) 
crash_data_11$extreme_age<-with(crash_data_11,rowSums(
	data.frame(YOUNGER,OLDER))) 
crash_data_12$extreme_age<-with(crash_data_12,rowSums(
	data.frame(YOUNGER,OLDER))) 
crash_data_13$extreme_age<-with(crash_data_13,rowSums(
	data.frame(YOUNGER,OLDER))) 

#############################################################################
#MERGING BOTH DATASETS
#############################################################################

final_data_09<-merge(crash_data_09,point_data_09,by="REPORT_NO",all=TRUE)
final_data_10<-merge(crash_data_10,point_data_10,by="REPORT_NO",all=TRUE)
final_data_11<-merge(crash_data_11,point_data_11,by="REPORT_NO",all=TRUE)
final_data_12<-merge(crash_data_12,point_data_12,by="REPORT_NO",all=TRUE)
final_data_13<-merge(crash_data_13,point_data_13,by="REPORT_NO",all=TRUE)

##############################################################################
#EXCLUDING ADDRESSES NOT USED
##############################################################################

final_data_09_cleaned<-subset(final_data_09,
	final_data_09$Match_addr!="6100 QUARANTINE RD" |
	final_data_09$Match_addr!="6200 QUARANTINE RD" |
	final_data_09$Match_addr!="2298 S CLINTON ST")

final_data_10_cleaned<-subset(final_data_10,
	final_data_10$Match_addr!="6100 QUARANTINE RD" |
	final_data_10$Match_addr!="6200 QUARANTINE RD" |
	final_data_10$Match_addr!="2298 S CLINTON ST")

final_data_11_cleaned<-subset(final_data_11,
	final_data_11$Match_addr!="6100 QUARANTINE RD" |
	final_data_11$Match_addr!="6200 QUARANTINE RD" |
	final_data_11$Match_addr!="2298 S CLINTON ST")

final_data_12_cleaned<-subset(final_data_12,
	final_data_12$Match_addr!="6100 QUARANTINE RD" |
	final_data_12$Match_addr!="6200 QUARANTINE RD" |
	final_data_12$Match_addr!="2298 S CLINTON ST")

final_data_13_cleaned<-subset(final_data_13,
	final_data_13$Match_addr!="6100 QUARANTINE RD" |
	final_data_13$Match_addr!="6200 QUARANTINE RD" |
	final_data_13$Match_addr!="2298 S CLINTON ST")

##############################################################################
#SEVERITY
##############################################################################

final_data_09_severe<-subset(final_data_09,
	final_data_09$CRASH_SEVE==4 |
	final_data_09$CRASH_SEVE==5)

final_data_10_severe<-subset(final_data_10,
	final_data_10$CRASH_SEVE==4 |
	final_data_10$CRASH_SEVE==5)

final_data_11_severe<-subset(final_data_11,
	final_data_11$CRASH_SEVE==4 |
	final_data_11$CRASH_SEVE==5)

final_data_12_severe<-subset(final_data_12,
	final_data_12$CRASH_SEVE==4 |
	final_data_12$CRASH_SEVE==5)

final_data_13_severe<-subset(final_data_13,
	final_data_13$CRASH_SEVE==4 |
	final_data_13$CRASH_SEVE==5)

##############################################################################
#Creating .csv files
##############################################################################

write.csv(final_data_09_cleaned,"/home/joao/Dropbox/datasets/DGHI/baltimore_gis/baltimore_2009_points.csv")

write.csv(final_data_10_cleaned,"/home/joao/Dropbox/datasets/DGHI/baltimore_gis/baltimore_2010_points.csv")

write.csv(final_data_11_cleaned,"/home/joao/Dropbox/datasets/DGHI/baltimore_gis/baltimore_2011_points.csv")

write.csv(final_data_12_cleaned,"/home/joao/Dropbox/datasets/DGHI/baltimore_gis/baltimore_2012_points.csv")

write.csv(final_data_13_cleaned,"/home/joao/Dropbox/datasets/DGHI/baltimore_gis/baltimore_2013_points.csv")

#Severity
write.csv(final_data_09_severe,"/home/joao/Dropbox/datasets/DGHI/baltimore_gis/baltimore_2009_points_severe.csv")

write.csv(final_data_10_severe,"/home/joao/Dropbox/datasets/DGHI/baltimore_gis/baltimore_2010_points_severe.csv")

write.csv(final_data_11_severe,"/home/joao/Dropbox/datasets/DGHI/baltimore_gis/baltimore_2011_points_severe.csv")

write.csv(final_data_12_severe,"/home/joao/Dropbox/datasets/DGHI/baltimore_gis/baltimore_2012_points_severe.csv")

write.csv(final_data_13_severe,"/home/joao/Dropbox/datasets/DGHI/baltimore_gis/baltimore_2013_points_severe.csv")



