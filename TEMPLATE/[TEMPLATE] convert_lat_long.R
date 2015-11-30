
#Instruções em http://goo.gl/Ofa7gQ
#data <- repmis::source_DropboxData("SOSAS_gis.csv","1qdc9i5y5xivsmq",sep = ",",header = TRUE)

data<-read.csv("/home/joao/Dropbox/datasets/DGNN/SOSAS/SOSAS_gis/SOSAS_gis.csv",header=TRUE)
data

convert_latalong<-function(data$Surg_GPS_Lat2){
  data$Surg_GPS_Lat2<-car::recode(data$Surg_GPS_Lat2,"''='NA.NA.NA.NA.NA'")
  z <- sapply((strsplit(as.character(data$Surg_GPS_Lat2), "[°\\.]")), as.character)
  z<-t(z)
  z<-as.data.frame(z)
  z[,1:4]<-sapply(z[,1:4], as.character)
  z[,1:4]<-sapply(z[,1:4], as.numeric)
  z[,5]<-car::recode(z[,5],"' N'='N';'N '='N';' S'='S';'S '='S'")
  z[,6]<-as.numeric(as.character(paste(z[,3],z[,4],sep=".")))
  lat<-z[,1] + z[,2]/60 + z[,6]/3600

  for (i in 1:length(z[,1])) {

  	if (z[,5][i]=="S"){
  		lat[i]<-lat[i]*-1
  	}

  	if (z[,5][i]=="W"){
  		lat[i]<-lat[i]*-1
  	}
  }
    return(lat)	
  }

ead_lat<-convert_latalong(data$EA_GPS_Lat)
ead_long<-convert_latalong(data$EA_GPS_Long)
surg_lat<-convert_latalong(data$Surg_GPS_Lat2)
surg_long<-convert_latalong(data$Surg_GPS_Long2)

for (i in 1:length(surg_lat)) {
	if(is.na(surg_lat[i])==TRUE){
		surg_lat[i]<-data$Surg_GPS_Lat[i]
	}
}

for (i in 1:length(surg_long)) {
	if(is.na(surg_long[i])==TRUE){
		surg_long[i]<-data$Surg_GPS_Long[i]
	}
}

final_data<-with(data,data.frame(EA_Number,EA_Name,Sub.Region,District,SurgCent_Name,ead_lat,ead_long,surg_lat,surg_long))

write.csv(final_data,"/home/joao/Desktop/gis_ugana.write.csv")


data<-read.csv("/home/joao/Desktop/hubdistance_uganda (2).csv",header=TRUE)
