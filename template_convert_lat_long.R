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