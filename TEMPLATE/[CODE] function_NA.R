NAto0<-function(x){
	car::recode(x,"NA=0")
	}

	data_full_NAto0<-lapply(data_full,NAto0)
