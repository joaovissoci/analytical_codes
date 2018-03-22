

data<-read.csv("/Users/joaovissoci/Downloads/venomous1015.csv")

names(data)

#Serum data - data$CON_SOROTE
table(data$CON_SOROTE)

data$CON_SOROTE_recoded<-car::recode(data$CON_SOROTE,"
					1='yes';
					2='no';
					9=NA")

table(data$CON_SOROTE_recoded)

#Time to treatment - data$ANT_TEMPO_
table(data$ANT_TEMPO_)

data$ANT_TEMPO__recoded<-car::recode(data$ANT_TEMPO_,"
					1:3='no';
					4:8='yes';
					9=NA")

table(data$ANT_TEMPO__recoded)

#count values by year and by municipality










data2<-with(data,data.frame(ANT_TEMPO__recoded,CON_SOROTE_recoded,ID_MUNICIP))

data2<-na.omit(data2)

data2$new<-NULL

for (i in 1:nrow(data2))
{
 if (data2$CON_SOROTE_recoded[i] == 'yes' &
	 data2$ANT_TEMPO__recoded[i] == 'yes')
 	{ 
       
       data2$new[i] <- 'yes'

 	} else {
	   
	   data2$new[i] <- 'no'

 	}
} # for (i in 1:nrow(dataframe)

with(data2,table(ID_MUNICIP,new))

blabla<-with(data2,prop.table(table(ID_MUNICIP,new),1))*100

write.csv(blabla,"/Users/joaovissoci/Desktop/deleteme_snakebite.csv")


