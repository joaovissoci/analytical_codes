#########################################################
#TEMPLATE - MAPDIST
#########################################################
#Function in the ggmap package
library(ggmap)

#organizing data set
#it works with addresses, city names and numeric lat and long indicator
# wherw x = a data.frame with lat and long in columns
from <- c('houston', 'houston', 'dallas')
to <- c('waco, texas', 'san antonio', 'houston')
wh <- geocode(from)
wm <- geocode(to)

#calculation line by line
test<-mapdist(c(wh[1,1],wh[1,2]), c(wm[1,1],wm[1,2]), mode = 'walking')

#loop to iterate in a data.frame
mapdist_loop<-function(from,to) {

library(ggmap)

#creating object list
final_list<-list()

#first loop to set a loop on the "to" reference
for(a in 1:length(to[,1])){

#creating object to receive information

dis_map2<-NULL

#Creating second loop = extract difference for each reference point
	for (i in 1:length(from[,1])) {
		dis_map<-NULL
		dis_map<-mapdist(c(from[i,1],from[i,2]), c(to[a,1],to[a,2]), mode = 'driving')

			if(is.na(dis_map$km==TRUE)){
			dis_map$m<-NA
			dis_map$km<-NA
			dis_map$miles<-NA
			dis_map$seconds<-NA
			dis_map$minutes<-NA
			dis_map$hours<-NA
			}
		
		dis_map$id<-i
		dis_map2<-rbind(dis_map2,dis_map)
		#dis_list<-list(dis_map2)
		#print(dis_map)
}
#print(dis_map2)
final_list[[a]]<-dis_map2
#print(final_list)
}
# end loop to calculate distances and return a dataset with distances for each "to" point
#return(final_list)

#extrct minimum distances per "to" location
distance_data_frame<-rbind.fill(final_list)
return(distance_data_frame)
#check how may queries are still available
x<-distQueryCheck()
print(x)
}

#print(distance_data_frame)

#x=a list for data.frames resulting from map_dist_loop
map_dist_aggregate<-function(from,to,x){

aggregate_data<-rbind.fill(x)

#test<-data.frame()
test1=NULL

for (b in 1:length(from[,1])) {
	test=NULL
	subset_data<-subset(aggregate_data,aggregate_data$id==b)
	#print(subset_data)
	if(is.na(subset_data$km[1]==TRUE)){
			subset_data2<-subset_data[1,]
			}
	else {
	subset_data2<-subset(subset_data,subset_data$km==min(na.omit(subset_data$km)))
	}
	#print(subset_data2)
	test<-cbind(from[b,],subset_data2,geocode(subset_data2$to))
	#print(subset_data2)
	test1<-rbind(test1,test)
	#print(subset_data2)
#print(test1)
}
return(test1)
}
#creating dataset
what2<-mapdist_loop(wh,wm)
what2

#########################################################
#END OF TEMPLATE
#########################################################