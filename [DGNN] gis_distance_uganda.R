data_to<-read.csv("/home/joao/Desktop/BANCO2_centroids_hemodinamicas.csv")
data_from<-read.csv("/home/joao/Desktop/BANCO1_centroids_municipios.csv")

to<-with(data_to,data.frame(XCNTRD,YCNTRD))
from<-with(data_from,data.frame(longitude,latitude))

#to<-na.omit(to)
#from<-na.omit(from)

tryout_100_hemo1<-mapdist_loop(from[1:1000,],to[1:1000,])

tryout_2<-map_dist_aggregate(from,to,list(tryout_100_hemo1,tryout_100_hemo2,tryout_100_hemo3))

write.csv(tryout_2,"/home/joao/Desktop/distance.csv")

2387