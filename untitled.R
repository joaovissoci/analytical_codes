data<-read.csv("/home/joao/Desktop/head_injury.csv",header=TRUE)


data_melt<-melt(data,id=("EA"))

data_cast1<-dcast(data_melt,EA~variable,length)
data_cast1$id<-c(1:105)

data_cast<-dcast(subset(data_melt,data_melt$value=="yes"),EA~variable,length)

write.csv(data_cast,"/home/joao/Desktop/deleteme.csv")