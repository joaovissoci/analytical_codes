
data<-read.csv("/Users/jnv4/Desktop/pub_baltimore.csv")

names(data)

summary(as.factor(data$Class))

data$outlet_type<-car::recode(data$Class,"0=NA;1='Off';2='Both';3='On';
	4='Both';8='Both';9='On'")

summary(data$outlet_t:
write.csv(data,"/Users/jnv4/Desktop/pub_baltimore_recoded.csv")Zach




