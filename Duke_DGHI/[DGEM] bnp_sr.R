


data_table2<-c(-4,16.7,13,10.6,36.5,0,13.2,26,22,
	    0,14.1,10,4.2,7.5,6,19.65,14,6,
	    -4,2.6,3,6.4,29,-6,-6.45,12,16)

group_table2<-c(rep("Isquemic",9),rep("Normal",9),rep("Delta",9))

name_table2<-rep("table2",27)

data_table3<-c(32,48.1,4.1,-2,1.67,
			   17,24,0.2,-0.6,2.84,
			   15,24.1,3.9,-1.4,-1.17)

group_table3<-c(rep("Isquemic",5),rep("Normal",5),rep("Delta",5))

name_table3<-rep("table3",15)


data_table4<-c(14,30,30,-15.59,14.5,3,38,
	           9,14,15,22,4,16,15,
	           5,16,15,-37.59,10.5,-13,23)

group_table4<-c(rep("Isquemic",7),rep("Normal",7),rep("Delta",7))

name_table4<-rep("table4",21)


data_table5<-c(1.2,26.4,5.4,9.03,
	           4,10.3,-0.7,-1.61,
	           -2.8,16.1,-0.7,-1.61)

group_table5<-c(rep("Isquemic",4),rep("Normal",4),rep("Delta",4))

name_table5<-rep("table5",12)


data<-c(data_table2,data_table3,data_table4,data_table5)
name<-c(name_table2,name_table3,name_table4,name_table5)
group<-c(group_table2,group_table3,group_table4,group_table5)

plot_data<-data.frame(data,name,group)

ggbox2   <- ggplot(plot_data,aes(x=group, 
	y=data))
ggbox2   <- ggbox2 + geom_boxplot(fill="grey")#, outlier.colour = "blue", outlier.shape = 16, outlier.size = 4)
#ggbox2   <- ggbox2 + stat_summary(fun.y=mean, geom="point", shape=21, size= 4, color= "red")
ggbox2   <- ggbox2 + coord_flip()
ggbox2   <- ggbox2 + facet_grid(name ~ .,space="free")
ggbox2   <- ggbox2 + labs(x="", y="Median ")
ggbox2   <- ggbox2 + theme(plot.title=element_text(face="bold"), 
	axis.text=element_text(color="black"))
ggbox2   <- ggbox2 + geom_jitter(aes(colour=
	plot_data$data),
	size=log(plot_data$data))
#ggbox2   <- ggbox2 + geom_text(aes(x=country, y=proportion_death, label=Author),size=4, colour="red",angle = 45)
# ggbox2   <- ggbox2 + theme()#plot.margin=unit(c(0.05,0.05,0.05,0),
#  	units="npc")
ggbox2 <- ggbox2 + theme_bw()
ggbox2 <- ggbox2 + theme(legend.position="none")
ggbox2
