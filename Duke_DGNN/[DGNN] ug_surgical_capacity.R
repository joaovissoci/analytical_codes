
library(qgraph)

data<-read.csv("/Users/joaovissoci/OneDrive - Duke University/datasets/DGNN/surgical_capacity/needs_consumable_network1.csv")

rownames(data[-1])<-data$X

vsize=c(10,(c(0.21,0.04,0.21,0.20,0.13,0.001,0.001,0.001,0.001,0.20)+5),
		10,(c(0.65,0.27,0.08,0.001,0.001)+5))

vsize=c(10,5,2,5,5,3,2,2,2,2,5,
		10,8,5,2,2,2)

network_meta <- qgraph(data[-1],layout = "spring",
	labels=c("Needs","Supplies","Delays in report","Drugs",
		"Esquipament","Low budget","Basic supplies","Budget limitation",
		"Personal supplies","RRH limitations","Stock out",
		"Assets","Supplies","Given camp","Private facilities",
		"Reuse","Adequate supplies"),
	label.scale=FALSE,label.cex = c(2,1,1,1,1,1,0.8,0.8,0.8,0.8,1,
		2,1,1,1,0.8,0.8),
	vsize=vsize,
	color=c("grey",rep("brown3",5),rep("darksalmon",4),"brown3",
		"grey",rep("darkolivegreen3",3),rep("khaki1",2)),
	shape=c(rep("circle",6),rep("square",4),"circle",
	rep("circle",4),rep("square",2)),borders = FALSE)
	#greyscale=T)
	#,minimum=0.5,cut=10














names(data)

data$Gulu<-car::recode(data$Gulu,"NA=0")
data$Mbarara<-car::recode(data$Mbarara,"NA=0")
data$Mulago<-car::recode(data$Mulago,"NA=0")
data

gulu_data<-with(data,prop.table(table(second_order_themes,Gulu),2))
Mbarara_data<-with(data,prop.table(table(second_order_themes,Mbarara),2))
Mulago_data<-with(data,prop.table(table(second_order_themes,Mulago),2))

network_data1<-data.frame(gulu=gulu_data[,2],
						 mbarara=Mbarara_data[,2],
						 mulago=Mulago_data[,2])

network_data<-network_data1

for(i in 1:24){ 

	network_data[,(i+3)]<-rep(0,24)

}

colnames(network_data)<-c("gulu","mbarara","mulago",
	rownames(network_data))

network_data2<-rbind(data.frame(gulu=rep(0,3),mbarara=rep(0,3),
	mulago=rep(0,3),t(network_data1)),network_data)

qgraph(network_data2,layout="circle") 

names<-c(c("Intolerance","Incompletion","Refusal","Tinnitus",
	"Second Diagnostic",
	"Voice Therapy","Pain",
	"Paralysis","Deceased",
	"Satisfied Result","Cancer",
	"Unknown","Technical Errors",
	"Discontinued","Dissatisfied"),rownames(study_data))

size_edges<-c(effect_sizes[,1]*10,intensity_sizes*3)
color<-c(c("red","steelblue","steelblue","lightgreen","steelblue","steelblue","lightgreen","lightgreen","red","red","red","steelblue","steelblue","red","red"),rep("white",16))
shape<-c(rep("circle",15),rep("square",16)) 
label.cex<- c(rep(2.0,15),rep(1.2,16))
groups<-c("Methodological Issues","Clinical Issues","Side-Effects")

tiff("/Users/rpietro/Desktop/voice_sr_network.tiff", width = 1300, height = 1300,compression = 'lzw')
network_meta <- qgraph(network_data,layout = "spring",minimum=0.5,cut=10,labels=names,label.scale=FALSE,label.cex = label.cex,vsize=size_edges,color=color,shape=shape,greyscale=T)
legend(0.8,-0.8, bty="n",c("Methodological Issues","Clinical Issues","Side-Effects"),cex=1.2,fill=c("steelblue","red","lightgreen"))
dev.off()


#rownames(network_data2)<-c("gulu","mbarara","mulago",
#	rownames(network_data))
