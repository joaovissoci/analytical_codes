
library(qgraph)

data<-read.csv("/Users/joaovissoci/OneDrive - Duke University/datasets/DGNN/surgical_capacity/network_consumable.csv")

data_naomit<-data

for(i in 1:dim(data)[2]){

	data_naomit[,i]<-car::recode(data[,i],"NA=0")
}


rownames(data_naomit[-1])<-data$X

vsize=c(10,7,2,5,2,6,2,2,4,2,2,
		10,5,7,5,6,2,2,2,2)

network_meta <- qgraph(data_naomit[-1],layout = "spring",
	labels=c("Needs","Supplies","Delays in report","Drugs",
		"Esquipament","Low budget","Basic supplies","Budget limitation",
		"Rationing","RRH limitations","Stock out",
		"Assets","Supplies","Program related","Collaboration",
		"Improvisation",
		"Adequate supplies",
		"Given camp",
		"Private facilities",
		"Reuse"),
	label.scale=FALSE,label.cex = c(2,1,0.8,1,0.8,1,0.8,0.8,1,0.8,0.8,
		2,1,1,1,1,0.8,0.8,0.8,0.8),
	vsize=vsize,
	color=c("grey","brown3","darksalmon","brown3","darksalmon",
		"brown3","darksalmon","darksalmon","brown3","darksalmon",
		"darksalmon","grey",rep("darkolivegreen3",4),rep("khaki1",4)),
	shape=c("circle","circle","square","circle","square",
		"circle","square","square","circle","square",
		"square","circle",rep("circle",4),rep("square",4)),
	borders = FALSE,posCol="grey")
	#greyscale=T)
	#,minimum=0.5,cut=10

##############

data<-read.csv("/Users/joaovissoci/OneDrive - Duke University/datasets/DGNN/surgical_capacity/network_equipament.csv")

data_naomit<-data

for(i in 1:dim(data)[2]){

	data_naomit[,i]<-car::recode(data[,i],"NA=0")
}


rownames(data_naomit[-1])<-data$X

vsize=c(10,7,4,5,6,4,rep(2,15),
		10,6,6,6,5,rep(2,7))

network_meta <- qgraph(data_naomit[-1],layout = "spring",
	labels=c("Needs",
			 "Personnel",
			 "Low budget",
			 "Spare parts",
			 "Lack of equipament",
			 "Infastructure",
			 "Biomedical Technician",
			 "Lack of spare parts",
			 "Drill",
			 "Maintenance",
			 "Missing parts",
			 "Monitors",
			 "Night and weekends",
			 "Over use or machinery",
			 "Power damage",
			 "Service contracts",
			 "Software updates",
			 "Staff training",
			 "Suction",
			 "Uniformed purchases",
			 "Ventilator",
			 "Assets",
			 "Adequate equipaments",
			 "Personnel",
			 "Rationing",
			 "Improvisation",
			 "Back up machines",
			 "Biomed Technicians",
			 "Donation",
			 "Hospital purchased",
			 "Personal equipament",
			 "Preventive maintenance",
			 "Non-protocol equipament"),
	label.scale=FALSE,label.cex = c(
		2,rep(1,5),rep(0.8,15),
		2,rep(1,4),rep(0.8,7)),
	vsize=vsize,
	color=c("grey",rep("brown3",5),rep("darksalmon",15),
			"grey",rep("darkolivegreen3",4),rep("khaki1",7)),
	shape=c(rep("circle",6),rep("square",15),
			rep("circle",5),rep("square",7)),
	borders = FALSE,posCol="grey")
	#greyscale=T)
	#,minimum=0.5,cut=10

###################

data<-read.csv("/Users/joaovissoci/OneDrive - Duke University/datasets/DGNN/surgical_capacity/network_infrastructure.csv")

data_naomit<-data

for(i in 1:dim(data)[2]){

	data_naomit[,i]<-car::recode(data[,i],"NA=0")
}


rownames(data_naomit[-1])<-data$X

vsize=c(10,8,5,4,4,4,rep(2,18),
		10,8,5,5,5,rep(2,10))

network_meta <- qgraph(data_naomit[-1],layout = "spring",
	labels=c("Needs",
			 "Structural need",
			 "Diagnostic capacity",
			 "Low budget infrastructure",
			 "Personnel",
			 "Low budget equipament",
			 "Ambulance",
			 "Blood bank",
			 "Budget limitation",
			 "Communication",
			 "CT scanner",
			 "ICU",
			 "Laboratory",
			 "Medical records",
			 "Night and weekends",
			 "Oxygen supply",
			 "Physical space",
			 "Procurement system",
			 "RRH limitation",
			 "Sterelization department",
			 "Transportation",
			 "Trauma department",
			 "Unstable electricity",
			 "Water sewage",
			 "Assets",
			 "Structural asset",
			 "Diagnostic capacity",
			 "Program related",
			 "Collaboration",
			 "Blood bank",
			 "CT scanner",
			 "Neurology space",
			 "New constructions",
			 "Oxygen supply",
			 "Physial space",
			 "Power protection",
			 "Private facilities",
			 "Sterelization department",
			 "Water sewage"),
	label.scale=FALSE,label.cex = c(
		2,rep(1,5),rep(0.8,18),
		2,rep(1,4),rep(0.8,10)),
	vsize=vsize,
	color=c("grey",rep("brown3",5),rep("darksalmon",18),
			"grey",rep("darkolivegreen3",4),rep("khaki1",9)),
	shape=c(rep("circle",6),rep("square",18),
			rep("circle",5),rep("square",10)),
	borders = FALSE,posCol="grey")
	#greyscale=T)
	#,minimum=0.5,cut=10

###################

data<-read.csv("/Users/joaovissoci/OneDrive - Duke University/datasets/DGNN/surgical_capacity/network_personnel.csv")

data_naomit<-data

for(i in 1:dim(data)[2]){

	data_naomit[,i]<-car::recode(data[,i],"NA=0")
}


rownames(data_naomit[-1])<-data$X

vsize=c(10,6,5,6,6,4,rep(2,9),
		10,7,6,5,rep(2,7))

network_meta <- qgraph(data_naomit[-1],layout = "spring",
	labels=c("Needs",
			 "Medical specialty",
			 "Equipament",
			 "More personnel",
			 "Personnel management",
			 "Low budget",
			 "Anesthesia",
			 "Biomedical technician",
			 "Medical doctors",
			 "Motivation",
			 "Neurosurgeons",
			 "Nurses",
			 "RRH limitations",
			 "Staff training",
			 "Surgeon",
			 "Assets",
			 "Equipament",
			 "Program related",
			 "Medical specialty",
			 "Biomedical technician",
			 "Camp training",
			 "Dedicated staff",
			 "Intern and residents",
			 "Maintenance department",
			 "Surgeon",
			 "University staff"),
	label.scale=FALSE,label.cex = c(
		2,rep(1,5),rep(0.8,9),
		2,rep(1,3),rep(0.8,7)),
	vsize=vsize,
	color=c("grey",rep("brown3",5),rep("darksalmon",9),
			"grey",rep("darkolivegreen3",3),rep("khaki1",7)),
	shape=c(rep("circle",6),rep("square",9),
			rep("circle",4),rep("square",7)),
	borders = FALSE,posCol="grey")
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
