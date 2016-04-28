
data1<-read.csv("/Users/joaovissoci/OneDrive - Duke University/datasets/DGNN/Ug_cost_ef/jihad_cost_basedata.csv")

colnames(data1)<-c("date","diagnosis","deleteme1","surgical_procedure",
	"deleteme2","age","medications","mediations_price","deleteme3",
	"investigations","investigations_price","deleteme4","bedside",
	"bedside_prices","deleteme5","deleteme6","deleteme7","deleteme8"
	,"deleteme9","deleteme10","deleteme11","deleteme12","deleteme13")

ref_lines<-which(data1$date=="Name")

teste<-data1[-c(ref_lines,ref_lines+1,ref_lines+2,ref_lines+3),]

which(teste$date=="Name")

#### CODING NAMES
names<-NULL

names[ref_lines[1]:(ref_lines[1+1]-4)]<-rep(as.character(
	data1$diagnosis[ref_lines[1]]),
	ref_lines[1+1]-4)

names[(ref_lines[2]-4):(ref_lines[2+1]-(1+(4*2)))]<-rep(as.character(
	data1$diagnosis[ref_lines[2]]),
	((ref_lines[2+1]-(1+(4*2)))-(ref_lines[2]-5)))

for(i in 3:1430){

names[(ref_lines[i]-(4*(i-1))):(ref_lines[i+1]-(1+(4*i)))]<-rep(as.character(
	data1$diagnosis[ref_lines[i]]),
	(ref_lines[i+1]-(1+(4*i))-(ref_lines[i]-(1+(4*(i-1))))))

}

names[(ref_lines[1431]-(4*(1431-1))):(length(data1$diagnosis)-((4*1431)))]<-rep(as.character(
	data1$diagnosis[ref_lines[1431]]),
	(length(data1$diagnosis)-((4*1431))-(ref_lines[1431]-(1+(4*(1431-1))))))

#### CODING NAMES
CIDdiagnosis<-NULL

CIDdiagnosis[ref_lines[1]:(ref_lines[1+1]-4)]<-rep(as.character(
	data1$diagnosis[ref_lines[1]+1]),
	ref_lines[1+1]-4)

CIDdiagnosis[(ref_lines[2]-4):(ref_lines[2+1]-(1+(4*2)))]<-rep(as.character(
	data1$diagnosis[ref_lines[2]+1]),
	((ref_lines[2+1]-(1+(4*2)))-(ref_lines[2]-5)))

for(i in 3:1430){

CIDdiagnosis[(ref_lines[i]-(4*(i-1))):(ref_lines[i+1]-(1+(4*i)))]<-rep(as.character(
	data1$diagnosis[ref_lines[i]+1]),
	(ref_lines[i+1]-(1+(4*i))-(ref_lines[i]-(1+(4*(i-1))))))

}

CIDdiagnosis[(ref_lines[1431]-(4*(1431-1))):(length(data1$diagnosis)-((4*1431)))]<-rep(as.character(
	data1$diagnosis[ref_lines[1431]+1]),
	(length(data1$diagnosis)-((4*1431))-(ref_lines[1431]-(1+(4*(1431-1))))))

#### SURGICAL PROCEDURE
#surgical_procedure<-NULL

#surgical_procedure[ref_lines[1]:(ref_lines[1+1]-4)]<-rep(as.character(
#	data1$surgical_procedure[ref_lines[1]]),
#	ref_lines[1+1]-4)

#surgical_procedure[(ref_lines[2]-4):(ref_lines[2+1]-(1+(4*2)))]<-rep(as.character(
#	data1$surgical_procedure[ref_lines[2]]),
#	((ref_lines[2+1]-(1+(4*2)))-(ref_lines[2]-5)))

#for(i in 3:1430){

#surgical_procedure[(ref_lines[i]-(4*(i-1))):(ref_lines[i+1]-(1+(4*i)))]<-rep(as.character(
#	data1$surgical_procedure[ref_lines[i]]),
#	(ref_lines[i+1]-(1+(4*i))-(ref_lines[i]-(1+(4*(i-1))))))

#}

#surgical_procedure[(ref_lines[1431]-(4*(1431-1))):(length(data1$surgical_procedure)-((4*1431)))]<-rep(as.character(
#	data1$surgical_procedure[ref_lines[1431]]),
#	(length(data1$surgical_procedure)-((4*1431))-(ref_lines[1431]-(1+(4*(1431-1))))))

#### Age
new_age<-NULL

new_age[ref_lines[1]:(ref_lines[1+1]-4)]<-rep(as.character(
	data1$age[ref_lines[1]]),
	ref_lines[1+1]-4)

new_age[(ref_lines[2]-4):(ref_lines[2+1]-(1+(4*2)))]<-rep(as.character(
	data1$age[ref_lines[2]]),
	((ref_lines[2+1]-(1+(4*2)))-(ref_lines[2]-5)))

for(i in 3:1430){

new_age[(ref_lines[i]-(4*(i-1))):(ref_lines[i+1]-(1+(4*i)))]<-rep(as.character(
	data1$age[ref_lines[i]]),
	(ref_lines[i+1]-(1+(4*i))-(ref_lines[i]-(1+(4*(i-1))))))

}

new_age[(ref_lines[1431]-(4*(1431-1))):(length(data1$age)-((4*1431)))]<-rep(as.character(
	data1$age[ref_lines[1431]]),
	(length(data1$age)-((4*1431))-(ref_lines[1431]-(1+(4*(1431-1))))))

#### Gender
gender<-NULL

gender[ref_lines[1]:(ref_lines[1+1]-4)]<-rep(as.character(
	data1$deleteme3[ref_lines[1]]),
	ref_lines[1+1]-4)

gender[(ref_lines[2]-4):(ref_lines[2+1]-(1+(4*2)))]<-rep(as.character(
	data1$deleteme3[ref_lines[2]]),
	((ref_lines[2+1]-(1+(4*2)))-(ref_lines[2]-5)))

for(i in 3:1430){

gender[(ref_lines[i]-(4*(i-1))):(ref_lines[i+1]-(1+(4*i)))]<-rep(as.character(
	data1$deleteme3[ref_lines[i]]),
	(ref_lines[i+1]-(1+(4*i))-(ref_lines[i]-(1+(4*(i-1))))))

}

gender[(ref_lines[1431]-(4*(1431-1))):(length(data1$deleteme3)-((4*1431)))]<-rep(as.character(
	data1$deleteme3[ref_lines[1431]]),
	(length(data1$deleteme3)-((4*1431))-(ref_lines[1431]-(1+(4*(1431-1))))))


new_data<-data.frame(names,gender,new_age,
	surgical_procedure=teste$surgical_procedure,CIDdiagnosis,
	date=teste$date,medications=teste$medications,
	investigations=teste$investigations,
	bedside_procedures=teste$bedside)

write.csv(new_data,"/Users/joaovissoci/OneDrive - Duke University/datasets/DGNN/Ug_cost_ef/jihad_cost_newdata.csv")



