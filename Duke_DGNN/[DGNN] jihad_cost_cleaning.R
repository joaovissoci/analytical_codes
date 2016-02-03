
data1<-read.csv("/Users/joaovissoci/Desktop/jihad_cost_basedata.csv")

colnames(data1)<-c("date","diagnosis","deleteme1","surgical_procedure",
	"deleteme2","age","medications","mediations_price","deleteme3",
	"investigations","investigations_price","deleteme4","bedside",
	"bedside_prices","deleteme5","deleteme6","deleteme7","deleteme8"
	,"deleteme9","deleteme10","deleteme11","deleteme12","deleteme13")

ref_lines<-which(data1$date=="Name")

teste<-data1[-c(ref_lines,ref_lines+1,ref_lines+2,ref_lines+3),]

which(teste$date=="Name")

bla<-NULL

bla[ref_lines[1]:(ref_lines[1+1]-4)]<-rep(as.character(
	data1$diagnosis[ref_lines[1]]),
	ref_lines[1+1]-4)

bla[(ref_lines[2]-4):(ref_lines[2+1]-(1+(4*2)))]<-rep(as.character(
	data1$diagnosis[ref_lines[2]]),
	((ref_lines[2+1]-(1+(4*2)))-(ref_lines[2]-5)))

for(i in 3:500){

bla[(ref_lines[i]-(4*(i-1))):(ref_lines[i+1]-(1+(4*i)))]<-rep(as.character(
	data1$diagnosis[ref_lines[i]]),
	(ref_lines[i+1]-(1+(4*i))-(ref_lines[i]-(1+(4*(i-1))))))

}






length(ref_lines[-c(1,2)])




bla[(ref_lines[4]-(4*(4-1))):(ref_lines[4+1]-(1+(4*4)))]<-rep(as.character(
	data1$diagnosis[ref_lines[4]]),
	(ref_lines[4+1]-(1+(4*4))-(ref_lines[4]-(1+(4*(4-1))))))




