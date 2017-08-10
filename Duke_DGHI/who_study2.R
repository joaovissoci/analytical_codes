#All packages must be installes with install.packages() function
lapply(c("sem","ggplot2", "psych", "irr", "nortest", "moments",
	"GPArotation","nFactors","boot","psy", "car","vcd", "gridExtra",
	"mi","VIM","epicalc","gdata","sqldf","reshape2","mclust",
	"foreign","survival","memisc","foreign","mice","MissMech",
	"repmis","sqldf","VIM","survival",
	"sas7bdat"), 
library, character.only=T)

###################################################
#IMPORTING DATA AND RECODING
###################################################
#Instructions here http://goo.gl/Ofa7gQ
#data <- repmis::source_DropboxData("alcohol_moshi.csv","wkcecvvrvert5h9",sep = ",",header = TRUE)
#data<-
#data<-read.sas7bdat("C:\\Users\\Joao\\Desktop\\tanzclean.sas7bdat")
#data<-read.sas7bdat("/Users/rpietro/Dropbox/datasets/Africa_DGHI/tanzclean.sas7bdat")
data<-read.sas7bdat("/Users/joaovissoci/OneDrive - Duke University/datasets/Global EM/Africa/tanzclean.sas7bdat")
data<-as.data.frame(data)
###################################################
#DATA MANAGEMENT
###################################################
#### CREATING ETHANOL AMOUT / ml data

c9999toNA<-function(x){
	car::recode(x,"9999=NA")
}
data<-lapply(data,c9999toNA)

#Recoding QB01a from hours to minutes
#data$QB01a<-data$QB01a*60

#Creating QB01 (summing up QB01A and QB01b)
#data$QB01<-NULL
#data$QB01<-with(data,rowSums(data.frame(QB01a,QB01b)))

#Creating GCS score
#gcs<-with(data,rowSums(data.frame(QL02,QL02a,QL02b)))


#Alcohol Dosage - Time 1
alcohol_amount_1_rowB<-with(data,(as.numeric(QF07B1B)*as.numeric(QF07B2))*as.numeric(QF07B3))
alcohol_amount_1_rowC<-with(data,(as.numeric(QF07C1B)*as.numeric(QF07C2))*as.numeric(QF07C3))
alcohol_amount_1_rowD<-with(data,(as.numeric(QF07D1B)*as.numeric(QF07D2))*as.numeric(QF07D3))
alcohol_amount_1_rowE<-with(data,(as.numeric(QF07E1B)*as.numeric(QF07E2))*as.numeric(QF07E3))
alcohol_amount_1_rowF<-with(data,(as.numeric(QF07F1B)*as.numeric(QF07F2))*as.numeric(QF07F3))
alcohol_amount_1_rowG<-with(data,(as.numeric(QF07G1B)*as.numeric(QF07G2))*as.numeric(QF07G3))
alcohol_amount_1_rowH<-with(data,(as.numeric(QF07H1B)*as.numeric(QF07H2))*as.numeric(QF07H3))
alcohol_amount_1_rowL1<-with(data,(as.numeric(QF07L11B)*as.numeric(QF07L12))*as.numeric(QF07L13))
alcohol_amount_1_rowL2<-with(data,(as.numeric(QF07L21B)*as.numeric(QF07L22))*as.numeric(QF07L23))
alcohol_amount_1_rowL3<-with(data,(as.numeric(QF07L31B)*as.numeric(QF07L32))*as.numeric(QF07L33))
alcohol_amount_1<-data.frame(alcohol_amount_1_rowB,alcohol_amount_1_rowC,alcohol_amount_1_rowD,alcohol_amount_1_rowE,alcohol_amount_1_rowF,alcohol_amount_1_rowG,alcohol_amount_1_rowH,alcohol_amount_1_rowL1,alcohol_amount_1_rowL2,alcohol_amount_1_rowL3)

NaNtoNA<-function(x){
	car::recode(x,"NaN=0")
}

alcohol_amount_1<-lapply(alcohol_amount_1,NaNtoNA)
alcohol_amount_1<-rowSums(as.data.frame(alcohol_amount_1))/100

#Alcohol Dosage - Time 2
alcohol_amount_2_rowB<-with(data,(as.numeric(QNH05B1B)*as.numeric(QNH05B2))*as.numeric(QNH05B3))
alcohol_amount_2_rowC<-with(data,(as.numeric(QNH05C1B)*as.numeric(QNH05C2))*as.numeric(QNH05C3))
alcohol_amount_2_rowD<-with(data,(as.numeric(QNH05D1B)*as.numeric(QNH05D2))*as.numeric(QNH05D3))
alcohol_amount_2_rowE<-with(data,(as.numeric(QNH05E1B)*as.numeric(QNH05E2))*as.numeric(QNH05E3))
alcohol_amount_2_rowF<-with(data,(as.numeric(QNH05F1B)*as.numeric(QNH05F2))*as.numeric(QNH05F3))
alcohol_amount_2_rowG<-with(data,(as.numeric(QNH05G1B)*as.numeric(QNH05G2))*as.numeric(QNH05G3))
alcohol_amount_2_rowH<-with(data,(as.numeric(QNH05H1B)*as.numeric(QNH05H2))*as.numeric(QNH05H3))
alcohol_amount_2_rowL1<-with(data,(as.numeric(QNH05L11B)*as.numeric(QNH05L12))*as.numeric(QNH05L13))
alcohol_amount_2_rowL2<-with(data,(as.numeric(QNH05L21B)*as.numeric(QNH05L22))*as.numeric(QNH05L23))
alcohol_amount_2_rowL3<-with(data,(as.numeric(QNH05L31B)*as.numeric(QNH05L32))*as.numeric(QNH05L33))
alcohol_amount_2<-data.frame(alcohol_amount_2_rowB,alcohol_amount_2_rowC,alcohol_amount_2_rowD,alcohol_amount_2_rowE,alcohol_amount_2_rowF,alcohol_amount_2_rowG,alcohol_amount_2_rowH,alcohol_amount_2_rowL1,alcohol_amount_2_rowL2,alcohol_amount_2_rowL3)

alcohol_amount_2<-lapply(alcohol_amount_2,NaNtoNA)
alcohol_amount_2<-rowSums(as.data.frame(alcohol_amount_2))/100

#Alcohol Dosage - Time 3

alcohol_amount_3_rowB<-with(data,(as.numeric(QG02B1B)*as.numeric(QG02B2))*as.numeric(QG02B3))
alcohol_amount_3_rowC<-with(data,(as.numeric(QG02C1B)*as.numeric(QG02C2))*as.numeric(QG02C3))
alcohol_amount_3_rowD<-with(data,(as.numeric(QG02D1B)*as.numeric(QG02D2))*as.numeric(QG02D3))
alcohol_amount_3_rowE<-with(data,(as.numeric(QG02E1B)*as.numeric(QG02E2))*as.numeric(QG02E3))
alcohol_amount_3_rowF<-with(data,(as.numeric(QG02F1B)*as.numeric(QG02F2))*as.numeric(QG02F3))
alcohol_amount_3_rowG<-with(data,(as.numeric(QG02G1B)*as.numeric(QG02G2))*as.numeric(QG02G3))
alcohol_amount_3_rowH<-with(data,(as.numeric(QG02H1B)*as.numeric(QG02H2))*as.numeric(QG02H3))
alcohol_amount_3_rowL1<-with(data,(as.numeric(QG02L11B)*as.numeric(QG02L12))*as.numeric(QG02L13))
alcohol_amount_3_rowL2<-with(data,(as.numeric(QG02L21B)*as.numeric(QG02L22))*as.numeric(QG02L23))
alcohol_amount_3_rowL3<-with(data,(as.numeric(QG02L31B)*as.numeric(QG02L32))*as.numeric(QG02L33))

alcohol_amount_3<-data.frame(alcohol_amount_3_rowB,alcohol_amount_3_rowC,alcohol_amount_3_rowD,alcohol_amount_3_rowE,alcohol_amount_3_rowF,alcohol_amount_3_rowG,alcohol_amount_3_rowH,alcohol_amount_3_rowL1,alcohol_amount_3_rowL2,alcohol_amount_3_rowL3)

alcohol_amount_3<-lapply(alcohol_amount_3,NaNtoNA)
alcohol_amount_3<-rowSums(as.data.frame(alcohol_amount_3))/100

bottles_drank_1<-alcohol_amount_1/(330*(5.0/100))
bottles_drank_2<-alcohol_amount_2/(330*(5.0/100))
bottles_drank_3<-alcohol_amount_3/(330*(5.0/100))

#### CREATING FINAL DATASET
data_moshi<-NULL
data_moshi<-with(data,data.frame(breath_level=QD04,
                                 age=AGE,
                                 gender=SEX,
                                 years_education=QI01,
                                 work=QI02,
                                 # sys_bp=QL01,
                                 gcs=QL02C,
                                 # resp_rate=QL03,
                                 # pulse=QL04,
                                 # avpu=QL05,
                                 freq_injuries=QL06,
                                 time_to_injury=QB01,
                                 fracture=QE011,
                                 dislocation=QE012,
                                 open_wound=QE013,
                                 bruise=QE014,
                                 burn=QE015,
                                 concussion=QE016,
                                 organ_injury=QE017,
                                 method_injury=QE02,
                                 type_vehicle=QE02B,
                                 motive_injury=QE03,
                                 location=QE06,
                                 activity_prior=QE071,
                                 past_alcohol_use=QG01,
                                 self_reported_alcohol_use=QF04,
                                 # high_alcohol_use=QG03,
                                 # past_alchol_use=QG04,
                                 # low_alcohol_use=QG04,
                                 # emergency_need=QG07,
                                 predictor_FUP1=QNH04,
                                 predictor_FUP2=QH02,
                                 alcohol_amount_1,
                                 alcohol_amount_2,
                                 alcohol_amount_3,
                                 bottles_drank_1,
                                 bottles_drank_2,
                                 bottles_drank_3))

#Creating KTS variable
kts<-NULL
kts$age<-car::recode(data$AGE,"6:55=2;else=1")
kts$number_injuries<-car::recode(data$QL06,"2=3;3=2;4=1;else=NA")
kts$sbp<-car::recode(data$QL01,"89.1:221=4;50:89=3;1:49=2;else=NA")
kts$resp_rate<-car::recode(data$QL03,"10:29=3;30:200=2;0:9=1;else=NA")
kts$avpu<-car::recode(data$QL05,"2=4;3=3;4=2;5=1;else=NA")
kts$total<-with(kts,rowSums(data.frame(age,number_injuries,sbp,resp_rate,avpu)))
data_moshi$kts<-NULL
data_moshi$kts<-kts$total

#Creating RTS variable
rts<-NULL
rts$sbp<-car::recode(data$QL01,"89.1:221=4;76:89=3;50:75=2;1:49=1;0=0;else=NA")
rts$sbp<-0.7326*rts$sbp
rts$resp_rate<-car::recode(data$QL03,"10:29=4;30:200=3;6:9=2;1:5=1;0=0;else=NA")
rts$resp_rate<-0.2908*rts$resp_rate
rts$gcs<-car::recode(data$QL02C,"13:15=4;9:12=3;6:8=2;5:4=1;3=0;else=NA")
rts$gcs<-0.9368*rts$gcs
rts$total<-with(rts,rowSums(data.frame(gcs,sbp,resp_rate)))
data_moshi$rts<-NULL
data_moshi$rts<-rts$total
#recode Missing from 99 to NA

#recode_moshi<-function(x){ 
#car::recode(x,"99=NA")
#}

## RECODING MISSING AND UNKNOWN DATA

data_moshi$method_injury<-car::recode(data_moshi$method_injury,"89=NA;99=NA")
data_moshi$type_vehicle<-car::recode(data_moshi$type_vehicle,"
               1='car';
               2='avru';
               3='avru';
               4='truck';
               9=NA;
               else='non-RTI'")
data_moshi$type_vehicle<-as.factor(data_moshi$type_vehicle)
data_moshi$work<-car::recode(data_moshi$work,"9=NA")
data_moshi$activity_prior<-car::recode(data_moshi$activity_prior,"99=NA")
# data_moshi$high_alcohol_use<-car::recode(data_moshi$high_alcohol_use,"99=NA")
data_moshi$past_alcohol_use<-car::recode(data_moshi$past_alcohol_use,"1:8='yes';9='no';99=NA")
data_moshi$past_alcohol_use<-as.factor(data_moshi$past_alcohol_use)
data_moshi$self_reported_alcohol_use<-car::recode(data_moshi$self_reported_alcohol_use,"1='yes';2='no';9=NA")
data_moshi$self_reported_alcohol_use<-as.factor(data_moshi$self_reported_alcohol_use)
# data_moshi$low_alcohol_use<-car::recode(data_moshi$low_alcohol_use,"99=NA")
data_moshi$predictor_FUP1<-car::recode(data_moshi$predictor_FUP1,"8=NA;9=NA;99=NA;NaN=2")
data_moshi$predictor_FUP2<-car::recode(data_moshi$predictor_FUP2,"8=NA;9=NA;99=NA;90=NA;NaN=2")
data_moshi$breath_level<-car::recode(data_moshi$breath_level,"0='no';9=NA;else='yes'")
data_moshi$breath_level<-as.factor(data_moshi$breath_level)
data_moshi$id<-c(1:516)
data_moshi$breath_level_limit<-car::recode(data$QD04,"0:0.08='no';NA=NA;else='yes'")
data_moshi$gcs<-car::recode(data_moshi$gcs,"99=NA")
data_moshi$method_injury<-car::recode(data_moshi$method_injury,"1='RTI';2='RTI';3='RTI';5='Violence';6='Violence';7='Violence';9='Outros';10='Violence';13='Outros';14='Outros'")
data_moshi$method_injury<-as.factor(data_moshi$method_injury)
# data_moshi$sys_bp<-car::recode(data_moshi$sys_bp,"999=NA")
# data_moshi$pulse<-car::recode(data_moshi$pulse,"999=NA")
# data_moshi$avpu<-car::recode(data_moshi$avpu,"9=NA")
data_moshi$fracture<-car::recode(data_moshi$fracture,"9=NA")
data_moshi$dislocation<-car::recode(data_moshi$dislocation,"9=NA")
data_moshi$open_wound<-car::recode(data_moshi$open_wound,"9=NA")
data_moshi$bruise<-car::recode(data_moshi$bruise,"9=NA")
data_moshi$concussion<-car::recode(data_moshi$concussion,"9=NA")
data_moshi$organ_injury<-car::recode(data_moshi$organ_injury,"9=NA")
data_moshi$type_vehicle<-car::recode(data_moshi$type_vehicle,"9=NA")
data_moshi$motive_injury<-car::recode(data_moshi$motive_injury,"9=NA")
data_moshi$location<-car::recode(data_moshi$location,"
               1='home';
               2='home';
               3='drinking place';
               4='drinking place';
               5='drinking place';
               6='drinking place';
               8='work place';
               9='Other';
               10='outdoor public place';
               11='outdoor public place';
               12='Other';
               99=NA")
data_moshi$location<-as.factor(data_moshi$location)
# data_moshi$emergency_need<-car::recode(data_moshi$emergency_need,"9=NA")
data_moshi$bottle<-car::recode(data_moshi$bottles_drank_1,"0=0;0.001:2.5='1 to 2';2.5001:4.5='3 to 4';4.50001:63.64='5 or more'")
data_moshi$bottle24<-car::recode(data_moshi$bottles_drank_2,"0=0;0.001:2.5='1 to 2';2.5001:4.5='3 to 4';4.50001:63.64='5 or more'")
data_moshi$bottle1week<-car::recode(data_moshi$bottles_drank_3,"0=0;0.001:2.5='1 to 2';2.5001:4.5='3 to 4';4.50001:159.1000='5 or more'")
# data_moshi$bottle_alcohol_positive<-car::recode(data_moshi$bottles_drank_1,"0=NA;0.001:2.5='1 to 2';2.5001:4.5='3 to 4';4.50001:63.64='5 or more'")
# data_moshi$bottle24_alcohol_positive<-car::recode(data_moshi$bottles_drank_2,"0=NA;0.001:2.5='1 to 2';2.5001:4.5='3 to 4';4.50001:63.64='5 or more'")
# data_moshi$bottle1week_alcohol_positive<-car::recode(data_moshi$bottles_drank_3,"0=NA;0.001:2.5='1 to 2';2.5001:4.5='3 to 4';4.50001:159.1000='5 or more'")

# data_moshi <- within(data_moshi, predictor_FUP1[QG01==9] <- 2)
# data_moshi <- within(data_moshi, predictor_FUP2[QG01==9] <- 2)

##### RTI data

rti_moshi<-data_moshi[data_moshi$method_injury == "RTI", ]



##### EPI data
data2 <- read.spss("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/Africa/Multi_country/Tz, SA, Mz alcohol use in ED/whoer_mozambique_southafrica.sav",
                       to.data.frame=TRUE)
data2<-as.data.frame(data2)

data2$method_injury<-car::recode(data2$qe02,"
						'Being hit by vehicle'='RTI';
						  'in vehicle collision as driver'='RTI';
						  'in vehicle collision as passenger'='RTI';
						  else='other'")

data2$breath_level<-car::recode(data2$qd04,"0='no';9=NA;else='yes'")
data2$breath_level<-as.factor(data2$breath_level)

rti_data2<-data2[data2$method_injury == "RTI", ]


propmiss <- function(dataframe) lapply(dataframe,function(x) data.frame(nmiss=sum(is.na(x)), n=length(x), propmiss=sum(is.na(x))/length(x)))

propmiss(data)

###################################################
#TABLE 1. DESCRIPTIVES
###################################################

##########RTI descriptive
mytable <- with(data_moshi,table(method_injury))
mytable
prop.table(mytable)

#########all injury
#BAC
mytable <- with(data_moshi,table(breath_level))
mytable
prop.table(mytable)

#Self-reported
mytable <- with(data_moshi,table(self_reported_alcohol_use))
mytable
prop.table(mytable)

##########RTI
#BAC
mytable <- with(data2,table(breath_level))
mytable
prop.table(mytable)

#Self-reported
mytable <- with(rti_moshi,table(self_reported_alcohol_use))
mytable
prop.table(mytable)


##########RTI descriptive
mytable <- with(data2,table(method_injury,qa03))
mytable
prop.table(mytable,2)

#########all injury
#BAC
mytable <- with(data2,table(breath_level,qa03))
mytable
prop.table(mytable,2)

#Self-reported
mytable <- with(data2,table(qf04,qa03))
mytable
prop.table(mytable,2)

##########RTI
#BAC
mytable <- with(rti_data2,table(breath_level,qa03))
mytable
prop.table(mytable,2)

#Self-reported
mytable <- with(rti_data2,table(qf04,qa03))
mytable
prop.table(mytable,2)

########### Pooling data

library(meta)

total_rti<-c(113,93,375)
total_injury<-c(459,464,516)
# total_non_rti<-total_injury-total_rti
country<-c("Mz","SA","Tz")
total_injury_self<-c(77,238,143)
total_rti_self<-c(26,37,96)
total_injury_bac<-c(92,244,154)
total_rti_bac<-c(27,39,107)

#Calculating metanalysis
m3<-metaprop(total_rti,total_injury,sm="PLN",
	studlab=country,comb.fixed=TRUE)
summary(m3)
meta::forest(m3)

#Calculating metanalysis
m3<-metaprop(total_injury_self,total_injury,sm="PLN",
	studlab=country,comb.fixed=TRUE)
summary(m3)
meta::forest(m3)

#Calculating metanalysis
m3<-metaprop(total_rti_self,total_rti,sm="PLN",
	studlab=country,comb.fixed=TRUE)
summary(m3)
meta::forest(m3)

#Calculating metanalysis
m3<-metaprop(total_injury_bac,total_injury,sm="PLN",
	studlab=country,comb.fixed=TRUE)
summary(m3)
meta::forest(m3)

#Calculating metanalysis
m3<-metaprop(total_rti_bac,total_rti,sm="PLN",
	studlab=country,comb.fixed=TRUE)
summary(m3)
meta::forest(m3)



#Calculating metanalysis
m3<-metaprop(total_rti,total_traume,sm="PLN",data=meta_trauma,
	studlab=Author,comb.fixed=FALSE)
 
tiff("/Users/jnv4/Desktop/rti_trauma_overall.tiff", 
	width = 700, height = 1200,compression = 'lzw')
meta::forest(m3)
dev.off()
metainf(m3)
metainf(m3, pooled="random")
funnel(m3)

