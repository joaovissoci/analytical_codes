

data<-read.csv("/Users/jnv4/Desktop/TBI_2015-09-25_1209_Clean.csv")

names(data)

table(data$male)
prop.table(table(data$male))

sd(na.omit(data$age))

table(data$moi)
prop.table(table(data$moi))

table(data$alcohol)
prop.table(table(data$alcohol))

table(data$ref)
prop.table(table(data$ref))

T1<-(data$t1_hrs*60)+data$t1_min
T2<-(data$t2_hrs*60)+data$t2_min
T3<-(data$t3_hrs*60)+data$t3_min
T4<-(data$t4_hrs*60)+data$t4_min

T_total<-data.frame(T1,T2,T3,T4)

T_total<-base::rowSums(T_total,na.rm=TRUE)


Tt1<-(data$time_injury1*60)+data$time_injury2
Tt2<-(data$time_arrival1*60)+data$time_arrival2

Tt_total<-na.omit(data.frame(Tt1,Tt2))
Tt_total<-Tt_total[1]-

T_total<-base::rowSums(T_total,na.rm=TRUE)

injuryT<-with(data,paste(inj_date, inj_time, sep=" "))
injury_time <- as.POSIXct(injuryT,
                      format='%m/%d/%y %H:%M')

arrivalT<-with(data,paste(date_arrival,time_arrival, sep=" "))
arrival_time <- as.POSIXct(arrivalT,
                      format='%m/%d/%y %H:%M')

dif_time<-difftime(injury_time, arrival_time,
         units = c("min"))*-1

dif_time<-car::recode(dif_time,"-9435=NA;-15=NA;
	-524370=NA;-2730=NA;-510=NA")

summary(dif_time)
