
#Calcular percentual de acerto por ano
#Simular a distribuição de percentual de acerto a calcular médias com comparação


#1o Ano
# 113 pessoas
# 40 questões

data1ano<-c(rep(0,1),rep(2,2),rep(5,10),rep(7,11),rep(10,22),rep(12,24),rep(14,27),rep(17,8),rep(19,4),rep(21,1),rep(24,3))
data1ano_norm<-(100*data1ano)/40
data1ano_cat<-rep("1ano",113)

#2o Ano
# 24 alunos
# 18 questões

data2ano<-c(rep(6,1),rep(7,1),rep(8,2),rep(9,3),rep(10,4),rep(11,2),rep(12,2),rep(13,4),rep(14,1),rep(15,2),rep(17,1),rep(18,1))
data2ano_norm<-(100*data2ano)/18
data2ano_cat<-rep("2ano",24)

#3o Ano
# 19 pessoas
# 40 questões

data3ano<-c(rep(10,1),rep(12,1),rep(14,2),rep(15,2),rep(16,3),rep(17,2),rep(18,1),rep(19,1),rep(20,5),rep(22,1))
data3ano_norm<-(100*data3ano)/40
data3ano_cat<-rep("3ano",19)

#4o Ano
# 39 pessoas	
# 40 questões

data4ano<-c(rep(8,1),rep(10,2),rep(11,3),rep(12,3),rep(13,3),rep(14,4),rep(15,3),rep(16,2),rep(17,2),rep(18,3),rep(19,4),rep(20,2),rep(21,1),rep(22,3),rep(23,2),rep(25,1))
data4ano_norm<-(100*data4ano)/40
data4ano_cat<-rep("4ano",39)

#5o Ano
# 81 alunos
# 40 questões

data5ano<-c(rep(7,1),rep(8,1),rep(9,1),rep(10,3),rep(11,3),rep(12,5),rep(13,4),rep(14,14),rep(15,10),rep(16,5),rep(17,5),rep(18,10),rep(19,3),rep(20,7),rep(21,2),rep(22,4),rep(24,1),rep(25,2))
data5ano_norm<-(100*data5ano)/40
data5ano_cat<-rep("5ano",81)

#6o Ano
# 103 alunos
# 40 questões

data6ano<-c(rep(8,4),rep(9,2),rep(10,3),rep(11,2),rep(12,1),rep(13,3),rep(14,3),rep(15,1),rep(16,7),rep(17,13),rep(18,14),rep(19,15),rep(20,11),rep(21,9),rep(22,7),rep(23,3),rep(24,3),rep(25,1),rep(26,1))
data6ano_norm<-(100*data6ano)/40
data6ano_cat<-rep("6ano",103)


#### BOXPLOT

data_boxplot<-data.frame(value=c(data1ano_norm,data2ano_norm,data3ano_norm,data4ano_norm,data5ano_norm,data6ano_norm),ano=c(data1ano_cat,data2ano_cat,data3ano_cat,data4ano_cat,data5ano_cat,data6ano_cat))

ggplot(data=data_boxplot, aes(x=ano, y=value,fill=ano)) + geom_boxplot() + ylab("% de Acertos") + xlab("Anos") + theme_bw()

ggplot(data=data_boxplot, aes(x=ano, y=value,fill=ano)) + geom_line() + ylab("% de Acertos") + xlab("Anos") + theme_bw()