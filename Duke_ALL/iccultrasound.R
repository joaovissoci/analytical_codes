
library("psych")

# add the path to you computer between " "
data<-read.csv("/Users/Joao/Desktop/duem_3DUSbilliary_data.csv",sep=',')


#BPD
ICC(data[,2:4])

ICC(data[,c(2,6)])

	
#BFemur
ICC(data[,7:9])

ICC(data[,c(7,11)])

#Humerus
ICC(data[,12:14])

ICC(data[,c(14,16)])


library(BlandAltmanLeh)
bland.altman.plot(data[,4], data[,5], main="This is a Bland Altman Plot", 
                        xlab="Means", 
                        ylab="Differences",
                        graph.sys = "ggplot2")

bland.altman.plot(data[,9], data[,10], main="This is a Bland Altman Plot", 
                        xlab="Means", 
                        ylab="Differences",
                        graph.sys = "ggplot2")

bland.altman.plot(data[,14], data[,15], main="This is a Bland Altman Plot", 
                        xlab="Means", 
                        ylab="Differences",
                        graph.sys = "ggplot2")

