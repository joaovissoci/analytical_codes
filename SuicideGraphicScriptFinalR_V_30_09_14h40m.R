#############################################################################
# BAYESIAN NETWORK- SUICIDE - MARINGÁ - PARANA - BRAZIL
#############################################################################
# Clean environment
closeAllConnections()
rm(list=ls())
setwd("~/Doutorado/01.BN_R_Scripts/Suicide")

# Installing / Loading packages 
#install.packages("bnlearn")  # -----------> UNCOMMENT IF NECESSARY TO INSTALL THE PACKAGE
library(bnlearn)
#install.packages("gdata")    # -----------> UNCOMMENT IF NECESSARY TO INSTALL THE PACKAGE
library(gdata)
# Install RGraphviz           # -----------> UNCOMMENT IF NECESSARY TO INSTALL THE PACKAGE
#source("http://bioconductor.org/biocLite.R") # -----------> UNCOMMENT IF NECESSARY TO INSTALL THE PACKAGE
#biocLite("Rgraphviz")        # -----------> UNCOMMENT IF NECESSARY TO INSTALL THE PACKAGE
library(Rgraphviz)
# install.packages("QuantPsyc")
library(QuantPsyc)
# install.packages("rbmn")
library(rbmn)

# Loading data  
data<-read.csv("suicide_gis.csv",sep=',')

#############################################################################
# DATA MANAGEMENT
#############################################################################
# Command below shows data is a dataframe, contains 399 obs and 20 numerical variables
str(data)
# Remove two variables
data<-remove.vars(data,c("CD_GEOCODM","Agric2000"))
# Show the firsts lines 
head(data)

# CREATING A DATA FRAME 
data_2000_com_CLB_com_IDH <- with(data,data.frame(school2000,Inform2000,Income00,Unemploy00,Chil_lab00,IDHM00,SMR151998,SMR202498,SMR252998))
data_2010_com_CLB_com_IDH <- with(data,data.frame(school2010,Inform2010,Income10,Unemploy10,Chil_lab10,IDHM10,SMR151908,SMR202408,SMR252908))

data_2000_com_CLB_sem_IDH <- with(data,data.frame(school2000,Inform2000,Income00,Unemploy00,Chil_lab00,SMR151998,SMR202498,SMR252998))
data_2010_com_CLB_sem_IDH <- with(data,data.frame(school2010,Inform2010,Income10,Unemploy10,Chil_lab10,SMR151908,SMR202408,SMR252908))

data_2000_sem_CLB_com_IDH <- with(data,data.frame(school2000,Inform2000,Income00,Unemploy00,IDHM00,SMR151998,SMR202498,SMR252998))
data_2010_sem_CLB_com_IDH <- with(data,data.frame(school2010,Inform2010,Income10,Unemploy10,IDHM10,SMR151908,SMR202408,SMR252908))

data_2000_sem_CLB_sem_IDH <- with(data,data.frame(school2000,Inform2000,Income00,Unemploy00,SMR151998,SMR202498,SMR252998))
data_2010_sem_CLB_sem_IDH <- with(data,data.frame(school2010,Inform2010,Income10,Unemploy10,SMR151908,SMR202408,SMR252908))

# CALCULATE AND ADD SUICIDE VARIABLE AS A MEAN OF SMR15 SMR2024 SMR2529
data_2000_com_CLB_com_IDH$suicide<-rowMeans(data_2000_com_CLB_com_IDH[7:9])  # BECAUSE REMOVE VARIABLE "Agric2000"
data_2000_com_CLB_sem_IDH$suicide<-rowMeans(data_2000_com_CLB_sem_IDH[6:8])  # BECAUSE REMOVE VARIABLE "Agric2000"
data_2000_sem_CLB_com_IDH$suicide<-rowMeans(data_2000_sem_CLB_com_IDH[6:8])  # BECAUSE REMOVE VARIABLE "Agric2000"
data_2000_sem_CLB_sem_IDH$suicide<-rowMeans(data_2000_sem_CLB_sem_IDH[5:7])  # BECAUSE REMOVE VARIABLE "Agric2000"

data_2010_com_CLB_com_IDH$suicide<-rowMeans(data_2010_com_CLB_com_IDH[7:9])  # BECAUSE REMOVE VARIABLE "Agric2010"
data_2010_com_CLB_sem_IDH$suicide<-rowMeans(data_2010_com_CLB_sem_IDH[6:8])  # BECAUSE REMOVE VARIABLE "Agric2010"
data_2010_sem_CLB_com_IDH$suicide<-rowMeans(data_2010_sem_CLB_com_IDH[6:8])  # BECAUSE REMOVE VARIABLE "Agric2010"
data_2010_sem_CLB_sem_IDH$suicide<-rowMeans(data_2010_sem_CLB_sem_IDH[5:7])  # BECAUSE REMOVE VARIABLE "Agric2010"

# CREATE A VARIABLE TO WORK WITH BN
# 1 - 2000_com_CLB_com_IDH - MEDIAN
directed_network1 <- with(data_2000_com_CLB_com_IDH,data.frame(data_2000_com_CLB_com_IDH$school2000, data_2000_com_CLB_com_IDH$Inform2000, 
                                                               data_2000_com_CLB_com_IDH$Income00,   data_2000_com_CLB_com_IDH$Unemploy00, 
                                                               data_2000_com_CLB_com_IDH$Chil_lab00, data_2000_com_CLB_com_IDH$IDHM00,
                                                               data_2000_com_CLB_com_IDH$suicide))

# 2 - 2000_com_CLB_com_IDH - 15 TO 19
directed_network2 <- with(data_2000_com_CLB_com_IDH,data.frame(data_2000_com_CLB_com_IDH$school2000, data_2000_com_CLB_com_IDH$Inform2000, 
                                                               data_2000_com_CLB_com_IDH$Income00,   data_2000_com_CLB_com_IDH$Unemploy00, 
                                                               data_2000_com_CLB_com_IDH$Chil_lab00, data_2000_com_CLB_com_IDH$IDHM00,
                                                               data_2000_com_CLB_com_IDH$SMR151998))
# 3 - 2000_com_CLB_com_IDH - 20 TO 24
directed_network3 <- with(data_2000_com_CLB_com_IDH,data.frame(data_2000_com_CLB_com_IDH$school2000, data_2000_com_CLB_com_IDH$Inform2000, 
                                                               data_2000_com_CLB_com_IDH$Income00, data_2000_com_CLB_com_IDH$Unemploy00, 
                                                               data_2000_com_CLB_com_IDH$Chil_lab00, data_2000_com_CLB_com_IDH$IDHM00,
                                                               data_2000_com_CLB_com_IDH$SMR202498))

# 4 - 2000_com_CLB_com_IDH - 25 TO 29
directed_network4 <- with(data_2000_com_CLB_com_IDH,data.frame(data_2000_com_CLB_com_IDH$school2000, data_2000_com_CLB_com_IDH$Inform2000, 
                                                               data_2000_com_CLB_com_IDH$Income00,   data_2000_com_CLB_com_IDH$Unemploy00, 
                                                               data_2000_com_CLB_com_IDH$Chil_lab00, data_2000_com_CLB_com_IDH$IDHM00,
                                                               data_2000_com_CLB_com_IDH$SMR252998))

# 5 - 2000_com_CLB_sem_IDH - MEDIAN
directed_network5 <- with(data_2000_com_CLB_sem_IDH,data.frame(data_2000_com_CLB_sem_IDH$school2000, data_2000_com_CLB_sem_IDH$Inform2000, 
                                                               data_2000_com_CLB_sem_IDH$Income00,   data_2000_com_CLB_sem_IDH$Unemploy00, 
                                                               data_2000_com_CLB_sem_IDH$Chil_lab00, data_2000_com_CLB_sem_IDH$suicide))

# 6 - 2000_com_CLB_sem_IDH - 15 TO 19
directed_network6 <- with(data_2000_com_CLB_sem_IDH,data.frame(data_2000_com_CLB_sem_IDH$school2000, data_2000_com_CLB_sem_IDH$Inform2000, 
                                                               data_2000_com_CLB_sem_IDH$Income00,   data_2000_com_CLB_sem_IDH$Unemploy00, 
                                                               data_2000_com_CLB_sem_IDH$Chil_lab00, data_2000_com_CLB_sem_IDH$SMR151998))
# 7 - 2000_com_CLB_sem_IDH - 20 TO 24
directed_network7 <- with(data_2000_com_CLB_sem_IDH,data.frame(data_2000_com_CLB_sem_IDH$school2000, data_2000_com_CLB_sem_IDH$Inform2000, 
                                                               data_2000_com_CLB_sem_IDH$Income00,   data_2000_com_CLB_sem_IDH$Unemploy00, 
                                                               data_2000_com_CLB_sem_IDH$Chil_lab00, data_2000_com_CLB_sem_IDH$SMR202498))

# 8 - 2000_com_CLB_sem_IDH - 25 TO 29
directed_network8 <- with(data_2000_com_CLB_sem_IDH,data.frame(data_2000_com_CLB_sem_IDH$school2000, data_2000_com_CLB_sem_IDH$Inform2000, 
                                                               data_2000_com_CLB_sem_IDH$Income00,   data_2000_com_CLB_sem_IDH$Unemploy00, 
                                                               data_2000_com_CLB_sem_IDH$Chil_lab00, data_2000_com_CLB_sem_IDH$SMR252998))

# 9 - 2000_sem_CLB_com_IDH - MEDIAN
directed_network9 <- with(data_2000_sem_CLB_com_IDH,data.frame(data_2000_sem_CLB_com_IDH$school2000, data_2000_sem_CLB_com_IDH$Inform2000, 
                                                               data_2000_sem_CLB_com_IDH$Income00, data_2000_sem_CLB_com_IDH$Unemploy00, 
                                                               data_2000_sem_CLB_com_IDH$IDHM00, data_2000_sem_CLB_com_IDH$suicide))

# 10 - 2000_sem_CLB_com_IDH - 15 TO 19
directed_network10 <- with(data_2000_sem_CLB_com_IDH,data.frame(data_2000_sem_CLB_com_IDH$school2000, data_2000_sem_CLB_com_IDH$Inform2000, 
                                                                data_2000_sem_CLB_com_IDH$Income00,   data_2000_sem_CLB_com_IDH$Unemploy00, 
                                                                data_2000_sem_CLB_com_IDH$IDHM00,     data_2000_sem_CLB_com_IDH$SMR151998))
# 11 - 2000_sem_CLB_com_IDH - 20 TO 24
directed_network11 <- with(data_2000_sem_CLB_com_IDH,data.frame(data_2000_sem_CLB_com_IDH$school2000, data_2000_sem_CLB_com_IDH$Inform2000, 
                                                                data_2000_sem_CLB_com_IDH$Income00,   data_2000_sem_CLB_com_IDH$Unemploy00, 
                                                                data_2000_sem_CLB_com_IDH$IDHM00,     data_2000_sem_CLB_com_IDH$SMR202498))

# 12 - 2000_sem_CLB_com_IDH - 25 TO 29
directed_network12 <- with(data_2000_sem_CLB_com_IDH,data.frame(data_2000_sem_CLB_com_IDH$school2000, data_2000_sem_CLB_com_IDH$Inform2000, 
                                                                data_2000_sem_CLB_com_IDH$Income00,   data_2000_sem_CLB_com_IDH$Unemploy00, 
                                                                data_2000_sem_CLB_com_IDH$IDHM00,     data_2000_sem_CLB_com_IDH$SMR252998))

# 13 - 2000_sem_CLB_sem_IDH - MEDIAN
directed_network13 <- with(data_2000_sem_CLB_sem_IDH,data.frame(data_2000_sem_CLB_sem_IDH$school2000, data_2000_sem_CLB_sem_IDH$Inform2000, 
                                                                data_2000_sem_CLB_sem_IDH$Income00,   data_2000_sem_CLB_sem_IDH$Unemploy00, 
                                                                data_2000_sem_CLB_sem_IDH$suicide))

# 14 - 2000_sem_CLB_sem_IDH - 15 TO 19
directed_network14 <- with(data_2000_sem_CLB_sem_IDH,data.frame(data_2000_sem_CLB_sem_IDH$school2000, data_2000_sem_CLB_sem_IDH$Inform2000, 
                                                                data_2000_sem_CLB_sem_IDH$Income00,   data_2000_sem_CLB_sem_IDH$Unemploy00, 
                                                                data_2000_sem_CLB_sem_IDH$SMR151998))
# 15 - 2000_sem_CLB_sem_IDH - 20 TO 24
directed_network15 <- with(data_2000_sem_CLB_sem_IDH,data.frame(data_2000_sem_CLB_sem_IDH$school2000, data_2000_sem_CLB_sem_IDH$Inform2000, 
                                                                data_2000_sem_CLB_sem_IDH$Income00,   data_2000_sem_CLB_sem_IDH$Unemploy00, 
                                                                data_2000_sem_CLB_sem_IDH$SMR202498))

# 16 - 2000_sem_CLB_sem_IDH - 25 TO 29
directed_network16 <- with(data_2000_sem_CLB_sem_IDH,data.frame(data_2000_sem_CLB_sem_IDH$school2000, data_2000_sem_CLB_sem_IDH$Inform2000, 
                                                                data_2000_sem_CLB_sem_IDH$Income00,   data_2000_sem_CLB_sem_IDH$Unemploy00, 
                                                                data_2000_sem_CLB_sem_IDH$SMR252998))

# 17 - 2010_com_CLB_com_IDH - MEDIAN
directed_network17 <- with(data_2010_com_CLB_com_IDH,data.frame(data_2010_com_CLB_com_IDH$school2010, data_2010_com_CLB_com_IDH$Inform2010, 
                                                                data_2010_com_CLB_com_IDH$Income10,   data_2010_com_CLB_com_IDH$Unemploy10, 
                                                                data_2010_com_CLB_com_IDH$Chil_lab10, data_2010_com_CLB_com_IDH$IDHM10,
                                                                data_2010_com_CLB_com_IDH$suicide))

# 18 - 2010_com_CLB_com_IDH - 15 TO 19
directed_network18 <- with(data_2010_com_CLB_com_IDH,data.frame(data_2010_com_CLB_com_IDH$school2010, data_2010_com_CLB_com_IDH$Inform2010, 
                                                                data_2010_com_CLB_com_IDH$Income10,   data_2010_com_CLB_com_IDH$Unemploy10, 
                                                                data_2010_com_CLB_com_IDH$Chil_lab10, data_2010_com_CLB_com_IDH$IDHM10,
                                                                data_2010_com_CLB_com_IDH$SMR151908))
# 19 - 2010_com_CLB_com_IDH - 20 TO 24
directed_network19 <- with(data_2010_com_CLB_com_IDH,data.frame(data_2010_com_CLB_com_IDH$school2010, data_2010_com_CLB_com_IDH$Inform2010, 
                                                                data_2010_com_CLB_com_IDH$Income10,   data_2010_com_CLB_com_IDH$Unemploy10, 
                                                                data_2010_com_CLB_com_IDH$Chil_lab10, data_2010_com_CLB_com_IDH$IDHM10,
                                                                data_2010_com_CLB_com_IDH$SMR202408))

# 20 - 2010_com_CLB_com_IDH - 25 TO 29
directed_network20 <- with(data_2010_com_CLB_com_IDH,data.frame(data_2010_com_CLB_com_IDH$school2010, data_2010_com_CLB_com_IDH$Inform2010, 
                                                                data_2010_com_CLB_com_IDH$Income10,   data_2010_com_CLB_com_IDH$Unemploy10, 
                                                                data_2010_com_CLB_com_IDH$Chil_lab10, data_2010_com_CLB_com_IDH$IDHM10,
                                                                data_2010_com_CLB_com_IDH$SMR252908))

# 21 - 2010_com_CLB_sem_IDH - MEDIAN
directed_network21 <- with(data_2010_com_CLB_sem_IDH,data.frame(data_2010_com_CLB_sem_IDH$school2010, data_2010_com_CLB_sem_IDH$Inform2010, 
                                                                data_2010_com_CLB_sem_IDH$Income10,   data_2010_com_CLB_sem_IDH$Unemploy10, 
                                                                data_2010_com_CLB_sem_IDH$Chil_lab10, data_2010_com_CLB_sem_IDH$suicide))

# 22 - 2010_com_CLB_sem_IDH - 15 TO 19
directed_network22 <- with(data_2010_com_CLB_sem_IDH,data.frame(data_2010_com_CLB_sem_IDH$school2010, data_2010_com_CLB_sem_IDH$Inform2010, 
                                                                data_2010_com_CLB_sem_IDH$Income10,   data_2010_com_CLB_sem_IDH$Unemploy10, 
                                                                data_2010_com_CLB_sem_IDH$Chil_lab10, data_2010_com_CLB_sem_IDH$SMR151908))
# 23 - 2010_com_CLB_sem_IDH - 20 TO 24
directed_network23 <- with(data_2010_com_CLB_sem_IDH,data.frame(data_2010_com_CLB_sem_IDH$school2010, data_2010_com_CLB_sem_IDH$Inform2010, 
                                                                data_2010_com_CLB_sem_IDH$Income10,   data_2010_com_CLB_sem_IDH$Unemploy10, 
                                                                data_2010_com_CLB_sem_IDH$Chil_lab10, data_2010_com_CLB_sem_IDH$SMR202408))

# 24 - 2010_com_CLB_sem_IDH - 25 TO 29
directed_network24 <- with(data_2010_com_CLB_sem_IDH,data.frame(data_2010_com_CLB_sem_IDH$school2010, data_2010_com_CLB_sem_IDH$Inform2010, 
                                                                data_2010_com_CLB_sem_IDH$Income10,   data_2010_com_CLB_sem_IDH$Unemploy10, 
                                                                data_2010_com_CLB_sem_IDH$Chil_lab10, data_2010_com_CLB_sem_IDH$SMR252908))

# 25 - 2010_sem_CLB_com_IDH - MEDIAN
directed_network25 <- with(data_2010_sem_CLB_com_IDH,data.frame(data_2010_sem_CLB_com_IDH$school2010, data_2010_sem_CLB_com_IDH$Inform2010, 
                                                                data_2010_sem_CLB_com_IDH$Income10,   data_2010_sem_CLB_com_IDH$Unemploy10, 
                                                                data_2010_sem_CLB_com_IDH$IDHM10,     data_2010_sem_CLB_com_IDH$suicide))

# 26 - 2010_sem_CLB_com_IDH - 15 TO 19
directed_network26 <- with(data_2010_sem_CLB_com_IDH,data.frame(data_2010_sem_CLB_com_IDH$school2010, data_2010_sem_CLB_com_IDH$Inform2010, 
                                                                data_2010_sem_CLB_com_IDH$Income10,   data_2010_sem_CLB_com_IDH$Unemploy10, 
                                                                data_2010_sem_CLB_com_IDH$IDHM10,     data_2010_sem_CLB_com_IDH$SMR151908))
# 27 - 2010_sem_CLB_com_IDH - 20 TO 24
directed_network27 <- with(data_2010_sem_CLB_com_IDH,data.frame(data_2010_sem_CLB_com_IDH$school2010, data_2010_sem_CLB_com_IDH$Inform2010, 
                                                                data_2010_sem_CLB_com_IDH$Income10,   data_2010_sem_CLB_com_IDH$Unemploy10, 
                                                                data_2010_sem_CLB_com_IDH$IDHM10,     data_2010_sem_CLB_com_IDH$SMR202408))

# 28 - 2010_sem_CLB_com_IDH - 25 TO 29
directed_network28 <- with(data_2010_sem_CLB_com_IDH,data.frame(data_2010_sem_CLB_com_IDH$school2010, data_2010_sem_CLB_com_IDH$Inform2010, 
                                                                data_2010_sem_CLB_com_IDH$Income10,   data_2010_sem_CLB_com_IDH$Unemploy10, 
                                                                data_2010_sem_CLB_com_IDH$IDHM10,     data_2010_sem_CLB_com_IDH$SMR252908))

# 29 - 2010_sem_CLB_sem_IDH - MEDIAN
directed_network29 <- with(data_2010_sem_CLB_sem_IDH,data.frame(data_2010_sem_CLB_sem_IDH$school2010, data_2010_sem_CLB_sem_IDH$Inform2010, 
                                                                data_2010_sem_CLB_sem_IDH$Income10,   data_2010_sem_CLB_sem_IDH$Unemploy10, 
                                                                data_2010_sem_CLB_sem_IDH$suicide))

# 30 - 2010_sem_CLB_sem_IDH - 15 TO 19
directed_network30 <- with(data_2010_sem_CLB_sem_IDH,data.frame(data_2010_sem_CLB_sem_IDH$school2010, data_2010_sem_CLB_sem_IDH$Inform2010, 
                                                                data_2010_sem_CLB_sem_IDH$Income10,   data_2010_sem_CLB_sem_IDH$Unemploy10, 
                                                                data_2010_sem_CLB_sem_IDH$SMR151908))
# 31 - 2010_sem_CLB_sem_IDH - 20 TO 24
directed_network31 <- with(data_2010_sem_CLB_sem_IDH,data.frame(data_2010_sem_CLB_sem_IDH$school2010, data_2010_sem_CLB_sem_IDH$Inform2010, 
                                                                data_2010_sem_CLB_sem_IDH$Income10,   data_2010_sem_CLB_sem_IDH$Unemploy10, 
                                                                data_2010_sem_CLB_sem_IDH$SMR202408))

# 32 - 2010_sem_CLB_sem_IDH - 25 TO 29
directed_network32 <- with(data_2010_sem_CLB_sem_IDH,data.frame(data_2010_sem_CLB_sem_IDH$school2010, data_2010_sem_CLB_sem_IDH$Inform2010, 
                                                                data_2010_sem_CLB_sem_IDH$Income10,   data_2010_sem_CLB_sem_IDH$Unemploy10, 
                                                                data_2010_sem_CLB_sem_IDH$SMR252908))

# Setting number of networks
nu_bns <- 32

# Create an array with n positions to store titles
atitle <- c(1:nu_bns)
atitle[1] <- "2000_com_CLB_com_IDH AVERAGE"
atitle[2] <- "2000_com_CLB_com_IDH 15 TO 19"
atitle[3] <- "2000_com_CLB_com_IDH 20 TO 24"
atitle[4] <- "2000_com_CLB_com_IDH 25 TO 29"
atitle[5] <- "2000_com_CLB_sem_IDH AVERAGE"
atitle[6] <- "2000_com_CLB_sem_IDH 15 TO 19"
atitle[7] <- "2000_com_CLB_sem_IDH 20 TO 24"
atitle[8] <- "2000_com_CLB_sem_IDH 25 TO 29"
atitle[9] <- "2000_sem_CLB_com_IDH AVERAGE"
atitle[10] <- "2000_sem_CLB_com_IDH 15 TO 19"
atitle[11] <- "2000_sem_CLB_com_IDH 20 TO 24"
atitle[12] <- "2000_sem_CLB_com_IDH 25 TO 29"
atitle[13] <- "2000_sem_CLB_sem_IDH AVERAGE"
atitle[14] <- "2000_sem_CLB_sem_IDH 15 TO 19"
atitle[15] <- "2000_sem_CLB_sem_IDH 20 TO 24"
atitle[16] <- "2000_sem_CLB_sem_IDH 25 TO 29"
atitle[17] <- "2010_com_CLB_com_IDH AVERAGE"
atitle[18] <- "2010_com_CLB_com_IDH 15 TO 19"
atitle[19] <- "2010_com_CLB_com_IDH 20 TO 24"
atitle[20] <- "2010_com_CLB_com_IDH 25 TO 29"
atitle[21] <- "2010_com_CLB_sem_IDH AVERAGE"
atitle[22] <- "2010_com_CLB_sem_IDH 15 TO 19"
atitle[23] <- "2010_com_CLB_sem_IDH 20 TO 24"
atitle[24] <- "2010_com_CLB_sem_IDH 25 TO 29"
atitle[25] <- "2010_sem_CLB_com_IDH AVERAGE"
atitle[26] <- "2010_sem_CLB_com_IDH 15 TO 19"
atitle[27] <- "2010_sem_CLB_com_IDH 20 TO 24"
atitle[28] <- "2010_sem_CLB_com_IDH 25 TO 29"
atitle[29] <- "2010_sem_CLB_sem_IDH AVERAGE"
atitle[30] <- "2010_sem_CLB_sem_IDH 15 TO 19"
atitle[31] <- "2010_sem_CLB_sem_IDH 20 TO 24"
atitle[32] <- "2010_sem_CLB_sem_IDH 25 TO 29"

# Creating names for nodes

varnames1 <- c("EDU","INF","INC","UNP","CLB","HDI","SCD")
## INSERTING VARNAMES AS HEADER OF DIRECTED_NETWORK
names(directed_network1) = varnames1
names(directed_network2) = varnames1
names(directed_network3) = varnames1
names(directed_network4) = varnames1

names(directed_network17) = varnames1
names(directed_network18) = varnames1
names(directed_network19) = varnames1
names(directed_network20) = varnames1

varnames2 <- c("EDU","INF","INC","UNP","CLB","SCD")
## INSERTING VARNAMES AS HEADER OF DIRECTED_NETWORK
names(directed_network5) = varnames2
names(directed_network6) = varnames2
names(directed_network7) = varnames2
names(directed_network8) = varnames2

names(directed_network21) = varnames2
names(directed_network22) = varnames2
names(directed_network23) = varnames2
names(directed_network24) = varnames2

varnames3 <- c("EDU","INF","INC","UNP","HDI","SCD")
## INSERTING VARNAMES AS HEADER OF DIRECTED_NETWORK
names(directed_network9)  = varnames3
names(directed_network10) = varnames3
names(directed_network11) = varnames3
names(directed_network12) = varnames3

names(directed_network25) = varnames3
names(directed_network26) = varnames3
names(directed_network27) = varnames3
names(directed_network28) = varnames3

varnames4 <- c("EDU","INF","INC","UNP","SCD")
## INSERTING VARNAMES AS HEADER OF DIRECTED_NETWORK
names(directed_network13) = varnames4
names(directed_network14) = varnames4
names(directed_network15) = varnames4
names(directed_network16) = varnames4

names(directed_network29) = varnames4
names(directed_network30) = varnames4
names(directed_network31) = varnames4
names(directed_network32) = varnames4

# Creating empty graphs
ag1=empty.graph(varnames1)
ag2=empty.graph(varnames2)
ag3=empty.graph(varnames3)
ag4=empty.graph(varnames4)


# Choose between a flag = 1 to flag = 5
# directed_network1 = 2000, directed_network2 = 2010, directed_network3 = 2010 15-19, 
# directed_network4 = 2010 20-24, directed_network5 = 2010 25-29
#for (x in 1:nu_bns)
for (x in 1:29)
{
  assign("directed_network",get(paste(c("directed_network", x), collapse = "")))
  
  cor(as.matrix(directed_network))
  

  if (x==1  | x==2  | x==3  | x==4  | x==9  | x==10 | x==11 | x==12 |
      x==17 | x==18 | x==19 | x==20 | x==25 | x==26 | x==27 | x==28)
  {
    wl = data.frame(from = c("EDU", "INC"), 
                      to = c("UNP", "HDI"))
    bl = data.frame(from = c("SCD", "SCD", "SCD", "SCD"), 
                      to = c("INC", "INF", "UNP", "EDU"))
  }
    
  if (x==5  | x==6  | x==7  | x==8  | x==13  | x==14 | x==15 | x==16 |
      x==21 | x==22 | x==23 | x==24 | x==29 | x==30 | x==31 | x==32)
  {
    wl = data.frame(from = c("EDU","UNP"), 
                      to = c("UNP", "SCD"))
    
    bl = data.frame(from = c("SCD", "SCD", "SCD", "SCD"), 
                    to = c("INC", "INF", "UNP", "EDU"))
  }
    
  if (x==4  | x==8)
  {
    wl = data.frame(from = c("EDU", "UNP"), 
                      to = c("UNP", "SCD"))
    
    bl = data.frame(from = c("SCD", "SCD", "SCD", "SCD", "SCD"), 
                      to = c("INC", "INF", "UNP", "EDU", "CLB"))
  }
  
  
  # Learning the BN ##########  
  
  # rb2 = hc(directed_network,  optimized = TRUE, debug = FALSE)
  bn = hc(directed_network, whitelist = wl, blacklist=bl, debug = FALSE)
  ft = bn.fit(bn, directed_network)
  ## BIC and BDe and BGeassign higher scores to DAGs that fit the data better
  print(paste(c("bge score", x, ":", score(bn, data = directed_network, type = "bge")), collapse = ""))
  
  # building a graph ##########  
    
  # CREATING A BN OBJECT TO WORK ON GRAPHIC LATER
  pp<-graphviz.plot(bn)
  # BUILDS A TITLE BASED ON BN LIKE "Suicide's predictor - BN1 - HC" TO MAKE IT EASY TO IDENTIFY
  Title <- paste("Suicide's predictors - BN", x)
    
  # STORE A STRENGTH OF EACH EDGE OF GRAPH
  strength<-arc.strength(bn, directed_network)
    
  # STORE THE NAME OF ALL EDGES 
  labels1 <- edgeNames(pp)
  labels <- "" 
    
  # SCAN ALL LINE OF VARIABLE LABELS1
  for (y in 1:length(labels1)) 
    {
      labels[y] = ""
      # SCAN ALL LINES OF VARIABLE STRENGTH 
      for (w in 1:nrow(strength)) 
      {      
        # compare from and to of labels1 with from and to of strength      
        if (substring(labels1[y],1,3) == substring(strength[w,1],1,3) &&
            substring(labels1[y],5,7) == substring(strength[w,2],1,3))
        {
          # BUILD A STRING TO TAKE THE INTERCEPT OF THE ARC
          intercept<-paste("ft", "$", substring(labels1[y],5,7), "$coefficients", "[1]", sep="")
          # Take all coefficients
          coefficientsG <- paste("ft", "$", substring(labels1[y],5,7), "$coefficients", sep="")
          coefficientsG <- eval(parse(text=coefficientsG)) 
          
          for (z in 2:10)
          {
            # when ther is no more coefficients it leaves from for loop
            if (is.na(names(coefficientsG[z]))) break
            # verify from where is incomming arc to take correctly regression coefficient 
            if (substring(labels1[y],1,3) == names(coefficientsG[z]))
            {
              coefficients <- paste("ft", "$", substring(labels1[y],5,7), "$coefficients[",z,"]", sep="")
            }
          }
          # TRANSFORM AN INTERCEPT IN A COMMAND
          intercept<-eval(parse(text=intercept))  
          coefficients<-eval(parse(text=coefficients))      
          # store the strength value of each connection from -> to to show in the edges
          labels[y] <- paste((toString(round(strength[w,3],2))), "(s) ", toString(round(intercept,2)), "(i) ", 
                              toString(round(coefficients,2)),"(c)",sep="")
          # GET OUT OF LOOPING WHEN CONDITION IS SATISFIED
          break
        } # if (substring(labels1[x],1,3) == substring(strength[y,1],1,3) &&
      }# for (w in 1:nrow(strength))
    } # for (y in 1:length(labels1)) 
    
    # Insert name of connection to labels to put each value on right edge
    names(labels) <- labels1
    
    # creates a graph object with new parameters
    pp <- layoutGraph(pp, edgeAttrs=list(label=labels))
    
    Title= paste0("Suicide",x," - ",atitle[x],".png")
    
    # Insert title and subtitle, size and color of it  
    graph.par(list(graph=list(main=Title,
                              sub="\nEDU=Education, INF=Informality, INC=Income,UNP=Unemployed, \nCLB=Chil Labor,HDI=Human Development Index, SCD=Suicide", 
                              cex.main=1.4, cex.sub=1.0, col.sub="black")))
    
    # PLOT AND SAVE THE GRAPH
    png(filename= paste0("Suicide",x), units="in", width=10, height=8, pointsize=12, res=300)
    graph <- renderGraph(pp)
    dev.off()
    
} # for (x in 1:nu_bns)

# BIC and BDe assign higher scores to DAGs that fit the data better2
score(bn, data = directed_network, type = "bge", iss = 10)

# Shows the BN parameters - Intercept of node and regression coefficient for each parent
ft

# Whitelist and blacklist for network choosed (29 and 30)
wl = data.frame(from = c("EDU","UNP"), 
                  to = c("UNP", "SCD"))

bl = data.frame(from = c("SCD", "SCD", "SCD", "SCD", "INF"), 
                  to = c("INC", "INF", "UNP", "EDU", "INC"))

# Using bootstrapped samples to validate the strengths of the edges
# Resampling to dsachs and learn a set of 1000 network structures
boot <- boot.strength(directed_network, R = 500, m = nrow(directed_network),algorithm = "hc",algorithm.args =
                      list(score = "bge", iss = 10, whitelist = wl, blacklist = bl))

# the default value for the threshold argument in averaged.network. 
# It is used when we do not specify threshold ourselves
averaged.network(boot)

boot[(boot$strength > 0.85) & (boot$direction >= 0.5), ]

avg.boot <- averaged.network(boot, threshold = 0.54)
avg.boot <- skeleton(avg.boot)
#plot(avg.boot)

nodes <- names(directed_network)

start <- random.graph(nodes = nodes, method = "ic-dag", num = 500, every = 50)
netlist <- lapply(start, function(net) 
  {
  hc(directed_network, score = "bge", iss = 10, start = net)
  })
rnd <- custom.strength(netlist, nodes = nodes)
rnd[(rnd$strength > 0.54) & (rnd$direction >= 0.5), ]
avg.start <- averaged.network(rnd, threshold = 0.54)

#========== EXECUTA REGRESSÃO PARA ANALISAR O MODELO =========================================
mean(directed_network$EDU)
#===================================================
lmINC <- lm(INC ~ EDU, data=directed_network)
lmINC

lmINCStandard <- lm.beta(lmINC)
lmINCStandard

confint(lmINC)

#===================================================
lmUNP <- lm(UNP ~ INC+EDU, data=directed_network)
lmUNP

lmUNPStandard <- lm.beta(lmUNP)
lmUNPStandard

confint(lmUNP)

#===================================================
lmINF <- lm(INF ~ INC, data=directed_network)
lmINF

lmINFStandard <- lm.beta(lmINF)
lmINFStandard

confint(lmINF)

#===================================================
lmSCD <- lm(SCD ~ INF+UNP+EDU, data=directed_network)
lmSCD

lmSCDStandard <- lm.beta(lmSCD)
lmSCDStandard

confint(lmSCD)
#===================================================
