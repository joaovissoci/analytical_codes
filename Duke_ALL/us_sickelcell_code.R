#################################################################
#IHD GEO EPI - BRAZIL
#################################################################
#
#
#
#
#
#################################################################
#SETTING ENVIRONMENT
#################################################################
#All packages must be installes with install.packages() function
lapply(c("ggplot2", "psych", "RCurl", "irr", "nortest", 
	"moments","GPArotation","nFactors","boot","psy", "car",
	"vcd", "gridExtra","mi","VIM","epicalc","gdata","sqldf",
	"reshape2","mclust","foreign","survival","memisc","lme4",
	"lmerTest","dplyr","meta","xlsx"), 
library, character.only=T)

install.packages("readxl") # CRAN version
install.packages("writexl") # CRAN version


library("readxl")
library("writexl")

#################################################################
#IMPORTING DATA
#################################################################
#LOADING DATA FROM A .CSV FILE
# data<-read.csv("/Users/Joao/Box Sync/Home Folder jnv4/Data/DUEM/sicke_cell/us_dukescikecell_data.csv")

data<-read_excel("/Users/Joao/Box Sync/Home Folder jnv4/Data/DUEM/sicke_cell/REDCAPreport.xlsx")

#information between " " are the path to the directory in your computer where the data is stored

#Import data from Dropbox, in .csv format
#Instructions here http://goo.gl/Ofa7gQ
#data1 <- repmis::source_DropboxData(".csv",
#                                  "",
#                                  sep = ",",
#                                  header = TRUE)
#############################################################################
#DATA MANAGEMENT
#############################################################################

names(data)

time_recode_data<-data.frame(
						data[20],
						data[24],
						data[26],
						data[40],
						data[42])

colnames(time_recode_data)<-c("time_arrival",
							  "time_room_assignment",
							  "time_ceu",
							  "time_first_drug",
							  "time_pca")

data$time_first_dose_drug<-with(time_recode_data,time_first_drug-time_arrival)/60
data$time_first_pca<-with(time_recode_data,time_pca-time_arrival)
data$ed_LOS<-with(time_recode_data,time_ceu-time_room_assignment)/60

write_xlsx(data,"/Users/Joao/Desktop/sicke_cell_data.xlsx",col_names=TRUE)

#############################################################################
#BASIC DESCRIPTIVES and EXPLORATORY ANALYSIS
#############################################################################


##############################################################################
#END
##############################################################################