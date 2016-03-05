#############################################################################
#leadership_profiles.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. see full license at the end of this file.
##############################################################################
#this script follows a combination of the guidelines proposed by Hadley Wickham http://goo.gl/c04kq as well as using the formatR package http://goo.gl/ri6ky
#if this is the first time you are conducting an analysis using this protocol, please watch http://goo.gl/DajIN while following step by step

#link to manuscript

#############################################################################
#SETTING ENVIRONMENT
#############################################################################
 #install.packages("VIM")
 #install.packages("VIMGUI")
 #install.packages("miP")
 #install.packages("gWidgetsRGtk2")
 #install.packages("mi")
 #install.packages("epicalc")

#Load packages neededz for the analysis
#All packages must be installes with install.packages() function
lapply(c("sem","ggplot2", "psych", "irr", "nortest", "moments","GPArotation",
  "nFactors","boot","psy", "car","vcd", "gridExtra","mi","VIM","epicalc",
  "gdata","mclust","reshape","repmis","memisc"), 
library, character.only=T)
#############################################################################
#IMPORTING DATA
#############################################################################

#uploading data ---------------------------------------------------------------------
#Load the data set.
#All data are stored in  http://figshare.com/articles/The_reliability_of_AO_classification_on_femur_fractures_among_orthopedic_residents/103664
#Note that the data set has been reorganized to be applied to some functions

#Functions to pull the dara from the internet file 
#see http://goo.gl/mQwxO on how to get this link
#webdata <- getURL("https://docs.google.com/spreadsheet/pub?key=0ArVW3PoO2euydHFkeHVMd3FyeVlkZE1ySlc2bWEwZFE&single=true&gid=1&output=csv"
#,ssl.verifypeer = FALSE)
#data<-read.csv(textConnection(webdata))

#data <- as.data.set(spss.system.file('/Users/joaovissoci/Desktop/dados_sem_leaderspihp_profile.sav'))
#data<-as.data.frame(data)

data_entry <- repmis::source_DropboxData("[DGHI] AGREE_SUMMARY_SCORES.csv","0ww7uuvzdbgufq8",sephttps://drive.google.com/open?id=0B4TReYGK49h_WmRPbDNUc1dEcDg = ",",header = TRUE)

data2<-read.csv("/home/joao/Dropbox/datasets/DGNN/TBI_quality_SR/data_barplot_AGREE.csv",sep=",")

#####################################################################################
#DATA MANAGEMENT
#####################################################################################
data<-remove.vars(data_entry,c("GUIDELINE","Name","Year"))

data$author<-NULL
data$author<-apply(cbind(as.character(data_entry$Name), as.character(data_entry$Year)), 1, paste, collapse=", ")

#data<-data[-15,]

#####################################################################################
#DescriptivesE as férias
#####################################################################################
psych::describe(data)

############################################################################
#Figure 1. Plot 1
############################################################################
#tiff("/Users/rpietro/Desktop/tbi_cpG_quality.tiff", width = 1000, height = 500,compression = 'lzw')
#qplot(value, reorder(item,value), data=data_new, fill=value, size=value,colour=value) + facet_grid(~score, scales="free_y", space = "free") + scale_size_area()+  xlab("AGREE Score") + ylab ("")+ theme(legend.position = "none")


#teste
library(ggplot2) #usar para carregar o pacote
library(reshape2)
#cor_data<-melt(data,by="State")
data_plot$value2<-round(data_plot$value,digits=2)

tiff("/home/joao/Desktop/tbi_try7.tiff", width = 700, height = 500,compression = 'lzw')
# heatmap by regions
ggplot(data_plot, aes(y=author, x=variable)) + geom_tile(fill=data_plot$color) + geom_text(aes(y=author, x=variable, label=value2, size=200)) 
avseq <- ggplot(data_plot, aes(y=author, x=variable)) + geom_tile(fill=data_plot$color) + geom_text(aes(y=author, x=variable, label=value2, size=200)) + theme_minimal() + xlab(label="Domains") + ylab(label="CPG")
levels<-c("SIGN, 2009","NZ, 2006","NICE, 2007","SCN, 2013","ACEP, 2009","NIHCE, 2007","BTF, 2012","NSW MoH, 2011","BTF/AANS, 2000","BTF/AANS, 1995","AAP/AAFP, 1999","AAP, 2001","BTF, 2000","RHSA, 2008","CMA, 2007","ACEP, 2002","BTF, 2007","EAST, 2002","Taiwan, 2009","EFNS, 2001","EFNS, 2002","JSN, 2006","EBIC, 1997","EAST, 2012","SINch/SIAARTI, 2000","CHOP, 2003","USP/BSN, 2001","ESICM, 1998","SINch, 1996")
revLevels<-rev(levels)
avseq + scale_y_discrete(limits=revLevels)
summary (data_plot)
data_plot$color<-NULL
data_plot$color[data_plot$value >= 28.30 & data_plot$value < 55.88]="gray90"
data_plot$color[data_plot$value >= 55.88 & data_plot$value < 69.75]="gray75"
data_plot$color[data_plot$value >= 69.75 & data_plot$value < 81.97]="gray50"
data_plot$color[data_plot$value >= 81.97]="gray40"
dev.off()

data_plot$color<-NULL
data_plot$color[data_plot$value <= 40] ="white"
data_plot$color[data_plot$value > 40 & data_plot$value <= 80]="blue"
data_plot$color[data_plot$value > 80 ]="black"

data_barplot<-remove.vars(data2,c("fi","A1","A2","A3","A4","A5","TOTAL_SUM","OA_MIN","OA_MAX"))

data_plot<-melt(data_barplot,id=c("author"))
#data$item<-NULL
#data$item<-car::recode(data$variable,"'internal_consistency'='reliability';'test_retest'='reliability';'content_validity'='content_validity';'structural_validity'='structural_validity';'discriminant_validity'='construct_validity';'convergent_validity'='construct_validity';'predictive_validity'='construct_validity';'responsiveness'='responsiveness'")
#data$family<-c(rep(c('specific'),100),rep(c('nonspecific'),100))
data_plot$family<-NULL
data_plot$family<-c("Guidelines")

data_new<-NULL
data_new$family<-data_plot$family
data_new$item<-data_plot$Name
data_new$score<-data_plot$variable
data_new$value<-data_plot$value
data_new<-as.data.frame(data_new)

tiff("C:\\Users\\Joao\\Desktop\\tbi_try1.tiff", width = 700, height = 500,compression = 'lzw')
polarBarChart(data_new,  familyLabel=TRUE, direction="inwards", binSize=0.3, spaceBar=0, spaceItem=0.3, spaceFamily=1.0, innerRadius=0.1,nguides=5,circleProportion=0.8,legLabels=c("D1","D2","D3","D4","D5","D6","Avarage"),legTitle="AGREE Domains")
dev.off()


lapply(c("sem","ggplot2", "psych", "irr", "nortest", 
	"moments","GPArotation","nFactors","boot","psy", 
	"car","vcd", "gridExtra","mi","VIM","gdata",
	"mclust","reshape","repmis","memisc"), 
library, character.only=T)

data <-read.csv("/Users/oper/Downloads/AGREE_DOMAINS_SCORES - Summary (6).csv",header=T)
data_entry <-remove.vars(data,c("Fi","Name","Year"))

data_entry$author<-NULL
data_entry$author<-apply(cbind(as.character(data$Name), as.character(data$Year)), 1, paste, collapse=", ")
psych::describe(data_entry)
data_plot<-melt(data_entry,id=c("author"))
data_plot$family<-NULL
data_plot$family<- rowMeans (data_entry)
data_new<-NULL
data_new$family<-data_plot$family
data_new$item<-data_plot$author
data_new$score<-data_plot$variable
data_new$value<-data_plot$value
data_new<-as.data.frame(data_new)
library(ggplot2)
library(reshape2)
data_plot$value2<-round(data_plot$value,digits=2)

ggplot(data_plot, aes(y=author, x=variable)) + geom_tile(fill=data_plot$color) + geom_text(aes(y=author, x=variable, label=value2)) theme(text = element_text(size=16))
avseq <- ggplot(data_plot, aes(y=author, x=variable)) + geom_tile(fill=data_plot$color) + geom_text(aes(y=author, x=variable, label=value2)) + theme_minimal() + xlab(label="Domains") + ylab(label="CPG") + theme(text = element_text(size=16)) 
levels<-c("SIGN, 2009","NZ, 2006","SCN, 2013",
          "ACEP, 2009","NIHCE, 2007","BTF, 2012","NSW MoH, 2011",
          "AAP/AAFP, 1999","AAP, 2001",
          "RHSA, 2008","CMA, 2007","BTF, 2007",
          "EAST, 2002","Taiwan, 2009","EFNS, 2011",
          "JSN, 2006","EBIC, 1997","EAST, 2012","SINch/SIAARTI, 2000",
          "CHOP, 2003","USP/BSN, 2001","ESICM, 1998","SINch, 1996")
revLevels<-rev(levels)
avseq + scale_y_discrete(limits=revLevels)
summary (data_plot)
data_plot$color<-NULL
data_plot$color[data_plot$value >= 28.30 & data_plot$value < 55.88]="lightcyan4"
data_plot$color[data_plot$value >= 55.88 & data_plot$value < 69.75]="lightcyan3"
data_plot$color[data_plot$value >= 69.75 & data_plot$value < 81.97]="lightcyan2"
data_plot$color[data_plot$value >= 81.97]="lightcyan1"
summary(data_plot)