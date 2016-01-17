######################################################################################
#leadership_profiles.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. see full license at the end of this file.
#######################################################################################
#this script follows a combination of the guidelines proposed by Hadley Wickham http://goo.gl/c04kq as well as using the formatR package http://goo.gl/ri6ky
#if this is the first time you are conducting an analysis using this protocol, please watch http://goo.gl/DajIN while following step by step

#link to manuscript

#####################################################################################
#SETTING ENVIRONMENT
#####################################################################################
 #install.packages("VIM")
 #install.packages("VIMGUI")
 #install.packages("miP")
 #install.packages("gWidgetsRGtk2")
 #install.packages("mi")
 #install.packages("epicalc")

#Load packages neededz for the analysis
#All packages must be installes with install.packages() function
lapply(c("sem","ggplot2", "psych", "irr", "nortest", "moments","GPArotation","nFactors","boot","psy", "car","vcd", "gridExtra","mi","VIM","epicalc","gdata","mclust","reshape","repmis","memisc"), library, character.only=T)
#####################################################################################
#IMPORTING DATA
#####################################################################################

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

data_entry <- repmis::source_DropboxData("[DGHI] AGREE_SUMMARY_SCORES.csv","0ww7uuvzdbgufq8",sep = ",",header = TRUE)

#####################################################################################
#DATA MANAGEMENT
#####################################################################################
data<-remove.vars(data_entry,c("GUIDELINE","Name","Year"))

data$author<-NULL
data$author<-apply(cbind(as.character(data_entry$Name), as.character(data_entry$Year)), 1, paste, collapse=", ")

#data<-data[-15,]

#####################################################################################
#Descriptives
#####################################################################################
psych::describe(data)

############################################################################
#Figure 1. Plot 1
############################################################################
data_plot<-melt(data,id=c("author"))
#data$item<-NULL
#data$item<-car::recode(data$variable,"'internal_consistency'='reliability';'test_retest'='reliability';'content_validity'='content_validity';'structural_validity'='structural_validity';'discriminant_validity'='construct_validity';'convergent_validity'='construct_validity';'predictive_validity'='construct_validity';'responsiveness'='responsiveness'")
#data$family<-c(rep(c('specific'),100),rep(c('nonspecific'),100))
data_plot$family<-NULL
data_plot$family<-c("Guidelines")

data_new<-NULL
data_new$family<-data_plot$family
data_new$item<-data_plot$author
data_new$score<-data_plot$variable
data_new$value<-data_plot$value
data_new<-as.data.frame(data_new)

#tiff("C:\\Users\\Joao\\Desktop\\tbi_try7.tiff", width = 700, height = 500,compression = 'lzw')
#tiff("/Users/rpietro/Desktop/tbi_cpG_quality.tiff", width = 1000, height = 500,compression = 'lzw')
#qplot(value, reorder(item,value), data=data_new, fill=value, size=value,colour=value) + facet_grid(~score, scales="free_y", space = "free") + scale_size_area()+  xlab("AGREE Score") + ylab ("")+ theme(legend.position = "none")
#dev.off()

#teste

library(ggplot2) #usar para carregar o pacote
library(reshape2)
#cor_data<-melt(data,by="State")
data_plot$value2<-round(data_plot$value,digits=2)

# heatmap by regions
ggplot(data_plot, aes(y=author, x=variable, fill=value2)) + geom_tile() + geom_text(aes(y=author, x=variable, label=value2)) + scale_fill_gradient2( limits=c(20,100))# + facet_grid(regions ~ .,scales="free_y",space="free") 














tiff("C:\\Users\\Joao\\Desktop\\tbi_try1.tiff", width = 700, height = 500,compression = 'lzw')
polarBarChart(data_new,  familyLabel=TRUE, direction="inwards", binSize=0.3, spaceBar=0, spaceItem=0.3, spaceFamily=1.0, innerRadius=0.1,nguides=5,circleProportion=0.8,legLabels=c("D1","D2","D3","D4","D5","D6","Avarage"),legTitle="AGREE Domains")
dev.off()

tiff("C:\\Users\\Joao\\Desktop\\tbi_try2.tiff", width = 700, height = 500,compression = 'lzw')
#polarBarChart_stacked(data_new)
polarHistogram(data_new) + theme(
      panel.background=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank()
    )
dev.off()
###########################################################
#Heatmap
###########################################################
#attach(qualitydata)
#Generate ggplot graph fro quality data information
tiff("C:\\Users\\Joao\\Desktop\\tbi_try3.tiff", width = 700, height = 500,compression = 'lzw')
ggplot(data_new, aes(score, item)) + geom_tile(aes(fill = value)) + scale_fill_continuous(name="AGREE Score", limits=c(28.30,100),low='white', high="black") +
 theme(axis.text.x = element_text(hjust = 0, colour = "black",size=14, angle=-30),
        axis.text.y = element_text(colour = "black",size=14),
        axis.title.x = element_text(face="bold",size=14),
        axis.title.y = element_text(face="bold", size=14))+theme_bw()#+
dev.off()

#scale_x_discrete(breaks=levels(qualitydata$variable),labels=c("Item generation","Population description","Population size","Questionnaire description","Scaling and scoring","Period recall","Item reduction","Internal consistency","Test-retest","Content validity","Structural validity","Discriminant validity","Convergent validity","Predictive validity","Responsiveness"),name="Psychometric Properties")+
#facet_grid(. ~ am)


tiff("C:\\Users\\Joao\\Desktop\\tbi_try4.tiff", width = 700, height = 500,compression = 'lzw')
qplot(value, item, data=data_new, fill=value, size=value, colour=value) + 
 facet_grid(~score, scales="free_y", space = "free") +
  scale_size_area()+  xlab("AGREE Score") + ylab ("")+ theme(legend.position = "none") +theme_bw(
 dev.off()




data_graph<-t(data[-1])
colnames(data_graph)<-data$GUIDELINE
graph<-cor(data_graph)

tiff("C:\\Users\\Joao\\Desktop\\tbi_try5.tiff", width = 700, height = 500,compression = 'lzw')
network_meta <- qgraph(graph,layout = "spring",minimum=0.5,cut=1.0,vsize=(data$AVARAGE/12)) #,label.scale=FALSE,label.cex = label.cex,vsize=size_edges,color=color,shape=shape,greyscale=T)
dev.off()
#legend(0.8,-0.8, bty="n",c("Methodological Issues","Clinical Issues","Side-Effects"),cex=1.2,fill=c("steelblue","red","lightgreen"))

tiff("C:\\Users\\Joao\\Desktop\\tbi_try6.tiff", width = 700, height = 1200,compression = 'lzw')
ggplot(data =  data_new, aes(x = score, y = item)) +
  geom_tile(aes(fill = value), colour = "white") +
  geom_text(aes(label = sprintf("%1.2f",value)), vjust = 1) +
  scale_fill_gradient(low = "white", high = "steelblue") +theme_bw()
dev.off()


