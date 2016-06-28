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
lapply(c("sem","ggplot2", "psych", "irr", "nortest", "moments",
  "GPArotation","nFactors","boot","psy", "car","vcd", "gridExtra",
  "mi","VIM","epicalc",
  "gdata","mclust","reshape","repmis","memisc"), 
library, character.only=T)
#############################################################################
#IMPORTING DATA
#############################################################################
data <-read.csv("/Users/joaovissoci/OneDrive - Duke University/datasets/DGNN/TBI_quality_SR/agree_domain.csv",header=T)

data_raters <-read.csv("/Users/joaovissoci/OneDrive - Duke University/datasets/DGNN/TBI_quality_SR/agree_domain_raters.csv",header=T)

#####################################################################################
#DATA MANAGEMENT
#####################################################################################

data_entry <-remove.vars(data,c("Name","Year","location"))

#####################################################################################
#Descriptives
#####################################################################################

############################################################################
#Figure 2. AGREE Scoring by domain for each Clinical Practice Guidelines
############################################################################


data_entry$author<-NULL
data_entry$author<-apply(cbind(as.character(data$Name),
  as.character(data$Year)), 1, paste, collapse=", ")
data_plot<-melt(data_entry,id=c("author"))
data_plot$value2<-round(data_plot$value,digits=2)

levels<-as.factor(data_entry$author[order(-data$Overall)])

data_plot$color<-NULL
data_plot$color[data_plot$value >= 28.30 & data_plot$value < 55.88]="lightcyan4"
data_plot$color[data_plot$value >= 55.88 & data_plot$value < 69.75]="lightcyan3"
data_plot$color[data_plot$value >= 69.75 & data_plot$value < 81.97]="lightcyan2"
data_plot$color[data_plot$value >= 81.97]="lightcyan1"

tiff("/Users/joaovissoci/Desktop/agree_domains_scores.tiff",
  width = 800, height = 600,compression = 'lzw')
avseq <- ggplot(data_plot[1:144,], aes(y=author, x=variable)) + 
  geom_tile(fill=data_plot[1:144,]$color) + 
  geom_text(aes(y=author, x=variable, label=value2)) + 
  theme_minimal() + 
  xlab(label="Domains") + 
  ylab(label="CPG") + 
  theme(text = element_text(size=16))  + 
  scale_y_discrete(limits=rev(levels))
avseq
dev.off()

############################################################################
#Table 3. Inter-rater reliability for each quality domain.
############################################################################
str(data_raters)

#DOMAIN 1
raters1_1<-with(data_raters,rowMeans(
  data.frame(D1_ITEM1_A1,D1_ITEM2_A1,D1_ITEM3_A1)))
raters2_1<-with(data_raters,rowMeans(
  data.frame(D1_ITEM1_A2,D1_ITEM2_A2,D1_ITEM3_A2)))
raters3_1<-with(data_raters,rowMeans(
  data.frame(D1_ITEM1_A3,D1_ITEM2_A3,D1_ITEM3_A3)))
raters4_1<-with(data_raters,rowMeans(
  data.frame(D1_ITEM1_A4,D1_ITEM2_A4,D1_ITEM3_A4)))
raters5_1<-with(data_raters,rowMeans(
  data.frame(D1_ITEM1_A5,D1_ITEM2_A5,D1_ITEM3_A5)))

ICC(data.frame(raters1_1,raters2_1,raters3_1,
  raters4_1,raters5_1))

#DOMAIN 2
raters1_2<-with(data_raters,rowMeans(
  data.frame(D2_ITEM1_A1,D2_ITEM2_A1,D2_ITEM3_A1)))
raters2_2<-with(data_raters,rowMeans(
  data.frame(D2_ITEM1_A2,D2_ITEM2_A2,D2_ITEM3_A2)))
raters3_2<-with(data_raters,rowMeans(
  data.frame(D2_ITEM1_A3,D2_ITEM2_A3,D2_ITEM3_A3)))
raters4_2<-with(data_raters,rowMeans(
  data.frame(D2_ITEM1_A4,D2_ITEM2_A4,D2_ITEM3_A4)))
raters5_2<-with(data_raters,rowMeans(
  data.frame(D2_ITEM1_A5,D2_ITEM2_A5,D2_ITEM3_A5)))

ICC(data.frame(raters1_2,raters2_2,raters3_2,
  raters4_2,raters5_2))

#DOMAIN 3
raters1_3<-with(data_raters,rowMeans(
  data.frame(D3_ITEM1_A1,D3_ITEM2_A1,D3_ITEM3_A1,
    D3_ITEM4_A1,D3_ITEM5_A1,D3_ITEM6_A1,D3_ITEM7_A1,
    D3_ITEM8_A1)))
raters2_3<-with(data_raters,rowMeans(
  data.frame(D3_ITEM1_A2,D3_ITEM2_A2,D3_ITEM3_A2,
    D3_ITEM4_A1,D3_ITEM5_A1)))
raters3_3<-with(data_raters,rowMeans(
  data.frame(D3_ITEM1_A3,D3_ITEM2_A3,D3_ITEM3_A3,
    D3_ITEM4_A1,D3_ITEM5_A1)))
raters4_3<-with(data_raters,rowMeans(
  data.frame(D3_ITEM1_A4,D3_ITEM2_A4,D3_ITEM3_A4,
    D3_ITEM4_A1,D3_ITEM5_A1)))
raters5_3<-with(data_raters,rowMeans(
  data.frame(D3_ITEM1_A5,D3_ITEM2_A5,D3_ITEM3_A5,
    D3_ITEM4_A1,D3_ITEM5_A1)))

ICC(data.frame(raters1_3,raters2_3,raters3_3,
  raters4_3,raters5_3))

#DOMAIN 4
raters1_4<-with(data_raters,rowMeans(
  data.frame(D4_ITEM1_A1,D4_ITEM2_A1,D4_ITEM3_A1)))
raters2_4<-with(data_raters,rowMeans(
  data.frame(D4_ITEM1_A2,D4_ITEM2_A2,D4_ITEM3_A2)))
raters3_4<-with(data_raters,rowMeans(
  data.frame(D4_ITEM1_A3,D4_ITEM2_A3,D4_ITEM3_A3)))
raters4_4<-with(data_raters,rowMeans(
  data.frame(D4_ITEM1_A4,D4_ITEM2_A4,D4_ITEM3_A4)))
raters5_4<-with(data_raters,rowMeans(
  data.frame(D4_ITEM1_A5,D4_ITEM2_A5,D4_ITEM3_A5)))

ICC(data.frame(raters1_4,raters2_4,raters3_4,
  raters4_4,raters5_4))

#DOMAIN 5
raters1_5<-with(data_raters,rowMeans(
  data.frame(D5_ITEM1_A1,D5_ITEM2_A1,D5_ITEM3_A1)))
raters2_5<-with(data_raters,rowMeans(
  data.frame(D5_ITEM1_A2,D5_ITEM2_A2,D5_ITEM3_A2)))
raters3_5<-with(data_raters,rowMeans(
  data.frame(D5_ITEM1_A3,D5_ITEM2_A3,D5_ITEM3_A3)))
raters4_5<-with(data_raters,rowMeans(
  data.frame(D5_ITEM1_A4,D5_ITEM2_A4,D5_ITEM3_A4)))
raters5_5<-with(data_raters,rowMeans(
  data.frame(D5_ITEM1_A5,D5_ITEM2_A5,D5_ITEM3_A5)))

ICC(data.frame(raters1_5,raters2_5,raters3_5,
  raters4_5,raters5_5))

#DOMAIN 6
raters1_6<-with(data_raters,rowMeans(
  data.frame(D6_ITEM1_A1,D6_ITEM2_A1)))
raters2_6<-with(data_raters,rowMeans(
  data.frame(D6_ITEM1_A2,D6_ITEM2_A2)))
raters3_6<-with(data_raters,rowMeans(
  data.frame(D6_ITEM1_A3,D6_ITEM2_A3)))
raters4_6<-with(data_raters,rowMeans(
  data.frame(D6_ITEM1_A4,D6_ITEM2_A4)))
raters5_6<-with(data_raters,rowMeans(
  data.frame(D6_ITEM1_A5,D6_ITEM2_A5)))

ICC(data.frame(raters1_6,raters2_6,raters3_6,
  raters4_6,raters5_6))

#OVERALL ASSESSMENT
ICC(with(data_raters,data.frame(A1R,A2R,A3R,A4R,A5R)))
