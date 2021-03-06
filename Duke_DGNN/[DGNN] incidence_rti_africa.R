###################################################
#TEMPLATE_FOR _META_ANALYSIS_OF_DIAGNOSTIC_ACCURACY#
#this script follows a combination of guidelines proposed by Doebler and Holling, according to (http://cran.r-project.org/web/packages/mada/vignettes/mada.pdf)#
#
#
###################################################
#SETTING ENVIRONMENT
###################################################
#Load packages (after installed) with the library function
lapply(c("metafor","ggplot2","gridExtra" ,"psych", "RCurl", "irr", "nortest", "moments","GPArotation","nFactors","gdata","meta","metafor","ggplot2","gridExtra" ,"psych", "RCurl", "irr", "nortest", "moments","GPArotation","nFactors","gdata"), library, character.only=T)

###################################################
#IMPORTING DATA AND RECODING
###################################################
#Instructions here http://goo.gl/Ofa7gQ
#data <- repmis::source_DropboxData("rti_sr_data.csv","yr0yf1szzyqji35",sep = ",",header = TRUE)

data<-read.csv("/home/joao/Dropbox/datasets/DGHI/Africa_DGHI/RTI SSA SR/rti_sr_data.csv",sep=',')

###################################################
#OVERALL ANALYSIS
###################################################

#TOTAL RTI Vs. TOTAL RTI DEATHS
#Aggregating data
meta_rti<-with(data,data.frame(total_rti,total_rti_death,Author))
meta_rti<-na.omit(meta_rti)

#Calculating metanalysis
m3<-metaprop(total_rti_death,total_rti,sm="PLN",data=meta_rti,studlab=Author,comb.fixed=FALSE)
 
tiff("/home/joao/Desktop/rti_deaths_overall.tiff", width = 700, height = 800,compression = 'lzw')
meta::forest(m3)
dev.off()
metainf(m3)
metainf(m3, pooled="random")
funnel(m3)

#TOTAL TRAUMA Vs. TOTAL RTI
#Aggregating data
meta_trauma<-with(data,data.frame(total_traume,total_rti,Author))
meta_trauma<-na.omit(meta_trauma)

#Calculating metanalysis
m3<-metaprop(total_rti,total_traume,sm="PLN",data=meta_trauma,studlab=Author,comb.fixed=FALSE)
 
tiff("/home/joao/Desktop/rti_trauma_overall.tiff", width = 700, height = 1200,compression = 'lzw')
meta::forest(m3)
dev.off()
metainf(m3)
metainf(m3, pooled="random")
funnel(m3)

###################################################
#BY COUNTRY
###################################################
#TOTAL RTI Vs. TOTAL RTI DEATHS

meta_bycoutry<-with(data,data.frame(total_rti,total_rti_death,Author,country))
meta_bycoutry<-na.omit(meta_bycoutry)
meta_bycoutry<-as.matrix(meta_bycoutry)
meta_bycoutry<-as.data.frame(meta_bycoutry)
meta_bycoutry$total_rti<-as.numeric(as.character(meta_bycoutry$total_rti))
meta_bycoutry$total_rti_death<-as.numeric(as.character(meta_bycoutry$total_rti_death))


m3_prev<-(meta_bycoutry$total_rti_death/meta_bycoutry$total_rti)
by(m3_prev,meta_bycoutry$country,median)
prop_country1<-as.character(meta_bycoutry$country)

m3<-metaprop(total_rti_death,total_rti,sm="PLN",byvar=country,data=meta_bycoutry,studlab=Author,comb.fixed=FALSE)

tiff("/home/joao/Desktop/rti_deaths_by_country.tiff", width = 700, height = 1500,compression = 'lzw')
meta::forest(m3)
dev.off()


#TOTAL TRAUMA Vs. TOTAL RTI

meta_bycoutry<-with(data,data.frame(total_traume,total_rti,Author,country))
meta_bycoutry<-na.omit(meta_bycoutry)
meta_bycoutry<-as.matrix(meta_bycoutry)
meta_bycoutry<-as.data.frame(meta_bycoutry)
meta_bycoutry$total_rti<-as.numeric(as.character(meta_bycoutry$total_rti))
meta_bycoutry$total_traume<-as.numeric(as.character(meta_bycoutry$total_traume))

m4_prev<-(meta_bycoutry$total_rti/meta_bycoutry$total_traume)
by(m3_prev,meta_bycoutry$country,median)
prop_country2<-as.character(meta_bycoutry$country)

m3<-metaprop(total_rti,total_traume,sm="PLN",byvar=country,data=meta_bycoutry,studlab=Author,comb.fixed=FALSE)

tiff("/home/joao/Desktop/rti_trauma_by_country.tiff", width = 700, height = 2000,compression = 'lzw')
meta::forest(m3)
dev.off()

###################################################
#BY TYPE OF INJURY
###################################################
#TOTAL RTI Vs. TOTAL RTI DEATHS

meta_byinjury<-with(data,data.frame(total_rti,total_rti_death,Author,type_injury))
meta_byinjury<-na.omit(meta_byinjury)
meta_byinjury<-as.matrix(meta_byinjury)
meta_byinjury<-as.data.frame(meta_byinjury)
meta_byinjury$total_rti<-as.numeric(as.character(meta_byinjury$total_rti))
meta_byinjury$total_rti_death<-as.numeric(as.character(meta_byinjury$total_rti_death))

tiff("/home/joao/Desktop/rti_deaths_by_injury.tiff", width = 700, height = 1500,compression = 'lzw')
m3<-metaprop(total_rti_death,total_rti,sm="PLN",byvar=type_injury,data=meta_byinjury,studlab=Author,comb.fixed=FALSE)
meta::forest(m3)
dev.off()

#TOTAL TRAUMA Vs. TOTAL RTI

meta_byinjury<-with(data,data.frame(total_traume,total_rti,Author,type_injury))
meta_byinjury<-na.omit(meta_byinjury)
meta_byinjury<-as.matrix(meta_byinjury)
meta_byinjury<-as.data.frame(meta_byinjury)
meta_byinjury$total_rti<-as.numeric(as.character(meta_byinjury$total_rti))
meta_byinjury$total_traume<-as.numeric(as.character(meta_byinjury$total_traume))

tiff("/home/joao/Desktop/rti_trauma_by_injury.tiff", width = 700, height = 1500,compression = 'lzw')
m3<-metaprop(total_rti,total_traume,sm="PLN",byvar=type_injury,data=meta_byinjury,studlab=Author,comb.fixed=FALSE)
meta::forest(m3)
dev.off()

###################################################
#BY AGE GROUPS
####################################################
#TOTAL RTI Vs. TOTAL RTI DEATHS
meta_byage_groups<-with(data,data.frame(total_rti,total_rti_death,Author,age_groups))
meta_byage_groups<-na.omit(meta_byage_groups)
meta_byage_groups<-as.matrix(meta_byage_groups)
meta_byage_groups<-as.data.frame(meta_byage_groups)
meta_byage_groups$total_rti<-as.numeric(as.character(meta_byage_groups$total_rti))
meta_byage_groups$total_rti_death<-as.numeric(as.character(meta_byage_groups$total_rti_death))
tiff("/home/joao/Desktop/rti_deaths_by_age_groups.tiff", width = 800, height = 1500,compression = 'lzw')
m3<-metaprop(total_rti_death,total_rti,sm="PLN",byvar=age_groups,data=meta_byage_groups,studlab=Author,comb.fixed=FALSE,comb.random=FALSE,print.byvar=FALSE)
meta::forest(m3)
dev.off()

#TOTAL TRAUMA Vs. TOTAL RTI

meta_byage_groups<-with(data,data.frame(total_traume,total_rti,Author,age_groups))
meta_byage_groups<-na.omit(meta_byage_groups)
meta_byage_groups<-as.matrix(meta_byage_groups)
meta_byage_groups<-as.data.frame(meta_byage_groups)
meta_byage_groups$total_rti<-as.numeric(as.character(meta_byage_groups$total_rti))
meta_byage_groups$total_traume<-as.numeric(as.character(meta_byage_groups$total_traume))
tiff("/home/joao/Desktop/rti_trauma_by_age_groups.tiff", width = 800, height = 1500,compression = 'lzw')
m3<-metaprop(total_rti,total_traume,sm="PLN",byvar=age_groups,data=meta_byage_groups,studlab=Author,comb.fixed=FALSE,comb.random=FALSE,print.byvar=FALSE)
meta::forest(m3)
dev.off()

###################################################
#BY TYPE OF POPULATION (
####################################################
#TOTAL RTI Vs. TOTAL RTI DEATHS
meta_bypopulation<-with(data,data.frame(total_rti,total_rti_death,Author,type_population,proportion_death))
meta_bypopulation$type_population<-car::recode(meta_bypopulation$type_population,"'' =NA")
meta_bypopulation<-na.omit(meta_bypopulation)
meta_bypopulation<-as.matrix(meta_bypopulation)
meta_bypopulation<-as.data.frame(meta_bypopulation)
meta_bypopulation$total_rti<-as.numeric(as.character(meta_bypopulation$total_rti))
meta_bypopulation$total_rti_death<-as.numeric(as.character(meta_bypopulation$total_rti_death))
meta_bypopulation$proportion_death<-as.numeric(as.character(meta_bypopulation$proportion_death))
tiff("/home/joao/Desktop/rti_deaths_by_population.tiff", width = 700, height = 1500,compression = 'lzw')
m3<-metaprop(total_rti_death,total_rti,sm="PLN",byvar=type_population,data=meta_bypopulation,studlab=Author,comb.fixed=FALSE,comb.random=FALSE,print.byvar=FALSE)
meta::forest(m3)
dev.off()

by(meta_bypopulation$proportion_death,meta_bypopulation$type_population,summary)

#TOTAL TRAUMA Vs. TOTAL RTI

meta_bypopulation<-with(data,data.frame(total_traume,total_rti,Author,type_population,proportion_rti))
meta_bypopulation$type_population<-car::recode(meta_bypopulation$type_population,"'' =NA")
meta_bypopulation<-na.omit(meta_bypopulation)
meta_bypopulation<-as.matrix(meta_bypopulation)
meta_bypopulation<-as.data.frame(meta_bypopulation)
meta_bypopulation$total_rti<-as.numeric(as.character(meta_bypopulation$total_rti))
meta_bypopulation$total_traume<-as.numeric(as.character(meta_bypopulation$total_traume))
meta_bypopulation$proportion_rti<-as.numeric(as.character(meta_bypopulation$proportion_rti))
tiff("/home/joao/Desktop/rti_trauma_by_population.tiff", width = 700, height = 1500,compression = 'lzw')
m3<-metaprop(total_rti,total_traume,sm="PLN",byvar=type_population,data=meta_bypopulation,studlab=Author,comb.fixed=FALSE,comb.random=FALSE,print.byvar=FALSE)
meta::forest(m3)
dev.off()	

by(meta_bypopulation$proportion_rti,meta_bypopulation$type_population,summary)

###################################################
#Separated by time
###################################################
#meta1<-with(data,data.frame(total_rti_death,total_rti,Author,country))
#meta1$country<-as.character(meta1$country)
#meta1<-na.omit(meta1)
#meta1<-meta1[-4,]
#m3<-metaprop(total_rti_death,total_rti,sm="PLN",data=meta1,studlab=Author)

data$year_cat<-car::recode(data$year,"1990:1995='1990-1995';1996:2000='1996-2000';2001:2005='2001-2005';2006:2010='2006-2010';2011:2015='2011-2015';else='< 1990'")

#TOTAL RTI Vs. TOTAL RTI DEATHS
meta_byyear<-with(data,data.frame(total_rti,total_rti_death,Author,year_cat))
meta_byyear<-na.omit(meta_byyear)
meta_byyear<-as.matrix(meta_byyear)
meta_byyear<-as.data.frame(meta_byyear)
meta_byyear$total_rti<-as.numeric(as.character(meta_byyear$total_rti))
meta_byyear$total_rti_death<-as.numeric(as.character(meta_byyear$total_rti_death))

m3_prev<-(meta_byyear$total_rti_death/meta_byyear$total_rti)
m3_year<-meta_byyear$year_cat


m3<-metaprop(total_rti_death,total_rti,sm="PLN",byvar=year_cat,data=meta_byyear,studlab=Author,comb.fixed=FALSE)
 
tiff("/home/joao/Desktop/rti_deaths_byyear.tiff", width = 700, height = 800,compression = 'lzw')
meta::forest(m3)
dev.off()
metainf(m3)
metainf(m3, pooled="random")
funnel(m3)


meta_byyear<-with(data,data.frame(total_traume,total_rti,Author,year_cat))
meta_byyear<-na.omit(meta_byyear)
meta_byyear<-as.matrix(meta_byyear)
meta_byyear<-as.data.frame(meta_byyear)
meta_byyear$total_rti<-as.numeric(as.character(meta_byyear$total_rti))
meta_byyear$total_traume<-as.numeric(as.character(meta_byyear$total_traume))

m4_prev<-(meta_byyear$total_rti/meta_byyear$total_traume)
m4_year<-meta_byyear$year_cat

m4<-metaprop(total_rti,total_traume,sm="PLN",byvar=year_cat,data=meta_byyear,studlab=Author)

#meta_nigeria<-subset(meta_bygroup,meta_bygroup$country=="Nigeria")

tiff("/home/joao/Desktop/rti_trauma_byyear.tiff", width = 700, height = 1200,compression = 'lzw')
forest(m4)
dev.off()
metainf(m3)
metainf(m3, pooled="random")
funnel(m3)



###################################################
#Sensitivity Analysis
###################################################
m_model<-c(rep("RTI",69),rep("Death",43))
m_year<-c(as.character(m4_year),as.character(m3_year))
m_year<-as.factor(m_year)
m_prev<-c(m4_prev,m3_prev)
m_data<-data.frame(m_model,m_year,m_prev)


value<-c(by(m4_prev,m4_year,median),by(m3_prev,m3_year,median))
npapers<-c(m4$k.w,m3$k.w)
dates<-c(m4$bylevs,m3$bylevs)
model<-c(rep("RTI",6),rep("Death",5))

#proportion<-m

graph_data<-data.frame(value,npapers,dates,model)

ggplot(data=graph_data, aes(x=dates, y=value, group=model,color=model)) + geom_line(size=1.5) + geom_point(size=5,fill="white") + ylab("Prevalence (%)") + xlab("Dates") + scale_colour_manual(values=c("black", "#E69F00"), name="")+ theme_bw() + geom_bar(aes(x=dates,y=npapers/100),stat="identity", alpha=0.5, fill="white") + 
annotate("text", x = 1, y = 0.02, label = "2",size=5)+
annotate("text", x = 2, y = 0.17, label = "16",size=5)+
annotate("text", x = 3, y = 0.11, label = "10",size=5)+
annotate("text", x = 4, y = 0.22, label = "21",size=5)+
annotate("text", x = 5, y = 0.29, label = "28",size=5)+
annotate("text", x = 6, y = 0.37, label = "36",size=5) + 
geom_jitter(data=m_data, aes(x=m_year,y=m_prev,group=m_model,color=m_model),size=3,fill="black")





year_cat



















