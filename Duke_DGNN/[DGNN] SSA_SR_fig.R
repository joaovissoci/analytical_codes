###################################################
#TEMPLATE_FOR _META_ANALYSIS_OF_DIAGNOSTIC_ACCURACY#
#this script follows a combination of guidelines proposed by Doebler and Holling, according to (http://cran.r-project.org/web/packages/mada/vignettes/mada.pdf)#
#
#
###################################################
#SETTING ENVIRONMENT
###################################################
#Load packages (after installed) with the library function
lapply(c("metafor","ggplot2","gridExtra" ,"psych", 
	"irr", "nortest", "moments","GPArotation",
	"nFactors","gdata","meta",
	"ggmap","gridExtra",
	"RColorBrewer","maptools","grid",
	"reshape"),
library, character.only=T)

###################################################
#IMPORTING DATA AND RECODING
###################################################
#Instructions here http://goo.gl/Ofa7gQ
#data <- repmis::source_DropboxData("rti_sr_data.csv","yr0yf1szzyqji35",sep = ",",header = TRUE)

data<-read.csv("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/Africa/RTI SSA SR/rti_sr_data.csv",sep=',')

data$study<-with(data,paste(Author,year, sep=", "))

data_country<-read.csv("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/Africa/RTI SSA SR/rti_sr_data.csv",sep=',')

wmap   <- readShapePoly("/Users/joaovissoci/Gits/analytical_codes/shapefiles/africaR/africa_R.shp")

wdata <- read.csv("/Users/joaovissoci/OneDrive - Duke University/datasets/DGHI/Africa/RTI SSA SR/africa.csv", header=TRUE)

wdata$ID_AFR<-c(0:50)



###################################################
#OVERALL ANALYSIS
###################################################

#TOTAL RTI Vs. TOTAL RTI DEATHS
#Aggregating data
meta_rti<-with(data,data.frame(total_rti,
	total_rti_death,Author))
meta_rti<-na.omit(meta_rti)

#Calculating metanalysis
m3<-metaprop(total_rti_death,total_rti,sm="PLN",
	data=meta_rti,studlab=Author,
	comb.fixed=FALSE,
	comb.random=FALSE)
 
tiff("/Users/jnv4/Desktop/rti_deaths_overall.tiff", 
	width = 700, height = 800,compression = 'lzw')
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
m3<-metaprop(total_rti,total_traume,sm="PLN",data=meta_trauma,
	studlab=Author,comb.fixed=FALSE)
 
tiff("/Users/jnv4/Desktop/rti_trauma_overall.tiff", 
	width = 700, height = 1200,compression = 'lzw')
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

tiff("/Users/jnv4/Desktop/rti_deaths_by_injury.tiff", width = 700, height = 1500,compression = 'lzw')
m3<-metaprop(total_rti_death,total_rti,sm="PLN",
	byvar=type_injury,data=meta_byinjury,
	studlab=Author,comb.fixed=FALSE,comb.random=FALSE)
meta::forest(m3)
dev.off()

#TOTAL TRAUMA Vs. TOTAL RTI

meta_byinjury<-with(data,data.frame(total_traume,total_rti,Author,type_injury))
meta_byinjury<-na.omit(meta_byinjury)
meta_byinjury<-as.matrix(meta_byinjury)
meta_byinjury<-as.data.frame(meta_byinjury)
meta_byinjury$total_rti<-as.numeric(as.character(meta_byinjury$total_rti))
meta_byinjury$total_traume<-as.numeric(as.character(meta_byinjury$total_traume))

tiff("/Users/jnv4/Desktop/rti_trauma_by_injury.tiff", width = 700, height = 1500,compression = 'lzw')
m3<-metaprop(total_rti,total_traume,sm="PLN",
	byvar=type_injury,data=meta_byinjury,
	studlab=Author,comb.fixed=FALSE,comb.random=FALSE)
meta::forest(m3)
dev.off()

###################################################
#BY AGE GROUPS
####################################################
#TOTAL RTI Vs. TOTAL RTI DEATHS
meta_byage_groups<-with(data,data.frame(total_rti,
	total_rti_death,Author,age_groups,study))
meta_byage_groups<-na.omit(meta_byage_groups)
meta_byage_groups<-as.matrix(meta_byage_groups)
meta_byage_groups<-as.data.frame(meta_byage_groups)
meta_byage_groups$total_rti<-as.numeric(as.character(meta_byage_groups$total_rti))
meta_byage_groups$total_rti_death<-as.numeric(as.character(meta_byage_groups$total_rti_death))
order<-meta_byage_groups$total_rti_death/meta_byage_groups$total_rti
meta_byage_groups<-meta_byage_groups[order(order),] 
tiff("/Users/jnv4/Desktop/rti_deaths_by_age_groups.tiff",
 width = 800, height = 1000,compression = 'lzw')
m3<-metaprop(total_rti_death,total_rti,sm="PLN",
	byvar=age_groups,data=meta_byage_groups,
	studlab=study,comb.fixed=FALSE,comb.random=FALSE,
	print.byvar=FALSE)
meta::forest(m3)
dev.off()

#TOTAL TRAUMA Vs. TOTAL RTI
meta_byage_groups<-with(data,data.frame(total_traume,total_rti,Author,age_groups))
meta_byage_groups<-na.omit(meta_byage_groups)
meta_byage_groups<-as.matrix(meta_byage_groups)
meta_byage_groups<-as.data.frame(meta_byage_groups)
meta_byage_groups$total_rti<-as.numeric(as.character(meta_byage_groups$total_rti))
meta_byage_groups$total_traume<-as.numeric(as.character(meta_byage_groups$total_traume))
order<-meta_byage_groups$total_rti/meta_byage_groups$total_traume
meta_byage_groups<-meta_byage_groups[order(order),] 
tiff("/Users/jnv4/Desktop/rti_trauma_by_age_groups.tiff", 
	width = 800, height = 1300,compression = 'lzw')
m3<-metaprop(total_rti,total_traume,sm="PLN",
	byvar=age_groups,data=meta_byage_groups,
	studlab=Author,comb.fixed=FALSE,comb.random=FALSE,
	print.byvar=FALSE)
meta::forest(m3,hetstat=FALSE)
dev.off()

###################################################
#BY TYPE OF POPULATION (
####################################################
#TOTAL RTI Vs. TOTAL RTI DEATHS
meta_bypopulation<-with(data,data.frame(total_rti,
	total_rti_death,Author,type_population,
	proportion_death,study))
meta_bypopulation$type_population<-car::recode(
	meta_bypopulation$type_population,"'' =NA")
meta_bypopulation<-na.omit(meta_bypopulation)
meta_bypopulation<-as.matrix(meta_bypopulation)
meta_bypopulation<-as.data.frame(meta_bypopulation)
meta_bypopulation$total_rti<-as.numeric(as.character(
	meta_bypopulation$total_rti))
meta_bypopulation$total_rti_death<-as.numeric(as.character(
	meta_bypopulation$total_rti_death))
meta_bypopulation$proportion_death<-as.numeric(as.character(
	meta_bypopulation$proportion_death))
order<-meta_bypopulation$total_rti_death/meta_bypopulation$total_rti
meta_bypopulation<-meta_bypopulation[order(order),] 

tiff("/Users/jnv4/Desktop/rti_deaths_by_population.tiff", 
	width = 700, height = 800,compression = 'lzw')
m3<-metaprop(total_rti_death,total_rti,sm="PLN",
	byvar=type_population,data=meta_bypopulation,
	studlab=study,comb.fixed=FALSE,comb.random=FALSE,
	print.byvar=FALSE)
meta::forest(m3,hetstat=FALSE)
dev.off()

by(meta_bypopulation$proportion_death,meta_bypopulation$type_population,summary)

#TOTAL TRAUMA Vs. TOTAL RTI

meta_bypopulation<-with(data,data.frame(total_traume,
	total_rti,Author,type_population,proportion_rti,study))
meta_bypopulation$type_population<-car::recode(
	meta_bypopulation$type_population,"'' =NA")
meta_bypopulation<-na.omit(meta_bypopulation)
meta_bypopulation<-as.matrix(meta_bypopulation)
meta_bypopulation<-as.data.frame(meta_bypopulation)
meta_bypopulation$total_rti<-as.numeric(as.character(
	meta_bypopulation$total_rti))
meta_bypopulation$total_traume<-as.numeric(as.character(
	meta_bypopulation$total_traume))
meta_bypopulation$proportion_rti<-as.numeric(as.character(
	meta_bypopulation$proportion_rti))
order<-meta_bypopulation$total_rti/meta_bypopulation$total_traume
meta_bypopulation<-meta_bypopulation[order(order),] 

tiff("/Users/jnv4/Desktop/rti_trauma_by_population.tiff",
	width = 700, height = 1000,compression = 'lzw')
m3<-metaprop(total_rti,total_traume,sm="PLN",
	byvar=type_population,data=meta_bypopulation,
	studlab=study,comb.fixed=FALSE,comb.random=FALSE,
	print.byvar=FALSE)
meta::forest(m3,hetstat=FALSE)
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

data$year_cat<-car::recode(data$year,"1990:1995='1990-1995';1996:2000='1996-2000';2001:2005='2001-2005';2006:2010='2006-2010';2011:2015='2011-2015';else='1990 or less'")

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

#Time series graph
###################################################
m_model<-c(rep("RTI",64),rep("Death",40))
m_year<-c(as.character(m4_year),as.character(m3_year))
m_year<-as.factor(m_year)
m_prev<-c(m4_prev,m3_prev)
m_data<-data.frame(m_model,m_year,m_prev)

value<-c(by(m4_prev,m4_year,stats::median),
	by(m3_prev,m3_year,stats::median))
npapers<-c(m4$k.w,m3$k.w)
dates<-c(m4$bylevs,m3$bylevs)
model<-c(rep("RTI",6),rep("Death",5))

#proportion<-m

graph_data<-data.frame(value,npapers,dates,model)

tiff("/Users/joaovissoci/Desktop/rti_trauma_byyear.tiff", width = 700, 
	height = 500,compression = 'lzw')
ggplot(data=graph_data, aes(x=dates, y=value, group=model,
	color=model)) + geom_line(size=1.5) + 
	geom_point(size=5,fill="white") + 
	ylab("Prevalence (%)") + 
	xlab("Dates") + 
	scale_colour_manual(values=c("black", "#E69F00"),name="")+ 
	theme_bw() + 
	geom_bar(aes(x=dates,y=npapers/100),
		stat="identity", alpha=0.5, fill="white") + 
	annotate("text", x = 1, y = 0.02, label = "2",size=5)+
	annotate("text", x = 2, y = 0.17, label = "16",size=5)+
	annotate("text", x = 3, y = 0.11, label = "10",size=5)+
	annotate("text", x = 4, y = 0.22, label = "21",size=5)+
	annotate("text", x = 5, y = 0.29, label = "28",size=5)+
	annotate("text", x = 6, y = 0.37, label = "36",size=5) + 
	geom_jitter(data=m_data, aes(x=m_year,y=m_prev,
		group=m_model,color=m_model),size=3,fill="black") +
	theme(legend.justification=c(1,0), legend.position=c(0.1,0.8)) +
	theme(axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()) 
dev.off()


###################################################
#MAPS
###################################################

#MAP 1
##################
#get.centroids = function(x){   # extract centroids from polygon with given ID
#  poly = wmap@polygons[[x]]
#  ID   = poly@ID
#  centroid = as.numeric(poly@labpt)
#  return(c(id=ID, c.long=centroid[1], c.lat=centroid[2]))
#}

#wyield <- transform(wyield, ID_1 = substr(ID_1,3,10))  #strip leading "TR"

# wmap@data and wyield have common, unique field: country
#wdata  <- data.frame(id=rownames(wmap@data),name=wmap@data$COUNTRY)
#wdata  <- merge(wdata,wyield, by="COUNTRY")
#labs   <- do.call(rbind,lapply(1:17,get.centroids)) # extract polygon IDs and centroids from shapefile
#wdata  <- merge(wdata,labs,by="id")
#wdata[c("c.lat","c.long")] <- sapply(wdata[c("c.lat","c.long")],function(x) as.numeric(as.character(x)))

wdata$id<- wdata$ID_AFR
wmap.df <- fortify(wmap)                # data frame for world map
wmap.df <- merge(wmap.df,wdata,by="id") # merge data to fill polygons
#wmap.df$XCNTRD<-as.numeric(wmap.df$XCNTRD)
#wmap.df$YCNTRD<-as.numeric(wmap.df$YCNTRD)
wdata$name<-NULL

for (x in 1:51)
{
	if (is.na(wdata$n_studies[x])) {
		wdata$name[x]=NA
	}
	else {
		wdata$name[x]=wdata$COUNTRY[x]
	}
}

wmap.df$name<-NULL

for (x in 1:247031)
{
	if (is.na(wmap.df$n_studies[x])) {
		wmap.df$name[x]=NA
	}
	else {
		wmap.df$name[x]=as.character(wmap.df$COUNTRY[x])
	}
}

# Building the map

palette <- brewer.pal(11,"Spectral")    # ColorBrewewr.org spectral palette, 11 colors
ggmap   <- ggplot(wmap.df, aes(x=long, y=lat, group=group))
ggmap   <- ggmap + geom_polygon(aes(fill=n_studies))
ggmap   <- ggmap + geom_path(colour="black", size=.2)
#ggmap   <- ggmap + scale_fill_gradient(name="N of studies",low = "#6E8B3D", high = "#006400",na.value = "#BCEE68")
ggmap   <- ggmap + scale_fill_gradient(name="N of studies",
	limits=c(1,29))
ggmap   <- ggmap + coord_fixed()  + theme_bw()
ggmap   <- ggmap + labs(x="Longitude",y="Latitude",title="")
ggmap	<- ggmap + theme_update(
	panel.background=element_rect(colour="white"))
ggmap   <- ggmap + theme(plot.margin=unit(c(0,0.05,-0.05,0),
	units="npc"),
	legend.position=c(0.2,0.3))
# ggmap   <- ggmap + geom_text(aes(x=XCNTRD, y=YCNTRD,
#  label=name),size=5,  color="brown2")
ggmap   <- ggmap + annotate("text", x = -15, y = 14.45, 
	label = "Senegal",size=5,color="red")
ggmap   <- ggmap + annotate("text", x = 11, y = 4, 
	label = "Cameroon",size=5,color="red")
ggmap   <- ggmap + annotate("text", x = 8, y = 9, 
	label = "Nigeria",size=5,color="red")
ggmap   <- ggmap + annotate("text", x = 40, y = 8, 
	label = "Ethiopia",size=5,color="red")
ggmap   <- ggmap + annotate("text", x = -2, y = 7, 
	label = "Ghana",size=5,color="red")
ggmap   <- ggmap + annotate("text", x = 39, y = 0, 
	label = "Kenya",size=5,color="red")
ggmap   <- ggmap + annotate("text", x = 35, y = -14, 
	label = "Malawi",size=5,color="red")
ggmap   <- ggmap + annotate("text", x = 17, y = -22, 
	label = "Namibia",size=5,color="red")
ggmap   <- ggmap + annotate("text", x = 30, y = -2, 
	label = "Rwanda",size=5,color="red")
ggmap   <- ggmap + annotate("text", x = 25, y = -30, 
	label = "South Africa",size=5,color="red")
ggmap   <- ggmap + annotate("text", x = 35, y = -8, 
	label = "Tanzania",size=5,color="red")
ggmap   <- ggmap + annotate("text", x = 32, y = 2, 
	label = "Uganda",size=5,color="red")
ggmap   <- ggmap + annotate("text", x = 35, y = -18, 
	label = "Mozambique",size=5,color="red")
ggmap 


#box.df       <- wdata[order(as.numeric(wdata$COUNTRY)),]    # order by ID_1
#box.df$label <- with(box.df, paste0(name_long," [",COUNTRY,"]")) # create labels for boxplot
#box.df       <- melt(box.df,id.vars="label",measure.vars=c("A1B","A1BLow","A1F","A1T","A2","B1","B1Low","B2"))
#box.df$label <- factor(box.df$label,levels=unique(box.df$label)) # need this so orderin is maintained in ggplot

ggbox   <- ggplot(data_country,aes(x=country, y=proportion_rti))
ggbox   <- ggbox + geom_boxplot(fill="grey")#, outlier.colour = "blue", outlier.shape = 16, outlier.size = 4)
#ggbox   <- ggbox + stat_summary(fun.y=mean, geom="point", shape=21, size= 4, color= "red")
ggbox <- ggbox + theme_bw() + theme_update(panel.background=element_rect(colour="white"))
ggbox   <- ggbox + coord_flip()
ggbox   <- ggbox + labs(x="", y="RTI Proportion")
ggbox   <- ggbox + theme(plot.title=element_text(face="bold"), axis.text=element_text(color="black"))
ggbox   <- ggbox + geom_jitter(aes(colour=data_country$proportion_rti*10),size=data_country$proportion_rti*10)
#ggbox   <- ggbox + geom_text(aes(x=country, y=proportion_rti, label=Author),size=4, colour="red",angle = 45)
ggbox   <- ggbox + theme(plot.margin=unit(c(0.05,0.05,0.05,0),units="npc"),legend.position="none")
ggbox

ggbox2   <- ggplot(data_country,aes(x=country, y=proportion_death))
ggbox2   <- ggbox2 + geom_boxplot(fill="grey")#, outlier.colour = "blue", outlier.shape = 16, outlier.size = 4)
#ggbox2   <- ggbox2 + stat_summary(fun.y=mean, geom="point", shape=21, size= 4, color= "red")
ggbox2 <- ggbox2 + theme_bw() + theme_update(panel.background=element_rect(colour="white"))
ggbox2   <- ggbox2 + coord_flip()
ggbox2   <- ggbox2 + labs(x="", y="Death Proportion")
ggbox2   <- ggbox2 + theme(plot.title=element_text(face="bold"), axis.text=element_text(color="black"))
ggbox2   <- ggbox2 + geom_jitter(aes(colour=data_country$proportion_death*20),size=data_country$proportion_death*20)
#ggbox2   <- ggbox2 + geom_text(aes(x=country, y=proportion_death, label=Author),size=4, colour="red",angle = 45)
ggbox2   <- ggbox2 + theme(plot.margin=unit(c(0.05,0.05,0.05,0),units="npc"),legend.position="none")
ggbox2

tiff("/Users/joaovissoci/Desktop/rti_deaths_bycountry.tiff", 
	width = 1500, height = 490,compression = 'lzw')
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,3)))#,heights=c(0.40,0.60))
print(ggmap, vp=viewport(layout.pos.row=1,layout.pos.col=1))
print(ggbox, vp=viewport(layout.pos.row=1,layout.pos.col=2))
print(ggbox2, vp=viewport(layout.pos.row=1,layout.pos.col=3))
dev.off()

