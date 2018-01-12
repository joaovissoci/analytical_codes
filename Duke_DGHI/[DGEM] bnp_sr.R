

lapply(c("meta"), library, character.only=T)

# data_table2<-c(-4,16.7,13,10.6,36.5,0,13.2,26,22,
# 	    0,14.1,10,4.2,7.5,6,19.65,14,6,
# 	    -4,2.6,3,6.4,29,-6,-6.45,12,16)

# group_table2<-c(rep("Isquemic",9),rep("Normal",9),rep("Delta",9))

# name_table2<-rep("table2",27)

# data_table3<-c(32,48.1,4.1,-2,1.67,
# 			   17,24,0.2,-0.6,2.84,
# 			   15,24.1,3.9,-1.4,-1.17)

# group_table3<-c(rep("Isquemic",5),rep("Normal",5),rep("Delta",5))

# name_table3<-rep("table3",15)


# data_table4<-c(14,30,30,-15.59,14.5,3,38,
# 	           9,14,15,22,4,16,15,
# 	           5,16,15,-37.59,10.5,-13,23)

# group_table4<-c(rep("Isquemic",7),rep("Normal",7),rep("Delta",7))

# name_table4<-rep("table4",21)


# data_table5<-c(1.2,26.4,5.4,9.03,
# 	           4,10.3,-0.7,-1.61,
# 	           -2.8,16.1,-0.7,-1.61)

# group_table5<-c(rep("Isquemic",4),rep("Normal",4),rep("Delta",4))

# name_table5<-rep("table5",12)

# bnp_sr_data<-read.csv("/Users/jnv4/OneDrive - Duke University/datasets/Global EM/BNP SR/bnp_SR_data.csv")

bnp_sr_metadata<-read.csv("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/SRs/BNP SR/US_bnpmetaanalysis_data2.csv")


#############################################################################
#Figure. 2
#############################################################################
## Suicide ideation metanalysis model
#extracting studies with for the BNP
# meta_bnp<-subset(bnp_sr_metadata,
# 	bnp_sr_metadata$group=="BNP")

#extracting studies with only Normal patients
meta_temp_normal<-subset(bnp_sr_metadata,
	bnp_sr_metadata$type=="Normal")

#run metanalysis model for continuous data
meta_model_normal<- metacont(N, 
				  mean_baseline,
				  sd_baseline,
				  N,
				  mean_poststress,
				  sd_poststress, 
  			data=meta_temp_normal, sm="MD")


mean_control <- round(meta_model_normal$TE,1)
sd_control <- round(meta_model_normal$seTE*sqrt(meta_temp_normal$N-1),1)

#extracting studies with only Ischemic patients
meta_temp_ischemic<-subset(bnp_sr_metadata,
	bnp_sr_metadata$type=="Ischemic")

#run metanalysis model for continuous data
meta_model_ischemic <- metacont(N, 
				  mean_baseline,
				  sd_baseline,
				  N,
				  mean_poststress,
				  sd_poststress,
  				  data=meta_temp_ischemic, sm="MD")


mean_exp <- round(meta_model_ischemic$TE,1)
sd_exp <- round(meta_model_ischemic$seTE*sqrt(meta_temp_ischemic$N-1),1)

#organizing dataset for metanalysis model
delat_data<-data.frame(mean_exp,
					   sd_exp,
					   sample_exp=meta_temp_ischemic$N,
					   mean_control,
					   sd_control,
					   sample_control=meta_temp_normal$N,
					   study=meta_temp_normal$Authors,
					   group=meta_temp_normal$group)

#run metanalysis model for continuous data
#FULL MODEL
meta1 <- metacont(	
					sample_exp, 
					mean_exp,
					sd_exp,
					sample_control,
					mean_control,
					sd_control,
					label.e="Ischemic",
				    label.c="Normal",
				    complab="SMD, Hodges' G",
				    comb.random=TRUE,
				    comb.fixed=FALSE,
  					data=delat_data,
  					sm="SMD",
 				    byvar=group,
 				    print.byvar=FALSE,
  					studlab=study)
summary(meta1)

# #Excluding Marumoto et al., 1995
# meta1 <- metacont(	
# 					sample_exp, 
# 					mean_exp,
# 					sd_exp,
# 					sample_control,
# 					mean_control,
# 					sd_control,
# 					label.e="Ischemic",
# 				    label.c="Normal",
# 				    complab="SMD, Hodges' G",
# 				    # label.left=TRUE,
# 				    comb.random=FALSE,
# 				    comb.fixed=TRUE,
#   					data=delat_data_bnp[1:3,],
#   					sm="SMD",
#   					# keepdata=FALSE,
#  				    # byvar=intervention,print.byvar=FALSE,
#   					studlab=meta_bnp_ischemic$Authors[1:3])
# summary(meta1)

tiff("/Users/joaovissoci/Desktop/figure2.tiff",
  width = 800, height = 400,compression = 'lzw')
forest(meta1,
	   leftcols=c("studlab"),
	   rigthcols=c("w.fixed"),
	   # overall=FALSE, #A logical indicating whether overall summaries should be plotted
	   xlab="Delta Hedges' g",
	   leftlabs=c("Study"),
	   just="center",
	   just.studlab="left",
	   layout="JAMA",
	   # leftlabs=c("Author", "N", "Delta", "SD"),
	   # comb.fixed=TRUE,
	   xlim=c(-3, 3),
	   overall=FALSE
	   # colgap.forest.left=unit(3.5,"cm")
	   )
dev.off()

funnel(meta1)
metainf(meta1)
metainf(meta1, pooled="random")

# meta1 <- metacont(	
# 					N[1:4], 
# 					mean_poststress[1:4],
# 					sd_poststress[1:4],
# 					N[5:8],
# 					mean_poststress[5:8],
# 					sd_poststress[5:8],
# 					label.e="Ischemic",
# 				    label.c="Normal",
# 				    # complab="SMD, Hodges' G",
# 				    comb.random=FALSE,
# 				    comb.fixed=TRUE,
#   					data=meta_bnp, 
#   					sm="SMD",
#  				    # byvar=intervention,print.byvar=FALSE,
#   					studlab=meta_bnp_ischemic$Authors)

#############################################################################
#Figure. 2
#############################################################################
## Suicide ideation metanalysis model
#extracting studies with for the BNP
meta_probnp<-subset(bnp_sr_metadata,
	bnp_sr_metadata$group=="ProBNP")

#extracting studies with only Normal patients
meta_bnp_normal<-subset(meta_probnp,
	meta_probnp$type=="Normal")

#run metanalysis model for continuous data
meta1 <- metacont(N, 
				  mean_baseline,
				  sd_baseline,
				  N,
				  mean_poststress,
				  sd_poststress, 
  			data=meta_bnp_normal, sm="MD")


mean_control <- meta1$TE
sd_control <- meta1$seTE*sqrt(meta_bnp_normal$N)

#extracting studies with only Ischemic patients
meta_bnp_ischemic<-subset(meta_probnp,
	meta_probnp$type=="Ischemic")

#run metanalysis model for continuous data
meta1 <- metacont(N, 
				  mean_baseline,
				  sd_baseline,
				  N,
				  mean_poststress,
				  sd_poststress, 
  			data=meta_bnp_ischemic, sm="MD")


mean_exp <- meta1$TE
sd_exp <- meta1$seTE*sqrt(meta_bnp_ischemic$N)

#organizing dataset for metanalysis model
delat_data_bnp<-data.frame(mean_exp,sd_exp,
	sample_exp=meta_bnp_ischemic$N,mean_control,sd_control,
	sample_control=meta_bnp_normal$N)

#run metanalysis model for continuous data
#Excluding Marumoto et al., 1995
meta1 <- metacont(	
					sample_exp, 
					mean_exp,
					sd_exp,
					sample_control,
					mean_control,
					sd_control,
					label.e="Ischemic",
				    label.c="Normal",
				    complab="SMD, Hodges' G",
				    # label.left=TRUE,
				    comb.random=FALSE,
				    comb.fixed=TRUE,
  					data=delat_data_bnp,
  					sm="MD",
  					# keepdata=FALSE,
 				    # byvar=intervention,print.byvar=FALSE,
  					studlab=meta_bnp_ischemic$Authors)
summary(meta1)

tiff("/Users/joaovissoci/Desktop/figure3.tiff",
  width = 800, height = 400,compression = 'lzw')
forest(meta1,
	   leftcols=c("studlab"),
	   rigthcols=c("w.fixed"),
	   # overall=FALSE, #A logical indicating whether overall summaries should be plotted
	   xlab="Delta Hedges' g",
	   leftlabs=c("Author"),
	   just="center",
	   just.studlab="left",
	   # layout="revman5",
	   # leftlabs=c("Author", "N", "Delta", "SD"),
	   comb.fixed=TRUE,
	   xlim=c(-1, 1),
	   colgap.forest.left=unit(3.5,"cm")
	   )
dev.off()







######
# data<-c(data_table2,data_table3,data_table4,data_table5)
# name<-c(name_table2,name_table3,name_table4,name_table5)
# group<-c(group_table2,group_table3,group_table4,group_table5)

# plot_data<-data.frame(data,name,group)

bnp_sr_data_bnp<-subset(bnp_sr_data,bnp_sr_data$name=="BNP")


ggbox2   <- ggplot(bnp_sr_data_bnp,aes(x=group, 
	y=data))
ggbox2   <- ggbox2 + geom_boxplot(fill="grey80")#, outlier.colour = "blue", outlier.shape = 16, outlier.size = 4)
#ggbox2   <- ggbox2 + stat_summary(fun.y=mean, geom="point", shape=21, size= 4, color= "red")
ggbox2   <- ggbox2 + facet_grid(name ~ .)
ggbox2 <- ggbox2 + scale_y_continuous(limits=c(-50, 60),
	breaks=seq(-50,60,10)) 
ggbox2   <- ggbox2 + coord_flip()
ggbox2   <- ggbox2 + labs(x="", y="Median ")
ggbox2   <- ggbox2 + theme(plot.title=element_text(face="bold"), 
	axis.text=element_text(color="black"))
ggbox2   <- ggbox2 + geom_jitter(aes(size=log(bnp_sr_data_bnp$sample)))
#ggbox2   <- ggbox2 + geom_text(aes(x=country, y=proportion_death, label=Author),size=4, colour="red",angle = 45)
# ggbox2   <- ggbox2 + theme()#plot.margin=unit(c(0.05,0.05,0.05,0),
#  	units="npc")
ggbox2 <- ggbox2 + theme_bw()
ggbox2 <- ggbox2 + theme(legend.position="none")
# ggbox2 <- ggbox2 + scale_y_discrete(breaks=seq(-50, 60, 10)) 
ggbox2

## N-BNP
bnp_sr_data_probnp<-subset(bnp_sr_data,bnp_sr_data$name=="Pro-BNP")

ggbox3   <- ggplot(bnp_sr_data_probnp,aes(x=group, 
	y=data))
ggbox3   <- ggbox3 + geom_boxplot(fill="grey80")#, outlier.colour = "blue", outlier.shape = 16, outlier.size = 4)
#ggbox3   <- ggbox3 + stat_summary(fun.y=mean, geom="point", shape=21, size= 4, color= "red")
ggbox3   <- ggbox3 + facet_grid(name ~ ., scales="free_x")
ggbox3 <- ggbox3 + scale_y_continuous(limits=c(-50, 60),
	breaks=seq(-50,60,10)) 
ggbox3   <- ggbox3 + coord_flip()
ggbox3   <- ggbox3 + labs(x="", y="Median ")
ggbox3   <- ggbox3 + theme(plot.title=element_text(face="bold"), 
	axis.text=element_text(color="black"))
ggbox3   <- ggbox3 + geom_jitter(aes(size=log(bnp_sr_data_probnp$sample)))
#ggbox3   <- ggbox3 + geom_text(aes(x=country, y=proportion_death, label=Author),size=4, colour="red",angle = 45)
# ggbox3   <- ggbox3 + theme()#plot.margin=unit(c(0.05,0.05,0.05,0),
#  	units="npc")
ggbox3 <- ggbox3 + theme_bw()
ggbox3 <- ggbox3 + theme(legend.position="none")
ggbox3

## N-BNP
bnp_sr_data_delta<-subset(bnp_sr_data,bnp_sr_data$name=="Delta")

ggbox4   <- ggplot(bnp_sr_data_delta,aes(x=group, 
	y=data))
ggbox4   <- ggbox4 + geom_boxplot(fill="grey80")#, outlier.colour = "blue", outlier.shape = 16, outlier.size = 4)
#ggbox4   <- ggbox4 + stat_summary(fun.y=mean, geom="point", shape=21, size= 4, color= "red")
ggbox4   <- ggbox4 + facet_grid(name ~ ., scales="free_x")
ggbox4 <- ggbox4 + scale_y_continuous(limits=c(-50, 60),
	breaks=seq(-50,60,10)) 
ggbox4   <- ggbox4 + coord_flip()
ggbox4   <- ggbox4 + labs(x="", y="Median ")
ggbox4   <- ggbox4 + theme(plot.title=element_text(face="bold"), 
	axis.text=element_text(color="black"))
ggbox4   <- ggbox4 + geom_jitter(aes(size=log(bnp_sr_data_delta$sample)))
#ggbox4   <- ggbox4 + geom_text(aes(x=country, y=proportion_death, label=Author),size=4, colour="red",angle = 45)
# ggbox4   <- ggbox4 + theme()#plot.margin=unit(c(0.05,0.05,0.05,0),
#  	units="npc")
ggbox4 <- ggbox4 + theme_bw()
ggbox4 <- ggbox4 + theme(legend.position="none")
ggbox4


#Merge graphs
tiff("/Users/jnv4/Desktop/bnp_summary_figure.tiff",
  width = 600, height = 500,compression = 'lzw+p',bg = "white")
grid.arrange(arrangeGrob(ggbox2,ggbox3,ggbox4,
  left = textGrob("Groups", rot = 90, vjust = 1),
  bottom = textGrob("% Stress delta"),nrow=3,ncol=1,
  heights=c(0.3,0.3,0.3)))
dev.off()

#### Metanalysis


