#################################################################
# SUICIDE PREVENTION INITIATIVES SYSTEMATIC REVIEW
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
lapply(c("epicalc", "sem","Hmisc","ggplot2", "psych", "irr", 
	"nortest", "moments","GPArotation","nFactors","repmis",
	"gdata","qgraph","igraph","meta","metafor"), 
library, character.only=T)
#################################################################
#IMPORTING DATA
#################################################################
#LOADING DATA FROM A .CSV FILE
data<-read.csv("/Users/jnv4/Desktop/data_cancerbucal.csv",sep=",")
#information between " " are the path to the directory in your computer 
#where the data is stored

#################################################################
#MANAGING DATA
#################################################################

#extracting the data involved with the analysis
data_cancerbucal<-with(data,data.frame(dif_idh,dif_gini,
	dif_rendapc,dif_razaosexo,dif_idosos,dif_equipamento,
	dif_insumo,dif_epi,dif_equipe_min,dif_vinculo_prec,
	dif_financiamento,dif_1consulta,dif_cob_esf,dif_cob_esb,
	dif_acoesprev,dif_ibdc,dif_txp_mort_cb,dif_medico_p,dif_ceo_p,
	dif_leito_p,dif_cob_acs))

#################################################################
#GRAPH ANALYSIS
#################################################################

cor<-cor_auto(data_cancerbucal)


graph_glasso<-qgraph(cor,layout="spring",vsize=6,
	esize=20,graph="glasso",sampleSize=nrow(data_cancerbucal),
	legend.cex = 0.5,GLratio=1.5)
graph_pcor<-qgraph(cor,layout="spring",vsize=6,
	esize=20,graph="pcor",threshold="holm",
	sampleSize=nrow(data_cancerbucal),legend.cex = 0.5,
	GLratio=1.5)
graph_cor<-qgraph(cor,layout="spring",vsize=6,
	esize=20,legend.cex = 0.5,GLratio=1.5)
Lqsg<-averageLayout(graph_glasso,graph_pcor,graph_cor)


qsgG3<-qgraph(cor,layout=Lqsg,nodeNames=colnames(data_cancerbucal),
	vsize=6,esize=20,
	graph="glasso",sampleSize=nrow(data_cancerbucal),
	legend.cex = 0.3,cut = 0.1, maximum = 1, minimum = 0, 
	repulsion = 0.8)#,gray=T,color=c("gray80","gray50"))#,groups=qsggr

