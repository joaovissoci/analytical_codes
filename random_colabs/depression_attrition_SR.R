################################################################################
#TEMPLATE_FOR _META_ANALYSIS_OF_DIAGNOSTIC_ACCURACY#
#this script follows a combination of guidelines proposed by Doebler and Holling, according to (http://cran.r-project.org/web/packages/mada/vignettes/mada.pdf)#
#
#
#####################################################################################
#SETTING ENVIRONMENT
#####################################################################################
#install.packages("mada")
#library("mada")
#install.packages("car", repos="http://cran.r-project.org")
#install.packages("ggplot2", repos="http://cran.r-project.org")
#install.packages("devtools")

#devtools::install_github("jennybc/googlesheets")

#Load packages (after installed) with the library function
lapply(c("metafor","ggplot2","gridExtra" ,"psych", 
	"RCurl", "irr", "nortest", "moments","GPArotation",
	"nFactors","gdata","meta","qgraph","devtools",
	"googlesheets","plyr"), library, character.only=T)
##############################################################
#IMPORTING DATA AND RECODING
##############################################################
data<-read.csv("/Users/joaovissoci/OneDrive - Duke University/datasets/RoR/depression_attrition_SR.csv",
	header=T)
#data<-read.csv("/Users/rpietro/Desktop/depressao_teens.csv",header=T)er

#recode age
age_ig<-car::recode(data$age_mean_ig,"40:50.99='40 to 50';51.01:60.99='50 to 60';61:100='elder';else='younger'")

# #recode depression
# depression_score_b_ig
# depression_score_b_cg

#recode freq_adm_ig
freq_adm_ig_recoded<-car::recode(freq_adm_ig,"0=0;0:1=1;2=2;3:10=3")

#recode follow_up
follow_up_recoded<-car::recode(follow_up_recoded,"1=1;2=2;else=3")

###########################################################
##INTERVENTION GROUP
###########################################################
#OVERALL ATTRITION
data_model1<-with(data,data.frame(attrition_ig,
	total_sample_ig,author_year))
data_model1<-na.omit(data_model1[order(data_model1$author_year),])
m3<-metaprop(attrition_ig,total_sample_ig,
	sm="PLN",data=data_model1,studlab=author_year)

tiff("/Users/jnv4/Desktop/depression_sr_overall_attrition_intervention.tiff", 
	width = 1000, height = 2000,compression = 'lzw')
forest(m3)
dev.off()
funnel(m3)
metainf(m3)
#metainf(m3, pooled="random")
#metareg(m3, ~ data_model1$age + data_model1$intervention_type + data_model1$country)

### BY AGE SUBGROUP ANALYSIS
data_model2<-with(data,data.frame(attrition_ig,total_sample_ig,author_year,age_ig))
data_model2<-na.omit(data_model2)
m3<-metaprop(attrition_ig,total_sample_ig,sm="PLN",data=data_model2,studlab=author_year,byvar=age_ig)
# tiff("/Users/rpietro/Desktop/depression_sr_overall_attrition_intervention_by_age.tiff", width = 1000, height = 1200,compression = 'lzw')
forest(m3)
# dev.off()

### BY depression_score type SUBGROUP ANALYSIS
data_model2<-with(data,data.frame(attrition_ig,total_sample_ig,author_year,depression_score))
data_model2<-na.omit(data_model2)
m3<-metaprop(attrition_ig,total_sample_ig,sm="PLN",data=data_model2,studlab=author_year,byvar=depression_score)
# tiff("/Users/rpietro/Desktop/depression_sr_overall_attrition_intervention_by_age.tiff", width = 1000, height = 1200,compression = 'lzw')
forest(m3)
# dev.off()
metainf(m3)
metainf(m3, pooled="random")

### BY outcome_method type SUBGROUP ANALYSIS
data_model2<-with(data,data.frame(attrition_ig,total_sample_ig,author_year,outcome_method))
data_model2<-na.omit(data_model2)
m3<-metaprop(attrition_ig,total_sample_ig,sm="PLN",data=data_model2,studlab=author_year,byvar=outcome_method)
# tiff("/Users/rpietro/Desktop/depression_sr_overall_attrition_intervention_by_age.tiff", width = 1000, height = 1200,compression = 'lzw')
forest(m3)
# dev.off()
metainf(m3)
metainf(m3, pooled="random")

### BY depression_diag_criteria type SUBGROUP ANALYSIS
data_model2<-with(data,data.frame(attrition_ig,total_sample_ig,author_year,depression_diag_criteria))
data_model2<-na.omit(data_model2)
m3<-metaprop(attrition_ig,total_sample_ig,sm="PLN",data=data_model2,studlab=author_year,byvar=depression_diag_criteria)
# tiff("/Users/rpietro/Desktop/depression_sr_overall_attrition_intervention_by_age.tiff", width = 1000, height = 1200,compression = 'lzw')
forest(m3)
# dev.off()
metainf(m3)
metainf(m3, pooled="random")

### BY freq_adm_ig_recoded type SUBGROUP ANALYSIS
data_model2<-with(data,data.frame(attrition_ig,total_sample_ig,author_year,freq_adm_ig_recoded))
data_model2<-na.omit(data_model2)
m3<-metaprop(attrition_ig,total_sample_ig,sm="PLN",data=data_model2,studlab=author_year,byvar=freq_adm_ig_recoded)
# tiff("/Users/rpietro/Desktop/depression_sr_overall_attrition_intervention_by_age.tiff", width = 1000, height = 1200,compression = 'lzw')
forest(m3)
# dev.off()
metainf(m3)

### BY follow_up_recoded type SUBGROUP ANALYSIS
data_model2<-with(data,data.frame(attrition_ig,total_sample_ig,author_year,follow_up_recoded))
data_model2<-na.omit(data_model2)
m3<-metaprop(attrition_ig,total_sample_ig,sm="PLN",data=data_model2,studlab=author_year,byvar=follow_up_recoded)
# tiff("/Users/rpietro/Desktop/depression_sr_overall_attrition_intervention_by_age.tiff", width = 1000, height = 1200,compression = 'lzw')
forest(m3)
# dev.off()
metainf(m3)

###########################################################
#EFFECT SIZES
##########################################################
data_ef<-remove.vars(data_summary,c("X"))
data_ef2<-NULL
data_ef2$efeitos_adversos<-with(data_ef,base::rowSums(data.frame(efeitos_adversos,efeitos_adversos.1,efeitos_adversos.2,efeitos_adversos.3,efeitos_adversos.4,efeitos_adversos.5,efeitos_adversos.6,efeitos_adversos.7,efeitos_adversos.8,efeitos_adversos.9,efeitos_adversos.10,efeitos_adversos.11,efeitos_adversos.12,efeitos_adversos.13,efeitos_adversos.14,efeitos_adversos.15,efeitos_adversos.16,efeitos_adversos.17,efeitos_adversos.18,efeitos_adversos.19,efeitos_adversos.20,efeitos_adversos.21,efeitos_adversos.22,efeitos_adversos.23,efeitos_adversos.24,efeitos_adversos.25,efeitos_adversos.26,efeitos_adversos.27,efeitos_adversos.28,efeitos_adversos.29,efeitos_adversos.30,efeitos_adversos.31,efeitos_adversos.32,efeitos_adversos.33,efeitos_adversos.34,efeitos_adversos.35,efeitos_adversos.36,efeitos_adversos.37,efeitos_adversos.38,efeitos_adversos.39,efeitos_adversos.40,efeitos_adversos.41,efeitos_adversos.42)))
data_ef2$outro_tratamento<-with(data_ef,base::rowSums(data.frame(outro_tratamento,outro_tratamento.1,outro_tratamento.2,outro_tratamento.3,outro_tratamento.4,outro_tratamento.5,outro_tratamento.6,outro_tratamento.7)))
data_ef2$rct_problems<-with(data_ef,base::rowSums(data.frame(rct_problems,  rct_problems.1,rct_problems.2,rct_problems.3,rct_problems.4,rct_problems.5,rct_problems.6,rct_problems.7,rct_problems.8,rct_problems.9,rct_problems.10,rct_problems.11,rct_problems.12,rct_problems.13,rct_problems.14,rct_problems.15,rct_problems.16)))
data_ef2$outros<-with(data_ef,base::rowSums(data.frame(outros,outros.1,outros.2,outros.3,outros.4,outros.5,outros.6)))
data_ef2$falha_retorno<-with(data_ef,base::rowSums(data.frame(falha_retorno,falha_retorno.1,falha_retorno.2,falha_retorno.3,falha_retorno.4)))
data_ef2$questoes_medicamento<-with(data_ef,base::rowSums(data.frame(questoes_medicamento,questoes_medicamento.1,questoes_medicamento.2,questoes_medicamento.3,questoes_medicamento.4)))
data_ef$resposta_inadequada<-with(data_ef,base::rowSums(data.frame(resposta_inadequada ,resposta_inadequada.1,resposta_inadequada.2,resposta_inadequada.3,resposta_inadequada.4,resposta_inadequada.5,resposta_inadequada.6,resposta_inadequada.7,resposta_inadequada.8,resposta_inadequada.9,resposta_inadequada.10)))
data_ef2$melhora_sintomas<-with(data_ef,base::rowSums(data.frame(melhora_sintomas,melhora_sintomas.1)))
data_ef2$questoes_familiares<-with(data_ef,base::rowSums(data.frame(questoes_familiares,  questoes_familiares.1,questoes_familiares.2,questoes_familiares.3,questoes_familiares.4,questoes_familiares.5,questoes_familiares.6,questoes_familiares.7,questoes_familiares.8,questoes_familiares.9,questoes_familiares.10)))

allto1<-function(x){
	car::recode(x,"1:100=1;0=0")
}
data_ef3<-lapply(data_ef2,allto1)

effect_size<-colSums(as.data.frame(data_ef3))

effect_sizes<-as.matrix(effect_size/dim(as.data.frame(data_ef3))[1])

data_ef4<-NULL
data_ef4$outro_tratamento<-car::recode(data_ef3$outro_tratamento,"1=0")
data_ef4$outros<-car::recode(data_ef3$outros,"1=0")
data_ef4$falha_retorno<-car::recode(data_ef3$falha_retorno,"1=0")
data_ef4$melhora_sintomas<-car::recode(data_ef3$melhora_sintomas,"1=0")
data_ef4$questoes_medicamento<-car::recode(data_ef3$questoes_medicamento,"1=0")
data_ef4$questoes_familiares<-car::recode(data_ef3$questoes_familiares,"1=0")
data_ef4$efeitos_adversos<-data_ef3$efeitos_adversos
data_ef4$rct_problems<-data_ef3$rct_problems

intensity<-length(dim(effect_sizes>=0.25))
intensity_data<-rowSums(as.data.frame(data_ef4))

intensity_sizes<-intensity_data/intensity
#########################################################
#NETWORK ANALYSIS
#########################################################
#data_network<-remove.vars(data,c("Study"))
data_network<-as.matrix(as.data.frame(data_ef3))
variable_data <- t(as.matrix(data_network)) %*% as.matrix(data_network)
study_data<-as.matrix(data_network)
rownames(study_data)<-data_summary$X
network_data<-rbind(variable_data,study_data)
network_data <- (as.matrix(network_data)) %*% t(as.matrix(network_data))
diag(network_data) <- 0
names<-c(c("EA","OT","ECR","OU","FR","MD","ST","QF"),rownames(study_data))

size_edges<-c(effect_sizes[,1]*10,intensity_sizes*1.5)
color<-c("red","yellow","lightblue","lightblue","red","red","yellow","yellow",rep("grey",46))
shape<-c(rep("circle",8),rep("square",46)) 
label.cex<- c(rep(1.5,8),rep(1.0,46))
#groups<-c("Ensaio Clínico","Medicamentos","Outras Razões")

tiff("/home/joao/Desktop/sporedata_depression_sr_network.tiff", width = 1000, height = 700,compression = 'lzw')
network_meta <- qgraph(network_data,layout = "spring",minimum=0.5,cut=100,labels=names,label.scale=FALSE,label.cex = label.cex,vsize=size_edges,shape=shape,grey=T,color=color,borders = FALSE,posCol = "grey")
legend(0.8,-0.8, bty=".",c("Ensaio Clínico","Medicamentos","Outras Razões"),cex=1.2,fill=c("lightblue","red","yellow"))
#legend(-1.32,-0.5	, bty="n",c("EA: Efeitos Adversos","OT: Outro Tratamento","ECR: Questões com o ECR","FR: Falha no Retorno","MD: Problemas com medicamentos","ST: Melhora nos Sintomas","QF: Questões Familiares","OU: Outras Razões"),cex=1.2)
dev.off()

###########################################################
#Quality Assessment
###########################################################

summary(qualitydata)
qualitydata$Item<-as.factor(qualitydata$Item)
#attach(qualitydata)
#Generate ggplot graph fro quality data information
ggplot(qualitydata, aes(Item, Author)) + geom_tile(aes(fill = Value),
colour = "white") + scale_fill_gradient(low = "white",
high = "steelblue", name="", breaks=c(0,5,10), labels=c("No","Not Clear","Yes")) +
 theme(axis.text.x = element_text(hjust = 0, colour = "black",size=14),
        axis.text.y = element_text(colour = "black",size=14),
        axis.title.x = element_text(face="bold",size=14),
        axis.title.y = element_text(face="bold", size=14))
