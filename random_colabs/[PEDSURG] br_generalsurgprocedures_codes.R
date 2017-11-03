######################################################################
#Pediatric surgery procedures availability
######################################################################
#
##IDC codes for procedures
# 0WJP0ZZ
# 0WQFXZ2
# 0DSP0ZZ
# 0DSP4ZZ 
# 0DSP7ZZ
# 0YQ54ZZ 
# 0YQ64ZZ 
# 0YQ50ZZ 
# 0YQ60ZZ 
# 0WQF4ZZ 
# 0WUF07Z 
# 0WUF0KZ 
# 0WQF0ZZ
# 0DTJ4ZZ 
# 0DTJ0ZZ 
# 0BQR4ZZ 
# 0BQS4ZZ 
# 0BQR0ZZ 
# 0BQS0ZZ 
#
#
#
######################################################################
#SETTING ENVIRONMENT
######################################################################
#PASCKAGES INSTALLATION CODES
#install.packages("Hmisc")
#install.packages("car")
#install.packages("psych")
#install.packages("nortest")
#install.packages("ggplot2")
#install.packages("pastecs")
#install.packages("repmis")
#install.packages("mvnormtest")
#install.packages("polycor")

#PACKAGES LOADING CODE
#Load packages neededz for the analysis
#library(Hmisc)

#All packages must be installes with install.packages() function
lapply(c("tidyverse","Hmisc","car",
		 "psych","nortest","ggplot2",
		 "repmis","polycor","MASS"), 
library, character.only=T)

######################################################################
#IMPORTING DATA
######################################################################
# load("/Users/Joao/Box Sync/Home Folder jnv4/Data/Pediatric Surgery/novas analises - pediatria 15 anos/jovens_sia.RData")
# data_sia<-apendectomy

procedimento <- read.table("/Users/Joao/Downloads/tb_procedimento.txt", header = F, sep = ";",
                           colClasses = c("character","character","character","character"
                                          ,"character","character","character","character"
                                          ,"character","character","character","character"
                                          ,"character","character","character"))
procedimento$V3 <- NULL
procedimento$V4 <- NULL
procedimento$V5 <- NULL
procedimento$V6 <- NULL
procedimento$V7 <- NULL
procedimento$V8 <- NULL
procedimento$V9 <- NULL
procedimento$V10 <- NULL
procedimento$V11<- NULL
procedimento$V12 <- NULL
procedimento$V13 <- NULL
procedimento$V14 <- NULL
procedimento$V15 <- NULL
procedimento$V2 <- trimws(procedimento$V2)

#data with procedures information
load("/Users/Joao/Box Sync/Home Folder jnv4/Data/Pediatric Surgery/ped_2010.Rdata")
load("/Users/Joao/Box Sync/Home Folder jnv4/Data/Pediatric Surgery/ped_2011.Rdata")
load("/Users/Joao/Box Sync/Home Folder jnv4/Data/Pediatric Surgery/ped_2012.Rdata")
load("/Users/Joao/Box Sync/Home Folder jnv4/Data/Pediatric Surgery/ped_2013.Rdata")
load("/Users/Joao/Box Sync/Home Folder jnv4/Data/Pediatric Surgery/ped_2014.Rdata")
load("/Users/Joao/Box Sync/Home Folder jnv4/Data/Pediatric Surgery/ped_2015.Rdata")

data_sih<-rbind(ped_2010,
				ped_2011[,c(1:87)][,-45],
				ped_2012[,c(1:87)][,-45],
				ped_2013[,c(1:87)][,-45],
				ped_2014[,c(1:87)][,-45],
				ped_2015[,c(1:87)][,-45])

#merge procedimento
procedimento$V1 <- as.numeric(procedimento$V1)
data_sih <- merge(data_sih, procedimento, by.x = "PROC_REA", by.y = "V1", all.x = TRUE)
data_sih$PROC_SOLIC_nome <- data_sih$V2
data_sih$V2 <- NULL

#data with socioneconomic classification by income level
income_data<-read.csv("/Users/Joao/Box Sync/Home Folder jnv4/Data/Global EM/Brazil/acs/br_acsdiagnotic_data.csv")

#data with iformation about state and regions
state_data<-read.csv("/Users/Joao/Box Sync/Home Folder jnv4/Data/Global EM/Brazil/epidemiologic transition/ihd_cities.csv")

# procedures<-levels(as.factor(apendectomy$PROC_SOLIC_nome))

# write.csv(procedures,"/Users/joaovissoci/Desktop/procedures.csv")

#data with informmation about hospital infrastructure
infrastructure_data<-read.csv("/Users/Joao/Box Sync/Home Folder jnv4/Data/Global EM/Brazil/br_datasets/br_hospitalinfrastructure_data.csv")

#data with pediatric population
pediatric_population<-read.csv("/Users/Joao/Box Sync/Home Folder jnv4/Data/Global EM/Brazil/br_datasets/pediatric_population_2015.csv",sep=",")

#data with avaiability of pediatric surgical care
availability_data<-read.csv("/Users/Joao/Box Sync/Home Folder jnv4/Data/Global EM/Brazil/br_datasets/br_peds_availability_data.csv")

mortality_data<-read.csv("/Users/Joao/Box Sync/Home Folder jnv4/Data/Global EM/Brazil/br_datasets/br_pedmortality_data.csv")
###################### SIA
# #Subsetting the SIA data to include only General Pediatric Surgery Procedures
# data_sia_subset<-subset(data_sia,data_sia$PROC_SOLIC_nome=="LAPAROTOMIA EXPLORADORA" |
# 						 data_sia$PROC_SOLIC_nome=="LAPAROTOMIA PARA AVALIAÇÃO DE TUMOR DE OVARIO EM ONCOLOGIA" |
# 						 data_sia$PROC_SOLIC_nome=="LAPAROTOMIA VIDEOLAPAROSCOPICA PARA DRENAGEM E/OU BIOPSIA" |
# 						 data_sia$PROC_SOLIC_nome=="REDUCAO CIRURGICA DE VOLVO POR LAPAROTOMIA" |
# 						 data_sia$PROC_SOLIC_nome=="COLOSTOMIA" |
# 						 data_sia$PROC_SOLIC_nome=="REPARACAO DE OUTRAS HERNIAS" |
# 						 data_sia$PROC_SOLIC_nome=="TRATAMENTO CIRÚRGICO DE HERNIA MUSCULAR" |
# 						 data_sia$PROC_SOLIC_nome=="HERNIORRAFIA INGUINAL VIDEOLAPAROSCOPICA" |
# 						 data_sia$PROC_SOLIC_nome=="HERNIORRAFIA C/ RESSECCAO INTESTINAL (HERNIA ESTRANGULADA)" |
# 						 data_sia$PROC_SOLIC_nome=="HERNIORRAFIA S/ RESSECCAO INTESTINAL (HERNIA ESTRANGULADA )" |
# 						 data_sia$PROC_SOLIC_nome=="HERNIORRAFIA UMBILICAL VIDEOLAPAROSCOPICA" |
# 						 data_sia$PROC_SOLIC_nome=="RESSUTURA DE PAREDE ABDOMINAL (POR DEISCENCIA TOTAL / EVISCERACAO)" |
# 						 data_sia$PROC_SOLIC_nome=="APENDICECTOMIA" |
# 						 data_sia$PROC_SOLIC_nome=="APENDICECTOMIA VIDEOLAPAROSCOPICA")

# #Recoding procedures
# data_sia_subset$procedure<-car::recode(data_sia_subset$PROC_SOLIC_nome,"
# 			'LAPAROTOMIA EXPLORADORA'='laparotomy';
# 			'LAPAROTOMIA PARA AVALIAÇÃO DE TUMOR DE OVARIO EM ONCOLOGIA'='laparotomy';
# 			'LAPAROTOMIA VIDEOLAPAROSCOPICA PARA DRENAGEM E/OU BIOPSIA'='laparotomy';
# 			'REDUCAO CIRURGICA DE VOLVO POR LAPAROTOMIA'='laparotomy';
# 			'COLOSTOMIA'='colostomy';
# 			'REPARACAO DE OUTRAS HERNIAS'='hernia';
# 			'TRATAMENTO CIRÚRGICO DE HERNIA MUSCULAR'='hernia';
# 			'HERNIORRAFIA INGUINAL VIDEOLAPAROSCOPICA'='hernia';
# 			'HERNIORRAFIA C/ RESSECCAO INTESTINAL (HERNIA ESTRANGULADA)'='hernia';
# 			'HERNIORRAFIA S/ RESSECCAO INTESTINAL (HERNIA ESTRANGULADA )'='hernia';
# 			'HERNIORRAFIA UMBILICAL VIDEOLAPAROSCOPICA'='hernia';
# 			'RESSUTURA DE PAREDE ABDOMINAL (POR DEISCENCIA TOTAL / EVISCERACAO)'='wall';
# 			'APENDICECTOMIA'='apendectomy';
# 			'APENDICECTOMIA VIDEOLAPAROSCOPICA'='apendectomy'")

# #reducing dataset
# data_sia_recoded<-NULL
# data_sia_recoded$ibge<-data_sia_subset$MUNIC_RES
# data_sia_recoded$procedures<-data_sia_subset$procedure
# data_sia_recoded<-as.data.frame(data_sia_recoded)

# Selecting general pediatric procedures
################################### 
# data_sih<-subset(data_sih,data_sih$ANO_CMPT == 2012:2016)

#Subsetting the SIA data to include only General Pediatric Surgery Procedures
data_sih_subset<-subset(data_sih,data_sih$PROC_SOLIC_nome=="LAPAROTOMIA EXPLORADORA" |
						 data_sih$PROC_SOLIC_nome=="LAPAROTOMIA PARA AVALIAÇÃO DE TUMOR DE OVARIO EM ONCOLOGIA" |
						 data_sih$PROC_SOLIC_nome=="LAPAROTOMIA VIDEOLAPAROSCOPICA PARA DRENAGEM E/OU BIOPsih" |
						 data_sih$PROC_SOLIC_nome=="REDUCAO CIRURGICA DE VOLVO POR LAPAROTOMIA" |
						 data_sih$PROC_SOLIC_nome=="COLOSTOMIA" |
						 data_sih$PROC_SOLIC_nome=="REPARACAO DE OUTRAS HERNIAS" |
						 data_sih$PROC_SOLIC_nome=="TRATAMENTO CIRÚRGICO DE HERNIA MUSCULAR" |
						 data_sih$PROC_SOLIC_nome=="HERNIORRAFIA INGUINAL VIDEOLAPAROSCOPICA" |
						 data_sih$PROC_SOLIC_nome=="HERNIORRAFIA C/ RESSECCAO INTESTINAL (HERNIA ESTRANGULADA)" |
						 data_sih$PROC_SOLIC_nome=="HERNIORRAFIA S/ RESSECCAO INTESTINAL (HERNIA ESTRANGULADA )" |
						 data_sih$PROC_SOLIC_nome=="HERNIORRAFIA UMBILICAL VIDEOLAPAROSCOPICA" |
						 data_sih$PROC_SOLIC_nome=="RESSUTURA DE PAREDE ABDOMINAL (POR DEISCENCIA TOTAL / EVISCERACAO)" |
						 data_sih$PROC_SOLIC_nome=="APENDICECTOMIA" |
						 data_sih$PROC_SOLIC_nome=="APENDICECTOMIA VIDEOLAPAROSCOPICA")

#recoding procedures
data_sih_subset$procedure<-car::recode(data_sih_subset$PROC_SOLIC_nome,"
			'LAPAROTOMIA EXPLORADORA'='laparotomy';
			'LAPAROTOMIA PARA AVALIAÇÃO DE TUMOR DE OVARIO EM ONCOLOGIA'='laparotomy';
			'LAPAROTOMIA VIDEOLAPAROSCOPICA PARA DRENAGEM E/OU BIOPSIA'='laparotomy';
			'REDUCAO CIRURGICA DE VOLVO POR LAPAROTOMIA'='laparotomy';
			'COLOSTOMIA'='colostomy';
			'REPARACAO DE OUTRAS HERNIAS'='hernia';
			'TRATAMENTO CIRÚRGICO DE HERNIA MUSCULAR'='hernia';
			'HERNIORRAFIA INGUINAL VIDEOLAPAROSCOPICA'='hernia';
			'HERNIORRAFIA C/ RESSECCAO INTESTINAL (HERNIA ESTRANGULADA)'='hernia';
			'HERNIORRAFIA S/ RESSECCAO INTESTINAL (HERNIA ESTRANGULADA )'='hernia';
			'HERNIORRAFIA UMBILICAL VIDEOLAPAROSCOPICA'='hernia';
			'RESSUTURA DE PAREDE ABDOMINAL (POR DEISCENCIA TOTAL / EVISCERACAO)'='wall';
			'APENDICECTOMIA'='apendectomy';
			'APENDICECTOMIA VIDEOLAPAROSCOPICA'='apendectomy'")

#reducing dataset
data_sih_recoded<-NULL
data_sih_recoded$ibge<-data_sih_subset$MUNIC_RES
data_sih_recoded$procedure<-data_sih_subset$procedure
data_sih_recoded<-as.data.frame(data_sih_recoded)

#Stacking datasets
data_procedures<-data.frame(data_sih_recoded,CNES=data_sih_subset$CNES,
	year=data_sih_subset$ANO_CMPT)


#building dataset with region information
states<-with(state_data,data.frame(Sigla,Regio,ibge=GEOCOD,POP))

# Healthcenters LEVEL
#####################################################################

# summary(as.factor(infrastructure_data$type_service))

surg_centers<-subset(infrastructure_data,
	infrastructure_data$type_service!="PRONTO ATENDIMENTO" &
	infrastructure_data$type_service!="UNIDADE MISTA")

# summary(as.factor(infrastructure_data$ped_icu_type1_all))

peds_icu_data<-with(surg_centers,data.frame(ped_icu_type1_all,
												   ped_icu_type2_all,
												   ped_icu_type3_all))

NAto0<-function(x){
	car::recode(x,"NA=0")
	}

icuNAto01<-lapply(peds_icu_data,NAto0)
peds_icu<-as.data.frame(icuNAto01)
surg_centers$peds_icu_sum<-rowSums(peds_icu)

surg_centers$peds_specific<-car::recode(surg_centers$peds_icu_sum,"0=0;
													  			   1:51=1")

# surg_centers2 <- merge(surg_centers,
# 							 data5,
# 							 by="ibge",
# 							 all.x = TRUE)

# write.csv(surg_centers2,"/Users/Joao/Desktop/surg_centers.csv")


# PROCEDURES LEVEL
################################### 

#merging region to full dataset
data_procedures_region <- merge(
	data_procedures,states,by="ibge",all.x = TRUE)

#merging pediatric and mortality population to full dataset
data_procedures_region_pop <- merge(
	data_procedures_region,
	pediatric_population, by="ibge",all.x = TRUE)
	
#merging pediatric and mortality population to full dataset
data_procedures_region_pop_income <- merge(
	data_procedures_region,
	income_data, by="ibge",all.x = TRUE)

#merging availability of care to full dataset
availability_data$ibge<-availability_data$X...ibge
data_procedures_region_pop_income_avail <- merge(
	data_procedures_region_pop_income,
	availability_data, by="ibge",all.x = TRUE)

#merging pediatric and mortality population to full dataset
data_sih_full_by_mrocedures <- base::merge(
	data_procedures_region_pop_income_avail,
	surg_centers, by="CNES",all.x = TRUE)

# MUNICIPALITY LEVEL
################################### 
#calculating counts of procedures by municipality
data_sih_recoded %>% 
  group_by(procedure,ibge) %>%
  summarise(no_rows = length(procedure)) %>%
  spread(procedure,no_rows) -> data_sih_recoded_bycity

# table(data$procedure)

# write.csv(data,"/Users/joaovissoci/Desktop/peds_procedures.csv")

#merging with income classification
data_sih_recoded_bycity_income <- merge(
	data_sih_recoded_bycity,
	income_data,by="ibge",all.x = TRUE)

#merging region to full dataset
data_sih_recoded_bycity_income_region <- merge(
	data_sih_recoded_bycity_income,states,by="ibge",all.x = TRUE)

#merging pediatric and mortality population to full dataset
data_sih_recoded_bycity_income_region_pop <- merge(
	data_sih_recoded_bycity_income_region,
	pediatric_population, by="ibge",all.x = TRUE)

# #merging pediatric and mortality population to full dataset
# data_sih_recoded_bycity_income_region_pop_surg <- merge(
# 	data_sih_recoded_bycity_income_region,
# 	surg_centers, by="CNES",all.x = TRUE)
	
#merging availability of care to full dataset
data_sih_recoded_bycity_income_region_pop_avail <- merge(
	data_sih_recoded_bycity_income_region_pop,
	availability_data, by="ibge",all.x = TRUE)

#merging availability of care to full dataset
data_sih_full_by_municipality <- merge(
	data_sih_recoded_bycity_income_region_pop_avail,
	mortality_data, by="ibge",all.x = TRUE)

data_sih_full_by_municipality$Regio<-car::recode(data_sih_full_by_municipality$Regio,"NA='Centro-Oeste'")

NAto0<-function(x){
	car::recode(x,"NA=0")
	}

data_sih_full_by_municipality$Regio<-car::recode(data_sih_full_by_municipality$Regio,"NA='Centro-Oeste'")

data_sih_full_by_municipalityNAto01<-lapply(data_sih_full_by_municipality,NAto0)
data_sih_full_by_municipality<-as.data.frame(data_sih_full_by_municipalityNAto01)

write.csv(data_sih_full_by_municipality,"/Users/Joao/Desktop/data_sih_full.csv")


######################################################################
#TABLE 1
######################################################################

summary_region<- with(data_sih_full_by_municipality,
				 data.frame(pop,
				 			Regio))
summary_region<-na.omit(summary_region)
# NAto0<-function(x){
# 	car::recode(x,"NA=0")
# 	}

# procNAto01<-lapply(summary_region,NAto0)
# summary_regionzeroed<-as.data.frame(summary_region)

library(plyr)
pop<-plyr::ddply(summary_region, "Regio", 
	numcolwise(sum))


region<-with(data_sih_full_by_mrocedures,
	table(Regio,procedures))

prop.table(region,2))

region<-as.data.frame(region)
region$pop<-rep(pop$pop,5)
region$rates<-(region$Freq/region$pop)*10000

hosp<-with(data_sih_full_by_mrocedures,
	table(peds_specific,procedure))
hosp
prop.table(hosp,2)

hosp<-as.data.frame(hosp)
hosp$pop<-rep(pop$pop,5)
hosp$rates<-(hosp$Freq/hosp$pop)*10000



summary_region<- with(data_sih_full_by_municipality,
				 data.frame(pop,
				 			income_level))
summary_region<-na.omit(summary_region)
# NAto0<-function(x){
# 	car::recode(x,"NA=0")
# 	}

# procNAto01<-lapply(summary_region,NAto0)
# summary_regionzeroed<-as.data.frame(summary_region)

library(plyr)
pop<-plyr::ddply(summary_region, "income_level", 
	numcolwise(sum))


income<-with(data_sih_full_by_mrocedures,
	table(income_level,procedure))
income
prop.table(income,2)

income<-as.data.frame(income)
income$pop<-rep(pop$pop,5)
income$rates<-(income$Freq/income$pop)*10000


# MORTALITY

blabla<- with(data_sih_full_by_municipality,
				 data.frame(mortality,
				 			income_level))
blabla<-na.omit(blabla)

library(plyr)
ble<-plyr::ddply(blabla, "income_level", 
	numcolwise(sum))
(ble$mortality/pop$pop)*10000


blabla<- with(data_sih_full_by_municipality,
				 data.frame(mortality,
				 			Regio))
blabla<-na.omit(blabla)

library(plyr)
ble<-plyr::ddply(blabla, "Regio", 
	numcolwise(sum))
ble$mortality/sum(ble$mortality)
(ble$mortality/pop$pop)*10000


blabla<- with(data_sih_full_by_municipality,
				 data.frame(mortality,
				 			Regio))
blabla<-na.omit(blabla)

library(plyr)
ble<-plyr::ddply(blabla, "Regio", 
	numcolwise(sum))
ble$mortality/sum(ble$mortality)
(ble$mortality/pop$pop)*10000

data_sih_full_by_municipality
	
fm_nbin <- glm(mortality ~ income_level+
							  accessibility_index_pedicu2 +
							  accessibility_index_surgpeds +
							  pop +
							  apendectomy +
							  Regio,
	data = data_sih_full_by_municipality,
	family=poisson())
summary(fm_nbin)
exp(coef(fm_nbin))
exp(confint(fm_nbin,level=0.95))

data_sih_full_by_municipality$accessibility_index_pedicu2<-data_sih_full_by_municipality$accessibility_index_pedicu*10000

fm_nbin <- glm.nb(mortality ~ income_level+
							  accessibility_index_pedicu2 +
							  accessibility_index_surgpeds +
							  pop +
							  apendectomy +
							  Regio,
	data = data_sih_full_by_municipality)
summary(fm_nbin)
exp(coef(fm_nbin))
exp(confint(fm_nbin,level=0.95))

# Procedures level network
####################################################################

# data_infrastructure <- merge(data_sih,
# 							 infrastructure_data,
# 							 by="CNES",
# 							 all.x = TRUE)

# data_infrastructure$ibge<-data_infrastructure$ibge.x

# data_infrastructure2 <- merge(data_infrastructure,
# 							 income_data,
# 							 by="ibge",
# 							 all.x = TRUE)

network_data<-with(data_sih,data.frame(CNES,PROC_SOLIC_nome))

network_data_melted<-dcast(network_data,CNES ~ PROC_SOLIC_nome)[,-1658]

network_data_melted_try<-colSums(network_data_melted)

network_data_melted<-as.matrix(network_data_melted)

dataMatrix <- t(network_data_melted) %*% network_data_melted

qgraph(dataMatrix,layout="spring")
