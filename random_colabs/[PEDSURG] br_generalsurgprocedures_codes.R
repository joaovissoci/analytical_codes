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
lapply(c("tidyverse"), 
library, character.only=T)

######################################################################
#IMPORTING DATA
######################################################################
load("/Users/Joao/Box Sync/Home Folder jnv4/Data/Pediatric Surgery/novas analises - pediatria 15 anos/jovens_sia.RData")
data_sia<-apendectomy

load("/Users/Joao/Box Sync/Home Folder jnv4/Data/Pediatric Surgery/novas analises - pediatria 15 anos/jovens_sih_rotulado.RData")
data_sih<-apendectomy

income_data<-read.csv("/Users/Joao/Box Sync/Home Folder jnv4/Data/Global EM/Brazil/acs/br_acsdiagnotic_data.csv")



# procedures<-levels(as.factor(apendectomy$PROC_SOLIC_nome))

# write.csv(procedures,"/Users/joaovissoci/Desktop/procedures.csv")

###################### SIA
#Subsetting the SIA data to include only General Pediatric Surgery Procedures
data_sia_subset<-subset(data_sia,data_sia$PROC_SOLIC_nome=="LAPAROTOMIA EXPLORADORA" |
						 data_sia$PROC_SOLIC_nome=="LAPAROTOMIA PARA AVALIAÇÃO DE TUMOR DE OVARIO EM ONCOLOGIA" |
						 data_sia$PROC_SOLIC_nome=="LAPAROTOMIA VIDEOLAPAROSCOPICA PARA DRENAGEM E/OU BIOPSIA" |
						 data_sia$PROC_SOLIC_nome=="REDUCAO CIRURGICA DE VOLVO POR LAPAROTOMIA" |
						 data_sia$PROC_SOLIC_nome=="COLOSTOMIA" |
						 data_sia$PROC_SOLIC_nome=="REPARACAO DE OUTRAS HERNIAS" |
						 data_sia$PROC_SOLIC_nome=="TRATAMENTO CIRÚRGICO DE HERNIA MUSCULAR" |
						 data_sia$PROC_SOLIC_nome=="HERNIORRAFIA INGUINAL VIDEOLAPAROSCOPICA" |
						 data_sia$PROC_SOLIC_nome=="HERNIORRAFIA C/ RESSECCAO INTESTINAL (HERNIA ESTRANGULADA)" |
						 data_sia$PROC_SOLIC_nome=="HERNIORRAFIA S/ RESSECCAO INTESTINAL (HERNIA ESTRANGULADA )" |
						 data_sia$PROC_SOLIC_nome=="HERNIORRAFIA UMBILICAL VIDEOLAPAROSCOPICA" |
						 data_sia$PROC_SOLIC_nome=="RESSUTURA DE PAREDE ABDOMINAL (POR DEISCENCIA TOTAL / EVISCERACAO)" |
						 data_sia$PROC_SOLIC_nome=="APENDICECTOMIA" |
						 data_sia$PROC_SOLIC_nome=="APENDICECTOMIA VIDEOLAPAROSCOPICA")

#Recoding procedures
data_sia_subset$procedure<-car::recode(data_sia_subset$PROC_SOLIC_nome,"
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
data_sia_recoded<-NULL
data_sia_recoded$ibge<-data_sia_subset$MUNIC_RES
data_sia_recoded$procedures<-data_sia_subset$procedure
data_sia_recoded<-as.data.frame(data_sia_recoded)

##################### SIH
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
data_sih_recoded$procedures<-data_sih_subset$procedure
data_sih_recoded<-as.data.frame(data_sih_recoded)

#Stacking datasets
data_procedures<-rbind(data_sih,data_sia)

#calculating counts of procedures by municipality
data_procedures %>% 
  group_by(procedures,ibge) %>%
  summarise(no_rows = length(procedures)) %>%
  spread(procedures,no_rows) -> data_procedures_bycity

# table(data$procedure)

# write.csv(data,"/Users/joaovissoci/Desktop/peds_procedures.csv")

#merging with income classification
data <- merge(data_procedures_bycity,income_data,by="ibge",all.x = TRUE)



