load("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Pediatric Surgery/novas analises - pediatria 15 anos/jovens_sia.RData")
data_sia<-apendectomy

load("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Pediatric Surgery/novas analises - pediatria 15 anos/jovens_sih_rotulado.RData")
data_sih<-apendectomy


0WJP0ZZ
0WQFXZ2
0DSP0ZZ
0DSP4ZZ 
0DSP7ZZ
0YQ54ZZ 
0YQ64ZZ 
0YQ50ZZ 
0YQ60ZZ 
0WQF4ZZ 
0WUF07Z 
0WUF0KZ 
0WQF0ZZ
0DTJ4ZZ 
0DTJ0ZZ 
0BQR4ZZ 
0BQS4ZZ 
0BQR0ZZ 
0BQS0ZZ 

procedures<-levels(as.factor(apendectomy$PROC_SOLIC_nome))

write.csv(procedures,"/Users/joaovissoci/Desktop/procedures.csv")

data<-subset(apendectomy,apendectomy$PROC_SOLIC_nome=="LAPAROTOMIA EXPLORADORA" |
						 apendectomy$PROC_SOLIC_nome=="LAPAROTOMIA PARA AVALIAÇÃO DE TUMOR DE OVARIO EM ONCOLOGIA" |
						 apendectomy$PROC_SOLIC_nome=="LAPAROTOMIA VIDEOLAPAROSCOPICA PARA DRENAGEM E/OU BIOPSIA" |
						 apendectomy$PROC_SOLIC_nome=="REDUCAO CIRURGICA DE VOLVO POR LAPAROTOMIA" |
						 apendectomy$PROC_SOLIC_nome=="COLOSTOMIA" |
						 apendectomy$PROC_SOLIC_nome=="REPARACAO DE OUTRAS HERNIAS" |
						 apendectomy$PROC_SOLIC_nome=="TRATAMENTO CIRÚRGICO DE HERNIA MUSCULAR" |
						 apendectomy$PROC_SOLIC_nome=="HERNIORRAFIA INGUINAL VIDEOLAPAROSCOPICA" |
						 apendectomy$PROC_SOLIC_nome=="HERNIORRAFIA C/ RESSECCAO INTESTINAL (HERNIA ESTRANGULADA)" |
						 apendectomy$PROC_SOLIC_nome=="HERNIORRAFIA S/ RESSECCAO INTESTINAL (HERNIA ESTRANGULADA )" |
						 apendectomy$PROC_SOLIC_nome=="HERNIORRAFIA UMBILICAL VIDEOLAPAROSCOPICA" |
						 apendectomy$PROC_SOLIC_nome=="RESSUTURA DE PAREDE ABDOMINAL (POR DEISCENCIA TOTAL / EVISCERACAO)" |
						 apendectomy$PROC_SOLIC_nome=="APENDICECTOMIA" |
						 apendectomy$PROC_SOLIC_nome=="APENDICECTOMIA VIDEOLAPAROSCOPICA")

data$procedure<-car::recode(data$PROC_SOLIC_nome,"
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

table(data$procedure)

write.csv(data,"/Users/joaovissoci/Desktop/peds_procedures.csv")

income_data<-read.csv("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/Brazil/acs/br_acsdiagnotic_data.csv")

data$ibge<-data$MUNIC_RES

total <- merge(data,income_data,by="ibge",all.x = TRUE)

income_analysis<-data.frame(total$income_level,total$procedure)

with(total,prop.table(table(income_level,procedure),2))



