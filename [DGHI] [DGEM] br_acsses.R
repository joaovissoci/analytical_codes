
library(readxl)


data_demog<-read_excel("/Users/Joao/Downloads/Base_diabeticos_hipertensos_limpa.xlsx")
data_ses<-read_excel("/Users/Joao/Downloads/IndicadoresSES.xlsx")
data_outcome<-read.csv("/Users/Joao/Downloads/br_acsmortality_data.csv")
data_outcome$ibge<-data_outcome[,1]
data_2step<-read.csv("/Users/Joao/Downloads/Indice_acessibilidade_sem_ecg2.csv")
data_2step$ibge<-data_2step[,1]
data_base<-read.csv("/Users/Joao/Downloads/Dados demograficos.csv")
data_base$ibge<-data_base[,1]

data_pop<-read.csv("/Users/Joao/Downloads/data_brazilpop.csv")
data_pop$ibge<-data_pop[,1]


#hospital access

#MERGE 1 2/ demographics

data_demog_subset<-with(data_demog,data.frame(at_hip_12,
											  at_di_12,
											  avc_2012,
											  infar_12,
											  ibge))

NAto0<-function(x){
	car::recode(x,"NA=0")
	}

data_demog_subset$infar_12<-NAto0(data_demog_subset$infar_12)
data_demog_subset$avc_2012<-NAto0(data_demog_subset$avc_2012)

data_base_plus_demog<-merge(x = data_base, 
					   y = data_demog_subset, 
					   by = "ibge", 
					   all.x = TRUE)

#MERGE 2

data_outcome_subset<-with(data_outcome,data.frame(mortality=X2012,
											  ibge))

data_base_plus_demog_plus_out<-merge(x = data_base_plus_demog, 
					   y = data_outcome_subset, 
					   by = "ibge", 
					   all.x = TRUE)

#MERGE 3 2/ 2 step

data_2step_subset<-with(data_2step,data.frame(accessibility_index_all_procedures,
											  ibge))

data_base_plus_demog_plus_out_plus_access<-merge(x = data_base_plus_demog_plus_out, 
					   y = data_2step_subset, 
					   by = "ibge", 
					   all.x = TRUE)

#MERGE 4 elderly pop

data_base_plus_demog_plus_out_plus_access_plus_pop<-merge(x = data_base_plus_demog_plus_out_plus_access, 
					   y = data_pop, 
					   by = "ibge", 
					   all.x = TRUE)

write.csv(data_base_plus_demog_plus_out_plus_access_plus_pop,"/Users/Joao/Desktop/br_ses_acs_data.csv")


