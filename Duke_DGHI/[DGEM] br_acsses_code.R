#################################################################
#IHD GEO EPI - BRAZIL
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
#All packages must be installes with install.packages() function
lapply(c("ggplot2", "psych", "RCurl", "irr", "nortest", 
	"moments","GPArotation","nFactors","boot","psy", "car",
	"vcd", "gridExtra","mi","VIM","epicalc","gdata","sqldf",
	"reshape2","mclust","foreign","survival","memisc","lme4",
	"lmerTest","dplyr","xlsx"),library, character.only=T)
#################################################################
#IMPORTING Data
#################################################################
#LOADING DATA FROM A .CSV FILE
# data<-read.csv("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/Brazil/acs/br_acsdiagnotic_data.csv",
# 	sep=",")

#base dataset
data_raw<-read.xlsx("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/Brazil/acs/br_acsdiagnostics_raw_data.xlsx",1)


#risk factor dataset
data_risk<-read.csv("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/Brazil/acs/br_acsriskfactor_data.csv")

#stacking up data

# mortality<-with(data_raw,c(mort_2008,
#                            mort_2009,
#                            mort_2010,
#                            mort_2011,
#                            mort_2012,
#                            mort_2013,
#                            mort_2014))
# population<-with(data_raw,c(pop_2008,
#                            pop_2009,
#                            pop_2010,
#                            pop_2011,
#                            pop_2012,
#                            pop_2013,
#                            pop_2014))
# year<-c(rep(2008,length(data_raw$mort_2008)),
#         rep(2009,length(data_raw$mort_2008)),
#         rep(2010,length(data_raw$mort_2008)),
#         rep(2011,length(data_raw$mort_2008)),
#         rep(2012,length(data_raw$mort_2008)),
#         rep(2013,length(data_raw$mort_2008)),
#         rep(2014,length(data_raw$mort_2008)))
# region<-with(data_raw,c(regi.c3..b5.es,
#                     regi.c3..b5.es,
#                     regi.c3..b5.es,
#                     regi.c3..b5.es,
#                     regi.c3..b5.es,
#                     regi.c3..b5.es,
#                     regi.c3..b5.es))
# income<-with(data_raw,c(income_level,
#                     income_level,
#                     income_level,
#                     income_level,
#                     income_level,
#                     income_level,
#                     income_level))
# access<-with(data,c(Indice_acessibilidade,
#                     Indice_acessibilidade,
#                     Indice_acessibilidade,
#                     Indice_acessibilidade,
#                     Indice_acessibilidade,
#                     Indice_acessibilidade,
#                     Indice_acessibilidade))

# logmodel<-data.frame(mortality,population,year,region,income,accessibility=scale(access))

# data_riskfactors<-read.csv("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/Brazil/acs/br_acsriskfactor_data.csv")

# ##### EPI data
# data <- read.spss("/Users/joaovissoci/Desktop/Modulo I e II.sav",
#                        to.data.frame=TRUE)
data<-as.data.frame(data)

#################################################################
# DATA MANAGEMENT
#################################################################

#MERGING

data_full<-merge(data_raw,
                 data_risk,
                 by=c("ibge"),
                 all.x=TRUE)

risk_factor_data<-with(data_risk,data.frame(
			at_hip_08,
			at_hip_09,
			at_hip_10,
			at_hip_11,
 			at_hip_12,
 			at_hip_13,
 			at_di_14,
 			at_di_08,
			at_di_09,
			at_di_10,
			at_di_11,
 			at_di_12,
 			at_di_13,
 			at_di_14,
 			di_cad_08,
			di_cad_09,
			di_cad_10,
			di_cad_11,
 			di_cad_12,
 			di_cad_13,
 			di_cad_14,
 			di_acom_08,
			di_acom_09,
			di_acom_10,
			di_acom_11,
 			di_acom_12,
 			di_acom_13,
 			di_acom_14,
 			hi_cad_08,
			hi_cad_09,
			hi_cad_10,
			hi_cad_11,
 			hi_cad_12,
 			hi_cad_13,
 			hi_cad_14,
 			hi_acom_08,
			hi_acom_09,
			hi_acom_10,
			hi_acom_11,
 			hi_acom_12,
 			hi_acom_13,
 			hi_acom_14,
 			avc_2008,
 			avc_2009,
 			avc_2010,
 			avc_2011,
 			avc_2012,
 			avc_2013,
 			avc_2014,
 			infar_08,
 			infar_09,
 			infar_10,
 			infar_11,
 			infar_12,
 			infar_13,
 			infar_14))

#pca
model <- principal(risk_factor_data ,nfactors=1, rotate='varimax', 
	scores=T, cov=T)
 L <- model$loadings            # Just get the loadings matrix
 S <- model$scores              # This gives an incorrect answer in the current version

 d <- model1_bea              # get your data
 dc <- scale(d,scale=FALSE)     # center the data but do not standardize it
 pca1 <- dc %*% L                 # scores are the centered data times the loadings
 # lowerCor(sc)                   #These scores, being principal components
#                                # should be orthogonal 

# plot(model)



names(data_full)



