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
#IMPORTING DATA
#################################################################
#LOADING DATA FROM A .CSV FILE
# data<-read.csv("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/Brazil/acs/br_acsdiagnotic_data.csv",
# 	sep=",")

data_raw<-read.xlsx("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/Brazil/acs/br_acsdiagnostics_raw_data.xlsx",1)

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

data_riskfactors<-read.csv("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/Brazil/acs/br_acsriskfactor_data.csv")

##### EPI data
data <- read.spss("/Users/joaovissoci/Desktop/Modulo I e II.sav",
                       to.data.frame=TRUE)
data<-as.data.frame(data)

#################################################################
# DATA MANAGEMENT
#################################################################

#MERGING

data_full<-merge(data_raw,
                 data_riskfactors,
                 by=c("ibge"),
                 all.x=TRUE)


names(data_full)



