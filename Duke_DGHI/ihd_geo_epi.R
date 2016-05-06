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
	"lmerTest","dplyr"),library, character.only=T)
#################################################################
#IMPORTING DATA
#################################################################
#LOADING DATA FROM A .CSV FILE
data<-read.csv("/Users/jnv4/OneDrive - Duke University/datasets/DGHI/Brazil/epidemiologic transition/ihd_cities.csv",
	sep=",")

#############################################################################
#DATA MANAGEMENT
#############################################################################

regdata<-with(data,data.frame(Regio,SMR_IHD,Distance,
	distance75))

#############################################################################
#MULTILEVEL MODELING
#############################################################################

model1<-lmer(SMR_IHD ~ Regio * Distance + (1 + Distance | Regio), 
	data=regdata)

summary(model1) 

difflsmeans(model1)
plot(difflsmeans(model1, test.effs="groups:moments"))
lsmeans(model1)
confint(model1,level=0.95)
lmmpower(model1, pct.change = 0.30, t = seq(0,5,1), power = 0.80)

#model1_2 = lmer(outcome ~ groups + moments + (1|subject),data = nlmedata,REML=FALSE)

#model1_3 = lmer(outcome ~ groups + (1|subject),data = nlmedata,REML=FALSE)

#anova(model1,model1_2,model1_3)

##############################################################################
#END
##############################################################################