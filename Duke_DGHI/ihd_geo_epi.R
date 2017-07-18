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
data<-read.csv("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/Brazil/acs/br_acsdiagnotic_data.csv",
	sep=",")

#############################################################################
#DATA MANAGEMENT
#############################################################################
t
# regdata<-with(data,data.frame(Regio,SMR_IHD,Distance,
# 	distance75))

summary(data)

library(tidyverse)

# foo <- data.frame(a = 1:100,
#                   b = runif(100, 50, 200),
#                   stringsAsFactors = FALSE)


data= data %>%
    mutate(quantile2 = ntile(Indice_acessibilidade, 5))

#############################################################################
#POISSON MODEL
#############################################################################

data_1<-data[data$income_level=="Lower middle income",]
with(data_1,by(media_mortalidade,quantile2,summary))
with(data_1,kruskal.test(media_mortalidade~
	quantile2))

data_2<-data[data$income_level=="Upper middle income",]
with(data_2,by(media_mortalidade,quantile2,summary))
with(data_2,wilcox.test(media_mortalidade~
	quantile2))

data_3<-data[data$income_level=="High income",]
with(data_3,by(media_mortalidade,quantile2,summary))
with(data_3,wilcox.test(media_mortalidade~
	quantile2))

ggplot(data,aes(as.factor(quantile2),
	media_mortalidade)) + 
geom_boxplot() +
facet_grid(. ~ income_level)

data_1<-data[data$income_level=="Lower middle income",]

ggplot(data_1, aes(Indice_acessibilidade,
	media_mortalidade,
	color=income_level)) +
  geom_point() + geom_smooth(se = TRUE)

data_2<-data[data$income_level=="Upper middle income",]

ggplot(data_2, aes(Indice_acessibilidade,
	media_mortalidade,
	color=income_level)) +
  geom_point() + geom_smooth(se = TRUE)

data_3<-data[data$income_level=="High income",]

ggplot(data_3, aes(Indice_acessibilidade,
	media_mortalidade,
	color=income_level)) +
  geom_point() + geom_smooth(se = TRUE)
#,color=agglomeration

#See
# http://stats.stackexchange.com/questions/17006/interpretation-of-incidence-rate-ratios
# http://stats.stackexchange.com/questions/11096/how-to-interpret-coefficients-in-a-poisson-regression
#http://datavoreconsulting.com/programming-tips/count-data-glms-choosing-poisson-negative-binomial-zero-inflated-poisson/

fm_nbin <- glm(media_mortalidade ~ Indice_acessibilidade*income_level,
	data = data,
	family = poisson)
summary(fm_nbin)
exp(coef(fm_nbin))
exp(confint(fm_nbin,level=0.95))

1 - pchisq(summary(fm_nbin)$deviance,
           summary(fm_nbin)$df.residual
           )


fm_nbin <- glm.nb(media_mortalidade ~ Indice_acessibilidade *
		income_level, 
	data = data)
summary(fm_nbin)
exp(coef(fm_nbin))
exp(confint(fm_nbin,level=0.95))

1 - pchisq(summary(fm_nbin)$deviance,
           summary(fm_nbin)$df.residual
           )

fm_nbin <- glm.nb(PNT_IMP_13 ~ agglomeration + 
    lenght_km + population, 
  data = data)
summary(fm_nbin)
exp(coef(fm_nbin))
exp(confint(fm_nbin,level=0.95))

1 - pchisq(summary(fm_nbin)$deviance,
           summary(fm_nbin)$df.residual
           )

PNTCNT_noIMP <- with(data,PNTCNT-PNT_IMP_13)

fm_nbin <- glm.nb(PNTCNT_noIMP ~ PNTCNT_PUB + agglomeration + 
    lenght_km + population, 
  data = data)
summary(fm_nbin)
exp(coef(fm_nbin))
exp(confint(fm_nbin,level=0.95))

1 - pchisq(summary(fm_nbin)$deviance,
           summary(fm_nbin)$df.residual
           )

fm_nbin <- glm.nb(PNT_IMP_13 ~ point_off + agglomeration + 
    lenght_km + population, 
  data = data)
summary(fm_nbin)
exp(coef(fm_nbin))
exp(confint(fm_nbin,level=0.95))

1 - pchisq(summary(fm_nbin)$deviance,
           summary(fm_nbin)$df.residual
           )

fm_nbin <- glm.nb(PNT_IMP_13 ~ Points_on + agglomeration + 
    lenght_km + population, 
  data = data)
summary(fm_nbin)
exp(coef(fm_nbin))
exp(confint(fm_nbin,level=0.95))

1 - pchisq(summary(fm_nbin)$deviance,
           summary(fm_nbin)$df.residual
           )