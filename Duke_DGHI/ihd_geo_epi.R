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
data<-read.csv("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/Brazil/acs/br_acsdiagnotic_data.csv",
	sep=",")

data_raw<-read.xlsx("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/Brazil/acs/br_acsdiagnostics_raw_data.xlsx",1)

#stacking up data

mortality<-with(data_raw,c(mort_2008,
                           mort_2009,
                           mort_2010,
                           mort_2011,
                           mort_2012,
                           mort_2013,
                           mort_2014))
population<-with(data_raw,c(pop_2008,
                           pop_2009,
                           pop_2010,
                           pop_2011,
                           pop_2012,
                           pop_2013,
                           pop_2014))
year<-c(rep(2008,length(data_raw$mort_2008)),
        rep(2009,length(data_raw$mort_2008)),
        rep(2010,length(data_raw$mort_2008)),
        rep(2011,length(data_raw$mort_2008)),
        rep(2012,length(data_raw$mort_2008)),
        rep(2013,length(data_raw$mort_2008)),
        rep(2014,length(data_raw$mort_2008)))
region<-with(data_raw,c(regi.c3..b5.es,
                    regi.c3..b5.es,
                    regi.c3..b5.es,
                    regi.c3..b5.es,
                    regi.c3..b5.es,
                    regi.c3..b5.es,
                    regi.c3..b5.es))
income<-with(data_raw,c(income_level,
                    income_level,
                    income_level,
                    income_level,
                    income_level,
                    income_level,
                    income_level))
access<-with(data,c(Indice_acessibilidade,
                    Indice_acessibilidade,
                    Indice_acessibilidade,
                    Indice_acessibilidade,
                    Indice_acessibilidade,
                    Indice_acessibilidade,
                    Indice_acessibilidade))

logmodel<-data.frame(mortality,population,year,region,income,accessibility=scale(access))

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
    mutate(quantile2 = ntile(Indice_acessibilidade, 3))

#############################################################################
#POISSON MODEL
#############################################################################
library(PMCMR)

with(data,by(media_mortalidade,income_level,summary))
with(data,kruskal.test(media_mortalidade~
  income_level))
with(data,posthoc.kruskal.nemenyi.test(x=media_mortalidade, 
                                       g=income_level, 
                                       method="Tukey"))

with(data,by(Indice_acessibilidade,income_level,summary))
with(data,kruskal.test(Indice_acessibilidade~
  income_level))
with(data,posthoc.kruskal.nemenyi.test(x=Indice_acessibilidade, 
                                       g=income_level, 
                                       method="Tukey"))

data_1<-data[data$income_level=="Lower middle income",]
with(data_1,by(media_mortalidade,quantile2,summary))
with(data_1,kruskal.test(media_mortalidade~
	quantile2))
with(data_1,posthoc.kruskal.nemenyi.test(x=media_mortalidade, 
                                       g=quantile2, 
                                       method="Tukey"))

data_2<-data[data$income_level=="Upper middle income",]
with(data_2,by(media_mortalidade,quantile2,summary))
with(data_2,kruskal.test(media_mortalidade~
	quantile2))
with(data_2,posthoc.kruskal.nemenyi.test(x=media_mortalidade, 
                                       g=quantile2, 
                                       method="Tukey"))

data_3<-data[data$income_level=="High income",]
with(data_3,by(media_mortalidade,quantile2,summary))
with(data_3,kruskal.test(media_mortalidade~
	quantile2))
with(data_3,posthoc.kruskal.nemenyi.test(x=media_mortalidade, 
                                       g=quantile2, 
                                       method="Tukey"))

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

data_log<-subset(logmodel,logmodel$year==2014)

write.csv(data_log,"/Users/joaovissoci/Desktop/ihd_data_2014.csv")

ggplot(data_log, aes(mortality, fill = as.factor(income))) +
  geom_histogram(position="dodge")

summary(m1 <- glm(mortality ~ as.factor(income) * 
                               access,
              family="poisson", 
              data=logmodel))

library(sandwich)
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
"Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
LL = coef(m1) - 1.96 * std.err,
UL = coef(m1) + 1.96 * std.err)

r.est

with(m1, cbind(res.deviance = deviance, df = df.residual,
  p = pchisq(deviance, df.residual, lower.tail=FALSE)))

library(msm)
s <- deltamethod(list(~ exp(x1), ~ exp(x2)),coef(m1), cov.m1)

## exponentiate old estimates dropping the p values
rexp.est <- exp(r.est[, -3])
## replace SEs with estimates for exponentiated coefficients
rexp.est[, "Robust SE"] <- s

rexp.est

fm_poisson <- glm(mortality ~ as.factor(income) *
                           # as.factor(region) +
                           # year +
                           accessibility,
                           offset(population),
	data = data_log,
	family = poisson)
summary(fm_poisson)
exp(coef(fm_poisson))
exp(confint(fm_poisson,level=0.95))

1 - pchisq(summary(fm_poisson)$deviance,
           summary(fm_poisson)$df.residual
           )


fm_nbin <- glm.nb(mortality ~ as.factor(income) *
                           # region,
                           # year +
                           accessibility, 
                           offset(population),
	data = data_log)
summary(fm_nbin)
exp(coef(fm_nbin))
exp(confint(fm_nbin,level=0.95))

1 - pchisq(summary(fm_nbin)$deviance,
           summary(fm_nbin)$df.residual
           )

install.packages("pscl")
library(pscl)
model.zip = zeroinfl(mortality ~ accessibility + 
                                 as.factor(income)|
                                 accessibility + 
                                 as.factor(income),
                     data = data_log,
                     offset(population))
summary(model.zip)
exp(coef(model.zip))
exp(confint(model.zip,level=0.95))

1 - pchisq(summary(model.zip)$deviance,
           summary(model.zip)$df.residual
           )

