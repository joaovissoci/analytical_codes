# # install packages
# install.packages("sem")
# install.packages("ggplot2")
# install.packages("psych")
# install.packages("RCurl")
# install.packages("irr")
# install.packages("nortest")
# install.packages("moments")
# install.packages("GPArotation")
# install.packages("nFactors")
# install.packages("boot")
# install.packages("psy")
# install.packages("car")
# install.packages("vcd")
# install.packages("gridExtra")
# install.packages("mi")
# install.packages("VIM")
# install.packages("nFactors")
# install.packages("gdata")
# install.packages("sqldf")
# install.packages("reshape2")
# install.packages("mclust")
# install.packages("foreign")
# install.packages("survival")
# install.packages("memisc")
# install.packages("lme4")
# install.packages("lmerTest")
# install.packages("dplyr")
# install.packages("QCA")
# install.packages("VennDiagram")
# install.packages("qgraph")
# install.packages("igraph")
# install.packages("ltm")
# install.packages("gmodels")
# install.packages("eRm")
# install.packages("mirt")
# install.packages("devtools")
# install.packages("reshape")
# install.packages("mice")
# install.packages("dplyr")

lapply(c("sem","ggplot2", "psych", "RCurl", "irr", "nortest", 
         "moments","GPArotation","nFactors","boot","psy", "car",
         "vcd", "gridExtra","mi","VIM","gdata","sqldf",
         "reshape2","mclust","foreign","survival","memisc","lme4",
         "lmerTest","dplyr","QCA","VennDiagram","qgraph","igraph",
         "ltm","gmodels","eRm","mirt","dplyr","devtools","reshape",
         "mice"),
       library, character.only=T)

# input the dataset
data <- read.csv("/Users/Joao/Box Sync/Home Folder jnv4/Data/Global EM/Africa/Tz/BNI/Tz_bnipatients_data.csv", header = TRUE)

# recode the reversing-word question
# data$alc_treatment_intelligent<-car::recode(data$alc_treatment_intelligent,"
#                                      1='6';2='5';3='4';
#                                      4='3';5='2';6='1'")
# data$alcoholic_trustworthy<-car::recode(data$alcoholic_trustworthy,"
#                                                    1='6';2='5';3='4';
#                                                    4='3';5='2';6='1'")
# data$alcoholic_close_friend<-car::recode(data$alcoholic_close_friend,"
#                                                    1='6';2='5';3='4';
#                                                    4='3';5='2';6='1'")
# data$recovered_alcoholic_teacher<-car::recode(data$recovered_alcoholic_teacher,"
#                                                        1='6';2='5';3='4';
#                                                        4='3';5='2';6='1'")
# data$recover_alcoholic_hired<-car::recode(data$recover_alcoholic_hired,"
#                                                  1='6';2='5';3='4';
#                                                  4='3';5='2';6='1'")
# data$recovered_alc_treat_same<-car::recode(data$recovered_alc_treat_same,"
#                                                       1='6';2='5';3='4';
#                                                       4='3';5='2';6='1'")

# one factor model
BNI_data<-with(data,data.frame(alc_treatment_intelligent,alcoholic_trustworthy,alc_treatment_failure,think_less_treated_person,
                               less_opinion_trtd_person,alcoholic_close_friend,recovered_alcoholic_teacher,recover_alcoholic_chldrn,
                               recover_alcoholic_hired,non_alcoholic_hired,recovered_alc_treat_same,no_date_hospital_for_alc))
# deal with missing data
#imputation: to create a new value to fill in blanks where the data is missing
data_imputed <- mice(BNI_data, seed = 2222, m=10)
#report the complete dataset with missing imputed
BNI_data<-mice::complete(data_imputed,4)

# reliability alpha should be larger than 0.70
psych::alpha(BNI_data,n.iter=1000,check.keys=TRUE)

# exploratory factor analysis 
#Function to exctract the factor loadings. 
#Arguments are DATA, Number of factors, rotation method.
#cor_auto: calculates the correlation matrix between vars, adjusting for the specific correlation that suites that relationship
cor_data<-cor_auto(BNI_data) 
fa(BNI_data,1,fm="pa",rotate="promax")
fa.poly(cor_data,1,fm="uls",rotate="oblimin")

## confirmatory factor analysis
cfa_model <- '
BNI_Devaluation =~ alc_treatment_intelligent+
                   think_less_treated_person + alcoholic_close_friend+recovered_alcoholic_teacher+
recover_alcoholic_hired+recovered_alc_treat_same+no_date_hospital_for_alc

'

fit <- lavaan::cfa(cfa_model,
                   data = BNI_data,
                   estimator="WLSMV",
                   ordered=colnames(BNI_data)
)

summary(fit, fit.measures=TRUE)
lavaan::fitMeasures(fit, fit.measures = c("rmsea.scaled",
                                          "rmsea.ci.lower.scaled",
                                          "rmsea.ci.upper.scaled",
                                          "cfi.scaled",
                                          "tli.scaled",
                                          "nnfi.scaled",
                                          "chisq.scaled",
                                          "pvalue.scaled"
)
)

#AIC(fit)
Est <- lavaan::parameterEstimates(fit, ci = TRUE, standardized = TRUE)
subset(Est, op == "=~")# factor loading
subset(Est, op == "~~")# error

#modification indexes
Mod <- lavaan::modificationIndices(fit)
subset(Mod, mi > 10) 


#two factor model 
#load dataset
BNI_Devaluation<-with(BNI_data,data.frame(alc_treatment_intelligent,alcoholic_trustworthy,alc_treatment_failure,
                                          think_less_treated_person,less_opinion_trtd_person))
BNI_Discrimination<-with(BNI_data,data.frame(alcoholic_close_friend,recovered_alcoholic_teacher,recover_alcoholic_chldrn,
                                             recover_alcoholic_hired,non_alcoholic_hired,recovered_alc_treat_same,no_date_hospital_for_alc))
#reliability
psych::alpha(BNI_Devaluation, n.iter = 1000, check.keys = TRUE)
psych::alpha(BNI_Discrimination, n.iter = 1000, check.keys = TRUE)
#exploratory factor analysis 
cor_data<-cor_auto(BNI_data)
fa(BNI_data,2,fm="pa",rotate="promax")
fa.poly(BNI_data,2,fm="uls",rotate="oblimin")

#confirmatory factor analysis
cfa_model <- '
BNI_Devaluation =~ alc_treatment_intelligent+
                   think_less_treated_person
BNI_Discrimination =~ alcoholic_close_friend+recovered_alcoholic_teacher+
recover_alcoholic_hired+recovered_alc_treat_same+no_date_hospital_for_alc
'

# alcoholic_trustworthy less_opinion_trtd_person recover_alcoholic_chldrn
# non_alcoholic_hired

fit<-lavaan::cfa(cfa_model,
                 data=BNI_data,
                 estimator='WLSMV',
                 ordered=colnames(BNI_data)
                 )

summary(fit,fit.measures=TRUE)
lavaan::fitmeasures(fit, fit.measures =c("rmsea.scaled",
                                         "rmsea.ci.lower.scaled",
                                         "rmsea.ci.upper.scaled",
                                         "cfi.scaled",
                                         "tli.scaled",
                                         "nnfi.scaled",
                                         "chisq.scaled",
                                         "pvalue.scaled")
                      )
Est=lavaan::parameterestimates(fit,ci=TRUE,standardized=TRUE)
subset(Est,op=="=~")
subset(Est,op=="~~")
#modification indexes
mod<-lavaan::modificationIndices(fit)
subset(mod,mi>10)


#positively and negatively worded items
#load dataset
BNI_positive<-with(BNI_data, data.frame(alc_treatment_intelligent,alcoholic_trustworthy,alcoholic_close_friend,
                                        recovered_alcoholic_teacher,recover_alcoholic_hired,recovered_alc_treat_same))
BNI_negative<-with(BNI_data,data.frame(alc_treatment_failure,think_less_treated_person,less_opinion_trtd_person,
                                       recover_alcoholic_chldrn,non_alcoholic_hired,no_date_hospital_for_alc))
#reliability
psych::alpha(BNI_positive,n.iter=1000,check.keys = TRUE)
psych::alpha(BNI_negative,n.iter=1000,check.keys = TRUE)
#exploratory factor analysis 
cor_data<-cor_auto(BNI_data)
fa(BNI_data,2,fm="pa",rotate="promax")
fa.poly(BNI_data,2,fm="uls",rotate="oblimin")
#confirmatory factor analysis
cfa<-'
BNI_positive=~ alc_treatment_intelligent+alcoholic_trustworthy+alcoholic_close_friend+
  recovered_alcoholic_teacher+recover_alcoholic_hired+recovered_alc_treat_same
BNI_negative=~ alc_treatment_failure+think_less_treated_person+less_opinion_trtd_person+
                                       recover_alcoholic_chldrn+non_alcoholic_hired+no_date_hospital_for_alc
'
fit<-lavaan::cfa(cfa,
                 data=BNI_data,
                 estimator='WLSMV',
                 ordered=colnames(BNI_data)
)

summary(fit,fit.measures=TRUE)
lavaan::fitmeasures(fit, fit.measures =c("rmsea.scaled",
                                         "rmsea.ci.lower.scaled",
                                         "rmsea.ci.upper.scaled",
                                         "cfi.scaled",
                                         "tli.scaled",
                                         "nnfi.scaled",
                                         "chisq.scaled",
                                         "pvalue.scaled")
)
Est=lavaan::parameterestimates(fit,ci=TRUE,standardized=TRUE)
subset(Est,op=="=~")
subset(Est,op=="~~")
#modification indexes
mod<-lavaan::modificationIndices(fit)
subset(mod,mi>10)


#one factor+latent method factor
BNI_data<-with(data,data.frame(alc_treatment_intelligent,alcoholic_trustworthy,alc_treatment_failure,think_less_treated_person,
                               less_opinion_trtd_person,alcoholic_close_friend,recovered_alcoholic_teacher,recover_alcoholic_chldrn,
                               recover_alcoholic_hired,non_alcoholic_hired,recovered_alc_treat_same,no_date_hospital_for_alc))
BNI_positive<-with(BNI_data, data.frame(alc_treatment_intelligent,alcoholic_trustworthy,alcoholic_close_friend,
                                        recovered_alcoholic_teacher,recover_alcoholic_hired,recovered_alc_treat_same))
#exploratory factor analysis 
cor_data<-cor_auto(BNI_data)
fa(BNI_data,2,fm="pa",rotate="promax")
fa.poly(BNI_data,2,fm="uls",rotate="oblimin")

#confirmatory factor analysis
cfa_model <- '
BNI =~ alc_treatment_intelligent+alcoholic_trustworthy+alc_treatment_failure+think_less_treated_person+
less_opinion_trtd_person+alcoholic_close_friend+recovered_alcoholic_teacher+recover_alcoholic_chldrn+
recover_alcoholic_hired+non_alcoholic_hired+recovered_alc_treat_same+no_date_hospital_for_alc
BNI_positive=~ alc_treatment_intelligent+alcoholic_trustworthy+alcoholic_close_friend+
  recovered_alcoholic_teacher+recover_alcoholic_hired+recovered_alc_treat_same
'

fit<-lavaan::cfa(cfa_model,
                 data=BNI_data,
                 estimator='WLSMV',
                 ordered=colnames(BNI_data)
)

summary(fit,fit.measures=TRUE)
lavaan::fitmeasures(fit, fit.measures =c("rmsea.scaled",
                                         "rmsea.ci.lower.scaled",
                                         "rmsea.ci.upper.scaled",
                                         "cfi.scaled",
                                         "tli.scaled",
                                         "nnfi.scaled",
                                         "chisq.scaled",
                                         "pvalue.scaled")
)

Est=lavaan::parameterestimates(fit,ci=TRUE,standardized=TRUE)
subset(Est,op=="=~")
subset(Est,op=="~~")
#modification indexes
mod<-lavaan::modificationIndices(fit)
subset(mod,mi>10)

#two factors+latent method factor 
BNI_Devaluation<-with(BNI_data,data.frame(alc_treatment_intelligent,alcoholic_trustworthy,alc_treatment_failure,
                                          think_less_treated_person,less_opinion_trtd_person))
BNI_Discrimination<-with(BNI_data,data.frame(alcoholic_close_friend,recovered_alcoholic_teacher,recover_alcoholic_chldrn,
                                             recover_alcoholic_hired,non_alcoholic_hired,recovered_alc_treat_same,no_date_hospital_for_alc))
BNI_positive<-with(BNI_data, data.frame(alc_treatment_intelligent,alcoholic_trustworthy,alcoholic_close_friend,
                                        recovered_alcoholic_teacher,recover_alcoholic_hired,recovered_alc_treat_same))
#exploratory factor analysis
cor_data<-cor_auto(BNI_data)
fa(BNI_data,3,fm="pa",rotate="promax")
fa.poly(BNI_data,3,fm="uls",rotate="oblimin")

#confirmatory factor analysis
cfa_model <- '
BNI_Devaluation =~ alc_treatment_intelligent+alcoholic_trustworthy+alc_treatment_failure+
                        less_opinion_trtd_person
BNI_Discrimination =~ alcoholic_close_friend+recovered_alcoholic_teacher+
recover_alcoholic_hired+recovered_alc_treat_same+no_date_hospital_for_alc
BNI_negative=~ alc_treatment_failure+less_opinion_trtd_person+
                                       no_date_hospital_for_alc
'
#excluded think_less_treated_person recover_alcoholic_chldrn non_alcoholic_hired

fit<-lavaan::cfa(cfa_model,
                 data=BNI_data,
                 estimator='WLSMV',
                 ordered=colnames(BNI_data)
)
 
summary(fit,fit.measures=TRUE)
lavaan::fitmeasures(fit, fit.measures =c("rmsea.scaled",
                                         "rmsea.ci.lower.scaled",
                                         "rmsea.ci.upper.scaled",
                                         "cfi.scaled",
                                         "tli.scaled",
                                         "nnfi.scaled",
                                         "chisq.scaled",
                                         "pvalue.scaled")
)

Est=lavaan::parameterestimates(fit,ci=TRUE,standardized=TRUE)
subset(Est,op=="=~")
subset(Est,op=="~~")
#modification indexes
mod<-lavaan::modificationIndices(fit)
subset(mod,mi>10)

