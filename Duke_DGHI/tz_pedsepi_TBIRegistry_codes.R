## Peds TBI Table 1

#############################
## SETTING THE ENVIRONMENT ##
#############################

library(ggplot2)
library(stringr)
library(forcats)
library(lubridate)
library(likert)
library(dplyr)
library(tidyr)
library(tidyverse)
library(kableExtra)
library(knitr)
library(forcats)
library(VIM)
library(mice)
library(scales)

####################
## IMPORTING DATA ##
####################

#Difference between date of injury and date of arrival to KCMC
df0 <- read.csv("/Users/Joao/Box Sync/Home Folder jnv4/Data/Global EM/Africa/Tz/tbi_registry/tz_TBIregistry_data.csv", na.strings = c(" ",""))
# df_d <- read.csv("/Users/Loren/Desktop/RA_work/cleanPedsData.csv")
#############################
## CREATING THE DATA FRAME ##
#############################

#Subset of pediatric dataset (age<19years)
df<-subset(df0,age<19)

#####################
## DATA MANAGEMENT ##
#####################

# create morbidity variable
df_c <- mutate(df, morbidity = case_when(gos <=3 | gose<=4 ~ "BNegative Outcome",
                                       gos >3 | gose>4 ~ "APositive Outcome"))

# #Combine gos and gose into one column
# # df_c <- mutate(df_c, gosC = if_else(is.na(df$gose==TRUE), gos, gose))
# # # df_c$gose <- NULL
# df_c$delete<-df_c$gos
# df_c$delete_2<-df_c$gose


# create death variable defined properly
df_c <- mutate(df_c, death_new = case_when(gos ==1 | gose==1 ~ "Death",
                                       gos >1 | gose>1 ~ "Alive"))

# #Find frequencies for temperature > 38 or < or equal to 38. Create categorical variable
# CatTemp<-cut(df_c$temp,breaks=c(0,38,100),labels= c("A","B"))

# df_c <- mutate(df_c, temp_cat = case_when(temp > 38 | temp <= 1 ~ "Death",
#                                        gos >1 | gose>1 ~ "Alive"))

##### Error in as.Date.default(df$inj_date, %d/%m/%Y)
#convert injury date to R-readable format using as.Date
Rinjurydate<-as.Date(df_c$inj_date,"%d/%m/%Y")

#convert date arrival to R-readable format using as.Date
Rdatearrival<-as.Date(df_c$date_arrival,"%d/%m/%Y")

# Convert times to strings
df_c$inj_date <- as.character(df_c$inj_date)
df_c$inj_time <- as.character(df_c$inj_time)
df_c$date_arrival <- as.character(df_c$date_arrival)
df_c$time_arrival <- as.character(df_c$time_arrival)

# Combine date and time of injury column data in order to calculation duration between time of injury and time of arrival
df_c$datetime_injury <- paste(df_c$inj_date, df_c$inj_time, sep = " ")

df_c$datetime_arrival <- paste(df_c$date_arrival, df_c$time_arrival, sep = " ")

# Convert datetime variables to datetime vectors in R
df_c$datetime_injury <- as.POSIXct(df_c$datetime_injury, format = "%m/%d/%y %H:%M", tz = "EST")
df_c$datetime_arrival <- as.POSIXct(df_c$datetime_arrival, format = "%m/%d/%y %H:%M", tz = "EST")

# Calculate time between arrival and injury
df_c$arrival_injury <- as.numeric(difftime(df_c$datetime_arrival, df_c$datetime_injury, units = "mins"))
df_c$arrival_injury<-df_c$arrival_injury/60

### RECODE 

#1 is severe, 2 is moderate, 3 is mild 
df_c$gcs_tot_cat <-car::recode(df_c$gcs_tot,"0:8='severe'; 9:13='moderate';14:15='mild'")


df_c$alcohol_cat <- as.factor(df_c$alcohol)
df_c$avpu_cat <- as.factor(df_c$avpu)
df_c$gcs_tot_cat <- as.factor(df_c$gcs_tot_cat)
df_c$breath_cat <- as.factor(df_c$breath)
df_c$oxygen_cat <- as.factor(df_c$oxygen)
df_c$cxr_cat <- as.factor(df_c$cxr)
df_c$fluids <- car::recode(df_c$fluids,"2=0")
df_c$fluids_cat <- as.factor(df_c$fluids)
df_c$move_ext <- car::recode(df_c$move_ext,"2=0")
df_c$move_ext_cat <- as.factor(df_c$move_ext)
df_c$seizure_cat <- as.factor(df_c$seizure)
df_c$skull_cat <- as.factor(df_c$skull)
df_c$ct_brain_cat <- as.factor(df_c$ct_brain)
df_c$mannitol_cat <- as.factor(df_c$mannitol)
df_c$sz_med_given_cat <- as.factor(df_c$sz_med_given)
df_c$tbi_surgery_cat <- as.factor(df_c$tbi_surgery)
df_c$surgtoicu_cat <- as.factor(df_c$surgtoicu)
df_c$death_cat <- as.factor(df_c$death_new)
df_c$airway_cat <- as.factor(df_c$airway)
df_c$airway_mgmt_cat <- as.factor(df_c$airway_mgmt)
df_c$male <- as.factor(df_c$male)
df_c$moi <- as.factor(df_c$moi)
df_c$cd_dispo <- as.factor(df_c$cd_dispo)
df_c$morbidity <- as.factor(df_c$morbidity)
df_c$labs <- as.factor(df_c$labs)
df_c$other_surgery <- as.factor(df_c$other_surgery)

df_c$alcohol_cat<-car::recode(df_c$alcohol_cat, "2=NA")
df_c$alcohol_cat<-droplevels(df_c$alcohol_cat)

df_c$avpu_cat %>% fct_collapse("1" = c ("1","2")) -> df_c$avpu_cat
df_c$avpu_cat <- car::recode(df_c$avpu_cat, "3:2")
df_c$avpu_cat<-car::recode(df_c$avpu_cat, "3=NA")
df_c$avpu_cat<-droplevels(df_c$avpu_cat)

df_c$cxr_cat<-car::recode(df_c$cxr_cat, "3=1")
df_c$cxr_cat<-droplevels(df_c$cxr_cat)

df_c$skull_cat<-car::recode(df_c$skull_cat, "2=NA; 3=1")
df_c$skull_cat<-droplevels(df_c$skull_cat)

df_c$sz_med_given_cat<-car::recode(df_c$skull_cat, "2=NA; 3=NA")
df_c$sz_med_given_cat<-droplevels(df_c$skull_cat)

df_c$move_ext_cat<-car::recode(df_c$move_ext_cat, "3=0")
df_c$move_ext_cat<-droplevels(df_c$move_ext_cat)

# turn PTA (prior to arrival) into yes for mannitol, ct brain, and fluids

df_c$mannitol_cat <- car::recode(df_c$mannitol_cat, "3=1")
df_c$ct_brain_cat <- car::recode(df_c$ct_brain_cat, "3=1")
df_c$fluids_cat <- car::recode(df_c$fluids_cat, "3=1")

df_c$moi <- car::recode(df_c$moi, "0='rti';1='other';2='other';3='fall';4='other'")

df_c$airway_mgmt_cat <- car::recode(df_c$airway_mgmt_cat, "
  0='None';else='yes'")

##############################################
#data imputation
# aggr_plot <- aggr(df, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(pediatricdata), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# create meaningful subset of data

impData <- dplyr::select(df_c, age, male,
                   moi, alcohol_cat, temp, resp_rate, heart_rate, sys_bp, pulse_ox, nurse_vs,
                  avpu_cat, gcs_tot_cat, airway_cat, airway_mgmt_cat, breath_cat, oxygen_cat, cxr_cat, fluids_cat, move_ext_cat,
                  seizure_cat, skull_cat, ct_brain_cat, mannitol_cat, sz_med_given_cat, cd_dispo, tbi_surgery_cat,
                  surgtoicu_cat,labs, gcs_tot, other_surgery) 

#md.pattern(impData)

#Perform imputation using mice and pediatricdata dataset
tempdf<-mice(impData,m=1,maxit=50,seed=500)

# summary(tempdf)
df_d <- mice::complete(tempdf,1)

df_d$death<-df_c$death_cat
df_d$morbidity<-df_c$morbidity

df_d$hypox <-car::recode(df_d$pulse_ox,"0:92='hypoxic'; 93:100='normal'")
df_d$tension <- car::recode(df_d$sys_bp, "0:90='hypo'; 91:129 = 'normo'; 130:300 = 'hyper'")
#write.csv(df_d, "/Users/Loren/Desktop/RA_work/cleanPedsData.csv")

# Data inspection

ICUvGCS <- table(df_d$surgtoicu_cat, df_d$gcs_tot_cat)

#########################
## BIVARIATE ANALYSIS ###
########################
# Continuous variables get t test
# Categorical variables get chi squared test
#d.age
t.test(df$age~df$death)
#m.age
t.test(df$age~df$morbidity)

d.male=table(df$male, df$death)
chisq.test(d.male)
m.male=table(df$male, df$morbidity)
chisq.test(m.male)

table(df_c$moi)
d.moi=table(df$moi, df$death)
chisq.test(d.moi)
m.moi=table(df$moi, df$morbidity)
chisq.test(m.moi)
#Fishers
fisher.test(d.moi)
fisher.test(m.moi)

d.alcohol=table(df$alcohol, df$death_cat)
chisq.test(d.alcohol)
m.alcohol=table(df$alcohol, df$morbidity)
chisq.test(m.alcohol)
#Fishers
fisher.test(d.alcohol)
fisher.test(m.alcohol)

#d.temp
t.test(df$temp~df$death)
#m.temp
t.test(df$male~df$morbidity)

#d.resp_rate
t.test(df$resp_rate~df$death)
#m.resp_rate
t.test(df$resp_rate~df$morbidity)

#d.heart_rate
t.test(df$heart_rate~df$death)
#m.heart_rate
t.test(df$heart_rate~df$morbidity)

#d.sys_bp
t.test(df$sys_bp~df$death)
#m.sys_bp
t.test(df$sys_bp~df$morbidity)

#d.dia_bp
t.test(df$dia_bp~df$death)
#m.dia_bp
t.test(df$dia_bp~df$morbidity)

#d.pulse_ox
t.test(df$pulse_ox~df$death)
#m.pulse_ox
t.test(df$pulse_ox~df$morbidity)

d.nurse_vs=table(df$nurse_vs, df$death)
chisq.test(d.nurse_vs)
m.nurse_vs=table(df$nurse_vs, df$morbidity)
chisq.test(m.nurse_vs)

d.avpu=table(df$avpu, df$death)
chisq.test(d.avpu)
m.avpu=table(df$avpu, df$morbidity)
chisq.test(m.avpu)
#Fishers
fisher.test(d.avpu)
fisher.test(m.avpu)

d.gcs_tot=table(df$gcs_tot, df$death)
chisq.test(d.gcs_tot)
m.gcs_tot=table(df$gcs_tot, df$morbidity)
chisq.test(m.gcs_tot)
#Fishers
fisher.test(d.gcs_tot)
fisher.test(m.gcs_tot)

d.airway=table(df_d$airway_cat, df_d$death)
chisq.test(d.airway)
m.airway=table(df$airway, df$morbidity)
chisq.test(m.airway)
#Fishers
fisher.test(d.airway)
fisher.test(m.airway)

d.airway_mgmt=table(df_d$airway_mgmt_cat, df_d$death)
chisq.test(d.airway_mgmt)
fisher.test(d.airway_mgmt)
m.airway_mgmt=table(df$airway_mgmt, df$morbidity)
chisq.test(m.airway_mgmt)
#Fishers
fisher.test(d.airway_mgmt)
fisher.test(m.airway_mgmt)

d.breath=table(df$breath, df$death)
chisq.test(d.breath)
m.breath=table(df$breath, df$morbidity)
chisq.test(m.breath)
#Fishers
fisher.test(d.breath)
fisher.test(m.breath)

d.oxygen=table(df$oxygen, df$death)
chisq.test(d.oxygen)
m.oxygen=table(df$oxygen, df$morbidity)
chisq.test(m.oxygen)
#Fishers
fisher.test(d.oxygen)
fisher.test(m.oxygen)

d.cxr=table(df$cxr, df$death)
chisq.test(d.cxr)
m.cxr=table(df$cxr, df$morbidity)
chisq.test(m.cxr)
#Fishers
fisher.test(d.cxr)
fisher.test(m.cxr)

d.chesttube=table(df$chesttube, df$death)
chisq.test(d.chesttube)
m.chesttube=table(df$chesttube, df$morbidity)
chisq.test(m.chesttube)

d.fluids=table(df$fluids, df$death)
chisq.test(d.fluids)
m.fluids=table(df$fluids, df$morbidity)
chisq.test(m.fluids)
#Fishers
fisher.test(d.fluids)
fisher.test(m.fluids)

d.labs=table(df$labs, df$death)
chisq.test(d.labs)
m.labs=table(df$labs, df$morbidity)
chisq.test(m.labs)
#Fishers
fisher.test(d.labs)
fisher.test(m.labs)

d.move_ext=table(df$move_ext, df$death)
chisq.test(d.move_ext)
m.move_ext=table(df$move_ext, df$morbidity)
chisq.test(m.move_ext)
#Fishers
fisher.test(d.move_ext)
fisher.test(m.move_ext)

d.seizure=table(df$seizure, df$death)
chisq.test(d.seizure)
m.seizure=table(df$seizure, df$morbidity)
chisq.test(m.seizure)
#Fishers
fisher.test(d.seizure)
fisher.test(d.seizure)

d.skull=table(df$skull, df$death)
chisq.test(d.skull)
m.skull=table(df$skull, df$morbidity)
chisq.test(m.skull)
#Fishers
fisher.test(d.skull)
fisher.test(m.skull)

d.ct_brain=table(df$ct_brain, df$death)
chisq.test(d.ct_brain)
m.ct_brain=table(df$ct_brain, df$morbidity)
chisq.test(m.ct_brain)
#Fishers
fisher.test(d.ct_brain)
fisher.test(m.ct_brain)

d.mannitol=table(df$mannitol, df$death)
chisq.test(d.mannitol)
m.mannitol=table(df$mannitol, df$morbidity)
chisq.test(m.mannitol)
#Fishers
fisher.test(d.mannitol)
fisher.test(m.mannitol)

d.ccollar=table(df$ccollar, df$death)
chisq.test(d.ccollar)
m.ccollar=table(df$ccollar, df$morbidity)
chisq.test(m.ccollar)
#Fishers
fisher.test(d.ccollar)
fisher.test(m.ccollar)

d.sz_med_given=table(df$sz_med_given, df$death)
chisq.test(d.sz_med_given)
m.sz_med_given=table(df$sz_med_given, df$morbidity)
chisq.test(m.sz_med_given)
#Fishers
fisher.test(d.sz_med_given)
fisher.test(m.sz_med_given)

d.cd_dispo=table(df$cd_dispo, df$death)
chisq.test(d.cd_dispo)
m.cd_dispo=table(df$cd_dispo, df$morbidity)
chisq.test(m.cd_dispo)
#Fishers
fisher.test(d.cd_dispo)
fisher.test(m.cd_dispo)

d.tbi_surgery=table(df$tbi_surgery, df$death)
chisq.test(d.tbi_surgery)
m.tbi_surgery=table(df$tbi_surgery, df$morbidity)
chisq.test(m.tbi_surgery)
#Fishers
fisher.test(d.tbi_surgery)
fisher.test(m.tbi_surgery)

d.surgtoicu=table(df$surgtoicu, df$death)
chisq.test(d.surgtoicu)
m.surgtoicu=table(df$surgtoicu, df$morbidity)
chisq.test(m.surgtoicu)
#Fishers
fisher.test(d.surgtoicu)
fisher.test(m.surgtoicu)
###########################

############################
## MULTIVARIATE ANALYSIS ###
############################

df.model <- df_d %>%
dplyr::select(      age,
                    male,
                    moi,
                    alcohol_cat, 
                    pulse_ox, 
                    avpu_cat, 
                    gcs_tot_cat, 
                    breath_cat, 
                    #oxygen_cat, 
                    fluids_cat,
                    move_ext_cat, 
                    seizure_cat, 
                    skull_cat, 
                    ct_brain_cat, 
                    mannitol_cat, 
                    sz_med_given_cat, 
                    tbi_surgery_cat,
                    surgtoicu_cat,
                    death,
                    morbidity,
                    airway_mgmt_cat,
                    airway_cat,
                    labs,
                    other_surgery)

# md.pattern(df.model)


#multivariate logistic regression for all red variables
model1 <- glm(death ~ 
               age +
               male +
               moi +
               alcohol_cat+
               gcs_tot_cat+
               breath_cat+
               #oxygen_cat+
               fluids_cat+
               move_ext_cat +
               # airway_cat +
               airway_mgmt_cat +
               # labs +
               seizure_cat+
               skull_cat+
               ct_brain_cat+
               tbi_surgery_cat+
               surgtoicu_cat,
             data = df.model,family = binomial(link="logit"))

summary(model1)

round(exp(cbind(Odds=coef(model1),confint(model1,level=0.95))), 3)

dope<-as.data.frame(summary(model1)$coefficients)

round(exp(cbind(Odds=coef(model1),confint(model1,level=0.95))), 3)

tmp<-data.frame(cbind(exp(coef(model1)), exp(confint(model1))))
odds<-tmp[-1,]
names(odds)<-c("OR", "lower", "upper")
rownames(odds) <- c("Alcohol Involved",
                    "Moderate TBI",
                    "Severe TBI",
                    "Breathing Assessed",
                    "Oxygen Applied",
                    "Fluids Given",
                    "Moved Extremities",
                    "Airway Intact",
                    "Airway Managed",
                    "Seizure-like Activity",
                    "Had Skull Radiograph",
                    "Had Head CT Scan",
                    "Had TBI Surgery",
                    "Went from Surgery to ICU"
)
odds$vars<-row.names(odds)
dope2<-dope[2:15,]
odds$pval<-dope2$`Pr(>|z|)`
ticks<-c(seq(.1, 1, by =.1), seq(0, 10, by =1), seq(10, 100, by =10))

ggplot(odds, aes(y=OR , x = reorder(vars, OR))) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), color=ifelse(odds$pval <.05, "orange", "grey"), size=ifelse (odds$pval <.05, 1.2, .9), width=.2) +
  #scale_y_log10(breaks=ticks, labels = ticks) +
  geom_hline(yintercept = 1, linetype=2) +
  coord_flip() +
  labs(x = "Predictors", y = "Odds", title = "Odds for Pediatric TBI Mortality") +
  theme_bw()


#multivariate logistic regression for all red variables
#multivariate logistic regression for all red variables
model2 <- glm(morbidity ~ 
               age +
               male +
               moi +
               # alcohol_cat+
               gcs_tot_cat+
               breath_cat+
               #oxygen_cat+
               fluids_cat+
               move_ext_cat +
               # airway_cat +
               airway_mgmt_cat +
               # labs +
               seizure_cat+
               skull_cat+
               ct_brain_cat+
               tbi_surgery_cat+
               surgtoicu_cat,
             data = df.model,family = binomial(link="logit"))

summary(model2)

round(exp(cbind(Odds=coef(model2),confint(model2,level=0.95))), 3)

dope<-as.data.frame(summary(model2)$coefficients)

round(exp(cbind(Odds=coef(model2),confint(model2,level=0.95))), 3)

tmp<-data.frame(cbind(exp(coef(model2)), exp(confint(model2))))
odds<-tmp[-1,]
names(odds)<-c("OR", "lower", "upper")
rownames(odds) <- c("Alcohol Involved",
                    "Moderate TBI",
                    "Severe TBI",
                    "Breathing Assessed",
                    "Oxygen Applied",
                    "Fluids Given",
                    "Moved Extremities",
                    "Airway Intact",
                    "Airway Managed",
                    "Seizure-like Activity",
                    "Had Skull Radiograph",
                    "Had Head CT Scan",
                    "Had TBI Surgery",
                    "Went from Surgery to ICU"
)
odds$vars<-row.names(odds)
dope2<-dope[2:15,]
odds$pval<-dope2$`Pr(>|z|)`
ticks<-c(seq(.1, 1, by =.1), seq(0, 10, by =1), seq(10, 100, by =10))

ggplot(odds, aes(y=OR , x = reorder(vars, OR))) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), color=ifelse(odds$pval <.05, "orange", "grey"), size=ifelse (odds$pval <.05, 1.2, .9), width=.2) +
  #scale_y_log10(breaks=ticks, labels = ticks) +
  geom_hline(yintercept = 1, linetype=2) +
  coord_flip() +
  labs(x = "Predictors", y = "Odds", title = "Odds for Pediatric TBI Morbidity") +
  theme_bw()

# subgroup analysis
mild <- subset(df_d, df_d$gcs_tot_cat=="mild")
#multivariate logistic regression for all red variables
model3 <- glm(death ~ 
               age +
               male +
               moi +
               alcohol_cat+
               # gcs_tot_cat+
               breath_cat+
               #oxygen_cat+
               fluids_cat+
               move_ext_cat +
               # airway_cat +
               airway_mgmt_cat +
               # labs +
               seizure_cat+
               skull_cat+
               ct_brain_cat+
               tbi_surgery_cat+
               surgtoicu_cat,
             data = mild,family = binomial(link="logit"))

summary(model3)
round(exp(cbind(Odds=coef(model3),confint(model3,level=0.95))), 3)

model4 <- glm(morbidity ~ 
                 alcohol_cat+
                 #gcs_tot_cat+
                 breath_cat+
                # oxygen_cat+
                 fluids_cat+
                 move_ext_cat+
                 airway_cat + 
                 airway_mgmt_cat +
                 # labs +
                 seizure_cat+
                 skull_cat+
                 ct_brain_cat+
                 tbi_surgery_cat+
                 surgtoicu_cat+
              other_surgery,
               data = mild, family = binomial(link="logit"))

summary(model4)
round(exp(cbind(Odds=coef(model4),confint(model4,level=0.95))), 3)

mod.severe <- subset(df_d, df_d$gcs_tot_cat=="moderate"|df_d$gcs_tot_cat=="severe")
model5 <- glm(death ~ 
                alcohol_cat+
                #gcs_tot_cat+
                breath_cat+
                # oxygen_cat+
                fluids_cat+
                move_ext_cat+
                airway_cat + 
                airway_mgmt_cat +
                # labs +
                seizure_cat+
                skull_cat+
                ct_brain_cat+
                tbi_surgery_cat+
                surgtoicu_cat+
                other_surgery,
              data = mod.severe,family = binomial(link="logit"))

summary(model5)
round(exp(cbind(Odds=coef(model5),confint(model5,level=0.95))), 3)

model6 <- glm(morbidity ~ 
                alcohol_cat+
                #gcs_tot_cat+
                breath_cat+
                # oxygen_cat+
                fluids_cat+
                move_ext_cat+
                airway_cat + 
                airway_mgmt_cat +
                # labs +
                seizure_cat+
                skull_cat+
                ct_brain_cat+
                tbi_surgery_cat+
                surgtoicu_cat+
                other_surgery,
              data = mod.severe, family = binomial(link="logit"))

summary(model6)
round(exp(cbind(Odds=coef(model6),confint(model6,level=0.95))), 3)


#############################
## AGE RESULTS
df_d$gcs_tot_fact <- as.factor(df_d$gcs_tot)
df_d$age_cat <- as.factor(df_c$age)

ggplot(df_d, aes(x=gcs_tot_fact, y=age)) +
  geom_boxplot() +
  labs(title = "Age Boxplot")
  coord_flip(xlim = NULL, ylim = NULL, expand = TRUE)

ggplot(df_d, aes(x = age_cat, y = gcs_tot, fill = gcs_tot_cat)) + 
    geom_bar(position = "fill", stat = "identity") +
    scale_y_continuous(labels = percent_format())+
    labs(title = "Percentage of TBI Severity per Age", x = "Patient Age", y = "Percentage", fill = "TBI Severity")

ggplot(df_d, aes(x = age_cat, y = gcs_tot, fill = moi)) + 
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = percent_format())+
  labs(title = "Percentage of TBI Severity per Age", x = "Patient Age", y = "Percentage", fill = "TBI Severity")

df_d$male <- car::recode(df_d$male, "0='Female';1='Male'")

ggplot(df_d, aes(x=age, fill = male))+
  geom_histogram(binwidth = 0.5) +
  labs(title = "Number of Pediatric Patients per Age", x = "Patient Age", y = "Number of Patients", fill = "Gender")

#STOLEN FROM CYRUS -- Joao said use a stacked bar chart instead of this to make it easier to interpret
pick <- function(condition) {
  function(d) d %>% filter_(condition)
}
ggplot(df_d, aes(age)) +
  geom_histogram(data = pick(~gcs_tot_cat == "mild"),
                 fill = "#FFCC00",
                 binwidth = 0.5,
                 alpha = .7) +
  geom_histogram(data = pick(~gcs_tot_cat == "moderate"),
                 fill = "#FF9933",
                 binwidth = 0.5,
                 alpha = .9) +
  geom_histogram(data = pick(~gcs_tot_cat == "severe"),
                 fill = "#990000",
                 binwidth = 0.5,
                 alpha = .7) +
  theme(axis.text = element_text(size = 20)) +
  theme(axis.title = element_text(size = 25)) +
  theme_minimal() +
  labs(title = "Histogram of Age by GCS", subtitle = "Yellow = Mild, Orange = Moderate, Red = Severe", x= "Patient Age", y= "Number of patients") #+
