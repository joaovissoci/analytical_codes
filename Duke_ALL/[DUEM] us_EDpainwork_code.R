######################################################################
#BASIC R STATISTICS TEMPLATE
######################################################################
#
#
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
lapply(c("sem","ggplot2", "psych", "irr", "nortest", "moments",
	"GPArotation","nFactors","boot","psy", "car","vcd", "gridExtra",
	"mi","VIM","epicalc","gdata","sqldf","reshape2","mclust",
	"foreign","survival","memisc","foreign","mice","MissMech"), 
library, character.only=T)

######################################################################
#IMPORTING DATA
######################################################################
#LOADING DATA FROM A .CSV FILE
data_raw<-read.csv("/Users/joaovissoci/Box Sync/Home Folder jnv4/Data/Global EM/AMPED/US_AMPEDPain_data.csv",sep=",")
#information between " " are the path to the directory in your computer where the data is stored

######################################################################
#DATA MANAGEMENT
######################################################################
#Creating a data frame (group of variables)

data<-with(data_raw,data.frame(
					age=X...age,
					gender,
					race,
					eDx_Primary,
					PainScore,
					PainScoreDischarge,
					treatreg,
					# day2_Back2Work,
					# day3_Back2Work,
					# day4_Back2Work,
					# day5_Back2Work,
					# day2_WEffectiveness,
					# day3_WEffectiveness,
					# day4_WEffectiveness,
					# day5_WEffectiveness,
					WorkStatus,
					retWrkDate))

data$gender<-car::recode(data$gender,"'F'='F';'M'='M';else=NA")

data$race<-car::recode(data$race,"'A'='A';
									  'AA'='AA';
									  'C'='C';
									  'H'='H';
									  'Other'='Other';
									  'U'='U';	
									  else=NA")

data$WorkStatus<-car::recode(data$WorkStatus,"'Disabled'='Not-employed';
									  'PartTimeG50'='Part time';
									  'PartTimeL50'='Part time';
									  'Retired'='Not-employed';
									  'Student'='Not-employed';
									  'Unemployed'='Not-employed';
									  'FullTime'='Full time';	
									  else=NA")


#ANALYZING MISSING DATA
#######################################################
#Studying missing data
#Calculating frequency of missing data per variable
propmiss <- function(dataframe) lapply(dataframe,function(x) data.frame(nmiss=sum(is.na(x)), n=length(x), propmiss=sum(is.na(x))/length(x)))

propmiss(data)

# #inspecting measure random of missing data
# # #Inspectif Weather Conditions
# weather_missing<-car::recode(data_epi$weather_condition,"NA=0;else=1")
# logmodel<-glm(day2_PhysVisit ~ age,family=binomial, data=data)
# summary(logmodel)
# logmodel<-glm(weather_missing ~  data_epi$hour_crash,family=binomial)
# summary(logmodel)
# logmodel<-glm(weather_missing ~  data_epi$road_type,family=binomial)
# summary(logmodel)
# logmodel<-glm(weather_missing ~  data_epi$road_condition,family=binomial)
# summary(logmodel)
# logmodel<-glm(weather_missing ~  data_epi$visibility,family=binomial)
# summary(logmodel)
# logmodel<-glm(weather_missing ~  data_epi$type_vehicle,family=binomial)
# summary(logmodel)
# logmodel<-glm(weather_missing ~  data_epi$type_vehicle2,family=binomial)
# summary(logmodel)
# logmodel<-glm(weather_missing ~  data_epi$gender,family=binomial)
# summary(logmodel)
# logmodel<-glm(weather_missing ~  data_epi$crash_type,family=binomial)
# summary(logmodel)

#out <- TestMCARNormality(data_epi)
#missing.pattern.plot(data_epi)
#MICE framework for imputation
# # describing the pattern of missingnesss
# md.pattern(data)

# # # showing pairs of missingines
# md.pairs(data)

# # plots impact of missing data for a set of pairs - works better for numerical data
# marginplot(data.frame(data_epi$outcome,data_epi$visibility), col = mdc(1:2), cex = 1.2, cex.lab = 1.2, cex.numbers = 1.3, pch = 19)

# # generate imputations

# # organize data set to be imputed
# data_tobeimp<-with(data_epi,data.frame(hour_crash,urban_location,outcome,
# 	holiday,day_week,crash_type,rd_condition,weather,light_condition,
# 	type_location,traffic_control,speed_limit_sign,type_vehicle,
# 	human_crash_factor,ped_precrash,alcohol_tested,victim_classification,
# 	rd_size))

# # argument method=c("") indicated the imputation system (see Table 1 in http://www.jstatsoft.org/article/view/v045i03). Leaving "" to the position of the variable in the method argument excludes the targeted variable from the imputation.
imp <- mice(data, seed = 2222, m=5)

# # reports the complete dataset with missing imputated. It returns 5 options of datasets, witht he 5 imputation possibilities. To choose a specific option, add # as argument. Ex. complete(imp,2)
data_imputed<-complete(imp,4)

# #Plost the distrbution of each of the 5 possibilities of imputations
# #stripplot(imp,pch=20,cex=1.2)

# #plots a scatter plot of pairs of variables
# #xyplot(imp, outcome ~ visibility | .imp, pch = 20, cex = 1.4)

# #returns the matrix specifying each variable used to -predict imputation - columns 1=predictor 0=not predictor. rows are the variables of interest
# #imp$predictorMatrix
# #pred <- imp$predictorMatrix #if you want to exclude  variable from the prediction model for imputation then assign an obect to pred
# #pred[, "bmi"] <- 0 #transform the column values into 0's for not predictiong
# #imp <- mice(nhanes, pred = pred, pri = FALSE) # rerun the model specifying pred argumento witht eh matriz recoded.


## Adding working variable
data_full<-data.frame(data_imputed,
					day2_Back2Work=data_raw$day2_Back2Work,
					day3_Back2Work=data_raw$day3_Back2Work,
					day4_Back2Work=data_raw$day4_Back2Work,
					day5_Back2Work=data_raw$day5_Back2Work,
					day2_WEffectiveness=data_raw$day2_WEffectiveness,
					day3_WEffectiveness=data_raw$day3_WEffectiveness,
					day4_WEffectiveness=data_raw$day4_WEffectiveness,
					day5_WEffectiveness=data_raw$day5_WEffectiveness,
					day2_MaxPainScore=data_raw$day2_MaxPainScore,
					day3_MaxPainScore=data_raw$day3_MaxPainScore,
					day4_MaxPainScore=data_raw$day4_MaxPainScore,
					day5_MaxPainScore=data_raw$day5_MaxPainScore,
					day2_MinPainScore=data_raw$day2_MinPainScore,
					day3_MinPainScore=data_raw$day3_MinPainScore,
					day4_MinPainScore=data_raw$day4_MinPainScore,
					day5_MinPainScore=data_raw$day5_MinPainScore)

NAto0<-function(x){
	car::recode(x,"NA=0")
	}

data_full_NAto0<-lapply(data_full,NAto0)


Neg1toNA<-function(x){
	car::recode(x,"-1=NA")
	}

data_full<-lapply(data_full,Neg1toNA)

data_full<-as.data.frame(data_full)

#Unscheduled visits
#######################################################

# for (i in 1:nrow(data_imputed))
# {
#  if (data_imputed$day2_PhysVisit[i] == 1 || 
#  	 data_imputed$day3_PhysVisit[i] == 1 || 
#  	 data_imputed$day4_PhysVisit[i] == 1 || 
#  	 data_imputed$day5_PhysVisit[i] == 1)
     
#      {
#        data_imputed$unscheduled_visits[i] <- "yes"
#      } else # if (v1[i] == "Sim" || 
#      {
#        data_imputed$unscheduled_visits[i] <-"no"
#      } # else if (v1[i] == "Sim" || 
# } # for (i in 1:nrow(dataframe)

for (i in 1:nrow(data_full))
{
 if (data_full_NAto0$day2_Back2Work[i] == 1)
 	{ 
       data_full$return_to_work_day[i] <- 2

 	} else if (data_full_NAto0$day3_Back2Work[i] == 1)

 	{
        data_full$return_to_work_day[i] <- 3

 	} else if (data_full_NAto0$day4_Back2Work[i] == 1)

 	{
        data_full$return_to_work_day[i] <- 4
 	
 	} else if (data_full_NAto0$day5_Back2Work[i] == 1)

 	{
        data_full$return_to_work_day[i] <- 5

     } else # if (v1[i] == "Sim" || 
     {
       data_full$return_to_work_day[i] <- NA
     } # else if (v1[i] == "Sim" || 
} # for (i in 1:nrow(dataframe)

data_returntowork<-subset(data_full,data_full$WorkStatus != "Not-employed")

######################################################################
#TABLE 1
######################################################################

#Treatment Regimin
table<-with(data_imputed,table(treatreg))
table
prop.table(table)

# Gender
table<-with(data_imputed,table(gender))
table
prop.table(table)
table<-with(data_imputed,table(gender,treatreg))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Age
with(data_imputed,describe(age))
with(data_imputed,describeBy(age,treatreg))
# t-test: # independent 2-group, 2 level IV
anova<-with(data_imputed,aov(age ~ treatreg))
summary(anova)

# Race
table<-with(data_imputed,table(race))
table
prop.table(table)
table<-with(data_imputed,table(race,treatreg))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

#Employment
table<-with(data_full,table(WorkStatus))
table
prop.table(table)

# Primary Diag
table<-with(data_imputed,table(eDx_Primary))
table
prop.table(table)
table<-with(data_imputed,table(eDx_Primary,treatreg))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Pain at Arrival
with(data_imputed,describe(PainScore))
with(data_imputed,describeBy(PainScore,treatreg))
# t-test: # independent 2-group, 2 level IV
anova<-with(data_imputed,aov(PainScore ~ treatreg))
summary(anova)

# Pain at DIscharge
with(data_imputed,describe(PainScoreDischarge))
with(data_imputed,describeBy(PainScoreDischarge,treatreg))
# t-test: # independent 2-group, 2 level IV
anova<-with(data_imputed,aov(PainScoreDischarge ~ treatreg))
summary(anova)

# Primary Diag
table<-with(data_imputed,table(eDx_Primary))
table
prop.table(table)
table<-with(data_imputed,table(eDx_Primary,treatreg))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Primary Diag
table<-with(data_imputed,table(unscheduled_visits))
table
prop.table(table)
table<-with(data_imputed,table(unscheduled_visits,treatreg))
table
prop.table(table,2)
chisq.test(table)
fisher.test(table)
assocstats(table) #vcd package

# Pain at DIscharge
with(data_imputed,describe(unscheduled_visits_day))
with(data_imputed,describeBy(unscheduled_visits_day,treatreg))
# t-test: # independent 2-group, 2 level IV
anova<-with(data_imputed,aov(unscheduled_visits_day ~ treatreg))
summary(anova)

# Days to work
with(data_full,describe(return_to_work_day))

######################################################################
#Figure 1
######################################################################

 tblFun <- function(x){
    tbl <- table(x)
    res <- cbind(tbl,round(prop.table(tbl)*100,2))
    # rownames(res) <- c('yes','no')
    # res[,4:6]
}

 summaryFun<- function(x){ 
 	sum<-summary(x)
 	sum[4]
 }


plot_data<-do.call(rbind,lapply(data_full[10:13],tblFun))

max_pain<-do.call(rbind,lapply(data_full[18:21],summaryFun))

min_pain<-do.call(rbind,lapply(data_full[22:25],summaryFun))

rownames(plot_data)<-c("day2yes","day2no",
				 "day3yes","day3no",
				 "day4yes","day4no",
				 "day5yes","day5no")

plot_data<-as.data.frame(plot_data)

plot_data$FUP<-c("day2","day2",
				 "day3","day3",
				 "day4","day4",
				 "day5","day5")


plot_data$outcome<-c("yes","no",
				 "yes","no",
				 "yes","no",
				 "yes","no")

barplot<-data.frame(value=c(min_pain,max_pain),
					FUP=as.factor(c("day2","day3","day4","day5")),
					Pain=c(rep("Min Pain",4),rep("Max Pain",4)))

library(ggplot2)
# Basic line plot with points
P <- ggplot() +
	geom_bar(data=barplot,aes(x=FUP,y=value*10,group=Pain,fill=Pain),
		stat="identity",
		alpha=0.5,
		position=position_dodge()) + 
	scale_fill_brewer(palette="Paired")
  
P

P <- P + geom_line(data=plot_data[plot_data$outcome=="yes",], 
	aes(x=FUP, y=V2,group=outcome,color=outcome),size=2) +
  	geom_point(size=3)

P

P <- P + theme_minimal()

P

library(lsr)

with(data_full,describeBy(day2_MaxPainScore,day2_Back2Work))
with(data_full,t.test(day2_MaxPainScore~day2_Back2Work))
with(data_full,cohensD(day2_MaxPainScore~day2_Back2Work))

with(data_full,describeBy(day2_MinPainScore,day2_Back2Work))
with(data_full,t.test(day2_MinPainScore~day2_Back2Work))
with(data_full,cohensD(day2_MinPainScore~day2_Back2Work))

with(data_full,describeBy(day3_MaxPainScore,day3_Back2Work))
with(data_full,t.test(day3_MaxPainScore~day3_Back2Work))
with(data_full,cohensD(day3_MaxPainScore~day3_Back2Work))

with(data_full,describeBy(day3_MinPainScore,day3_Back2Work))
with(data_full,t.test(day3_MinPainScore~day3_Back2Work))
with(data_full,cohensD(day3_MinPainScore~day3_Back2Work))

with(data_full,describeBy(day4_MaxPainScore,day4_Back2Work))
with(data_full,t.test(day4_MaxPainScore~day4_Back2Work))
with(data_full,cohensD(day4_MaxPainScore~day4_Back2Work))

with(data_full,describeBy(day4_MinPainScore,day4_Back2Work))
with(data_full,t.test(day4_MinPainScore~day4_Back2Work))
with(data_full,cohensD(day4_MinPainScore~day4_Back2Work))

with(data_full,describeBy(day5_MaxPainScore,day5_Back2Work))
with(data_full,t.test(day5_MaxPainScore~day5_Back2Work))
with(data_full,cohensD(day5_MaxPainScore~day5_Back2Work))

with(data_full,describeBy(day5_MinPainScore,day5_Back2Work))
with(data_full,t.test(day5_MinPainScore~day5_Back2Work))
with(data_full,cohensD(day5_MinPainScore~day5_Back2Work))


aov()



# # Change the line type
# ggplot(data=df, aes(x=dose, y=len, group=1)) +
#   geom_line(linetype = "dashed")+
#   geom_point()
# # Change the color
# ggplot(data=df, aes(x=dose, y=len, group=1)) +
#   geom_line(color="red")+
#   geom_point()

plot_data2$work<-with(data_imputed,c()

library(ggplot2)
# Basic line plot with points
ggplot(data=plot_data2[plot_data2$out=="no",], 
	aes(x=fup, y=prop,group=med,color=med)) +
  geom_line()+
  geom_point() +
  scale_color_brewer(palette="Paired")+
  theme_minimal()
######################################################################
#Figure 2
######################################################################

plot_data_fig2<-NULL

plot_data_fig2$xvar<-with(data_full,c(
				day2_WEffectiveness,
				day3_WEffectiveness,
				day4_WEffectiveness,
				day5_WEffectiveness))

plot_data_fig2$yvar1<-with(data_full,c(
				day2_MaxPainScore,
				day3_MaxPainScore,
				day4_MaxPainScore,
				day5_MaxPainScore))

plot_data_fig2$yvar2<-with(data_full,c(
				day2_MinPainScore,
				day3_MinPainScore,
				day4_MinPainScore,
				day5_MinPainScore))

plot_data_fig2$cond<-with(data_full,c(
				rep("day2",length(day2_MinPainScore)),
				rep("day3",length(day3_MinPainScore)),
				rep("day4",length(day4_MinPainScore)),
				rep("day5",length(day5_MinPainScore))))


plot_data_fig2<-as.data.frame(plot_data_fig2)

# Same, but with different colors and add regression lines
ggplot(plot_data_fig2, aes(x=yvar2, y=xvar, color=cond)) +
    geom_point() + geom_jitter() +
    scale_colour_hue(l=50) + # Use a slightly darker palette than normal
    geom_smooth()    # Don't add shaded confidence region

# Same, but with different colors and add regression lines
ggplot(plot_data_fig2, aes(x=yvar1, y=xvar, color=cond)) +
    geom_point() + geom_jitter() +
    scale_colour_hue(l=50) + # Use a slightly darker palette than normal
    geom_smooth()    # Don't add shaded confidence region

lmmodel<-glm(scale(xvar) ~ scale(yvar1) *
					# yvar2 +
					cond,
					family=gaussian(),
					data=plot_data_fig2)

summary(lmmodel)
confint(lmmodel, level=0.95)
library(lm.beta)
print(lm.beta(lmmodel))

######################################################################
#END
######################################################################