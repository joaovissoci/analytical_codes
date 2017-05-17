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
					day2_PhysVisit,
					day3_PhysVisit,
					day4_PhysVisit,
					day5_PhysVisit))

data$gender<-car::recode(data$gender,"'F'='F';'M'='M';else=NA")

data$race<-car::recode(data$race,"'A'='A';
									  'AA'='AA';
									  'C'='C';
									  'H'='H';
									  'Other'='Other';
									  'U'='U';	
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


#Unscheduled visits
#######################################################

for (i in 1:nrow(data_imputed))
{
 if (data_imputed$day2_PhysVisit[i] == 1 || 
 	 data_imputed$day3_PhysVisit[i] == 1 || 
 	 data_imputed$day4_PhysVisit[i] == 1 || 
 	 data_imputed$day5_PhysVisit[i] == 1)
     
     {
       data_imputed$unscheduled_visits[i] <- "yes"
     } else # if (v1[i] == "Sim" || 
     {
       data_imputed$unscheduled_visits[i] <-"no"
     } # else if (v1[i] == "Sim" || 
} # for (i in 1:nrow(dataframe)


for (i in 1:nrow(data_imputed))
{
 if (data_imputed$day2_PhysVisit[i] == 1)
 	{ 
       data_imputed$unscheduled_visits_day[i] <- 2

 	} else if (data_imputed$day3_PhysVisit[i] == 1)

 	{
        data_imputed$unscheduled_visits_day[i] <- 3

 	} else if (data_imputed$day4_PhysVisit[i] == 1)

 	{
        data_imputed$unscheduled_visits_day[i] <- 4
 	
 	} else if (data_imputed$day5_PhysVisit[i] == 1)

 	{
        data_imputed$unscheduled_visits_day[i] <- 5

     } else # if (v1[i] == "Sim" || 
     {
       data_imputed$unscheduled_visits_day[i] <- NA
     } # else if (v1[i] == "Sim" || 
} # for (i in 1:nrow(dataframe)


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

######################################################################
#Figure 1
######################################################################

 tblFun <- function(x,y){
    tbl <- table(x,y)
    res <- cbind(tbl,round(prop.table(tbl,2)*100,2))
    rownames(res) <- c('yes','no')
    res[,4:6]
}

plot_data<-do.call(rbind,lapply(data_imputed[8:11],tblFun,y=data_imputed[,7]))
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

plot_data2<-NULL
plot_data2$prop<-c(plot_data[,1],plot_data[,2],plot_data[,3])
plot_data2$med<-c(rep("OPO",8),rep("SPO",8),rep("SPP",8))
plot_data2$out<-c(plot_data$outcome,plot_data$outcome,plot_data$outcome)
plot_data2$fup<-c(plot_data$FUP,plot_data$FUP,plot_data$FUP)
plot_data2<-as.data.frame(plot_data2)

library(ggplot2)
# Basic line plot with points
ggplot(data=plot_data2[plot_data2$out=="no",], 
	aes(x=fup, y=prop,group=med,color=med)) +
  geom_line()+
  geom_point() +
  scale_color_brewer(palette="Paired")+
  theme_minimal()

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
#Table 2
######################################################################

reg_model<-glm(as.factor(unscheduled_visits) ~ treatreg + 
									gender + 
									age + 
									# race +
                            		eDx_Primary + 
                            		PainScore
                            ,family=binomial, data=data_imputed)
summary(reg_model)
#anova(reglogGEU)
#exp(coef(model1_death)) # exponentiated coefficients
#exp(confint(model1_death)) # 95% CI for exponentiated coefficients
exp(cbind(Odds=coef(reg_model),confint(reg_model,level=0.95))) 
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
#logistic.display(baselineXFUP3)



######################################################################
#END
######################################################################