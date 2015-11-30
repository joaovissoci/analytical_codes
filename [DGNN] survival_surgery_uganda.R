######################################################
#.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. see full license at the end of this file.
######################################################
#this script follows a combination of the guidelines proposed by Hadley Wickham http://goo.gl/c04kq as well as using the formatR package http://goo.gl/ri6ky
#if this is the first time you are conducting an analysis using this protocol, please watch http://goo.gl/DajIN while following step by step

#link to manuscript

 #The general plan is to compare the fibrinogen and platelet curves of RS vs Copperhead snakes.  The times points are Baseline, nadir during hospitalization, day 5, day 8, day 15.  There is some missing mess.   I am hoping we can get it done in time for an abstract deadline soon.  Let me know what is best.

######################################################
#SETTING ENVIRONMENT
######################################################
 #install.packages("VIM")
 #install.packages("VIMGUI")
 #install.packages("miP")
 #install.packages("gWidgetsRGtk2")
 #install.packages("mi")
 #install.packages("epicalc")

#Load packages neededz for the analysis
#All packages must be installes with install.packages() function
lapply(c("sem","ggplot2", "psych", "RCurl", "irr", "nortest", "moments","GPArotation","nFactors", "boot","psy", "car","vcd", "gridExtra","mi","VIM","epicalc","gdata","sqldf","reshape2","mclust","foreign","survival","memisc","lme4","lmerTest","dplyr","car"),library, character.only=T)

######################################################
#IMPORTING DATA
######################################################

#uploading data ---------------------------------------------------------------------
#Load the data set.
#All data are stored in  http://figshare.com/articles/The_reliability_of_AO_classification_on_femur_fractures_among_orthopedic_residents/103664
#Note that the data set has been reorganized to be applied to some functions

#baseline_lena<-read.csv("/Users/rpietro/Google Drive/research_groups/RoR/IPq/Suicide_Anxiety/baseline.csv",sep=",")
 
#Pulling data from dropbox
data <- repmis::source_DropboxData("tony_neurosurgery_uganda.csv","iyqhev5zz6wj49x",sep = ",",header = TRUE)

#pulling data from Google Spreadsheet - using http://goo.gl/VV4o1g
#authorize(new_user = TRUE)
#my_sheets <- list_sheets()

#data_cp<-read.csv("/home/joao/Desktop/data_cp.csv",sep=',')

#data_cp<-read.csv("/home/joao/Desktop/demographics2.csv",sep=',')

##############################################################
#DATA MANAGEMENT
##############################################################

data_coded<-sapply(data,function(x) recode(x,"9999=NA"))
data_coded<-as.data.frame(data_coded)

data_coded$pre_post<-car::recode(data_coded$FY,"1:3='pre';4:8='post';9=NA")
##############################################################
#DESCRIPTIVES
##############################################################

summary(data_coded)

with(data_alive,by(GCS_Admit,pre_post,summary))

#############################################################
#SURVIVAL CURVES
#############################################################
# ALL Data

#data_death30days<-subset(data_coded,data_coded$Outcome==0)
data_coded$survival_outcome<-car::recode(data_coded$Outcome,"0=1;2=0")
data_coded$survival_time<-rep(0,1878)

for (i in 1:length(data_coded$survival_outcome)) {
	#print(i)
	if (is.na(data_coded$survival_outcome[i])==FALSE && data_coded$survival_outcome[i]==0){ 
		data_coded$survival_time[i]<-530
	}
	else if (is.na(data_coded$survival_outcome[i])==TRUE) {
		data_coded$survival_time[i]<-NA
	}
	else {
		data_coded$survival_time[i]<-data_coded$LOS_Post[i]
	}
}

# drawing survival curves with several strata
t.Surv <- Surv(data_coded$survival_time, data_coded$survival_outcome)
t.survfit <- survfit(t.Surv~pre_post, data=data_coded)
summary(t.survfit)
survdiff(t.Surv~pre_post, data=data_coded) 
MaleMod <- coxph(t.Surv~pre_post,data=data_coded)
x<-cox.zph(MaleMod)
print(x)                  # display the results 
plot(x)

#overall_dates<-rep(data_alive$dates,2)
#overall_attempt<-rep(data_alive$ATTEMPT_P,2)
#overall_FY<-c(data_alive$FY,rep("Total",1630))

#overall_plot<-data.frame(overall_dates,overall_attempt,overall_diagnostic)
# two strata
#t.Surv <- Surv(overall_dates, overall_attempt)
#t.survfit <- survfit(t.Surv~overall_diagnostic, data=overall_plot)
t.survframe <- createSurvivalFrame(t.survfit)

# add ggplot options, use different shape
 
qplot_survival(t.survframe, TRUE, 1) + theme_bw() #+ scale_color_manual(values=c("black","green", "steelblue"),name="Strata",breaks=c("overall_diagnostic=Total", "overall_diagnostic=TAB", "overall_diagnostic=MDD"),labels=c("Overall", "BD", "MDD")) + xlab("Time (Weeks)") + ylab("Survival Suicide Attempt") + geom_text(x = 400, y = 0.75, label = "P=0.282", colour="black",size = 3)

#ALIVE ONES ###########################
# Separating deads from discharged ones because deads have a different LOS reference

data_alive<-subset(data_coded,data_coded$Outcome==2)
# drawing survival curves with several strata
t.Surv <- Surv(data_alive$LOS_In, data_alive$Outcome)
t.survfit <- survfit(t.Surv~data_alive$pre_post, data=data_alive)
summary(t.survfit)
survdiff(t.Surv~pre_post, data=data_alive) 
MaleMod <- coxph(t.Surv~pre_post,data=data_alive)
cox.zph(MaleMod)


#overall_dates<-rep(data_alive$dates,2)
#overall_attempt<-rep(data_alive$ATTEMPT_P,2)
#overall_FY<-c(data_alive$FY,rep("Total",1630))

#overall_plot<-data.frame(overall_dates,overall_attempt,overall_diagnostic)
# two strata
#t.Surv <- Surv(overall_dates, overall_attempt)
#t.survfit <- survfit(t.Surv~overall_diagnostic, data=overall_plot)
t.survframe <- createSurvivalFrame(t.survfit)

# add ggplot options, use different shape
 
qplot_survival(t.survframe, TRUE, 1) + theme_bw() # + scale_color_manual(values=c("black","green", "steelblue"),name="Strata",breaks=c("overall_diagnostic=Total", "overall_diagnostic=TAB", "overall_diagnostic=MDD"),labels=c("Overall", "BD", "MDD")) + xlab("Time (Weeks)") + ylab("Survival Suicide Attempt") + geom_text(x = 400, y = 0.75, label = "P=0.282", colour="black",size = 3)


#DEATH WITHIN 30 days ###########################

# Separating deads from discharged ones because deads have a different LOS reference

data_death30days<-subset(data_coded,data_coded$Outcome==0)
data_death30days$Outcome<-car::recode(data_death30days$Outcome,"0=1")

# drawing survival curves with several strata
t.Surv <- Surv(data_death30days$LOS_Post, data_death30days$Outcome)
t.survfit <- survfit(t.Surv~pre_post, data=data_death30days)
summary(t.survfit)
survdiff(t.Surv~pre_post, data=data_death30days) 
MaleMod <- coxph(t.Surv~pre_post,data=data_death30days)
x<-cox.zph(MaleMod)
print(x)                  # display the results 
plot(x)

#overall_dates<-rep(data_alive$dates,2)
#overall_attempt<-rep(data_alive$ATTEMPT_P,2)
#overall_FY<-c(data_alive$FY,rep("Total",1630))

#overall_plot<-data.frame(overall_dates,overall_attempt,overall_diagnostic)
# two strata
#t.Surv <- Surv(overall_dates, overall_attempt)
#t.survfit <- survfit(t.Surv~overall_diagnostic, data=overall_plot)
t.survframe <- createSurvivalFrame(t.survfit)

# add ggplot options, use different shape
 
qplot_survival(t.survframe, TRUE, 1) + theme_bw() #+ scale_color_manual(values=c("black","green", "steelblue"),name="Strata",breaks=c("overall_diagnostic=Total", "overall_diagnostic=TAB", "overall_diagnostic=MDD"),labels=c("Overall", "BD", "MDD")) + xlab("Time (Weeks)") + ylab("Survival Suicide Attempt") + geom_text(x = 400, y = 0.75, label = "P=0.282", colour="black",size = 3)

#DEATH WITHIN 30 days ###########################

# Separating deads from discharged ones because deads have a different LOS reference

data_deathmore30days<-subset(data_coded,data_coded$Outcome==1)

# drawing survival curves with several strata
t.Surv <- Surv(data_deathmore30days$LOS_Post, data_deathmore30days$Outcome)
t.survfit <- survfit(t.Surv~pre_post, data=data_deathmore30days)
summary(t.survfit)
survdiff(t.Surv~pre_post, data=data_deathmore30days) 
MaleMod <- coxph(t.Surv~pre_post,data=data_deathmore30days)
cox.zph(MaleMod)


#overall_dates<-rep(data_alive$dates,2)
#overall_attempt<-rep(data_alive$ATTEMPT_P,2)
#overall_FY<-c(data_alive$FY,rep("Total",1630))

#overall_plot<-data.frame(overall_dates,overall_attempt,overall_diagnostic)
# two strata
#t.Surv <- Surv(overall_dates, overall_attempt)
#t.survfit <- survfit(t.Surv~overall_diagnostic, data=overall_plot)
t.survframe <- createSurvivalFrame(t.survfit)

# add ggplot options, use different shape
 
qplot_survival(t.survframe, TRUE, 1) + theme_bw() #+ scale_color_manual(values=c("black","green", "steelblue"),name="Strata",breaks=c("overall_diagnostic=Total", "overall_diagnostic=TAB", "overall_diagnostic=MDD"),labels=c("Overall", "BD", "MDD")) + xlab("Time (Weeks)") + ylab("Survival Suicide Attempt") + geom_text(x = 400, y = 0.75, label = "P=0.282", colour="black",size = 3)

#ALL DEATHS days ###########################

# Separating deads from discharged ones because deads have a different LOS reference

data_deathoverall<-subset(data_coded,data_coded$Outcome!=2)
data_deathoverall$Outcome<-car::recode(data_deathoverall$Outcome,"0=1;1=1")
# drawing survival curves with several strata
t.Surv <- Surv(data_deathoverall$LOS_Post, data_deathoverall$Outcome)
t.survfit <- survfit(t.Surv~data_deathoverall$pre_post, data=data_deathoverall)
summary(t.survfit)
survdiff(t.Surv~pre_post, data=data_deathoverall) 
MaleMod <- coxph(t.Surv~pre_post,data=data_deathoverall)
cox.zph(MaleMod)


#overall_dates<-rep(data_alive$dates,2)
#overall_attempt<-rep(data_alive$ATTEMPT_P,2)
#overall_FY<-c(data_alive$FY,rep("Total",1630))

#overall_plot<-data.frame(overall_dates,overall_attempt,overall_diagnostic)
# two strata
#t.Surv <- Surv(overall_dates, overall_attempt)
#t.survfit <- survfit(t.Surv~overall_diagnostic, data=overall_plot)
t.survframe <- createSurvivalFrame(t.survfit)

# add ggplot options, use different shape
 
qplot_survival(t.survframe, TRUE, 1) + theme_bw() #+ scale_color_manual(values=c("black","green", "steelblue"),name="Strata",breaks=c("overall_diagnostic=Total", "overall_diagnostic=TAB", "overall_diagnostic=MDD"),labels=c("Overall", "BD", "MDD")) + xlab("Time (Weeks)") + ylab("Survival Suicide Attempt") + geom_text(x = 400, y = 0.75, label = "P=0.282", colour="black",size = 3)

#### TRAUMA CASES ONLY ##############################

data_trauma<-subset(data_coded,data_coded$Dx==5)

#data_death30days<-subset(data_coded,data_coded$Outcome==0)
data_trauma$survival_outcome<-car::recode(data_trauma$Outcome,"0=1;2=0")
data_trauma$survival_time<-rep(0,length(data_trauma$Outcome))

for (i in 1:length(data_trauma$survival_outcome)) {
	#print(i)
	if (is.na(data_trauma$survival_outcome[i])==FALSE && data_trauma$survival_outcome[i]==0){ 
		data_trauma$survival_time[i]<-max(na.omit(data_trauma$LOS_Post))
	}
	else if (is.na(data_trauma$survival_outcome[i])==TRUE) {
		data_trauma$survival_time[i]<-NA
	}
	else {
		data_trauma$survival_time[i]<-data_trauma$LOS_Post[i]
	}
}


# drawing survival curves with several strata
t.Surv <- Surv(data_trauma$survival_time, data_trauma$survival_outcome)
t.survfit <- survfit(t.Surv~pre_post, data=data_trauma)
summary(t.survfit)
survdiff(t.Surv~pre_post, data=data_trauma) 
MaleMod <- coxph(t.Surv~pre_post,data=data_trauma)
x<-cox.zph(MaleMod)
print(x)                  # display the results 
plot(x)

#overall_dates<-rep(data_alive$dates,2)
#overall_attempt<-rep(data_alive$ATTEMPT_P,2)
#overall_FY<-c(data_alive$FY,rep("Total",1630))

#overall_plot<-data.frame(overall_dates,overall_attempt,overall_diagnostic)
# two strata
#t.Surv <- Surv(overall_dates, overall_attempt)
#t.survfit <- survfit(t.Surv~overall_diagnostic, data=overall_plot)
t.survframe <- createSurvivalFrame(t.survfit)

# add ggplot options, use different shape
 
qplot_survival(t.survframe, TRUE, 1) + theme_bw() #+ scale_color_manual(values=c("black","green", "steelblue"),name="Strata",breaks=c("overall_diagno	stic=Total", "overall_diagnostic=TAB", "overall_diagnostic=MDD"),labels=c("Overall", "BD", "MDD")) + xlab("Time (Weeks)") + ylab("Survival Suicide Attempt") + geom_text(x = 400, y = 0.75, label = "P=0.282", colour="black",size = 3)


#### TRAUMA - DISCHARGE CASES ONLY ########################

# Separating deads from discharged ones because deads have a different LOS reference

data_alive<-subset(data_coded,data_coded$Outcome==2)
data_alive_trauma<-subset(data_alive,data_alive$Dx==5)
# drawing survival curves with several strata
t.Surv <- Surv(data_alive_trauma$LOS_Post, data_alive_trauma$Outcome)
t.survfit <- survfit(t.Surv~data_alive_trauma$pre_post, data=data_alive_trauma)
summary(t.survfit)
survdiff(t.Surv~pre_post, data=data_alive_trauma) 
MaleMod <- coxph(t.Surv~pre_post,data=data_alive_trauma)
cox.zph(MaleMod)


#overall_dates<-rep(data_alive$dates,2)
#overall_attempt<-rep(data_alive$ATTEMPT_P,2)
#overall_FY<-c(data_alive$FY,rep("Total",1630))

#overall_plot<-data.frame(overall_dates,overall_attempt,overall_diagnostic)
# two strata
#t.Surv <- Surv(overall_dates, overall_attempt)
#t.survfit <- survfit(t.Surv~overall_diagnostic, data=overall_plot)
t.survframe <- createSurvivalFrame(t.survfit)

# add ggplot options, use different shape
 
qplot_survival(t.survframe, TRUE, 1) + theme_bw() # + scale_color_manual(values=c("black","green", "steelblue"),name="Strata",breaks=c("overall_diagnostic=Total", "overall_diagnostic=TAB", "overall_diagnostic=MDD"),labels=c("Overall", "BD", "MDD")) + xlab("Time (Weeks)") + ylab("Survival Suicide Attempt") + geom_text(x = 400, y = 0.75, label = "P=0.282", colour="black",size = 3)

#### TRAUMA - ALL DEATHS ###########

# Separating deads from discharged ones because deads have a different LOS reference

data_death<-subset(data_trauma,data_trauma$Outcome!=2)
data_death$Outcome<-car::recode(data_death$Outcome,"0=1")

# drawing survival curves with several strata
t.Surv <- Surv(data_death$LOS_Post, data_death$Outcome)
t.survfit <- survfit(t.Surv~pre_post, data=data_death)
summary(t.survfit)
survdiff(t.Surv~pre_post, data=data_death) 
MaleMod <- coxph(t.Surv~pre_post,data=data_death)
x<-cox.zph(MaleMod)
print(x)                  # display the results 
plot(x)

#overall_dates<-rep(data_alive$dates,2)
#overall_attempt<-rep(data_alive$ATTEMPT_P,2)
#overall_FY<-c(data_alive$FY,rep("Total",1630))

#overall_plot<-data.frame(overall_dates,overall_attempt,overall_diagnostic)
# two strata
#t.Surv <- Surv(overall_dates, overall_attempt)
#t.survfit <- survfit(t.Surv~overall_diagnostic, data=overall_plot)
t.survframe <- createSurvivalFrame(t.survfit)

# add ggplot options, use different shape
 
qplot_survival(t.survframe, TRUE, 1) + theme_bw() #+ scale_color_manual(values=c("black","green", "steelblue"),name="Strata",breaks=c("overall_diagnostic=Total", "overall_diagnostic=TAB", "overall_diagnostic=MDD"),labels=c("Overall", "BD", "MDD")) + xlab("Time (Weeks)") + ylab("Survival Suicide Attempt") + geom_text(x = 400, y = 0.75, label = "P=0.282", colour="black",size = 3)

#DEATH WITHIN 30 days ###########################

# Separating deads from discharged ones because deads have a different LOS reference

data_trauma30days<-subset(data_trauma,data_trauma$Outcome==0)
data_trauma30days$Outcome<-car::recode(data_trauma30days$Outcome,"0=1")

# drawing survival curves with several strata
t.Surv <- Surv(data_trauma30days$LOS_Post, data_trauma30days$Outcome)
t.survfit <- survfit(t.Surv~pre_post, data=data_trauma30days)
summary(t.survfit)
survdiff(t.Surv~pre_post, data=data_trauma30days) 
MaleMod <- coxph(t.Surv~pre_post,data=data_trauma30days)
x<-cox.zph(MaleMod)
print(x)                  # display the results 
plot(x)

#overall_dates<-rep(data_alive$dates,2)
#overall_attempt<-rep(data_alive$ATTEMPT_P,2)
#overall_FY<-c(data_alive$FY,rep("Total",1630))

#overall_plot<-data.frame(overall_dates,overall_attempt,overall_diagnostic)
# two strata
#t.Surv <- Surv(overall_dates, overall_attempt)
#t.survfit <- survfit(t.Surv~overall_diagnostic, data=overall_plot)
t.survframe <- createSurvivalFrame(t.survfit)

# add ggplot options, use different shape
 
qplot_survival(t.survframe, TRUE, 1) + theme_bw() #+ scale_color_manual(values=c("black","green", "steelblue"),name="Strata",breaks=c("overall_diagnostic=Total", "overall_diagnostic=TAB", "overall_diagnostic=MDD"),labels=c("Overall", "BD", "MDD")) + xlab("Time (Weeks)") + ylab("Survival Suicide Attempt") + geom_text(x = 400, y = 0.75, label = "P=0.282", colour="black",size = 3)

#DEATH WITHIN 30 days ###########################

# Separating deads from discharged ones because deads have a different LOS reference

data_traumamore30days<-subset(data_trauma,data_trauma$Outcome==1)

# drawing survival curves with several strata
t.Surv <- Surv(data_traumamore30days$LOS_Post, data_traumamore30days$Outcome)
t.survfit <- survfit(t.Surv~pre_post, data=data_traumamore30days)
summary(t.survfit)
survdiff(t.Surv~pre_post, data=data_traumamore30days) 
MaleMod <- coxph(t.Surv~pre_post,data=data_traumamore30days)
cox.zph(MaleMod)


#overall_dates<-rep(data_alive$dates,2)
#overall_attempt<-rep(data_alive$ATTEMPT_P,2)
#overall_FY<-c(data_alive$FY,rep("Total",1630))

#overall_plot<-data.frame(overall_dates,overall_attempt,overall_diagnostic)
# two strata
#t.Surv <- Surv(overall_dates, overall_attempt)
#t.survfit <- survfit(t.Surv~overall_diagnostic, data=overall_plot)
t.survframe <- createSurvivalFrame(t.survfit)

# add ggplot options, use different shape
 
qplot_survival(t.survframe, TRUE, 1) + theme_bw() #+ scale_color_manual(values=c("black","green", "steelblue"),name="Strata",breaks=c("overall_diagnostic=Total", "overall_diagnostic=TAB", "overall_diagnostic=MDD"),labels=c("Overall", "BD", "MDD")) + xlab("Time (Weeks)") + ylab("Survival Suicide Attempt") + geom_text(x = 400, y = 0.75, label = "P=0.282", colour="black",size = 3)

