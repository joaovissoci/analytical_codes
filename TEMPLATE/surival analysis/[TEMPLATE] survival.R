##############################################################
#Survival analysis
#############################################################

surv_data<-read.csv("")

surv_data<-remove.vars(surv_data,c("transport_mode_1",
								   "transport_mode_2",
								   "transport_mode_other_2"))

#data_death30days<-subset(data_coded,data_coded$Outcome==0)
surv_data$gcs_cat<-car::recode(surv_data$gcs_tot,"0:8=1;9:15=0")
surv_data$time_to_care<-car::recode(surv_data$time_to_care,
			"1441:1057080=1440")
surv_data$gos_cat<-car::recode(surv_data$gos,"0:4='bad';5='good'")


# data_coded$survival_time<-rep(0,1878)

# for (i in 1:length(data_coded$survival_outcome)) {
# 	#print(i)
# 	if (is.na(data_coded$survival_outcome[i])==FALSE && data_coded$survival_outcome[i]==0){ 
# 		data_coded$survival_time[i]<-530
# 	}
# 	else if (is.na(data_coded$survival_outcome[i])==TRUE) {
# 		data_coded$survival_time[i]<-NA
# 	}
# 	else {
# 		data_coded$survival_time[i]<-data_coded$LOS_Post[i]
# 	}
# }

surv_data$survival_outcome<-1

surv_data<-na.omit(surv_data)


# drawing survival curves with several strata
t.Surv <- Surv(surv_data$time_to_care, surv_data$survival_outcome)
t.survfit <- survfit(t.Surv~1, data=surv_data)
summary(t.survfit)
plot(t.survfit)
print(t.survfit, show.rmean=T)

#overall_plot<-data.frame(overall_dates,overall_attempt,overall_diagnostic)
# two strata
#t.Surv <- Surv(overall_dates, overall_attempt)
#t.survfit <- survfit(t.Surv~overall_diagnostic, data=overall_plot)
t.survframe <- createSurvivalFrame(t.survfit)

# add ggplot options, use different shape
ggsurv(t.survframe)#+ scale_color_manual(values=c("black","green", "steelblue"),name="Strata",breaks=c("overall_diagnostic=Total", "overall_diagnostic=TAB", "overall_diagnostic=MDD"),labels=c("Overall", "BD", "MDD")) + xlab("Time (Weeks)") + ylab("Survival Suicide Attempt") + geom_text(x = 400, y = 0.75, label = "P=0.282", colour="black",size = 3)

#compare curves - GCS
t.survfit <- survfit(t.Surv~gcs_cat, data=surv_data)
summary(t.survfit)
plot(t.survfit)
survdiff(t.Surv~gcs_cat, data=surv_data, rho=1) 
#overall_plot<-data.frame(overall_dates,overall_attempt,overall_diagnostic)
# two strata
#t.Surv <- Surv(overall_dates, overall_attempt)
#t.survfit <- survfit(t.Surv~overall_diagnostic, data=overall_plot)
t.survframe <- createSurvivalFrame(t.survfit)

# add ggplot options, use different shape
qplot_survival(t.survframe)#+ scale_color_manual(values=c("black","green", "steelblue"),name="Strata",breaks=c("overall_diagnostic=Total", "overall_diagnostic=TAB", "overall_diagnostic=MDD"),labels=c("Overall", "BD", "MDD")) + xlab("Time (Weeks)") + ylab("Survival Suicide Attempt") + geom_text(x = 400, y = 0.75, label = "P=0.282", colour="black",size = 3)

#compare curves - GCS
t.survfit <- survfit(t.Surv~gos_cat, data=surv_data)
summary(t.survfit)
plot(t.survfit)
survdiff(t.Surv~gos_cat, data=surv_data, rho=1) 
#overall_plot<-data.frame(overall_dates,overall_attempt,overall_diagnostic)
# two strata
#t.Surv <- Surv(overall_dates, overall_attempt)
#t.survfit <- survfit(t.Surv~overall_diagnostic, data=overall_plot)
t.survframe <- createSurvivalFrame(t.survfit)

# add ggplot options, use different shape
qplot_survival(t.survframe)#+ scale_color_manual(values=c("black","green", "steelblue"),name="Strata",breaks=c("overall_diagnostic=Total", "overall_diagnostic=TAB", "overall_diagnostic=MDD"),labels=c("Overall", "BD", "MDD")) + xlab("Time (Weeks)") + ylab("Survival Suicide Attempt") + geom_text(x = 400, y = 0.75, label = "P=0.282", colour="black",size = 3)

#Cox model
MaleMod <- coxph(t.Surv~surv_data$gcs_cat +
						alcohol	+
						male +
						transport_legs
						,data=surv_data)
# baseline<-basehaz(MaleMod)
summary(MaleMod)
# plot(baseline$time, baseline$hazard, type='l',main="Hazard rates")
x<-cox.zph(MaleMod)
print(x)                  # display the results 
plot(x)
