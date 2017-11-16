#Tentei limpar para colocar numa template ta?

data<-read.csv("/Users/Joao/Desktop/template_odds_forestplot_ggplot2.csv")

logmodel_gos<-glm(outcome ~ time_to_care_cat + 
						transport_legs_cat + 
						age + 
						male +
                        moi + 
                        alcohol +
                        transport_leg1 +
                        gcs_cat
                       ,family=binomial, data=analysis_data)
summary(logmodel_gos)
#anova(reglogGEU)
#exp(coef(model1_death)) # exponentiated coefficients
#exp(confint(model1_death)) # 95% CI for exponentiated coefficients
exp(cbind(Odds=coef(logmodel_gos),confint(logmodel_gos,level=0.95))) 
#predict(model1_death, type="response") # predicted values
#residuals(model1_death, type="deviance") # residuals
# logistic.display(logmodel)

# plot_odds<-function(x, title = NULL){
tmp_gcs<-data.frame(cbind(exp(coef(logmodel_gcs)),
	exp(confint(logmodel_gcs))))
tmp_gos<-data.frame(cbind(exp(coef(logmodel_gos)),
	exp(confint(logmodel_gos))))
tmp<-rbind(tmp_gcs,tmp_gos)
odds<-tmp[-c(1,16,31),]
names(odds)<-c('OR', 'lower', 'upper')
odds$vars<-rep(c("1-2 hrs vs. 0-1 hrs",
			 "2-3 hrs vs. 0-1 thrs",
			 "3-4 hrs vs. 0-1 hrs",
			 "4 or more hrs vs. 0-1 hrs",
			 "# Journey legs",
			 "Age",
			 "Male vs. Female",
			 "RTI vs. non-RTI",
			 "Alcohol use vc. Abstainer",
			 "Moshi Rural vs. Moshi Urban",
			 "Hai vs. Moshi Urban",
			 "Rombo vs. Moshi Urban",
			 "Mwanga vs. Moshi Urban",
			 "Same vs. Moshi Urban"),2)
odds$groups<-rep(c(rep("Time to care",4),
					"Course points",
					"Age", 
					"Gender",
					"MOI",
					"Alcohol use",
					rep("District",5)),2)
odds$models<-c(rep("Mild vs. Moderate/Severe TBI",14),
			   rep("Good vs. Poor Outcome",14))
#ticks<-c(seq(.1, 1, by =.1), seq(0, 10, by =1), seq(10, 100, by =10))

setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
postscript("/Users/Joao/Desktop/[GLOBAL EM] tz_prehospitalTBI_figure2.eps",
	width = 8, height = 6)
ggplot(odds, aes(y= OR, x = reorder(vars, OR))) +
geom_point() +
geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
#scale_y_log10(breaks=ticks, labels = ticks) +
geom_hline(yintercept = 1, linetype=2) +
scale_x_discrete(limits=c(
			 "4 or more hrs vs. 0-1 hrs",
			 "3-4 hrs vs. 0-1 hrs",
			 "2-3 hrs vs. 0-1 thrs",
			 "1-2 hrs vs. 0-1 hrs",
			 "# Journer legs",
			 "Age",
			 "Male vs. Female",
			 "RTI vs. non-RTI",
			 "Alcohol use vc. Abstainer",
			 "Moshi Rural vs. Moshi Urban",
			 "Hai vs. Moshi Urban",
			 "Rombo vs. Moshi Urban",
			 "Mwanga vs. Moshi Urban",
			 "Same vs. Moshi Urban")) +
facet_grid(.~models, scales="free_y") +
coord_flip() +
labs(x = 'Predictors of TBI Outcomes', y = 'OR (CI 95%)') +
theme_bw()
# }
dev.off()