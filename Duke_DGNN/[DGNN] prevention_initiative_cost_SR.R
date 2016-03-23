

data<-read.csv("/Users/joaovissoci/OneDrive - Duke University/datasets/DGNN/prevention_initiatives_cost_SR/prevention_initiatives_cost_SR.csv")


#exp(cbind(Odds=coef(fit),confint(fit,level=0.90))) 

plot_odds<-function(x, title = NULL){
tmp<-data.frame(cbind(exp(coef(x)), exp(confint(x))))
odds<-tmp[-1,]
names(odds)<-c(‘OR’, ‘lower’, ‘upper’)
odds$vars<-row.names(odds)
ticks<-c(seq(.1, 1, by =.1), seq(0, 10, by =1), seq(10, 100, by =10))

ggplot(plot, aes(y= median, x = reorder(intervention, median))) +
geom_point() +
geom_errorbar(aes(ymin=low_limit, ymax=upper_limit), width=.2) +
#scale_y_log10(breaks=ticks, labels = ticks) +
geom_hline(yintercept = 1, linetype=2) +
coord_flip() +
labs(x = 'Interventions', y = 'Cost per DALY') +
theme_bw()
}

library(Hmisc)
library(psych)
x<-with(data,by(DALY,Intervention,summary))
str(x)

median_daly<-c(x[1][[1]][3],x[2][[1]][3],x[4][[1]][3],x[6][[1]][3],
  x[7][[1]][3],x[8][[1]][3],x[10][[1]][3],x[12][[1]][3],
  x[14][[1]][3],x[15][[1]][3])
low_limit_daly<-c(x[1][[1]][1],x[2][[1]][1],x[4][[1]][1],x[6][[1]][1],
  x[7][[1]][1],x[8][[1]][1],x[10][[1]][1],x[12][[1]][1],
  x[14][[1]][1],x[15][[1]][1])
upper_limit_daly<-c(x[1][[1]][6],x[2][[1]][6],x[4][[1]][6],x[6][[1]][6],
  x[7][[1]][6],x[8][[1]][6],x[10][[1]][6],x[12][[1]][6],
  x[14][[1]][6],x[15][[1]][6])
intervention_daly-c("Bike helmet","Drink Driving","Enforcement","Helmet",
	"Media","Random Breath Test","Seat Belt","Selective Breath Test",
	"Speed bumps","Speed limits")

median_death<-c(x[1][[1]][3],x[2][[1]][3],x[4][[1]][3],x[6][[1]][3],
  x[7][[1]][3],x[8][[1]][3],x[10][[1]][3],x[12][[1]][3],
  x[14][[1]][3],x[15][[1]][3])
low_limit_death<-c(x[1][[1]][1],x[2][[1]][1],x[4][[1]][1],x[6][[1]][1],
  x[7][[1]][1],x[8][[1]][1],x[10][[1]][1],x[12][[1]][1],
  x[14][[1]][1],x[15][[1]][1])
upper_limit_death<-c(x[1][[1]][6],x[2][[1]][6],x[4][[1]][6],x[6][[1]][6],
  x[7][[1]][6],x[8][[1]][6],x[10][[1]][6],x[12][[1]][6],
  x[14][[1]][6],x[15][[1]][6])
intervention_death<-c("Bike helmet","Drink Driving","Enforcement","Helmet",
	"Media","Random Breath Test","Seat Belt","Selective Breath Test",
	"Speed bumps","Speed limits")

plot<-data.frame(median,low_limit,upper_limit,intervention)

ggplot(plot, aes(y= median, x = reorder(intervention, median))) +
geom_point() +
geom_errorbar(aes(ymin=low_limit, ymax=upper_limit), width=.2) +
#scale_y_log10(breaks=ticks, labels = ticks) +
geom_hline(yintercept = 1, linetype=2) +
coord_flip() +
labs(x = 'Interventions', y = 'Cost per DALY') +
theme_bw()
