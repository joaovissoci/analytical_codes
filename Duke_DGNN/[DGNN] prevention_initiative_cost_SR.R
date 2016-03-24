

data<-read.csv("/Users/joaovissoci/OneDrive - Duke University/datasets/DGNN/prevention_initiatives_cost_SR/prevention_initiatives_cost_SR.csv")

library(Hmisc)
library(psych)
library(ggplot2)
daly_descriptive<-with(data,by(daly_cost,intervention,summary))
str(daly_descriptive)
median_daly<-c(daly_descriptive[1][[1]][3],daly_descriptive[2][[1]][3],
  daly_descriptive[4][[1]][3],daly_descriptive[5][[1]][3],
  daly_descriptive[6][[1]][3],
  daly_descriptive[7][[1]][3],daly_descriptive[8][[1]][3],
  daly_descriptive[9][[1]][3],
  daly_descriptive[10][[1]][3],daly_descriptive[12][[1]][3],
  daly_descriptive[13][[1]][3],
  daly_descriptive[14][[1]][3],daly_descriptive[15][[1]][3])
low_limit_daly<-c(daly_descriptive[1][[1]][1],daly_descriptive[2][[1]][1],
  daly_descriptive[4][[1]][1],daly_descriptive[5][[1]][1],
  daly_descriptive[6][[1]][1],
  daly_descriptive[7][[1]][1],daly_descriptive[8][[1]][1],
  daly_descriptive[9][[1]][1],
  daly_descriptive[10][[1]][1],daly_descriptive[12][[1]][1],
  daly_descriptive[13][[1]][1],
  daly_descriptive[14][[1]][1],daly_descriptive[15][[1]][1])
upper_limit_daly<-c(daly_descriptive[1][[1]][6],daly_descriptive[2][[1]][6],
  daly_descriptive[4][[1]][6],daly_descriptive[5][[1]][6],
  daly_descriptive[6][[1]][6],
  daly_descriptive[7][[1]][6],daly_descriptive[8][[1]][6],
  daly_descriptive[9][[1]][6],
  daly_descriptive[10][[1]][6],daly_descriptive[12][[1]][6],
  daly_descriptive[13][[1]][6],
  daly_descriptive[14][[1]][6],daly_descriptive[15][[1]][6])
intervention_daly<-c("Bike helmet","Drink Driving","Enforcement",
  "Enforcement with Media","Helmet",
	"Media","Random Breath Test","Random Breath with Media","Seat Belt",
  "Selective Breath Test", "Selective Breath with Media",
	"Speed bumps","Speed limits")

death_data<-na.omit(with(data,data.frame(death_cost,intervention,reference,
            region)))
death_descriptive<-with(death_data,by(death_cost,intervention,summary))
str(death_descriptive)
median_death<-c(death_descriptive[4][[1]][3],death_descriptive[5][[1]][3],
                death_descriptive[6][[1]][3],
                death_descriptive[7][[1]][3],
                death_descriptive[14][[1]][3])
low_limit_death<-c(death_descriptive[4][[1]][1],death_descriptive[5][[1]][1],
                death_descriptive[6][[1]][1],
                death_descriptive[7][[1]][1],
                death_descriptive[14][[1]][1])
upper_limit_death<-c(death_descriptive[4][[1]][6],
                death_descriptive[5][[1]][6],
                death_descriptive[6][[1]][6],
                death_descriptive[7][[1]][6],
                death_descriptive[14][[1]][6])
intervention_death<-c("Enforcement","Enforcement and Media",
	"Helmet","Media","Speed bumps")

life_data<-na.omit(with(data,data.frame(life_cost,intervention,reference,
            region)))
life_descriptive<-with(life_data,by(life_cost,intervention,summary))
str(life_descriptive)
median_life<-c(life_descriptive[4][[1]][3],
               life_descriptive[5][[1]][3],
               life_descriptive[7][[1]][3],
               life_descriptive[14][[1]][3])
low_limit_life<-c(life_descriptive[4][[1]][1],
                  life_descriptive[5][[1]][1],
                  life_descriptive[7][[1]][1],
                  life_descriptive[14][[1]][1])
upper_limit_life<-c(life_descriptive[4][[1]][6],
                    life_descriptive[5][[1]][6],
                    life_descriptive[7][[1]][6],
                    life_descriptive[14][[1]][6])
intervention_life<-c("Enforcement","Enforcement and Media"
  ,"Media","Speed bumps")

median<-c(median_daly,median_death,median_life)
low_limit<-c(low_limit_daly,low_limit_death,low_limit_life)
upper_limit<-c(upper_limit_daly,upper_limit_death,upper_limit_life)
intervention<-c(intervention_daly,intervention_death,intervention_life)
measure<-c(rep("Cost per DALY",13),rep("Cost per Death Averted",5),
  rep("Cost per Life Saved",4))

plot<-data.frame(median,low_limit,upper_limit,intervention,measure)

ggplot(plot, aes(y= reorder(intervention,median), x = median)) +
facet_grid( measure ~ ., scales="free_y", space="free") +
geom_point() +
geom_errorbarh(aes(xmin=low_limit, xmax=upper_limit), height=.2) +
#scale_y_log10(breaks=ticks, labels = ticks) +
geom_vline(xintercept = 1, linetype=2) +
#coord_flip() +
#facet_grid(measure ~ ., scales="free_x", space="free") +
labs(y = 'Interventions', x = 'Cost-Effectiveness') +
theme_bw()



