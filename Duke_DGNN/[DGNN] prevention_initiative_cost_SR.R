

data<-read.csv("/Users/jnv4/OneDrive - Duke University/datasets/DGNN/prevention_initiatives_cost_SR/prevention_initiatives_cost_SR.csv")

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




##### Cmparison with Lancet

low_limit_lancet<-c(500.41,453.74,51.86,12.96,6.48)
upper_limit_lancet<-c(706.54,648.20,220.39,25.93,22.04)
median_limit_lancet<-c(706.54,648.20,220.39,25.93,22.04)
intervention_lancet<-c("Aspirin and Beta-Blockers for Ichemic heart disease",
                "Antiretroviral therapy for HIV",
                "BCG vaccine for tuberculosis prevention",
                "Vaccines for tuberculosis, diphteria
                pertussis, tetanus, polio, and measles",
                "Bednets for malaria prevention")

median<-c(median_daly,median_limit_lancet)
low_limit<-c(low_limit_daly,low_limit_lancet)
upper_limit<-c(upper_limit_daly,upper_limit_lancet)
intervention<-c(intervention_daly,intervention_lancet)
#measure<-c(rep("Cost per DALY",13),rep("Cost per Death Averted",5),
#  rep("Cost per Life Saved",4))
color<-c(rep("black",13),rep("red",5))

plot<-data.frame(median,low_limit,upper_limit,intervention,color)

ggplot(plot, aes(y= reorder(intervention,median), x = median)) +
#facet_grid( measure ~ ., scales="free_y", space="free") +
geom_point(color=color) +
geom_errorbarh(aes(xmin=low_limit, xmax=upper_limit), height=.2,
  color=color) +
#scale_y_log10(breaks=ticks, labels = ticks) +
geom_vline(xintercept = 1, linetype=2) +
#coord_flip() +
#facet_grid(measure ~ ., scales="free_x", space="free") +
labs(y = 'Interventions', x = '$ per DALY averted (US$)') +
theme_bw()


#### Quality assessment
library(reshape)
data_quality<-read.csv("/Users/jnv4/OneDrive - Duke University/datasets/DGNN/prevention_initiatives_cost_SR/prevention_initiatives_cost_quality.csv")
colnames(data_quality)<-c("Studies","V1","V2","V3","V4","V5","V6","V7","V8","V9",
  "V10","V11","V12","V13","V14","V15","V16")
str(data_quality)

data_quality_m<-melt(data_quality)
data_quality_m$value<-as.factor(data_quality_m$value)
#nba.m <- ddply(data_quality_m, .(variable), transform,
#     rescale = rescale(value))
p <- ggplot(data_quality_m, aes(Studies, variable)) + 
     geom_raster(aes(fill = value)) + 
     scale_fill_manual(values=c("white","steelblue"),
      labels=c("No","Yes"),name="Evaluation")
p <-   p +  scale_y_discrete(limits=rev(c("V1","V2","V3","V4","V5","V6",
      "V7","V8","V9","V10","V11","V12","V13","V14","V15","V16")),
     labels=rev(c(
      "1. Was the study objective presented in a clear, specific, 
      and measurable manner?",
      "2. Was the perspective of the analysis (societal, 
      third-party, payer, etc) and reason for its selection stated?",
      "3. Were variable estimates used in the analysis from 
      the best available source?",
      "4. If estimates came from a subgroup analysis, 
      were the groups pre-specified at the beginning of the study?",
      "5. Was the uncertainty handled by (1) statistical analysis to address 
      random events; or (2) sensitivity analyis to cover a range of assumptions.",
      "6. Was incrememtal analysis performed between alternatives 
      of resources and costs?",
      "7. Was the methodology for data abstraction 
      (including value health states and other benefits) stated?",
      "8. Did the analytic horizon allow time for all relevant and 
      important outcomes? Were benefits and costs that went beyond 
      1 year discounted (3-5%) and justification given for the discount rate?",
      "9. Was the measurement of costs appropriate, and the methodology 
      for the estimation of quantities and unit costs clearly described?",
      "10. Were the primary outcome measure(s) for the economic 
      evaluation clearly stated and were the major short-term, 
      long-term, and negative outcomes included?",
      "11. Were the health outcomes measures/scale valid and reliable? 
      If previously tested valid and reliable measures were not 
      available, was justification given for the measures/scales used?",
      "12. Were the economic model (including structure) study methods 
      and analysis and the components of the numerator and denominator 
      displayed in a clear transparent manner?",
      "13. Were the choice of economic model and main assumptions 
      and limitations of the study stated and justified?",
      "14. Did the author(s) explicitly discuss direction 
      and magnitude of potential bias?",
      "15. Were the conclusions/recommendations of the study 
      justified and based on the study results?",
      "16. Was there a statement disclosing the source of 
      funding for the study?")))
p<- p +  scale_x_discrete(limits=c("Bishai & Hyder, 2006",
  "Bishai et al., 2008","Stevenson et al., 2008",
  "Chisholm et al., 2012","Ditsuwan et al., 2013",
  "Olson et al., 2016","% of Yes"))
p<- p +  theme_bw() +
     theme(axis.title.x = element_text(face="bold"), 
      axis.text.x  = element_text(angle=330, 
      size=10, hjust=0)) + ylab("") +
     annotate("label", x =7, y = 1:16, 
      label = rev(c("71%","86%","71%","71%","43%",
                "29%","86%","43%","86%","86%",
                "86%","86%","71%","29%","86%",
                "71%")))


tiff("/Users/jnv4/Desktop/quality_cost_SR.tiff",
  height=600, width=800)
p
dev.off()  

