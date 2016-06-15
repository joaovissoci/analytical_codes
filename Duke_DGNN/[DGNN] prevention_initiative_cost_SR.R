

data<-read.csv("/Users/joaovissoci/OneDrive - Duke University/datasets/DGNN/prevention_initiatives_cost_SR/prevention_initiatives_cost_SR.csv")

library(Hmisc)
library(psych)
library(ggplot2)
library(gridExtra)
library(gtable)
library(grid)

daly_descriptive<-with(data,by(daly_cost,intervention,summary))

#str(daly_descriptive)

median_daly<-c(daly_descriptive[2][[1]][3],
               daly_descriptive[4][[1]][3],
               daly_descriptive[11][[1]][3],
               daly_descriptive[12][[1]][3],
               daly_descriptive[13][[1]][3],
               daly_descriptive[14][[1]][3],
               daly_descriptive[15][[1]][3],
               daly_descriptive[16][[1]][3],
               daly_descriptive[17][[1]][3],
               daly_descriptive[18][[1]][3],
               daly_descriptive[19][[1]][3],
               daly_descriptive[20][[1]][3],
               daly_descriptive[21][[1]][3])
low_limit_daly<-c(daly_descriptive[2][[1]][1],
               daly_descriptive[4][[1]][1],
               daly_descriptive[11][[1]][1],
               daly_descriptive[12][[1]][1],
               daly_descriptive[13][[1]][1],
               daly_descriptive[14][[1]][1],
               daly_descriptive[15][[1]][1],
               daly_descriptive[16][[1]][1],
               daly_descriptive[17][[1]][1],
               daly_descriptive[18][[1]][1],
               daly_descriptive[19][[1]][1],
               daly_descriptive[20][[1]][1],
               daly_descriptive[21][[1]][1])
upper_limit_daly<-c(daly_descriptive[2][[1]][6],
               daly_descriptive[4][[1]][6],
               daly_descriptive[11][[1]][6],
               daly_descriptive[12][[1]][6],
               daly_descriptive[13][[1]][6],
               daly_descriptive[14][[1]][6],
               daly_descriptive[15][[1]][6],
               daly_descriptive[16][[1]][6],
               daly_descriptive[17][[1]][6],
               daly_descriptive[18][[1]][6],
               daly_descriptive[19][[1]][6],
               daly_descriptive[20][[1]][6],
               daly_descriptive[21][[1]][6])
intervention_daly<-c("RBT with media",
                     "SBT with media",
                     "Bike helmet",
                     "Drink driving",
                     "Enforcement",
                     "Enforcement with media",
                     "Helmet",
                     "Media",
                     "RBT",
                     "Seat belt",
                     "SBT",
                     "Speed bump",
                     "Speed limits")

death_data<-na.omit(with(data,data.frame(death_cost,intervention,reference,
            region)))
death_descriptive<-with(death_data,by(death_cost,intervention,summary))
#str(death_descriptive)
median_death<-c(death_descriptive[13][[1]][3],
                death_descriptive[14][[1]][3],
                death_descriptive[15][[1]][3],
                death_descriptive[16][[1]][3],
                death_descriptive[20][[1]][3])
low_limit_death<-c(death_descriptive[13][[1]][1],
                death_descriptive[14][[1]][1],
                death_descriptive[15][[1]][1],
                death_descriptive[16][[1]][1],
                death_descriptive[20][[1]][1])
upper_limit_death<-c(death_descriptive[13][[1]][6],
                death_descriptive[14][[1]][6],
                death_descriptive[15][[1]][6],
                death_descriptive[16][[1]][6],
                death_descriptive[20][[1]][6])
intervention_death<-c("Enforcement","Enforcement and Media",
	"Helmet","Media","Speed bumps")

life_data<-na.omit(with(data,data.frame(life_cost,intervention,reference,
            region)))
life_descriptive<-with(life_data,by(life_cost,intervention,summary))
#str(life_descriptive)
median_life<-c(life_descriptive[13][[1]][3],
               life_descriptive[14][[1]][3],
               life_descriptive[16][[1]][3],
               life_descriptive[20][[1]][3])
low_limit_life<-c(life_descriptive[13][[1]][1],
                  life_descriptive[14][[1]][1],
                  life_descriptive[16][[1]][1],
                  life_descriptive[20][[1]][1])
upper_limit_life<-c(life_descriptive[13][[1]][6],
                    life_descriptive[14][[1]][6],
                    life_descriptive[16][[1]][6],
                    life_descriptive[20][[1]][6])
intervention_life<-c("Enforcement","Enforcement and Media"
  ,"Media","Speed bumps")

median<-c(median_daly,median_death,median_life)
low_limit<-c(low_limit_daly,low_limit_death,low_limit_life)
upper_limit<-c(upper_limit_daly,upper_limit_death,upper_limit_life)
intervention<-c(intervention_daly,intervention_death,intervention_life)
measure<-c(rep("DALY",13),rep("Death Averted",5),
  rep("Life Saved",4))

plot<-data.frame(median,low_limit,upper_limit,intervention,measure)
plot$upper_limit<-car::recode(plot$upper_limit,"2001:6000=2000")

object1<- ggplot(plot[1:13,], aes(y= reorder(intervention,median), 
  x = round(median,2))) +
facet_grid( measure ~ .,space="free") +
geom_point() +
geom_errorbarh(aes(xmin=low_limit, xmax=upper_limit), height=.2) +
geom_vline(xintercept = 1, linetype=2) +
geom_text(aes(label=format(round(median,2),nsmall=2)), 
  vjust=-0.5, hjust=0, size=3) +
#coord_flip() +
#facet_grid(measure ~ ., scales="free_x", space="free") +
labs(y = '', x = '') +
scale_x_continuous(breaks=seq(0, 2000, 200)) +
annotate("segment", x = 2000, xend=2020, y = 13, yend=13,
  colour = "black",
  arrow=arrow(length=unit(0.2,"cm"),type = "closed")) +
annotate("segment", x = 2000, xend=2020, y = 10, yend=10,
  colour = "black",
  arrow=arrow(length=unit(0.2,"cm"),type = "closed")) +
annotate("text", x = 1990, y = 12.7,
  colour = "black",label="2940.52",size=3) +
annotate("text", x = 1990, y = 9.7,
  colour = "black",label="2014.76",size=3) +
theme_bw()

object1

object2<- ggplot(plot[14:18,], aes(y= reorder(intervention,median), 
  x = round(median,2))) +
facet_grid( measure ~ .,space="free") +
geom_point() +
geom_errorbarh(aes(xmin=low_limit, xmax=upper_limit), height=.2) +
#scale_y_log10(breaks=ticks, labels = ticks) +
geom_vline(xintercept = 1, linetype=2) +
geom_text(aes(label=format(round(median,2),nsmall=2)), 
  vjust=-0.5, hjust=0, size=3) +
#coord_flip() +
#facet_grid(measure ~ ., scales="free_x", space="free") +
labs(y = '', x = '') +
scale_x_continuous(breaks=seq(0, 2000, 200)) +
annotate("segment", x = 2000, xend=2020, y = 4, yend=4,
  colour = "black",
  arrow=arrow(length=unit(0.2,"cm"),type = "closed")) +
annotate("segment", x = 2000, xend=2020, y = 3, yend=3,
  colour = "black",
  arrow=arrow(length=unit(0.2,"cm"),type = "closed")) +
annotate("text", x = 1990, y = 3.7,
  colour = "black",label="5560.00",size=3) +
annotate("text", x = 1990, y = 2.7,
  colour = "black",label="5171.00",size=3) +
theme_bw()

object2

object3<- ggplot(plot[19:22,], aes(y= reorder(intervention,median), 
  x = round(median,2))) +
facet_grid( measure ~ .,space="free") +
geom_point() +
geom_errorbarh(aes(xmin=low_limit, xmax=upper_limit), height=.2) +
#scale_y_log10(breaks=ticks, labels = ticks) +
geom_vline(xintercept = 1, linetype=2) +
geom_text(aes(label=format(round(median,2),nsmall=2)), 
  vjust=-0.5, hjust=0, size=3) +
#coord_flip() +
#facet_grid(measure ~ ., scales="free_x", space="free") +
scale_x_continuous(breaks=seq(0, 120, 20)) +
labs(y = '', x = '') +
theme_bw()

object3

tiff("/Users/joaovissoci/Desktop/figure1.tiff",
  width = 600, height = 600,compression = 'lzw')
grid.arrange(arrangeGrob(object1,object2,object3,
  left = textGrob("Interventions", rot = 90, vjust = 1),
  bottom = textGrob("Cost-Effectiveness/US$"),
  heights=c(0.8,0.4,0.35)))
dev.off()

##### Cmparison with Lancet

low_limit_lancet<-c(500.41,453.74,51.86,12.96,6.48)
upper_limit_lancet<-c(706.54,648.20,220.39,25.93,22.04)
median_limit_lancet<-c(706.54,648.20,220.39,25.93,22.04)
intervention_daly<-c("RBT with media",
                     "SBT with media",
                     "Bike helmet",
                     "Drink driving",
                     "Enforcement",
                     "                                            Enforcement with media",
                     "Helmet",
                     "Media",
                     "RBT",
                     "Seat belt",
                     "SBT",
                     "Speed bump",
                     "Speed limits")
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
facet<-c(rep("RTI Prevention Initiatives",13),
  rep("Public Health Initiatives",5))
#measure<-c(rep("Cost per DALY",13),rep("Cost per Death Averted",5),
#  rep("Cost per Life Saved",4))
color<-c(rep("black",13),rep("red",5))

# facet_names <- c(
#                     'ours' = "RTI Prevention Initiatives",
#                     'theirs' = "Public Health Initiatives"
#                     )

plot<-data.frame(median,low_limit,upper_limit,intervention,color,facet)
plot$upper_limit<-car::recode(plot$upper_limit,"2001:6000=2000")

object1<-ggplot(plot[1:13,], aes(y= reorder(intervention,median), x = median)) +
geom_point(color="black") +
geom_errorbarh(aes(xmin=low_limit, xmax=upper_limit), height=.2,
  color="black") + 
facet_grid(facet ~ .,space="free") + 
geom_vline(xintercept = 1, linetype=2) +
geom_text(aes(label=format(round(median,2),nsmall=2)), 
  vjust=-0.5, hjust=0, size=2.5) +
scale_x_continuous(breaks=seq(0, 2000, 200)) +
annotate("segment", x = 2000, xend=2020, y = 13, yend=13,
  colour = "black",
  arrow=arrow(length=unit(0.2,"cm"),type = "closed")) +
annotate("text", x = 1990, y = 12.7,
  colour = "black",label="2940.52",size=3) +
annotate("text", x = 1990, y = 9.7,
  colour = "black",label="2014.76",size=3) +
annotate("segment", x = 2000, xend=2020, y = 10, yend=10,
  colour = "black",
  arrow=arrow(length=unit(0.2,"cm"),type = "closed")) +
labs(y = '', x = '') +
theme_bw()



object2<-ggplot(plot[14:18,], aes(y= reorder(intervention,median), 
  x = median)) +
# geom_point(color="red") +
geom_errorbarh(aes(xmin=low_limit, xmax=upper_limit), height=.2,
  color="red",linetype=2) + 
facet_grid(facet ~ .,space="free") + 
geom_vline(xintercept = 1, linetype=2) +
geom_text(aes(label=format(round(median,2),nsmall=2)), 
  vjust=-0.5, hjust=0, size=2.5) +
geom_text(aes(label=format(round(low_limit,2),nsmall=2)), 
  vjust=-0.5, hjust=c(2.3,2.3,2.2,1.1,1.2), size=2.5) +
annotate("point", x = 2000, y = 1,
  colour = "white") +
scale_x_continuous(breaks=seq(0, 2000, 200)) +
labs(y = '', x = '') +
theme_bw()

tiff("/Users/joaovissoci/Desktop/figure2.tiff",
  width = 600, height = 500,compression = 'lzw')
grid.arrange(arrangeGrob(object1,object2,
  left = textGrob("Interventions", rot = 90, vjust = 1),
  bottom = textGrob("$ per DALY averted (US$)"),nrow=2,ncol=1,
  heights=c(0.8,0.4)))
dev.off()

#### Quality assessment
library(reshape)
data_quality<-read.csv("/Users/joaovissoci/OneDrive - Duke University/datasets/DGNN/prevention_initiatives_cost_SR/prevention_initiatives_cost_quality.csv")
colnames(data_quality)<-c("Studies","V1","V2","V3","V4","V5","V6","V7","V8","V9",
  "V10","V11","V12","V13","V14","V15","V16")
str(data_quality)

data_quality_sum<-colSums(data_quality[,-1])
data_quality_prop<-round((data_quality_sum/6)*100,0)

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
      label = rev(data_quality_prop))


tiff("/Users/joaovissoci/Desktop/figure0.tiff",
  height=600, width=800)
p
dev.off()  

