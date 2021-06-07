
data<-read.csv("/Users/joaovissoci/Downloads/PRACTR01Intervention_DATA_2021-01-18_2131.csv")

table(as.factor(data$int_nurse))
prop.table(table(as.factor(data$int_nurse)))

#KNOWLEDGE
likert_data1<-with(data,data.frame(
	bnias0,
	bnias1,
	bnias2,
	bnias3,
	bnias4,
	bnias5,
	bnias6,
	bnias7,
	bnias8,
	bnias9,
	bnias10,
	bnias11,
	bnias12,
	bnias13,
	bnias14,
	bnias15,
	bnias16,
	bnias17,
	bnias18,
	bnias19,
	bnias20,
	bnias21,
	bnias22))

likert_data1[likert_data1==1]<-"Counselor performed"
likert_data1[likert_data1==0]<-"Counselor did NOT perform"

mylevels <- c("Counselor performed", "Counselor did NOT perform")

likert_data2<-likert_data1
for(i in seq_along(likert_data1)) {
	likert_data2[,i] <- factor(likert_data1[,i], levels=mylevels)
}

# likert_data2$grouping<-as.factor(data$int_nurse)

data$int_nurse_recoded<-car::recode(data$int_nurse,"
	0='Eliza';
	1='Amina';
	2='Anna';
	3='Kelvin';
	4='Joyce'")

library(likert)
knowledge_data<-likert(likert_data2,grouping=data$int_nurse_recoded)



# knowledge_data$Group<-c("Knowledge")
summary(knowledge_data)
knowledge_plot<-plot(knowledge_data,
					 colors=c("#D33F6A",
					 		  # "#E07B91",
					 		  # "lightgrey",
					 		  # "#8595E1",
					 		  "#4A6FE3"))

knowledge_plot<- knowledge_plot + #scale_x_discrete(breaks=c(
							  # "talking_can_be_successful",
    						  # "discuss_risky_alc",
    						  # "discuss_counsel_pts",
    						  # "not_my_role",
    					      # "called_harmful_drinkers"),
													# labels=c(
							  # "Talking to patients about decreasing\n their alcohol ingestion can be successful.",
							  # "In my schooling, we discussed at­-risk\n alcohol behavior and alcohol abuse.",
							  # "In my schooling, we discussed counseling\n patients with at­risk drinking behaviors.",
							  # "It is my role to ask\n about alcohol use",
							  # "Once patients suffer an injury from\n drinking they are called 'harmful drinkers"  							
							  # )) +
				theme_bw() +
				theme(legend.position="bottom")

summary_data<-summary(knowledge_data)

mean_plot<-ggplot(summary_data, aes(y=mean,x=Item)) + 
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) +
    geom_line() +
    geom_point() +
    coord_flip() +
    xlab("") +
    ylab("Mean/Standard Deviation") +
    scale_x_discrete(limits=rev(c("talking_can_be_successful",
    							  "discuss_risky_alc",
    							  "discuss_counsel_pts",
    							  "not_my_role",
    							  "called_harmful_drinkers"))) +
    expand_limits(y=c(1:5)) +
    theme_bw() +
    theme(panel.background = element_rect(colour = 'grey'))

# require(ggpubr)

knowledge_grid<-ggarrange(knowledge_plot, #+ rremove("y.text"), 
		  mean_plot + rremove("y.text"), 
          labels = c("A", "B"),
          # heights = c(2, 0.7),
          ncol = 2,
          widths = c(1, 0.3),
          align = c("h"),
          # common.legend = TRUE,
          legend = "none")

ggsave("figure1.eps", #change .eps to .pdf for different format
		knowledge_plot, #plot is the name of the fig, but the function assumes the last plot if argument is NULL
		path="/Users/joaovissoci/Desktop", #path to save the plot
		width = 5, 
		height = 25, 
		device=cairo_ps) #cairo_ps is a d