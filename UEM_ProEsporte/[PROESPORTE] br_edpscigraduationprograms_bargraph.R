
data_freq_psy<-read.csv("/Users/Joao/Desktop/psy_wordfreq.csv")
data_freq_psy <- data_freq_psy[with(data_freq_psy,order(-contribution)),]
data_freq_psy <- data_freq_psy[1:20,]
data_freq_psy$word2<-c("sport",
					   "education",
					   "video game",
					   "game",
					   "behavioral",
					   "play",
					   "children",
					   "health",
					   "development",
					   "school",
					   "adolescents",
					   "evaluation",
					   "cognitive",
					   "context",
					   "football",
					   "learning",
					   "physical activity",
					   "physical education",
					   "elderly",
					   "intervention")

data_freq_edf<-read.csv("/Users/Joao/Desktop/edfisica_wordfreq.csv")
data_freq_edf$contribution<-data_freq_edf$contribution*-1
data_freq_edf <- data_freq_edf[with(data_freq_edf,order(contribution)),]
data_freq_edf <- data_freq_edf[1:20,]
data_freq_edf$word2<-c("quality of life",
					   "physical activity",
					   "exercise",
					   "sport",
					   "athlete",
					   "health",
					   "functional",
					   "elderly",
					   "training",
					   "aspects",
					   "behavioral",
					   "disease",
					   "motor",
					   "cognitive",
					   "stress",
					   "practice",
					   "instruments",
					   "program",
					   "performance",
					   "evaluation")

data_freq<-rbind(data_freq_psy,data_freq_edf)
data_freq$course<-c(rep("Psychology",length(data_freq_psy[,1])),
					rep("Physical Education",length(data_freq_edf[,1])))

tiff("/Users/Joao/Desktop/frequency_figure.tiff",
 width = 1500, height = 2000,compression = 'lzw', res=300)
data_freq %>%
        # mutate(contribution = n * score) %>%
        # arrange(desc(abs(contribution))) %>%
        # group_by(course) %>%
        # head(10) %>%
        ggplot(aes(reorder(word2, contribution), contribution, fill = course)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        xlab("Most frequent words") +
        ylab("Propotion of times the word appeared in a title") + 
        coord_flip() +
        facet_grid(course ~.,scales = "free") +
        theme_bw()
dev.off()