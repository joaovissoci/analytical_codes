plot_odds<-function(x, title = NULL){
tmp<-data.frame(cbind(exp(coef(logmodel)), exp(confint(logmodel))))
odds<-tmp[-1,]
names(odds)<-c('OR', 'lower', 'upper')
odds$vars<-row.names(odds)
odds$groups<-c(rep("Hour of day",2),"Urban vs. Rural",
	rep("Day of week",6), "Collision VRU","Road condition","Type of location", "Speed sign",rep("Type Vehicle",3),"Speeding",
	"Alcohol test", rep("Victim type",2),"Road size")
#ticks<-c(seq(.1, 1, by =.1), seq(0, 10, by =1), seq(10, 100, by =10))

ggplot(odds, aes(y= OR, x = reorder(vars, OR))) +
geom_point() +
geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
#scale_y_log10(breaks=ticks, labels = ticks) +
geom_hline(yintercept = 1, linetype=2) +
#facet_grid(groups~.,scales="free_y") +
coord_flip() +
labs(x = 'Variables', y = 'OR') +
theme_bw()
}

plot_odds(logmodel)