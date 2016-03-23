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