
data<-read.csv("/home/joao/Desktop/IHD_data.csv")
data<-data[-2]

library(ggplot2)
library(reshape2)
cor_data<-melt(data,by="Estados")
cor_data$value2<-round(cor_data$value,digits=2)

# heatmap by regions
ggplot(cor_data, aes(y=Estados, x=variable, fill=value2)) + geom_tile() + geom_text(aes(y=Estados, x=variable, label=value2)) + scale_fill_gradient2(low="darkblue",high="darkred", limits=c(-1,1)) + facet_grid(regions ~ .,scales="free_y",space="free") 


## Dendogram
heatmap_data<-as.matrix(data[-c(1,9)])
rownames(heatmap_data)<-data$Estados
heatmap(heatmap_data, Colv=NA, scale='none',col=cm.colors(256))
