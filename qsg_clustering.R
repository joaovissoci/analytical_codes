library(repmis)
library(igraph)
library(qgraph)
library(psych)
library(nFactors)

qsgb <- read.csv("/home/joao/Dropbox/working stations/coxinhas/Artigo QSG/QSG puc.csv",sep=";")

#MAMBAC(scale(NeckDisabilityIndexNA)[,1:3], Comp.Data = T)
#EFA
#Group of functinos to determine the number os items to be extracted
#Group of functinos to determine the number os items to be extracted
par(mfrow=c(2,2)) #Command to configure the plot area for the scree plot graph
ev <- eigen(cor_auto(qsgb)) # get eigenvalues - insert the data you want to calculate the scree plot for
ev # Show eigend values
ap <- parallel(subject=nrow(qsgb),var=ncol(qsgb),rep=100,cent=.05) #Calculate the acceleration factor
summary(ap)
nS <- nScree(ev$values) #Set up the Scree Plot 
plotnScree(nS) # Plot the ScreePlot Graph

qsgc<-cor_auto(qsgb)
#qsgc<-qsgc$rho

qsggr<-list(positivos=c(1,3,4,7,8,12),negativos=c(2,5,6,9,10,11))
nomesqsg<-c("Tem podido concentrar-se no que faz","Suas preocupaçoes o fazem perder o sono","Tem sentido que tem papel útil na vida","Tem sido capaz de tomar decisões","Tem notado que está agoniado","Tem sensação de não superar dificuldades","Tem sido capaz de desfrutar de atividades","Tem sido capaz de enfrentar problemas","Tem se sentido pouco feliz e deprimido","Tem perdido confiança em si mesmo","Tem pensado que não serve para nada","Sente-se razoavelmente feliz")

qsgg3<-qgraph(qsgc,layout="spring",vsize=6,esize=20,graph="glasso",sampleSize=nrow(qsgb),legend.cex = 0.5,GLratio=1.5)
qsgg2<-qgraph(qsgc,layout="spring",vsize=6,esize=20,graph="pcor",threshold="holm",sampleSize=nrow(qsgb),legend.cex = 0.5,GLratio=1.5)
qsgg1<-qgraph(qsgc,layout="spring",vsize=6,esize=20,legend.cex = 0.5,GLratio=1.5)
Lqsg<-averageLayout(qsgg1,qsgg2,qsgg3)

qsgG1<-qgraph(qsgc,layout=Lqsg,nodeNames=nomesqsg,vsize=6,esize=20,legend.cex = 0.3,cut = 0.3, maximum = 1, minimum = 0, esize = 20,vsize = 5, repulsion = 0.8,groups=qsggr,gray=TRUE,color=c("gray80","gray50"),legend=F)
qsgG2<-qgraph(qsgc,layout=Lqsg,nodeNames=nomesqsg,vsize=6,esize=20,graph="pcor",legend.cex = 0.3,cut = 0.1, maximum = 1, minimum = 0, esize = 20,vsize = 5, repulsion = 0.8,groups=qsggr,gray=TRUE,color=c("gray80","gray50"),legend=F)
qsgG3<-qgraph(qsgc,layout=Lqsg,nodeNames=nomesqsg,vsize=6,esize=20,graph="glasso",sampleSize=nrow(qsgb),legend.cex = 0.3,cut = 0.1, maximum = 1, minimum = 0, esize = 20,vsize = 5, repulsion = 0.8,groups=qsggr,gray=T,color=c("gray80","gray50"))
metric<-centrality(qsgG3)
metric$ShortestPathLengths
## Dendogram
heatmap_data<-as.matrix(data[-c(1,9)])
rownames(heatmap_data)<-data$Estados
heatmap(metric$ShortestPathLengths, scale='none',col=cm.colors(256))#, Colv=NA
heatmap(cor_auto(qsgb), scale='none',col=cm.colors(256))#, Colv=NA
heatmap(X, scale='none',col=cm.colors(256))#, Colv=NA

qsgG3$Edgelist$from
qsgG3$Edgelist$to
sum(qsgG3$Edgelist$weight)

subset(importance_network_glasso$Edgelist$weight,importance_network_glasso$Edgelist$from==1 & importance_network_glasso$Edgelist$to==15)




centralityPlot(qsgG3)
clusteringPlot(qsgG3)
g<-as.igraph(qsgg3)

# Algorítmo que as pessoas mais usam
community1<-walktrap.community(g)
plot(community1,g)

# Algorítmo secundário
# Cuidado ao utilizar porque o peso da associação é importante para o algorítmo
# Processo de simulação para comparação com os dados amostrais
community2<-spinglass.community(g)
plot(community2,g)

modularity(community2)
X<-modularity_matrix(g,community2$membership)

subg1<-induced.subgraph(g, which(membership(community2)==2)) #membership id differs for each cluster
ecount(subg1)
ecount(g)
intradensity1 <- ecount(subg1)/ecount(g) #for each cluster
intradensity1

vcount(subg1)
ecount(subg1)
E(subg1)$weight

wdensity<-function(x,community) {
	weighted_density<-NULL
	graph<-NULL
	output <- matrix(NA, nrow=max(membership(community))+1, ncol=2)
	for(i in 1:max(membership(community))){
	subg<-induced.subgraph(x, which(membership(community)==i)) #membership id differs for each cluster
	weighted_density[i]<-2*sum(E(subg)$weight) / (abs(vcount(subg)) + sum(E(subg)$weight) - abs(ecount(subg)))*(abs(vcount(subg)) + sum(E(subg)$weight) - (abs(ecount(subg)) - 1))
	graph[i]<-c(i)
	output[i,]<-c(weighted_density[i],graph[i])
	}
	final_line<-max(membership(community))+1
	weighted_density[final_line]<-2*sum(E(x)$weight) / (abs(vcount(x)) + sum(E(x)$weight) - abs(ecount(x)))*(abs(vcount(x)) + sum(E(x)$weight) - (abs(ecount(x)) - 1))
	graph[final_line]<-c('general network')
	output[final_line,]<-c(weighted_density[final_line],graph[final_line])
	output<-as.data.frame(output)
	output[,1]<-as.numeric(as.character(output[,1]))
	colnames(output)<-c("wdens","com")
	output$reliability<-output$wdens/(weighted_density[final_line]-output$wdens)
	return(output)
}
wdensity(g,community1)




#density = 2 (sum of weights) / ((|V| + sum of weights - |E|)(|V| + sum of weights - |E| - 1))
#https://www.quora.com/How-do-you-compute-the-density-of-a-weighted-graph

2*sum(E(subg1)$weight) / (abs(vcount(subg1)) + sum(E(subg1)$weight) - abs(ecount(subg1)))*(abs(vcount(subg1)) + sum(E(subg1)$weight) - (abs(ecount(subg1)) - 1))