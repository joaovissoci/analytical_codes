data<-read.csv("/Users/joaovissoci/OneDrive - Duke University/datasets/DGNN/network_dgnn.csv")

library(qgraph)
str(data)

values<-as.matrix(data[,-1])
rownames(values)<-c("DGNN","Integraty","Collaborative", 
	"Passion","Research","Diversity","Education")

qgraph(values,layout="spring",borders=TRUE,
	labels=c("DGNN","Integraty","Teamwork","Passion","Research",
	"Diversity","Education"),label.scale=FALSE,label.cex=c(2,rep(1.5,6)),
	posCol="greenyellow",vsize=c(15,rep(10,6)),
	border.color=c("darkblue",rep("steelblue",6)),border.width=15,esize=20)

qgraph(values,borders=TRUE,
	labels=c("DGNN","Integraty","Teamwork","Passion","Research",
	"Diversity","Education"),label.scale=FALSE,label.cex=c(2,rep(1.5,6)),
	posCol="gray",vsize=c(15,rep(10,6)),
	border.color=c("darkblue",rep("steelblue",6)),border.width=15,esize=20)

numeric<-rep(10,6)
names<-c("Integrity","Collaborative", 
	"Passion","Research","Diversity","Education")

pie(numeric,names, col=c("darkblue","dodgerblue4","dodgerblue3",
	"dodgerblue2","lightblue3","lightblue1"),
 border="white",cex=1.5)
par(new=TRUE)  
pie(c(1), labels=NA, border="white", radius=0.5)
text(0,0,labels="Duke Global\nNeuroscience and\nNeurosurgery", 
	cex=1.5, font=2)


library(wordcloud)

wordcloud(jeopCorpus, max.words = 100, random.order = FALSE)


words<-as.factor(c("Duke","Global","Neuroscience","Neurosurgery","DGNN","Integrity",
	"Collaborative","Passion","Research","Diversity","Education","Vision"))
freq <- as.numeric(c(2,2,2,2,4,7,7,7,7,7,7,10))
color<-c("steelblue","steelblue","steelblue","steelblue","steelblue",
	     "darkblue","darkblue","darkblue","darkblue","darkblue","darkblue",
	     "greenyellow")
#tdm <- TermDocumentMatrix(words)

wordcloud(words,freq,min.freq=1,ordered.color=TRUE,colors=color)

data(crude)

tdm <- TermDocumentMatrix(crude)
    m <- as.matrix(tdm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
 d
    wordcloud(d$word,d$freq)	
