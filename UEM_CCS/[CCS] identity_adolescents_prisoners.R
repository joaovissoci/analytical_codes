------------
#setwd("/Users/joaovissoci/Google Drive/ToDos")

#lendo o arquivo "noname"
#file <- file.choose()
dados <- read.csv("/home/joao/Dropbox/datasets/CCS/juvenile_offenders/adolecents_prisoners_text_data_coded.csv", header = TRUE)
dados_theme1<-subset(dados,dados$Theme=="perspectiva de futuro")
dados_theme2<-subset(dados,dados$Theme=="reconhecimento")
dados_theme3<-subset(dados,dados$Theme=="relacoes e vinculos familiares")
dados_theme4<-subset(dados,dados$Theme=="vivencia")
#dados_theme5<-subset(dados,dados$Theme=="vivencia escolar")
dados_theme6<-subset(dados,dados$Theme=="medida_socioeducativa")
#str(dados)
library(tm)
library(SnowballC)
library(qgraph)
library(polycor)
library(igraph)

#head(dados)
#names(dados)
#colnames(dados)[3] <- "renda"
#colnames(dados)[2] <- "entrevista"
#names(dados)

#dados<-dados[2,]

#### THEME 1 ################################################
#fazendo a mineração de texto:
corpus = Corpus(VectorSource(dados_theme1$Text))
#corpus<- tm_map(corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("portuguese")))
corpus <- tm_map(corpus, stemDocument)


# olhando o primeiro conteudo das entrevistas
corpus[[1]]

#transformando em matriz
dtm <- DocumentTermMatrix(corpus)
dtm

#removendo "sparse terms": vai fazer cair de 3 mil e cacetada termos para menos termos
dtm <- removeSparseTerms(dtm, 0.8)
dtm

#findFreqTerms(dtm, 5)
#findAssocs(dtm, "vencer", 0.5)

#fazendo um plot da relação entre as palavras novamente
#plot(dtm, corThreshold = 0.3, weighting = TRUE)

data<-as.matrix(dtm)
word_freq<-colSums(data)
#data[data>=1] <- 1
dataMatrix <- t(data) %*% data

cor <- hetcor(data)
names<-rownames(cor$correlations)

#Q1_atleta1 <- qgraph(dataMatrix, borders = FALSE, cut=20, 
#  minimum = 5, labels=rownames(dataMatrix),label.cex = 0.60, label.color="black",
#  layout = "spring",directed=FALSE,label.scale=FALSE,
#  posCol=c("#BF0000","red"),
#  gray=FALSE)

Q2_atleta2 <- qgraph(cor$correlations, borders = TRUE, cut=0.8, minimum = 0.4, labels=names,label.cex = 1, vsize=word_freq,label.color="black",layout = "spring",directed=FALSE,label.scale=FALSE,gray=FALSE,posCol=c("gray","gray"))

g<-as.igraph(Q2_atleta2)
h<-walktrap.community(g)
plot(h,g)

Q2_atleta2 <- qgraph(dataMatrix, borders = FALSE, cut=10, minimum = 3, labels=names,label.cex = 0.80, label.color="black",layout = "spring",directed=FALSE,label.scale=FALSE,gray=FALSE,posCol=c("gray","gray"))


#### THEME 2 ###############################################
#fazendo a mineração de texto:
corpus = Corpus(VectorSource(dados_theme2$Text))
#corpus<- tm_map(corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("portuguese")))
corpus <- tm_map(corpus, stemDocument)


# olhando o primeiro conteudo das entrevistas
corpus[[1]]

#transformando em matriz
dtm <- DocumentTermMatrix(corpus)
dtm

# removendo "sparse terms": vai fazer cair de 3 mil e cacetada termos para menos termos
dtm <- removeSparseTerms(dtm, 0.80)
dtm

#findFreqTerms(dtm, 5)
#findAssocs(dtm, "vencer", 0.5)

#fazendo um plot da relação entre as palavras novamente
#plot(dtm, corThreshold = 0.3, weighting = TRUE)

data<-as.matrix(dtm)
#data[data>=1] <- 1
dataMatrix <- t(data) %*% data

cor <- cor(data,method="spearman")
names<-rownames(cor)

#Q1_atleta1 <- qgraph(dataMatrix, borders = FALSE, cut=20, 
#  minimum = 5, labels=rownames(dataMatrix),label.cex = 0.60, label.color="black",
#  layout = "spring",directed=FALSE,label.scale=FALSE,
#  posCol=c("#BF0000","red"),
#  gray=FALSE)

Q2_atleta2 <- qgraph(cor, borders = FALSE, cut=0.6, minimum = 0.4, labels=names,label.cex = 0.80, label.color="black",layout = "spring",directed=FALSE,label.scale=FALSE,gray=FALSE,posCol=c("gray","gray"))

#g<-as.igraph(Q2_atleta2)
#h<-walktrap.community(g)
#plot(h,g)

Q2_atleta2 <- qgraph(dataMatrix, borders = FALSE, labels=names,label.cex = 0.80, minimum = 4, cut=10,label.color="black",layout = "spring",directed=FALSE,label.scale=FALSE,gray=FALSE,posCol=c("grey","black"))

#### THEME 3 ################################################
#fazendo a mineração de texto:
corpus = Corpus(VectorSource(dados_theme3$Text))
#corpus<- tm_map(corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("portuguese")))
corpus <- tm_map(corpus, stemDocument)


# olhando o primeiro conteudo das entrevistas
corpus[[1]]

#transformando em matriz
dtm <- DocumentTermMatrix(corpus)
dtm

# removendo "sparse terms": vai fazer cair de 3 mil e cacetada termos para menos termos
dtm <- removeSparseTerms(dtm, 0.80)
dtm

#findFreqTerms(dtm, 5)
#findAssocs(dtm, "vencer", 0.5)

#fazendo um plot da relação entre as palavras novamente
#plot(dtm, corThreshold = 0.3, weighting = TRUE)

data<-as.matrix(dtm)
#data[data>=1] <- 1
dataMatrix <- t(data) %*% data

cor <- cor(data,method="spearman")
names<-rownames(cor)#$correlations)

#Q1_atleta1 <- qgraph(dataMatrix, borders = FALSE, cut=20, 
#  minimum = 5, labels=rownames(dataMatrix),label.cex = 0.60, label.color="black",
#  layout = "spring",directed=FALSE,label.scale=FALSE,
#  posCol=c("#BF0000","red"),
#  gray=FALSE)

Q2_atleta2 <- qgraph(cor, borders = FALSE, cut=0.6, minimum = 0.2, labels=names,label.cex = 0.80, label.color="black",layout = "spring",directed=FALSE,label.scale=FALSE,gray=FALSE,posCol=c("gray","gray"))

#g<-as.igraph(Q2_atleta2)
#h<-walktrap.community(g)
#plot(h,g)

#### THEME 4 ################################################
#fazendo a mineração de texto:
corpus = Corpus(VectorSource(dados_theme4$Text))
#corpus<- tm_map(corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("portuguese")))
corpus <- tm_map(corpus, stemDocument)


# olhando o primeiro conteudo das entrevistas
corpus[[1]]

#transformando em matriz
dtm <- DocumentTermMatrix(corpus)
dtm

# removendo "sparse terms": vai fazer cair de 3 mil e cacetada termos para menos termos
#dtm <- removeSparseTerms(dtm, 0.90)
#dtm

#findFreqTerms(dtm, 5)
#findAssocs(dtm, "vencer", 0.5)

#fazendo um plot da relação entre as palavras novamente
#plot(dtm, corThreshold = 0.3, weighting = TRUE)

data<-as.matrix(dtm)
#data[data>=1] <- 1
dataMatrix <- t(data) %*% data

cor <- hetcor(data)
names<-rownames(cor$correlations)

#Q1_atleta1 <- qgraph(dataMatrix, borders = FALSE, cut=20, 
#  minimum = 5, labels=rownames(dataMatrix),label.cex = 0.60, label.color="black",
#  layout = "spring",directed=FALSE,label.scale=FALSE,
#  posCol=c("#BF0000","red"),
#  gray=FALSE)

Q2_atleta2 <- qgraph(cor$correlations, borders = FALSE, cut=0.8, minimum = 0.6, labels=names,label.cex = 0.80, label.color="black",layout = "spring",directed=FALSE,label.scale=FALSE,gray=FALSE,posCol=c("gray","gray"))

#g<-as.igraph(Q2_atleta2)
#h<-walktrap.community(g)
#plot(h,g)

#### THEME 5 ################################################
#fazendo a mineração de texto:
corpus = Corpus(VectorSource(dados_theme5$Text))
#corpus<- tm_map(corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("portuguese")))
corpus <- tm_map(corpus, stemDocument)


# olhando o primeiro conteudo das entrevistas
corpus[[1]]

#transformando em matriz
dtm <- DocumentTermMatrix(corpus)
dtm

# removendo "sparse terms": vai fazer cair de 3 mil e cacetada termos para menos termos
#dtm <- removeSparseTerms(dtm, 0.90)
#dtm

#findFreqTerms(dtm, 5)
#findAssocs(dtm, "vencer", 0.5)

#fazendo um plot da relação entre as palavras novamente
#plot(dtm, corThreshold = 0.3, weighting = TRUE)

data<-as.matrix(dtm)
#data[data>=1] <- 1
dataMatrix <- t(data) %*% data

cor <- hetcor(data)
names<-rownames(cor$correlations)

#Q1_atleta1 <- qgraph(dataMatrix, borders = FALSE, cut=20, 
#  minimum = 5, labels=rownames(dataMatrix),label.cex = 0.60, label.color="black",
#  layout = "spring",directed=FALSE,label.scale=FALSE,
#  posCol=c("#BF0000","red"),
#  gray=FALSE)

Q2_atleta2 <- qgraph(cor$correlations, borders = FALSE, cut=0.8, minimum = 0.6, labels=names,label.cex = 0.80, label.color="black",layout = "spring",directed=FALSE,label.scale=FALSE,gray=FALSE,posCol=c("gray","gray"))

#g<-as.igraph(Q2_atleta2)
#h<-walktrap.community(g)
#plot(h,g)

#### THEME 6 ################################################
#fazendo a mineração de texto:
corpus = Corpus(VectorSource(dados_theme6$Text))
#corpus<- tm_map(corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("portuguese")))
corpus <- tm_map(corpus, stemDocument)


# olhando o primeiro conteudo das entrevistas
corpus[[1]]

#transformando em matriz
dtm <- DocumentTermMatrix(corpus)
dtm

# removendo "sparse terms": vai fazer cair de 3 mil e cacetada termos para menos termos
dtm <- removeSparseTerms(dtm, 0.80)
dtm

#findFreqTerms(dtm, 5)
#findAssocs(dtm, "vencer", 0.5)

#fazendo um plot da relação entre as palavras novamente
#plot(dtm, corThreshold = 0.3, weighting = TRUE)

data<-as.matrix(dtm)
#data[data>=1] <- 1
dataMatrix <- t(data) %*% data

cor <- cor(data,method="spearman")
names<-rownames(cor)#$correlations)

#Q1_atleta1 <- qgraph(dataMatrix, borders = FALSE, cut=20, 
#  minimum = 5, labels=rownames(dataMatrix),label.cex = 0.60, label.color="black",
#  layout = "spring",directed=FALSE,label.scale=FALSE,
#  posCol=c("#BF0000","red"),
#  gray=FALSE)

Q2_atleta2 <- qgraph(cor, borders = FALSE, cut=0.6, minimum = 0.4, labels=names,label.cex = 0.80, label.color="black",layout = "spring",directed=FALSE,label.scale=FALSE,gray=FALSE,posCol=c("gray","gray"))

#g<-as.igraph(Q2_atleta2)
#h<-walktrap.community(g)
#plot(h,g)






















##############################################################
#CALCULATING DESCRIPTIVES FROM THE NETWORK
##############################################################

#Transforming network into a IGRAPH object (created with the igraph package)
Q3<-as.igraph(qgraph(Q1_atleta1, DoNotPlot = TRUE))

#Calculating shortest path - association metric
#ADD A DEFINITION
shortest.paths(Q3)

#Calculating degree
#ADD A DEFINITION
degree(Q3)

#Calculating centrality
#ADD A DEFINITION
assortativity.degree(Q3)

#Calculating closeness
#ADD A DEFINITION
closeness(Q3)

#Calculating betweeness
#ADD A DEFINITION
betweenness(Q3)

#Calculating Eigenvalue centrality
#ADD A DEFINITION
evcent(Q3)

#Calculating Network density
#ADD A DEFINITION
graph.density(Q3)

#Calculating Network diameter
#ADD A DEFINITION
diameter(Q3)

### Clustering
#The local clustering coefficient, cl(v), gives for node n the proportion that the neighboors of v are also connected to each other.
#Maximum value is 1, value closer to 1 means higher clustering
transitivity(Q3, "local")
transitivity(Q3, "global")
transitivity(Q3, "average")

### Checking for Small World configuration of your network

#Function to calculate avareg length path in random graphs
#Just run every line without changing anything
APLr <- function(x) {
if ("qgraph" %in% class(x))
x <- as.igraph(x)
if ("igraph" %in% class(x))
x <- get.adjacency(x) 
N = nrow(x)
p = sum(x/2)/sum(lower.tri(x))
eulers_constant <- 0.57721566490153
l = (log(N) - eulers_constant)/log(p * (N - 1)) + 0.5 
l
}

#Run function
APLr(Q3)

#Function to calculate clustering in random graphs
#Just run every line without changing anything
Cr <- function(x) {
if ("qgraph" %in% class(x))
x <- as.igraph(x)
if ("igraph" %in% class(x))
x <- get.adjacency(x) 
N = nrow(x)
p = sum(x/2)/sum(lower.tri(x)) 
t = (p * (N - 1)/N)
t
}

#Run function
Cr(Q3)

#Valiues above 3 means your network is a small world configured network
(transitivity(Q3) / Cr(Q3)) / (average.path.length(Q3) / APLr(Q3))

#Calculating all descriptives from a qgraph object (might take a while)
#ADD A DEFINITION
x<-centrality(Q1_atleta1) #Returns all descriptives

