------------
#setwd("/Users/joaovissoci/Google Drive/ToDos")

#lendo o arquivo "noname"
#file <- file.choose()
dados <- read.csv("/Users/jnv4/Box Sync/Home Folder jnv4/Data/CCS/juvenile_offenders/adolecents_prisoners_text_data_coded.csv", header = TRUE)
dados_theme1<-subset(dados,dados$Theme=="perspectiva de futuro")
dados_theme2<-subset(dados,dados$Theme=="reconhecimento")
dados_theme3<-subset(dados,dados$Theme=="relacoes e vinculos familiares")
dados_theme4<-subset(dados,dados$Theme=="vivencia")
dados_theme5<-subset(dados,dados$Theme=="vivencia escolar")
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

tiff("/Users/jnv4/Desktop/ana_fig1.tiff", units='in', 
  width = 15,
 height = 10,compression = 'lzw',res=1200,bg = "white")
Q2_atleta2 <- qgraph(cor$correlations,
                     borders = TRUE,
                     cut=0.8, 
                     minimum = 0.4, 
                     labels=names,
                     label.cex = 1, 
                     vsize=word_freq,
                     label.color="black",
                     layout = "spring",
                     directed=FALSE,
                     label.scale=FALSE,
                     gray=TRUE)
                     # posCol=c("gray","gray"))
dev.off()

g<-as.igraph(Q2_atleta2)
h<-spinglass.community(g)
plot(h,g)
h$membership

# Q2_atleta2 <- qgraph(dataMatrix, borders = FALSE, cut=10, minimum = 3, labels=names,label.cex = 0.80, label.color="black",layout = "spring",directed=FALSE,label.scale=FALSE,gray=FALSE,posCol=c("gray","gray"))

# g<-as.igraph(Q2_atleta2)
# h<-spinglass.community(g)
# plot(h,g)
# h$membership

# #Calculating Community measures
# g<-as.igraph(network_glasso) #creating igraph object
# h<-walktrap.community(g) #creatin community object
# h<-spinglass.community(g, weights=NA) #creatin community object
# # h<-fastgreedy.community(g, weights=NA) #creatin community object
# # h<-edge.betweenness.community(g, weights=NA) #creatin community object
# h<-cluster_leading_eigen(g,weights=NA) #creatin community object
# plot(h,g) #plotting community network
# h$membership #extracting community membership for each node on the network
community<-data.frame(h$membership,rownames(dataMatrix))

#listing grouping variables in the network resulting from the community analysis
network_groups<-list(
Component1=as.numeric(rownames(community)
	[community[,1]==1]),
Component2=as.numeric(rownames(community)
	[community[,1]==2]),
Component3=as.numeric(rownames(community)
	[community[,1]==3])
)

# # network_groups<-list(
# Component1=c(1,3,4,5,15,14),
# Component2=c(2,16,6,7),
# Component3=c(11,12,13,10),
# Component4=c(19,20,21,23),
# Component5=c(9,17,18,22,8)
# )

# creating vectors for labels
# node_labels<-c(
# "What is the area of the roadway?",
# "What type of roadway?",
# "Is this point at an intersection/junction?",
# "How many lanes in the roadway?",
# "Is there an auxiliary/other lane?",
# "How is the road surface conditions?",
# "Is there space on the side of the road 
# for any reason or use?",
# "Are there pedestrian pathways?",
# "Is there a Bus Stop?",
# "Is there a Speed bump?",
# "Is there a traffic light at this location?",
# "Are there road traffic signs at this hotspot?",
# "Is there a sign for speed limit of road?",
# "Road visibility is influenced by curves?",
# "Is the visibility influenced by 
# environmental factors?",
# "Are there bridges on the road?",
# "Is there a safe area for pedestrians 
# to cross the road?",
# "Is there a safe area for pedestrians
# to in the center of the road?",
# "Count the number of cars",
# "Count the number of moto",
# "Count the number of bike",
# "Count the number of pedestrians",
# "Count the number of bus/trucks"
# )

# creating nodes labels vector
# node_names<-c("Q1",
#               "Q2",
#               "Q3",
#               "Q4",
#               "Q5",
#               "Q6",
#               "Q7",
#               "Q8")

node_names<-rownames(cor$correlations)

# creating vector with mean values for each node
#mean_data<-sapply(network_data,mean)

#creating vector with mean values adjusted to proportional sizes to be plotted
#importance_vSize<-c(mean_data[1:14]/min(mean_data[1:14]),1.81)

#building network figures 
# 3 types are created to get an avarege position and layout
#GLASSO NETWORK
# network_glasso<-qgraph(cor_data,layout="spring",
#   vsize=6,esize=20,graph="glasso",
#   sampleSize=nrow(bea_data),
#   legend.cex = 0.5,GLratio=1.5)

# #PARTIAL CORRELATION NETWORK
# network_pcor<-qgraph(cor_data,layout="spring",
#   vsize=6,esize=20,graph="pcor",threshold="holm",
#   sampleSize=nrow(bea_data),
#   legend.cex = 0.5,GLratio=1.5)

# #CORRELATION NETWORK
# network_cor<-qgraph(cor_data,layout="spring",
#   vsize=6,esize=20,legend.cex = 0.5,GLratio=1.5)
# #layout1<-averageLayout(network_glasso,network_pcor,network_cor)

# # Organizing both figures to be with the same layout
# layout_final<-averageLayout(network_glasso,
#   network_pcor,
#   network_cor)

# postscript("/home/joao/Desktop/info_consent_figure2.eps",
#   width = 1500, height = 1200,horizontal = FALSE, 
#   onefile = FALSE)
# postscript("/Users/joaovissoci/Desktop/info_consent_figure2.eps",
#   width = 1500, height = 1200,horizontal = FALSE, 
#   onefile = FALSE)
# tiff("/Users/jnv4/Desktop/bea_pca_network.tiff", width = 1200,
 # height = 700,compression = 'lzw')
  network_glasso<-qgraph(cor$correlations,
  layout='spring',
  # esize=20,
  # graph="glasso",
  # sampleSize=nrow(sf8_data),
  legend.cex = 0.5,
  cut = 0.8,
  # maximum = 1, 
  minimum = 0.4,
  # esize = 20,
  vsize = word_freq, 
  # repulsion = 0.8,
  # nodeNames=
  # shape="square",
  borders=TRUE,
  # border.width=5,
  # groups=network_groups,
  # color=c("grey"),
  labels=rownames(cor$correlations)
  # label.scale=FALSE
  #gray=T,
  )
# dev.off()
#legend(0.8,-0.8, bty=".",c("Ensaio Clínico","Medicamentos","Outras Razões"),cex=1.2,fill=c("lightblue","red","yellow"))

#Directed Acyclic Graph / require package bnlearn
# dag_data <- data.frame(apply(sf8_data, 2, as.factor))
dag_data<-as.data.frame(dataMatrix)
res<-rsmax2(dag_data,
            restrict = "si.hiton.pc",
            maximize = "tabu")
res2<-(res$arcs)
qgraph(res2)

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
word_freq<-colSums(data)
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

# Q2_atleta2 <- qgraph(cor$correlations, borders = TRUE, cut=0.8, minimum = 0.4, labels=names,label.cex = 1, vsize=word_freq,label.color="black",layout = "spring",directed=FALSE,label.scale=FALSE,gray=FALSE,posCol=c("gray","gray"))

g<-as.igraph(Q2_atleta2)
h<-spinglass.community(g)
plot(h,g)
h$membership

# Q2_atleta2 <- qgraph(dataMatrix, borders = FALSE, cut=10, minimum = 3, labels=names,label.cex = 0.80, label.color="black",layout = "spring",directed=FALSE,label.scale=FALSE,gray=FALSE,posCol=c("gray","gray"))

# g<-as.igraph(Q2_atleta2)
# h<-spinglass.community(g)
# plot(h,g)
# h$membership

# #Calculating Community measures
# g<-as.igraph(network_glasso) #creating igraph object
# h<-walktrap.community(g) #creatin community object
# h<-spinglass.community(g, weights=NA) #creatin community object
# # h<-fastgreedy.community(g, weights=NA) #creatin community object
# # h<-edge.betweenness.community(g, weights=NA) #creatin community object
# h<-cluster_leading_eigen(g,weights=NA) #creatin community object
# plot(h,g) #plotting community network
# h$membership #extracting community membership for each node on the network
community<-data.frame(h$membership,
	rownames(cor))

#listing grouping variables in the network resulting from the community analysis
network_groups<-list(
Component1=as.numeric(rownames(community)
	[community[,1]==1]),
Component2=as.numeric(rownames(community)
	[community[,1]==2]),
Component3=as.numeric(rownames(community)
	[community[,1]==3]),
Component4=as.numeric(rownames(community)
	[community[,1]==4])
)

# # network_groups<-list(
# Component1=c(1,3,4,5,15,14),
# Component2=c(2,16,6,7),
# Component3=c(11,12,13,10),
# Component4=c(19,20,21,23),
# Component5=c(9,17,18,22,8)
# )

# creating vectors for labels
# node_labels<-c(
# "What is the area of the roadway?",
# "What type of roadway?",
# "Is this point at an intersection/junction?",
# "How many lanes in the roadway?",
# "Is there an auxiliary/other lane?",
# "How is the road surface conditions?",
# "Is there space on the side of the road 
# for any reason or use?",
# "Are there pedestrian pathways?",
# "Is there a Bus Stop?",
# "Is there a Speed bump?",
# "Is there a traffic light at this location?",
# "Are there road traffic signs at this hotspot?",
# "Is there a sign for speed limit of road?",
# "Road visibility is influenced by curves?",
# "Is the visibility influenced by 
# environmental factors?",
# "Are there bridges on the road?",
# "Is there a safe area for pedestrians 
# to cross the road?",
# "Is there a safe area for pedestrians
# to in the center of the road?",
# "Count the number of cars",
# "Count the number of moto",
# "Count the number of bike",
# "Count the number of pedestrians",
# "Count the number of bus/trucks"
# )

# creating nodes labels vector
# node_names<-c("Q1",
#               "Q2",
#               "Q3",
#               "Q4",
#               "Q5",
#               "Q6",
#               "Q7",
#               "Q8")

node_names<-rownames(cor)

# creating vector with mean values for each node
#mean_data<-sapply(network_data,mean)

#creating vector with mean values adjusted to proportional sizes to be plotted
#importance_vSize<-c(mean_data[1:14]/min(mean_data[1:14]),1.81)

#building network figures 
# 3 types are created to get an avarege position and layout
#GLASSO NETWORK
# network_glasso<-qgraph(cor_data,layout="spring",
#   vsize=6,esize=20,graph="glasso",
#   sampleSize=nrow(bea_data),
#   legend.cex = 0.5,GLratio=1.5)

# #PARTIAL CORRELATION NETWORK
# network_pcor<-qgraph(cor_data,layout="spring",
#   vsize=6,esize=20,graph="pcor",threshold="holm",
#   sampleSize=nrow(bea_data),
#   legend.cex = 0.5,GLratio=1.5)

# #CORRELATION NETWORK
# network_cor<-qgraph(cor_data,layout="spring",
#   vsize=6,esize=20,legend.cex = 0.5,GLratio=1.5)
# #layout1<-averageLayout(network_glasso,network_pcor,network_cor)

# # Organizing both figures to be with the same layout
# layout_final<-averageLayout(network_glasso,
#   network_pcor,
#   network_cor)

word_freq<-word_freq/3
word_freq<-car::recode(word_freq,"1:5=7;8=9;10:11=15;13:14=20")

# postscript("/home/joao/Desktop/info_consent_figure2.eps",
#   width = 1500, height = 1200,horizontal = FALSE, 
#   onefile = FALSE)
# postscript("/Users/joaovissoci/Desktop/info_consent_figure2.eps",
#   width = 1500, height = 1200,horizontal = FALSE, 
#   onefile = FALSE)
# tiff("/Users/jnv4/Desktop/bea_pca_network.tiff", width = 1200,
 # height = 700,compression = 'lzw')
  network_glasso<-qgraph(cor,
  layout='spring',
  # esize=20,
  # graph="glasso",
  # sampleSize=nrow(sf8_data),
  legend.cex = 0.5,
  cut = 0.8,
  # maximum = 1, 
  minimum = 0.4,
  # esize = 20,
  vsize = word_freq/2, 
  # repulsion = 0.8,
  # nodeNames=
  # shape="square",
  borders=TRUE,
  # border.width=5,
  # groups=network_groups,
  # color=c("grey50"),
  labels=rownames(cor)
  # label.scale=FALSE
  #gray=T,
  )
# dev.off()
#legend(0.8,-0.8, bty=".",c("Ensaio Clínico","Medicamentos","Outras Razões"),cex=1.2,fill=c("lightblue","red","yellow"))

#Directed Acyclic Graph / require package bnlearn
# dag_data <- data.frame(apply(sf8_data, 2, as.factor))
dag_data<-as.data.frame(dataMatrix)
res<-rsmax2(dag_data,
            restrict = "si.hiton.pc",
            maximize = "tabu")
res2<-(res$arcs)
qgraph(res2)


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
word_freq<-colSums(data)

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

# Q2_atleta2 <- qgraph(cor$correlations, borders = TRUE, cut=0.8, minimum = 0.4, labels=names,label.cex = 1, vsize=word_freq,label.color="black",layout = "spring",directed=FALSE,label.scale=FALSE,gray=FALSE,posCol=c("gray","gray"))

g<-as.igraph(Q2_atleta2)
h<-spinglass.community(g)
plot(h,g)
h$membership

# Q2_atleta2 <- qgraph(dataMatrix, borders = FALSE, cut=10, minimum = 3, labels=names,label.cex = 0.80, label.color="black",layout = "spring",directed=FALSE,label.scale=FALSE,gray=FALSE,posCol=c("gray","gray"))

# g<-as.igraph(Q2_atleta2)
# h<-spinglass.community(g)
# plot(h,g)
# h$membership

# #Calculating Community measures
# g<-as.igraph(network_glasso) #creating igraph object
# h<-walktrap.community(g) #creatin community object
# h<-spinglass.community(g, weights=NA) #creatin community object
# # h<-fastgreedy.community(g, weights=NA) #creatin community object
# # h<-edge.betweenness.community(g, weights=NA) #creatin community object
# h<-cluster_leading_eigen(g,weights=NA) #creatin community object
# plot(h,g) #plotting community network
# h$membership #extracting community membership for each node on the network
community<-data.frame(h$membership,
	rownames(cor))

#listing grouping variables in the network resulting from the community analysis
network_groups<-list(
Component1=as.numeric(rownames(community)
	[community[,1]==1]),
Component2=as.numeric(rownames(community)
	[community[,1]==2]),
Component3=as.numeric(rownames(community)
	[community[,1]==3])
# Component4=as.numeric(rownames(community)
# 	[community[,1]==4])
)

# # network_groups<-list(
# Component1=c(1,3,4,5,15,14),
# Component2=c(2,16,6,7),
# Component3=c(11,12,13,10),
# Component4=c(19,20,21,23),
# Component5=c(9,17,18,22,8)
# )

# creating vectors for labels
# node_labels<-c(
# "What is the area of the roadway?",
# "What type of roadway?",
# "Is this point at an intersection/junction?",
# "How many lanes in the roadway?",
# "Is there an auxiliary/other lane?",
# "How is the road surface conditions?",
# "Is there space on the side of the road 
# for any reason or use?",
# "Are there pedestrian pathways?",
# "Is there a Bus Stop?",
# "Is there a Speed bump?",
# "Is there a traffic light at this location?",
# "Are there road traffic signs at this hotspot?",
# "Is there a sign for speed limit of road?",
# "Road visibility is influenced by curves?",
# "Is the visibility influenced by 
# environmental factors?",
# "Are there bridges on the road?",
# "Is there a safe area for pedestrians 
# to cross the road?",
# "Is there a safe area for pedestrians
# to in the center of the road?",
# "Count the number of cars",
# "Count the number of moto",
# "Count the number of bike",
# "Count the number of pedestrians",
# "Count the number of bus/trucks"
# )

# creating nodes labels vector
# node_names<-c("Q1",
#               "Q2",
#               "Q3",
#               "Q4",
#               "Q5",
#               "Q6",
#               "Q7",
#               "Q8")

node_names<-rownames(cor)

# creating vector with mean values for each node
#mean_data<-sapply(network_data,mean)

#creating vector with mean values adjusted to proportional sizes to be plotted
#importance_vSize<-c(mean_data[1:14]/min(mean_data[1:14]),1.81)

#building network figures 
# 3 types are created to get an avarege position and layout
#GLASSO NETWORK
# network_glasso<-qgraph(cor_data,layout="spring",
#   vsize=6,esize=20,graph="glasso",
#   sampleSize=nrow(bea_data),
#   legend.cex = 0.5,GLratio=1.5)

# #PARTIAL CORRELATION NETWORK
# network_pcor<-qgraph(cor_data,layout="spring",
#   vsize=6,esize=20,graph="pcor",threshold="holm",
#   sampleSize=nrow(bea_data),
#   legend.cex = 0.5,GLratio=1.5)

# #CORRELATION NETWORK
# network_cor<-qgraph(cor_data,layout="spring",
#   vsize=6,esize=20,legend.cex = 0.5,GLratio=1.5)
# #layout1<-averageLayout(network_glasso,network_pcor,network_cor)

# # Organizing both figures to be with the same layout
# layout_final<-averageLayout(network_glasso,
#   network_pcor,
#   network_cor)

word_freq<-car::recode(word_freq,"24=15")

# postscript("/home/joao/Desktop/info_consent_figure2.eps",
#   width = 1500, height = 1200,horizontal = FALSE, 
#   onefile = FALSE)
# postscript("/Users/joaovissoci/Desktop/info_consent_figure2.eps",
#   width = 1500, height = 1200,horizontal = FALSE, 
#   onefile = FALSE)
# tiff("/Users/jnv4/Desktop/bea_pca_network.tiff", width = 1200,
 # height = 700,compression = 'lzw')
  network_glasso<-qgraph(cor,
  layout='spring',
  # esize=20,
  # graph="glasso",
  # sampleSize=nrow(sf8_data),
  legend.cex = 0.5,
  cut = 0.6,
  # maximum = 1, 
  minimum = 0.4,
  # esize = 20,
  vsize = word_freq, 
  # repulsion = 0.8,
  # nodeNames=
  # shape="square",
  borders=TRUE,
  # border.width=5,
  # groups=network_groups,
  # color=c("gold","steelblue","red"),#,"grey50"),
  labels=rownames(cor)
  #gray=T,
  )
# dev.off()
#legend(0.8,-0.8, bty=".",c("Ensaio Clínico","Medicamentos","Outras Razões"),cex=1.2,fill=c("lightblue","red","yellow"))

#Directed Acyclic Graph / require package bnlearn
# dag_data <- data.frame(apply(sf8_data, 2, as.factor))
dag_data<-as.data.frame(dataMatrix)
res<-rsmax2(dag_data,
            restrict = "si.hiton.pc",
            maximize = "tabu")
res2<-(res$arcs)
qgraph(res2)

#### THEME 4 ################################################
#fazendo a mineração de texto:
corpus = Corpus(VectorSource(dados_theme4$Text))
#plot(h,g)#corpus<- tm_map(corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
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
word_freq<-colSums(data)

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

# Q2_atleta2 <- qgraph(cor$correlations, borders = TRUE, cut=0.8, minimum = 0.4, labels=names,label.cex = 1, vsize=word_freq,label.color="black",layout = "spring",directed=FALSE,label.scale=FALSE,gray=FALSE,posCol=c("gray","gray"))

g<-as.igraph(Q2_atleta2)
h<-spinglass.community(g)
plot(h,g)
h$membership

# Q2_atleta2 <- qgraph(dataMatrix, borders = FALSE, cut=10, minimum = 3, labels=names,label.cex = 0.80, label.color="black",layout = "spring",directed=FALSE,label.scale=FALSE,gray=FALSE,posCol=c("gray","gray"))

# g<-as.igraph(Q2_atleta2)
# h<-spinglass.community(g)
# plot(h,g)
# h$membership

# #Calculating Community measures
# g<-as.igraph(network_glasso) #creating igraph object
# h<-walktrap.community(g) #creatin community object
# h<-spinglass.community(g, weights=NA) #creatin community object
# # h<-fastgreedy.community(g, weights=NA) #creatin community object
# # h<-edge.betweenness.community(g, weights=NA) #creatin community object
# h<-cluster_leading_eigen(g,weights=NA) #creatin community object
# plot(h,g) #plotting community network
# h$membership #extracting community membership for each node on the network
community<-data.frame(h$membership,
	rownames(cor))

#listing grouping variables in the network resulting from the community analysis
network_groups<-list(
Component1=as.numeric(rownames(community)
	[community[,1]==1]),
Component2=as.numeric(rownames(community)
	[community[,1]==2]),
Component3=as.numeric(rownames(community)
	[community[,1]==3])
# Component4=as.numeric(rownames(community)
# 	[community[,1]==4])
)

# # network_groups<-list(
# Component1=c(1,3,4,5,15,14),
# Component2=c(2,16,6,7),
# Component3=c(11,12,13,10),
# Component4=c(19,20,21,23),
# Component5=c(9,17,18,22,8)
# )

# creating vectors for labels
# node_labels<-c(
# "What is the area of the roadway?",
# "What type of roadway?",
# "Is this point at an intersection/junction?",
# "How many lanes in the roadway?",
# "Is there an auxiliary/other lane?",
# "How is the road surface conditions?",
# "Is there space on the side of the road 
# for any reason or use?",
# "Are there pedestrian pathways?",
# "Is there a Bus Stop?",
# "Is there a Speed bump?",
# "Is there a traffic light at this location?",
# "Are there road traffic signs at this hotspot?",
# "Is there a sign for speed limit of road?",
# "Road visibility is influenced by curves?",
# "Is the visibility influenced by 
# environmental factors?",
# "Are there bridges on the road?",
# "Is there a safe area for pedestrians 
# to cross the road?",
# "Is there a safe area for pedestrians
# to in the center of the road?",
# "Count the number of cars",
# "Count the number of moto",
# "Count the number of bike",
# "Count the number of pedestrians",
# "Count the number of bus/trucks"
# )

# creating nodes labels vector
# node_names<-c("Q1",
#               "Q2",
#               "Q3",
#               "Q4",
#               "Q5",
#               "Q6",
#               "Q7",
#               "Q8")

node_names<-rownames(cor)

# creating vector with mean values for each node
#mean_data<-sapply(network_data,mean)

#creating vector with mean values adjusted to proportional sizes to be plotted
#importance_vSize<-c(mean_data[1:14]/min(mean_data[1:14]),1.81)

#building network figures 
# 3 types are created to get an avarege position and layout
#GLASSO NETWORK
# network_glasso<-qgraph(cor_data,layout="spring",
#   vsize=6,esize=20,graph="glasso",
#   sampleSize=nrow(bea_data),
#   legend.cex = 0.5,GLratio=1.5)

# #PARTIAL CORRELATION NETWORK
# network_pcor<-qgraph(cor_data,layout="spring",
#   vsize=6,esize=20,graph="pcor",threshold="holm",
#   sampleSize=nrow(bea_data),
#   legend.cex = 0.5,GLratio=1.5)

# #CORRELATION NETWORK
# network_cor<-qgraph(cor_data,layout="spring",
#   vsize=6,esize=20,legend.cex = 0.5,GLratio=1.5)
# #layout1<-averageLayout(network_glasso,network_pcor,network_cor)

# # Organizing both figures to be with the same layout
# layout_final<-averageLayout(network_glasso,
#   network_pcor,
#   network_cor)

# word_freq<-car::recode(word_freq,"17=15")

# postscript("/home/joao/Desktop/info_consent_figure2.eps",
#   width = 1500, height = 1200,horizontal = FALSE, 
#   onefile = FALSE)
# postscript("/Users/joaovissoci/Desktop/info_consent_figure2.eps",
#   width = 1500, height = 1200,horizontal = FALSE, 
#   onefile = FALSE)
# tiff("/Users/jnv4/Desktop/bea_pca_network.tiff", width = 1200,
 # height = 700,compression = 'lzw')
  network_glasso<-qgraph(cor,
  layout='spring',
  # esize=20,
  # graph="glasso",
  # sampleSize=nrow(sf8_data),
  legend.cex = 0.5,
  cut = 0.6,
  # maximum = 1, 
  minimum = 0.4,
  # esize = 20,
  vsize = word_freq/3, 
  # repulsion = 0.8,
  # nodeNames=
  # shape="square",
  borders=TRUE,
  # border.width=5,
  # groups=network_groups,
  # color=c("gold","steelblue","red"),#,"grey50"),
  labels=rownames(cor)
  #gray=T,
  )
# dev.off()
#legend(0.8,-0.8, bty=".",c("Ensaio Clínico","Medicamentos","Outras Razões"),cex=1.2,fill=c("lightblue","red","yellow"))

#Directed Acyclic Graph / require package bnlearn
# dag_data <- data.frame(apply(sf8_data, 2, as.factor))
dag_data<-as.data.frame(dataMatrix)
res<-rsmax2(dag_data,
            restrict = "si.hiton.pc",
            maximize = "tabu")
res2<-(res$arcs)
qgraph(res2)

#### THEME 5 ################################################
#fazendo a mineração de texto:
corpus = Corpus(VectorSource(dados_theme5$Text))
#plot(h,g)#corpus<- tm_map(corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
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
word_freq<-colSums(data)

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

# Q2_atleta2 <- qgraph(cor$correlations, borders = TRUE, cut=0.8, minimum = 0.4, labels=names,label.cex = 1, vsize=word_freq,label.color="black",layout = "spring",directed=FALSE,label.scale=FALSE,gray=FALSE,posCol=c("gray","gray"))

g<-as.igraph(Q2_atleta2)
h<-spinglass.community(g)
plot(h,g)
h$membership

# Q2_atleta2 <- qgraph(dataMatrix, borders = FALSE, cut=10, minimum = 3, labels=names,label.cex = 0.80, label.color="black",layout = "spring",directed=FALSE,label.scale=FALSE,gray=FALSE,posCol=c("gray","gray"))

# g<-as.igraph(Q2_atleta2)
# h<-spinglass.community(g)
# plot(h,g)
# h$membership

# #Calculating Community measures
# g<-as.igraph(network_glasso) #creating igraph object
# h<-walktrap.community(g) #creatin community object
# h<-spinglass.community(g, weights=NA) #creatin community object
# # h<-fastgreedy.community(g, weights=NA) #creatin community object
# # h<-edge.betweenness.community(g, weights=NA) #creatin community object
# h<-cluster_leading_eigen(g,weights=NA) #creatin community object
# plot(h,g) #plotting community network
# h$membership #extracting community membership for each node on the network
community<-data.frame(h$membership,
	rownames(cor))

#listing grouping variables in the network resulting from the community analysis
network_groups<-list(
Component1=as.numeric(rownames(community)
	[community[,1]==1]),
Component2=as.numeric(rownames(community)
	[community[,1]==2]),
Component3=as.numeric(rownames(community)
	[community[,1]==3])
# Component4=as.numeric(rownames(community)
# 	[community[,1]==4])
)

# # network_groups<-list(
# Component1=c(1,3,4,5,15,14),
# Component2=c(2,16,6,7),
# Component3=c(11,12,13,10),
# Component4=c(19,20,21,23),
# Component5=c(9,17,18,22,8)
# )

# creating vectors for labels
# node_labels<-c(
# "What is the area of the roadway?",
# "What type of roadway?",
# "Is this point at an intersection/junction?",
# "How many lanes in the roadway?",
# "Is there an auxiliary/other lane?",
# "How is the road surface conditions?",
# "Is there space on the side of the road 
# for any reason or use?",
# "Are there pedestrian pathways?",
# "Is there a Bus Stop?",
# "Is there a Speed bump?",
# "Is there a traffic light at this location?",
# "Are there road traffic signs at this hotspot?",
# "Is there a sign for speed limit of road?",
# "Road visibility is influenced by curves?",
# "Is the visibility influenced by 
# environmental factors?",
# "Are there bridges on the road?",
# "Is there a safe area for pedestrians 
# to cross the road?",
# "Is there a safe area for pedestrians
# to in the center of the road?",
# "Count the number of cars",
# "Count the number of moto",
# "Count the number of bike",
# "Count the number of pedestrians",
# "Count the number of bus/trucks"
# )

# creating nodes labels vector
# node_names<-c("Q1",
#               "Q2",
#               "Q3",
#               "Q4",
#               "Q5",
#               "Q6",
#               "Q7",
#               "Q8")

node_names<-rownames(cor)

# creating vector with mean values for each node
#mean_data<-sapply(network_data,mean)

#creating vector with mean values adjusted to proportional sizes to be plotted
#importance_vSize<-c(mean_data[1:14]/min(mean_data[1:14]),1.81)

#building network figures 
# 3 types are created to get an avarege position and layout
#GLASSO NETWORK
# network_glasso<-qgraph(cor_data,layout="spring",
#   vsize=6,esize=20,graph="glasso",
#   sampleSize=nrow(bea_data),
#   legend.cex = 0.5,GLratio=1.5)

# #PARTIAL CORRELATION NETWORK
# network_pcor<-qgraph(cor_data,layout="spring",
#   vsize=6,esize=20,graph="pcor",threshold="holm",
#   sampleSize=nrow(bea_data),
#   legend.cex = 0.5,GLratio=1.5)

# #CORRELATION NETWORK
# network_cor<-qgraph(cor_data,layout="spring",
#   vsize=6,esize=20,legend.cex = 0.5,GLratio=1.5)
# #layout1<-averageLayout(network_glasso,network_pcor,network_cor)

# # Organizing both figures to be with the same layout
# layout_final<-averageLayout(network_glasso,
#   network_pcor,
#   network_cor)

word_freq<-car::recode(word_freq,"15=8")

# postscript("/home/joao/Desktop/info_consent_figure2.eps",
#   width = 1500, height = 1200,horizontal = FALSE, 
#   onefile = FALSE)
# postscript("/Users/joaovissoci/Desktop/info_consent_figure2.eps",
#   width = 1500, height = 1200,horizontal = FALSE, 
#   onefile = FALSE)
# tiff("/Users/jnv4/Desktop/bea_pca_network.tiff", width = 1200,
 # height = 700,compression = 'lzw')
  network_glasso<-qgraph(cor,
  layout='spring',
  # esize=20,
  # graph="glasso",
  # sampleSize=nrow(sf8_data),
  legend.cex = 0.5,
  cut = 0.6,
  # maximum = 1, 
  minimum = 0.4,
  # esize = 20,
  vsize = word_freq*2, 
  # repulsion = 0.8,
  # nodeNames=
  # shape="square",
  borders=TRUE,
  # border.width=5,
  # groups=network_groups,
  # color=c("gold","steelblue","red"),#,"grey50"),
  labels=rownames(cor)
  #gray=T,
  )
# dev.off()
#legend(0.8,-0.8, bty=".",c("Ensaio Clínico","Medicamentos","Outras Razões"),cex=1.2,fill=c("lightblue","red","yellow"))

#Directed Acyclic Graph / require package bnlearn
# dag_data <- data.frame(apply(sf8_data, 2, as.factor))
dag_data<-as.data.frame(dataMatrix)
res<-rsmax2(dag_data,
            restrict = "si.hiton.pc",
            maximize = "tabu")
res2<-(res$arcs)
qgraph(res2)

#### THEME 6 ################################################
#fazendo a mineração de texto:
corpus = Corpus(VectorSource(dados_theme6$Text))
#plot(h,g)#corpus<- tm_map(corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
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
word_freq<-colSums(data)

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

# Q2_atleta2 <- qgraph(cor$correlations, borders = TRUE, cut=0.8, minimum = 0.4, labels=names,label.cex = 1, vsize=word_freq,label.color="black",layout = "spring",directed=FALSE,label.scale=FALSE,gray=FALSE,posCol=c("gray","gray"))

g<-as.igraph(Q2_atleta2)
h<-spinglass.community(g)
plot(h,g)
h$membership

# Q2_atleta2 <- qgraph(dataMatrix, borders = FALSE, cut=10, minimum = 3, labels=names,label.cex = 0.80, label.color="black",layout = "spring",directed=FALSE,label.scale=FALSE,gray=FALSE,posCol=c("gray","gray"))

# g<-as.igraph(Q2_atleta2)
# h<-spinglass.community(g)
# plot(h,g)
# h$membership

# #Calculating Community measures
# g<-as.igraph(network_glasso) #creating igraph object
# h<-walktrap.community(g) #creatin community object
# h<-spinglass.community(g, weights=NA) #creatin community object
# # h<-fastgreedy.community(g, weights=NA) #creatin community object
# # h<-edge.betweenness.community(g, weights=NA) #creatin community object
# h<-cluster_leading_eigen(g,weights=NA) #creatin community object
# plot(h,g) #plotting community network
# h$membership #extracting community membership for each node on the network
community<-data.frame(h$membership,
	rownames(cor))

#listing grouping variables in the network resulting from the community analysis
network_groups<-list(
Component1=as.numeric(rownames(community)
	[community[,1]==1]),
Component2=as.numeric(rownames(community)
	[community[,1]==2]),
Component3=as.numeric(rownames(community)
	[community[,1]==3])
# Component4=as.numeric(rownames(community)
# 	[community[,1]==4])
)

# # network_groups<-list(
# Component1=c(1,3,4,5,15,14),
# Component2=c(2,16,6,7),
# Component3=c(11,12,13,10),
# Component4=c(19,20,21,23),
# Component5=c(9,17,18,22,8)
# )

# creating vectors for labels
# node_labels<-c(
# "What is the area of the roadway?",
# "What type of roadway?",
# "Is this point at an intersection/junction?",
# "How many lanes in the roadway?",
# "Is there an auxiliary/other lane?",
# "How is the road surface conditions?",
# "Is there space on the side of the road 
# for any reason or use?",
# "Are there pedestrian pathways?",
# "Is there a Bus Stop?",
# "Is there a Speed bump?",
# "Is there a traffic light at this location?",
# "Are there road traffic signs at this hotspot?",
# "Is there a sign for speed limit of road?",
# "Road visibility is influenced by curves?",
# "Is the visibility influenced by 
# environmental factors?",
# "Are there bridges on the road?",
# "Is there a safe area for pedestrians 
# to cross the road?",
# "Is there a safe area for pedestrians
# to in the center of the road?",
# "Count the number of cars",
# "Count the number of moto",
# "Count the number of bike",
# "Count the number of pedestrians",
# "Count the number of bus/trucks"
# )

# creating nodes labels vector
# node_names<-c("Q1",
#               "Q2",
#               "Q3",
#               "Q4",
#               "Q5",
#               "Q6",
#               "Q7",
#               "Q8")

node_names<-rownames(cor)

# creating vector with mean values for each node
#mean_data<-sapply(network_data,mean)

#creating vector with mean values adjusted to proportional sizes to be plotted
#importance_vSize<-c(mean_data[1:14]/min(mean_data[1:14]),1.81)

#building network figures 
# 3 types are created to get an avarege position and layout
#GLASSO NETWORK
# network_glasso<-qgraph(cor_data,layout="spring",
#   vsize=6,esize=20,graph="glasso",
#   sampleSize=nrow(bea_data),
#   legend.cex = 0.5,GLratio=1.5)

# #PARTIAL CORRELATION NETWORK
# network_pcor<-qgraph(cor_data,layout="spring",
#   vsize=6,esize=20,graph="pcor",threshold="holm",
#   sampleSize=nrow(bea_data),
#   legend.cex = 0.5,GLratio=1.5)

# #CORRELATION NETWORK
# network_cor<-qgraph(cor_data,layout="spring",
#   vsize=6,esize=20,legend.cex = 0.5,GLratio=1.5)
# #layout1<-averageLayout(network_glasso,network_pcor,network_cor)

# # Organizing both figures to be with the same layout
# layout_final<-averageLayout(network_glasso,
#   network_pcor,
#   network_cor)

word_freq<-car::recode(word_freq,"13=8")

# postscript("/home/joao/Desktop/info_consent_figure2.eps",
#   width = 1500, height = 1200,horizontal = FALSE, 
#   onefile = FALSE)
# postscript("/Users/joaovissoci/Desktop/info_consent_figure2.eps",
#   width = 1500, height = 1200,horizontal = FALSE, 
#   onefile = FALSE)
# tiff("/Users/jnv4/Desktop/bea_pca_network.tiff", width = 1200,
 # height = 700,compression = 'lzw')
  network_glasso<-qgraph(cor,
  layout='spring',
  # esize=20,
  # graph="glasso",
  # sampleSize=nrow(sf8_data),
  legend.cex = 0.5,
  cut = 0.6,
  # maximum = 1, 
  minimum = 0.4,
  # esize = 20,
  vsize = word_freq*2, 
  # repulsion = 0.8,
  # nodeNames=
  # shape="square",
  borders=TRUE,
  # border.width=5,
  # groups=network_groups,
  # color=c("gold","steelblue","red"),#,"grey50"),
  labels=rownames(cor)
  #gray=T,
  )
# dev.off()
#legend(0.8,-0.8, bty=".",c("Ensaio Clínico","Medicamentos","Outras Razões"),cex=1.2,fill=c("lightblue","red","yellow"))

#Directed Acyclic Graph / require package bnlearn
# dag_data <- data.frame(apply(sf8_data, 2, as.factor))
dag_data<-as.data.frame(dataMatrix)
res<-rsmax2(dag_data,
            restrict = "si.hiton.pc",
            maximize = "tabu")
res2<-(res$arcs)
qgraph(res2)



# ##############################################################
# #CALCULATING DESCRIPTIVES FROM THE NETWORK
# ##############################################################

# #Transforming network into a IGRAPH object (created with the igraph package)
# Q3<-as.igraph(qgraph(Q1_atleta1, DoNotPlot = TRUE))

# #Calculating shortest path - association metric
# #ADD A DEFINITION
# shortest.paths(Q3)

# #Calculating degree
# #ADD A DEFINITION
# degree(Q3)

# #Calculating centrality
# #ADD A DEFINITION
# assortativity.degree(Q3)

# #Calculating closeness
# #ADD A DEFINITION
# closeness(Q3)

# #Calculating betweeness
# #ADD A DEFINITION
# betweenness(Q3)

# #Calculating Eigenvalue centrality
# #ADD A DEFINITION
# evcent(Q3)

# #Calculating Network density
# #ADD A DEFINITION
# graph.density(Q3)

# #Calculating Network diameter
# #ADD A DEFINITION
# diameter(Q3)

# ### Clustering
# #The local clustering coefficient, cl(v), gives for node n the proportion that the neighboors of v are also connected to each other.
# #Maximum value is 1, value closer to 1 means higher clustering
# transitivity(Q3, "local")
# transitivity(Q3, "global")
# transitivity(Q3, "average")

# ### Checking for Small World configuration of your network

# #Function to calculate avareg length path in random graphs
# #Just run every line without changing anything
# APLr <- function(x) {
# if ("qgraph" %in% class(x))
# x <- as.igraph(x)
# if ("igraph" %in% class(x))
# x <- get.adjacency(x) 
# N = nrow(x)
# p = sum(x/2)/sum(lower.tri(x))
# eulers_constant <- 0.57721566490153
# l = (log(N) - eulers_constant)/log(p * (N - 1)) + 0.5 
# l
# }

# #Run function
# APLr(Q3)

# #Function to calculate clustering in random graphs
# #Just run every line without changing anything
# Cr <- function(x) {
# if ("qgraph" %in% class(x))
# x <- as.igraph(x)
# if ("igraph" %in% class(x))
# x <- get.adjacency(x) 
# N = nrow(x)
# p = sum(x/2)/sum(lower.tri(x)) 
# t = (p * (N - 1)/N)
# t
# }

# #Run function
# Cr(Q3)

# #Valiues above 3 means your network is a small world configured network
# (transitivity(Q3) / Cr(Q3)) / (average.path.length(Q3) / APLr(Q3))

# #Calculating all descriptives from a qgraph object (might take a while)
# #ADD A DEFINITION
# x<-centrality(Q1_atleta1) #Returns all descriptives

