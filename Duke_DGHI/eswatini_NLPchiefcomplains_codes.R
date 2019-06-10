install.packages("tm" )
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("ggplot2")
install.packages("qgraph")
install.packages("lsa")
install.packages("stringr")
install.packages("mclust")

#Duvidas: 
#documentos vazios e 'line abuse' pode afetar os resultados? 
# Como remover palavras pouco ou muito frequentes (proporcionalmente).

library(tm)
library(SnowballC)
library(wordcloud)
library(ggplot2)
library(qgraph)
library(lsa)
library(stringr)
library(mclust)



#Carregando os dados com os textos originais
#setwd('F:/WAGNER 14.03')
text <- readLines("C:/Users/rapiduser/Downloads/Correct_Calls2017_Complete.csv")
text <- text[-(1)] #removendo a primeira linha 'history'
View(text)

#removendo casos de 'line abuse' e 'null'
text <- Filter(function (x) str_detect(x, "line abuse", negate=TRUE), text)
text <- Filter(function (x) str_detect(x, "null", negate=TRUE), text)
View(text)

#Corpus
df <- data.frame(text, stringsAsFactors=FALSE)
#View(df)
corpus <- Corpus(VectorSource(df$text))
corpus

#Limpando os dados

# apenas minusculas
corpus <- tm_map(corpus, tolower)
# removendo pontua??es
corpus <- tm_map(corpus, removePunctuation)
# removendo "stopwords"
corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("english")))
# stemiza??o (manter a raiz)
corpus <- tm_map(corpus, stemDocument, language = "english")
#removendo n?meros
corpus <- tm_map(corpus, removeNumbers)
#remover espa?o em branco entre as palavras 
corpus <- tm_map(corpus, stripWhitespace)

# verificando o corpus
writeLines(as.character(corpus[[75]]))


# gerando matrizes de TD e DT
tdm <- TermDocumentMatrix(corpus)
dtm <- DocumentTermMatrix(corpus)

# freq de palavras
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
head(freq, 15)

wf <- data.frame(word=names(freq), freq=freq)   
head(wf)  

p <- ggplot(subset(wf, freq>50), aes(x = reorder(word, -freq), y = freq)) +
            geom_bar(stat = "identity") +
            theme(axis.text.x=element_text(angle=45, hjust=1))
p   

set.seed(142)   
wordcloud(names(freq), freq, min.freq=20) 


# Escalonamento Multidimensional com LSA
td.mat <- as.matrix(tdm)
# Ponderando os termos em peso local (lw) e peso global (gw)
td.mat.lsa <- lw_bintf(td.mat) * gw_idf(td.mat) 
#View(td.mat.lsa)
# criando o espa?o latente (M = T S t(D); a matriz M ? o produto das matrizes de termos "T", documentos "D" e a diagonal "S"
# de valores singulares
lsaSpace <- lsa(td.mat.lsa)
lsaSpace$dk
#View(lsaSpace)
# calculando as dist?ncias
dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace))) 
dist.mat.lsa

# Escalonamento Multidimensional com LSA, duas dimens?es (k=2)
fit <- cmdscale(dist.mat.lsa, eig=TRUE, k=2)
# gerando o gr?fico
points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])
ggplot(points,aes(x=x, y=y)) + 
  geom_point(data=points,aes(x=x, y=y)) + 
  geom_text(data=points,aes(x=x, y=y-0.2, label=row.names(df)))


#Gerando classificacoes com mclust

#Model Based Clustering


#Plot das classifica??es com base em Escalonamento Multidimensional com LSA, duas dimens?es (k=2)
#fit2 <- Mclust(points)
#plot(points[,1],points[,2],col= fit2$classification)

#numero de classifica??es
#summary(fit2)
#class <- fit2$classification

library(skmeans)

#Cluster analysis
set.seed(123456)
clust<-kmeans(points,20) #dividir estes dados em 3 clusters 
clust
plot(points[,1],points[,2],col= clust$cluster)
text(points[,1],points[,2],row.names(df))
# rect.hclust(clust, k=5, border="red")

# fit2 <- Mclust(points)

#Plot das classifica??es com base em Escalonamento Multidimensional com LSA, duas dimens?es (k=2)
# plot(points[,1],points[,2],col= fit2$classification)

# #numero de classifica??es
print(clust)

class <- clust$cluster

#Tabela final com as classifica??es 
table_final = data.frame(originalText = text,
                         # PointX  = fit$points[,1],
                         # PointY  = fit$points[,2],
                         class =  clust$cluster)

#write.csv2(table_final,"table_final.csv",row.names = F)

#associações 
findAssocs(dtm,"pain",corlimit = 0.3)
#dtm2<-removeSparseTerms(dtm,0.99)
cor_t<- cor(as.matrix(dtm),method = "spearman")
#View(cor_t)
qgraph(cor_t,layout="spring",labels=colnames(cor_t),threshold=0.3)

################################################################################
#By Cluster Analysis - Cluster 1 Instruments
################################################################################
# Análise por cluster
#Corpus
df_c1 <- data.frame(table_final$originalText[which(table_final$class==1)], stringsAsFactors=FALSE)
# View(df_c1)
corpus_c1 <- VCorpus(VectorSource(df_c1$table_final.originalText.which.table_final.class....1..))

# apenas minusculas
corpus_c1 <- tm_map(corpus_c1, tolower)
# removendo pontua??es
corpus_c1 <- tm_map(corpus_c1, removePunctuation)
# removendo "stopwords"
corpus_c1 <- tm_map(corpus_c1, function(x) removeWords(x, stopwords("english")))
# stemiza??o (manter a raiz)
corpus_c1 <- tm_map(corpus_c1, stemDocument, language = "english")
#removendo n?meros
corpus_c1 <- tm_map(corpus_c1, removeNumbers)
#remover espa?o em branco entre as palavras 
corpus_c1 <- tm_map(corpus_c1, stripWhitespace)

#criando uma matrix de palavras e frequencia de palavras
dtm_c1 <- DocumentTermMatrix(corpus_c1)

# descritivos por cluster
freq_c1 <- sort(colSums(as.matrix(dtm_c1)), decreasing=TRUE)   
head(freq_c1, 20)

wf_c1 <- data.frame(word=names(freq_c1), freq=freq_c1)   
# head(wf_c1)  

p_c1 <- ggplot(subset(wf_c1, freq>3), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_freqs.eps",
#           width = 8, height = 8)
p_c1  
#dev.off()

# set.seed(142)   
# wordcloud(names(freq_c1), freq_c1, min.freq=20)

dtm_c1_2<-removeSparseTerms(dtm_c1,0.90)
cor_c1 <- cor(as.matrix(dtm_c1_2),method = "spearman")
cor_c1 <- ifelse(cor_c1<0,0,cor_c1)
colnames(cor_c1)

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_network.eps",
#           width = 8, height = 8)
rede1<-qgraph(cor_c1,
                  layout="spring",
                  labels=colnames(cor_c1),
                  threshold=0.1,
                  vsize=5,
                  label.scale=FALSE,
                  # grey=T,
                  color="lightblue",
                  borders = FALSE,
                  posCol = "grey",
                  label.cex=1.2)

#dev.off()

################################################################################
#By Cluster Analysis - Cluster 2 Instruments
################################################################################
# Análise por cluster
#Corpus
df_c2 <- data.frame(table_final$originalText[which(table_final$class==2)], stringsAsFactors=FALSE)
# View(df_c2)
corpus_c2 <- VCorpus(VectorSource(df_c2$table_final.originalText.which.table_final.class....2..))

# apenas minusculas
corpusc2 <- tm_map(corpusc2, tolower)
# removendo pontua??es
corpusc2 <- tm_map(corpusc2, removePunctuation)
# removendo "stopwords"
corpusc2 <- tm_map(corpusc2, function(x) removeWords(x, stopwords("english")))
# stemiza??o (manter a raiz)
corpusc2 <- tm_map(corpusc2, stemDocument, language = "english")
#removendo n?meros
corpusc2 <- tm_map(corpusc2, removeNumbers)
#remover espa?o em branco entre as palavras 
corpusc2 <- tm_map(corpusc2, stripWhitespace)

#criando uma matrix de palavras e frequencia de palavras
dtm_c2 <- DocumentTermMatrix(corpus_c2)

# descritivos por cluster
freq_c2 <- sort(colSums(as.matrix(dtm_c2)), decreasing=TRUE)   
head(freq_c2, 20)

wf_c2 <- data.frame(word=names(freq_c2), freq=freq_c2)   
# head(wf_c2)  

p_c2 <- ggplot(subset(wf_c2, freq>3), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_freqs.eps",
#           width = 8, height = 8)
p_c2  
#dev.off()

# set.seed(142)   
# wordcloud(names(freq_c2), freq_c2, min.freq=20)

dtm_c2_2<-removeSparseTerms(dtm_c2,0.90)
cor_c2 <- cor(as.matrix(dtm_c2_2),method = "spearman")
cor_c2 <- ifelse(cor_c2<0,0,cor_c2)
colnames(cor_c2)

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_network.eps",
#           width = 8, height = 8)
rede2<-qgraph(cor_c2,
                  layout="spring",
                  labels=colnames(cor_c2),
                  threshold=0.1,
                  vsize=5,
                  label.scale=FALSE,
                  # grey=T,
                  color="lightblue",
                  borders = FALSE,
                  posCol = "grey",
                  label.cex=1.2)

#dev.off()

################################################################################
#By Cluster Analysis - Cluster 3 Instruments
################################################################################
# Análise por cluster
#Corpus
df_c3 <- data.frame(table_final$originalText[which(table_final$class==3)], stringsAsFactors=FALSE)
# View(df_c3)
corpus_c3 <- VCorpus(VectorSource(df_c3$table_final.originalText.which.table_final.class....3..))

# apenas minusculas
corpus_c3 <- tm_map(corpus_c3, tolower)
# removendo pontua??es
corpus_c3 <- tm_map(corpus_c3, removePunctuation)
# removendo "stopwords"
corpus_c3 <- tm_map(corpus_c3, function(x) removeWords(x, stopwords("english")))
# stemiza??o (manter a raiz)
corpus_c3 <- tm_map(corpus_c3, stemDocument, language = "english")
#removendo n?meros
corpus_c3 <- tm_map(corpus_c3, removeNumbers)
#remover espa?o em branco entre as palavras 
corpus_c3 <- tm_map(corpus_c3, stripWhitespace)

#criando uma matrix de palavras e frequencia de palavras
dtm_c3 <- DocumentTermMatrix(corpus_c3)

# descritivos por cluster
freq_c3 <- sort(colSums(as.matrix(dtm_c3)), decreasing=TRUE)   
head(freq_c3, 20)

wf_c3 <- data.frame(word=names(freq_c3), freq=freq_c3)   
# head(wf_c3)  

p_c3 <- ggplot(subset(wf_c3, freq>3), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_freqs.eps",
#           width = 8, height = 8)
p_c3  
#dev.off()

# set.seed(142)   
# wordcloud(names(freq_c3), freq_c3, min.freq=20)

dtm_c3_2<-removeSparseTerms(dtm_c3,0.90)
cor_c3 <- cor(as.matrix(dtm_c3_2),method = "spearman")
cor_c3 <- ifelse(cor_c3<0,0,cor_c3)
colnames(cor_c3)

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_network.eps",
#           width = 8, height = 8)
rede3<-qgraph(cor_c3,
                  layout="spring",
                  labels=colnames(cor_c3),
                  threshold=0.1,
                  vsize=5,
                  label.scale=FALSE,
                  # grey=T,
                  color="lightblue",
                  borders = FALSE,
                  posCol = "grey",
                  label.cex=1.2)

#dev.off()################################################################################
#By Cluster Analysis - Cluster 4 Instruments
################################################################################
# Análise por cluster
#Corpus
df_c4 <- data.frame(table_final$originalText[which(table_final$class==4)], stringsAsFactors=FALSE)
# View(df_c4)
corpus_c4 <- VCorpus(VectorSource(df_c4$table_final.originalText.which.table_final.class....4..))

# apenas minusculas
corpus_c4 <- tm_map(corpus_c4, tolower)
# removendo pontua??es
corpus_c4 <- tm_map(corpus_c4, removePunctuation)
# removendo "stopwords"
corpus_c4 <- tm_map(corpus_c4, function(x) removeWords(x, stopwords("english")))
# stemiza??o (manter a raiz)
corpus_c4 <- tm_map(corpus_c4, stemDocument, language = "english")
#removendo n?meros
corpus_c4 <- tm_map(corpus_c4, removeNumbers)
#remover espa?o em branco entre as palavras 
corpus_c4 <- tm_map(corpus_c4, stripWhitespace)

#criando uma matrix de palavras e frequencia de palavras
dtm_c4 <- DocumentTermMatrix(corpus_c4)

# descritivos por cluster
freq_c4 <- sort(colSums(as.matrix(dtm_c4)), decreasing=TRUE)   
head(freq_c4, 20)

wf_c4 <- data.frame(word=names(freq_c4), freq=freq_c4)   
# head(wf_c4)  

p_c4 <- ggplot(subset(wf_c4, freq>3), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_freqs.eps",
#           width = 8, height = 8)
p_c4  
#dev.off()

# set.seed(142)   
# wordcloud(names(freq_c4), freq_c4, min.freq=20)

dtm_c4_2<-removeSparseTerms(dtm_c4,0.90)
cor_c4 <- cor(as.matrix(dtm_c4_2),method = "spearman")
cor_c4 <- ifelse(cor_c4<0,0,cor_c4)
colnames(cor_c4)

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_network.eps",
#           width = 8, height = 8)
rede4<-qgraph(cor_c4,
                  layout="spring",
                  labels=colnames(cor_c4),
                  threshold=0.1,
                  vsize=5,
                  label.scale=FALSE,
                  # grey=T,
                  color="lightblue",
                  borders = FALSE,
                  posCol = "grey",
                  label.cex=1.2)

#dev.off()

################################################################################
#By Cluster Analysis - Cluster 5 Instruments
################################################################################
# Análise por cluster
#Corpus
df_c5 <- data.frame(table_final$originalText[which(table_final$class==5)], stringsAsFactors=FALSE)
# View(df_c5)
corpus_c5 <- VCorpus(VectorSource(df_c5$table_final.originalText.which.table_final.class....5..))

# apenas minusculas
corpus_c5 <- tm_map(corpus_c5, tolower)
# removendo pontua??es
corpus_c5 <- tm_map(corpus_c5, removePunctuation)
# removendo "stopwords"
corpus_c5 <- tm_map(corpus_c5, function(x) removeWords(x, stopwords("english")))
# stemiza??o (manter a raiz)
corpus_c5 <- tm_map(corpus_c5, stemDocument, language = "english")
#removendo n?meros
corpus_c5 <- tm_map(corpus_c5, removeNumbers)
#remover espa?o em branco entre as palavras 
corpus_c5 <- tm_map(corpus_c5, stripWhitespace)

#criando uma matrix de palavras e frequencia de palavras
dtm_c5 <- DocumentTermMatrix(corpus_c5)

# descritivos por cluster
freq_c5 <- sort(colSums(as.matrix(dtm_c5)), decreasing=TRUE)   
head(freq_c5, 20)

wf_c5 <- data.frame(word=names(freq_c5), freq=freq_c5)   
# head(wf_c5)  

p_c5 <- ggplot(subset(wf_c5, freq>3), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_freqs.eps",
#           width = 8, height = 8)
p_c5  
#dev.off()

# set.seed(142)   
# wordcloud(names(freq_c5), freq_c5, min.freq=20)

dtm_c5_2<-removeSparseTerms(dtm_c5,0.90)
cor_c5 <- cor(as.matrix(dtm_c5_2),method = "spearman")
cor_c5 <- ifelse(cor_c5<0,0,cor_c5)
colnames(cor_c5)

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_network.eps",
#           width = 8, height = 8)
rede5<-qgraph(cor_c5,
                  layout="spring",
                  labels=colnames(cor_c5),
                  threshold=0.1,
                  vsize=5,
                  label.scale=FALSE,
                  # grey=T,
                  color="lightblue",
                  borders = FALSE,
                  posCol = "grey",
                  label.cex=1.2)

#dev.off()################################################################################
#By Cluster Analysis - Cluster 6 Instruments
################################################################################
# Análise por cluster
#Corpus
df_c6 <- data.frame(table_final$originalText[which(table_final$class==6)], stringsAsFactors=FALSE)
# View(df_c6)
corpus_c6 <- VCorpus(VectorSource(df_c6$table_final.originalText.which.table_final.class....6..))

# apenas minusculas
corpus_c6 <- tm_map(corpus_c6, tolower)
# removendo pontua??es
corpus_c6 <- tm_map(corpus_c6, removePunctuation)
# removendo "stopwords"
corpus_c6 <- tm_map(corpus_c6, function(x) removeWords(x, stopwords("english")))
# stemiza??o (manter a raiz)
corpus_c6 <- tm_map(corpus_c6, stemDocument, language = "english")
#removendo n?meros
corpus_c6 <- tm_map(corpus_c6, removeNumbers)
#remover espa?o em branco entre as palavras 
corpus_c6 <- tm_map(corpus_c6, stripWhitespace)

#criando uma matrix de palavras e frequencia de palavras
dtm_c6 <- DocumentTermMatrix(corpus_c6)

# descritivos por cluster
freq_c6 <- sort(colSums(as.matrix(dtm_c6)), decreasing=TRUE)   
head(freq_c6, 20)

wf_c6 <- data.frame(word=names(freq_c6), freq=freq_c6)   
# head(wf_c6)  

p_c6 <- ggplot(subset(wf_c6, freq>3), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_freqs.eps",
#           width = 8, height = 8)
p_c6  
#dev.off()

# set.seed(142)   
# wordcloud(names(freq_c6), freq_c6, min.freq=20)

dtm_c6_2<-removeSparseTerms(dtm_c6,0.90)
cor_c6 <- cor(as.matrix(dtm_c6_2),method = "spearman")
cor_c6 <- ifelse(cor_c6<0,0,cor_c6)
colnames(cor_c6)

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_network.eps",
#           width = 8, height = 8)
rede6<-qgraph(cor_c6,
                  layout="spring",
                  labels=colnames(cor_c6),
                  threshold=0.1,
                  vsize=5,
                  label.scale=FALSE,
                  # grey=T,
                  color="lightblue",
                  borders = FALSE,
                  posCol = "grey",
                  label.cex=1.2)

#dev.off()################################################################################
#By Cluster Analysis - Cluster 7 Instruments
################################################################################
# Análise por cluster
#Corpus
df_c7 <- data.frame(table_final$originalText[which(table_final$class==7)], stringsAsFactors=FALSE)
# View(df_c7)
corpus_c7 <- VCorpus(VectorSource(df_c7$table_final.originalText.which.table_final.class....7..))

# apenas minusculas
corpus_c7 <- tm_map(corpus_c7, tolower)
# removendo pontua??es
corpus_c7 <- tm_map(corpus_c7, removePunctuation)
# removendo "stopwords"
corpus_c7 <- tm_map(corpus_c7, function(x) removeWords(x, stopwords("english")))
# stemiza??o (manter a raiz)
corpus_c7 <- tm_map(corpus_c7, stemDocument, language = "english")
#removendo n?meros
corpus_c7 <- tm_map(corpus_c7, removeNumbers)
#remover espa?o em branco entre as palavras 
corpus_c7 <- tm_map(corpus_c7, stripWhitespace)

#criando uma matrix de palavras e frequencia de palavras
dtm_c7 <- DocumentTermMatrix(corpus_c7)

# descritivos por cluster
freq_c7 <- sort(colSums(as.matrix(dtm_c7)), decreasing=TRUE)   
head(freq_c7, 20)

wf_c7 <- data.frame(word=names(freq_c7), freq=freq_c7)   
# head(wf_c7)  

p_c7 <- ggplot(subset(wf_c7, freq>3), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_freqs.eps",
#           width = 8, height = 8)
p_c7  
#dev.off()

# set.seed(142)   
# wordcloud(names(freq_c7), freq_c7, min.freq=20)

dtm_c7_2<-removeSparseTerms(dtm_c7,0.90)
cor_c7 <- cor(as.matrix(dtm_c7_2),method = "spearman")
cor_c7 <- ifelse(cor_c7<0,0,cor_c7)
colnames(cor_c7)

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_network.eps",
#           width = 8, height = 8)
rede7<-qgraph(cor_c7,
                  layout="spring",
                  labels=colnames(cor_c7),
                  threshold=0.1,
                  vsize=5,
                  label.scale=FALSE,
                  # grey=T,
                  color="lightblue",
                  borders = FALSE,
                  posCol = "grey",
                  label.cex=1.2)

#dev.off()################################################################################
#By Cluster Analysis - Cluster 8 Instruments
################################################################################
# Análise por cluster
#Corpus
df_c8 <- data.frame(table_final$originalText[which(table_final$class==8)], stringsAsFactors=FALSE)
# View(df_c8)
corpus_c8 <- VCorpus(VectorSource(df_c8$table_final.originalText.which.table_final.class....8..))

# apenas minusculas
corpus_c8 <- tm_map(corpus_c8, tolower)
# removendo pontua??es
corpus_c8 <- tm_map(corpus_c8, removePunctuation)
# removendo "stopwords"
corpus_c8 <- tm_map(corpus_c8, function(x) removeWords(x, stopwords("english")))
# stemiza??o (manter a raiz)
corpus_c8 <- tm_map(corpus_c8, stemDocument, language = "english")
#removendo n?meros
corpus_c8 <- tm_map(corpus_c8, removeNumbers)
#remover espa?o em branco entre as palavras 
corpus_c8 <- tm_map(corpus_c8, stripWhitespace)

#criando uma matrix de palavras e frequencia de palavras
dtm_c8 <- DocumentTermMatrix(corpus_c8)

# descritivos por cluster
freq_c8 <- sort(colSums(as.matrix(dtm_c8)), decreasing=TRUE)   
head(freq_c8, 20)

wf_c8 <- data.frame(word=names(freq_c8), freq=freq_c8)   
# head(wf_c8)  

p_c8 <- ggplot(subset(wf_c8, freq>3), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_freqs.eps",
#           width = 8, height = 8)
p_c8  
#dev.off()

# set.seed(142)   
# wordcloud(names(freq_c8), freq_c8, min.freq=20)

dtm_c8_2<-removeSparseTerms(dtm_c8,0.90)
cor_c8 <- cor(as.matrix(dtm_c8_2),method = "spearman")
cor_c8 <- ifelse(cor_c8<0,0,cor_c8)
colnames(cor_c8)

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_network.eps",
#           width = 8, height = 8)
rede8<-qgraph(cor_c8,
                  layout="spring",
                  labels=colnames(cor_c8),
                  threshold=0.1,
                  vsize=5,
                  label.scale=FALSE,
                  # grey=T,
                  color="lightblue",
                  borders = FALSE,
                  posCol = "grey",
                  label.cex=1.2)

#dev.off()################################################################################
#By Cluster Analysis - Cluster 9 Instruments
################################################################################
# Análise por cluster
#Corpus
df_c9 <- data.frame(table_final$originalText[which(table_final$class==9)], stringsAsFactors=FALSE)
# View(df_c9)
corpus_c9 <- VCorpus(VectorSource(df_c9$table_final.originalText.which.table_final.class....9..))

# apenas minusculas
corpus_9 <- tm_map(corpus_9, tolower)
# removendo pontua??es
corpus_9 <- tm_map(corpus_9, removePunctuation)
# removendo "stopwords"
corpus_9 <- tm_map(corpus_9, function(x) removeWords(x, stopwords("english")))
# stemiza??o (manter a raiz)
corpus_9 <- tm_map(corpus_9, stemDocument, language = "english")
#removendo n?meros
corpus_9 <- tm_map(corpus_9, removeNumbers)
#remover espa?o em branco entre as palavras 
corpus_9 <- tm_map(corpus_9, stripWhitespace)

#criando uma matrix de palavras e frequencia de palavras
dtm_c9 <- DocumentTermMatrix(corpus_c9)

# descritivos por cluster
freq_c9 <- sort(colSums(as.matrix(dtm_c9)), decreasing=TRUE)   
head(freq_c9, 20)

wf_c9 <- data.frame(word=names(freq_c9), freq=freq_c9)   
# head(wf_c9)  

p_c9 <- ggplot(subset(wf_c9, freq>3), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_freqs.eps",
#           width = 8, height = 8)
p_c9  
#dev.off()

# set.seed(142)   
# wordcloud(names(freq_c9), freq_c9, min.freq=20)

dtm_c9_2<-removeSparseTerms(dtm_c9,0.90)
cor_c9 <- cor(as.matrix(dtm_c9_2),method = "spearman")
cor_c9 <- ifelse(cor_c9<0,0,cor_c9)
colnames(cor_c9)

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_network.eps",
#           width = 8, height = 8)
rede9<-qgraph(cor_c9,
                  layout="spring",
                  labels=colnames(cor_c9),
                  threshold=0.1,
                  vsize=5,
                  label.scale=FALSE,
                  # grey=T,
                  color="lightblue",
                  borders = FALSE,
                  posCol = "grey",
                  label.cex=1.2)

#dev.off()################################################################################
#By Cluster Analysis - Cluster 10 Instruments
################################################################################
# Análise por cluster
#Corpus
df_c10 <- data.frame(table_final$originalText[which(table_final$class==10)], stringsAsFactors=FALSE)
# View(df_c10)
corpus_c10 <- VCorpus(VectorSource(df_c10$table_final.originalText.which.table_final.class....10..))

# apenas minusculas
corpus_c10 <- tm_map(corpus_c10, tolower)
# removendo pontua??es
corpus_c10 <- tm_map(corpus_c10, removePunctuation)
# removendo "stopwords"
corpus_c10 <- tm_map(corpus_c10, function(x) removeWords(x, stopwords("english")))
# stemiza??o (manter a raiz)
corpus_c10 <- tm_map(corpus_c10, stemDocument, language = "english")
#removendo n?meros
corpus_c10 <- tm_map(corpus_c10, removeNumbers)
#remover espa?o em branco entre as palavras 
corpus_c10 <- tm_map(corpus_c10, stripWhitespace)

#criando uma matrix de palavras e frequencia de palavras
dtm_c10 <- DocumentTermMatrix(corpus_c10)

# descritivos por cluster
freq_c10 <- sort(colSums(as.matrix(dtm_c10)), decreasing=TRUE)   
head(freq_c10, 20)

wf_c10 <- data.frame(word=names(freq_c10), freq=freq_c10)   
# head(wf_c10)  

p_c10 <- ggplot(subset(wf_c10, freq>3), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_freqs.eps",
#           width = 8, height = 8)
p_c10  
#dev.off()

# set.seed(142)   
# wordcloud(names(freq_c10), freq_c10, min.freq=20)

dtm_c10_2<-removeSparseTerms(dtm_c10,0.90)
cor_c10 <- cor(as.matrix(dtm_c10_2),method = "spearman")
cor_c10 <- ifelse(cor_c10<0,0,cor_c10)
colnames(cor_c10)

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_network.eps",
#           width = 8, height = 8)
rede10<-qgraph(cor_c10,
                  layout="spring",
                  labels=colnames(cor_c10),
                  threshold=0.1,
                  vsize=5,
                  label.scale=FALSE,
                  # grey=T,
                  color="lightblue",
                  borders = FALSE,
                  posCol = "grey",
                  label.cex=1.2)

#dev.off()################################################################################
#By Cluster Analysis - Cluster 1 Instruments
################################################################################
# Análise por cluster
#Corpus
df_c11 <- data.frame(table_final$originalText[which(table_final$class==11)], stringsAsFactors=FALSE)
# View(df_c11)
corpus_c11 <- VCorpus(VectorSource(df_c11$table_final.originalText.which.table_final.class....11..))

# apenas minusculas
corpus_c11 <- tm_map(corpus_c11, tolower)
# removendo pontua??es
corpus_c11 <- tm_map(corpus_c11, removePunctuation)
# removendo "stopwords"
corpus_c11 <- tm_map(corpus_c11, function(x) removeWords(x, stopwords("english")))
# stemiza??o (manter a raiz)
corpus_c11 <- tm_map(corpus_c11, stemDocument, language = "english")
#removendo n?meros
corpus_c11 <- tm_map(corpus_c11, removeNumbers)
#remover espa?o em branco entre as palavras 
corpus_c11 <- tm_map(corpus_c11, stripWhitespace)

#criando uma matrix de palavras e frequencia de palavras
dtm_c11 <- DocumentTermMatrix(corpus_c11)

# descritivos por cluster
freq_c11 <- sort(colSums(as.matrix(dtm_c11)), decreasing=TRUE)   
head(freq_c11, 20)

wf_c11 <- data.frame(word=names(freq_c11), freq=freq_c11)   
# head(wf_c11)  

p_c11 <- ggplot(subset(wf_c11, freq>3), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_freqs.eps",
#           width = 8, height = 8)
p_c11  
#dev.off()

# set.seed(142)   
# wordcloud(names(freq_c11), freq_c11, min.freq=20)

dtm_c11_2<-removeSparseTerms(dtm_c11,0.90)
cor_c11 <- cor(as.matrix(dtm_c11_2),method = "spearman")
cor_c11 <- ifelse(cor_c11<0,0,cor_c11)
colnames(cor_c11)

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_network.eps",
#           width = 8, height = 8)
rede11<-qgraph(cor_c11,
                  layout="spring",
                  labels=colnames(cor_c11),
                  threshold=0.1,
                  vsize=5,
                  label.scale=FALSE,
                  # grey=T,
                  color="lightblue",
                  borders = FALSE,
                  posCol = "grey",
                  label.cex=1.2)

#dev.off()################################################################################
#By Cluster Analysis - Cluster 12 Instruments
################################################################################
# Análise por cluster
#Corpus
df_c12 <- data.frame(table_final$originalText[which(table_final$class==12)], stringsAsFactors=FALSE)
# View(df_c12)
corpus_c12 <- VCorpus(VectorSource(df_c12$table_final.originalText.which.table_final.class....12..))

# apenas minusculas
corpus_c12 <- tm_map(corpus_c12, tolower)
# removendo pontua??es
corpus_c12 <- tm_map(corpus_c12, removePunctuation)
# removendo "stopwords"
corpus_c12 <- tm_map(corpus_c12, function(x) removeWords(x, stopwords("english")))
# stemiza??o (manter a raiz)
corpus_c12 <- tm_map(corpus_c12, stemDocument, language = "english")
#removendo n?meros
corpus_c12 <- tm_map(corpus_c12, removeNumbers)
#remover espa?o em branco entre as palavras 
corpus_c12 <- tm_map(corpus_c12, stripWhitespace)

#criando uma matrix de palavras e frequencia de palavras
dtm_c12 <- DocumentTermMatrix(corpus_c12)

# descritivos por cluster
freq_c12 <- sort(colSums(as.matrix(dtm_c12)), decreasing=TRUE)   
head(freq_c12, 20)

wf_c12 <- data.frame(word=names(freq_c12), freq=freq_c12)   
# head(wf_c12)  

p_c12 <- ggplot(subset(wf_c12, freq>3), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_freqs.eps",
#           width = 8, height = 8)
p_c12  
#dev.off()

# set.seed(142)   
# wordcloud(names(freq_c12), freq_c12, min.freq=20)

dtm_c12_2<-removeSparseTerms(dtm_c12,0.90)
cor_c12 <- cor(as.matrix(dtm_c12_2),method = "spearman")
cor_c12 <- ifelse(cor_c12<0,0,cor_c12)
colnames(cor_c12)

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_network.eps",
#           width = 8, height = 8)
rede12<-qgraph(cor_c12,
                  layout="spring",
                  labels=colnames(cor_c12),
                  threshold=0.1,
                  vsize=5,
                  label.scale=FALSE,
                  # grey=T,
                  color="lightblue",
                  borders = FALSE,
                  posCol = "grey",
                  label.cex=1.2)

#dev.off()################################################################################
#By Cluster Analysis - Cluster 13 Instruments
################################################################################
# Análise por cluster
#Corpus
df_c13 <- data.frame(table_final$originalText[which(table_final$class==13)], stringsAsFactors=FALSE)
# View(df_c13)
corpus_c13 <- VCorpus(VectorSource(df_c13$table_final.originalText.which.table_final.class....13..))

# apenas minusculas
corpus_c13 <- tm_map(corpus_c13, tolower)
# removendo pontua??es
corpus_c13 <- tm_map(corpus_c13, removePunctuation)
# removendo "stopwords"
corpus_c13 <- tm_map(corpus_c13, function(x) removeWords(x, stopwords("english")))
# stemiza??o (manter a raiz)
corpus_c13 <- tm_map(corpus_c13, stemDocument, language = "english")
#removendo n?meros
corpus_c13 <- tm_map(corpus_c13, removeNumbers)
#remover espa?o em branco entre as palavras 
corpus_c13 <- tm_map(corpus_c13, stripWhitespace)

#criando uma matrix de palavras e frequencia de palavras
dtm_c13 <- DocumentTermMatrix(corpus_c13)

# descritivos por cluster
freq_c13 <- sort(colSums(as.matrix(dtm_c13)), decreasing=TRUE)   
head(freq_c13, 20)

wf_c13 <- data.frame(word=names(freq_c13), freq=freq_c13)   
# head(wf_c13)  

p_c13 <- ggplot(subset(wf_c13, freq>3), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_freqs.eps",
#           width = 8, height = 8)
p_c13  
#dev.off()

# set.seed(142)   
# wordcloud(names(freq_c13), freq_c13, min.freq=20)

dtm_c13_2<-removeSparseTerms(dtm_c13,0.90)
cor_c13 <- cor(as.matrix(dtm_c13_2),method = "spearman")
cor_c13 <- ifelse(cor_c13<0,0,cor_c13)
colnames(cor_c13)

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_network.eps",
#           width = 8, height = 8)
rede13<-qgraph(cor_c13,
                  layout="spring",
                  labels=colnames(cor_c13),
                  threshold=0.1,
                  vsize=5,
                  label.scale=FALSE,
                  # grey=T,
                  color="lightblue",
                  borders = FALSE,
                  posCol = "grey",
                  label.cex=1.2)

#dev.off()################################################################################
#By Cluster Analysis - Cluster 1 Instruments
################################################################################
# Análise por cluster
#Corpus
df_c14 <- data.frame(table_final$originalText[which(table_final$class==14)], stringsAsFactors=FALSE)
# View(df_c14)
corpus_c14 <- VCorpus(VectorSource(df_c14$table_final.originalText.which.table_final.class....14..))

# apenas minusculas
corpus_c14 <- tm_map(corpus_c14, tolower)
# removendo pontua??es
corpus_c14 <- tm_map(corpus_c14, removePunctuation)
# removendo "stopwords"
corpus_c14 <- tm_map(corpus_c14, function(x) removeWords(x, stopwords("english")))
# stemiza??o (manter a raiz)
corpus_c14 <- tm_map(corpus_c14, stemDocument, language = "english")
#removendo n?meros
corpus_c14 <- tm_map(corpus_c14, removeNumbers)
#remover espa?o em branco entre as palavras 
corpus_c14 <- tm_map(corpus_c14, stripWhitespace)

#criando uma matrix de palavras e frequencia de palavras
dtm_c14 <- DocumentTermMatrix(corpus_c14)

# descritivos por cluster
freq_c14 <- sort(colSums(as.matrix(dtm_c14)), decreasing=TRUE)   
head(freq_c14, 20)

wf_c14 <- data.frame(word=names(freq_c14), freq=freq_c14)   
# head(wf_c14)  

p_c14 <- ggplot(subset(wf_c14, freq>3), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_freqs.eps",
#           width = 8, height = 8)
p_c14  
#dev.off()

# set.seed(142)   
# wordcloud(names(freq_c14), freq_c14, min.freq=20)

dtm_c14_2<-removeSparseTerms(dtm_c14,0.90)
cor_c14 <- cor(as.matrix(dtm_c14_2),method = "spearman")
cor_c14 <- ifelse(cor_c14<0,0,cor_c14)
colnames(cor_c14)

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_network.eps",
#           width = 8, height = 8)
rede14<-qgraph(cor_c14,
                  layout="spring",
                  labels=colnames(cor_c14),
                  threshold=0.1,
                  vsize=5,
                  label.scale=FALSE,
                  # grey=T,
                  color="lightblue",
                  borders = FALSE,
                  posCol = "grey",
                  label.cex=1.2)

#dev.off()################################################################################
#By Cluster Analysis - Cluster 1 Instruments
################################################################################
# Análise por cluster
#Corpus
df_c15 <- data.frame(table_final$originalText[which(table_final$class==15)], stringsAsFactors=FALSE)
# View(df_c15)
corpus_c15 <- VCorpus(VectorSource(df_c15$table_final.originalText.which.table_final.class....15..))

# apenas minusculas
corpus_c15 <- tm_map(corpus_c15, tolower)
# removendo pontua??es
corpus_c15 <- tm_map(corpus_c15, removePunctuation)
# removendo "stopwords"
corpus_c15 <- tm_map(corpus_c15, function(x) removeWords(x, stopwords("english")))
# stemiza??o (manter a raiz)
corpus_c15 <- tm_map(corpus_c15, stemDocument, language = "english")
#removendo n?meros
corpus_c15 <- tm_map(corpus_c15, removeNumbers)
#remover espa?o em branco entre as palavras 
corpus_c15 <- tm_map(corpus_c15, stripWhitespace)

#criando uma matrix de palavras e frequencia de palavras
dtm_c15 <- DocumentTermMatrix(corpus_c15)

# descritivos por cluster
freq_c15 <- sort(colSums(as.matrix(dtm_c15)), decreasing=TRUE)   
head(freq_c15, 20)

wf_c15 <- data.frame(word=names(freq_c15), freq=freq_c15)   
# head(wf_c15)  

p_c15 <- ggplot(subset(wf_c15, freq>3), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_freqs.eps",
#           width = 8, height = 8)
p_c15  
#dev.off()

# set.seed(142)   
# wordcloud(names(freq_c15), freq_c15, min.freq=20)

dtm_c15_2<-removeSparseTerms(dtm_c15,0.90)
cor_c15 <- cor(as.matrix(dtm_c15_2),method = "spearman")
cor_c15 <- ifelse(cor_c15<0,0,cor_c15)
colnames(cor_c15)

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_network.eps",
#           width = 8, height = 8)
rede15<-qgraph(cor_c15,
                  layout="spring",
                  labels=colnames(cor_c15),
                  threshold=0.1,
                  vsize=5,
                  label.scale=FALSE,
                  # grey=T,
                  color="lightblue",
                  borders = FALSE,
                  posCol = "grey",
                  label.cex=1.2)

#dev.off()################################################################################
#By Cluster Analysis - Cluster 1 Instruments
################################################################################
# Análise por cluster
#Corpus
df_c16 <- data.frame(table_final$originalText[which(table_final$class==16)], stringsAsFactors=FALSE)
# View(df_c16)
corpus_c16 <- VCorpus(VectorSource(df_c16$table_final.originalText.which.table_final.class....16..))

# apenas minusculas
corpus_c16 <- tm_map(corpus_c16, tolower)
# removendo pontua??es
corpus_c16 <- tm_map(corpus_c16, removePunctuation)
# removendo "stopwords"
corpus_c16 <- tm_map(corpus_c16, function(x) removeWords(x, stopwords("english")))
# stemiza??o (manter a raiz)
corpus_c16 <- tm_map(corpus_c16, stemDocument, language = "english")
#removendo n?meros
corpus_c16 <- tm_map(corpus_c16, removeNumbers)
#remover espa?o em branco entre as palavras 
corpus_c16 <- tm_map(corpus_c16, stripWhitespace)

#criando uma matrix de palavras e frequencia de palavras
dtm_c16 <- DocumentTermMatrix(corpus_c16)

# descritivos por cluster
freq_c16 <- sort(colSums(as.matrix(dtm_c16)), decreasing=TRUE)   
head(freq_c16, 20)

wf_c16 <- data.frame(word=names(freq_c16), freq=freq_c16)   
# head(wf_c16)  

p_c16 <- ggplot(subset(wf_c16, freq>3), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_freqs.eps",
#           width = 8, height = 8)
p_c16  
#dev.off()

# set.seed(142)   
# wordcloud(names(freq_c16), freq_c16, min.freq=20)

dtm_c16_2<-removeSparseTerms(dtm_c16,0.90)
cor_c16 <- cor(as.matrix(dtm_c16_2),method = "spearman")
cor_c16 <- ifelse(cor_c16<0,0,cor_c16)
colnames(cor_c16)

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_network.eps",
#           width = 8, height = 8)
rede16<-qgraph(cor_c16,
                  layout="spring",
                  labels=colnames(cor_c16),
                  threshold=0.1,
                  vsize=5,
                  label.scale=FALSE,
                  # grey=T,
                  color="lightblue",
                  borders = FALSE,
                  posCol = "grey",
                  label.cex=1.2)

#dev.off()################################################################################
#By Cluster Analysis - Cluster 17 Instruments
################################################################################
# Análise por cluster
#Corpus
df_c17 <- data.frame(table_final$originalText[which(table_final$class==17)], stringsAsFactors=FALSE)
# View(df_c17)
corpus_c17 <- VCorpus(VectorSource(df_c17$table_final.originalText.which.table_final.class....17..))

# apenas minusculas
corpus_c17 <- tm_map(corpus_c17, tolower)
# removendo pontua??es
corpus_c17 <- tm_map(corpus_c17, removePunctuation)
# removendo "stopwords"
corpus_c17 <- tm_map(corpus_c17, function(x) removeWords(x, stopwords("english")))
# stemiza??o (manter a raiz)
corpus_c17 <- tm_map(corpus_c17, stemDocument, language = "english")
#removendo n?meros
corpus_c17 <- tm_map(corpus_c17, removeNumbers)
#remover espa?o em branco entre as palavras 
corpus_c17 <- tm_map(corpus_c17, stripWhitespace)

#criando uma matrix de palavras e frequencia de palavras
dtm_c17 <- DocumentTermMatrix(corpus_c17)

# descritivos por cluster
freq_c17 <- sort(colSums(as.matrix(dtm_c17)), decreasing=TRUE)   
head(freq_c17, 20)

wf_c17 <- data.frame(word=names(freq_c17), freq=freq_c17)   
# head(wf_c17)  

p_c17 <- ggplot(subset(wf_c17, freq>3), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_freqs.eps",
#           width = 8, height = 8)
p_c17  
#dev.off()

# set.seed(142)   
# wordcloud(names(freq_c17), freq_c17, min.freq=20)

dtm_c17_2<-removeSparseTerms(dtm_c17,0.90)
cor_c17 <- cor(as.matrix(dtm_c17_2),method = "spearman")
cor_c17 <- ifelse(cor_c17<0,0,cor_c17)
colnames(cor_c17)

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_network.eps",
#           width = 8, height = 8)
rede17<-qgraph(cor_c17,
                  layout="spring",
                  labels=colnames(cor_c17),
                  threshold=0.1,
                  vsize=5,
                  label.scale=FALSE,
                  # grey=T,
                  color="lightblue",
                  borders = FALSE,
                  posCol = "grey",
                  label.cex=1.2)

#dev.off()################################################################################
#By Cluster Analysis - Cluster 1 Instruments
################################################################################
# Análise por cluster
#Corpus
df_c18 <- data.frame(table_final$originalText[which(table_final$class==18)], stringsAsFactors=FALSE)
# View(df_c18)
corpus_c18 <- VCorpus(VectorSource(df_c18$table_final.originalText.which.table_final.class....18..))

# apenas minusculas
corpus_c17 <- tm_map(corpus_c17, tolower)
# removendo pontua??es
corpus_c17 <- tm_map(corpus_c17, removePunctuation)
# removendo "stopwords"
corpus_c17 <- tm_map(corpus_c17, function(x) removeWords(x, stopwords("english")))
# stemiza??o (manter a raiz)
corpus_c17 <- tm_map(corpus_c17, stemDocument, language = "english")
#removendo n?meros
corpus_c17 <- tm_map(corpus_c17, removeNumbers)
#remover espa?o em branco entre as palavras 
corpus_c17 <- tm_map(corpus_c17, stripWhitespace)

#criando uma matrix de palavras e frequencia de palavras
dtm_c18 <- DocumentTermMatrix(corpus_c18)

# descritivos por cluster
freq_c18 <- sort(colSums(as.matrix(dtm_c18)), decreasing=TRUE)   
head(freq_c18, 20)

wf_c18 <- data.frame(word=names(freq_c18), freq=freq_c18)   
# head(wf_c18)  

p_c18 <- ggplot(subset(wf_c18, freq>3), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_freqs.eps",
#           width = 8, height = 8)
p_c18  
#dev.off()

# set.seed(142)   
# wordcloud(names(freq_c18), freq_c18, min.freq=20)

dtm_c18_2<-removeSparseTerms(dtm_c18,0.90)
cor_c18 <- cor(as.matrix(dtm_c18_2),method = "spearman")
cor_c18 <- ifelse(cor_c18<0,0,cor_c18)
colnames(cor_c18)

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_network.eps",
#           width = 8, height = 8)
rede18<-qgraph(cor_c18,
                  layout="spring",
                  labels=colnames(cor_c18),
                  threshold=0.1,
                  vsize=5,
                  label.scale=FALSE,
                  # grey=T,
                  color="lightblue",
                  borders = FALSE,
                  posCol = "grey",
                  label.cex=1.2)

#dev.off()################################################################################
#By Cluster Analysis - Cluster 19 Instruments
################################################################################
# Análise por cluster
#Corpus
df_c19 <- data.frame(table_final$originalText[which(table_final$class==19)], stringsAsFactors=FALSE)
# View(df_c19)
corpus_c19 <- VCorpus(VectorSource(df_c19$table_final.originalText.which.table_final.class....19..))

# apenas minusculas
corpus_c19 <- tm_map(corpus_c19, tolower)
# removendo pontua??es
corpus_c19 <- tm_map(corpus_c19, removePunctuation)
# removendo "stopwords"
corpus_c19 <- tm_map(corpus_c19, function(x) removeWords(x, stopwords("english")))
# stemiza??o (manter a raiz)
corpus_c19 <- tm_map(corpus_c19, stemDocument, language = "english")
#removendo n?meros
corpus_c19 <- tm_map(corpus_c19, removeNumbers)
#remover espa?o em branco entre as palavras 
corpus_c19 <- tm_map(corpus_c19, stripWhitespace)

#criando uma matrix de palavras e frequencia de palavras
dtm_c19 <- DocumentTermMatrix(corpus_c19)

# descritivos por cluster
freq_c19 <- sort(colSums(as.matrix(dtm_c19)), decreasing=TRUE)   
head(freq_c19, 20)

wf_c19 <- data.frame(word=names(freq_c19), freq=freq_c19)   
# head(wf_c19)  

p_c19 <- ggplot(subset(wf_c19, freq>3), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_freqs.eps",
#           width = 8, height = 8)
p_c19  
#dev.off()

# set.seed(142)   
# wordcloud(names(freq_c19), freq_c19, min.freq=20)

dtm_c19_2<-removeSparseTerms(dtm_c19,0.90)
cor_c19 <- cor(as.matrix(dtm_c19_2),method = "spearman")
cor_c19 <- ifelse(cor_c19<0,0,cor_c19)
colnames(cor_c19)

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_network.eps",
#           width = 8, height = 8)
rede19<-qgraph(cor_c19,
                  layout="spring",
                  labels=colnames(cor_c19),
                  threshold=0.1,
                  vsize=5,
                  label.scale=FALSE,
                  # grey=T,
                  color="lightblue",
                  borders = FALSE,
                  posCol = "grey",
                  label.cex=1.2)

#dev.off()################################################################################
#By Cluster Analysis - Cluster 1 Instruments
################################################################################
# Análise por cluster
#Corpus
df_c20 <- data.frame(table_final$originalText[which(table_final$class==20)], stringsAsFactors=FALSE)
# View(df_c20)
corpus_c20 <- VCorpus(VectorSource(df_c20$table_final.originalText.which.table_final.class....20..))

# apenas minusculas
corpus_c20 <- tm_map(corpus_c20, tolower)
# removendo pontua??es
corpus_c20 <- tm_map(corpus_c20, removePunctuation)
# removendo "stopwords"
corpus_c20 <- tm_map(corpus_c20, function(x) removeWords(x, stopwords("english")))
# stemiza??o (manter a raiz)
corpus_c20 <- tm_map(corpus_c20, stemDocument, language = "english")
#removendo n?meros
corpus_c20 <- tm_map(corpus_c20, removeNumbers)
#remover espa?o em branco entre as palavras 
corpus_c20 <- tm_map(corpus_c20, stripWhitespace)

#criando uma matrix de palavras e frequencia de palavras
dtm_c20 <- DocumentTermMatrix(corpus_c20)

# descritivos por cluster
freq_c20 <- sort(colSums(as.matrix(dtm_c20)), decreasing=TRUE)   
head(freq_c20, 20)

wf_c20 <- data.frame(word=names(freq_c20), freq=freq_c20)   
# head(wf_c20)  

p_c20 <- ggplot(subset(wf_c20, freq>3), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_freqs.eps",
#           width = 8, height = 8)
p_c20  
#dev.off()

# set.seed(142)   
# wordcloud(names(freq_c20), freq_c20, min.freq=20)

dtm_c20_2<-removeSparseTerms(dtm_c20,0.90)
cor_c20 <- cor(as.matrix(dtm_c20_2),method = "spearman")
cor_c20 <- ifelse(cor_c20<0,0,cor_c20)
colnames(cor_c20)

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_network.eps",
#           width = 8, height = 8)
rede20<-qgraph(cor_c20,
                  layout="spring",
                  labels=colnames(cor_c20),
                  threshold=0.1,
                  vsize=5,
                  label.scale=FALSE,
                  # grey=T,
                  color="lightblue",
                  borders = FALSE,
                  posCol = "grey",
                  label.cex=1.2)

#dev.off()################################################################################
#By Cluster Analysis - Cluster 1 Instruments
################################################################################
# Análise por cluster
#Corpus
df_c21 <- data.frame(table_final$originalText[which(table_final$class==21)], stringsAsFactors=FALSE)
# View(df_c21)
corpus_c21 <- VCorpus(VectorSource(df_c21$table_final.originalText.which.table_final.class....21..))

# apenas minusculas
corpus_c21 <- tm_map(corpus_c21, tolower)
# removendo pontua??es
corpus_c21 <- tm_map(corpus_c21, removePunctuation)
# removendo "stopwords"
corpus_c21 <- tm_map(corpus_c21, function(x) removeWords(x, stopwords("english")))
# stemiza??o (manter a raiz)
corpus_c21 <- tm_map(corpus_c21, stemDocument, language = "english")
#removendo n?meros
corpus_c21 <- tm_map(corpus_c21, removeNumbers)
#remover espa?o em branco entre as palavras 
corpus_c21 <- tm_map(corpus_c21, stripWhitespace)

#criando uma matrix de palavras e frequencia de palavras
dtm_c21 <- DocumentTermMatrix(corpus_c21)

# descritivos por cluster
freq_c21 <- sort(colSums(as.matrix(dtm_c21)), decreasing=TRUE)   
head(freq_c21, 20)

wf_c21 <- data.frame(word=names(freq_c21), freq=freq_c21)   
# head(wf_c21)  

p_c21 <- ggplot(subset(wf_c21, freq>3), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_freqs.eps",
#           width = 8, height = 8)
p_c21  
#dev.off()

# set.seed(142)   
# wordcloud(names(freq_c21), freq_c21, min.freq=20)

dtm_c21_2<-removeSparseTerms(dtm_c21,0.90)
cor_c21 <- cor(as.matrix(dtm_c21_2),method = "spearman")
cor_c21 <- ifelse(cor_c21<0,0,cor_c21)
colnames(cor_c21)

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_network.eps",
#           width = 8, height = 8)
rede21<-qgraph(cor_c21,
                  layout="spring",
                  labels=colnames(cor_c21),
                  threshold=0.1,
                  vsize=5,
                  label.scale=FALSE,
                  # grey=T,
                  color="lightblue",
                  borders = FALSE,
                  posCol = "grey",
                  label.cex=1.2)