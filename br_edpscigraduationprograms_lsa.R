# aula prática 1: análise qualitativa avançada  - https://github.com/wagnerLM/text_mining
#carregando textos
#manipulando textos
#descrição de textos por frequência de termos

#instalando e ativando os pacotes:
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("devtools")
install.packages("ggplot2")
install.packages("tidyverse")
devtools::install_github("dfalbel/PTtextmining")

#load packages
library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(ggplot2)
library(PTtextmining)


data<-read.csv("/Users/joaovissoci/Desktop/Titulos_csv2.csv",
               stringsAsFactors=FALSE)

#minerar o texto em poetugues

# for(i in 1:length(data[,1])){

# data[i,1] %>%
#   transformar_minusculo() %>%
#   transformar_stemming() %>%
#   remover_stopwords() %>%
#   remover_acentos() %>%
#   remover_pontuacao() %>%
#   remover_numeros() %>%
#   remover_dinheiro() %>%
#   remover_espacos_excedentes() -> mined_data[i]

# }


data[,1] %>%
  transformar_minusculo() %>%
  transformar_stemming() %>%
  remover_stopwords() %>%
  remover_acentos() %>%
  remover_pontuacao() %>%
  remover_numeros() %>%
  remover_dinheiro() %>%
  remover_espacos_excedentes() -> mined_data2

cleaned_data<-data.frame(mined_data2,area=data[,2])

# #primeiras manipulações
# #removendo pontuações
# ppgp<-tm_map(ppgp,removePunctuation)
# #transformando maiúsculas em minísculas
# ppgp <- tm_map(ppgp,content_transformer(tolower)),
# #removendo números
# ppgp <- tm_map(ppgp, removeNumbers),
# #removendo "stopwords" (https://en.wikipedia.org/wiki/Stop_words) 
# ppgp <- tm_map(ppgp, removeWords, stopwords("pt")),
# #casos específicos pt-br
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "á", replacement = "a"),
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "é", replacement = "e"),
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "í", replacement = "i"),
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "ó", replacement = "o"),
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "ú", replacement = "u"),
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "ã", replacement = "a"),
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "õ", replacement = "o"),
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "ç", replacement = "c"),
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "ê", replacement = "e"),
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "â", replacement = "a"),
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "à", replacement = "a"),
# ppgp <- tm_map(ppgp, content_transformer(gsub),
#                pattern = "ô", replacement = "o"),

# #stemming ou "stemização"  (https://pt.wikipedia.org/wiki/Stemiza%C3%A7%C3%A3o)
# #ppgp<-tm_map(ppgp,stemDocument,language = "pt")
# #outra possibildiade é a lematização (https://portuguese.stackexchange.com/questions/1417/o-que-%C3%A9-lematiza%C3%A7%C3%A3o)

#Separating corpuses acordind to the AREA

#educacao fisica
corpus_ef<-Corpus(VectorSource(cleaned_data[,1][cleaned_data$area=="Educacao fisica"]))
corpus_ef

corpus_ef <- tm_map(corpus_ef, removeWords,

#outras correções no texto
corpus_ef <- tm_map(corpus_ef, content_transformer(function(x) gsub(x,
               pattern = "atividade fisica", replacement = "atividade_fisica")))
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "exercicio fisica", replacement = "exercicio_fisica")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "educacao fisica", replacement = "educacao_fisica")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "condicionamento fisica", replacement = "condicionamento_fisica")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "qualidade vida", replacement = "qualidade_vida")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "estilo vida", replacement = "estilo_vida")

#exclusão de stopwords manulamente
corpus_ef <- tm_map(corpus_ef, removeWords, c("acaso",
                                              "acinte",
                                              "adiante",
                                              "adrede",
                                              "afinal",
                                              "afora",
                                              "agora",
                                              "algures",
                                              "alem",
                                              "ali",
                                              "amanha",
                                              "antes",
                                              "aqui",
                                              "assim",
                                              "atras",
                                              "bem",
                                              "breve",
                                              "cedo",
                                              "certamente",
                                              "efetivamente",
                                              "enfim",
                                              "hoje",
                                              "mal",
                                              "mais",
                                              "melhor",
                                              "menos",
                                              "muito",
                                              "nao",
                                              "ontem",
                                              "pior",
                                              "pouco",
                                              "quanto",
                                              "quao",
                                              "quase",
                                              "realmente",
                                              "sera",
                                              "sim",
                                              "talvez",
                                              "tanto",
                                              "e",
                                              "nem",
                                              "mas",
                                              "tambem",
                                              "como",
                                              "bem",
                                              "porem",
                                              "todavia",
                                              "contudo",
                                              "entretanto",
                                              "entanto",
                                              "ou",
                                              "ora",
                                              "quer",
                                              "ja",
                                              "logo",
                                              "portanto",
                                              "por",
                                              "assim",
                                              "conseguinte",
                                              "que",
                                              "porque",
                                              "porquanto",
                                              "pois",
                                              "sendo",
                                              "visto",
                                              "como",
                                              "tal",
                                              "tao",
                                              "tanto",
                                              "assim",
                                              "conforme",
                                              "segundo",
                                              "consoante",
                                              "mesmo",
                                              "mais",
                                              "ainda",
                                              "se",
                                              "bem",
                                              "embora",
                                              "se",
                                              "caso",
                                              "contanto",
                                              "salvo",
                                              "medida",
                                              "quanto",
                                              "fim",
                                              "quando",
                                              "enquanto",
                                              "sempre",
                                              "depois",
                                              "a",
                                              "ante",
                                              "apos",
                                              "ate",
                                              "com",
                                              "contra",
                                              "de",
                                              "desde",
                                              "para",
                                              "per",
                                              "perante",
                                              "por",
                                              "sem",
                                              "sob",
                                              "sobre",
                                              "tras",
                                              "algo",
                                              "alguem",
                                              "algum",
                                              "alguns",
                                              "cada",
                                              "cujo",
                                              "muitos",
                                              "nada",
                                              "nenhum",
                                              "nenhuns",
                                              "ninguem",
                                              "outrem",
                                              "outros",
                                              "poucos",
                                              "quaisquer",
                                              "qualquer",
                                              "quantos",
                                              "quem",
                                              "tantos",
                                              "todos",
                                              "tudo",
                                              "que",
                                              "nao",
                                              "para",
                                              "varios",
                                              "de",
                                              "a",
                                              "o",
                                              "que",
                                              "e",
                                              "do",
                                              "da",
                                              "em",
                                              "um",
                                              "para",
                                              "com",
                                              "nao",
                                              "uma",
                                              "os",
                                              "no",
                                              "se",
                                              "na",
                                              "por",
                                              "mais",
                                              "as",
                                              "dos",
                                              "como",
                                              "mas",
                                              "ao",
                                              "ele",
                                              "das",
                                              "a",
                                              "seu",
                                              "sua",
                                              "ou",
                                              "quando",
                                              "muito",
                                              "nos",
                                              "ja",
                                              "eu",
                                              "tambem",
                                              "so",
                                              "pelo",
                                              "pela",
                                              "ate",
                                              "isso",
                                              "ela",
                                              "entre",
                                              "depois",
                                              "sem",
                                              "mesmo",
                                              "aos",
                                              "seus",
                                              "quem",
                                              "nas",
                                              "me",
                                              "esse",
                                              "eles",
                                              "voce",
                                              "essa",
                                              "num",
                                              "nem",
                                              "suas",
                                              "meu",
                                              "as",
                                              "minha",
                                              "numa",
                                              "pelos",
                                              "elas",
                                              "qual",
                                              "nos",
                                              "lhe",
                                              "deles",
                                              "essas",
                                              "esses",
                                              "pelas",
                                              "este",
                                              "dele",
                                              "tu",
                                              "te",
                                              "voces",
                                              "vos",
                                              "lhes",
                                              "meus",
                                              "minhas",
                                              "teu",
                                              "tua",
                                              "teus",
                                              "tuas",
                                              "nosso",
                                              "nossa",
                                              "nossos",
                                              "nossas",
                                              "dela",
                                              "delas",
                                              "esta",
                                              "estes",
                                              "estas",
                                              "aquele",
                                              "aquela",
                                              "aqueles",
                                              "aquelas",
                                              "isto",
                                              "aquilo",
                                              "estou",
                                              "esta",
                                              "estamos",
                                              "estao",
                                              "estive",
                                              "esteve",
                                              "estivemos",
                                              "estiveram",
                                              "estava",
                                              "estavamos",
                                              "estavam",
                                              "estivera",
                                              "estiveramos",
                                              "esteja",
                                              "estejamos",
                                              "estejam",
                                              "estivesse",
                                              "estivessemos",
                                              "estivessem",
                                              "estiver",
                                              "estivermos",
                                              "estiverem",
                                              "hei",
                                              "ha",
                                              "havemos",
                                              "hao",
                                              "houve",
                                              "houvemos",
                                              "houveram",
                                              "houvera",
                                              "houveramos",
                                              "haja",
                                              "hajamos",
                                              "hajam",
                                              "houvesse",
                                              "houvessemos",
                                              "houvessem",
                                              "houver",
                                              "houvermos",
                                              "houverem",
                                              "houverei",
                                              "houvera",
                                              "houveremos",
                                              "houverao",
                                              "houveria",
                                              "houveriamos",
                                              "houveriam",
                                              "sou",
                                              "somos",
                                              "sao",
                                              "era",
                                              "eramos",
                                              "eram",
                                              "fui",
                                              "foi",
                                              "fomos",
                                              "foram",
                                              "fora",
                                              "foramos",
                                              "seja",
                                              "sejamos",
                                              "sejam",
                                              "fosse",
                                              "fossemos",
                                              "fossem",
                                              "for",
                                              "formos",
                                              "forem",
                                              "serei",
                                              "sera",
                                              "seremos",
                                              "serao",
                                              "seria",
                                              "seriamos",
                                              "seriam",
                                              "tenho",
                                              "tem",
                                              "temos",
                                              "tem",
                                              "tinha",
                                              "tinhamos",
                                              "tinham",
                                              "tive",
                                              "teve",
                                              "tivemos",
                                              "tiveram",
                                              "tivera",
                                              "tiveramos",
                                              "tenha",
                                              "tenhamos",
                                              "tenham",
                                              "tivesse",
                                              "tivessemos",
                                              "tivessem",
                                              "tiver",
                                              "tivermos",
                                              "tiverem",
                                              "terei",
                                              "tera",
                                              "teremos",
                                              "terao",
                                              "teria",
                                              "teriamos",
                                              "teriam"))

#selecionando o texto para visualizar
writeLines(as.character(corpus_ef))

#lista de transformações
getTransformations()

#cria uma matriz de documentos e termos (https://en.wikipedia.org/wiki/Document-term_matrix)
dtm_ppgp <- DocumentTermMatrix(corpus_ef)
dtm_ppgp
#visualizando a matriz
inspect(dtm_ppgp[1,1:5])

#verificando a frequência de palavras
freq <- colSums(as.matrix(dtm_ppgp))
freq
length(freq)
ord <- order(freq,decreasing=TRUE)
freq[head(ord)]
freq[tail(ord)]

#recriando a dtm excluindo palavras pequenas
dtmr_ppgp <-DocumentTermMatrix(ppgp, control=list(wordLengths=c(3, Inf)))
dtmr_ppgp

freq <- colSums(as.matrix(dtmr_ppgp))
length(freq)
ord <- order(freq,decreasing=TRUE)
freq[head(ord)]
freq[tail(ord)]

#frequencia de termos
findFreqTerms(dtmr_ppgp,lowfreq=5)

#visualização gráfico de freq. termos
wf=data.frame(term=names(freq),occurrences=freq)
wf
p <- ggplot(subset(wf, freq>4), aes(term, occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

#wordcloud
#setting the same seed each time ensures consistent look across clouds
set.seed(30)
#limit words by specifying min frequency
wordcloud(names(freq),freq, max.words =100,min.freq=5,#scale=c(1,.01), 
          random.order = F,random.color=F,rot.per=.5,colors = freq)

#psicologia
corpus_psi<-Corpus(VectorSource(cleaned_data[,1][cleaned_data$area=="Psicologia"]))
corpus_psi

corpus_psi <- tm_map(corpus_psi, removeWords,

#selecionando o texto para visualizar
writeLines(as.character(corpus_psi))

#lista de transformações
getTransformations()

#outras correções no texto
corpus_psi <- tm_map(corpus_psi, content_transformer(function(x) gsub(x,
               pattern = "atividade fisica", replacement = "atividade_fisica")))
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "exercicio fisica", replacement = "exercicio_fisica")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "educacao fisica", replacement = "educacao_fisica")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "condicionamento fisica", replacement = "condicionamento_fisica")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "qualidade vida", replacement = "qualidade_vida")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "estilo vida", replacement = "estilo_vida")

#exclusão de stopwords manulamente
corpus_psi <- tm_map(corpus_psi, removeWords, c("acaso",
                                              "acinte",
                                              "adiante",
                                              "adrede",
                                              "afinal",
                                              "afora",
                                              "agora",
                                              "algures",
                                              "alem",
                                              "ali",
                                              "amanha",
                                              "antes",
                                              "aqui",
                                              "assim",
                                              "atras",
                                              "bem",
                                              "breve",
                                              "cedo",
                                              "certamente",
                                              "efetivamente",
                                              "enfim",
                                              "hoje",
                                              "mal",
                                              "mais",
                                              "melhor",
                                              "menos",
                                              "muito",
                                              "nao",
                                              "ontem",
                                              "pior",
                                              "pouco",
                                              "quanto",
                                              "quao",
                                              "quase",
                                              "realmente",
                                              "sera",
                                              "sim",
                                              "talvez",
                                              "tanto",
                                              "e",
                                              "nem",
                                              "mas",
                                              "tambem",
                                              "como",
                                              "bem",
                                              "porem",
                                              "todavia",
                                              "contudo",
                                              "entretanto",
                                              "entanto",
                                              "ou",
                                              "ora",
                                              "quer",
                                              "ja",
                                              "logo",
                                              "portanto",
                                              "por",
                                              "assim",
                                              "conseguinte",
                                              "que",
                                              "porque",
                                              "porquanto",
                                              "pois",
                                              "sendo",
                                              "visto",
                                              "como",
                                              "tal",
                                              "tao",
                                              "tanto",
                                              "assim",
                                              "conforme",
                                              "segundo",
                                              "consoante",
                                              "mesmo",
                                              "mais",
                                              "ainda",
                                              "se",
                                              "bem",
                                              "embora",
                                              "se",
                                              "caso",
                                              "contanto",
                                              "salvo",
                                              "medida",
                                              "quanto",
                                              "fim",
                                              "quando",
                                              "enquanto",
                                              "sempre",
                                              "depois",
                                              "a",
                                              "ante",
                                              "apos",
                                              "ate",
                                              "com",
                                              "contra",
                                              "de",
                                              "desde",
                                              "para",
                                              "per",
                                              "perante",
                                              "por",
                                              "sem",
                                              "sob",
                                              "sobre",
                                              "tras",
                                              "algo",
                                              "alguem",
                                              "algum",
                                              "alguns",
                                              "cada",
                                              "cujo",
                                              "muitos",
                                              "nada",
                                              "nenhum",
                                              "nenhuns",
                                              "ninguem",
                                              "outrem",
                                              "outros",
                                              "poucos",
                                              "quaisquer",
                                              "qualquer",
                                              "quantos",
                                              "quem",
                                              "tantos",
                                              "todos",
                                              "tudo",
                                              "que",
                                              "nao",
                                              "para",
                                              "varios",
                                              "de",
                                              "a",
                                              "o",
                                              "que",
                                              "e",
                                              "do",
                                              "da",
                                              "em",
                                              "um",
                                              "para",
                                              "com",
                                              "nao",
                                              "uma",
                                              "os",
                                              "no",
                                              "se",
                                              "na",
                                              "por",
                                              "mais",
                                              "as",
                                              "dos",
                                              "como",
                                              "mas",
                                              "ao",
                                              "ele",
                                              "das",
                                              "a",
                                              "seu",
                                              "sua",
                                              "ou",
                                              "quando",
                                              "muito",
                                              "nos",
                                              "ja",
                                              "eu",
                                              "tambem",
                                              "so",
                                              "pelo",
                                              "pela",
                                              "ate",
                                              "isso",
                                              "ela",
                                              "entre",
                                              "depois",
                                              "sem",
                                              "mesmo",
                                              "aos",
                                              "seus",
                                              "quem",
                                              "nas",
                                              "me",
                                              "esse",
                                              "eles",
                                              "voce",
                                              "essa",
                                              "num",
                                              "nem",
                                              "suas",
                                              "meu",
                                              "as",
                                              "minha",
                                              "numa",
                                              "pelos",
                                              "elas",
                                              "qual",
                                              "nos",
                                              "lhe",
                                              "deles",
                                              "essas",
                                              "esses",
                                              "pelas",
                                              "este",
                                              "dele",
                                              "tu",
                                              "te",
                                              "voces",
                                              "vos",
                                              "lhes",
                                              "meus",
                                              "minhas",
                                              "teu",
                                              "tua",
                                              "teus",
                                              "tuas",
                                              "nosso",
                                              "nossa",
                                              "nossos",
                                              "nossas",
                                              "dela",
                                              "delas",
                                              "esta",
                                              "estes",
                                              "estas",
                                              "aquele",
                                              "aquela",
                                              "aqueles",
                                              "aquelas",
                                              "isto",
                                              "aquilo",
                                              "estou",
                                              "esta",
                                              "estamos",
                                              "estao",
                                              "estive",
                                              "esteve",
                                              "estivemos",
                                              "estiveram",
                                              "estava",
                                              "estavamos",
                                              "estavam",
                                              "estivera",
                                              "estiveramos",
                                              "esteja",
                                              "estejamos",
                                              "estejam",
                                              "estivesse",
                                              "estivessemos",
                                              "estivessem",
                                              "estiver",
                                              "estivermos",
                                              "estiverem",
                                              "hei",
                                              "ha",
                                              "havemos",
                                              "hao",
                                              "houve",
                                              "houvemos",
                                              "houveram",
                                              "houvera",
                                              "houveramos",
                                              "haja",
                                              "hajamos",
                                              "hajam",
                                              "houvesse",
                                              "houvessemos",
                                              "houvessem",
                                              "houver",
                                              "houvermos",
                                              "houverem",
                                              "houverei",
                                              "houvera",
                                              "houveremos",
                                              "houverao",
                                              "houveria",
                                              "houveriamos",
                                              "houveriam",
                                              "sou",
                                              "somos",
                                              "sao",
                                              "era",
                                              "eramos",
                                              "eram",
                                              "fui",
                                              "foi",
                                              "fomos",
                                              "foram",
                                              "fora",
                                              "foramos",
                                              "seja",
                                              "sejamos",
                                              "sejam",
                                              "fosse",
                                              "fossemos",
                                              "fossem",
                                              "for",
                                              "formos",
                                              "forem",
                                              "serei",
                                              "sera",
                                              "seremos",
                                              "serao",
                                              "seria",
                                              "seriamos",
                                              "seriam",
                                              "tenho",
                                              "tem",
                                              "temos",
                                              "tem",
                                              "tinha",
                                              "tinhamos",
                                              "tinham",
                                              "tive",
                                              "teve",
                                              "tivemos",
                                              "tiveram",
                                              "tivera",
                                              "tiveramos",
                                              "tenha",
                                              "tenhamos",
                                              "tenham",
                                              "tivesse",
                                              "tivessemos",
                                              "tivessem",
                                              "tiver",
                                              "tivermos",
                                              "tiverem",
                                              "terei",
                                              "tera",
                                              "teremos",
                                              "terao",
                                              "teria",
                                              "teriamos",
                                              "teriam"))
#cria uma matriz de documentos e termos (https://en.wikipedia.org/wiki/Document-term_matrix)
dtm_ppgp <- DocumentTermMatrix(corpus_psi)
dtm_ppgp
#visualizando a matriz
inspect(dtm_ppgp[1,1:5])

#verificando a frequência de palavras
freq <- colSums(as.matrix(dtm_ppgp))
freq
length(freq)
ord <- order(freq,decreasing=TRUE)
freq[head(ord)]
freq[tail(ord)]

#recriando a dtm excluindo palavras pequenas
dtmr_ppgp <-DocumentTermMatrix(ppgp, control=list(wordLengths=c(3, Inf)))
dtmr_ppgp

freq <- colSums(as.matrix(dtmr_ppgp))
length(freq)
ord <- order(freq,decreasing=TRUE)
freq[head(ord)]
freq[tail(ord)]

#frequencia de termos
findFreqTerms(dtmr_ppgp,lowfreq=5)

#visualização gráfico de freq. termos
wf=data.frame(term=names(freq),occurrences=freq)
wf
p <- ggplot(subset(wf, freq>4), aes(term, occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

#wordcloud
#setting the same seed each time ensures consistent look across clouds
set.seed(30)
#limit words by specifying min frequency
wordcloud(names(freq),freq, max.words =100,min.freq=5,#scale=c(1,.01), 
          random.order = F,random.color=F,rot.per=.5,colors = freq)