# aula prática 1: análise qualitativa avançada  - https://github.com/wagnerLM/text_mining
#carregando textos
#manipulando textos
#descrição de textos por frequência de termos

#instalando e ativando os pacotes:
# install.packages("tm")
# install.packages("SnowballC")
# install.packages("wordcloud")
# install.packages("devtools")
# install.packages("ggplot2")
# install.packages("rlang")
# devtools::install_github("dfalbel/PTtextmining")

#load packages
library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(ggplot2)
library(PTtextmining)
library(rlang)
library(lsa)


data<-read.csv("/Users/Joao/Downloads/Titulos_csv2.csv",
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
corpus_ef<-VCorpus(VectorSource(cleaned_data[,1][cleaned_data$area=="Educacao fisica"]))
corpus_ef

#outras correções no texto
corpus_ef <- tm_map(corpus_ef, content_transformer(function(x) gsub(x,
               pattern = "atividade fisica", replacement = "atividade_fisica")))
corpus_ef <- tm_map(corpus_ef, content_transformer(function(x) gsub(x,
               pattern = "aptidao fisica", replacement = "aptidao_fisica")))
corpus_ef <- tm_map(corpus_ef, content_transformer(function(x) gsub(x,
               pattern = "dor fisica", replacement = "dor")))
corpus_ef <- tm_map(corpus_ef, content_transformer(function(x) gsub(x,
               pattern = "treinamento fisica", replacement = "treinamento")))
# corpus_ef <- tm_map(corpus_ef, content_transformer(function(x) gsub(x,
#                pattern = "atividade", replacement = "atividade_fisica")))
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "exercicio fisica", replacement = "exercicio")
# corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
#                pattern = "exercicio", replacement = "exercicio_fisica")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "educacao fisica", replacement = "educacao_fisica")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "condicionamento fisica", replacement = "condicionamento")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "qualidade vida", replacement = "qualidade_vida")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "estilo vida", replacement = "estilo_vida")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "efeitos", replacement = "efeito")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "adaptacao", replacement = "adaptado")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "adolescencia", replacement = "adolescentes")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "aerobias", replacement = "aerobio")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "arbitros", replacement = "arbitragem")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "aspectos psicologicos", replacement = "aspectos_psicologicos")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "atleta", replacement = "atletas")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "cardiorrespiratoria", replacement = "cardiaca")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "cardiorrespiratorios", replacement = "cardiaca")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "cardiometabolicos", replacement = "cardiaca")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "cardiopulmonar", replacement = "cardiaca")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "cardiovasculares", replacement = "cardiaca")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "santa catarina", replacement = "santa_catarina")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "sao paulo", replacement = "sao_paulo")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "cognicao", replacement = "cognitivas")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "comportamento", replacement = "comportamental")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "comportamentais", replacement = "comportamental")
# corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
#                pattern = "condicionamento", replacement = "condicionamento_fisica")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "decisao", replacement = "decisoes")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "amputados", replacement = "deficiencia")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "desportos", replacement = "esporte")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "emocionais", replacement = "emocional")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "alunos", replacement = "escolares")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "espiritualidade", replacement = "religiosidade")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "fisiologico", replacement = "fisiologicas")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "funcao", replacement = "funcional")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "funcionais", replacement = "funcional")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "funcionalidade", replacement = "funcional")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "funcoes", replacement = "funcional")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "hiperdia", replacement = "hipertensa")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "hipertensos", replacement = "hipertensa")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "imuno", replacement = "imunologicos")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "inflamacao", replacement = "inflamatorios")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "instituicoes", replacement = "instituto")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "instrumentacao", replacement = "instrumentos")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "intervencoes", replacement = "intervencao")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "jogadores", replacement = "atletas")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "juvenis", replacement = "jovens")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "lesao", replacement = "lesoes")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "lesados", replacement = "lesoes")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "lombalgia", replacement = "dor")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "marcador", replacement = "marcadores")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "marcas", replacement = "marcadores")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "metabolica", replacement = "metabolicos")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "modalidadades", replacement = "modalidades")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "motivacionais", replacement = "motivacao")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "motivacional", replacement = "motivacao")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "motivadores", replacement = "motivacao")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "motoras", replacement = "motor")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "motores", replacement = "motor")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "muscle", replacement = "muscular")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "neuromusculares", replacement = "musculo")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "neuromusculares", replacement = "musculo")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "oncologia", replacement = "doenca")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "paralimpicos", replacement = "deficiencia")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "participantes", replacement = "participacao")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "percepcoes", replacement = "percepcao")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "precursores", replacement = "preliminares")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "profissional", replacement = "profissionais")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "psicofisica", replacement = "psicobiologicas")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "psicofisiologico", replacement = "psicobiologicas")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "psicossociais", replacement = "psicologicas")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "psicossocial", replacement = "psicologicas")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "questionario", replacement = "intrumentos")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "recuperacao", replacement = "reabilitacao")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "recuperacao", replacement = "reabilitacao")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "competitiva", replacement = "rendimento")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "resqt", replacement = "restq")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "sadios", replacement = "saudaveis")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "satisfaction", replacement = "satisfacao")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "scale", replacement = "intrumentos")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "overtraining", replacement = "sobrecarga")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "teorico", replacement = "teoria")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "trabalhadores", replacement = "trabalhador")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "trabalho", replacement = "trabalhador")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "tecnico", replacement = "treinador")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "treinadores", replacement = "treinador")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "treino", replacement = "treinamento")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "universitaria", replacement = "estudantes")
corpus_ef <- tm_map(corpus_ef, content_transformer(gsub),
               pattern = "universitarios", replacement = "estudantes")

#exclusão de stopwords manulamente
# corpus_ef <- tm_map(corpus_ef, removeWords, c("acaso",
#                                               "acinte",
#                                               "alto",
#                                               "relacao",
#                                               "relacionados",
#                                               "sujeitos",
#                                               "relacoes",
#                                               "variaveis",
#                                               "variavel",
#                                               "alta",
#                                               "niveis",
#                                               "influencia",
#                                               "parametros",
#                                               "pre",
#                                               "analise",
#                                               "individuos",
#                                               "pioram",
#                                               "melhoria",
#                                               "perfil",
#                                               "lafisaef",
#                                               "atraves",
#                                               "adiante",
#                                               "aplicada",
#                                               "iefd",
#                                               "iii",
#                                               "atribuidos",
#                                               "explicativo",
#                                               "fator",
#                                               "fatores",
#                                               "estudo",
#                                               "apoio",
#                                               "adrede",
#                                               "afinal",
#                                               "afora",
#                                               "agora",
#                                               "algures",
#                                               "alem",
#                                               "ali",
#                                               "amanha",
#                                               "antes",
#                                               "aqui",
#                                               "assim",
#                                               "atras",
#                                               "bem",
#                                               "breve",
#                                               "cedo",
#                                               "certamente",
#                                               "efetivamente",
#                                               "enfim",
#                                               "hoje",
#                                               "mal",
#                                               "mais",
#                                               "melhor",
#                                               "menos",
#                                               "muito",
#                                               "nao",
#                                               "ontem",
#                                               "pior",
#                                               "pouco",
#                                               "quanto",
#                                               "quao",
#                                               "quase",
#                                               "realmente",
#                                               "sera",
#                                               "sim",
#                                               "talvez",
#                                               "tanto",
#                                               "e",
#                                               "nem",
#                                               "mas",
#                                               "tambem",
#                                               "como",
#                                               "bem",
#                                               "porem",
#                                               "todavia",
#                                               "contudo",
#                                               "entretanto",
#                                               "entanto",
#                                               "ou",
#                                               "ora",
#                                               "quer",
#                                               "ja",
#                                               "logo",
#                                               "portanto",
#                                               "por",
#                                               "assim",
#                                               "conseguinte",
#                                               "que",
#                                               "porque",
#                                               "porquanto",
#                                               "pois",
#                                               "sendo",
#                                               "visto",
#                                               "como",
#                                               "tal",
#                                               "tao",
#                                               "tanto",
#                                               "assim",
#                                               "conforme",
#                                               "segundo",
#                                               "consoante",
#                                               "mesmo",
#                                               "mais",
#                                               "ainda",
#                                               "se",
#                                               "bem",
#                                               "embora",
#                                               "se",
#                                               "caso",
#                                               "contanto",
#                                               "salvo",
#                                               "medida",
#                                               "quanto",
#                                               "fim",
#                                               "quando",
#                                               "enquanto",
#                                               "sempre",
#                                               "depois",
#                                               "a",
#                                               "ante",
#                                               "apos",
#                                               "ate",
#                                               "com",
#                                               "contra",
#                                               "de",
#                                               "desde",
#                                               "para",
#                                               "per",
#                                               "perante",
#                                               "por",
#                                               "sem",
#                                               "sob",
#                                               "sobre",
#                                               "tras",
#                                               "algo",
#                                               "alguem",
#                                               "algum",
#                                               "alguns",
#                                               "cada",
#                                               "cujo",
#                                               "muitos",
#                                               "nada",
#                                               "nenhum",
#                                               "nenhuns",
#                                               "ninguem",
#                                               "outrem",
#                                               "outros",
#                                               "poucos",
#                                               "quaisquer",
#                                               "qualquer",
#                                               "quantos",
#                                               "quem",
#                                               "tantos",
#                                               "todos",
#                                               "tudo",
#                                               "que",
#                                               "nao",
#                                               "para",
#                                               "varios",
#                                               "de",
#                                               "a",
#                                               "o",
#                                               "que",
#                                               "e",
#                                               "do",
#                                               "da",
#                                               "em",
#                                               "um",
#                                               "para",
#                                               "com",
#                                               "nao",
#                                               "uma",
#                                               "os",
#                                               "no",
#                                               "se",
#                                               "na",
#                                               "por",
#                                               "mais",
#                                               "as",
#                                               "dos",
#                                               "como",
#                                               "mas",
#                                               "ao",
#                                               "ele",
#                                               "das",
#                                               "a",
#                                               "seu",
#                                               "sua",
#                                               "ou",
#                                               "quando",
#                                               "muito",
#                                               "nos",
#                                               "ja",
#                                               "eu",
#                                               "tambem",
#                                               "so",
#                                               "pelo",
#                                               "pela",
#                                               "ate",
#                                               "isso",
#                                               "ela",
#                                               "entre",
#                                               "depois",
#                                               "sem",
#                                               "mesmo",
#                                               "aos",
#                                               "seus",
#                                               "quem",
#                                               "nas",
#                                               "me",
#                                               "esse",
#                                               "eles",
#                                               "voce",
#                                               "essa",
#                                               "num",
#                                               "nem",
#                                               "suas",
#                                               "meu",
#                                               "as",
#                                               "minha",
#                                               "numa",
#                                               "pelos",
#                                               "elas",
#                                               "qual",
#                                               "nos",
#                                               "lhe",
#                                               "deles",
#                                               "essas",
#                                               "esses",
#                                               "pelas",
#                                               "este",
#                                               "dele",
#                                               "tu",
#                                               "te",
#                                               "voces",
#                                               "vos",
#                                               "lhes",
#                                               "meus",
#                                               "minhas",
#                                               "teu",
#                                               "tua",
#                                               "teus",
#                                               "tuas",
#                                               "nosso",
#                                               "nossa",
#                                               "nossos",
#                                               "nossas",
#                                               "dela",
#                                               "delas",
#                                               "esta",
#                                               "estes",
#                                               "estas",
#                                               "aquele",
#                                               "aquela",
#                                               "aqueles",
#                                               "aquelas",
#                                               "isto",
#                                               "aquilo",
#                                               "efeito",
#                                               "efeitos",
#                                               "estou",
#                                               "esta",
#                                               "estamos",
#                                               "estao",
#                                               "estive",
#                                               "esteve",
#                                               "estivemos",
#                                               "estiveram",
#                                               "estava",
#                                               "estavamos",
#                                               "estavam",
#                                               "estivera",
#                                               "estiveramos",
#                                               "esteja",
#                                               "estejamos",
#                                               "estejam",
#                                               "estivesse",
#                                               "estivessemos",
#                                               "estivessem",
#                                               "estiver",
#                                               "estivermos",
#                                               "estiverem",
#                                               "hei",
#                                               "ha",
#                                               "havemos",
#                                               "hao",
#                                               "houve",
#                                               "houvemos",
#                                               "houveram",
#                                               "houvera",
#                                               "houveramos",
#                                               "haja",
#                                               "hajamos",
#                                               "hajam",
#                                               "houvesse",
#                                               "houvessemos",
#                                               "houvessem",
#                                               "houver",
#                                               "houvermos",
#                                               "houverem",
#                                               "houverei",
#                                               "houvera",
#                                               "houveremos",
#                                               "houverao",
#                                               "houveria",
#                                               "houveriamos",
#                                               "houveriam",
#                                               "sou",
#                                               "somos",
#                                               "sao",
#                                               "era",
#                                               "eramos",
#                                               "eram",
#                                               "fui",
#                                               "foi",
#                                               "fomos",
#                                               "foram",
#                                               "fora",
#                                               "foramos",
#                                               "seja",
#                                               "sejamos",
#                                               "sejam",
#                                               "fosse",
#                                               "fossemos",
#                                               "fossem",
#                                               "for",
#                                               "formos",
#                                               "forem",
#                                               "serei",
#                                               "sera",
#                                               "seremos",
#                                               "serao",
#                                               "seria",
#                                               "seriamos",
#                                               "seriam",
#                                               "tenho",
#                                               "tem",
#                                               "temos",
#                                               "tem",
#                                               "tinha",
#                                               "tinhamos",
#                                               "tinham",
#                                               "tive",
#                                               "teve",
#                                               "tivemos",
#                                               "tiveram",
#                                               "tivera",
#                                               "tiveramos",
#                                               "tenha",
#                                               "tenhamos",
#                                               "tenham",
#                                               "tivesse",
#                                               "tivessemos",
#                                               "tivessem",
#                                               "tiver",
#                                               "tivermos",
#                                               "tiverem",
#                                               "terei",
#                                               "tera",
#                                               "teremos",
#                                               "terao",
#                                               "teria",
#                                               "teriamos",
#                                               "teriam",
#                                               "pessoas",
#                                               "durante",
#                                               "nivel"))

#selecionando o texto para visualizar
writeLines(as.character(corpus_ef))

#lista de transformações
getTransformations()

#cria uma matriz de documentos e termos (https://en.wikipedia.org/wiki/Document-term_matrix)
dtm_ppgp <- DocumentTermMatrix(corpus_ef)
dtm_ppgp
#visualizando a matriz
# inspect(dtm_ppgp)

# dtm_ppgp <- removeSparseTerms(dtm_ppgp, 0.93)
# dtm_ppgp

#verificando a frequência de palavras
freq <- colSums(as.matrix(dtm_ppgp))
freq
length(freq)
ord <- order(freq,decreasing=TRUE)
freq[head(ord)]
freq[tail(ord)]

#recriando a dtm excluindo palavras pequenas
# dtmr_ppgp <-DocumentTermMatrix(corpus_ef)#, control=list(wordLengths=c(3, Inf)))
# dtmr_ppgp

# freq <- colSums(as.matrix(dtmr_ppgp))
# length(freq)
# ord <- order(freq,decreasing=TRUE)
# freq[head(ord)]
# freq[tail(ord)]

#frequencia de termos
findFreqTerms(dtmr_ppgp,lowfreq=5)

#visualização gráfico de freq. termos
wf=data.frame(word=names(freq),freq)

wf<-wf[wf$freq>10,]

p <- ggplot(wf, aes(x = reorder(word, -freq), y = freq)) +
            geom_bar(stat = "identity") +
            theme(axis.text.x=element_text(angle=45, hjust=1))
p   


#wordcloud
#setting the same seed each time ensures consistent look across clouds
set.seed(30)
#limit words by specifying min frequency
wordcloud(names(freq),freq,min.freq=5,#scale=c(1,.01), max.words =100
          random.order = F,random.color=F,rot.per=.5,colors = freq)



# para comparar as técnicas, primeiro uma análise de escalonamento multidimensional
# com a matriz bruta dos dados
td.mat <- as.matrix(t(as.matrix(dtm_ppgp)))
#View(td.mat)
# a distância é calculada com a matriz transposta, aquele "t" antes da matriz anterior
#View(t(as.matrix(td.mat)))
# dist.mat <- dist(as.matrix(dtm_ppgp))
# verificando a matriz
# dist.mat

# Escalonamento Multidimensional, em 2 dimensões (k=2)
# fit <- cmdscale(dist.mat, eig=TRUE, k=2)
# # criando um gráfico com ggplot2
# points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])
# ggplot(points, aes(x=x,y=y)) + 
#   geom_point(data=points,aes(x=x, y=y)) #+ 
  # geom_text(data=points,aes(x=x, y=y-0.2, label=row.names(cleaned_data[,1])))


# Escalonamento Multidimensional com LSA
# Ponderando os termos em peso local (lw) e peso global (gw)
td.mat.lsa <- lw_bintf(td.mat) * gw_idf(td.mat) 
#View(td.mat.lsa)
# criando o espaço latente (M = T S t(D); a matriz M é o produto das matrizes de termos "T", documentos "D" e a diagonal "S"
# de valores singulares
lsaSpace <- lsa(td.mat.lsa)
#View(lsaSpace)
# calculando as distâncias
dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace))) 
dist.mat.lsa

# df <- data.frame(data, stringsAsFactors=FALSE)

# Escalonamento Multidimensional com LSA, duas dimensões (k=2)
fit <- cmdscale(dist.mat.lsa, eig=TRUE, k=2)
# gerando o gráfico
points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])
ggplot(points,aes(x=x, y=y)) + 
  geom_point(data=points,aes(x=x, y=y)) #+ 
  # geom_text(data=points,aes(x=x, y=y-0.2))#, label=row.names(df)))


#Kmeans 
# install.packages("skmeans")
library(skmeans)

clust<-kmeans(points,10) #dividir estes dados em 3 clusters 
clust
plot(points[,1],points[,2],col= clust$cluster)
text(points[,1],points[,2],row.names(df))


#Model Based Clustering
# install.packages("mclust")
library(mclust)
fit2 <- Mclust(points)

#Plot das classifica??es com base em Escalonamento Multidimensional com LSA, duas dimens?es (k=2)
plot(points[,1],points[,2],col= fit2$classification)

#numero de classifica??es
summary(fit2)


class <- clust$cluster


#Tabela final com as classifica??es 
table_final = data.frame(originalText = cleaned_data[,1][cleaned_data$area=="Educacao fisica"],
                   # PointX  = fit$points[,1],
                   # PointY  = fit$points[,2],
                    class =  clust$cluster)
#View(table_final)

#write.csv2(table_final,"table_final.csv",row.names = F)

#associações 
findAssocs(dtm_ppgp,"fisica",corlimit = 0.3)
#dtm2<-removeSparseTerms(dtm,0.99)
cor_t<- cor(as.matrix(dtm_ppgp),method = "spearman")
#View(cor_t)
qgraph(cor_t,layout="spring",labels=colnames(cor_t),threshold=0.3)


# Análise por cluster
#Corpus
# Análise por cluster
#Corpus
df_c1 <- data.frame(table_final$originalText[which(table_final$class==1)], stringsAsFactors=FALSE)
View(df_c1)
corpus_c1 <- VCorpus(VectorSource(df_c1$table_final.originalText.which.table_final.class....1..))
corpus_c1
dtm_c1 <- DocumentTermMatrix(corpus_c1)

# descritivos por cluster
freq_c1 <- sort(colSums(as.matrix(dtm_c1)), decreasing=TRUE)   
head(freq_c1, 15)

wf_c1 <- data.frame(word=names(freq_c1), freq=freq_c1)   
head(wf_c1)  

p_c1 <- ggplot(subset(wf_c1, freq>5), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))
p_c1  

set.seed(142)   
wordcloud(names(freq_c1), freq_c1, min.freq=20)

#dtm_c1_2<-removeSparseTerms(dtm_c1,0.99)
cor_c1 <- cor(as.matrix(dtm_c1),method = "spearman")
#View(cor_c1)
qgraph(cor_c1,layout="spring",labels=colnames(cor_c1),threshold=0.3)


# Análise por cluster
#Corpus
df_c2 <- data.frame(table_final$originalText[which(table_final$class==2)], stringsAsFactors=FALSE)
View(df_c2)
corpus_c2 <- VCorpus(VectorSource(df_c2$table_final.originalText.which.table_final.class....2..))
corpus_c2
dtm_c2 <- DocumentTermMatrix(corpus_c2)

# descritivos por cluster
freq_c2 <- sort(colSums(as.matrix(dtm_c2)), decreasing=TRUE)   
head(freq_c2, 15)

wf_c2 <- data.frame(word=names(freq_c2), freq=freq_c2)   
head(wf_c2)  

p_c1 <- ggplot(subset(wf_c2, freq>1), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))
p_c1  

set.seed(142)   
wordcloud(names(freq_c2), freq_c2, min.freq=20)

#dtm_c2_2<-removeSparseTerms(dtm_c2,0.99)
cor_c2 <- cor(as.matrix(dtm_c2),method = "spearman")
#View(cor_c2)
qgraph(cor_c2,layout="spring",labels=colnames(cor_c2),threshold=0.4)

# Análise por cluster
#Corpus
df_c3 <- data.frame(table_final$originalText[which(table_final$class==3)], stringsAsFactors=FALSE)
View(df_c3)
corpus_c3 <- VCorpus(VectorSource(df_c3$table_final.originalText.which.table_final.class....3..))
corpus_c3
dtm_c3 <- DocumentTermMatrix(corpus_c3)

# descritivos por cluster
freq_c3 <- sort(colSums(as.matrix(dtm_c3)), decreasing=TRUE)   
head(freq_c3, 15)

wf_c3 <- data.frame(word=names(freq_c3), freq=freq_c3)   
head(wf_c3)  

p_c1 <- ggplot(subset(wf_c3, freq>1), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))
p_c1  

set.seed(142)   
wordcloud(names(freq_c3), freq_c3, min.freq=20)

#dtm_c3_2<-removeSparseTerms(dtm_c3,0.99)
cor_c3 <- cor(as.matrix(dtm_c3),method = "spearman")
#View(cor_c3)
qgraph(cor_c3,layout="spring",labels=colnames(cor_c3))

# Análise por cluster
#Corpus
df_c4 <- data.frame(table_final$originalText[which(table_final$class==4)], stringsAsFactors=FALSE)
View(df_c4)
corpus_c4 <- VCorpus(VectorSource(df_c4$table_final.originalText.which.table_final.class....4..))
corpus_c4
dtm_c4 <- DocumentTermMatrix(corpus_c4)

# descritivos por cluster
freq_c4 <- sort(colSums(as.matrix(dtm_c4)), decreasing=TRUE)   
head(freq_c4, 15)

wf_c4 <- data.frame(word=names(freq_c4), freq=freq_c4)   
head(wf_c4)  

p_c1 <- ggplot(subset(wf_c4, freq>1), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))
p_c1  

set.seed(142)   
wordcloud(names(freq_c4), freq_c4, min.freq=20)

#dtm_c4_2<-removeSparseTerms(dtm_c4,0.99)
cor_c4 <- cor(as.matrix(dtm_c4),method = "spearman")
#View(cor_c4)
qgraph(cor_c4,layout="spring",labels=colnames(cor_c4),threshold=0.4)

# Análise por cluster
#Corpus
df_c5 <- data.frame(table_final$originalText[which(table_final$class==5)], stringsAsFactors=FALSE)
View(df_c5)
corpus_c5 <- VCorpus(VectorSource(df_c5$table_final.originalText.which.table_final.class....5..))
corpus_c5
dtm_c5 <- DocumentTermMatrix(corpus_c5)

# descritivos por cluster
freq_c5 <- sort(colSums(as.matrix(dtm_c5)), decreasing=TRUE)   
head(freq_c5, 15)

wf_c5 <- data.frame(word=names(freq_c5), freq=freq_c5)   
head(wf_c5)  

p_c1 <- ggplot(subset(wf_c5, freq>3), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))
p_c1  

set.seed(142)   
wordcloud(names(freq_c5), freq_c5, min.freq=20)

#dtm_c5_2<-removeSparseTerms(dtm_c5,0.99)
cor_c5 <- cor(as.matrix(dtm_c5),method = "spearman")
#View(cor_c5)
qgraph(cor_c5,layout="spring",labels=colnames(cor_c5),threshold=0.4)

# Análise por cluster
#Corpus
df_c6 <- data.frame(table_final$originalText[which(table_final$class==6)], stringsAsFactors=FALSE)
# View(df_c6)
corpus_c6 <- VCorpus(VectorSource(df_c6$table_final.originalText.which.table_final.class....6..))
corpus_c6
dtm_c6 <- DocumentTermMatrix(corpus_c6)

# descritivos por cluster
freq_c6 <- sort(colSums(as.matrix(dtm_c6)), decreasing=TRUE)   
head(freq_c6, 15)

wf_c6 <- data.frame(word=names(freq_c6), freq=freq_c6)   
head(wf_c6)  

p_c1 <- ggplot(subset(wf_c6, freq>3), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))
p_c1  

set.seed(142)   
wordcloud(names(freq_c6), freq_c6, min.freq=20)

#dtm_c6_2<-removeSparseTerms(dtm_c6,0.99)
cor_c6 <- cor(as.matrix(dtm_c6),method = "spearman")
#View(cor_c6)
qgraph(cor_c6,layout="spring",labels=colnames(cor_c6),threshold=0.4)

# Análise por cluster
#Corpus
df_c7 <- data.frame(table_final$originalText[which(table_final$class==7)], stringsAsFactors=FALSE)
# View(df_c7)
corpus_c7 <- VCorpus(VectorSource(df_c7$table_final.originalText.which.table_final.class....7..))
corpus_c7
dtm_c7 <- DocumentTermMatrix(corpus_c7)

# descritivos por cluster
freq_c7 <- sort(colSums(as.matrix(dtm_c7)), decreasing=TRUE)   
head(freq_c7, 15)

wf_c7 <- data.frame(word=names(freq_c7), freq=freq_c7)   
head(wf_c7)  

p_c1 <- ggplot(subset(wf_c7, freq>1), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))
p_c1  

set.seed(142)   
wordcloud(names(freq_c7), freq_c7, min.freq=20)

#dtm_c7_2<-removeSparseTerms(dtm_c7,0.99)
cor_c7 <- cor(as.matrix(dtm_c7),method = "spearman")
#View(cor_c7)
qgraph(cor_c7,layout="spring",labels=colnames(cor_c7),threshold=0.4)

# Análise por cluster
#Corpus
df_c8 <- data.frame(table_final$originalText[which(table_final$class==8)], stringsAsFactors=FALSE)
# View(df_c8)
corpus_c8 <- VCorpus(VectorSource(df_c8$table_final.originalText.which.table_final.class....8..))
corpus_c8
dtm_c8 <- DocumentTermMatrix(corpus_c8)

# descritivos por cluster
freq_c8 <- sort(colSums(as.matrix(dtm_c8)), decreasing=TRUE)   
head(freq_c8, 15)

wf_c8 <- data.frame(word=names(freq_c8), freq=freq_c8)   
head(wf_c8)  

p_c1 <- ggplot(subset(wf_c8, freq>1), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))
p_c1  

set.seed(142)   
wordcloud(names(freq_c8), freq_c8, min.freq=20)

#dtm_c8_2<-removeSparseTerms(dtm_c8,0.99)
cor_c8 <- cor(as.matrix(dtm_c8),method = "spearman")
#View(cor_c8)
qgraph(cor_c8,layout="spring",labels=colnames(cor_c8),threshold=0.4)

# Análise por cluster
#Corpus
df_c9 <- data.frame(table_final$originalText[which(table_final$class==9)], stringsAsFactors=FALSE)
# View(df_c9)
corpus_c9 <- VCorpus(VectorSource(df_c9$table_final.originalText.which.table_final.class....9..))
corpus_c9
dtm_c9 <- DocumentTermMatrix(corpus_c9)

# descritivos por cluster
freq_c9 <- sort(colSums(as.matrix(dtm_c9)), decreasing=TRUE)   
head(freq_c9, 15)

wf_c9 <- data.frame(word=names(freq_c9), freq=freq_c9)   
head(wf_c9)  

p_c1 <- ggplot(subset(wf_c9, freq>1), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))
p_c1  

set.seed(142)   
wordcloud(names(freq_c9), freq_c9, min.freq=20)

#dtm_c9_2<-removeSparseTerms(dtm_c9,0.99)
cor_c9 <- cor(as.matrix(dtm_c9),method = "spearman")
#View(cor_c9)
qgraph(cor_c9,layout="spring",labels=colnames(cor_c9),threshold=0.4)

# Análise por cluster
#Corpus
df_c10 <- data.frame(table_final$originalText[which(table_final$class==10)], stringsAsFactors=FALSE)
# View(df_c10)
corpus_c10 <- VCorpus(VectorSource(df_c10$table_final.originalText.which.table_final.class....10..))
corpus_c10
dtm_c10 <- DocumentTermMatrix(corpus_c10)

# descritivos por cluster
freq_c10 <- sort(colSums(as.matrix(dtm_c10)), decreasing=TRUE)   
head(freq_c10, 15)

wf_c10 <- data.frame(word=names(freq_c10), freq=freq_c10)   
head(wf_c10)  

p_c1 <- ggplot(subset(wf_c10, freq>3), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))
p_c1  

set.seed(142)   
wordcloud(names(freq_c10), freq_c10, min.freq=20)

#dtm_c10_2<-removeSparseTerms(dtm_c10,0.99)
cor_c10 <- cor(as.matrix(dtm_c10),method = "spearman")
#View(cor_c10)
qgraph(cor_c10,layout="spring",labels=colnames(cor_c10),threshold=0.4)

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