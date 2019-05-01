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
# install.packag3s("rlang")
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
library(qgraph)
library(skmeans)
library(mclust)

data<-read.csv("/Users/joaovissoci/Downloads/Titulos_csv2 (1).csv",
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
corpus_psi<-VCorpus(VectorSource(cleaned_data[,1][cleaned_data$area=="Psicologia"]))
corpus_psi

#outras correções no texto
corpus_psi <- tm_map(corpus_psi, content_transformer(function(x) gsub(x,
               pattern = "dor fisica", replacement = "dor")))
corpus_psi <- tm_map(corpus_psi, content_transformer(function(x) gsub(x,
               pattern = "atividade fisica", replacement = "atividade_fisica")))
corpus_psi <- tm_map(corpus_psi, content_transformer(function(x) gsub(x,
               pattern = "aptidao fisica", replacement = "aptidao_fisica")))
corpus_psi <- tm_map(corpus_psi, content_transformer(function(x) gsub(x,
               pattern = "dor fisica", replacement = "dor")))
corpus_psi <- tm_map(corpus_psi, content_transformer(function(x) gsub(x,
               pattern = "treinamento fisica", replacement = "treinamento")))
# corpus_psi <- tm_map(corpus_psi, content_transformer(function(x) gsub(x,
#                pattern = "atividade", replacement = "atividade_fisica")))
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "exercicio fisica", replacement = "exercicio")
# corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
#                pattern = "exercicio", replacement = "exercicio_fisica")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "educacao fisica", replacement = "educacao_fisica")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "educacao fisiica", replacement = "educacao_fisica")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "condicionamento fisica", replacement = "condicionamento")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "qualidade vida", replacement = "qualidade_vida")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "estilo vida", replacement = "estilo_vida")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "efeitos", replacement = "efeito")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "adaptacao", replacement = "adaptado")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "adolescencia", replacement = "adolescentes")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "aerobias", replacement = "aerobio")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "arbitros", replacement = "arbitragem")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "aspectos psicologicos", replacement = "psicologicos")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "atletas", replacement = "atleta")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "cardiorrespiratoria", replacement = "cardiaca")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "cardiorrespiratorios", replacement = "cardiaca")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "cardiometabolicos", replacement = "cardiaca")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "cardiopulmonar", replacement = "cardiaca")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "cardiovasculares", replacement = "cardiaca")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "santa catarina", replacement = "santa_catarina")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "sao paulo", replacement = "sao_paulo")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "cognicao", replacement = "cognitivas")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "comportamento", replacement = "comportamental")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "comportamentais", replacement = "comportamental")
# corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
#                pattern = "condicionamento", replacement = "condicionamento_fisica")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "decisao", replacement = "decisoes")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "amputados", replacement = "deficiencia")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "desportos", replacement = "esporte")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "emocionais", replacement = "emocional")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "alunos", replacement = "escolares")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "espiritualidade", replacement = "religiosidade")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "fisiologico", replacement = "fisiologicas")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "funcao", replacement = "funcional")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "funcionais", replacement = "funcional")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "funcionalidade", replacement = "funcional")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "funcoes", replacement = "funcional")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "hiperdia", replacement = "hipertensa")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "hipertensos", replacement = "hipertensa")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "imuno", replacement = "imunologicos")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "inflamacao", replacement = "inflamatorios")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "instituicoes", replacement = "instituto")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "instrumentacao", replacement = "instrumentos")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "intervencoes", replacement = "intervencao")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "jogadores", replacement = "atletas")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "juvenis", replacement = "jovens")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "lesao", replacement = "lesoes")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "lesados", replacement = "lesoes")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "lombalgia", replacement = "dor")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "marcador", replacement = "marcadores")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "marcas", replacement = "marcadores")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "metabolica", replacement = "metabolicos")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "modalidadades", replacement = "modalidades")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "motivacionais", replacement = "motivacao")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "motivacional", replacement = "motivacao")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "motivadores", replacement = "motivacao")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "motoras", replacement = "motor")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "motores", replacement = "motor")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "muscle", replacement = "muscular")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "neuromusculares", replacement = "musculo")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "neuromusculares", replacement = "musculo")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "oncologia", replacement = "doenca")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "paralimpicos", replacement = "deficiencia")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "participantes", replacement = "participacao")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "percepcoes", replacement = "percepcao")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "precursores", replacement = "preliminares")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "profissional", replacement = "profissionais")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "psicofisica", replacement = "psicobiologicas")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "psicofisiologico", replacement = "psicobiologicas")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "psicossociais", replacement = "psicologicas")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "psicossocial", replacement = "psicologicas")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "questionario", replacement = "instrumentos")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "recuperacao", replacement = "reabilitacao")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "recuperacao", replacement = "reabilitacao")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "competitiva", replacement = "rendimento")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "resqt", replacement = "restq")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "sadios", replacement = "saudaveis")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "satisfaction", replacement = "satisfacao")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "scale", replacement = "instrumentos")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "escala", replacement = "instrumentos")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "intrumentos", replacement = "instrumentos")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "overtraining", replacement = "sobrecarga")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "teorico", replacement = "teoria")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "trabalhadores", replacement = "trabalhador")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "trabalho", replacement = "trabalhador")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "tecnico", replacement = "treinador")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "treinadores", replacement = "treinador")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "treino", replacement = "treinamento")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "universitaria", replacement = "estudantes")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "universitarios", replacement = "estudantes")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "bem estar", replacement = "bemestar")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "chen yang tai chi chuan", replacement = "exercicio")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "futebol de campo", replacement = "futebol esporte")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "futebol campo", replacement = "futebol esporte")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "futsal", replacement = "futebol esporte")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "football", replacement = "futebol esporte")
# corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               # pattern = "cognitiva comportamental", replacement = "cognitiva_comportamental")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "padrao sono", replacement = "padrao_sono")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "avaliando", replacement = "avaliacao")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "brinquedos", replacement = "brincar")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "brinquedoteca", replacement = "brincar")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "ludica", replacement = "brincar")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "caracterizacao", replacement = "caracteristicas")
# corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               # pattern = "categorizacao", replacement = "caracteristicas")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "constribuicoes", replacement = "contribuicoes")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "cultural", replacement = "cultura")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "cyberbulling", replacement = "cyberbullying")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "depressivos", replacement = "depressao")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "desportivos", replacement = "esporte")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "educacionais", replacement = "educacao")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "pedagogicas", replacement = "educacao")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "educativos", replacement = "educacao")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "ensino", replacement = "educacao")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "metodologias", replacement = "metodos")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "rendimento", replacement = "performance")
# corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
#                pattern = "sociais", replacement = "social")
# corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
#                pattern = "socializacao", replacement = "social")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "jogos eletronicos", replacement = "video game")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "jogos online", replacement = "video game")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "jogos computacionais", replacement = "video game")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "domino", replacement = "video game")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "jogos", replacement = "video game")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "redes sociais", replacement = "redes_sociais")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "rede sociais", replacement = "redes_sociais")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "estar subjetivo", replacement = "bemestarsubjetivo")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "yoga", replacement = "yoga esporte")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "natacao", replacement = "natacao esporte")
corpus_psi <- tm_map(corpus_psi, content_transformer(gsub),
               pattern = "voleibol", replacement = "natacao esporte")

#exclusão de stopwords manulamente
corpus_psi <- tm_map(corpus_psi, removeWords, c("levantador", "nacional","rio","grande","sul","kundalini","impacto","acesso","processo","recife","internalizantes","callejero", "populacao","pessoas","interfaces","grande","dois","diferentes","correlacao","longitudinal","sindrome","freq","uencia","sub","brasileiros","restq","acaso","pos","estado","acinte","alto","relacao","relacionados","sujeitos","relacoes","variaveis","variavel","alta","niveis","influencia","parametros","pre","analise","individuos","pioram","melhoria","perfil","lafisaef","atraves","adiante","aplicada","iefd","iii","atribuidos","explicativo","fator","fatores","estudo","apoio","adrede","afinal","afora","agora","algures","alem","ali","amanha","antes","aqui","assim","atras","bem","breve","cedo","certamente","efetivamente","enfim","hoje","mal","mais","melhor","menos","muito","nao","ontem","pior","pouco","quanto","quao","quase","realmente","sera","sim","talvez","tanto","e","nem","mas","tambem","como","bem","porem","todavia","contudo","entretanto","entanto","ou","ora","quer","ja","logo","portanto","por","assim","conseguinte","que","porque","porquanto","pois","sendo","visto","como","tal","tao","tanto","assim","conforme","segundo","consoante","mesmo","mais","ainda","se","bem","embora","se","caso","contanto","salvo","medida","quanto","fim","quando","enquanto","sempre","depois","a","ante","apos","ate","com","contra","de","desde","para","per","perante","por","sem","sob","sobre","tras","algo","alguem","algum","alguns","cada","cujo","muitos","nada","nenhum","nenhuns","ninguem","outrem","outros","poucos","quaisquer","qualquer","quantos","quem","tantos","todos","tudo","que","nao","para","varios","de","a","o","que","e","do","da","em","um","para","com","nao","uma","os","no","se","na","por","mais","as","dos","como","mas","ao","ele","das","a","seu","sua","ou","quando","muito","nos","ja","eu","tambem","so","pelo","pela","ate","isso","ela","entre","depois","sem","mesmo","aos","seus","quem","nas","me","esse","eles","voce","essa","num","nem","suas","meu","as","minha","numa","pelos","elas","qual","nos","lhe","deles","essas","esses","pelas","este","dele","tu","te","voces","vos","lhes","meus","minhas","teu","tua","teus","tuas","nosso","nossa","nossos","nossas","dela","delas","esta","estes","estas","aquele","aquela","aqueles","aquelas","isto","aquilo","efeito","efeitos","estou","esta","estamos","estao","estive","esteve","estivemos","estiveram","estava","estavamos","estavam","estivera","estiveramos","esteja","estejamos","estejam","estivesse","estivessemos","estivessem","estiver","estivermos","estiverem","hei","ha","havemos","hao","houve","houvemos","houveram","houvera","houveramos","haja","hajamos","hajam","houvesse","houvessemos","houvessem","houver","houvermos","houverem","houverei","houvera","houveremos","houverao","houveria","houveriamos","houveriam","sou","somos","sao","era","eramos","eram","fui","foi","fomos","foram","fora","foramos","seja","sejamos","sejam","fosse","fossemos","fossem","for","formos","forem","serei","sera","seremos","serao","seria","seriamos","seriam","tenho","tem","temos","tem","tinha","tinhamos","tinham","tive","teve","tivemos","tiveram","tivera","tiveramos","tenha","tenhamos","tenham","tivesse","tivessemos","tivessem","tiver","tivermos","tiverem","terei","tera","teremos","terao","teria","teriamos","teriam","pessoas","durante","nivel","sao","luiz","resposta","respostas","jose","mato","grosso","psicologicas","atletas","continua","durante","cem","status","municipio","atraves","associados","fatores","brasil","principal","novas","interna","resqt","relacoes","caracterização","apos","nao","niveis","teorico","parana","santa","catarina","iefd","uberaraba","florianopolis","usuarios","atraves","brazilian","regiao","uso","pernambuco"))

#selecionando o texto para visualizar
writeLines(as.character(corpus_psi))

#lista de transformações
# getTransformations()

#cria uma matriz de documentos e termos (https://en.wikipedia.org/wiki/Document-term_matrix)
dtm_ppgp <- DocumentTermMatrix(corpus_psi)
# dtm_ppgp
#visualizando a matriz
# inspect(dtm_ppgp)

# dtm_ppgp <- removeSparseTerms(dtm_ppgp, 0.97)
# dtm_ppgp

#verificando a frequência de palavras
freq <- colSums(as.matrix(dtm_ppgp))
freq
length(freq)
ord <- order(freq,decreasing=TRUE)
freq[head(ord)]
freq[tail(ord)]

#recriando a dtm excluindo palavras pequenas
# dtmr_ppgp <-DocumentTermMatrix(corpus_psi)#, control=list(wordLengths=c(3, Inf)))
# dtmr_ppgp

# freq <- colSums(as.matrix(dtmr_ppgp))
# length(freq)
# ord <- order(freq,decreasing=TRUE)
# freq[head(ord)]
# freq[tail(ord)]

#frequencia de termos
findFreqTerms(dtm_ppgp,lowfreq=5)

#visualização gráfico de freq. termos
wf=data.frame(word=names(freq),freq)

# wf<-wf[wf$freq>,10]

wf$contribution<-(wf$freq/39)*100

wf<-wf[wf$contribution>5,]

wf

write.csv(wf,"/Users/joaovissoci/Desktop/psy_wordfreq.csv")

p <- ggplot(wf, aes(x = reorder(word, -freq), y = freq)) +
            geom_bar(stat = "identity") +
            theme(axis.text.x=element_text(angle=45, hjust=1))
p   


#wordcloud
# setting the same seed each time ensures consistent look across clouds
set.seed(30)
#limit words by specifying min frequency
with(wf,wordcloud(word,freq,min.freq=3,#scale=c(1,.01), max.words =100
          random.order = FALSE,random.color=FALSE,rot.per=.5,colors = freq))



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
# points <- data.f3ame(x=fit$points[, 1], y=fit$points[, 2])
# ggplot(points, aes(x=x,y=y3) + 
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


#Model Based Clustering
# install.packages("mclust")
library(mclust)
# install.packages("skmeans")
library(skmeans)

wss <- (nrow(points)-1)*sum(apply(points,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(points, 
   centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
  ylab="Within groups sum of squares")

#Cluster analysis
set.seed(123456)
clust<-kmeans(points,3) #dividir estes dados em 3 clusters 
clust
plot(points[,1],points[,2],col= clust$cluster)
text(points[,1],points[,2],row.names(df))
# rect.hclust(clust, k=5, border="red")

# fit2 <- Mclust(points)

# Plot das classifica??es com base em Escalonamento Multidimensional com LSA, duas dimens?es (k=2)
# plot(points[,1],points[,2],col= fit2$classification)

# #numero de classifica??es
print(clust)

class <- clust$cluster

#Tabela final com as classifica??es 
table_final = data.frame(originalText = cleaned_data[,1][cleaned_data$area=="Psicologia"],
                   # PointX  = fit$points[,1],
                   # PointY  = fit$points[,2],
                    class =  clust$cluster)

write.csv(table_final,"/Users/joaovissoci/Desktop/psicodeleteme.csv")
########################################
#By Cluster Analysis - Cluster 1
########################################
# Análise por cluster
#Corpus
df_c1 <- data.frame(table_final$originalText[which(table_final$class==1)], stringsAsFactors=FALSE)
# View(df_c1)
corpus_c1 <- VCorpus(VectorSource(df_c1$table_final.originalText.which.table_final.class....1..))

#outras correções no texto
corpus_c1 <- tm_map(corpus_c1, content_transformer(function(x) gsub(x,
               pattern = "dor fisica", replacement = "dor")))
corpus_c1 <- tm_map(corpus_c1, content_transformer(function(x) gsub(x,
               pattern = "atividade fisica", replacement = "atividade_fisica")))
corpus_c1 <- tm_map(corpus_c1, content_transformer(function(x) gsub(x,
               pattern = "aptidao fisica", replacement = "aptidao_fisica")))
corpus_c1 <- tm_map(corpus_c1, content_transformer(function(x) gsub(x,
               pattern = "dor fisica", replacement = "dor")))
corpus_c1 <- tm_map(corpus_c1, content_transformer(function(x) gsub(x,
               pattern = "treinamento fisica", replacement = "treinamento")))
# corpus_c1 <- tm_map(corpus_c1, content_transformer(function(x) gsub(x,
#                pattern = "atividade", replacement = "atividade_fisica")))
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "exercicio fisica", replacement = "exercicio")
# corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
#                pattern = "exercicio", replacement = "exercicio_fisica")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "educacao fisica", replacement = "educacao_fisica")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "educacao fisiica", replacement = "educacao_fisica")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "condicionamento fisica", replacement = "condicionamento")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "qualidade vida", replacement = "qualidade_vida")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "estilo vida", replacement = "estilo_vida")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "efeitos", replacement = "efeito")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "adaptacao", replacement = "adaptado")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "adolescencia", replacement = "adolescentes")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "aerobias", replacement = "aerobio")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "arbitros", replacement = "arbitragem")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "aspectos psicologicos", replacement = "aspectos_psicologicos")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "atletas", replacement = "atleta")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "cardiorrespiratoria", replacement = "cardiaca")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "cardiorrespiratorios", replacement = "cardiaca")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "cardiometabolicos", replacement = "cardiaca")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "cardiopulmonar", replacement = "cardiaca")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "cardiovasculares", replacement = "cardiaca")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "santa catarina", replacement = "santa_catarina")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "sao paulo", replacement = "sao_paulo")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "cognicao", replacement = "cognitivas")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "comportamento", replacement = "comportamental")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "comportamentais", replacement = "comportamental")
# corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
#                pattern = "condicionamento", replacement = "condicionamento_fisica")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "decisao", replacement = "decisoes")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "amputados", replacement = "deficiencia")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "desportos", replacement = "esporte")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "emocionais", replacement = "emocional")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "alunos", replacement = "escolares")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "espiritualidade", replacement = "religiosidade")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "fisiologico", replacement = "fisiologicas")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "funcao", replacement = "funcional")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "funcionais", replacement = "funcional")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "funcionalidade", replacement = "funcional")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "funcoes", replacement = "funcional")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "hiperdia", replacement = "hipertensa")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "hipertensos", replacement = "hipertensa")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "imuno", replacement = "imunologicos")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "inflamacao", replacement = "inflamatorios")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "instituicoes", replacement = "instituto")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "instrumentacao", replacement = "instrumentos")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "intervencoes", replacement = "intervencao")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "jogadores", replacement = "atletas")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "juvenis", replacement = "jovens")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "lesao", replacement = "lesoes")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "lesados", replacement = "lesoes")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "lombalgia", replacement = "dor")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "marcador", replacement = "marcadores")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "marcas", replacement = "marcadores")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "metabolica", replacement = "metabolicos")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "modalidadades", replacement = "modalidades")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "motivacionais", replacement = "motivacao")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "motivacional", replacement = "motivacao")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "motivadores", replacement = "motivacao")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "motoras", replacement = "motor")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "motores", replacement = "motor")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "muscle", replacement = "muscular")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "neuromusculares", replacement = "musculo")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "neuromusculares", replacement = "musculo")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "oncologia", replacement = "doenca")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "paralimpicos", replacement = "deficiencia")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "participantes", replacement = "participacao")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "percepcoes", replacement = "percepcao")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "precursores", replacement = "preliminares")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "profissional", replacement = "profissionais")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "psicofisica", replacement = "psicobiologicas")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "psicofisiologico", replacement = "psicobiologicas")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "psicossociais", replacement = "psicologicas")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "psicossocial", replacement = "psicologicas")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "questionario", replacement = "instrumentos")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "recuperacao", replacement = "reabilitacao")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "recuperacao", replacement = "reabilitacao")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "competitiva", replacement = "rendimento")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "resqt", replacement = "restq")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "sadios", replacement = "saudaveis")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "satisfaction", replacement = "satisfacao")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "scale", replacement = "instrumentos")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "escala", replacement = "instrumentos")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "intrumentos", replacement = "instrumentos")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "overtraining", replacement = "sobrecarga")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "teorico", replacement = "teoria")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "trabalhadores", replacement = "trabalhador")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "trabalho", replacement = "trabalhador")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "tecnico", replacement = "treinador")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "treinadores", replacement = "treinador")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "treino", replacement = "treinamento")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "universitaria", replacement = "estudantes")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "universitarios", replacement = "estudantes")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "bem estar", replacement = "bemestar")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "chen yang tai chi chuan", replacement = "exercicio")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "futebol de campo", replacement = "futebol")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "futebol campo", replacement = "futebol")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "futsal", replacement = "futebol")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "football", replacement = "futebol")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "cognitiva comportamental", replacement = "cognitiva_comportamental")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "padrao sono", replacement = "padrao_sono")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "avaliando", replacement = "avaliacao")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "brinquedos", replacement = "brincar")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "brinquedoteca", replacement = "brincar")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "ludica", replacement = "brincar")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "caracterizacao", replacement = "caracteristicas")
# corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               # pattern = "categorizacao", replacement = "caracteristicas")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "constribuicoes", replacement = "contribuicoes")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "cultural", replacement = "cultura")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "cyberbulling", replacement = "cyberbullying")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "depressivos", replacement = "depressao")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "desportivos", replacement = "esporte")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "educacionais", replacement = "educacao")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "pedagogicas", replacement = "educacao")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "educativos", replacement = "educacao")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "ensino", replacement = "educacao")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "metodologias", replacement = "metodos")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "rendimento", replacement = "performance")
# corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
#                pattern = "sociais", replacement = "social")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "socializacao", replacement = "social")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "jogos eletronicos", replacement = "videogame")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "jogos online", replacement = "videogame")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "jogos computacionais", replacement = "videogame")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "domino", replacement = "videogame")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "jogos", replacement = "videogame")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "redes sociais", replacement = "redes_sociais")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "rede sociais", replacement = "redes_sociais")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "estar subjetivo", replacement = "bemestarsubjetivo")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "yoga", replacement = "yoga esporte")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "natacao", replacement = "natacao esporte")
corpus_c1 <- tm_map(corpus_c1, content_transformer(gsub),
               pattern = "voleibol", replacement = "natacao esporte")

#exclusão de stopwords manulamente
corpus_c1 <- tm_map(corpus_c1, removeWords, c("rio","grande","sul","kundalini","impacto","acesso","processo","recife","internalizantes","callejero", "populacao","pessoas","interfaces","grande","dois","diferentes","correlacao","longitudinal","sindrome","freq","uencia","sub","brasileiros","restq","acaso","pos","estado","acinte","alto","relacao","relacionados","sujeitos","relacoes","variaveis","variavel","alta","niveis","influencia","parametros","pre","analise","individuos","pioram","melhoria","perfil","lafisaef","atraves","adiante","aplicada","iefd","iii","atribuidos","explicativo","fator","fatores","estudo","apoio","adrede","afinal","afora","agora","algures","alem","ali","amanha","antes","aqui","assim","atras","bem","breve","cedo","certamente","efetivamente","enfim","hoje","mal","mais","melhor","menos","muito","nao","ontem","pior","pouco","quanto","quao","quase","realmente","sera","sim","talvez","tanto","e","nem","mas","tambem","como","bem","porem","todavia","contudo","entretanto","entanto","ou","ora","quer","ja","logo","portanto","por","assim","conseguinte","que","porque","porquanto","pois","sendo","visto","como","tal","tao","tanto","assim","conforme","segundo","consoante","mesmo","mais","ainda","se","bem","embora","se","caso","contanto","salvo","medida","quanto","fim","quando","enquanto","sempre","depois","a","ante","apos","ate","com","contra","de","desde","para","per","perante","por","sem","sob","sobre","tras","algo","alguem","algum","alguns","cada","cujo","muitos","nada","nenhum","nenhuns","ninguem","outrem","outros","poucos","quaisquer","qualquer","quantos","quem","tantos","todos","tudo","que","nao","para","varios","de","a","o","que","e","do","da","em","um","para","com","nao","uma","os","no","se","na","por","mais","as","dos","como","mas","ao","ele","das","a","seu","sua","ou","quando","muito","nos","ja","eu","tambem","so","pelo","pela","ate","isso","ela","entre","depois","sem","mesmo","aos","seus","quem","nas","me","esse","eles","voce","essa","num","nem","suas","meu","as","minha","numa","pelos","elas","qual","nos","lhe","deles","essas","esses","pelas","este","dele","tu","te","voces","vos","lhes","meus","minhas","teu","tua","teus","tuas","nosso","nossa","nossos","nossas","dela","delas","esta","estes","estas","aquele","aquela","aqueles","aquelas","isto","aquilo","efeito","efeitos","estou","esta","estamos","estao","estive","esteve","estivemos","estiveram","estava","estavamos","estavam","estivera","estiveramos","esteja","estejamos","estejam","estivesse","estivessemos","estivessem","estiver","estivermos","estiverem","hei","ha","havemos","hao","houve","houvemos","houveram","houvera","houveramos","haja","hajamos","hajam","houvesse","houvessemos","houvessem","houver","houvermos","houverem","houverei","houvera","houveremos","houverao","houveria","houveriamos","houveriam","sou","somos","sao","era","eramos","eram","fui","foi","fomos","foram","fora","foramos","seja","sejamos","sejam","fosse","fossemos","fossem","for","formos","forem","serei","sera","seremos","serao","seria","seriamos","seriam","tenho","tem","temos","tem","tinha","tinhamos","tinham","tive","teve","tivemos","tiveram","tivera","tiveramos","tenha","tenhamos","tenham","tivesse","tivessemos","tivessem","tiver","tivermos","tiverem","terei","tera","teremos","terao","teria","teriamos","teriam","pessoas","durante","nivel","sao","luiz","resposta","respostas","jose","mato","grosso","psicologicas","atletas","continua","durante","cem","status","municipio","atraves","associados","fatores","brasil","principal","novas","interna","resqt","relacoes","caracterização","apos","nao","niveis","teorico","parana","santa","catarina","iefd","uberaraba","florianopolis","usuarios","atraves","brazilian","regiao","uso","pernambuco"))

#criando uma matrix de palavras e frequencia de palavras
dtm_c1 <- DocumentTermMatrix(corpus_c1)

# descritivos por cluster
freq_c1 <- sort(colSums(as.matrix(dtm_c1)), decreasing=TRUE)   
head(freq_c1, 20)

wf_c1 <- data.frame(word=names(freq_c1), freq=freq_c1)   
# head(wf_c1)  

p_c1 <- ggplot(subset(wf_c1, freq>1), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#Ploting network
# setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# postscript("/Users/Joao/Desktop/figure_cluster1psicologia_freqs.eps",
     # width = 8, height = 8)
p_c1  
# dev.off()

# set.seed(142)   
# wordcloud(names(freq_c1), freq_c1, min.freq=20)

dtm_c1_2<-removeSparseTerms(dtm_c1,0.93)
cor_c1 <- cor(as.matrix(dtm_c1_2),method = "spearman")
cor_c1 <- ifelse(cor_c1<0,0,cor_c1)
colnames(cor_c1)

#Ploting network
# setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# postscript("/Users/Joao/Desktop/figure_cluster1psicologia_network.eps",
     # width = 8, height = 8)
rede1_edf<-qgraph(cor_c1,
                   layout="spring",
                   # labels=colnames(cor_c1),
                   labels=c("anxiety",
                            "physical activity",
                            "evaluation",
                            "cognitive",
                            "behavioral",
                            "depression",
                            "sport",
                            "elderly",
                            "intervention",
                            "swimming",
                            "patients",
                            "program",
                            "promotion",
                            "quality of life",
                            "health",
                            "therapy"),
                   threshold=0.2,
                   vsize=5,
                   label.scale=FALSE,
                   # grey=T,
                   color="lightblue",
                   borders = FALSE,
                   posCol = "grey",
                   label.cex=1.2)
# dev.off()

########################################
#By Cluster Analysis - Cluster 2
########################################
# Análise por cluster
#Corpus
df_c2 <- data.frame(table_final$originalText[which(table_final$class==2)], stringsAsFactors=FALSE)
# View(df_c2)
corpus_c2 <- VCorpus(VectorSource(df_c2$table_final.originalText.which.table_final.class....2..))

#outras correções no texto
corpus_c2 <- tm_map(corpus_c2, content_transformer(function(x) gsub(x,
               pattern = "dor fisica", replacement = "dor")))
corpus_c2 <- tm_map(corpus_c2, content_transformer(function(x) gsub(x,
               pattern = "atividade fisica", replacement = "atividade_fisica")))
corpus_c2 <- tm_map(corpus_c2, content_transformer(function(x) gsub(x,
               pattern = "aptidao fisica", replacement = "aptidao_fisica")))
corpus_c2 <- tm_map(corpus_c2, content_transformer(function(x) gsub(x,
               pattern = "dor fisica", replacement = "dor")))
corpus_c2 <- tm_map(corpus_c2, content_transformer(function(x) gsub(x,
               pattern = "treinamento fisica", replacement = "treinamento")))
# corpus_c2 <- tm_map(corpus_c2, content_transformer(function(x) gsub(x,
#                pattern = "atividade", replacement = "atividade_fisica")))
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "exercicio fisica", replacement = "exercicio")
# corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
#                pattern = "exercicio", replacement = "exercicio_fisica")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "educacao fisica", replacement = "educacao_fisica")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "educacao fisiica", replacement = "educacao_fisica")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "condicionamento fisica", replacement = "condicionamento")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "qualidade vida", replacement = "qualidade_vida")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "estilo vida", replacement = "estilo_vida")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "efeitos", replacement = "efeito")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "adaptacao", replacement = "adaptado")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "adolescencia", replacement = "adolescentes")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "aerobias", replacement = "aerobio")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "arbitros", replacement = "arbitragem")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "aspectos psicologicos", replacement = "aspectos_psicologicos")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "atletas", replacement = "atleta")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "cardiorrespiratoria", replacement = "cardiaca")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "cardiorrespiratorios", replacement = "cardiaca")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "cardiometabolicos", replacement = "cardiaca")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "cardiopulmonar", replacement = "cardiaca")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "cardiovasculares", replacement = "cardiaca")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "santa catarina", replacement = "santa_catarina")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "sao paulo", replacement = "sao_paulo")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "cognicao", replacement = "cognitivas")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "comportamento", replacement = "comportamental")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "comportamentais", replacement = "comportamental")
# corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
#                pattern = "condicionamento", replacement = "condicionamento_fisica")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "decisao", replacement = "decisoes")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "amputados", replacement = "deficiencia")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "desportos", replacement = "esporte")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "emocionais", replacement = "emocional")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "alunos", replacement = "escolares")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "espiritualidade", replacement = "religiosidade")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "fisiologico", replacement = "fisiologicas")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "funcao", replacement = "funcional")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "funcionais", replacement = "funcional")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "funcionalidade", replacement = "funcional")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "funcoes", replacement = "funcional")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "hiperdia", replacement = "hipertensa")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "hipertensos", replacement = "hipertensa")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "imuno", replacement = "imunologicos")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "inflamacao", replacement = "inflamatorios")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "instituicoes", replacement = "instituto")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "instrumentacao", replacement = "instrumentos")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "intervencoes", replacement = "intervencao")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "jogadores", replacement = "atletas")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "juvenis", replacement = "jovens")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "lesao", replacement = "lesoes")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "lesados", replacement = "lesoes")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "lombalgia", replacement = "dor")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "marcador", replacement = "marcadores")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "marcas", replacement = "marcadores")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "metabolica", replacement = "metabolicos")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "modalidadades", replacement = "modalidades")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "motivacionais", replacement = "motivacao")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "motivacional", replacement = "motivacao")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "motivadores", replacement = "motivacao")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "motoras", replacement = "motor")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "motores", replacement = "motor")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "muscle", replacement = "muscular")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "neuromusculares", replacement = "musculo")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "neuromusculares", replacement = "musculo")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "oncologia", replacement = "doenca")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "paralimpicos", replacement = "deficiencia")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "participantes", replacement = "participacao")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "percepcoes", replacement = "percepcao")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "precursores", replacement = "preliminares")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "profissional", replacement = "profissionais")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "psicofisica", replacement = "psicobiologicas")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "psicofisiologico", replacement = "psicobiologicas")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "psicossociais", replacement = "psicologicas")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "psicossocial", replacement = "psicologicas")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "questionario", replacement = "instrumentos")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "recuperacao", replacement = "reabilitacao")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "recuperacao", replacement = "reabilitacao")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "competitiva", replacement = "rendimento")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "resqt", replacement = "restq")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "sadios", replacement = "saudaveis")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "satisfaction", replacement = "satisfacao")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "scale", replacement = "instrumentos")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "escala", replacement = "instrumentos")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "intrumentos", replacement = "instrumentos")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "overtraining", replacement = "sobrecarga")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "teorico", replacement = "teoria")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "trabalhadores", replacement = "trabalhador")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "trabalho", replacement = "trabalhador")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "tecnico", replacement = "treinador")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "treinadores", replacement = "treinador")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "treino", replacement = "treinamento")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "universitaria", replacement = "estudantes")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "universitarios", replacement = "estudantes")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "bem estar", replacement = "bemestar")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "chen yang tai chi chuan", replacement = "exercicio")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "futebol de campo", replacement = "futebol esporte")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "futebol campo", replacement = "futebol esporte")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "futsal", replacement = "futebol esporte")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "football", replacement = "futebol esporte")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "cognitiva comportamental", replacement = "cognitiva_comportamental")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "padrao sono", replacement = "padrao_sono")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "avaliando", replacement = "avaliacao")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "brinquedos", replacement = "brincar")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "brinquedoteca", replacement = "brincar")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "ludica", replacement = "brincar")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "caracterizacao", replacement = "caracteristicas")
# corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               # pattern = "categorizacao", replacement = "caracteristicas")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "constribuicoes", replacement = "contribuicoes")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "cultural", replacement = "cultura")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "cyberbulling", replacement = "cyberbullying")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "depressivos", replacement = "depressao")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "desportivos", replacement = "esporte")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "educacionais", replacement = "educacao")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "pedagogicas", replacement = "educacao")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "educativos", replacement = "educacao")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "ensino", replacement = "educacao")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "metodologias", replacement = "metodos")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "rendimento", replacement = "performance")
# corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
#                pattern = "sociais", replacement = "social")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "socializacao", replacement = "social")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "jogos eletronicos", replacement = "videogame")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "jogos online", replacement = "videogame")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "jogos computacionais", replacement = "videogame")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "domino", replacement = "videogame")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "jogos", replacement = "videogame")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "redes sociais", replacement = "redes_sociais")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "rede sociais", replacement = "redes_sociais")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "estar subjetivo", replacement = "bemestarsubjetivo")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "yoga", replacement = "yoga esporte")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "natacao", replacement = "natacao esporte")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "voleibol", replacement = "natacao esporte")
corpus_c2 <- tm_map(corpus_c2, content_transformer(gsub),
               pattern = "fair play", replacement = "fair_play")

#exclusão de stopwords manulamente
corpus_c2 <- tm_map(corpus_c2, removeWords, c("areas","caracteristicas","construcao","manual","implicacoes","participacao","preditores","contribuicoes","rio","grande","sul","kundalini","impacto","acesso","processo","recife","internalizantes","callejero", "populacao","pessoas","interfaces","grande","dois","diferentes","correlacao","longitudinal","sindrome","freq","uencia","sub","brasileiros","restq","acaso","pos","estado","acinte","alto","relacao","relacionados","sujeitos","relacoes","variaveis","variavel","alta","niveis","influencia","parametros","pre","analise","individuos","pioram","melhoria","perfil","lafisaef","atraves","adiante","aplicada","iefd","iii","atribuidos","explicativo","fator","fatores","estudo","apoio","adrede","afinal","afora","agora","algures","alem","ali","amanha","antes","aqui","assim","atras","bem","breve","cedo","certamente","efetivamente","enfim","hoje","mal","mais","melhor","menos","muito","nao","ontem","pior","pouco","quanto","quao","quase","realmente","sera","sim","talvez","tanto","e","nem","mas","tambem","como","bem","porem","todavia","contudo","entretanto","entanto","ou","ora","quer","ja","logo","portanto","por","assim","conseguinte","que","porque","porquanto","pois","sendo","visto","como","tal","tao","tanto","assim","conforme","segundo","consoante","mesmo","mais","ainda","se","bem","embora","se","caso","contanto","salvo","medida","quanto","fim","quando","enquanto","sempre","depois","a","ante","apos","ate","com","contra","de","desde","para","per","perante","por","sem","sob","sobre","tras","algo","alguem","algum","alguns","cada","cujo","muitos","nada","nenhum","nenhuns","ninguem","outrem","outros","poucos","quaisquer","qualquer","quantos","quem","tantos","todos","tudo","que","nao","para","varios","de","a","o","que","e","do","da","em","um","para","com","nao","uma","os","no","se","na","por","mais","as","dos","como","mas","ao","ele","das","a","seu","sua","ou","quando","muito","nos","ja","eu","tambem","so","pelo","pela","ate","isso","ela","entre","depois","sem","mesmo","aos","seus","quem","nas","me","esse","eles","voce","essa","num","nem","suas","meu","as","minha","numa","pelos","elas","qual","nos","lhe","deles","essas","esses","pelas","este","dele","tu","te","voces","vos","lhes","meus","minhas","teu","tua","teus","tuas","nosso","nossa","nossos","nossas","dela","delas","esta","estes","estas","aquele","aquela","aqueles","aquelas","isto","aquilo","efeito","efeitos","estou","esta","estamos","estao","estive","esteve","estivemos","estiveram","estava","estavamos","estavam","estivera","estiveramos","esteja","estejamos","estejam","estivesse","estivessemos","estivessem","estiver","estivermos","estiverem","hei","ha","havemos","hao","houve","houvemos","houveram","houvera","houveramos","haja","hajamos","hajam","houvesse","houvessemos","houvessem","houver","houvermos","houverem","houverei","houvera","houveremos","houverao","houveria","houveriamos","houveriam","sou","somos","sao","era","eramos","eram","fui","foi","fomos","foram","fora","foramos","seja","sejamos","sejam","fosse","fossemos","fossem","for","formos","forem","serei","sera","seremos","serao","seria","seriamos","seriam","tenho","tem","temos","tem","tinha","tinhamos","tinham","tive","teve","tivemos","tiveram","tivera","tiveramos","tenha","tenhamos","tenham","tivesse","tivessemos","tivessem","tiver","tivermos","tiverem","terei","tera","teremos","terao","teria","teriamos","teriam","pessoas","durante","nivel","sao","luiz","resposta","respostas","jose","mato","grosso","psicologicas","atletas","continua","durante","cem","status","municipio","atraves","associados","fatores","brasil","principal","novas","interna","resqt","relacoes","caracterização","apos","nao","niveis","teorico","parana","santa","catarina","iefd","uberaraba","florianopolis","usuarios","atraves","brazilian","regiao","uso","pernambuco"))

#criando uma matrix de palavras e frequencia de palavras
dtm_c2 <- DocumentTermMatrix(corpus_c2)

# descritivos por cluster
freq_c2 <- sort(colSums(as.matrix(dtm_c2)), decreasing=TRUE)   
head(freq_c2, 15)

wf_c2 <- data.frame(word=names(freq_c2), freq=freq_c2)   
head(wf_c2)  

# setEPS()
# # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# postscript("/Users/Joao/Desktop/figure_cluster2psicologia_freqs.eps",
#      width = 8, height = 8)
# p_c2 <- ggplot(subset(wf_c2, freq>1), aes(x = reorder(word, -freq), y = freq)) +
#   geom_bar(stat = "identity") +
#   theme(axis.text.x=element_text(angle=45, hjust=1))
p_c2  
# dev.off()

# set.seed(142)   
# wordcloud(names(freq_c2), freq_c2, min.freq=20)

dtm_c2_2<-removeSparseTerms(dtm_c2,0.86)
cor_c2 <- cor(as.matrix(dtm_c2_2),method = "spearman")
cor_c2 <- ifelse(cor_c2<0,0,cor_c2)
colnames(cor_c2)

# #Ploting network
# setEPS()
# # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# postscript("/Users/Joao/Desktop/figure_cluster2psicologia_network.eps",
#      width = 8, height = 8)
#View(cor_c1)
rede2_edf<-qgraph(cor_c2,
                   layout="spring",
                   # labels=colnames(cor_c2),
                   labels=c("actions",
                            "implementation",
                            "refereeing",
                            # "areas",
                            # "characteristics",
                            "behavioral",
                            "comunity",
                            "conflicts",
                            # "build",
                            "context",
                            "coping",
                            "performance",
                            "development",
                            "creation",
                            "sport",
                            "strategies",
                            "fair play",
                            "strengthening",
                            "football",
                            "instruments",
                            "leisure",
                            # "manual",
                            "mediation",
                            "personality",
                            "poverty",
                            "professionals",
                            "risk",
                            "street",
                            "health",
                            "situation",
                            "social",
                            "validation",
                            "values"),
                   threshold=0.2,
                   vsize=5,
                   label.scale=FALSE,
                   # grey=T,
                   color="lightblue",
                   borders = FALSE,
                   posCol = "grey",
                   label.cex=1.2)
# dev.off()

########################################
#By Cluster Analysis - Cluster 3
########################################
# Análise por cluster
#Corpus

df_c3 <- data.frame(table_final$originalText[which(table_final$class==3)], stringsAsFactors=FALSE)
# View(df_c3)
corpus_c3 <- VCorpus(VectorSource(df_c3$table_final.originalText.which.table_final.class....3..))

#outras correções no texto
corpus_c3 <- tm_map(corpus_c3, content_transformer(function(x) gsub(x,
               pattern = "dor fisica", replacement = "dor")))
corpus_c3 <- tm_map(corpus_c3, content_transformer(function(x) gsub(x,
               pattern = "atividade fisica", replacement = "atividade_fisica")))
corpus_c3 <- tm_map(corpus_c3, content_transformer(function(x) gsub(x,
               pattern = "aptidao fisica", replacement = "aptidao_fisica")))
corpus_c3 <- tm_map(corpus_c3, content_transformer(function(x) gsub(x,
               pattern = "dor fisica", replacement = "dor")))
corpus_c3 <- tm_map(corpus_c3, content_transformer(function(x) gsub(x,
               pattern = "treinamento fisica", replacement = "treinamento")))
# corpus_c3 <- tm_map(corpus_c3, content_transformer(function(x) gsub(x,
#                pattern = "atividade", replacement = "atividade_fisica")))
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "exercicio fisica", replacement = "exercicio")
# corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
#                pattern = "exercicio", replacement = "exercicio_fisica")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "educacao fisica", replacement = "educacao_fisica")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "educacao fisiica", replacement = "educacao_fisica")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "condicionamento fisica", replacement = "condicionamento")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "qualidade vida", replacement = "qualidade_vida")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "estilo vida", replacement = "estilo_vida")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "efeitos", replacement = "efeito")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "adaptacao", replacement = "adaptado")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "adolescencia", replacement = "adolescentes")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "aerobias", replacement = "aerobio")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "arbitros", replacement = "arbitragem")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "aspectos psicologicos", replacement = "aspectos_psicologicos")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "atletas", replacement = "atleta")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "cardiorrespiratoria", replacement = "cardiaca")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "cardiorrespiratorios", replacement = "cardiaca")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "cardiometabolicos", replacement = "cardiaca")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "cardiopulmonar", replacement = "cardiaca")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "cardiovasculares", replacement = "cardiaca")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "santa catarina", replacement = "santa_catarina")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "sao paulo", replacement = "sao_paulo")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "cognicao", replacement = "cognitivas")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "comportamento", replacement = "comportamental")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "comportamentais", replacement = "comportamental")
# corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
#                pattern = "condicionamento", replacement = "condicionamento_fisica")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "decisao", replacement = "decisoes")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "amputados", replacement = "deficiencia")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "desportos", replacement = "esporte")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "emocionais", replacement = "emocional")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "alunos", replacement = "escolares")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "espiritualidade", replacement = "religiosidade")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "fisiologico", replacement = "fisiologicas")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "funcao", replacement = "funcional")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "funcionais", replacement = "funcional")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "funcionalidade", replacement = "funcional")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "funcoes", replacement = "funcional")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "hiperdia", replacement = "hipertensa")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "hipertensos", replacement = "hipertensa")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "imuno", replacement = "imunologicos")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "inflamacao", replacement = "inflamatorios")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "instituicoes", replacement = "instituto")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "instrumentacao", replacement = "instrumentos")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "intervencoes", replacement = "intervencao")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "jogadores", replacement = "atletas")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "juvenis", replacement = "jovens")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "lesao", replacement = "lesoes")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "lesados", replacement = "lesoes")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "lombalgia", replacement = "dor")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "marcador", replacement = "marcadores")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "marcas", replacement = "marcadores")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "metabolica", replacement = "metabolicos")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "modalidadades", replacement = "modalidades")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "motivacionais", replacement = "motivacao")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "motivacional", replacement = "motivacao")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "motivadores", replacement = "motivacao")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "motoras", replacement = "motor")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "motores", replacement = "motor")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "muscle", replacement = "muscular")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "neuromusculares", replacement = "musculo")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "neuromusculares", replacement = "musculo")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "oncologia", replacement = "doenca")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "paralimpicos", replacement = "deficiencia")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "participantes", replacement = "participacao")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "percepcoes", replacement = "percepcao")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "precursores", replacement = "preliminares")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "profissional", replacement = "profissionais")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "psicofisica", replacement = "psicobiologicas")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "psicofisiologico", replacement = "psicobiologicas")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "psicossociais", replacement = "psicologicas")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "psicossocial", replacement = "psicologicas")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "questionario", replacement = "instrumentos")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "recuperacao", replacement = "reabilitacao")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "recuperacao", replacement = "reabilitacao")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "competitiva", replacement = "rendimento")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "resqt", replacement = "restq")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "sadios", replacement = "saudaveis")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "satisfaction", replacement = "satisfacao")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "scale", replacement = "instrumentos")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "escala", replacement = "instrumentos")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "intrumentos", replacement = "instrumentos")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "overtraining", replacement = "sobrecarga")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "teorico", replacement = "teoria")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "trabalhadores", replacement = "trabalhador")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "trabalho", replacement = "trabalhador")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "tecnico", replacement = "treinador")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "treinadores", replacement = "treinador")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "treino", replacement = "treinamento")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "universitaria", replacement = "estudantes")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "universitarios", replacement = "estudantes")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "bem estar", replacement = "bemestar")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "chen yang tai chi chuan", replacement = "exercicio")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "futebol de campo", replacement = "futebol")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "futebol campo", replacement = "futebol")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "futsal", replacement = "futebol")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "football", replacement = "futebol")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "cognitiva comportamental", replacement = "cognitiva_comportamental")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "padrao sono", replacement = "padrao_sono")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "avaliando", replacement = "avaliacao")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "brinquedos", replacement = "brincar")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "brinquedoteca", replacement = "brincar")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "ludica", replacement = "brincar")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "caracterizacao", replacement = "caracteristicas")
# corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               # pattern = "categorizacao", replacement = "caracteristicas")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "constribuicoes", replacement = "contribuicoes")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "cultural", replacement = "cultura")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "cyberbulling", replacement = "cyberbullying")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "depressivos", replacement = "depressao")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "desportivos", replacement = "esporte")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "educacionais", replacement = "educacao")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "pedagogicas", replacement = "educacao")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "educativos", replacement = "educacao")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "ensino", replacement = "educacao")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "metodologias", replacement = "metodos")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "rendimento", replacement = "performance")
# corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
#                pattern = "sociais", replacement = "social")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "socializacao", replacement = "social")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "jogos eletronicos", replacement = "videogame")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "jogos online", replacement = "videogame")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "jogos computacionais", replacement = "videogame")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "domino", replacement = "videogame")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "jogos", replacement = "videogame")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "redes sociais", replacement = "redes_sociais")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "rede sociais", replacement = "redes_sociais")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "estar subjetivo", replacement = "bemestarsubjetivo")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "yoga", replacement = "yoga esporte")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "natacao", replacement = "natacao esporte")
corpus_c3 <- tm_map(corpus_c3, content_transformer(gsub),
               pattern = "voleibol", replacement = "natacao esporte")

#exclusão de stopwords manulamente
corpus_c3 <- tm_map(corpus_c3, removeWords, c("rio","grande","sul","kundalini","impacto","acesso","processo","recife","internalizantes","callejero", "populacao","pessoas","interfaces","grande","dois","diferentes","correlacao","longitudinal","sindrome","freq","uencia","sub","brasileiros","restq","acaso","pos","estado","acinte","alto","relacao","relacionados","sujeitos","relacoes","variaveis","variavel","alta","niveis","influencia","parametros","pre","analise","individuos","pioram","melhoria","perfil","lafisaef","atraves","adiante","aplicada","iefd","iii","atribuidos","explicativo","fator","fatores","estudo","apoio","adrede","afinal","afora","agora","algures","alem","ali","amanha","antes","aqui","assim","atras","bem","breve","cedo","certamente","efetivamente","enfim","hoje","mal","mais","melhor","menos","muito","nao","ontem","pior","pouco","quanto","quao","quase","realmente","sera","sim","talvez","tanto","e","nem","mas","tambem","como","bem","porem","todavia","contudo","entretanto","entanto","ou","ora","quer","ja","logo","portanto","por","assim","conseguinte","que","porque","porquanto","pois","sendo","visto","como","tal","tao","tanto","assim","conforme","segundo","consoante","mesmo","mais","ainda","se","bem","embora","se","caso","contanto","salvo","medida","quanto","fim","quando","enquanto","sempre","depois","a","ante","apos","ate","com","contra","de","desde","para","per","perante","por","sem","sob","sobre","tras","algo","alguem","algum","alguns","cada","cujo","muitos","nada","nenhum","nenhuns","ninguem","outrem","outros","poucos","quaisquer","qualquer","quantos","quem","tantos","todos","tudo","que","nao","para","varios","de","a","o","que","e","do","da","em","um","para","com","nao","uma","os","no","se","na","por","mais","as","dos","como","mas","ao","ele","das","a","seu","sua","ou","quando","muito","nos","ja","eu","tambem","so","pelo","pela","ate","isso","ela","entre","depois","sem","mesmo","aos","seus","quem","nas","me","esse","eles","voce","essa","num","nem","suas","meu","as","minha","numa","pelos","elas","qual","nos","lhe","deles","essas","esses","pelas","este","dele","tu","te","voces","vos","lhes","meus","minhas","teu","tua","teus","tuas","nosso","nossa","nossos","nossas","dela","delas","esta","estes","estas","aquele","aquela","aqueles","aquelas","isto","aquilo","efeito","efeitos","estou","esta","estamos","estao","estive","esteve","estivemos","estiveram","estava","estavamos","estavam","estivera","estiveramos","esteja","estejamos","estejam","estivesse","estivessemos","estivessem","estiver","estivermos","estiverem","hei","ha","havemos","hao","houve","houvemos","houveram","houvera","houveramos","haja","hajamos","hajam","houvesse","houvessemos","houvessem","houver","houvermos","houverem","houverei","houvera","houveremos","houverao","houveria","houveriamos","houveriam","sou","somos","sao","era","eramos","eram","fui","foi","fomos","foram","fora","foramos","seja","sejamos","sejam","fosse","fossemos","fossem","for","formos","forem","serei","sera","seremos","serao","seria","seriamos","seriam","tenho","tem","temos","tem","tinha","tinhamos","tinham","tive","teve","tivemos","tiveram","tivera","tiveramos","tenha","tenhamos","tenham","tivesse","tivessemos","tivessem","tiver","tivermos","tiverem","terei","tera","teremos","terao","teria","teriamos","teriam","pessoas","durante","nivel","sao","luiz","resposta","respostas","jose","mato","grosso","psicologicas","atletas","continua","durante","cem","status","municipio","atraves","associados","fatores","brasil","principal","novas","interna","resqt","relacoes","caracterização","apos","nao","niveis","teorico","parana","santa","catarina","iefd","uberaraba","florianopolis","usuarios","atraves","brazilian","regiao","uso","pernambuco"))

#criando uma matrix de palavras e frequencia de palavras
dtm_c3 <- DocumentTermMatrix(corpus_c3)

# descritivos por cluster
freq_c3 <- sort(colSums(as.matrix(dtm_c3)), decreasing=TRUE)   
head(freq_c3, 15)

wf_c3 <- data.frame(word=names(freq_c3), freq=freq_c3)   
head(wf_c3)  

# setEPS()
# # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# postscript("/Users/Joao/Desktop/figure_cluster3psicologia_freqs.eps",
#      width = 8, height = 8)
# p_c3 <- ggplot(subset(wf_c3, freq>3), aes(x = reorder(word, -freq), y = freq)) +
#   geom_bar(stat = "identity") +
#   theme(axis.text.x=element_text(angle=45, hjust=1))
p_c3  
# dev.off()

# set.seed(142)   
# wordcloud(names(freq_c3), freq_c3, min.freq=20)

dtm_c3_2<-removeSparseTerms(dtm_c3,0.90)
cor_c3 <- cor(as.matrix(dtm_c3_2),method = "spearman")
#View(cor_c1)
cor_c3<-ifelse(cor_c3<0,0,cor_c3)
colnames(cor_c3)

#Ploting network
# setEPS()
# # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# postscript("/Users/Joao/Desktop/figure_cluster3psicologia_network.eps",
#      width = 8, height = 8)

rede3_edf<-qgraph(cor_c3,
                   layout="spring",
                   # labels=colnames(cor_c3),
                   labels=c("adolescents",
                            "learning",
                            "evaluation",
                            "playing",
                            "context",
                            "children",
                            "culture",
                            "development",
                            "education",
                            "physical education",
                            "school",
                            "sport",
                            "formation",
                            "practice",
                            "social networks",
                            "subjetivacao",
                            "video games"),
                   threshold=0.2,
                   vsize=5,
                   label.scale=FALSE,
                   # grey=T,
                   color="lightblue",
                   borders = FALSE,
                   posCol = "grey",
                   label.cex=1.2)
# dev.off()

########################################
#By Cluster Analysis - Cluster 4
########################################
# Análise por cluster
#Corpus

# df_c4 <- data.frame(table_final$originalText[which(table_final$class==4)], stringsAsFactors=FALSE)
# # View(df_c4)
# corpus_c4 <- VCorpus(VectorSource(df_c4$table_final.originalText.which.table_final.class....4..))

# #outras correções no texto
# corpus_c4 <- tm_map(corpus_c4, content_transformer(function(x) gsub(x,
#                pattern = "dor fisica", replacement = "dor")))
# corpus_c4 <- tm_map(corpus_c4, content_transformer(function(x) gsub(x,
#                pattern = "atividade fisica", replacement = "atividade_fisica")))
# corpus_c4 <- tm_map(corpus_c4, content_transformer(function(x) gsub(x,
#                pattern = "aptidao fisica", replacement = "aptidao_fisica")))
# corpus_c4 <- tm_map(corpus_c4, content_transformer(function(x) gsub(x,
#                pattern = "dor fisica", replacement = "dor")))
# corpus_c4 <- tm_map(corpus_c4, content_transformer(function(x) gsub(x,
#                pattern = "treinamento fisica", replacement = "treinamento")))
# # corpus_c4 <- tm_map(corpus_c4, content_transformer(function(x) gsub(x,
# #                pattern = "atividade", replacement = "atividade_fisica")))
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "exercicio fisica", replacement = "exercicio")
# # corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
# #                pattern = "exercicio", replacement = "exercicio_fisica")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "educacao fisica", replacement = "educacao_fisica")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "educacao fisiica", replacement = "educacao_fisica")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "condicionamento fisica", replacement = "condicionamento")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "qualidade vida", replacement = "qualidade_vida")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "estilo vida", replacement = "estilo_vida")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "efeitos", replacement = "efeito")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "adaptacao", replacement = "adaptado")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "adolescencia", replacement = "adolescentes")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "aerobias", replacement = "aerobio")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "arbitros", replacement = "arbitragem")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "aspectos psicologicos", replacement = "aspectos_psicologicos")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "atletas", replacement = "atleta")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "cardiorrespiratoria", replacement = "cardiaca")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "cardiorrespiratorios", replacement = "cardiaca")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "cardiometabolicos", replacement = "cardiaca")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "cardiopulmonar", replacement = "cardiaca")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "cardiovasculares", replacement = "cardiaca")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "santa catarina", replacement = "santa_catarina")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "sao paulo", replacement = "sao_paulo")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "cognicao", replacement = "cognitivas")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "comportamento", replacement = "comportamental")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "comportamentais", replacement = "comportamental")
# # corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
# #                pattern = "condicionamento", replacement = "condicionamento_fisica")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "decisao", replacement = "decisoes")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "amputados", replacement = "deficiencia")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "desportos", replacement = "esporte")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "emocionais", replacement = "emocional")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "alunos", replacement = "escolares")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "espiritualidade", replacement = "religiosidade")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "fisiologico", replacement = "fisiologicas")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "funcao", replacement = "funcional")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "funcionais", replacement = "funcional")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "funcionalidade", replacement = "funcional")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "funcoes", replacement = "funcional")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "hiperdia", replacement = "hipertensa")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "hipertensos", replacement = "hipertensa")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "imuno", replacement = "imunologicos")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "inflamacao", replacement = "inflamatorios")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "instituicoes", replacement = "instituto")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "instrumentacao", replacement = "instrumentos")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "intervencoes", replacement = "intervencao")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "jogadores", replacement = "atletas")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "juvenis", replacement = "jovens")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "lesao", replacement = "lesoes")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "lesados", replacement = "lesoes")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "lombalgia", replacement = "dor")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "marcador", replacement = "marcadores")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "marcas", replacement = "marcadores")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "metabolica", replacement = "metabolicos")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "modalidadades", replacement = "modalidades")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "motivacionais", replacement = "motivacao")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "motivacional", replacement = "motivacao")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "motivadores", replacement = "motivacao")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "motoras", replacement = "motor")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "motores", replacement = "motor")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "muscle", replacement = "muscular")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "neuromusculares", replacement = "musculo")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "neuromusculares", replacement = "musculo")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "oncologia", replacement = "doenca")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "paralimpicos", replacement = "deficiencia")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "participantes", replacement = "participacao")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "percepcoes", replacement = "percepcao")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "precursores", replacement = "preliminares")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "profissional", replacement = "profissionais")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "psicofisica", replacement = "psicobiologicas")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "psicofisiologico", replacement = "psicobiologicas")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "psicossociais", replacement = "psicologicas")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "psicossocial", replacement = "psicologicas")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "questionario", replacement = "instrumentos")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "recuperacao", replacement = "reabilitacao")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "recuperacao", replacement = "reabilitacao")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "competitiva", replacement = "rendimento")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "resqt", replacement = "restq")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "sadios", replacement = "saudaveis")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "satisfaction", replacement = "satisfacao")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "scale", replacement = "instrumentos")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "escala", replacement = "instrumentos")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "intrumentos", replacement = "instrumentos")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "overtraining", replacement = "sobrecarga")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "teorico", replacement = "teoria")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "trabalhadores", replacement = "trabalhador")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "trabalho", replacement = "trabalhador")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "tecnico", replacement = "treinador")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "treinadores", replacement = "treinador")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "treino", replacement = "treinamento")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "universitaria", replacement = "estudantes")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "universitarios", replacement = "estudantes")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "bem estar", replacement = "bemestar")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "chen yang tai chi chuan", replacement = "exercicio")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "futebol de campo", replacement = "futebol")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "futebol campo", replacement = "futebol")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "futsal", replacement = "futebol")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "football", replacement = "futebol")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "cognitiva comportamental", replacement = "cognitiva_comportamental")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "padrao sono", replacement = "padrao_sono")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "avaliando", replacement = "avaliacao")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "brinquedos", replacement = "brincar")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "brinquedoteca", replacement = "brincar")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "ludica", replacement = "brincar")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "caracterizacao", replacement = "caracteristicas")
# # corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                # pattern = "categorizacao", replacement = "caracteristicas")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "constribuicoes", replacement = "contribuicoes")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "cultural", replacement = "cultura")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "cyberbulling", replacement = "cyberbullying")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "depressivos", replacement = "depressao")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "desportivos", replacement = "esporte")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "educacionais", replacement = "educacao")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "pedagogicas", replacement = "educacao")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "educativos", replacement = "educacao")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "ensino", replacement = "educacao")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "metodologias", replacement = "metodos")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "rendimento", replacement = "performance")
# # corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
# #                pattern = "sociais", replacement = "social")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "socializacao", replacement = "social")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "jogos eletronicos", replacement = "videogame")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "jogos online", replacement = "videogame")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "jogos computacionais", replacement = "videogame")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "domino", replacement = "videogame")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "jogos", replacement = "videogame")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "redes sociais", replacement = "redes_sociais")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "rede sociais", replacement = "redes_sociais")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "estar subjetivo", replacement = "bemestarsubjetivo")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "yoga", replacement = "yoga esporte")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "natacao", replacement = "natacao esporte")
# corpus_c4 <- tm_map(corpus_c4, content_transformer(gsub),
#                pattern = "voleibol", replacement = "natacao esporte")

# #exclusão de stopwords manulamente
# corpus_c4 <- tm_map(corpus_c4, removeWords, c("rio","grande","sul","kundalini","impacto","acesso","processo","recife","internalizantes","callejero", "populacao","pessoas","interfaces","grande","dois","diferentes","correlacao","longitudinal","sindrome","freq","uencia","sub","brasileiros","restq","acaso","pos","estado","acinte","alto","relacao","relacionados","sujeitos","relacoes","variaveis","variavel","alta","niveis","influencia","parametros","pre","analise","individuos","pioram","melhoria","perfil","lafisaef","atraves","adiante","aplicada","iefd","iii","atribuidos","explicativo","fator","fatores","estudo","apoio","adrede","afinal","afora","agora","algures","alem","ali","amanha","antes","aqui","assim","atras","bem","breve","cedo","certamente","efetivamente","enfim","hoje","mal","mais","melhor","menos","muito","nao","ontem","pior","pouco","quanto","quao","quase","realmente","sera","sim","talvez","tanto","e","nem","mas","tambem","como","bem","porem","todavia","contudo","entretanto","entanto","ou","ora","quer","ja","logo","portanto","por","assim","conseguinte","que","porque","porquanto","pois","sendo","visto","como","tal","tao","tanto","assim","conforme","segundo","consoante","mesmo","mais","ainda","se","bem","embora","se","caso","contanto","salvo","medida","quanto","fim","quando","enquanto","sempre","depois","a","ante","apos","ate","com","contra","de","desde","para","per","perante","por","sem","sob","sobre","tras","algo","alguem","algum","alguns","cada","cujo","muitos","nada","nenhum","nenhuns","ninguem","outrem","outros","poucos","quaisquer","qualquer","quantos","quem","tantos","todos","tudo","que","nao","para","varios","de","a","o","que","e","do","da","em","um","para","com","nao","uma","os","no","se","na","por","mais","as","dos","como","mas","ao","ele","das","a","seu","sua","ou","quando","muito","nos","ja","eu","tambem","so","pelo","pela","ate","isso","ela","entre","depois","sem","mesmo","aos","seus","quem","nas","me","esse","eles","voce","essa","num","nem","suas","meu","as","minha","numa","pelos","elas","qual","nos","lhe","deles","essas","esses","pelas","este","dele","tu","te","voces","vos","lhes","meus","minhas","teu","tua","teus","tuas","nosso","nossa","nossos","nossas","dela","delas","esta","estes","estas","aquele","aquela","aqueles","aquelas","isto","aquilo","efeito","efeitos","estou","esta","estamos","estao","estive","esteve","estivemos","estiveram","estava","estavamos","estavam","estivera","estiveramos","esteja","estejamos","estejam","estivesse","estivessemos","estivessem","estiver","estivermos","estiverem","hei","ha","havemos","hao","houve","houvemos","houveram","houvera","houveramos","haja","hajamos","hajam","houvesse","houvessemos","houvessem","houver","houvermos","houverem","houverei","houvera","houveremos","houverao","houveria","houveriamos","houveriam","sou","somos","sao","era","eramos","eram","fui","foi","fomos","foram","fora","foramos","seja","sejamos","sejam","fosse","fossemos","fossem","for","formos","forem","serei","sera","seremos","serao","seria","seriamos","seriam","tenho","tem","temos","tem","tinha","tinhamos","tinham","tive","teve","tivemos","tiveram","tivera","tiveramos","tenha","tenhamos","tenham","tivesse","tivessemos","tivessem","tiver","tivermos","tiverem","terei","tera","teremos","terao","teria","teriamos","teriam","pessoas","durante","nivel","sao","luiz","resposta","respostas","jose","mato","grosso","psicologicas","atletas","continua","durante","cem","status","municipio","atraves","associados","fatores","brasil","principal","novas","interna","resqt","relacoes","caracterização","apos","nao","niveis","teorico","parana","santa","catarina","iefd","uberaraba","florianopolis","usuarios","atraves","brazilian","regiao","uso","pernambuco"))

# #criando uma matrix de palavras e frequencia de palavras
# dtm_c4 <- DocumentTermMatrix(corpus_c4)

# # descritivos por cluster
# freq_c4 <- sort(colSums(as.matrix(dtm_c4)), decreasing=TRUE)   
# head(freq_c4, 15)

# wf_c4 <- data.frame(word=names(freq_c4), freq=freq_c4)   
# head(wf_c4)  

# # setEPS()
# # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# # postscript("/Users/Joao/Desktop/figure_cluster4psicologia_freqs.eps",
# #      width = 8, height = 8)
# p_c4 <- ggplot(subset(wf_c4, freq>1), aes(x = reorder(word, -freq), y = freq)) +
#   geom_bar(stat = "identity") +
#   theme(axis.text.x=element_text(angle=45, hjust=1))
# p_c4  
# # dev.off()

# # set.seed(142)   
# # wordcloud(names(freq_c4), freq_c4, min.freq=20)

# dtm_c4_2<-removeSparseTerms(dtm_c4,0.90)
# cor_c4 <- cor(as.matrix(dtm_c4_2),method = "spearman")
# cor_c4<-ifelse(cor_c4<0,0,cor_c4)


# #Ploting network
# # setEPS()
# # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# # postscript("/Users/Joao/Desktop/figure_cluster4psicologia_network.eps",
#      # width = 8, height = 8)
# #View(cor_c1)
# rede4_edf<-qgraph(cor_c4,
#                    layout="spring",
#                    labels=colnames(cor_c4),
#                    # labels=c("physical activity",
#                    #          "corporal",
#                    #          "deficiency",
#                    #          "disease",
#                    #          "physical education",
#                    #          "elderly",
#                    #          "exercise",
#                    #          "aging",
#                    #          "image",
#                    #          "mental",
#                    #          "military",
#                    #          "women",
#                    #          "police",
#                    #          "practice",
#                    #          "quality of life",
#                    #          "regular",
#                    #          "health",
#                    #          "worker"),
#                    threshold=0.0,
#                    vsize=5,
#                    label.scale=FALSE,
#                    # grey=T,
#                    color="lightblue",
#                    borders = FALSE,
#                    posCol = "grey",
#                    label.cex=1.2)

# # dev.off()

# ########################################
# #By Cluster Analysis - Cluster 5 ...
# ########################################
# # Análise por cluster
# #Corpus

# df_c5 <- data.frame(table_final$originalText[which(table_final$class==5)], stringsAsFactors=FALSE)
# # View(df_c5)
# corpus_c5 <- VCorpus(VectorSource(df_c5$table_final.originalText.which.table_final.class....5..))

# #outras correções no texto
# corpus_c5 <- tm_map(corpus_c5, content_transformer(function(x) gsub(x,
#                pattern = "dor fisica", replacement = "dor")))
# corpus_c5 <- tm_map(corpus_c5, content_transformer(function(x) gsub(x,
#                pattern = "atividade fisica", replacement = "atividade_fisica")))
# corpus_c5 <- tm_map(corpus_c5, content_transformer(function(x) gsub(x,
#                pattern = "aptidao fisica", replacement = "aptidao_fisica")))
# corpus_c5 <- tm_map(corpus_c5, content_transformer(function(x) gsub(x,
#                pattern = "dor fisica", replacement = "dor")))
# corpus_c5 <- tm_map(corpus_c5, content_transformer(function(x) gsub(x,
#                pattern = "treinamento fisica", replacement = "treinamento")))
# # corpus_c5 <- tm_map(corpus_c5, content_transformer(function(x) gsub(x,
# #                pattern = "atividade", replacement = "atividade_fisica")))
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "exercicio fisica", replacement = "exercicio")
# # corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
# #                pattern = "exercicio", replacement = "exercicio_fisica")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "educacao fisica", replacement = "educacao_fisica")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "educacao fisiica", replacement = "educacao_fisica")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "condicionamento fisica", replacement = "condicionamento")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "qualidade vida", replacement = "qualidade_vida")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "estilo vida", replacement = "estilo_vida")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "efeitos", replacement = "efeito")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "adaptacao", replacement = "adaptado")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "adolescencia", replacement = "adolescentes")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "aerobias", replacement = "aerobio")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "arbitros", replacement = "arbitragem")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "aspectos psicologicos", replacement = "aspectos_psicologicos")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "atletas", replacement = "atleta")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "cardiorrespiratoria", replacement = "cardiaca")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "cardiorrespiratorios", replacement = "cardiaca")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "cardiometabolicos", replacement = "cardiaca")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "cardiopulmonar", replacement = "cardiaca")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "cardiovasculares", replacement = "cardiaca")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "santa catarina", replacement = "santa_catarina")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "sao paulo", replacement = "sao_paulo")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "cognicao", replacement = "cognitivas")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "comportamento", replacement = "comportamental")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "comportamentais", replacement = "comportamental")
# # corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
# #                pattern = "condicionamento", replacement = "condicionamento_fisica")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "decisao", replacement = "decisoes")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "amputados", replacement = "deficiencia")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "desportos", replacement = "esporte")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "emocionais", replacement = "emocional")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "alunos", replacement = "escolares")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "espiritualidade", replacement = "religiosidade")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "fisiologico", replacement = "fisiologicas")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "funcao", replacement = "funcional")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "funcionais", replacement = "funcional")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "funcionalidade", replacement = "funcional")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "funcoes", replacement = "funcional")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "hiperdia", replacement = "hipertensa")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "hipertensos", replacement = "hipertensa")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "imuno", replacement = "imunologicos")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "inflamacao", replacement = "inflamatorios")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "instituicoes", replacement = "instituto")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "instrumentacao", replacement = "instrumentos")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "intervencoes", replacement = "intervencao")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "jogadores", replacement = "atletas")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "juvenis", replacement = "jovens")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "lesao", replacement = "lesoes")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "lesados", replacement = "lesoes")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "lombalgia", replacement = "dor")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "marcador", replacement = "marcadores")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "marcas", replacement = "marcadores")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "metabolica", replacement = "metabolicos")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "modalidadades", replacement = "modalidades")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "motivacionais", replacement = "motivacao")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "motivacional", replacement = "motivacao")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "motivadores", replacement = "motivacao")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "motoras", replacement = "motor")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "motores", replacement = "motor")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "muscle", replacement = "muscular")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "neuromusculares", replacement = "musculo")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "neuromusculares", replacement = "musculo")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "oncologia", replacement = "doenca")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "paralimpicos", replacement = "deficiencia")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "participantes", replacement = "participacao")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "percepcoes", replacement = "percepcao")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "precursores", replacement = "preliminares")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "profissional", replacement = "profissionais")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "psicofisica", replacement = "psicobiologicas")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "psicofisiologico", replacement = "psicobiologicas")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "psicossociais", replacement = "psicologicas")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "psicossocial", replacement = "psicologicas")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "questionario", replacement = "instrumentos")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "recuperacao", replacement = "reabilitacao")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "recuperacao", replacement = "reabilitacao")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "competitiva", replacement = "rendimento")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "resqt", replacement = "restq")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "sadios", replacement = "saudaveis")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "satisfaction", replacement = "satisfacao")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "scale", replacement = "instrumentos")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "escala", replacement = "instrumentos")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "intrumentos", replacement = "instrumentos")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "overtraining", replacement = "sobrecarga")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "teorico", replacement = "teoria")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "trabalhadores", replacement = "trabalhador")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "trabalho", replacement = "trabalhador")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "tecnico", replacement = "treinador")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "treinadores", replacement = "treinador")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "treino", replacement = "treinamento")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "universitaria", replacement = "estudantes")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "universitarios", replacement = "estudantes")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "bem estar", replacement = "bemestar")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "chen yang tai chi chuan", replacement = "exercicio")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "futebol de campo", replacement = "futebol")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "futebol campo", replacement = "futebol")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "futsal", replacement = "futebol")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "football", replacement = "futebol")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "cognitiva comportamental", replacement = "cognitiva_comportamental")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "padrao sono", replacement = "padrao_sono")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "avaliando", replacement = "avaliacao")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "brinquedos", replacement = "brincar")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "brinquedoteca", replacement = "brincar")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "ludica", replacement = "brincar")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "caracterizacao", replacement = "caracteristicas")
# # corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                # pattern = "categorizacao", replacement = "caracteristicas")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "constribuicoes", replacement = "contribuicoes")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "cultural", replacement = "cultura")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "cyberbulling", replacement = "cyberbullying")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "depressivos", replacement = "depressao")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "desportivos", replacement = "esporte")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "educacionais", replacement = "educacao")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "pedagogicas", replacement = "educacao")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "educativos", replacement = "educacao")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "ensino", replacement = "educacao")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "metodologias", replacement = "metodos")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "rendimento", replacement = "performance")
# # corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
# #                pattern = "sociais", replacement = "social")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "socializacao", replacement = "social")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "jogos eletronicos", replacement = "videogame")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "jogos online", replacement = "videogame")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "jogos computacionais", replacement = "videogame")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "domino", replacement = "videogame")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "jogos", replacement = "videogame")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "redes sociais", replacement = "redes_sociais")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "rede sociais", replacement = "redes_sociais")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "estar subjetivo", replacement = "bemestarsubjetivo")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "yoga", replacement = "yoga esporte")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "natacao", replacement = "natacao esporte")
# corpus_c5 <- tm_map(corpus_c5, content_transformer(gsub),
#                pattern = "voleibol", replacement = "natacao esporte")

# #exclusão de stopwords manulamente
# corpus_c5 <- tm_map(corpus_c5, removeWords, c("rio","grande","sul","kundalini","impacto","acesso","processo","recife","internalizantes","callejero", "populacao","pessoas","interfaces","grande","dois","diferentes","correlacao","longitudinal","sindrome","freq","uencia","sub","brasileiros","restq","acaso","pos","estado","acinte","alto","relacao","relacionados","sujeitos","relacoes","variaveis","variavel","alta","niveis","influencia","parametros","pre","analise","individuos","pioram","melhoria","perfil","lafisaef","atraves","adiante","aplicada","iefd","iii","atribuidos","explicativo","fator","fatores","estudo","apoio","adrede","afinal","afora","agora","algures","alem","ali","amanha","antes","aqui","assim","atras","bem","breve","cedo","certamente","efetivamente","enfim","hoje","mal","mais","melhor","menos","muito","nao","ontem","pior","pouco","quanto","quao","quase","realmente","sera","sim","talvez","tanto","e","nem","mas","tambem","como","bem","porem","todavia","contudo","entretanto","entanto","ou","ora","quer","ja","logo","portanto","por","assim","conseguinte","que","porque","porquanto","pois","sendo","visto","como","tal","tao","tanto","assim","conforme","segundo","consoante","mesmo","mais","ainda","se","bem","embora","se","caso","contanto","salvo","medida","quanto","fim","quando","enquanto","sempre","depois","a","ante","apos","ate","com","contra","de","desde","para","per","perante","por","sem","sob","sobre","tras","algo","alguem","algum","alguns","cada","cujo","muitos","nada","nenhum","nenhuns","ninguem","outrem","outros","poucos","quaisquer","qualquer","quantos","quem","tantos","todos","tudo","que","nao","para","varios","de","a","o","que","e","do","da","em","um","para","com","nao","uma","os","no","se","na","por","mais","as","dos","como","mas","ao","ele","das","a","seu","sua","ou","quando","muito","nos","ja","eu","tambem","so","pelo","pela","ate","isso","ela","entre","depois","sem","mesmo","aos","seus","quem","nas","me","esse","eles","voce","essa","num","nem","suas","meu","as","minha","numa","pelos","elas","qual","nos","lhe","deles","essas","esses","pelas","este","dele","tu","te","voces","vos","lhes","meus","minhas","teu","tua","teus","tuas","nosso","nossa","nossos","nossas","dela","delas","esta","estes","estas","aquele","aquela","aqueles","aquelas","isto","aquilo","efeito","efeitos","estou","esta","estamos","estao","estive","esteve","estivemos","estiveram","estava","estavamos","estavam","estivera","estiveramos","esteja","estejamos","estejam","estivesse","estivessemos","estivessem","estiver","estivermos","estiverem","hei","ha","havemos","hao","houve","houvemos","houveram","houvera","houveramos","haja","hajamos","hajam","houvesse","houvessemos","houvessem","houver","houvermos","houverem","houverei","houvera","houveremos","houverao","houveria","houveriamos","houveriam","sou","somos","sao","era","eramos","eram","fui","foi","fomos","foram","fora","foramos","seja","sejamos","sejam","fosse","fossemos","fossem","for","formos","forem","serei","sera","seremos","serao","seria","seriamos","seriam","tenho","tem","temos","tem","tinha","tinhamos","tinham","tive","teve","tivemos","tiveram","tivera","tiveramos","tenha","tenhamos","tenham","tivesse","tivessemos","tivessem","tiver","tivermos","tiverem","terei","tera","teremos","terao","teria","teriamos","teriam","pessoas","durante","nivel","sao","luiz","resposta","respostas","jose","mato","grosso","psicologicas","atletas","continua","durante","cem","status","municipio","atraves","associados","fatores","brasil","principal","novas","interna","resqt","relacoes","caracterização","apos","nao","niveis","teorico","parana","santa","catarina","iefd","uberaraba","florianopolis","usuarios","atraves","brazilian","regiao","uso","pernambuco"))

# #criando uma matrix de palavras e frequencia de palavras
# dtm_c5 <- DocumentTermMatrix(corpus_c5)

# # descritivos por cluster
# freq_c5 <- sort(colSums(as.matrix(dtm_c5)), decreasing=TRUE)   
# head(freq_c5, 15)

# wf_c5 <- data.frame(word=names(freq_c5), freq=freq_c5)   
# head(wf_c5)  

# # setEPS()
# # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# # postscript("/Users/Joao/Desktop/figure_cluster5psicologia_freqs.eps",
#      # width = 8, height = 8)
# p_c5 <- ggplot(subset(wf_c5, freq>2), aes(x = reorder(word, -freq), y = freq)) +
#   geom_bar(stat = "identity") +
#   theme(axis.text.x=element_text(angle=45, hjust=1))
# p_c5  
# # dev.off()

# # set.seed(142)   
# # wordcloud(names(freq_c5), freq_c5, min.freq=20)

# dtm_c5_2<-removeSparseTerms(dtm_c5,0.90)
# cor_c5 <- cor(as.matrix(dtm_c5_2),method = "spearman")
# cor_c5<-ifelse(cor_c5<0,0,cor_c5)

# #Ploting network
# # setEPS()
# # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# # postscript("/Users/Joao/Desktop/figure_cluster5psicologia_network.eps",
#      # width = 8, height = 8)
# #View(cor_c1)
# rede5_edf<-qgraph(cor_c5,
#                    layout="spring",
#                    labels=colnames(cor_c5),
#                    # labels=c("physical activity",
#                    #          "corporal",
#                    #          "deficiency",
#                    #          "disease",
#                    #          "physical education",
#                    #          "elderly",
#                    #          "exercise",
#                    #          "aging",
#                    #          "image",
#                    #          "mental",
#                    #          "military",
#                    #          "women",
#                    #          "police",
#                    #          "practice",
#                    #          "quality of life",
#                    #          "regular",
#                    #          "health",
#                    #          "worker"),
#                    threshold=0.2,
#                    vsize=5,
#                    label.scale=FALSE,
#                    # grey=T,
#                    color="lightblue",
#                    borders = FALSE,
#                    posCol = "grey",
#                    label.cex=1.2)

# # dev.off()
# ########################################
# #By Cluster Analysis - Cluster 6
# ########################################
# # Análise por cluster
# #Corpus

# # df_c6 <- data.frame(table_final$originalText[which(table_final$class==6)], stringsAsFactors=FALSE)
# # # View(df_c6)
# # corpus_c6 <- VCorpus(VectorSource(df_c6$table_final.originalText.which.table_final.class....6..))

# # #outras correções no texto
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(function(x) gsub(x,
# #                pattern = "dor fisica", replacement = "dor")))
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(function(x) gsub(x,
# #                pattern = "atividade fisica", replacement = "atividade_fisica")))
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(function(x) gsub(x,
# #                pattern = "aptidao fisica", replacement = "aptidao_fisica")))
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(function(x) gsub(x,
# #                pattern = "dor fisica", replacement = "dor")))
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(function(x) gsub(x,
# #                pattern = "treinamento fisica", replacement = "treinamento")))
# # # corpus_c6 <- tm_map(corpus_c6, content_transformer(function(x) gsub(x,
# # #                pattern = "atividade", replacement = "atividade_fisica")))
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "exercicio fisica", replacement = "exercicio")
# # # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# # #                pattern = "exercicio", replacement = "exercicio_fisica")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "educacao fisica", replacement = "educacao_fisica")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "educacao fisiica", replacement = "educacao_fisica")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "condicionamento fisica", replacement = "condicionamento")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "qualidade vida", replacement = "qualidade_vida")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "estilo vida", replacement = "estilo_vida")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "efeitos", replacement = "efeito")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "adaptacao", replacement = "adaptado")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "adolescencia", replacement = "adolescentes")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "aerobias", replacement = "aerobio")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "arbitros", replacement = "arbitragem")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "aspectos psicologicos", replacement = "aspectos_psicologicos")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "atletas", replacement = "atleta")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "cardiorrespiratoria", replacement = "cardiaca")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "cardiorrespiratorios", replacement = "cardiaca")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "cardiometabolicos", replacement = "cardiaca")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "cardiopulmonar", replacement = "cardiaca")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "cardiovasculares", replacement = "cardiaca")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "santa catarina", replacement = "santa_catarina")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "sao paulo", replacement = "sao_paulo")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "cognicao", replacement = "cognitivas")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "comportamento", replacement = "comportamental")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "comportamentais", replacement = "comportamental")
# # # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# # #                pattern = "condicionamento", replacement = "condicionamento_fisica")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "decisao", replacement = "decisoes")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "amputados", replacement = "deficiencia")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "desportos", replacement = "esporte")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "emocionais", replacement = "emocional")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "alunos", replacement = "escolares")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "espiritualidade", replacement = "religiosidade")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "fisiologico", replacement = "fisiologicas")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "funcao", replacement = "funcional")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "funcionais", replacement = "funcional")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "funcionalidade", replacement = "funcional")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "funcoes", replacement = "funcional")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "hiperdia", replacement = "hipertensa")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "hipertensos", replacement = "hipertensa")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "imuno", replacement = "imunologicos")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "inflamacao", replacement = "inflamatorios")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "instituicoes", replacement = "instituto")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "instrumentacao", replacement = "instrumentos")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "intervencoes", replacement = "intervencao")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "jogadores", replacement = "atletas")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "juvenis", replacement = "jovens")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "lesao", replacement = "lesoes")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "lesados", replacement = "lesoes")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "lombalgia", replacement = "dor")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "marcador", replacement = "marcadores")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "marcas", replacement = "marcadores")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "metabolica", replacement = "metabolicos")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "modalidadades", replacement = "modalidades")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "motivacionais", replacement = "motivacao")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "motivacional", replacement = "motivacao")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "motivadores", replacement = "motivacao")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "motoras", replacement = "motor")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "motores", replacement = "motor")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "muscle", replacement = "muscular")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "neuromusculares", replacement = "musculo")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "neuromusculares", replacement = "musculo")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "oncologia", replacement = "doenca")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "paralimpicos", replacement = "deficiencia")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "participantes", replacement = "participacao")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "percepcoes", replacement = "percepcao")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "precursores", replacement = "preliminares")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "profissional", replacement = "profissionais")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "psicofisica", replacement = "psicobiologicas")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "psicofisiologico", replacement = "psicobiologicas")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "psicossociais", replacement = "psicologicas")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "psicossocial", replacement = "psicologicas")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "questionario", replacement = "instrumentos")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "recuperacao", replacement = "reabilitacao")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "recuperacao", replacement = "reabilitacao")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "competitiva", replacement = "rendimento")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "resqt", replacement = "restq")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "sadios", replacement = "saudaveis")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "satisfaction", replacement = "satisfacao")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "scale", replacement = "instrumentos")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "escala", replacement = "instrumentos")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "intrumentos", replacement = "instrumentos")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "overtraining", replacement = "sobrecarga")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "teorico", replacement = "teoria")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "trabalhadores", replacement = "trabalhador")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "trabalho", replacement = "trabalhador")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "tecnico", replacement = "treinador")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "treinadores", replacement = "treinador")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "treino", replacement = "treinamento")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "universitaria", replacement = "estudantes")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "universitarios", replacement = "estudantes")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "bem estar", replacement = "bemestar")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "chen yang tai chi chuan", replacement = "exercicio")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "futebol de campo", replacement = "futebol")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "futebol campo", replacement = "futebol")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "futsal", replacement = "futebol")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "football", replacement = "futebol")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "cognitiva comportamental", replacement = "cognitiva_comportamental")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "padrao sono", replacement = "padrao_sono")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "avaliando", replacement = "avaliacao")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "brinquedos", replacement = "brincar")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "brinquedoteca", replacement = "brincar")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "ludica", replacement = "brincar")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "caracterizacao", replacement = "caracteristicas")
# # # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                # pattern = "categorizacao", replacement = "caracteristicas")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "constribuicoes", replacement = "contribuicoes")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "cultural", replacement = "cultura")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "cyberbulling", replacement = "cyberbullying")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "depressivos", replacement = "depressao")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "desportivos", replacement = "esporte")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "educacionais", replacement = "educacao")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "pedagogicas", replacement = "educacao")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "educativos", replacement = "educacao")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "ensino", replacement = "educacao")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "metodologias", replacement = "metodos")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "rendimento", replacement = "performance")
# # # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# # #                pattern = "sociais", replacement = "social")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "socializacao", replacement = "social")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "jogos eletronicos", replacement = "videogame")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "jogos online", replacement = "videogame")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "jogos computacionais", replacement = "videogame")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "domino", replacement = "videogame")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "jogos", replacement = "videogame")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "redes sociais", replacement = "redes_sociais")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "rede sociais", replacement = "redes_sociais")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "estar subjetivo", replacement = "bemestarsubjetivo")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "yoga", replacement = "yoga esporte")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "natacao", replacement = "natacao esporte")
# # corpus_c6 <- tm_map(corpus_c6, content_transformer(gsub),
# #                pattern = "voleibol", replacement = "natacao esporte")

# # #exclusão de stopwords manulamente
# # corpus_c6 <- tm_map(corpus_c6, removeWords, c("rio","grande","sul","kundalini","impacto","acesso","processo","recife","internalizantes","callejero", "populacao","pessoas","interfaces","grande","dois","diferentes","correlacao","longitudinal","sindrome","freq","uencia","sub","brasileiros","restq","acaso","pos","estado","acinte","alto","relacao","relacionados","sujeitos","relacoes","variaveis","variavel","alta","niveis","influencia","parametros","pre","analise","individuos","pioram","melhoria","perfil","lafisaef","atraves","adiante","aplicada","iefd","iii","atribuidos","explicativo","fator","fatores","estudo","apoio","adrede","afinal","afora","agora","algures","alem","ali","amanha","antes","aqui","assim","atras","bem","breve","cedo","certamente","efetivamente","enfim","hoje","mal","mais","melhor","menos","muito","nao","ontem","pior","pouco","quanto","quao","quase","realmente","sera","sim","talvez","tanto","e","nem","mas","tambem","como","bem","porem","todavia","contudo","entretanto","entanto","ou","ora","quer","ja","logo","portanto","por","assim","conseguinte","que","porque","porquanto","pois","sendo","visto","como","tal","tao","tanto","assim","conforme","segundo","consoante","mesmo","mais","ainda","se","bem","embora","se","caso","contanto","salvo","medida","quanto","fim","quando","enquanto","sempre","depois","a","ante","apos","ate","com","contra","de","desde","para","per","perante","por","sem","sob","sobre","tras","algo","alguem","algum","alguns","cada","cujo","muitos","nada","nenhum","nenhuns","ninguem","outrem","outros","poucos","quaisquer","qualquer","quantos","quem","tantos","todos","tudo","que","nao","para","varios","de","a","o","que","e","do","da","em","um","para","com","nao","uma","os","no","se","na","por","mais","as","dos","como","mas","ao","ele","das","a","seu","sua","ou","quando","muito","nos","ja","eu","tambem","so","pelo","pela","ate","isso","ela","entre","depois","sem","mesmo","aos","seus","quem","nas","me","esse","eles","voce","essa","num","nem","suas","meu","as","minha","numa","pelos","elas","qual","nos","lhe","deles","essas","esses","pelas","este","dele","tu","te","voces","vos","lhes","meus","minhas","teu","tua","teus","tuas","nosso","nossa","nossos","nossas","dela","delas","esta","estes","estas","aquele","aquela","aqueles","aquelas","isto","aquilo","efeito","efeitos","estou","esta","estamos","estao","estive","esteve","estivemos","estiveram","estava","estavamos","estavam","estivera","estiveramos","esteja","estejamos","estejam","estivesse","estivessemos","estivessem","estiver","estivermos","estiverem","hei","ha","havemos","hao","houve","houvemos","houveram","houvera","houveramos","haja","hajamos","hajam","houvesse","houvessemos","houvessem","houver","houvermos","houverem","houverei","houvera","houveremos","houverao","houveria","houveriamos","houveriam","sou","somos","sao","era","eramos","eram","fui","foi","fomos","foram","fora","foramos","seja","sejamos","sejam","fosse","fossemos","fossem","for","formos","forem","serei","sera","seremos","serao","seria","seriamos","seriam","tenho","tem","temos","tem","tinha","tinhamos","tinham","tive","teve","tivemos","tiveram","tivera","tiveramos","tenha","tenhamos","tenham","tivesse","tivessemos","tivessem","tiver","tivermos","tiverem","terei","tera","teremos","terao","teria","teriamos","teriam","pessoas","durante","nivel","sao","luiz","resposta","respostas","jose","mato","grosso","psicologicas","atletas","continua","durante","cem","status","municipio","atraves","associados","fatores","brasil","principal","novas","interna","resqt","relacoes","caracterização","apos","nao","niveis","teorico","parana","santa","catarina","iefd","uberaraba","florianopolis","usuarios","atraves","brazilian","regiao","uso","pernambuco"))

# # #criando uma matrix de palavras e frequencia de palavras
# # dtm_c6 <- DocumentTermMatrix(corpus_c6)

# # # descritivos por cluster
# # freq_c6 <- sort(colSums(as.matrix(dtm_c6)), decreasing=TRUE)   
# # head(freq_c6, 15)

# # wf_c6 <- data.frame(word=names(freq_c6), freq=freq_c6)   
# # head(wf_c6)  

# # setEPS()
# # # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# # postscript("/Users/Joao/Desktop/figure_cluster6psicologia_freqs.eps",
# #      width = 8, height = 8)
# # p_c6 <- ggplot(subset(wf_c6, freq>3), aes(x = reorder(word, -freq), y = freq)) +
# #   geom_bar(stat = "identity") +
# #   theme(axis.text.x=element_text(angle=45, hjust=1))
# # p_c6  
# # dev.off()

# # # set.seed(142)   
# # # wordcloud(names(freq_c6), freq_c6, min.freq=20)

# # dtm_c6_2<-removeSparseTerms(dtm_c6,0.95)
# # cor_c6 <- cor(as.matrix(dtm_c6_2),method = "spearman")

# # #Ploting network
# # setEPS()
# # # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# # postscript("/Users/Joao/Desktop/figure_cluster6psicologia_network.eps",
# #      width = 8, height = 8)
# # #View(cor_c1)
# # qgraph(cor_c6,layout="spring",labels=colnames(cor_c6),threshold=0.3)

# # dev.off()
# # ########################################
# # #By Cluster Analysis - Cluster 7
# # ########################################
# # # Análise por cluster
# # #Corpus

# # df_c7 <- data.frame(table_final$originalText[which(table_final$class==7)], stringsAsFactors=FALSE)
# # # View(df_c7)
# # corpus_c7 <- VCorpus(VectorSource(df_c7$table_final.originalText.which.table_final.class....7..))

# # #outras correções no texto
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(function(x) gsub(x,
# #                pattern = "dor fisica", replacement = "dor")))
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(function(x) gsub(x,
# #                pattern = "atividade fisica", replacement = "atividade_fisica")))
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(function(x) gsub(x,
# #                pattern = "aptidao fisica", replacement = "aptidao_fisica")))
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(function(x) gsub(x,
# #                pattern = "dor fisica", replacement = "dor")))
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(function(x) gsub(x,
# #                pattern = "treinamento fisica", replacement = "treinamento")))
# # # corpus_c7 <- tm_map(corpus_c7, content_transformer(function(x) gsub(x,
# # #                pattern = "atividade", replacement = "atividade_fisica")))
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "exercicio fisica", replacement = "exercicio")
# # # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# # #                pattern = "exercicio", replacement = "exercicio_fisica")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "educacao fisica", replacement = "educacao_fisica")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "condicionamento fisica", replacement = "condicionamento")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "qualidade vida", replacement = "qualidade_vida")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "estilo vida", replacement = "estilo_vida")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "efeitos", replacement = "efeito")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "adaptacao", replacement = "adaptado")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "adolescencia", replacement = "adolescentes")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "aerobias", replacement = "aerobio")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "arbitros", replacement = "arbitragem")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "aspectos psicologicos", replacement = "aspectos_psicologicos")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "atletas", replacement = "atleta")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "cardiorrespiratoria", replacement = "cardiaca")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "cardiorrespiratorios", replacement = "cardiaca")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "cardiometabolicos", replacement = "cardiaca")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "cardiopulmonar", replacement = "cardiaca")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "cardiovasculares", replacement = "cardiaca")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "santa catarina", replacement = "santa_catarina")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "sao paulo", replacement = "sao_paulo")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "cognicao", replacement = "cognitivas")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "comportamento", replacement = "comportamental")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "comportamentais", replacement = "comportamental")
# # # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# # #                pattern = "condicionamento", replacement = "condicionamento_fisica")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "decisao", replacement = "decisoes")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "amputados", replacement = "deficiencia")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "desportos", replacement = "esporte")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "emocionais", replacement = "emocional")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "alunos", replacement = "escolares")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "espiritualidade", replacement = "religiosidade")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "fisiologico", replacement = "fisiologicas")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "funcao", replacement = "funcional")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "funcionais", replacement = "funcional")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "funcionalidade", replacement = "funcional")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "funcoes", replacement = "funcional")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "hiperdia", replacement = "hipertensa")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "hipertensos", replacement = "hipertensa")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "imuno", replacement = "imunologicos")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "inflamacao", replacement = "inflamatorios")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "instituicoes", replacement = "instituto")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "instrumentacao", replacement = "instrumentos")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "intervencoes", replacement = "intervencao")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "jogadores", replacement = "atletas")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "juvenis", replacement = "jovens")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "lesao", replacement = "lesoes")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "lesados", replacement = "lesoes")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "lombalgia", replacement = "dor")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "marcador", replacement = "marcadores")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "marcas", replacement = "marcadores")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "metabolica", replacement = "metabolicos")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "modalidadades", replacement = "modalidades")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "motivacionais", replacement = "motivacao")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "motivacional", replacement = "motivacao")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "motivadores", replacement = "motivacao")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "motoras", replacement = "motor")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "motores", replacement = "motor")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "muscle", replacement = "muscular")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "neuromusculares", replacement = "musculo")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "neuromusculares", replacement = "musculo")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "oncologia", replacement = "doenca")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "paralimpicos", replacement = "deficiencia")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "participantes", replacement = "participacao")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "percepcoes", replacement = "percepcao")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "precursores", replacement = "preliminares")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "profissional", replacement = "profissionais")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "psicofisica", replacement = "psicobiologicas")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "psicofisiologico", replacement = "psicobiologicas")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "psicossociais", replacement = "psicologicas")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "psicossocial", replacement = "psicologicas")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "questionario", replacement = "instrumentos")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "recuperacao", replacement = "reabilitacao")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "recuperacao", replacement = "reabilitacao")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "competitiva", replacement = "rendimento")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "resqt", replacement = "restq")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "sadios", replacement = "saudaveis")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "satisfaction", replacement = "satisfacao")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "scale", replacement = "instrumentos")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "intrumentos", replacement = "instrumentos")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "overtraining", replacement = "sobrecarga")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "teorico", replacement = "teoria")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "trabalhadores", replacement = "trabalhador")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "trabalho", replacement = "trabalhador")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "tecnico", replacement = "treinador")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "treinadores", replacement = "treinador")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "treino", replacement = "treinamento")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "universitaria", replacement = "estudantes")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "universitarios", replacement = "estudantes")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "bem estar", replacement = "bemestar")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "chen yang tai chi chuan", replacement = "exercicio")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "futebol de campo", replacement = "futebol")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "futebol campo", replacement = "futebol")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "futsal", replacement = "futebol")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "football", replacement = "futebol")
# # corpus_c7 <- tm_map(corpus_c7, content_transformer(gsub),
# #                pattern = "cognitiva comportamental", replacement = "cognitiva_comportamental")

# # #exclusão de stopwords manulamente
# # corpus_c7 <- tm_map(corpus_c7, removeWords, c("diferentes","correlacao","longitudinal","sindrome","freq","uencia","sub","brasileiros","restq","acaso","pos","estado","acinte","alto","relacao","relacionados","sujeitos","relacoes","variaveis","variavel","alta","niveis","influencia","parametros","pre","analise","individuos","pioram","melhoria","perfil","lafisaef","atraves","adiante","aplicada","iefd","iii","atribuidos","explicativo","fator","fatores","estudo","apoio","adrede","afinal","afora","agora","algures","alem","ali","amanha","antes","aqui","assim","atras","bem","breve","cedo","certamente","efetivamente","enfim","hoje","mal","mais","melhor","menos","muito","nao","ontem","pior","pouco","quanto","quao","quase","realmente","sera","sim","talvez","tanto","e","nem","mas","tambem","como","bem","porem","todavia","contudo","entretanto","entanto","ou","ora","quer","ja","logo","portanto","por","assim","conseguinte","que","porque","porquanto","pois","sendo","visto","como","tal","tao","tanto","assim","conforme","segundo","consoante","mesmo","mais","ainda","se","bem","embora","se","caso","contanto","salvo","medida","quanto","fim","quando","enquanto","sempre","depois","a","ante","apos","ate","com","contra","de","desde","para","per","perante","por","sem","sob","sobre","tras","algo","alguem","algum","alguns","cada","cujo","muitos","nada","nenhum","nenhuns","ninguem","outrem","outros","poucos","quaisquer","qualquer","quantos","quem","tantos","todos","tudo","que","nao","para","varios","de","a","o","que","e","do","da","em","um","para","com","nao","uma","os","no","se","na","por","mais","as","dos","como","mas","ao","ele","das","a","seu","sua","ou","quando","muito","nos","ja","eu","tambem","so","pelo","pela","ate","isso","ela","entre","depois","sem","mesmo","aos","seus","quem","nas","me","esse","eles","voce","essa","num","nem","suas","meu","as","minha","numa","pelos","elas","qual","nos","lhe","deles","essas","esses","pelas","este","dele","tu","te","voces","vos","lhes","meus","minhas","teu","tua","teus","tuas","nosso","nossa","nossos","nossas","dela","delas","esta","estes","estas","aquele","aquela","aqueles","aquelas","isto","aquilo","efeito","efeitos","estou","esta","estamos","estao","estive","esteve","estivemos","estiveram","estava","estavamos","estavam","estivera","estiveramos","esteja","estejamos","estejam","estivesse","estivessemos","estivessem","estiver","estivermos","estiverem","hei","ha","havemos","hao","houve","houvemos","houveram","houvera","houveramos","haja","hajamos","hajam","houvesse","houvessemos","houvessem","houver","houvermos","houverem","houverei","houvera","houveremos","houverao","houveria","houveriamos","houveriam","sou","somos","sao","era","eramos","eram","fui","foi","fomos","foram","fora","foramos","seja","sejamos","sejam","fosse","fossemos","fossem","for","formos","forem","serei","sera","seremos","serao","seria","seriamos","seriam","tenho","tem","temos","tem","tinha","tinhamos","tinham","tive","teve","tivemos","tiveram","tivera","tiveramos","tenha","tenhamos","tenham","tivesse","tivessemos","tivessem","tiver","tivermos","tiverem","terei","tera","teremos","terao","teria","teriamos","teriam","pessoas","durante","nivel","sao","luiz","resposta","respostas","jose","mato","grosso","psicologicas","atletas","continua","durante","cem","status","municipio","atraves","associados","fatores","brasil","principal","novas","interna","resqt","relacoes","caracterização","apos","nao","niveis","teorico","parana","santa","catarina","iefd","uberaraba","florianopolis","usuarios","atraves","brazilian","regiao","uso","pernambuco"))

# # #criando uma matrix de palavras e frequencia de palavras
# # dtm_c7 <- DocumentTermMatrix(corpus_c7)

# # # descritivos por cluster
# # freq_c7 <- sort(colSums(as.matrix(dtm_c7)), decreasing=TRUE)   
# # head(freq_c7, 15)

# # wf_c7 <- data.frame(word=names(freq_c7), freq=freq_c7)   
# # head(wf_c7)  

# # setEPS()
# # # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# # postscript("/Users/Joao/Desktop/figure_cluster7psicologia_freqs.eps",
# #      width = 8, height = 8)
# # p_c7 <- ggplot(subset(wf_c7, freq>3), aes(x = reorder(word, -freq), y = freq)) +
# #   geom_bar(stat = "identity") +
# #   theme(axis.text.x=element_text(angle=45, hjust=1))
# # p_c7  
# # dev.off()

# # # set.seed(142)   
# # # wordcloud(names(freq_c7), freq_c7, min.freq=20)

# # dtm_c7_2<-removeSparseTerms(dtm_c7,0.95)
# # cor_c7 <- cor(as.matrix(dtm_c7_2),method = "spearman")
# # cor_c7<-ifelse(cor_c7<0,0,cor_c7)

# # #Ploting network
# # setEPS()
# # # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# # postscript("/Users/Joao/Desktop/figure_cluster7psicologia_network.eps",
# #      width = 8, height = 8)
# # #View(cor_c1)
# # qgraph(cor_c7,layout="spring",labels=colnames(cor_c7),threshold=0.3)

# # dev.off()
# # ########################################
# # #By Cluster Analysis - Cluster 8
# # ########################################
# # # Análise por cluster
# # #Corpus

# # df_c8 <- data.frame(table_final$originalText[which(table_final$class==8)], stringsAsFactors=FALSE)
# # # View(df_c8)
# # corpus_c8 <- VCorpus(VectorSource(df_c8$table_final.originalText.which.table_final.class....8..))

# # #outras correções no texto
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(function(x) gsub(x,
# #                pattern = "dor fisica", replacement = "dor")))
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(function(x) gsub(x,
# #                pattern = "atividade fisica", replacement = "atividade_fisica")))
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(function(x) gsub(x,
# #                pattern = "aptidao fisica", replacement = "aptidao_fisica")))
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(function(x) gsub(x,
# #                pattern = "dor fisica", replacement = "dor")))
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(function(x) gsub(x,
# #                pattern = "treinamento fisica", replacement = "treinamento")))
# # # corpus_c8 <- tm_map(corpus_c8, content_transformer(function(x) gsub(x,
# # #                pattern = "atividade", replacement = "atividade_fisica")))
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "exercicio fisica", replacement = "exercicio")
# # # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# # #                pattern = "exercicio", replacement = "exercicio_fisica")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "educacao fisica", replacement = "educacao_fisica")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "condicionamento fisica", replacement = "condicionamento")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "qualidade vida", replacement = "qualidade_vida")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "estilo vida", replacement = "estilo_vida")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "efeitos", replacement = "efeito")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "adaptacao", replacement = "adaptado")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "adolescencia", replacement = "adolescentes")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "aerobias", replacement = "aerobio")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "arbitros", replacement = "arbitragem")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "aspectos psicologicos", replacement = "aspectos_psicologicos")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "atletas", replacement = "atleta")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "cardiorrespiratoria", replacement = "cardiaca")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "cardiorrespiratorios", replacement = "cardiaca")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "cardiometabolicos", replacement = "cardiaca")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "cardiopulmonar", replacement = "cardiaca")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "cardiovasculares", replacement = "cardiaca")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "santa catarina", replacement = "santa_catarina")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "sao paulo", replacement = "sao_paulo")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "cognicao", replacement = "cognitivas")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "comportamento", replacement = "comportamental")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "comportamentais", replacement = "comportamental")
# # # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# # #                pattern = "condicionamento", replacement = "condicionamento_fisica")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "decisao", replacement = "decisoes")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "amputados", replacement = "deficiencia")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "desportos", replacement = "esporte")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "emocionais", replacement = "emocional")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "alunos", replacement = "escolares")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "espiritualidade", replacement = "religiosidade")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "fisiologico", replacement = "fisiologicas")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "funcao", replacement = "funcional")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "funcionais", replacement = "funcional")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "funcionalidade", replacement = "funcional")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "funcoes", replacement = "funcional")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "hiperdia", replacement = "hipertensa")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "hipertensos", replacement = "hipertensa")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "imuno", replacement = "imunologicos")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "inflamacao", replacement = "inflamatorios")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "instituicoes", replacement = "instituto")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "instrumentacao", replacement = "instrumentos")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "intervencoes", replacement = "intervencao")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "jogadores", replacement = "atletas")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "juvenis", replacement = "jovens")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "lesao", replacement = "lesoes")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "lesados", replacement = "lesoes")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "lombalgia", replacement = "dor")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "marcador", replacement = "marcadores")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "marcas", replacement = "marcadores")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "metabolica", replacement = "metabolicos")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "modalidadades", replacement = "modalidades")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "motivacionais", replacement = "motivacao")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "motivacional", replacement = "motivacao")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "motivadores", replacement = "motivacao")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "motoras", replacement = "motor")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "motores", replacement = "motor")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "muscle", replacement = "muscular")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "neuromusculares", replacement = "musculo")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "neuromusculares", replacement = "musculo")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "oncologia", replacement = "doenca")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "paralimpicos", replacement = "deficiencia")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "participantes", replacement = "participacao")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "percepcoes", replacement = "percepcao")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "precursores", replacement = "preliminares")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "profissional", replacement = "profissionais")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "psicofisica", replacement = "psicobiologicas")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "psicofisiologico", replacement = "psicobiologicas")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "psicossociais", replacement = "psicologicas")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "psicossocial", replacement = "psicologicas")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "questionario", replacement = "instrumentos")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "recuperacao", replacement = "reabilitacao")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "recuperacao", replacement = "reabilitacao")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "competitiva", replacement = "rendimento")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "resqt", replacement = "restq")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "sadios", replacement = "saudaveis")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "satisfaction", replacement = "satisfacao")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "scale", replacement = "instrumentos")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "intrumentos", replacement = "instrumentos")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "overtraining", replacement = "sobrecarga")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "teorico", replacement = "teoria")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "trabalhadores", replacement = "trabalhador")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "trabalho", replacement = "trabalhador")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "tecnico", replacement = "treinador")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "treinadores", replacement = "treinador")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "treino", replacement = "treinamento")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "universitaria", replacement = "estudantes")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "universitarios", replacement = "estudantes")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "bem estar", replacement = "bemestar")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "chen yang tai chi chuan", replacement = "exercicio")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "futebol de campo", replacement = "futebol")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "futebol campo", replacement = "futebol")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "futsal", replacement = "futebol")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "football", replacement = "futebol")
# # corpus_c8 <- tm_map(corpus_c8, content_transformer(gsub),
# #                pattern = "cognitiva comportamental", replacement = "cognitiva_comportamental")

# # #exclusão de stopwords manulamente
# # corpus_c8 <- tm_map(corpus_c8, removeWords, c("diferentes","correlacao","longitudinal","sindrome","freq","uencia","sub","brasileiros","restq","acaso","pos","estado","acinte","alto","relacao","relacionados","sujeitos","relacoes","variaveis","variavel","alta","niveis","influencia","parametros","pre","analise","individuos","pioram","melhoria","perfil","lafisaef","atraves","adiante","aplicada","iefd","iii","atribuidos","explicativo","fator","fatores","estudo","apoio","adrede","afinal","afora","agora","algures","alem","ali","amanha","antes","aqui","assim","atras","bem","breve","cedo","certamente","efetivamente","enfim","hoje","mal","mais","melhor","menos","muito","nao","ontem","pior","pouco","quanto","quao","quase","realmente","sera","sim","talvez","tanto","e","nem","mas","tambem","como","bem","porem","todavia","contudo","entretanto","entanto","ou","ora","quer","ja","logo","portanto","por","assim","conseguinte","que","porque","porquanto","pois","sendo","visto","como","tal","tao","tanto","assim","conforme","segundo","consoante","mesmo","mais","ainda","se","bem","embora","se","caso","contanto","salvo","medida","quanto","fim","quando","enquanto","sempre","depois","a","ante","apos","ate","com","contra","de","desde","para","per","perante","por","sem","sob","sobre","tras","algo","alguem","algum","alguns","cada","cujo","muitos","nada","nenhum","nenhuns","ninguem","outrem","outros","poucos","quaisquer","qualquer","quantos","quem","tantos","todos","tudo","que","nao","para","varios","de","a","o","que","e","do","da","em","um","para","com","nao","uma","os","no","se","na","por","mais","as","dos","como","mas","ao","ele","das","a","seu","sua","ou","quando","muito","nos","ja","eu","tambem","so","pelo","pela","ate","isso","ela","entre","depois","sem","mesmo","aos","seus","quem","nas","me","esse","eles","voce","essa","num","nem","suas","meu","as","minha","numa","pelos","elas","qual","nos","lhe","deles","essas","esses","pelas","este","dele","tu","te","voces","vos","lhes","meus","minhas","teu","tua","teus","tuas","nosso","nossa","nossos","nossas","dela","delas","esta","estes","estas","aquele","aquela","aqueles","aquelas","isto","aquilo","efeito","efeitos","estou","esta","estamos","estao","estive","esteve","estivemos","estiveram","estava","estavamos","estavam","estivera","estiveramos","esteja","estejamos","estejam","estivesse","estivessemos","estivessem","estiver","estivermos","estiverem","hei","ha","havemos","hao","houve","houvemos","houveram","houvera","houveramos","haja","hajamos","hajam","houvesse","houvessemos","houvessem","houver","houvermos","houverem","houverei","houvera","houveremos","houverao","houveria","houveriamos","houveriam","sou","somos","sao","era","eramos","eram","fui","foi","fomos","foram","fora","foramos","seja","sejamos","sejam","fosse","fossemos","fossem","for","formos","forem","serei","sera","seremos","serao","seria","seriamos","seriam","tenho","tem","temos","tem","tinha","tinhamos","tinham","tive","teve","tivemos","tiveram","tivera","tiveramos","tenha","tenhamos","tenham","tivesse","tivessemos","tivessem","tiver","tivermos","tiverem","terei","tera","teremos","terao","teria","teriamos","teriam","pessoas","durante","nivel","sao","luiz","resposta","respostas","jose","mato","grosso","psicologicas","atletas","continua","durante","cem","status","municipio","atraves","associados","fatores","brasil","principal","novas","interna","resqt","relacoes","caracterização","apos","nao","niveis","teorico","parana","santa","catarina","iefd","uberaraba","florianopolis","usuarios","atraves","brazilian","regiao","uso","pernambuco"))

# # #criando uma matrix de palavras e frequencia de palavras
# # dtm_c8 <- DocumentTermMatrix(corpus_c8)

# # # descritivos por cluster
# # freq_c8 <- sort(colSums(as.matrix(dtm_c8)), decreasing=TRUE)   
# # head(freq_c8, 15)

# # wf_c8 <- data.frame(word=names(freq_c8), freq=freq_c8)   
# # head(wf_c8)  

# # setEPS()
# # # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# # postscript("/Users/Joao/Desktop/figure_cluster8psicologia_freqs.eps",
# #      width = 8, height = 8)
# # p_c8 <- ggplot(subset(wf_c8, freq>3), aes(x = reorder(word, -freq), y = freq)) +
# #   geom_bar(stat = "identity") +
# #   theme(axis.text.x=element_text(angle=45, hjust=1))
# # p_c8  
# # dev.off()

# # # set.seed(142)   
# # wordcloud(names(freq_c8), freq_c8, min.freq=20)

# dtm_c8_2<-removeSparseTerms(dtm_c8,0.99)
# cor_c8 <- cor(as.matrix(dtm_c8_2),method = "spearman")
# cor_c8<-ifelse(cor_c8<0,0,cor_c8)

# #Ploting network
# setEPS()
# # tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
# postscript("/Users/Joao/Desktop/figure_cluster8psicologia_network.eps",
#      width = 8, height = 8)
# #View(cor_c1)
# qgraph(cor_c8,layout="spring",labels=colnames(cor_c8),threshold=0.3)

# dev.off()

# dev.off()
tiff("/Users/joaovissoci/Desktop/psy_networks.tiff",
 width = 2500, height = 3000,compression = 'lzw', res=300)
#Add plot
par(mfrow = c(2, 2))  # 3 rows and 2 columns
plot(rede1_edf)
title("Cluster 1 - Physcial activity, sport \n and health",line=2)
plot(rede2_edf)
title("Cluster 2 - Psychosocial aspects \n of sport",line=2)
plot(rede3_edf)
title("Cluster 3 - Sport and physical education in \n child and adolescents development",line=2)
# plot(rede4_edf)
# title("Cluster 4 - Psychosocial aspects of social networks, \n games and cyberspace",line=2)
# plot(rede5_edf)
# title("Cluster 4 - Physical exercise, health promotion \n and rehabilitation",line=2)
with(wf,wordcloud(word,
                    # c("aspects",
                    #      "physical activity",
                    #      "athlete",
                    #      "evaluation",
                    #      "cognitive",
                    #      "behavioral",
                    #      "disease",
                    #      "sport",
                    #      "stress",
                    #      "exercise",
                    #      "functional",
                    #      "elderly",
                    #      "instruments",
                    #      "motor",
                    #      "practice",
                    #      "program",
                    #      "quality of life",
                    #      "performance",
                    #      "health",
                    #      "training"),
                       freq,
                       min.freq=3,
                       random.order = FALSE,
                       random.color=FALSE,
                       rot.per=.5,
                       colors = brewer.pal(8, "Dark2")))#scale=c(1,.01), max.words =100,
title("Most common words used in \n Psychology institutional projects",line=2)
dev.off()
