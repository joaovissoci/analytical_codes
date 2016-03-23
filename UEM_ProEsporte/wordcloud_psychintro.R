library(wordcloud)

words<-c("Motivação",
		 "Emoções",
		 "Imagem corporal",
		 "Liderança",
		 "Carreira esportiva",
		 "Estresse",
		 "Intervenção psicológica",
		 "Qualidade de vida",
		 "Percepção subjetiva do esforço",
		 "Estado de humor",
		 "Percepçao de competência",
		 "Burnout",
		 "Suporte parental",
		 "Bem-estar e exercício",
		 "Identidade",
		 "Metas",
		 "Distúrbio alimentar",
		 "Satisfação de vida",
		 "Ansiedade",
		 "Talentos esportivos",
		 "Coesão de grupo")


#Encoding(words) <- "Latin1"
words

freq <- as.numeric(c(49,41,28,26,22,19,18,12,12,10,7,7,6,
	5,6,6,6,5,5,4,4))
#tdm <- TermDocumentMatrix(words)
pal <- brewer.pal(9,"YlGnBu")
pal <- pal[-(1:4)]

wordcloud(words,freq,min.freq=1,,colors=pal)


