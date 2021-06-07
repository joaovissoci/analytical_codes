# Invariância fatorial e funcionamento diferenciial de itens DIF
# se necessario use a funcao install.packages() para instalar os pacotes
library(lavaan)
library(devtools)
library(semTools)
library(mirt)

# Exemplo: DASS21, instrumento para avaliacao de afetos negativos (estresse, depressao e ansiedade)
# Disponivel em http://www2.psy.unsw.edu.au/dass/Portuguese/DASS%2021%20Brazilian%20Portuguese%20Tucci.pdf
# Amostra: como descrita em http://www.scielo.br/scielo.php?script=sci_arttext&pid=S1413-82712015000200259 

# carregando o banco de dados
fdass<-read.csv("https://raw.githubusercontent.com/wagnerLM/quantia/master/dass_sex",sep=";")
# visualizando o banco de dados
View(fdass)
fdass<-na.omit(fdass)

# Nome resumido dos itens
dasslabels<-scan("https://raw.githubusercontent.com/wagnerLM/netusf/master/dasslabels",what = "character", sep = "\n")
dasslabels[c(3,5,10,13,16,17,21)]
# Itens completos 
dassnames<-scan("https://raw.githubusercontent.com/wagnerLM/netusf/master/dassnames",what = "character", sep = "\n")
dassnames[c(3,5,10,13,16,17,21)]



# definindo o modelo (fatores e itens) 
dass.model<-'
Depression =~ DASS3 + DASS5 + DASS10 + DASS13 + DASS16 + DASS17 + DASS21
'
# estimando o ajuste do modelo aos dados
fdass.fit<-cfa(dass.model,na.omit(fdass),estimator = "WLSMV",ordered = colnames(fdass[,-1]))
summary(fdass.fit,fit.measures=T,standardized=T)
# calculando a fidedignidade dos fatores por meio do Omega de McDonald
reliability(fdass.fit)
# por grupo
fdass.fit<-cfa(dass.model,na.omit(fdass),estimator = "WLSMV",ordered = colnames(fdass[,-1]),group = "sex")
summary(fdass.fit,fit.measures=T,standardized=T)
# observar as diferencas nas cargas fatoriais

# testar modelo de invariancia
fdassmg.fit_inv <- measEq.syntax(configural.model = dass.model,
                                 data = fdass,
                                 ordered = colnames(fdass[,-1]),
                                 parameterization = "delta",
                                 ID.fac = "std.lv",
                                 ID.cat = "Wu.Estabrook.2016",
                                 group = "sex",
                                 group.equal = c("configural","thresholds","loadings"))

model.inv <- as.character(fdassmg.fit_inv)

fdassmg.fit2 <- cfa(model.inv, data = fdass, group = "sex",
                    ordered =colnames(fdass[,-1]))
summary(fdassmg.fit2,fit.measures=T,standardized=T)

### por nível de invariancia
# configural
fdassmg.fit_conf <- measEq.syntax(configural.model = dass.model,
                                 data = fdass,
                                 ordered = colnames(fdass[,-1]),
                                 parameterization = "delta",
                                 ID.fac = "std.lv",
                                 ID.cat = "Wu.Estabrook.2016",
                                 group = "sex",
                                 group.equal = "configural")

model.inv_conf <- as.character(fdassmg.fit_conf)

fdassmg.fit2_conf <- cfa(model.inv_conf, data = fdass, group = "sex",
                    ordered =colnames(fdass[,-1]))
summary(fdassmg.fit2_conf,fit.measures=T,standardized=T)

# loadings
fdassmg.fit_load <- measEq.syntax(configural.model = dass.model,
                                  data = fdass,
                                  ordered = colnames(fdass[,-1]),
                                  parameterization = "delta",
                                  ID.fac = "std.lv",
                                  ID.cat = "Wu.Estabrook.2016",
                                  group = "sex",
                                  group.equal =c("configural","loadings"))

model.inv_load <- as.character(fdassmg.fit_load)

fdassmg.fit2_load <- cfa(model.inv_load, data = fdass, group = "sex",
                         ordered =colnames(fdass[,-1]))
summary(fdassmg.fit2_load,fit.measures=T,standardized=T)

# interceptos
fdassmg.fit_int <- measEq.syntax(configural.model = dass.model,
                                  data = fdass,
                                  ordered = colnames(fdass[,-1]),
                                  parameterization = "delta",
                                  ID.fac = "std.lv",
                                  ID.cat = "Wu.Estabrook.2016",
                                  group = "sex",
                                  group.equal = c("configural","loadings","intercepts"))

model.inv_int <- as.character(fdassmg.fit_int)

fdassmg.fit2_int <- cfa(model.inv_int, data = fdass, group = "sex",
                         ordered =colnames(fdass[,-1]))
summary(fdassmg.fit2_int,fit.measures=T,standardized=T)
#
all.results<-matrix(NA, nrow = 3, ncol = 6)
all.results[1,]<-round(data.matrix(fitmeasures
(fdassmg.fit2_conf, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))), digits=3)
all.results[2,]<-round(data.matrix(fitmeasures
(fdassmg.fit2_load, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))), digits=3)
all.results[3,]<-round(data.matrix(fitmeasures
(fdassmg.fit2_int, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"))), digits=3)
colnames(all.results)<-c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled")
all.results
# um artigo ótimo sobre o tema foi indicado pelo Leo Martins (https://www.facebook.com/leofer.martins?comment_id=Y29tbWVudDoyNTUyMTA3NjkxNTg1MDM0XzI1Njk0OTgwNjMxNzkzMzA%3D)
# https://sci-hub.tw/10.1080/10705511.2019.1602776

###
library(mirt)

### DIF para itens dicotômicos

dassbin_sex<-read.csv("https://raw.githubusercontent.com/wagnerLM/quantia/master/dass_sex",sep = ";")
dassbin_sex<-na.omit(dassbin_sex)
dassnames2<-colnames(dassbin_sex[,c(3,5,10,13,16,17,21)])
dassnames2
dass_dif_conf<-multipleGroup(dassbin_sex[,c(3,5,10,13,16,17,21)],1, group = as.factor(dassbin_sex$sex))
coef(dass_dif_conf)
M2(dass_dif_conf)
#(dassbin_sex[,c(3,5,10,13,16,17,21)],1, group = as.factor(dassbin_sex$sex), invariance = c("free_means","free_var",dassnames2[7]), method = "EM",dentype = "Gaussian")
dass_dif_met<-multipleGroup(dassbin_sex[,c(3,5,10,13,16,17,21)],1, group = as.factor(dassbin_sex$sex),invariance = c("slopes"))
coef(dass_dif_met)
M2(dass_dif_met)
dass_dif_scal<-multipleGroup(dassbin_sex[,c(3,5,10,13,16,17,21)],1, group = as.factor(dassbin_sex$sex),invariance = c('slopes', 'intercepts', 'free_var','free_means'))
coef(dass_dif_scal)
M2(dass_dif_scal)
dass_dif_strongscal<-multipleGroup(dassbin_sex[,c(3,5,10,13,16,17,21)],1, group = as.factor(dassbin_sex$sex),invariance = c('slopes', 'intercepts'))
coef(dass_dif_strongscal)
M2(dass_dif_strongscal)

summary(dass_dif_strongscal)
coef(dass_dif_strongscal, simplify=TRUE)
plot(dass_dif_conf)
plot(dass_dif_conf, type = 'info')
itemplot(dass_dif_conf, 1)
itemplot(dass_dif_conf, 2)
itemplot(dass_dif_conf, 3)
itemplot(dass_dif_conf, 4)
itemplot(dass_dif_conf, 5)
itemplot(dass_dif_conf, 6)
itemplot(dass_dif_conf, 7)