#####################################################################################
#TEMPLATE DE ANALISE - coach_athlete.R
#####################################################################################
#
# Glossário
# $ = Pertence a = data$sexo, que dizer variável sexo pertencente ao banco data
#
#
#####################################################################################
#ORGANIZANDO O AMBIENTE DO R
#####################################################################################
#Exemplo de funçao para instalar pacotes
#install.packages("Hmisc")

#Exemplo de função para carregar os pacotes
#library(Hmisc)

#Carregar todos os pacotes ao mesmo tempo
#Substituir o termo em " " pelo nome do pacote que quer carregar
#Por exemplo, se eu queri inserir o pacote SEM pra analise do 
#Modelo de equaçoes estruturais, eu inclui na função "sem"
lapply(c("Hmisc","car","GPArotation","psych","nortest","ggplot2","pastecs","repmis",
  "mvnormtest","polycor","sem","nortest","ltm","gdata"), 
library, character.only=T)
#####################################################################################
#ORGANIZANDO O BANCO DE DADOS
#####################################################################################
#Exemplo de como inserir o banco de dados direto de um arquivo físico do computador
data<-read.csv("/Users/joaovissoci/OneDrive - Duke University/datasets/valoresdimensoes.csv",sep=",")

graph<-read.csv("/Users/joaovissoci/OneDrive - Duke University/datasets/graph.csv",sep=",")

#####################################################################################
#ANALISES DESCRITIVAS
#####################################################################################
summary(data) # produz descritivos basicos (media, mdiana etc)
names(data) # retorna os nomes das variaveis do banco
#summary(data$Persis) #retorna os decritivos apenas para a variavel Persis
#ad.test(data$Persis)### teste de normalidade 
cordata<-na.omit(data)
x<-rcorr(as.matrix(cordata))
x<-as.table(x)
#library(xlsReadWrite)
#write.csv(x, "/Users/joaovissoci/Desktop/mydata.csv")
###########################################################################################
#MODELO DE MENSURAÇÃO
###########################################################################################
#criando um banco de dados somente com os dados da eficacia coletiva
sf36_RAW<-with(data,data.frame(Capacidade_Funcional_RAW,
          Limitacao_por_Aspectos_Fisicos_RAW,Dor_RAW,Estado_Geral_de_Saude_RAW,
          Vitalidade_RAW,Aspectos_Sociais_RAW,Aspectos_Emocionais_RAW,
          Saude_Mental_RAW)) 
sf36<-with(data,data.frame(Capacidade_Funcional,
          Limitacao_por_Aspectos_Fisicos,Dor,Estado_Geral_de_Saude,
          Vitalidade,Aspectos_Sociais,Aspectos_Emocionais,
          Saude_Mental)) 
summary(sf36) #retorna os descritivos basicos (media, mediana etc)
#sf36<-na.omit(sf36)
SMS<-with(data,data.frame(Regulacao_externa,
          Introjecao,Identificacao,Atingir_objetivos,
          Experiencias_estimulantes,Para_conhecer)) 
summary(SMS) #retorna os descritivos basicos (media, mediana etc)
#SMS<-na.omit(SMS)
#str(eficacia)##descreve as variaveis (caracteristicas)
#cor(eficacia)# calcula a matriz de correlacao
#rcorr(as.matrix(eficacia),type="spearman") #calcula a matriz de correlação e valores de significancia

#CALCULANDO OS PRESSUPOSTOS PARA A ANALISE FATORIAL EXPLORATORIA

### SF36
par(mfrow=c(2,2)) #organiza o espaço para geração dos gráfico
ev <- eigen(cor(sf36)) # gera os eigenvalues para definir o número de dimensões da AFE
ev #retorna os valores dos eigenvalues
# Calcula a analise paralela para verificar o numero de dimesoes da AFE
ap <- parallel(subject=nrow(sf36),var=ncol(sf36),rep=100,cent=.05)
#Gera os calcualr para o scree plot
nS <- nScree(ev$values)
#retorna o scree plot  - grafico do ombro - onde a curva indica o numero
# de dimensões
plotnScree(nS)

#Calcula o Keiser-Meyer-Olkin para verificar a adequação da amostra para a analise
#KMO - ingora e roda tudo desde a linha 80 até a 127
kmo = function( x ){
  
  library(MASS)
  X <- cor(as.matrix(x))
  iX <- ginv(X)
  S2 <- diag(diag((iX^-1)))
  AIS <- S2%*%iX%*%S2                      # anti-image covariance matrix
  IS <- X+AIS-2*S2                         # image covariance matrix
  Dai <- sqrt(diag(diag(AIS)))
  IR <- ginv(Dai)%*%IS%*%ginv(Dai)         # image correlation matrix
  AIR <- ginv(Dai)%*%AIS%*%ginv(Dai)       # anti-image correlation matrix
  a <- apply((AIR - diag(diag(AIR)))^2, 2, sum)
  AA <- sum(a)
  b <- apply((X - diag(nrow(X)))^2, 2, sum)
  BB <- sum(b)
  MSA <- b/(b+a)                        # indiv. measures of sampling adequacy
  
  AIR <- AIR-diag(nrow(AIR))+diag(MSA)  # Examine the anti-image of the
  # correlation matrix. That is the
  # negative of the partial correlations,
  # partialling out all other variables.
  
  kmo <- BB/(AA+BB)                     # overall KMO statistic
  
  # Reporting the conclusion
  if (kmo >= 0.00 && kmo < 0.50){
    test <- 'The KMO test yields a degree of common variance
    unacceptable for FA.'
  } else if (kmo >= 0.50 && kmo < 0.60){
    test <- 'The KMO test yields a degree of common variance miserable.'
  } else if (kmo >= 0.60 && kmo < 0.70){
    test <- 'The KMO test yields a degree of common variance mediocre.'
  } else if (kmo >= 0.70 && kmo < 0.80){
    test <- 'The KMO test yields a degree of common variance middling.'
  } else if (kmo >= 0.80 && kmo < 0.90){
    test <- 'The KMO test yields a degree of common variance meritorious.'
  } else {
    test <- 'The KMO test yields a degree of common variance marvelous.'
  }
  
  ans <- list(  overall = kmo,
                report = test,
                individual = MSA,
                AIS = AIS,
                AIR = AIR )
  return(ans)
  
}    # end of kmo()
## Roda a função kmo para ao banco de dados eficacia
## Função KMO é o conjunto de codigos que você rocou da linha 80 à 127
#Indica a adequação da amostra à analise
kmo(sf36)

#EXTRAÇÃO DOS FATORES E RESULRADOS ESTATISICOS DA ANALISE FATORIAL EXPLORATÓRIA
# eficacia = Nome do banco de dados
# fm = metodo de estimação, aqui usando 'pa' que é principal axis
# rotate = metodo de rotação, aqui usando varimax
fa(sf36,3,fm="wls",rotate="oblimin")
fa(sf36,1,fm="wls",rotate="oblimin")

#CALCULANDO OS PRESSUPOSTOS PARA A ANALISE FATORIAL CONFIRMATORIA
# Função para especificar o modelo que vai ser analisar
modefic <- specifyModel() # Após rodar a função, inserir linha após linha do modelo 

QV->Capacidade_Funcional_RAW,NA,1
QV->Limitacao_por_Aspectos_Fisicos_RAW,var4,NA
QV->Dor_RAW,var5,NA
QV->Estado_Geral_de_Saude_RAW,var6,NA
QV->Aspectos_Emocionais_RAW,var7,NA
QV->Saude_Mental_RAW,var8,NA
QV->Aspectos_Sociais_RAW,var9,NA
QV->Vitalidade_RAW,var10,NA


#Erros and COv
QV<->QV,erro1,NA
Saude_Mental_RAW<->Vitalidade_RAW,coverro1,NA
Aspectos_Emocionais_RAW<->Limitacao_por_Aspectos_Fisicos_RAW,coverro2,NA

# Criando matriz de covariância para analise do modelo
cov <- cov(sf36, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))

# Rodando o modelo de equações estruturais aqui no caso uma Analise Fatorial Confirmatória
# Argumentos são: modefic = modelo especificado na linha 141
# cov = matriz de covariancia criada na linha 154
# N = numero de observações (amostra)
sem <- sem(modefic, cov, N=184)

# Reporta o resumo das analises do modelo de equações estruturais
summary(sem,fit.indices=c("GFI", "AGFI", "RMSEA", "NFI", 
                          "NNFI", "CFI", "RNI", "IFI", "SRMR", "AIC", "AICc"))

# Reporta os efeitos das trjetórias do modelo
effects(sem)

# Reporta os coeficientes estandardizados das trajetórias (linhas)
standardizedCoefficients(sem)

# Reporta os indices de modificação para melhorar o ajustamento do modelo
modIndices(sem)

### SMS
par(mfrow=c(2,2)) #organiza o espaço para geração dos gráfico
ev <- eigen(cor(SMS)) # gera os eigenvalues para definir o número de dimensões da AFE
ev #retorna os valores dos eigenvalues
# Calcula a analise paralela para verificar o numero de dimesoes da AFE
ap <- parallel(subject=nrow(SMS),var=ncol(SMS),rep=100,cent=.05)
#Gera os calcualr para o scree plot
nS <- nScree(ev$values)
#retorna o scree plot  - grafico do ombro - onde a curva indica o numero
# de dimensões
plotnScree(nS)

#Calcula o Keiser-Meyer-Olkin para verificar a adequação da amostra para a analise
#KMO - ingora e roda tudo desde a linha 80 até a 127
kmo = function( x ){
  
  library(MASS)
  X <- cor(as.matrix(x))
  iX <- ginv(X)
  S2 <- diag(diag((iX^-1)))
  AIS <- S2%*%iX%*%S2                      # anti-image covariance matrix
  IS <- X+AIS-2*S2                         # image covariance matrix
  Dai <- sqrt(diag(diag(AIS)))
  IR <- ginv(Dai)%*%IS%*%ginv(Dai)         # image correlation matrix
  AIR <- ginv(Dai)%*%AIS%*%ginv(Dai)       # anti-image correlation matrix
  a <- apply((AIR - diag(diag(AIR)))^2, 2, sum)
  AA <- sum(a)
  b <- apply((X - diag(nrow(X)))^2, 2, sum)
  BB <- sum(b)
  MSA <- b/(b+a)                        # indiv. measures of sampling adequacy
  
  AIR <- AIR-diag(nrow(AIR))+diag(MSA)  # Examine the anti-image of the
  # correlation matrix. That is the
  # negative of the partial correlations,
  # partialling out all other variables.
  
  kmo <- BB/(AA+BB)                     # overall KMO statistic
  
  # Reporting the conclusion
  if (kmo >= 0.00 && kmo < 0.50){
    test <- 'The KMO test yields a degree of common variance
    unacceptable for FA.'
  } else if (kmo >= 0.50 && kmo < 0.60){
    test <- 'The KMO test yields a degree of common variance miserable.'
  } else if (kmo >= 0.60 && kmo < 0.70){
    test <- 'The KMO test yields a degree of common variance mediocre.'
  } else if (kmo >= 0.70 && kmo < 0.80){
    test <- 'The KMO test yields a degree of common variance middling.'
  } else if (kmo >= 0.80 && kmo < 0.90){
    test <- 'The KMO test yields a degree of common variance meritorious.'
  } else {
    test <- 'The KMO test yields a degree of common variance marvelous.'
  }
  
  ans <- list(  overall = kmo,
                report = test,
                individual = MSA,
                AIS = AIS,
                AIR = AIR )
  return(ans)
  
}    # end of kmo()
## Roda a função kmo para ao banco de dados eficacia
## Função KMO é o conjunto de codigos que você rocou da linha 80 à 127
#Indica a adequação da amostra à analise
kmo(SMS)

#EXTRAÇÃO DOS FATORES E RESULRADOS ESTATISICOS DA ANALISE FATORIAL EXPLORATÓRIA
# eficacia = Nome do banco de dados
# fm = metodo de estimação, aqui usando 'pa' que é principal axis
# rotate = metodo de rotação, aqui usando varimax
fa(SMS,2,fm="wls",rotate="oblimin")

#CALCULANDO OS PRESSUPOSTOS PARA A ANALISE FATORIAL CONFIRMATORIA
# Função para especificar o modelo que vai ser analisar
modefic <- specifyModel() # Após rodar a função, inserir linha após linha do modelo 

ME->Regulacao_externa,NA,1
ME->Introjecao,var4,NA
ME->Identificacao,var5,NA
MI->Atingir_objetivos,NA,1
MI->Experiencias_estimulantes,var7,NA
MI->Para_conhecer,var8,NA

#Erros and COv
ME<->ME,erro1,NA
MI<->MI,erro2,NA
MI<->ME,cov1,NA
Para_conhecer<->Atingir_objetivos,coverros1,NA
Identificacao<->Introjecao,coverros2,NA
Para_conhecer<->Introjecao,coverros3,NA

# Criando matriz de covariância para analise do modelo
cov <- cov(SMS, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))

# Rodando o modelo de equações estruturais aqui no caso uma Analise Fatorial Confirmatória
# Argumentos são: modefic = modelo especificado na linha 141
# cov = matriz de covariancia criada na linha 154
# N = numero de observações (amostra)
sem <- sem(modefic, cov, N=81)

# Reporta o resumo das analises do modelo de equações estruturais
summary(sem,fit.indices=c("GFI", "AGFI", "RMSEA", "NFI", 
                          "NNFI", "CFI", "RNI", "IFI", "SRMR", "AIC", "AICc"))

# Reporta os efeitos das trjetórias do modelo
effects(sem)

# Reporta os coeficientes estandardizados das trajetórias (linhas)
standardizedCoefficients(sem)

# Reporta os indices de modificação para melhorar o ajustamento do modelo
modIndices(sem)

###########################################################################################
#MODELO ESTRUTURAL
###########################################################################################
modeloSem<-data.frame(SMS,sf36,data$TempPratica)
modeloSem<-na.omit(modeloSem)
cor(modeloSem)
modeloSem<-remove.vars(modeloSem,c("Estado_Geral_de_Saude","Dor"))

#MODELO 1

#identifying the model
#MEASUREMENT MODELO
model<-'
ME =~ Regulacao_externa + Introjecao + Identificacao
MI =~ Atingir_objetivos + Experiencias_estimulantes + 
      Para_conhecer
PCS =~ Capacidade_Funcional + Limitacao_por_Aspectos_Fisicos + 
       Aspectos_Emocionais
MCS =~ Saude_Mental + Aspectos_Sociais + Vitalidade 

#regressios - before the ~ comes the outcome of the regression
PCS ~ MI + ME + data.TempPratica
MCS ~ MI
'
 
fit <- sem(model, data=modeloSem) #fitting the model
summary(fit, standardized=TRUE) #getting a summary of the model stats
# parameterEstimates(fit) #extracting parameters
standardizedSolution(fit) #standardized parameters
fitMeasures(fit,c("cfi","tli","rmsea","rmsea.ci.upper",
  "rmsea.ci.lower","srmr")) #fit indices
# mi <- modindices(fit) #modification index
# mi[mi$op == "=~",] #extract modificartion index to latent models
semPaths(fit,"std",layout="spring",residuals=FALSE,
  edge.color="black")
text(0.9,0.9,
  labels="Fit Indices \nCFI=0.98 \nTLI=0.97 \nRMSEA (95%CI)=0.05(0.00;0.08")

###########################################################################################
#MODELO GRAFOS
###########################################################################################
#attach(table2)
# data<-t(graph)
# data<-as.matrix(data)
names<-c("Iniciação com Amigos",
                   "Iniciação Reabilitação", "Benefícios Sociais",
                   "Beneíficos Físicos","Beneficios Psicológicos",
                   "Suporte Parental","Suporte Familiar","Busca por Saúde",
                   "Melhora da Competência","Reconhecimento","Novas Experiencias",
                   "Ansiedade para aprender","Experiencias previas",
                   "Mudanças contextuais",
                   "Prática Lazer","Formação Humana","Mot.Intrínseca",
                   "Mot.Extrínseca","QV Funcional","QV Subjetiva")

cor_data<-cor_auto(graph)

#layout = spring
Q1 <- qgraph(cor_data,
  borders = TRUE,
  cut=0.6, 
  minimum = 0.2, 
  layout = "spring",
  directed=FALSE,
  gray=FALSE)

#layout = circular
Q1 <- qgraph(dataMatrix, borders = TRUE, cut=10, 
  minimum = 5, labels=names,label.cex = 0.5, 
  layout = "circular",label.scale=FALSE,
  gray=TRUE)

























# # # Função para especificar o modelo que vai ser analisar
# # modefic <- specifyModel()

# #Latent Variables
# # ME->Regulacao_externa,NA,1
# # ME->Introjecao,var1,NA
# # MI->Identificacao,var2,NA
# # MI->Atingir_objetivos,NA,1
# # MI->Experiencias_estimulantes,var3,NA
# # MI->Para_conhecer,var4,NA
# # F1->Capacidade_Funcional,NA,1
# # F1->Limitacao_por_Aspectos_Fisicos,var5,NA
# # F1->Aspectos_Emocionais,var7,NA
# # F2->Saude_Mental,NA,1
# # F2->Aspectos_Sociais,var9,NA
# # F2->Vitalidade,var10,NA
# MI->F1,pred1,NA
# # ME->F1,pred2,NA
# MI->F2,pred3,NA
# ME->F2,pred4,NA
# #data.TempPratica->data.Amotivacao,pred7,NA
# # data.TempPratica->MI,pred8,NA
# # data.TempPratica->ME,pred9,NA
# data.TempPratica->F1,pred10,NA
# data.TempPratica->F2,pred11,NA


# #Erros and COv
# ME<->ME,erro1,NA
# MI<->MI,erro2,NA
# # ME<->MI,coverro1,NA
# F1<->F1,erro3,NA
# F2<->F2,erro4,NA
# data.TempPratica<->data.TempPratica,erro5,NA
# #data.Amotivacao<->data.Amotivacao,erro6,NA
# # data.Amotivacao<->Regulacao_externa,coverro1,NA

# Aspectos_Emocionais<->Limitacao_por_Aspectos_Fisicos,coverros1,NA
# Identificacao<->Introjecao,coverros2,NA
# Para_conhecer<->Introjecao,coverros3,NA
# Saude_Mental<->Vitalidade,coverro1,NA
# Aspectos_Emocionais<->Limitacao_por_Aspectos_Fisicos,coverro2,NA

# #EndofMODEL

# # Criando matriz de covariância para analise do modelo
# cov <- cov(modeloSem, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))

# # Rodando o modelo de equações estruturais aqui no caso uma Analise Fatorial Confirmatória
# # Argumentos são: modefic = modelo especificado na linha 141
# # cov = matriz de covariancia criada na linha 154
# # N = numero de observações (amostra)
# sem <- sem(modefic, cov, N=81)

# # Reporta o resumo das analises do modelo de equações estruturais
# summary(sem,fit.indices=c("GFI", "AGFI", "RMSEA", "NFI", 
#                           "NNFI", "CFI", "RNI", "IFI", 
#                           "SRMR", "AIC", "AICc","BIC"))

# # Reporta os efeitos das trjetórias do modelo
# effects(sem)

# # Reporta os coeficientes estandardizados das trajetórias (linhas)
# standardizedCoefficients(sem)

# # Reporta os indices de modificação para melhorar o ajustamento do modelo
# modIndices(sem)

# #Bootstrap SEM model
# bootSem(sem,R=100,Cov=cov,data=modeloSem,max.failures=1000000)

# #MODELO 2 - QV 1 dominio apenas
# # Funco para especificar o modelo que vai ser analisar
# modefic <- specifyModel() 
# # Apos rodar a funcao, inserir linha após linha do modelo 

# #Latent Variables
# ME->Regulacao_externa,NA,1
# ME->Introjecao,var1,NA
# ME->Identificacao,var2,NA
# MI->Atingir_objetivos,NA,1
# MI->Experiencias_estimulantes,var3,NA
# MI->Para_conhecer,var4,NA
# QV->Capacidade_Funcional,NA,1
# QV->Limitacao_por_Aspectos_Fisicos,var5,NA
# QV->Aspectos_Emocionais,var7,NA
# QV->Saude_Mental,NA,1
# QV->Aspectos_Sociais,var9,NA
# QV->Vitalidade,var10,NA
# MI->QV,pred1,NA
# ME->QV,pred2,NA

# #data.TempPratica->data.Amotivacao,pred7,NA
# data.TempPratica->MI,pred8,NA
# data.TempPratica->ME,pred9,NA
# data.TempPratica->QV,pred10,NA

# #Erros and COv
# ME<->ME,erro1,NA
# MI<->MI,erro2,NA
# QV<->QV,erro3,NA
# data.TempPratica<->data.TempPratica,erro5,NA
# #data.Amotivacao<->data.Amotivacao,erro6,NA
# # data.Amotivacao<->Regulacao_externa,coverro1,NA

# # Aspectos_Emocionais<->Limitacao_por_Aspectos_Fisicos,coverros1,NA
# # Identificacao<->Introjecao,coverros2,NA
# # Para_conhecer<->Introjecao,coverros3,NA
# # Saude_Mental<->Vitalidade,coverro1,NA
# # Aspectos_Emocionais<->Limitacao_por_Aspectos_Fisicos,coverro2,NA

# #EndofMODEL

# # Criando matriz de covariância para analise do modelo
# cov <- cov(modeloSem, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))

# # Rodando o modelo de equações estruturais aqui no caso uma Analise Fatorial Confirmatória
# # Argumentos são: modefic = modelo especificado na linha 141
# # cov = matriz de covariancia criada na linha 154
# # N = numero de observações (amostra)
# sem <- sem(modefic, cov, N=81)

# # Reporta o resumo das analises do modelo de equações estruturais
# summary(sem,fit.indices=c("GFI", "AGFI", "RMSEA", "NFI", 
#                           "NNFI", "CFI", "RNI", "IFI", 
#                           "SRMR", "AIC", "AICc","BIC"))

# # Reporta os efeitos das trjetórias do modelo
# effects(sem)

# # Reporta os coeficientes estandardizados das trajetórias (linhas)
# standardizedCoefficients(sem)

# # Reporta os indices de modificação para melhorar o ajustamento do modelo
# modIndices(sem)





# #MODELO 2
# # Função para especificar o modelo que vai ser analisar
# modefic <- specifyModel() # Após rodar a função, inserir linha após linha do modelo 

# #Latent Variables
# ME->Regulacao_externa,NA,1
# ME->Introjecao,var1,NA
# ME->Identificacao,var2,NA
# MI->Atingir_objetivos,NA,1
# MI->Experiencias_estimulantes,var3,NA
# MI->Para_conhecer,var4,NA
# ME<->ME,erro1,NA
# MI<->MI,erro2,NA
# MI->Capacidade_Funcional_RAW,mipred01,NA
# MI->Limitacao_por_Aspectos_Fisicos_RAW,mipred02,NA
# MI->Dor_RAW,mipred03,NA
# MI->Estado_Geral_de_Saude_RAW,mipred04,NA
# MI->Aspectos_Emocionais_RAW,mipred05,NA
# MI->Saude_Mental_RAW,mipred06,NA
# MI->Aspectos_Sociais_RAW,mipred07,NA
# MI->Vitalidade_RAW,mipred08,NA
# ME->Capacidade_Funcional_RAW,mepred11,NA
# ME->Limitacao_por_Aspectos_Fisicos_RAW,mepred12,NA
# ME->Dor_RAW,mepred13,NA
# ME->Estado_Geral_de_Saude_RAW,mepred14,NA
# ME->Aspectos_Emocionais_RAW,mepred15,NA
# ME->Saude_Mental_RAW,mepred16,NA
# ME->Aspectos_Sociais_RAW,mepred17,NA
# ME->Vitalidade_RAW,mepred18,NA

# #EndofMODEL

# # Criando matriz de covariância para analise do modelo
# cov <- cov(modeloSem, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))

# # Rodando o modelo de equações estruturais aqui no caso uma Analise Fatorial Confirmatória
# # Argumentos são: modefic = modelo especificado na linha 141
# # cov = matriz de covariancia criada na linha 154
# # N = numero de observações (amostra)
# sem <- sem(modefic, cov, N=184)

# # Reporta o resumo das analises do modelo de equações estruturais
# summary(sem,fit.indices=c("GFI", "AGFI", "RMSEA", "NFI", 
#                           "NNFI", "CFI", "RNI", "IFI", "SRMR", "AIC", "AICc"))

# # Reporta os efeitos das trjetórias do modelo
# effects(sem)

# # Reporta os coeficientes estandardizados das trajetórias (linhas)
# standardizedCoefficients(sem)

# # Reporta os indices de modificação para melhorar o ajustamento do modelo
# modIndices(sem)

# #ISSO NAO FUI EU QUEM COLOCOU AQUI, NAO SEI O QUE ACHO. ACHO QUE VOCE QUE INSERIU :)
# library(pastecs)
# stat.desc(data) 

# #MODELO 2 - Excluindo os paths:
# #EC <--- Ego  
# #Ego <--- Comple
# # Função para especificar o modelo que vai ser analisar
# modefic <- specifyModel() # Após rodar a função, inserir linha após linha do modelo 

# #Latent Variables
# EC->Persis,NA,1
# EC->Uniao,var4,NA
# EC->Prep,var5,NA
# EC->Habil,var6,NA
# EC->Esfo,var7,NA
# Taref->EC,pred1,NA
# Compro->Taref,cartq1,NA
# Compro->Ego,cartq2,NA
# Comple->Taref,cartq3,NA
# Prox->Taref,cartq5,NA
# Prox->Ego,cartq6,NA

# #Erros and COv
# EC<->EC,erro1,NA
# Habil<->Prep,cov1,NA
# Comple<->Comple,error2,NA
# Compro<->Compro,error3,NA
# Prox<->Prox,error4,NA
# Comple<->Compro,cov2,NA

# # Criando matriz de covariância para analise do modelo
# cov <- cov(data, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))

# # Rodando o modelo de equações estruturais aqui no caso uma Analise Fatorial Confirmatória
# # Argumentos são: modefic = modelo especificado na linha 141
# # cov = matriz de covariancia criada na linha 154
# # N = numero de observações (amostra)
# sem <- sem(modefic, cov, N=184)

# # Reporta o resumo das analises do modelo de equações estruturais
# summary(sem,fit.indices=c("GFI", "AGFI", "RMSEA", "NFI", 
#                           "NNFI", "CFI", "RNI", "IFI", "SRMR", "AIC", "AICc"))

# # Reporta os efeitos das trjetórias do modelo
# effects(sem)

# # Reporta os coeficientes estandardizados das trajetórias (linhas)
# standardizedCoefficients(sem)

# # Reporta os indices de modificação para melhorar o ajustamento do modelo
# modIndices(sem)

# #ISSO NAO FUI EU QUEM COLOCOU AQUI, NAO SEI O QUE ACHO. ACHO QUE VOCE QUE INSERIU :)
# library(pastecs)
# stat.desc(data) 

# #MODELO 3 - Excluindo Prox do modelo
# # Função para especificar o modelo que vai ser analisar
# modefic <- specifyModel() # Após rodar a função, inserir linha após linha do modelo 

# #Latent Variables
# EC->Persis,NA,1
# EC->Uniao,var4,NA
# EC->Prep,var5,NA
# EC->Habil,var6,NA
# EC->Esfo,var7,NA
# Taref->EC,pred1,NA
# Compro->Taref,cartq1,NA
# Compro->Ego,cartq2,NA
# Comple->Taref,cartq3,NA

# #Erros and COv
# EC<->EC,erro1,NA
# Habil<->Prep,cov1,NA
# Comple<->Comple,error2,NA
# Compro<->Compro,error3,NA
# Comple<->Compro,cov2,NA

# # Criando matriz de covariância para analise do modelo
# data<-remove.vars(data,c("Prox"))
# cov <- cov(data, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))

# # Rodando o modelo de equações estruturais aqui no caso uma Analise Fatorial Confirmatória
# # Argumentos são: modefic = modelo especificado na linha 141
# # cov = matriz de covariancia criada na linha 154
# # N = numero de observações (amostra)
# sem <- sem(modefic, cov, N=184)

# # Reporta o resumo das analises do modelo de equações estruturais
# summary(sem,fit.indices=c("GFI", "AGFI", "RMSEA", "NFI", 
#                           "NNFI", "CFI", "RNI", "IFI", "SRMR", "AIC", "AICc"))

# # Reporta os efeitos das trjetórias do modelo
# effects(sem)

# # Reporta os coeficientes estandardizados das trajetórias (linhas)
# standardizedCoefficients(sem)

# # Reporta os indices de modificação para melhorar o ajustamento do modelo
# modIndices(sem)

# #MODELO 4 - Variante com o Relação treinados altet como latente
# # Função para especificar o modelo que vai ser analisar
# modefic <- specifyModel() # Após rodar a função, inserir linha após linha do modelo 

# #Latent Variables
# EC->Persis,NA,1
# EC->Uniao,var4,NA
# EC->Prep,var5,NA
# EC->Habil,var6,NA
# EC->Esfo,var7,NA
# Compro->RTA,NA,1
# Comple->RTA,rta3,NA
# Prox->RTA,cartq5,NA
# Taref->EC,pred1,NA
# Ego->EC,pred2,NA
# RTA->Taref,pred3,NA
# RTA->Ego,pred4,NA

# #Erros and COv
# EC<->EC,erro1,NA
# RTA<->RTA,erro2,NA
# Habil<->Prep,cov1,NA
# Comple<->Comple,error2,NA
# Compro<->Compro,error3,NA
# Prox<->Prox,error4,NA
# Comple<->Prox,erro5,NA
# Compro<->Prox,erro6,NA
# #Comple<->Compro,cov2,NA

# # Criando matriz de covariância para analise do modelo
# cov <- cov(data, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))

# # Rodando o modelo de equações estruturais aqui no caso uma Analise Fatorial Confirmatória
# # Argumentos são: modefic = modelo especificado na linha 141
# # cov = matriz de covariancia criada na linha 154
# # N = numero de observações (amostra)
# sem <- sem(modefic, cov, N=184)

# # Reporta o resumo das analises do modelo de equações estruturais
# summary(sem,fit.indices=c("GFI", "AGFI", "RMSEA", "NFI", 
#                           "NNFI", "CFI", "RNI", "IFI", "SRMR", "AIC", "AICc"))

# # Reporta os efeitos das trjetórias do modelo
# effects(sem)

# # Reporta os coeficientes estandardizados das trajetórias (linhas)
# standardizedCoefficients(sem)

# # Reporta os indices de modificação para melhorar o ajustamento do modelo
# modIndices(sem)

# #ISSO NAO FUI EU QUEM COLOCOU AQUI, NAO SEI O QUE ACHO. ACHO QUE VOCE QUE INSERIU :)
# library(pastecs)
# stat.desc(data) 

# ###########################################################################################
# #NETWORK ANALYSIS
# ###########################################################################################


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
