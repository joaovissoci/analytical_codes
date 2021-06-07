###+++===--- Grupo de Estudos de Psicometria ---===+++###
### Deep Neural Network
##  Prof. PhD Víthor Rosa Franco - USF

### Start: CUIDADO! Apaga o espaço de trabalho, as figuras e o console
rm(list=ls()) # Apagar o environment
dev.off()     # Apagar os plots
cat("\014")   # Apagar o console

### Simulate data====
# Escolher o seed aleatório
seed = 1234
set.seed(seed)
# Escolher o tamanho amostral
n   <- 1000
# Simular valores para a variável preditora
x   <- runif(n, -4, 4)
# Estabelecer a função preditiva
f_x <- x*sin(pi*x) + x*cos(pi*x)
# Adicionar erro aos dados
y   <- rnorm(n, f_x, .5)
# Fazer um gráfico da relação observada
plot(y ~ x, col="gray"); lines(f_x[order(x)] ~ sort(x), col="black", lwd=2)
# Unir as variáveis em um mesmo banco de dados
df <- data.frame(x=x, y=y)

### GAM====
# Ajustar um modelo GAM para benchmark
require(mgcv) # Carregar o pacote mgcv
fit <- mgcv::gam(y ~ s(x))
# Plotar as predições
lines(predict(fit, data.frame(x))[order(x)] ~ sort(x), col="blue", lwd=2)
# Usar o MSE do GAM como threshold
TH <- mean(fit$residuals ^ 2)

### Neural Network: Passo-a-passo====
## Listar parâmetros iniciais
# Quantidade de nódulos latentes
M <- 5
# Nomear os parâmetros
parm.names <- c(paste("theta[",1:(M*2),"]", sep=""), paste("bias[",1:(M+1),"]", sep=""))
# Identificar quais são os pesos do modelo
pos.theta <- grep("theta", parm.names)
# Identificar os interceptos
pos.bias  <- grep("bias", parm.names)
# Listar as informações necessárias para o algoritmo de gradient descent
Data <- list(x=x, M=M, y=y, pos.theta=pos.theta, pos.bias=pos.bias)

## Escolher valores iniciais para os parâmetros
Initial.Values <- function(Data, seed){
  # Fixar o seed
  set.seed(seed)
  # Gerar valores iniciais para os pesos seguindo uma distribuição normal
  theta <- rnorm(Data$M*2)
  # Gerar valores iniciais para os interceptos seguindo uma distribuição normal
  bias <- rnorm(Data$M+1)
  # Retornar os valores em ordem
  return(c(theta, bias))
}

## Modelo de NN simples
Model <- function(parm, Data){
  ## Parameters
  theta <- parm[Data$pos.theta] # Pesos
  bias  <- parm[Data$pos.bias]  # Interceptos
  
  ## Feed-forward
  # Calcular: H_i = bias_1i + theta_1i * x
  L1 <- sweep(as.matrix(Data$x) %*% t(as.matrix(theta[1:Data$M])), 2, bias[1:Data$M], "+")
  # Calcular: Transformar cada H_i com uma função logística
  a1 <- apply(L1, 2, plogis)
  # Calcular: Y = bias_2i + somatório de i=1 a M de {theta_2i * H_i}
  L2 = rowSums(cbind(bias[Data$M+1],
                     a1 %*% as.matrix(theta[(Data$M+1):(Data$M * 2)])))
  
  ### Cost
  # MSE do modelo de NN
  C    <- mean((L2 - Data$y) ^ 2)
  return(list(C=C, pred=L2))
}

## Função custo
Cost <- function(parm, Model, Data) {
  # Retornar apenas o MSE
  Model(parm, Data)[["C"]]
}
steep <- function(parm, Model, Data, est, df) {
  # Retornar apenas o MSE
  Model(est - {parm * df}, Data)[["C"]]
}

## Calcular o gradiente
grad <- function(Model, par, Data, Interval=1e-6) {
  mat  <- matrix(par, nrow=length(par), ncol=length(par))
  diag(mat) <- par + Interval
  df <- {apply(mat, 2, Cost, Model=Model, Data=Data) - Cost(par, Model, Data)} / Interval
  df[which(!is.finite(df))] <- 0
  return(df)
}

## Algoritmo de gradient descent
GD <- function(Model, startvalue, Data, Interval=1e-6, maxit=100, step=1e-4, threshold=1e-2) {
  ## Opening message
  cat("Gradient Descent estimation will run for ", maxit, " iterations.\n\n", sep="")
  startTime = proc.time()
  
  ## Initial settings
  pb   <- txtProgressBar(min=0, max=maxit, style=3)
  par  <- df <- array(dim = c(maxit,length(startvalue)))
  f    <- vector("numeric", length=maxit)
  step <- rep(step, length=maxit)
  
  ## First step
  # Calculate gradient
  df[1,]  <- grad(Model, startvalue, Data, Interval=Interval)
  #step[1] <- optim(0, steep, df=df[1,], Model=Model,
  #                 Data=Data, est=startvalue)$par
  # Calculate new value for parameters
  par[1,] <- startvalue - {step[1] * df[1,]}
  # Calculate cost
  f[1]    <- Model(par[1,], Data)[["C"]]
  # Update progress bar
  setTxtProgressBar(pb, 1)
  
  ## Start estimation
  for(run in 2:maxit) {
    # Calculate gradient
    df[run,]  <- grad(Model, par[run-1,], Data, Interval=Interval)
    #step[run] <- optim(0, steep, df=df[run,], Model=Model,
    #                   Data=Data, est=par[run-1,])$par
    # Calculate new value for parameters
    par[run,] <- par[run-1,] - {step[run] * df[run,]}
    # Calculate cost
    f[run]    <- Model(par[run,], Data)[["C"]]
    # Update progress bar
    setTxtProgressBar(pb, run)
    if (f[run] < threshold) {
      setTxtProgressBar(pb, maxit)
      cat("\nConvergence achieved!")
      break
    }
  }
  if (run < maxit) {
    f    <- f[-c((run+1):maxit)]
    par  <- par[-c((run+1):maxit),]
    df   <- df[-c((run+1):maxit),]
    step <- step[-c((run+1):maxit)]
  }
  close(pb)
  cat("\n")
  
  # Final messages
  stopTime = proc.time()
  elapsedTime = stopTime - startTime
  cat("It took ",round(elapsedTime[3],2)," secs for the run to finish. \n", sep="")
  
  # Return results
  Results <- list("Cost"=f, "Estimates"=par, "Gradients"=df,
                  "Best"=par[which.max(f),], "step"=step)
  return(Results)
}

### Ajustar o modelo====
# Gerar valores aleatórios
IV <- Initial.Values(Data, seed=seed)
# Iniciar estimação usando o algoritmo de Gradient Descent
NNp <- GD(Model=Model, IV, Data=Data, Interval=1e-6,
          maxit=5000, step=1e-2, threshold=TH)
# Plotar valores preditos
lines(Model(NNp$Best, Data)[["pred"]][order(x)] ~ sort(x), col="purple", lwd=2)

### Neural Network====
## Vamos usar um pacote com algoritmos mais efetivos e eficientes :)
require(neuralnet)
#require(deepnet)
# Ajustar um modelo igual ao nosso "Model"
NN <- neuralnet(formula=y ~ x, data=df, hidden=5, startweights=NULL,
                lifesign="full", threshold=TH)
#NN <- nn.train(y=y, x=as.matrix(x), hidden=5, activationfun="tanh")
# Plotar as predições
lines(predict(NN, data.frame(x))[order(x)] ~ sort(x), col="red", lwd=2)
#lines(nn.predict(NN, as.matrix(x))[order(x)] ~ sort(x), col="red", lwd=2)

# Ajustar modelo alternativo "deep" com profundidade 2 e largura 5
DNN1 <- neuralnet(y ~ x, data=df, hidden=c(5, 5), startweights=NULL,
                  lifesign="full", threshold=TH)
# Plotar as predições
lines(predict(DNN1, data.frame(x))[order(x)] ~ sort(x), col="green", lwd=2)

# Ajustar modelo alternativo "deep" com profundidade 3 e largura 5
DNN2 <- neuralnet(y ~ x, data=df, hidden=c(5, 5, 5), startweights=NULL,
                  lifesign="full", threshold=TH)
# Plotar as predições
lines(predict(DNN2, data.frame(x))[order(x)] ~ sort(x), col="gold", lwd=2)

### Comparar resultados====
cbind("GAM"=TH, "NN"=mean({predict(NN, data.frame(x)) - y}^2),
      "DNN1"=mean({predict(DNN1, data.frame(x)) - y}^2),
      "DNN2"=mean({predict(DNN2, data.frame(x)) - y}^2))

### Exemplo 2: Autoencoders====
### Start
rm(list=ls())
dev.off()
cat("\014")

### Packages====
require(psych)
require(paran)
require(h2o)

### Functions====
predScores <- function(model, x) {
  sweep(t(as.matrix(model$loadings) %*% t(model$scores)), 2, colMeans(x), "+")
}

### Data====
data(bfi)
ocean <- bfi[complete.cases(bfi[,1:25]),1:25]

### PA-EFA====
# Parallel analysis and EFA with Maximum Likelihood
PA  <- paran::paran(ocean)
EFA <- psych::fa(ocean, nfactors=PA$Retained, fm="mle")
# Extract Factor scores
EFA_FS  <- EFA$scores

### AE1 - Single Hidden Layer==== 
# Start h2o
h2o.init()
# Conver to h2o format
features <- as.h2o(ocean)
# Select the depth and width of the hidden layers
hidden = c(PA$Retained)
# Train an autoencoder
ae1 <- h2o.deeplearning(x=seq_along(features), training_frame=features,
                        autoencoder=T, hidden=hidden, activation = 'Tanh',
                        sparse=F, epochs=100)
# Extract the deep features-deep factor scores
ae1_FS <- as.matrix(h2o.deepfeatures(ae1, features, layer = 1))

### AE3 - Three Hidden Layers==== 
# Select the depth and width of the hidden layers
hidden = c(ncol(ocean), PA$Retained, ncol(ocean))
# Train an autoencoder
ae3 <- h2o.deeplearning(x=seq_along(features), training_frame=features,
                        autoencoder=T, hidden=hidden, activation = 'Tanh',
                        sparse=F, epochs=350)
# Extract the deep features-deep factor scores
ae3_FS <- as.matrix(h2o.deepfeatures(ae3, features, layer = 2))

### Compare performance====
# Get the predict observed scores
efa_ER <- predScores(EFA, ocean)
ae1_ER <- predict(ae1, features)
ae3_ER <- predict(ae3, features)

# Calculate MAE
mean(unlist(abs(as.matrix(efa_ER) - ocean)))
mean(unlist(abs(as.matrix(ae1_ER) - ocean)))
mean(unlist(abs(as.matrix(ae3_ER) - ocean)))

# Calculate RMSE
sqrt(mean(unlist((as.matrix(efa_ER) - ocean)^2)))
sqrt(mean(unlist((as.matrix(ae1_ER) - ocean)^2)))
sqrt(mean(unlist((as.matrix(ae3_ER) - ocean)^2)))

####====---- THE END ----====####