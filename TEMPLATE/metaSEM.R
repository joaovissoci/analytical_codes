
devtools::install_github("mikewlcheung/metasem")

library(metaSEM)

head(Digman97$data)

Digman97$n

fixed1 <- tssem1(Cov = Digman97$data, n = Digman97$n, method = "FEM")

fixed1 <- tssem1(Becker94$data, Becker94$n, method="FEM")

summary(fixed1)

coef(fixed1)

## Factor covariance among latent factors
Phi <- matrix(c(1,"0.3*cor","0.3*cor",1), ncol=2, nrow=2)

## Error covariance matrix
Psi <- Diag(c("0.2*e1","0.2*e2","0.2*e3","0.2*e4","0.2*e5"))

## S matrix
S1 <- bdiagMat(list(Psi, Phi))

## This step is not necessary, but it is useful for inspecting the model.
dimnames(S1)[[1]] <- dimnames(S1)[[2]] <- c("A","C","ES","E","I","Alpha","Beta") 

## Display S1
S1

## A matrix
Lambda <-
matrix(c(".3*Alpha_A",".3*Alpha_C",".3*Alpha_ES",rep(0,5),".3*Beta_E",".3*Beta_I"),
       ncol=2, nrow=5)
A1 <- rbind( cbind(matrix(0,ncol=5,nrow=5), Lambda),
             matrix(0, ncol=7, nrow=2) )

## This step is not necessary, but it is useful for inspecting the model.
dimnames(A1)[[1]] <- dimnames(A1)[[2]] <- c("A","C","ES","E","I","Alpha","Beta") 

## Display A1
A1

## F matrix to select the observed variables
F1 <- create.Fmatrix(c(1,1,1,1,1,0,0), as.mxMatrix=FALSE)

## Display F1
F1

### USINFG LAVAAN

model1 <- "## Factor loadings
           Alpha=~A+C+ES
           Beta=~E+I
           ## Factor correlation
           Alpha~~Beta"

RAM1 <- lavaan2RAM(model1, obs.variables=c("A","C","ES","E","I"),
                   A.notation="on", S.notation="with")
RAM1


#Estimating model
fixed2 <- tssem2(fixed1, Amatrix=A1, Smatrix=S1, Fmatrix=F1, 
                 diag.constraints=FALSE, intervals="LB",
                 model.name="TSSEM2 Digman97")
summary(fixed2)