#################################################################
#template_mediationlavaan_code.R
#################################################################
#
#
#################################################################
#MEDIATION ANALYSYS WITH LAVAAN
##################################################################
#
# The example illustrates the use of the ":=" operator in the 
# lavaan model syntax. This operator 'defines' new parameters 
# which take on values that are an arbitrary function of the 
# original model parameters. The function, however, must be 
# specified in terms of the parameter labels that are 
# explicitly mentioned in the model syntax. 
# By default, the standard errors for these defined 
# parameters are computed by using the so-called Delta method. 
# As with other models, bootstrap standard errors can be requested 
# simply by specifying se = "bootstrap" in the fitting function.
#
#extracted from - http://lavaan.ugent.be/tutorial/mediation.html
#################################################################

set.seed(1234) #setting a seed number to replicate the sample generation
X <- rnorm(100) #ganarate a random dataset
M <- 0.5*X + rnorm(100) #ganarate a random dataset
Y <- 0.7*M + rnorm(100) #ganarate a random dataset

Data <- data.frame(X = X, Y = Y, M = M) #organize dataframe

#specify model
# Y - outcome
# X - predictor
# M - mediator

model <- ' # direct effect
             Y ~ c*X
           # mediator
             M ~ a*X
             Y ~ b*M
           # indirect effect (a*b)
             ab := a*b
           # total effect
             total := c + (a*b)
         '

fit <- sem(model, data = Data)
summary(fit)