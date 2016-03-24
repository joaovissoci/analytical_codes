library(poLCA)
res2 = poLCA(cbind(homeless=homeless+1, 
   cesdcut=cesdcut+1, satreat=satreat+1, 
   linkstatus=linkstatus+1) ~ 1, 
   maxiter=50000, nclass=3, 
   nrep=10, data=ds)

# Tutorial on LCA
http://www.ats.ucla.edu/stat/mplus/seminars/lca/default.htm

# MOdel Fit
*BIC and AIC

*X2 Stats

*Lo-Mendell-Rubin Test
Tests your choice of model to another possiblity by comparing to
one less category.

*Standarized Residuals

# Model Usefulness
*Model interpretability

# Classification quality
*Classification tables

*Entropy
Summary of how well people are classified into the models