####
#Spatial Autocorrelation
####

http://www.bias-project.org.uk/ASDARcourse/unit6_slides.pdf
http://www.people.fas.harvard.edu/~zhukov/Spatial6.pdf
https://cran.r-project.org/doc/contrib/intro-spatial-rl.pdf
http://spatial.ly/r/
http://spatial.ly/2013/04/analysis-visualisation-spatial-data/
https://pakillo.github.io/R-GIS-tutorial/
http://www.springer.com/us/book/9781461476177
http://gis.humboldt.edu/OLM/r/Spatial%20Analysis%20With%20R.pdf
http://gis.stackexchange.com/questions/45327/tutorials-to-handle-spatial-data-in-r

####
#An Introduction to Spatial Regression Analysis in R
###

library(spdep)

data(columbus)
summary(columbus)

#tranform a nb document (neighbourhood document) into a listw objetct which specifies the weights for each nb
col.listw <- nb2listw(col.gal.nb)

#Apply the Moran I test for residuals spatial autocorrelation
#arguments are a lm object with the regression predictors and the litsw object
col.moran <- lm.morantest(columbus.lm,col.listw)

#Lagrange Multiplier Test Statistics for Spatial Autocorrelation
columbus.lagrange <- lm.LMtests(columbus.lm,col.listw,test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))

#Maximum Likelihood Estimation of the Spatial Lag Model
columbus.lag <- lagsarlm(CRIME ~ INC + HOVAL,data=columbus,col.listw)

#OSL estimator
lagCRIME <- lag.listw(col.listw,CRIME)
wrong.lag <- lm(CRIME ~ lagCRIME + INC + HOVAL)
summary(wrong.lag)

#Maximum Likelihood Estimation of the Spatial Error Model
columbus.err <- errorsarlm(CRIME ~ INC + HOVAL,data=columbus,col.listw)

#Spatial Durbin Model
columbus.durbin <- lagsarlm(CRIME ~ INC+HOVAL,data=columbus,col.listw,type="mixed")
durbin.test1 <- LR.sarlm(columbus.durbin,columbus.err)